# Telling Maxima which cell an output belongs to     {#asyncmaximaoutput}

## The question

If Maxima gained multi-threading, a command could start a background job
that produces its output at some later time -- long after the cell that
started it has finished and the user has moved on. **Can we tell Maxima
which cell a given output belongs to, in a way a background thread will
still know when it eventually produces that output?**

Short answer: **yes**, and nothing about it is exotic. The identifier can be
a plain Lisp special variable that wxMaxima sets ahead of each cell, which
a thread captures at spawn time and echoes back inside its output. The
mechanism was tried against a real Maxima over a real socket and works.

The interesting part is not the mechanism, it is the four sharp edges
around it. Three of them are demonstrated below with output from actual
runs; all four will silently produce wrong behaviour rather than an error
if they are got wrong.

Everything here was measured against Maxima on SBCL 2.6.0 (`:SB-THREAD`
present in `*features*`, `sb-thread:make-thread` genuinely available),
driven through a socket set up exactly the way wxMaxima sets one up
(`maxima -s <port> --very-quiet`).

## How output finds its cell today

There is no identifier of any kind in the wxMaxima <-> Maxima protocol.
Association is purely positional:

- `MaximaOutputAppender::ConsoleAppend()` asks
  `Worksheet::GetWorkingGroup(true)` -- "the cell being evaluated right now,
  or failing that the last one" -- and appends there.
- `DocumentCellPointers::m_workingGroup` is set by `MaximaEvaluator` as the
  evaluation queue advances.
- The queue advances in `EvaluationQueue::RemoveFirst()`, called from
  `MaximaResponseReader` for **every** main `(%iN)` prompt Maxima emits.

So "which cell is this output for" is answered entirely by *when* the
output arrives. That assumption is exactly what background jobs break, and
it is why an identifier has to be added rather than derived.

## The mechanism, demonstrated

wxMaxima sets a Lisp variable before each cell's own commands. A command
that spawns a thread captures the *current* value lexically; the thread
wraps its eventual output in a tag carrying it.

```lisp
:lisp-quiet (defparameter *wx-cell-id* nil)
:lisp-quiet (setq *wx-cell-id* "cell-AAA")
:lisp (let ((id *wx-cell-id*))          ; <-- captured HERE, at spawn time
        (sb-thread:make-thread
          (lambda ()
            (sleep 3)
            (format t "~%<wxasync id=\"~a\">...</wxasync>~%" id)
            (finish-output))))
:lisp-quiet (setq *wx-cell-id* "cell-BBB")
2+2;
```

Real transcript from the socket, trimmed:

```
>>>SEND: :lisp-quiet (setq *wx-cell-id* "cell-BBB")
job started
<wxasync id="cell-AAA">RESULT FROM BACKGROUND JOB OF cell-AAA</wxasync>

>>>SEND: 2+2;
                                       4
```

The background output arrives while the *current* cell is already
`cell-BBB`, and correctly carries `cell-AAA`. That is the whole idea.

Two incidental facts that make this easier than expected:

- Maxima's socket is the **global** value of `*standard-output*`
  (`#<FD-STREAM for "socket ...">`), not a per-thread dynamic binding, so a
  spawned thread can write to the real socket with no plumbing at all --
  plain `(format t ...)` from the thread reaches wxMaxima.
- `:lisp-quiet` forms produce no prompt and no output of their own, so
  setting the variable per cell costs nothing visible. This is the same
  property `m_configCommands` already depends on (see AGENTS.md).

## The four sharp edges

### 1. The id must be captured at spawn time, not read when the thread runs

This is the one that will bite. Reading `*wx-cell-id*` *inside* the thread
body reads whatever the global value happens to be when the thread finally
runs -- which is, by construction, a different cell. Confirmed:

```lisp
:lisp-quiet (setq *wx-cell-id* "cell-AAA")
:lisp (sb-thread:make-thread
        (lambda () (sleep 3)
          (format t "<wxasync id=\"~a\">...</wxasync>" *wx-cell-id*)))
:lisp-quiet (setq *wx-cell-id* "cell-BBB")
```

produced

```
<wxasync id="cell-BBB">WRONG-read-at-run-time</wxasync>
```

-- the wrong cell, silently, with no error. Whatever API is offered to
users for starting a background job must capture the id itself, rather than
leaving it to whoever writes the thread body to remember.

### 2. Concurrent writes to Maxima's output stream corrupt it

Not merely interleave -- **corrupt**. Three threads each writing 40 small
chunks to `*standard-output*` without a lock produced output in which the
command's own result echo appeared **three times** and one thread's entire
40-chunk block appeared **twice**: SBCL's FD-stream buffer is shared, and
concurrent flushes replay buffered content.

A single mutex around each thread's write fixes it completely. Measured, on
the identical test with `sb-thread:with-mutex` added:

| | without lock | with lock |
|---|---|---|
| command echo | 3x (duplicated) | 1x |
| chunks per thread | duplicated blocks | exactly 40, 40, 40 |
| ordering | one block emitted twice | each thread's block contiguous |

So any threading support needs one output lock that *every* writer --
background threads and the main evaluation thread alike -- goes through,
or an output queue drained by a single writer.

### 3. A background job must never emit a prompt

`EvaluationQueue::RemoveFirst()` advances the queue by exactly one cell for
every main `(%iN)` prompt, and has no way to tell whose prompt it is.
AGENTS.md already documents this costing a whole 21-cell evaluation queue
in one shot when a plain statement crept into `m_configCommands`.

Background output is safe only as long as it is pure output with no prompt
attached -- which it is in the transcript above, and which `:lisp-quiet`
guarantees for the id-setting side. Any design where a finishing thread
causes Maxima to print a prompt will silently drop queued cells.

### 4. `ProcessData()` matches tags without attributes

`Maxima::ProcessData()` recognises a top-level tag by an exact string
compare:

```cpp
wxString tagstartname = wxS("<") + tag->first + wxS(">");
if (m_processingBuffer.StartsWith(tagstartname))
```

So `<wxasync id="cell-AAA">` -- the form used in the experiments above,
chosen because it is the natural one -- would **not** be recognised by the
current dispatcher. Either

- put the id inside the body (`<wxasync><id>cell-AAA</id>...</wxasync>`),
  which needs no change to `Maxima.cpp` at all, only a new entry in
  `m_knownTags` and a new `EventCause`; or
- extend the matcher to also accept `<tag` followed by a space.

The first is the smaller change and keeps the framing rule ("a known tag is
a bare `<tag>` at the start of the buffer") intact.

## What the cell identifier should be

Two candidates, both already present:

- **`Cell::GetUUID()`** -- stable across a Maxima restart, already used by
  the MCP tools, and already what `#UUID` filename fragments navigate by.
  Caveat: it is generated lazily (empty until something asks for it), so
  wxMaxima would have to force one for every cell it sends, and a generated
  UUID is written out on the next save. The MCP server already has exactly
  this side effect and it is documented as acceptable.
- **Maxima's own `%i<N>` counter** -- costs no new state and both sides
  already see it, but wxMaxima keeps no label-to-cell map today (there is
  no `GetCellByLabel()` anywhere), and the numbering restarts when Maxima
  does, so a background job outliving a restart would point at the wrong
  cell.

UUID looks like the better answer, with the laziness handled explicitly
rather than accidentally.

## Delivering the output to the cell

On arrival, the routing change is small and local:

- A new `Maxima::EventCause` plus an `m_knownTags` entry for the new tag --
  the established extension point; `XML_ASCIIMATH` is the most recent
  precedent.
- A handler that looks the cell up by UUID and, instead of
  `Worksheet::GetWorkingGroup(true)`, points `m_parser` at that cell
  (`MathParser::SetGroup()`) before appending.

One thing that genuinely has to be handled rather than assumed: **the cell
may be gone** by the time its background job finishes -- deleted, or the
worksheet closed. Per this project's own rule, anything holding a cell
across time must use `CellPtr` (it nulls itself on destruction) and
null-check on use; and the lookup must tolerate a UUID matching nothing at
all, since Maxima has no way of knowing the cell was deleted.

## Availability

Threads are a property of the Lisp underneath Maxima, not of Maxima itself.
SBCL here has them. GCL -- still a common Maxima build on some platforms --
does not. So this would be a conditionally-available feature, and wxMaxima
would need to cope with a Maxima that never sends an async tag at all,
which the design above does for free: no tag, no new behaviour.

## Status

This is an investigation, not an implementation. Nothing in wxMaxima has
been changed. The experiments above are reproducible against a stock
Maxima/SBCL with nothing but a socket and the snippets quoted.
