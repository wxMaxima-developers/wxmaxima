# Telling Maxima which cell an output belongs to     {#asyncmaximaoutput}

## What this is

Maxima is single-threaded today. If it gains the ability to run a command
in the background, a job will produce its output long after the cell that
started it has finished and the user has moved on -- and wxMaxima has no
identifier for a cell anywhere in its protocol. It decides which cell an
output belongs to purely by *when* the output arrives: whatever cell is
being evaluated at that moment. For a background job that is not merely
imprecise, it is reliably wrong.

wxMaxima's half of a protocol that fixes this is implemented and working.
Nothing exercises it in a normal session yet; it is here so that a Maxima
that can run things in the background has something correct to talk to.

The mechanism is one variable and one tag:

- wxMaxima sets `*wx-cell-id*` ahead of each cell, as a `:lisp-quiet` form
  bundled with the config commands it already sends.
- A background job captures that value **at spawn time** and wraps its
  eventual output in `<wxasync><id>...</id>...</wxasync>`.
- wxMaxima routes that block to the cell the id names, not to whatever cell
  is current.

Everything below was measured against Maxima on SBCL 2.6.0, and then
verified end to end in the real application driving real background threads.

## Using it from Maxima

```lisp
:lisp (wx-spawn-async
        (sleep 30)
        (wx-async-display (meval ...)))
```

`wx-spawn-async` (`src/wxMathML.lisp`) runs its body in a background thread
on behalf of the cell being evaluated now. Inside it:

| | |
|---|---|
| `wx-async-text` | plain text onto the originating cell |
| `wx-async-display` | a Maxima expression, rendered as that cell's output would be |
| `wx-async-error` | a failure message onto the originating cell |

All three default their target to `*wx-cell-id*`, which the macro rebinds
inside the thread -- so the obvious call is also the correct one.

## The four things that are easy to get wrong

Each of these was found by testing rather than by reading, and each fails
silently rather than loudly.

### 1. The id must be captured at spawn time

Reading `*wx-cell-id*` when the thread *runs* yields whatever cell is
current by then, which is by construction a different one. Measured: a job
started under cell A delivered its output to cell B, with no error.

This is why `wx-spawn-async` **rebinds** `*wx-cell-id*` inside the thread
rather than only capturing it into a variable the body cannot see. The
first implementation did the latter, and the natural thing to write in the
body -- `(wx-async-text "...")`, which consults `*wx-cell-id*` -- was
therefore wrong. An API whose obvious use is the broken one is a bad API;
the rebinding removes the trap instead of documenting it.

### 2. Concurrent writes corrupt the output stream

Not merely interleave. Three threads each writing 40 short chunks to
`*standard-output*` with no lock produced output in which the command's own
result echo appeared **three times** and one thread's entire block appeared
**twice** -- SBCL's FD-stream buffer is shared, and concurrent flushes
replay buffered content.

| | without lock | with lock |
|---|---|---|
| command echo | 3x | 1x |
| chunks per thread | duplicated blocks | exactly 40, 40, 40 |
| ordering | one block emitted twice | each block contiguous |

`wx-emit-async` therefore takes `*wx-output-lock*` for the whole block.
Any future writer has to take the same lock.

### 3. A background job must not ask questions

The sharpest one, and the one where the obvious defence is insufficient.

Maxima's `retrieve` (`src/macsys.lisp`) **prints the question first and
reads afterwards**:

```lisp
(format-prompt t "~M" msg) (mterpri)
(mread-noprompt *standard-input* nil)
```

Maxima's input and output are the *same socket*, so an unguarded read in a
background thread would eat bytes out of the command stream wxMaxima is
writing into. But closing the thread's `*standard-input*` only stops the
read -- by then the question is already on the wire, and wxMaxima, which
cannot know which thread wrote it, reads it as a question from whatever
cell is currently being evaluated. Observed exactly that: a background
`asksign()` put its question on an unrelated cell and left that cell
waiting for an answer that would have gone somewhere else. Binding
`*standard-input*` to an *empty* stream is no better -- the read returns
EOF, the ask machinery loops, and the session stops answering commands at
all.

So the ask is refused before it prints anything, by wrapping `retrieve`
itself, gated on `*wx-in-async-job*` so ordinary synchronous questions are
completely unaffected. The job gets a clear message on its own cell:

> This background computation tried to ask a question, which a background
> computation cannot do -- give it everything it needs up front (for
> example with assume()).

The closed `*standard-input*` is kept behind that, for anything that reads
without going through `retrieve`.

### 4. `ProcessData()` matches tags without attributes

`Maxima::ProcessData()` recognises a top-level tag by an exact compare
against `"<" + name + ">"`, so `<wxasync id="...">` would never be
recognised at all. The id travels in the body instead, as `<id>...</id>`,
which needs no change to the framing rule.

## The wxMaxima side

- `Maxima::XML_ASYNC_OUTPUT` plus a `m_knownTags` entry for `wxasync` --
  the established extension point, `XML_ASCIIMATH` being the previous one.
- `MaximaEvaluator::CellIdConfigCommand()` emits the per-cell
  `:lisp-quiet (setq *wx-cell-id* "...")`, appended to `m_configCommands`
  next to the existing `LinelConfigCommand()`. `:lisp-quiet` is not
  stylistic: `EvaluationQueue::RemoveFirst()` advances the queue by one
  cell for every main prompt and cannot tell whose prompt it is, so a plain
  statement here would silently drop a queued cell per command sent. The id
  is only resent when it changes, not once per statement.
- `MaximaResponseReader::ReadAsyncOutput()` parses the block, looks the
  cell up, and appends through the ordinary path with only the destination
  changed.
- `Worksheet::GetInsertGroup()` honours `m_asyncOutputTarget` when set;
  `Worksheet::AsyncOutputTarget` is the scoped setter, so an early return
  cannot leave later ordinary output misrouted.

Three things that had to be handled explicitly, none of them obvious until
the feature was actually run:

- **The cell may be gone.** `m_asyncOutputTarget` is a `CellPtr`, and a
  UUID matching nothing is discarded with a log message rather than filed
  somewhere arbitrary -- Maxima has no way of knowing a cell was deleted.
- **A cell's first output cell is its label slot.** `AppendOutput()`
  assigns the first cell it is ever given to `m_output`, which `GetLabel()`
  returns and `GetOutput()` skips. Every ordinary Maxima response starts
  with a `(%oN)` label, so that slot is always already filled; a background
  job's output has none, so on a cell that has produced no output yet --
  one ending in `$`, or one whose output was cleared -- it silently
  *became* the label and never rendered. An empty label is inserted first
  when the slot is free.
- **Nothing else schedules the recalculation.** `InsertLine()` only asks
  for a redraw; the layout of a cell that gained output normally comes from
  that cell being the one under evaluation, which is exactly what a
  background job's cell is not. `ReadAsyncOutput()` requests it explicitly.

Async output deliberately does **not** scroll the worksheet, even with
"follow evaluation" on, and deliberately does not un-collapse a cell whose
output the user has hidden -- same reasoning as GH #1952: a result arriving
is not a reason to yank the view away from what the user is doing.

## Availability

Threads are a property of the Lisp under Maxima, not of Maxima.
SBCL has them; GCL does not. `wx-async-available-p` reflects that,
`*wx-output-lock*` is `nil` where there is nothing to serialize, and a
Maxima that never sends the tag simply never triggers any of this.

## Verified

In the real application, driving real `sb-thread` background jobs:

- A job started by cell 1 delivers to cell 1, six seconds after cell 1
  stopped being current and two cells later.
- Two concurrent jobs each reach their own cell.
- A job that tries to `asksign()` reports the refusal on its own cell; no
  question reaches the wire and no other cell is left waiting.
- A job that signals an error reports it on its own cell.
- Output naming a cell that does not exist is discarded with a log message,
  leaving the session healthy.
- Ordinary synchronous evaluation in the same session is unaffected
  throughout.
