---
name: wxmaxima-maxima-protocol
description: How wxMaxima talks to Maxima - the socket, the MathML-like XML, wxMathML.lisp, the evaluation queue, and batch mode. Use when touching Maxima.cpp, MathParser, wxMathML.lisp, the evaluation queue, --batch/--exit-on-error, or when output arrives wrong, late, or not at all.
---

# Talking to Maxima

wxMaxima drives a real `maxima` subprocess over a **local TCP socket**. Maxima
answers with a MathML-like XML dialect that `MathParser` turns into cells.

## The path a result takes

1. `MaximaEvaluator` sends a command (evaluation queue + command protocol).
2. `Maxima` (`src/Maxima.cpp`) reads the socket **on a worker thread** and posts
   `EVT_MAXIMA` events to the main thread.
3. `MaximaResponseReader` handles the incoming tags and dispatches them.
4. `MathParser` builds the cell tree.

`src/wxMathML.lisp` is what teaches Maxima to emit that XML. It is compiled into
the binary via CMake's bin2h - but `--wxmathml-lisp=<path>` overrides it with an
external file, so a change can be tried **without rebuilding**. Use that while
iterating; it saves a great deal of time on this codebase.

## Chunk boundaries are not output boundaries

The single most misleading property of this interface: **data arrives in
socket/timer-sized chunks that have nothing to do with Maxima's own output
structure.** Any logic that classifies a chunk by looking at how it *starts* is
wrong, and will be wrong only sometimes.

That is a real bug that shipped: 2-D ASCII-art maths was rendered with the label
line in a different font from the rest, because the code decided
monospace-vs-proportional per chunk by testing whether it began with `"(%"`. A
block's label could land in a separate read and be misclassified. The fix was to
make Maxima delimit the block explicitly - `wxMathML.lisp` wraps the stock ASCII
printer in `<wxxml-asciimath>` markers via an `*alt-display2d*` hook, and
`Maxima::ProcessData()` only fires the event once the whole tag is complete.

**Rule: if you need to know what something is, make Maxima tag it. Never infer
it from what a chunk happens to contain.**

## Lisp side conventions (`wxMathML.lisp`)

- `with-output-to-string`, not recursive concatenation - the latter is
  quadratic on big outputs.
- `unwind-protect` when modifying global state such as `$lmxchar`.
- `(intern ...)` rather than `read-from-string` for dynamic symbols.
- Reuse Maxima's own printers where possible (as the ASCII-art hook does) rather
  than reimplementing them.

## Escaping

`Maxima::EscapeVarnameForMaxima` handles the characters that need it (`,`, `°`,
and friends). A **leading digit** must be escaped too (`\1a`) - easy to miss,
because it is the position that matters, not the character.

## Batch mode

`--batch` / `--exit-on-error` are the headless path the test suite leans on, and
they have sharp edges that have all bitten:

- An **unanswerable question** from Maxima must halt, not hang - batch mode has
  no one to answer it. Killing Maxima immediately on that halt matters; merely
  closing left processes behind.
- `--exit-on-error` must not go **toothless** after a transient empty queue.
- A **startup-config race** could silently drop queued cells.
- A worksheet that asks an interactive question will block forever under
  `--batch`. Either pick a non-interactive worksheet or wrap it in `timeout`.

## Process lifetime

`MaximaProcessManager` owns spawn/kill/connect. Two things to remember:

- The Lisp process is a **grandchild**, so cleaning up needs a group kill, not
  just killing the direct child. Orphaned `maxima` processes after an abnormal
  wxMaxima death are this.
- **gnuplot children count too.** An orphaned async gnuplot query holding a pipe
  open is what made a CI test look like a teardown wedge for weeks - wxMaxima
  had exited fine, but the pipe kept the harness waiting. The
  `wxmaxima_no_stray_children` test guards it.
- On Windows, restarting requires explicitly resetting the network client
  (`m_client.reset()`) and streams in `KillMaxima`, or the socket state is
  wrong on the next start.

## Probing gnuplot

Must be asynchronous (`wxEXEC_ASYNC`). A synchronous probe blocks the UI and,
on Linux, can disrupt the global menu.
