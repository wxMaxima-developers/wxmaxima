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

## Two intermittent CI failures, in full

Both are evaluation-queue bugs that surface as a flaky test, and both cost
several sessions each. `lisp_mode` is **fixed**; `tutorial_10Minutes` has a
verified workaround but its underlying bug - a whole statement silently
dropped before it ever reaches `Maxima::Write()` - is **confirmed and still
open** (GH #2196).

Kept at length deliberately: most of the value is the list of theories that
were directly disproven, and the reproduction recipes, which are not obvious
(`lisp_mode` needs CPU contention, not repetition; `tutorial_10Minutes` needs
~50 attempts before "cannot reproduce" means anything).

> **Provenance.** The entries below were moved here verbatim from
> `AGENTS.md`, which had grown past 4800 lines. Where one of them says
> "this file" about a *document* (as opposed to the C++ source it is
> discussing), it means `AGENTS.md` -- check there, and in the sibling
> skills, for anything it cross-references that is not here.

- **`tutorial_10Minutes` intermittent CI failure -- the workaround below is
  verified, but the real underlying bug is CONFIRMED and still UNFIXED
  (2026-08). Do not re-close this as "explained by Maxima-side
  nondeterminism" -- that theory was directly disproven, see below.**
  Failed with "Batch mode: Maxima asked a question with no scripted answer
  available" for `Is a positive or negative?`, at a genuinely low rate
  (reproduced locally at roughly 1-in-20 to 1-in-30 with a tight
  parallel-Xvfb repro loop -- not reproducible from a handful of manual
  runs, so don't conclude "can't reproduce" from fewer than ~50 attempts).
  The asking cell is `assume(a > 0)$ integrate(1/(x^2+a),x); forget(a > 0)$`
  (the tutorial's own demonstration that `assume()` normally makes the
  question unnecessary) -- and since it was never meant to need an
  interactive answer, it had none recorded, hence the halt.
  **Two theories were tried and directly disproven, in order, before the
  real one was confirmed -- both by hard evidence, not by reasoning about
  the code:**
  1. *Stale "current working group" pointer.* Instrumenting
     `Worksheet::WillAutoAnswer()` to dump
     `GetDocumentCellPointers().GetWorkingGroup(false)` vs.
     `Worksheet::GetWorkingGroup(true)`'s resolved cell showed they were
     always the *same*, correct `GroupCell` -- ruled out.
  2. *Maxima/GCL-internal nondeterminism* (the theory originally written
     here): that `integrate()`'s own algorithm occasionally doesn't consult
     the assumption database, e.g. via GCL's address-based hash-table
     iteration order. This looked plausible because `EvaluationQueue` sends
     each cell's statements one at a time, gated on receiving Maxima's own
     prompt for the previous one, so it *seemed* like `assume(a > 0)$` must
     already have been fully processed before `integrate(...)` was sent.
     **This was wrong, and directly disproven** by an actual `tcpdump`
     capture (`tcp portrange 49000-49999` on `lo`, per the debugging
     technique note under Communication with Maxima) of a live failing run,
     followed with `tshark -z follow,tcp,ascii,<stream>`: the raw
     wxMaxima->Maxima wire transcript shows `integrate( 1 / (x^2 + a), x);`
     sent *immediately* after the previous cell's last command, with
     **`assume(a > 0)$` never transmitted at all**. Not corrupted, not
     reordered, not delayed -- entirely absent from the wire. (Credit:
     this line of investigation started from the user's specific recollection
     of a past incident where a Lisp runtime's flush-on-no-wait behavior hit
     an MTU-triggered code path that shuffled packets while keeping their
     content correct -- a good reason to check the wire directly instead of
     trusting either "the client surely sent it" or "Maxima is nondeterministic".)
  **The real bug: `assume(a > 0)$`, the FIRST statement of a multi-statement
  cell, is being silently dropped somewhere in the client-side command
  queuing before it ever reaches `Maxima::Write()`.** Confirmed narrowed
  further: instrumenting `EvaluationQueue::RemoveFirst()` to log whenever
  `m_commands.front()` contains `"assume("` -- gated on a plain, cheap
  `wxString::Contains()` check so it fires on essentially none of the many
  calls per run -- caught a live failure where *that log never fired at
  all*, meaning `"assume(a > 0)$"` never even transiently became
  `m_commands.front()`; the drop happens no later than the very first
  `AddTokens()`/`ProduceNextCommand()` peel for that cell (or possibly
  even earlier, in what `cell->GetEditable()->ToString(true)` itself
  returns -- not yet distinguished). **This remains the open question.**
  A follow-up attempt using a genuinely zero-I/O in-memory ring buffer
  (plain array writes in `RemoveFirst()`/`AddTokens()`/
  `ProduceNextCommand()`, dumped only from the one place that's already
  proven zero-cost -- the halt branch) failed to reproduce across 3
  consecutive 150-run batches (450 runs, 0 hits), a real deviation from the
  established ~1-in-20-to-30 baseline -- this is an extraordinarily
  narrow race, and printf/logging-based approaches (even genuinely cheap
  ones) may be fundamentally unable to catch it without perturbing it away;
  a live `gdb` session with conditional breakpoints (no per-hit I/O) is the
  more promising next tool, following the pattern already used successfully
  for the *different* `--exit-on-error` timing bugs elsewhere in this file.
  **The workaround that IS verified and shipped:** recording an auto-answer
  ("p;") for this cell too, mirroring the defensive multi-variant recording
  the *other* "positive or negative?" cell earlier in the same file already
  has -- this makes the test resilient to the halt regardless of the
  underlying cause, verified with 180 back-to-back parallel runs (0
  failures) vs. the ~1-in-20-to-30 rate before it, but **it papers over the
  symptom, not the underlying silent-statement-drop bug**, which is a real
  correctness issue (a side-effecting command a user's worksheet depends on
  can silently never execute) independent of this specific tutorial file.
  See GH #2196 for the ongoing follow-up. Also worth flagging: this bug
  class (a whole statement silently dropped, no error, no visible symptom)
  would be invisible to nearly every other test in this suite -- it was
  only caught here because this one specific cell happens to have an
  observable side effect (whether Maxima needs to ask an interactive
  question) that differs depending on whether the dropped statement ran.
  A cell without such a canary would just silently produce a
  different-but-plausible-looking answer.
  - **This sandbox cannot run `rr` or eBPF uprobes, and gdb hardware
    watchpoints insert but then fail on resume -- confirmed live, not
    assumed.** `rr record` fails immediately (`Unable to open performance
    counter with 'perf_event_open'`): `/sys/bus/event_source/devices/` has
    no `cpu` entry (only `breakpoint`/`msr`/`power`/`software`/`tracepoint`/
    `uprobe`), so there is no hardware PMU exposed to the container at all --
    not a `perf_event_paranoid` permission issue, a missing device. `rr`
    needs that PMU for its retired-conditional-branch counting; there is no
    workaround, this is a hard environment limit. `bpftrace`'s `BEGIN`
    probe fires fine (plain BPF program loading works), but a `uprobe:`
    probe on the built `wxmaxima` binary silently reports "No probes to
    attach" -- uprobe attachment itself is blocked even though the kernel
    lists it as a source. Oddest of all: `gdb`'s hardware watchpoints
    (`watch this->m_commands` on a live, multi-threaded `wxmaxima`) report
    success and show no error at the moment they're set, faking out a quick
    check -- but the FIRST subsequent `continue` fails with "Could not
    insert hardware watchpoint" / "Could not insert hardware breakpoints:
    You may have requested too many hardware breakpoints/watchpoints",
    reproduced 3/3 with a minimal `watch` + `continue` script and 0/3
    failures with the identical `watch` alone (no `continue`) -- so the
    debug registers can be written once but not reprogrammed across the
    process's threads when the kernel actually tries to arm them for
    execution. Software breakpoints (plain `break`/`tbreak`) work
    completely normally, including hitting, `commands` blocks, and
    `continue` across hundreds of hits -- only the *hardware*-assisted
    paths (perf counters, uprobes, debug-register resume) are affected,
    consistent with a sandboxing layer that fakes/no-ops specific
    hardware-facility syscalls rather than a resource exhaustion or a
    generic ptrace restriction (plain ptrace, software breakpoints, and
    even setting-not-resuming a hardware watchpoint all work). **On real
    (non-sandboxed) hardware, `rr record` + `rr replay` is almost
    certainly the right tool for this bug** -- it would let a natural
    reproduction under `rr record` (much lower overhead than gdb
    breakpoints or logging, since it only needs to log nondeterministic
    inputs, not trap on every call) be replayed deterministically
    afterward, with arbitrarily heavy breakpoints/watchpoints during
    *replay* costing nothing towards reproducing the original race. Try
    that first outside this sandbox before repeating any of the above.
  - A gdb software-breakpoint hunt (`EvaluationQueue.cpp:124`, right after
    `AddTokens(GetCell())` on every cell-to-cell advance, logging
    `m_commands[0]` and continuing automatically) was run against
    `commandSequenceIntegrity.wxm` as the most targeted live attempt so
    far, checking every run's advance log for a gap in the expected
    1,3,5,...,299 sequence. See the follow-up note below (or GH #2196
    directly) for whether it caught anything.

- **`lisp_mode` intermittent CI failure -- reproduced, root-caused and
  FIXED (2026-09-18). The fix is two lines in two files and neither of them
  works without the other; the one-line version of it hangs every batch run.
  Read the "fix that does NOT work" bullet before touching either.**
  Distinct from the `m_configCommands`/`RemoveFirst()` prompt-count
  bug documented under "Communication with Maxima"; that one was fixed, and
  this is a different mechanism with the same victim.
  - **Reproduction, which is the genuinely reusable part.** It needs CPU
    contention, not repetition: **0 failures in 60 runs** on an idle box,
    **6 in 180** with 12 concurrent workers on 4 cores (3x oversubscribed).
    Run the real ctest command per worker with its own `DISPLAY` and its own
    `TMPDIR`/`MAXIMA_USERDIR`/`MAXIMA_OBJDIR`/`MAXIMA_TEMPDIR`/
    `XDG_CONFIG_HOME` (copy them out of the generated
    `test/CTestTestfile.cmake`), under `timeout`, keeping the log of any run
    whose exit code is non-zero. Don't conclude "can't reproduce" from an
    unloaded machine -- that is the one condition guaranteed to hide it.
  - **Signature**: exit code 90, always aborting on the `to_lisp();` cell,
    and the `--logtostderr` log shows **two "Sending a new command to
    Maxima." lines back to back with no "Got a new input prompt!" between
    them**, where a passing run strictly alternates the two. `lisp_mode`
    catches it because it exists to detect REPL desync; a worksheet without
    that property would just return a wrong-but-plausible answer.
  - **The race.** Opening the worksheet restarts Maxima -- `OpenFile()` ->
    `StartMaxima()`, which kills the running process and spawns a
    replacement (a *passing* run does this too, so "Maxima processes
    spawned: 2" is normal and not the anomaly). The next idle then reaches
    `wxMaxima.cpp`'s `if (m_evalOnStartup)` branch, which queues the document
    and calls `TriggerEvaluation()` **with no readiness guard at all** --
    unlike its sibling site ~26 lines below (the no-file-to-open path), which
    guards on `m_ready`. When the killed Maxima had already got as far as its
    own first prompt, enough state survives that the first command is
    dispatched into a connection that is being replaced.
    Measured discriminator: "the killed Maxima logged `Received maxima's
    first prompt` *before* `File opened`" held in **8 of 8** failures and in
    **0** passes that failed -- but also in 74 runs that passed, so it is the
    precondition that opens the window, not a guarantee. Under load the
    process being replaced has more wall-clock time to reach its prompt,
    which is why contention raises the rate.
  - **The fix that does NOT work, and why -- don't repeat it.** Adding
    `&& !m_first` to that branch (`m_first` being re-armed by `StartMaxima()`
    on every spawn and cleared by `ReadFirstPrompt()`, so it really does mean
    "the Maxima running *now* has prompted") looks exactly right and
    **hangs every single batch run**: 12 of 12 workers timed out (exit 124)
    on their first attempt. The branch sits inside
    `if (m_updateEvaluationQueueLengthDisplay)`, and that flag is cleared at
    the bottom of the same block and only ever set true again by
    `EvaluationQueueLength()` *when the queue length changes*. Declining to
    queue the document therefore ensures the queue length never changes, so
    the flag is never re-armed, so the idle block never runs again --
    and `ReadFirstPrompt()` doesn't rescue it either, because with nothing
    queued it takes its own "evaluation queue is empty" path instead of
    calling `TriggerEvaluation()`. The one chance to start the document is
    missed and the run sits until the ctest timeout. `m_ready` is no better:
    `ReadPrompt()` sets it *and* clears `m_evalOnStartup`, so gating on it
    would stop the branch ever running for a different reason.
  - **The fix that does work: defer the start instead of dropping it, in two
    places that only work together.** (1) `wxMaxima::OnIdle()` gains an
    `m_evalOnStartup && m_first` case *ahead of* the existing
    `if (m_evalOnStartup)` branch that returns without clearing
    `m_evalOnStartup` -- so the document is still owed a start -- and
    *without* leaving `m_updateEvaluationQueueLengthDisplay` set, so this
    does not turn into an idle spin burning a core for the whole of Maxima's
    startup (`RequestMore()` makes wx deliver idle events back to back with
    no blocking). (2) `MaximaResponseReader::ReadFirstPrompt()` sets
    `m_updateEvaluationQueueLengthDisplay = true` again when
    `m_evalOnStartup` is still set, which is what brings that idle block back
    to life once the replacement Maxima really has prompted. Without (2), (1)
    is exactly the hang above. Deliberately **not** done by starting the
    document from `ReadFirstPrompt()` itself, the other route this entry used
    to suggest: that runs on a socket event, which can be delivered from a
    re-entrant event pump *inside* `OpenFile()` -- i.e. before the document
    tree has been inserted -- so it could queue a half-loaded worksheet.
    Bouncing back through the idle handler is what guarantees `OpenFile()`
    has returned.
  - **Verification, both failure modes, because the natural guard trades one
    for the other and an unloaded run passes either way**: 180 runs under the
    identical 12-worker/4-core load that produced 6 failures before the fix,
    **0 failures**; a plain single `xvfb-run ctest -R '^lisp_mode$'` passes in
    9s with no hang (the bad guard timed out here); and every one of those 180
    logs has its last `Received maxima's first prompt` *before* `Starting
    evaluation of the document`, i.e. the new wait is actually being taken and
    not just getting lucky. Full suite (`ctest -E
    "tutorial|openMacFiles|wxmaxima_version"`) 170/170.
