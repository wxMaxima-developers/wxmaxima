# Project Instructions

This file contains architectural insights, conventions, and operational knowledge to assist AI agents working on the wxMaxima codebase. **Agents are explicitly permitted and encouraged to update this file with new findings that improve context and safety.**

## Build System

Configure once (a Debug build is the default), then build and run without
installing:

```sh
cmake -S . -B build -G Ninja
ninja -C build
./build/wxmaxima-local
```

For a release build (more optimization, log window hidden by default) add
`-DCMAKE_BUILD_TYPE=Release` to the `cmake` line.

Run the tests -- most of them need Maxima installed:

```sh
ctest --test-dir build                        # everything
ctest --test-dir build -R <name-of-the-test>  # one test; names in test/CMakeLists.txt
xvfb-run ctest --test-dir build               # headless, no X server
```

The sanitizer build (ASan + UBSan) is what CI runs on every push. Run it
locally before merging changes to cell lifetime, layout or parsing:

```sh
cmake -S . -B build-asan -G Ninja -DWXM_SANITIZE=address,undefined
ninja -C build-asan
ASAN_OPTIONS=detect_leaks=0:check_initialization_order=1:strict_string_checks=1:suppressions=test/asan_suppressions.txt \
    UBSAN_OPTIONS=print_stacktrace=1 \
    xvfb-run ctest --test-dir build-asan
```

Leak detection stays off (`detect_leaks=0`) because GTK/Pango leak noise drowns
out real findings. **The `suppressions=test/asan_suppressions.txt` is not
optional -- omitting it makes `imageFormat` fail every single time**, with an
`AddressSanitizer: strncpy-param-overlap` inside `wxXPMDecoder::ReadFile`
(confirmed via `md5sum` that the test's XPM fixture is byte-identical to what
CI uses, and confirmed deterministic here across 6 repeated runs -- it is not
the tutorial_10Minutes-style rare race it might look like at first). This is a
real, pre-existing bug inside wxWidgets' own XPM decoder (not wxMaxima's code,
confirmed by the stack trace bottoming out in `libwx_gtk3u_core`), already
known and already suppressed -- see `test/asan_suppressions.txt`'s own
comment and `compile_ubuntu.yml`'s `run_tests` step, which sets exactly this
`ASAN_OPTIONS` string. Running the shorter, suppressions-less command from
memory (as opposed to copying it from here or from the workflow file) will
reliably misreport this pre-existing, already-triaged third-party issue as a
regression in whatever change you're actually testing.

Other useful targets: `ninja -C build Doxygen` builds the source documentation
(note the capital D, and the target only exists when Doxygen is installed), and
`ninja -C build update-locale` refreshes the translation files.

`CheckPo4aVersion.cmake` (included from `info/CMakeLists.txt` and
`locales/manual/CMakeLists.txt`, the only two places that invoke `po4a`)
refuses `po4a` older than 0.70 -- pre-0.70 parses text encodings loosely and
can silently corrupt non-ASCII translated text with no warning of its own,
confirmed directly with Ubuntu 24.04's own `po4a` 0.69 package turning a
German manual paragraph into mangled English on nothing more than a plain
reconfigure. `PO4A` ends up `PO4A-NOTFOUND` (falsy) in that case, same
contract `find_program()` itself has, so existing `if(PO4A)` guards keep
working without extra checks.

- **Sandbox: missing `maxima-index.lisp` and its knock-on ctest failures.**
  In this sandbox's container image, `/etc/dpkg/dpkg.cfg.d/*` has
  `path-exclude=/usr/share/doc/*` (a common image-slimming policy), so every
  package's `/usr/share/doc/*` content is silently dropped on install --
  `apt-get install --reinstall maxima maxima-doc` does not bring it back.
  `maxima`'s own package ships `/usr/share/doc/maxima/info/maxima-index.lisp.gz`
  (confirmed by extracting the real `.deb` with `dpkg-deb -x`), so its absence
  here is this sandbox's doc-stripping, not a missing dependency -- the actual
  wxMaxima `.deb` (`CPACK_DEBIAN_PACKAGE_DEPENDS "maxima, maxima-doc"` in
  `src/CMakeLists.txt`) already hard-`Depends:` on both, and a normal install
  on a normal system is unaffected. Workaround for this sandbox only (not a
  repo change): `apt-get download maxima && dpkg-deb -x maxima_*.deb
  /tmp/x && gunzip -c /tmp/x/usr/share/doc/maxima/info/maxima-index.lisp.gz >
  /usr/share/doc/maxima/info/maxima-index.lisp`. Without it, Maxima logs
  `Warning: SIMPLE-WARNING: Maxima is unable to set up the help system` on
  every startup, and `ctest` targets that use `--exit-on-error`
  (`openMacFiles`, `openMacFiles2`, and most of the `*_cmdline_wxmathml`/
  `tutorial_*`/similar batch tests in `test/CMakeLists.txt`) fail near-instantly
  on that warning alone -- with the workaround applied, those specific two
  tests (`openMacFiles`/`openMacFiles2`) instead *time out* (confirmed to
  reproduce identically on an unmodified `main` checkout in an isolated
  worktree, so it's pre-existing and unrelated to any particular change) --
  not yet root-caused. Don't burn time re-diagnosing either symptom from
  scratch; both are sandbox/pre-existing, not something a code change here
  broke.

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

- **macOS translation files never reaching the app bundle (GH #1711) --
  two independent bugs, neither of which this sandbox (Linux, no
  `.app`/`MACOSX_BUNDLE`/DragNDrop support at all) can actually build or
  verify.** `Dirstructure::LocaleDir()`/`wxFileTranslationsLoader` (see
  `main.cpp`) look specifically under
  `Contents/Resources/locale/<lang>/LC_MESSAGES/wxMaxima.mo` at runtime.
  1. `locales/wxMaxima/CMakeLists.txt`'s `copy_mo_file_${LANG}_for_wxmaxima_local`
     target -- the only thing that populates
     `${CMAKE_BINARY_DIR}/share/locale/<lang>/LC_MESSAGES/wxMaxima.mo` with
     that exact nesting, `ALL`-tagged so it runs on every normal build -- was
     unconditionally skipped `if(NOT APPLE)`, per a comment saying it
     "does not work with Apple XCode." Confirmed live (Linux, but the CMake
     logic itself doesn't depend on the platform): a plain `ninja` in this
     sandbox, having never explicitly invoked `update-locale`, still produces
     a fully populated `build/share/locale/*/LC_MESSAGES/*.mo` from this
     target alone -- so on macOS it was producing nothing, full stop. The
     macOS CI job that actually ships the DMG passes `-G Ninja`, not Xcode
     (only a separate, non-packaging smoke-test job uses Xcode) -- narrowed
     the guard to `if(NOT (APPLE AND CMAKE_GENERATOR STREQUAL "Xcode"))`
     instead of excluding all of Apple.
  2. Separately, `src/CMakeLists.txt`'s macOS bundle resource list tried to
     `file(GLOB ${CMAKE_BINARY_DIR}/locale/*.mo)` into the `RESOURCE` target
     property. Two bugs stacked here too: `file(GLOB)` freezes its result at
     *configure* time, before a single build step has run and generated any
     `.mo` file at all (confirmed empirically the same way as above -- this
     glob's directory doesn't exist yet on a fresh configure); and even if
     the files existed, this pattern is non-recursive and wouldn't match
     their actual `locale/<lang>/LC_MESSAGES/wxMaxima.mo` nesting, and
     CMake's `RESOURCE` property flattens whatever it *does* match directly
     into `Contents/Resources` with no way to reproduce a subdirectory
     structure. Fixed by dropping the glob and instead copying the
     (now-populated, already-correctly-nested) `share/locale` directory into
     `Contents/Resources/locale` via a plain `file(COPY ...)` inside the
     existing `install(CODE ...)` block that already runs `fixup_bundle` --
     the same "has to be a separate step, everything the build produces is
     only guaranteed to exist by then" reasoning that block's own comment
     already gives for deferring `fixup_bundle` itself.
  Verified as much as is possible without a Mac: the CMake configure and a
  full build succeed unaffected on Linux (the `if(NOT (APPLE AND ...))`
  change is a no-op there, `CMAKE_GENERATOR` is never `"Xcode"` outside
  Apple), `share/locale` still populates correctly, and the exact
  `file(COPY ...)` logic (including its `if(EXISTS ...)` guard, which fails
  *silently* rather than breaking the build if this is somehow still wrong)
  was validated standalone via `cmake -P` against a fake
  `share/locale/<lang>/LC_MESSAGES/*.mo` tree, confirming it reproduces the
  nesting correctly. The actual Apple-only code paths themselves remain
  unverified -- if a real macOS build still doesn't get translations, check
  here first before re-deriving any of the above from scratch.

## Architecture & GUI

wxMaxima is a GUI front-end to the Maxima CAS; it talks to a Maxima process over
a local TCP socket.

- **wxAuiManager:** The application uses `wxAuiManager` for its complex layout (sidebars, toolbars, worksheet).
  - **Linux/GTK Timing:** On Linux (especially KDE Plasma with Global Menus), calling `m_manager.Update()` can disrupt the menu bar if it's already attached. This is a known environmental issue in the interaction between wxWidgets, GTK3, and the KDE Global Menu proxy.
    - **Automated Fix:** On systems with wxWidgets <= 3.2 running on KDE, Unity, or with `appmenu-gtk-module` enabled, wxMaxima automatically sets `UBUNTU_MENUPROXY=0` at startup in `main.cpp` to force menus to remain within the window and prevent disappearance.
    - If the menu still disappears, clearing `GTK_MODULES` (e.g., `GTK_MODULES=""`) can also restore local menus.
- **Dockable "Find and Replace" (GH #2249, `Configuration::FindDialogDockable()`):**
  `FindReplaceDialog`/`FindReplacePane` were already split apart (a `wxDialog`
  wrapper around a `wxPanel` holding the actual controls) specifically
  anticipating this feature -- `FindReplacePane` climbs to the top-level
  window and queues its search/replace events there
  (`while(topLevelWindow->GetParent()) ...`), so it already works correctly
  regardless of whether it's embedded in the floating dialog or registered
  directly as an AUI sidebar pane; no changes were needed to
  `FindReplacePane.cpp`'s event-firing logic. `Worksheet::GetActiveFindPane()`
  is the single place that decides which presentation is live right now (the
  dockable pane if `Configuration::FindDialogDockable()` is set, otherwise the
  floating dialog's own pane if one is open) -- every call site that used to
  reach into `m_findDialog` directly (the incremental-search idle task,
  `OnFind`/`OnReplace`/`OnReplaceAll`, the wrapped-search warning dialog's
  parent) goes through it instead. The dockable pane is registered once,
  eagerly, in `wxMaximaFrame`'s constructor (like every other sidebar, so its
  docked position/size persists via the AUI perspective) and is backed by its
  own `wxMaximaFrame::m_findPaneData` member -- it can't reuse
  `wxMaxima::m_findData` because `wxMaximaFrame`'s constructor body (where
  `AddPane()` runs) executes *before* `wxMaxima`'s own members are
  constructed, a plain base-before-derived C++ ordering issue. The two data
  objects don't need to be the same instance: `FindReplacePane` already
  persists its own live flags straight to `wxConfig` on every change, so each
  just seeds itself independently via the new
  `FindReplacePane::FindReplaceData::LoadFromConfig()`. Un-hiding the pane
  from the Ctrl+F handler (`MaximaCommandMenus.cpp`) needs the base class's
  `wxMaximaFrame::ShowPane(int, bool)` explicitly qualified as
  `m_wxMaxima.wxMaximaFrame::ShowPane(...)` -- `wxMaxima` declares its own,
  unrelated `ShowPane(wxCommandEvent&)` (a menu-event handler) which hides
  the *entire* base-class overload set from lookup on `m_wxMaxima.ShowPane(...)`
  per ordinary C++ derived-class member-hiding rules; this exact qualification
  is already the established idiom elsewhere in the same file and in
  `MaximaResponseReader.cpp` for the same reason. Going through the generic
  `ShowPane()`/`IsPaneDisplayed()` (shared by every `EventIDs::menu_pane_*`
  sidebar) is what makes Ctrl+F correctly un-minimize the pane and focus it
  even when it starts out closed/hidden -- confirmed live in Xvfb, this was
  the specific risk the issue itself called out ("does that still work if
  the sidebar is minimized?").
- **Cursors:** The worksheet has 2 types of Cursor: A standard cursor in an EditorCell or a hCaret between two worksheet cells (`m_hCaretPosition`, the horizontal bar that marks a position *between* group cells, used for inserting and for selecting whole cells). Only one cursor is active at a time.
- **Key Classes:**
  - `wxMaxima` (`src/wxMaxima.cpp`): The main application class (subclass of `wxMaximaFrame`). Holds most of the program logic -- Maxima process management, parsing incoming XML, menu and toolbar actions, file I/O.
  - `wxMaximaFrame` (`src/wxMaximaFrame.cpp`): The base frame class handling layout and sidebars (TOC, variables, history, symbols, draw), the toolbars and the central worksheet.
  - `Worksheet` (`src/worksheet/Worksheet.cpp`): The scrollable document view. Owns the cell tree (`m_tree`) and handles drawing, keyboard and mouse input, the cursors, the selection and the evaluation queue.
  - `GroupCell` (`src/cells/GroupCell.cpp`): The top-level container cell that bundles an input `EditorCell` with its output. The worksheet is a linked list of `GroupCell`s.
  - `Cell` (`src/cells/Cell.cpp`): Base class of all maths display cells -- `TextCell`, `FracCell`, `SqrtCell`, `IntCell`, `MatrCell`, `AnimationCell` and friends.
  - `EditorCell`: Handles text and code input, including Markdown-like formatting (bullet lists).
  - `MathParser` (`src/MathParser.cpp`): Parses the MathML-like XML Maxima produces (via `wxMathML.lisp`) into a tree of `Cell` objects.
  - `Maxima` (`src/Maxima.cpp`): Owns the TCP socket to the Maxima process and emits `EVT_MAXIMA` events carrying incoming data.
  - `Variablespane`: Manages the list of defined variables and their values.
  - `AutoComplete`: Handles the autocomplete logic for commands, variables, and files.
- **`wxLogMessage`/`wxLogWarning`/`wxLogError` are NOT reliably visible to the
  user in this app -- don't reach for them when something needs to actually
  be seen.** `main.cpp` installs a `wxLogWindow` with `passToOld=false`
  (both branches of its `#if (DEBUG==1)`), which means every `wxLogXXX` call
  goes *only* to that custom log window and nowhere else -- not to wx's
  stock `wxLogGui` popups, which is what raises the "but wxLogError usually
  shows something" intuition. The log window itself is constructed with
  `show=false` in a normal (non-`DEBUG`) build, i.e. hidden until the user
  explicitly picks View -> Toggle Log Window or passes `--logtostderr`.
  Confirmed live while building the gnuplot-popout-warning feature below: a
  real `wxLogWarning()` call reached the log window's backing store (visible
  once the window was forced to raise) but the window itself never mapped
  on screen on its own, even for a Warning-level message -- a user running
  a normal release build would never see it. When a message genuinely needs
  to reach the user, use `LoggingMessageBox`/`LoggingMessageDialog`
  (`src/dialogs/LoggingMessageDialog.h`) instead: it logs the same way
  `wxLogMessage` does *and* shows a real modal dialog, and it already
  honors `LoggingMessageDialog::IsNonInteractive()` so batch/test runs
  don't block on it. This is already the established pattern (~40 call
  sites across `wxMaxima.cpp`, `MaximaFileIO.cpp`, `MaximaCommandMenus.cpp`,
  `WXMXformat.cpp`, ...) -- `wxLogXXX` alone is for the debug-messages
  sidebar, not for anything the user is expected to act on.
- **Gnuplot "Pop out interactively" now warns about gnuplot errors/warnings
  (GH #1973):** the popout handler (`MaximaCommandMenus.cpp`,
  `popid_popup_gnuplot`) launches a *second*, independent gnuplot process
  alongside the real interactive one, running the identical script with
  `set term unknown` instead of a real terminal so it needs no display and
  exits immediately once the script finishes executing.
  `MaximaProcessManager::OnGnuplotPopoutCheckClose` (`wxEVT_END_PROCESS` for
  `EventIDs::gnuplot_popout_check_id`) reads back its stdout+stderr and, if
  anything survives filtering, shows it via `LoggingMessageBox`. The real
  interactive process (`m_gnuplotProcess`) is deliberately **never**
  `Redirect()`ed: doing so would replace its console's actual stdin/stdout
  with pipes wx owns, silently breaking the "type further gnuplot commands
  into the popped-out console" feature the manual documents (Windows'
  `wgnuplot.exe` specifically) -- since `set term unknown` needs no console
  at all, redirecting *that* one is free of this tradeoff. **Filtering
  gotcha, confirmed against a real gnuplot 6.0, not assumed:** `set term
  unknown` makes gnuplot print `WARNING: Plotting with 'unknown'
  terminal.\nNo output will be generated. Please select a terminal with
  'set terminal'.` to stderr on *every single* `plot`/`replot` statement,
  even for a script with nothing else wrong with it -- these two lines are
  a side effect of the diagnostic's own terminal choice, not a finding
  about the user's script, and must be filtered out (matched by substring,
  not exact string, since gnuplot's exact wording could vary by version) or
  every popout would raise a spurious warning. Verified end-to-end in a
  live Xvfb session with a real Maxima+gnuplot: a `wxdraw2d` with a bad
  `user_preamble` (`set y2tics out` with no y2 data, reproducing the
  original bug report) raises a "Warning" dialog quoting gnuplot's actual
  `"...gnuplot" line NN: warning: y2 axis range undefined or overflow,
  resetting to [0:0]"` message, while the same plot without the bad
  preamble raises nothing -- and the real interactive popout window (a
  separate, still-running, reparented-to-init process once its short-lived
  wx-tracked launcher process exits -- a pre-existing, unrelated forking
  detail of how gnuplot/`--persist` behaves under X11) keeps working
  exactly as before in both cases.
- **The draw list is computed, not stored (2026-08, closes GH #1445):** `Cell`
  used to carry a `mutable CellPtr<Cell> m_nextToDraw` member -- a second
  always-present `CellPtr` on *every* cell, threaded by hand via a virtual
  `SetNextToDraw()` override on each 2D-capable compound cell (`FracCell`,
  `ParenCell`, `SqrtCell`, `AbsCell`, `BoxCell`, `ConjugateCell`, `ListCell`,
  `ExptCell`, `SumCell`, `IntCell`, `LimitCell`, `IntervalCell`, `DiffCell`,
  `FunCell`, `NamedBoxCell`, `LongNumberCell`) whenever `BreakUp()`/`Unbreak()`
  ran. `CellDrawListIterator` (`src/cells/CellIterators.h`) now computes the
  same flattened "line" sequence on the fly instead: it walks `GetNext()` for
  ordinary siblings, and when a cell `IsBrokenIntoLines()` it descends into
  `GetBrokenCellCount()`/`GetBrokenCell()` (an explicit stack in the iterator
  remembers where to resume once a nested expansion is exhausted -- normal
  documents nest only a few levels deep, so this stays empty, with zero
  allocation, for any line containing no broken cell). `GetBrokenCellCount()`/
  `GetBrokenCell()` default to the existing `GetInnerCellCount()`/
  `GetInnerCell()` (the *semantic*-children interface, previously used only
  for `ResetSize_Recursively()`/`CollectWideCells()`/tooltip fallback), which
  turned out to already match the draw sequence exactly for 11 of the 15
  classes above (confirmed by direct comparison against each `BreakUp()`,
  not assumed). **Four classes needed a real, separate override**, because
  their structural inner-cell set and their actual linear draw sequence
  diverge under runtime conditions: `IntCell` (the linear form omits the
  lower/upper limit slots entirely when `HasLimits()`), `SumCell` (shows
  `Base()` -- the bare, unwrapped content -- instead of the `ParenCell`
  wrapper `GetInnerCell()` reports, and conditionally omits the upper-limit
  pieces when `over` is empty), `IntervalCell` and `LimitCell` (both have
  structural slots -- bracket glyphs, the "lim" name label -- that exist only
  for the 2D form and are never part of the linear one). Getting one of
  these four wrong is a real rendering bug, not just a wrong tree-shape for
  an unrelated recursive walk, since there is no longer a separate
  hand-threaded pointer chain to cross-check the sequence against -- treat
  any future change to a class's `GetInnerCellCount()`/`GetInnerCell()` (or
  `GetBrokenCellCount()`/`GetBrokenCell()` override) as a rendering-order
  change and re-verify it against that class's actual `BreakUp()` logic.
  A 2020 attempt at this same removal (branch
  `feature/KubaO/remove-nexttodraw`, never merged) shipped visible
  regressions in exactly this class of nested-breaking scenario (a broken
  fraction inside a broken fraction/paren/diff cell) because it didn't
  account for this divergence; this attempt was verified against it
  directly -- both with a dedicated nested-broken-cell unit test
  (`test/unit_tests/test_CellPtr.cpp`, `SCENARIO("DrawListIterator works")`)
  and by running the real batch tests (`absCells`, `boxCells`, `diffCells`,
  `conjugateCells`, `exptCells`, `fracCells`, `intCells`, `intervals`,
  `limitCells`, `matrixCells`, `parenthesisCells`, `sumCells`) against a real
  Maxima, plus a manual Xvfb+ImageMagick screenshot of
  `diff(abs(f(x)/g(x)),x)` at a narrow width (a broken `diff` containing a
  broken `abs` containing a broken nested fraction, all at once) to visually
  confirm correct rendering -- this sandbox can install `maxima`/`maxima-doc`
  (see the sandbox note under Build System for the doc-stripping workaround)
  and `Xvfb`/`xdotool`/`imagemagick` for exactly this kind of check when a
  change is rendering-sensitive and the automated test suite's XML/structural
  assertions aren't enough on their own. `CellList.cpp`'s `SetNext()`/
  `AppendCell()`/`SpliceInAfter()`/`TearOut()` no longer need any
  draw-list-mirroring bookkeeping, since there's nothing stored to keep in
  sync.

- **`Worksheet::AnonymizeCodeCells()` (GH #1339, Help menu -> "Anonymize Code
  for Bug Report"):** renames every non-builtin variable/function name in the
  selected code cells (whole document if nothing's selected, after a
  confirmation `wxMessageBox`) to a random `anon_...` name, the same
  replacement for every occurrence of a given original name, as a single
  undo step. Telling "a user-defined name" apart from "a name Maxima already
  knows" needs **two** independent checks on each `TS_CODE_VARIABLE`/
  `TS_CODE_FUNCTION` token, not one: `AutoComplete::GetSymbolList()` (Maxima
  builtins plus session-loaded package symbols -- deliberately *not*
  polluted by user-typed worksheet words, which live in a separate
  `m_worksheetWords` map) catches real Maxima functions/variables, but
  `MaximaTokenizer` tokenizes its own hardcoded control-flow keywords
  (`for`/`in`/`then`/`while`/`do`/`thru`/`next`/`step`/`unless`/`from`/`if`/
  `else`/`elseif`/`and`/`or`/`not`/`true`/`false`) with that same
  variable/function style, and only 4 of those 18
  (`and`/`false`/`in`/`true`) also happen to appear in
  `data/builtin_commands.txt` -- confirmed directly by grepping that file.
  A filter using only `GetSymbolList()` would rename `for`/`then`/`do`/...
  themselves and corrupt the Maxima syntax outright. Fixed by exposing the
  tokenizer's private keyword set as a new public static
  `MaximaTokenizer::IsHardcodedKeyword()` and checking both. That accessor
  needed its own fix first: the keyword map was populated lazily inside the
  constructor, so calling it before any `MaximaTokenizer` instance existed
  silently returned false for everything -- fixed by extracting
  `EnsureHardcodedFunctionsInitialized()` and calling it from both the
  constructor and the new accessor.
  `test/unit_tests/test_AnonymizeCodeCells.cpp` pins this with a real
  `Worksheet`/`Configuration` (no live Maxima), calling the narrow,
  synchronous `AutoComplete::LoadBuiltinSymbols()` in its `main()` rather
  than the full `Worksheet::LoadSymbols()` -- the latter also kicks off
  `LoadableFiles_BackgroundTask`'s directory scan for Maxima's share/demo
  folders, which stalled for 70+ seconds in this sandbox and got the test
  process killed by its ctest timeout. A substring check like
  `after.Contains(wxS("f("))` to confirm a renamed function is gone is
  fragile and intermittently flaky (confirmed live, ~1-in-3 failure rate
  over repeated runs): the random 13-character `anon_...` replacement for
  some *other* name can itself end in the letter being searched for, and
  since a real function call always has `(` immediately after its name in
  valid Maxima syntax, the reconstructed text can contain a coincidental
  `...anon_xyzqwrtf(...` match. Use exact per-token comparison via
  `MaximaTokenizer` instead (see that test file's `HasExactToken()` helper).
  The "nothing selected -> confirm whole document" `wxMessageBox` path can't
  be driven or screenshotted reliably in this sandbox's Xvfb (no window
  manager is running, and a GTK modal dialog's window never became visible
  to `import -window <id>`/`-window root` in several attempts, though the
  underlying `wxMessageBox` call is the same well-established idiom used
  elsewhere in this codebase) -- verified instead by exercising the
  already-selected-cells path end-to-end in a live Xvfb session (typed real
  code, selected the group cell via hCaret + Shift+Up, confirmed the
  rendered text changed consistently and a single Ctrl+Z restored it).

### Communication with Maxima

wxMaxima sends Lisp and Maxima commands over the socket; Maxima answers with XML
wrapped in known tags. `Maxima` reads that data on a worker thread and posts
`EVT_MAXIMA` events to the main thread, where `wxMaxima` handles them.

`src/wxMathML.lisp` is compiled into the binary (through CMake's bin2h) and is
what tells Maxima to format its output as MathML-like XML. For development,
`--wxmathml-lisp=<path>` overrides it with an external file, so a change can be
tried without rebuilding.

- **`m_configCommands` (`wxMaxima.cpp`):** the string of startup/config commands
  sent to Maxima on connect (and again whenever settings change while it's
  running). **Every entry in it MUST be a `:lisp-quiet (...)` directive --
  never a plain Maxima statement ending in `$`/`;`.** `m_configCommands` is
  sent bundled immediately ahead of the evaluation queue's next real per-cell
  command (`MaximaEvaluator::TriggerEvaluation()`), and
  `EvaluationQueue::RemoveFirst()` has no way to tell "a prompt answering a
  config command" from "a prompt answering a real queued cell" -- it advances
  the queue by one cell for *every* main `(%iN)` prompt it sees. A plain
  statement here makes Maxima print its own extra prompt, and
  `RemoveFirst()` then silently drops one real (never-sent) queued cell for
  each such prompt -- confirmed live with `tcpdump` on the raw
  wxMaxima<->Maxima socket to drop the whole evaluation queue (21 cells to 0)
  in one shot, root-causing an intermittent hang/failure in
  `automatic_test_files/lisp_mode.wxm`. This bit `wxdirs`, the Maxima struct
  exposing wxMaxima's own paths: it has to be built via genuine Maxima syntax
  (`defstruct`, `wxdirs@field: "value"`, ...) since Maxima's `defstruct` does
  not evaluate named-field initializers to the field's value -- it silently
  stores the unevaluated `field = value` equation instead (confirmed against
  a real Maxima 5.46), and `new(wxdirs(field=value, ...))` therefore doesn't
  work either. The fix: build the Maxima-syntax statement (still going
  through `wxMaxima::EscapeForLisp()` per value -- despite the name it is
  exactly the escaping Maxima string literals need too, for `"`/`\` in any
  filesystem path), then wrap the *whole* statement text as the argument to
  `:lisp-quiet (with-input-from-string (wxst "...") (meval (caddr (mread
  wxst 0))))` -- reads and evaluates it from Lisp with no separate prompt of
  its own, same as every other `m_configCommands` entry. The statement text
  needs `EscapeForLisp()` applied a *second* time at that point, since it is
  now itself the content of a Lisp string literal (each individual field
  value was already escaped once, for the Maxima string literal it sits
  inside).
  - **Debugging technique note:** when in doubt about what actually crossed
    the wxMaxima<->Maxima socket (vs. what a log line *claims* was sent),
    `wxLogMessage()`-based tracing inside wxMaxima can itself be misleading --
    `Maxima::Write()` only enqueues to `m_outputQueue`; the worker thread
    flushes it to the socket separately and asynchronously, so a logged "sent"
    call is not proof the bytes ever left the process (e.g. if the app exits
    first). `tcpdump -i lo -w file.pcap 'tcp portrange 49000-49999'` plus a
    small manual pcap parser (this sandbox's Python has no working `scapy` --
    `cryptography`'s Rust backend panics on import here -- so parse the
    classic pcap format directly: 24-byte global header, then repeated
    16-byte-record-header + packet frames; skip the 14-byte Ethernet header,
    read the IP header's IHL for its length, then the TCP header's data
    offset for its length) gives ground truth immune to any wxMaxima-internal
    misattribution. Also: `wxLogMessage()` is not safe to call from
    `Maxima::WorkerThread()` (a non-GUI thread) -- it crashed with a
    `wxArgNormalizer` format-specifier assertion the first time it was tried
    there for debugging (triggered by `%zu` specifically; the crash went away
    switching to `%lu` + an explicit `(unsigned long)` cast, but the
    thread-safety of logging from that thread at all remains unverified --
    treat any such tracing as temporary/debug-only, never ship it).

- **ASCII-art 2D display (`set_display('ascii)`) and `*alt-display2d*`:**
  when `$display2d` is on, Maxima's evaluator checks the special variable
  `*alt-display2d*` before printing a result: if it's a function symbol,
  that function is called *instead of* Maxima's own stock printer (this is
  how `mydispla` in `wxMathML.lisp` produces the normal `<mth>`/XML output);
  if it's `nil`, Maxima falls through to its own built-in ASCII-art printer,
  which pads a result's lines with literal spaces so a multi-line fraction/
  matrix/etc. lines up correctly under the `(%oN)` label -- but that padding
  assumes every line, including the label, ends up rendered in one uniform
  monospace font. `wxMathML.lisp`'s `wx-ascii-displa` wraps that stock
  printer in `<wxxml-asciimath>`/`</wxxml-asciimath>` markers *without*
  reimplementing it: it dynamically rebinds `*alt-display2d*` to `nil` for
  just the duration of `(displa x)`, which re-enters Maxima's own dispatch
  and this time takes the stock ASCII path (confirmed live: this is
  correctly reentrant-safe through Maxima's own recursive sub-expression
  `displa` calls, e.g. matrix rows, since they run inside the same dynamic
  extent). `Maxima::ProcessData()` only fires the corresponding
  `XML_ASCIIMATH` event once it has seen the *complete* matching closing
  tag, so `MaximaResponseReader::ReadAsciiMath()` always receives one whole
  block in one piece and can render all of it in one uniform style --
  before this, `ReadMiscText()` guessed a chunk's style from whether it
  happened to start with `"(%"`, and since chunks are split by socket/timer
  batching (not by where Maxima's actual output boundaries are), a block's
  label line could land in a separate batch than its neighbors and get
  misclassified into a different (proportional) font, breaking the
  alignment Maxima's padding assumed -- root-caused with a raw
  `:lisp-quiet (with-input-from-string ...)` / socket-level reproduction
  (see the debugging technique note above) before the fix, not guessed.

### File Formats

- **`.wxmx`** -- a ZIP archive holding `content.xml` (the MathML-like XML) plus
  the embedded images. The format version lives in `src/WXMXformat.h`.
- **`.wxm`** -- the plain-text format, read by `Format::ParseWXMFile()`.

### Translations (`locales/`)

- **One combined `.po` per language, in `locales/wxMaxima/`, is the file a
  translator edits.** It covers both wxMaxima's own UI strings
  (`xgettext`-extracted from `src/**/*.cpp`/`*.h`) and the manual's prose
  (`po4a`-extracted from `info/wxmaxima.md`). `locales/wxMaxima/wxMaxima.pot`,
  the template both `msgmerge` and Crowdin work against, is regenerated as
  the union of a fresh source scan and `locales/manual/wxmaxima.md.pot`
  (`msgcat --use-first`, preferring `wxMaxima.pot`'s own header) by the
  `update-locale` CMake target.
- **The xgettext source list is an explicit glob, not a recursive one, and a
  file it misses loses its strings silently.** `POT_SOURCE_FILES`/
  `POT_SOURCE_FILES_REL` in `locales/wxMaxima/CMakeLists.txt` list
  `src/*.cpp;src/*.h;src/*/*.cpp;src/*/*.h` -- exactly two levels. It was
  flat `src/*` until `03b16f2d8`, so every string under `src/cells`,
  `src/wizards` and `src/graphical_io` was missing from the POT from
  2020-08-05, and `src/sidebars`/`src/dialogs` from 2024-01. Cost: ~7000
  translations across 21 languages, restored from git history only in
  2026-08. **Nothing warns about this**: xgettext is happy with a short file
  list, and the POT-drift check in CI regenerates the POT and diffs it, so a
  broken glob truncates both sides identically and the check passes. Adding
  a `src/<a>/<b>/` nesting level would break it again -- `check-pot-coverage`
  (a `ctest`, needs neither a build nor gettext) now fails the build if any
  source file sits deeper than the glob reaches, or if a file containing a
  `_("...")` marker is unreferenced by the committed POT.
- **A Crowdin sync can silently wipe existing translations if it's built from
  a stale base -- confirmed twice now (2026-08-05 and 2026-08-10), both times
  around the same subdir-glob restoration work above, and both times a plain
  git merge, not a Crowdin misconfiguration found so far.** The second
  incident: PR #2241 ("Restore 7083 translations the flat `src/*` gettext
  glob had dropped") merged at 19:59:43; Crowdin's own `l10n_main2` branch
  (a long-lived branch it force-pushes to repeatedly, see PRs #2187/#2199/
  #2239/#2244) had already branched off main at 19:55:40 -- 4 minutes
  *before* #2241 -- and didn't re-sync before its own PR #2244 merged 30
  minutes later. Confirmed directly from the commit graph, not inferred:
  `git merge-base <l10n_main2 tip> <main-before-#2241>` equals that
  pre-#2241 commit exactly, i.e. Crowdin's new per-language commits were
  still parented on the stale base. The result once merged: a plain 3-way
  git merge doesn't understand PO-file semantics, so wherever Crowdin's
  diff and the restore's diff touched entries in the same file without a
  textual line conflict, whichever side's hunk landed could silently win --
  in this case, largely Crowdin's older, translation-poorer version. Net
  effect measured with `polib` (msgid+msgctxt keyed diff, not
  `msgfmt --statistics` counts alone, since those can't tell "a translation
  reverted to worse text" from "line-wrapping changed"): 464 translations
  across 16 languages went from non-empty to empty, and *zero* went the
  other way -- a real regression, not translator churn. Recovered with a
  surgical text-level patch (locate each entry's own line range via
  `polib`'s `.linenum` in both the pre-regression and current file
  independently, splice only the `msgstr` block, leave everything else
  byte-identical) -- reusing `polib`'s own serializer to resave the whole
  file was tried first and rejected: it reformats every line's wrapping,
  turning a 464-line fix into a ~200KB diff across 16 files that would have
  buried the actual change completely. If this happens a third time, the
  fix is on the Crowdin project side (a webhook trigger that also
  re-syncs sources immediately before generating its PR, not just before
  the languages it already had), not on wxMaxima's own git handling -- ask
  Crowdin support directly, since this session cannot access Crowdin's
  own dashboard/API to confirm the exact trigger without credentials.
  **It did happen a third time (2026-08-12, PR #2257 from `l10n_main2`):**
  same shape exactly, `merge-base` landing before the 464-translation
  restoration commit (`3cf4f9c97`) this time instead of before #2241 --
  same 464 entries, same 16 languages, zero gained, caught before merging
  by diffing the PR branch against `origin/main` with the same
  msgid+msgctxt-keyed `polib` technique, this time *before* merging rather
  than after. Crowdin claimed to sync hourly regardless. The maintainer
  switched Crowdin to a fresh `l10n_main2` -> `l10n_main3` branch as a
  workaround, which "immediately started syncing" -- consistent with
  Crowdin's export being tied to when *that specific branch* was created/
  configured, not to actual polling frequency against upstream's current
  state; still unconfirmed without Crowdin dashboard access, so don't
  treat that as the root cause, just an observation. Added a permanent
  safety net for this: `locales/wxMaxima/check_translations_not_wiped.py`
  (wired into `compile_ubuntu.yml` as its own fast, standalone
  `check_translations_not_wiped` job, no build dependencies needed) does
  this exact msgid+msgctxt-keyed comparison against `origin/main` on every
  push and fails loudly if any translation would go from non-empty to
  empty -- catching this class of regression in CI before a human has to
  notice and diff it by hand a fourth time. It only catches "translated
  text disappeared entirely," not e.g. "translation is now provably
  worse," and can in principle false-positive if a legitimate, unrelated
  PR both changes a translatable string's English source text *and*
  commits a regenerated `.po` under the old convention in the same push
  (the old msgid's entry vanishing under a genuine rename looks identical
  to it being wiped) -- rare in practice, since this repo's convention is
  to not casually commit `update-locale` drift alongside unrelated changes
  (see the "committing a `make update-locale` run's output" entry below),
  but worth knowing if this check ever fires on something that turns out
  to be legitimate.
- **`test/check-pot-coverage.cmake` needs `cmake_policy(SET CMP0057 NEW)`
  explicitly.** It runs in script mode (`cmake -P`), which does not inherit
  the top-level `CMakeLists.txt`'s policy settings -- without this line,
  `if(NOT f IN_LIST covered)` hard-errors with "Unknown arguments
  specified" on every invocation, on any CMake version, confirmed against a
  clean `main` checkout (not something introduced by unrelated local
  changes). `IN_LIST` needs CMP0057 set to `NEW` to be recognized as an
  operator at all in `if()`; the default/OLD behavior predates that
  operator's existence.
- **Don't drop `--previous` from `msgmerge`.** It is what keeps the
  `#| msgid` comment recording what a fuzzy entry used to say, which is how a
  translator works out *why* something went fuzzy (`00ba34121`). A plain
  `msgmerge` discards those comments wholesale and gives no hint it did --
  212 entries' worth in `zh_CN.po` alone, found only by counting them before
  and after.
- **`po4a` must never be pointed at `locales/wxMaxima/<lang>.po` directly.**
  It looks like the obvious way to keep the manual's translations inside the
  combined file (`po4a.cfg`'s `$lang:` path *was* set to
  `locales/wxMaxima/$lang.po` at one point), but `po4a` doesn't treat a `.po`
  file as something to add/update entries in - it treats it as *its own*,
  and **rewrites it wholesale to contain only the entries it itself
  extracted from `info/wxmaxima.md`, silently discarding everything else**.
  Confirmed live: a language with 1000 translated UI strings and 69
  translated manual strings dropped to 69 (the UI strings gone) after one
  `po4a` run against the combined file - and this shipped merged to `main`
  before being caught. `po4a` therefore keeps writing its own
  `locales/manual/<lang>.po` (`po4a.cfg`'s `$lang:` path), exactly as before
  the two catalogs were combined; `locales/wxMaxima/CMakeLists.txt`'s
  `${LANG}_po` target runs `merge_manual_po.cmake` (`msgcat --use-first`) to
  fold `locales/manual/<lang>.po` into `locales/wxMaxima/<lang>.po` *before*
  `msgmerge`, every time - that script is the only place allowed to write
  manual content into the combined file. `locales/manual/*.po` only exists
  for the languages that actually have a manual translation (not the full
  `locales/wxMaxima/*.po` language list) - `info/CMakeLists.txt` expects a
  matching `info/wxmaxima.<lang>.md` to already exist for every language
  `po4a.cfg`'s language list names, and doesn't create a fresh empty one, so
  don't widen that language list to languages that have no manual
  translation yet without also handling that.
- **`po4a.cfg`'s manual `[type: text]` line needs `opt:"-o markdown"`
  explicitly** - `Locale::Po4a::Text`'s own default for that option is `1`,
  but that default does not take effect through `[type: text]`'s normal
  invocation; confirmed live by extracting the same heading with and without
  an explicit `-o markdown`. Without it, every markdown structural element
  (`##` headings, list items, ...) is extracted as generic wrapped "Plain
  text" instead of being recognized as its own no-wrap markdown construct,
  which is what caused #2047: a translated heading long enough to wrap got a
  literal newline inserted mid-heading when `po4a` wrote it back out,
  turning the second half into a normal paragraph in the rendered manual.
  The tempting broader fix, `opt:"-o neverwrap"` (disables wrapping
  entirely), is a trap: it doesn't just change *output* wrapping, it changes
  how `po4a` *segments source paragraphs into msgids* (each source line
  becomes a literal embedded `\n` in the msgid instead of the paragraph
  being one reflowed string) - confirmed live it turns 293 cleanly-matched
  German translations into 2 clean matches + 328 fuzzy, i.e. it invalidates
  the translation of nearly every multi-line paragraph in the whole manual,
  for a bug that's specifically about headings. `-o markdown` alone fixes
  the reported bug (headings/lists become their own no-wrap entries) with a
  much smaller, semantically-justified cost: only headings, list items and
  fenced code blocks need re-confirming as fuzzy (e.g. 293 clean -> 183
  clean + 156 fuzzy for German - about 89 of those are headings whose old
  msgstr still has the now-redundant leading `##` and its space baked in,
  since `po4a`
  auto-prepends it from the `Title ##` type instead of storing it in the
  translated text; the rest are fenced-code-block delimiters `po4a` now
  reconstructs itself instead of storing literally, plus a handful of
  entries that were already fuzzy for unrelated reasons - real source-text
  edits, not a `markdown` side effect), not full paragraphs. **The old
  translation text is not deleted** (`.po` keeps the fuzzy msgstr plus the
  previous msgid in a `#|` comment) **but it stops appearing in the
  generated manual** until a translator re-confirms it - `po4a-translate`
  skips fuzzy entries by default the same way `msgfmt` does for a compiled
  `.mo`, falling back to the untranslated English source. Don't describe
  this fix as lossless to a translator without that caveat: a previously
  fully-translated heading really does render in English again in
  `info/wxmaxima.<lang>.md` until someone reviews the (mostly mechanical:
  strip the leading `#+` and the space after it) fuzzy diff. Fixing the wrapping bug and keeping
  every translation rendering are in tension - there's no `po4a` option that
  gets both, since the whole point of `-o markdown` is to change what a
  heading's msgid *is*.
- **`locales/wxMaxima/CMakeLists.txt`'s `${LANG}_po` target needs
  `wxMaxima.pot` in its own `DEPENDS`, not just as a plain path string
  inside a `COMMAND` argument.** `add_custom_command(OUTPUT wxMaxima.pot
  ...)` only creates a file-level build rule; a *different* custom command
  (or target) that merely references that output path in a shell argument
  gets no ordering guarantee from it. Confirmed live: after fixing the
  manual's extraction to `-o markdown`, `make update-locale` kept producing
  stale results (matching the pre-fix fuzzy/translated counts exactly)
  because `wxMaxima.pot` itself hadn't been rebuilt - `${LANG}_po`'s
  `PRE_BUILD` command still consumed yesterday's `wxMaxima.pot` on disk.
  Fixed by declaring `DEPENDS ${LANG}.po wxMaxima.pot` on the
  `add_custom_target(${LANG}_po ...)` line.
- **Committing a `make update-locale` run's output means committing
  *every* language's drift against the current C++ source, not just the
  fix you're testing.** Running it live in this sandbox (to verify the two
  bugs above) also picked up ~1100 real UI strings that exist in
  `src/**/*.cpp`/`*.h` today but were missing from every committed
  `locales/wxMaxima/*.po` and `wxMaxima.pot` (confirmed genuine, not a
  sandbox artifact: `grep`-verified several, e.g. `Configuration.cpp`'s
  `_("  Font cache hits: %ld")`, actually exist in the source at `HEAD` -
  `xgettext` just hadn't been re-run against the source in a while before
  `wxMaxima.pot` was last committed). That's real, legitimate drift, but
  it's a separate concern from a `po4a`-pipeline bug fix - bundling ~1100
  new untranslated strings across 24 languages into a bugfix commit buries
  the actual fix and forces reviewers (translators included) to wade
  through unrelated noise. After verifying a `po4a`/CMake fix works
  end-to-end in the build directory, `git checkout --
  locales/wxMaxima/*.po locales/wxMaxima/wxMaxima.pot` to drop that
  incidental drift back to the committed state before committing, keeping
  only the actual pipeline files (`po4a.cfg.in`, the `CMakeLists.txt`s,
  `merge_manual_po.cmake`) plus whatever's scoped to the manual itself
  (`locales/manual/*.po`, `locales/manual/wxmaxima.md.pot`,
  `info/wxmaxima.<lang>.md`). The UI-string staleness is a legitimate
  follow-up `make update-locale` run of its own, on its own commit.
- **Don't run this sandbox's stock `po4a` (0.69) against real translated
  content.** See `CheckPo4aVersion.cmake`'s corruption warning above --
  verifying a change to the translation *pipeline*
  (`po4a.cfg.in`/`merge_manual_po.cmake`/the CMake wiring) is fine without
  running `po4a` itself, but regenerating actual `.po`/`.md` output needs a
  real `po4a` >= 0.70. It wasn't reachable via any of apt (only has 0.69),
  Debian's package archive, or po4a's own GitHub releases (both blocked by
  this sandbox's proxy) - but a source tarball of a current release, fetched
  outside the sandbox and handed to the agent as a file, runs perfectly well
  unpacked with no install step beyond `PERL5LIB=<unpacked-dir>/lib`
  pointing at its `Locale::Po4a::*` modules; the `po4a`/`po4a-updatepo`/
  `po4a-translate`/etc. scripts at the tarball's top level need nothing else
  to work standalone, e.g. `PERL5LIB=lib ./po4a --version`. Regenerating
  translated content live and diffing it (`msgfmt --statistics` before/after
  per language, matching translated-message counts) is how both bugs above
  were actually caught, not guessed from reading `Locale::Po4a::Text`'s
  source.
- **`git clean -fd` after `git checkout --` on a directory wipes untracked
  files in it too, including ones you meant to keep** (e.g. a new
  `locales/manual/<lang>.po` restored from a scratch copy, or a brand new
  `.cmake` helper script that was never committed yet) - it doesn't
  distinguish "test-run droppings" from "uncommitted new work" by intent,
  only by whether `git add` has seen the path yet. Prefer reverting only the
  specific files that are actually wrong (or committing work-in-progress to
  a scratch commit first) over a blanket `git checkout -- <dir> && git clean
  -fd <dir>` once a directory has a mix of both kinds of changes in it.

## Conventions & Standards

- **Git Environment:** Note that running `git diff` might launch the visual diff tool `meld` instead of outputting to the terminal. Always use `git diff --no-ext-diff` if you need terminal output.
- **String Literals & Translations:** Use the `wxS()` macro for all string literals and `_()` for user-facing translatable strings.
- **Logging:** Use `wxLogMessage()` for debugging; messages are visible in **View -> Toggle Log Window** or by using the option `--logtostderr`.
- **Asynchronous Sidebars & Safety:** Sidebars (TOC, Variables Pane) update asynchronously. Always validate `GroupCell` pointers (using `m_tree->Contains()`) before use.
- **Long-Lived Cell References:** Anything that keeps a reference to a `Cell` or `GroupCell` beyond the current call -- undo/redo actions, the evaluation queue, the selection, the sidebars, a cached "last clicked" cell -- MUST hold it as a `CellPtr<...>`, never as a raw pointer. `CellPtr` derives from `Observed` and nulls itself when the cell is destroyed, so a stale reference reads as `nullptr` instead of dangling; null-check it when consuming it, because the cell may have died since it was stored. A raw `Cell *` is fine only for the duration of a single function or event.
- **Cell UUIDs & Navigation:** Cells have unique `m_uuid`. Filenames support `#UUID` fragments.
- **Forward Compatibility:** `ToXML()` implementations MUST call `GetXMLFlags()` and include its output in the opening tag to preserve unknown attributes.
- **Serialization Tags:** Some cells use shortened tags (e.g., `LimitCell` uses `<lm>`). Verify in `MathParser.cpp` before modifying.
- **Gnuplot Probe:** MUST be done asynchronously (e.g., `wxEXEC_ASYNC`). Synchronous execution blocks the UI and can disrupt the Linux global menu system.
- **Variable Escaping:** Use `Maxima::EscapeVarnameForMaxima` for characters like `,`, `°`, and special symbols. A digit at the *start* of a variable name must be escaped (e.g., `\1a`).
- **Maxima Restart (Windows):** Restarting requires a manual reset of the network client (`m_client.reset()`) and streams in `KillMaxima` (which lives in `MaximaProcessManager`, not in `wxMaxima` any more) to avoid socket state errors.
- **Worksheet Search Logic:** Traverse in visual order: Prompt → Editor → Output (Forward) or Output → Editor → Prompt (Reverse). Resume from current caret position.
- **Layout Timeout:** Complex output can trigger a timeout (configurable in Options), replacing slow-to-render cells with a warning.
- **C++ Standard:** The project uses **C++20**. To support users on older operating systems (like Debian-oldstable or RHEL), wxMaxima aims to stay approximately 10 years behind the current C++ standard.
- **wxWidgets Version:** Maintain compatibility with wxWidgets 3.0.5 where possible. Avoid features only available in 3.1+ (e.g., use `MakeAbsolute()` + `GetFullPath()` instead of `GetAbsolutePath()`).
- **Sizer Flags Are Different Enum Types:** `wxDirection` (`wxLEFT`/`wxRIGHT`/`wxALL`/...), `wxAlignment` (`wxALIGN_*`) and `wxStretch` (`wxEXPAND`/...) are three distinct unscoped enums; OR'ing two of them directly (e.g. `wxALIGN_CENTER_VERTICAL | wxALL`) is deprecated in C++20 and GCC warns `-Wdeprecated-enum-enum-conversion`. Fix by casting the *first* operand of the OR-chain to `int` (e.g. `static_cast<int>(wxALIGN_CENTER_VERTICAL) | wxALL`) -- since `|` is left-associative, this makes every subsequent operation `int | enum`, which is unambiguous and unwarned, without needing to touch the rest of the chain. Only the leftmost token needs the cast, however many differently-typed flags follow.
- **`[[maybe_unused]]` on data members and GCC < 12:** GCC before version 12 doesn't support `[[maybe_unused]]` on non-static data members at all and warns `'maybe_unused' attribute ignored [-Wattributes]` regardless of whether the member is actually used (reproduced directly against `g++-11`; fixed by `g++-12`). Since the attribute is still needed for Clang (`-Wunused-private-field`), don't just delete it -- wrap the declaration in `#if defined(__GNUC__) && !defined(__clang__) && __GNUC__ < 12` / `#pragma GCC diagnostic push/ignored "-Wattributes"` ... `#pragma GCC diagnostic pop` / `#endif` (see `SvgBitmap.h`, `wxMathml.h`, `graphical_io/Printout.h`).
- **CI Warnings Live On the Non-`-Werror` Jobs:** `compile_latest_and_test` and `compile_without_webview` (Ubuntu) build with `-Werror`, so they can't show warnings by construction -- check `compile_2204` (Ubuntu 22.04, plain `-Wall -Wextra`, GCC 11) for real warnings that survive to a release build. Don't assume that job's warning list is exhaustive, though: e.g. the `[[maybe_unused]]`-on-a-data-member GCC<12 warning above showed up for `Printout.h` in one such log but not for the identical pattern in `SvgBitmap.h`/`wxMathml.h` in the same run, for reasons that weren't tracked down (not precompiled headers -- `WXM_ENABLE_PRECOMPILED_HEADERS` defaults `OFF`) -- a clean local build with `g++-11 -Wall -Wextra` is the more reliable check for this specific class of warning.
- **`Cell` Bitfields Use C++20 Default Member Initializers, Not `InitBitFields_ClassName()`:** Every per-class flag bit-field (`Cell`, `EditorCell`, `GroupCell`, `TextCell`, `MatrCell`, and the rest of `src/cells/`) declares its default inline, e.g. `bool m_foo : 1 = false;`. The older pattern -- an `InitBitFields_ClassName()` method called from the constructor body, with each field tagged `/* InitBitFields_ClassName */` -- predated C++20 support for bit-field default member initializers and has been fully removed (2026-08); don't reintroduce it for new flags. Classes with zero bit-fields of their own no longer carry an empty stub either. Before folding an existing full-size `bool m_foo;` into a class's bitfield, check (1) nothing takes its address (`&m_foo` doesn't work on a bit-field member) and (2) it's only touched from the GUI thread (no cross-thread `bool` atomicity/tearing expectations) -- worksheet cells are not thread-shared, but double-check call sites rather than assuming. **Declaration order matters more than usual here**: C++ initializes members in declaration order, not constructor-init-list order, so a bit-field read by a *later*-declared member's own initializer (e.g. `IntervalCell::m_leftBracketOpensLeft`/`m_rightBracketOpensRight`, read by the `m_openBracket`/`m_closeBracket` initializers) must stay declared *before* those members -- relocating it next to an unrelated bitfield group to save a byte is undefined behavior (reading the bit-field before it's initialized), not just a style choice, caught before it shipped by tracing the actual initialization order rather than trusting the mem-initializer-list order. When a field can't move, bit-fielding it in place still works: two adjacent `: 1` declarations pack into a shared byte regardless of position.
- **Tab Characters in `EditorCell`:** A `'\t'` is a real, single character in `m_text` (see `EditorCell::NormalizeLineEndings()`, which replaced the old `TabExpand()` that irreversibly rewrote every tab to 1-4 spaces on input/paste/load). It is expanded to the next 4-column tab stop -- one column being the width of a space glyph in the current font -- only where text becomes pixels, via `EditorCell::NextTabStop(startX)`/`MeasureTextWidth(startX, text)`. Tab width is **position-dependent**, the one thing `GetTextExtent()`/`GetTextSize()` cannot compute on their own (unlike every other character), so it can never be cached the way `StyledText::SetWidth()` caches other tokens' widths. `MaximaTokenizer` guarantees a tab is always its own isolated, single-character token (never merged into a space run, mirroring how a newline is already its own token) -- this is *load-bearing*: every `m_styledText`-based site (`Draw()`, `Recalculate()`, `GetLineWidth()`, `SelectPointText()`'s code-cell branch, `StyleTextCode()`) only needs a `text == wxS("\t")` equality check as a result, never substring splitting. Prose/text cells don't go through `MaximaTokenizer` at all, so `EditorCell::StyleTextTexts()` uses its own splitter, `PushTextLine()`, to get the same isolation guarantee for a tab embedded in an otherwise plain line of text. Sites that measure a raw `m_text` substring instead of a single token (`MarkSelection()`, `MixedDirectionOffset()`, `StyleTextTexts()`'s wrap check) go through `MeasureTextWidth()` instead, which splits on `'\t'` internally since a substring can still have one embedded anywhere. Left/Right arrow and Delete need **no special-casing** for tabs -- they already move/delete exactly one `m_text` character, which is now correct automatically. The plain `WXK_BACK` case's old "gobble up to 4 trailing spaces" shim was a workaround for the old space-expanded-tab world and is gone; a real tab deletes in one plain single-character backspace like anything else.

- **`TextCell::ToTeX()`'s `TS_SPECIAL_CONSTANT` branch is a hardcoded
  allowlist with a silent fall-through, not a general style handler
  (GH #972):** `<s>` in wxMathML.lisp is used for `%pi`/`%i`/`%e`/`inf`/
  `minf` and, separately, for the "d" of an integral's "dx"/"d\theta"/...
  (`wxxml-int`) -- all five constants get an explicit `if/else if`, but
  "d" didn't, so it fell through to the branch's final `else return text;`
  as a bare, unstyled character instead of ever reaching the later
  `\ensuremath{\mathrm{...}}` wrapping code that runs for `TS_VARIABLE`/
  `TS_GREEK_CONSTANT`/`TS_SPECIAL_CONSTANT` further down the function --
  that later code is dead for every `TS_SPECIAL_CONSTANT` value, since the
  early branch always returns first. Fixed by adding an explicit `d` case
  returning `\mathrm{d}` (no `\ensuremath{}` needed: it's only ever emitted
  already inside `IntCell::ToTeX()`'s math-mode string, which also already
  supplies the separating `\,` and the space ahead of it, so don't duplicate
  that here).
  Adding a new special case to this list resurfaced a second, easy-to-miss
  coupling: `TextCell::ToTeX()`'s own multiplication-dot logic (for e.g. the
  denominator of `d/dt` or a `dx*dy`-style differential product) identifies
  "the previous cell was that same 'd'" by comparing
  `GetPrevious()->ToTeX() == wxS("d")` -- once "d" stopped returning the
  literal string `"d"`, this comparison went permanently false. Any
  `TS_SPECIAL_CONSTANT` case whose `ToTeX()` output no longer equals its raw
  text needs same-file call sites recompared with `ToString()` (returns the
  untransformed `m_text`) instead of `ToTeX()`, not just the one place a new
  case is added.

- **`TreeUndoAction`'s discriminant model, and adding a fourth action kind
  (GH #266, fold/unfold undo):** `TreeUndoAction` (`src/TreeUndoAction.h`) is
  a single, non-polymorphic struct, not an `Action`/`Undo()`/`Redo()` class
  hierarchy -- `Worksheet::TreeUndo()` (`Worksheet.cpp`) tells apart the
  three original action kinds (text change, cell insertion, cell deletion)
  by which of `m_newCellsEnd`/`m_oldCells`/neither is set, not by a type
  tag. Adding fold/unfold as a fourth kind followed the same style rather
  than introducing polymorphism for a fourth fixed case: a `std::optional<
  FoldDirection>` field (`FoldDirection::Folded`/`Unfolded`), checked in
  `TreeUndo()`'s dispatch *before* the existing `m_oldCells`-vs-text-change
  fallback (a `std::nullopt` field, like the others, defaults via the
  member's own default constructor -- no explicit initializer needed in the
  three original constructors). Undoing a fold/unfold just applies the
  opposite direction to the same cell (`m_start`), which is naturally
  reversible/re-doable through the same generic replay loop the other three
  kinds already use (`Worksheet::TreeUndo()`'s do-while + swapped-stacks
  trick already makes redo "undo, but backwards" for free).
  **Two things worth getting right if you touch this again:**
  1. `GroupCell::Fold()`/`Unfold()` (`GroupCell.cpp`) are the low-level
     primitives (`CellList::TearOut`/`SpliceInAfter`, same as `DeleteRegion`/
     `InsertGroupCells` use) and do **not** themselves record undo -- only
     `WorksheetDocument::Fold()`/`Unfold()`/`ToggleFold()`/`FoldAll()`/
     `UnfoldAll()` do, since only `WorksheetDocument` owns the
     `TreeUndoManager`. Anywhere that needs a fold/unfold NOT to be
     independently undoable (the automatic auto-unfold in `RevealHidden()`,
     and the "make room" auto-unfold in `Worksheet.cpp`'s new-cell-insertion
     logic when the h-caret sits inside a folded ancestor) must keep calling
     the raw `GroupCell::Fold()`/`Unfold()` directly, not the
     `WorksheetDocument`-level wrappers, or it'll silently start occupying
     an undo slot it shouldn't.
  2. `TreeUndoManager::AppendAction()`'s ordering is easy to get backwards:
     since actions are pushed with `emplace_front` (newest at the front),
     marking an entry's `m_partOfAtomicAction = true` means "when the entry
     pushed *after* me gets undone, keep going and undo me too" -- so to
     chain N actions (e.g. every cell "Fold All" actually folded) into one
     atomic Ctrl+Z, call `AppendAction()` after each push *except the
     last-pushed one* (see `WorksheetDocument::RecordFoldUndo()`), not after
     the first.
  A pre-existing unit test (`test/unit_tests/test_TreeUndo.cpp`,
  "Undoing an insertion whose cell was folded away...") had to switch from
  `g_ws->ToggleFold()` to the raw `section->Fold()` once folding became
  independently undoable: it was relying on folding *not* pushing its own
  undo action so that a single `TreeUndo()` call would reach past it to the
  insertion underneath -- exactly the kind of test that silently encodes an
  old architectural assumption and breaks the moment that assumption stops
  holding, worth checking for before assuming "existing tests pass" means
  "no behavior changed."

- **`SumCell`'s always-on parentheses (GH #1536), and why the fix needed
  both wxMathML.lisp and C++:** `sum(k,k,1,n)` used to always display as
  `Σ (k)`, even though Maxima's own terminal printer shows a bare `k` --
  `SumCell`'s constructor (`src/cells/SumCell.cpp`) unconditionally wrapped
  the summand in its own `ParenCell` (`m_paren`), regardless of what the
  summand actually was. The tempting "just use the existing operator-
  precedence machinery" fix doesn't quite apply here: `%sum`/`%product` have
  no `lbp`/`rbp` registered at all in real Maxima (confirmed live:
  `(get '%sum 'lbp)` is `NIL`), so Maxima's own printer can't be using
  generic precedence comparison for this either -- it must special-case it,
  and the same real distinction it makes (parenthesize a compound summand
  like `k+k^2`, not a bare one like `k`) is exactly `mplusp` on the actual
  Maxima expression, which is what `wxxml-sum` (`wxMathML.lisp`) now checks.
  This deliberately avoids inventing a new binding-power value for `sum` --
  the maintainer has flagged doing that as risky in the past (wxMaxima's own
  operator precedences drifting out of sync with Maxima's, GH #1536's
  comment thread), so `mplusp` (an existing Maxima predicate on the real
  parsed expression) is used instead of a numeric precedence comparison.
  That decision alone isn't sufficient, though: it has to reach `SumCell`'s
  2D on-screen layout, which is computed entirely in C++
  (`Recalculate()`/`Draw()`), and `ParenCell`'s own `m_print` flag -- which
  looked like the obvious existing mechanism to reuse -- turned out to only
  suppress parentheses in `ToString()`/`ToTeX()`/`ToMathML()` (text/export
  formats); `Recalculate()`/`Draw()`/`SetCurrentPoint()` don't check it at
  all and always reserve/draw the paren glyphs regardless. So the Lisp-side
  decision is carried across the wire as a `needsparen` attribute on `<sm>`,
  which `MathParser::ParseSumTag` reads and feeds into `SumCell::NeedsParen()`
  -- a new setter that drives the *already-existing* `m_displayParen`/
  `DisplayedBase()` mechanism (previously only toggled by `BreakUp()`/
  `Unbreak()` for the broken-into-lines case) via a new persistent
  `m_baseNeedsParen` field, rather than inventing a second, competing
  wrapping mechanism. Confirmed end-to-end with a live Xvfb screenshot
  (`sum(k,k,1,n)` bare, `sum(k+k^2,k,1,n)` parenthesized, and
  `sum(k,k,1,n)+L` -- which motivated the original always-parenthesize
  decision -- correctly getting *outer* parens around the whole sum from
  the unrelated, already-existing `%sum` `rbp` registration, not extra
  parens around the summand).

- **"Don't unfold cells just because their folded tree is being evaluated"
  (GH #1952):** `Worksheet::ScrollToError()` -- called automatically by
  `MaximaEvaluator::CheckForErrors()` whenever `AbortOnError()` is on (the
  default) and a cell errors -- used to call `errorCell->RevealHidden()`
  unconditionally. If the errored cell was folded away, that silently
  unfolded the *entire* enclosing section just to point at it, defeating the
  whole reason many users fold a calculation down to one line in the first
  place: to keep the worksheet readable while it evaluates, errors included.
  Confirmed live in a real Xvfb session (a folded section containing
  `a:1$ error("...")$ a+1$`, evaluated via "Evaluate All Cells"): the section
  sprang open the instant the `error()` cell ran, on unmodified `main`.
  Fixed by walking up `GroupCell::GetHiddenTreeParent()` (returns non-null
  exactly when a cell sits in someone's torn-out `m_hiddenTree` -- see the
  `Fold()`/`Unfold()`/`CellList::TearOut` notes elsewhere in this section)
  to the outermost cell that *is* part of the visible tree, and -- only if
  that ancestor differs from the error cell itself, i.e. the cell actually
  is hidden -- targeting that ancestor (`SetHCaret`+`ScrollToCaret`) instead
  of calling `RevealHidden()`/touching `errorCell`'s own (still-hidden,
  un-renderable) `EditorCell` at all. The ordinary (not-folded) case falls
  through to the original code completely unchanged. Deliberately left
  `Worksheet::OpenQuestionCaret()`'s own `RevealHidden()` call alone: unlike
  an error, an interactive Maxima question genuinely blocks the evaluation
  queue until the user answers it, so there is no way to let the user
  respond without unfolding down to the cell that's asking. Regression-
  tested in `test/unit_tests/test_TreeUndo.cpp` (`SCENARIO("An error inside
  a folded section does not unfold it (GH #1952)")`) by folding a section,
  calling the *public* `ErrorList::Add()` directly to simulate what
  `MaximaEvaluator` does on a real error (no live Maxima needed), then
  asserting `ScrollToError()` leaves `GetHiddenTree()`/`GetHiddenTreeParent()`
  untouched and lands the h-caret on the header -- confirmed to actually
  catch the regression by reverting the fix and watching the new assertions
  fail against the old code before restoring it.

- **`wxUILocale` (GH #2233) -- `main.cpp` already had a `#if
  wxCHECK_VERSION(3, 1, 6)` branch preferring `wxUILocale` over `wxLocale`,
  but it had two live bugs, both confirmed with a standalone compiled
  reproduction against the real wxWidgets 3.2.4 in this sandbox (see
  `wx/uilocale.h`), not guessed from reading the header:**
  1. `wxUILocale::UseDefault()` was called *unconditionally*, discarding the
     user's own configured language entirely -- it always applied the
     system's default locale, even when the user had explicitly picked a
     different one in wxMaxima's own settings (`wxTranslations::SetLanguage()`
     right below it still respected the choice for UI *text*, so this bug
     was invisible for translated strings and only affected locale-driven
     formatting -- numbers, dates, etc.). Fixed by calling
     `wxUILocale::UseLocaleName(wxLocale::GetLanguageInfo(lang)->CanonicalName)`
     when `lang != wxLANGUAGE_DEFAULT`, falling back to `UseDefault()` only
     if that lookup or the switch itself fails.
  2. `wxLocale().GetCanonicalName()` -- a **fresh, never-`Init()`-ed**
     temporary `wxLocale` object -- was still used in two places (`main.cpp`,
     for building Maxima's own `LANG` environment variable, and
     `wxMaximaFrame::wxMaximaManualLocation()`, for picking the localized
     manual) to ask "what's the active locale?". `GetCanonicalName()` reads
     back `m_strShort`, a plain member that only `wxLocale::Init()` ever
     populates -- on the `wxUILocale` branch, no `wxLocale::Init()` call
     happens anywhere in the process, so this **always returned an empty
     string**, unconditionally falling both call sites back to "C"/the plain
     English manual regardless of the configured language. Confirmed with a
     standalone reproduction: `wxUILocale::UseLocaleName("de")` succeeds and
     `wxUILocale::GetCurrent().GetName()` correctly reports `"de_DE.UTF-8"`
     immediately afterwards, while a fresh `wxLocale().GetCanonicalName()`
     called in the same process stays `""`. Fixed two different ways for two
     different needs: `main.cpp`'s `LANG`-building code now reads
     `wxUILocale::GetCurrent().GetName()` instead (wants the OS's actual,
     fully-resolved locale string, which is exactly what that query returns);
     `wxMaximaManualLocation()` instead derives the language name directly
     from the already-known configured language ID via the static, lookup-
     table-only `wxLocale::GetLanguageCanonicalName(lang)` -- deliberately
     *not* from whatever the OS ended up resolving, since that lookup needs
     no locale to be installed or supported at all (confirmed live: in this
     sandbox, which ships only the `C`/`C.utf8`/`POSIX` locales,
     `wxLocale::Init(wxLANGUAGE_GERMAN)` itself reports failure, yet still
     leaves a usable `"de_DE"` in `GetCanonicalName()` -- the old `wxLocale`
     path's `Init()` populates its bookkeeping from the *requested* language
     on a best-effort basis regardless of whether the underlying OS
     `setlocale()` call actually succeeded, which is precisely the property
     `wxUILocale::GetCurrent()` lacks and why it can't be used as a
     replacement query for this specific "which language did the user pick"
     question). This lookup needs no version guard -- it's been present and
     works identically on both pre- and post-3.1.6 wxWidgets.
  - **Not yet done** (deferred, filed as open follow-ups by the maintainer,
    not part of this fix): #2229 (minimizable sidebars, wxWidgets >= 3.3.2),
    #2230 (accessible SVG export, >= 3.3.3), #2231 (PNG description chunks
    on exported cells, >= 3.3.1), #2232 (`wxNO_UNUSED_VARIABLES`, >= 3.2.7).
    None of the four could be verified in this sandbox, which only has
    wxWidgets 3.2.4 installed -- any implementation of them here could only
    be compile-checked on the pre-version-guard fallback path, not the
    actual new behavior.

- **Scaled images losing transparency (GH #2227, `Image::GetBitmap()` in
  `src/Image.cpp`):** the final step of building a scaled display bitmap
  converted the already-loaded/decoded bitmap back to a `wxImage`, called
  `Rescale()` on it, then rebuilt the bitmap with `wxBitmap(img, 24)` --
  an explicit `depth` argument. Passing a depth to this `wxBitmap`
  constructor forces that bit depth and **discards any alpha channel**,
  even when `wxImage::HasAlpha()` is true on the source image; omitting the
  parameter (the default, `-1`) auto-detects depth and preserves alpha
  instead. Every other bitmap-construction call in the same file --
  `GetUnscaledBitmap()`'s SVG-rasterize and compressed-image-decode paths,
  and `GetBitmap()`'s own first construction a few lines earlier -- already
  omits the depth argument; the scaled-bitmap path was the one outlier.
  The visible symptom (per the issue) is a previously-transparent region of
  an image rendering as solid, usually black, once the image needed
  scaling to fit its on-screen size -- black because that's what most
  image encoders leave in the RGB channels of a fully-transparent pixel,
  and once the alpha channel is gone there's nothing left to mask it.
  Fixed by dropping the explicit depth: `m_scaledBitmap = wxBitmap(img);`.
  Verified with the existing `imageFormat` ctest (`test/image-test/`,
  covers PNG/BMP/TIFF/GIF/JPG/WEBP/PNM/XPM sources, PNG and BMP both
  confirmed to actually carry an alpha channel via `file`) under
  `xvfb-run` -- it needs a real X display (`Error: Unable to initialize
  GTK+, is DISPLAY set properly?` without one) -- plus the full `ctest`
  suite for regressions. This is narrowly a "don't destroy an alpha
  channel we already have" fix; it does not address the separate,
  genuinely open design question the same issue also raises (also flagged
  by the maintainer in the issue itself): whether leaving a transparent
  pixel fully transparent is actually correct once a dark worksheet
  background is involved (e.g. black line art becoming invisible against
  it), which needs a product decision, not a bug fix, and is left for a
  follow-up.

- **RTF/OMML export (GH #1456, GH #1457) -- previously had zero test
  coverage; `test/unit_tests/test_RTFExport.cpp` is the first.** RTF export
  has two independent code paths that both matter: `TextCell::ToRTF()`
  (plain RTF text, one `\cf<N>{...}` run per cell) and `Cell::ToOMML()` +
  `Cell::OMML2RTF()` (an embedded Word/LibreOffice math field, used whenever
  `Cell::ListToRTF()` hits a cell whose `ToRTF()` is empty but whose
  `ToOMML()` isn't -- see `Cell::ListToRTF()`'s two-branch loop). Getting
  either path's cell-specific override wrong is invisible to every other
  export format's tests, since TeX/XML/MathML export don't share this code.
  - **`TextCell::ToRTF()` didn't check `IsHidden()`/`GetHidableMultSign()`/
    `HidemultiplicationSign()` at all (GH #1456)**, unlike `ToTeX()` and
    `ToXML()`, which both already do. Confirmed via a standalone harness
    (parse the real `<h>*</h>` XML `wxxmlnumformat` in `wxMathML.lisp` emits
    for scientific notation, e.g. "2*10^7" for `2e7`, through `MathParser`,
    then call `ListToRTF()` directly) that with `HidemultiplicationSign()`
    on vs. off the RTF output was byte-for-byte *identical* -- the literal
    `*` always appeared. Fixed by mirroring `ToTeX()`'s exact logic: when
    hidden, a lone `*`/`·` becomes a plain space (never removed
    outright) so cells on either side don't run together, while any other
    kind of `IsHidden()` cell (e.g. an invisible parenthesis) still clears
    to empty. The "run together" failure mode is real, not theoretical: the
    two content types don't mix in `Cell::ListToRTF()`'s output -- plain
    text and an OMML math field are adjacent, unrelated RTF constructs, so
    a `2` (plain text) immediately followed by a hidden-then-vanished `*`
    and then a `10^7` (OMML field, since `ExptCell` only implements
    `ToOMML()`, not `ToRTF()`) would have rendered as the unreadable "210^7"
    with no separator between the plain-text run and the math field.
  - **`MatrCell::ToOMML()` emitted `<m:grow>\"1\"</m:grow>` -- a *child
    element* whose text content is the two literal characters `"1"`,
    complete with quote marks -- instead of the `m:grow="1"` *attribute*
    form `ParenCell`/`ListCell`/`IntervalCell::ToOMML()` all already use
    correctly (GH #1457).** `Cell::OMML2RTF()` is a generic, mechanical
    XML-to-RTF-control-word transliterator: an attribute `m:grow="1"` and a
    same-named child element `<m:grow>1</m:grow>` both produce the
    identical, well-formed RTF math control word `{\mgrow 1}` -- but the
    quoted-text-content form MatrCell used produced `{\mgrow "1"}`, with
    stray literal quote characters inside what must be a bare flag.
    Confirmed live that this is what a real RTF-math consumer (Word,
    LibreOffice) needs by comparing against the three sibling cells' already
    -working attribute-based form, not by guessing at the OOXML schema.
    Fixed by switching `MatrCell::ToOMML()` to the same attribute form,
    which also makes all four delimiter-emitting cell types consistent.
    Word/LibreOffice silently ignoring the malformed flag and falling back
    to a small, fixed-size (non-growing) bracket regardless of the matrix's
    actual height is exactly the "big parenthesis...displayed as small
    parenthesis" the issue reported.
  - **`AbsCell::ToOMML()` was missing `m:grow="1"` entirely** (not a filed
    issue, found by auditing every `ToOMML()` for the same bug class while
    fixing #1457) -- `abs()` of a fraction or matrix would have rendered
    its `|  |` bars at a fixed, non-growing size in RTF/Word export, unlike
    every other bracket-drawing cell in this codebase. Fixed the same way.
  - **Verification methodology**, since none of this was previously
    testable at all: a standalone harness (same pattern as
    `test_IntegralToTeX.cpp` -- real `MathParser`, hand-written XML matching
    exactly what `wxMathML.lisp` emits, no live Maxima needed) was used to
    reproduce both bugs live *before* writing the fix, then promoted into
    `test/unit_tests/test_RTFExport.cpp` as a permanent regression test
    once the fix was confirmed. Confirmed the new test actually catches the
    regression (not just passing vacuously) by reverting the three
    `ToOMML()`/`ToRTF()` fixes via `git stash` and re-running it: all three
    `SCENARIO`s failed with the exact old symptoms, then passed again once
    the fixes were restored.

- **"Maxima started but never connects" watchdog (GH #1182, open since
  2019).** Before this, if the Maxima process launched successfully but
  its socket connection back to wxMaxima's `wxSocketServer` never arrived
  -- wxMaxima is the TCP *server* here; the spawned `maxima` binary is the
  *client* that has to connect back, see `MaximaProcessManager::
  StartServer()`/`OnMaximaConnect()` -- nothing timed this out or told the
  user: the worksheet just sat at "Maxima started. Waiting for
  connection..." forever, with no error, no retry, no explanation. This is
  distinct from the *other* code path that already existed
  (`OnMaximaConnect()`'s `m_unsuccessfulConnectionAttempts < 12` retry
  loop): that one only fires once a connection attempt reaches wxMaxima and
  then fails -- it does nothing if no attempt ever arrives at all, which is
  exactly what happens when the child process never gets far enough to open
  the socket. Confirmed live in this sandbox (Linux, can't reproduce the
  actual macOS Gatekeeper trigger, but the missing-timeout mechanism itself
  is platform-independent): pointed wxMaxima's `-m` flag at a throwaway
  shell script that just `sleep`s forever instead of a real `maxima`
  binary -- unmodified `main` sits at "Waiting for connection..." with zero
  further log output, indefinitely.
  Fixed with a new one-shot `wxTimer` (`MAXIMA_CONNECT_WATCHDOG_ID`,
  `wxMaxima::m_maximaConnectWatchdogTimer`) armed for 5 seconds (matching
  the issue title's own number) every time `StartMaxima()` successfully
  spawns a process, and stopped both on a real successful connection
  (`OnMaximaConnect()`) and on `KillMaxima()` (covers deliberate shutdown
  and the top of every restart, since `StartMaxima(force=true)` always
  calls `KillMaxima()` before re-arming). If it fires while the process is
  still alive (`wxProcess::Exists()`) and still not connected, it shows a
  `LoggingMessageBox` once per run (`m_maximaConnectWatchdogWarningShown`
  latches so the automatic restart loop -- which re-arms this same timer on
  every retry -- doesn't reshow the dialog up to 12 times in a row). The
  message branches on `__WXOSX__`: on macOS it names the quarantine
  possibility specifically (a background process wxMaxima spawns can never
  answer the interactive security prompt Gatekeeper would otherwise show,
  so it just silently never finishes starting) and suggests both re-running
  the shown command from a Terminal once and `xattr -d
  com.apple.quarantine`; elsewhere it's a generic "still waiting, check the
  debug sidebar or your firewall" message, since quarantine isn't the
  relevant cause there. Verified end-to-end in a live Xvfb session with the
  same fake-hung-process technique: the log line appears at exactly +5s and
  only once, and a screenshot confirms the dialog renders correctly with
  the non-macOS wording (the `__WXOSX__` branch itself is untestable here
  for the same reason #2229-#2232 were -- no macOS hardware in this
  sandbox -- but it's the same string-formatting/branching mechanism,
  already exercised by the generic path).

## Layout & Compatibility

The rules below are the ones worth carrying around at all times. The reasoning
behind them, the pipeline they belong to, and the recurring shapes layout bugs
take are in the **`wxmaxima-layout` skill** (`.claude/skills/wxmaxima-layout/`),
which loads on demand -- keep the detail there rather than growing this file.

- **`RequestRecalculation()` only SCHEDULES; `RecalculateIfNeeded()` executes.**
  Never read geometry (positions, sizes) on the line after asking for a
  recalculation -- at that point nothing has been recalculated yet, and you get
  the previous layout. `AdjustSize()` deliberately defers while positions are
  stale. The name is the trap: it was called `Recalculate()` until 2026-07-08,
  and code written against the old name reads as if it were synchronous.
- **A composite cell's `Recalculate()` override MUST recurse unconditionally.**
  Roughly twenty composite cells (`FracCell`, `SqrtCell`, `ParenCell`, ...) used
  to skip recursing into their children when they judged themselves unchanged.
  That is wrong: a child can be dirty for a reason the parent cannot see (a font
  size change on partial breakup), and the parent's guard then strands it at a
  stale width -- which is how parens ended up too narrow for their contents.
  Recurse every time and let each child's own changed-flag decide.
- **List caches must be invalidated when the configuration counter changes.**
  Cached per-list geometry (`m_listCacheCfgCnt` and friends) survives a
  configuration change unless it is stamped with the counter and compared on
  use. Forgetting this produces the "stale spacing" family of bugs, where cells
  keep a width computed under the previous font/zoom settings.

- **Mathematical Cell Padding:** Use `MC_TEXT_PADDING` (in `Configuration.h`) for text-based cells. **Exception:** `DigitCell` does not include padding to ensure visual consistency in broken-up numbers.
- **Three-Step Layout Process:**
  1. `UnBreakUpCells()`: Reset to 2D.
  2. `BreakUpCells()`: Convert wide 2D objects to 1D fallback.

     **Recursive Strategy:** If a 2D object is too wide, `CollectWideCells` recursively identifies sub-cells that are already >80% of the available width. These sub-cells are also converted to linear form in the same pass. This heuristic accounts for font size increases that occur when a parent object is linearized, preventing redundant O(N^2) size resets and recalculations in deeply nested structures.
  3. `BreakLines_List()`: Final line wrapping.
- **High-DPI / wxBitmapBundle:** Use `wxBitmapBundle` for SVG rendering.
- **Windows Focus Management:** Use `CallAfter` for focus transitions (e.g., `m_searchText->SetFocus()`) to prevent the worksheet from "stealing" focus back.
- **Graphical export temp files (`src/graphical_io/OutCommon.cpp`):** `wxSVGFileDC`/EMF's DC only write to a real path, so the SVG/EMF representation rendered for the clipboard needs a temp file (unlike a real "Export as..." target file, which is the user's own chosen path and untouched by this). `PrivateTempDir()` puts it in a mode-0700 `tmp/` subdirectory of `Dirstructure::UserConfDir()` instead of the shared system temp dir, so another unprivileged user can't win a race between the file's creation and its being opened by name (the classic symlink-swap window any path-only API leaves open). Falls back to `wxFileName::CreateTempFileName()`'s own default location if that directory can't be created.
- **Bidi (`src/Bidi.h`/`.cpp`):** Reorders a line of text per the Unicode Bidirectional Algorithm (UAX #9), using `libfribidi` when it's available at build time (`USE_FRIBIDI` in `BuildConfig.h`, optional, `WXM_USE_FRIBIDI` CMake option, on by default when `pkg-config fribidi` is found) and falling back to a single-run approximation otherwise. No wxWidgets backend exposes this reordering itself -- Pango/CoreText/DirectWrite compute it internally to shape glyphs but never hand it back to the app. `EditorCell::GetLineBidiRuns()` wraps it as absolute `m_text` positions; `MixedDirectionOffset()` (used by `PositionToPoint()`, hence also `MarkSelection()` and `SelectPointText()`'s click search) and `HandleSpecialKey()`'s arrow-key handling are the consumers. wxmTestApp is an OBJECT library (`test/unit_tests/CMakeLists.txt`): it compiles `Bidi.cpp` itself and needs `PkgConfig::FRIBIDI` linked directly to *it* (not just to `wxmaxima`) to get fribidi's include path at that compile step; separately, its own `target_link_libraries()` don't propagate through `$<TARGET_OBJECTS:wxmTestApp>`, so anything it needs must *also* be linked into each consuming test executable directly (`WXM_TESTAPP_EXTRA_LIBS`) for the final link. The imported target has to be declared `GLOBAL` since `test/` is a sibling directory of `src/`, not a descendant. `#include <fribidi.h>`, not `<fribidi/fribidi.h>`: pkg-config's own `-I` already points *at* fribidi's header directory (confirmed on both Debian's and Homebrew's `.pc` files), so the extra `fribidi/` prefix only "worked" on Linux by accident, via `/usr/include` being an implicit compiler search path Homebrew's non-default prefix doesn't share -- caught by a real macOS CI failure, not by this sandbox.
- **ConfigDialogue Tabs Must Scroll:** Every tab panel in `src/dialogs/ConfigDialogue.cpp` is a `wxScrolled<wxPanel>` with `SetScrollRate(5 * GetContentScaleFactor(), 5 * GetContentScaleFactor())` and `SetMinSize(wxSize(GetContentScaleFactor() * mMinPanelWidth, GetContentScaleFactor() * mMinPanelHeight))`. Without this, a tab's natural size (which grows with font size/DPI/translation length) can make the whole dialog taller than a hi-DPI screen with no way to reach what's cut off. When adding a new tab, copy this pattern (see `CreateWorksheetPanel()`) rather than a plain `wxPanel`.
- **Constructor Initialization:** Order initialization lists to match header declaration order to prevent `-Wreorder` warnings.
- **AUI: `RestorePane()` does NOT undo `MinimizePane()`.** Despite the name it is
  the counterpart to `MaximizePane()`, and its implementation rewrites *every*
  pane's hidden state from `savedHiddenState` -- using it to un-minimize one
  sidebar silently reshuffles all the others. There is no separate "minimized"
  state to undo: `MinimizePane()` just calls `paneInfo.Hide()` and adds an entry
  to a min-dock strip, and wx's own restore is nothing more than
  `pane->Show(); m_mgr.Update();` -- exactly what `ShowPane()` and
  `ShowWizardPane()` already do. So showing a pane the normal way restores it
  from minimized for free. Note also that centre panes cannot be minimized and
  `MinimizePane()` asserts on panes without a minimize button -- so wherever
  `MinimizeButton(true)` is handed out, the worksheet and the toolbar must be
  excluded.
- **A wxWidgets-version fallback `#define` must come AFTER the wx header that
  may define it.** `Compat.h` includes `<wx/defs.h>` *above* its
  `#ifndef wxWARN_UNUSED` fallback for this reason. Reached in the other order,
  our header defines the macro empty first, wxWidgets' own
  `#ifndef wxWARN_UNUSED` then declines to redefine it, and the feature is
  silently disabled on exactly the compilers that support it -- a change that
  compiles everywhere and does nothing. The same trap applies to any future
  `wxSOMETHING` shim added there.

## Performance & Documentation Mandates

- **NEWS.md Updates:** Every non-trivial change MUST be documented in `NEWS.md` under the "# Current development version" section.
- **Doxygen Comments:** Include descriptions for all new classes and public methods. Complex algorithms (e.g., LCS alignment) require detailed architectural comments.
- **Background Tasks:** Use `jthread` for automatic joining, protect data with `std::mutex`, check for abort flags regularly, and update `Doxygen/Readme.md`.
- **Lisp Performance (`wxMathML.lisp`):**
  - Use `with-output-to-string` instead of recursive concatenation for large inputs.
  - Use `unwind-protect` when modifying global state like `$lmxchar`.
  - Prefer `(intern ...)` over `read-from-string` for dynamic symbol generation.
- **Strict XML Mandate:** To avoid duplicate attributes, add any new manually-handled XML attribute (e.g., `noneParens="true"`) to the filter list in `MathParser.cpp`.

## Visual Documentation (`art/Doxygen/`)

- **Geometry Awareness:** If modifying `Recalculate()` or `Draw()` geometry (padding, center alignment), you MUST update the corresponding SVG diagrams in `art/Doxygen/`.
- **Consistency:** New cell types should have `*Geometry.svg` and `*LinearGeometry.svg` (if applicable) diagrams.

## Key Subsystems Map

Deeper, per-subsystem background lives in `.claude/skills/`, which load only
when they are relevant -- so that hard-won detail is available without every
line of it sitting in context permanently. Read the matching one *before*
starting work in that area; each is mostly a list of the ways that subsystem
has already been broken.

| Skill | Covers |
|---|---|
| `wxmaxima-layout` | the schedule/recalculate/resize pipeline and the layout invariants |
| `wxmaxima-architecture` | where code lives, the friend-class decomposition, the extraction recipe |
| `wxmaxima-translations` | the POT glob, po4a, Crowdin, and how translations get lost |
| `wxmaxima-export` | HTML/LaTeX/image export, accessible labels, round-trip guarantees |
| `wxmaxima-maxima-protocol` | the socket, `wxMathML.lisp`, batch mode, process lifetime |
| `wxmaxima-packaging` | the CI matrix's blind spots, installers, signing, releases |
| `run-wxmaxima` | building, launching and screenshotting the app |

- **Layout Engine:** `src/cells/` and `src/worksheet/` (`Worksheet.cpp` and its siblings moved into that subdirectory).
- **MathML Formatting:** `src/wxMathML.lisp` and `src/MathParser.cpp`.
- **Main Logic:** `src/wxMaxima.cpp` and `src/wxMaximaFrame.cpp` -- but much of what used to sit in `wxMaxima` has been peeled off into friend classes, so look there first: `MaximaProcessManager` (spawn/kill/connect and the data pump), `MaximaEvaluator` (evaluation queue and command protocol), `MaximaResponseReader` (the incoming-XML handlers), `MaximaFileIO` (worksheet open/save) and `MaximaCommandMenus` (the menu handlers).
- **Configuration:** `src/Configuration.cpp`.

## Backlog / Future Work

Items the maintainer has flagged as worth doing but hasn't asked for yet -- don't
start on these without checking in first, but pick them up if asked for "what's
next" style work.

- **Real tab handling in `EditorCell`:** tabs are currently just replaced by
  spaces on input instead of being handled as their own character/column-stop
  concept.
- **GH #1335 -- cell allocations are non-local (still open, unstarted):**
  `Cell`s are still individually heap-allocated and linked via each cell's own
  `m_previous`/`m_next`, not stored contiguously. `CellList.h`'s own header
  comment already says "the eventual plan is to have a list of cells be a
  dedicated lightweight class working together with an arena allocator", but
  `CellListBuilderBase` still just holds a `std::unique_ptr<Cell> m_head` --
  that plan was never implemented. The issue's three proposed moves are all
  still open: (1) drop `m_previous`/`m_next` from `Cell` in favor of a
  `CellList` that owns contiguous storage, (2) hoist `m_group` from `Cell` to
  `CellList` (one owner per list), (3) hoist the per-line-geometry caches --
  the issue calls them `m_fullWidth`/`m_maxCenter`/`m_maxDrop`, renamed since
  to `m_cachedSumOfWidths`/`m_cachedCenterList`/`m_cachedMaxDrop`/
  `m_cachedLineWidth` -- from `Cell` to `CellList` too. Confirmed `sizeof(Cell)`
  is 224 bytes on the current tree (checked directly, post-#1445), not the 112
  the issue was measured against in 2020 -- `Cell` has grown substantially
  since (accessibility support, config-change-tracking atomics, UUID string,
  extra-XML-attributes map, ...), so the issue's "112 -> 76 bytes" estimate is
  stale, but the underlying proposal is still real. This is a bigger
  undertaking than #1445: it changes the core list *storage model*
  (`m_next`/`m_previous` becoming array-relative instead of pointer-based),
  touching every list-manipulation site in `CellList.cpp` plus anything
  walking `GetNext()`/`GetPrevious()` directly -- scope it out carefully
  before starting, don't assume it's a small follow-on to #1445 just because
  they're adjacent/both filed by KubaO in 2020.

## Error resilience

- To err is human => If your instructions don't seem to make sense feel free to ask.
