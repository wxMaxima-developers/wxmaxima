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
  broke. **Neither this workaround nor `gnuplot`'s installation (below)
  persists across sandbox instances** -- confirmed directly: a session that
  applied both earlier came back to a broad `ctest -E
  "tutorial|openMacFiles|_cmdline_wxmathml|wxmaxima_version"` run showing 66
  of 194 tests failing (`boxes`, `lisp`, `threadtest`, `autosave`,
  `printf_*`, `multiplication`, `config_dialogue_sample`, ... -- a broad,
  cross-cutting spread with no relation to whatever code change was
  actually being tested that session), which briefly looked like a real
  regression until re-running a handful of the failing tests in isolation
  showed the exact `maxima-index.lisp`/`--exit-on-error` symptom above.
  **A second, independent gap found the same way, same session: `gnuplot`
  itself was not installed at all** (`threadtest` failed with `/bin/sh: 1:
  gnuplot: not found`, no relation to the maxima-index.lisp issue). Plain
  `apt-get install gnuplot`/`gnuplot-nox` failed here with an unmet
  `libgd3` dependency -- this sandbox's apt sources include a `ppa:ondrej/
  php` entry offering a newer `libgd3` build than Ubuntu's own archive, and
  that PPA's package host was blocked by this sandbox's proxy (`403` on
  `ppa.launchpadcontent.net`), while the plain Ubuntu-archive `libgd3`
  (also present as a candidate, just lower-priority) was fetchable fine.
  Fixed with `apt-get install libgd3=2.3.3-9ubuntu5 gnuplot-nox` (the exact
  archive version may drift; `apt-cache policy libgd3` shows both
  candidates and which one is blocked) -- pinning the plain-archive version
  explicitly sidesteps the blocked PPA instead of needing the PPA fixed.
  **Moral for future sessions:** if a broad ctest run shows a large,
  topically-scattered batch of failures all at once (rather than one
  focused area related to the change being tested), suspect a fresh
  sandbox instance missing one of these two pre-existing workarounds
  before suspecting a real regression -- re-run 2-3 of the failing tests
  in isolation and check their actual output for these exact symptoms
  first, per the "don't burn time re-diagnosing from scratch" note above.

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

- **MCP server (`src/mcp/McpServer.{h,cpp}`, `src/mcp/McpTools.{h,cpp}`,
  2026-09) -- lets an external AI tool read the current worksheet as
  context.** User request: "they say that there is a de facto standard on
  how to create an AI sidebar that allows to talk to an AI and gives it
  access to a worksheet" -- identified as MCP (Model Context Protocol).
  Scoped down with the maintainer to a first, self-contained PR before any
  in-app chat sidebar: an MCP server only, read-only except for two
  explicitly-approved exceptions (see below), no write/insert/edit/evaluate
  capability of any kind.
  - **Split for testability**: `McpTools` is the actual worksheet-reading
    logic (`ListCells`/`ReadCell`/`ReadWorksheet`/`ReadToc`/`ReadSection`/
    `ReadVariables`/`WatchVariable`/`UnwatchVariable`), with zero networking
    or JSON-RPC framing -- takes a `Worksheet*`/`Variablespane*`, returns
    `nlohmann::json`, throws `McpToolError` for a bad tool name/arguments.
    `McpServer` is purely the HTTP+JSON-RPC transport around it. This mirrors
    why `TableOfContents`'s own structure-walking isn't reused as-is: its
    public surface (`GetCell(displayedIndex)`, filtered by the TOC's own
    search box/depth setting) is display-oriented, not a clean data API, so
    `McpTools::ReadToc()` just re-walks `OnList(tree)` filtering
    `GroupCell::IsHeading()` directly instead -- the same primitive
    `TableOfContents::UpdateStruct()` itself uses.
  - **Why "read-only except two things" and not stricter**: the maintainer
    explicitly approved `watch_variable`/`unwatch_variable` (adding/removing
    a name from the Variables sidebar's watchlist) as safe despite being
    nominal writes -- they only change what that sidebar happens to be
    displaying, the same as a user typing a name into it by hand; they touch
    no worksheet content and cannot insert, edit or evaluate anything. Every
    other tool cannot mutate anything even in principle, since all they do
    is turn existing worksheet/cell state into text.
  - **`read_section`, added after the maintainer's own suggestion** ("AIs
    often [read] the table of contents [of] big documents ... perhaps
    another useful command would be read_section"): reads one whole
    section's text by its heading cell's UUID (from `read_toc`), so an AI
    can navigate a large worksheet via TOC -> one section at a time instead
    of pulling `read_worksheet`'s entire (size-capped) text or walking
    `list_cells` one UUID at a time. Its "how far does this section extend"
    walk is the exact same loop `GroupCell::Fold()` already uses (stop at
    the first following cell whose type equals, or is a higher heading
    level than, the section's own -- `GroupCell::IsLesserGCType()`, already
    public) -- reimplemented read-only rather than calling `Fold()` itself,
    since folding actually tears the range out of the tree
    (`CellList::TearOut`) and this must never touch the tree at all.
  - **Cell identity**: `Cell::GetUUID()`/`GenerateUUID()` (lazy -- empty
    until first needed, see `Cell.h`) is the only existing stable identifier
    a cell has; every tool that returns cells generates one on the fly for
    any cell that doesn't have one yet. This is a real, if minor, side
    effect worth knowing about: once an MCP tool has looked at a cell, that
    cell's UUID is no longer empty and will be written out on the next
    save -- same as opening the XML inspector or diff view already does
    elsewhere in this codebase, not something unique to this feature.
  - **`GroupCell::GetOutput()` deliberately skips the first cell in
    `m_output` -- that's the answer label ("(%o1)"), not part of the actual
    output text**, per its own doc comment (`GetLabel()` returns that first
    cell instead). `McpTools::OutputText()` relies on this to return clean
    output text with no label prefix -- confirmed both live (a real
    `--eval`-loaded worksheet's `read_cell` showed plain output text, no
    "(%o1)" noise) and by a test-fixture bug this exact behavior caused
    while writing `test_McpTools.cpp`: building a cell's test output as a
    single `TextCell` via `SetOutput()` alone left `GetOutput()` returning
    `nullptr` (nothing follows a lone cell), because a *real* Maxima
    response's output always starts with that label cell first -- the test
    fixture had to chain a label cell then the real content via
    `SetOutput()` followed by `AppendOutput()`, mirroring that real shape,
    before `GetOutput()` returned anything.
  - **Transport (`McpServer`)**: MCP's "Streamable HTTP", the read-only
    subset of it that needs no server-initiated push -- one HTTP endpoint
    (`POST /mcp`) answering one JSON-RPC 2.0 request per connection, closing
    afterwards (`Connection: close`, no keep-alive, no chunked encoding); a
    `GET /mcp` gets a plain 405, which the spec allows for a server with no
    SSE stream to offer. No `Mcp-Session-Id` bookkeeping either -- optional
    for the server to assign per spec, and every tool call here is
    independently answerable from live worksheet state with no session to
    track. Implemented directly on `wxSocketServer`/`wxSocketBase`
    (event-driven, `Notify()`-based, the same primitive `Maxima.cpp` already
    uses for the Maxima<->wxMaxima protocol -- just event-driven here
    instead of on `Maxima.cpp`'s dedicated worker thread) rather than
    pulling in an HTTP library: the request shape needed is tiny (one
    method, one path, a couple of headers, a Content-Length-delimited
    body), so a general-purpose HTTP server buys nothing. Runs entirely on
    the GUI thread's own event loop for exactly this reason -- it can call
    directly into `Worksheet`/`GroupCell`/`Variablespane` with zero
    marshaling, since none of them are thread-safe. Never make this
    transport threaded without adding the `CallAfter()`-based marshaling
    every cross-thread worksheet touch elsewhere in this codebase needs.
  - **JSON**: vendored `nlohmann/json.hpp` (single header, MIT, v3.11.3,
    `src/vendor/nlohmann/`) rather than a hand-rolled parser -- correctness
    matters here since real MCP clients need to interoperate with this, and
    a hand-rolled JSON parser is exactly the kind of "looks fine until an
    edge case" component not worth re-deriving for this. Wired in via
    `include_directories(SYSTEM ".../src/vendor")` in `src/CMakeLists.txt`
    (so its own warnings don't hit `-Werror` builds, the same reasoning
    `privateNanoSVG.cmake`'s pragma-based warning guard exists for, just via
    CMake's own mechanism since there's no symbol collision to rename around
    the way nanoSVG's vendoring needs) -- **this include path is
    directory-scoped and does not reach `test/unit_tests/`** (a sibling
    directory of `src/`, not a descendant -- the exact same trap
    `WXM_USE_FRIBIDI`'s include wiring already documents elsewhere in this
    file), so `test/unit_tests/CMakeLists.txt` needed the identical
    `include_directories(SYSTEM ...)` line added independently, or
    `test_McpTools`'s very first build failed with a plain "no such file"
    on `<nlohmann/json.hpp>` despite `wxmaxima` itself building cleanly.
  - **Safety**: binds only to `127.0.0.1` (`wxIPV4address::LocalHost()`),
    double-checks each accepted connection's peer address is loopback too
    (defense in depth beyond the bind itself), and validates the `Origin`
    header against localhost/127.0.0.1 when a client sends one (most
    non-browser MCP clients don't; browsers always do) -- this is
    specifically the MCP spec's own called-out DNS-rebinding mitigation: a
    malicious web page resolving an attacker-controlled hostname to
    127.0.0.1 so a victim's browser fetch reaches this server looking
    same-origin. Off by default (`Configuration::McpServerEnabled()`,
    opt-in via Options).
  - **wxSocketBase::Destroy(), not `delete`/a default-deleter
    `unique_ptr`, for every client connection** -- connections are always
    torn down (both the normal "request handled, close it" path and the
    `wxSOCKET_LOST` path) from inside that same socket's own `wxEVT_SOCKET`
    event handler call stack, which is exactly the situation wx's own docs
    say `Destroy()` (defers actual destruction rather than deleting
    mid-callback) exists for. `Connection` therefore holds a raw
    `wxSocketBase*`, not an owning smart pointer, and every removal site
    calls `->Destroy()` explicitly before erasing the connection from
    `McpServer::m_connections`. The listening `wxSocketServer` itself is a
    plain `std::unique_ptr` since it's only ever destroyed from
    `ReconcileWithConfig()`/`~McpServer()`, neither of which nests inside
    the server socket's own accept-event handler.
  - **Verification**: `test/unit_tests/test_McpTools.cpp` pins `McpTools`
    against a real, headless `Worksheet`/`Variablespane` (no live Maxima, no
    sockets -- the same `InsertGroupCells`/`SetOutput` pattern
    `test_WorksheetFind.cpp`/`test_AnonymizeCodeCells.cpp` already use) --
    cell listing/reading, TOC extraction, section boundaries (including a
    nested-subsection case), and the full watch/read/unwatch variable round
    trip, plus the "unknown tool name/UUID/missing argument throws
    `McpToolError`" error paths. The transport itself (`McpServer`) was
    verified live instead, since sockets/HTTP framing don't fit that
    fixture style: a real `-e`-loaded worksheet in a live Xvfb session,
    `curl`-ing every tool (`initialize`, `tools/list`, `tools/call` for all
    eight tools, including the full watch/read/unwatch flow against a real
    connected Maxima) plus the transport-level edge cases (`GET` -> 405, a
    non-localhost `Origin` -> 403, a localhost `Origin` -> 200, an unknown
    path -> 404, a JSON-RPC notification -> empty 202) -- then confirmed a
    clean shutdown (`SIGTERM`) leaves no lingering `wxmaxima`/`maxima`
    process and the port genuinely closes. Also confirmed the whole
    `ctest` suite's non-batch/non-live-Maxima tests still pass and the
    full tree rebuilds with zero new warnings.
- **AI chat sidebar (`src/ai/AiProvider.{h,cpp}`, `src/sidebars/AiChatSidebar.{h,cpp}`)
  -- a follow-up to the MCP server above, for the case where the "AI tool"
  is wxMaxima's own UI rather than an external one.** A docked sidebar
  (View -> Sidebars -> AI Chat) that chats with Anthropic, OpenAI, Google
  Gemini or Qwen (via DashScope's OpenAI-compatible endpoint) using a
  pasted API key -- no OAuth, since none of these four offer a legitimate
  third-party OAuth flow for a desktop app to use. v1 is deliberately
  read-only and has no tool-calling: `AiChatSidebar::BuildContextSnapshot()`
  sends one plain-text snapshot of the worksheet (via the *same* `McpTools`
  the MCP server already uses, `McpTools::ReadWorksheet()`) as a system-
  style context message, truncated to `MAX_CONTEXT_LENGTH` (8000 chars);
  the model can discuss it but has no way to act on the worksheet, since
  wiring up real tool-calling (there are 8 tools now, MCP's `tools/call`
  can already run them) is real design work of its own -- a natural
  follow-up, not attempted here.
  - **Provider abstraction (`AiProvider.h`):** one small `AiProvider` base
    class per provider family, each implementing four things that differ
    per API -- `RequestUrl()`, `AuthHeaders()`, `BuildRequestBody()`,
    `ParseReply()` -- all pure string/JSON logic with no networking of its
    own, which is what makes `test/unit_tests/test_AiProvider.cpp` able to
    pin all four providers' request/response shapes with zero live network
    access (`#include "ai/AiProvider.cpp"` directly, same lightweight
    pattern as `test_MaximaProtocol.cpp`). OpenAI and Qwen share one
    `OpenAiCompatibleProvider` implementation (DashScope's compatible-mode
    endpoint is byte-for-byte the same `chat/completions` shape, only the
    URL differs) -- confirmed via `test_AiProvider.cpp`'s "Qwen, via
    DashScope's OpenAI-compatible endpoint" scenario, not assumed from the
    provider's marketing docs.
  - **Lifetime safety across the async boundary:** `MakeAiProvider()`
    returns a `std::shared_ptr<AiProvider>`, not `unique_ptr`, and the
    static `AiProvider::SendChat(std::shared_ptr<const AiProvider> self,
    ...)` takes that shared ownership explicitly and captures it by value
    into the completion lambda. This is load-bearing, not defensive
    overkill: `AiChatSidebar::m_provider` can be replaced mid-flight (the
    user opens Options and changes provider/model while a request is still
    in the air), and without the shared_ptr the callback's `self->Name()`/
    `self->ParseReply()` calls would use a dangling pointer to whatever the
    sidebar used to point at. `wxWebRequest` itself follows the exact same
    "local value, never stored anywhere else, goes out of scope right
    after `.Start()`" shape as the codebase's one prior use
    (`wxMaxima::CheckForUpdates()`) -- confirmed this is fine because
    `wxWebRequest` is a ref-counted handle wxWebSession itself keeps alive
    while the request is in flight, not something this code has to keep
    alive itself. The completion lambda is deliberately never `Unbind()`'d
    for the same reason `CheckForUpdates()`'s never is: wx's functor-based
    `Bind()` has no reliable identity to `Unbind()` a lambda by, and one
    small permanently-bound no-op-after-firing lambda per chat turn is not
    a real leak at realistic chat volumes. The request's id must be a real,
    unique one from `wxWindow::NewControlId()` (stored in a
    `wxWindowIDRef`, which releases it back to wx's finite id pool once
    nothing references it, unlike a plain `int`) -- with the default
    `wxID_ANY` every request would share one id and every past request's
    stale lambda would refire on every new request's completion.
  - **`wxWebRequest::State_Unauthorized` is not optional to handle -- an
    invalid API key hangs the chat forever, silently, if you only handle
    `State_Completed`/`State_Failed`/`State_Cancelled` (confirmed live,
    not guessed).** The natural-looking implementation handles exactly
    those three states and falls through to `default: break;` for
    anything else. Live-testing against the real Anthropic API with a
    deliberately wrong key (`sk-ant-fake-test-key-1234` -- this sandbox's
    `no_proxy` list happens to include `api.anthropic.com`, so direct
    outbound HTTPS to it works here without a real key or a full model
    call) reproduced a genuine hang: the sidebar sat on "Waiting for
    Anthropic..." with Send disabled for 7+ minutes and never recovered.
    Root-caused with `/proc/<pid>/fd` + `/proc/<pid>/net/tcp` (this
    sandbox has no `tcpdump`): a real ESTABLISHED TCP connection to
    `api.anthropic.com`'s actual IP was sitting open and idle (0 bytes in
    either queue), and a raw `curl` to the same endpoint with the same fake
    key returned instantly with a normal HTTP 401 -- so the network path
    itself was never the problem. Confirmed with temporary `wxLogMessage`
    tracing in the event handler (removed before committing) that
    `wxWebRequestEvent::GetState()` reports `State_Unauthorized` (value
    `1`) for this response, not `State_Completed` with `status==401` --
    wx's web-request backend intercepts any 401 (confirmed via a raw
    `curl -i` that Anthropic's actual 401 response carries no
    `WWW-Authenticate` header at all, so this isn't about a real HTTP-auth
    challenge, just the status code alone) and diverts it to this separate
    state, meant for the case where the app might retry with different
    credentials via `wxWebAuthChallenge::SetCredentials()`. None of these
    four providers use real HTTP Basic/Digest auth -- they authenticate via
    a plain header (`x-api-key`/`Authorization: Bearer`/`x-goog-api-key`)
    -- so there is no challenge this app could ever answer, and left
    unhandled the request simply never produces another event on its own.
    Fixed by adding an explicit `case wxWebRequest::State_Unauthorized`
    that reads `evt.GetResponse()` (still valid here -- the server's full
    401 response already arrived before wx reinterpreted it), reports it
    through the same `callback(false, ...)` path as a non-2xx
    `State_Completed`, and calls `.Cancel()` on a copy of
    `evt.GetRequest()` (a ref-counted handle, so the copy cancels the same
    underlying request) so the connection doesn't dangle. `Cancel()` itself
    raises a *second* event (`State_Cancelled`) for the same request id,
    so a `std::shared_ptr<bool> done` guard (captured by the lambda,
    checked at the top and set before every `callback()` call) stops that
    second event from invoking `callback` twice for one chat turn -- caught
    by reasoning through the control flow before it could ship as a
    double-`AppendToHistory()` bug, not found live. Re-verified live after
    the fix with the identical fake-key setup: the sidebar's history now
    shows the real `{"type":"error","error":{"type":"authentication_error",
    "message":"API key is invalid."},...}` body from Anthropic, the status
    line returns to "Chatting with Anthropic", and Send/Clear both
    re-enable -- confirming the fix, not just the absence of a hang.
  - **Layout: this sidebar docks into a narrow shared column (e.g. next to
    Table of Contents), and any horizontal-sibling layout inside it risks
    the same "one control crowds another down to an invisible sliver"
    failure.** First cut put the input box beside a button column, and
    separately tried two buttons side by side below a full-width input --
    both silently rendered one of the two controls at a few pixels wide,
    invisible in normal use. Confirmed live (not guessed) by temporarily
    setting each competing control's background to a distinct debug colour
    and screenshotting in Xvfb: the "input" area was entirely one colour
    with zero of the other visible anywhere. Fixed by abandoning every
    horizontal pairing and stacking status/history/input/Send/Clear as
    five independent children of one vertical `wxBoxSizer`, each full
    width -- no two controls ever compete for the same row's width.
  - **Verification recap:** `test_AiProvider` (54 assertions across all
    four providers' request/response/error shapes, no live network) plus
    the live Xvfb round trip above through the real Anthropic API endpoint
    (this sandbox's `no_proxy` list permits it) covering the layout fix,
    the full send/receive/error cycle, and the `State_Unauthorized` hang
    and its fix. Not verified here: a real, valid API key's successful
    reply for any of the four providers (this sandbox has no real
    credentials for any of them, and the user's own account is Anthropic's
    -- theirs to try first), and OpenAI/Google/Qwen's actual live endpoints
    at all (only Anthropic was reachable from this sandbox; the other
    three were checked only against their documented request/response
    shapes in `test_AiProvider.cpp`, not against a live server).
  - **Follow-up (2026-09-06): the worksheet context carried no notion of
    "where the user is" or "which cell errored" at all** -- raised by the
    user directly ("if a user asks the AI about the 'current' cell or the
    cell above the cursor that output an error message... we provide
    little to no info that allows the AI to navigate/find out where in
    the worksheet the user currently is"), and confirmed exactly right by
    reading `McpTools::ReadWorksheet()`/`CellSummary()`: neither carried
    any cursor or error information, only cell type/input/output text.
    Fixed by adding `McpTools::CurrentCell()` (thin wrapper over
    `Worksheet::GetHCaret()`, which already resolves "where the user
    is" -- active editor, h-caret, selection, or a last-resort fallback --
    for the worksheet's own insertion-point logic, so this reuses an
    existing, already-correct notion rather than inventing a second one)
    and `McpTools::HasError()` (a thin wrapper over the existing
    `DocumentCellPointers::ErrorList::Contains()`, the same mechanism
    `Worksheet::ScrollToError()` already relies on -- see the GH #1952
    entry above). Both are surfaced two ways: as `is_current`/`has_error`
    booleans on `list_cells`/`read_cell`'s JSON, and -- since the AI chat
    sidebar sends only `read_worksheet`'s plain-text dump, with no
    tool-calling to fall back on -- as inline markers
    (`"(CURRENT CELL -- the user's cursor is here)"`/`"(THIS CELL HAS AN
    ERROR)"`) directly in that text, extracted into a new shared
    `McpTools::CellText()` helper so `ReadWorksheet()` and `ReadSection()`
    can't drift apart on this (an actual near-miss: the first pass only
    patched `ReadWorksheet()`, and `ReadSection()`'s own near-identical
    per-cell loop would have silently kept lacking both markers).
    **A second, related bug found while fixing the first**:
    `AiChatSidebar::BuildContextSnapshot()`'s truncation for
    `MAX_CONTEXT_LENGTH` (8000 chars) took `text.Left(...)` -- for any
    worksheet long enough to actually need truncating, this silently cuts
    off content from the *end*, which is exactly where the new
    "(CURRENT CELL...)" marker is most likely to sit in the first place
    (a worksheet only needs truncating once it's already long, and a user
    is far more likely to be asking about their current cell in a long
    worksheet than a short one that never gets truncated at all) --
    quietly defeating the very feature just added, for precisely the
    worksheets where it matters most. Fixed by locating the marker first
    and centering the kept window on it (falling back to the original
    from-the-start behavior when there is no marker to center on, e.g. no
    real cursor position could be determined). Verified live, not just by
    reasoning about the arithmetic: loaded a real ~15KB synthetic
    worksheet (60 filler cells plus one distinctively-named final cell) in
    a live Xvfb session, moved the cursor to the very last cell
    (Ctrl+End), and confirmed via temporary logging (removed before
    committing, same discipline as the `State_Unauthorized` investigation
    above) that the ~8KB snapshot actually sent still contained both the
    `(CURRENT CELL` marker and the final cell's distinctive text -- on
    unfixed code this exact scenario reproduces the bug (the marker sits
    at the very end of a >8000-character document, so a plain `Left()`
    truncation cuts it every time, deterministically, not intermittently).
  - **Follow-up (2026-09-06): a variable's value can be empty just because
    Maxima hasn't answered yet, not because it's undefined -- and nothing
    told a tool-calling AI that.** `Variablespane::GetWatchedValues()`
    returns whatever the grid's value column currently holds, which starts
    empty the instant a variable is watched and only updates once Maxima's
    asynchronous response for that query actually arrives -- there is no
    "pending" state distinct from "empty," so `read_variables` right after
    `watch_variable` (a completely natural sequence for a tool-calling AI)
    can read back an empty value and have no way to tell "not answered
    yet" apart from "genuinely undefined." Fixed by adding
    `McpTools::MaximaIsBusy()` (`Worksheet::GetWorkingGroup(false) !=
    nullptr` -- something is actively being evaluated right now -- or a
    non-empty `Worksheet::GetEvaluationQueue()` -- more work is queued
    behind it) and surfacing it as `read_variables`' own `maxima_busy`
    field, with both tools' descriptions in `ListTools()` updated to spell
    out the implication (an empty/unchanged value while `maxima_busy` is
    true may just mean "not answered yet").
  - **Follow-up (2026-09-06): `read_cell` had no cap on a single cell's
    output at all, and `read_worksheet`/`read_section` only capped the
    *aggregate* text, not each cell's own contribution to it** -- raised
    by the user directly ("if a cell produced way too much output... do
    we provide the AI with methods to only read the input/only read the
    beginning or only read the end of the output?"), and confirmed exactly
    right by reading `OutputText()`/`CapLength()`: a single pathological
    cell (a huge matrix, a long list, ...) could return an unbounded
    response via `read_cell`, or silently crowd out every other cell's
    info in a `read_worksheet`/`read_section` response (worse: since that
    aggregate cap truncates from the *start*, one huge cell early in the
    document could crowd out not just later cells' content but the
    `(CURRENT CELL...)`/`(THIS CELL HAS AN ERROR)` markers just added
    above too, the same "truncate from the wrong end" bug shape as the AI
    chat sidebar's own truncation, independently). Fixed with a new shared
    `McpTools::TruncateText(text, maxLen, fromEnd, wasTruncated)` used two
    ways: (1) `read_cell` gained optional `output_length`/`output_from_end`
    arguments (defaulting to the full `MAX_TEXT_LENGTH` hard cap, i.e. no
    real-world limit for any normal cell, but never unbounded) plus an
    `output_truncated` result field, so an AI can ask for just a preview or
    specifically the tail (e.g. "did this converge") instead of the whole
    thing; (2) `ReadWorksheet()`/`ReadSection()` now cap *each* cell's own
    output to a smaller `OUTPUT_PREVIEW_LENGTH` (2000 chars) before
    concatenating, with an inline note pointing at `read_cell` when a
    cell's own output was individually truncated -- extracted into the
    same `CellText()` helper the current-cell/error markers already use,
    so the two follow-ups share one place to get right rather than two.
    **A real nlohmann::json gotcha hit while writing this, worth keeping in
    mind for any future tool argument parsing here**: `is_number_unsigned()`
    only returns true for a value that was *already* typed unsigned (e.g.
    text-parsed JSON with no leading minus sign) -- the identical positive
    value built from a plain C++ `int` literal, as any hand-constructed
    `json` object (including every test in `test_McpTools.cpp`) does, is
    classified `number_integer` (signed) instead despite being positive,
    and would have silently failed the check. Used `is_number_integer()`
    instead, which covers both representations, and simply ignores a
    negative value rather than rejecting the whole call.
  - **Follow-up (2026-09-06): "would it be possible to provide it with a
    'login' button or, if not, an info on how to obtain such an API key?"**
    A real "log in" button isn't possible -- same reasoning as the sidebar's
    original design (see the AI chat sidebar entry above): none of these
    four providers offer a legitimate third-party OAuth flow a desktop app
    could use, so there is no automated way to obtain a key. Added the next
    best thing instead: `AiProviderApiKeyUrl(AiProviderKind)` (alongside
    the existing `AiProviderKindName()`/`AiProviderDefaultModel()`) returns
    each provider's own API-key page, shown as a `wxHyperlinkCtrl` under
    that provider's key field in Options -> AI Chat. Also added an "Open
    Options..." button to the sidebar itself, shown only while no provider
    is configured (`ReloadProviderFromConfig()` toggles it), so a user who
    opens the sidebar cold has one click to the exact place that both
    explains the settings and links to where to get a key -- re-posting
    `wxEVT_MENU`/`wxID_PREFERENCES` to `GetParent()`'s event handler rather
    than constructing `ConfigDialogue` itself, reusing
    `MaximaCommandMenus.cpp`'s existing handling (re-reading the config
    file first, applying settings on OK, ...) instead of duplicating any of
    it -- the same "re-post the menu event, don't reimplement the handler"
    idiom `TrayIcon::OnInterrupt()`/`OnExit()` already use elsewhere.
    **A real bug caught by looking at the live screenshot, not by reading
    the code:** the four links were built from one shared format string,
    `wxString::Format(_("Get an %s API key..."), AiProviderKindName(kind))`
    -- grammatically fine for "Anthropic (Claude)"/"OpenAI" (both take
    "an"), but wrong for "Google (Gemini)"/"Qwen (Alibaba)" (both need
    "a"), rendering as the actually-shipped-then-caught "Get an Google
    (Gemini) API key..." Fixed by rewording to "Get an API key for %s...",
    which sidesteps the a/an agreement entirely rather than trying to track
    which of the four provider names needs which article.
  - **Follow-up (2026-09-06): "do we need to hardcode the model names? they
    tend to change over time"** -- a fair challenge, and the user's own
    follow-up ("model auto detection feels like hunting a mechanism that
    catches outdated strings with a mechanism that can get outdated") is
    exactly why this wasn't turned into a live "fetch the model list from
    each provider's API" feature: that mechanism would itself need
    maintaining against four APIs just to answer a question a plain link
    to each provider's own docs already answers, permanently, for free.
    Two changes instead: (1) `AiProviderDefaultModel()`'s Anthropic entry
    now uses `claude-3-5-sonnet-latest`, that provider's own "rolling"
    alias, instead of the dated snapshot `claude-3-5-sonnet-20241022` it
    used to be -- OpenAI/Google/Qwen's defaults (`gpt-4o-mini`/
    `gemini-1.5-flash`/`qwen-plus`) were already un-dated in this same
    sense, so only Anthropic's needed changing; this only pushes the
    staleness problem up one level (from "this exact snapshot got retired"
    to "this whole model line got superseded"), which a plain string
    constant genuinely cannot solve by itself. (2) A new
    `AiProviderModelListUrl()`, shown as a "See current models for %s..."
    link next to each provider's Model field in Options, mirroring
    `AiProviderApiKeyUrl()`'s own link added just above.
    **A real, independent bug found live while verifying this, not by
    reading the code**: after changing Anthropic's default in
    `AiProvider.cpp`, Options kept showing the *old* `-20241022` value
    regardless -- `AiProviderDefaultModel()` turned out to not be called
    from anywhere at all; `Configuration::ResetAllToDefaults()` had its
    *own*, completely separate hardcoded copy of all four model strings
    (`m_aiModelAnthropic = wxS("claude-3-5-sonnet-20241022")` et al.),
    silently disconnected from the function whose entire purpose is to be
    the one place these live. Exactly the class of bug the user's original
    question was worried about, already present in the code before this
    session touched it, just latent until an actual edit exposed it (a
    duplicated constant can drift silently for a long time if nothing
    ever changes the value in only one of its two copies). Fixed by
    having `ResetAllToDefaults()` call `AiProviderDefaultModel()` for all
    four, removing the second copy entirely; re-verified live in Xvfb
    that Options now genuinely shows `claude-3-5-sonnet-latest`.
  - **Follow-up (2026-09-06): a new `search_cells` MCP tool, so an AI can
    find a cell instead of reading the whole worksheet.** Raised by the
    user directly ("would the AI profit from some regex search for input/
    output?") -- a fair gap, since `list_cells`/`read_worksheet` were the
    only way to find "which cell mentions X" and both mean reading every
    cell yourself, exactly the friction the current-cell/error markers
    above already exist to reduce. Added `McpTools::SearchCells()`: a
    `pattern` argument, a plain case-insensitive substring by default, or
    a POSIX extended regular expression with `"regex": true` (`wxRegEx`,
    the same engine `RegexSearch`/`FindReplacePane` already use elsewhere
    in this codebase -- reused directly rather than adding a second regex
    dependency); optional `case_sensitive` and `scope` ("input"/"output"/
    "both", default "both") to narrow a search. Returns each matching
    cell's usual summary (uuid/is_current/has_error, via the existing
    `CellSummary()`) plus `matched_in` and a short `match_snippet` (40
    characters of context on each side of the match, with "..." markers
    where it was cut) -- enough to judge relevance without dumping a
    potentially huge cell's entire text for every hit. Capped to
    `MAX_SEARCH_MATCHES` (50) with a `truncated` flag, the same shape as
    every other capped response in this file, so a pathological pattern
    matching most of a huge worksheet can't turn into an unbounded reply.
    An invalid regex (checked via `wxRegEx::IsValid()` right after
    construction) throws `McpToolError` rather than matching nothing
    silently or crashing. The whole search runs under a `wxLogNull` guard
    -- a bad pattern or a match attempt could otherwise trigger a
    `wxLogXXX` call that, per this file's own "not reliably visible"
    entry, could in the worst case surface as an unwanted modal `wxLogGui`
    popup from what is, from the caller's perspective, an ordinary
    JSON-RPC error response. Verified both ways: `test_McpTools.cpp` gained
    a new SCENARIO (substring/regex/case-sensitivity/scope/invalid-pattern/
    the 50-match cap, 18 new assertions, 86 total in the file) against a
    real headless `Worksheet`, and the transport itself was re-checked live
    in Xvfb via `curl` against a real running MCP server (`tools/list`
    includes it; a plain-substring call, a case-insensitive call, and an
    invalid-regex call all matched expectations) -- catching, in the
    process, an unrelated test-harness mistake of my own (a `.wxm` file
    written for the live check without the required `/* [wxMaxima batch
    file version 1] ... */` header line fails to load with a completely
    silent-to-the-MCP-caller empty worksheet, `Format::ParseWXMFile()`'s
    own recognized-header check rejecting it before a single cell is
    parsed -- not a McpTools bug, just a reminder that a `.wxm` fixture
    needs that exact header, see the existing fixtures under
    `test/automatic_test_files/` for the correct shape).
  - **Follow-up (2026-09-08): AI Chat and Accessibility tabs had no icon of
    their own, and the Anthropic "Get an API key" link sends a subscriber
    to a "buy credits" page -- raised directly by the user.** Both
    `ConfigDialogue.cpp`'s `AddPage()` calls for these two tabs had reused
    tab index 4 (`wxmaximaART_CONFIG_OPTIONS`, a generic gear/wrench glyph)
    with an explicit "no dedicated one exists for this tab" comment -- fixed
    by adding two new hand-drawn SVG icons (`art/config/accessibility.svg`,
    a white stick figure with arms/legs spread inside a blue circle, the
    same motif as GNOME's `preferences-desktop-accessibility`; `art/config/
    ai-chat.svg`, a blue chat bubble with an orange four-point sparkle) and
    wiring them the same way every other `art/config/*.svg.gz` icon already
    is: gzip the plain SVG, add the base name to `art/config/
    CMakeLists.txt`'s `IMAGE_FILES` (bin2h's `string(MAKE_C_IDENTIFIER
    ...)` step sanitizes the hyphen in `ai-chat` to `AI_CHAT_SVG_GZ`
    automatically -- no special-casing needed, same as the pre-existing
    hyphenated `edit-copy-confdialogue`), a new `wxmaximaART_CONFIG_*` art
    ID in `wxMaximaArtProvider.h`/`.cpp`, and two new entries appended to
    *both* of `ConfigDialogue.cpp`'s parallel image-list branches (the
    `wxCHECK_VERSION(3, 1, 6)` `wxBitmapBundle` one and the older
    `wxImageList` fallback) -- missing either branch would silently break
    only pre-3.1.6 wx or only 3.1.6+, so both need touching together.
    Verified live in Xvfb: the AI Chat tab shows the new sparkle-bubble
    icon correctly (screenshotted, matches the standalone-rendered PNG
    pixel-for-pixel in shape); the Accessibility tab's icon could only be
    confirmed via the same standalone SVG render, not live, since this
    sandbox's wxWidgets build has `wxUSE_ACCESSIBILITY` off (the tab is
    `#if wxUSE_ACCESSIBILITY`-gated and never appears in this environment
    at all -- consistent with the sandbox limitation this file's Key
    Subsystems section doesn't otherwise document per-tab, just worth
    knowing if a future session can't find this tab locally either).
    **The second half of the request -- "does a subscription need a
    separate link" -- turned out to have a firm, current (2026) answer,
    not a wrong-link bug**: researched directly (this postdates training
    data, so worth citing rather than assuming) that Anthropic's OAuth
    token for a Claude Pro/Max subscription (`claude setup-token`,
    `sk-ant-oat01-...`) is *rejected* by the Messages API this sidebar
    calls -- it only authenticates against Claude Code/claude.ai -- and
    Anthropic has explicitly banned third-party use of subscription auth
    outside those two surfaces since January 2026. So there is no
    "subscription" link this sidebar could offer instead; a console.
    anthropic.com API key, billed per-token and separately from any chat
    subscription, is the only way this feature (or any raw-Messages-API
    integration) can authenticate, for every account regardless of
    subscription status. This matches the same "no legitimate third-party
    OAuth flow" reasoning the AI chat sidebar's own top-level entry already
    gives for all four providers -- confirmed still true, not just assumed
    unchanged. Fixed by adding a short, upfront note to the AI Chat
    Options panel (right under the existing intro paragraph, not per-
    provider) stating plainly that an API key is billed separately from a
    Claude Pro/Max, ChatGPT Plus or Gemini Advanced plan, so hitting a
    "buy credits" page isn't mistaken for a broken/outdated link. Verified
    live in Xvfb that the note renders under the intro text on the AI Chat
    tab.
  - **Not implemented, and shouldn't be without a separate decision: a
    write/evaluate-capable MCP tool.** Raised and discussed directly with
    the user (2026-09-06): unlike `watch_variable`/`unwatch_variable` (see
    this section's own "why 'read-only except two things' and not
    stricter" note above), an `evaluate_cell`-style tool is not a bounded
    side effect -- Maxima has no sandbox at all around it (it can `system()`
    out to the shell, read/write arbitrary files the user can access, ...),
    so letting an AI evaluate a cell is, in effect, letting it run arbitrary
    code on the user's machine. Per-call user consent does not bound that
    blast radius the way it does for a sidebar-display change. If this is
    picked up later: a consent-gated `insert_cell` that only inserts text
    *without* auto-evaluating it (the user still has to press Enter/
    Shift-Enter themselves) is a meaningfully safer middle ground than a
    tool that evaluates anything -- treat a real `evaluate_cell` as a
    separate, much bigger decision that needs either genuine sandboxing
    around the Maxima process or explicit human-in-the-loop confirmation
    on every single call, not a one-time "make this permanent" toggle.
- **wxAuiManager:** The application uses `wxAuiManager` for its complex layout (sidebars, toolbars, worksheet).
  - **Linux/GTK Timing:** On Linux (especially KDE Plasma with Global Menus), calling `m_manager.Update()` can disrupt the menu bar if it's already attached. This is a known environmental issue in the interaction between wxWidgets, GTK3, and the KDE Global Menu proxy.
    - **Automated Fix:** On systems with wxWidgets <= 3.2 running on KDE, Unity, or with `appmenu-gtk-module` enabled, wxMaxima automatically sets `UBUNTU_MENUPROXY=0` at startup in `main.cpp` to force menus to remain within the window and prevent disappearance.
    - If the menu still disappears, clearing `GTK_MODULES` (e.g., `GTK_MODULES=""`) can also restore local menus.
  - **Center pane must have dock layer/row/position all 0 (found via a
    third-party GTK4 wxWidgets port hitting the assert in
    `framemanager.cpp`'s `wxAuiPaneInfo::IsValid()`; the underlying bug is
    wxMaxima's own, not that port's, and not GTK4-specific).**
    `wxMaximaFrame.cpp` declared the worksheet/console pane with both
    `.Center()` *and* `.Row(2)` -- twice: once in the initial `AddPane()`
    call, and again in the post-`LoadPerspective()` "the loaded perspective
    might be broken, force sane values back" defensive block (both added in
    2019's "Try harder to make broken perspectives work again", apparently
    meant to distinguish the console pane's row from the other sidebars'
    `.Row(1)`). Per `wxAuiPaneInfo::IsValid()`'s own contract, a center
    pane's `dock_layer`/`dock_row`/`dock_pos` must all be exactly 0 --
    `Row(2)` violates that unconditionally. This most likely went unnoticed
    on the officially-supported wxWidgets 3.0.5-3.2.x/GTK3 combination
    either because that older `IsValid()` didn't check this for center
    panes, or because assertions are compiled out (`NDEBUG`) in the release
    builds most users and CI actually run -- `IsValid()`'s own fallback
    return value (`dock_layer==0 && dock_row==0 && dock_pos==0`, i.e.
    `false` here even without the assert firing) suggests this was already
    silently making the pane report itself invalid, just never loudly
    enough to notice. Since only one pane is ever `.Center()`, row ordering
    is meaningless for it regardless (the center pane always fills
    whatever space the docked side panes don't use, irrespective of a row
    number that has no other center pane to be ordered against) -- so
    `.Row(2)` never had any real layout effect to lose. Fixed by dropping
    `.Row(2)` from the initial `AddPane()` call, and by making the
    post-`LoadPerspective()` defensive block explicitly force `.Layer(0)
    .Row(0).Position(0)` (matching its own "overwrite whatever the loaded
    perspective got wrong" stated purpose, rather than only partially
    addressing the same invariant `LoadPerspective()` could equally well
    have clobbered).
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

- **"Copy as HTML" (GH #2265/#2266/#2267) -- `WorksheetExport::
  SelectionToSelfContainedHTML()` / `Worksheet::CopyHTML()`:** a right-click
  context menu item placing a *self-contained* HTML document on the
  clipboard (inline `<style>`, every image as a base64 `data:` URI, no
  external file references at all) -- unlike `ExportToHTML()`'s on-disk
  export, which deliberately writes a separate `.css` file and an
  `_htmlimg/` directory next to the `.html`. Reuses `ExportCodeCell()`/
  `ExportOtherCell()` (the same per-GroupCell renderers `ExportToHTML()`
  uses) completely unchanged, rather than duplicating them, by pointing
  their `imgDir` at a fresh private scratch directory and post-processing
  the result:
  - Images still have to be rendered to a real file (`Svgout`,
    `WorksheetExport::CopyToFile`, `ImgCellBase::ToImageFile`,
    `AnimationCell::ToGif` all only know how to write to a path -- same
    constraint `OutCommon.cpp`'s `PrivateTempDir()` documents for the
    SVG/EMF clipboard flavors), so `MakeSelfContainedHtmlTempDir()` (a
    small helper local to `WorksheetExport.cpp`, deliberately *not* shared
    with `OutCommon.cpp`'s own `PrivateTempDir()`, which lives in a
    different translation unit and isn't exposed via a header) creates one.
  - `InlineImagesAsDataURIs()` then rewrites every `HtmlImageTag()`-
    generated `src="..."` into a `data:` URI by reading the matching file
    back out of that directory and base64-encoding it
    (`wxBase64Encode()`). It doesn't need to know the exact
    `<prefix>_htmlimg/<prefix>_<N><ext>` naming convention
    `HtmlImageTag()` bakes into the HTML text: every writer above always
    saves the real file *flat*, as `<imgDir>/<prefix>_<N><ext>` (no
    `_htmlimg` component -- that only matters for the on-disk exporter's
    *relative* HTML path), so the two share the same basename and a plain
    "look up whatever comes after the last `/`" is sufficient.
  - The stylesheet is generated by the exact same `WriteHtmlStyleSheet()`
    the on-disk exporter uses, but into a `wxStringOutputStream` instead of
    a file -- unlike images, wx has no "only writes to a real path"
    constraint for plain text, so no temp file is needed for the CSS at
    all.
  - The scratch directory is removed (`wxFileName::Rmdir(...,
    wxPATH_RMDIR_RECURSIVE)`) before the function returns, every time --
    the whole point is that nothing the clipboard payload references lives
    outside the payload itself.
  - `MakeSelfContainedHtmlTempDir()` mirrors `PrivateTempDir()`'s graceful
    fallback: if `Dirstructure::UserConfDir()` is empty --
    **always the case in every unit test binary**, since that string is
    only ever populated by the `Dirstructure` member `wxMaxima` itself
    constructs (`wxMaxima.h`'s `m_dirstruct`), and no unit test builds a
    full `wxMaxima` app object -- `wxFileName::CreateTempFileName()` is
    called with a bare prefix instead of a rooted path, which makes it fall
    back to its own default (system) temp location. `test_WorksheetClipboard.cpp`
    checks both possible locations for this reason (see its
    `HtmlClipTempEntryCount()`), not just `Dirstructure::UserConfDir()`.
  - Clipboard format: a single `wxHTMLDataObject` (wx's own portable
    abstraction for `wxDF_HTML`/`CF_HTML`/`text/html`, already used
    elsewhere in this file for the MathML-as-HTML clipboard flavor -- see
    `Worksheet::CopyMathML()`) plus a plain-text fallback, matching that
    same function's `+ wxS('\0')` workaround for a wx string-truncation
    quirk. Unlike RTF (GH #2264, immediately above/below this entry
    depending on merge order), `wxHTMLDataObject` needed no equivalent
    three-format workaround: wx already handles the platform-specific
    `CF_HTML` wrapper header internally.
  - Verified two ways: `test_WorksheetClipboard.cpp`'s new SCENARIOs pin
    the structural invariants (inline `<style>`, no `<link
    rel="stylesheet">`, no `src="` other than `data:`, scratch-dir cleanup,
    null-range safety) directly against `SelectionToSelfContainedHTML()`
    without touching the real clipboard (same reasoning as the rest of
    that file). Separately, a **live** end-to-end check in a real Xvfb +
    fluxbox session (a window manager is required here -- unlike most of
    this file's other Xvfb checks, right-clicking to open a context menu
    and navigating it needs real window-manager focus/stacking behavior,
    which a bare `Xvfb` without any WM doesn't provide) drove the actual
    app: typed `wxdraw2d(explicit(sin(x),x,0,10))$`, evaluated it for a
    real rendered plot, right-clicked the group cell, clicked "Copy as
    HTML", and read the X clipboard back with `xclip -o -selection
    clipboard -t text/html` -- confirming a real `data:image/png;base64,`
    payload, zero non-`data:` `src=` attributes, zero leaked `htmlclip`
    temp-path fragments, and no leftover scratch directory afterward.

- **"ASCII maths" style not actually defaulting to a monospace font -- two
  independent bugs stacked, and the second one made the first one look
  unfixable while debugging it.** Maxima's own ASCII-art 2D printer (see
  "ASCII-art 2D display" further down this file) pads multi-line output
  with literal spaces assuming every character is the same width, so
  `TS_ASCIIMATHS` needs a genuinely fixed-pitch font, not just one that
  happens to look monospace-ish.
  1. `Styles::SetDefaults()` used to construct
     `wxFont(10, wxFONTFAMILY_MODERN, ...)` and use whatever face name that
     resolved to -- already flagged in the code as `// TODO It's a fat
     chance that this font actually will be monospace.` On this sandbox's
     GTK/Pango setup it resolved to plain "Sans", confirmed via
     `wxFont::IsFixedWidth()` returning false. Fixed with a new
     `MakeMonospaceFont()` helper (`Styles.cpp`, anonymous namespace) that
     tries a list of well-known monospace font names through
     `wxFontEnumerator::IsValidFacename()` first (an actual "is this
     installed" check, not a family hint) and only falls back to the loose
     `wxFONTFAMILY_TELETYPE` hint if none of them are installed.
  2. Fixing #1 alone changed nothing observable, and re-verifying it via a
     fresh `Configuration cfg;` kept showing the OLD "Sans" default no
     matter how the fix was re-checked -- confirmed with a temporary trace
     across `Styles::SetDefaults()` (correctly computed "DejaVu Sans Mono")
     and immediately after `Configuration::ReadConfig()` (back to "Sans").
     Root cause: `Style::Read()` (`cells/TextStyle.cpp`) had an `else
     SetFontName(wxNORMAL_FONT->GetFaceName())` branch that fired whenever
     a style's `fontname` key was missing from the persisted config --
     which is the *common* case, true for every user who never explicitly
     changed a font in Options. Since `Configuration::ReadConfig()` always
     calls `ReadStyles()` (which calls `Styles::Read()` for every style)
     immediately after `InitStyles()`/`SetDefaults()`, this silently
     clobbered every style's just-picked default font -- not only
     `TS_ASCIIMATHS`'s -- with one generic UI font, on every fresh install.
     Every *other* field in `Style::Read()` already matched its own
     documented contract ("Only touches the attributes that were
     successfully read. Remaining attributes are unchanged.") by simply
     having no `else` branch at all; only `fontname` violated it. Fixed by
     deleting the `else` branch, matching the pattern already used by
     every sibling field in the same function.
  3. **This second bug is also why a stale `wxConfig` file can permanently
     hide a fixed default during debugging, and cost real time here before
     being recognized.** `wxConfig::Get()` in an ad hoc unit-test binary or
     a manually-run app resolves to a real file under `$HOME` (e.g.
     `~/.test_StyleConfigRoundtrip` for a bare `wxApp`-only test binary
     with no explicit app name set, or `~/.config/wxMaxima.conf` for the
     real app) that *persists across separate process invocations* --
     unlike most other test state, which resets every run. A single
     earlier run (in this case, an interactive Xvfb session used to verify
     the unrelated "Copy as HTML" feature, and this test binary's own
     pre-fix runs) had already written the old, wrong "Sans" value to that
     file; every subsequent run silently read it back regardless of what
     the current code's `SetDefaults()` computed, exactly reproducing bug
     #2 from a completely different (external, filesystem) cause. Confirmed
     by grepping `$HOME` for stray `fontname=Sans` entries and deleting the
     files; the fix then verified correctly on the first truly clean run.
     `test_StyleConfigRoundtrip.cpp`'s new "TS_ASCIIMATHS defaults to..."
     SCENARIO now calls `wxConfig::Get()->DeleteAll()` before constructing
     its `Configuration`, specifically so it can't be shadowed by this same
     class of contamination on a re-run or a persistent CI runner -- don't
     drop that call when touching this test.
  Regression coverage: `test_StyleConfigRoundtrip.cpp` gained two SCENARIOs
  -- one pinning that a fresh `Configuration`'s `TS_ASCIIMATHS` font passes
  `wxFont::IsFixedWidth()`, and one pinning `Style::Read()`'s contract
  directly (a sentinel font name survives a `Read()` against a config with
  no `fontname` key for that style, using a `wxFileConfig` constructed with
  `style=0` so it never touches disk at all -- the in-memory-only
  hermeticity this whole investigation shows is worth having).

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

- **GH #2278 -- selection-rectangle width can slightly differ from the
  rendered text's actual width, investigated but NOT YET FIXED (2026-08).**
  Root cause confirmed by reading the measurement/draw code side by side, not
  guessed: `EditorCell` computes horizontal position two structurally
  different ways that both amount to "measure pieces separately and sum
  them," and the pieces don't line up the same way in both places.
  `EditorCell::Draw()` (`EditorCell.cpp` ~line 1042) paints text **per
  `StyledText` token** -- each token gets its own `dc->GetTextExtent()` /
  `dc->DrawText()` call, and `TextCurrentPoint.x += width` accumulates the
  *pen* position as the sum of those independently-shaped token widths, so
  any kerning or (for a contextual script) glyph-shape change that would
  normally happen *across* a token boundary is never applied -- the two
  neighboring glyphs are shaped in total isolation from each other.
  `EditorCell::GetLineWidth()` (used by `PositionToPoint()` for an
  ordinary, single-direction line) reimplements that same per-token
  accumulation independently (`lineWidth += GetTextSize(snippet).GetWidth()`,
  with the final partial token measured via `snippet.Left(pos)`) -- so for a
  single-direction line the two at least agree with each other, both being
  equally kerning-blind at token boundaries. The bidi work
  (`MixedDirectionOffset()`, added for mixed-direction line support) does
  something different: it measures each `BidiRun` **as one whole
  substring** via `MeasureTextWidth()` (`m_text.SubString(...)`), which
  *does* let the font shape it correctly -- kerning pairs and (critically,
  for Arabic-like scripts) contextual join forms all resolve the way they
  would if the run were drawn as a single unit. That's a strictly *more*
  accurate measurement of what the font would produce for that span, but
  it's answering a different question than what `Draw()` actually paints
  (per-token, unshaped-across-boundaries) -- so on a mixed-direction line,
  `MarkSelection()`'s selection rectangle (built from two
  `MixedDirectionOffset()`-derived `PositionToPoint()` calls, `EditorCell.cpp`
  ~line 852-877) can come out a few pixels narrower or wider than the glyphs
  `Draw()` actually painted for that same span, especially where a token
  boundary falls in the middle of a script that reshapes heavily by context.
  **This is not a simple "measures per character instead of per whole
  string" bug** (that specific hypothesis, which is how the issue itself is
  worded, doesn't survive reading `MeasureTextWidth()` -- it already
  measures its input as one `GetTextExtent()` call, not character by
  character); it is a *disagreement between two independently-correct-looking
  but differently-grained measurement strategies*, one of which (`Draw()`'s
  per-token painting) is the one that actually determines what's on screen
  and should be the one every other measurement is judged against.
  A real fix needs one of: (a) make `MixedDirectionOffset()` sum cached
  per-`StyledText`-token widths the same way `GetLineWidth()`/`Draw()` do
  (loses the bidi work's kerning-accuracy improvement, but makes the
  selection rectangle match pixel-for-pixel what's actually drawn -- the
  correct alignment target), or (b) make `Draw()` paint each maximal
  same-direction run as a single `DrawText()` call instead of per token
  (recovers the accuracy `MixedDirectionOffset()` already computes, but
  touches the same per-token color-styling/tab/indent-char logic that
  `EditorCell::Draw()`'s text loop handles all at once, and duplicated across
  `MarkSelection()`'s own line-splitting loop). Deliberately **not**
  attempted in this pass: both routes touch code that the 2026-08 bidi work
  (cursor placement, click-to-position, arrow-key navigation -- see
  "Extend bidi fix to caret placement..." in git log) already spent real
  effort getting right, and a "few pixels off" selection-rectangle glitch
  does not obviously justify the regression risk of changing it blind. Route
  (a) is probably the lower-risk one to attempt first: `StyledText` doesn't
  currently track its own `m_text` character offset, so the main work is
  adding/deriving that mapping (tokens are already emitted in `m_text`
  order, so it is a running-counter walk, not a search) rather than touching
  any of the already-stabilized cursor/click bidi logic itself.

- **GH #2274 -- Windows Dark Mode only affecting the worksheet, not the rest
  of the interface. Root cause found by reading wxWidgets 3.3's own MSW
  source (`src/msw/darkmode.cpp` -- fetched directly, this sandbox only has
  wxWidgets 3.2 installed and cannot compile or run the `wxCHECK_VERSION(3,
  3, 0)` code path at all, so this could not be tested live and needs a
  Windows report to confirm) -- fixed on a "the mechanism is exact, but
  unverified on the actual platform" basis, the same caution a blind fix
  deserves.** `main.cpp`'s `MyApp::OnInit()` already had a comment
  explaining that `ApplyAppearanceToApp()` (which calls
  `wxTheApp->SetAppearance()`) has to run "before the first top-level window
  is created further down" for Windows to pick it up -- but the very first
  thing `OnInit()` actually did, several hundred lines *earlier*, was
  `m_logWindow = new wxLogWindow(...)`. `wxLogWindow`'s constructor
  unconditionally does `m_pLogFrame = new wxLogFrame(...)` -- a real
  `wxFrame` -- regardless of its `show` argument; only `Show()` afterwards is
  conditional (confirmed by reading `src/generic/logg.cpp` directly, not
  assumed from the class name). A `wxFrame` registers itself in the global
  `wxTopLevelWindows` list at construction, not at `Show()` time. wx 3.3's
  MSW `wxApp::SetAppearance()` opens with `if (!wxTopLevelWindows.empty() ||
  gs_appMode != AppMode_Default) return AppearanceResult::CannotChange;` --
  so by the time `ApplyAppearanceToApp()` ran, `wxTopLevelWindows` already
  held the (still-hidden) log window's frame, and `SetAppearance()` silently
  gave up every single time, on every startup, regardless of what the
  in-code comment intended. This is MSW-specific: GTK's implementation has no
  such "only before any window exists" restriction, which is exactly why the
  maintainer's own diagnostic logging (added just before this fix, still
  worth keeping) showed `AppearanceResult::Ok` on their Linux dev machine --
  the bug was never visible there, only on the platform it was actually
  reported on. Since the worksheet's own colors come from `Configuration`,
  entirely independent of `wxApp::SetAppearance()`, it always reflected the
  chosen appearance correctly regardless of this bug -- exactly matching the
  reported symptom ("only the worksheet is in dark mode"). Fixed by moving
  `m_logWindow`'s construction to *after* the `ApplyAppearanceToApp()` block,
  the smallest change that gets a genuinely empty `wxTopLevelWindows` at the
  point `SetAppearance()` runs, rather than trying to move the (config-file-
  dependent, command-line-parsing-dependent) appearance-reading code earlier
  instead. Checked for anything else constructing a top-level window before
  that point (nothing does; `RepairFileAssociations()`, the only other
  Windows-specific startup step ahead of it, only touches the registry) and
  for any code between the old and new construction points that dereferences
  `m_logWindow` before it exists (one `wxLogMessage()` call, which safely
  falls through to whatever the default wx log target is when no custom one
  is installed yet, no different from any `wxLogMessage()` that already ran
  even earlier in `OnInit()`). Verified on Linux: builds clean, a live Xvfb
  session starts up normally end to end (Maxima connects, worksheet is
  usable), and View -> Toggle log window still successfully creates and
  toggles the (real, `xdotool`-visible) log window frame after being moved --
  confirming the reordering itself doesn't break anything, though the actual
  dark-mode effect this targets can only be confirmed by someone running a
  build on real Windows. A prior "speculative go" at this same issue
  (changing `wxTheApp->SetAppearance()` to a hypothetical
  `wxApp::SetAppearance()` static call) was reverted for a compile error --
  don't repeat that: `SetAppearance()` is an ordinary (non-static) `wxApp`
  member function.
  - **Follow-up (found live, 2026-08): that "one `wxLogMessage()` call...
    safely falls through to whatever the default wx log target is" note
    above undersold the actual consequence -- "the default wx log target"
    when no custom one is installed is `wxLogGui`, and `wxLogGui` pops up a
    real modal dialog for every message, not just errors.** Since
    `ApplyAppearanceToApp()`'s own diagnostic logging (`"Appearance was
    successfully changed."` etc., the maintainer's pre-existing debug
    logging this whole fix was built around keeping) runs from `OnInit()`
    at the exact point this fix moved `m_logWindow`'s construction *after*,
    every normal startup now hit that fallback and popped up a modal
    dialog reporting a mere debug-level message -- worse than the original
    bug this section fixes, and directly caused by it (the log call was
    already there before this fix; it only started using the no-custom-
    target fallback once `m_logWindow` moved later). Fixed by NOT deferring
    `SetAppearance()` itself (still has to run before the log window's
    frame exists, unchanged) but only deferring when its result gets
    logged: `ApplyAppearanceToApp()` gained a `logImmediately` parameter
    (default `true`, preserving `wxMaxima::ConfigChanged()`'s existing
    runtime call site unchanged, since a log target already exists by
    then) and now returns the message as a plain `wxString` instead of
    logging it directly. `main.cpp`'s startup call passes
    `logImmediately=false` and holds onto the returned string in a local,
    then logs it itself with a plain `wxLogMessage()` right after
    `m_logWindow` is constructed -- by which point a real target exists and
    the message goes to the (hidden-by-default) log window exactly like
    every other `wxLogMessage()` call in this app, instead of popping up
    a dialog. Kept the return type as a version-independent `wxString`
    (empty when there's nothing to report) rather than the version-gated
    `wxApp::AppearanceResult` enum itself, so the function's declared
    signature in `wxMaxima.h` stays valid pre-3.3 too, matching how the
    rest of this function is already guarded. Live-verified on Linux (this
    sandbox's wxWidgets 3.2.4 means the whole `#if wxCHECK_VERSION(3, 3,
    0)` body -- and therefore this exact bug -- is unreachable here,
    `ApplyAppearanceToApp()` always takes the no-op `#else` branch; the fix
    is a mechanical, easily-verified-by-reading change, not something this
    sandbox's build could exercise): builds clean, and a live Xvfb startup
    shows no unexpected modal dialog (only the normal, unrelated "Did you
    know?" startup tip, `Show tips at startup` -- present with or without
    this change).

- **`wxmaxima_version_string` CI test failing on the minGW Windows runner on
  essentially every push since 2026-08-15 -- STILL UNSOLVED (2026-09-06).
  The `_dup2()` fix below was tried, pushed, and the very next CI run
  reproduced the exact same failure -- the struct-copy theory is DISCONFIRMED,
  not just unverified. Read this whole entry before touching
  `BindStdStreamToParent()` again; don't re-derive or re-attempt the
  struct-copy theory from scratch.** The test runs `wxmaxima --debug
  --logtostderr --pipe --version` and expects stdout to match `wxMaxima
  <VERSION>.*`; it consistently fails with "Required regular expression not
  found" while the process still exits 0 -- no crash, just no (or wrong)
  captured output -- on a job that otherwise builds and passes every other
  test cleanly (including `wxmaxima_version_returncode`, the same command
  with only the exit code checked, which passes every time -- so the process
  really does run to completion normally). `main.cpp` already has a large
  Windows-only block explaining why this needs special handling at all:
  wxMaxima is a `WIN32`-subsystem binary (`add_executable(wxmaxima WIN32
  ...)`), so it has no stdio wired up by default, and `RedirectStdioToParent()`
  (`BindStdStreamToParent()`) exists specifically to bind `stdout`/`stderr`/
  `stdin` onto whatever the parent process gave it (an inherited pipe, as
  ctest sets up, or an attached console).
  - **Attempt 1 (2026-09-06, commit `3f8fd15`): the struct-copy theory --
    DISCONFIRMED.** The original code did:
    ```cpp
    int fd = _open_osfhandle((intptr_t)handle, _O_TEXT);
    FILE *opened = _fdopen(fd, mode);
    *stream = *opened;  // stream is the global stdout/stderr/stdin pointer
    ```
    The theory: `*stream = *opened` is a shallow struct copy of a `FILE`
    object allocated at a *different* address onto the CRT's real,
    globally-visible `stdout`/`stderr` object, and might leave CRT-internal
    bookkeeping (a per-stream lock, internal buffering-state pointers) keyed
    to the wrong address. Replaced with `_dup2(fd, _fileno(stream))`, the
    documented way to repoint an *existing* stream's descriptor without
    fabricating a second `FILE` object. **The next real CI run on this exact
    commit reproduced the identical failure** (133/134 passed, the same
    single `wxmaxima_version_string` failure, same shape) -- so this was not
    the bug, or at least not the whole of it.
  - **Follow-up verification (2026-09-06, same day, no code change): built a
    genuine Wine-based test harness and confirmed the disconfirmation
    directly, not just via CI's word for it.** This sandbox has no Windows,
    but `apt-get install wine64` (plus, once that turned out to need it,
    `dpkg --add-architecture i386 && apt-get install libgd3:i386 wine32:i386`
    to unblock a dependency chain, and `g++-mingw-w64-i686-win32` since this
    Wine build turned out to only support 32-bit prefixes -- `WINEARCH=win64`
    silently downgrades itself with a warning rather than erroring, and a
    genuine 64-bit `x86_64-w64-mingw32`-built exe fails with a misleading
    "Bad EXE format" / ShellExecuteEx error under it, which looks like a
    corrupt binary but is actually just "wrong bitness for this prefix") gets
    a working Windows environment good enough to actually *run* a
    GUI-subsystem (`-mwindows`) cross-compiled exe with its stdout genuinely
    piped, the same shape ctest uses. A minimal standalone repro
    (`WinMain()` calling the exact old-vs-new `BindStdStreamToParent()` body
    verbatim, then one `fprintf(stdout, ...)` + `exit(0)`) was built in both
    variants (`-DUSE_STRUCT_COPY` old, plain new) with `i686-w64-mingw32-g++
    -mwindows -static` (static linking needed too -- a dynamically-linked
    exe fails differently again, `libgcc_s_dw2-1.dll not found`, status
    `c0000135`, since nothing copies MinGW's runtime DLLs next to a
    standalone cross-compiled exe) and run under `wine` with `DISPLAY`
    pointed at a real `Xvfb`, both as `wine exe.exe > file` (matches a shell
    redirect) and `wine exe.exe | cat` (matches ctest's actual anonymous-pipe
    shape more closely). **Every one of the four combinations (old/new x
    file/pipe) printed the expected line correctly and exited 0** -- the
    struct-copy version is just as capable of getting output through a piped
    GUI-subsystem stdout as the `_dup2()` version, in this controlled,
    Wine-level test. This means the bug was very likely never in
    `BindStdStreamToParent()`'s specific mechanism at all (on real Windows it
    could still theoretically differ from Wine's behavior here, but combined
    with the CI re-failure on the actual fix, treat that as the weaker
    possibility) -- something else in wxMaxima's *actual* Windows startup
    path is interfering with stdout between `RedirectStdioToParent()`
    succeeding and the `--version` branch's `Printf()` call, in a way this
    isolated repro (no wxWidgets, no threads, no sockets) doesn't reproduce.
    The `_dup2()` change itself is still kept -- it is still the more
    correct, standard way to repoint a stream, and does no harm -- but it
    should no longer be described as a fix for this issue anywhere in this
    repo; it isn't one.
  - **Most promising remaining lead, NOT YET INVESTIGATED further: the
    timeline correlation with GH #2274's fix (commit `3f5e895`, landed
    2026-08-17, two days after this test's "since 2026-08-15" onset).** That
    change moved `m_logWindow`'s construction (a real, unconditionally-
    constructed `wxFrame` under the hood, see the GH #2274 entry above) from
    immediately after `RedirectStdioToParent()`/`wxMessageOutput::Set(...)`
    to several hundred lines later, after `wxSocketBase::Initialize()`,
    `wxArtProvider::Push`, `MaximaProcessManager::SetupTerminationHandlers()`,
    the full `cmdLineParser.Parse()` call, `wxConfig::Set(...)`, and
    `ApplyAppearanceToApp()` -- all of that now runs *before* any top-level
    window (even a hidden one) exists, where before it ran after. None of
    those individually look like an obvious stdout-breaker on paper, but one
    of them is the actual differentiator between this investigation's
    minimal repro (which never reproduced the bug) and the real app (which
    does) -- worth bisecting properly (a real Windows machine, or a full
    wxWidgets-for-MinGW build run under Wine the same way as this session's
    minimal repro, reproducing the exact `OnInit()` sequence up to and
    including the `-v` branch) rather than guessing which specific call is
    responsible. A full wx+wxMaxima MinGW build was judged too large an
    undertaking for this pass (wxWidgets alone takes CI real build minutes
    from source) but is the logical next step if this is picked up again.
    **Do not re-attempt the struct-copy-vs-dup2 theory a third time** -- it
    has now been disconfirmed twice, once by real CI and once by a
    controlled, reproducible Wine-based test built specifically to check it.
  - **Follow-up session (2026-09-06): three more concrete theories tested
    and disconfirmed with targeted Wine repros, and one genuinely new,
    unexamined fact surfaced -- still unsolved, but the search space is
    narrower.** Prompted by the user directly asking why a "theoretically
    deterministic" system could be flaky at all -- a fair challenge, and
    the answer turned out to be "several plausible mechanisms exist and
    were checked one at a time," not "there's an obvious smoking gun."
    1. *Richer repro, closer to the real `OnInit()` sequence.* The
       previous session's Wine repro was a bare `fprintf` -- this pass
       built a second repro (no wxWidgets, still cross-compiled
       `-mwindows -static` MinGW, run under the same 32-bit Wine prefix)
       that adds back three things the minimal repro omitted: a real
       native `HWND` via `CreateWindowExA`/`RegisterClassA` (mimicking
       `wxLogWindow`'s always-real `wxFrame`), a real Windows registry
       touch via `RegCreateKeyExA`/`RegCloseKey` (mimicking
       `wxConfig::Set(new wxConfig(...))`), a `LoadLibraryA("uxtheme.dll")`
       call (mimicking `ApplyAppearanceToApp()`'s likely internal use of
       that DLL on wx >= 3.3, per GH #2274's own investigation), and
       several `fprintf(stderr, ...)` calls interleaved with the above
       (mimicking `--logtostderr`'s `wxLogChain(new wxLogStderr)` plus the
       several real startup log lines the actual app emits before reaching
       `-v`) -- then the real `--version`-equivalent `fprintf(stdout,
       ...)` and `exit(0)`. Piped through `| cat` (this sandbox has no
       Windows/real ctest to test against directly) for 150 runs: **0
       failures.** This doesn't clear any of those four mechanisms
       individually, but it does rule out this *specific combination* of
       them as sufficient on its own to reproduce the bug under Wine.
    2. *A new, specific hypothesis about `_dup2()` itself, tested and
       disconfirmed decisively (not just "didn't reproduce in N runs" --
       a targeted mechanism check).* Reasoned through
       `BindStdStreamToParent()`'s exact sequence
       (`_open_osfhandle()` -> `_dup2(fd, target)` -> `_close(fd)`) and
       asked: does `_dup2()` actually duplicate the underlying Win32
       `HANDLE` (POSIX-correct: the source and target fds become
       independently closeable), or does it just copy the raw handle
       *value* into the target fd's slot -- in which case `_close(fd)`
       right after would close the ONE shared handle out from under the
       target too, a genuine use-after-close bug whose symptom (silently
       broken or misdirected writes) would exactly fit the observed
       failure? Wrote a standalone micro-test (`dup2test.cpp`, this
       session's scratchpad) around a real `CreatePipe()`: opens the
       write end via `_open_osfhandle()`, `_dup2()`s it onto `stdout`,
       closes the source fd exactly like the real code does, then probes
       with `_get_osfhandle()` before/after and an actual
       `fprintf(stdout, ...)` read back via `PeekNamedPipe`/`ReadFile` on
       the pipe's read end. Result: `_dup2()` produced a genuinely
       *independent* duplicated handle (different raw value from the
       source), closing the source left the target's handle valid, and
       the probe write landed correctly in the pipe. **This specific
       failure mode is ruled out** -- at least as Wine's `msvcrt`
       reimplementation behaves; real Windows' actual `ucrtbase.dll`/
       `msvcrt.dll` could in principle differ from Wine's compatibility
       shim here, which is the one caveat this test can't close without
       real hardware.
    3. *A newly-surfaced, previously-undocumented fact: the failing test
       runs under real `ctest -j 2` parallelism on the actual CI job,*
       not serially. `.github/workflows/compile_windows.yml`'s "Run
       integration tests" step (where `wxmaxima_version_string` actually
       runs -- it isn't part of the earlier `-L unittest` step, which is
       serial) is `ctest -LE "unittest|needs_posix" -E "multithreadtest"
       --repeat after-timeout:2 -j 2 --output-on-failure`. Nothing in the
       investigation up to this point had considered concurrent test
       execution as a factor at all. `--repeat after-timeout:2` only
       retries a *timed-out* test, not this failure mode (exit 0, wrong/
       missing content) -- consistent with every observed failure being a
       first-try, non-retried one. Tested by running pairs of the richer
       repro (#1 above) concurrently under Wine, 60 pairs (120 runs): **0
       failures.** Real contention for CPU/scheduler time between two
       genuinely concurrent Windows processes is very plausibly still a
       necessary ingredient this sandbox's Wine environment (limited
       parallelism, no real multi-core Windows scheduler, no antivirus/
       Defender real-time scanning, none of the other background load a
       shared GitHub Actions Windows runner carries) simply can't
       reproduce regardless of how faithfully the *code path* is copied --
       which would also explain why every purely-mechanism-level Wine
       repro across two sessions has failed to reproduce this, while the
       bug remains completely reliable-in-its-unreliability on the actual
       CI runner. **This `-j 2` fact belongs in any future bisection
       attempt** (e.g. a real Windows machine specifically running the
       full integration suite with `-j 2` many times, not just the single
       `wxmaxima_version_string` test in isolation, to preserve whatever
       contention the parallel scheduling introduces) and should not be
       re-discovered from scratch.
    **Net effect of this session's pass**: three more plausible, concrete
    mechanisms individually checked and ruled out (or at least not
    reproduced) with real, targeted tests rather than guesses -- the
    struct-copy/dup2 theory, the richer-repro combination, the
    shared-handle-after-close theory, and the bare concurrent-execution
    theory are all now either disconfirmed or failed to reproduce under
    Wine. The bug is still open. The most defensible remaining position:
    whatever the actual trigger is, it very likely needs *real Windows*
    (not Wine) under *real contention* (not a quiet, idle sandbox) to
    reproduce at all -- which matches this bug's own history of being
    essentially unreproducible everywhere except the actual CI runner.
  - **Follow-up (2026-09-06): a full wxWidgets-for-MinGW build under Wine
    was judged, again, too large an undertaking to attempt blind for
    another round of a theory that keeps failing to reproduce here --
    deployed a cheap, real-CI experiment instead of another Wine repro.**
    This sandbox has no prebuilt wxWidgets-for-MinGW package available via
    apt, and building it from source cross-compiled would be a genuinely
    large, open-ended undertaking (the same reasoning that already shelved
    this exact idea in the previous entry) for a lead -- concurrent `ctest
    -j 2` scheduling -- that a synthetic two-process Wine repro *already*
    failed to reproduce (60 pairs, 0 failures, see above), meaning even a
    full real build under Wine might well repeat that same non-result
    without actually settling anything, since Wine's scheduler is not a
    stand-in for a real, loaded Windows CI runner's contention either way.
    Rather than spend that build effort on another likely-inconclusive Wine
    experiment, added `RUN_SERIAL TRUE` to `wxmaxima_version_string`
    itself (`test/CMakeLists.txt`) -- CTest never schedules a `RUN_SERIAL`
    test concurrently with anything else, regardless of `-j`. This tests
    the contention theory directly against the one environment that has
    ever actually reproduced the bug (the real Windows CI runner) instead
    of against another simulation of it. It is a real, if indirect,
    experiment, not a guess dressed up as one: if several real CI runs
    with this in place stop failing, that is genuine evidence contention
    is a necessary ingredient (worth then hunting for what state two
    concurrent `wxmaxima --version` processes could actually contend
    over -- a shared named object, a registry key, a temp file, ... --
    none of which this investigation has looked at yet); if it still
    fails under `RUN_SERIAL`, that rules out simple ctest-level contention
    cleanly and cheaply, no build required either way. Deliberately left
    `wxmaxima_version_returncode` (the sibling test checking only the exit
    code, which has never once been observed to fail) untouched -- this
    experiment targets only the test that actually exhibits the bug.
    **Revert this single `RUN_SERIAL TRUE` if a future session confirms it
    made no difference** -- it is an experiment to gather evidence, not a
    fix, and should not linger indefinitely presented as one.
    **First two real data points (2026-09-06, PR #2294, commit `54598a7`):
    both of the first two real CI runs with `RUN_SERIAL TRUE` in place
    still failed with the exact same symptom** ("Required regular
    expression not found", 133/134 tests passed, same single test). This
    is meaningful, if not yet conclusive: simple ctest-level self-
    concurrency (this test racing some *other* ctest job for CPU/scheduler
    time within the same `-j 2` invocation) does not look sufficient on
    its own to explain the failure, since removing exactly that kind of
    contention for this one test didn't stop it from failing twice in a
    row. Two important caveats before concluding contention is irrelevant
    entirely: (1) `RUN_SERIAL` only keeps *this* test from running
    concurrently with anything else -- it does nothing about contention
    for the *machine's* resources in general (another GitHub Actions
    Windows runner's own background load, antivirus scanning, etc. are
    unaffected), so this doesn't rule out contention as a class, only
    ctest's own internal `-j 2` scheduling specifically; (2) two data
    points is still a small sample against a failure this reports as
    "essentially every push" -- worth accumulating more real CI runs
    before drawing a firm conclusion. Do not spend further Wine-repro
    effort chasing plain ctest-level contention specifically based on this
    -- that narrow mechanism now has two real, direct data points against
    it, which outweighs the earlier from-first-principles Wine simulation
    that failed to reproduce it either way.
    **Third data point (2026-09-07, commit `45dc8ff`, a genuinely separate
    push -- not a re-run of the same commit): failed again, identically.**
    Three for three real CI runs with `RUN_SERIAL TRUE` in place, all
    failing the same way. Treat plain `ctest -j 2` self-concurrency as
    reasonably disconfirmed as *the* cause at this point -- not worth a
    fourth confirmatory run. `RUN_SERIAL TRUE` is left in place (it is
    harmless either way -- this test is fast and gains nothing from
    parallelism -- and the finding itself is worth keeping visible on the
    test), but stop describing it as an open experiment still gathering
    data; it has its answer. **Correction to this entry's own earlier
    wording**: "real Windows hardware with `rr`" was never a coherent next
    step and should not have been written here -- `rr` (Mozilla's
    record-replay debugger) is Linux-only and has no Windows port at all;
    that line was carried over by mistake from this file's unrelated
    `tutorial_10Minutes` entry, which is a genuinely different, Linux-side
    Maxima flake where `rr` really is the right tool. Separately, "real
    Windows hardware" was also the wrong framing of the actual blocker:
    GitHub Actions' `windows-latest` runner already *is* real Windows --
    the actual limitation this investigation has run into is that runner's
    non-interactivity (no way to attach a live debugger to the exact
    process instance that reproduces the bug), not a lack of a genuine
    Windows target to test against. The next real lead, if this is picked
    up again, is either (a) a live interactive session on the actual runner
    via `mxschmitt/action-tmate` (or equivalent), or (b) targeted
    diagnostic tracing shipped in the binary itself and captured
    automatically as a CI artifact -- see the follow-up entry immediately
    below, which does (b) -- or bisecting the GH #2274 startup-reordering
    commit (`3f5e895`) directly on the real runner. Not another synthetic
    Wine repro of ctest-level contention specifically -- that lead is now
    closed.
  - **Follow-up (2026-09-07): "does this only fail on PRs?" -- checked
    directly, and no.** A fair question to ask given how much of this
    investigation happened on PR branches -- but `compile_windows.yml`
    has no `pull_request` trigger at all (`on: [push, workflow_dispatch]`
    only), so every run, PR-associated or not, is a plain `push` event
    checking out the exact pushed commit -- never a synthetic PR-merge-ref
    checkout, so there is no mechanism by which "PR" vs. "not PR" could
    change what gets built or tested. Confirmed empirically too: pulled
    the last 20 `compile_windows` runs on `main` itself (real merges/
    direct pushes, back to 2026-08-17 -- the flake's own documented onset)
    via `list_workflow_runs` (`branch: "main", event: "push"`) and *every
    single one* shows `conclusion: failure`; spot-checked one directly
    (the #2292 merge, run `34058521313`, job `101554735776`) and it's the
    identical signature -- `99% tests passed, 1 tests failed`,
    `71 - wxmaxima_version_string (Failed)`. So this fails at the same
    rate on `main` as on every PR branch; there is no PR-specific
    mechanism to chase. The likely reason it *feels* PR-specific: CI
    status is mostly surfaced and acted on via a PR's own checks tab,
    while `main` pushes happen less often and nothing blocks on their
    failure once the merge has already landed, so those failures are
    easier to not notice -- a visibility/sampling effect, not a real
    behavioral difference between the two trigger paths. Don't re-open
    "PR-specific" as a lead without new evidence.
  - **Follow-up (2026-09-07): shipped real diagnostic tracing in the binary
    itself, gated behind an env var, to capture actual runner-side evidence
    on the next CI run instead of another Wine simulation.** Prompted by
    the user's own correct pushback on "go get real Windows hardware" (see
    the correction two entries above) -- the runner is already real
    Windows, and every debugging tool that needs live interactivity (a
    real debugger, `rr`, hardware watchpoints) is unavailable here anyway,
    so the only way to learn something new without an interactive session
    is to have the actual failing process record its own evidence and hand
    it back as a CI artifact. Two previously-unexamined facts motivated
    what got instrumented, specifically: (1) every prior Wine repro across
    multiple sessions tested a bare `fprintf(stdout, ...)` -- never the
    *actual* code path `--version` uses, which is
    `wxMessageOutput::Get()->Printf(...)` through a `wxMessageOutputStderr`
    object (`wxMessageOutput::Set(new wxMessageOutputStderr(stdout))`,
    `main.cpp`, called once early in `OnInit()`); (2) `cmdLineParser.Parse()`
    (`wxCmdLineParser`, a real library call) runs between that `Set()` call
    and the `-v` branch, and has never been traced through either. Added a
    small, self-contained diagnostic block to `main.cpp` (Windows-only,
    anonymous namespace, immediately before `BindStdStreamToParent()`):
    `StdioDebugLog(msg)` appends a timestamp-free but PID-prefixed line to
    whatever file `WXM_STDIO_DEBUG_LOG` names (read once, cached, via
    `wxGetEnv()`) -- entirely inert (single cheap env-var check, cached
    after the first call) unless that variable is set, which only the CI
    workflow does, scoped to just the one ctest step that reproduces this;
    a normal build or a normal user's run never sets it and pays only that
    one cached check. Deliberately written via raw `CreateFileW`/`WriteFile`,
    never CRT stdio -- tracing a stdio bug through the very subsystem
    suspected of being broken would be self-defeating. Opened with
    `FILE_APPEND_DATA` alone (no `GENERIC_WRITE`), which per MSDN makes
    Windows itself position each `WriteFile` atomically at end-of-file --
    load-bearing, since this ctest step runs `-j 2` and many independent
    `wxmaxima` processes share this one log path; a separate
    `SetFilePointer(..., FILE_END)` call would reintroduce exactly the
    seek-then-write race this flag exists to avoid, so don't add one back.
    Each line is prefixed with `GetCurrentProcessId()` so interleaved
    processes' lines can be told apart afterward.
    `DescribeStdHandle(stdHandleId)` (raw `GetStdHandle`/`GetFileType` --
    "none"/"handle=%p type=pipe|disk|char|unknown") and
    `DescribeStream(FILE*)` (`_fileno()` + `_get_osfhandle()` -- "fd=%d
    osHandle=%p") are the two probes; checkpoints call one or the other at:
    the very start and end of `BindStdStreamToParent()` (per stream, so
    stdout/stderr/stdin each get their own before/after pair), the end of
    `RedirectStdioToParent()`, right after the `SetHandleInformation(...,
    HANDLE_FLAG_INHERIT, 0)` block, immediately before and after
    `wxMessageOutput::Set(...)`, immediately before and after
    `cmdLineParser.Parse()`, and right as the `-v` branch is entered plus
    right after its `Printf()` call (before `exit(0)`). The specific
    question this is built to answer: does `_fileno(stdout)`'s resolved
    `osHandle` (or the raw `GetStdHandle(STD_OUTPUT_HANDLE)` value) ever
    silently change, or does `GetFileType` ever stop reporting `pipe`,
    somewhere across that chain -- something no prior Wine repro could
    observe since none of them ever ran the real chain end to end.
    **A `const wxChar*` gotcha caught during review, not live**:
    `DescribeStdHandle()`'s type-name strings are built from `wxS(...)`
    literals, not plain `"pipe"`/`"disk"`/... `char*` ones -- passing a
    narrow `char*` for a `%s` conversion in `wxString::Format` on a
    Unicode build is exactly the class of mismatch this file's own
    `wxLogMessage`/`%zu`-from-a-worker-thread note (under "Communication
    with Maxima") already flags as capable of asserting rather than just
    printing; fixed before ever running by matching every `%s` argument's
    width to the build's native `wxChar`, not found by tripping it live.
    **Deliberately did NOT add an `fflush(stdout)` before `exit(0)`**: an
    earlier draft of this instrumentation added one gated only by
    `#ifdef __WXMSW__` (i.e. unconditionally on every Windows build, not
    behind the env var) on the theory "can't hurt, might even help
    flush a stuck buffer" -- caught in self-review before committing: that
    would have been a real, non-diagnostic behavior change riding along
    with what's supposed to be a strictly inert change, and worse, if it
    happened to paper over the actual bug, the next CI run would report
    "fixed" while teaching nothing about the real mechanism. Removed
    outright rather than gating it behind the env var too, since gating a
    behavior change behind a flag that's only set on the one CI run
    collecting evidence would make that evidence describe a different code
    path than every other build ships.
    Wired into `.github/workflows/compile_windows.yml`: `WXM_STDIO_DEBUG_LOG`
    is set (to `${{ github.workspace }}\wxm_stdio_debug.log`) only as an
    `env:` on the existing "Run integration tests" step, and a new "Upload
    stdio debug log" step immediately follows it,
    `actions/upload-artifact@v7` with `if-no-files-found: ignore` and
    `if: always()` (not `if: failure()`) -- `upload-artifact` already
    no-ops cleanly when the path doesn't exist, and `always()` means a
    green run's log (which should show nothing anomalous, itself a useful
    negative data point) gets uploaded too, not just a red run's.
    **Not yet verified against a real CI run as of this writing** -- this
    entry documents the instrumentation's design and the reasoning behind
    each choice; the actual captured evidence (or absence of anything
    anomalous, which would itself be informative) belongs in a follow-up
    entry once the next push's `compile_windows` run completes and the
    artifact can be pulled and read. This diagnostic block should be
    removed (or at least re-gated more strictly) once this investigation
    either finds its answer or is shelved again -- it's instrumentation for
    an open question, not a permanent feature.
  - **Follow-up (2026-09-07, same day): the very first real CI run reproduced
    the failure exactly as expected, but the uploaded artifact ZIP itself
    turned out to be unreachable from this sandbox.** PR #2295's
    `compile_windows` run failed `wxmaxima_version_string` right on cue
    ("Required regular expression not found", 133/134 -- identical signature
    to every prior occurrence) and `actions/upload-artifact@v7` uploaded a
    real 5525-byte `wxm-stdio-debug-log` artifact. But both `curl` (via this
    sandbox's egress proxy) and the `WebFetch` tool failed identically on the
    artifact's actual storage backend
    (`productionresultssa12.blob.core.windows.net`, an Azure Blob Storage
    host, not `github.com` itself) with `EGRESS_BLOCKED` -- this sandbox's
    network policy allowlists GitHub's own API/web domains but not the
    separate blob-storage domain artifact downloads are redirected to.
    `mcp__github__actions_get`'s `download_workflow_run_artifact` method
    happily returns a valid, correctly-signed short-lived SAS URL for the
    artifact -- the GitHub API call itself works fine -- but actually
    fetching that URL's bytes is what fails, and no available MCP tool
    proxies that fetch through an allowlisted path. **Fixed by not depending
    on artifact download at all**: added a new "Print stdio debug log" step
    (`.github/workflows/compile_windows.yml`, right before "Upload stdio
    debug log", also `if: always()`) that `Get-Content`s the log file
    straight into the job's own console output when it exists. Job step logs
    are fetched through `mcp__github__get_job_logs`, a plain GitHub API call
    against `github.com` itself -- confirmed working earlier in this exact
    investigation (that's how the `wxmaxima_version_string` failure text
    above was read) -- so this sidesteps the blob-storage egress gap
    entirely instead of trying to work around it. The artifact upload step
    is left in place too (harmless, and useful for anyone reading this from
    an environment that *can* reach Azure Blob Storage), but the console
    print is now the primary, verified-reachable way to retrieve this
    trace. **Not yet re-verified**: this fix hasn't had a CI run of its own
    yet -- the analysis above is from the run that used only the
    artifact-upload step, so the actual `StdioDebugLog()` trace content
    (the real payoff of this whole diagnostic effort) is still unread as of
    this entry. That comes in the next follow-up once the console-print
    step lands and a fresh run reproduces the failure again.
  - **Follow-up (2026-09-07, same day): the console-print fallback worked,
    the trace was read end to end for the failing `wxmaxima_version_string`
    invocation itself, and it shows a completely clean, anomaly-free
    sequence all the way through -- which redirects the investigation
    rather than closing it.** Commit `0dfb7ba`'s `compile_windows` run
    (job `101648978179`, run `34092563005`) failed the same way again
    ("Compile using minGW" red; the earlier commit's run on this same PR
    had the identical `wxmaxima_version_string` signature and nothing else
    in this diff touches non-Windows code, so this is the same failure,
    not a new one). `mcp__github__get_job_logs` returned the trace directly
    in the job's own console output exactly as designed -- the
    Azure-Blob-Storage egress gap from the previous entry is now fully
    worked around for good.
    The trace for the one process that reached the `-v` branch (pid 3420,
    identified unambiguously: `wxmaxima_version_string` is the only test in
    the whole suite that both passes `--version` and checks the captured
    text, so it's the only process that could ever log an "entering -v
    branch" line) is, verbatim:
    ```
    [pid 3420] BindStdStreamToParent(4294967285) start: handle=0000000000000450 type=char
    [pid 3420] BindStdStreamToParent(4294967285) done: fd=1 osHandle=0000000000000240
    [pid 3420] BindStdStreamToParent(4294967284) start: handle=0000000000000454 type=char
    [pid 3420] BindStdStreamToParent(4294967284) done: fd=2 osHandle=000000000000042c
    [pid 3420] BindStdStreamToParent(4294967286) start: handle=00000000000000b4 type=char
    [pid 3420] BindStdStreamToParent(4294967286) done: fd=0 osHandle=0000000000000454
    [pid 3420] RedirectStdioToParent() done: stdout fd=1 osHandle=0000000000000240, stderr fd=2 osHandle=000000000000042c
    [pid 3420] after SetHandleInformation: stdout fd=1 osHandle=0000000000000240, stderr fd=2 osHandle=000000000000042c
    [pid 3420] after wxMessageOutput::Set: stdout fd=1 osHandle=0000000000000240
    [pid 3420] before cmdLineParser.Parse(): stdout fd=1 osHandle=0000000000000240
    [pid 3420] after cmdLineParser.Parse() (result=0): stdout fd=1 osHandle=0000000000000240
    [pid 3420] entering -v branch: stdout fd=1 osHandle=0000000000000240, wxMessageOutput::Get()=00000217191f0790
    [pid 3420] after Printf, before exit(0)
    ```
    **Every single checkpoint is exactly what a correctly-working process
    should show**: the initial handle is valid (never null/invalid, so the
    `AttachConsole` fallback branch never fires), `_dup2()` succeeds and
    produces a stable `osHandle` (`0x240`) that *never changes* across
    `SetHandleInformation`, `wxMessageOutput::Set()`, or
    `cmdLineParser.Parse()`, `wxMessageOutput::Get()` returns a valid
    non-null pointer right before the real `Printf()` call, and the trace
    reaches "after Printf, before exit(0)" -- meaning the `Printf()` call
    itself returned normally, no exception, no crash, nothing to indicate
    the write failed at the C++ level. **This rules out every mechanism
    this instrumentation was built to catch**: the fd/handle chain does not
    silently change, drop, or point somewhere unexpected anywhere between
    `RedirectStdioToParent()` and the actual write call. Whatever is wrong
    is downstream of a call that, from inside the process, looks completely
    successful.
    **A genuinely new, unexpected, load-bearing fact surfaced as a side
    effect of tracing this**: `GetFileType()` on the very first
    `GetStdHandle(STD_OUTPUT_HANDLE)` -- before any of this code's own
    logic runs -- reports `FILE_TYPE_CHAR`, not `FILE_TYPE_PIPE`, and this
    is true for *every* wxmaxima subprocess this ctest step spawns, not
    just the failing one (confirmed by grepping the same trace for every
    other pid's `BindStdStreamToParent(...) start` line -- all say
    `type=char`). ctest's `--output-on-failure`/regex-matching machinery
    has to capture each test's stdout+stderr somehow to check
    `PASS_REGULAR_EXPRESSION`-style assertions against it, and the
    textbook assumption (an anonymous pipe, which would show up as
    `FILE_TYPE_PIPE` to the child) does not match what's actually
    happening here -- every child inherits a real console-type handle
    instead. This was previously completely unknown and changes the shape
    of the problem: it is not "something about wxMaxima's own code breaks
    a working pipe redirection" (the redirection, whatever it targets, is
    demonstrably rock-solid across this entire trace) -- it is "does a
    write through a `FILE_TYPE_CHAR` handle via this specific
    `wxMessageOutputStderr::Printf()` code path actually reach wherever
    ctest is reading captured test output from, on this toolchain, for a
    WIN32-subsystem (GUI) child process specifically."
    **Why there is no working comparison case to rule this in or out**:
    `wxMessageOutput`/`wxMessageOutputStderr` is used for exactly two
    things in this codebase -- the `--version` text and `wxCmdLineParser`'s
    own `--help` usage text -- and `wxmaxima_help_returncode` (the sibling
    test for `--help`) only checks the exit code, never the captured text.
    So `wxmaxima_version_string` is the *only* test in the entire suite
    that both (a) goes through this specific WIN32-subsystem console-output
    code path and (b) actually asserts on the captured content -- every
    other content-checked batch test's expected text comes from Maxima's
    own separate output-writing mechanism over the TCP socket, which never
    touches `BindStdStreamToParent()`/`wxMessageOutput` at all. There is no
    other "this exact mechanism, but it happens to pass" test to compare
    against; the FILE_TYPE_CHAR fact, while true for every process, cannot
    be dismissed as "clearly fine, everything else uses it too" the way it
    might look at first glance, because nothing else's *pass/fail result*
    actually depends on it.
    **Not yet investigated, and the concrete next steps if this is picked
    up again**: (1) instrument (still diagnostic-only, still gated behind
    `WXM_STDIO_DEBUG_LOG`) whether the underlying write call inside
    `wxMessageOutputStderr::Printf()` -- which per wxWidgets' own source is
    a plain `fputs(psz, m_fp)` against the `FILE*` passed to its
    constructor (`stdout`) -- actually succeeds, e.g. by checking `errno`/
    `ferror(stdout)` right after the call, since the trace above cannot
    currently distinguish "the write genuinely succeeded but the bytes
    went somewhere ctest doesn't read" from "some later, still-unlogged
    step silently swallows or discards them"; (2) as a genuinely separate,
    clearly-labeled *experiment* (not a fix riding along with a diagnostic
    change) -- add an explicit `fflush(stdout)` right after the `Printf()`
    call in the `-v` branch, gated behind the same env var so it only runs
    on the one CI invocation collecting evidence, and see whether that
    changes the outcome; if it does, that's real evidence toward a
    buffering-related mechanism despite `setvbuf(..., _IONBF, 0)` already
    having been set on this stream earlier -- worth double-checking that
    the unbuffered mode actually survived being set on a `FILE_TYPE_CHAR`-
    backed stream specifically, since Windows' CRT console-handle plumbing
    is not guaranteed to behave identically to its pipe/file plumbing here;
    (3) look directly at how CMake/CTest's own child-process execution
    (`cmsysProcess`/kwsys on Windows) sets up stdio redirection for a
    `WIN32`-subsystem child specifically -- the `FILE_TYPE_CHAR` finding
    suggests it may not be using `STARTF_USESTDHANDLES` + anonymous pipes
    the way it would for an ordinary console-subsystem test executable,
    and if so, understanding *that* mechanism (not wxMaxima's own code) is
    probably the real key to this bug. This is the first concrete lead in
    this entire investigation that isn't "one more mechanism ruled out" --
    it's a real, previously-unknown fact about how this specific CI
    environment captures a WIN32-subsystem child's output, and the next
    session picking this up should start here rather than re-tracing the
    fd/handle chain again, which is now about as thoroughly instrumented
    as it usefully can be.
  - **Follow-up (2026-09-07): likely root cause identified via external
    research on the `FILE_TYPE_CHAR` finding above, and a fix applied to
    the test itself (not to wxMaxima's code) -- STILL UNVERIFIED against
    real CI as of this writing.** Two externally-confirmed facts, found via
    web search rather than assumed: (1) `GetFileType()` reports
    `FILE_TYPE_CHAR` for BOTH a real console handle AND a Windows ConPTY
    (pseudo-console) conout handle -- they are indistinguishable to the
    child via this API; (2) ConPTY has multiple real, currently-open
    upstream issues in `microsoft/terminal` describing races around
    draining/forwarding a child process's output relative to that child's
    own exit -- e.g. `ClosePseudoConsole()` not reliably waiting for the
    output pipe to drain before tearing down, worse the faster the child
    exits after writing. `wxmaxima --version` writes exactly one line via
    `Printf()` and calls `exit(0)` immediately afterwards with nothing else
    happening in between -- exactly the write-then-exit-fast shape that
    triggers this class of bug -- and (per the entry above) this is the
    only test in the whole suite whose pass/fail depends on that specific
    console-output path's content for a WIN32-subsystem process; every
    other content-checked test asserts on Maxima's answer over the TCP
    socket instead, which never touches this path at all. Put together:
    this is very likely a Windows console/ConPTY output-draining race
    around a short-lived GUI-subsystem process's exit, not a bug in
    wxMaxima's own code -- consistent with the diagnostic trace above
    already proving the app-side write itself completes normally, and with
    `RUN_SERIAL` (a ctest-level, not an OS-console-level, mitigation)
    having no effect. This does not itself prove GitHub Actions'
    `windows-latest` runner's `pwsh`/CTest process tree actually goes
    through ConPTY specifically (that could not be confirmed from outside
    the runner) -- but a real console handle has the exact same
    `FILE_TYPE_CHAR` signature and the exact same class of
    "output near process exit is not fully synchronous" risk, so the fix
    below does not depend on picking between the two.
    **Fix applied (`test/CMakeLists.txt`)**: since the race lives in
    whatever console/pty layer sits between `wxmaxima.exe` and CTest, not
    in wxMaxima's own code, route the captured text through a real file
    instead of a live console handle on Windows -- disk I/O has no
    equivalent async-drain step. The test's `COMMAND` is now
    platform-conditional (`if(WIN32)`): Windows runs
    `cmd /c "$<TARGET_FILE:wxmaxima> ... > wxmaxima_version_string.out 2>&1 && type wxmaxima_version_string.out"`,
    every other platform keeps the original direct invocation unchanged.
    `type`'s own write-then-exit is spawned by CTest the exact same way as
    every other (reliably passing) test in this suite -- it's a plain
    console-subsystem process, which removes the one factor (a
    WIN32-subsystem process as CTest's *direct* child) unique to this test,
    rather than guessing at the race's exact timing. `RUN_SERIAL` is left
    in place (harmless, and the earlier three-real-CI-failures finding
    against ctest-level contention stays valid regardless of this fix).
    Verified only that this doesn't regress non-Windows: a clean local
    Linux configure+build+`ctest -R wxmaxima_version_string` still passes,
    and the generated Linux command line is byte-for-byte unchanged from
    before this change (confirmed by inspecting the generated
    `CTestTestfile.cmake`) -- the actual Windows behavior, same as
    everything else in this investigation, can only be confirmed by a real
    `compile_windows.yml` CI run. **If this run still fails**: that rules
    out a console/ConPTY drain race specifically (since a real file write
    has no equivalent race) and reopens the question of what else could be
    dropping this one test's captured output -- worth re-reading this
    whole entry's history before re-guessing, and worth checking whether
    the `.out` file itself ended up empty/short (checked into the
    artifact/console-print mechanism already built for this investigation)
    to tell "file write also failed" apart from "`type`'s own output was
    lost the same way," which would point back at the outer console layer
    being the common factor after all.

- **System tray icon (`src/TrayIcon.{h,cpp}`, GH #2286) -- mirrors the busy
  status, gated entirely by `wxUSE_TASKBARICON`.** The maintainer's own
  issue text was just "wxAppIndicator -- we don't seem to use that on gtk,
  currently"; root-caused by finding that `StatusBar::UpdateStatusMaximaBusy()`
  already has a Windows-only `#ifdef __WXMSW__` block driving the taskbar
  button's progress/overlay state via `MSWGetTaskBarButton()` -- `TrayIcon`
  is the portable, GTK-reaching equivalent of exactly that, using the
  cross-platform `wxTaskBarIcon`. On GTK specifically, whether the icon is
  actually *visible* depends on whether the linked wxWidgets was itself
  built with AppIndicator/Ayatana support (`wxUSE_APPINDICATOR`, checked in
  `wx/gtk/taskbar.cpp`, a wxWidgets-internal macro this app's own code never
  needs to check) -- Ubuntu 24.04's stock `libwxgtk3.2-dev` package does
  *not* have it defined at all, so on that specific distro the icon falls
  back to the older GtkStatusIcon/XEmbed mechanism, which still worked and
  was confirmed genuinely visible end-to-end after configuring a real
  systray host (`fluxbox`'s toolbar needs an explicit
  `session.screen0.toolbar.tools: ..., systemtray, ...` in `~/.fluxbox/init`
  -- it isn't there by default).
  - **Two real bugs found only by comparing a live screenshot against the
    app's own status bar icon side by side, not by reading the code:**
    1. First attempt built 4 collapsed "category" icons (Idle/Busy/
       Attention/Error) from the *wrong* bitmap family --
       `StatusBar`'s `m_network_idle`/`m_network_transmit_receive` members,
       which track the **separate** `m_networkStatus` icon (raw socket
       send/receive activity, driven by `HandleTimerEvent()`'s send/receive
       timers) -- not `m_maximaStatus`'s own per-status bitmaps
       (`m_bitmap_waiting`, `m_bitmap_calculating`, ...), which is what
       actually answers "what is Maxima doing." The two icon families exist
       side by side in the real status bar (look for `m_networkStatus` vs
       `m_maximaStatus` in `StatusBar.cpp`) and are easy to conflate by name
       alone. Symptom: the tray showed a barely-visible speck for "idle"
       instead of a normal icon, since that family's idle glyph is a subtle,
       mostly-transparent icon designed to sit unobtrusively next to actual
       traffic icons, not to stand alone. Fixed by exposing
       `StatusBar::GetTrayIconBitmap(MaximaStatus)`, a straight mirror of
       `UpdateStatusMaximaBusy()`'s own per-status `m_maximaStatus->SetBitmap(...)`
       choices, and confirmed by cropping both icons from the same
       screenshot and eyeballing them side by side -- they now match
       exactly, pixel for pixel.
    2. `art/statusbar/*.h` (the bin2h-generated byte-array headers) declare
       their arrays without `static`/`extern` -- fine when `#include`d from
       exactly one `.cpp` (which is all `StatusBar.cpp` ever did), but
       `#include`-ing the same header a second time from `TrayIcon.cpp` to
       build its own icon set independently is a duplicate-symbol *link*
       error (`multiple definition of NETWORK_IDLE_SVG_GZ`, ...), not a
       compile error -- caught immediately on the first real link attempt.
       Fixed by never re-embedding the art at all: `TrayIcon` only ever
       calls the new `StatusBar::GetTrayIconBitmap()` getter and reuses the
       bitmaps `StatusBar` already decoded once in its own constructor.
  - The popup menu's "Interrupt"/"Exit" items deliberately reuse the *exact*
    label text (`"&Interrupt\tCtrl+G"`, `"E&xit\tCtrl+Q"`) the Maxima/File
    menus already use, and the two longer status tooltips
    (debugging/lispmode) reuse `StatusBar`'s own full multi-line wording
    verbatim, rather than shorter tray-only paraphrases -- purely to avoid
    growing the translatable-string count for a near-duplicate of an
    existing string; `test/check-pot-coverage.cmake` only asserts that
    every file containing a `_("...")` marker is referenced *somewhere* in
    `wxMaxima.pot` (by filename, not per-string), so adding this new file
    needed exactly one hand-added `msgid "&Show wxMaxima"` entry (its own
    genuinely new string) -- not a full `update-locale` regen, which would
    have swept in ~1000 lines of unrelated pre-existing POT drift (see the
    translations skill for why that regen-then-revert dance is the right
    move here, not a shortcut).
  - Menu actions never duplicate existing logic: `OnInterrupt()`/`OnExit()`
    re-post a plain `wxCommandEvent(wxEVT_MENU, <id>)` to the main frame's
    event handler instead of reimplementing what `MaximaProcessManager::
    Interrupt`/`MaximaCommandMenus::FileMenu` already do for those same IDs
    on the real menu -- necessary because `wxTaskBarIcon`'s own popup menu
    delivers `wxEVT_MENU` to itself, not to the frame that created it.
    Verified live: clicking "Interrupt" from the tray logs the same
    "Sending Maxima a SIGINT signal," and clicking "Exit" raises the same
    save-changes `Save As` prompt a normal File > Exit does.

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
- **`.wxm`'s marker comments are NOT uniformly self-closed -- exactly one
  bug class (GH #1907) comes from that asymmetry.** `WXMHeaders[]`
  (`src/WXMformat.h`/`.cpp`) has two different shapes:
  1. **Input/code markers are each a single, already-closed one-line
     comment** -- `"/* [wxMaxima: input   start ] */"` opens *and* closes
     `/* */` on the same line, so the code text between the start and end
     markers sits completely outside any comment. This is deliberate and
     load-bearing: it's what lets a plain, wxMaxima-unaware `batch()`/
     `load()` parse the code between them with zero special handling, and
     it's why a literal `/*`/`*/` inside actual Maxima code (a real Maxima
     comment) must stay byte-for-byte unescaped -- `WXM_INPUT`/
     `WXM_HIDDEN_INPUT` are excluded from the escaping below for exactly
     this reason.
  2. **Every other marker (title/section/subsection/subsubsection/
     heading5/heading6/comment/caption) opens a comment on its start line
     that is left open across the cell's entire content**, only closing at
     the *end* marker's own trailing `*/` (e.g. start = `"/* [wxMaxima:
     title   start ]"`, no closing `*/`; end = `"   [wxMaxima: title
     end   ] */"`, no opening `/*`). A literal `*/` inside such a cell's
     own prose closes that comment early -- everything from there up to
     whatever `*/` a plain Maxima scanner finds *next* in the file is read
     as live, executable input instead of inert text. A title cell
     containing `"abc */ x:2$ /* def"` silently ran `x:2$` when the file
     was `batch()`ed or opened, with no error and no visible sign anything
     had executed. (`WXM_CAPTION` shares its ordinal with `GC_TYPE_IMAGE`
     and covers only an image cell's own descriptive label text -- the
     separate `WXM_IMAGE` marker pair, wrapping just the raw base64 bitmap
     bytes, was never at risk here: base64's alphabet has no `*` character
     at all.)
  Fixed in `src/WXMformat.cpp` with a reversible, HTML-entity-style
  transform (`EscapeWXMSlashes()`/`UnescapeWXMSlashes()`), applied only to
  the type-2 markers above: escape every literal `&` to `&amp;` first (so
  the scheme stays unambiguous if the original text already has one), then
  -- in a single linear scan, not two separate global replaces, so a
  pathological run like `"*/*"` or `"/*/"` is handled correctly rather than
  double-encoded -- replace every `/` that sits immediately next to a `*`
  with `&#47;`, leaving that `*` and any unrelated `/` (an ordinary `1/2`)
  untouched. This closely follows a fix the maintainers had already
  discussed but never implemented (GH #1907's own comment thread:
  "how about if all text cells have `/*` and `*/` replaced by HTML
  entities?"), narrowed to only the `/` adjacent to a `*` rather than every
  slash, to avoid visual noise in the raw `.wxm` file for cells that just
  happen to contain an ordinary fraction. Also applied to the equivalent
  `GC_TYPE_TEXT` write path in the non-`.wxm` (`.mac`/xmaxima interop)
  export, for the same reason (defense in depth -- `.mac` is always
  directly Maxima-loadable) -- but `Format::ParseMACContents` (the `.mac`
  reader) has no corresponding unescape, since round-tripping a
  wxMaxima-exported `.mac`'s text cells back into wxMaxima is out of scope
  for this fix and only costs a cosmetic `&#47;` showing up literally.
  **Known, accepted limitation** (also already flagged in the same GH
  #1907 thread): this can't retroactively fix a `.wxm` file already on
  disk from before this existed, and a file whose prose coincidentally
  contains the literal text `&amp;` (e.g. discussing the HTML entity
  itself) will be mis-decoded after this fix -- an intentional tradeoff,
  not an oversight. Regression coverage in
  `test/unit_tests/test_WXMRoundtrip.cpp` pins both the injection fix
  (title/section/text cells containing `*/`/`/*` round-trip losslessly and
  the raw `.wxm` line has neither substring left unescaped) and the
  "code cells are never touched" invariant, confirmed to actually catch the
  bug by reverting just the source fix (`git stash push -- src/WXMformat.cpp`)
  and re-running: the injection scenarios failed with exactly the reported
  symptom before the fix was restored.

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
  first attempted workaround, which "immediately started syncing" -- but
  this did *not* fix anything: `l10n_main3`'s own PR (#2259) forked from
  `7d0407f8c`, a commit well after the restoration, yet still carried the
  exact same 464 empty translations (confirmed the same way, spot-checked
  down to individual entries, e.g. German's translation of the literal
  command-line flag `"      -X \"--control-stack-size <int>\""` -- present
  and correct, self-identical to the msgid, on `main`, but back to `""` on
  `l10n_main3`). That ruled out "stale git branch point" as the actual
  cause, since a fresh branch reproduced it identically. **Root cause,
  confirmed by the maintainer directly with Crowdin: the export had
  exceeded Crowdin's string-count limit for their free-tier account plan,
  which made Crowdin silently fall back to an old, pre-restoration
  translation snapshot instead of erroring or refusing to export** --
  explaining why recreating the branch changed nothing: the stale state
  lived in Crowdin's own translation memory, not in which commit its
  export happened to be based on. The account has since been deleted;
  both #2257 and #2259 were closed unmerged. If Crowdin integration is
  ever reconnected, check the account's plan/string-limit status first --
  this failure mode gives no visible error on either the Crowdin or the
  GitHub side, only a silent, correct-looking PR that happens to carry
  stale content. Added a permanent
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

- **A Maxima `{...}`/`setify(...)` set rendering as completely blank output
  (GH #2270), despite the value being computed correctly:** `SetCell`
  (`src/cells/SetCell.cpp`) extends `ListCell` and overrides
  `SetCurrentPoint()` -- but the override was
  `void SetCell::SetCurrentPoint(wxPoint point) const { Cell::SetCurrentPoint(point); }`,
  which positions only the `SetCell` object itself and completely skips the
  inherited `ListCell::SetCurrentPoint()`'s logic that positions
  `m_open`/`m_innerCell`/`m_close` (the "{" glyph, the actual list content,
  the "}" glyph). `GroupCell::UpdateOutputPositions()` calls
  `tmp.SetCurrentPoint(in)` on each top-level entry of the output's draw
  list (`OnDrawList()`) -- for an unbroken (fits-on-one-line) `SetCell` that
  entry *is* the whole `SetCell`, so this override, not `ListCell`'s, is
  what fires. Since it never touches the children, they keep whatever stale
  or default position they last had and get drawn there instead of inside
  the set's own bounding box -- invisible within the visible viewport, while
  the underlying value is completely correct (confirmed live: copying the
  blank output cell's clipboard content, or `listify(%)`, reveals the right
  answer). The override did strictly *less* than the version it shadowed and
  had no reason to exist at all -- deleting it outright (from both
  `SetCell.h` and `SetCell.cpp`) is the fix, letting `SetCell` inherit
  `ListCell::SetCurrentPoint()` normally, which already handles `m_open`/
  `m_close` correctly regardless of what glyphs they hold. Confirmed live in
  Xvfb: `{1,2,3};` rendered as a totally blank `(%o1)` line on an unmodified
  build, both at default window width and narrower (forcing the set to wrap
  across lines) -- `[1,2,3];` (a plain `ListCell`, no divergent override)
  rendered correctly in every case tested, which is what pointed at
  `SetCell`'s own code rather than the shared `ListCell`/layout-pipeline
  machinery. Root-caused with gdb (`gdb -p <pid> -batch -x script.py`,
  breaking on `SetCell::Draw`/`ListCell::Recalculate`/`Cell::BreakUpAndMark`
  from a live, real Xvfb session -- the sandbox's earlier-documented
  hardware-breakpoint/`rr` limitations don't affect plain software
  breakpoints, which is all this needed) -- an initial hypothesis blaming
  `Cell::BreakUpCells()`'s line-wrap width heuristic (a `CachedInteger`
  reading back its `INT_MAX` "invalid" sentinel as a width) turned out to be
  a red herring from noisy manual multi-window testing, not reproducible in
  a clean single-cell session; always reproduce a rendering bug in a fresh,
  isolated worksheet before trusting a gdb trace's numbers. Also fixed a
  smaller, related inconsistency found while auditing this: `SetCell`'s
  constructor replaces `m_open`/`m_close` with fresh "{"/"}" `TextCell`s but
  never called `SetStyle(TS_FUNCTION)` on them the way `ListCell`'s own
  constructor does for "["/"]", leaving the braces in the wrong style.

- **`ProductCell` showing "sum(" instead of "product(" when broken into
  lines, and rendering as nothing at all when it isn't (found live, no GH
  issue filed yet) -- two independent bugs, both variations of mistakes
  already documented elsewhere in this file.**
  1. `SumCell::MakeBreakUpCells()` (called from `SumCell`'s own
     constructor, `SumCell.cpp`) builds `m_open`'s text from the virtual
     `GetMaximaCommandName()`. A virtual call made during a base class's
     constructor can never dispatch to a derived class's override --
     `ProductCell`'s part of the object doesn't exist yet at that point --
     so `m_open` was unconditionally built from `SumCell::
     GetMaximaCommandName()` ("sum("/"lsum(") even for a genuine
     `ProductCell`, regardless of how the cell was later broken into
     lines. Fixed with a new protected `SumCell::RefreshBreakUpCommandName()`
     that re-applies `GetMaximaCommandName()` to the already-built `m_open`
     via `TextCell::SetValue()`; `ProductCell`'s constructor calls it once,
     from its own constructor body (where virtual dispatch has already
     started resolving to `ProductCell`'s overrides), immediately after
     delegating to `SumCell`'s constructor. Any future `SumCell` subclass
     that overrides `GetMaximaCommandName()` must do the same.
  2. `ProductCell::SetCurrentPoint()`/`Draw()` (`ProductCell.cpp`) only
     ever called `Cell::SetCurrentPoint()`/`Cell::Draw()` -- skipping
     `SumCell::SetCurrentPoint()`/`SumCell::Draw()` entirely, which are the
     implementations that actually position/paint the operator glyph, the
     limits and the base. Exactly the same "override does strictly less
     than what it shadows and had no reason to exist" shape as the
     `SetCell` bug immediately above this entry: an unbroken `ProductCell`
     drew nothing, while the underlying value was computed correctly (same
     "invisible, not wrong" symptom, same root cause pattern, different
     cell). Fixed by deleting both overrides outright (header and source),
     letting `ProductCell` inherit `SumCell`'s implementations -- which is
     safe here specifically because `Draw()`/`SetCurrentPoint()` never run
     during construction, so by the time they're actually called,
     `GetSvgSymbolData()`/`GetSymbolSize()`/... already dispatch correctly
     to `ProductCell`'s own overrides.
  Confirmed live in Xvfb: `product(k,k,1,n);` rendered the correct Π glyph
  with `n`/`k=1` once unbroken-form positioning was fixed (previously blank
  on unmodified `main`); the broken-form text bug was pinned deterministically
  via a new regression test (`test/unit_tests/test_LayoutInvariants.cpp`,
  `SCENARIO("A ProductCell positions its symbol/limits/base and breaks up
  with the right command name")`) that parses real `wxxml-sum`-shaped XML
  through `MathParser` (no live Maxima needed, mirroring the `SetCell`
  scenario's own pattern) and checks `GetBrokenCell(0)`'s text is
  `"product("`, not `"sum("` -- reverting the fix reproduces both original
  symptoms (confirmed by re-running the test against the unfixed code).
  Auditing `SumCell::MakeBreakUpCells()` while fixing this also turned up a
  third, smaller, unrelated bug: unlike every sibling cell with an analogous
  `"name("` opening glyph (`BoxCell`, `AbsCell`, `SqrtCell`, `ExptCell`,
  `ConjugateCell`, `NamedBoxCell` -- all call `DontEscapeOpeningParenthesis()`
  on their `m_open` right after constructing it), `SumCell`'s `m_open` never
  did, so `TextCell::ToString()`'s `TS_FUNCTION`-style quoting (the default
  `TextCell` style, since `MakeBreakUpCells()` never calls `SetStyle()`
  either) escaped the trailing "(" into a literal backslash-paren whenever
  that cell's own `ToString()` was read directly (e.g. the new regression
  test's own `GetBrokenCell(0)->ToString()` check, before this was noticed
  and fixed) -- invisible on screen (`Draw()` paints `m_displayedText`,
  which this escaping logic never touches) and invisible in `SumCell::
  ToString()`'s own clipboard/copy-as-text output (it builds directly from
  `GetMaximaCommandName()`, never through `m_open`), so this specific
  escaping bug had no live user-visible symptom found so far -- fixed
  anyway since it's a one-line, well-precedented, no-risk addition in the
  exact function already being edited.

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
  - **Follow-up (found live, 2026-08): the watchdog fired a false
    "Maxima isn't connecting" warning on Linux for a large worksheet, with
    no macOS/Gatekeeper connection at all -- root cause was the watchdog's
    5-second budget being spent on wxMaxima's own busy-ness, not on
    Maxima.** `MaximaProcessManager::StartMaxima()` armed the watchdog
    (`StartOnce(5000)`) immediately after spawning the process -- but one
    of its callers, `MaximaFileIO::OpenWXMXFile()`, calls `StartMaxima()`
    *in the middle* of a synchronous sequence: parse the whole worksheet
    XML into a cell tree (`CreateTreeFromXMLNode()`, before `StartMaxima()`
    even runs) and, right after `StartMaxima()` returns,
    `InsertGroupCells()` the tree ("this also requests a recalculate" per
    its own comment) -- laying out however many cells the worksheet
    contains, all on the same GUI thread, all before that thread ever
    returns to the event loop. For a large enough document this alone can
    exceed 5 seconds. Since the watchdog's `wxTimerEvent` can only be
    *processed* once the thread is back pumping events, and Maxima's own
    incoming-connection event is stuck behind the exact same jam, the two
    end up racing to be processed once the thread frees up -- and when the
    watchdog's already-queued event happens to be serviced first, it finds
    `!connected` and fires, even though Maxima tried (or even succeeded)
    to connect well within its own 5 seconds; the delay was entirely
    wxMaxima's own busy-work eating into the window it was supposed to
    spend genuinely listening. Reproduced by tracing the call chain (not
    by hitting the race live in this sandbox, where the sample worksheets
    on hand parse fast enough on today's hardware not to trigger it) --
    `OpenWXMXFile()`'s `CreateTreeFromXMLNode()` / `StartMaxima()` /
    `InsertGroupCells()` ordering is unambiguous in the source regardless.
    Fixed by deferring the `StartOnce(5000)` call itself via `CallAfter()`
    -- the same idiom `StartMaxima()`'s own "Cannot start the maxima
    binary" dialog a few lines above already uses, and for the identical
    reason (don't act synchronously inside a call chain that isn't back at
    the event loop yet). This makes the 5-second countdown start only once
    wxMaxima is actually idle and able to process an incoming connection,
    so however long the worksheet's own parse-and-layout took no longer
    counts against Maxima. `KillMaxima()`'s existing `.Stop()` plus the
    watchdog handler's own `processAlive` re-check already made this safe
    against the (rare) case of the deferred callback running after the
    spawn it was arming for is no longer relevant -- no separate guard
    needed. Also reworded the non-macOS warning text (still gated on
    `!__WXOSX__`, unchanged from a working baseline otherwise) to spell
    out that wxMaxima and Maxima talk over a *local, loopback* socket --
    users don't necessarily think of "this machine talking to itself" as
    something a firewall/antivirus's network protection would touch, and
    some of it blocks loopback inter-process communication too.

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
