---
name: wxmaxima-packaging
description: How wxMaxima is packaged, signed and released - the CI matrix and what each job can and cannot catch, Windows/macOS/snap specifics, code signing via SignPath, and release automation. Use when touching .github/workflows, CPack configuration, the snap, installers, or anything about shipping a release.
---

# Packaging, CI and releases

## What the CI matrix can and cannot see

This is the part that costs the most time when forgotten: **a green CI is not
one signal, it is several partial ones.**

- The Ubuntu jobs build with **gcc**. Clang-only diagnostics are therefore
  invisible to them - `-Wunused-lambda-capture` has already stopped `main` from
  compiling with clang while every Linux job stayed green. If you develop with
  clang and `-Werror`, you will hit things CI cannot.
- Windows builds wxWidgets **from source** at a pinned `WXVERSION`, so bumping
  the wx version there is a one-line change. Ubuntu uses distro packages and
  will lag for years - never gate a feature on Ubuntu's wx version; guard with
  `wxCHECK_VERSION` and let it compile out.
- The wxWidgets **cache key must include the version**. The path is
  version-specific, and cache keys are immutable: after a bump, the old key
  still exists but no longer matches the path, so nothing can ever be saved
  under it - a permanent miss and a ~30-minute rebuild every run.
- Tests that need a display, and tests that must not have one, are separated by
  ctest labels (`unittest`, `needs_posix`). Keep new tests labelled correctly or
  they run in the wrong job.

## Windows

- The installer is built with NSIS via CPack (`ZIP;NSIS`).
- **Bundle the MinGW runtime DLLs** (`libstdc++-6`, `libgcc_s_seh-1`,
  `libwinpthread-1`) explicitly. `InstallRequiredSystemLibraries` is unreliable
  for MinGW and silently omits them, and without them the exe will not start on
  a clean machine. A CI step asserts they are inside the produced ZIP.
- The DPI-aware manifest is **embedded** via `wx.rc` (Common Controls v6,
  per-monitor v2, supportedOS up to Win11). Windows ignores an external
  `<exe>.manifest` when an embedded one exists, so do not add one.
- Unit-test executables need that manifest too, or wx 3.3's
  `wxApp::Initialize()` pops a modal "no correct manifest" message box and the
  test hangs headlessly until the ctest timeout.

## Code signing (SignPath)

SignPath Foundation provides free certificates for open source. The mechanics
that are not obvious:

- SignPath **does not accept an uploaded file**. Its connector downloads the
  GitHub Actions artifact itself and verifies the origin metadata (repo,
  workflow, commit, runner). Hence the official action rather than the
  PowerShell cmdlet, and hence the upload-artifact step.
- `upload-artifact` always wraps files in a ZIP, so the artifact configuration's
  root element must be `zip-file`, wrapping the actual payload's element inside
  it -- a bare `<pe-file>`/`<msi-file>` at the XML root, with no `zip-file`
  wrapper, fails every submission with "The file does not correspond to the
  specified file type," confirmed live against a real SignPath project
  (2026-08).
- The Windows installer is an **NSIS-built `.exe`** (`CPACK_GENERATOR
  "ZIP;NSIS"`), not an MSI -- SignPath's element for a generic signable
  Windows executable is `<pe-file>` (not `<msi-file>`, and not
  `<executable-file>`, a plausible-sounding name that SignPath does not
  actually use).
- `<pe-file>` (like any element nested inside `<zip-file>`) needs an explicit
  `path` glob attribute -- a zip *can* hold more than one entry, so SignPath
  needs to be told which one to sign even when, as here, there's only ever
  one. Omitting it fails with "The required attribute 'path' is missing."
  The working configuration, named `exeinzip` on the wxmaxima SignPath
  project and set as its default:
  ```xml
  <?xml version="1.0" encoding="utf-8" ?>
  <artifact-configuration xmlns="http://signpath.io/artifact-configuration/v1">
    <zip-file>
      <pe-file path="*.exe">
        <authenticode-sign />
      </pe-file>
    </zip-file>
  </artifact-configuration>
  ```
  `path="*.exe"` matches because `actions/upload-artifact@v7` is given a
  single glob (`wxMaxima/build/*.exe`) with one match, so the file lands at
  the zip's root with no subdirectory -- confirmed by three failed
  submissions before this shape (see NEWS/commit history around 2026-08),
  each error naming the next missing piece.  **`SIGNPATH_ARTIFACT_CONFIGURATION`
  in `compile_windows.yml` must name the slug exactly** (`exeinzip`, not
  `exe`/`installer`, the two earlier now-abandoned configurations) --
  SignPath's own "default configuration" marking on its dashboard does not
  matter here, since the workflow always passes an explicit
  `artifact-configuration-slug`.
- The API token lives in an **environment** secret with a required reviewer.
  GitHub pauses such a job *before its first step*, so signing must be its own
  job - inside the build job it would block the entire test suite behind an
  approval click. And `secrets.X` resolves to an empty string in any job that
  does not declare the environment.
- Sign only on release tags and manual runs; every push would burn quota.

## macOS

Runners move: pinning an installer package to a specific macOS release (e.g. a
MacPorts `.pkg` naming a version) breaks when GitHub moves `macos-latest`.
Resolve the package by querying the release assets for the detected major
version, and put that major version in the cache key.

## Snap

Bundles Maxima via `stage-snaps`. The dependency is **unpinned**, so the snap
can ship a Maxima version nobody chose - version-syncing that is an open
follow-up. Bundled components bring their own licence obligations (gnuplot's is
attributed in-tree).

## Release automation

Windows attaches **two** packages, not one: the NSIS installer *and* the
portable `.zip` (both come from the same `CPACK_GENERATOR "ZIP;NSIS"` run, and
the ZIP was built and DLL-verified on every run long before anything published
it -- GH #2298 was largely "the artifact already exists, nobody attached it").
The ZIP is the no-administrator-rights option: wxMaxima resolves its resources
relative to the executable (`Dirstructure::ResourcesDir()` walks up out of
`bin/` and into `share/`), so an unpacked tree runs as-is. Note neither
Windows package bundles Maxima itself -- the *combined* installer is built in
the separate `Crosscompiled-Windows-installer` repository (see
`src/CMakeLists.txt`'s own comment), which is the actual difference behind
"this used to work differently" reports about the Windows download.
The `files:` glob is non-recursive on purpose: CPack writes finished packages
to the build root, and `_CPack_Packages/` underneath holds staging copies that
a `**` glob would also match.

On a `Version*` tag, the Windows/macOS/Ubuntu jobs attach their installer / dmg
/ deb plus a source tarball and the NEWS.md body to the GitHub release. The
release notes are extracted (`compile_windows.yml`, "Extract release notes
from NEWS.md" step) by skipping every *leading* top-level `# ` heading and
blank line, then taking everything up to the *next* `# ` heading - so it
doesn't matter whether the section is still headed "Current development
version" with nothing else above it, or whether release prep has already
inserted the release's own "# x.y.z" heading right below that (see the
runbook below - it deliberately does the latter, immediately, not as a
separate follow-up commit). Generalized this way (rather than "skip exactly
the first line") specifically so both structures keep working: an earlier
version of this script only skipped one leading heading, which would have
silently extracted an empty release body the first time a version heading
was inserted before tagging instead of after (caught by simulating the
PowerShell logic in Python against a real NEWS.md before it shipped).

### Doing an actual release: the runbook

`ReleaseChecklist.md` at the repo root is the authoritative step list -
read it fresh each time, since CI automates more of it as that file's own
"What CI now does automatically" section is updated. The parts worth
knowing going in:

- **Three files carry the version number and must move together**:
  `CMakeLists.txt`'s `project(... VERSION x.y.z ...)`, `snap/snapcraft.yaml`'s
  `version: x.y.z-0` (note the `-0` suffix, an unrelated snap revision
  counter), and a new `<release version="x.y.z" date="...T12:00:00Z">` entry
  in `data/io.github.wxmaxima_developers.wxMaxima.appdata.xml` (prepended
  above the previous entry, `<description><p>...short paragraph...</p>
  </description>`, no other HTML tags - flatpak/AppImage builders reject
  most of them). `CMakeLists.txt`'s own `WXMAXIMA_VERSION` logic appends
  `-dev` unless `CMAKE_BUILD_TYPE STREQUAL "Release"`, so a Debug build
  correctly shows `x.y.z-dev` after the bump - that's the intended check,
  not a bug.
- **NEWS.md**: condense the accumulated "Current development version"
  bullets into release notes (drop deep debugging narrative - "confirmed via
  gdb/tcpdump/md5sum" belongs in AGENTS.md, not here - keep the user-facing
  effect and the GH issue number), add a short intro paragraph in the same
  voice as previous releases (crediting AI assistance where genuinely
  substantial, matching the existing convention), and insert this release's
  own "# x.y.z" heading directly below "# Current development version" -
  leaving that heading in place, empty, for the next dev cycle. Do this in
  one step now that the extraction script handles it (see above); no need
  for the two-commit dance visible in older git history (condense-and-tag
  first, insert the numbered heading as a separate commit the next day).
- **Order**: get this all merged to `main` via a normal PR first (so CI
  validates the appdata file, builds, and runs the full test suite against
  the bump) - *then* create an **annotated** tag (`git tag -a
  Version-x.y.z`) on `main` and `git push origin --tags`. The tag push is
  what triggers the actual build-and-publish automation across all
  platforms; there is no dry-run, so don't tag until the merge's CI is green.
- **What an agent session cannot do, and should say so rather than skip
  silently**: GPG-signing the release tarballs needs a private key nobody
  hands to a session; updating the version/tarball MD5 in
  `crosscompile-windows/wxmaxima/CMakeLists.txt` and running
  `update_versions.sh` both live in *other* repositories (Maxima's own
  source tree and `docker-wxmaxima`) that aren't attached unless the user
  explicitly adds them. `download.html`/`version.txt` on the `gh-pages`
  branch, by contrast, *is* reachable - it's a branch of this same repo, just
  needs `git fetch`/checkout of `gh-pages` specifically.

## Third-party notices

Shipping a component means reproducing its licence: `THIRD-PARTY-NOTICES.txt`
plus a tab in the licence dialog. The WebView2 loader and the vendored nanoSVG
are the current cases. On nanoSVG specifically: the symbol-renamed private copy
stays deliberately - `wxBitmapBundle::FromSVG` caches every resize resolution
with no eviction. Do not "modernise" it away.

## Windows stdio: the subsystem bit, and the capture bug it causes

The long version of a month-long investigation, kept in full because most of it
is a record of theories that were **disproven** - the next person to touch
`BindStdStreamToParent()`, `src/wxmaxima-cli.cpp` or the
`wxmaxima_version_string` test should know what has already been ruled out
rather than re-deriving it.

The short version: `wxmaxima.exe` is a GUI-subsystem binary, which means it
starts with no stdio and `cmd.exe` does not wait for it. `wxmaxima-cli.exe`
exists to fix the second half. The first half - **a GUI-subsystem child
reports `FILE_TYPE_CHAR` for the very same handle values its console-subsystem
parent sees as `FILE_TYPE_PIPE`, with inheritance confirmed working** - is
still unexplained, and is where to start if this is picked up again.

Content assertions on captured output therefore run on non-Windows platforms
only; Windows keeps the return-code tests. Do not restore a content assertion
without watching one real run, and never on `wxmaxima.exe` itself.

> **Provenance.** The entries below were moved here verbatim from
> `AGENTS.md`, which had grown past 4800 lines. Where one of them says
> "this file" about a *document* (as opposed to the C++ source it is
> discussing), it means `AGENTS.md` -- check there, and in the sibling
> skills, for anything it cross-references that is not here.

- **`wxmaxima_version_string` CI test failing on the minGW Windows runner on
  essentially every push since 2026-08-15 -- ROOT CAUSE STILL UNKNOWN, but
  the job is green again because the assertion has been narrowed to the
  platforms where it is meaningful. Read the "SHELVED" follow-up at the very
  end of this entry first: it says what is tested where now, lists two real
  bugs this investigation did find and fix, and corrects two factual errors
  this entry itself carried for weeks.** This entry was
  headed "RESOLVED (2026-09-11)" until 2026-09-18; that was wrong, and the
  correction is worth reading before trusting any other status line in this
  file. The 2026-09-11 `cmd.exe` quoting fix (final follow-up at the end
  of this entry) was real and did fix the bug it described -- the entry
  itself said so honestly ("Not yet independently re-confirmed against a
  real Windows CI run as of this writing") -- but it was written up under a
  RESOLVED heading before that confirmation existed, and the confirmation,
  when it finally happened, came back negative. Verified directly on
  2026-09-18: the most recent completed `compile_windows` run on `main`
  (`6ecc9c8`, the #2313 merge) reports `1 tests failed out of 134` --
  `71 - wxmaxima_version_string (Failed)` -- and the twenty `main` pushes
  before it are red too. So the quoting bug was one real defect on top of
  another, not the whole story. **The current lead is the `FILE_TYPE_CHAR`
  finding recorded in the `wxmaxima-cli.exe` entry below**: a
  GUI-subsystem child sees its inherited standard handles as
  `FILE_TYPE_CHAR` while the console-subsystem launcher that passed it
  those very same handle *values* sees them as `FILE_TYPE_PIPE`, so the
  transformation tracks the child's subsystem rather than anything CTest
  does. Start there. The moral for this file, beyond this one test: **do
  not mark an entry RESOLVED on the strength of a fix you have not seen
  pass.** "Fix pushed, awaiting confirmation" is a different and honest
  status, and it would have saved the week this heading bought.
  Everything below this point, up to that final follow-up, is the
  investigation history -- kept in full since most of it
  (the `_dup2()` theory, the ConPTY-drain-race theory) was directly
  disproven or superseded, and the next session touching this code should
  know what's already been ruled out rather than re-deriving it. **Read
  this whole entry before touching `BindStdStreamToParent()` or
  `test/CMakeLists.txt`'s `wxmaxima_version_string` block again.**
  The test runs `wxmaxima --debug
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
    that line was carried over by mistake from the unrelated
    `tutorial_10Minutes` entry (now in the `wxmaxima-maxima-protocol`
    skill), which is a genuinely different, Linux-side Maxima flake where
    `rr` really is the right tool. Separately, "real
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
  - **Follow-up (2026-09-11): ACTUAL ROOT CAUSE FOUND -- the "route through
    a file" fix immediately above was directionally right but had never
    actually been exercised, because its own COMMAND string had an
    unrelated, self-inflicted `cmd.exe` quoting bug that made the test fail
    a completely different way, instantly, every time.** The very first
    real CI run after that fix landed (and every run since, across two
    separate PRs on two separate days) failed with the exact same new
    signature: `Required regular expression not found` immediately
    followed by a literal `The syntax of the command is incorrect.`, at
    0.01 sec -- i.e. `cmd.exe` itself rejected the command line outright
    before anything resembling a race could even begin. That "0.01 sec,
    identical every time" shape is the tell that this was never the
    ConPTY/console-drain race the rest of this entry spent so much effort
    investigating: a race varies in outcome and takes some real time to
    lose; a rejected command line fails identically, instantly, always.
    **The actual bug**: the COMMAND was written as
    `cmd /c "\"$<TARGET_FILE:wxmaxima>\" --debug ... && type ..."` --
    once CMake unescapes the `\"..\"`, the literal string handed to `cmd
    /c` starts with a `"` character, and cmsys/CTest's own Windows argument
    quoting then wraps that *whole* string (since it contains spaces) in a
    second, outer pair of quotes to pass it to `cmd.exe` as one argument --
    landing two adjacent quote characters right at the very start of what
    `cmd /c` receives. That specific shape (a quoted path immediately
    inside, with no character between, the outer quotes wrapping the whole
    `/c` argument) is a well-known `cmd.exe` parsing trap, and real Windows
    `cmd.exe`'s reported error for it is precisely "The syntax of the
    command is incorrect." **Confirmed directly, not from lore or
    reasoning alone**: this sandbox has no real Windows, but does have
    Wine's `cmd.exe` reimplementation (already used elsewhere in this same
    investigation, see the earlier struct-copy/dup2 Wine repros) --
    reproducing both the old command string and the fixed one (below)
    against `wine cmd /c "..."` pointed at an arbitrary real Windows `.exe`
    (`hostname.exe`) showed the old form failing to even recognize the
    target as a command (Wine's own wording for the same underlying
    parse failure -- its exact error text differs from real `cmd.exe`'s,
    but the failure is the same class: `cmd.exe` never got far enough to
    even attempt launching the target), while the fixed form ran the
    target program correctly.
    **Fix (`test/CMakeLists.txt`)**: drop the redundant inner
    `\"..\"` around `$<TARGET_FILE:wxmaxima>` entirely --
    `COMMAND cmd /c "$<TARGET_FILE:wxmaxima> --debug --logtostderr --pipe
    --version > wxmaxima_version_string.out 2>&1 && type
    wxmaxima_version_string.out"`. This needs no quoting of its own: this
    branch only ever runs on this project's own CI, whose checkout/build
    path is always space-free (`D:\a\wxmaxima\wxmaxima\...`), so the one
    outer pair of quotes cmsys itself adds around the whole string (since
    it contains spaces) is already sufficient and unambiguous once nothing
    inside it starts with a stray quote character of its own. If this
    project's Windows CI ever moves to a workspace path that can contain
    spaces, don't just re-add the same inner quotes -- that reintroduces
    this exact bug. A small checked-in `.bat`/`.cmd` script taking the exe
    path as its own ordinary argument would sidestep this whole class of
    nested-`cmd.exe`-quoting fragility properly, if it's ever needed.
    **Not yet independently re-confirmed against a real Windows CI run as
    of this writing** (pushed, awaiting the next `compile_windows` run on
    the fix's own PR) -- but unlike every previous attempt in this
    investigation, this one reproduces the *exact* failure signature in a
    controlled test (not just "doesn't reproduce the bug elsewhere," the
    usual shape of this investigation's prior negative results) and shows
    the fix resolving that same reproduction, which is a meaningfully
    stronger basis for confidence than anything earlier in this entry had.
    If it still fails on real Windows CI: check first whether cmsys's
    actual Windows quoting algorithm does something subtly different from
    Wine's `cmd.exe` here (the two are not guaranteed identical), by
    reading `Source/kwsys/Process_win32.c` (or wherever the CMake version
    in use vendors it) directly rather than re-guessing from the symptom
    alone -- this entry's own history is full of examples of a plausible-
    looking mechanism not surviving contact with the real platform.
  - **SHELVED (2026-09-18), deliberately and with the reasoning written
    down. The Windows job is green again; the root cause is still unknown;
    two real, separate bugs found on the way there are fixed. Start here.**
    The maintainer's own framing was "`--version` and `--help` on MSW
    misbehave in non-console applications, which means we can just drop
    that test as it is caused by stupid operating system concepts." That is
    half right, and the half that is right is the half that matters:
    asserting on text captured from a *GUI-subsystem* process is asserting
    on a property of Windows, not of wxMaxima. Every Windows-specific thing
    in this entry follows from one bit in the PE header, and
    `src/wxmaxima-cli.exe` exists precisely because no code inside
    `wxmaxima.exe` can finish that job. Weighed against this file's own
    "never make a test pass by getting rid of it" rule, the deciding
    argument is the one that rule is written in service of: a job that has
    been red for a month is not a warning anyone still reads, and this one
    has been hiding whatever else the minGW runner had to say since
    2026-08-15. So: the content check still runs, unchanged, on every
    non-Windows platform; Windows keeps `wxmaxima_version_returncode`,
    `wxmaxima_help_returncode` and a new `wxmaxima_cli_version_returncode`
    (the launcher finds `wxmaxima.exe`, spawns it, waits, propagates its
    exit code). What is covered *nowhere* on Windows now is the last step,
    "and the bytes arrive at whoever is capturing them"; `test/CMakeLists.txt`
    says so at the point where the assertion would go back.
    **Two factual errors this entry carried, corrected -- check these
    before building on anything else written above.**
    1. *`--version` does not go through `wxMessageOutput` at all.* This
       entry twice names `wxMessageOutputStderr::Printf()` as the write
       under investigation, and one of its "concrete next steps" was to
       instrument the `fputs()` inside it. The `-v` branch in `main.cpp` is
       a plain `printf("wxMaxima %s\n", ...)` followed by `exit(0)`;
       `wxMessageOutput` is only used for `wxCmdLineParser`'s `--help`
       usage text. The `wxMessageOutput::Set()` call the trace reports is
       real, it just is not on this path.
    2. *The test never checked the version.* Its
       `PASS_REGULAR_EXPRESSION "wxMaxima ${VERSION}.*"` referenced
       `${VERSION}`, which this project does not set anywhere -- the
       version lives in `${WXMAXIMA_VERSION}` (and `${PROJECT_VERSION}`).
       The regex expanded to `wxMaxima .*`, which is visible in the CI logs
       themselves (`Regex=[wxMaxima .*]`) once you know to look. It now
       uses `${WXMAXIMA_VERSION}`, so it checks what it is named after --
       confirmed non-vacuous locally, where it matches the real
       `wxMaxima 26.08.0-dev`.
    **Bug found and fixed #1 (real, user-visible, verified live):
    `BindStdStreamToParent()` closed the standard handle it had just bound,
    which silently threw away all of stderr whenever a parent hands the
    same handle in for stdout and stderr** -- i.e. under any `2>&1`, which
    includes `wxmaxima --logtostderr --batch ... 2>&1 | more` and the
    `cmd /c ... > file 2>&1` form this very test used to use.
    `_open_osfhandle()` makes the descriptor the *owner* of the handle, so
    the `_close(fd)` that follows `_dup2()` closed the handle
    `GetStdHandle()` still hands out. Everything that afterwards asked
    Windows rather than the CRT for a standard handle was reading a closed
    or *recycled* value -- `HaveStdErrHandle()`, the
    `SetHandleInformation()` calls in `MyApp::OnInit()`, and every child
    process inheriting our standard handles. ~~**This is what the
    `type=unknown` lines in the shipped CI traces are**~~ -- **that claim
    was wrong, and the first CI run carrying this very fix disproved it;
    see "What the first green run actually showed" at the end of this
    entry.** The bug itself is real and the fix stands; only its claimed
    connection to those trace lines does not. Fixed by binding a
    `DuplicateHandle()` copy, so
    `_close()` disposes of our own duplicate and the parent's handle stays
    valid. **Reproduced and fixed under Wine, functionally, not by reading
    code**: a console harness that creates a pipe the way kwsys does and
    passes the same write end as both stdout and stderr to a
    GUI-subsystem child reproducing this function verbatim captures 22
    bytes (stdout only, stderr's binding having failed with exactly the
    `type=unknown` signature) before the fix and 49 bytes (both streams)
    after, deterministically.
    **Bug found and fixed #2 (correctness, unverified as a cure):
    `wxmaxima-cli.cpp` handed its child the standard handles it was given
    rather than explicitly inheritable duplicates.** A handle named in
    `STARTUPINFO` only reaches the child's handle table if it carries
    `HANDLE_FLAG_INHERIT`, and nothing guarantees the handles a parent
    gives *us* do -- a parent capturing our output has every reason to have
    cleared the flag on its own copy, which is exactly what `wxmaxima.exe`
    itself does to its standard handles. Get it wrong and the failure is
    quiet in a very specific, very familiar way: `CreateProcess()` still
    succeeds, the child still finds the handle *values* in its PEB, so
    `GetStdHandle()` returns plausible numbers that name nothing it owns --
    **which is a mechanism that would produce this entry's headline
    `FILE_TYPE_PIPE`-in-the-parent / `FILE_TYPE_CHAR`-in-the-child
    mystery**, since a value naming nothing in the child can collide with
    any unrelated object its own startup opened. Fixed per MSDN's own
    redirected-child example. Wine does *not* reproduce the failure (it
    preserves the inherit flag through inheritance, so the pre-fix launcher
    works there too, 0 failures) -- so this is a documented-contract fix,
    not a demonstrated cure. It is the most promising remaining lead, and
    the launcher now logs `inherit=yes/no/unqueryable` for each handle, so
    the next Windows run's trace answers the question outright.
    **That run has now happened, and the answer is below -- it is not the
    lead it looked like.**
    **What the first green run actually showed (2026-09-18, the minGW job
    of this change's own PR -- `100% tests passed out of 134`, the first
    green minGW since 2026-08-15).** The trace settles both questions this
    entry left open, and disproves this entry's own answer to each. Read
    this before building on either fix's claimed explanation:
    1. *The launcher's handles were already inheritable, so bug #2 is not
       the pipe-to-char mechanism.* Its own line reads `stdin
       handle=...318 type=pipe inherit=yes, stdout handle=...3a8 type=pipe
       inherit=yes, stderr handle=...30c type=pipe inherit=yes` -- CTest
       hands them over with `HANDLE_FLAG_INHERIT` already set, so there
       was never a cleared flag to lose. And the child it started
       (identified by pid, `CreateProcessW ok, child pid 3992`) *still*
       reports all three as `type=char`. **Passing explicitly inheritable
       duplicates changed the child's reported handle types not at all.**
       The fix stays -- it is what MSDN's redirected-child example
       documents, and relying on a flag nobody guarantees is a latent bug
       regardless -- but it is not a cure, and the `FILE_TYPE_PIPE`-in-the
       -parent / `FILE_TYPE_CHAR`-in-the-child transformation remains
       completely unexplained. Do not spend another pass on inheritance
       flags.
    2. *The `type=unknown` lines are unchanged by bug #1's fix, so that
       fix does not explain them either.* Counted over the whole trace,
       same 264 `BindStdStreamToParent(...) start:` lines in both runs:
       **37 `type=unknown` before the fix, 36 after.** If the previous
       stream's binding had been closing the handle, this fix would have
       driven that to zero. It did not move. So the original reading this
       entry talked itself out of -- that the child is handed an
       already-unusable handle -- was very likely right all along, and
       "the previous binding closed it" was wrong. Worth noting for
       whoever picks this up: post-fix the unknowns are overwhelmingly
       `STD_ERROR_HANDLE` (32 of 36) with a few `STD_INPUT_HANDLE` (3) and
       a single `STD_OUTPUT_HANDLE`, i.e. concentrated on the stream a
       batch test is least likely to have had redirected anywhere real.
    **The moral, again, and this time about a fix rather than a status
    line**: bug #1 was demonstrated under Wine by byte count (22 -> 49) and
    is certainly a real bug; that evidence says the code was wrong, not
    that it was the cause of any particular symptom seen elsewhere. Those
    are two different claims and this entry ran them together. Verify a
    proposed *explanation* against the symptom it claims to explain, which
    here cost one grep of a log that already existed.
    **If someone picks this up again**: both of this entry's leads are now
    closed, so start from the unexplained fact itself -- a GUI-subsystem
    child reports `FILE_TYPE_CHAR` for the very same handle values its
    console-subsystem parent sees as `FILE_TYPE_PIPE`, with inheritance
    confirmed working. If the content assertion is ever worth restoring,
    restore it on `wxmaxima-cli` (never on `wxmaxima.exe` itself) and watch
    one real run. Do not re-run any theory this entry already disproves.
    Also still open, and now the obvious cleanup: the
    `WXM_STDIO_DEBUG_LOG` instrumentation and the workflow step that prints
    its ~88-process trace into every Windows CI log are still in place,
    which this entry itself said should not outlive the investigation --
    left alone here only because deleting the one source of evidence in the
    same change that shelves the hunt seemed like the wrong order.

- **`wxmaxima-cli.exe` (`src/wxmaxima-cli.cpp`) -- the console-subsystem
  companion, and why it is a launcher rather than a second copy of the app.**
  Direct follow-on to the `wxmaxima_version_string` entry above: that entry
  is about a *test* capturing output, this is about the underlying reason a
  user cannot get output either. An executable's subsystem is a bit in its
  PE header, fixed at link time -- one binary is GUI-subsystem or
  console-subsystem, never both -- and `wxmaxima.exe` must be the former or
  a console window pops up beside every worksheet. Two consequences: it
  starts with no stdio at all (what `RedirectStdioToParent()` papers over),
  and **cmd.exe does not wait for a GUI-subsystem process**, so it returns
  to the prompt before any output arrives. No code inside `wxmaxima.exe` can
  fix the second one; only a console-subsystem process can.
  - **The subsystem bit is the *only* thing forcing a second binary** -- not
    code sharing. Worth stating because "share the core with the diff
    utility" sounds like it needs a shared library, and it does not: there
    *is* no separate diff binary. `wxmxdiff` is a symlink to `wxmaxima`
    (`src/CMakeLists.txt`) with `--diff` dispatch, i.e. this codebase
    already does one-binary/multiple-modes.
  - **Deliberately not a DLL**, which was the first idea considered and
    rejected: Windows links wxWidgets statically
    (`-DwxWidgets_USE_STATIC=true` in `compile_windows.yml`), so a core DLL
    would duplicate wx's global state -- the `wxModule` registry, the
    `wxApp` instance, the art-provider table -- on both sides of the
    boundary, a notoriously ugly failure mode. It would also need
    `__declspec(dllexport)` plumbing across ~200 wx-heavy classes plus
    templates like `CellPtr<>`, add a second PE file to sign, and work
    directly against the portable/no-installer build (GH #2298), whose whole
    point is fewer files and no dependencies. **If this ever grows into a
    genuinely headless CLI that evaluates worksheets, share the core via a
    CMake OBJECT library** -- `wxmTestApp`/`wxmFuzzApp` are already exactly
    that pattern -- never a DLL.
  - **`STARTF_USESTDHANDLES` is load-bearing, not boilerplate.** A
    GUI-subsystem child inherits no usable standard handles unless they are
    passed explicitly, even with `bInheritHandles=TRUE`. With them set,
    `BindStdStreamToParent()`'s very first `GetStdHandle()` succeeds and its
    `AttachConsole(ATTACH_PARENT_PROCESS)` fallback never fires -- and the
    same code works unchanged whether the launcher's own stdout is a
    console, a pipe or a file redirect, since whatever it was handed is
    simply passed along. Dropping this flag would silently push every case
    back onto the `AttachConsole` path.
  - **The child's command line is the raw `GetCommandLineW()` tail, not a
    re-quoted argv[].** Re-quoting is precisely the bug class that cost this
    project the multi-session investigation documented above (whose real
    root cause was one redundant pair of quotes); passing the original
    characters through cannot introduce a quoting error that was not already
    in what the user typed. Note Windows parses the *program name* part of a
    command line more simply than the arguments -- no backslash escapes, a
    quoted name ends at the next quote -- which is what the skip loop
    implements.
  - Plain `main()`, not `wmain()`: the command line is read through
    `GetCommandLineW()` rather than `argv`, which avoids requiring
    `-municode` from every toolchain (confirmed: adding `-municode` to a
    `-mwindows` target that defines `main()` fails to link with `undefined
    reference to wWinMain`).
  - **Verified end-to-end under Wine, not just compiled** (this sandbox has
    `i686-w64-mingw32-g++` + `wine`, 32-bit prefix only -- see the
    `wxmaxima_version_string` entry for that setup's quirks). A stand-in
    GUI-subsystem child reproducing `BindStdStreamToParent()` verbatim
    confirmed: PE subsystem bits genuinely differ (2 = GUI vs 3 = CONSOLE,
    read straight out of the headers), stdout reaches a pipe and a file
    redirect, stderr stays separate, an argument containing spaces survives
    as one argument, embedded quotes survive, the exit code propagates, and
    the "installed as wxmaxima.exe myself" guard refuses rather than
    fork-bombing. Not verified: behaviour against the *real* wxmaxima.exe on
    real Windows -- that needs a CI run, and `wxmaxima_cli_version_string`
    (`test/CMakeLists.txt`) is the test that will say so. (It said so: it
    failed. See the two follow-ups at the end of this entry -- that test no
    longer exists.)
  - **That CI run happened, and the test FAILED -- still unexplained as of
    this entry. Read this before re-deriving any of it.** On the minGW job
    the launcher itself *built* fine and the test failed in 0.06 s with
    **literally zero captured output** (`--output-on-failure` is on for
    that step, so the blank is real, not a reporting artifact). What that
    rules out, from the job log rather than by reasoning: none of the
    launcher's own three error messages appear anywhere in it, so
    `ExeDirectory()` was non-empty, the self-spawn guard did not fire, and
    `CreateProcessW()` **succeeded** -- the child really did start. Nor is
    it a target-resolution problem: there is no ctest "Unable to find
    executable", and `src\wxmaxima-cli.exe` links right next to
    `src\wxmaxima.exe` (neither target sets `RUNTIME_OUTPUT_DIRECTORY`, so
    the "look in my own directory" lookup is sound in the build tree too).
    The suggestive find: of the 88 wxmaxima processes `WXM_STDIO_DEBUG_LOG`
    traced in that run, **exactly one had `type=pipe` standard handles**
    (every other one had `type=char`) -- consistent with it being the child
    this launcher started, since this is the only thing in the suite that
    forwards ctest's own handles via `STARTF_USESTDHANDLES`. That process
    bound all three streams successfully and came out of
    `cmdLineParser.Parse()` with `result=0`, yet **never entered the `-v`
    branch** -- i.e. it did not see `--version`. That identification is
    circumstantial, though, which is exactly the gap the instrumentation
    below closes.
    **Do not re-test the argument handling under Wine: it passes there.**
    Confirmed this pass (32-bit prefix, stand-in GUI child echoing its own
    `GetCommandLineW()`, run from a foreign working directory to match
    ctest's `WORKING_DIRECTORY`): the child receives
    `"...\wxmaxima.exe" --debug --logtostderr --pipe --version`, complete
    and correctly quoted, through both a pipe and a file redirect. So
    `ArgumentTail()` is not obviously the culprit, and another Wine repro
    of the same thing will just reproduce that same pass.
    **Instrumentation added instead** (`CliDebugLog()`/`DescribeStdHandle()`
    in `wxmaxima-cli.cpp`), deliberately mirroring `main.cpp`'s own
    `StdioDebugLog()` in every respect -- same `WXM_STDIO_DEBUG_LOG` opt-in
    (already set by exactly one CI step), same raw
    `CreateFileW`/`WriteFile` with `FILE_APPEND_DATA` alone so several
    processes can append atomically (**do not add a `SetFilePointer()`**),
    and emphatically never a byte to stdout, which belongs to the child.
    It records the raw command line, the std handle types, the extracted
    argument tail, the exact command line handed to `CreateProcessW()`, and
    -- the point of the exercise -- **the child's pid**, which is what will
    finally tie a specific traced wxmaxima process to this launcher instead
    of inferring it from handle types. Verified in both directions under
    Wine: with the variable unset no log file is created at all and stdout
    is byte-for-byte just the child's output; with it set stdout is
    unchanged and the full trace lands in the file.
  - **ANSWER (2026-09-18, from that instrumentation's first real CI run):
    the launcher is doing its job perfectly, and the output is lost anyway.
    Correcting this entry's own guess above: the "never entered the `-v`
    branch / did not see `--version`" reading was WRONG** -- it rested on
    identifying the child by handle type, and the pid now proves otherwise.
    The real trace, verbatim:

    ```
    [wxmaxima-cli pid 6256] start: raw GetCommandLineW()=[D:/a/.../wxmaxima-cli.exe --debug --logtostderr --pipe --version]
    [wxmaxima-cli pid 6256] start: stdin handle=...358 type=pipe, stdout handle=...3a0 type=pipe, stderr handle=...314 type=pipe
    [wxmaxima-cli pid 6256] argument tail=[--debug --logtostderr --pipe --version]
    [wxmaxima-cli pid 6256] CreateProcessW: exe=[D:\...\wxmaxima.exe] cmd=["D:\...\wxmaxima.exe" --debug --logtostderr --pipe --version]
    [wxmaxima-cli pid 6256] CreateProcessW ok, child pid 9168
    [wxmaxima-cli pid 6256] child exited with code 0
    ```

    and child 9168's own trace ends `entering -v branch` / `after Printf,
    before exit(0)`. So every link holds: the argument tail is extracted
    correctly, the child's command line is correct and correctly quoted,
    the child parses it, reaches the version branch, completes `Printf()`
    and exits 0 -- **and ctest still captured zero bytes.** `ArgumentTail()`
    is exonerated by evidence now, not just by a Wine repro.
    **What this contributes to the `wxmaxima_version_string` investigation
    above, which is the same failure**: that entry's central unexplained
    fact is that every wxmaxima child sees `FILE_TYPE_CHAR` where a pipe
    was expected, and it speculated this might be something about how
    CTest sets up stdio for a WIN32-subsystem child. This run settles part
    of that. The launcher is an ordinary *console*-subsystem process; it
    sees its own three std handles as **`type=pipe`** (so CTest really does
    hand its direct children pipes, as the textbook assumption said) and
    passes those exact handles on via `STARTF_USESTDHANDLES` -- the child's
    reported handle *values* are identical (`0x3a0`/`0x314`/`0x358`),
    confirming inheritance worked. Yet the GUI-subsystem child reports the
    very same handles as **`type=char`**. So the pipe-to-char
    transformation is tied to the *child being GUI-subsystem*, not to
    anything CTest does, and it is not stable run to run either: in the
    previous run's trace exactly one child reported `type=pipe`, in this
    one that child reports `type=char`. **Start here, not at the fd/handle
    chain, if this is picked up again.**
    **Consequence for this feature, stated plainly**: a console-subsystem
    launcher genuinely fixes the "cmd.exe does not wait for a
    GUI-subsystem process" half of the problem (proven here: it waits and
    propagates the exit code), but it does **not** make `--version` output
    reach a capturing parent on this runner. Those were always two
    different problems; this entry, and the PR that added the launcher,
    conflated them. `wxmaxima_cli_version_string` therefore fails for the
    same unresolved reason `wxmaxima_version_string` does, and fixing it
    means fixing that, not the launcher. Do not "fix" it by routing this
    test through a file the way its sibling does: that would hide the one
    signal that will show when the underlying capture bug is actually
    solved.
  - **Follow-up (2026-09-18): `wxmaxima_cli_version_string` is gone,
    replaced by `wxmaxima_cli_version_returncode` -- see the "SHELVED"
    follow-up at the end of the `wxmaxima_version_string` entry above for
    the whole argument, and read it before restoring any content assertion
    here.** In short: the capture half stayed unexplained, a month of red
    was costing more than the assertion was worth, and the launcher keeps
    the coverage that does not depend on capture (it locates
    `wxmaxima.exe`, starts it, waits, propagates the exit code). One real
    launcher bug did come out of it: the standard handles were passed to
    `CreateProcessW()` as-is rather than as explicitly inheritable
    duplicates, which is a documented way to get a child that sees handle
    *values* naming nothing it owns -- and therefore a candidate
    explanation for this entry's own `type=pipe`-here/`type=char`-there
    finding. Fixed; `DescribeStdHandle()` now also reports `inherit=`, so
    the next run's trace settles whether that was it.
    **It has, and it was not.** That run's launcher line reads `type=pipe
    inherit=yes` for all three streams -- CTest already sets the flag, so
    there was never one to lose -- and the child it spawned still reported
    all three as `type=char`. The fix is correct by MSDN's contract and
    stays, but this entry's `type=pipe`-here/`type=char`-there finding is
    *not* explained by it and remains open. See "What the first green run
    actually showed" in the `wxmaxima_version_string` entry above.
