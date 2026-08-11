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
  root element must be `zip-file`.
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
