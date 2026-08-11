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
release notes are extracted as the "Current development version" section of
NEWS.md - which is why that section must be kept current as you go, not written
at release time.

## Third-party notices

Shipping a component means reproducing its licence: `THIRD-PARTY-NOTICES.txt`
plus a tab in the licence dialog. The WebView2 loader and the vendored nanoSVG
are the current cases. On nanoSVG specifically: the symbol-renamed private copy
stays deliberately - `wxBitmapBundle::FromSVG` caches every resize resolution
with no eviction. Do not "modernise" it away.
