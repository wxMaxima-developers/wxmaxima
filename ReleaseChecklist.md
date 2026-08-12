# wxMaxima Release Checklist

## GitHub documentation about managing releases:

<https://docs.github.com/en/github/administering-a-repository/managing-releases-in-a-repository>

## What CI now does automatically

Pushing an **annotated tag whose name starts with `Version`** triggers the
release automation. You no longer need to build or upload the platform
binaries, create the release, or un-draft it by hand:

- All platforms are built as **Release** builds and attached to the GitHub
  release for that tag:
  - Windows: the NSIS installer (`.exe`) — `compile_windows.yml`
  - macOS: the `.dmg` — `compile_mac.yml`
  - Linux: the `.deb` — `compile_ubuntu.yml`
  - Windows on Arm (ARM64): the self-contained portable `.zip`
    — `compile_windows_arm.yml`
- The release is created (or updated) and un-drafted automatically, with its
  body taken from the `# Current development version` section of `NEWS.md`.
- `download.html`/`version.txt` on the `gh-pages` branch are updated
  automatically (`update_website_version.yml`) with the tag's version number.
  No manual edit needed.
- The Windows installer is submitted to SignPath for code signing
  (`compile_windows.yml`'s `sign_windows` job). This only runs on a
  `Version-*` tag push or a manual `workflow_dispatch` run of that
  workflow -- an ordinary push/PR always shows it as skipped, which is
  expected, not a problem. The job needs approval in the `SIGNPATH_API_TOKEN`
  GitHub Environment (Settings -> Environments) before it proceeds; that
  approval prompt only appears once the job actually queues, i.e. only after
  a real tag push (or manual dispatch) -- not before.

Other checks that now run on every push (so they can't surprise you at release
time):

- Full build + unit/integration tests on all platforms.
- `appstream-util validate` of the appdata file (`compile_ubuntu.yml`).
- The snap package builds from `snap/snapcraft.yaml` (`build_snap.yml`).

## Manual steps (still required)

- Make sure the current git version compiles and **all GitHub checks are
  green** — the release jobs only publish after their build+tests pass.
- Enter the new version number into `CMakeLists.txt`.
- Update `NEWS.md` (this also becomes the release notes / the announcement on
  <https://freshcode.club/>).
- Update `data/io.github.wxmaxima_developers.wxMaxima.appdata.xml` with the new
  `<release>` entry. Most HTML tags are forbidden by the flatpak/appImage
  builders. (CI now validates the file, but you still write the entry.)
- Update `snap/snapcraft.yaml` (at least the `version:`). CI now *builds* the
  snap, but does not bump its version for you.
- Update the version numbers in the `docker-wxmaxima` repository
  (`update_versions.sh`).
- Update the included HTML manuals.
- Confirm `test/testbench_simple.wxmx` still works.
- Create an **annotated** tag: `git tag -a Version-<x.y.z>` and push it:
  `git push origin --tags`. **This push is what triggers the automated build +
  release above.**
- Approve the SignPath signing request: once `compile_windows.yml` runs on the
  tag, its `sign_windows` job pauses for approval in the `SIGNPATH_API_TOKEN`
  GitHub Environment. Approve it from the workflow run's page (or Settings ->
  Environments -> SIGNPATH_API_TOKEN) so the Windows installer actually gets
  signed instead of the job timing out unapproved.
- After the release is published, verify it as an anonymous user (log out of
  GitHub and check the release page).
- Download the source tarball (`.tar.gz` and `.zip`) and sign each:
  `gpg --armor --detach-sign <filename>`, then add the two `.asc` files to the
  release page. (Signing needs your private key and is therefore still manual.)
- In Maxima's source tree, `crosscompile-windows/wxmaxima/CMakeLists.txt`:
  update the version number and the MD5 sum of the release tarball.

### Superseded

- The old "create a Windows installer using the *Crosscompiled-Windows-installer*
  repository and add it to the release" step is superseded by the Windows
  installer that `compile_windows.yml` now builds and attaches automatically.
  Keep it only as a fallback if the CI installer is ever unavailable.
