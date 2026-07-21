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
- After the release is published, verify it as an anonymous user (log out of
  GitHub and check the release page).
- Update the release info in `download.html` and `version.txt` in the
  `gh_pages` branch.
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
