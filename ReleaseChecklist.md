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
  body taken from the topmost section of `NEWS.md` that has any content
  (see the `NEWS.md` step below).
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
- Update `NEWS.md`. Its `# Current development version` section becomes the
  release notes on GitHub and the announcement on <https://freshcode.club/>,
  and during development it collects an entry for every change, often with
  more detail than a user needs. Before tagging:
  - **Shorten it** to what a user of the previous release needs to know:
    group related entries by topic, cut implementation detail, and start
    with a short paragraph naming the release's main topics.
  - **Drop entries about things the previous release didn't have.** A fix
    to, or a change of, a feature that is itself new in this release is no
    news to anybody who upgrades: fold it into that feature's own entry, or
    remove it. Check by looking for the feature in the previous release's
    section of `NEWS.md` (or with `git show Version-<previous>:NEWS.md`).
  - **Rename the section to the release**: the heading
    `# Current development version` becomes `# <x.y.z>`. Then put a fresh,
    empty `# Current development version` heading above it, so that the
    next change has somewhere to go. The release workflow
    (`compile_windows.yml`, "Extract release notes from NEWS.md") skips all
    leading `# ` headings and blank lines and takes everything up to the next
    `# ` heading, so it finds the release's notes either way.
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
