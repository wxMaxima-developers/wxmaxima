---
name: wxmaxima-translations
description: How wxMaxima's translations work and the many ways they have silently been lost - the xgettext source glob, msgmerge flags, the combined UI+manual .po files, po4a's data-destruction habits, and the Crowdin sync races. Use before touching anything under locales/, info/*.md, po4a.cfg, or any CI job that regenerates the POT or the translated manuals.
---

# Translations: what breaks, and how it breaks silently

Translations in this project have been lost repeatedly, always quietly, and
usually only noticed months later. Every rule here exists because something
disappeared. Treat "the build changed a file under `locales/` or `info/`" as a
thing to look at, never as noise.

## The layout

- **One combined `.po` per language**, `locales/wxMaxima/<lang>.po` - the file a
  translator actually edits. It holds **both** wxMaxima's UI strings
  (xgettext-extracted from the sources) **and** the manual's prose
  (po4a-extracted from `info/wxmaxima.md`).
- `locales/wxMaxima/wxMaxima.pot` is the template both `msgmerge` and Crowdin
  work against. It is regenerated as the union of a fresh source scan and
  `locales/manual/wxmaxima.md.pot`, by the `update-locale` target - and by
  nothing else.
- `locales/manual/<lang>.po` is po4a's own file, merged into the combined one by
  `merge_manual_po.cmake`.

## The failures, in order of how much they cost

**1. The xgettext source list is an explicit glob, not a recursive one.**
`POT_SOURCE_FILES` / `POT_SOURCE_FILES_REL` in `locales/wxMaxima/CMakeLists.txt`
list `src/*.cpp;src/*.h;src/*/*.cpp;src/*/*.h` - exactly two levels. It was flat
`src/*` until 2026-07, so every string under `src/cells`, `src/wizards` (since
2020-08) and `src/sidebars`, `src/dialogs` (since 2024-01) was missing from the
POT. Cost: **~7000 translations across 21 languages**, recovered from git
history only in 2026-08.

Nothing warned about it. xgettext is happy with a short file list, and the CI
POT-drift check regenerates the POT and diffs it - a broken glob truncates both
sides identically, so the check passes. That hole is now covered by the
`check-pot-coverage` ctest, which fails if a source file sits deeper than the
glob reaches or if a file containing `_("...")` is unreferenced by the POT.
**Adding a `src/<a>/<b>/` level breaks the glob again**; the test will tell you.

**2. `po4a` must never be pointed at `locales/wxMaxima/<lang>.po`.** It does not
add entries to a `.po` - it treats it as *its own* and rewrites it to contain
only what it extracted from the manual, discarding everything else. Confirmed
live: a language with 1000 translated UI strings dropped to 69. This shipped to
`main` once already.

**3. po4a still deletes content, and a plain build triggers it.** An ordinary
`ninja` rewrites the eight tracked `info/wxmaxima.*.md` manuals, and at the time
of writing *removes* previously-present English paragraphs from them. So:
**never `git add -A` after a build** in this repo, and any CI job that
regenerates translations must commit an explicit path list and fail loudly if
anything else changed. Also: po4a < 0.70 can silently corrupt translated text
and the build is supposed to reject it.

**4. Crowdin syncs can wipe translations.** Not theoretical - a sync race wiped
464 translations across 16 catalogues one day after a restore, and an earlier
"initial project sync" wiped UI translations wholesale. When you restore or bulk-
edit catalogues, the change has to reach Crowdin too, or the next sync undoes
it. Actively-synced catalogues also have their obsolete (`#~`) entries pruned,
which is what turned failure 1 from recoverable into archaeology.

**5. Don't drop `msgmerge --previous`.** It is what preserves the `#| msgid`
comment recording what a fuzzy entry used to say - the only way a translator can
see *why* something went fuzzy. A plain `msgmerge` discards those silently
(212 entries' worth in `zh_CN.po` alone).

## Recovering lost translations

They are usually still in git history. The verified recipe, per language:

```sh
# keep only genuinely translated entries from each historic snapshot
msgattrib --translated --no-fuzzy -o snap.tr.po <historic>.po
# newest translation wins
msgcat --use-first newer.tr.po older.tr.po -o compendium.po
# fill only what is currently untranslated, exact msgid matches only
msgmerge --quiet --no-wrap --previous --compendium=compendium.po \
         --no-fuzzy-matching <lang>.po wxMaxima.pot -o <lang>.po
```

`msgattrib` first is load-bearing: `msgcat --use-first` will otherwise happily
pick an *empty* msgstr from the newer file. `--no-fuzzy-matching` keeps it to
exact matches, so a restored string is exactly what a translator once wrote -
restore them unfuzzied, since fuzzy entries are not used at runtime.

Two shapes of catalogue need different treatment: one that already has the
entries but empty can be filled **in place** (one changed line per string, no
reordering); one that is missing the entries entirely needs a real `msgmerge`
first, because no in-place fill can reach an entry that does not exist.

## Manual PDF composition can fail for reasons unrelated to translation (GH #2271)

`info/CMakeLists.txt`'s `generate_wxmaxima_documentation_pdf()` used to pass
`-V 'CJKmainfont:NotoSerifCJK-Regular.ttc'` to pandoc **unconditionally, for
every language**, not only the CJK ones (`zh_CN`/`zh_TW`). That makes pandoc
emit `\usepackage{xeCJK}` into every language's generated LaTeX regardless of
content, so composing *any* manual PDF -- Hungarian included, despite having
no CJK text at all, since it has no real translation yet and silently falls
back to the English `wxmaxima.md` (see `info/CMakeLists.txt`'s `else()`
branch a few lines above `generate_wxmaxima_documentation_pdf` for that
fallback) -- hard-depended on `texlive-lang-cjk`, a large package genuinely
easy not to have on a "normal" (not `texlive-full`) install. Confirmed live:
reproduced `! LaTeX Error: File 'xeCJK.sty' not found.` for `hu` with the CJK
package removed, and confirmed the same command with the `-V CJKmainfont`
argument dropped composes cleanly with the identical package set. Fixed by
only adding that argument when `PDFLANG MATCHES "^zh"`.
**A CI reproduction of this class of bug is not realistic**: CI has no
`texlive-xetex`/`texlive-lang-cjk` installed at all for the unit-test job (the
whole PDF target is silently skipped via `if(WXMAXIMA_LATEX_COMMAND)` when
neither `xelatex` nor `lualatex` is found), so a *partial* TeX install -
exactly what exposed this - can only be caught by someone with a real,
non-`texlive-full` TeX toolchain building the manual locally, same as the
original reporter. If a similar "PDF for language X won't compose" report
comes in again, check first whether it reproduces for *every* language (a
generic, non-language-specific missing-package problem, the actual shape this
one turned out to have) before assuming the bug is specific to language X.
Installing a missing texlive package inside an agent sandbox one at a time as
each new `! LaTeX Error: File 'X' not found` appears (`lmodern` -> Noto fonts
-> `texlive-lang-cjk`, in that order, here) is a reasonable way to find every
missing piece **for reproduction purposes** - but doing so also leaves the
sandbox's LaTeX toolchain in a different state than before, which can make an
*unrelated*, previously-untested code path (e.g. `test_WorksheetExport`'s
`RequireTexCompiles`, gated on `pdflatex`/`lualatex` being on `PATH` at all)
start running for the first time and fail for reasons that have nothing to do
with the change under investigation (here: `lualatex`'s `luaotfload` font
cache failing to initialize under `ctest`'s per-test isolated, empty
`TMPDIR`/`XDG_CONFIG_HOME` - reproduced with a trivial `\usepackage{fontspec}`
"Hello world" document, so it is a toolchain/cache-initialization issue, not
anything in wxMaxima's own LaTeX export). Don't mistake a newly-appearing
failure in a previously-dormant (engine-not-installed-so-skipped) test for a
regression in unrelated code just because it started failing in the same
session a TeX toolchain got installed piecemeal.

## Checks that exist

- `check-translations` - `msgfmt` on every catalogue, chiefly `--check-format`
  (a msgstr whose format specifiers do not match its msgid makes wxWidgets
  format against the wrong arguments, in a language we may not read).
- `check-pot-coverage` - the glob guard described above. Note it runs via
  `cmake -P`, so it must set its own policies (`CMP0057` for `IN_LIST`);
  a script run that way inherits nothing from the top-level `CMakeLists.txt`.
- The CI POT-drift check - fails when the committed POT is stale.
