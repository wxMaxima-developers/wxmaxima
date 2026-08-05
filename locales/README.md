# wxMaxima's Translations

Translations are an important aspect of open-source programs. Any help with them
is always appreciated.

`wxMaxima/*.po` holds one combined catalog per language, covering both

- wxMaxima's own UI strings (extracted from the C++ source via `xgettext`), and
- wxMaxima's manual (extracted from `info/wxmaxima.md` via `po4a`)

`wxMaxima.pot` (the untranslated template `msgmerge`/Crowdin work against) is
the union of both sources: `make update-locale` regenerates it by combining a
fresh `xgettext` scan of the source with `manual/wxmaxima.md.pot` (itself kept
current by `make update-locale-manual-in-source`, which needs `po4a` >= 0.70 -
see `CheckPo4aVersion.cmake`) via `msgcat`. A translator therefore only has one
file per language to work in for both the UI and the manual.

`manual/*.po` still exists, one file per language that has a manual
translation: po4a owns that file exclusively and rewrites it wholesale on
every run (it does not just add/update entries - pointing it at a file that
also held UI strings silently dropped all of them, confirmed live). Each
`make update-locale` run folds `manual/<lang>.po`'s translations into
`wxMaxima/<lang>.po` (`merge_manual_po.cmake`, via `msgcat`) *before*
`msgmerge`, so a translator only has to look in `wxMaxima/<lang>.po` to find
or edit the manual's translations, even though po4a's own working copy lives
elsewhere.

## Translating wxMaxima to a new language

In order to translate wxMaxima to a entirely new language two things have to be done:

- Translate the .pot files to your language. If you don't want to translate
  everything in one go that is completely fine: Quality is more important than
  quantity here and getmessage is able to work with a partially-translated program
  just fine; If the .pot files in the repository change whilst translation is
  ongoing that is not the end of the world, neither: msgmerge will be able to
  sort this out, too.
- For Linux desktop integration additionally
  data/io.github.wxmaxima_developers.wxMaxima.desktop needs to be equipped with
  a translation.

WxMaxima also uses translations of the wxWidgets framework (e.g. for standard
dialogs).

## Improving an existing translation

In order to improve an existing translation just edit the according .po file.
Any help is always welcome.
