# wxMaxima's Translations

Translations are an important aspect of open-source programs. Any help with them
is always appreciated.

The subdirectories of this folder contain the translations of

- wxMaxima itself
- wxWidgets, the framework wxMaxima makes extensive use of
- wxMaxima's manual

### Translating wxMaxima to a new language

In order to translate wxMaxima to a entirely new language two things have to be done:

- Translate the .pot files to your language. If you don't want to translate
  everything in one go that is completely fine: Quality is more important than
  quantity here and getmessage is able to work with a partially-translated program
  just fine; If the .pot files in the repository change whilst translation is
- ongoing that is not the end of the world, neither: msgmerge will be able to
  sort this out, too.
- For Linux desktop integration additionally
  data/io.github.wxmaxima_developers.wxMaxima.desktop needs to be equipped with
  a translation.

### Improving an existing translation

In order to improve an existing translation just edit the according .po file.
Any help is always welcome.
