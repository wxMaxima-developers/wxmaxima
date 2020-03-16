The Translation of the manual
=============================

### Translating the manual to a new language

In order to translate the manual to a entirely new language it is sufficient to 
equip wxmaxima.md.pot with the translations and to give the resulting file a name in 
the format `<language>.po`.

### Improving an existing translation

In order to improve an existing translation just edit the according .po file.
Any help is always welcome.

### Specialities of the manual

[po4a](https://po4a.org) offers to translate the characters
that contain the formatting hints for the markdown parser, too. If you remove 
them or exchange them by something completely different the formatting will change, too.

### Why not to translate the manual file directly

Translating a text file is more convenient than translating a .po file.
But manually keeping the translation of a text file in sync with the original 
if the original changes is a complicated task.

Translating .po files is a more complicated than translating the original text.
But po4a keeps track about which paragraphs have already been translated or 
need to be translated which makes maintaining the translation easier.
