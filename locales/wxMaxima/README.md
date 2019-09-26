The Translation of the program itself
=====================================

### Translating the program to a new language

In order to translate the manual to a entirely new language it is sufficient to 
equip wxMaxima.pot with the translations and to give the resulting file a name in 
the format `<language>.po`.

### Improving an existing translation

In order to improve an existing translation just edit the according .po file.
Any help is always welcome.

### Specialities of translating the program

 * `%i`, `%li` and `%s` are placeholders for integers, long integers and strings
 * `\n` is a newline character
 * Since every localized keyboard offers different buttons the hotkeys can be
   localized, too, even if it makes sense to try to make the hotkeys consistent
   between languages, wherever possible. In menus, button texts and similar 
   therefore `\t` marks that the text that now follows is is a description of the
   hotkey that activates this button/menu/...
