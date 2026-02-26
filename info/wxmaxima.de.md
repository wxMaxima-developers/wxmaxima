# Das Handbuch von wxMaxima

WxMaxima ist ein graphisches Benutzerinterface (GUI) für das
Computer-Algebrasystem Maxima. wxMaxima erlaubt die Nutzung aller Funktionen
dieses Programms und bietet Assistenten für dessen wichtigste Funktionen
an. Dieses Handbuch beschreibt die Features, die wxMaxima zu einer der
beliebtesten graphischen Benutzerumgebung für Maxima gemacht haben.

![wxMaxima Logo](./wxMaximaLogo.png){ id=img_wxMaximaLogo }

______________________________________________________________________

# Einführung in wxMaxima

## _Maxima_ und wxMaxima

Im Open-Source-Bereich ist es üblich, große Systeme in kleine Projekte
aufzuteilen, die jeweils klein genug sind, dass eine endliche Menge an
Entwicklern ausreicht, um sie weiterzuentwickeln. Beispielsweise ist ein
CD-Brennprogramm aus einem Kommandozeilentool zusammengesetzt, das die CDs
brennt und eine graphische Benutzerumgebung, die dieses Tool bedient. Ein
Vorteil dieses Ansatzes ist es, daß dasselbe Kommandozeilenprogramm von
vielen Stellen aus genutzt werden: Das Brennprogramm, ein "Sende an die
CD"-Knopf und beispielsweise ein Backup-Tool. Ein weiterer Vorteil ist es,
dass auf diese Weise für eine Funktionalität mehrere Benutzerinterfaces
angeboten werden können.

Ein Computer-Algebrasystem (CAS) wie _Maxima_ ist hervorragend für diesen
Ansatz geeignet: Es kann die Logik hinter einem Taschenrechner liefern, der
mit beliebig langen Zahlen hantieren kann, Formeln für ein größeres System,
z.B. [Sage](https://www.sagemath.org/) umstellen. Alternativ kann es direkt
verwendet werden. Dies kann von der Kommandozeile aus geschehen, oder von
wxMaxima aus, das eine komfortablere Bedienung unterstützt.

### _Maxima_

Maxima ist ein komplettes Computer-Algebrasystem (CAS): Ein Programm, das
die Formel, nicht nur die Zahl sucht, die ein mathematisches Problem
löst. Auch wenn es darauf  spezialisiert ist, mit Buchstaben zu rechnen,
bietet es auch eine Menge an Funktionen, die Probleme lösen, für die nur
numerische Lösungen existieren.

![Maxima Screenshot, Kommandozeile](./maxima_screenshot.png){
id=img_maxima_screenshot }

Umfangreiche Dokumentation für Maxima ist [im Internet
verfügbar](https://maxima.sourceforge.io/documentation.html). Teile davon
sind auch im Hilfe-Menü von wxMaxima verfügbar. Ein Druck auf die
Hilfe-Taste (auf den meisten Systemen ist dies die <kbd>F1</kbd>-Taste)
springt automatisch zur Stelle im _Maxima_-Handbuch, wo das Kommando unter
dem Cursor erklärt wird.

### WxMaxima

_wxMaxima_ ist ein graphisches Benutzer-Interface, das die komplette
Funktionalität und Flexibilität von Maxima zu nutzen erlaubt. Zudem bietet
es viele Funktionalitäten an, die die Verwendung von _Maxima_ einfacher
gestalten. Beispielsweise kann es den Inhalt eines Arbeitsblattes, einer
Zelle des Arbeitsblatts oder einen Teil einer Formel nach LaTeX, MathML oder
in eine RTF-Formel für eine Textverarbeitung konvertieren. Dokumentation für
wxMaxima kann [im
Internet](https://wxMaxima-developers.github.io/wxmaxima/help.html) gefunden
werden, oder über das Hilfemenü.

![wxMaxima Fenster](./wxMaximaWindow.png){ id=img_wxMaximaWindow }

_wxMaxima_ lässt alle Berechnungen im Hintergrund durch das
Kommandozeilen-Werkzeug _Maxima_ durchführen.

## Grundlagen zum Arbeitsblatt

wxMaxima ist größtenteils selbsterklärend. [Diese
Internetseite](https://wxMaxima-developers.github.io/wxmaxima/help.html)
bietet einige Beispiele an, wovon das "10-Minuten-Tutorial" besonders zu
empfehlen ist. Dieses Handbuch beschreibt einige zusätzliche Aspekte von
wxMaxima.

### Der Arbeitsblatt-Ansatz

Eine der Sachen, die neue Benutzer oft verwirrt, ist, dass das Arbeitsblatt
von wxMaxima in Zellen aufgeteilt ist, die nur auf Befehl vom Benutzer an
_Maxima_ gesendet werden. Wenn eine Zelle ausgewertet wird, werden alle
Maxima-Befehle (nur) dieser Zelle als Batch-Befehle ausgewertet. Man kann
auch mehrere Zellen auswählen und gleichzeitig auswählen. Oder das gesamte
Arbeitsblatt auf einmal auswerten _WxMaxima_'s Ansatz für die Auswertung von
Befehlen mag seltsam erscheinen. Es vereinfacht aber die Arbeit mit großen
Dokumenten (wo man nicht jede Änderung eine komplette Neu-Evaluierung des
ganzen Dokuments auslösen will. Ausserdem ist dieser Ansatz für die
Fehlersuche praktisch.

Wenn Text in _wxMaxima_ eingegeben wird, erzeugt er automatisch eine neue
Zelle des Arbeitsblattes. Wenn dies eine Code-Zelle ist, kann ihr Inhalt an
_Maxima_ gesendet werden und das Resultat dieser Aktion wird unter der Zelle
angezeigt, wie unten abgebildet.

![Eingabe/Ausgabe Zelle](./InputCell.png){ id=img_InputCell }

Wird eine Code-Zelle ausgewertet, weist ihr _Maxima_ eine Marke zu
(standardmäßig ist diese in roter Schrift gehalten und beginnt mit einem
`%i`), über das sie später wieder referenziert werden kann. Für die Ausgabe
wird eine Marke generiert, die standardmäßig mit `%o` beginnt und nicht
angezeigt wird, außer der Benutzer hat der Formel einen sprechenden Namen
gegeben, der stattdessen angezeigt werden kann. Der `%o`-Label den Maxima
automatisch erzeugt, ist trotzdem verwendbar.

Außer Code-Zellen kennt wxMaxima auch Textzellen und solche mit Bildern oder
Überschriften. Jede Zelle hat ihren eigenen Speicher für das
Rückgängigmachen von Aktionen, was sich oft als hilfreich erwiesen
hat. Zudem besitzt, wie in fast allen Applikationen, das Arbeitsblatt einen
eigenen Speicher für die Rückgängigmachen-Funktion.

Die nun folgende Abbildung zeigt verschiedene Zelltypen (Titelzellen,
Untertitelzellen, Textzellen, Eingabe/Ausgabezellen und eine Bildzelle).

![Ein Beispiel verschiedener wxMaxima Zelltypen](./cell-example.png)

### Zellen

Das Arbeitsblatt ist in Zellen aufgeteilt. WxMaxima kennt die folgenden
Zelltypen:

- Mathematik-Eingabezellen beinhalten eine oder mehrere Eingabe-Zeilen für
  _Maxima_
- Ausgaben oder Fragen von _Maxima_.
- Bild-Zellen.
- Text-Zellen, die beispielsweise für Dokumentation verwendet werden können.
- Ein Titel, Überschrift oder Unterüberschrift. 6 unterschiedliche
  Überschriftenebenen sind möglich.
- Seitenumbrüche.

Wenn Text eingegeben ist, erzeugt _wxMaxima_ normalerweise gleich eine
Code-Zelle. Andere Zelltypen können über das "Zellen"-Menü, die dort
dokumentierten Tastenkombinationen oder über die Werkzeugleiste erzeugt
werden. Wenn eine nicht-mathematische Zelle (Überschrift, Text, etc.)
erzeugt wurde, wird alles was eingetippt wird, als Text interpretiert.

Ein
[Kommentar](https://maxima.sourceforge.io/docs/manual/maxima_singlepage.html#Comments)
(wie in der Programmiersprache C) kann als Teil einer
Mathematik-Eingabezelle eingegeben werden: `/* Dieser Kommentar wird von
Maxima ignoriert */`.

"`/*`" markiert den Beginn des Kommentars, "`*/`" das Ende.

### Horizontale und vertikale Cursors

Wenn in einer Textverarbeitung versucht wird, einen Satz auszuwählen, wird
diese versuchen, Beginn und Ende der Auswahl so zu verschieben, dass ganze
Wörter ausgewählt sind. _wxMaxima_ wird aus diesem Grund, wenn mehr als eine
Zelle ausgewählt wird, die Auswahl auf ganze Zellen ausdehnen.

Was ungewöhnlich erscheinen kann ist, dass _wxMaxima_ alternativ einen
vertikalen oder einen horizontalen Cursor darstellen kann. _wxMaxima_
wechselt zwischen ihnen nach Bedarf:

- Der Cursor wird vertikal dargestellt wird, wenn er zwischen zwei Zellen zu
  liegen kommt.
- Innerhalb einer Zelle wird der Cursor vertikal dargestellt. Der Cursor
  wird mit dem Mauszeiger oder den Cursortasten in einer Textzelle aktiviert
  und funktioniert ähnlich wie in einem Text-Editor.

When you start wxMaxima, you will only see the blinking horizontal
cursor. If you start typing, a math cell will be automatically created and
the cursor will change to a regular vertical one (you will see a right arrow
as "prompt", after the Math cell is evaluated
(<kbd>CTRL</kbd>+<kbd>ENTER</kbd>), you will see the labels, e.g. `(%i1)`,
`(%o1)`).

![(Blinkender) Horizontaler Cursor nachdem wxMaxima gestartet
wurde](./horizontal-cursor-only.png){ id=img_horizontal_cursor_only }

Möglicherweise wollen Sie einen anderen Zelltyp erzeugen (im "Zellen" Menü),
vielleicht eine Titelzelle oder Textzelle, die beschreibt, was das
Arbeitsblatt machen wird, wenn Sie beginnen, ein Arbeitsblatt zu erstellen.

Wenn Sie zwischen den Zellen navigieren, sehen Sie den (blinkenden)
horizontalen Cursor. An dieser Stelle können Sie eine Zelle einfügen -
entweder eine mathematische Eingabezelle, indem einfach der Maxima-code
eingegeben wird), oder eine andere Zelle (unter Verwendung des
Zellen-Menüs).

![(Blinkender) Horizontaler Cursor zwischen den
Zellen](./horizontal-cursor-between-cells.png){
id=img_horizontal_cursor_between_cells }

### Eingabezellen an_Maxima schicken

Die Befehle in einer Codezelle werden ausgeführt, sobald die
<kbd>CTRL</kbd>+<kbd>ENTER</kbd>, <kbd>SHIFT</kbd>+<kbd>ENTER</kbd> oder
<kbd>ENTER</kbd> Taste im Nummernblock gedrückt wird. Standardmässig nimmt
_wxMaxima_ Befehle mit <kbd>CTRL+ENTER</kbd> oder <kbd>SHIFT+ENTER</kbd>
entgegen, aber _wxMaxima_ kann auch konfiguriert werden, dass Befehle nach
<kbd>ENTER</kbd> ausgeführt werden.

### Auto-Vervollständigung

_WxMaxima_ versucht, automatisch die Namen von Befehlen oder Variablen zu
vervollständigen, wenn der Menüpunkt (Zellen/Vervollständige Befehl)
angewählt wird, oder die Tastenkombination <kbd>CTRL</kbd>+<kbd>SPACE</kbd>
gedrückt wird. Die automatische Vervollständigung erkennt oft den Kontext,
in dem sie ausgeführt wird, und kann beispielsweise Dateinamen oder
Einheiten für ezUnits vorschlagen.

![ezUnits](./ezUnits.png){ id=img_ezUnits }

Außer der Vervollständigung des Namens einer Datei, einer Einheit, eines
Kommandos oder eines Variablennamens kann für viele Befehle eine Liste der
erwarteten Argumente angezeigt werden. Hierfür muss einfach
<kbd>SHIFT</kbd>+<kbd>CTRL</kbd>+<kbd>SPACE</kbd> gedrückt werden, oder der
entsprechende Menüeintrag gewählt (Zelle/Zeige Parameter).

#### Griechische Zeichen

Computer speichern Zeichen meist als 8-Bit-Werte, was maximal 256
unterschiedliche Typen von Zeichen erlaubt. Die meisten Sprachen nutzen
inklusive Steuerzeichen, Ziffern und ein paar Zeichen, aus denen Graphiken
zusammengesetzt werden können, weniger als dies.

Für die Mehrzahl der Länder ist aus diesem Grund eine Codepage mit 256
Zeichen definiert worden, die allerdings in den meisten Ländern
beispielsweise keine griechischen Buchstaben beinhaltet, die in der
Mathematik oft verwendet werden. Um diese Begrenzung zu umgehen, wurde
[Unicode](https://home.unicode.org/) entwickelt: Eine Zeichencodierung, die
englischen Text wie gewohnt aussehen lässt, aber viel mehr als 256 Zeichen
ermöglicht.

Wenn Maxima mit einem Lisp-Compiler generiert wurde, der Unicode unterstützt
oder sich nicht darum kümmert, auf welche Weise Zeichen codiert werden,
unterstützt es Unicode. Da meist eine dieser Bedingungen gegeben ist, bietet
_WxMaxima_ eine Methode an, griechische Zeichen mit der Tastatur einzugeben:

- Ein griechischer Buchstabe kann eingegeben werden, indem erst auf die
  <kbd>ESC</kbd>-Taste gedrückt wird, und dann sein Name einzugeben begonnen
  wird.
- Ebenso kann er eingegeben werden, indem <kbd>ESC</kbd> gedrückt wird, ein
  Buchstabe (bzw. zwei für den griechischen Buchstaben omicron) und dann
  erneut <kbd>ESC</kbd>. In diesem Fall werden die folgenden Zeichen
  unterstützt:

| Zeichen | Griechischer Buchstabe | Zeichen |Griechischer Buchstabe | Zeichen | Griechischer Buchstabe |
| :-----: | :--------------------: | :-----: | :-------------------: | :-----: | :--------------------: |
|  a      |    alpha               |  i      |     iota              |  r      |     rho                |
|  b      |     beta               |  k      |    kappa              |  s      |    sigma               |
|  g      |    gamma               |  l      |    lambda             |  t      |     tau                |
|  d      |    delta               |  m      |      mu               |  u      |   upsilon              |
|  e      |   epsilon              |  n      |      nu               |  f      |     phi                |
|  z      |     zeta               |  x      |      xi               |  c      |     chi                |
|  h      |     eta                | om      |   omicron             |  y      |     psi                |
|  q      |    theta               |  p      |      pi               |  o      |    omega               |
|  A      |    Alpha               |  I      |     Iota              |  R      |     Rho                |
|  B      |     Beta               |  K      |    Kappa              |  S      |    Sigma               |
|  G      |    Gamma               |  L      |    Lambda             |  T      |     Tau                |
|  D      |    Delta               |  M      |      Mu               |  U      |   Upsilon              |
|  E      |   Epsilon              |  N      |      Nu               |  P      |     Phi                |
|  Z      |     Zeta               |  X      |      Xi               |  C      |     Chi                |
|  H      |     Eta                | Om      |   Omicron             |  Y      |     Psi                |
|  T      |    Theta               |  P      |      Pi               |  O      |    Omega               |

You can also use the "Greek letters"-sidebar to enter the Greek letters.

##### Attention: Lookalike characters

Several Latin letters look like the Greek letters, e.g. the Latin letter "A"
and the Greek letter "Alpha". Although they look identical, they are two
different Unicode characters, represented by different Unicode code points
(numbers).

This might be problematic, if you assign a value to the variable A and later
use the Greek letter Alpha to do something with this variable, especially on
printouts.  For the Greek letter my (which is also used as prefix for micro)
there are also two different Unicode code points.

The "Greek letters"-sidebar therefore has the option, that lookalike
characters are not available (which can be changed using a right-click
menu).

Derselbe Mechanismus erlaubt es auch, einige andere mathematische Symbole
einzugeben:

| Zeicheneingabe | Mathematisches Symbol                                 |
| -------------- | ----------------------------------------------------- |
| hbar           | Planck constant: a h with a horizontal bar above it   |
| Hbar           | a H with a horizontal bar above it                    |
| 2              | squared                                               |
| 3              | to the power of three                                 |
| /2             | 1/2                                                   |
| partial        | partial sign (the d of dx/dt)                         |
| integral       | integral sign                                         |
| sq             | square root                                           |
| ii             | imaginary                                             |
| ee             | element                                               |
| in             | in                                                    |
| impl implies   | implies                                               |
| inf            | infinity                                              |
| empty          | empty                                                 |
| TB             | big triangle right                                    |
| tb             | small triangle right                                  |
| and            | and                                                   |
| or             | or                                                    |
| xor            | xor                                                   |
| nand           | nand                                                  |
| nor            | nor                                                   |
| equiv          | equivalent to                                         |
| not            | not                                                   |
| union          | union                                                 |
| inter          | intersection                                          |
| subseteq       | subset or equal                                       |
| subset         | subset                                                |
| notsubseteq    | not subset or equal                                   |
| notsubset      | not subset                                            |
| approx         | approximately                                         |
| propto         | proportional to                                       |
| neq != /= or # | not equal to                                          |
| +/- or pm      | a plus/minus sign                                     |
| \<= or leq     | equal or less than                                    |
| >= or geq      | equal or greater than                                 |
| \<\< or ll     | much less than                                        |
| >> or gg       | much greater than                                     |
| qed            | end of proof                                          |
| nabla          | a nabla operator                                      |
| sum            | sum sign                                              |
| prod           | product sign                                          |
| exists         | there exists sign                                     |
| nexists        | there is no sign                                      |
| parallel       | a parallel sign                                       |
| perp           | a perpendicular sign                                  |
| leadsto        | a leads to sign                                       |
| ->             | a right arrow                                         |
| -->            | a long right arrow                                    |

You can also use the "Symbols"-sidebar to enter these Mathematical symbols.

If a special symbol isn’t in the list, it is possible to input arbitrary
Unicode characters by pressing <kbd>ESC</kbd> \[number of the character
(hexadecimal)\] <kbd>ESC</kbd>. Additionally the "symbols" sidebar has a
right-click menu that allow to display a list of all available Unicode
symbols one can add to this toolbar or to the worksheet.

<kbd>ESC</kbd><kbd>6</kbd><kbd>1</kbd><kbd>ESC</kbd> erzeugt daher ein `a`.

Viele der Unicode-Symbole (mit der Ausnahme der logischen Verknüpfungen)
haben keine direkte Entsprechung in _Maxima_ und werden daher als normale
Zeichen interpretiert. Falls _Maxima_ eine Lisp-Version verwendet, die
Unicode nicht unterstützt, kann das eine Fehlermeldung verursachen.

Es kann passieren, dass z.B. griechische Buchstaben oder mathematische
Symbole in der ausgewählten Schriftart nicht vorhanden sind, dann können sie
nicht dargestellt werden. Um das Problem zu lösen, bitte eine andere
Schriftart wählen (Bearbeiten -> Einstellungen -> Stil).

### Unicode Ersetzung

wxMaxima will replace several Unicode characters with their respective
Maxima expressions, e.g. `²` with `^2`, `³` with `^3`, the square root sign
with the function `sqrt()`, the (mathematical) Sigma sign (which is not the
same Unicode character as the corresponding Greek letter) with `sum()`, etc.

Unicode has several "common" fractions encoded as one Unicode code point:
`¼, ½, ¾, ⅐, ⅑, ⅒, ⅓, ⅔, ⅕, ⅖, ⅗, ⅘, ⅙, ⅚, ⅛, ⅜, ⅝, ⅞`

wxMaxima will replace them with their Maxima representations, e.g `(1/4)`
before the input is sent do Maxima. There are also `⅟`, which will be
replaced by `1/` and `↉` (used in baseball), which will be replaced by
`(0/3)`.

It is recommended to use **Maxima code** (not these Unicode code points) in
input cells (Rationale: (a) it might be possible, that the used font for
math input does not contain them; (b) if you save the document as
`wxm`-file, it is usually readable by (command line) Maxima, but these
changes will of course not work in command line Maxima); but they may occur,
if you cut&paste a formula from another document.


### Seitenleisten

Die meisten _Maxima_-Befehle, ein Inhaltsverzeichnis, Debug-Meldungen oder
die Liste der zuletzt verwendeten Befehle werden in Seitenleisten angeboten,
die über das "Ansicht"-Menü aktiviert und an beliebige Stellen in oder
außerhalb des _wxMaxima_-Fensters verschoben werden können. Ebenso kann eine
Seitenleiste mit griechischen Buchstaben geöffnet werden, die eine
alternative Methode anbietet, griechische Buchstaben einzugeben.

![Beispiele verschiedener Seitenbereiche](./SidePanes.png){ id=img_SidePanes
}

In the "table of contents" side pane, one can increase or decrease a heading
by just clicking on the heading with the right mouse button and select the
next higher or lower heading type.

![Increase or decrease headings in the TOC side
pane](./Sidepane-TOC-convert-headings.png){ id=Sidepane-TOC-convert-headings
}

### MathML-Ausgabe

Einige Textverarbeitungen verstehen entweder
[MathML](https://www.w3.org/Math/) oder (wie LibreOffice) haben einen
Formeleditor, der die Option “importiere MathML aus der Zwischenablage”
unterstützt. Andere unterstützen Mathematik im RTF-Format. _wxMaxima_ bietet
daher einige entsprechende Optionen im Rechtsklick-Menü an. 

### Markdown Unterstützung

_WxMaxima_ unterstützt einige
[Markdown](https://en.wikipedia.org/wiki/Markdown)-Kommandos, die nicht mit
Konstrukten kollidieren, die in mathematischen Formeln vorkommen. Eines
dieser Elemente sind Aufzählungen:

```
Normaler Text
 * Ein eingerückter Aufzählungspunkt
 * Ein zweiter Aufzählungspunkt
   * Eine Aufzählung in der Aufzählung
   * Ein zweiter Aufzählungspunkt der Aufzählung in der Aufzählung
 * Ein Punkt in der äußeren Aufzählung
Normaler text
```

_WxMaxima_ erkennt Text, der mit einem `>` beginnt, als ein Zitat:

``` Ordinary text > quote quote quote quote > quote quote quote quote >
quote quote quote quote Ordinary text ```

Die TeX- und HTML-Ausgabe von _WxMaxima_ erkennt auch `=>` und ersetzt es
durch das entsprechende Unicode-Symbol:

``` cogito => sum. ```

Andere Symbole, die vom HTML- und TeX-Export erkannt werden, sind `<=` und
`>=` (Vergleichsoperatoren), ein Doppelpfeil (`<=>`), einfache Pfeile
(`<->`, `->` und `<-`) und `+/-`. Die TeX-Ausgabe erkennt zusätzlich `<<`
und `>>`.

### Tastenkürzel

Die meisten Tastenkürzel entstammen den Menüs, was bedeutet, dass sie mit
diesen übersetzt werden können, falls die Tastatur der aktuellen Sprache
dies erforderlich macht. Nicht dort dokumentiert ist:

- <kbd>CTRL</kbd>+<kbd>SHIFT</kbd>+<kbd>DELETE</kbd> löscht eine komplette
  Zelle.
- <kbd>CTRL</kbd>+<kbd>TAB</kbd> oder
  <kbd>CTRL</kbd>+<kbd>SHIFT</kbd>+<kbd>TAB</kbd> aktiviert die
  Auto-Vervollständigung.
- <kbd>SHIFT</kbd>+<kbd>SPACE</kbd> fügt ein nicht-umbrechbares Leerzeichen
  ein.

### Direkte Eingabe von TeX-Befehlen im TeX-Export

Wenn eine Textzelle mit "TeX:" beginnt, wird der Rest ihres Inhalts bei der
Konvertierung des Dokuments nach TeX unverändert ausgegeben.

## Dateiformate

Das Arbeitsblatt kann auf verschiedene Weisen gespeichert werden:

### .mac

.mac-Dateien sind Textdateien mit _Maxima_-Befehlen. Sie können über
Maxima's `batch()` oder `load()`-Befehl oder in wxMaxima über "Datei/Batch
File laden" gelesen werden.

One example is shown below. `Quadratic.mac` defines a function and afterward
generates a plot with `wxdraw2d()`.  Afterward the contents of the file
`Quadratic.mac` are printed and newly defined function `f()` is evaluated.

![Laden einer Datei mit `batch()`](./BatchImage.png){ id=img_BatchImage }

Achtung: Obwohl die Datei `Quadratic.mac` eine übliche Maxima-extension hat
(`.mac`), kann sie nur durch wxMaxima verarbeitet werden, der Befehl
`wxdraw2d()` ist eine wxMaxima-Erweiterung für
_Maxima_. (Kommandozeilen)-Maxima wird den unbekannten Befehl ignorieren und
ihn als Ausgabe erneut ausgeben.

`.mac`-Dateien eignen sich dafür, eine Bibliothek von Maxima-Befehlen
aufzubauen. Ausreichend Information über die Struktur eines
_wxMaxima_-Arbeitsblatts, dass sie es erlauben, dieses wieder zu
rekonstruieren, enthalten sie aber nicht.

### .wxm

`.wxm` files contain the worksheet except for _Maxima_’s output. On Maxima
versions >5.38 they can be read using _Maxima_’s `load()` function just as
.mac files. Well - mostly. Questions (like `asksign(x)`) are problematic, as
the answer is written in the `.wxm` file (so that it can be suggested after
loading), but that can Maxima not evaluate.  You can prevent Maxima from
asking questions by using `assume()` to declare some properties, Maxima
wants to know.

With this plain-text format, it sometimes is unavoidable that worksheets
that use new features are not downwards-compatible with older versions of
_wxMaxima_.

#### Dateiformat von wxm-Dateien

This is just a plain text file (you can open it with a text editor),
containing the cell contents as some special Maxima comments.

It starts with the following comment:

``` /* [wxMaxima batch file version 1] [ DO NOT EDIT BY HAND! ]*/ /* [
Created with wxMaxima version 24.02.2_DevelopmentSnapshot ] */ ```

And then the cells follow, encoded as Maxima comments, e.g. a section cell:

```
/* [wxMaxima: section start ]
Title of the section
   [wxMaxima: section end   ] */
```

or (in a Math cell the input is of course *not* commented out (the output is
not saved in a `wxm` file)):

```
/* [wxMaxima: input   start ] */
f(x):=x^2+1$
f(2);
/* [wxMaxima: input   end   ] */
```

Images are [Base64 encoded](https://en.wikipedia.org/wiki/Base64) with the
image type as first line):

```
/* [wxMaxima: image   start ]
jpg
[very chaotic looking character sequence]
   [wxMaxima: image   end   ] */
```

A page break is just one line containing:

```
/* [wxMaxima: page break    ] */
```

And folded cells marked by:

```
/* [wxMaxima: fold    start ] */
...
/* [wxMaxima: fold    end   ] */
```

### .wxmx

Dieses XML basiertes Dateiformat enthält das gesamte Arbeitsblatt, inklusive
Eigenschaften wie den Zoom-Faktor oder die Watchlist. Es ist das empfohlene
Dateiformat für wxMaxima-Arbeitsblätter.

#### Dateiformat von wxmx-Dateien

A `wxmx`-file seems to be a binary format, but one can handle it with tools,
which are included in your OS. It is a zip file, one can decompress it with
`unzip` (maybe rename it before, so that is recognized by the unzip program
of your OS).  We do not use the compression function, just the possibility
to merge several files into one file - images are already compressed and the
rest is simple text (probably much smaller, than huge images, which are
included).

It does contain the following files:

- `mimetype`: this file does contain the mimetype of wxMaxima files:
  `text/x-wxmathml`
- `format.txt`: a short description about wxMaxima and the wxmx file format
- Images (e.g. png, jpeg): inline plots which were produced in the wxMaxima
  session and included images.
- `content.xml`: a XML document, which contains the various cells of your
  document in XML format.

So, if something goes wrong, you can unzip a wxMaxima document (maybe rename
it before to a `zip`-file), maybe make changes in the `content.xml` file
with a text editor, or replace an broken image, zip the files again,
probably rename the `zip` to a `wxmx`-file - and you get another modified
`wxmx`-file.

## Konfigurations-Optionen

Einige Variablen, über die Maxima konfiguriert wird, können auf zwei Arten
eingestellt werden:

- Die können global im Konfigurationsdialog eingestellt werden.
- Die Werte der meisten Konfigrations-Variablen können nur für die aktuelle
  Sitzung geändert werden, indem die Werte im Arbeitsblatt geändert werden,
  wie unten gezeigt.

![wxMaxima Konfiguration 1](./wxMaxima_configuration_001.png){
id=img_wxMaxima_configuration_001 }

### Die Standard-Bildwiederholrate bei Animationen

Die Bildwiederholgeschwindigkeit für Animationen wird in der Variable
`wxanimate_framerate` gespeichert. wxMaxima setzt sie auf den Wert aus dem
Konfigurationsdialog, wenn ein neues _Maxima_ gestartet wird.

### Standardgröße der Diagramme bei neuen _Maxima_ Sitzungen

Beim nächsten Start werden im Arbeitsblatt eingebettete Diagramme mit dieser
Größe erstellt, wenn der Wert von `wxplot_size` nicht geändert wird.

Diese Variable kann notfalls auch nur für das aktuelle Diagramm gesetzt
werden:

```maxima
wxdraw2d(
   explicit(
       x^2,
	   x,-5,5
   )
), wxplot_size=[480,480]$
```

### Automatisches Schließen von Klammern

Diese Option schaltet zwei Funktionen ein:

- Wenn eine öffnende Klammer, eckige Klammer oder ein Anführungszeichen
  eingegeben wird, fügt _wxMaxima_ automatisch das schließende Zeichen
  danach ein.
- Markierter Text wird, wenn man eine geöffnete Klammer einzugeben versucht,
  automatisch in Klammern gesetzt.

### Arbeitsblatt nicht automatisch speichern

Wenn diese Option gewählt ist, wird das Arbeitsblatt nur überschrieben, wenn
der Benutzer es speichert. Ein aktuelles Backup wird in diesem Fall im
temp-Verzeichnis abgelegt.

Ist diese Option nicht angewählt, arbeitet _wxMaxima_ wie eine moderne
Handy-App:

- Dateien werden beim Schließen automatisch gespeichert
- Und die Datei wird automatisch alle 3 Minuten gespeichert.

### Wo wird die Konfiguration gespeichert?

Auf Linux/Unix-Rechnern wird die Konfiguration im Home-Verzeichnis in der
Datei `.wxMaxima` gespeichert, oder (für wxWidgets >3.1.1) in
`.config/wxMaxima.conf` (XDG-Standard). Die wxWidgets-Version kann über
`wxbuild_info()` oder über das Hilfe->Über-Menü abgerufen
werden. [wxWidgets](https://www.wxwidgets.org/) ist eine Bibliothek für
graphische Benutzeroberflächen, die auf verschiedensten Betriebssystemen
läuft und für das `wx` im Namen von wxMaxima verantwortlich ist.
(Da der Dateiname mit einem Punkt beginnt, ist `.wxMaxima` oder `.config`
eine versteckte Datei).

Unter Windows wird die Konfiguration in der Registry unter
`HKEY_CURRENT_USER\Software\wxMaxima` gespeichert.

______________________________________________________________________

# Erweiterungen für _Maxima_

_wxMaxima_'s Hauptaufgabe ist es, die Ein- und Ausgaben von Maxima graphisch
darzustellen. An einigen Stellen fügt es jedoch Funktionalitäten zu _Maxima_
hinzu. WxMaxima's Fähigkeiten, Reports zu generieren, indem Arbeitsblätter
nach HTML und LaTeX exportiert werden können, wurde erwähnt. Dieser
Abschnitt betrachtet einige Arten, wie _wxMaxima_ die Einbindung von
Graphiken in ein Arbeitsblatt verbessert

## Variablen mit tiefgestelltem Index

`wxsubscripts` gibt an, wie (und ob) _wxMaxima_ Variablennamen automatisch
tiefstellen wird:

Falls es `false` gesetzt wird, wird die Funktion deaktiviert, wxMaxima wird
nicht Teile von Variablennamen nach einem Unterstrich tiefstellen.

Falls es auf `'all` gesetzt wird, wird alles nach einem Unterstrich
tiefgestellt.

Wenn es auf `true` gesetzt ist, werden Variablennamen im Format `x_y` mit
einem tiefgestellten `y` dargestellt, wenn

- Entweder `x` oder `y` ist ein einzelner Buchstabe ist oder
- `y` ist eine ganze Zahl (kann länger als ein Zeichen lang sein).

![Wie Variablen mit wxsubscripts automatisch tiefgestellt
werden](./wxsubscripts.png){ id=img_wxsubscripts }

Wenn nicht, kann jede Variable als mit tiefgestelltem `y` ausgestattet
deklariert werden mittels `wxdeclare_subscript(variablenname);` oder
`wxdeclare_subscript([variablenname1,variablenname2,...]);`
Rückgängig gemacht werden kann dies mittels
`wxdeclare_subscript(variablenname,false);`

Sie können das Menü "Anzeige->Automatisches Tiefstellen" verwenden, um diese
Werte zu setzen.

## Meldungen in der Statusleiste

Kommandos, die lange arbeiten, können Fortschrittsinformationen in die
Statuszeile ausgeben. Diese Informationen überschreiben deren
Inhalt. `wxstatusbar()` kann sogar in Bibliotheken verwendet werden, bei
denen nicht bekannt ist, ob sie mit _wxMaxima_ oder in einem _Maxima_ ohne
Frontend verwendet werden: Wenn _wxMaxima_ diese Funktion nicht definiert
hat, wird sie von Maxima einfach als undefinierte Funktion betrachtet und
nicht weiter bearbeitet.

```maxima
    for i:1 thru 10 do (
        /* Gib dem User Bescheid, wie weit wir schon sind */
        wxstatusbar(concat("Pass ",i)),
        /* (sleep n) ist eine Lisp-Funktion, die mit einem */
        /* "?" Zeichen vorher verwendet werden kann. */
        /* Sie verzögert die Programmausführung für n Sekunden */
        /* (in diesem Beispiel: für 3 Sekunden). */
        ?sleep(3)
    )$
```

## Diagramme

Diagramme haben per definitionem mit einer graphischen Benutzerumgebung zu
tun, weswegen an dieser Stelle Erweiterungen von Maxima zu erwarten sind.

### Diagramme in das Arbeitsblatt einbetten

_Maxima_ weist normalerweise Gnuplot an, Diagramme in eigenen Fenstern
darzustellen. Wenn dem entsprechenden `draw` oder `plot`-Kommando ein "wx"
vorangestellt wird, wird das Bild stattdessen in das Arbeitsblatt eingebaut.

Die folgenden Plot-Funktionen haben wx-Äquivalente:

| wxMaxima’s plot function | Maxima’s plot function                                                                          |
| ------------------------ | ----------------------------------------------------------------------------------------------- |
| `wxplot2d()`             | [plot2d](https://maxima.sourceforge.io/docs/manual/maxima_singlepage.html#plot2d)               |
| `wxplot3d()`             | [plot3d](https://maxima.sourceforge.io/docs/manual/maxima_singlepage.html#plot3d)               |
| `wxdraw2d()`             | [draw2d](https://maxima.sourceforge.io/docs/manual/maxima_singlepage.html#draw2d)               |
| `wxdraw3d()`             | [draw2d](https://maxima.sourceforge.io/docs/manual/maxima_singlepage.html#draw3d)               |
| `wxdraw()`               | [draw](https://maxima.sourceforge.io/docs/manual/maxima_singlepage.html#draw)                   |
| `wximplicit_plot()`      | [implicit_plot](https://maxima.sourceforge.io/docs/manual/maxima_singlepage.html#implicit_plot) |
| `wxhistogram()`          | [histogram](https://maxima.sourceforge.io/docs/manual/maxima_singlepage.html#histogram)         |
| `wxscatterplot()`        | [scatterplot](https://maxima.sourceforge.io/docs/manual/maxima_singlepage.html#scatterplot)     |
| `wxbarsplot()`           | [barsplot](https://maxima.sourceforge.io/docs/manual/maxima_singlepage.html#barsplot)           |
| `wxpiechart()`           | [piechart](https://maxima.sourceforge.io/docs/manual/maxima_singlepage.html#piechart)           |
| `wxboxplot()`            | [boxplot](https://maxima.sourceforge.io/docs/manual/maxima_singlepage.html#boxplot)             |

If a `wxm`-file is read by (console) Maxima, these functions are ignored
(and printed as output, as other unknown functions in Maxima).

If you got problems with one of these functions, please check, if the
problem exists in the the Maxima function too (e.g. you got an error with
`wxplot2d()`, check the same plot in the Maxima command `plot2d()` (which
opens the plot in a separate Window)). If the problem does not disappear, it
is most likely a Maxima issue and should be reported in the [Maxima
bugtracker](https://sourceforge.net/p/maxima/bugs/). Or maybe a Gnuplot
issue.

### Eingebettete Diagramme größer oder kleiner machen

Wie bereits beschrieben kann die Größe von Diagrammen mittels der Variable
`wxplot_size` definiert werden. Die plot-Funktionen von _wxMaxima_ beachten
den Inhalt dieser Variablen, die die Größe von Plots in Pixeln angibt. Ihr
Inhalt kann jederzeit ausgelesen oder geändert werden:

```maxima
wxplot_size:[1200,800]$
wxdraw2d(
    explicit(
        sin(x),
        x,1,10
    )
)$
```

Wenn die Größe nur des aktuellen Diagramms geändert werden soll, erlaubt
_Maxima_ dies für die aktuelle Zelle zu machen. In diesem Beispiel wird
`wxplot_size = [wert1, wert2]` an den `wxdraw2d( )` Befehl angehängt, es ist
nicht Teil des `wxdraw2d` Befehls.

```maxima
    wxdraw2d(
        explicit(
            sin(x),
            x,1,10
        )
    ),wxplot_size=[1600,800]$
```

Setting the size of embedded plot with `wxplot_size` works for embedded
plots using e.g. `wxplot`, `wxdraw`, `wxcontour_plot` and `wximplicit_plot`
commands and for embedded animations with `with_slider_draw` and `wxanimate`
commands.

### Hochqualitativere Diagramme

_Gnuplot_ doesn’t seem to provide a portable way of determining whether it
supports the high-quality bitmap output that the Cairo library provides. On
systems where _Gnuplot_ is compiled to use this library the pngCairo option
from the configuration menu (that can be overridden by the variable
`wxplot_pngcairo`) enables support for antialiasing and additional line
styles. If `wxplot_pngCairo` is set without _Gnuplot_ supporting this the
result will be error messages instead of graphics.

### Öffnen eingebetteter Diagramme in interaktiven _Gnuplot_ Fenstern

Wenn ein Diagramm mittels einem `wxdraw`-ähnlichen Befehl erstellt wurde
(`wxplot2d` und `wxplot3d` werden hier nicht unterstützt) und die
Gnuplot-Datei nicht allzu lang ist, bietet _wxMaxima_ einen
Rechts-Klick-Menüpunkt an, der das Diagramm in einem interaktiven
Gnuplot-Fenster öffnet.

### Öffnen der Gnuplot Kommandozeile in `plot` Fenstern

On MS Windows, there are two Gnuplot programs, `gnuplot.exe` and
`wgnuplot.exe`.  You can configure, which command should be used using the
configuration menu. `wgnuplot.exe` offers the possibility to open a console
window, where _gnuplot_ commands can be entered into, `gnuplot.exe` does not
offer this possibility. Unfortunately, `wgnuplot.exe` causes _Gnuplot_ to
"steal" the keyboard focus for a short time every time a plot is prepared.

### Einbetten von Animationen in das Arbeitsblatt

Es ist meist schwer, aus 3D-Diagrammen quantitative Aussagen zu
entnehmen. Eine Alternative ist es, den 3. Parameter auf das Mausrad zu
legen. Das Kommando `with_slider_draw` ist eine Version von  `wxdraw2d`, die
mehrere Plots erstellt und es erlaubt, mittels eines Schiebers zwischen
diesen hin- und herzuschalten. _WxMaxima_ kann die Animation auch als
animierte `.gif`-Datei exportieren.

Die ersten beiden Argumente von `with_slider_draw` sind der Name der
Variable des zu variierenden Parameters und die Liste der Werte, die dieser
annehmen soll. Darauf folgen die ganz normalen Argumente, die `wxdraw2d`
akzeptiert:

```maxima
with_slider_draw(
    f,[1,2,3,4,5,6,7,10],
    title=concat("f=",f,"Hz"),
    explicit(
        sin(2*%pi*f*x),
        x,0,1
    ),grid=true
);
```

Für 3D-Diagramme ist dieselbe Funktionalität als `with_slider_draw3d`
verfügbar, die auch rotierende 3D-Diagramme erstellen kann:

```maxima
wxanimate_autoplay:true;
wxanimate_framerate:20;
with_slider_draw3d(
    α,makelist(i,i,1,360,3),
    title=sconcat("α=",α),
    surface_hide=true,
    contour=both,
    view=[60,α],
    explicit(
        sin(x)*sin(y),
        x,-π,π,
        y,-π,π
    )
)$
```

Wenn es nur um die generelle Form der Kurve geht, reicht es oft, das Bild
ein wenig zu bewegen, so, dass es von der Intuition erfasst werden kann:

```maxima
wxanimate_autoplay:true;
wxanimate_framerate:20;
with_slider_draw3d(
    t,makelist(i,i,0,2*π,.05*π),
    title=sconcat("α=",α),
    surface_hide=true,
    contour=both,
    view=[60,30+5*sin(t)],
    explicit(
        sin(x)*y^2,
        x,-2*π,2*π,
        y,-2*π,2*π
    )
)$
```

Wer `plot` `draw` vorzieht, dem steht ein zweiter Satz an Funktionen zur
Verfügung:

- - `with_slider` und
- `wxanimate`.

Die Standard-Bildwiederholrate für Animationen kann im Konfigurations-Dialog
von _wxMaxima_ eingestellt werden. Um die Geschwindigkeit einer einzelnen
Animation zu ändern, kann die Variable `wxanimate_framerate` geändert
werden:

```maxima
wxanimate(a, 10,
    sin(a*x), [x,-5,5]), wxanimate_framerate=6$
```

Die Animations-Funktionen haben eine Eigentümlichkeit, die daher rührt, dass
sie `makelist` verwenden: Der Parameter, der sich von Bild zu Bild ändert,
wird nur eingesetzt, wenn die Variable direkt sichtbar ist. Das folgende
Beispiel scheitert daher:

```maxima
f:sin(a*x);
with_slider_draw(
    a,makelist(i/2,i,1,10),
    title=concat("a=",float(a)),
    grid=true,
    explicit(f,x,0,10)
)$
```

Wenn _Maxima_ explizit gebeten wird, den Wert einzusetzen, funktioniert der
Befehl stattdessen:

```maxima
f:sin(a*x);
with_slider_draw(
    b,makelist(i/2,i,1,10),
    title=concat("a=",float(b)),
    grid=true,
    explicit(
        subst(a=b,f),
        x,0,10
    )
)$
```

### Mehrere Diagramme gleichzeitig in Fenstern öffnen

Strenggenommen kein Feature von _wxMaxima_. Aber Maxima erlaubt auf vielen
Systemen das folgende Beispiel von Mario Rodriguez auszuführen:

```maxima load(draw);

/* Parabola in window #1 */ draw2d(terminal=[wxt,1],explicit(x^2,x,-1,1));

/* Parabola in window #2 */ draw2d(terminal=[wxt,2],explicit(x^2,x,-1,1));

/* Paraboloid in window #3 */
draw3d(terminal=[wxt,3],explicit(x^2+y^2,x,-1,1,y,-1,1)); ```

Mehrere Diagramme in einem Fenster sind auch möglich (dies ist auch möglich
in Kommandozeilen-Maxima mit dem Standard-`draw()`-Befehl:

```maxima
wxdraw(
  gr2d(
    key="sin (x)",grid=[2,2],
    explicit(sin(x),x,0,2*%pi)),
  gr2d(
    key="cos (x)",grid=[2,2],
    explicit(cos(x),x,0,2*%pi))
);
```

### Die "Plotte mittels Draw"-Seitenleiste

Die "Plotte mittels Draw"-Seitenleiste enthält einen Codegenerator, der es
erlaubt, einen Teil der Flexibilität des _draw_-Pakets von _Maxima_ zu
nutzen.

#### 2D

Generates the skeleton of a `draw()` command that draws a 2D scene. This
scene later has to be filled with commands that generate the scene’s
contents, for example by using the buttons in the rows below the "2D"
button.

One helpful feature of the 2D button is that it allows to set up the scene
as an animation in which a variable (by default it is _t_) has a different
value in each frame: Often a moving 2D plot allows easier interpretation
than the same data in a non-moving 3D one.

#### 3D

Generates the skeleton of a `draw()` command that draws a 3D scene. If
neither a 2D nor a 3D scene is set up, all of the other buttons set up a 2D
scene that contains the command the button generates.

#### Ausdruck

Fügt den aktuellen `draw()`-Befehl den Plot einer Kurve wie `sin(x)`,
`x*sin(x)` oder `x^2+2*x-4` hinzu. Besteht noch kein `draw()`-Befehl, wird
automatisch eine 2D-Szene erzeugt. Jede Szene kann beliebig viele Plots
beinhalten.

#### Impliziter Plot

Markiert alle Punkte, an denen eine Bedingung wie `y=sin(x)`, `y*sin(x)=3`
oder `x^2+y^2=4` erfüllt ist und zeichnet diese Kurve in das aktuelle
Diagramm ein. Gibt es kein aktuelles Diagramm, wird ein 2D-Diagramm erzeugt.

#### Parametrischer Plot

Bewegt eine Variable von einem Start- zu einem Endwert und verwendet
getrennte Ausdrücke wie `t*sin(t)` und `t*cos(t)`, um die x-, die y- (und in
3D-Diagrammen auch die z-) Koordinaten zu generieren.

#### Punkte

Zeichnet eine Reihe von Punkten, die optional miteinander verbunden
werden. Die Koordinaten der Punkte können aus einer Liste von Listen, einem
2-dimensionalen Array oder einer Liste oder einem Array pro Achse entnommen
werden.

#### Diagrammtitel

Bestimmt den Titel des Diagramms.

#### Achsen

Die Einstellungen für die Achsen.

#### Höhenlinien

(Only for 3D plots): Adds contour lines similar to the ones one can find in
a map of a mountain to the plot commands that follow in the current `draw()`
command and/or to the ground plane of the diagram. Alternatively, this
wizard allows skipping drawing the curves entirely only showing the contour
plot.

#### Name des Plots

Fügt einen Eintrag zur Legende hinzu, der für die nächsten Objekte gilt. Ein
leerer Name bedeutet, dass die nun folgenden Objekte keinen eigenen Eintrag
erhalten.

#### Linienfarbe

Setzt die Linienfarbe für die nun folgenden Plots des aktuellen
draw-Befehls.

#### Füllfarbe

Setzt die Füllfarbe für die nun folgenden Objekte des aktuellen
`draw`-Kommandos.

#### Gitternetz

Ein Assistent, der die Gitterlinien einzustellen hilft.

#### Genauigkeit

Erlaubt das Wählen zwischen Geschwindigkeit und Genauigkeit bei der
Erstellung der folgenden Kurven.

### Schrift und Schriftgröße for Plots ändern

Especially when you use a high resolution display, the default font size
might be very small. For the `draw`-based commands, you can set the font /
font size using options like `font=...`, `font_size=...`, e.g.:

~~~maxima
wxdraw2d(
     font="Helvetica",
     font_size=30,
     explicit(sin(x),x,1,10));
~~~

For the `plot`-commands (e.g. `wxplot2d`, `wxplot3d`) font sizes and fonts
can be set using the `gnuplot_preamble` command, e.g.:

~~~maxima
wxplot2d(sin(x),[x,1,10],
         [gnuplot_preamble, "set tics font \"Arial, 30\"; set xlabel font \",20\"; set ylabel font \",20\";"]);
~~~

This sets the font for the numbers to Arial with size 30, the size for the
xlabel and ylabel font to 20 (with the default font).

Read the Maxima and Gnuplot documentation for further information.  Note:
Gnuplot seems to have issues with larger font sizes, see [wxMaxima issue
1966](https://github.com/wxMaxima-developers/wxmaxima/issues/1966).

## Einbetten von Bildern

Das `.wxmx`-Dateiformat erlaubt es, Bilder per Drag-And-Drop
einzubetten. Manchmal (z.B. wenn die Bildinhalte sich später während einer
Sitzung ändern könnten) ist es besser, die Bilddatei während der Auswertung
zu laden:

```maxima show_image("man.png"); ```

## Startbefehle

Der Konfigurationsdialog von _wxMaxima_ bietet an, zwei Dateien mit
_Maxima_-Befehlen zu bearbeiten:

- Eine Datei mit Befehlen, die _Maxima_ beim Hochfahren ausführt:
  `maxima-init.mac`
- one file of additional commands that are executed if _wxMaxima_ is
  starting _Maxima_: `wxmaxima-init.mac`

For example, if Gnuplot is installed in `/opt` (maybe on MacOS), you can add
`gnuplot_command:"/opt/local/bin/gnuplot"$` (or `/opt/gnuplot/bin/gnuplot`
or any other path) to these files.

These files are in the Maxima user directory (usually `%USERPROFILE%/maxima`
in Windows, `$HOME/.maxima` otherwise). The location can be found out with
the command: `maxima_userdir;`

## Spezielle Variablen, die wxMaxima definiert

- `wxsubscripts` informiert _Maxima_, welche Variablen mit Unterstrich
  (z.B. `R_150`) in Variablen mit Subscript konvertieren soll. Details
  werden unter `wxdeclare_subscript` beschrieben.
- `wxfilename`: Diese Variable sagt, welchen Dateinamen das Arbeitsblatt
  laut _wxMaxima_ besitzt.
- `wxdirname`: Diese Variable enthält den Namen des Verzeichnisses, in
  welchem die aktuelle Datei geöffnet wurde.
- `wxplot_pngcairo` sagt Maxima, ob es versuchen soll, Gnuplot zu
  instruieren, pngcairo zu nutzen, das mehr Linientypen und hochqualitative
  Bilder unterstützt.
- `wxplot_size` legt die Größe eingebetteter Diagramme fest.
- `wxchangedir`: Normalerweise setzt _wxMaxima_ _Maxima_'s
  Arbeitsverzeichnis auf dasjenige, in dem sich das Arbeitsblatt
  befindet. Dies ist praktisch, wenn Maxima auf Dateien zugreifen soll
  (z.B. via `read_matrix`). Unter Windows geht dieses Feature manchmal
  schief und kann daher auf `false` gesetzt werden.
- `wxanimate_framerate`: Die Geschwindigkeit, mit der Animationen abgespielt
  werden sollen.
- `wxanimate_autoplay`: Sollen Animationen automatisch abgespielt werden?
- `wxmaximaversion`: Ergibt die Versionsnummer von _wxMaxima_.
- `wxwidgetsversion`: Ergibt die Versionsnummer der verwendeten
  wxWidgets-Version.

## 2D-Tabellen sauber ausgeben

Die Funktion `table_form()` konvertiert 2D-Listen in eine lesbarere
Tabellenform als _Maxima_ selbst. Die Eingabe ist eine Liste von einer oder
mehreren Listen. Wie der "print" Befehl wird die Ausgabe auch gemacht, wenn
der Befehl durch ein Dollarzeichen abgeschlossen wurde. Das abschließende
`done` kann durch Verwendung eines `$` anstelle eines `;` unterdrückt
werden.

```maxima
table_form(
    [
        [1,2],
        [3,4]
    ]
)$
```

Das folgende Beispiel zeigt das Zusammensetzen der Listen für solch eine
Tabelle.

![Ein drittes Tabellen-Beispiel](./MatrixTableExample.png){
id=img_MatrixTableExample }

Da Matrizen effektiv Listen von Listen sind, können auch sie in Tabellen
verwandelt werden.

![Ein anderes table_form Beispiel](./SecondTableExample.png)

## Fehler melden

_WxMaxima_ bietet einige Funktionen für das Melden von Fehlern an:

- `wxbuild_info()` sammelt Informationen über die derzeit ausgeführte
  Version von _wxMaxima_
- `wxbug_report()` sagt, wo und wie Fehler gemeldet werden können


## Rotes Markieren von Formelteilen

_Maxima_’s `box()` command causes _wxMaxima_ to print its argument with a
red foreground, if the second argument to the command is the text
`highlight`.

## Output rendering.

Mittels `set_display()` kann man angeben, wie wxMaxima die Ausgabe anzeigt.

`set_display('xml)` is the default value. Here Maxima speaks to wxMaxima
using an (machine readable) XML-dialect (can be seen in the "Raw XML
sidebar") and outputs the resulting formulas nicely rendered, e.g. pretty
Matrices, Square root signs, fractions, etc.

<!--- Currently that does not work as it should, the line with the output
label is shifted right (issue: #2006) --> `set_display('ascii)` causes
wxMaxima to output formulas as in command line Maxima - as ASCII-Art.

`set_display('none)` bewirkt  'einzeilige' ASCII Ausgaben - dasselbe was
(Kommandozeilen-)Maxima mit `display2d:false;` macht.

# Hilfemenü

WxMaxima’s help menu provides access to the Maxima and wxMaxima manual,
tips, some example worksheets and in command line Maxima included demos (the
`demo()` command).

Please notice, that the demos write:

~~~ At the ’_’ prompt, type ’;’ and <enter> to proceed with the
demonstration.  ~~~

That is valid for command-line Maxima, however in wxMaxima by default it is
necessary to continue the demonstration with:
<kbd>CTRL</kbd>+<kbd>ENTER</kbd>

(That can be configured in the Configure->Worksheet->"Hotkeys for sending
commands to Maxima" menu.)

______________________________________________________________________

# Fehlersuche

## Keine Verbindung zu _Maxima_ möglich

Since _Maxima_ (the program that does the actual mathematics) and _wxMaxima_
(providing the easy-to-use user interface) are separate programs that
communicate by the means of a local network connection. Therefore the most
probable cause is that this connection is somehow not working. For example,
a firewall could be set up in a way that it doesn’t just prevent
unauthorized connections from the internet (and perhaps intercept some
connections to the internet, too), but also blocks
inter-process-communication inside the same computer. Note that since
_Maxima_ is being run by a Lisp processor the process communication that is
blocked does not necessarily have to be named "maxima". Common names of the
program that opens the network connection would be sbcl, gcl, ccl, lisp.exe,
or similar names.

Auf Unix/Linux-Rechnern kann ein weiterer Grund sein, dass das
"loopback"-Netzwerkgerät (erlaubt Netzwerkverbindungen zwischen 2 Prorammen
am selben Rechner) nicht korrekt eingestellt ist.

## Wie repariere ich kaputte .wxmx-Dateien?

Die meisten modernen XML-basierten Formate sind von ihrem Inhalt her
einfache .zip-Dateien. _wxMaxima_ schaltet bei ihnen die Kompression nicht
ein, weswegen ihr Inhalt in einem normalen Texteditor lesbar ist.

If the zip signature at the end of the file is still intact after renaming a
broken `.wxmx` file to `.zip` most operating systems will provide a way to
extract any portion of the information that is stored inside it. This can be
done when there is a need of recovering the original image files from a text
processor document. If the zip signature isn’t intact that does not need to
be the end of the world: If _wxMaxima_ during saving detected that something
went wrong there will be a `.wxmx~` file whose contents might help.

And even if there isn’t such a file: The `.wxmx` file is a container format
and the XML portion is stored uncompressed. It it is possible to rename the
`.wxmx` file to a `.txt` file and to use a text editor to recover the XML
portion of the file’s contents (it starts with `<?xml version="1.0"
encoding="UTF-8"?>` and ends with `</wxMaximaDocument>`. Before and after
that text you will see some unreadable binary contents in the text editor).

Wird eine Textdatei mit diesem Text (z.B. indem er mit Copy+Paste in eine
neue Datei eingefügt wird) als `.xml`-Datei gespeichert, weiß _wxMaxima_,
wie man den Text-Teil des Dokuments rekonstruiert.

## Ich will Statusmeldungen am Bildschirm ausgeben, während mein Befehl
ausgeführt wird

Normalerweise gibt _wxMaxima_ erst etwas aus, wenn die komplette Ausgabe
steht. Das `disp`-Kommando wird hingegen sofort ausgeführt:

```maxima
for i:1 thru 10 do (
   disp(i),
   /* (sleep n) ist eine Lisp-Funktion, die mit einem */
   /* "?" Zeichen vorher verwendet werden kann. */
   /* Sie verzögert die Programmausführung für n Sekunden */
   /* (in diesem Beispiel: für 3 Sekunden). */
   ?sleep(3)
)$
```

Alternativ kann man sich das `wxstatusbar()`-Kommando oben ansehen.

## Statt eines Diagramms wird ein Briefumschlag mit einer Fehlermeldung
dargestellt

_wxMaxima_ konnte die Datei, die _Maxima_ _Gnuplot_ instruiert hat, zu
generieren, nicht lesen.

Mögliche Gründe für diesen Fehler sind:

- `implicit_plot` wurde verwendet, ohne vorher via `load()` geladen zu
  werden.
- _Maxima_ tried to do something the currently installed version of
  _Gnuplot_ isn’t able to understand. In this case, a file ending in
  `.gnuplot` located in the directory, which _Maxima_’s variable
  `maxima_userdir` is pointing, contains the instructions from _Maxima_ to
  _Gnuplot_. Most of the time, this file’s contents therefore are helpful
  when debugging the problem.
- Gnuplot was instructed to use the pngCairo library that provides
  antialiasing and additional line styles, but it was not compiled to
  support this possibility. Solution: Uncheck the "Use the Cairo terminal
  for the plot" checkbox in the configuration dialog and don’t set
  `wxplot_pngcairo` to true from _Maxima_.
- Gnuplot hat keine gültige `.png`-Datei ausgegeben.

## Animationen enden in "Error: Undefined Variable"

The value of the slider variable by default is only substituted into the
expression that is to be plotted if it is visible there. Using a `subst`
command that substitutes the slider variable into the equation to plot
resolves this problem. At the end of section [Embedding animations into the
spreadsheet](#embedding-animations-into-the-spreadsheet), you can see an
example.

## Rückgängig-machen erinnert sich nicht an das, was ich brauche

Es gibt zwei Rückgängigmach- Funktionen, die beide die wichtige Information
enthalten können:

- _WxMaxima_ actually has two undo features: The global undo buffer that is
  active if no cell is selected and a per-cell undo buffer that is active if
  the cursor is inside a cell. It is worth trying to use both undo options
  in order to see if an old value can still be accessed.
- Wenn die Ausgabe einer Marke (z.B. `%o20`) zugewiesen wurde, kann diese
  Marke eingegeben werden und der betreffende Text erscheint.
- Wenn nicht: Keine Panik: Im “Ansicht”-Menü kann eine Seitenleiste
  eingeschaltet werden, die die letzten Befehle anzeigt.
- Wenn nichts anderes funktioniert, bietet Maxima die Möglichkeit an, alle
  bisherigen Befehle nochmals auszuführen:

```maxima playback(); ```

## _WxMaxima_ meldet gleich beim Hochfahren "Maxima hat sich beendet."

Ein möglicher Grund ist, dass Maxima nicht dort gefunden worden kann, wo
dies in wxMaxima's Konfigurationsdialog angegeben ist. Korrektur des dort
angegebenen Pfades zum Programm löst dieses Problem.

## Maxima hört nicht auf zu rechnen und reagiert nicht auf Eingaben

Theoretisch kann es passieren, daß wxMaxima nicht erkennt, dass Maxima mit
der Berechnung fertig ist und daher nie neue Befehle an Maxima sendet. In
diesem Fall kann der Befehl 'Trigger Evaluation' die Synchronisation wieder
herstellen.

## _Maxima_ (mit SBCL compilliert) beschwert sich über einen Mangel an
Speicher

The Lisp compiler SBCL by default comes with a memory limit that allows it
to run even on low-end computers. When compiling a big software package like
Lapack or dealing with extremely big lists of equations this limit might be
too low. In order to extend the limits, SBCL can be provided with the
command line parameter `--dynamic-space-size` that tells SBCL how many
megabytes it should reserve. A 32bit Windows-SBCL can reserve up to 999
Megabytes. A 64-bit SBCL version running on Windows can be instructed to use
more than the about 1280 Megabytes compiling Lapack needs.

Kommandozeilenparameter für _Maxima_ (und daher für SBCL) können in
_wxMaxima_'s Konfigurationsdialog eingegeben werden.

![SBCL Speicherkonfiguration](./sbclMemory.png){ id=img_sbclMemory }

## Ubuntu: Die Tastatur ist langsam oder ignoriert einzelne Tasten

Das Installieren von `ibus-gtk` behebt dieses Problem meist. Auf
([https://bugs.launchpad.net/ubuntu/+source/wxwidgets3.0/+bug/1421558](https://bugs.launchpad.net/ubuntu/+source/wxwidgets3.0/+bug/1421558))
findet man genauere Angaben dazu.

## _WxMaxima_ stoppt, wenn _Maxima_ griechische Buchstaben oder Umlaute
verarbeitet

Wenn _Maxima_ mittels SBCL compiliert wurde, können die folgenden Befehle
zur `.sbclrc` hinzugefügt werden:

```commonlisp (setf sb-impl::*default-external-format* :utf-8)  ```

Wo diese Datei abgelegt werden muss, ist systemabhängig. Aber ein mit SBCL
compiliertes Maxima kann durch den folgenden Befehl angewiesen werden, den
Ort zu nennen:

``` :lisp (sb-impl::userinit-pathname)  ```

## Anmerkung bezüglich Wayland (aktuelle Linux/BSD Distributionen)

There seem to be issues with the Wayland Display Server and wxWidgets.
WxMaxima may be affected, e.g. that sidebars are not moveable.

You can either disable Wayland and use X11 instead (globally)  or just tell,
that wxMaxima should use the X Window System by setting: `GDK_BACKEND=x11`

z.B. Wxmaxima wie folgt starten:

`GDK_BACKEND=x11 wxmaxima`

## Why is the integrated manual browser not offered on my Windows PC?

Either wxWidgets wasn’t compiled with support for Microsoft’s webview2 or
Microsoft’s webview2 isn’t installed.

## Warum funktioniert der externe Hilfebrowser nicht unter Linux?

The HTML browser might be a snap, flatpack or appimage version. All of these
typically cannot access files that are installed on your local
system. Another reason might be that maxima or wxMaxima is installed as a
snap, flatpack or something else that doesn’t give the host system access to
its contents. A third reason might be that the maxima HTML manual isn’t
installed and the online one cannot be accessed.

### Kann _wxMaxima_ das eingebettete Diagramm gleich noch als Datei
ausgeben?

Das Arbeitsblatt enthält png-Dateien. _WxMaxima_ erlaubt dem User anzugeben,
wo sie generiert werden sollen:

```maxima
wxdraw2d(
    file_name="test",  /* extension .png automatically added */
    explicit(sin(x),x,1,10)
);
```

Wenn ein unterschiedliches Format benutzt wird, ist es einfacher, Bilder zu
generieren und dann in das Arbeitsblatt zu importieren:

```maxima
load("draw");
pngdraw(name,[contents]):=
(
    draw(
        append(
            [
                terminal=pngcairo,
                dimensions=wxplot_size,
                file_name=name
            ],
            contents
        )
    ),
    show_image(printf(false,"~a.png",name))
);
pngdraw2d(name,[contents]):=
    pngdraw(name,gr2d(contents));

pngdraw2d("Test",
        explicit(sin(x),x,1,10)
);
```

Kann ich das Seitenverhältnis eines eingebetteten Plots angeben?

Verwenden Sie die Variable `wxplot_size`:

```maxima
     wxdraw2d(
         proportional_axis=xy,
         explicit(sin(x),x,1,10)
     ),wxplot_size=[1000,1000];
```

### Nach dem Upgrade auf MacOS 13.1 geben Plot- oder Draw Befehle
Fehlermeldungen aus, wie

``` 1 HIToolbox 0x00007ff80cd91726
_ZN15MenuBarInstance22EnsureAutoShowObserverEv + 102 2 HIToolbox
0x00007ff80cd912b8 _ZN15MenuBarInstance14EnableAutoShowEv + 52 3 HIToolbox
0x00007ff80cd35908 SetMenuBarObscured + 408 ...  ```

This might be an issue with the operating system. Disable the hiding of the
menu bar (SystemSettings => Desktop & Dock => Menu Bar) might solve the
issue. See [wxMaxima issue
#1746](https://github.com/wxMaxima-developers/wxmaxima/issues/1746) for more
information.

## Fehlerprotokollierung

Log messages might be helpful to debug problems. WxMaxima can log many
events. Most log entries will be helpful for developers, especially in case
of problems or bugs. If you run a "Release"-Build, the log windows is not
shown by default, if you run a development version, it is shown by default
as a second window. You can enable and disable this window using the
"View->Toggle log window" menu entry.

Messages are not 'lost', if the log window is not shown, if you select to
show the log window later, you will see past log messages (if you did not
clear the messages).

Such messages may be helpful, when you create bug reports (or trying to find
a bug by yourself).

Log messages can (additionally) be printed to STDERR, when using the command
line option "--logtostderr". On Windows a separate text console will be
opened, as a Windows GUI application does not have the standard IO
connected.

______________________________________________________________________

# FAQ

## Gibt es eine Möglichkeit mehr Text auf eine LaTeX-Seite zu schreiben?

Ja. Verwenden Sie das [LaTeX Paket
"geometry"](https://ctan.org/pkg/geometry) um die Größe der Ränder
anzugeben.

Ja, gibt es. Geben Sie einfach die folgenden Zeilen im LaTeX-Vorspann
(z.B. indem sie das entsprechende Feld im Konfigurationsdialog
("Exportieren"->"Zusätzliche Zeilen für die LaTeX preamble") angeben, um
Ränder von 1cm zu setzen):

```latex \usepackage[left=1cm,right=1cm,top=1cm,bottom=1cm]{geometry} ```

## Gibt es einen Dark Mode?

If wxWidgets is new enough, _wxMaxima_ will automatically be in dark mode if
the rest of the operating system is. The worksheet itself is by default
equipped with a bright background. But it can be configured
otherwise. Alternatively, there is a `View/Invert worksheet brightness` menu
entry that allows to quickly convert the worksheet from dark to bright and
vice versa.

## _WxMaxima_ hängt manchmal in der ersten Minute einmal für mehrere
Sekunden

WxMaxima lagert große Aufgaben, wie das Interpretieren des
>1000-Seiten-Handbuchs von Maxima an Hintergrundtasks aus. Solange die
Ergebnisse dieser Tasks nicht benötigt werden, kann während dieser Zeit ganz
normal weitergearbeitet werden. Wird aber eine Aktion ausgeführt, für die
die Ergebnisse eines Tasks benötigt werden, muss _wxMaxima_ warten, bis
dieser beendet ist.

## Speziell wenn man neue Spracheinstellungen testet, kann eine
Nachrichtenbox "locale 'xx_YY' can not be set" angezeigt werden.

![Locale Warnung](./locale-warning.png){ id=img_locale_warning}

(Dieses Problem kann bei anderen Anwendungen auch auftreten). Die
Übersetzungen scheinen okay zu sein, nachdem auf 'OK' geklickt
wurde. WxMaxima verwendet nicht nur die eigenen Übersetzungen sondern auch
Übersetzungen des wxWidgets Frameworks.

Diese Übersetzungen sind möglicherweise im System nicht vorhanden. Auf
Ubuntu/Debian Linux-Systemen können sie mit `dpkg-reconfigure locales`
erzeugt werden.

## Wie kann man Symbole für reele Zahlen, ganze Zahlen (ℝ, ℕ),
etc. verwenden?

You can find these symbols in the Unicode sidebar (search for ’double-struck
capital’). But the selected font must also support these symbols. If they do
not display properly, select another font.

## Wie kann ein Maxima-Skript feststellen, ob es unter wxMaxima oder
Kommandozeilen-Maxima läuft?

Wenn wxMaxima verwendet wird, hat die Maxima-Variable `maxima_frontend` den
Wert wxmaxima`. Die Maxima-Variable `maxima_frontend_version` enthält dann
die wxMaxima-Version.

Wenn kein Frontend verwendet wird (Kommandozeilen-Maxima wird verwendet),
dann haben diese Variablen den Wert `false`).

______________________________________________________________________

# Kommandozeilen-Optionen

Üblicherweise werden Programme mit einer graphischen Oberfläche einfach mit
einem Mausklick auf ein Desktop-Icon oder einen Desktop-Menüeintrag
gestartet. WxMaxima - falls es von der Kommandozeile aus gestartet wird -
bietet doch einige Kommandozeilen-Optionen.

- `-v` oder `--version`: Gibt die Versionsnummer aus
- `-h` oder `--help`: Gibt einen kurzen Hilfetext aus
- `-o` oder `--open=<str>`: Öffnet den Dateinamen, der als Argument dieser
  Kommandozeilenoption angegeben wurde.
- `-e` oder `--eval`: Evaluiere die Datei nach dem Öffnen.
- `-b` or `--batch`: If the command-line opens a file all cells in this file
  are evaluated and the file is saved afterward. This is for example useful
  if the session described in the file makes _Maxima_ generate output
  files. Batch-processing will be stopped if _wxMaxima_ detects that
  _Maxima_ has output an error and will pause if _Maxima_ has a question:
  Mathematics is somewhat interactive by nature so a completely
  interaction-free batch processing cannot always be guaranteed.
- `--logtostderr`:                 Log all "debug messages" sidebar messages to stderr, too.
- `--pipe`:                        Pipe messages from Maxima to stdout.
- `--exit-on-error`:               Close the program on any maxima error.
- `-f` or `--ini=<str>`: Use the init file that was given as an argument to this command-line switch
- `-u`, `--use-version=<str>`:     Use maxima version `<str>`.
- `-l`, `--lisp=<str>`:              Use a Maxima compiled with Lisp compiler `<str>`.
- `-X`, `--extra-args=<str>`:        Allows to specify extra Maxima arguments
- `-m` or `--maxima=<str>`:    allows specifying the location of the _maxima_ binary
- `--enableipc`: Lets Maxima control wxMaxima via interprocess communications. Use this option with care.
- `--wxmathml-lisp=<str>`:   Location of wxMathML.lisp (if not the built-in should be used, mainly for developers).

Manche Betriebssysteme verwenden bei Kommandozeilenparametern einen
Schrägstrich statt eines Minuszeichens.

______________________________________________________________________

# Über das Programm, zu wxMaxima beitragen

WxMaxima wird hauptsächlich in der Programmiersprache C++ entwickelt,
verwendet das [wxWidgets framework](https://www.wxwidgets.org), als
Build-System verwenden wir CMake, ein kleiner Teil ist in Lisp
geschrieben. Sie können zu wxMaxima beitragen, werden Sie Teil des
wxMaxima-Projekts auf <https://github.com/wxMaxima-developers/wxmaxima>,
wenn Sie Kenntnisse dieser Programmiersprachen haben und zum
Open-Source-Projekt wxMaxima etwas beitragen können.

Aber nicht nur Programmierer sind notwendig! Sie können auch zu wxMaxima
beitragen, indem Sie die Dokumentation verbessern, Fehler finden und melden
(und vielleicht auch Fehlerlösungen) oder wxMaxima oder die Anleitung
übersetzen helfen (Siehe die Readme-Datei im [locale
subdirectory](https://github.com/wxMaxima-developers/wxmaxima/tree/main/locales),
wie das übersetzen funktioniert.

Oder Fragen von anderen Benutzern im Diskussionsforum beantworten.

Der Quellcode von wxMaxima ist mit Doxygen
[hier](https://wxmaxima-developers.github.io/wxmaxima/Doxygen-documentation/)
dokumentiert.

Das Programm ist nahezu unabhängig, ausser einiger Systembibliotheken (und
der wxWidgets-Bibliothek) sind keine externen Abhängigkeiten (z.B. externe
Graphiken oder der Lisp-Teil (die `wxMathML.lisp`-Datei) notwendig, diese
Dateien sind alle in wxMaxima eingebaut.

If you are a developer, you might want to try out a modified
`wxmathML.lisp`-file without recompiling everything, one can use the command
line option `--wxmathml-lisp=<str>` to use another Lisp file, not the
included one.
