The wxMaxima user manual {-}
============================

wxMaxima is a graphical user interface (GUI) for the Maxima computer algebra
system (CAS). wxMaxima allows one to use all of Maximaâs functions. In
addition, it provides convenient wizards for accessing the most commonly
used features. This manual describes some of the features that make wxMaxima
one of the most popular GUIs for Maxima.

[![wxMaxima logo](./wxMaximaLogo.png)](wxMaximaLogo)

Vor dem eigentlichen Inhalt ist es vielleicht angebracht, die Navigation zu
erklären: Die nun folgende Liste enthält Hyperlinks auf die einzelnen
Kapitel der Dokumentation. [Contents](#SEC_Contents \"Detailliertes
Inhaltsverzeichnis\") fürt zu einer entsprechenden Sektion, die eine
genauere Navigation ermöglicht. Zudem steht ein (#Index "Alphabetischer
Index") zur Verfügung, der auf viele Punkte des Handbuchs verweist.

* [Introduction](#Introduction "Introduction to wxMaxima"):



Die Grundlagen

* [Extensions](#Extensions "Extensions"):



Die Kommandos, mittels dessen wxMaxima Maxima erweitert

* [Troubleshooting](#Fehlersuche "Fehlersuche"):



Was tun, wenn wxMaxima nicht wie erwartet funktioniert

* • [FAQ](#FAQ "Häufig gestellte Fragen"):



Häufig gestellte Fragen

* [CommandLine](#CommandLine "Maxima Command Line"):



Die Kommandozeilenargumente von wxMaxima

* * *


Einführung in wxMaxima
======================

## Maxima und wxMaxima

In the open-source domain big systems are normally split into smaller
projects that are easier to handle for small groups of developers. For
example a CD burner program will consist of a command-line tool that
actually burns the CD and a graphical user interface that allows users to
implement it without having to learn about all the command line switches and
in fact without using the command line at all. One advantage of this
approach is that the developing work that was invested into the command-line
program can be shared by many programs: The same CD-burner command-line
program can be used as a âsend-to-CDâ-plug-in for a file manager
application, for the âburn to CDâ function of a music player and as the
CD writer for a DVD backup tool. Another advantage is that splitting one big
task into smaller parts allows the developers to provide several user
interfaces for the same program.

A computer algebra system (CAS) like Maxima fits into this framework. A CAS
can provide the logic behind a arbitrary precision calculator application or
it can do automatic transforms of formulas in the background of a bigger
system (e.g., [Sage](http://www.sagemath.org/)). Alternatively, it can be
used directly as a free-standing system. Maxima can be accessed via a
command line. Often, however, an interface like wxMaxima proves a more
efficient way to access the software, especially for newcomers.

### Maxima

Maxima ist ein komplettes Computer-Algebrasystem (CAS): Ein Programm, das
die Formel, nicht nur die Zahl sucht, die ein mathematisches Problem
löst. Auch wenn es darauf  spezialisiert ist, mit Buchstaben zu rechnen,
bietet es auch eine Menge an Funktionen, die Probleme lösen, für die nur
numerische Lösungen existieren.

![Maxima screenshot](./maxima_screenshot.png)

Extensive documentation for Maxima is [available in the
internet](http://maxima.sourceforge.net/documentation.html). Part of this
documentation is also available in wxMaximaâs help menu. Pressing the Help
key (on most systems that would be the F1 key) causes wxMaximaâs
context-sensitive help feature will automatically jump to Maximaâs manual
page for the command at the cursor.

### wxMaxima

wxMaxima is a graphical user interface that provides the full functionality
and flexibility of Maxima. wxMaxima offers users a graphical display and
many features that make working with Maxima easier. For example wxMaxima
allows one to export any cellâs contents (or, if that is needed, any part
of a formula, as well) as text, as LaTeX or as MathML specification at a
simple right-click. Indeed, an entire workbook can be exported, either as an
HTML file or as a LaTeX file. Documentation for wxMaxima, including
workbooks to illustrate aspects of its use, is online at the wxMaxima [help
site](https://wxMaxima-developers.github.io/wxmaxima/help.html), as well as
via the help menu.

![wxMaxima window](./wxMaximaWindow.png)

wxMaxima lässt alle Berechnungen im Hintergrund durch das
Kommandozeilen-Werkzeug Maxima durchführen.


## Grundlagen zum Arbeitsblatt

Much of wxMaxima is self-explaining, but some details require
attention. [This
site](https://wxMaxima-developers.github.io/wxmaxima/help.html) contains a
number of workbooks that address various aspects of wxMaxima. Working
through some of these (particularly the "10 minute (wx)Maxima tutorial")
will increase oneâs familiarity with both the content of _Maxima_ and the
use of wxMaxima to interact with Maxima. This manual concentrates on
describing aspects of wxMaxima that are not likely to be self-evident and
that might not be covered in the online material.

Der Arbeitsblatt-Ansatz

Eine der Sachen, die neue Benutzer oft verwirrt, ist, dass das Arbeitsblatt
von wxMaxima in Zellen aufgeteilt ist, die nur auf Befehl vom Benutzer an
_Maxima_ gesendet werden. Dieser Ansatz ist jedoch für die Fehlersuche
praktisch. Auch hat der gegenüber der Alternative, nach jeder Änderung alle
Zellen im das Arbeitsblatt neu auszuwerten, oft viel Zeit.

Wenn Text in _wxMaxima_ eingegeben wird, erzeugt er automatisch eine neue
Zelle des Arbeitsblattes. Wenn dies eine Code-Zelle ist, kann ihr Inhalt an
Maxima gesendet werden und das Resultat dieser Aktion wird unter der Zelle
angezeigt, wie unten abgebildet.

![Input/output cell](./InputCell.png)

On evaluation of an input cell's contents the input cell _Maxima_ assigns a
label to the input (by default shown in red and recognizable by the `%i`) by
which it can be referenced later in the _wxMaxima_ session. The output that
_Maxima_ generates also gets a label that begins with `%o` and by default is
hidden, except if the user assigns the output a name. In this case by
default the user-defined label is displayed. The `%o`\-style label _Maxima_
auto-generates will also be accessible, though.

Außer Code-Zellen kennt wxMaxima auch Testzellen und solche mit Bildern oder
Überschriften. Jede Zelle hat ihren eigenen Speicher für das
Rückgängigmachen von Aktionen, was sich oft als hilfreich erwiesen
hat. Zudem besitzt, wie in fast allen Applikationen, das Arbeitsblatt einen
eigenen Speicher für die Rückgängigmachen-Funktion.

Die nun folgende Abbildung zeigt verschiedene Zelltypen.

![Ein Beispiel verschiedener Zelltypen](./cell-example.png)


### Zellen

Das Arbeitsblatt ist in Zellen aufgeteilt, von denen jede mehr Zellen oder
Elemente der folgenden Typen enthalten kann:

*   Eine oder mehr Zeilen an Eingabe für _Maxima_
*   Ein oder mehrere Bilder
*   Ausgaben oder Fragen von _Maxima_
*   Einen Textblock mit Dokumentation
*   Eine Überschrift

Wenn Text eingegeben ist, erzeugt _wxMaxima_ normalerweise gleich eine
Code-Zelle. Andere Zelltypen können über das "Zellen"-Menü, die dort
dokumentierten Tastenkombinationen oder über die Werkzeugleiste erzeugt
werden.

### Horizontale und vertikale Cursors

Wenn in einer Textverarbeitung versucht wird, einen Satz auszuwählen, wird
diese versuchen, Beginn und Ende der Auswahl so zu verschieben, dass ganze
Wörter ausgewählt sind. _wxMaxima_ wird aus diesem Grund, wenn mehr als eine
Zelle ausgewählt wird, die Auswahl auf ganze Zellen ausdehnen.

Was ungewöhnlich erscheinen kann ist, dass _wxMaxima_ alternativ einen
vertikalen oder einen horizontalen Cursor darstellen kann:

*   Wenn der Cursor über einer Zelle und unter einer anderen Zelle steht,
    wird er vertikal dargestellt, und erlaubt es, ganze Zellen auszuwählen.
*   Innerhalb einer Zelle wird der Cursor vertikal dargestellt.

### Auto-Vervollständigung

wxMaxima versucht, automatisch die Namen von Befehlen oder Variablen zu
vervollständigen, wenn der Menüpunkt (Vervollständige Befehl) angewählt
wird, oder die Tastenkombination Strg+Leertaste gedrückt wird. Die
automatische Vervollständigung erkennt oft den Kontext, in dem sie
ausgeführt wird, und kann beispielsweise Dateinamen oder Einheiten für
ezUnits vorschlagen.

![ezUnits](./ezUnits.png)

Besides completing a file name, an unit name or the current commandâs or
variableâs name the autocompletion is able to show a template for most of
the commands indicating the type (and meaning) of the parameters this
program expects. To activate this feature press
&lt;Shift&gt;+&lt;Ctrl&gt;+&lt;Space&gt; or select the respective menu item
(Cell/Show Template).

#### Griechische Zeichen

Computers traditionally store characters in 8-bit values. This allows for a
maximum of 256 different characters. All letters, numbers, and control
symbols (end of transmission, end of string, lines and edges for drawing
rectangles for menus _etc_.) of nearly any given language can fit within
that limit.

For most countries the codepage of 256 characters that has been chosen does
not include Greek letters, though. To overcome this limitation Unicode has
been invented: This is a method of including characters that are not
normally used in the English language in a text that (as long as only the
basic form of Latin characters is used) looks like plain 8-bit ASCII.

_Maxima_ allows for unicode characters if it runs on a lisp (the language on
which _Maxima_ is built) that supports them. If the _wxWidgets_ library on
which _wxMaxima_ is built on supports Unicode characters, too, then
_wxMaxima_ can be built with Unicode support. In this case it (besides a
Greek Characters Sidebar) provides a method of entering Greek characters
using the keyboard:

*   An alpha is entered by pressing the <ESC> key and then typing in the
    character a followed by a press on the <ESC> key.
*   A beta is entered by pressing the <ESC> key and then typing in the
    character b followed by a press on the <ESC> key.
*   A gamma is entered by pressing the <ESC> key and then typing in the
    character c followed by a press on the <ESC> key.
*   ...and so on.

If the system does not provide unicode support _wxMaxima_ will still provide
a method of showing Greek characters: Variable names like "alpha" are always
displayed as the corresponding Greek symbols in _wxMaxima_ output.

The lowercase Greek letters actually can be entered both by enter a Latin
letter or the whole Latin name of the Greek letter followed and preceded by
a press of the escape key:

~~~~
a alpha    i iota      r rho
b beta     k kappa     s sigma
g gamma    l lambda    t tau
d delta    m mu        u upsilon
e epsilon  n nu        f phi
z zeta     x xi        c chi
h eta      om omicron  y psi
q theta    p pi        o omega
~~~~

The same is true for the uppercase greek letters:

~~~~
A Alpha    I Iota      R Rho
B Beta     K Kappa     S Sigma
G Gamma    L Lambda    T Tau
D Delta    M Mu        U Upsilon
E Epsilon  N Nu        P Phi
Z Zeta     X Xi        C Chi
H Eta      Om Omicron  Y Psi
T Theta    P Pi        O Omega
~~~~

This mechanism also allows to enter some miscellaneous mathematical symbols:

~~~~
hbar          planck's constant: a h with a horizontal bar above it
Hbar          a H with a horizontal bar above it
~~~~

This mechanism also allows to enter some miscellaneous mathematical symbols:


~~~~
2             squared
3             to the power of three
/2            1/2
partial       partial sign (the d of dx/dt)
integral      integral sign
sq            root
ii            imaginary
ee            element
hb or hbar    h barred
Hbar          H barred
in            in
impl,implies  implies
inf           infinity
empty         empty
TB            Big triangle right
tb            small triangle right
and           and
or            or
xor           xor
nand          nand
nor           nor
equiv         equivalent
not           not
union         union
inter         intersection
subseteq      subset or equal
subset        subset
notsubseteq   not subset or equal
notsubset     not subset
approx        approximately
propto        proportional to
neq,!=,/=
or #          not equal to
+/- or pm     a plus/minus sign
<= or leq     equal or less than
>= or geq     equal or greater than
<< or ll      much less than
>> or gg      much greater than
equiv         equivalent to
qed           end of proof
sum           sum sign
prod          product sign
exists        "there exists" sign
nexists       "there is no" sign
parallel      a "parallel" sign
perp          a "perpendicular" sign
leadsto       a "leads to" sign
->            a right arrow
-->           a long right arrow
~~~~

If a special symbol isnât in the list it is possible to input arbitrary
unicode characters by pressing `<ESC>number of the
character<ESC>`. `<ESC>61<ESC>` therefore results in an `a`.

Be aware that a _Maxima_ running on a lisp without Unicode support might not
be able to deal with files that contain special Unicode characters.

Please note that most of these symbols (notable exceptions are the logic
symbols) do not have a special meaning in _Maxima_ and therefore will be
interpreted as ordinary characters - or might provoke unexpected results in
case that the _Maxima_ is being run by a lisp that doesnât support dealing
with special Unicode characters.


### Side Panes

Shortcuts to the most important _Maxima_ commands and a history of the last
issued commands can be accessed using the side panes. They can be enabled
using the "View" menu. The shortcuts can be moved to where the location is
most convenient, inside or outside the _wxMaxima_ window. The same commands
can be accessed via the menus. Also, a pane that contains Greek letters can
be opened. This pane provides an alternative to the methods of entering
these letters that the preceding section discusses.

![Example of different side panes](./SidePanes.png)

### MathML output

Several word processors and similar programs either recognize MathML input
and automatically insert it as an editable 2D equation - or (like
LibreOffice 5.1) have an equation editor that offers an âimport MathML
from clipboardâ feature. In order to accommodate this feature, _wxMaxima_
offers a âcopy to word processorâ right-click menu entry that outputs
the selected part of an equation with MathML formatting.


### Markdown support

A wxMaxima workbook can be exported as either an HTML file or a LaTeX
file. The program uses a set of standard markdown conventions. Markdown in
many cases collides with the notations that are frequently used for
mathematics. wxMaxima will recognize bullet lists, though, for the HTML and
TeX export when the items are marked with stars.

~~~~
Ordinary text
 * One item, indentation level 1
 * Another item at indentation level 1
   * An item at a second indentation level
   * A second item at the second indentation level
 * A third item at the first indentation level
Ordinary text
~~~~

wxMaxima will recognize text starting with > chars as block quotes:

~~~~ Ordinary text > quote quote quote quote > quote quote quote quote >
quote quote quote quote Ordinary text ~~~~

wxMaxima will also recognize `=>` and replace it by a

cogito => sum.

Other symbols the markdown parser will recognize are `<=` and `>=` for
comparisons, a double-pointed double arrow (`<=>`), single- headed arrows
(`<->`, `->` and `<-`) and `+/-` as the respective sign. For TeX output also
`<<` and `>>` are recognized.

### Hotkeys

Most hotkeys can be found in the text of the respective menus. Since they
are actually taken from the menu text and thus can be customized by the
translations of _wxMaxima_ to match the needs of users of the local
keyboard, we do not document them here. A few hotkeys or hotkey aliases,
though, are not documented in the menus:

*   `Ctrl+Shift+Delete` deletes a complete cell.
*   `Ctrl+Tab` or `Ctrl+Shift` triggers the auto-completion mechanism.
*   `Ctrl+I` or `Ctrl+O` Zoom in or out.
*   `Shift+Space` inserts a non-breaking space.

### Raw TeX in the TeX export

If a text cell begins with `TeX:` the TeX export contains the literal text
that follows the `TeX:` marker. Using this feature allows the entry of TeX
markup within the _wxMaxima_ workbook.

## File Formats

The material that is developed in a wxMaxima session can be stored for later
use in any of three ways:


### .mac

.mac files are ordinary text files that can be read using Maximaâs read
command or wxMaximaâs File/Batch File menu entry.

One Example:

A .mac file named Quadratic.mac was created. It consists of two commands:
`f(x) := (x - 5)^2;` and `wxdraw2d( explicit(f(x),x,-5,15))`. The result of
entering that batch file appears below. (Some directory path information is
deleted.)

![Batch image](./BatchImage.png)

    %i1 read("test.mac");

You can be use `.mac` files for writing own library of macros. But since
they donât contain enough structural information they cannot be read back
as a wxMaxima session.

### .wxm

.wxm files contain the input for Maxima, as well as any text cells, title
cells and chapter or section cells the user has typed in. On maxima versions
>5.38 they can be read using Maxima's `load()` function just as .mac files
can be. Maximaâs output is not saved along with the .wxm file, though.

### .wxmx

This xml-based file format saves all text and images the work sheet
contains. It is the preferred file format now and comes in two flavors:

*   Files saved in the version-control friendly .wxmx format contain all
    images in a standard compressed format (.png) and a uncompressed copy of
    the xml tree that contains the structure of the document and the text
    typed in by the user. Since the images are compressed individually and
    the text is saved as text a version control system like bazaar,
    subversion, git or mercurial will only have to save the elements of the
    file that actually have changed since the last version.
*   Files saved in disk space-optimized .wxmx format are compressed as a
    whole. If no version control system is used this will save disk space:

    *   The portion of the file that is pure xml data tends to get
        fundamentally smaller when being compressed
    *   and after the compression recurring data like image headers will use
        up only a fraction of the space they originally did.

    This comes at the cost, though, that the change of even a single line of text in the uncompressed version tends to completely change the structure of the compressed version of a file. A version control system that deals with such a file will - however optimized it might be on handling differences between binary files - will therefore have to track (and to store) a much higher number of differences between two file versions than necessary; Since most version control systems compress the data they store on the server the server space occupied by the initial version of both .wxmx flavors should be nearly identical in size.



## Configuration options

For some common configuration variables wxMaxima offers two ways of
configuring:

*   The configuration dialog box below lets you change their default values
    for the current and subsequent sessions.
*   Also, the values for most configuration variables can be changed for the
    current session only by overwriting their values from the worksheet, as
    shown below.

![wxMaxima configuration 1](./wxMaxima_configuration_001.png)

### Default animation framerate

The animation framerate that is used for new animations is kept in the
variable `wxanimate_framerate`. The initial value this variable will contain
in a new worksheet can be changed using the configuration dialogue.

### Default plot size for new maxima sessions

After the next start plots embedded into the worksheet will be created with
this size if the value of `wxplot_size` isn't changed by maxima.

In order to set the plot size of a single graph only use the following
notation can be used that sets a variableâs value for one command only:

    wxdraw2d( explicit(x^2,x,-5,5)), wxplot_size=[480,480]$

### Use jsMath fonts

The style menu allows customization that pertains to the appearance of your
notebook as you work.

![wxMaxima configuration 2](./wxMaxima_configuration_002.png)

It also contains the option to use jsMath fonts. It is a good idea to
install these fonts since they were especially designed for mathematics and
tend to resolve issues that can be caused by broken fonts. If they are not
installed on your computer, the option to activate them will not be
highlighted. The jsMath fonts can be found on [this
site](https://www.mpim-bonn.mpg.de/node/258). The site describes JsMath
fonts and provides installation instructions.


### Match parenthesis in text controls

This option enables two things:

*   If an opening parenthesis, bracket or double quote is entered wxMaxima
    will insert a closing one after it.
*   If text is selected if any of these keys is pressed the selected text
    will be put between the matched signs.

### Autosave interval

If this value is set to a value bigger than zero _Maxima_ will work in a
more mobile-device-like fashion:

*   Files are saved automatically on exit
*   And the file will automatically be saved every n minutes.

For the automatic saving functionality to work wxMaxima needs to know a name
to save the file with, though. This means this feature will only work if the
file has already been saved to or opened from the disk.

* * *


Extensions to _Maxima_
======================

_wxMaxima_ is primarily a graphical user interface for _Maxima_. As such,
its main purpose is to pass along commands to _Maxima_ and to report the
results of executing those commands. In some cases, however, _wxMaxima_ adds
functionality to _Maxima_. _wxMaxima_âs ability to generate reports by
exporting a workbookâs contents to HTML and LaTeX files has been
mentioned. This section considers some ways that _wxMaxima_ enhances the
inclusion of graphics into a session. described here.

## Subscripted variables

if wxsubscripts is set to true variable names of the format `x_y` are
displayed using a subscript if

*   `y` is a single letter
*   `y` is an integer

If the variable name doesnât match these requirements it can still be
declared as "to be subscripted" using the command
`wxdeclare_subscript(variable_name);` or
`wxdeclare_subscript([variable_name1,variable_name2,...]);` Declaring a
variable as subscripted can be reverted using the following command:
`wxdeclare_subscript(variable_name,false);`

## User feedback in the statusbar

Long-running commands can provide user-feedback in the status bar. This user
feedback is replaced by any new feedback that is placed there (allowing to
use it as a progress indicator) and is deleted as soon as the current
command sent to maxima is finished. It is safe to use `wxstatusbar()` even
in libraries that might be used with plain Maxima (as opposed to wxMaxima):
If wxMaxima isn't present the `wxstatusbar()` command will just be left
unevelated.

    (%i2)	for i:1 thru 10 do (
	            /* Tell the user how far we got */
	            wxstatusbar(concat("Pass ",i)),
	            /* A truly long-running command */
	           a:makelist(o^i,o,1,100000)
	        )$

## Plotting

Plotting (having fundamentally to do with graphics) is a place where a
graphical user interface will have to provide some extensions to the
original program.

### Embedding a plot into the work sheet

Maxima normally instructs the external program gnuplot to open a separate
window for every diagram it creates. Since many times it is convenient to
embed graphs into the work sheet instead wxMaxima provides its own set of
plot functions that donât differ from the corresponding maxima functions
save in their name: They are all prefixed by a âwxâ. For example
`wxplot` corresponds to `plot`, `wxdraw` corresponds to `draw` and
`wxhistogram` corresponds to `histogram`.

### Making embedded plots bigger or smaller

As noted above, the configure dialog provides a way to change the default
size plots are created with which sets the starting value of
`wxplot_size`. The plotting routines of wxMaxima respect this variable that
specifies the size of a plot in pixels. It can always be queried or used to
set the size of the following plots:

    %i1 wxplot_size:[1200,800];
    %o1 [1200,800];

    %i2 wxdraw2d(
        explicit(
            sin(x),
            x,1,10
        )
    )$

If the size of only one plot is to be changed _Maxima_ provides a canonical
way to change an attribute only for the current cell.

     %i1 wxplot_size:[1200,800];
     %o1 [1200,800];

     %i1 wxdraw2d(
             explicit(
                 sin(x),
                 x,1,10
             )
         ),wxplot_size=[1600,800]$

    %i1 wxdraw2d(
        explicit(
            sin(x),
            x,1,10
        )
    ),wxplot_size=[1600,800]$


### Better quality plots

Gnuplot doesnât seem to provide a portable way of determining whether it
supports the high-quality bitmap output the cairo library provides. On
systems where gnuplot is compiled to use this library the pngcairo option
from the configuration menu (that can be overridden by the variable
`wxplot_pngcairo`) enables support for antialiasing and additional line
styles.

### Opening up plots in interactive gnuplot windows

If a plot was generated using the `wxdraw`-type commands (`wxplot` isn't
supported by this feature) and the file size of the underlying gnuplot
project isn't way too high wxMaxima offers a right-click menu that allows to
open the plot in an interactive gnuplot window.

### Opening gnuplot's command console in windows created by `plot` and
`draw`

On MS Windows if in Maxima's variable `gnuplot_command` "gnuplot" is
replaced by "wgnuplot" gnuplot offers the possibility to open a console
window gnuplot commands can be entered into. Unfortunately enabling this
feature causes gnuplot to "steal" the keyboard focus for a short time every
time a plot is prepared.

### Embedding animations into the spreadsheet

The `with_slider_draw` command is a version of `wxdraw2d` that does prepare
multiple plots and allows to switch between them by moving the slider on top
of the screen. If ImageMagick is installed wxMaxima even allows to export
this animation as an animated gif.

The first two arguments for `with_slider_draw` are the name of the variable
that is stepped between the plots and a list of the values of these
variable. The arguments that follow are the ordinary arguments for
`wxdraw2d`:

    with_slider_draw(
        f,[1,2,3,4,5,6,7,10],
        title=concat("f=",f,"Hz"),
        explicit(
            sin(2*%pi*f*x),
            x,0,1
        ),grid=true
    );

The same functionality for 3D plots is accessible as `with_slider_draw3d`.

There is a second set of functions making use of the slider

*   `wxanimate_draw` and
*   `wxanimate_draw3d`:

    wxanimate_draw(
        a, 3,
        explicit(sin(a*x), x, -4, 4),
        title=printf(false, "a=~a", a));

Normally the animations are played back or exported with the frame rate
chosen in the configuration of wxMaxima. To set the speed an individual
animation is played back the variable wxanimate\_framerate can be used:

    wxanimate(a, 10,
        sin(a*x), [x,-5,5]), wxanimate_framerate=6$

The animation functions have a pitfall that one has to be aware of when
using them: The slider variable's value are substituted into the expression
that is to be plotted - which will fail, if the variable isn't directly
visible in the expression. Therefore the following example will fail:

    f:sin(a*x);
    with_slider_draw(
        a,makelist(i/2,i,1,10),
        title=concat("a=",float(a)),
        grid=true,
        explicit(f,x,0,10)
    )$

If Maxima is forced to first evaluate the expression and then asked to
substitute the sliderâs value plotting works fine instead:

    f:sin(a*x);
    with_slider_draw(
        a,makelist(i/2,i,1,10),
        title=concat("a=",float(a)),
        grid=true,
        explicit(''f,x,0,10)
    )$

### Opening multiple plots in contemporaneous windows

While not being a provided by wxMaxima this feature of Maxima (on setups
that support it) sometimes comes in handily. The following example comes
from a post from Mario Rodriguez to the Maxima mailing list:

    load(draw);

    /* Parabola in window #1 */
    draw2d(terminal=[wxt,1],explicit(x^2,x,-1,1));

    /* Parabola in window #2 */
    draw2d(terminal=[wxt,2],explicit(x^2,x,-1,1));

    /* Paraboloid in window #3 */
    draw3d(terminal=[wxt,3],explicit(x^2+y^2,x,-1,1,y,-1,1));

### The "Plot using draw" sidepane

The "Plot using draw" sidebar hides a simple code generator that allows to
generate scenes that make use of some of the flexibility of the _draw_
package maxima comes with.

#### 2D

Generates the sceleton of a `draw()` command that draws a 2D scene. This
scene later has to be filled with commands that generate the scene's
contents, for example by using the buttons in the rows below the "2D"
button.

One helpful feature of the 2D button is that it allows to setup the scene as
an animation in which a variable (by default it is _t_ has a different value
in each frame: Often an moving 2D plot allows easier interpretation than the
same data in a non-moving 3D one.

#### 3D

Generates the sceleton of a `draw()` command that draws a 3D scene. If
neither a 2D or a 3D scene are set up all of the other buttons set up a 2D
scene that contains the command the button generates.

#### Expression

Appends a standard plot of an expression like `sin(x)`, `x*sin(x)` or
`x^2+2*x-4` to the `draw()` command the cursor currently is in. If there is
no draw command a 2D scene with the plot is generated. Each scene can be
filled with any number of plots.

#### Implicit plot

Tries to find all points an expression like `y=sin(x)`, `y*sin(x)=3` or
`x^2+y^2=4` is true at and plots the resulting curve in the `draw()` command
the cursor currently is in. If there is no draw command a 2D scene with the
plot is generated.

#### Parametric plot

Steps a variable from a lower limit to an upper limit and uses two
expressions like `t*sin(t)` and `t*cos(t)` for generating the x, y (and in
3D plots also z) coordinates of a curve that is put into the current draw
command.

#### Points

Draws many points that can optionally be joined. The coordinates of the
points are taken from a list of lists, an 2D array or one list or array for
each axis.

#### Diagram title

Draws a title on the upper end of the diagram,

#### Axis

Sets up the axis.

#### Contour

(Only for 3D plots): Adds contour lines similar to the ones one can find in
a map of a mountain to the plot commands that follow in the current draw()
command and/or to the ground plane of the diagram. Alternatively this wizard
allows skipping drawing the curves entirely only showing the contour plot.

#### Plot name

Adds a legend entry showing the next plot's name to the legend of the
diagram. An empty name disables generating legend entries for the following
plots.

#### Line color

Sets the line color for the following plots the current draw command
contains.

#### Fill color

Sets the fill color for the following plots the current draw command
contains.

#### Grid

Pops up a wizard that allows to set up grid lines.

#### Accuracy

Allows to select an adequate point in the speed vs. accuracy tradeoff that
is part of any plot program.

## Embedding graphics

if the `.wxmx` file format is being used embedding files in a wxMaxima
project can be done as easily as per drag-and-drop. But sometimes (for
example if an imageâs contents might change later on in a session) it is
better to tell the file to load the image on evaluation:

    show_image("man.png");

## wxmaximarc

The _Maxima_ user directory contains a text file named `wxmaxima-init.mac`
the contents of the file is passed to Maxima automatically every time a new
worksheet has been started.

To find out which directory Maxima uses as the user directory just type in
the following line:

    maxima_userdir;

The answer from Maxima will specify the name of the directory that the
startup file can be placed in.

    %o1 /home/username/.maxima


## Special variables wx...

*   `wxsubscripts` tells _Maxima_ if it should convert variable names that
    contain an underscore (`R_150` or the like) into subscripted
    variables. See `wxdeclare_subscript` for details which variable names
    are automatically converted.
*   `wxfilename`: This variable contains the name of the file currently
    opened in _wxMaxima_. On Windows this piece of information is available
    only if in the configuration dialogue the checkbox `Maxima/maxima's pwd
    is path to document` is checked.
*   `wxplot_pngcairo` tells whether _wxMaxima_ tries to use _gnuplot_âs
    pngcairo terminal that provides more line styles and a better overall
    graphics quality. This variable can be used for reading or overriding
    the respective setting in the configuration dialog.
*   `wxplot_size` defines the size of embedded plots.
*   `wxchangedir`: On most operating systems _wxMaxima_ automatically sets
    _Maxima_âs working directory to the directory of the current
    file. This allows file I/O (e.g. by `read_matrix`) to work without
    specifying the whole path to the file that has to be read or written. On
    Windows this feature is deactivated: The Lisp Standard doesnât contain
    a concept of the current working directory. Therefore there is no
    standard way of setting it and changing to a directory that isnât on
    the drive Maxima has been installed to might cause Maxima to try to read
    is own package files from this drive, too, instead of from the drive to
    which _Maxima_ has been installed. Setting wxchangedir to `true` tells
    wxMmaxima that it has to risk that and to set Maximaâs working
    directory.
*   `wxanimate_framerate` The number of frames per second the following
    animations have to be played back with. -1 tells wxMaxima to use the
    default frame rate from the config dialog.

## Pretty-printing 2D output

The function `table_form()` displays a 2D list in a form that is more
readable than the output Maximaâs default output routine. The input is a
list of one or more lists. Like the print command, this command displays
output even when ended with a dollar sign. Ending the command with a
semicolon results in the same table along with a "done" statement.

    table_form(
        [
            [1,2],
            [3,4]
        ]
    )$

As the next example shows, the lists that are assembled by the `table_form`
command can be created before the command is executed.

![A third table example](./Matrix%20table%20example.png)

Also, because a matrix is a list of lists, matrices can be converted to
tables in a similar fashion.

![Another table_form example](./Second%20table%20example.png)

## Bug reporting

wxMaxima provides a few functions that gather bug reporting information
about the current system:

*   `wxbuild_info()` gathers information about the currently running version
    of wxMaxima
*   `wxbug_report()` tells how and where to file bugs

* * *


## Marking output being drawn in red

Maxima's `box()` command causes wxMaxima to print its argument with a red
background.

Troubleshooting
===============

## Cannot connect to Maxima

Since Maxima (the program that does the actual mathematics) and wxMaxima
(providing the easy-to-use user interface) are separate programs that
communicate by the means of a local network connection. Therefore the most
probable cause is that this connection is somehow not working. For example a
firewall could be set up in a way that it doesnât just prevent against
unauthorized connections from the internet (and perhaps to intercept some
connections to the internet, too), but it also to blocks
inter-process-communication inside the same computer. Note that since Maxima
is being run by a lisp processor the process communication that is blocked
from does not necessarily have to be named "maxima". Common names of the
program that opens the network connection would be sbcl, gcl, ccl, lisp.exe
or similar names.

On Un\*x computers another possible reason would be that the loopback
network that provides network connections between two programs in the same
computer isnât properly configured.

## How to save data from a broken .wxmx file

Internally most modern xml-based formats are ordinary zip-files with one
special characteristic: the first file in the archive is stored uncompressed
and provides information about what type of program can open this file.

If the zip signature at the end of the file is still intact after renaming a
broken .wxmx file to .zip most operating systems will provide a way to
extract any portion of information that is stored inside it. The can be done
when there is the need of recovering the original image files from a text
processor document. If the zip signature isnât intact that does not need
to be the end of the world: If wxMaxima during saving detected that
something went wrong there will be a wxmx~ file whose contents might help
and even if there isnât such a file: If the configuration option is set
that .wxmx files have to be optimized for version control it is possible to
rename the .wxmx file to a .txt file and to use a text editor to recover the
XML portion of the file's contents.

If the text file containing this contents is saved as a file ending in .xml
wxMaxima will know how to recover the text of the document from it.

## I want some debug info to be displayed on the screen before my command
has finished

Normally wxMaxima waits for the whole 2D formula to be transferred before it
begins to typeset. This saves time for making many attempts to typeset a
only partially completed equation. There is a `disp` command, though, that
will provide debug output immediately and without waiting for the current
Maxima command to finish:

    for i:1 thru 10 do (
       disp(i),
       t:makelist(i,i,1000000),
       length(t)
    )$

## wxMaxima on Windows crashes on displaying seemingly simple equations

The jsMath fonts allow for excellent 2D-display of equations. But there are
broken versions of this package that crash wxMaxima. A working version can
be downloaded from
[http://www.math.union.edu/~dpvc/jsmath/download/jsMath-fonts.html](http://www.math.union.edu/%7Edpvc/jsmath/download/jsMath-fonts.html).
To make wxMaxima actually use these fonts the according checkbox has to be
enabled in the Styles tab of wxMaximaâs configuration dialogue.

## Plotting only shows an closed empty envelope with an error message

This means that _wxMaxima_ could not read the file _Maxima_ that was
supposed to instruct gnuplot to create.

Possible reasons for this error are:

*   The plotting command is part of a third-party package like
    `implicit_plot` but this package was not loaded by Maximaâs `load()`
    command before trying to plot.
*   Maxima tried to do something the currently installed version of gnuplot
    isnât able to understand. In this case the file maxout.gnuplot in the
    directory Maximaâs variable maxima\_userdir points to contains the
    instructions from Maxima to gnuplot. Most of the time this fileâs
    contents therefore are helpful when debugging the problem.
*   Gnuplot was instructed to use the pngcairo library that provides
    antialiasing and additional line styles, but it was not compiled to
    support this possibility. Solution: Uncheck the "Use the cairo terminal
    for plot" checkbox in the configuration dialog and donât set
    `wxplot_pngcairo` to true from Maxima.
*   Gnuplot didnât output a valid .png file.

## Plotting an animation results in âerror: undefined variableâ

The value of the slider variable by default is only substituted into the
expression that is to be plotted if it is visible there. Putting an `ev()`
around this expression should resolve this problem.

## I lost a cell contents and undo doesnât remember

There are separate undo functions for cell operations and for changes inside
of cells so chances are low that this ever happens. If it does there are
several methods to recover data:

*   wxMaxima actually has two undo features: The global undo buffer that is
    active if no cell is selected and a per-cell undo buffer that is active
    if the cursor is inside a cell. It is worth trying to use both undo
    options in order to see if an old value can still be accessed.
*   If you still have a way to find out what label Maxima has assigned to
    the cell just type in the cellâs label and its contents will reappear.
*   If you donât: Donât panic. In the âViewâ menu there is a way to
    show a history pane that shows all Maxima commands that have been issued
    recently.
*   Wenn nichts anderes funktioniert, bietet Maxima die Möglichkeit an, alle
    bisherigen Befehle nochmals auszuführen:

    %i1 playback();


## wxMaxima starts up with the message âMaxima process Terminated.â

One possible reason is that Maxima cannot be found in the location that is
set in the âMaximaâ tab of wxMaximaâs configuration dialog and
therefore wonât run at all. Setting the path to a working Maxima binary
should fix this problem.

## Maxima hört nicht auf zu rechnen und reagiert nicht auf Eingaben

It is theoretically possible that wxMaxima doesnât realize that Maxima has
finished calculating and therefore never gets informed it can send new data
to Maxima. If this is the case âTrigger evaluationâ might resynchronize
the two programs.

## File I/O from Maxima doesnât work on Windows

On Windows, file I/O is not relative to the directory of the current file by
default. If you store the Maxima file on the drive on which wxMaxima is
installed, then setting `wxchangedir` to `true` will fix that for `load`,
`read_list`, `batch`, `read_matrix`, `save` and all similar commands.

Setting this variable to `true` might have a drawback, though: Maxima knows
which directory it is installed in and will search for any additional
package that is requested by a `load` command in this directory, too. But it
might not know which drive it is installed on. If `wxchangedir` is `true`
and the current file is saved on a different drive than the one Maxima is
installed on Maxima therefore might fail to load the additional packages it
was bundled with.

## My SBCL-based Maxima runs out of memory

SBCL by default comes with a memory limit that allows it to run even on
low-end computers. When compiling a big software package like lapack or
dealing with extremely big lists or equations this limit might be too
low. In order to extend the limits sbcl can be provided with the command
line parameter `--dynamic-space-size` that tells sbcl how many megabytes it
should reserve. A 32bit-windows-sbcl can reserve up to 999 Megabytes,
1800. A 64-bit sbcl version running on windows can be instructed to use more
than the about 1280 Megabytes compiling lapack needs.

One way to provide maxima (and thus sbcl) with command line parameters is
the "Additional parameters for Maxima" field of wxMaximaâs configuration
dialogue.

![sbcl memory](./sbclMemory.png)


## Input sometimes is sluggish/ignoring keys on Ubuntu

Installing the package `ibus-gtk` should resolve this issue. See
([https://bugs.launchpad.net/ubuntu/+source/wxwidgets3.0/+bug/1421558](https://bugs.launchpad.net/ubuntu/+source/wxwidgets3.0/+bug/1421558))
for details.

## wxMaxima halts when Maxima processes Greek characters or Umlauts

If your Maxima is based on sbcl the following lines have to be added to your
`.sblrc`:

    (setf sb-impl::*default-external-format* :utf-8)

The folder this file has to be placed in is system- and
installation-specific. But any sbcl-based Maxima that already has evaluated
a cell in the current session will happily tell where it can be found after
getting the following command:

    :lisp (sb-impl::userinit-pathname)

## Plotting

### Can I make wxMaxima output both image files and embedded plots at once?

It is easier to make it first generate the images and then import them again
as the following two functions will do:

~~~~
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
~~~~

### Can I set the aspect ratio of a plot?

Not directly using Maxima. But there are gnuplot commands for it:

     wxdraw2d(
         user_preamble="set size ratio 1; set tmargin 3; set bmargin 3;
                        set lmargin 3; set rmargin 3",
         explicit(sin(x),x,1,10)
     ),wxplot_size=[1000,1000];

* * *


FAQ
===

## Is there a way to make more text fit on a pdfLaTeX page?

There is: Just add the following lines to the LaTeX preamble (for example by
using the respective field in the config dialogue ("Export"->"Additional
lines for the TeX preamble"):

    \usepackage[a4paper,left=1cm,right=1cm,top=1cm,bottom=1cm]{geometry}

* * *


Command-line arguments
======================

Most operating systems provide less complicated ways of starting programs
than the command line so this possibility is only rarely used. wxMaxima
still provides some command line switches, though.

*   `-v` or `--version`: Output the version information
*   `-h` or `--help`: Output a short help text
*   `-o` or `--open`: Open the filename given as argument to this
    command-line switch
*   `-e` or `--eval`: Evaluate the file after opening it.
*   `-b` or `--batch`: If the command-line opens a file all cells in this
    file are evaluated and the file is saved afterwards. This is for example
    useful if the session described in the file makes Maxima generate output
    files. Batch-processing will be stopped if wxMaxima detects that Maxima
    has output an error and will pause if Maxima has a question: Mathematics
    is somewhat interactive by nature so a completely interaction-free batch
    processing cannot always be guaranteed.
*   `-f` or `--ini`: Use the init file that was given as argument to this
    command-line switch
*   `-m` or `--maxima`    allows to specify the location of the maxima binary

Instead of a minus some operating systems might use a dash in front of the
command-line switches.



* * *

Annotated Index
===============

**A**
[Animations](#Embedding-animations-into-the-spreadsheet): commands to embed animations into a wxMaxima workbook

[Animations, failure](#Outputting-animations-doesn_0027t-work): why creation
and execution of some animations might not work

[Autocompletion](#Command-autocompletion): using _wxMaxima_ a command

[Aspect ratio](#Can-I-set-the-aspect-ratio-of-a-plot_003f): set the aspect
ratio for a plot

[Autosave](#Autosave-interval): set interval for saving a wxMaxima session
file (workbook)

**C**
[Cells](#Cells): the nature and use of cells in a _wxMaxima_ workbook

[Cells, Input/Output](#Input%20Output%20Cell): Cells that contain one or
more commands and the results of executing the command(s)

[Command autocompletion](#Command-autocompletion): using _wxMaxima_ a
command

[Configure wxMaxima](#Configuration-options): how to configure a _wxMaxima_
workbook

[Cursors, vertical and horizontal](#Drag%20and%20drop): entering text or
selecting entered material

**D**
[Drag and drop](#Drag%20and%20drop): Copying and pasting material in one or more cells or executing a sequence of cells

**F**
[File Info](#wxfilename%20command): command to print name and location of currently-open wxMaxima workbook

[File Formats](#File-Formats): types of files that _wxMaxima_ can create and
open

**G**
[Greek characters](#Greek-characters): Ways to enter Greek letters into a _wxMaxima_ workbook (see Side Panels below)

**H**
[HTML](#Markdown-support): export material as html

[Hotkeys](#Hotkeys): shortcut key combinations, alternatives to menu options

**I**
[Input/Output Cells](#Input%20Output%20Cell): Cells that contain one or more commands and the results of executing the command(s)

**J**
[jsMath](#Use-jsMath-fonts): useful fonts set for mathematical expressions

**L**
[LaTeX](#Markdown-support): export material as LaTeX

[Lost content](#I-lost-a-cell-contents-and-undo-doesn_0027t-remember): ways
to recover lost cell contents

**M**
[Macro files](#g_t_002emac): .mac files, batch files that can be entered into _wxMaxima_ workbooks

[MathML](#MathML-output): export mathematical expressions with MathML markup

[Matrix](#Matrix): Create a matrix and convert the matrix to a table

[Maxima and wxMaxima](#Maxima-and-wxMaxima "Maxima and wxMaxima"): the
relationship between the Maxima CAS and the wxMaxima front-end

[Maxima-wxMaxima connection](#Cannot-connect-to-Maxima): possible reasons
from failure of wxMaxima to communicate with Maxima

**N**
[Name of file](#wxfilename%20command): command to print name and location of currently-open wxMaxima workbook

**P**
[Plot, aspect ratio](#Can-I-set-the-aspect-ratio-of-a-plot_003f): set the aspect ratio for a plot

[Plot, embedding](#Embedding-a-plot-into-the-work-sheet): embed a
_Maxima_\-generated plot in a _wxMaxima_ workbook

[Plot,
failure](#Plotting-only-shows-an-closed-empty-envelope-with-an-error-message):
some reasons that attempts to plot might fail

[Plot file, embedding](#Embedding-graphics): embed a plot file in a
_wxMaxima_ workbook

[Plot size](#Making-embedded-plots-bigger-or-smaller): control the size of
embedded plots

[Plots, better quality](#Better-quality-plots): notes related to the quality
of gnuplot plots

[Plots, multiple](#Opening-multiple-plots-in-contemporaneous-windows): open
multiple plots in contemporaneous windows

[Progress indicators](#statusbar): Giving user feedback in the status bar

**R**
[Recover lost content](#I-lost-a-cell-contents-and-undo-doesn_0027t-remember)

**S**
[Side Panes](#Side-Panes): collections of buttons that open dialog boxes, enter commands, and enter special characters

[Startup file location](#index-Startup-File): location of an initialization
file that is passed to _wxMaxima_ when it opens

[Status bar](#statusbar): Giving user feedback in the status bar

[Subscripted variables](#Subscripted-variables): using the command
`wxsubscripts` to display subscripted variables

**T**
[Table](#Pretty_002dprinting-2D-output): show lists of text and values as a table

[TeX markup](#Raw-TeX-in-the-TeX-export): allows TeX markup to be entered
into a text file

**U**
[undo](#I-lost-a-cell-contents-and-undo-doesn_0027t-remember): ways to recover lost cell contents

**W**
[with\_slider\_draw](#Embedding-animations-into-the-spreadsheet): command to embed animations -- alternative: `wxanimate_draw`

[with\_slider\_draw3d](#Embedding-animations-into-the-spreadsheet): command
to embed animations -- alternative: `wxanimate_draw3d`

[wxanimate\_draw](#Embedding-animations-into-the-spreadsheet): command to
embed animations

[wxanimate\_draw3d](#Embedding-animations-into-the-spreadsheet): command to
embed animations

[wxMaxima and Maxima](#Maxima-and-wxMaxima "wxMaxima and Maxima"): the
relationship between the Maxima CAS and the wxMaxima front-end

[wxMaxima documentation](#wxMaxima)

[wxMaxima workbook structure](#The-worksheet-approach): central features of
a _wxMaxima_ workbook

[wxm and wxmx files](#g_t_002ewxm): formats for _wxMaxima_ workbooks

[wxmx data](#Broken%20wxmx): retrieve data from a broken wxmx file
