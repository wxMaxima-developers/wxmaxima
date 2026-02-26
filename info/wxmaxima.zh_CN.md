# The wxMaxima user manual

WxMaxima is a graphical user interface (GUI) for the _Maxima_ computer
algebra system (CAS). WxMaxima allows one to use all of _Maxima_’s
functions. In addition, it provides convenient wizards for accessing the
most commonly used features. This manual describes some of the features that
make wxMaxima one of the most popular GUIs for _Maxima_.

![wxMaxima 的 logo](./wxMaximaLogo.png){ id=img_wxMaximaLogo }

______________________________________________________________________

# Introduction to wxMaxima

## _Maxima_ and wxMaxima

In the open-source domain, big systems are normally split into smaller
projects that are easier to handle for small groups of developers. For
example a CD burner program will consist of a command-line tool that
actually burns the CD and a graphical user interface that allows users to
implement it without having to learn about all the command line switches and
in fact without using the command line at all. One advantage of this
approach is that the developing work that was invested into the command-line
program can be shared by many programs: The same CD-burner command-line
program can be used as a “send-to-CD”-plug-in for a file manager
application, for the “burn to CD” function of a music player and as the CD
writer for a DVD backup tool. Another advantage is that splitting one big
task into smaller parts allows the developers to provide several user
interfaces for the same program.

一个像 Maxima 这样的计算机代数系统（CAS，computer algebra system）也适合前述框架。Maxima
可以用作任意精度计算器程序的底层软件，也可以在更大计算系统中作为公式转换程序来帮助计算（例如
[Sage](https://www.sagemath.org/)）。Maxima
也可以直接作为独立的计算系统使用，它可以通过命令行来访问。然而，在通常情况下，像 wxMaxima 这样的用户界面是使用 Maxima
的一种更有效的方法，特别是对于新手而言更是如此。

### _Maxima_

Maxima 是一个功能齐全的计算机代数系统（CAS）。CAS
是一个程序，它可以通过对公式变形并找到问题的解来求解数学问题，而不是仅仅输出结果的数值。换言之，Maxima
可以作为一个计算器，给出变量的数值表示，也可以提供解析解。此外，它还提供了一系列数值分析方法，用于求解没有解析解的方程或方程组。

![Maxima 截图，命令行](./maxima_screenshot.png){ id=img_maxima_screenshot }

Extensive documentation for _Maxima_ is [available in the
internet](https://maxima.sourceforge.io/documentation.html). Part of this
documentation is also available in wxMaxima’s help menu. Pressing the Help
key (on most systems the <kbd>F1</kbd> key) causes _wxMaxima_’s
context-sensitive help feature to automatically jump to _Maxima_’s manual
page for the command at the cursor.

### WxMaxima

_WxMaxima_ is a graphical user interface that provides the full
functionality and flexibility of _Maxima_. WxMaxima offers users a graphical
display and many features that make working with _Maxima_ easier. For
example _wxMaxima_ allows one to export any cell’s contents (or, if that is
needed, any part of a formula, as well) as text, as LaTeX or as MathML
specification at a simple right-click. Indeed, an entire workbook can be
exported, either as a HTML file or as a LaTeX file. Documentation for
_wxMaxima_, including workbooks to illustrate aspects of its use, is online
at the _wxMaxima_ [help
site](https://wxMaxima-developers.github.io/wxmaxima/help.html), as well as
via the help menu.

![wxMaxima 界面](./wxMaximaWindow.png){ id=img_wxMaximaWindow }

在 wxMaxima 中输入的计算式是由 Maxima 命令行工具在后台执行的。

## Workbook basics

Much of _wxMaxima_ is self-explaining, but some details require
attention. [This
site](https://wxMaxima-developers.github.io/wxmaxima/help.html) contains a
number of workbooks that address various aspects of _wxMaxima_. Working
through some of these (particularly the "10 minute _(wx)Maxima_ tutorial")
will increase one’s familiarity with both the content of _Maxima_ and the
use of _wxMaxima_ to interact with _Maxima_. This manual concentrates on
describing aspects of _wxMaxima_ that are not likely to be self-evident and
that might not be covered in the online material.

### The workbook approach

One of the very few things that are not standard in _wxMaxima_ is that it
organizes the data for _Maxima_ into cells that are evaluated (which means:
sent to _Maxima_) only when the user requests this. When a cell is
evaluated, all commands in that cell, and only that cell, are evaluated as a
batch. (The preceding statement is not quite accurate: One can select a set
of adjacent cells and evaluate them together. Also, one can instruct
_Maxima_ to evaluate all cells in a workbook in one pass.) _WxMaxima_’s
approach to submitting commands for execution might feel unfamiliar at the
first sight. It does, however, drastically ease work with big documents
(where the user does not want every change to automatically trigger a full
re-evaluation of the whole document). Also, this approach is very handy for
debugging.

如果输入文本到 wxMaxima
中，则会自动创建一个新的工作簿单元格。可以在工具栏中选择此单元格的类型。如果创建了一个（数学）代码单元格，则可以将该单元格发送到
Maxima，这将使得计算结果显示在代码下方。下面显示了一对这样的命令：

![输入、输出单元格](./InputCell.png){ id=img_InputCell }

On evaluation of an input cell’s contents the input cell _Maxima_ assigns a
label to the input (by default shown in red and recognizable by the `%i`) by
which it can be referenced later in the _wxMaxima_ session. The output that
_Maxima_ generates also gets a label that begins with `%o` and by default is
hidden, except if the user assigns the output a name. In this case by
default the user-defined label is displayed. The `%o`-style label _Maxima_
auto-generates will also be accessible, though.

除了（数学）输入单元格外，wxMaxima
中还可以使用文本单元格、图片单元格、标题单元格、节单元格和小节单元格等。每个单元格都有独立的撤消缓冲区，因此通过更改多个单元格的内容，然后逐步撤销不需要的更改来进行调试，是非常容易的。此外，工作簿本身有一个全局撤消缓冲区，可以撤消单元格编辑、添加和删除操作。

下图显示了不同的单元格类型（标题单元格、节单元格、小节单元格、文本单元格、输入/输出单元格和图片单元格）：

![wxMaxima 的不同类型单元格](./cell-example.png){ id=img_cell-example }

### Cells

The worksheet is organized in cells. WxMaxima knows the following cell
types:

- Math cells, containing one or more lines of _Maxima_ input.
- Output of, or a question from, _Maxima_.
- Image cells.
- Text cells, that can for example be used for documentation.
- A title, section or a subsection. 6 levels of different headings are
  possible.
- Page breaks.

当输入文本时，wxMaxima
的默认行为是自动创建一个数学单元格。可以使用“单元格”菜单、菜单中显示的热键或工具栏中的下拉列表创建其他类型的单元格。一旦创建了非数学单元格，输入到文档的任何内容都将被解释为文本。

A (C-style) [comment
text](https://maxima.sourceforge.io/docs/manual/maxima_singlepage.html#Comments)
can be part of a math cell as follows: `/* This comment will be ignored by
Maxima */`

"`/*`" marks the start of the comment, "`*/`" the end.

### Horizontal and vertical cursors

If the user tries to select a complete sentence, a word processor will try
to extend the selection to automatically begin and end with a word
boundary. Likewise, if more than one cell is selected, _wxMaxima_ will
extend the selection to whole cells.

What isn’t standard is that _wxMaxima_ provides drag-and-drop flexibility by
defining two types of cursors. _WxMaxima_ will switch between them
automatically when needed:

- 如果光标在两个单元格之间移动或单击，则会使用水平光标；
- A vertical cursor that works inside a cell. This cursor is activated by
  moving the cursor inside a cell using the mouse pointer or the cursor keys
  and works much like the cursor in a text editor.

When you start wxMaxima, you will only see the blinking horizontal
cursor. If you start typing, a math cell will be automatically created and
the cursor will change to a regular vertical one (you will see a right arrow
as "prompt", after the Math cell is evaluated
(<kbd>CTRL</kbd>+<kbd>ENTER</kbd>), you will see the labels, e.g. `(%i1)`,
`(%o1)`).

![(blinking) horizontal cursor after wxMaxima
start](./horizontal-cursor-only.png){ id=img_horizontal_cursor_only }

You might want to create a different cell type (using the "Cell" menu),
maybe a title cell or text cell, which describes, what will be done, when
you start creating your worksheet.

If you navigate between the different cells, you will also see the
(blinking) horizontal cursor, where you can insert a cell into your
worksheet (either a math cell, by just start typing your formula - or a
different cell type using the menu).

![(blinking) horizontal cursor between
cells](./horizontal-cursor-between-cells.png){
id=img_horizontal_cursor_between_cells }

### Sending cells to Maxima

The command in a code cell is executed once by pressing
<kbd>CTRL</kbd>+<kbd>ENTER</kbd>, <kbd>SHIFT</kbd>+<kbd>ENTER</kbd> or the
<kbd>ENTER</kbd> key on the keypad. The _wxMaxima_ default is to enter
commands when either <kbd>CTRL</kbd>+<kbd>ENTER</kbd> or
<kbd>SHIFT</kbd>+<kbd>ENTER</kbd> is entered, but _wxMaxima_ can be
configured to execute commands in response to <kbd>ENTER</kbd>.

### Command autocompletion

_WxMaxima_ contains an autocompletion feature that is triggered via the menu
(Cell/Complete Word) or alternatively by pressing the key combination
<kbd>CTRL</kbd>+<kbd>SPACE</kbd>. The autocompletion is
context-sensitive. For example, if activated within a unit specification for
ezUnits it will offer a list of applicable units.

![ezUnits](./ezUnits.png){ id=img_ezUnits }

Besides completing a file name, a unit name, or the current command or
variable name, the autocompletion is able to show a template for most of the
commands indicating the type (and meaning) of the parameters this program
expects. To activate this feature press
<kbd>SHIFT</kbd>+<kbd>CTRL</kbd>+<kbd>SPACE</kbd> or select the respective
menu item (Cell/Show Template).

#### Greek characters

计算机一般以 8 位值（8-bit）存储字符，所以最多可以储存 256
个不同字符。大部分语言中的所有字母、数字和控制符号（传输结束、字符串结束、为菜单绘制矩形的线条和边缘等）都符合该限制。

For most countries, the codepage of 256 characters that has been chosen does
not include things like Greek letters, though, that are frequently used in
mathematics. To overcome this type of limitation
[Unicode](https://home.unicode.org/) has been invented: An encoding that
makes English text work like normal, but to use much more than 256
characters.

_Maxima_ allows Unicode if it was compiled using a Lisp compiler that either
supports Unicode or that doesn’t care about the font encoding. As at least
one of this pair of conditions is likely to be true. _WxMaxima_ provides a
method of entering Greek characters using the keyboard:

- A Greek letter can be entered by pressing the <kbd>ESC</kbd> key and then
  starting to type the Greek character’s name.
- Alternatively it can be entered by pressing <kbd>ESC</kbd>, one letter (or
  two for the Greek letter omicron) and <kbd>ESC</kbd> again. In this case
  the following letters are supported:

| key | Greek letter | key | Greek letter | key | Greek letter |
| :-: | :----------: | :-: | :----------: | :-: | :----------: |
|  a  |    alpha     |  i  |     iota     |  r  |     rho      |
|  b  |     beta     |  k  |    kappa     |  s  |    sigma     |
|  g  |    gamma     |  l  |    lambda    |  t  |     tau      |
|  d  |    delta     |  m  |      mu      |  u  |   upsilon    |
|  e  |   epsilon    |  n  |      nu      |  f  |     phi      |
|  z  |     zeta     |  x  |      xi      |  c  |     chi      |
|  h  |     eta      | om  |   omicron    |  y  |     psi      |
|  q  |    theta     |  p  |      pi      |  o  |    omega     |
|  A  |    Alpha     |  I  |     Iota     |  R  |     Rho      |
|  B  |     Beta     |  K  |    Kappa     |  S  |    Sigma     |
|  G  |    Gamma     |  L  |    Lambda    |  T  |     Tau      |
|  D  |    Delta     |  M  |      Mu      |  U  |   Upsilon    |
|  E  |   Epsilon    |  N  |      Nu      |  P  |     Phi      |
|  Z  |     Zeta     |  X  |      Xi      |  C  |     Chi      |
|  H  |     Eta      | Om  |   Omicron    |  Y  |     Psi      |
|  T  |    Theta     |  P  |      Pi      |  O  |    Omega     |

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

用同样的方法还可以输入一些其他的数学符号：

| keys to enter  | mathematical symbol                                   |
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

<kbd>ESC</kbd><kbd>6</kbd><kbd>1</kbd><kbd>ESC</kbd> therefore results in an
`a`.

Please note that most of these symbols (notable exceptions are the logic
symbols) do not have a special meaning in _Maxima_ and therefore will be
interpreted as ordinary characters. If _Maxima_ is compiled using a Lisp
that doesn’t support Unicode characters they might cause an error message.

It may be the case that e.g. Greek characters or mathematical symbols are
not included in the selected font, then they can not be displayed.  To solve
that problem, select other fonts (using: Edit -> Configure -> Style).

### Unicode replacement

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


### Side Panes

Shortcuts to the most important _Maxima_ commands, things like a table of
contents, windows with debug messages or a history of the last issued
commands can be accessed using the side panes. They can be enabled using the
"View" menu. They all can be moved to other locations inside or outside the
_wxMaxima_ window. Other useful panes is the one that allows to input Greek
letters using the mouse.

![各种类型的侧边栏](./SidePanes.png){ id=img_SidePanes }

In the "table of contents" side pane, one can increase or decrease a heading
by just clicking on the heading with the right mouse button and select the
next higher or lower heading type.

![Increase or decrease headings in the TOC side
pane](./Sidepane-TOC-convert-headings.png){ id=Sidepane-TOC-convert-headings
}

### MathML output

Several word processors and similar programs either recognize
[MathML](https://www.w3.org/Math/) input and automatically insert it as an
editable 2D equation - or (like LibreOffice) have an equation editor that
offers an “import MathML from clipboard” feature. Others support RTF
maths. _WxMaxima_, therefore, offers several entries in the right-click
menu.

### Markdown support

_WxMaxima_ offers a set of standard
[Markdown](https://en.wikipedia.org/wiki/Markdown) conventions that don’t
collide with mathematical notation. One of these elements is bullet lists.

```
Ordinary text
 * One item, indentation level 1
 * Another item at indentation level 1
   * An item at a second indentation level
   * A second item at the second indentation level
 * A third item at the first indentation level
Ordinary text
```

_WxMaxima_ will recognize text starting with `>` chars as block quotes:

``` Ordinary text > quote quote quote quote > quote quote quote quote >
quote quote quote quote Ordinary text ```

_WxMaxima_’s TeX and HTML output will also recognize `=>` and replace it by
the corresponding Unicode sign:

``` cogito => sum.  ```

Other symbols the HTML and TeX export will recognize are `<=` and `>=` for
comparisons, a double-pointed double arrow (`<=>`), single-headed arrows
(`<->`, `->` and `<-`) and `+/-` as the respective sign. For TeX output also
`<<` and `>>` are recognized.

### Hotkeys

大多数快捷键可以在相应菜单的文本中找到。由于它们实际上是从菜单文本中获取的，所以可以通过 wxMaxima
的翻译文本进行定制，以满足本地用户的需求，因此我们不在这里记录它们。但是，一些快捷键或其别名并没有记录在菜单中：

- <kbd>CTRL</kbd>+<kbd>SHIFT</kbd>+<kbd>DELETE</kbd> 删除一个单元格。
- <kbd>CTRL</kbd>+<kbd>TAB</kbd> or
  <kbd>CTRL</kbd>+<kbd>SHIFT</kbd>+<kbd>TAB</kbd> triggers the
  auto-completion mechanism.
- <kbd>SHIFT</kbd>+<kbd>SPACE</kbd> 插入一个不可断行的空格。

### Raw TeX in the TeX export

如果文本单元格以 `TeX: ` 开头，则导出 TeX 时，文件将包含 `TeX: ` 标记后面的文本。使用此功能可以在 wxMaxima 工作簿中输入
TeX 代码。

## File Formats

在 wxMaxima 中输入的内容可以存为以下三种文件格式之一，以供以后使用：

### .mac

`.mac` files are ordinary text files that contain _Maxima_ commands. They
can be read using _Maxima_’s `batch()` or `load()` command or _wxMaxima_’s
File/Batch File menu entry.

One example is shown below. `Quadratic.mac` defines a function and afterward
generates a plot with `wxdraw2d()`.  Afterward the contents of the file
`Quadratic.mac` are printed and newly defined function `f()` is evaluated.

![Loading a file with `batch()`](./BatchImage.png){ id=img_BatchImage }

Attention: Although the file `Quadratic.mac` has a usual _Maxima_ extension
(`.mac`), it can only be read by _wxMaxima_, since the command `wxdraw2d()`
is a wxMaxima-extension to _Maxima_. (Command line) Maxima will ignore the
unknown command `wxdraw2d()` and print it as output again.

You can be use `.mac` files for writing your own library of macros. But
since they don’t contain enough structural information they cannot be read
back as a _wxMaxima_ session.

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

#### File format of wxm files

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

这种基于 XML 的文件格式保存了完整的工作簿内容，包括缩放因子和观察列表等内容。它是首选的文件格式。

#### File format of wxmx files

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

## Configuration options

对于一些常见的可设置变量，wxMaxima提供了两种设置方法：

- 下面的设置对话框允许用户更改当前和后续会话的默认值。
- Also, the values for most configuration variables can be changed for the
  current session only by overwriting their values from the worksheet, as
  shown below.

![wxMaxima 设置对话框](./wxMaxima_configuration_001.png){
id=img_wxMaxima_configuration_001 }

### Default animation framerate

用于新生成动图的帧率保存在变量 `wxanimate_framerate` 中，可以使用设置对话框更改此变量在新工作簿中的初始值。

### Default plot size for new _maxima_ sessions

After the next start, plots embedded into the worksheet will be created with
this size if the value of `wxplot_size` isn’t changed by _maxima_.

In order to set the plot size of a single graph only use the following
notation can be used that sets a variable’s value for one command only:

```maxima
wxdraw2d(
   explicit(
       x^2,
       x,-5,5
   )
), wxplot_size=[480,480]$
```

### Match parenthesis in text controls

该选项使以下两个功能生效：

- If an opening parenthesis, bracket, or double quote is entered _wxMaxima_
  will insert a closing one after it.
- 如果在输入前述字符中的任何一个时选择了文本，则所选文本将被放在配对的两个字符之间。

### Don’t save the worksheet automatically

If this option is set, the file where the worksheet is will be overwritten
only the request of the user. In case of a crash/power loss/... a recent
backup copy is still made available in the temp directory, though.

If this option isn’t set _wxMaxima_ behaves more like a modern cellphone
app:

- 文件在退出时被自动保存；
- And the file will automatically be saved every 3 minutes.

### Where is the configuration saved?

If you are using Unix/Linux, the configuration information will be saved in
a file `.wxMaxima` in your home directory (if you are using wxWidgets \<
3.1.1), or `.config/wxMaxima.conf` ((XDG-Standard) if wxWidgets >= 3.1.1 is
used). You can retrieve the wxWidgets version from the command
`wxbuild_info();` or by using the menu option
Help->About. [wxWidgets](https://www.wxwidgets.org/) is the cross-platform
GUI library, which is the base for _wxMaxima_ (therefore the `wx` in the
name).  (Since the filename starts with a dot, `.wxMaxima` or `.config` will
be hidden).

当使用 Windows 系统时，配置信息将存储在注册表中。在注册表中的以下位置可以找到 wxMaxima
的相关条目：`HKEY_CURRENT_USER\Software\wxMaxima`。

______________________________________________________________________

# Extensions to _Maxima_

_WxMaxima_ is primarily a graphical user interface for _Maxima_. As such,
its main purpose is to pass along commands to _Maxima_ and to report the
results of executing those commands. In some cases, however, _wxMaxima_ adds
functionality to _Maxima_. _WxMaxima_’s ability to generate reports by
exporting a workbook’s contents to HTML and LaTeX files has been
mentioned. This section considers some ways that _wxMaxima_ enhances the
inclusion of graphics in a session.

## Subscripted variables

`wxsubscripts` specifies, if (and how) _wxMaxima_ will autosubscript
variable names:

If it is `false`, the functionality is off, wxMaxima will not autosubscript
part of variable names after an underscore.

If it is set to `'all`, everything after an underscore will be subscripted.

If it is set to `true` variable names of the format `x_y` are displayed
using a subscript if

- Either `x` or `y` is a single letter or
- `y` is an integer (can include more than one character).

![How variables are autosubscripted using wxsubscripts](./wxsubscripts.png){
id=img_wxsubscripts }

If the variable name doesn’t match these requirements, it can still be
declared as "to be subscripted" using the command
`wxdeclare_subscript(variable_name);` or
`wxdeclare_subscript([variable_name1,variable_name2,...]);` Declaring a
variable as subscripted can be reverted using the following command:
`wxdeclare_subscript(variable_name,false);`

You can use the menu "View->Autosubscript" to set these values.

## User feedback in the status bar

Long-running commands can provide user feedback in the status bar. This user
feedback is replaced by any new feedback that is placed there (allowing to
use it as a progress indicator) and is deleted as soon as the current
command sent to _Maxima_ is finished. It is safe to use `wxstatusbar()` even
in libraries that might be used with plain _Maxima_ (as opposed to
_wxMaxima_): If _wxMaxima_ isn’t present the `wxstatusbar()` command will
just be left unevaluated.

```maxima
for i:1 thru 10 do (
    /* Tell the user how far we got */
    wxstatusbar(concat("Pass ",i)),
    /* (sleep n) is a Lisp function, which can be used */
    /* with the character "?" before. It delays the */
    /* program execution (here: for 3 seconds) */
    ?sleep(3)
)$
```

## Plotting

因为绘图功能与图形密切相关，所以图形用户界面理应为原始程序的绘图功能提供一些扩充内容。

### Embedding a plot into the worksheet

_Maxima_ normally instructs the external program _Gnuplot_ to open a
separate window for every diagram it creates. Since many times it is
convenient to embed graphs into the worksheet instead _wxMaxima_ provides
its own set of plot functions that don’t differ from the corresponding
_maxima_ functions save in their name: They are all prefixed by a “wx”.

The following plotting functions have wx-counterparts:

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

### Making embedded plots bigger or smaller

As noted above, the configure dialog provides a way to change the default
size plots created which sets the starting value of `wxplot_size`. The
plotting routines of _wxMaxima_ respect this variable that specifies the
size of a plot in pixels. It can always be queried or used to set the size
of the following plots:

```maxima
wxplot_size:[1200,800]$
wxdraw2d(
    explicit(
        sin(x),
        x,1,10
    )
)$
```

如果只更改一个绘制图片的大小，wxMaxima 提供了一种只为当前单元格更改属性的方法。具体用法为，将
`wxplot_size=[value1,value2]` 附加到 `wxdraw2d()` 命令结尾，而不是作为该命令的一部分：

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

### Better quality plots

_Gnuplot_ doesn’t seem to provide a portable way of determining whether it
supports the high-quality bitmap output that the Cairo library provides. On
systems where _Gnuplot_ is compiled to use this library the pngCairo option
from the configuration menu (that can be overridden by the variable
`wxplot_pngcairo`) enables support for antialiasing and additional line
styles. If `wxplot_pngCairo` is set without _Gnuplot_ supporting this the
result will be error messages instead of graphics.

### Opening embedded plots in interactive _Gnuplot_ windows

If a plot was generated using the `wxdraw`-type commands (`wxplot2d` and
`wxplot3d` isn’t supported by this feature) and the file size of the
underlying _Gnuplot_ project isn’t way too high _wxMaxima_ offers a
right-click menu that allows to open the plot in an interactive _Gnuplot_
window.

### Opening Gnuplot’s command console in `plot` windows

On MS Windows, there are two Gnuplot programs, `gnuplot.exe` and
`wgnuplot.exe`.  You can configure, which command should be used using the
configuration menu. `wgnuplot.exe` offers the possibility to open a console
window, where _gnuplot_ commands can be entered into, `gnuplot.exe` does not
offer this possibility. Unfortunately, `wgnuplot.exe` causes _Gnuplot_ to
"steal" the keyboard focus for a short time every time a plot is prepared.

### Embedding animations into the spreadsheet

3D diagrams tend to make it hard to read quantitative data. A viable
alternative might be to assign the 3rd parameter to the mouse wheel. The
`with_slider_draw` command is a version of `wxdraw2d` that does prepare
multiple plots and allows to switch between them by moving the slider on top
of the screen. _WxMaxima_ allows to export this animation as an animated
gif.

`with_slider_draw` 命令的前两个参数是用于控制绘图的变量名称和变量值的列表，接下来的参数与 `wxdraw2d` 命令的参数相同：

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

对三维绘图而言，类似的功能由命令 `with_slider_draw3d` 提供，可用参数来旋转三维图形：

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

如果重要的是图的大致形状，那么只要稍微移动一点，就可以直观地看出它的三维特征：

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

For those more familiar with `plot` than with `draw`, there is a second set
of functions:

- `with_slider` and
- `wxanimate`。

Normally the animations are played back or exported with the frame rate
chosen in the configuration of _wxMaxima_. To set the speed at an individual
animation is played back the variable `wxanimate_framerate` can be used:

```maxima
wxanimate(a, 10,
    sin(a*x), [x,-5,5]), wxanimate_framerate=6$
```

The animation functions use _Maxima_’s `makelist` command and therefore
share the pitfall that the slider variable’s value is substituted into the
expression only if the variable is directly visible in the
expression. Therefore the following example will fail:

```maxima
f:sin(a*x);
with_slider_draw(
    a,makelist(i/2,i,1,10),
    title=concat("a=",float(a)),
    grid=true,
    explicit(f,x,0,10)
)$
```

If _Maxima_ is explicitly asked to substitute the slider’s value plotting
works fine instead:

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

### Opening multiple plots in contemporaneous windows

While not being provided by _wxMaxima_ this feature of _Maxima_ (on setups
that support it) sometimes comes in handily. The following example comes
from a post from Mario Rodriguez to the _Maxima_ mailing list:

```maxima load(draw);

/* Parabola in window #1 */ draw2d(terminal=[wxt,1],explicit(x^2,x,-1,1));

/* Parabola in window #2 */ draw2d(terminal=[wxt,2],explicit(x^2,x,-1,1));

/* Paraboloid in window #3 */
draw3d(terminal=[wxt,3],explicit(x^2+y^2,x,-1,1,y,-1,1)); ```

Plotting multiple plots in the same window is possible, too (the same is
possible in command line Maxima with the standard `draw()` command):

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

### The "Plot using draw" side pane

The "Plot using draw" sidebar hides a simple code generator that allows
generating scenes that make use of some of the flexibility of the _draw_
package _maxima_ comes with.

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

#### Expression

在光标当前所在的 `draw()` 命令中添加绘图函数表达式，如 `sin(x)`, `x*sin(x)` 或者 `x^2+2*x-4` 等
。每个图中可以绘制任意数量的图形。如果没有 `draw()` 命令，将调用绘制二维图形的模板。

#### Implicit plot

尝试寻找满足 `y=sin(x)`，`y*sin(x)=3` 或 `x^2+y^2=4` 之类的表达式的所有解，并在光标当前所在的 `draw()`
命令中绘制对应曲线。如果没有 `draw()` 命令，将调用绘制二维图形的模板。

#### Parametric plot

将变量（如 t）从下限变化到上限，并在当前绘图命令中使用两个表达式（如 `t*sin(t)` 和 `t*cos(t)`），以生成曲线上点的横、纵坐标
(x,y)（在三维图形中，第三个表达式生成竖坐标 z）。

#### Points

绘制多个点，并且可以选择顺次连接它们。点的坐标取自列表的列表、二维数组或为每个轴指定的一个列表或数组。

#### Diagram title

在图形上方绘制标题。

#### Axis

设置坐标轴。

#### Contour

(Only for 3D plots): Adds contour lines similar to the ones one can find in
a map of a mountain to the plot commands that follow in the current `draw()`
command and/or to the ground plane of the diagram. Alternatively, this
wizard allows skipping drawing the curves entirely only showing the contour
plot.

#### Plot name

Adds a legend entry showing the next plot’s name to the legend of the
diagram. An empty name disables generating legend entries for the following
plots.

#### Line colour

为当前 `draw()` 命令所绘图形设置线条颜色。

#### Fill colour

为当前 `draw()` 命令所绘图形设置填充颜色。

#### Grid

弹出设置网格线的向导。

#### Accuracy

允许选择适当数目的点，以兼顾速度与精度（任何绘图程序都会权衡两者）。

### Modify font and font size for plots

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

## Embedding graphics

If the `.wxmx` file format is being used embedding files in a _wxMaxima_
project can be done as easily as per drag-and-drop. But sometimes (for
example if an image’s contents might change later on in a session) it is
better to tell the file to load the image on evaluation:

```maxima show_image("man.png"); ```

## Startup files

在 wxMaxima 的设置对话框中可以编辑两个文件的命令，这些命令在启动时被执行：

- 文件 `maxima-init.mac` 中包含每次启动 Maxima 时被执行的命令；
- one file of additional commands that are executed if _wxMaxima_ is
  starting _Maxima_: `wxmaxima-init.mac`

For example, if Gnuplot is installed in `/opt` (maybe on MacOS), you can add
`gnuplot_command:"/opt/local/bin/gnuplot"$` (or `/opt/gnuplot/bin/gnuplot`
or any other path) to these files.

These files are in the Maxima user directory (usually `%USERPROFILE%/maxima`
in Windows, `$HOME/.maxima` otherwise). The location can be found out with
the command: `maxima_userdir;`

## Special variables wx...

- `wxsubscripts` ：告诉 Maxima 是否应将包含下划线的变量名（例如
  `R_150`）转换为带下标的变量名。有关自动转换哪些变量名的详细信息，请参见 `wxdeclare_subscript`。
- `wxfilename`：此变量包含当前在 wxMaxima 中打开的文件的名称。
- `wxdirname`: This variable contains the name the directory, in which the
  file currently opened in _wxMaxima_ is.
- `wxplot_pngcairo` tells whether _wxMaxima_ tries to use _Gnuplot_’s
  pngcairo terminal that provides more line styles and a better overall
  graphics quality.
- `wxplot_size`：定义嵌入绘图的分辨率。
- `wxchangedir`: On most operating systems _wxMaxima_ automatically sets
  _Maxima_’s working directory to the directory of the current file. This
  allows file I/O (e.g. by `read_matrix`) to work without specifying the
  whole path to the file that has to be read or written. On Windows this
  feature sometimes causes error messages and therefore can be set to
  `false` from the config dialogue.
- `wxanimate_framerate`：下一个动图回放的帧率。
- `wxanimate_autoplay`：是否在默认情况下自动播放动图。
- `wxmaximaversion`: Returns the version number of _wxMaxima_.
- `wxwidgetsversion`: Returns the wxWidgets version _wxMaxima_ is using.

## Pretty-printing 2D output

The function `table_form()` displays a 2D list in a form that is more
readable than the output from _Maxima_’s default output routine. The input
is a list of one or more lists. Like the "print" command, this command
displays output even when ended with a dollar sign. Ending the command with
a semicolon results in the same table along with a "done" statement.

```maxima
table_form(
    [
        [1,2],
        [3,4]
    ]
)$
```

正如下一个例子所示，可以在执行 `table_form` 命令之前创建需要显示的列表。

![一个三行表的例子](./MatrixTableExample.png){ id=img_MatrixTableExample }

此外，由于矩阵是含列表的列表，它也可以用类似的方式转换为表格。

![另一个 table_form 的例子](./SecondTableExample.png){ id=img_SecondTableExample }

## Bug reporting

_WxMaxima_ provides a few functions that gather bug reporting information
about the current system:

- `wxbuild_info()` gathers information about the currently running version
  of _wxMaxima_
- `wxbug_report()` 说明如何以及在何处提交错误报告。


## Marking output being drawn in red

_Maxima_’s `box()` command causes _wxMaxima_ to print its argument with a
red foreground, if the second argument to the command is the text
`highlight`.

## Output rendering.

With `set_display()` one can set, how wxMaxima will render the output.

`set_display('xml)` is the default value. Here Maxima speaks to wxMaxima
using an (machine readable) XML-dialect (can be seen in the "Raw XML
sidebar") and outputs the resulting formulas nicely rendered, e.g. pretty
Matrices, Square root signs, fractions, etc.

<!--- Currently that does not work as it should, the line with the output
label is shifted right (issue: #2006) --> `set_display('ascii)` causes
wxMaxima to output formulas as in command line Maxima - as ASCII-Art.

`set_display('none)` causes 'one-line' ASCII results - the same as the
command line Maxima command `display2d:false;` does.

# Help menu

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

# Troubleshooting

## Cannot connect to _Maxima_

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

On Unix computers another possible reason would be that the loopback network
that provides network connections between two programs in the same computer
isn’t properly configured.

## How to save data from a broken .wxmx file

Internally most modern XML-based formats are ordinary zip files. _WxMaxima_
doesn’t turn on compression, so the contents of `.wxmx` files can be viewed
in any text editor.

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

If a text file containing only these contents (e.g. copy and paste this text
into a new file) is saved as a file ending in `.xml`, _wxMaxima_ will know
how to recover the text from the document.

## I want some debug info to be displayed on the screen before my command
has finished

通常 wxMaxima 会等待最后的公式形成后才开始输出，这节省了多次尝试输出部分公式的时间。不过， `disp()`
命令可以立即输出调试信息，而不必等待当前的 Maxima 命令完成：

```maxima
for i:1 thru 10 do (
   disp(i),
   /* (sleep n) is a Lisp function, which can be used */
   /* with the character "?" before. It delays the */
   /* program execution (here: for 3 seconds) */
   ?sleep(3)
)$
```

Alternatively one can look for the `wxstatusbar()` command above.

## Plotting only shows a closed empty envelope with an error message

This means that _wxMaxima_ could not read the file _Maxima_ that was
supposed to instruct _Gnuplot_ to create.

这个错误的可能原因如下：

- The plotting command is part of a third-party package like `implicit_plot`
  but this package was not loaded by _Maxima_’s `load()` command before
  trying to plot.
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
- Gnuplot didn’t output a valid `.png` file.

## Plotting an animation results in “error: undefined variable”

The value of the slider variable by default is only substituted into the
expression that is to be plotted if it is visible there. Using a `subst`
command that substitutes the slider variable into the equation to plot
resolves this problem. At the end of section [Embedding animations into the
spreadsheet](#embedding-animations-into-the-spreadsheet), you can see an
example.

## I lost cell content and undo doesn’t remember

对单元格的操作和单元格内部的更改均有单独的撤消函数，因此发生这种情况的可能性很低。如果确实是这样，有如下方法可以恢复数据：

- _WxMaxima_ actually has two undo features: The global undo buffer that is
  active if no cell is selected and a per-cell undo buffer that is active if
  the cursor is inside a cell. It is worth trying to use both undo options
  in order to see if an old value can still be accessed.
- If you still have a way to find out what label _Maxima_ has assigned to
  the cell just type in the cell’s label and its contents will reappear.
- If you don’t: Don’t panic. In the “View” menu there is a way to show a
  history pane that shows all _Maxima_ commands that have been issued
  recently.
- 如果以上都没有帮助，可以调用 Maxima 的回放功能：

```maxima playback(); ```

## _WxMaxima_ starts up with the message “Maxima process terminated.”

One possible reason is that _Maxima_ cannot be found in the location that is
set in the “Maxima” tab of _wxMaxima_’s configuration dialog and therefore
won’t run at all. Setting the path to a working _Maxima_ binary should fix
this problem.

## Maxima is forever calculating and not responding to input

It is theoretically possible that _wxMaxima_ doesn’t realize that _Maxima_
has finished calculating and therefore never gets informed it can send new
data to _Maxima_. If this is the case “Trigger evaluation” might
resynchronize the two programs.

## My SBCL-based _Maxima_ runs out of memory

The Lisp compiler SBCL by default comes with a memory limit that allows it
to run even on low-end computers. When compiling a big software package like
Lapack or dealing with extremely big lists of equations this limit might be
too low. In order to extend the limits, SBCL can be provided with the
command line parameter `--dynamic-space-size` that tells SBCL how many
megabytes it should reserve. A 32bit Windows-SBCL can reserve up to 999
Megabytes. A 64-bit SBCL version running on Windows can be instructed to use
more than the about 1280 Megabytes compiling Lapack needs.

One way to provide _Maxima_ (and thus SBCL) with command line parameters is
the "Additional parameters for Maxima" field of _wxMaxima_’s configuration
dialogue.

![SBCL 内存](./sbclMemory.png){ id=img_sbclMemory }

## Input sometimes is sluggish/ignoring keys on Ubuntu

安装软件包 `ibus-gtk` 应该可以解决此问题. 详情见
[https://bugs.launchpad.net/ubuntu/+source/wxwidgets3.0/+bug/1421558](https://bugs.launchpad.net/ubuntu/+source/wxwidgets3.0/+bug/1421558)。

## _WxMaxima_ halts when _Maxima_ processes Greek characters or Umlauts

如果所使用的 Maxima 基于SBCL，则必须将以下行添加到 `.sbclrc` 文件中：

```commonlisp (setf sb-impl::*default-external-format* :utf-8)  ```

The folder where this file has to be placed is system- and
installation-specific. But any SBCL-based _Maxima_ that already has
evaluated a cell in the current session will happily tell where it can be
found after getting the following command:

``` :lisp (sb-impl::userinit-pathname)  ```

## Note concerning Wayland (recent Linux/BSD distributions)

There seem to be issues with the Wayland Display Server and wxWidgets.
WxMaxima may be affected, e.g. that sidebars are not moveable.

You can either disable Wayland and use X11 instead (globally)  or just tell,
that wxMaxima should use the X Window System by setting: `GDK_BACKEND=x11`

E.g. start wxMaxima with:

`GDK_BACKEND=x11 wxmaxima`

## Why is the integrated manual browser not offered on my Windows PC?

Either wxWidgets wasn’t compiled with support for Microsoft’s webview2 or
Microsoft’s webview2 isn’t installed.

## Why is the external manual browser not working on my Linux box?

The HTML browser might be a snap, flatpack or appimage version. All of these
typically cannot access files that are installed on your local
system. Another reason might be that maxima or wxMaxima is installed as a
snap, flatpack or something else that doesn’t give the host system access to
its contents. A third reason might be that the maxima HTML manual isn’t
installed and the online one cannot be accessed.

## Can I make _wxMaxima_ output both image files and embedded plots at once?

The worksheet embeds `.png files. _WxMaxima_ allows the user to specify
where they should be generated:

```maxima
wxdraw2d(
    file_name="test",  /* extension .png automatically added */
    explicit(sin(x),x,1,10)
);
```

If a different format is to be used, it is easier to generate the images and
then import them into the worksheet again:

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

## Can I set the aspect ratio of an embedded plot?

Use the variable `wxplot_size`:

```maxima
wxdraw2d(
    explicit(sin(x),x,1,10)
),wxplot_size=[1000,1000];
```

## After upgrading to MacOS 13.1 plot and/or draw commands output error
messages like

``` 1 HIToolbox 0x00007ff80cd91726
_ZN15MenuBarInstance22EnsureAutoShowObserverEv + 102 2 HIToolbox
0x00007ff80cd912b8 _ZN15MenuBarInstance14EnableAutoShowEv + 52 3 HIToolbox
0x00007ff80cd35908 SetMenuBarObscured + 408 ...  ```

This might be an issue with the operating system. Disable the hiding of the
menu bar (SystemSettings => Desktop & Dock => Menu Bar) might solve the
issue. See [wxMaxima issue
#1746](https://github.com/wxMaxima-developers/wxmaxima/issues/1746) for more
information.

## Logging

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

## Is there a way to make more text fit on a LaTeX page?

Yes. Use the [LaTeX package "geometry"](https://ctan.org/pkg/geometry) to
specify the size of the borders.

You can add the following line to the LaTeX preamble (for example by using
the respective field in the config dialogue ("Export"->"Additional lines for
the TeX preamble"), to set borders of 1cm):

```latex \usepackage[left=1cm,right=1cm,top=1cm,bottom=1cm]{geometry} ```

## Is there a dark mode?

If wxWidgets is new enough, _wxMaxima_ will automatically be in dark mode if
the rest of the operating system is. The worksheet itself is by default
equipped with a bright background. But it can be configured
otherwise. Alternatively, there is a `View/Invert worksheet brightness` menu
entry that allows to quickly convert the worksheet from dark to bright and
vice versa.

## _WxMaxima_ sometimes hangs for several seconds once in the first minute

_WxMaxima_ delegates some big tasks like parsing _Maxima_’s
>1000-page-manual to background tasks, which normally goes totally
unnoticed. At the moment the result of such a task is needed, though, it is
possible that _wxMaxima_ needs to wait a couple of seconds before it can
continue its work.

## Especially when testing new locale settings, a message box "locale
’xx_YY’ can not be set" occurs

![Locale warning](./locale-warning.png){ id=img_locale_warning}

(The same problem can occur with other applications too). The translations
seem okay after you click on ’OK’. WxMaxima does not only use its own
translations but the translations of the wxWidgets framework too.

These locales maybe not present in the system. On Ubuntu/Debian systems they
can be generated using: `dpkg-reconfigure locales`

## How can I use symbols for real numbers, natural numbers (ℝ, ℕ), etc.?

You can find these symbols in the Unicode sidebar (search for ’double-struck
capital’). But the selected font must also support these symbols. If they do
not display properly, select another font.

## How can a Maxima script determine, if it is running under wxMaxima or
command line Maxima?

If wxMaxima is used, the Maxima variable `maxima_frontend` is set to
`wxmaxima`. The Maxima variable `maxima_frontend_version` contains the
wxMaxima version in this case.

If no frontend is used (you are using command line Maxima), these variables
are `false`.

______________________________________________________________________

# Command-line arguments

Usually you can start programs with a graphical user interface just by
clicking on a desktop icon or desktop menu entry. WxMaxima - if started from
the command line - still provides some command-line switches, though.

- `-v` 或 `--version`：输出版本信息。
- `-h` 或 `--help`: 输出简短的帮助文本。
- `-o` or `--open=<str>`: Open the filename given as an argument to this
  command-line switch
- `-e` 或 `--eval`：打开文件后对公式单元格求值。
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

Instead of a minus, some operating systems might use a dash in front of the
command-line switches.

______________________________________________________________________

# About the program, contributing to wxMaxima

wxMaxima is mainly developed using the programming language C++ using the
[wxWidgets framework](https://www.wxwidgets.org), as build system we use
[CMake](https://www.cmake.org), a small part is written in Lisp. You can
contribute to wxMaxima, join the wxMaxima project at
<https://github.com/wxMaxima-developers/wxmaxima>, if you have knowledge of
these programming languages and want to help and contribute to the open
source project wxMaxima.

But not only programmers are necessary! You can also contribute to wxMaxima,
if you help to improve the documentation, find and report bugs (and maybe
bugfixes), suggest new features, help to translate wxMaxima or the manual to
your language (read the README.md in the [locale
subdirectory](https://github.com/wxMaxima-developers/wxmaxima/tree/main/locales)
how wxMaxima and the manual can be translated).

Or answer questions of other users in the discussion forum.

The source code of wxMaxima is documented using Doxygen
[here](https://wxmaxima-developers.github.io/wxmaxima/Doxygen-documentation/).

The program is nearly self-contained, so except for system libraries (and
the wxWidgets library), no external dependencies (like graphic files or the
Lisp part (the `wxmathML.lisp`-file) is needed, these files are included in
the executable.

If you are a developer, you might want to try out a modified
`wxmathML.lisp`-file without recompiling everything, one can use the command
line option `--wxmathml-lisp=<str>` to use another Lisp file, not the
included one.
