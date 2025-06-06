# wxMaxima                         {#mainpage}

## What is this file about?

This is the documentation for the C++ code of wxMaxima. If you are looking
on information how to use the program instead you might want to consult
the online manual instead that is shipped with the program and
(after compilation of the program) can be found at
[info/wxmaxima.html](../../info/wxmaxima.html) in the directory the
contents of the tarball or the git repository the code is in.

## What is wxMaxima?

wxMaxima is a full-featured graphical frontend for maxima, a full-featured
computer algebra system that will do numeric calculations, if one wants to.
But it is specialized in manipulating and solving symbolic equations.

\image html wxMaxima.gif "A random screenshot of wxMaxima"

## How does wxMaxima work?

The main things that are important to know are:

- Maxima is a command-line program that can use a network connection in order
  to communicate with a frontend.
- In data/wxmathml.lisp wxMaxima teaches Maxima to talk in a XML dialect:
  Maxima's normal output format is human-readable. But it can be tricked into
  containing strings that look like input or output labels and it doesn't
  guarantee that there is a way to transform output from Maxima into a valid input
  again that has exactly the same meaning.
- The worksheet is defined in the class Worksheet.
- It is organized in GroupCells that each can contain a cell containing a list of
  cells containing a label and the user input and a list of cells containing the
  output label and 2d math from maxima.
- All cells contain pointers so they can be used as a part of a double-linked list
  for the logical order they appear in and a second double-linked list that tells
  which cell to draw next. The latter is needed for handling the fact that some
  things (like fractions) can be displayed as 2D maths and in a more linear way.
- For every mathematical function, image or piece of text a Cell there is a specialized
  MathCell type that "knows" how to draw it, how to convert it to a string or how to
  convert it to Mathml, OOML, RTF or any other data format wxMaxima supports.
  There is, for example, an AbsCell for the <code>abs()</code> command, an IntCell
  representing <code>integrate</code>, a ParenCell for parenthesis and a FunCell for
  all the functions no special handling is needed for.
- Configuration is something like a central object keeping the configuration needed for
  displaying cells.

## How to get documentation for the code?

- Install [Doxygen](https://www.doxygen.nl),

- Optionally install [GraphViz](https://www.graphviz.org)
  (it generates call graphs, inheritance graphs and caller graphs
  so it really makes the documentation more usable).

- Make sure these two utilities are in the search path of your
  system.

- Configure wxMaxima using `cmake` (see Compiling.md) and build
  the Doxygen sources using

  `cmake --build . -- Doxygen`

  (or `make Doxygen` if you use `make` as build tool).

  afterwards.

## Where to start reading?

This naturally depends on what you want to achieve.

- The worksheet is mostly handled by the class Worksheet
- All the cells that are displayed in the worksheet and all the sub-
  cells that represent individual elements inside one of these cells
  are child objects of MathCell. This object also contains most of
  the logic that allows to connect the individual cells to a list
  as well as the pointers MathCell::m_previous and MathCell::m_next.
- The undo buffer and the conversion of cells into characters for
  drag-and-drop or saving files are handles by the class EditorCell
- The menus are generated by wxMaximaFrame and most of the menu actions
  are handled by the class wxMaxima.

Many of the most important concepts that are important to know are 
displayed in "Things you have to know."

## Things you have to know

- C++ provides the magic that allows to attach objects of all imaginable
  cell types to a list of the type MathCell \*

- MathCell provides a second list (MathCell::m_nextToDraw and
  MathCell::m_previousToDraw that represents the order the objects
  are displayed in: Some things like Fractions and text contained
  between parenthesis might be layouted differently when they contain
  a line break.

- There are several ways child objects from MathCell can form trees:

  - Some objects like sqrt() or a pair of parenthesis might contain
    a list.
  - Super- and subscripts are lists.
  - GroupCells allow to move their children to a separate list that isn't
    displayed (which enables folding of chapters).

- Methods that operate on lists typically start processing the list from
  the element they were called from. This means that they only will work
  as expected (in other words: process the complete list) only if they
  are called for the first element of the list.
  wxMaxima currently makes sure that this requirement is met by treating
  the first element of a list as the list itself.
  Searching the first element of a given list would be simple, though:

  ```
   MathCell *ListStart=this;
   while(ListStart->m_previous) ListStart = ListStart->m_previous;
  ```

## Breaking long 2D objects into lines

wxMaxima can break some 2D objects into lines if they otherwise would be
longer wider than the screen. This is done in GroupCell::BreakLines.

Objects that are broken apart this way technically aren't displayed
any more. Instead the lists of objects they contain are. If an object
isn't broken apart it is displayed directly and automatically handles
displaying all of its contents.

## Naming rules

Keeping the code more or less homogenous increases the readability. In
order to achieve that wxMaxima uses a few naming rules:

- The names of member variables are prefixed with `m_` for "member".
- The names of member functions (aka methods) are written in CamelCase.
- The names of enums do not really matter as they are rarely used and if
  they are they are used in context where it is obvious that they name an
  enum type so there aren't any rules for the names of enums right now.

`WXUNUSED` Tell the C++ compiler that it is OK that one parameter is unused.

```
    #define WXUNUSED(x) x
```
