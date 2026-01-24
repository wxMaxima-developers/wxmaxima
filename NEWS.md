# Current development version

- Fix search and replace. Before in a longer string - e.g. "aaaaaaaaa"
  only every second occurrence was replaced, when (e.g.) one wanted to
  replace "a" by "b".

# 26.01.0

- Keep the wxm/mac difference when saving wxm/exporting mac.
  This omits unneeded comments (/* [wxMaxima: input   start ] */
  (and input end) in a mac file (structural comments (section, ...)
  are still saved) and especially omits question/answer comments -
  they are unreadable by Maxima (see #2014, not only wxm files but
  also mac files were problematic). At least the issue with mac
  files is solved.
- Replace unicode fractions in math input.
- Use wxArtProvider images for the copy/print icons in the config dialogue.
  They were very small on Linux (Ubuntu).
- Support webp images if wxWidgets >= 3.3.0 is used.
- Fix the General Math sidebar resize issue (#2022).
- Fix the resizing of the 'Insert cell' sidebar (same problem as
  the 'General Math' sidebar).
- MacOS: Fix debug message boxes after closing a file (#2016).
- Fix build with current Mac XCode ('old'/new' build system).
  (reported in a comment in issue #2016).
- Fix a crash when copying a input cell (collapsed to only show the input).
- Fix a crash when closing the configuration menu on OpenBSD (#2027).
- Fix a problem with wxplot2d/wxplot3d on OpenBSD (#2027).
- Fix a crash when deleting a text cell (#2041).
- Fix subscripts of greek letters (#2045).
- Fix the "Numerical solutions of polynomial" (with and without bigfloats) wizards.
  The allroots() and bfallroots() functions use only one argument, not two.
- Save the chosen invertion status in the configuration. Fixes issue #2049.
- Try to highlight input text that coincides with selected output text.
- diff() now doesn't add a ",1" if only the 1st derivate is meant.
- A more consistent worksheet zoom feature.
- Maxima's state should now be displayed in the windows task bar.

# 25.04.0

- Use the (standard) wxWidgets log window instead of our own log pane.
  Less code to maintain, more features (e.g. clear log, save log).
  In "Release" builds the log window will be hidden (and can be enabled
  using View->Toggle log window, otherwise it is shown.
- Make the option "--logtostderr" work on Windows.
  (Windows GUI applications do not have STDIN, STDOUT and STDERR assigned,
  an extra 'text window' with the log messages will be opened at startup).
- Do not set lmxchar/rmxchar (used for displaying a matrix in text mode).
  It did not work at all. Maxima provides a reasonable default - and
  people probably expect the same, if they are using command line Maxima
  and wxMaxima. (#1926)
- Kill Lisp (e.g. sbcl.exe) when closing wxMaxima. (#1963, #1922, #1824, #1199)
- Correct heading colors (#2005)
- Repaired the Equations->Solve numerical->Find numerical solution" menu (#2010)
- Included examples (10MinuteTutorial.wxm, testbench_simple.wxmx) were affected
  by the Maxima/Gnuplot 6 problem. (see issue #1960)
- Support wxWidgets, if it was build with '--enable-utf8=yes' (#2012)
- Fix compiling with wxWidgets without webview.
- HTML export/Mathjax: Don't remove 1st/last label char (#2003)

# 25.01.0

- Remove the change label width popup menu (#1964).
  It was not working at all and caused crashes. And I think the
  label width is not changed so often, so that a (working)
  possibility in the configuration menu is enough.
- If an empty worksheet is saved as wxmx, allow it to be read again (#1978).
  The generated content.xml was empty, which was no valid XML.
- Fix compiling with clang++.
- CopyAsMathML: encode "<" and ">" as HTML entities (valid XML).
- CopyAsMathML: improvements (operators and identifiers)
- Fix the check for Gnuplot-Cairo on Windows.
- Fix the (Windows) crash in the suggestions menu (#1980).
- HTML export: print the output labels on the left side.

# 24.11.0

- A Spanish translation update by cyphra.
- Resolved a crash when inverting the worksheet (#1951)
- Do not strip Lisp comments before sending the input to Maxima (#1953)
- Correctly copy superscript when CopyAsMathML (#1945)
- Copy as MathML: Use a centered dot instead of *, if configured (#1956)
- Fix superfluous quote marks in text copied by "Copy as MathML" (#1946)
- Fix copy as MathML for boxes.
- Correct display/handling of minf.
- WxMaxima was not responsive in the 1st minute.
- Fix display of subscripts. (#1584, #1807)
- Allow Maxima strings with newlines. (#76)
- Support Maxima which uses ABCL.

# 24.08.0

- An advanced plotting tutorial
- Try harder to kill maxima on closing it
- Try harder to clean up maxima's temporary files on closing it
- Resolved a race condition on closing multiple windows
- Resolved a CMake error on configuration with -B switch (#1917)
- More menu icons
- A better dialogue if Maxima isn't found
- Better search for the Maxima binary
- Find Gnuplot on Windows if wxMaxima is packaged alone. This
  is needed for the 'Popout interactively' function of wxdraw().
- Security: Use a standard function (CreateTempFileName) for
  creating a temporary file for the 'popout interactively'
  function of wxdraw.
- Security: Add a random number to lisp generated temporary files.
- Fix the 'Gnuplot command window' (on Windows) (issues #771, #1680 and #1732).
- A Spanish translation update by cyphra
- Fix the greek letter replacement for 'beta' with newer Maxima versions (#1921).
- Fix a problem, when 'maxima-local' (a not installed version of Maxima)
  was chosen as Maxima. WxMaxima did not finish on File->Exit and used
  much CPU power.
- Size calculations for numbers that were broken into lines failed the first time
  (#1931)
- On MS Windows one can now choose between Gnuplot and wGnuplot (#1934)
- Fixed the error message about dual manifest files on MSVC

# 24.05.0

- Faster discarding of maths that is too long to read
- Resolved a crash on closing the last window (#1898)
- Now only wxWidgets builds with Unicode support (default) are allowed.
  Builds without Unicode support are strongly discouraged
  by the wxWidgets team.
- Update German and Italian translations
- Use po4a instead of po4a-translate for the generation of
  internationalized manuals. po4a-translate is deprecated (#1899)
- Allow compilation with the current wxWidgets development version
- Code reorganization to promote stability
- Security: Use a standard function (CreateTempFileName) for creating
  temporary filenames instead of using the process id as 'random' number.
- Security: (autosaved) temporary files now use secure permissions,
  are not readable by everyone.
- wxm files no more truncate text (#1908)
- Unicode sidebar: Allow searching for unicode numbers too, not only the
  character name.
- Unicode sidebar: Correct the search filter, the last char was always
  shown, even if there was no match.
- Improvements of the manual.
- Performance: Replaced many copy operations by references to const
- Correctly handle deletion of the last cell (#1911, #1840)
- A link from the help menu to maxima's help page
- Offer more maxima demos in the right-click menu
- Resolved a threading race condition (#1912)
- Printing integral, sum and product signs on MS Windows (#1909)
- Resolved literally hundreds of warnings from static analysis tools
- Support for the %catalan constant
- Better MSVC support
- On MSW wxWidgets >=3.2 now is mandatory for getting high-DPI support
- A backward-search fix (#1913)

# 24.02.2

- Set maxima's LANG variable, not wxMaxima's LANG (#1897)
- Sped up the communication between maxima and wxMaxima
- Sometimes newlines in misc text from maxima got lost (#1894)

# 24.02.1

- wxMaxima now correctly installs its icons (#1892)
- A race condition on closing Maxima
- Update the autocompletion only in idle state
- Fixed a race condition on dropping the log target
- Alt+Up at startup no more crashes (#1886)
- Resolved a compilation error on old wxWidgets versions
- Resolved GCC errors about too long functions
- Resolved an assert if no history file exists
- Ctrl+Alt+M now restarts maxima
- Added a Maxima/Memory menu

# 24.02.0

- Faster start-up
- Better performance directly after startup
- Faster loading of files
- Better button placement in sidebars
- Got rid of a crash on startup in fedora (#1862)
- Clear wxMaxima's input buffer on starting a new maxima process (#1856)
- A nicer ChangeLog dialogue
- Corrected for swapped row and columns in wizard enter matrix
- Fixed a few bugs in the XML saving code (#1867)
- Zooming did cause recalculation only for the 1st cell (#1870)
- Many config changes now have immediately effect on the worksheet
- Added the unicode-enhanced ASCII art from maxima to the menus
- Steamlined the cell size calculation stuff
- Completely overhauled the printing functionality
- Moved more of the help file indexing to the background (#1856)
- Better event handling in the unicode sidebars (#1875, #1863)
- Reading out the values of maxima variables for the GUI was broken
- Many checkmarks in menus had the wrong value
- Maxima's demos are now available in the menu and context menu
- Default the filter search boxes to text search, not regex
- Rescaling affected size calculations for code only with a delay
- Mac Os: way higher stability if multiple windows are open
- The context menu in the "greek letters" sidebar now works (#1878)
- Resolved an assert if the internal help browser was disabled,
  but requested (#1881)
- wxMaxima now preserves history entries between sessions
- Better LibreOffice compatibility with the MathML output
- Better HTML output
- Fix crash when exporting a worksheet with animations to TeX
- Resolved a crash on closing a window (#1889)

# 23.12.0

- Input text selection was cleared when right-clicking
  on it (#1845)
- Pressing both mouse buttons simultaneously caused an
  assert (#1844)
- Corrected the cursor position after unsuccessful autocompletes
- Corrected the handling of question prompts from maxima (#1827)
- Bug corrections in the search functionality
- RegEx search works again
- Clicking on the notification now is more likely to focus the worksheet
- Corrected the cell folding logic (#1853)
- Folded cells are no more evaluated (#1853)
- Now we try to generate a backtrace on crashes (#1802)
- Corrected the position of integral limits
- Nicer product, sum and integral signs
- Hidden cells weren't restored from wxm files (#1855)
- diff() no longer causes spurious multiplication dots (#1825)

# 23.11.0

- Correctly draw the bracket of the cell under the cursor (#1811)
- Maths display was corrupted if the window was temporarily not wide
  enough to display it in 2D (#1812)
- Added many range checks to the code
- Correct label scaling after changing the zoom factor (#1815)
- Repaired "copy as bitmap" (#1820)
- Repaired EMF and SVG output (#1838)
- Focus the search text on activating the search box (#1821)
- Correctly support dropping multiple files on the worksheet
- If the worksheet isn't empty dropping a file on the worksheet
  no more closes the currently open file
- Try harder to kill maxima on closing it (#1824)
- Menu items for displaying the internal representation
- Better display of diff(), if multiplication dots are enabled (#1825)
- Tried to resolve a few asserts (#1831)
- Resolved many warnings from several static analysis tools
- Fine-tuning of the "undo" function

# 23.10.0

- A big rewrite to make the static analysis tools happy
- Many stability improvements
- Maxima's help commands work again out-of-the-box.
- Corrected the worksheet size calculation
- Better support for loading .wxmx files from weird paths
- Finally resolved the "Right-click resets zoom" problem (#1810)
- A speed up
- Many right-click menu fixes (#1810)
- Display/Hide label fixes
- Many help browser improvements (#1802)
- The worksheet size no more is underestimated (#1802)
- Cell creation fixes
- Copy-and-paste improvements (#1809)
- Better error messages for bitmaps
- Many additional bug fixes

# 23.08.0

- Better print scaling
- More consistent page size handling for printing
- Many performance and stability fixes from coverity-scan
- Resolved many warnings from cppcheck
- A RegEx search

# 23.07.0

- Printing: Made the page margins configurable (#1787)
- Printing: We no more print page headers (#1791)
- Printing: Printing no more omits labels (#1507)
- Printing: A better pagebreak algorithm
- Printing: No more ghost lines around text cells
- Less ugly integral signs
- Many stability fixes
- More consistent worksheet scaling and sizes (#1780)
- Correctly save the color settings (#1789)
- Correctly check the numeric and other menu items
- The cursor no more disappears (#1788)
- Updated italian translation

# 23.05.1

- Updated the Russian and Italian translation.
- Process the command line option `--maxima=<str>`.
- Resolved several possibilities to crash the config dialogue
  (#1774, #1766)
- Correctly calculate the worksheet size in the background (#1766)
- Correctly restore parenthesis on load (#1779)
- Always revise the decision what to display as 2D equation on
  window width changes
- 2D display of integrals had zero size (#1780)

# 23.05.0

- Got rid of some compiler and cppcheck warnings
- Big performance improvements
- A "maxima versus typical programming languages" tutorial.
- Table of contents: Jump to the chapter the cursor is in.
- Resolved another crash in the config dialogue
- Updated the unicode character list
- Resolved more potential event ID clashes
- Use an external file as wxMathML.lisp is now selected using
  a command line option, not configured in the GUI.
  This is mostly needed for developers, no need to expose it
  to the end user.
- Better handling of paths to maxima when starting new
  wxMaxima processes
- new wxMaxima processes now get more of the command-line args
  of the parent process
- wxMaxima no more tries to install its own fonts.

# 23.04.1

- More config dialogue tweaks
- MacOs: No more create config icons that aren't needed
- Recent file list: Don't try to re-use no-more-used wxWidgets IDs
- Make sure that the help sidebar is shown when opening a help topic
- If maxima wants us to open a help browser and wxMaxima was compiled
  without one it now opens an external help browser
- Be aware that the MSW port recycles unused window IDs.
- The load() command now again works with symbols as package names.

# 23.04.0

- Made the GUI more responsive for functions with much output
- Handle errors in the XML from maxima more gracefully
- Rewrote parts of the lisp part of wxMaxima
- Resolved a few lisp warnings
- Resolved a few XML errors caused by not escaping special chars
- Automatic XML generation tests
- More MacOs fixes
- Correct display of operators
- A better style sample for the config dialogue
- Resolved several asserts
- Resolved a few small bugs in the autosave logic

# 23.03.0

- Make sure all bitmaps are valid on High-DPI MacOs (#1749)
- More styles consistency (#1753)
- Better configuration validation
- Depending on the wxWidgets version Unicode letters were
  interpreted inorrectly (#1754)
- The manual anchors cache now saves the file-per-chapter URLs, too.
- cmake -DWXM_DISABLE_WEBVIEW now allows to find wxWidgets, if
  wxWebView wasn't compiled.
- Completely rewrote the dockable-sidebars-stuff
- Many files are now build on demand, not at configure time

# 23.02.0

- Fix XML generation of wxmx documents (#1556)
- A faster font cache
- Faster communication between wxMaxima and Maxima
- Dropped the dependency on an \[internal\] ww898 library
- More consistent text style handling
- A try to improve locale management on the Mac platform
- A separate style for operators (#1002)
- A nicer ChangeLog

# 22.12.0

- A cleaner status bar with double-click actions
- Better display of labels
- box() now is displayed as a box, as the maxima manual states
- box(expr, "highlight") display expr in red, instead
- Resolved a few display glitches
- Resolved OS-dependant wrong actions on gui events
- A few system and compiler specific fixes
- Resolved a few compilation warnings
- Try harder to connect maxima if one communication port is blocked (#1717)
- Better display of help text in the console
- Faster saving of .wxmx files
- Better toolbar bitmap handling

# 22.11.1

- Repaired zooming the worksheet

# 22.11.0

- Resolved an ID collision (#1726)
- Faster restart of maxima (#1715)
- Make various dialogs window-modal (#1712)
- Proper handling of unicode in selection (#1726)
- An unicode conversion error (#1727)
- Repaired the "enter matrix" dialogue
- Jump to the correct anchor even if using online help
- Translation updates
- Many windows build improvements
- Resolved a crash on restarting maxima

# 22.09.0

- Support for wxWidgets 3.1.7
- The description field for wizards no more is a MouseOverToolTip
- Wizards now accept chars from the unicode buttons
- Wizards now can balance parenthesis
- Wizards for operatingsystem, basic programming and string operations
- More miscellaneous work on the wizards
- The watchlist was broken
- In MacOs the config dialogue crashed
- Better scalability of the config dialogue
- Folded multiline code cells are now sent to Maxima correctly (#1674)
- The output of the "?" command contained spurious "\<" (#1688)
- ?, ?? and describe() now use a browser window, by default
  (an up-to-date Maxima is required for that feature)
- Search harder for the Maxima manual
- Search harder for Maxima manual entries
- Now we find Maxima manual entries that have no explicit anchor
- On Linux, MacOs and, if wxWidgets was compiled with edge support
  the help browser can be obened in a dockable window
- wxMaxima can now compiled, even if wxWidgets was configured with
  the option "--enable-privatefonts=no".
  Use the option -DWXM_INCLUDE_FONTS=NO when compiling wxMaxima.
- Names that end in an underscore are now printed correctly (#1706)
- Made the tests work on more platforms (#1709)
- Resolved some asserts (#1574)
- Working MathJaX (#1702)
- A primitive integrated ChangeLog viewer
- Better documentation (#1693)
- Better unicode handling (#1691)
- Many additional bugfixes

# 22.05.0:

- Updated the Ukrainian translation
- Added `guess_exact_value` to the numeric menu.
- Added lapack to the matrix menu
- Added linpack to the numeric menu
- Added a "declare facts" right-click menu entry to variables
- We no more reserve space for hidden input
- A better logexpand section in the numeric menu
- Many wizards have a more modern look and feel, now
- The modern-style wizards are now dockable by default
- Undo and redo buttons for the toolbar
- A "construct fraction" wizard (#1664)
- Printing multiple-page documents now should work again on Linux (#1671)
- The selection should now no more overwrite test (#1665)
- Hidden code cells now are correctly sent to maxime (#1674)
- MacOs: The toolbar icons work now on retina screens (#1657)
- MacOs: The config dialogue works again (#1662)
- Many additional MacOs fixes
- The Cygwin build works again
- Support for wxWidgets 3.1.6

# 22.04.0:

- A Russian translation update from Olesya Gerasimenko
- Repaired drag-and-drop on MacOs (#1624)
- Resolved a bug in parenthesis matching (#1649)
- Faster startup logic

# 22.03.0:

- Add an compile option "WXM_INCLUDE_FONTS", which allows to
  specify, if additional fonts should be included.
  Current default: YES; use -DWXM_INCLUDE_FONTS=NO when calling
  CMake to disable that.
  Including fonts caused some issues on MacOS. (#1580)
- Rename command line option --logtostdout to --logtostderr.
  Because the option does print the log messages to STDERR, not STDOUT.
- A better heuristics for finding out what to style as 1D maths
- An updated french translation from Frédéric Chapoton
- Consistent image resolutions
- No more crashes on exporting multiple animations at once (#1616)
- Drag-and-Drop in the table of contents
- Table of contents: Convert sections to subsections and similar (#1629)
- Parenthesis and quote matching now uses the Maxima parser.
  This means it now both knows the details of the Maxima language
  and is much faster than before.
- A way of limiting the toc depth shown in the table of contents
- Now the problems after a kill(all) should be finally resolved
- Better support for the debugger (#1625)
- Machine-Float NaNs no more causes XML errors
- Labels with hexadecimal numbers no more cause crashes (#1633)
- wxMaxima no more skips chars in subscripted variables (#1632)
- Merging cells per keyboard shortcut works again (#1626)
- Symbols buttons whose symbols the GUI font lacks are now hidden

# 21.11.0:

- Allow seeing all files in the file save dialog. (#1557)
- Fix an error when using Alt up arrow (show previous command) (#1566)
- Rewrote a config dialogue
- Improvements in the manual
- Update Spanish translation of manual
- Update German, Russian, Hungarian and Ukrainian translation
- Fix a warning, which occurred, when starting wxMaxima the first time
- ww898 unnecessarily limited the list of supported platforms
- wxMaxima's properties to Maxima symbols can now no more be killed
- Allow single GroupCells to be converted to LaTeX
- Enable reloading and changing images inserted via "Cell" -> "Insert Image..." (#1597)
- Correctly set the document saved state when the size of an image is changed
- Use the AUTHORS file for the Credits page in the "About" dialogue
- Handle power events (#1607)
- Fix ugly buttons in the sidebars with recent wxWidgets

# 21.05.2:

- The integral signs disappeared or were misplaced on some MS Windows computers

# 21.05.1:

- Resolved a segfault on printing that could be triggered on some computers

# 21.05.0:

- Long numbers now can be broken into lines
- On Export long numbers sometimes got lost
- The height of folded cells now is calculated correctly, again (bug #1532)
- Printing now can insert pagebreaks within GroupCells (bug #970, #1528,
  #1241, #181, #573,#1082)
- A Russian translation update by OlesyaGerasimenko
- A Ukrainian translation update by Yuri Chornoivan
- Changed the font for the PDF manuals to free Google Noto fonts.
  Now all PDF manuals can be created without missing characters
  (before cyrillic, chinese and greek characters were missing).
- Solved a crash when printing.

# 21.04.0

- Fix visualization after "Evaluate all cells (Ctrl+Shift+R)" (bug #1512).
- Fix context sensitive help with F1 (bug #1508).
- Add an option for not showing matching parenthesis (bug #1509).
- Changed the weather icons in the toolbar to more meaningful ones (bug #1514).
- Many bugfixes with respect to correctly breaking lines (bugs #1528, #1471, #1512).
- Fix a bug in copy-paste of saved/loaded results (bug #1518).
- Fix a crash when pressing Ctrl-X while a math cell is selected (bug #1519).
- CMake 3.10 is now required for building wxMaxima.
- C++ 14 is required for building wxMaxima. Now CMake knows about that fact.
- Many external build scripts are now replaced by C++ and CMake commands.
- A menu entry for when to invoke the debugger.

# 21.02.0

- A Spanish translation update by cyphra
- Replaced openMP by c++11's built-in thread handling. This should improve
  stability at the cost of making loading files with lots of images slower.
- Language selection now works on Windows.
- Resolved a crash when deleting regions.
- The --ini switch should work again correctly.
- Fix an issue, where demos didn't re-center screen anymore, when screen
  is full and print was incomplete.
- Made the time between autosaves configurable.
- Fixed an issue with reproducible builds.

# 21.01.0

- Chinese translation updates by 刘荣.
- A Russian translation update by OlesyaGerasimenko
- Many small bugfixes including a crash on loading files with images.

# Changes in older wxMaxima versions you can find here

<https://github.com/wxMaxima-developers/wxmaxima/blob/Version-21.02.0/NEWS.md>
