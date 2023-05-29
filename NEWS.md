# 23.05.1
 * Updated the Russian and Italian translation.
 * Process the command line option --maxima=<str>.
 * Resolved several possibilities to crash the config dialogue 
   (#1774, #1766)
 * Correctly calculate the worksheet size in the background (#1766)
 * Correctly restore parenthesis on load (#1779)
 * Always revise the decision what to display as 2D equation on 
   window width changes
 * 2D display of integrals had zero size (#1780)

# 23.05.0
 * Got rid of some compiler and cppcheck warnings
 * Big performance improvements
 * A "maxima versus typical programming languages" tutorial.
 * Table of contents: Jump to the chapter the cursor is in.
 * Resolved another crash in the config dialogue
 * Updated the unicode character list
 * Resolved more potential event ID clashes
 * Use an external file as wxMathML.lisp is now selected using
   a command line option, not configured in the GUI.
   This is mostly needed for developers, no need to expose it
   to the end user.
 * Better handling of paths to maxima when starting new 
   wxMaxima processes
 * new wxMaxima processes now get more of the command-line args
   of the parent process
 * wxMaxima no more tries to install its own fonts.

# 23.04.1
 * More config dialogue tweaks
 * MacOs: No more create config icons that aren't needed
 * Recent file list: Don't try to re-use no-more-used wxWidgets IDs
 * Make sure that the help sidebar is shown when opening a help topic
 * If maxima wants us to open a help browser and wxMaxima was compiled
   without one it now opens an external help browser
 * Be aware that the MSW port recycles unused window IDs.
 * The load() command now again works with symbols as package names.

# 23.04.0
 * Made the GUI more responsive for functions with much output
 * Handle errors in the XML from maxima more gracefully
 * Rewrote parts of the lisp part of wxMaxima
 * Resolved a few lisp warnings
 * Resolved a few XML errors caused by not escaping special chars
 * Automatic XML generation tests
 * More MacOs fixes
 * Correct display of operators
 * A better style sample for the config dialogue
 * Resolved several asserts
 * Resolved a few small bugs in the autosave logic

# 23.03.0
 * Make sure all bitmaps are valid on High-DPI MacOs (#1749)
 * More styles consistency (#1753)
 * Better configuration validation
 * Depending on the wxWidgets version Unicode letters were 
   interpreted inorrectly (#1754)
 * The manual anchors cache now saves the file-per-chapter URLs, too.
 * cmake -DWXM_DISABLE_WEBVIEW now allows to find wxWidgets, if 
   wxWebView wasn't compiled.
 * Completely rewrote the dockable-sidebars-stuff
 * Many files are now build on demand, not at configure time

# 23.02.0
 * Fix XML generation of wxmx documents (#1556)
 * A faster font cache
 * Faster communication between wxMaxima and Maxima
 * Dropped the dependency on an [internal] ww898 library
 * More consistent text style handling
 * A try to improve locale management on the Mac platform
 * A separate style for operators (#1002)
 * A nicer ChangeLog

# 22.12.0
 * A cleaner status bar with double-click actions
 * Better display of labels
 * box() now is displayed as a box, as the maxima manual states
 * box(expr, "highlight") display expr in red, instead
 * Resolved a few display glitches
 * Resolved OS-dependant wrong actions on gui events
 * A few system and compiler specific fixes
 * Resolved a few compilation warnings
 * Try harder to connect maxima if one communication port is blocked (#1717)
 * Better display of help text in the console
 * Faster saving of .wxmx files
 * Better toolbar bitmap handling

# 22.11.1
 * Repaired zooming the worksheet

# 22.11.0
 * Resolved an ID collision (#1726)
 * Faster restart of maxima (#1715)
 * Make various dialogs window-modal (#1712)
 * Proper handling of unicode in selection (#1726)
 * An unicode conversion error (#1727)
 * Repaired the "enter matrix" dialogue
 * Jump to the correct anchor even if using online help
 * Translation updates
 * Many windows build improvements
 * Resolved a crash on restarting maxima

# 22.09.0
 * Support for wxWidgets 3.1.7
 * The description field for wizards no more is a MouseOverToolTip
 * Wizards now accept chars from the unicode buttons
 * Wizards now can balance parenthesis
 * Wizards for operatingsystem, basic programming and string operations
 * More miscellaneous work on the wizards
 * The watchlist was broken
 * In MacOs the config dialogue crashed
 * Better scalability of the config dialogue
 * Folded multiline code cells are now sent to Maxima correctly (#1674)
 * The output of the "?" command contained spurious "<" (#1688)
 * ?, ?? and describe() now use a browser window, by default
   (an up-to-date Maxima is required for that feature)
 * Search harder for the Maxima manual
 * Search harder for Maxima manual entries
 * Now we find Maxima manual entries that have no explicit anchor
 * On Linux, MacOs and, if wxWidgets was compiled with edge support
   the help browser can be obened in a dockable window
 * wxMaxima can now compiled, even if wxWidgets was configured with
   the option "--enable-privatefonts=no".
   Use the option -DWXM_INCLUDE_FONTS=NO when compiling wxMaxima.
 * Names that end in an underscore are now printed correctly (#1706)
 * Made the tests work on more platforms (#1709)
 * Resolved some asserts (#1574)
 * Working MathJaX (#1702)
 * A primitive integrated ChangeLog viewer
 * Better documentation (#1693)
 * Better unicode handling (#1691)
 * Many additional bugfixes

# 22.05.0:
 * Updated the Ukrainian translation
 * Added `guess_exact_value` to the numeric menu.
 * Added lapack to the matrix menu
 * Added linpack to the numeric menu
 * Added a "declare facts" right-click menu entry to variables
 * We no more reserve space for hidden input
 * A better logexpand section in the numeric menu
 * Many wizards have a more modern look and feel, now
 * The modern-style wizards are now dockable by default
 * Undo and redo buttons for the toolbar
 * A "construct fraction" wizard (#1664)
 * Printing multiple-page documents now should work again on Linux (#1671)
 * The selection should now no more overwrite test (#1665)
 * Hidden code cells now are correctly sent to maxime (#1674)
 * MacOs: The toolbar icons work now on retina screens (#1657)
 * MacOs: The config dialogue works again (#1662)
 * Many additional MacOs fixes
 * The Cygwin build works again
 * Support for wxWidgets 3.1.6

# 22.04.0:
 * A Russian translation update from Olesya Gerasimenko
 * Repaired drag-and-drop on MacOs (#1624)
 * Resolved a bug in parenthesis matching (#1649)
 * Faster startup logic

# 22.03.0:
 * Add an compile option "WXM_INCLUDE_FONTS", which allows to
   specify, if additional fonts should be included.
   Current default: YES; use -DWXM_INCLUDE_FONTS=NO when calling
   CMake to disable that.
   Including fonts caused some issues on MacOS. (#1580)
 * Rename command line option --logtostdout to --logtostderr.
   Because the option does print the log messages to STDERR, not STDOUT.
 * A better heuristics for finding out what to style as 1D maths
 * An updated french translation from Frédéric Chapoton
 * Consistent image resolutions
 * No more crashes on exporting multiple animations at once (#1616)
 * Drag-and-Drop in the table of contents
 * Table of contents: Convert sections to subsections and similar (#1629)
 * Parenthesis and quote matching now uses the Maxima parser.
   This means it now both knows the details of the Maxima language
   and is much faster than before.
 * A way of limiting the toc depth shown in the table of contents
 * Now the problems after a kill(all) should be finally resolved
 * Better support for the debugger (#1625)
 * Machine-Float NaNs no more causes XML errors
 * Labels with hexadecimal numbers no more cause crashes (#1633)
 * wxMaxima no more skips chars in subscripted variables (#1632)
 * Merging cells per keyboard shortcut works again (#1626)
 * Symbols buttons whose symbols the GUI font lacks are now hidden

# 21.11.0:
 * Allow seeing all files in the file save dialog. (#1557)
 * Fix an error when using Alt up arrow (show previous command) (#1566)
 * Rewrote a config dialogue
 * Improvements in the manual
 * Update Spanish translation of manual
 * Update German, Russian, Hungarian and Ukrainian translation
 * Fix a warning, which occurred, when starting wxMaxima the first time
 * ww898 unnecessarily limited the list of supported platforms
 * wxMaxima's properties to Maxima symbols can now no more be killed
 * Allow single GroupCells to be converted to LaTeX
 * Enable reloading and changing images inserted via "Cell" -> "Insert Image..." (#1597)
 * Correctly set the document saved state when the size of an image is changed
 * Use the AUTHORS file for the Credits page in the "About" dialogue
 * Handle power events (#1607)
 * Fix ugly buttons in the sidebars with recent wxWidgets

# 21.05.2:
 * The integral signs disappeared or were misplaced on some MS Windows computers

# 21.05.1:
 * Resolved a segfault on printing that could be triggered on some computers

# 21.05.0:
 * Long numbers now can be broken into lines
 * On Export long numbers sometimes got lost
 * The height of folded cells now is calculated correctly, again (bug #1532)
 * Printing now can insert pagebreaks within GroupCells (bug #970, #1528,
    #1241, #181, #573,#1082)
 * A Russian translation update by OlesyaGerasimenko
 * A Ukrainian translation update by Yuri Chornoivan
 * Changed the font for the PDF manuals to free Google Noto fonts.
   Now all PDF manuals can be created without missing characters
   (before cyrillic, chinese and greek characters were missing).
 * Solved a crash when printing.

# 21.04.0
 * Fix visualization after "Evaluate all cells (Ctrl+Shift+R)" (bug #1512).
 * Fix context sensitive help with F1 (bug #1508).
 * Add an option for not showing matching parenthesis (bug #1509).
 * Changed the weather icons in the toolbar to more meaningful ones (bug #1514).
 * Many bugfixes with respect to correctly breaking lines (bugs #1528, #1471, #1512).
 * Fix a bug in copy-paste of saved/loaded results (bug #1518).
 * Fix a crash when pressing Ctrl-X while a math cell is selected (bug #1519).
 * CMake 3.10 is now required for building wxMaxima.
 * C++ 14 is required for building wxMaxima. Now CMake knows about that fact.
 * Many external build scripts are now replaced by C++ and CMake commands.
 * A menu entry for when to invoke the debugger.

# 21.02.0
 * A Spanish translation update by cyphra
 * Replaced openMP by c++11's built-in thread handling. This should improve
   stability at the cost of making loading files with lots of images slower.
 * Language selection now works on Windows.
 * Resolved a crash when deleting regions.
 * The --ini switch should work again correctly.
 * Fix an issue, where demos didn't re-center screen anymore, when screen
   is full and print was incomplete.
 * Made the time between autosaves configurable.
 * Fixed an issue with reproducible builds.

# 21.01.0
 * Chinese translation updates by 刘荣.
 * A Russian translation update by OlesyaGerasimenko
 * Many small bugfixes including a crash on loading files with images.

# Changes in older wxMaxima versions you can find here:
<https://github.com/wxMaxima-developers/wxmaxima/blob/Version-21.02.0/NEWS.md>
