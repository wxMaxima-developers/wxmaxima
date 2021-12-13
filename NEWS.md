# Current:
 * Add an compile option "WXM_INCLUDE_FONTS", which allows to
   specify, if additional fonts should be included.
   Current default: YES; set WXM_INCLUDE_FONTS=NO when calling
   CMake to disable that.
   Including fonts caused some issues on MacOS. (#1580)
 * Rename command line option --logtostdout to --logtostderr.
   Because the option does print the log messages to STDERR, not STDOUT.

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
