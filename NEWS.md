# 21.04.0 (not released yet)
 * Fix visualization after "Evaluate all cells (Ctrl+Shift+R)" (bug #1512).
 * Fix context sensitive help with F1 (bug #1508).
 * Add an option for not showing matching parenthesis (bug #1509).
 * Changed the weather icons in the toolbar to more meaningful ones (bug #1514).
 * Fix a bug in copy-paste of saved/loaded results (bug #1518).
 * Fix a crash when pressing Ctrl-X while a math cell is selected (bug #1519).
 * CMake 3.10 is now required for building wxMaxima
 * C++ 14 is now required for building wxMaxima

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
