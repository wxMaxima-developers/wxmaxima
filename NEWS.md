# Current
 * Russian translation updates from OlesyaGerasimenko and Yuri Chornoivan
 * An italian translation update from Marco Ciampa
 * Repaired the non-case-sensitive "Replace All" functionality
 * Updated Ukrainian translation by Yuri Chornoivan
 * More efficient and accurate calculation of worksheet object sizes
 * (Hopefully) reduced the last visual glitches
 * Reduced the UI flicker on MS Windows
 * diff cells can now be broken into lines
 * A try to replace manual MacOs desktop integration with wxWidgets one
 * Another massive performance update
 * Reduced the memory footprint of big equations
 * A better 2D ASCII Art mode
 * Better High-DPI support
 * If it is configurable it now might have a right-click menu
 * Reduced the memory footprint of cells used only when line breaking is performed
 * Subscripts created by derivabbrev are now correctly copied as "diff" command
 * If Maxima comes without suitable manual we now use the online one instead
 * Added buttons that reset the configuration
 * A few menu items now are checkboxes set and cleared by maxima
 * wxMaxima forgot to release the communication port it used for talking to maxima
 * Improved performance of receiving data from maxima (over the network socket)
 * The font sizes are stored in configuration with full precision, without rounding to integer
 * Many additional bug fixes

# 20.07.0
 * A massive performance update
 * A new translation of the Turkish manual by Tufan Şirin
 * Improved italian translation by Marco Ciampa
 * An Russian translation update by OlesyaGerasimenko
 * Better display of sqrt() cells
 * Better display of fractions
 * Nicer display of lists
 * Made sure we read in the whole .mac file when opening .mac files
 * Allow building wxMaxima, if pandoc or po4translate are not installed
 * As always many small additional bug fixes

# Why is wxMaxima able to open truncated wxmx files?

.wxmx Files are technically .zip files like .pptx- files,
.docx- files and .xlsx-files. If you ever want to extract an image
in the original format (or remove a nasty write protection) you can
always rename the document to an .zip file and look at its internal 
structure.

Since .zip files, when broken, can be hard to handle wxMaxima invests
a big effort in making sure that saving succeeds:

 * First the file is saved to a .wxmx~-file. If anything goes wrong in
   this step the original .wxmx-file is still there and unharmed.
 * After saving the file wxMaxima tests if the .wxmx~-file is a valid
   . zip file that can still be opened.
 * It also tests if the content.xml (which contains everything except
   the images) inside the .zip archive can be read and is valid xml.
 * Only then it moves the file to the final location - which doesn't 
   physically move the data, but just changes the directory entry to
   point from the correct name to the correct file.
 * And since on one operating system virus scanners can prevent this
   last step while they still are scanning the file wxMaxima tests if
   the move has been successful and re-tries the move, if necessary.

In theory nothing can go wrong here, except if the storage device is
faulty, cosmic rays have altered a write cache before the data was 
actually written or the file has been altered by an external program.
Even in that case not everything is lost, though:

 * The text part of the .wxmx file isn't compressed, so it can be 
   viewed when opening the .wxmx file using a text editor.
 * Also the text part of the .wxmx file is always to be found near 
   the very beginning of the .wxmx file.
 
This means that even if there should be a bad block somewhere in the 
middle of the file chances that wxMaxima will be able to restore at 
least the text and code part automatically are relatively high.
 
# 20.06.6
 * Fixed an error 20.06.5 has introduced

# 20.06.5
 * wxMaxima now ships around a pecularity in MinGW 
 * wxMaxima now ships around a MinGW-specific bug in catch2

# 20.06.4
 * wxMaxima now automatically recovers truncated .wxmx files
 * Resolved an compile error with the crosscompiled Windows build

# 20.06.3
 * An over-zealous assert

# 20.06.2
 * Updated Turkish translation by Tufan Şirin
 * Fixed a major cell leak in the undo action system.

# 20.06.1
 * A bugfix that resolves a crash on selection

# 20.06.0
 * Many spell corrections and bugfixes
 * Updated Russian translation by OlesyaGerasimenko
 * Updated Ukrainian translation by Yuri Chornoivan
 * Many corrections to the german translation
 * The history sidebar now works again
 * TextCells with copy-alt-text caused an crash
 * A few additional small bugfixes
 * Many (really many!) big stability improvements
 * in sqrt(conjugate(f(x))) the horizontal lines coincided
 * Strings were searched for comment starts when adding line endings
 * Faster start-up by avoiding unneeded redraw events
 * A big number of performance improvements
 * Unfortunately one last temporary fix reduces performance again
 * Corrected a bug that sometimes caused parts of equations not to be displayed
 * As this allows to improve performance and stability C++14 is now used
 * Speed boost The list of anchors from the manual is now cached on disk

# 20.04.0
 * Resolved a crash on right-clicking a GroupCell
 * Help works again in the german translation
 * View/Invert worksheet brightness now toggles an instant dark worksheet mode
 * Removed an unnecessary info dialogue on startup
 * Right-click-suggestions for similar command names
 * Corrected the initial slideshow size
 * An Ukrainian translation update from Yuri Chornoivan
 * A Russian translation update from OlesyaGerasimenko
 * A more stable language selection mechanism
 * Extended the "fitting data" example
 * A example on how to iterate through lists efficiently
 * Many spelling corrections and better formulations
 * A big number of small bug-fixes
 * Another noticeable SpeedUp

# 20.03.0
 * Corrected a few last bugs for pre- and post- super- and subscripts
 * Many improvements to the manpage and the html documentation
 * Improved support for pre- and post- super- and subscripts provided 
   by Robert Dodier
 * wxWidget's integrated help browser on some platforms was broken 
   => now the default web browser is used
 * Support for importing static and some types of animated gif files
 * Animation frames created using draw() now can be popped out 
   interactively
 * An option to configure the max amount of gnuplot data to save for 
   this feature
 * wxMaxima now no more crashes on FreeBSD
 * wxMaxima now tries not to use stderr as stderr on the mac breaks
   communication with maxima.
 * As always: Many additional bugfixes.

# 20.02.3
 * Support for pre- and post- super- and subscripts provided by Robert Dodier

# 20.02.2
 * Resolved two deadlocks that were introduced in 20.02.1
 * Resolved two asserts on MS Windows
 * Actually included the russian translation update
 * Many code cleanups and additional bug fixes
 * Another small performance boost

# 20.02.1
 * wxMaxima now can use Multi-threading, if a new enough OpenMP is found
 * Part of the added performance is only effective if omp.h (often in the 
   libomp package) is installed.
 * Better scroll wheel support
 * A Russian translation update by OlesyaGerasimenko
 * It was possible that a part of a fraction wasn't displayed
 * Several additional bug fixes
 * Cppcheck code analysis now is only run if explicitly enabled
 * The minimum cmake version now is 3.7

# Multi-Threading
Most modern computers allow to use more than CPU core at a time.
This can greatly boost the performance of an application. But there
are tasks that are not suited for that:
 * Essentially everything that involves GUI actions is not thread-save.
   The main purpose of wxMaxima is providing a GUI, though.
 * Loops in which every run requires knowing the result of the last cannot
   be split into multiple threads. This includes most of the string and 
   list processing.
 * If the program needs the result now trying to calculate this result 
   in a separate thread doesn't help.
 * If a task is memory-limited and accesses memory in a linear way dividing 
   the task into threads can increase the memory access times (due to 
   constantly switching memory rows)
 * Additionally for short operations the overhead of starting a new 
   thread is way higher than the gain
This means wxMaxima can use multiple threads. But the number of places
this is possible in is surprisingly low.

# 20.02.0
 * The MacOs build should now generate working .dmg installers
 * Another small tutorial
 * Symbols sidebar: Correctly remember additional symbols
 * Unicode chars can now be input using the <hexcode>alt+x method
 * A unicode sidebar that isn't shown by default as it starts up slow
 * Resolved an assert shown by some systems on startup
 * Many corrections to the display routines

# 20.01.3
 * Language selection now works even on systems with missing language support
 * Resolved a copy-and-paste regression
 * A russian translation update by OlesyaGerasimenko
 * German translation updates
 * Improved stability of the communication with Maxima
 * Preparation for Maxima's pre-sub-and supscript feature
 * Corrected a few visual glitches of the worksheet
 * More tutorials describing a few of Maxima's features

# 20.01.2
 * Corrected html export of the lowest sectioning unit
 * Sometimes text after fractions wasn't displayed
 * Copy-and-paste should now work in more cases
 * Selecting text within fractions should now work again
 * Corrected a few display glitches of the worksheet
 * wxMaxima now comes with five example files
 * More and better-working mouse-over tooltips

# 20.01.1
 * Nice menu icons for the operating systems that support this
 * Started to bundle small tutorials with wxMaxima
 * Many small additional bug fixes

# 19.12.4
Since 19.12.3 depending on the system allocated immense amounts of
RAM and crashed a 19.12.4 had to be released. Together they port
many big improvements to the display code.

 * Resolved a crash whilst unpacking the svg icons.

# Transition to C++11 - Part II

Many annoying bugs have to do with memory management. Typical examples are:
 * Out-of-bonds reads and writes: n+1 bytes of an n-bytes-long string are
   read or written.
 * Use of uninitialized memory: Memory is first read and then set to the
   value that one would have wanted to read from it.
 * Use-after-free: Memory was first marked as "free" and then used again.
 * Memory Leaks: Memory isn't marked as "free" even if it is no more used.
Against the last two of these bugs C++11 offers automatic pointers that
stay allocated while they are in used and then are automatically marked
as free. In the next wxMaxima release about 2500 lines will be changed
in order to make use of that functionality.

# 19.12.3
 * A Russian translation update by OlesyaGerasimenko
 * The manual is now found on Gentoo, too
 * Big improvements to the maths output
 * A major speedUp by caching already-determined text snippet sizes
 * All icons that aren't provided by the operating system now are
   rendered from SVG
 * Internal communication with maxima is now more MathML-like in
   many places
 * Many additional small bug fixes.

# 19.12.2
 * An hungarian translation update by István Blahota
 * Drastically sped up the lookalike char detection
 * Fixes for many small bugs
 * Many changes that prevent typical programming errors
 * Saving to .wxm sets the worksheet to "saved" again
 * More cases of modern wxWidgets signal handling
 * svg support that unfortunately lacks a font renderer
 * Right-click menus for the symbols sidebars
 * (Mostly) fixed the index in the manual
 * Allow building and installing the manual in other formats (epub, pdf, ...)

# 19.12.1
 * Made the tooltips more visible
 * Added a warning tooltip about missing multiplication signs
 * Sped up drawing of new cells correcting the size calculations
 * If lookalike chars are used in a way that can cause hard-to-find errors now a
   warning tooltip is generated
 * A few additional bug fixes that (besides others) resolve a potential crash
 * An option to turn all multiplication dots on

# 19.12.0
  * wxMaxima now uses C++11 that makes the code more readable and helps
	finding bugs
  * If found wxMaxima now asks cppcheck to find bugs
  * Increased the speed of the program again
  * Opening the config dialogue since 19.11.0 unchecked "offer known answers"
  * For Cmake>=3.10.0 the build system now asks cppcheck to find bugs.
  * Changing the worksheet style was partially broken.
  * A big number of additional bug fixes in various places
  * A config option that forces displaying all multiplication signs.

# Transition to C++11

The next wxMaxima release will make use of C++11 features: I *hope* that C++
has been out long enough in 2019 that every relevant compiler supports it.
C++11 brings in features that allow to make compilation fail on otherwise
hard-to-detect bugs, some features that might make the code easier to read...
...and I am afraid that plain old c++ might look antiquated one day.

Additionally it will contain many bug fixes hoping that I won't fix bugs that
in retrospect didn't exist... ...on the plus side the program should again
have gotten faster by giving the compiler additional hints for optimization
and the transition has shrunk wxMaxima by about 100 lines. Much help with
finding bugs came from the cppcheck utility which caused changes in over
1700 lines of code, mainly in order to prevent possible future bugs.

# 19.11.1
  * Improved italian translation by Marco Ciampa
  * Autocompletion no more causes asserts
  * A better logic that decides which cell to send to maxima
  * wxMaxima now supports "-l", "-u" and "-X" command-line args like maxima
  * The help files now are installed in the Right Place
  * Many additional small bug fixes

# 19.11.0
  * Chinese translation updates from liulitchi
  * Turkish translation updates from TufanSirin
  * Hungarian translation updates from Blahota István
  * German translation updates from Wolfgang Dautermann and Gunter Königsmann
  * Better detection of the Gnuplot location
  * Better detection of the right place to open autocomplete popups in
  * Better formatting of special unicode operators
  * If autosave on closing fails: Allow the user to ask to exit anyway
  * Enable Autosave even if the option to keep the file saved is disabled
  * A context menu with the "display labels" choices
  * A context menu with "max number of digits" choices
  * A speedup by preventing recursive calls to resize functions
  * Corrected parametric plots in the plot wizard
  * A command-line switch that copies all log messages to stderr.
  * Batch mode no more adds documents to "recent documents".
  * For purists: A "don't offer known answers" config option.
  * Now some toolbar items can be disabled.
  * Added more toolbar buttons that evaluate cells.
  * Many additional bug fixes

# 19.10.0
  * Corrected a Regression: The better display of exponents failed for exp(x^2).
  * Seems like some Windows computers only have a numpad enter key.
  * Chinese translation updates from liulitchi.
  * Russian translation updates from OlesyaGerasimenko.

# 19.09.1
  * The names of greek letters are no more converted to greek letters in variable
    names by default.
  * Smarter Formatting of asterisk and hyphen.
  * An automatic per-commit Windows build for Appveyor.
  * Always recalculate the worksheet size when needed.
  * Better vertical alignment of exponents.
  * The layout is now more consistent between platforms.
  * Chinese translation updates from liulitchi.
  * wxMaxima now remembers the question for each answer, not only its number.
  * Now the manual can be localized, too.
  * Non-breaking spaces are now replaced by ordinary spaces before being sent to Maxima.
  * Many other bug fixes.

# 19.09.0
  * Handle timeouts when writing to maxima and partial writes.
  * Worksheet text disappeared on scrolling on some platforms.
  * Sped up sending code to Maxima.
  * Removed quoting of Unicode characters within strings sent to maxima.

# 19.08.1
  * Bugfixes for nearly every instance something was drawn on the screen.
  * Better handling of multi-cell Lisp code

# 19.08.0
  * Updated the russian translation
  * More and better test cases
  * Another SpeedUp
  * Correctly handle nested comments
  * Format lisp code correctly
  * Correctly read the default style
  * Gracefully handle invalid colors
  * Changed the default communication port to 40100
  * Several fixes for different combinations of wxWidgets and GTK versions

# 19.07.0
 * Many improvements and bugfixes in the LaTeX and HTML export function
 * Many updated translations
 * Resolved a freeze on autocompletion
 * A sidebar that shows the contents of variables
 * Simplified the recalculation logic in order to squash the remaining bugs
 * Simplified the sync between input and output cells

# 19.05.7
 * Support :lisp as the first command of the worksheet, in case that
   Maxima is new enough to support that.

# 19.05.6
 * Correctly save the autosave interval
 * A better logic for saving on exit if autosave is on and the file
   cannot be created.

# 19.05.5
 * An updated hungarian translation
 * The new Maxima command cartesian_product_list is now known to
   the autocompletion.
 * Better xim compatibility.

# 19.05.4
 * Corrected saving formatting styles in the config dialogue
 * Allow to change the title etc. font again
 * Correctly output uppercase greek letters in TeX
 * Re-enable the old-style ESC commands.
 * Made wxMaxima default to auto-searching for the maxima binary.

# 19.05.3
 * Corrected the autowrap line width for high zoom factors
 * Added a few missing "Update the user interface" events

# 19.05.2
 * Sometimes the auto-evaluation of the worksheet auto-triggered
 * Support for broken locales that for valid non-ascii chars returned
   isprint()= false
 * Corrected reading font sizes from styles and configuring styles.

# 19.05.1
 * Fixes for various combinations of GTK and wxWidgets versions
 * A working Mac version
 * A few Windows bug Fixes
 * Big performance improvements.

# 19.05.0
 * Another ArchLinux displaying issue
 * More GTK3 compatibility
 * Resolved a windows hang
 * Better Dark Mode compatibility
 * Performance improvements and bugfixes
 * Many code optimizations.

# 19.04.3
 * wxMaxima now informs Maxima which front-end is in use.

# 19.04.2
 * Corrected the size of error messages
 * A "Copy to Matlab/Octave"-feature.
 * wxMaxima now delays interpreting the data from Maxima until it encounters
   a newline or a timer expires.
 * EMF output no more causes crashes and strange behaviour.
 * RTF output should now work again
 * entermatrix() now works again.
 * Better help file detection on MS Windows.

# 19.04.1
 * Corrected the size of error messages

# 19.04.0
 * The cursor width now is taken from the current screen
 * Autocompletion sometimes cleared the result after inputting it
 * Added more commands to autocompletion
 * Spanish and italian translation updates
 * We no more need to re-implement Maxima's load() routine.
   Instead we call the original one now.
 * Animations now on load remember which slide they stopped at.
 * A more error-proof language selection.
 * If the system looks like it uses UTF8 by default the locale name
   that is passed to Maxima now ends in ".UTF8" which should resolve
   the "setting locale failed" errors from Maxima.
 * Corrected the initial size of error messages.
 * Many additional bug fixes
 * Code cleanups

# 19.03.1
 * HighDPI fixes for ArchLinux, Windows and SuSE
 * Resolved crashes on RedHat and MS Windows
 * SBCL's compilation messages now appear in the status bar, not
   in the worksheet.
 * Removed an unneeded "maxima has finished calculating" on startup.
 * Many small bug fixes.
 * wxWidgets >= 3.1.0: Corrected the toolbar icon size.

# 19.03.0
 * The ESC sequences for inputting symbols now use autocompletion.
 * Use wxAutoBufferedPaintDC for drawing the worksheet instead of
   implementing our own version of it.
 * The usual bug fixes, this time including one bug fix that makes
   wxMaxima work again on Gentoo and some help path lookup fixes.

# 19.01.1-19.02.2:
 * Bug fixes only

# 19.01.0
 * Many bugfixes for the display code
 * The desktop integration files now adher to the newest standards
 * Refactoring in order to make the code easier to understand.
 * Spelling fixes
 * Shipped around a wxWidgets <3.1 bug for embedding bitmaps in svg
 * Many Links were updated to https
 * A better help file detection that still works if there is no .hhp file.
 * A command-line option that allows to specify the location of Maxima.

# 18.12.0
  * Use C++'s strong typing for more enums.
  * Another big SpeedUp.
  * Now we make sure that the Manifest telling wxMaxima is high-DPI aware is
    the only manifest packaged with the .exe file.
  * Tried to simplify the display code
  * Source file and function names that more closely their current meaning.
  * Many bug fixes.
  * Now we pass the canonical locale name to Maxima, not the short one.
  * We no more try to set Maxima's locale if it doesn't differ from the system's
    default one.

# 18.11.2
  * Better horizontal spacing of cells that are wider than lines

# 18.11.1
  * Better horizontal spacing of headings.

# 18.11.0
  * GTK3 Compatibility
  * Worked around C compiler bugs that truncated the beginning of long strings
  * Zoom gestures for more computers
  * A better lisp mode
  * Limit cells now can be broken into lines if they are too wide for the window
  * Major performance improvements
  * Several appearance improvements
  * Many additional bug fixes

# 18.10
  * Defer any try to change Maxima's configuration until all questions are answered.
  * A menu item that sets the autosubscript feature for the current worksheet.
  * A menu allowing to switch floats to engineering format.
  * Maxima can now pass the values of variables to wxMaxima.
  * Autocompletion for functions from lisp-only packages and for all of Maxima's.
    built in functions.
  * A context-sensitive "draw" sidepane.
  * We now allow a packet from Maxima to end in the middle of an unicode codepoint.
  * If the name of a lisp function is known it now is displayed.
  * A right-click menu that copies an animation to the clipboard.
  * A new default value for the current MathJaX release.
  * A "recent packages" menu for packages loaded from load() or from the menu.
  * Advertise the current file and the file's state to the Operating System
  * Copy as Enhanced Metafile (Windows only).
  * Code simplifications.
  * A dockable panel for the debug messages.
  * A more modern theme.
  * Draw: A right-click menu allowing to open images from the current session as
    interactive gnuplot sessions.
  * Migrate the config file to a XDG-compatible location if wxWidgets is new enough
    to support it.
  * A "Tip of the day" dialogue that allows for resizing, scrollbars and work while
    it is still open.
  * Matrices can now be displayed using round and square brackets.
  * Various High-DPI enhancements.
  * Enhancements for low-resolution screens.
  * Many bug fixes.
  * Reduced the number of autogenerated files, installed files and external
    dependencies to a minimum.
  * Big performance improvements, again.

# 18.02
  * More tooltips
  * A menu containing the most important commands for lists.
  * TableOfContents: A right-click menu item that allows to toggle display of section numbers
  * The size of images can now be restricted to any amount of centimeters.
  * On a few operating systems: The network indicator icon now knows how much CPU
    power Maxima uses.
  * Autocompletion now suggests the names of built-in packages for load(), batch()
    and batchload().
  * Autocompletion now suggests the names of built-in demos for demo().
  * wxMaxima can now handle requests to display raw lisp elements.
  * A drop-down box for changing cell styles
  * If an input cell is hidden and has no output it now is no more displayed
    as a blank vertical space.
  * wxMaxima no more requires the icon files, tooltips and autocompletion files to be
    shipped as separate files, which makes creating a ready-to-install package of wxMaxima
    easier and less error-prone.
  * Autosave is now enabled by default on new installs.
  * Big code cleanups.
  * Many bugfixes
  * Many performance improvements
  * Dropped the autotools as the cmake build of wxMaxima is way more powerful.

# 17.10
  * Jump to the char containing the error on encountering unmatched parenthesis.
  * Try to place the cursor near the error in all other cases.
  * Attempt to mark the whole error message in red. This is bound to fail in
    some cases as Maxima sometimes only sends part of an error message through
    the error() command. But it is better than nothing.
  * Detect and mark most warnings.
  * A "evaluate cells below" right-click-menu item.
  * Autosave now even works for unsaved documents adding them to the
    "recent documents" list if needed.
  * Big parenthesis are now drawn using unicode characters, if available.
  * Nicer integral, Product and Sum signs, optionally including antialiassing
    even if no font provides them.
  * MouseOver tooltips for some of the more exotic worksheet items.
  * A "Copy as svg" function that is as good as wxSVGFileDc allows us to be.
  * Worksheet objects now can provide their own ToolTips.
  * Config Dialogue: An editor for the startup file.
  * It is now possible to select which formats "Ctrl+C" should place data on
    the clipboard in.
  * Now multiple animations can run concurrently and can be told to autostart.
  * A menu entry that allows to select 1D and ASCII Art Maths
  * Recent files that currently don't exist are grayed out now.
  * A command-line switch that triggers evaluation on startup of wxMaxima
  * Many Bug Fixes and Performance improvements.

# 17.05
  * A config item that allows to disable printing of Cell brackets.
  * Cell brackets of inactive cells can now be hidden.
  * Due to popular demand we can now suppress the output of automatic labels.
  * The option to use a Minus instead of a Hyphen.
  * Table of Contents: Folded items are now grayed out.
  * Table of Contents: A right-click menu.
  * Copy-and-paste now includes hidden cells and folded items.
  * Eye Candy: An indicator that shows when Maxima and wxMaxima exchange data.
    Double-clicking this indicator opens up the network monitor.
  * Autocompletion now collects words from the current code cell, too.
  * Better indentation of if clauses and loops.
  * If the number is too long to display it now is still possible to copy it.
  * Documented that box() highlights text in Maxima's output.
  * the with_slider type of commands now uses wxstatusbar() in order to provide
  * progress information for the user.
  * Several scrolling fixes, again.
  * Two concurrent wxMaxima instances now no more overwrite each other's "Recent files"
    list.
  * If a error message or question arises or Maxima finishes calculating and the window
    currently isn't active the operating system is asked to notify the user in a
    non-disrupting way. For the case of the end of a calculation this can be turned off.
  * The notification can be activated for the case that the window isn't active and Maxima
    finishes calculating, too.
  * Invalid XML tags are now handled more gracefully
  * Deduplicated the code for saving .wxm files
  * content.xml files that have been extracted from a broken .wxmx file (e.G.
    using a text editor as UTF8-encoded TeXt without BOM can now be loaded by
    wxMaxima in order to reconstruct the worksheet.
  * MacOs: Upgrading from a previous version should no more cause crashes due to
    font issues.
  * MathJaX export can now be configured to instruct the html export to download
    MathJaX from a different URL: They moved the URL one can use and might do so
    again somewhere in the future.
  * Maximum number of digits and if we use user-defined labels now are no more
    hardcoded into the worksheet at evaluation time.
  * Anwers to Maxima's questions are now remembered across sessions.
  * CMake is now our main build system: It is supported under MSVC, Xcode,
    Code::Blocks, its output looks much nicer than the one from the autotools
    and setting it up uses much more straightforward constructs.
    The recipe to build a debian package using cmake can be found at
    https://code.launchpad.net/~peterpall/wxmaxima/packaging
  * Massive speed-ups
  * File/Open can now import .mac and .out files from Xmaxima
  * Many additional bug fixes and stability enhancements.

# 16.12
  * Not a change in wxMaxima, but useful: In Maxima >5.38 the load() command can
    load .wxm files like it would load .mac files.
  * Better detection and diagnosis for Maxima process that terminate unexpectedly
    even if the OS fails to notify us that the network connection with
    Maxima has dropped.
  * Incremental search
  * Automatic line wrap
  * Bullet lists now use real bullets.
  * Markdown: Block quote support
  * A button that temporarily hides all code cells
  * Massive speedups in the drawing code.
  * Added a wxstatusbar() command that allows a long-ranging block() to send a string
    about its progress to the status bar.
  * Support for cells that are >5000 pixels wide
  * Better High-DPI support
  * A Kabyle Translation for users of wxWidgets >= 3.0.1
  * Holding the "evaluate" key now evaluates all cells of the document one-by-one
  * .wxm files now include image cells
  * Drag-and-drop now handles image cells
  * Copy bitmap now can generate higher-resolution bitmaps
  * The application we drag-and-drop to now can select a format to get the data in
    including RTF, MathML, wxm, bitmap and plain text.
  * A new "copy as plain text" right-click menu item that copies the input and
    output of a cell.
  * An all-new manual and improved documentation of the source code.
  * More unicode characters in the symbols pane
  * Several content assistent enhancements for platforms that support it.
  * Corrected a few scrolling bugs.
  * Shift+Click now selects text or cells.
  * Several small editor enhancements.
  * Even if we scrolled away from evaluation wxMaxima now scrolls back to the
    cell if an error occurs.
  * Integrals should now always be saved correctly.
  * Errors in xml from Maxima or in the output cells of .wxmx files now result in error
    messages, not in silently discarded math elements.
  * Many additional bug fixes and performance improvements.

# 16.04
  * A TeX-like subscript notation: A_1 is now by default shown as an A with
    an 1 as subscript
  * High-quality icons for high-dpi screens.
  * Some broken .wxmx files generated with wxMaxima 13.04 and older will now
    automatically be repaired
  * Tried to make communication with Maxima more stable
  * Debug: A monitor for the xml communication between Maxima and wxMaxima
  * Leading White-space in front of a character that has to be escaped in
    XML is no more ignored.
  * jpeg-encoded images are now no more converted to much larger png files
    on saving .wxmx files and on exporting.
  * Images will now actually be converted to the target format.
  * Bumped the minor version of the .wxmx format: Some old versions of
    wxMaxima will replace .jpg files embedded in .wxmx files by a
    placeholder on load
  * Increased both speed and memory-efficiency of image handling
  * Save the zoom factor at exit and reload it on opening a new file
  * An option that makes wxMaxima use user-defined labels instead of %o where an
    user-defined label exists. Works well together with unchecking the
    "export code cells" config item.
  * A separate text style for user-defined labels.
  * The "evaluate all cells" and "evaluate till here" now start with a fresh
    instance of Maxima
  * TeX export now should work with most unicode chars we have ESC sequences
    for.
  * An auto-repair functionality that repairs most .wxmx files wxMaxima 13.04
    was not able to open again.
  * A few new ESC sequences and markdown commands.
  * "#" now is shown as a "not equal" sign as a "not equal" sign already would
    be interpreted as "#".
  * Teach CCL on Windows how to deal with unicode variable names like
    ü and α. For SBCL the necessary change has been done on the Maxima side.
  * Sidebars for symbols
  * Printing text cells now works again
  * The <ESC> key now works as a shortcut that closes the search dialogue.
  * A copy-to-MathML feature
  * Many bug, usability and performance fixes
  * Most importantly: If it is a cursor it now blinks.

# 15.08
  * Compatibility with Maxima 5.37.0
  * MathJAX now provides scalable equations and extended drag-and-drop for the html export.
  * The table-of-contents-sidebar now shows the current cursor position
  * Fixed a few instances of cursors jumping out of the screen
  * Fixed a few instances of cursors jumping to the beginning or end of the worksheet
  * Better detection which cell Maxima is processing and if it still is doing so
  * Regression: Hiding the toolbar didn't work on some systems
  * Markdown support for <=, >=, <=>, <-, ->, <->, => and +/- symbols.
  * An option to not export Maxima's input as well as the output.
  * An option to use High-resolution bitmaps for the HTML export.
  * Images that are too big for the window now are displayed in a scaled-down version.
  * Fixed the support for out-of-tree-bulds that was broken in 15.04
  * Meaningful ALT texts for the HTML export to provide accessibility
  * An option to include the .wxmx file in the .html export
  * Performance fixes that are espectionally effective for MSW
  * Unification of some platform-specific code
  * bash autocompletion
  * A fourth sectioning level
  * Made entering uppercase greek letters easier and documented how to input special
	unicode symbols
  * Syntax highlighting in code cells
  * Automatic highlighting of text equal to the currently selected one.
  * A batch mode that pauses evaluation if Maxima asks a question.
  * A "halt on error" feature
  * Now evaluation of a new command is only triggered if evaluation of the last
	command has finished. This means that output from Maxima is always appended
	to the right cell.
  * Un-broke error and question handling for multiple commands per cell. Sincewe now send
	Maxima's input command-per-command this means a cell with multiple commands is no
	more evaluated faster than multiple cells with single commands each.
  * If ever a end-of-evaluation marker gets lost there is a new "trigger evaluation"
	menu entry in the Maxima menu.
  * On wxGtk autocompletion was replaced by a content assistant that is based on the
	surprisingly powerful autocompletion feature.
  * Ctrl+Tab now launches the autocompletion (or content assistant, if available)
  * Tab and Shift+Tab now indent and unindent regions.
  * Ctrl+Mouse wheel and Ctrl++/- now zoom in and out of the worksheet.
  * Allow Extending selection from part of a single cell to multiple cells.
  * A Autoindent functionality.

# 15.04.0
  * wxWidgets 3.0 is now a mandatory requirement
  * Various bugfixes
  * Loads of stability and performance fixes
  * Adjustable framerate for animations
  * A version-control friendly flavour of the wxmx format
  * A mimetype marker at the beginning of wxmx files
  * Better desktop integration
  * An offline manual
  * Autodetection of Maxima's working directory on Mac and Windows
  * Use gnuplot_postamble instead of gnuplot_preamble
  * Dropped the hard dependency of TeXinfo
  * Translation updates
  * LaTeX: Use centered dots for multiplications
  * LaTeX: Added an option to select if superscripts should be
    placed above or after subscripts
  * LaTeX: Allow the user to add additional commands to the preamble.
  * Export of animations to pdf (via a pdfTeX file run) and html
  * Now complex conjugates are drawn as overstrike text.
  * bumped the minor version number of .wxmx: overstrike text is
    a new feature and therefore a file containing it cannot be read by
    old wxMaxima versions.
  * An autosave functionality that makes Maxima work more like a mobile
    app whose documents are always saved.
  * A table-of-contents pane for faster navigation
  * It is now possible to scroll away from a running evaluation for
    arbitrary lengths of time and to choose to follow the evaluation
    process again.
  * Now TeX scales down images that are obviously too big for the page.
  * An undo for cell deletes and for adding cells.
  * Autocompletion for units from ezUnits
