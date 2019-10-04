# wxMaxima 19.10.0 - Stable
  * Corrected a Regression: The better display of exponents failed for exp(x^2).
  * Seems like some Windows computers only have a numpad enter key.
  * Chinese translation updates from liulitchi.

# wxMaxima 19.09.1 - Stable
  * The names of greek letters are no more converted to greek letters in variable
	names by default
  * Smarter Formatting of asterisk and hyphen
  * An automatic per-commit Windows Build for Appveyor
  * Always recalculate the worksheet size when needed
  * Better vertical alignment of exponents
  * The layout is now more consigstent between platforms
  * Chinese translation updates from liulitchi
  * wxMaxima now remembers the question for each answer, not only its number
  * Now the manual can be localized, too
  * Non-breaking spaces are now replaced by ordinary spaces
    before being sent to maxima
  * As always: Many additional bug fixes

# wxMaxima 19.09.0 - Stable
  * Handle timeouts when writing to maxima and partial writes.
  * Worksheet text disappeared on scrolling on some platforms.
  * Sped up sending code to maxima.
  * Removed quoting of Unicode characters within strings sent to maxima.

# wxMaxima 19.08.0 - Stable
  * Handle timeouts when writing to maxima and partial writes.
  * Worksheet text disappeared on scrolling on some platforms.
  * Sped up sending code to maxima.
  * Removed quoting of Unicode characters within strings sent to maxima.

# wxMaxima 19.07.1 - Stable
  * Bugfixes for nearly every instance something was drawn on the screen.
  * Better handling of multi-cell lisp code

# wxMaxima 19.07.0 - Stable
 * Many improvements and bugfixes in the LaTeX and HTML export function
 * Many updated translations
 * Resolved a freeze on autocompletion
 * A sidebar that shows the contents of variables
 * Simplified the recalculation logic in order to squash the remaining bugs
 * Simplified the sync between input and output cells

# wxMaxima 19.05.7 - Stable
 * In conjunction with a new enough maxima worksheets can now start
   with a :lisp command.
 
# wxMaxima 19.05.6 - Stable
 * Correctly save the autosave interval
 * A better logic for saving on exit if autosave is on and the file 
   cannot be created.
 
# wxMaxima 19.05.5 - Stable
 * An updated hungarian translation
 * The new maxima command cartesian_product_list is now known to
   the autocompletion.
 * Better xim compatibility.

# wxMaxima 19.05.4 - Stable
 * Corrected saving formatting styles in the config dialogue
 * Allow to change the title etc. font again
 * Correctly output uppercase greek letters in TeX
 * Re-enable the old-style ESC commands.
 * Made wxMaxima default to auto-searching for the maxima binary.

# wxMaxima 19.05.3 - Stable
 * Corrected the autowrap line width for high zoom factors
 * Added a few missing "Update the user interface" events

# wxMaxima 19.05.2 - Stable
 * Sometimes the auto-evaluation of the worksheet auto-triggered
 * Support for broken locales that for valid non-ascii chars returned
   isprint()= false
 * Corrected reading font sizes from styles and configuring styles.

# wxMaxima 19.05.1 - Stable
 * Fixes for various combinations of GTK and wxWidgets versions
 * A working Mac version
 * A few Windows bug Fixes
 * Big performance improvements.

# wxMaxima 19.05.0 - Stable
 * Another ArchLinux displaying issue
 * More GTK3 compatibility
 * Resolved a windows hang
 * Better Dark Mode compatibility
 * Performance improvements and bugfixes
 * Many code optimizations.

# wxMaxima 19.04.3 - Stable
 * wxMaxima now informs maxima which front-end is in use.

# wxMaxima 19.04.2 - Stable
 * Corrected the size of error messages
 * A "Copy to mathlab"-feature.
 * Maxima now delays interpreting the data from maxima until it encounters
   a newline or an Timer expires.
 * EMF output no no more causes crashes and strange behaviour.
 * RTF output should now work again
 * entermatrix() now works again.
 * Better help file detection on MS Windows.

# wxMaxima 19.04.1 - Stable
 * Corrected the size of error messages
 
# wxMaxima 19.04.0 - Stable

 * The cursor width now is taken from the current screen
 * Autocompletion sometimes cleared the result after inputting it
 * Added more commands to autocompletion
 * Spanish and italian translation updates
 * We no more need to re-implement maxima's load() routine.
   Instead we call the original one now.
 * Animations now on load remember which slide they stopped at.
 * A more error-proof language selection.
 * If the system looks like it uses UTF8 by default the locale name
   that is passed to maxima now ends in ".UTF8" which should resolve
   the "setting locale failed" errors from maxima.
 * Corrected the initial size of error messages.
 * Many additional bug fixes
 * Code cleanups


# wxMaxima 19.03.1 - Stable

 * HighDPI fixes for ArchLinux, Windows and SuSE
 * Resolved crashes on RedHat and MS Windows
 * SBCL's compilation messages now appear in the status bar, not
   in the worksheet.
 * Removed an unneeded "maxima has finished calculating" on startup.
 * Many small bug fixes.
 * wxWidgets >= 3.1.0: Corrected the toolbar icon size.

# wxMaxima 19.03.0 - Stable

 * The ESC sequences for inputting symbols now use autocompletion.
 * Use wxAutoBufferedPaintDC for drawing the worksheet instead of
   implementing our own version of it.
 * The usual bug fixes, this time including one bug fix that makes
   wxMaxima work again on Gentoo and help path lookup fixes for
   different operating systems.

# wxMaxima 19.02.2 - Stable

 * Corrected the GTK3 fix
 * Cleaned up the cleanup process on closing wxMaxima

# wxMaxima 19.02.1 - Stable

* Support for the broken ArchLinux compiler
* Resolved the Crash on Closing for Mac Os
* A "File open" bug that was depending on the language setting
* Corrected scrolling on wxWidgets 3.1.3 for GTK3

# wxMaxima 19.02.0 - Stable

* Resolved a cell size recalculation bug
* Better wxWidgets 3.1.2 compatibility
* Improved performance

# wxMaxima 19.01.3 - Stable

* Repaired compilation on MacOs for wxWidgets 3.1.1.
* Better path detection on MS Windows
* The "-m" switch needed for the appimage

# wxMaxima 19.01.2 - Stable

* Corrected the calculation of image heights
* Corrected the line break algorithm for printing.
