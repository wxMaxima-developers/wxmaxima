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
