# WxMaxima

WxMaxima is a document based interface for the computer algebra system
Maxima.  For more information about Maxima, visit
<https://maxima.sourceforge.io/>.  WxMaxima uses wxWidgets and runs
natively on Windows, macOS, Linux and other platforms.
WxMaxima provides menus and dialogs for many common Maxima commands,
autocompletion, inline plots and simple animations.
WxMaxima is distributed under the GPL license.

WxMaxima is included with the Windows and the Macintosh installer for
Maxima. Packages are also available for many Linux distributions. Screenshots
and documentation can be found at <https://wxmaxima-developers.github.io/wxmaxima>/

Instructions on where to get a complete Maxima package from
can be found at <https://maxima.sourceforge.io/download.html>.

Information on how to compile wxMaxima from source instead can be
found at <https://github.com/wxMaxima-developers/wxmaxima/blob/main/Compiling.md>.
The documentation for the code itself can be found here instead: 
<https://wxmaxima-developers.github.io/wxmaxima/Doxygen-documentation/>

WxMaxima is an open source project developed by volunteers and your
contributions are always welcome.

The wxMaxima Team

## Note concering Wayland (recent Linux/BSD distributions)

There seem to be issues with the Wayland Display Server and
not really recent wxWidgets version. WxMaxima may be affected,
e.g. that sidebars are not moveable.

You can either disable Wayland and use X11 instead (globally)
or just tell, that wxMaxima should use the X Window System by setting:
`GDK_BACKEND=x11`

E.g. start wxMaxima with:

`GDK_BACKEND=x11 wxmaxima`

(For example Ubuntu 22.04 (with a Wayland capable graphics card) is affected.)
