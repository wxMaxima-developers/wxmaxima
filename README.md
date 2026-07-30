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

## Note concerning Wayland (recent Linux/BSD distributions)

There seem to be issues with the Wayland Display Server and wxWidgets.
WxMaxima may be affected, e.g. that sidebars are not moveable.

You can either disable Wayland and use X11 instead (globally)
or just tell, that wxMaxima should use the X Window System by setting:
`GDK_BACKEND=x11`

E.g. start wxMaxima with:

`GDK_BACKEND=x11 wxmaxima`

(For example, Ubuntu 22.04 (with a Wayland capable graphics card) is affected.)

## Status

|Architecture|OS|Test|Status|
|---|---|---|---|
|AMD64+ARM64|Ubuntu|Update of the snappy package of maxima|[![build_maxima_snap](https://github.com/wxMaxima-developers/wxmaxima/actions/workflows/build_maxima_snap.yml/badge.svg)](https://github.com/wxMaxima-developers/wxmaxima/actions/workflows/build_maxima_snap.yml)|
|AMD64+ARM64|Ubuntu|Update of the snappy package/edge channel|[![build_snap](https://github.com/wxMaxima-developers/wxmaxima/actions/workflows/build_snap.yml/badge.svg)](https://github.com/wxMaxima-developers/wxmaxima/actions/workflows/build_snap.yml)|
|AMD64+ARM64|Ubuntu|Update of the snappy package/stable channel|[![build_snap_stable](https://github.com/wxMaxima-developers/wxmaxima/actions/workflows/build_snap_stable.yml/badge.svg)](https://github.com/wxMaxima-developers/wxmaxima/actions/workflows/build_snap_stable.yml)|
|AMD64|Ubuntu|Fuzzing of file and keyboard input|[![fuzz](https://github.com/wxMaxima-developers/wxmaxima/actions/workflows/fuzz.yml/badge.svg)](https://github.com/wxMaxima-developers/wxmaxima/actions/workflows/fuzz.yml)|
|AMD64|Ubuntu|Coverity code scan|[![coverity_scan](https://github.com/wxMaxima-developers/wxmaxima/actions/workflows/coverity-scan.yml/badge.svg)](https://github.com/wxMaxima-developers/wxmaxima/actions/workflows/coverity-scan.yml)|
|AMD64|Ubuntu|Check: Is MathJaX current?|[![check_mathjax_version](https://github.com/wxMaxima-developers/wxmaxima/actions/workflows/check_mathjax_version.yml/badge.svg)](https://github.com/wxMaxima-developers/wxmaxima/actions/workflows/check_mathjax_version.yml)|
|AMD64|Ubuntu|CodeQL code scan|[![CodeQL](https://github.com/wxMaxima-developers/wxmaxima/actions/workflows/codeql-analysis.yml/badge.svg)](https://github.com/wxMaxima-developers/wxmaxima/actions/workflows/codeql-analysis.yml)|
|AMD64|Windows|Compile using Cygwin|[![compile_cygwin](https://github.com/wxMaxima-developers/wxmaxima/actions/workflows/compile_cygwin.yml/badge.svg)](https://github.com/wxMaxima-developers/wxmaxima/actions/workflows/compile_cygwin.yml)|
|ARM64|Mac OS|Compile on the mac|[![compile_mac](https://github.com/wxMaxima-developers/wxmaxima/actions/workflows/compile_mac.yml/badge.svg)](https://github.com/wxMaxima-developers/wxmaxima/actions/workflows/compile_mac.yml)|
|AMD64|Ubuntu|Compile and test under Ubuntu|[![compile_ubuntu](https://github.com/wxMaxima-developers/wxmaxima/actions/workflows/compile_ubuntu.yml/badge.svg)](https://github.com/wxMaxima-developers/wxmaxima/actions/workflows/compile_ubuntu.yml)|
|IA32|Ubuntu|Compile under Ubuntu|[![compile_ubuntu_32bit](https://github.com/wxMaxima-developers/wxmaxima/actions/workflows/compile_ubuntu_32bit.yml/badge.svg)](https://github.com/wxMaxima-developers/wxmaxima/actions/workflows/compile_ubuntu_32bit.yml)|
|AMD64|Windows|Compile and test under Windows|[![compile_windows](https://github.com/wxMaxima-developers/wxmaxima/actions/workflows/compile_windows.yml/badge.svg)](https://github.com/wxMaxima-developers/wxmaxima/actions/workflows/compile_windows.yml)|
|AMD64|Ubuntu|Coverity Scan|[![coverity_scan](https://github.com/wxMaxima-developers/wxmaxima/actions/workflows/coverity-scan.yml/badge.svg)](https://github.com/wxMaxima-developers/wxmaxima/actions/workflows/coverity-scan.yml)|
|AMD64|Ubuntu|Flawfinder|[![flawfinder](https://github.com/wxMaxima-developers/wxmaxima/actions/workflows/flawfinder.yml/badge.svg)](https://github.com/wxMaxima-developers/wxmaxima/actions/workflows/flawfinder.yml)|
|AMD64|Ubuntu|Microsoft C++ Code analysis|[![Microsoft C++ Code Analysis](https://github.com/wxMaxima-developers/wxmaxima/actions/workflows/msvc.yml/badge.svg)](https://github.com/wxMaxima-developers/wxmaxima/actions/workflows/msvc.yml)|
