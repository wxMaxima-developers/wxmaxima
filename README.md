[![Build Status](https://travis-ci.org/andrejv/wxmaxima.svg?branch=master)](https://travis-ci.org/andrejv/wxmaxima)

wxMaxima
========

wxMaxima is a document based interface for the computer algebra system
Maxima.  For more information about Maxima, visit
http://maxima.sourceforge.net/.  wxMaxima uses wxWidgets and runs
natively on Windows, X11 and Mac OS X.  wxMaxima provides menus and
dialogs for many common Maxima commands, autocompletion, inline plots
and simple animations. wxMaxima is distributed under the GPL license.

wxMaxima is included with the Windows and the Macintosh installer for
Maxima. Packages are also available for many Linux distributions. Screenshots
and documentation can be found at http://andrejv.github.io/wxmaxima/


Building wxMaxima from source
-----------------------------

To build wxMaxima from sources you need to have a C++ compiler and the
wxWidgets library installed.

### Compiling wxMaxima on Linux and Linux-like systems (Cygwin, MacPorts, Homebrew,...)

wxMaxima is built using the CMake build system.
The following steps will build and install wxMaxima using CMake:

    mkdir -p build
    cd build
    cmake ..
    cmake --build .
    sudo cmake --build . -- install

If you want to install into a special prefix (not `/usr/local`), use

    cmake -DCMAKE_INSTALL_PREFIX:PATH=/your/installation/prefix ..

in the cmake call above.

If you want to create binary packages (tar.gz, tar.bz2, DEB & RPM), you can
create them with:

    cmake --build . -- package


On Ubuntu or Debian the build prerequisites can be installed by doing
a

    sudo apt-get install build-essential libwxbase3.0-dev libwxgtk3.0-dev ibus-gtk ibus-gtk3 checkinstall gettext cmake

beforehand or (if apt-get is configured to load the source package
repositories and not only the binary packages) by the simpler

    sudo apt-get build-dep wxmaxima


### Compiling on Mac OS X

On Mac OS X you should install XCode. To build wxMaxima open the
Terminal application and follow the instructions for building with cmake.
It is recommended that you compile your own version of
wxMac. See the section about compiling wxWidgets.


### Compiling on Windows

On Windows install MinGW (https://sourceforge.net/projects/mingw/). In
the installation process make sure you select `g++`, `MSYS Basic
System` and `MinGW Developer ToolKit` in the `Select components` page
of the installer.  Then run the MinGW Shell and follow the
instructions for compiling wxWidgets and wxMaxima with cmake.


### Compiling wxWidgets on Mac OS X and Windows

Before compiling wxMaxima you need to compile the wxWidgets
library. Download the source (we recommend version 3.1 for Mac OS X),
unarchive and in the source directory execute

    mkdir build
    cd build
    cmake ..
    make

You do not need to install the library with `make install`. You will
need to specify a path to wx-config when configuring wxMaxima. There
are two files in `build/lib/wx/config`. The correct file to use is
`inplace-msw-ansi-release-static-3.1` on Windows and
`inplace-mac-unicode-release-static-3.1` on Mac OS X. You will also
need to copy the file `wxwin.m4` to `acinclude.m4` in the wxMaxima
source directory.

#### Mac OS X specific instructions

Building on Mac OS X sometimes requires additional arguments to the
configure script. To build a portable binary configure wxWidgets with

    ../configure --disable-shared ---with-libjpeg=builtin --with-libpng=builtin --with-regex=builtin --with-libtiff=builtin --with-zlib=builtin --with-expat=builtin --with-macosx-version-min=10.7

With these options the build process will take a little longer, but
the resulting binary will have less library dependencides.

#### Mac OS X specific instructions

To build an application bundle of wxMaxima on Mac OS X

    ./configure --with-wx-config=<path to wx-config> --with-macosx-version-min=10.7
    make
    make allmo
    make wxMaxima.app

Note that the version specified in `--with-macosx-version-min` should match the version
used when configuring wxWidgets.

#### Windows specific instructions

On Windows execute instead:

    ./configure --with-wx-config=<path to wx-config>
    make
    make allmo
    make wxMaxima.win

which builds the directory structure necessary for running wxMaxima. Note
that this structure might be lacking a few .dll files, depending on the
compiler. They will be named libstdc++-6.dll and libgcc_s_sjlj-1.dll or
similar.

It is also possible to tell the GNU C compiler to try to include all
necessary .dll files in wxMaxima.exe while compiling wxMaxima. In order to
make this work wxWidgets has to be compiled statically which allows to
include it directly into an .exe file:

    ./configure --with-wx-config=<path to wx-config> --enable-static-wx -enable-fullystatic
    make allmo
    make wxMaxima.win


The build system also offers a build target that creates a zip archive whose
contents is a self-contained wxMaxima installation that can be placed in the folder
maxima was installed in, again with the caveat of the .dll files:

    make wxMaxima.win.zip

Additional information about installing and configuring wxMaxima
----------------------------------------------------------------
 - Mac OS X: https://themaximalist.org/about/my-mac-os-installation/
