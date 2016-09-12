[![Build Status](https://travis-ci.org/andrejv/wxmaxima.svg?branch=master)](https://travis-ci.org/andrejv/wxmaxima)

wxMaxima
========

wxMaxima is a document based interface for the computer algebra system
Maxima.  For more information about Maxima, visit
http://maxima.sourceforge.net/.  wxMaxima uses wxWidgets and runs
natively on Windows, X11 and Mac OS X.  wxMaxima provides menus and
dialogs for many common maxima commands, autocompletion, inline plots
and simple animations. wxMaxima is distributed under the GPL license.

wxMaxima is included with the Windows and the macintosh installer for
Maxima. Packages are also available for many Linux distributions. Screenshots
and documentation can be found at http://andrejv.github.io/wxmaxima/;
If you wish to compile wxMaxima from source, please read the instructions below.


Building wxMaxima from source
-----------------------------

To build wxMaxima from sources you need to have a C++ compiler and the
wxWidgets library installed.


### Compiling on Mac OS X

On Mac OS X you should install XCode. To build wxMaxima open the
Terminal application and follow the instructions for building with GNU
autotools.  It is recommended that you compile your own version of
wxMac. See the section about compiling wxWidgets.


### Compiling on Windows

On Windows install MinGW (http://sourceforge.net/projects/mingw/). In
the installation process make sure you select `g++`, `MSYS Basic
System` and `MinGW Developer ToolKit` in the `Select components` page
of the installer.  Then run the MinGW Shell and follow the
instructions for compiling wxWidgets and wxMaxima with autotools.


### Compiling wxWidgets on Mac OS X and Windows

Before compiling wxMaxima you need to compile the wxWidgets
library. Download the source, unarchive and in the source directory
execute

    mkdir build
    cd build

On Mac OS X configure wxWidgets with

    ../configure --disable-shared --enable-unicode

and on Windows with

    ../configure --disable-shared

Now build wxWidgets with

    make

You do not need to install the library with `make install`. You will
need to specify a path to wx-config when configuring wxMaxima. There
are two files in `build/lib/wx/config`. The correct file to use is
`inplace-msw-ansi-release-static-3.0` on Windows and
`inplace-mac-unicode-release-static-3.0` on Mac OS X. You will also
need to copy the file `wxwin.m4` to `acinclude.m4` in the wxMaxima
source directory.


### Compiling with autotools

If you are not building an official tarball but using the git version it
is necessary to execute `./bootstrap` first in order to get the file
./configure

To build wxMaxima on Linux execute

    ./configure
    make
    make allmo
    sudo make install

You can also try to create a .deb package instead of installing the
program by doing a

    ./configure
    make
    make allmo
    checkinstall -D make install

On ubuntu or debian the build prerequisites can be installed by doing
a

    sudo apt-get install build-essential libwxbase3.0-dev libwxgtk3.0-dev autoconf imagemagick ibus-gtk ibus-gtk3

beforehand or (if apt-get is configured to load the source package
repositories and not only the binary packages) by the simpler

    sudo apt-get build-dep wxmaxima

To build an application bundle of wxMaxima on Mac OS X

    ./configure --with-wx-config=<path to wx-config>
    make
    make allmo
    make wxMaxima.app

Sometimes the configure step requires an extra
`--with-macosx-version-min=10.5` argument.

On Windows execute instead:

    ./configure --with-wx-config=<path to wx-config> --with-hhc=<path to hhc.exe> --enable-chm
    make
    make allmo
    make wxMaxima.win

which builds the directory structure necessary for running wxMaxima.
Alternatively

    make wxMaxima.win.zip

will build the whole application as a zip archive whose contents is a self-contained wxMaxima
installation that can be placed in the folder maxima was installed in.

The `--enable-chm` and the `--with-hhc` are only necessary to allow the
builder to convert the wxMaxima offline manual to a format the
built-in help browser of windows understands. For this conversion
the Microsoft HTML Help workshop is necessary which 
is distributed separately. If they aren't added to the configure
command line wxMaxima is shipped with a html version of the manual
that can be viewed using the internet browser instead.
