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

    ../configure --disable-shared --enable-unicode --with-macosx-version-min=10.9 --disable-mediactrl

and on Windows with

    ../configure --disable-shared

Now build wxWidgets with

    make

You do not need to install the library with `make install`. You will
need to specify a path to wx-config when configuring wxMaxima. There
are two files in `build/lib/wx/config`. The correct file to use is
`inplace-msw-ansi-release-static-3.1` on Windows and
`inplace-mac-unicode-release-static-3.1` on Mac OS X. You will also
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

    sudo apt-get install build-essential libwxbase3.0-dev libwxgtk3.0-dev autoconf imagemagick ibus-gtk ibus-gtk3 checkinstall gettext

beforehand or (if apt-get is configured to load the source package
repositories and not only the binary packages) by the simpler

    sudo apt-get build-dep wxmaxima

To build an application bundle of wxMaxima on Mac OS X

    ./configure --with-wx-config=<path to wx-config>
    make
    make allmo
    make wxMaxima.app

Sometimes the configure step requires an extra
`--with-macosx-version-min=10.9` argument. Also in order to create a
application that runs on other computers, as well, sometimes .dylib
files may need to be copied into the Frameworks folder of the application
bundle wxMaxima.app

Note: libtiff depends on liblzma.


Tomio Arisaka has found out a way to make wxMaxima independent of the shared library
which is not included in MacOS X:
    (1) Copy the shared library into the application bundle of wxMaxima.
    (2) Changes the dependent shared library install name old to new in the shared library of wxMaxima.
    (3) Changes the dependent shared library install name old to new in the executable file of wxMaxima.

(Note: I use "otool -L" to display the path name of the dependent shared library.)
--------------------------------------------------------------------------------
$ cp -iRp ./wxmaxima-15.08.2/wxMaxima.app ./
$ 
$ otool -L ./wxMaxima.app/Contents/MacOS/wxmaxima 
$ 
$ mkdir ./wxMaxima.app/Contents/MacOS/lib
$ 
$ pushd ./wxMaxima.app/Contents/MacOS/lib
$ 
$ cp -ip /opt/local/lib/libpng16.16.dylib ./
$ cp -ip /opt/local/lib/libjpeg.9.dylib ./
$ cp -ip /opt/local/lib/libtiff.5.dylib ./
$ cp -ip /opt/local/lib/libexpat.1.dylib ./
$ cp -ip /opt/local/lib/libz.1.dylib ./
$ cp -ip /opt/local/lib/libiconv.2.dylib ./
$ cp -ip /opt/local/lib/liblzma.5.dylib ./
$ 
$ otool -L ./libtiff.5.dylib 
$ 
$ install_name_tool -change /opt/local/lib/liblzma.5.dylib @executable_path/lib/liblzma.5.dylib libtiff.5.dylib
$ 
$ install_name_tool -change /opt/local/lib/libjpeg.9.dylib @executable_path/lib/libjpeg.9.dylib libtiff.5.dylib
$ 
$ install_name_tool -change /opt/local/lib/libz.1.dylib @executable_path/lib/libz.1.dylib libtiff.5.dylib
$ 
$ otool -L ./libtiff.5.dylib
$ 
$ otool -L ./liblzma.5.dylib 
$ 
$ install_name_tool -change /opt/local/lib/liblzma.5.dylib @executable_path/lib/liblzma.5.dylib liblzma.5.dylib
$ 
$ otool -L ./liblzma.5.dylib 
$ 
$ otool -L ./libpng16.16.dylib 
$ 
$ install_name_tool -change /opt/local/lib/libz.1.dylib @executable_path/lib/libz.1.dylib libpng16.16.dylib
$ 
$ otool -L ./libpng16.16.dylib 
$ 
$ pushd ..
$ 
$ install_name_tool -change /opt/local/lib/libpng16.16.dylib @executable_path/lib/libpng16.16.dylib wxmaxima
$ 
$ install_name_tool -change /opt/local/lib/libjpeg.9.dylib @executable_path/lib/libjpeg.9.dylib wxmaxima
$ 
$ install_name_tool -change /opt/local/lib/libtiff.5.dylib @executable_path/lib/libtiff.5.dylib wxmaxima
$ 
$ install_name_tool -change /opt/local/lib/libexpat.1.dylib @executable_path/lib/libexpat.1.dylib wxmaxima
$ 
$ install_name_tool -change /opt/local/lib/libz.1.dylib @executable_path/lib/libz.1.dylib wxmaxima
$ 
$ install_name_tool -change /opt/local/lib/libiconv.2.dylib @executable_path/lib/libiconv.2.dylib wxmaxima
$ 
$ popd;popd
$ 
$ otool -L ./wxMaxima.app/Contents/MacOS/wxmaxima 



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