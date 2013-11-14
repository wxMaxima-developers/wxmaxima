wxMaxima
========

wxMaxima is a document based interface for the computer algebra system Maxima.
For more information about Maxima, visit http://maxima.sourceforge.net/.
wxMaxima uses wxWidgets and runs natively on Windows, X11 and Mac OS X.
wxMaxima provides menus and dialogs for many common maxima commands,
autocompletion, inline plots and simple animations. wxMaxima is distributed
under the GPL license.

wxMaxima is included with the Windows installer for Maxima. Packages are
available for many Linux distributions. If you wish to compile wxMaxima from
source, read the instructions below.


Building wxMaxima from source
-----------------------------

To build wxMaxima from sources you need to have a C++ compiler and the
wxWidgets library installed.


### Compiling on Mac OS X

On Mac OS X you should install XCode. To build wxMaxima open the Terminal
application and follow the instructions for building with GNU autotools.
It is recommended that you compile you own version of wxMac. See the section
about compiling wxWidgets.


### Compiling on Windows

On Windows install MinGW (http://sourceforge.net/projects/mingw/). In the
installation process make sure you select g++, "MSYS Basic System" and
"MinGW Developer ToolKit" in the "Select components" page of the installer.
Then run MinGW Shell and follow the instructions for compiling wxWidgets and
wxMaxima with autotools.


### Compiling wxWidgets on Mac OS X and Windows

Before compiling wxMaxima you need to compile the wxWidgets library. Download
the source, unarchive and in the source directory execute

    mkdir build
    cd build

On Mac OS X configure wxWidgets with

    ../configure --disable-shared --enable-unicode

and on Windows with

    ../configure --disable-shared

Now build wxWidgets with

    make

You do not need to install the library with `make install`. You will need to
specify a path to wx-config when configuring wxMaxima. There are two files in
`build/lib/wx/config`. The correct file to use is
`inplace-msw-ansi-release-static-2.8` on Windows and
`implace-mac-unicode-release-static-2.8` on Mac OS X. You will also need to copy
the file `wxwin.m4` to `acinclude.m4` in the wxMaxima source directory.


### Compiling with autotools

If you are building from git, execute `./bootstrap` first.

To build wxMaxima on Linux execute

    ./configure
    make
    cd locales
    make allmo
    cd ..
    sudo make install

To build wxMaxima on Mac OS X and Windows first execute

    ./configure --with-wx-config=<path to wx-config>
    make
    cd locales
    make allmo
    cd ..

On Mac OS X you should build an application bundle:

    make wxMaxima.app

On Windows execute

    make wxMaxima.win

which builds the directory structure necessary for running wxMaxima.
