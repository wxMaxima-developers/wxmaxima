Building wxMaxima from source
-----------------------------

To build wxMaxima from sources you need to have a C++ compiler,
wxWidgets >= 3.0 (including development headers) and
CMake >= 3.7 installed.

### Compiling wxMaxima on Linux and Linux-like systems (Cygwin, MacPorts, Fink, Homebrew,...)

wxMaxima is built using the CMake build system.

The following steps will build and install wxMaxima using CMake.
Assume you start inside wxmaxima source tree root folder. Then:

        mkdir -p ../build-wxm           # create a build directory
        cd ../build-wxm                 # change to the build directory
        cmake ../wxmaxima               # configure the build
                                        # ../wxmaxima is the path to the wxMaxima sources.
        cmake --build .                 # build wxMaxima
                                        # You can speed up the build, if you
                                        # have multiple CPU cores, using
                                        # cmake --build . -- -j 4
                                        # (-j 4 means: Use 4 CPU cores).
                                        # Adjust the number for your CPU.
        sudo cmake --build . -- install # install it

If you want to install into a special prefix (not `/usr/local`), add
`-DCMAKE_INSTALL_PREFIX=/your/installation/prefix` to the first
cmake invocation. E.g.

    cmake -DCMAKE_INSTALL_PREFIX=/opt/wxmaxima ../wxmaxima

Of course you can use the CMake GUI (`cmake-gui`) or curses based CMake
(`ccmake`) for the configure step and change some CMake variables.

If you want to create binary packages (tar.gz, tar.bz2, DEB & RPM, on MacOs
also .dmg), the following command will create them:

    cmake --build . -- package


#### Mac Os: Creating a portable .apk and .dmg installer

This requires wxWidgets to be installed, e.G. using macports

    mkdir ../build-wxm
    cmake -S . -B ../build-wxm -DCMAKE_INSTALL_PREFIX=.
    cmake --build ../build-wxm
    cmake --build ../build-wxm --install

#### Ubuntu or Debian build prerequisites

    sudo apt-get install build-essential libwxbase3.0-dev libwxgtk3.0-dev ibus-gtk ibus-gtk3 checkinstall gettext cmake pandoc po4a

beforehand or (if apt-get is configured to load the source package
repositories and not only the binary packages) by the simpler

    sudo apt-get build-dep wxmaxima

#### CentOS and Redhat build prerequisites

CentOS / Redhat have rather old versions of CMake and wxWidgets installed,
install `cmake3` and `wxGTK3-devel` from the "Extra Packages for Enterprise Linux (EPEL)"
repository, to compile wxMaxima. (and use `cmake3` instead of `cmake` to call
the newer version of CMake in the commands above).

#### Mac OS X prerequisites

On Mac OS X you most probably need the command-line compiler one can tell
Xcode to install. Additionally wxWidgets needs to be installed, which can
be done using homebrew, fink or macports and should be named wxWidgets or
wxMac there.

Additional information about building on MacOS:

- https://themaximalist.org/about/my-mac-os-installation/


### Compiling on Windows

You can install MinGW (https://sourceforge.net/projects/mingw/). In
the installation process make sure you select `g++`, `MSYS Basic
System` and `MinGW Developer ToolKit` in the `Select components` page
of the installer. Also select po4a, cmake and install pandoc.
Then run the MinGW Shell and follow the instructions for compiling
wxWidgets and wxMaxima with cmake.

Another compiler option would be MinGW-w64 (http://www.mingw-w64.org/).

You can also build using MS Visual Studio 2019 or newer. Since MSVS
provides adequate versions of both cmake and ninja, there's no need
to install those. The build has to start from within the  *x64 Native
Tools Command Prompt*, and you have to first build wxWidgets before
you build wxmaxima. You can get some inspiration about how to do both
of these steps by examining the PowerShell scripts that do the building
within `.appveyor.yml` file.


### Note for the Mingw-w64 compiler and crosscompiler

These compilers come with two forms of thread support - win32 and posix.
The win32 thread version does not support std::thread, which wxMaxima
uses for multithreading, so be sure to install or reconfigure the
posix version.

On Ubuntu Linux the crosscompiler `x86_64-w64-mingw32-g++` can be
reconfigured using:

    update-alternatives --config x86_64-w64-mingw32-g++

(For 32 bit the same works for the 32 bit crosscompiler `i686-w64-mingw32-g++`.)

### Documentation of the source

An HTML version of wxMaxima's source code documentation can be found at
https://www.peterpall.de/wxMaxima/html/index.xhtml

A local version of the documentation of wxMaxima's source can be
generated using doxygen and the `doxygen` target:

    cmake --build ../build-wxm -- doxygen


Additional information for packagers
------------------------------------

### Running the test suite on a virtual machine

On Linux, wxMaxima's test suite requires a display to work with.
Since you likely don't want the test suite to pollute your screen,
or may even be running a headless virtual machine without an X server,
the `xvfb-run` wrapper should be used:

    xvfb-run ctest

On Mac OS or Windows, X displays aren't used, and the test suite can be
run directly:

    ctest

### Creating a standalone wxMaxima

wxMaxima, if linked statically, is pretty standalone and therefore fit for
creating a portable app:

 * It only depends on one single library: wxWidgets, that can be linked
   statically,
 * And besides a working maxima installation it only attempts to use two
   sets of external files: The manual and the translation files.

If a wxMaxima install should come without manual or translation files that
might not be the end of the world, neither:

 * If the translation files are not found, the program will still work,
   but will be using english.
 * And if there are no manual files wxMaxima will redirect the system's web
   browser to an online version of the manual, instead.

