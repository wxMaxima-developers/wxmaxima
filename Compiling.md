Building wxMaxima from source
-----------------------------

To build wxMaxima from sources you need to have a C++ compiler and the
wxWidgets library installed.

### Compiling wxMaxima on Linux and Linux-like systems (Cygwin, MacPorts, Fink, Homebrew,...)

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

    sudo apt-get install build-essential libwxbase3.0-dev libwxgtk3.0-dev ibus-gtk ibus-gtk3 checkinstall gettext cmake pandoc

beforehand or (if apt-get is configured to load the source package
repositories and not only the binary packages) by the simpler

    sudo apt-get build-dep wxmaxima


If in wxMaxima a few key combinations like TAB, Strg+"+" and Strg+"-" aren't
working a typical cause is that the keyboard driver, a window manager or a
similar entity suppresses them. In this case installing ibus normally resolves
the problem.


CentOS / Redhat have rather old versions of CMake and wxWidgets installed,
install "cmake3" and "wxGTK3-devel" from the "Extra Packages for Enterprise Linux (EPEL)"
repository, to compile wxMaxima. (and use "cmake3" instead of "cmake" to call
the newer version of CMake in the commands above).

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


Additional information about installing and configuring wxMaxima
----------------------------------------------------------------
 - Mac OS X: https://themaximalist.org/about/my-mac-os-installation/

### Documentation of the source

An html version of wxMaxima's documentation can be found at
https://www.peterpall.de/wxMaxima/html/index.xhtml

A local version of the documentation of wxMaxima's source can be
generated using doxygen and the

    make html

target.


Additional information for packagers
------------------------------------

On linux wxMaxima's "make test" target requires a way to connect to a
display - which by default doesn't exist on a build server. But a

    xvfb-run make test

might succeed.
