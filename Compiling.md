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

in the cmake call above. By default, wxMaxima tries to use OpenMP for
parallelization, if the compiler supports that. You can remove OpenMP
support with the option -DWXM_USE_OPENMP=NO in the cmake call.

If installed, wxMaxima can use cppcheck to check the source for bugs
during compilation. You can enable that with: -DWXM_USE_CPPCHECK=YES

If you want to create binary packages (tar.gz, tar.bz2, DEB & RPM, on MacOs
also .dmg), the following command will create them:

    cmake --build . -- package

At least CMake 3.7.0 is required to build wxMaxima. If your OS provides
an older version (e.g. Ubuntu 14.04) download a recent version from
https://cmake.org/download/ and use that.

#### Mac Os: Creating a portable .apk and .dmg installer

Just replace the 

	cmake ..

by a

	cmake -DMACOSX_BUNDLE=YES ..

#### Ubuntu or Debian the build prerequisites

    sudo apt-get install build-essential libomp-dev libwxbase3.0-dev libwxgtk3.0-dev ibus-gtk ibus-gtk3 checkinstall gettext cmake pandoc po4a

beforehand or (if apt-get is configured to load the source package
repositories and not only the binary packages) by the simpler

    sudo apt-get build-dep wxmaxima

#### CentOS and Redhat build prerequisites
CentOS / Redhat have rather old versions of CMake and wxWidgets installed,
install "cmake3" and "wxGTK3-devel" from the "Extra Packages for Enterprise Linux (EPEL)"
repository, to compile wxMaxima. (and use "cmake3" instead of "cmake" to call
the newer version of CMake in the commands above).

#### Compiling on Mac OS X prerequisites

On Mac OS X you most probably need the command-line compiler one can tell 
Xcode to install. Additionally wxWidgets needs to be installed, which can
be done using homebrew, fink or macports and should be named wxWidgets or
wxMac there. If libomp is installed, as well, wxMaxima is able to speed
up some more tasks using multithreading.

### Compiling on Windows

On Windows install MinGW (https://sourceforge.net/projects/mingw/). In
the installation process make sure you select `g++`, `MSYS Basic
System` and `MinGW Developer ToolKit` in the `Select components` page
of the installer. Also select po4a, cmake, openmp (if openmp is 
available) and install pandoc.
Then run the MinGW Shell and follow the instructions for compiling
wxWidgets and wxMaxima with cmake.


Additional information about installing and configuring wxMaxima
----------------------------------------------------------------
 - Mac OS X: https://themaximalist.org/about/my-mac-os-installation/

### Documentation of the source

An html version of wxMaxima's documentation can be found at
https://www.peterpall.de/wxMaxima/html/index.xhtml

A local version of the documentation of wxMaxima's source can be
generated using doxygen and the

    make doxygen

target.


Additional information for packagers
------------------------------------

### Running the test suite on a virtual machine

On Linux wxMaxima's "make test" target requires a way to connect to a
display - which by default doesn't exist on a build server. The 
following command should succeed even there, though:

    xvfb-run make test

If anyone has an idea on how to do a similar thing on a Mac I would
be very thankful.

### Creating a standalone wxMaxima

wxMaxima, if linked statically, is pretty standalone and therefore fit for
creating a portable app: The only external file wxMaxima requires 
(besides a working maxima installation) is the wxMaxima help file and if 
this help file cannot be found it will instruct the web browser to open 
the online version of the manual instead.
