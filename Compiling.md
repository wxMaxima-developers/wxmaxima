## Building wxMaxima from source

To build wxMaxima from sources you need to have a C++ compiler (C++14),
wxWidgets >= 3.0 (including development headers) and
CMake >= 3.16 installed.

### Compiling wxMaxima on Linux and Linux-like systems (Cygwin, MacPorts, Fink, Homebrew,...)

wxMaxima is built using the CMake build system.

The following steps will build and install wxMaxima using CMake.
Download and extract wxMaxima and change in the wxMaxima tree. Then:

```
    mkdir -p build          # create a build directory (may even be outside the wxmaxima code)
    cd build                # change to the build directory
    cmake ..                # configure the build.
                            # .. is the path to the extracted wxMaxima code - if you use another build directory, adjust.
    cmake --build .         # build wxMaxima
                            # You can speed up the build, if you have multiple CPU cores, using
                            # cmake --build . -- -j 4
                            # (-j 4 means: Use 4 CPU cores). Adjust the number for your CPU.
```

Now you can test your wxMaxima build (without installing it) by calling: `./wxmaxima-local`

If everything is okay install it with:
```
    sudo cmake --build . -- install
```

If you want to install into a special prefix (not `/usr/local`), add
`-DCMAKE_INSTALL_PREFIX=/your/installation/prefix` to the first
cmake invocation. E.g.

```
cmake -DCMAKE_INSTALL_PREFIX=/opt/wxmaxima ..
```

By default a "Debug" build will be made (less optimization, debug symbols) - and this build shows the
wxMaxima log window (may contain helpful log messages) when started; if you want to create a
"Release" build (more optimization, the log window is not shown by default, it can be toggled
on and off using "View->Toggle log window") set the build type (CMake supports other build types too) using:
```
cmake -DCMAKE_BUILD_TYPE=Release ..
```


Of course you can use the CMake GUI (`cmake-gui`) or curses based CMake
(`ccmake`) for the configure step and change some CMake variables.

If you want to create binary packages (tar.gz, tar.bz2, DEB & RPM, on macOS
also .dmg), the following command will create them:

```
cmake --build . -- package
```

To update the locale files (po and pot) in the source code:

```
cmake --build . -- update-locale
```

#### Testing wxMaxima

You can use (or test) a fresh compiled wxMaxima (without installation) by calling
`./wxmaxima-local` from the build-directory.

wxMaxima comes with several tests, which can be run using

```
ctest
```

from the build directory. If you want to run a single test, you can use the option `-R <name-of-the-test>`
when calling `ctest`. The tests (and their names) can be seen in the file `test/CMakeLists.txt` of the source code.

On Linux/Unix the tests usually require a X Server; if you start the tests without an X server (e.g.
automatic tests using cron, when doing tests using a continuous integration system, etc.)
you can use `xvfb-run` to create a virtual X Server environment, e.g. just run: `xvfb-run ctest`

#### macOS: Creating a portable .apk and .dmg installer

This requires wxWidgets to be installed, e.G. using MacPorts.
If you are in the extracted wxMaxima directory, call e.g.

```
mkdir ../build-wxm
cmake -S . -B ../build-wxm -DCMAKE_INSTALL_PREFIX=.
cmake --build ../build-wxm
cmake --build ../build-wxm --install
```

#### Ubuntu or Debian build prerequisites

```
sudo apt-get install build-essential libwxgtk3.2-gtk3-dev libwxgtk-webview3.2-gtk3-dev wx3.2-i18n ibus-gtk ibus-gtk3 checkinstall gettext cmake pandoc po4a
```

On Ubuntu < 24.04 install an older wxWidgets version (`libwxbase3.0-dev libwxgtk3.0-gtk3-dev libwxgtk-webview3.0-gtk3-dev` instead of the version 3.2).


beforehand or (if apt-get is configured to load the source package
repositories and not only the binary packages) by the simpler

```
sudo apt-get build-dep wxmaxima
```

You must use the apt-get install command when the version you are building has dependencies
which are not listed as dependencies in your distro's repository version of wxmaxima.


#### macOS X prerequisites

On macOS X you most probably need the command-line compiler one can tell
Xcode to install. Additionally wxWidgets needs to be installed, which can
be done using Homebrew, Fink or MacPorts and should be named wxWidgets or
wxMac there. XCode versions older than version 13.0 have problems compiling
wxMaxima.

Additional information about building on macOS:

- https://themaximalist.org/about/my-mac-os-installation/
- https://github.com/wxMaxima-developers/wxmaxima/issues/2026#issuecomment-3172804134

### Compiling on Windows


You can use the MinGW-w64 (https://www.mingw-w64.org/) compilers.

You can also build using MS Visual Studio 2019 or newer. Since MSVS
provides adequate versions of both cmake and ninja, there's no need
to install those. The build has to start from within the  *x64 Native
Tools Command Prompt*, and you have to first build wxWidgets before
you build wxmaxima. You can get some inspiration about how to do both
of these steps by examining the PowerShell scripts that do the building
within `.appveyor.yml` file.

#### Note for the Mingw-w64 compiler and crosscompiler

These compilers come with two forms of thread support - win32 and posix.
The win32 thread version does not support std::thread, which wxMaxima
uses for multithreading, so be sure to install or reconfigure the
posix version.

On Ubuntu Linux the crosscompiler `x86_64-w64-mingw32-g++` can be
reconfigured using:

```
update-alternatives --config x86_64-w64-mingw32-g++
```

(For 32 bit the same works for the 32 bit crosscompiler `i686-w64-mingw32-g++`.)

### Compiling for Android 9

Some information how to build for Android 9 can be found in the wxMaxima
discussion forum:
https://github.com/wxMaxima-developers/wxmaxima/discussions/1594

### Documentation of the source

An HTML version of wxMaxima's source code documentation can be found at
https://wxmaxima-developers.github.io/wxmaxima/Doxygen-documentation/

A local version of the documentation of wxMaxima's source can be
generated using doxygen and the `doxygen` target:

```
cmake --build ../build-wxm -- doxygen
```

### wxWidgets isn't found

CMake has currently (Feb 2022) problems finding a wxWidgets that was
compiled using CMake. If that is the reason maybe wxWidgets was compiled
without wxWebView. In that case

```
cmake -S . -B ../build-wxm -DWXM_DISABLE_WEBVIEW=on
```

might allow cmake to find wxWidgets at the cost of loosing the integrated
dockable help browser.

## Additional information for packagers

### Creating a standalone wxMaxima

wxMaxima, if linked statically, is pretty standalone and therefore fit for
creating a portable app:

- It only depends on one single library: wxWidgets, that can be linked
  statically,
- And besides a working maxima installation it only attempts to use two
  sets of external files: The manual and the translation files.

If a wxMaxima install should come without manual or translation files that
might not be the end of the world, neither:

- If the translation files are not found, the program will still work,
  but will be using english.
- And if there are no manual files wxMaxima will redirect the system's web
  browser to an online version of the manual, instead.

### Troubleshooting

If you have problems when compiling wxMaxima, maybe a look in the configurations
for various CI-Systems (`.appveyor.yml` and files in `.github/workflows/`)
may be helpful to see, how wxMaxima is compiled on these systems.
