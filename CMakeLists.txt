# -*- mode: CMake; cmake-tab-width: 4; -*-

cmake_minimum_required(VERSION 3.16)

if (POLICY CMP0048)
    cmake_policy(SET CMP0048 NEW)
endif (POLICY CMP0048)

# Note: The VERSION in the project command should only contain digits and periods.
# I it used for Mac Info.plist and there this is a requirement.
project(wxMaxima
        DESCRIPTION "wxMaxima is a document based interface for the computer algebra system Maxima."
        HOMEPAGE_URL "https://wxmaxima-developers.github.io/wxmaxima/"
        VERSION 26.01.0
        LANGUAGES CXX
       )

add_compile_options("$<$<C_COMPILER_ID:MSVC>:/utf-8>")
add_compile_options("$<$<CXX_COMPILER_ID:MSVC>:/utf-8>")

# define C++ macro DEBUG=1 if CMAKE_BUILD_TYPE=Debug
add_compile_definitions("DEBUG=$<CONFIG:Debug>")

# Set the locale to default C to prevent issues due to localization of commands.
# This is necessary as we call commands like "po4a --version"
set(ENV{LANG} C)

if(WIN32)
    enable_language(RC)
endif()

# if we build a "Release", do not add "-DevelopmentSnapshot", otherwise do that.
if(CMAKE_BUILD_TYPE STREQUAL "Release")
        set(WXMAXIMA_VERSION ${PROJECT_VERSION})
else()
        set(WXMAXIMA_VERSION "${PROJECT_VERSION}-DevelopmentSnapshot")
endif()

# Set a CACHE variable - make it overrideable via command line.
set(CMAKE_CXX_STANDARD 20 CACHE STRING "The target C++ standard. wxMaxima requires C++14 or higher.")
if(CMAKE_CXX_STANDARD EQUAL 98 OR CMAKE_CXX_STANDARD EQUAL 11)
    message(FATAL_ERROR "At least C++14 is required to build wxMaxima.")
endif()
set(MACOSX_BUNDLE_COPYRIGHT "GPL2+")
set(MACOSX_BUNDLE_BUNDLE_VERSION ${PROJECT_VERSION})
set(MACOSX_BUNDLE_BUNDLE_NAME "wxMaxima")
# set(MACOSX_BUNDLE_GUI_IDENTIFIER "wxMaxima")
##
# Options
#

option(WXM_DISABLE_WEBVIEW
    "Disable webview for https://trac.macports.org/ticket/62904" OFF)
option(WXM_ENABLE_PRECOMPILED_HEADERS
    "Enable precompiled headers to potentially speed up compilation." OFF)
option(WXM_USE_CPPCHECK
    "Perform CPPCHECK during compilation." OFF)
option(WXM_UNIT_TESTS
    "Compile unit tests and enable the tests." OFF)
option(WXM_INTERPROCEDURAL_OPTIMIZATION
    "Enable interprocedural optimization (IPO/LTO)." OFF)
option(WXM_PO4A_UPDATE_MANUAL_PO_POT_FILES
    "Update the po/pot locale files in the source tree." OFF)
option(WXM_MAXIMA_DEMO_TESTS
    "Run Maxima demo files as test." OFF)

if(DEFINED MACOSX_VERSION_MIN)
    set(CMAKE_OSX_DEPLOYMENT_TARGET ${MACOSX_VERSION_MIN} CACHE STRING FORCE)
    unset(MACOSX_VERSION_MIN)
    unset(MACOSX_VERSION_MIN CACHE)
    message(WARNING "MACOSX_VERSION_MIN is deprecated. Use CMAKE_OSX_DEPLOYMENT_TARGET instead")
endif()
if(DEFINED USE_CPPCHECK)
    set(WXM_USE_CPPCHECK ${USE_CPPCHECK} FORCE)
    unset(USE_CPPCHECK)
    unset(USE_CPPCHECK CACHE)
    message(WARNING "USE_CPPCHECK is deprecated. Use WXM_USE_CPPCHECK instead")
endif()


##
# CMake Policies
#

# Better clang support
if(POLICY CMP0025)
cmake_policy(SET CMP0025 NEW)
endif()
# Avoid a warning by deciding which version of this policy we prefer.
if(POLICY CMP0066)
    cmake_policy(SET CMP0066 NEW)
endif()

##
# Compiler-Specific Tweaks
#

if(MSVC)
    # Turn off many warnings wxWidgets triggers on MSVC
    add_definitions(-D_CRT_SECURE_NO_WARNINGS)
    # Safely handle exceptions everywhere
    add_compile_options(/EHsc)
endif()

if(MSYS OR MINGW)
    add_compile_options(-Wa,-mbig-obj)
endif()

##
# Environment Check
#

# Set a default build type if none was specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    message(STATUS "Setting build type to 'Debug' as none was specified.")
    set(CMAKE_BUILD_TYPE Debug CACHE STRING "Choose the type of build." FORCE)
    set(CMAKE_INSTALL_DEBUG_LIBRARIES 1)
    # Set the possible values of build type for cmake-gui
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

# Set the install configuration the same as the build type (for cpack)
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
    set(CMAKE_INSTALL_CONFIG_NAME "${CMAKE_BUILD_TYPE}")
endif()

get_filename_component(srcdir "${CMAKE_SOURCE_DIR}" REALPATH)
get_filename_component(bindir "${CMAKE_BINARY_DIR}" REALPATH)
if("${srcdir}" STREQUAL "${bindir}")
    message(WARNING
        "In-source builds are strongly deprecated. "
        "Instead, build the project out-of source: Create a "
        "separate directory for the build *outside of the source folder*, and run "
        "cmake <path to the source dir> and build from there.")
endif()

##
# Dependencies/Packages
#

include(GNUInstallDirs)

message(STATUS "CMake version:                      ${CMAKE_VERSION}")
message(STATUS "CMake build type:                   ${CMAKE_BUILD_TYPE}")
message(STATUS "wxWidgets config:                   ${wxWidgets_CONFIGURATION}")
message(STATUS "Looking for wxWidgets in:           ${wxWidgets_ROOT_DIR}")
message(STATUS "Looking for wxWidgets libraries in: ${wxWidgets_LIB_DIR}")

# The package order below matters.
###
###  Incorrect order WILL BREAK Unix and MinGW STATIC BUILDS.
###
# The correct order is a topological sort based on the dependency graph
# here: https://docs.wxwidgets.org/3.0/page_libs.html
# (or for wxWidgets 3.2): https://docs.wxwidgets.org/3.2/page_libs.html
# The root dependency (base) must come last.
#
if(WXM_DISABLE_WEBVIEW)
	find_package(wxWidgets 3 REQUIRED richtext aui adv net qa html xml core base)
else()
	find_package(wxWidgets 3 REQUIRED richtext aui adv webview net qa html xml core base)
endif()
message(STATUS "Found wxWidgets: (found version \"${wxWidgets_VERSION_STRING}\")")
include(${wxWidgets_USE_FILE})


# Get the git version, if available.
find_package(Git)
if(Git_FOUND)
    if(EXISTS "${CMAKE_SOURCE_DIR}/.git")
        execute_process(COMMAND ${GIT_EXECUTABLE} rev-parse --short HEAD
                        WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
                        OUTPUT_VARIABLE WXMAXIMA_GIT_SHORT_HASH
                        OUTPUT_STRIP_TRAILING_WHITESPACE)
        message(STATUS "Building from git development tree, revision: ${WXMAXIMA_GIT_SHORT_HASH}")
    endif()
endif()



##
# Utility Functions
#


##
# Build Items
#

# Convert AUTHORS.md to a C++ include, which can be included in wxMaxima.cpp
file(WRITE "${CMAKE_BINARY_DIR}/contributors.h.tmp" "") # create an empty file
file(STRINGS AUTHORS.md AUTHORSLINES ENCODING UTF-8)
list(REMOVE_AT AUTHORSLINES 0 1) # remove main heading line.
    foreach(l ${AUTHORSLINES})
      if("${l}" STREQUAL "## Developers")
        set(AUTHORTYPE "AddDeveloper")
      elseif("${l}" STREQUAL "## Translators")
        set(AUTHORTYPE "AddTranslator")
      elseif("${l}" STREQUAL "## Artwork")
        set(AUTHORTYPE "AddArtist")
      elseif("${l}" STREQUAL "## The Manual")
        set(AUTHORTYPE "AddDocWriter")
      else()
        string(REPLACE "- " "" l2 "${l}")
        file(APPEND "${CMAKE_BINARY_DIR}/contributors.h.tmp" "info.${AUTHORTYPE}(wxT(\"${l2}\"));\n")
      endif()
    endforeach()
# write to a *.tmp file first and then 'copy_if_different' prevents rebuilding source files, if nothing (but a timestamp) changed.
execute_process(COMMAND ${CMAKE_COMMAND} -E copy_if_different "${CMAKE_BINARY_DIR}/contributors.h.tmp" "${CMAKE_BINARY_DIR}/contributors.h")

add_definitions(-DwxNO_UNSAFE_WXSTRING_CONV)

# It is complicated. po4a expects that [po_directory] is a relative path. Don't know why.
#
# But just creating a relative path from CMAKE_BINARY_DIR won't work too, one can call CMake from
# an other directory and specify the build directory using -B <path>
# See issue https://github.com/wxMaxima-developers/wxmaxima/issues/1917
# So for Unix use "$ENV{PWD}" to compute the relative path.
# For Windows the environment variable PWD does not exist, so - for now - just use the CMAKE_BINARY_DIR
# (and assume, that cmake is called from the build dir.
# TODO: This can probably solved using the output of 'cd', better would be a *portable* way to
# get the working directory from CMake.
if(WIN32 OR CYGWIN)
    file(RELATIVE_PATH RELPATH_TO_PO4A_PO_DIRECTORY "${CMAKE_BINARY_DIR}" "${CMAKE_SOURCE_DIR}/locales/manual")
else()
    file(RELATIVE_PATH RELPATH_TO_PO4A_PO_DIRECTORY "$ENV{PWD}" "${CMAKE_SOURCE_DIR}/locales/manual")
endif()
file(GLOB MANUAL_LANGUAGES "${CMAKE_CURRENT_SOURCE_DIR}/locales/manual/*.po")
list(TRANSFORM MANUAL_LANGUAGES REPLACE ".*/(.*).po$" "\\1")
string(REPLACE ";" " " LANGUAGES_SPACESEPARATED "${MANUAL_LANGUAGES}")
file(RELATIVE_PATH WXMAXIMA_MD_RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}" "${CMAKE_SOURCE_DIR}/info/wxmaxima.md")

configure_file(${CMAKE_SOURCE_DIR}/locales/manual/po4a.cfg.in po4a.cfg)

add_subdirectory(src)
if(MSVC)
    # Tell MSVC not to generate its own manifest file.
    # Must be done after the target was created.
    set_property(TARGET wxmaxima APPEND PROPERTY LINK_FLAGS /MANIFEST:NO)
endif()

if(NOT CYGWIN)
    add_subdirectory(Doxygen)
endif()
add_subdirectory(data)
add_subdirectory(info)
add_subdirectory(locales)
add_subdirectory(art)
add_subdirectory(examples)

enable_testing()
add_subdirectory(test)

add_custom_target(dist COMMAND ${CMAKE_MAKE_PROGRAM} package_source)

install(FILES AUTHORS.md COPYING GPL.txt NEWS.md README README.md DESTINATION share/doc/wxmaxima)

# include wxWidgets DLLs on Windows on appveyor
if(WIN32 AND APPVEYOR_BUILD)
    install(PROGRAMS ${CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS}
        DESTINATION programs
        COMPONENT applications)
endif()
