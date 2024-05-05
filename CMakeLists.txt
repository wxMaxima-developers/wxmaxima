# -*- mode: CMake; cmake-tab-width: 4; -*-

cmake_minimum_required(VERSION 3.10)
project(wxMaxima LANGUAGES CXX)

add_compile_options("$<$<C_COMPILER_ID:MSVC>:/utf-8>")
add_compile_options("$<$<CXX_COMPILER_ID:MSVC>:/utf-8>")

# Set the locale to default C to prevent issues due to localization of commands.
# This is necessary as we call commands like "po4a --version"
set(ENV{LANG} C)

if(WIN32)
    enable_language(RC)
endif()


set(VERSION_MAJOR 24)
set(VERSION_MINOR 02)
set(VERSION_PATCHLEVEL 2)
set(VERSION_SUFFIX "DevelopmentSnapshot")
string(COMPARE EQUAL "${VERSION_SUFFIX}" "" no_version_suffix)
if(no_version_suffix)
        set(VERSION ${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCHLEVEL})
else()
        set(VERSION ${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCHLEVEL}_${VERSION_SUFFIX})
endif()

set(GITVERSION ${VERSION})
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(MACOSX_BUNDLE_COPYRIGHT "GPL2+")
set(MACOSX_BUNDLE_BUNDLE_VERSION ${VERSION})
set(MACOSX_BUNDLE_BUNDLE_NAME "wxMaxima")
set(MACOSX_BUNDLE_LONG_VERSION_STRING "wxMaxima ${VERSION}")
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
    # Tell MSVC not to generate its own manifest file
    #set_property(TARGET wxmaxima APPEND PROPERTY LINK_FLAGS /MANIFEST:NO)
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
message(STATUS "CMake config:                       ${CONFIG}")
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

# Prepends a prefix to all elements in a list. Needed only in
# cmake < 3.12, but we claim to support 3.10, so this is needed.
# Usage: list_transform_prepend(FILES_TO_TRANSLATE "prefix/")
# See https://stackoverflow.com/a/59155344/1329652
#
# According to #1388 cmake 3.19.2 requires this to be defined
# in each source file separately

function(list_transform_prepend var prefix)
    set(temp "")
    foreach(f ${${var}})
        list(APPEND temp "${prefix}${f}")
    endforeach()
    set(${var} "${temp}" PARENT_SCOPE)
endfunction()

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

file(RELATIVE_PATH RELPATH_TO_PO4A_PO_DIRECTORY "${CMAKE_BINARY_DIR}" "${CMAKE_SOURCE_DIR}/locales/manual")
configure_file(${CMAKE_SOURCE_DIR}/locales/manual/po4a.cfg.in po4a.cfg)

add_subdirectory(src)
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
