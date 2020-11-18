# -*- mode: CMake; cmake-tab-width: 4; -*-

cmake_minimum_required(VERSION 3.7)
project(wxmaxima LANGUAGES CXX)

if(WIN32)
    enable_language(RC)
endif()


set(VERSION 20.11.0-DevelopmentSnapshot)
set(GITVERSION ${VERSION})
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

##
# Options
#

option(WXM_BUILD_BINARY_ONLY
    "Only build the wxmaxima binary. Don't generate documentation, translations, etc." OFF)
option(WXM_ENABLE_PRECOMPILED_HEADERS
    "Enable precompiled headers to potentially speed up compilation." OFF)
option(WXM_USE_CPPCHECK
    "Perform CPPCHECK during compilation." OFF)
option(WXM_USE_OPENMP
    "Use OpenMP for parallelizing code." ON)
option(WXM_UNIT_TESTS
    "Compile unit tests and enable the tests." OFF)

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
if(DEFINED USE_OPENMP)
    set(WXM_USE_OPENMP ${USE_OPENMP} FORCE)
    unset(USE_OPENMP)
    unset(USE_OPENMP CACHE)
    message(WARNING "USE_OPENMP is deprecated. Use WXM_USE_OPENMP instead")
endif()

##
# Internal Options
#

if(CMAKE_VERSION VERSION_LESS "3.16")
    unset(CMAKE_UNITY_BUILD)
    unset(CMAKE_UNITY_BUILD_BATCH_SIZE)
endif()

if(NOT APPVEYOR_BUILD)
  set(NONCRIT_WARNING WARNING)
else()
  # Warnings cause trouble when running under PowerShell, since they are output to
  # stderr and are interpreted as if they were hard failures.
  set(NONCRIT_WARNING STATUS)
endif()

##
# CMake Policies
#

# Avoid a warning by deciding which version of this policy we prefer.
if(POLICY CMP0066)
    cmake_policy(SET CMP0066 NEW)
endif()

# link time optimization seems to need that
if(POLICY CMP0069)
    cmake_policy(SET CMP0069 NEW)
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
    message(FATAL_ERROR
        "In-source builds not allowed. They are unsupportable in practice, they spill "
        "their guts into .gitignore, and they turn buildsystem bugs into damage to sources. "
        "Instead, build the project out-of source: Create a "
        "separate directory for the build *outside of the source folder*, and run "
        "cmake <path to the source dir> and build from there.")
endif()

##
# Dependencies/Packages
#

include(GNUInstallDirs)

message(STATUS "Looking for wxWidgets in:           ${wxWidgets_ROOT_DIR}")
message(STATUS "Looking for wxWidgets libraries in: ${wxWidgets_LIB_DIR}")

# The package order below matters.
###
###  Incorrect order WILL BREAK Unix and MinGW STATIC BUILDS.
###
# The correct order is a topological sort based on the dependency graph
# here: https://docs.wxwidgets.org/3.0/page_libs.html#page_libs_wxrichtext
# The root dependency (base) must come last.
#
find_package(wxWidgets 3 REQUIRED richtext aui adv html core xml net base)
include(${wxWidgets_USE_FILE})

find_program(CCACHE_PROGRAM ccache)
if(CCACHE_PROGRAM)
  # Support Unix Makefiles and Ninja
  set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE "${CCACHE_PROGRAM}")
  if(UNIX)
    set_property(GLOBAL PROPERTY RULE_LAUNCH_LINK "${CCACHE_PROGRAM}")
  endif()
  message(STATUS "Found CCache ${CCACHE_PROGRAM}")
else()
  message(STATUS "Not found a ccache that speeds up incremental compilation.")
endif()

# Get the git version, if available.
find_package(Git)
if(Git_FOUND)
    if(EXISTS "${CMAKE_SOURCE_DIR}/.git")
        execute_process(COMMAND ${GIT_EXECUTABLE} rev-parse --short HEAD
                        WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
                        OUTPUT_VARIABLE WXMAXIMA_GIT_VERSION
                        OUTPUT_STRIP_TRAILING_WHITESPACE)
        message(STATUS "Building from git development tree, revision: ${WXMAXIMA_GIT_VERSION}")
        add_definitions("-DWXMAXIMA_GIT_VERSION=\"${WXMAXIMA_GIT_VERSION}\"")
    endif()
endif()

if(WXM_USE_OPENMP)
    find_package(OpenMP)
    include(CheckIncludeFileCXX)
    check_include_file_cxx("omp.h" HAVE_OMP_HEADER)
endif()

if(WXM_USE_OPENMP AND OpenMP_CXX_FOUND)
    # For CMake < 3.9, we need to make the target ourselves
    if(NOT TARGET OpenMP::OpenMP_CXX)
        find_package(Threads REQUIRED)
        add_library(OpenMP::OpenMP_CXX IMPORTED INTERFACE)
        set_property(TARGET OpenMP::OpenMP_CXX
            PROPERTY INTERFACE_COMPILE_OPTIONS ${OpenMP_CXX_FLAGS})
        # Only works if the same flag is passed to the linker; use CMake 3.9+ otherwise (Intel, AppleClang)
        set_property(TARGET OpenMP::OpenMP_CXX
            PROPERTY INTERFACE_LINK_LIBRARIES ${OpenMP_CXX_FLAGS} Threads::Threads)
    endif()
    set(CMAKE_INSTALL_OPENMP_LIBRARIES TRUE) # Include OpenMP libraries in the required system libraries
endif()

if(WXM_USE_OPENMP AND OpenMP_CXX_FOUND)
    if(OpenMP_CXX_SPEC_DATE LESS 201107)
        message(STATUS "OpenMP too old to be used for multiprocessing in wxMaxima (" ${OpenMP_CXX_SPEC_DATE} ").")
    else()
        if(HAVE_OMP_HEADER)
            message(STATUS "Using OpenMP for multiprocessing.")
        else()
            message(STATUS "Using OpenMP, but omp.h missing => Can use multiprocessing only in a few cases.")
        endif()
    endif()
else()
    message(STATUS "No OpenMP found => Not using multiprocessing.")
endif()

##
# Utility Functions
#

# Prepends a prefix to all elements in a list. Needed only in
# cmake < 3.12, but we claim to support 3.7, so this is needed.
# Usage: list_transform_prepend(FILES_TO_TRANSLATE "prefix/")
# See https://stackoverflow.com/a/59155344/1329652

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

if(NOT WXM_BUILD_BINARY_ONLY)
    add_subdirectory(Doxygen)
    add_subdirectory(data)
    add_subdirectory(info)
    add_subdirectory(locales)
endif()

add_subdirectory(src)

enable_testing()
add_subdirectory(test)

if(NOT WXM_BUILD_BINARY_ONLY)
    add_custom_target(dist COMMAND ${CMAKE_MAKE_PROGRAM} package_source)

    add_custom_target(checksums
        ${CMAKE_COMMAND} -DVERSION=${VERSION} -DCMAKE_SYSTEM_NAME=${CMAKE_SYSTEM_NAME} -P ${CMAKE_SOURCE_DIR}/checksums.cmake)

    install(FILES AUTHORS COPYING ChangeLog GPL.txt NEWS.md README README.md DESTINATION share/doc/wxmaxima)

    # include wxWidgets DLLs on Windows on appveyor
    if(WIN32 AND APPVEYOR_BUILD)
        install(PROGRAMS ${CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS}
            DESTINATION programs
            COMPONENT applications)
    endif()
endif()
