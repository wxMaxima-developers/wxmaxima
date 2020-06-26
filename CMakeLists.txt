# -*- mode: CMake; cmake-tab-width: 4; -*-

cmake_minimum_required(VERSION 3.7)

##
# Options
#

option(WXM_BUILD_SOURCE_ONLY
    "Only build the source code. Don't generate documentation, translations, etc." OFF)
option(WXM_ENABLE_PRECOMPILED_HEADERS
    "Enable precompiled headers to potentially speed up compilation." OFF)
option(WXM_USE_CPPCHECK
    "Perform CPPCHECK during compilation." OFF)
option(WXM_USE_OPENMP
    "Use OpenMP for parallelizing code." ON)

if (DEFINED MACOSX_VERSION_MIN)
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

# Ignore the CMAKE_UNITY_BUILD variable if it's not used due to cmake version
# older than 3.16.
set(ignoreMe "${CMAKE_UNITY_BUILD}")

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

if(NOT APPVEYOR_BUILD)
  set(NONCRIT_WARNING WARNING)
else()
  # Warnings cause trouble when running under PowerShell, since they are output to
  # stderr and are interpreted as if they were hard failures.
  set(NONCRIT_WARNING STATUS)
endif()

project(wxmaxima LANGUAGES CXX)
set(VERSION 20.06.6-DevelopmentSnapshot)
set(GITVERSION ${VERSION})
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(GNUInstallDirs)

find_package(wxWidgets 3 REQUIRED base core adv xml html aui net richtext)
include(${wxWidgets_USE_FILE})

# Avoid a warning by deciding which version of this policy we prefer.
if(POLICY CMP0066)
    cmake_policy(SET CMP0066 NEW)
endif()

# link time optimization seems to need that
if(POLICY CMP0069)
    cmake_policy(SET CMP0069 NEW)
endif()

# Turn off many warnings wxWidgets triggers on MSVC
if(MSVC)
    add_definitions(-D_CRT_SECURE_NO_WARNINGS)
endif()

# Clang requires that flag (that should fix the error on travis too)
if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    add_definitions(-Wno-c++11-narrowing)
endif()

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


if("${PROJECT_SOURCE_DIR}" STREQUAL "${PROJECT_BINARY_DIR}")
    message(STATUS "Cmake discourages in-source builds making them the less tested option. It is therefore advisable to do an out-of-source-build instead: Create a separate directory for the build and run cmake <path to the source dir> and make from there).")
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
# Build Items
#

if(NOT WXM_BUILD_SOURCE_ONLY)
    add_subdirectory(locales)
    add_subdirectory(Doxygen)
    add_subdirectory(data)
    add_subdirectory(info)
endif()

add_subdirectory(src)

enable_testing()
add_subdirectory(test)

if(NOT WXM_BUILD_SOURCE_ONLY)
    add_custom_target(dist COMMAND ${CMAKE_MAKE_PROGRAM} package_source)

    add_custom_target(checksums
        ${CMAKE_COMMAND} -DVERSION=${VERSION} -DCMAKE_SYSTEM_NAME=${CMAKE_SYSTEM_NAME} -P ${CMAKE_SOURCE_DIR}/checksums.cmake)

    install(FILES AUTHORS COPYING ChangeLog GPL.txt NEWS.md README README.md DESTINATION share/doc/wxmaxima)

    # include wxWidgets DLLs on Windows on appveyor
    if(WIN32 AND APPVEYOR_BUILD)
        file(GLOB wxwidgets_dlls "${wxWidgets_LIB_DIR}/*.dll")
        install(FILES ${wxwidgets_dlls} DESTINATION bin/)
        install(PROGRAMS ${CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS}
            DESTINATION programs
            COMPONENT applications)
    endif()
endif()
