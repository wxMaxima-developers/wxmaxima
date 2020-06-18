# -*- mode: CMake; cmake-tab-width: 4; -*-

cmake_minimum_required(VERSION 3.7)
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
set(PACKAGE wxmaxima)
set(APP_NAME wxmaxima)
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(GNUInstallDirs)

find_package(wxWidgets 3 REQUIRED base core adv xml html aui net richtext)
include(${wxWidgets_USE_FILE})

# Avoid a warning by deciding which version of this policy we prefer.
if(POLICY CMP0066)
    cmake_policy(SET CMP0066 NEW)
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

# MacOSX version-min compiler settings
if(MACOSX_VERSION_MIN)
    message(STATUS "Compiling with min macosx version ${MACOSX_VERSION_MIN}")
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -mmacosx-version-min=${MACOSX_VERSION_MIN}")
    set(CMAKE_EXE_LINKER_FLAGS_DEBUG "${CMAKE_LINKER_FLAGS_DEBUG} -mmacosx-version-min=${MACOSX_VERSION_MIN}")
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -mmacosx-version-min=${MACOSX_VERSION_MIN}")
    set(CMAKE_EXE_LINKER_FLAGS_RELEASE "${CMAKE_LINKER_FLAGS_RELEASE} -mmacosx-version-min=${MACOSX_VERSION_MIN}")
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

if(NOT BUILD_SOURCE_ONLY)
   add_subdirectory(locales)
   add_subdirectory(Doxygen)
   add_subdirectory(data)
   add_subdirectory(info)
endif()
add_subdirectory(src)

install(FILES AUTHORS COPYING ChangeLog GPL.txt NEWS.md README README.md DESTINATION share/doc/wxmaxima)

# include wxWidgets DLLs on Windows on appveyor
if(WIN32 AND APPVEYOR_BUILD)
    file(GLOB wxwidgets_dlls "${wxWidgets_LIB_DIR}/*.dll")
    install(FILES ${wxwidgets_dlls} DESTINATION bin/)
    install(PROGRAMS ${CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS}
	DESTINATION programs
	COMPONENT applications)
endif()

add_custom_target(dist COMMAND ${CMAKE_MAKE_PROGRAM} package_source)

add_custom_target(checksums
    ${CMAKE_COMMAND} -DVERSION=${VERSION} -DCMAKE_SYSTEM_NAME=${CMAKE_SYSTEM_NAME} -P ${CMAKE_SOURCE_DIR}/checksums.cmake)

enable_testing()
add_subdirectory(test)
