# Taken from the Krita project: https://invent.kde.org/graphics/krita/-/blob/master/cmake/modules/FindFriBidi.cmake
# Changed CamelCase (FriBidi) to Fribidi, as CMake outputs a warning
#
# SPDX-FileCopyrightText: 2022 L. E. Segovia <amy@amyspark.me>
# SPDX-FileCopyrightText: 2023 Alvin Wong <alvin@alvinhc.com>
# SPDX-License-Identifier: BSD-3-Clause

#[=======================================================================[.rst:
FindFribidi
--------------

Find Fribidi headers and libraries.

Imported Targets
^^^^^^^^^^^^^^^^

``Fribidi::Fribidi``
  The Fribidi library, if found.

Result Variables
^^^^^^^^^^^^^^^^

This will define the following variables in your project:

``Fribidi_FOUND``
  true if (the requested version of) Fribidi is available.
``Fribidi_VERSION``
  the version of Fribidi.
``Fribidi_LIBRARIES``
  the libraries to link against to use Fribidi.
``Fribidi_INCLUDE_DIRS``
  where to find the Fribidi headers.
``Fribidi_COMPILE_OPTIONS``
  this should be passed to target_compile_options(), if the
  target is not used for linking

#]=======================================================================]

include(FindPackageHandleStandardArgs)

find_package(PkgConfig QUIET)

if (PkgConfig_FOUND)
    pkg_check_modules(PC_FRIBIDI QUIET fribidi)
    set(Fribidi_VERSION ${PC_FRIBIDI_VERSION})
    set(Fribidi_COMPILE_OPTIONS "${PC_FRIBIDI_CFLAGS} ${PC_FRIBIDI_CFLAGS_OTHER}")
endif ()

find_path(Fribidi_INCLUDE_DIR
    NAMES fribidi/fribidi.h
    HINTS ${PC_FRIBIDI_INCLUDEDIR} ${PC_FRIBIDI_INCLUDE_DIRS}
)
if(Fribidi_INCLUDE_DIR)
    set(Fribidi_INCLUDE_DIR ${Fribidi_INCLUDE_DIR}/fribidi)
endif()

find_library(Fribidi_LIBRARY
    NAMES ${Fribidi_NAMES} fribidi
    HINTS ${PC_FRIBIDI_LIBDIR} ${PC_FRIBIDI_LIBRARY_DIRS}
)

if (NOT Fribidi_VERSION AND Fribidi_INCLUDE_DIR)
    file(READ ${Fribidi_INCLUDE_DIR}/fribidi-config.h _fribidi_version_content)

    string(REGEX MATCH "#define FRIBIDI_VERSION[ \t]+\"([0-9.]+)\"\n" _version_match ${_fribidi_version_content})

    if (_version_match)
        set(Fribidi_VERSION "${CMAKE_MATCH_1}")
    else()
        if(NOT Fribidi_FIND_QUIETLY)
            message(WARNING "Failed to get version information from ${Fribidi_INCLUDE_DIR}/fribidi-config.h")
        endif()
    endif()

endif()

if (Fribidi_INCLUDE_DIR AND Fribidi_LIBRARY)
    set(Fribidi_FOUND ON)
else()
    set(Fribidi_FOUND OFF)
endif()

find_package_handle_standard_args(Fribidi
    FOUND_VAR Fribidi_FOUND
    REQUIRED_VARS Fribidi_INCLUDE_DIR Fribidi_LIBRARY
    HANDLE_COMPONENTS
    VERSION_VAR Fribidi_VERSION
)

if (Fribidi_FOUND)
if (Fribidi_LIBRARY AND NOT TARGET Fribidi::Fribidi)
    add_library(Fribidi::Fribidi UNKNOWN IMPORTED GLOBAL)
    set_target_properties(Fribidi::Fribidi PROPERTIES
        IMPORTED_LOCATION "${Fribidi_LIBRARY}"
        INTERFACE_COMPILE_OPTIONS "${PC_FRIBIDI_CFLAGS_OTHER}"
        INTERFACE_INCLUDE_DIRECTORIES "${Fribidi_INCLUDE_DIR}"
        INTERFACE_LINK_LIBRARIES "${PC_FRIBIDI_LINK_LIBRARIES}"
        INTERFACE_LINK_DIRECTORIES "${PC_FRIBIDI_LIBDIR}"
    )
endif ()

mark_as_advanced(
    Fribidi_INCLUDE_DIR
    Fribidi_LIBRARY
)

set(Fribidi_LIBRARIES ${Fribidi_LIBRARY})
set(Fribidi_INCLUDE_DIRS ${Fribidi_INCLUDE_DIR})
endif()
