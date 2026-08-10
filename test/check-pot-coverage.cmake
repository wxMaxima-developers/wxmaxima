# Guard against translatable strings becoming invisible to xgettext.
#
# locales/wxMaxima/CMakeLists.txt feeds xgettext an explicit glob. It used to be
# a flat src/*.cpp;src/*.h, which meant every string under src/cells, src/wizards,
# src/dialogs and src/sidebars was silently missing from wxMaxima.pot -- and from
# there, missing from every catalogue. That cost about 7000 translations before
# anyone noticed, because nothing in the build ever complains: xgettext is
# perfectly happy to extract from a shorter file list.
#
# The existing wxMaxima.pot drift check in CI cannot catch this. It regenerates
# the POT and diffs it against the committed one -- but a broken glob produces
# the same truncated POT on both sides, so the diff is empty and the check
# passes while the strings stay lost.
#
# Two assertions, both cheap and neither needing a build or gettext:
#
#   1. Every source file under src/ is at a depth the glob actually reaches.
#      This is the exact regression: the day someone adds src/cells/foo/Bar.cpp,
#      the two-level glob stops covering it.
#   2. Every source file that contains a _("...") marker is referenced by the
#      committed POT. This catches the same class of loss coming from a
#      direction the depth rule cannot see, e.g. a new file extension.
#
# Run standalone with:
#   cmake -DSOURCE_DIR=<repo> -P test/check-pot-coverage.cmake

if(NOT SOURCE_DIR)
  message(FATAL_ERROR "check-pot-coverage: SOURCE_DIR must be set")
endif()

# Script mode (cmake -P) doesn't inherit the top-level CMakeLists.txt's policy
# settings, so IN_LIST below needs this set explicitly -- without it CMake
# falls back to the pre-3.3 OLD behavior, where if() doesn't know IN_LIST at
# all, and errors out on it as a bare unquoted argument list.
cmake_policy(SET CMP0057 NEW)

set(pot "${SOURCE_DIR}/locales/wxMaxima/wxMaxima.pot")
if(NOT EXISTS "${pot}")
  message(FATAL_ERROR "check-pot-coverage: ${pot} not found")
endif()

# --- 1. depth: keep this list in step with POT_SOURCE_FILES in ---------------
#        locales/wxMaxima/CMakeLists.txt.
file(GLOB_RECURSE all_sources RELATIVE "${SOURCE_DIR}"
     "${SOURCE_DIR}/src/*.cpp" "${SOURCE_DIR}/src/*.h")
file(GLOB covered_abs
     "${SOURCE_DIR}/src/*.cpp" "${SOURCE_DIR}/src/*.h"
     "${SOURCE_DIR}/src/*/*.cpp" "${SOURCE_DIR}/src/*/*.h")
set(covered "")
foreach(f IN LISTS covered_abs)
  file(RELATIVE_PATH rel "${SOURCE_DIR}" "${f}")
  list(APPEND covered "${rel}")
endforeach()

set(too_deep "")
foreach(f IN LISTS all_sources)
  if(NOT f IN_LIST covered)
    list(APPEND too_deep "${f}")
  endif()
endforeach()

if(too_deep)
  string(REPLACE ";" "\n    " pretty "${too_deep}")
  message(FATAL_ERROR
    "These source files are nested deeper than the POT glob reaches, so every\n"
    "translatable string in them is invisible to xgettext and will never appear\n"
    "in any translation catalogue:\n    ${pretty}\n\n"
    "Fix: add the matching pattern (e.g. src/*/*/*.cpp and src/*/*/*.h) to both\n"
    "POT_SOURCE_FILES_REL and POT_SOURCE_FILES in locales/wxMaxima/CMakeLists.txt,\n"
    "and to the covered list in test/check-pot-coverage.cmake.")
endif()

# --- 2. marker coverage -----------------------------------------------------
file(STRINGS "${pot}" pot_refs REGEX "^#: ")
set(referenced "")
foreach(line IN LISTS pot_refs)
  string(REPLACE "#: " "" line "${line}")
  string(REPLACE " " ";" refs "${line}")
  foreach(ref IN LISTS refs)
    string(REGEX REPLACE ":[0-9]+$" "" ref "${ref}")
    string(REGEX REPLACE "^\\.\\./\\.\\./" "" ref "${ref}")
    list(APPEND referenced "${ref}")
  endforeach()
endforeach()
list(REMOVE_DUPLICATES referenced)

set(missing "")
foreach(f IN LISTS all_sources)
  if(f IN_LIST referenced)
    continue()
  endif()
  # Strip comments before looking for the marker: a commented-out _("...") is
  # not a string xgettext should have found. src/Notification.cpp is exactly
  # this case and must not be reported.
  file(STRINGS "${SOURCE_DIR}/${f}" lines)
  set(in_block FALSE)
  set(found FALSE)
  foreach(line IN LISTS lines)
    if(in_block)
      if(line MATCHES "\\*/")
        string(REGEX REPLACE "^.*\\*/" "" line "${line}")
        set(in_block FALSE)
      else()
        continue()
      endif()
    endif()
    string(REGEX REPLACE "/\\*.*\\*/" "" line "${line}")
    if(line MATCHES "/\\*")
      string(REGEX REPLACE "/\\*.*$" "" line "${line}")
      set(in_block TRUE)
    endif()
    string(REGEX REPLACE "//.*$" "" line "${line}")
    if(line MATCHES "_\\(\"")
      set(found TRUE)
      break()
    endif()
  endforeach()
  if(found)
    list(APPEND missing "${f}")
  endif()
endforeach()

if(missing)
  string(REPLACE ";" "\n    " pretty "${missing}")
  message(FATAL_ERROR
    "These files contain translatable _(\"...\") strings but are not referenced\n"
    "by locales/wxMaxima/wxMaxima.pot, so their strings reach no translator:\n"
    "    ${pretty}\n\n"
    "Either the POT is stale (run 'cmake --build build --target update-locale'\n"
    "and commit the result) or the xgettext glob in\n"
    "locales/wxMaxima/CMakeLists.txt does not reach these files.")
endif()

list(LENGTH all_sources n_src)
list(LENGTH referenced n_ref)
message(STATUS
  "check-pot-coverage: ${n_src} source files, all within the POT glob; "
  "${n_ref} referenced by the POT.")
