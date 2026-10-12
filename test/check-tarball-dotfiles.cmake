# Guard against dot files ending up in the release source tarball.
#
# GitHub builds a tag's source tarball with `git archive`, which leaves out
# whatever .gitattributes marks export-ignore. Distributions repackage that
# tarball, and three releases in a row shipped dot files they had to complain
# about (GH #2068, #2101, #2272): each time .gitattributes listed the dot files
# by name, and each time a new one had been added since. .gitattributes now
# excludes them all with a single pattern; this checks that it still does, by
# looking at what `git archive` actually produces rather than at the pattern.
#
# Needs git and a git checkout, so test/CMakeLists.txt only registers it when
# both are there (a build from the tarball itself has nothing to check).
#
# Run standalone with:
#   cmake -DGIT_EXECUTABLE=git -DSOURCE_DIR=<repo> -DWORK_DIR=<scratch dir> \
#         -P test/check-tarball-dotfiles.cmake

cmake_minimum_required(VERSION 3.16)

foreach(var GIT_EXECUTABLE SOURCE_DIR WORK_DIR)
  if(NOT ${var})
    message(FATAL_ERROR "check-tarball-dotfiles: ${var} must be set")
  endif()
endforeach()

set(archive "${WORK_DIR}/check-tarball-dotfiles.tar")
file(MAKE_DIRECTORY "${WORK_DIR}")
execute_process(
  COMMAND "${GIT_EXECUTABLE}" -C "${SOURCE_DIR}" archive --format=tar
          "--output=${archive}" HEAD
  RESULT_VARIABLE result
  ERROR_VARIABLE error)
if(NOT result EQUAL 0)
  message(FATAL_ERROR "check-tarball-dotfiles: git archive failed: ${error}")
endif()

execute_process(
  COMMAND "${CMAKE_COMMAND}" -E tar tf "${archive}"
  RESULT_VARIABLE result
  OUTPUT_VARIABLE listing
  ERROR_VARIABLE error)
file(REMOVE "${archive}")
if(NOT result EQUAL 0)
  message(FATAL_ERROR "check-tarball-dotfiles: listing the archive failed: ${error}")
endif()

string(REPLACE "\n" ";" entries "${listing}")
list(LENGTH entries count)
# An empty or near-empty listing would make the check below pass vacuously.
if(count LESS 100)
  message(FATAL_ERROR "check-tarball-dotfiles: the archive only has ${count} entries")
endif()

set(offending "")
foreach(entry IN LISTS entries)
  # A dot file or dot directory at any depth: a path component starting with
  # a dot.
  if(entry MATCHES "(^|/)\\.")
    list(APPEND offending "${entry}")
  endif()
endforeach()

if(offending)
  list(JOIN offending "\n  " offending)
  message(FATAL_ERROR
    "The source tarball (git archive HEAD) would contain dot files:\n"
    "  ${offending}\n"
    "Distributions reject these (GH #2272). .gitattributes is meant to "
    "exclude them all via `.* export-ignore`.")
endif()
message(STATUS "check-tarball-dotfiles: ${count} entries, no dot files")
