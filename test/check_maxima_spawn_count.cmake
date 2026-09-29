# -*- mode: cmake -*-
#
# Regression guard against starting more Maxima processes than a run needs.
#
# wxMaxima starts a Maxima as soon as it comes up, so that the process is
# already running by the time the user sends off their first cell. Opening a
# file used to make that head start worthless: Maxima has to run in the
# directory the file lives in, it learns which directory that is from
# MAXIMA_INITIAL_FOLDER, and it reads that only at startup -- so a Maxima
# started before the file was known could not be moved and was killed and
# replaced instead. Opening one file started two Maxima processes, one of
# which never did anything at all except take a second or so to start up.
#
# This script runs one `wxmaxima --batch <TESTFILE>` and counts how many
# Maxima processes it started. One file opened, one Maxima.
#
# How it counts (GH #2351): the run's --maxima points at a small wrapper this
# script writes -- a shell script, or a .cmd file on Windows -- which appends
# one line to a count file and then hands over to the real Maxima. The count
# is the number of lines in that file.
#
# It used to count the "Running maxima as:" lines wxMaxima writes to its own
# log instead. That never worked on Windows: wxmaxima.exe is a GUI-subsystem
# program, and what it writes to stderr does not reliably reach whoever
# captures it there (the unexplained FILE_TYPE_CHAR behaviour the
# wxmaxima-packaging skill records). The captured log was simply empty and
# read as "started 0 Maxima processes". The wrapper's count file never
# touches wxMaxima's stdio, so the test now runs on every platform.
#
# Parameters (all passed as -D on the cmake -P command line):
#   WXMAXIMA  - path to the wxmaxima executable
#   MAXIMA    - path to the real Maxima the wrapper hands over to
#   TESTFILE  - batch file to run (relative to WORKDIR)
#   WORKDIR   - working directory for the run
#   EXPECTED  - how many Maxima processes the run may start (default 1)

cmake_minimum_required(VERSION 3.16)

if(NOT DEFINED EXPECTED)
  set(EXPECTED 1)
endif()
if(NOT MAXIMA)
  message(FATAL_ERROR "MAXIMA must name the real Maxima the wrapper runs.")
endif()

set(wrapperdir "${WORKDIR}/maxima_spawn_count")
set(countfile "${wrapperdir}/spawns.txt")
set(logfile "${wrapperdir}/run.err")
file(REMOVE_RECURSE "${wrapperdir}")
file(MAKE_DIRECTORY "${wrapperdir}")

file(TO_NATIVE_PATH "${countfile}" countfile_native)
file(TO_NATIVE_PATH "${MAXIMA}" maxima_native)
if(WIN32)
  # Maxima on Windows is itself maxima.bat. Starting a batch file from a batch
  # file without "call" hands control over to it for good, which is the
  # closest cmd.exe has to exec.
  set(wrapper "${wrapperdir}/maxima-wrapper.cmd")
  file(WRITE "${wrapper}"
    "@echo off\r\n"
    "echo started>>\"${countfile_native}\"\r\n"
    "\"${maxima_native}\" %*\r\n")
else()
  # Written elsewhere first, since file(WRITE) can't set the executable bit
  # and file(CHMOD) needs a newer CMake than this project requires.
  set(wrapper "${wrapperdir}/maxima-wrapper.sh")
  set(staging "${wrapperdir}/staging/maxima-wrapper.sh")
  file(WRITE "${staging}"
    "#!/bin/sh\n"
    "echo started >> '${countfile}'\n"
    "exec '${MAXIMA}' \"$@\"\n")
  file(COPY "${staging}" DESTINATION "${wrapperdir}"
       FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE
                        GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
endif()

# In backslash form on Windows, where cmd.exe ends up running it.
file(TO_NATIVE_PATH "${wrapper}" wrapper_native)
execute_process(
  COMMAND "${WXMAXIMA}" --debug --logtostderr --pipe --batch --exit-on-error
          "--maxima=${wrapper_native}" "${TESTFILE}"
  WORKING_DIRECTORY "${WORKDIR}"
  OUTPUT_FILE "${wrapperdir}/run.out"
  ERROR_FILE  "${logfile}"
  RESULT_VARIABLE run_rc)

if(NOT run_rc EQUAL 0)
  # A run that failed for an unrelated reason would make the spawn count
  # meaningless, so report that instead of a count nobody can interpret. (On
  # Windows the log may well be empty, for the reason given above.)
  file(READ "${logfile}" log_tail)
  message(FATAL_ERROR
    "The batch run itself failed (exit code ${run_rc}), so the number of "
    "Maxima processes it started says nothing. Log:\n${log_tail}")
endif()

set(spawn_count 0)
if(EXISTS "${countfile}")
  file(STRINGS "${countfile}" spawn_lines REGEX "started")
  list(LENGTH spawn_lines spawn_count)
endif()

if(NOT spawn_count EQUAL EXPECTED)
  message(FATAL_ERROR
    "Opening one file started ${spawn_count} Maxima process(es), expected "
    "${EXPECTED}. A count of 0 from a run that succeeded means the wrapper "
    "(${wrapper}) was never used, not that no Maxima ran; anything above "
    "${EXPECTED} means a Maxima was started and then thrown away unused.")
endif()

message(STATUS "OK: the run started ${spawn_count} Maxima process(es).")
