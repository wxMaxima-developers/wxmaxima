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
# This script runs one `wxmaxima --batch <TESTFILE>` and counts the "Running
# maxima as:" lines its log contains. One file opened, one Maxima.
#
# Parameters (all passed as -D on the cmake -P command line):
#   WXMAXIMA  - path to the wxmaxima executable
#   TESTFILE  - batch file to run (relative to WORKDIR)
#   WORKDIR   - working directory for the run
#   EXPECTED  - how many Maxima processes the run may start (default 1)
#
# The run has to produce that marker in English, so the test sets LC_ALL/
# LANGUAGE in its ENVIRONMENT. A marker that has been reworded or translated
# away yields a count of zero, which fails this test loudly rather than
# letting it pass while checking nothing.
#
# It also means this only works where wxMaxima's stderr actually reaches the
# file we point it at, which on Windows it does not -- see the comment on the
# test in test/CMakeLists.txt, which is where that is decided. A count of zero
# there meant an empty log, not a Maxima that never started.

cmake_minimum_required(VERSION 3.16)

if(NOT DEFINED EXPECTED)
  set(EXPECTED 1)
endif()

set(logfile "${WORKDIR}/maxima_spawn_count_run.err")

execute_process(
  COMMAND "${WXMAXIMA}" --debug --logtostderr --pipe --batch --exit-on-error
          "${TESTFILE}"
  WORKING_DIRECTORY "${WORKDIR}"
  OUTPUT_FILE "${WORKDIR}/maxima_spawn_count_run.out"
  ERROR_FILE  "${logfile}"
  RESULT_VARIABLE run_rc)

if(NOT run_rc EQUAL 0)
  # A run that failed for an unrelated reason would make the spawn count
  # meaningless, so report that instead of a count nobody can interpret.
  file(READ "${logfile}" log_tail)
  message(FATAL_ERROR
    "The batch run itself failed (exit code ${run_rc}), so the number of "
    "Maxima processes it started says nothing. Log:\n${log_tail}")
endif()

file(STRINGS "${logfile}" spawn_lines REGEX "Running maxima as")
list(LENGTH spawn_lines spawn_count)

if(NOT spawn_count EQUAL EXPECTED)
  string(REPLACE ";" "\n  " spawn_lines_pretty "${spawn_lines}")
  message(FATAL_ERROR
    "Opening one file started ${spawn_count} Maxima process(es), expected "
    "${EXPECTED}:\n  ${spawn_lines_pretty}\n"
    "A count of 0 usually means the log line this test looks for was "
    "reworded or was not written in English; anything above ${EXPECTED} "
    "means a Maxima was started and then thrown away unused.")
endif()

message(STATUS "OK: the run started ${spawn_count} Maxima process(es).")
