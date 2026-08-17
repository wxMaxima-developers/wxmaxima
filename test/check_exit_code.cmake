# -*- mode: cmake -*-
#
# Regression guard for GH #2276: a --batch/--exit-on-error run that halts
# before finishing must exit with the dedicated status code documented for
# the reason it halted (see wxMaxima::ExitCode in wxMaxima.h and the
# "EXIT STATUS" section of data/wxmaxima.1.in), not the generic 1 every such
# reason used to share.
#
# This is a `cmake -P` script (not a shell script) so it runs identically on
# the Linux and Windows CI, the same reasoning check_no_stray_procs.cmake
# gives for its own use of this pattern.
#
# Parameters (all passed as -D on the cmake -P command line):
#   WXMAXIMA           - path to the wxmaxima executable
#   ARGS                - semicolon-separated argument list to pass it
#   WORKDIR             - working directory for the run
#   EXPECTED_EXIT_CODE  - the exit code the run must produce

cmake_minimum_required(VERSION 3.16)

if(NOT DEFINED WXMAXIMA)
  message(FATAL_ERROR "check_exit_code: WXMAXIMA must be set")
endif()
if(NOT DEFINED EXPECTED_EXIT_CODE)
  message(FATAL_ERROR "check_exit_code: EXPECTED_EXIT_CODE must be set")
endif()

execute_process(
  COMMAND "${WXMAXIMA}" ${ARGS}
  WORKING_DIRECTORY "${WORKDIR}"
  OUTPUT_FILE "${WORKDIR}/check_exit_code_run.out"
  ERROR_FILE  "${WORKDIR}/check_exit_code_run.err"
  RESULT_VARIABLE run_rc)

if(NOT "${run_rc}" STREQUAL "${EXPECTED_EXIT_CODE}")
  file(READ "${WORKDIR}/check_exit_code_run.err" run_stderr)
  message(FATAL_ERROR
    "wxMaxima exited with code ${run_rc}, expected ${EXPECTED_EXIT_CODE}. "
    "Command: ${WXMAXIMA} ${ARGS}\n"
    "stderr:\n${run_stderr}")
endif()

message(STATUS "OK: wxMaxima exited with the expected code ${EXPECTED_EXIT_CODE}.")
