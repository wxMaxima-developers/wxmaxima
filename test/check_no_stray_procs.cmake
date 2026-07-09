# -*- mode: cmake -*-
#
# Regression guard for leaked child processes (see the Maxima/gnuplot
# child-process leak fixed alongside this test): wxMaxima owns the Maxima and
# gnuplot processes it launches and MUST terminate them before it exits.
# A survivor is a bug in its own right, and on the CI runners a leaked child
# also keeps wxMaxima's inherited stdout/stderr pipe open, so the launching
# ctest never sees EOF and hangs until its timeout.
#
# This script runs one `wxmaxima --batch <TESTFILE>` and then verifies that the
# run left behind no *new* Maxima/Lisp/gnuplot process. It is a `cmake -P`
# script (not a shell script) so it runs identically on the Linux and Windows
# CI - CMake is always available and can shell out to the per-platform process
# lister (tasklist / ps).
#
# Parameters (all passed as -D on the cmake -P command line):
#   WXMAXIMA  - path to the wxmaxima executable
#   TESTFILE  - batch file to run (relative to WORKDIR)
#   WORKDIR   - working directory for the run
#   GRACE     - seconds to keep polling for children to disappear (default 20)
#
# IMPORTANT: the test must be run with the RUN_SERIAL property so that no other
# test's Maxima is alive concurrently - otherwise a sibling batch test spawning
# its own Maxima under `ctest -j` would look like a stray of this run.

cmake_minimum_required(VERSION 3.16)

if(NOT DEFINED GRACE)
  set(GRACE 20)
endif()

# Exact process (base) names we consider "ours to have cleaned up": Maxima's
# launcher, the Lisp backends it may run as, and gnuplot. Matched by EXACT name
# equality (after lower-casing and stripping a trailing .exe) - NOT substring -
# so we neither match the wxmaxima process itself ("maxima" != "wxmaxima") nor
# unrelated processes that merely contain these letters (e.g. a kernel worker
# "kvfree_rcu_reclaim" contains "ecl").
set(PROC_NAMES "maxima" "sbcl" "clisp" "ecl" "gcl" "ccl" "gnuplot" "wgnuplot")

# Normalise a raw image/comm string to a lower-case base name (drop any
# directory and a trailing .exe) and, if it is one of PROC_NAMES, append
# "PID:name" to the list variable named by out_var.
function(_classify_proc out_var pid raw)
  get_filename_component(base "${raw}" NAME)
  string(TOLOWER "${base}" base)
  string(REGEX REPLACE "\\.exe$" "" base "${base}")
  if(base IN_LIST PROC_NAMES)
    set(acc "${${out_var}}")
    list(APPEND acc "${pid}:${base}")
    set(${out_var} "${acc}" PARENT_SCOPE)
  endif()
endfunction()

# Collect "PID:name" entries of every currently-running process whose base name
# is exactly one of PROC_NAMES.
function(collect_pids out_var)
  set(pids "")
  if(WIN32)
    execute_process(COMMAND tasklist /FO CSV /NH
                    OUTPUT_VARIABLE listing OUTPUT_STRIP_TRAILING_WHITESPACE
                    ERROR_QUIET)
    string(REPLACE "\r" "" listing "${listing}")
    string(REPLACE "\n" ";" lines "${listing}")
    foreach(line IN LISTS lines)
      # CSV row: "image.exe","PID","Session","N","Mem K"
      string(REGEX MATCH "^\"([^\"]+)\",\"([0-9]+)\"" matched "${line}")
      if(matched)
        _classify_proc(pids "${CMAKE_MATCH_2}" "${CMAKE_MATCH_1}")
      endif()
    endforeach()
  else()
    execute_process(COMMAND ps -eo "pid=,comm="
                    OUTPUT_VARIABLE listing OUTPUT_STRIP_TRAILING_WHITESPACE
                    ERROR_QUIET)
    string(REPLACE "\n" ";" lines "${listing}")
    foreach(line IN LISTS lines)
      string(STRIP "${line}" line)
      string(REGEX MATCH "^([0-9]+)[ \t]+(.+)$" matched "${line}")
      if(matched)
        _classify_proc(pids "${CMAKE_MATCH_1}" "${CMAKE_MATCH_2}")
      endif()
    endforeach()
  endif()
  if(pids)
    list(REMOVE_DUPLICATES pids)
  endif()
  set(${out_var} "${pids}" PARENT_SCOPE)
endfunction()

collect_pids(before)
message(STATUS "Pre-existing Maxima/gnuplot processes (ignored): ${before}")

# Run the batch file. wxMaxima's stdout/stderr are sent to files, NOT inherited
# from this script: that way a leaked child inherits a harmless file handle
# instead of the ctest pipe, so ctest can still see EOF and report this test's
# result promptly even when the leak we are testing for is present. We do not
# treat wxMaxima's own exit code as a failure - the point of this test is the
# cleanup, not whether the plot succeeded.
execute_process(
  COMMAND "${WXMAXIMA}" --debug --logtostderr --pipe --batch "${TESTFILE}"
  WORKING_DIRECTORY "${WORKDIR}"
  OUTPUT_FILE "${WORKDIR}/stray_process_check_run.out"
  ERROR_FILE  "${WORKDIR}/stray_process_check_run.err"
  RESULT_VARIABLE run_rc)
message(STATUS "wxmaxima batch run finished (exit code ${run_rc}).")

# Poll until either every child the run spawned is gone (success) or the grace
# period elapses (failure). A child that exits on its own within the grace
# period is harmless - only one that *outlives* the run holds the pipe open and
# indicates the leak - so we deliberately allow it a moment to die.
math(EXPR iterations "${GRACE} * 2")
set(strays "")
foreach(i RANGE ${iterations})
  collect_pids(after)
  set(strays "")
  foreach(entry IN LISTS after)
    if(NOT entry IN_LIST before)
      list(APPEND strays "${entry}")
    endif()
  endforeach()
  if(NOT strays)
    message(STATUS "OK: no stray Maxima/gnuplot process survived the batch run.")
    return()
  endif()
  execute_process(COMMAND "${CMAKE_COMMAND}" -E sleep 0.5)
endforeach()

message(FATAL_ERROR
  "wxMaxima left ${strays} running ${GRACE} s after the batch run exited. "
  "These Maxima/gnuplot children should have been killed during shutdown "
  "(child-process leak).")
