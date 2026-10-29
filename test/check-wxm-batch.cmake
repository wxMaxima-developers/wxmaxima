# Loads a .wxm file with Maxima's batch() and checks Maxima's output against a
# regular expression -- that is, that Maxima can still read a file wxMaxima
# wrote, without help from wxMaxima. Used by the wxm_loads_in_maxima test
# (GH #1907): its file contains text cells that would end their comments
# early, and known answers, which would be read as code, if they weren't
# escaped.
#
# The batch() command is sent through a file rather than --batch-string, so no
# quoting layer between CMake and Maxima (CTest's command line, Windows'
# argument quoting, maxima.bat) can mangle it.
#
# Expected -D arguments:
#   MAXIMA     the maxima executable (maxima.bat on Windows)
#   WXM_FILE   the .wxm file to load
#   WORK_DIR   a directory to write the command file to
#   REGEX      the regular expression Maxima's output has to match

foreach(var MAXIMA WXM_FILE WORK_DIR REGEX)
  if(NOT ${var})
    message(FATAL_ERROR "${var} isn't set. Is Maxima installed?")
  endif()
endforeach()

file(TO_CMAKE_PATH "${WXM_FILE}" wxm_file)
file(WRITE "${WORK_DIR}/wxm_batch_input.mac" "batch(\"${wxm_file}\")$\n")

# Naming the same variable for both streams merges them in the order Maxima
# wrote them.
execute_process(
  COMMAND "${MAXIMA}" --quiet
  INPUT_FILE "${WORK_DIR}/wxm_batch_input.mac"
  OUTPUT_VARIABLE maxima_output
  ERROR_VARIABLE maxima_output
  RESULT_VARIABLE maxima_result
)

# Always print the output: the test also sets FAIL_REGULAR_EXPRESSION, which
# CTest checks against what this script prints.
message("${maxima_output}")

if(maxima_output MATCHES "${REGEX}")
  message("Success!")
else()
  message("Regex for expected output:\n${REGEX}")
  message("Maxima's exit code: ${maxima_result}")
  message(FATAL_ERROR "Maxima's output doesn't match the expected regex.")
endif()
