# Runs one Maxima command through src/wxMathML.lisp and checks that the
# output matches a regular expression. The tests using it are defined with
# wxm_add_wxmathml_test() in test/CMakeLists.txt.
#
# This used to be check-wxMathML.sh, a POSIX shell script, which had two
# problems this script doesn't (GH #1775):
#  - It only ran where there is a POSIX shell, so none of these tests ran on
#    Windows.
#  - It only searched Maxima's stdout. Some Maxima builds print error messages
#    to stderr, so a test that expects an error message (load_symbol) failed
#    on them even though Maxima printed exactly the expected text.
#
# The command and the regular expression are read from files that
# wxm_add_wxmathml_test() writes at configure time, rather than being passed
# as arguments: they are full of quotes, angle brackets, backslashes and
# semicolons, each of which some layer between CMake and Maxima (CTest's
# command line, Windows' argument quoting, maxima.bat) would otherwise mangle.
#
# The regular expression uses CMake's syntax, not grep's: "(", ")", "+", "?"
# and "|" are operators and need a backslash to stand for themselves.
#
# Expected -D arguments:
#   MAXIMA         the maxima executable (maxima.bat on Windows)
#   WXMATHML_LISP  src/wxMathML.lisp
#   INPUT_FILE     a file holding what is sent to Maxima's standard input
#   REGEX_FILE     a file holding the regular expression

foreach(var MAXIMA WXMATHML_LISP INPUT_FILE REGEX_FILE)
  if(NOT ${var})
    message(FATAL_ERROR "${var} isn't set. Is Maxima installed?")
  endif()
endforeach()

file(READ "${REGEX_FILE}" expected_regex)

# Naming the same variable for both streams merges them in the order Maxima
# wrote them.
execute_process(
  COMMAND "${MAXIMA}" --quiet "--init-lisp=${WXMATHML_LISP}"
  INPUT_FILE "${INPUT_FILE}"
  OUTPUT_VARIABLE maxima_output
  ERROR_VARIABLE maxima_output
  RESULT_VARIABLE maxima_result
)

# The expressions are written for the output on one line, line breaks
# replaced by spaces.
string(REPLACE "\r\n" " " maxima_output "${maxima_output}")
string(REPLACE "\n" " " maxima_output "${maxima_output}")

# Always print the output: some tests also set FAIL_REGULAR_EXPRESSION, which
# CTest checks against what this script prints.
message("${maxima_output}")

if(maxima_output MATCHES "${expected_regex}")
  message("Success!")
else()
  message("Regex for expected output:\n${expected_regex}")
  message("Maxima's exit code: ${maxima_result}")
  message(FATAL_ERROR "Maxima's output doesn't match the expected regex.")
endif()
