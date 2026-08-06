# GH #1672 regression test for wx-cd (src/wxMathML.lisp): wxMaxima keeps
# Maxima's working directory in sync with the worksheet's via wx-cd, and on
# Windows this used to fail for a directory name containing characters
# outside the OS's active codepage -- sb-posix:chdir there goes through the
# C runtime's ANSI-only _chdir (SBCL bug launchpad #2100706), which cannot
# represent e.g. CJK characters on a default English-Windows codepage.
#
# This is a CMake script rather than the check-wxMathML.sh POSIX-shell
# helper the other check-wxMathML.lisp_* tests use, specifically so it can
# also run on the Windows CI job (the .sh-based tests are labelled
# "needs_posix" and skipped there). The CJK directory name is a literal
# string in this UTF-8 source file, written into an input file for Maxima
# rather than passed as a process argument, so the only Unicode boundary
# being exercised is the one the bug is actually about: Maxima/SBCL reading
# a UTF-8-encoded path from its input and cd-ing into it.
#
# Expected -D arguments: MAXIMA (path to the maxima executable),
# WXMATHML_LISP (path to src/wxMathML.lisp), TEST_BASE_DIR (an existing,
# writable, plain-ASCII directory to create the non-ASCII fixture under).

if(NOT DEFINED MAXIMA OR NOT DEFINED WXMATHML_LISP OR NOT DEFINED TEST_BASE_DIR)
  message(FATAL_ERROR "MAXIMA, WXMATHML_LISP and TEST_BASE_DIR must all be set (-D...) when invoking this script.")
endif()

set(dirname "wxcd_test_文档")
set(test_dir "${TEST_BASE_DIR}/${dirname}")
file(MAKE_DIRECTORY "${test_dir}")

# wx-cd is always called by wxMaxima with the worksheet's own *file* path
# (see wxMaxima::SetCWD in src/wxMaxima.cpp), not a bare directory -- it
# strips the trailing name/type itself. Mirror that here via the trailing
# dummy.wxm component.
set(input_file "${TEST_BASE_DIR}/wxcd_non_ascii_input.txt")
file(WRITE "${input_file}"
  ":lisp-quiet (progn (wx-cd \"${test_dir}/dummy.wxm\") (princ (namestring *default-pathname-defaults*)))\n")

execute_process(
  COMMAND "${MAXIMA}" --quiet "--init-lisp=${WXMATHML_LISP}"
  INPUT_FILE "${input_file}"
  OUTPUT_VARIABLE maxima_output
  ERROR_VARIABLE maxima_error
  RESULT_VARIABLE maxima_result
)

# A failed chdir leaves *default-pathname-defaults* pointing at Maxima's own
# startup directory, which never contains "wxcd_test_" -- so that (ASCII)
# substring showing up in the printed namestring is proof the chdir into
# the non-ASCII directory actually succeeded. Matching only the ASCII
# prefix (not the CJK part) keeps this check independent of how the
# terminal/log re-encodes the captured output for display.
string(FIND "${maxima_output}" "wxcd_test_" found_pos)
if(found_pos EQUAL -1)
  message(FATAL_ERROR
    "wx-cd failed to cd into a non-ASCII directory name (GH #1672).\n"
    "Maxima exit code: ${maxima_result}\n"
    "Maxima stdout:\n${maxima_output}\n"
    "Maxima stderr:\n${maxima_error}")
endif()

message(STATUS "Success!")
