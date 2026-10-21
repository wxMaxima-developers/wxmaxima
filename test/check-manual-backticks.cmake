# Guard against a stray backtick in the manual's Markdown source (GH #2271).
#
# A backtick that doesn't open or close a code span reaches pandoc's LaTeX
# output as a literal ` character. Hungarian's babel module (magyar.ldf) makes
# ` an active character, and pandoc >= 3.2 loads babel with shorthands=off,
# which leaves that active character undefined: every manual that contains one
# fails to compose as a Hungarian PDF with "Undefined control sequence" /
# "Missing $ inserted". The English source is what every language without a
# translated manual (Hungarian among them) is built from, so it must not
# contain one.
#
# Checked per paragraph (blank-line separated, fenced code blocks skipped):
# an odd number of backticks in a paragraph means one of them is unpaired.
# Code spans that use doubled backticks still count evenly.
#
# Run standalone with:
#   cmake -DMANUAL=info/wxmaxima.md -P test/check-manual-backticks.cmake

cmake_minimum_required(VERSION 3.16)

if(NOT MANUAL)
  message(FATAL_ERROR "check-manual-backticks: MANUAL must be set")
endif()

file(READ "${MANUAL}" text)
# Make the text safe to split into a CMake list: semicolons would split lines
# further, and brackets would stop the list from splitting at all.
string(REPLACE ";" "<semicolon>" text "${text}")
string(REPLACE "[" "<lbracket>" text "${text}")
string(REPLACE "]" "<rbracket>" text "${text}")
string(REPLACE "\\" "<backslash>" text "${text}")
# A trailing empty line closes the last paragraph.
string(REPLACE "\n" ";" lines "${text};")

set(linenumber 0)
set(in_fence FALSE)
set(backticks "")
set(paragraph_start 0)
set(bad "")
foreach(line IN LISTS lines)
  math(EXPR linenumber "${linenumber} + 1")
  if(line MATCHES "^[ \t]*(```|~~~)")
    if(in_fence)
      set(in_fence FALSE)
    else()
      set(in_fence TRUE)
    endif()
    continue()
  endif()
  if(in_fence)
    continue()
  endif()
  if(line MATCHES "^[ \t]*$")
    string(LENGTH "${backticks}" count)
    math(EXPR odd "${count} % 2")
    if(odd)
      list(APPEND bad ${paragraph_start})
    endif()
    set(backticks "")
    set(paragraph_start 0)
    continue()
  endif()
  if(paragraph_start EQUAL 0)
    set(paragraph_start ${linenumber})
  endif()
  string(REGEX REPLACE "[^`]" "" ticks "${line}")
  string(APPEND backticks "${ticks}")
endforeach()

if(bad)
  string(REPLACE ";" ", " bad "${bad}")
  message(FATAL_ERROR
    "${MANUAL}: unpaired backtick in the paragraph starting at line(s) ${bad}. "
    "It ends up as a literal ` in the LaTeX pandoc writes, which breaks "
    "composing the Hungarian manual PDF (GH #2271).")
endif()
message(STATUS "${MANUAL}: no unpaired backticks")
