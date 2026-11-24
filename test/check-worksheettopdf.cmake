# -*- mode: cmake -*-
#
# Runs wxworksheettopdf() through a real Maxima in a batch run and checks
# that the PDF it asks for gets written, and is tagged. test_PdfExport
# checks the PDF's contents in detail; this checks the way there: the Lisp
# command, its options, and wxMaxima acting on the request.
#
# Parameters (-D on the cmake -P command line):
#   WXMAXIMA - path to the wxmaxima executable
#   WORKDIR  - directory holding worksheettopdf.wxm

cmake_minimum_required(VERSION 3.16)

set(pdf "${WORKDIR}/worksheettopdf_out.pdf")
file(REMOVE "${pdf}")
execute_process(
  COMMAND "${WXMAXIMA}" --batch --exit-on-error worksheettopdf.wxm
  WORKING_DIRECTORY "${WORKDIR}"
  RESULT_VARIABLE result
  OUTPUT_VARIABLE output
  ERROR_VARIABLE output)
if(NOT result EQUAL 0)
  message(FATAL_ERROR "The batch run failed (${result}):\n${output}")
endif()
if(NOT EXISTS "${pdf}")
  message(FATAL_ERROR "wxworksheettopdf() wrote no PDF:\n${output}")
endif()
file(READ "${pdf}" head LIMIT 5 HEX)
if(NOT head STREQUAL "255044462d") # "%PDF-"
  message(FATAL_ERROR "worksheettopdf_out.pdf is no PDF")
endif()
# The catalog isn't compressed: these are plain text in the file.
file(STRINGS "${pdf}" tagged REGEX "/StructTreeRoot")
if(NOT tagged)
  message(FATAL_ERROR "worksheettopdf_out.pdf isn't tagged")
endif()
# paper = letter: a page 612 points wide
file(STRINGS "${pdf}" letter REGEX "/MediaBox \\[0 0 612")
if(NOT letter)
  message(FATAL_ERROR "worksheettopdf_out.pdf didn't get the paper size it asked for")
endif()
message(STATUS "wxworksheettopdf() wrote a tagged PDF")
