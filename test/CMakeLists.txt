# -*- mode: CMake; cmake-tab-width: 4; -*-

# Compile a list of test files
file(GLOB TEST_FILES automatic_test_files/*.png automatic_test_files/*.wxmx automatic_test_files/*.wxm automatic_test_files/*.mac automatic_test_files/*.cfg *.png)

find_program(MAXIMA maxima)
execute_process(COMMAND ${MAXIMA} --version
                OUTPUT_VARIABLE MAXIMA_VERSION_TEXT OUTPUT_STRIP_TRAILING_WHITESPACE)
message(STATUS "Found maxima: ${MAXIMA} (found version \"${MAXIMA_VERSION_TEXT}\")")

configure_file("${CMAKE_CURRENT_SOURCE_DIR}/xmlpatterngen.sh.in" xmlpatterngen.sh)
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/check-wxMathML.sh.in" check-wxMathML.sh)

# Copy the .wxm files to the binary dir so they can be used as test case without
# being afraid that we change the test cases somehow.
file(
    COPY
    ${TEST_FILES}
    DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    FILE_PERMISSIONS OWNER_WRITE OWNER_READ
    )

# IMPORTANT:
# On any reasonable system made in the last decade, the default timeout is
# more than enough. If *any* tests fail to complete within this timeout,
# it is a performance bug that must be investigated and fixed. Compared
# to any "real" work, the test documents are all trivial - even the one
# that has all cell types. Reference: On Bionic running on a 2009
# Core 2 Duo MacBook Pro, most tests run in under 10 seconds, and just
# a few take 30 seconds, and even that feels like it's too long to be
# acceptable. In other words: never extend test timeouts. Fix the
# performance bugs that cause such behavior.


set(CTEST_TEST_TIMEOUT 120)

if(WXM_UNIT_TESTS)
    add_subdirectory(unit_tests)
endif()

# Test if maxima is working
add_test(
    NAME runMaxima
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND echo "quit();" | ${MAXIMA})

# Test if maxima is able to communicate with a backend
add_test(
    NAME runMaxima-over-network
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND echo 'quit() ;' | nc -l 40000 & sleep 10 ; ${MAXIMA} -s 40000)

# A (very simple) test, if wxMathML.lisp file works (creates the expected XML output).
add_test(
    NAME check-wxMathML.lisp_1
    COMMAND "sh ${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "1+1;" "<math><lbl>(%o1) </lbl><mn>2</mn></math>"
)

add_test(
    NAME wxstatusbar
    COMMAND "sh ${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "wxstatusbar(\"a<1>b\");" "<statusbar>a&lt;1&gt;b</statusbar>"
)

add_test(
    NAME wxbuildinfo
    COMMAND "sh ${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "wxbuild_info();" "wxMaxima's idea of the directory layout is:"
)

add_test(
    NAME wxbug_report
    COMMAND "sh ${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "wxbug_report();" "wxMaxima is a graphical front end for Maxima, which does the mathematics in the background"
)

add_test(
    NAME wxxml-array-no-display-properties
    COMMAND "sh ${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "make_array(any,10,10);" "<mi lisp=.*>#{Lisp array \[10,10\]}</mi>"
)

# add_test(
#     NAME wxxml-array-display-properties
#     COMMAND "sh ${CMAKE_CURRENT_BINARY_DIR}/check-wxMathML.sh" "declare_index_properties(O,[presubscript,presuperscript,postsubscript,postsuperscript]);arraymake(O,[1,2,3,4]);" "<mmultiscripts .*<mi>O</mi>.*<mrow><mn>3</mn></mrow>.*<mrow>.*<mn>4</mn>.*</mrow>.*<mprescripts/>.*<mrow><mn>1</mn></mrow>.*<mrow><mn>2</mn></mrow>.*</mmultiscripts>"
# )

add_test(
    NAME wxmaxima_batch_emptyFile
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch empty_file.wxm)

add_test(
    NAME wxmaxima_performance
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch performance.wxm)

add_test(
    NAME wxmaxima_batch_textcell
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch textcells.wxm)

add_test(
    NAME wxmaxima_batch_foreign_characters
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch foreign-characters.wxm)

add_test(
    NAME wxmaxima_version_string
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --version)
set_tests_properties(wxmaxima_version_string PROPERTIES  PASS_REGULAR_EXPRESSION "wxMaxima ${VERSION}.*")

add_test(
    NAME wxmaxima_version_returncode
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --version)

#add_test(
#    NAME maxima_lisp_switch
#    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
#    COMMAND wxmaxima --logtostderr --pipe -l undefined_lisp)
#set_tests_properties(maxima_lisp_switch PROPERTIES WILL_FAIL true)

#add_test(
#    NAME maxima_version_switch
#    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
#    COMMAND wxmaxima --logtostderr --pipe -u undefined_version)
#set_tests_properties(maxima_version_switch PROPERTIES WILL_FAIL true)

#add_test(
#    NAME maxima_args_switch
#    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
#    COMMAND wxmaxima --logtostderr --pipe -X gibberish)
#set_tests_properties(maxima_args_switch PROPERTIES WILL_FAIL true)

add_test(
    NAME wxmaxima_help
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --help)

add_test(
    NAME all_celltypes
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch testbench_all_celltypes.wxm)

add_test(
    NAME simpleInput
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch simpleInput.wxm)

add_test(
    NAME rememberingAnswers
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch rememberingAnswers.wxm)

add_test(
    NAME absCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch absCells.wxm)

add_test(
    NAME absCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch absCells.wxm)

add_test(
    NAME boxCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch boxCells.wxm)

add_test(
    NAME boxCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch boxCells.wxm)

add_test(
    NAME diffCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch diffCells.wxm)

add_test(
    NAME diffCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch diffCells.wxm)

add_test(
    NAME atCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch atCells.wxm)

add_test(
    NAME atCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch atCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME atCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch atCells.wxm)

add_test(
    NAME conjugateCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch conjugateCells.wxm)

add_test(
    NAME conjugateCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch conjugateCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME conjugateCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch conjugateCells.wxm)

add_test(
    NAME exptCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch exptCells.wxm)

add_test(
    NAME exptCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch exptCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME exptCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch exptCells.wxm)

add_test(
    NAME fracCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch fracCells.wxm)

add_test(
    NAME fracCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch fracCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME fracCells_cmdline_wxmathml_over_network
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND echo 'load(fracCells.wxm) ; quit() ;' | nc -l 40000 & sleep 10 ; ${MAXIMA}-s 40000
    VERBATIM
    )

add_test(
    NAME fracCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch fracCells.wxm)

add_test(
    NAME intCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch intCells.wxm)

add_test(
    NAME intCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch intCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME intCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch intCells.wxm)

add_test(
    NAME intervals_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch intervals.wxm)

add_test(
    NAME intervals_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch intervals.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME intervals
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch intervals.wxm)

add_test(
    NAME functionCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch functionCells.wxm)

add_test(
    NAME functionCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch functionCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME functionCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch functionCells.wxm)

add_test(
    NAME slideshowCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch slideshowCells.wxm)

add_test(
    NAME imageCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch imageCells.wxm)
# Maxima sometimes fails if this test is run in parallel,
# see https://sourceforge.net/p/maxima/bugs/3699/
set_tests_properties(imageCells PROPERTIES RUN_SERIAL true)

add_test(
    NAME limitCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch limitCells.wxm)

add_test(
    NAME limitCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch limitCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME limitCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch limitCells.wxm)

add_test(
    NAME matrixCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch matrixCells.wxm)

add_test(
    NAME matrixCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch matrixCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME matrixCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch matrixCells.wxm)

add_test(
    NAME parenthesisCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch parenthesisCells.wxm)

add_test(
    NAME parenthesisCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch parenthesisCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME parenthesisCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch parenthesisCells.wxm)

add_test(
    NAME listCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch listCells.wxm)

add_test(
    NAME listCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch listCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME listCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch listCells.wxm)

add_test(
    NAME setCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch listCells.wxm)

add_test(
    NAME setCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch setCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME setCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch setCells.wxm)

add_test(
    NAME sqrtCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch sqrtCells.wxm)

add_test(
    NAME sqrtCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch sqrtCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME sqrtCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch sqrtCells.wxm)

add_test(
    NAME subCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --pipe --batch subCells.wxm)

add_test(
    NAME subCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --pipe --batch subCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME subCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch subCells.wxm)

add_test(
    NAME subsupCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch subsupCells.wxm)

add_test(
    NAME subsupCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch subsupCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME subsupCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch subsupCells.wxm)

add_test(
    NAME presubsupcells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --pipe --batch presubsupcells.wxm)

add_test(
    NAME presubsupcells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --pipe --batch presubsupcells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME presubsupcells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch presubsupcells.wxm)

add_test(
    NAME sumCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch sumCells.wxm)

add_test(
    NAME sumCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch sumCells.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME sumCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch sumCells.wxm)

add_test(
    NAME printf_simple_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch printf_simple.wxm)

add_test(
    NAME printf_simple_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --batch printf_simple.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME printf_simple
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch printf_simple.wxm)

add_test(
    NAME printf_equations_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --pipe --batch printf_equations.wxm)

add_test(
    NAME printf_equations_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --pipe --batch printf_equations.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME printf_equations
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch printf_equations.wxm)

add_test(
    NAME printf_continuationLines_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --pipe --batch printf_continuationLines.wxm)

add_test(
    NAME printf_continuationLines_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND ${MAXIMA} --pipe --batch printf_continuationLines.wxm -p ${CMAKE_SOURCE_DIR}/src/wxMathML.lisp)

add_test(
    NAME printf_continuationLines
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch printf_continuationLines.wxm)

add_test(
    NAME weirdLabels
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch weirdLabels.wxm)

add_test(
    NAME xmlQuote
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch xmlQuote.wxm)

add_test(
    NAME formerCrashes
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch formerCrashes.wxm)

add_test(
    NAME multiplication
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch multiplication.wxm)
add_test(
    NAME nonsenseConstructs
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch nonsenseConstructs.wxm)

add_test(
    NAME lisp
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch lisp.wxm)

add_test(
    NAME boxes
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch boxes.wxm)

add_test(
    NAME unicode
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch unicode.wxm)

add_test(
    NAME unicode_specialchars
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch unicode_specialchars.wxm)

add_test(
    NAME openMacFiles
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe -f autosave.cfg --batch testbench_simple.mac)

add_test(
    NAME openMacFiles2
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe -f autosave.cfg --batch testbench_simple2.mac)

add_test(
    NAME tutorial_10Minutes
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe  --batch 10MinuteTutorial.wxm)

add_test(
    NAME comment_begin
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch commentBegin.wxm)

add_test(
    NAME threadtest
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --batch threadtest.wxm)
# Maxima sometimes fails if this test is run in parallel,
# see https://sourceforge.net/p/maxima/bugs/3699/
set_tests_properties(threadtest PROPERTIES RUN_SERIAL true)

add_test(
    NAME autosave
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe -f autosave.cfg --batch unicode.wxm)

add_test(
    NAME noautosave
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe -f noautosave.cfg --batch unicode.wxm)

add_test(
    NAME config_from_19.11
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe -f config_from_19.11.cfg --batch unicode.wxm)

add_test(
    NAME invalid_commandline_arg
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --gibberish --batch unicode.wxm)
set_tests_properties(invalid_commandline_arg PROPERTIES WILL_FAIL true)

add_test(
    NAME multiple_batch_files
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --gibberish --batch unicode.wxm boxes.wxm)
set_tests_properties(multiple_batch_files PROPERTIES WILL_FAIL true)

add_test(
    NAME multiple_batch_files2
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --gibberish --batch --single_process unicode.wxm boxes.wxm)
set_tests_properties(multiple_batch_files2 PROPERTIES WILL_FAIL true)

#add_test(
#    NAME multiwindow
#    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
#    COMMAND wxmaxima --logtostderr --pipe --single_process --batch unicode.wxm boxes.wxm)

# Maxima sometimes fails if this test is run in parallel,
# see https://sourceforge.net/p/maxima/bugs/3699/
add_test(
    NAME multithreadtest
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostderr --pipe --single_process --batch threadtest.wxm threadtest2.wxm)
# Maxima sometimes fails if this test is run in parallel,
# see https://sourceforge.net/p/maxima/bugs/3699/
#set_tests_properties(multithreadtest PROPERTIES RUN_SERIAL true)

#add_test(
#    NAME ipc_copypaste1
#    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
#    COMMAND wxmaxima --enableipc --batch ipc_copypaste1.wxm)

#add_test(
#    NAME ipc_copypaste_all-celltypes
#    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
#    COMMAND wxmaxima --enableipc --batch ipc_copypaste_all-celltypes.wxm)

add_test(
    NAME tutorial_fastListAccess
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../examples
    COMMAND wxmaxima --logtostderr --pipe  --batch fastListAccess.wxm)

add_test(
    NAME tutorial_numberFormats
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../examples
    COMMAND wxmaxima --logtostderr --pipe  --batch numberFormats.wxm)

add_test(
    NAME tutorial_toleranceCalculations
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../examples
    COMMAND wxmaxima --logtostderr --pipe  --batch toleranceCalculations.wxm)

add_test(
    NAME tutorial_fittingEquations
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../examples
    COMMAND wxmaxima --logtostderr --pipe  --batch fittingEquations.wxm)

add_test(
    NAME tutorial_variableNames
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../examples
    COMMAND wxmaxima --logtostderr --pipe  --batch variableNames.wxm)

add_test(
    NAME tutorial_displaying3DCurves
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../examples
    COMMAND wxmaxima --logtostderr --pipe  --batch displaying3DCurves.wxm)

#add_test(
#    NAME tutorial_memoizing
#    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../examples
#    COMMAND wxmaxima --logtostderr --pipe  --batch memoizing.wxm)

add_test(
    NAME tutorial_solvingEquations
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../examples
    COMMAND wxmaxima --logtostderr --pipe  --batch solvingEquations.wxm)

find_program(DESKTOP_FILE_VALIDATE desktop-file-validate)
if(DESKTOP_FILE_VALIDATE)
    add_test(
        NAME check_desktop_file
        COMMAND ${DESKTOP_FILE_VALIDATE} ${CMAKE_SOURCE_DIR}/data/io.github.wxmaxima_developers.wxMaxima.desktop)
endif()

find_program(APPSTREAM_UTIL appstream-util)
if(APPSTREAM_UTIL)
    # Get appstream-util version number
    execute_process(COMMAND ${APPSTREAM_UTIL} --version
                    OUTPUT_VARIABLE APPSTREAM_UTIL_VERSION_TEXT)
    string(REGEX REPLACE "^Version:\t*([^\n]*).*" "\\1"  APPSTREAM_UTIL_VERSION "${APPSTREAM_UTIL_VERSION_TEXT}")
    message(STATUS "Found appstream-util: ${APPSTREAM_UTIL} (found version \"${APPSTREAM_UTIL_VERSION}\")")
    # In older versions (e.g. the version included in Ubuntu 18.04) the test fails, so disable it.
    # If it is okay in newer versions, I assume, our appstream file is valid and just the test was to hard.
    if(APPSTREAM_UTIL_VERSION VERSION_LESS "0.7.16")
        message(WARNING "appstream-util < 0.7.16 found. Disable the appdata validation test.")
    else()
        add_test(
            NAME check_appstream_file
            COMMAND ${APPSTREAM_UTIL} --nonet validate "${CMAKE_SOURCE_DIR}/data/io.github.wxmaxima_developers.wxMaxima.appdata.xml")
    endif()
else()
    message(STATUS "appstream-util not found. Do not check of the appstream file.")
endif()

