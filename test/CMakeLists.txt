# -*- mode: CMake; cmake-tab-width: 4; -*-

if(WXM_UNIT_TESTS)
    add_subdirectory(unit_tests)
endif()

file(GLOB TEST_FILES automatic_test_files/*.png automatic_test_files/*.wxmx automatic_test_files/*.wxm automatic_test_files/*.mac automatic_test_files/*.cfg *.png)

install(FILES ${TEST_FILES} DESTINATION share/wxMaxima)
file(
    COPY
    ${TEST_FILES}
    DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    FILE_PERMISSIONS OWNER_WRITE OWNER_READ
    )

# Test if maxima is working
add_test(
    NAME runMaxima
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND echo "quit();" | maxima)
set_tests_properties(runMaxima PROPERTIES TIMEOUT 60)

# Test if maxima is able to communicate with a backend
add_test(
    NAME runMaxima-over-network
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND echo 'quit() ;' | nc -l 40000 & sleep 10 ; maxima -s 40000)
set_tests_properties(runMaxima-over-network PROPERTIES TIMEOUT 60)

add_test(
    NAME wxmaxima_batch_emptyFile
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch empty_file.wxm)
set_tests_properties(wxmaxima_batch_emptyFile PROPERTIES TIMEOUT 60)

add_test(
    NAME wxmaxima_batch_textcell
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch textcells.wxm)
set_tests_properties(wxmaxima_batch_textcell PROPERTIES TIMEOUT 60)

add_test(
    NAME wxmaxima_batch_foreign_characters
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch foreign-characters.wxm)
set_tests_properties(wxmaxima_batch_foreign_characters PROPERTIES TIMEOUT 60)

add_test(
    NAME wxmaxima_version_string
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --version)
set_tests_properties(wxmaxima_version_string PROPERTIES  PASS_REGULAR_EXPRESSION "wxMaxima ${VERSION}.*" TIMEOUT 60)

add_test(
    NAME wxmaxima_version_returncode
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --version)
set_tests_properties(wxmaxima_version_returncode PROPERTIES TIMEOUT 60)

#add_test(
#    NAME maxima_lisp_switch
#    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
#    COMMAND wxmaxima --logtostdout --pipe -l undefined_lisp)
#set_tests_properties(maxima_lisp_switch PROPERTIES TIMEOUT 60 WILL_FAIL true)

#add_test(
#    NAME maxima_version_switch
#    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
#    COMMAND wxmaxima --logtostdout --pipe -u undefined_version)
#set_tests_properties(maxima_version_switch PROPERTIES TIMEOUT 60 WILL_FAIL true)

#add_test(
#    NAME maxima_args_switch
#    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
#    COMMAND wxmaxima --logtostdout --pipe -X gibberish)
#set_tests_properties(maxima_args_switch PROPERTIES TIMEOUT 60 WILL_FAIL true)

add_test(
    NAME wxmaxima_help
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --help)
set_tests_properties(wxmaxima_version_returncode PROPERTIES TIMEOUT 60)

add_test(
    NAME all_celltypes
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch testbench_all_celltypes.wxm)
set_tests_properties(all_celltypes PROPERTIES TIMEOUT 60)

add_test(
    NAME simpleInput
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch simpleInput.wxm)
set_tests_properties(simpleInput PROPERTIES TIMEOUT 60)

add_test(
    NAME rememberingAnswers
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch rememberingAnswers.wxm)
set_tests_properties(rememberingAnswers PROPERTIES TIMEOUT 60)

add_test(
    NAME absCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --batch absCells.wxm)
set_tests_properties(absCells_cmdline PROPERTIES TIMEOUT 60)

add_test(
    NAME absCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch absCells.wxm)
set_tests_properties(absCells PROPERTIES TIMEOUT 60)

add_test(
    NAME diffCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --batch diffCells.wxm)
set_tests_properties(diffCells_cmdline PROPERTIES TIMEOUT 60)

add_test(
    NAME diffCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch diffCells.wxm)
set_tests_properties(diffCells PROPERTIES TIMEOUT 60)

add_test(
    NAME atCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --batch atCells.wxm)
set_tests_properties(atCells_cmdline PROPERTIES TIMEOUT 60)

add_test(
    NAME atCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --batch atCells.wxm -p ${CMAKE_SOURCE_DIR}/data/wxMathML.lisp)
set_tests_properties(atCells_cmdline_wxmathml PROPERTIES TIMEOUT 60)

add_test(
    NAME atCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch atCells.wxm)
set_tests_properties(atCells PROPERTIES TIMEOUT 60)

add_test(
    NAME conjugateCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --batch conjugateCells.wxm)
set_tests_properties(conjugateCells_cmdline PROPERTIES TIMEOUT 60)

add_test(
    NAME conjugateCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --batch conjugateCells.wxm -p ${CMAKE_SOURCE_DIR}/data/wxMathML.lisp)
set_tests_properties(conjugateCells_cmdline_wxmathml PROPERTIES TIMEOUT 60)

add_test(
    NAME conjugateCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch conjugateCells.wxm)
set_tests_properties(conjugateCells PROPERTIES TIMEOUT 60)

add_test(
    NAME exptCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --batch exptCells.wxm)
set_tests_properties(exptCells_cmdline PROPERTIES TIMEOUT 60)

add_test(
    NAME exptCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --batch exptCells.wxm -p ${CMAKE_SOURCE_DIR}/data/wxMathML.lisp)
set_tests_properties(exptCells_cmdline_wxmathml PROPERTIES TIMEOUT 60)

add_test(
    NAME exptCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch exptCells.wxm)
set_tests_properties(exptCells PROPERTIES TIMEOUT 60)

add_test(
    NAME fracCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --batch fracCells.wxm)
set_tests_properties(fracCells_cmdline PROPERTIES TIMEOUT 60)

add_test(
    NAME fracCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --batch fracCells.wxm -p ${CMAKE_SOURCE_DIR}/data/wxMathML.lisp)
set_tests_properties(fracCells_cmdline_wxmathml PROPERTIES TIMEOUT 60)

add_test(
    NAME fracCells_cmdline_wxmathml_over_network
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND echo 'load(fracCells.wxm) ; quit() ;' | nc -l 40000 & sleep 10 ; maxima -s 40000
    VERBATIM
    )
set_tests_properties(fracCells_cmdline_wxmathml_over_network PROPERTIES TIMEOUT 60)

add_test(
    NAME fracCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch fracCells.wxm)
set_tests_properties(fracCells_cmdline PROPERTIES TIMEOUT 60)

add_test(
    NAME intCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --batch intCells.wxm)
set_tests_properties(intCells_cmdline PROPERTIES TIMEOUT 60)

add_test(
    NAME intCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --batch intCells.wxm -p ${CMAKE_SOURCE_DIR}/data/wxMathML.lisp)
set_tests_properties(intCells_cmdline_wxmathml PROPERTIES TIMEOUT 60)

add_test(
    NAME intCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch intCells.wxm)
set_tests_properties(intCells PROPERTIES TIMEOUT 60)

add_test(
    NAME functionCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --batch functionCells.wxm)
set_tests_properties(functionCells_cmdline PROPERTIES TIMEOUT 60)

add_test(
    NAME functionCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --batch functionCells.wxm -p ${CMAKE_SOURCE_DIR}/data/wxMathML.lisp)
set_tests_properties(functionCells_cmdline_wxmathml PROPERTIES TIMEOUT 60)

add_test(
    NAME functionCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch functionCells.wxm)
set_tests_properties(functionCells PROPERTIES TIMEOUT 60)

add_test(
    NAME slideshowCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch slideshowCells.wxm)
set_tests_properties(slideshowCells PROPERTIES TIMEOUT 60)

add_test(
    NAME imageCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch imageCells.wxm)
set_tests_properties(imageCells PROPERTIES TIMEOUT 60)

add_test(
    NAME limitCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --batch limitCells.wxm)
set_tests_properties(limitCells_cmdline PROPERTIES TIMEOUT 60)

add_test(
    NAME limitCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --batch limitCells.wxm -p ${CMAKE_SOURCE_DIR}/data/wxMathML.lisp)
set_tests_properties(limitCells_cmdline_wxmathml PROPERTIES TIMEOUT 60)

add_test(
    NAME limitCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch limitCells.wxm)
set_tests_properties(limitCells PROPERTIES TIMEOUT 60)

add_test(
    NAME matrixCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --batch matrixCells.wxm)
set_tests_properties(matrixCells_cmdline PROPERTIES TIMEOUT 60)

add_test(
    NAME matrixCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --batch matrixCells.wxm -p ${CMAKE_SOURCE_DIR}/data/wxMathML.lisp)
set_tests_properties(matrixCells_cmdline_wxmathml PROPERTIES TIMEOUT 60)

add_test(
    NAME matrixCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch matrixCells.wxm)
set_tests_properties(matrixCells PROPERTIES TIMEOUT 60)

add_test(
    NAME parenthesisCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --batch parenthesisCells.wxm)
set_tests_properties(parenthesisCells_cmdline PROPERTIES TIMEOUT 60)

add_test(
    NAME parenthesisCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --batch parenthesisCells.wxm -p ${CMAKE_SOURCE_DIR}/data/wxMathML.lisp)
set_tests_properties(parenthesisCells_cmdline_wxmathml PROPERTIES TIMEOUT 60)

add_test(
    NAME parenthesisCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch parenthesisCells.wxm)
set_tests_properties(parenthesisCells PROPERTIES TIMEOUT 60)

add_test(
    NAME listCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --batch parenthesisCells.wxm)
set_tests_properties(parenthesisCells_cmdline PROPERTIES TIMEOUT 60)

add_test(
    NAME listCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --batch listCells.wxm -p ${CMAKE_SOURCE_DIR}/data/wxMathML.lisp)
set_tests_properties(listCells_cmdline_wxmathml PROPERTIES TIMEOUT 60)

add_test(
    NAME listCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch listCells.wxm)
set_tests_properties(listCells PROPERTIES TIMEOUT 60)

add_test(
    NAME sqrtCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --batch sqrtCells.wxm)
set_tests_properties(sqrtCells_cmdline PROPERTIES TIMEOUT 60)

add_test(
    NAME sqrtCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --batch sqrtCells.wxm -p ${CMAKE_SOURCE_DIR}/data/wxMathML.lisp)
set_tests_properties(sqrtCells_cmdline_wxmathml PROPERTIES TIMEOUT 60)

add_test(
    NAME sqrtCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch sqrtCells.wxm)
set_tests_properties(sqrtCells PROPERTIES TIMEOUT 60)

add_test(
    NAME subCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --pipe --batch subCells.wxm)
set_tests_properties(subCells_cmdline PROPERTIES TIMEOUT 60)

add_test(
    NAME subCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --pipe --batch subCells.wxm -p ${CMAKE_SOURCE_DIR}/data/wxMathML.lisp)
set_tests_properties(subCells_cmdline_wxmathml PROPERTIES TIMEOUT 60)

add_test(
    NAME subCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch subCells.wxm)
set_tests_properties(subCells PROPERTIES TIMEOUT 60)

add_test(
    NAME subsupCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --batch subsupCells.wxm)
set_tests_properties(subsupCells_cmdline PROPERTIES TIMEOUT 60)

add_test(
    NAME subsupCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --batch subsupCells.wxm -p ${CMAKE_SOURCE_DIR}/data/wxMathML.lisp)
set_tests_properties(subsupCells_cmdline_wxmathml PROPERTIES TIMEOUT 60)

add_test(
    NAME subsupCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch subsupCells.wxm)
set_tests_properties(subsupCells PROPERTIES TIMEOUT 60)

add_test(
    NAME presubsupcells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --pipe --batch presubsupcells.wxm)
set_tests_properties(presubsupcells_cmdline PROPERTIES TIMEOUT 60)

add_test(
    NAME presubsupcells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --pipe --batch presubsupcells.wxm -p ${CMAKE_SOURCE_DIR}/data/wxMathML.lisp)
set_tests_properties(presubsupcells_cmdline_wxmathml PROPERTIES TIMEOUT 60)

add_test(
    NAME presubsupcells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch presubsupcells.wxm)
set_tests_properties(presubsupcells PROPERTIES TIMEOUT 60)

add_test(
    NAME sumCells_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --batch sumCells.wxm)
set_tests_properties(sumCells_cmdline PROPERTIES TIMEOUT 60)

add_test(
    NAME sumCells_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --batch sumCells.wxm -p ${CMAKE_SOURCE_DIR}/data/wxMathML.lisp)
set_tests_properties(sumCells_cmdline_wxmathml PROPERTIES TIMEOUT 60)

add_test(
    NAME sumCells
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch sumCells.wxm)
set_tests_properties(sumCells PROPERTIES TIMEOUT 60)

add_test(
    NAME printf_simple_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --batch printf_simple.wxm)
set_tests_properties(printf_simple_cmdline PROPERTIES TIMEOUT 60)

add_test(
    NAME printf_simple_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --batch printf_simple.wxm -p ${CMAKE_SOURCE_DIR}/data/wxMathML.lisp)
set_tests_properties(printf_simple_cmdline_wxmathml PROPERTIES TIMEOUT 60)

add_test(
    NAME printf_simple
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch printf_simple.wxm)
set_tests_properties(printf_simple PROPERTIES TIMEOUT 60)

add_test(
    NAME printf_equations_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --pipe --batch printf_equations.wxm)
set_tests_properties(printf_equations_cmdline PROPERTIES TIMEOUT 60)

add_test(
    NAME printf_equations_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --pipe --batch printf_equations.wxm -p ${CMAKE_SOURCE_DIR}/data/wxMathML.lisp)
set_tests_properties(printf_equations_cmdline_wxmathml PROPERTIES TIMEOUT 60)

add_test(
    NAME printf_equations
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch printf_equations.wxm)
set_tests_properties(printf_equations PROPERTIES TIMEOUT 60)

add_test(
    NAME printf_continuationLines_cmdline
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --pipe --batch printf_continuationLines.wxm)
set_tests_properties(printf_continuationLines_cmdline PROPERTIES TIMEOUT 60)

add_test(
    NAME printf_continuationLines_cmdline_wxmathml
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND maxima --pipe --batch printf_continuationLines.wxm -p ${CMAKE_SOURCE_DIR}/data/wxMathML.lisp)
set_tests_properties(printf_continuationLines_cmdline_wxmathml PROPERTIES TIMEOUT 60)

add_test(
    NAME printf_continuationLines
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch printf_continuationLines.wxm)
set_tests_properties(printf_continuationLines PROPERTIES TIMEOUT 60)

add_test(
    NAME weirdLabels
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch weirdLabels.wxm)
set_tests_properties(weirdLabels PROPERTIES TIMEOUT 60)

add_test(
    NAME xmlQuote
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch xmlQuote.wxm)
set_tests_properties(xmlQuote PROPERTIES TIMEOUT 60)

add_test(
    NAME formerCrashes
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch formerCrashes.wxm)
set_tests_properties(formerCrashes PROPERTIES TIMEOUT 60)

add_test(
    NAME multiplication
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch multiplication.wxm)
set_tests_properties(multiplication PROPERTIES TIMEOUT 60)

add_test(
    NAME nonsenseConstructs
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch nonsenseConstructs.wxm)
set_tests_properties(nonsenseConstructs PROPERTIES TIMEOUT 60)

add_test(
    NAME lisp
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch lisp.wxm)
set_tests_properties(lisp PROPERTIES TIMEOUT 60)

add_test(
    NAME boxes
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch boxes.wxm)
set_tests_properties(boxes PROPERTIES TIMEOUT 60)

add_test(
    NAME unicode
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch unicode.wxm)
set_tests_properties(unicode PROPERTIES TIMEOUT 60)

add_test(
    NAME unicode_specialchars
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch unicode_specialchars.wxm)
set_tests_properties(unicode_specialchars PROPERTIES TIMEOUT 60)

add_test(
    NAME openMacFiles
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe -f autosave.cfg --batch testbench_simple.mac)
set_tests_properties(openMacFiles PROPERTIES TIMEOUT 240)

add_test(
    NAME openMacFiles2
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe -f autosave.cfg --batch testbench_simple2.mac)
set_tests_properties(openMacFiles2 PROPERTIES TIMEOUT 60)

add_test(
    NAME comment_begin
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe --batch commentBegin.wxm)
set_tests_properties(comment_begin PROPERTIES TIMEOUT 60)

# add_test(
#     NAME threadtest
#     WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
#     COMMAND wxmaxima --logtostdout --pipe --batch threadtest.wxm)
# set_tests_properties(threadtest PROPERTIES TIMEOUT 160)

add_test(
    NAME autosave
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe -f autosave.cfg --batch unicode.wxm)
set_tests_properties(autosave PROPERTIES TIMEOUT 60)

add_test(
    NAME noautosave
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe -f noautosave.cfg --batch unicode.wxm)
set_tests_properties(noautosave PROPERTIES TIMEOUT 60)

add_test(
    NAME config_from_19.11
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --logtostdout --pipe -f config_from_19.11.cfg --batch unicode.wxm)
set_tests_properties(config_from_19.11 PROPERTIES TIMEOUT 60)

add_test(
    NAME invalid_commandline_arg
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --gibberish --batch unicode.wxm)
set_tests_properties(invalid_commandline_arg PROPERTIES TIMEOUT 60 WILL_FAIL true)

add_test(
    NAME ipc_copypaste1
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --enableipc --batch ipc_copypaste1.wxm)
set_tests_properties(ipc_copypaste1 PROPERTIES TIMEOUT 60)

# This test times out in CI. There's some work to be done in IPC
# code to make it do the right thing.
add_test(
    NAME ipc_copypaste2
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/automatic_test_files
    COMMAND wxmaxima --enableipc --batch ipc_copypaste2.wxm)
set_tests_properties(ipc_copypaste2 PROPERTIES TIMEOUT 60 DISABLED YES)

find_program(DESKTOP_FILE_VALIDATE desktop-file-validate)
add_test(
    NAME check_desktop_file
    COMMAND ${DESKTOP_FILE_VALIDATE} ${CMAKE_SOURCE_DIR}/data/io.github.wxmaxima_developers.wxMaxima.desktop)
set_tests_properties(check_desktop_file PROPERTIES TIMEOUT 60)
if(NOT DESKTOP_FILE_VALIDATE)
    set_tests_properties(check_desktop_file PROPERTIES DISABLED YES)
endif()

find_program(APPSTREAM_UTIL appstream-util)
add_test(
    NAME check_appstream_file
    COMMAND ${APPSTREAM_UTIL} validate ${CMAKE_SOURCE_DIR}/data/io.github.wxmaxima_developers.wxMaxima.appdata.xml)
set_tests_properties(check_appstream_file PROPERTIES TIMEOUT 600)
if(NOT APPSTREAM_UTIL)
    set_tests_properties(check_appstream_file PROPERTIES DISABLED YES)
endif()

