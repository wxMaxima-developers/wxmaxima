file(GLOB TEST_FILES *.png *.wxmx)

if(WIN32)
    install(FILES ${TEST_FILES} DESTINATION wxMaxima/data)
else()
    install(FILES ${TEST_FILES} DESTINATION share/wxMaxima)
endif()
file(COPY ${TEST_FILES} DESTINATION ${CMAKE_CURRENT_BINARY_DIR})


add_test(NAME wxmaxima_batch_textcell WORKING_DIRECTORY ${CMAKE_BINARY_DIR} COMMAND ./wxmaxima-local --batch test/textcells.wxmx)
add_test(NAME wxmaxima_batch_foreign_characters WORKING_DIRECTORY ${CMAKE_BINARY_DIR} COMMAND ./wxmaxima-local --batch test/foreign-characters.wxmx)
add_test(NAME wxmaxima_version_string WORKING_DIRECTORY ${CMAKE_BINARY_DIR} COMMAND ./wxmaxima-local --version)
set_tests_properties(wxmaxima_version_string PROPERTIES  PASS_REGULAR_EXPRESSION "wxMaxima ${VERSION}.*")
add_test(NAME wxmaxima_version_returncode WORKING_DIRECTORY ${CMAKE_BINARY_DIR} COMMAND ./wxmaxima-local --version)
add_test(NAME wxmaxima_help WORKING_DIRECTORY ${CMAKE_BINARY_DIR} COMMAND ./wxmaxima-local --help)

add_test(NAME all_celltypes WORKING_DIRECTORY ${CMAKE_BINARY_DIR} COMMAND ./wxmaxima-local --batch test/testbench_all_celltypes.wxmx)
set_tests_properties(all_celltypes PROPERTIES TIMEOUT 180)

add_test(NAME other_features WORKING_DIRECTORY ${CMAKE_BINARY_DIR} COMMAND ./wxmaxima-local --batch test/other-features.wxmx)
set_tests_properties(other_features PROPERTIES TIMEOUT 180)

add_test(NAME misc_problematic_things WORKING_DIRECTORY ${CMAKE_BINARY_DIR} COMMAND ./wxmaxima-local --batch test/misc-problematic-things.wxmx)
set_tests_properties(misc_problematic_things PROPERTIES TIMEOUT 180)

add_test(NAME simpleInput WORKING_DIRECTORY ${CMAKE_BINARY_DIR} COMMAND ./wxmaxima-local --batch test/testbench_automatic_simpleInput.wxm)
set_tests_properties(simpleInput PROPERTIES TIMEOUT 180)

add_test(NAME rememberingAnswers WORKING_DIRECTORY ${CMAKE_BINARY_DIR} COMMAND ./wxmaxima-local --batch test/testbench_automatic_rememberingAnswers.wxm)
set_tests_properties(rememberingAnswers PROPERTIES TIMEOUT 180)

add_test(NAME absCells WORKING_DIRECTORY ${CMAKE_BINARY_DIR} COMMAND ./wxmaxima-local --batch test/testbench_automatic_absCells.wxm)
set_tests_properties(absCells PROPERTIES TIMEOUT 180)

add_test(NAME diffCells WORKING_DIRECTORY ${CMAKE_BINARY_DIR} COMMAND ./wxmaxima-local --batch test/testbench_automatic_diffCells.wxm)
set_tests_properties(diffCells PROPERTIES TIMEOUT 180)

add_test(NAME atCells WORKING_DIRECTORY ${CMAKE_BINARY_DIR} COMMAND ./wxmaxima-local --batch test/testbench_automatic_atCells.wxm)
set_tests_properties(atCells PROPERTIES TIMEOUT 180)

add_test(NAME conjugateCells WORKING_DIRECTORY ${CMAKE_BINARY_DIR} COMMAND ./wxmaxima-local --batch test/testbench_automatic_conjugateCells.wxm)
set_tests_properties(conjugateCells PROPERTIES TIMEOUT 180)

add_test(NAME exptCells WORKING_DIRECTORY ${CMAKE_BINARY_DIR} COMMAND ./wxmaxima-local --batch test/testbench_automatic_exptCells.wxm)
set_tests_properties(exptCells PROPERTIES TIMEOUT 180)

add_test(NAME fracCells WORKING_DIRECTORY ${CMAKE_BINARY_DIR} COMMAND ./wxmaxima-local --batch test/testbench_automatic_fracCells.wxm)
set_tests_properties(fracCells PROPERTIES TIMEOUT 180)

add_test(NAME intCells WORKING_DIRECTORY ${CMAKE_BINARY_DIR} COMMAND ./wxmaxima-local --batch test/testbench_automatic_intCells.wxm)
set_tests_properties(intCells PROPERTIES TIMEOUT 180)

add_test(NAME functionCells WORKING_DIRECTORY ${CMAKE_BINARY_DIR} COMMAND ./wxmaxima-local --batch test/testbench_automatic_functionCells.wxm)
set_tests_properties(functionCells PROPERTIES TIMEOUT 180)

add_test(NAME runMaxima WORKING_DIRECTORY ${CMAKE_BINARY_DIR} COMMAND echo "quit();"|maxima)
set_tests_properties(runMaxima PROPERTIES TIMEOUT 180)

