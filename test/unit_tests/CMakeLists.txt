# -*- mode: CMake; cmake-tab-width: 4; -*-

add_compile_options("$<$<CXX_COMPILER_ID:MSVC>:/utf-8>")
set(CMAKE_CXX_CPPCHECK "")

include_directories(
    ${CMAKE_SOURCE_DIR}/src
    ${CMAKE_SOURCE_DIR}/src/cells
    ${CMAKE_SOURCE_DIR}/test
    ${CMAKE_SOURCE_DIR}/test/unit_tests)

# These tests fail on GTK since I don't know how to init GTK for
# the tests and without initializing GTK using wxNORMAL_FONT causes
# a SIGTRAP
#
#add_executable(test_CellPtr test_CellPtr.cpp)
#target_link_libraries(test_CellPtr PRIVATE ${wxWidgets_LIBRARIES})
#target_compile_features(test_CellPtr PUBLIC cxx_std_14)
#add_test(CellPtr test_CellPtr)
#
# add_executable(test_SqrtCell test_SqrtCell.cpp)
# target_link_libraries(test_SqrtCell PRIVATE ${wxWidgets_LIBRARIES})
# target_compile_features(test_SqrtCell PUBLIC cxx_std_14)
# add_test(SqrtCell test_SqrtCell)
#
# add_executable(test_ImgCell test_ImgCell.cpp)
# target_link_libraries(test_ImgCell PRIVATE ${wxWidgets_LIBRARIES})
# target_compile_features(test_ImgCell PUBLIC cxx_std_14)
# add_test(ImgCell test_ImgCell)

add_executable(test_AFontSize test_AFontSize.cpp)
target_link_libraries(test_AFontSize PRIVATE ${wxWidgets_LIBRARIES})
#target_compile_features(test_ImgCell PUBLIC cxx_std_14)
add_test(AFontSize test_AFontSize)
