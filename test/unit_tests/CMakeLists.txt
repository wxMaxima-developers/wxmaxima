# -*- mode: CMake; cmake-tab-width: 4; -*-

add_compile_options("$<$<CXX_COMPILER_ID:MSVC>:/utf-8>")
set(CMAKE_CXX_CPPCHECK "")

include_directories(
    ${CMAKE_SOURCE_DIR}/src
    ${CMAKE_SOURCE_DIR}/test
    ${CMAKE_SOURCE_DIR}/test/unit_tests)

add_executable(test_CellPtr test_CellPtr.cpp)
target_link_libraries(test_CellPtr PRIVATE ${wxWidgets_LIBRARIES})
add_test(CellPtr test_CellPtr)

add_executable(test_ImgCell
  test_ImgCell.cpp
  ../../src/VisiblyInvalidCell.cpp
  ../../src/TextCell.cpp
)

if(WXM_USE_OPENMP AND OpenMP_CXX_FOUND)
    target_link_libraries(test_ImgCell PRIVATE OpenMP::OpenMP_CXX ${wxWidgets_LIBRARIES})
else()
    target_link_libraries(test_ImgCell PRIVATE ${wxWidgets_LIBRARIES})
endif()

add_test(ImgCell test_ImgCell)
