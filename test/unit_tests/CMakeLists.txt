# -*- mode: CMake; cmake-tab-width: 4; -*-

add_compile_options("$<$<CXX_COMPILER_ID:MSVC>:/utf-8>")
set(CMAKE_CXX_CPPCHECK "")
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include_directories(
    ${CMAKE_SOURCE_DIR}/src
    ${CMAKE_SOURCE_DIR}/src/cells
    ${CMAKE_SOURCE_DIR}/test
    ${CMAKE_SOURCE_DIR}/test/unit_tests
    ${CMAKE_BINARY_DIR})

add_executable(test_CellPtr test_CellPtr.cpp)
target_link_libraries(test_CellPtr PRIVATE ${wxWidgets_LIBRARIES})
add_test(CellPtr test_CellPtr)

add_executable(test_SqrtCell test_SqrtCell.cpp)
target_link_libraries(test_SqrtCell PRIVATE ${wxWidgets_LIBRARIES})
add_test(SqrtCell test_SqrtCell)

add_executable(test_ImgCell test_ImgCell.cpp)
target_link_libraries(test_ImgCell PRIVATE ${wxWidgets_LIBRARIES})
add_test(ImgCell test_ImgCell)

add_executable(test_AFontSize test_AFontSize.cpp)
target_link_libraries(test_AFontSize PRIVATE ${wxWidgets_LIBRARIES})
#target_compile_features(test_ImgCell PUBLIC cxx_std_14)
add_test(AFontSize test_AFontSize)

add_executable(test_DiffAlgorithm test_DiffAlgorithm.cpp)
target_link_libraries(test_DiffAlgorithm PRIVATE ${wxWidgets_LIBRARIES})
add_test(DiffAlgorithm test_DiffAlgorithm)

# The diff viewer's scroll-sync geometry is GUI-free (see dialogs/DiffScrollSync),
# so it is tested directly, without the heavy wxmTestApp object library.
add_executable(test_DiffScrollSync test_DiffScrollSync.cpp)
target_link_libraries(test_DiffScrollSync PRIVATE ${wxWidgets_LIBRARIES})
add_test(DiffScrollSync test_DiffScrollSync)

# Layout/recalculation regression test. Unlike the tests above (which stub out
# GroupCell via TestStubs.cpp), this links the real application code through the
# wxmTestApp object library so it can exercise the genuine GroupCell/EditorCell
# recalculation pipeline.
if(TARGET wxmTestApp)
    add_executable(test_GroupCellLayout
        test_GroupCellLayout.cpp groupcell_test_stubs.cpp
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_GroupCellLayout PUBLIC cxx_std_20)
    target_link_libraries(test_GroupCellLayout PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
    add_test(GroupCellLayout test_GroupCellLayout)

    # Hardening/fuzz test for EditorCell's text-editing core: drives the real
    # key-event paths and checks the cursor/selection invariants after every op.
    add_executable(test_EditorCellInvariants
        test_EditorCellInvariants.cpp groupcell_test_stubs.cpp
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_EditorCellInvariants PUBLIC cxx_std_20)
    target_link_libraries(test_EditorCellInvariants PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
    add_test(EditorCellInvariants test_EditorCellInvariants)
endif()
