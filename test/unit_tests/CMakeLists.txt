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

# Linked into every test executable: a static initializer that, on Windows,
# disables the modal crash/assert dialogs which would otherwise hang a failing
# test until ctest's watchdog kills it. Compiles to nothing off Windows.
set(WXM_TEST_SETUP "${CMAKE_CURRENT_SOURCE_DIR}/wxm_test_setup.cpp")

add_executable(test_CellPtr test_CellPtr.cpp ${WXM_TEST_SETUP})
target_link_libraries(test_CellPtr PRIVATE ${wxWidgets_LIBRARIES})
add_test(CellPtr test_CellPtr)

add_executable(test_SqrtCell test_SqrtCell.cpp ${WXM_TEST_SETUP})
target_link_libraries(test_SqrtCell PRIVATE ${wxWidgets_LIBRARIES})
add_test(SqrtCell test_SqrtCell)

add_executable(test_ImgCell test_ImgCell.cpp ${WXM_TEST_SETUP})
target_link_libraries(test_ImgCell PRIVATE ${wxWidgets_LIBRARIES})
add_test(ImgCell test_ImgCell)

add_executable(test_AFontSize test_AFontSize.cpp ${WXM_TEST_SETUP})
target_link_libraries(test_AFontSize PRIVATE ${wxWidgets_LIBRARIES})
#target_compile_features(test_ImgCell PUBLIC cxx_std_14)
add_test(AFontSize test_AFontSize)

add_executable(test_DiffAlgorithm test_DiffAlgorithm.cpp ${WXM_TEST_SETUP})
target_link_libraries(test_DiffAlgorithm PRIVATE ${wxWidgets_LIBRARIES})
add_test(DiffAlgorithm test_DiffAlgorithm)

# The diff viewer's scroll-sync geometry is GUI-free (see dialogs/DiffScrollSync),
# so it is tested directly, without the heavy wxmTestApp object library.
add_executable(test_DiffScrollSync test_DiffScrollSync.cpp ${WXM_TEST_SETUP})
target_link_libraries(test_DiffScrollSync PRIVATE ${wxWidgets_LIBRARIES})
add_test(DiffScrollSync test_DiffScrollSync)

# Layout/recalculation regression test. Unlike the tests above (which stub out
# GroupCell via TestStubs.cpp), this links the real application code through the
# wxmTestApp object library so it can exercise the genuine GroupCell/EditorCell
# recalculation pipeline.
if(TARGET wxmTestApp)
    add_executable(test_GroupCellLayout
        test_GroupCellLayout.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_GroupCellLayout PUBLIC cxx_std_20)
    target_link_libraries(test_GroupCellLayout PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
    add_test(GroupCellLayout test_GroupCellLayout)

    # Hardening/fuzz test for EditorCell's text-editing core: drives the real
    # key-event paths and checks the cursor/selection invariants after every op.
    add_executable(test_EditorCellInvariants
        test_EditorCellInvariants.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_EditorCellInvariants PUBLIC cxx_std_20)
    target_link_libraries(test_EditorCellInvariants PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
    add_test(EditorCellInvariants test_EditorCellInvariants)

    # Round-trip test for the worksheet styles: guards that ReadStyles() and
    # WriteStyles() share one TextStyle -> config-key mapping so styles can never
    # be written and read under mismatched keys.
    add_executable(test_StyleConfigRoundtrip
        test_StyleConfigRoundtrip.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_StyleConfigRoundtrip PUBLIC cxx_std_20)
    target_link_libraries(test_StyleConfigRoundtrip PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
    add_test(StyleConfigRoundtrip test_StyleConfigRoundtrip)

    # Direct parser test for MaximaManual's help anchors: batch mode no longer
    # compiles anchors (interactive-only), so they need their own regression net.
    add_executable(test_ManualAnchors
        test_ManualAnchors.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_ManualAnchors PUBLIC cxx_std_20)
    target_link_libraries(test_ManualAnchors PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
    add_test(ManualAnchors test_ManualAnchors)
endif()

# Same watchdog timeout the parent directory applies to its tests: keep a hung
# unit test (e.g. one that trips an assert and would otherwise wait forever on a
# modal assert dialog) from blocking the whole suite. WXM_TEST_TIMEOUT is set in
# the parent test/CMakeLists.txt before this subdirectory is added.
#
# Also tag every test in this directory with the "unittest" label. These tests
# are self-contained C++ executables that need neither Maxima nor a headless
# display, so platforms that cannot run the full integration suite (e.g. the
# Windows CI build) can still run them via `ctest -L unittest`.
get_property(_wxm_unit_tests DIRECTORY PROPERTY TESTS)
foreach(_wxm_t IN LISTS _wxm_unit_tests)
  get_test_property("${_wxm_t}" TIMEOUT _wxm_existing_timeout)
  if(NOT _wxm_existing_timeout)
    set_property(TEST "${_wxm_t}" PROPERTY TIMEOUT ${WXM_TEST_TIMEOUT})
  endif()
  set_property(TEST "${_wxm_t}" APPEND PROPERTY LABELS unittest)
endforeach()
