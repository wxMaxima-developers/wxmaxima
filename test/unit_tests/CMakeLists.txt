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

if(MINGW)
    # Statically link the GCC/C++/winpthread runtimes into the test executables.
    # Otherwise each exe depends on libstdc++-6.dll, libgcc_s_seh-1.dll and
    # libwinpthread-1.dll being found at launch; if the loader cannot find one it
    # pops a modal "the code execution cannot proceed" dialog *before main()*,
    # which under ctest just hangs the test until the watchdog kills it -- the
    # "every unit test times out with no output" symptom seen on the Windows CI.
    # An in-process SetErrorMode() cannot help: the loader runs before our code.
    # Static linking makes the test exes self-contained and removes the failure
    # mode. add_link_options() applies to every target defined below it here.
    add_link_options(-static)
endif()

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

# The worksheet's virtual-size arithmetic (extracted from Worksheet::AdjustSize)
# is GUI-free (see WorksheetSizeMath.h), so it is tested directly.
add_executable(test_WorksheetSizeMath test_WorksheetSizeMath.cpp ${WXM_TEST_SETUP})
target_link_libraries(test_WorksheetSizeMath PRIVATE ${wxWidgets_LIBRARIES})
add_test(WorksheetSizeMath test_WorksheetSizeMath)

# The parser for the <variables> XML Maxima sends (extracted from
# wxMaxima::ReadVariables) is GUI-free, so it is tested directly too.
add_executable(test_MaximaVariableUpdates
    test_MaximaVariableUpdates.cpp ${WXM_TEST_SETUP})
target_link_libraries(test_MaximaVariableUpdates PRIVATE ${wxWidgets_LIBRARIES})
add_test(MaximaVariableUpdates test_MaximaVariableUpdates)

# Guards the wxWidgets 3.3 "item" assert that wxMenu::GetHelpString() trips when
# a highlighted menu id is not an item of that menu (seen mainly via the Windows
# native menu code). MenuHelpString() is a tiny GUI-object helper, so it is
# tested directly -- and, crucially, the Windows CI's `ctest -L unittest` step
# runs this even though it drives no GUI, giving Gunter (who has no Windows box)
# a check for a Windows-surfacing bug.
add_executable(test_MenuHelpString
    test_MenuHelpString.cpp ${CMAKE_SOURCE_DIR}/src/MenuHelpString.cpp ${WXM_TEST_SETUP})
target_link_libraries(test_MenuHelpString PRIVATE ${wxWidgets_LIBRARIES})
add_test(MenuHelpString test_MenuHelpString)

# Guards the Greek/symbol sidebars' Buttonwrapsizer (a wxWrapSizer subclass that
# uses the protected m_availSize -- fragile across wx versions, broke wrapping on
# wx 3.0 and again on wx 3.3). Small, standalone: compiles the class directly.
add_executable(test_ButtonWrapSizer
    test_ButtonWrapSizer.cpp ${CMAKE_SOURCE_DIR}/src/sidebars/ButtonWrapSizer.cpp
    ${WXM_TEST_SETUP})
target_link_libraries(test_ButtonWrapSizer PRIVATE ${wxWidgets_LIBRARIES})
add_test(ButtonWrapSizer test_ButtonWrapSizer)

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

    # Drives the full layout pipeline (RequestRecalculation ->
    # RecalculateIfNeeded -> virtual size pushed to a mock view) with real
    # GroupCells and a memory-DC Configuration but NO Worksheet window - the
    # payoff of the WorksheetLayout split.
    add_executable(test_WorksheetLayout
        test_WorksheetLayout.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_WorksheetLayout PUBLIC cxx_std_20)
    target_link_libraries(test_WorksheetLayout PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
    add_test(WorksheetLayout test_WorksheetLayout)

    # Logic-level regression tests for the custom wxAccessible classes (worksheet
    # and toolbar). They only assert where wxUSE_ACCESSIBILITY is set (the Windows
    # CI); elsewhere they compile out to a harness sanity check. This catches the
    # accessibility-logic bugs (orphaned accessible, wrong role, invisible tools)
    # in CI instead of via the multi-day screen-reader feedback loop.
    add_executable(test_Accessibility
        test_Accessibility.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_Accessibility PUBLIC cxx_std_20)
    target_link_libraries(test_Accessibility PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
    add_test(Accessibility test_Accessibility)

    # Guards everything a .wxm file (= the copy-to-clipboard path) can hold --
    # images in several formats, every text cell type, code cell answers and the
    # send-known-answers flag -- surviving the TreeToWXM/TreeFromWXM round-trip.
    add_executable(test_WXMRoundtrip
        test_WXMRoundtrip.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_WXMRoundtrip PUBLIC cxx_std_20)
    target_link_libraries(test_WXMRoundtrip PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
    add_test(WXMRoundtrip test_WXMRoundtrip)

    # Round-trip guard for the .wxmx content.xml (the MathML-like math format):
    # parsing a content.xml into a cell tree and serialising it back must be a
    # fixed point, over the real corpus plus a crafted all-math-classes fragment.
    add_executable(test_WXMXRoundtrip
        test_WXMXRoundtrip.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_WXMXRoundtrip PUBLIC cxx_std_20)
    target_compile_definitions(test_WXMXRoundtrip PRIVATE
        WXM_CORPUS_DIR="${CMAKE_SOURCE_DIR}/test/fuzz/corpus_mathparser")
    target_link_libraries(test_WXMXRoundtrip PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
    add_test(WXMXRoundtrip test_WXMXRoundtrip)

    # Safety net for extracting the export/serialization cluster out of
    # Worksheet: loads the real math corpus plus sentinel cells into a live
    # Worksheet, runs ExportToHTML/TeX/MAC and the selection-to-string
    # converters twice each, and requires byte-identical, content-complete
    # output. WXM_EXPORT_DUMP_DIR=<dir> keeps the files for a before/after
    # `diff -r` around the refactor.
    add_executable(test_WorksheetExport
        test_WorksheetExport.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_WorksheetExport PUBLIC cxx_std_20)
    target_compile_definitions(test_WorksheetExport PRIVATE
        WXM_CORPUS_DIR="${CMAKE_SOURCE_DIR}/test/fuzz/corpus_mathparser")
    target_link_libraries(test_WorksheetExport PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
    add_test(WorksheetExport test_WorksheetExport)

    # Pins the right-click context menu (WorksheetContextMenu, extracted from
    # Worksheet::OnMouseRightDown): drives the main right-click states on a
    # real Worksheet and checks the expected menu entries appear.
    add_executable(test_WorksheetContextMenu
        test_WorksheetContextMenu.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_WorksheetContextMenu PUBLIC cxx_std_20)
    target_link_libraries(test_WorksheetContextMenu PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
    add_test(WorksheetContextMenu test_WorksheetContextMenu)

    # Pins the worksheet's two-cursor model (h-caret between cells XOR editor
    # cursor inside a cell; neither while cells are selected) ahead of the
    # WorksheetCursor extraction.
    add_executable(test_WorksheetCursor
        test_WorksheetCursor.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_WorksheetCursor PUBLIC cxx_std_20)
    target_link_libraries(test_WorksheetCursor PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
    add_test(WorksheetCursor test_WorksheetCursor)

    # Pins the worksheet's find/replace behavior (wrap-around group loop,
    # prompt/editor/output order, start-from-cursor skip rules) ahead of the
    # WorksheetSearch extraction.
    add_executable(test_WorksheetFind
        test_WorksheetFind.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_WorksheetFind PUBLIC cxx_std_20)
    target_link_libraries(test_WorksheetFind PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
    add_test(WorksheetFind test_WorksheetFind)

    # Hardening/fuzz test for EditorCell's text-editing core: drives the real
    # key-event paths and checks the cursor/selection invariants after every op.
    add_executable(test_EditorCellInvariants
        test_EditorCellInvariants.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_EditorCellInvariants PUBLIC cxx_std_20)
    target_link_libraries(test_EditorCellInvariants PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
    add_test(EditorCellInvariants test_EditorCellInvariants)

    # Layout idempotency invariants: after zoom / canvas-size changes, the
    # converged layout must equal a fresh layout of the same content - catches
    # any cell that stays marked "recalculated" while holding stale geometry
    # (the MatrCell-after-deadline / ParenCell-spacing bug family).
    add_executable(test_LayoutInvariants
        test_LayoutInvariants.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_LayoutInvariants PUBLIC cxx_std_20)
    target_link_libraries(test_LayoutInvariants PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
    add_test(LayoutInvariants test_LayoutInvariants)

    # Round-trip test for the mechanical scalar settings: guards that
    # ReadConfig() and the settings-write side share one key<->member table
    # (Configuration::ScalarConfigSettings) so a setting can never be written
    # and read under mismatched keys or types.
    add_executable(test_ConfigRoundtrip
        test_ConfigRoundtrip.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_ConfigRoundtrip PUBLIC cxx_std_20)
    target_link_libraries(test_ConfigRoundtrip PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
    add_test(ConfigRoundtrip test_ConfigRoundtrip)

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

    # Regression test for the cell-tree undo/redo (TreeUndo_*); the safety net for
    # extracting that subsystem out of the Worksheet god class.
    add_executable(test_TreeUndo
        test_TreeUndo.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_TreeUndo PUBLIC cxx_std_20)
    target_link_libraries(test_TreeUndo PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
    add_test(TreeUndo test_TreeUndo)

    # Drives a real GreekSidebar to guard its line wrapping: when too small to show
    # every letter it must grow its virtual height so the wrapped rows stay
    # reachable by the vertical scrollbar (broke on wx 3.3 -- OnSize clamped it).
    add_executable(test_GreekSidebar
        test_GreekSidebar.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_GreekSidebar PUBLIC cxx_std_20)
    target_link_libraries(test_GreekSidebar PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
    add_test(GreekSidebar test_GreekSidebar)

    # Drives a real BTextCtrl to guard the last-active-text-control tracking
    # (which text control the sidebars' symbol buttons type into). It was a raw
    # pointer in Configuration that a destroyed control could not unregister
    # (issue #2027) -- a use-after-free once its wizard was closed; now a
    # wxWeakRef-backed static on BTextCtrl that must null itself on destruction.
    add_executable(test_LastActiveTextCtrl
        test_LastActiveTextCtrl.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_LastActiveTextCtrl PUBLIC cxx_std_20)
    target_link_libraries(test_LastActiveTextCtrl PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
    add_test(LastActiveTextCtrl test_LastActiveTextCtrl)

    # Guards the declarative variable-to-menu-items table (MaximaMenuSync) that
    # keeps menu check marks in sync with the option variables Maxima reports -
    # it replaced a dozen hand-written VariableAction* handlers in wxMaxima.
    # Builds real menus from the table's own ids and asserts on the check marks.
    add_executable(test_MaximaMenuSync
        test_MaximaMenuSync.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_MaximaMenuSync PUBLIC cxx_std_20)
    target_link_libraries(test_MaximaMenuSync PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
    add_test(MaximaMenuSync test_MaximaMenuSync)
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
get_property(wxm_unit_tests DIRECTORY PROPERTY TESTS)
foreach(wxm_t IN LISTS wxm_unit_tests)
  get_test_property("${wxm_t}" TIMEOUT wxm_existing_timeout)
  if(NOT wxm_existing_timeout)
    set_property(TEST "${wxm_t}" PROPERTY TIMEOUT ${WXM_TEST_TIMEOUT})
  endif()
  set_property(TEST "${wxm_t}" APPEND PROPERTY LABELS unittest)
endforeach()
