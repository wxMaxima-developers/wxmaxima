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

# On Windows a test executable needs an application manifest, or the loader
# binds COMCTL32 to the legacy 5.82 assembly, which lacks the window-subclassing
# entry points wx's GUI code imports, and refuses to start it at all. See
# wxm_test_manifest.rc for the full story.
#
# Only for the tests that are built standalone, though. The ones that link the
# wxmTestApp object library already get wx's resources from src/wxmaxima.rc,
# which is part of that library; adding a second copy makes the linker fail with
# "duplicate resource" for every bitmap and icon in it. Cygwin builds no PE
# resources at all.
if(WIN32 AND NOT CYGWIN)
    set(WXM_TEST_MANIFEST "${CMAKE_CURRENT_SOURCE_DIR}/wxm_test_manifest.rc")
else()
    set(WXM_TEST_MANIFEST "")
endif()

# GUI tests are started through the headless wrapper, which supplies an isolated
# display and refuses to fall back to the developer's desktop. It is a POSIX
# shell script, though, and Windows cannot execute one: ctest reports
# BAD_COMMAND and the test never runs. Windows runners have a real desktop
# session anyway - which is exactly what the EnsureDisplay() helpers this
# replaced said in their own "#ifndef _WIN32" - so there the executable is
# started directly. An empty launcher expands to nothing in the COMMAND below.
if(WIN32)
    set(WXM_TEST_LAUNCHER "")
else()
    set(WXM_TEST_LAUNCHER "${WXM_TEST_WRAPPER}")
endif()

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

add_executable(test_CellPtr test_CellPtr.cpp ${WXM_TEST_SETUP} ${WXM_TEST_MANIFEST})
target_link_libraries(test_CellPtr PRIVATE ${wxWidgets_LIBRARIES})
add_test(CellPtr test_CellPtr)

add_executable(test_SqrtCell test_SqrtCell.cpp ${WXM_TEST_SETUP} ${WXM_TEST_MANIFEST})
target_link_libraries(test_SqrtCell PRIVATE ${wxWidgets_LIBRARIES})
add_test(SqrtCell test_SqrtCell)

add_executable(test_ImgCell test_ImgCell.cpp ${WXM_TEST_SETUP} ${WXM_TEST_MANIFEST})
target_link_libraries(test_ImgCell PRIVATE ${wxWidgets_LIBRARIES})
add_test(ImgCell test_ImgCell)

if(WXM_SANITIZE)
    # These three tests build a deliberately partial class universe: they
    # #include a hand-picked set of src/ files (via TestStubs.cpp) and leave
    # e.g. EditorCell.cpp out. UBSan's vptr check emits a reference to the
    # std::type_info of every polymorphic class accessed through a pointer -
    # a symbol that is only defined next to the class's vtable in the .cpp
    # that was left out, so the sanitized link fails with "undefined
    # reference to typeinfo for EditorCell" (whether it does depends on the
    # GCC version's codegen; GCC 14 emits it, 15 doesn't). The vptr check is
    # meaningless for these stubbed builds anyway, so switch it off for them.
    target_compile_options(test_CellPtr PRIVATE -fno-sanitize=vptr)
    target_compile_options(test_SqrtCell PRIVATE -fno-sanitize=vptr)
    target_compile_options(test_ImgCell PRIVATE -fno-sanitize=vptr)
endif()

add_executable(test_AFontSize test_AFontSize.cpp ${WXM_TEST_SETUP} ${WXM_TEST_MANIFEST})
target_link_libraries(test_AFontSize PRIVATE ${wxWidgets_LIBRARIES})
#target_compile_features(test_ImgCell PUBLIC cxx_std_14)
add_test(AFontSize test_AFontSize)

add_executable(test_DiffAlgorithm test_DiffAlgorithm.cpp ${WXM_TEST_SETUP} ${WXM_TEST_MANIFEST})
target_link_libraries(test_DiffAlgorithm PRIVATE ${wxWidgets_LIBRARIES})
add_test(DiffAlgorithm test_DiffAlgorithm)

# The diff viewer's scroll-sync geometry is GUI-free (see dialogs/DiffScrollSync),
# so it is tested directly, without the heavy wxmTestApp object library.
add_executable(test_DiffScrollSync test_DiffScrollSync.cpp ${WXM_TEST_SETUP} ${WXM_TEST_MANIFEST})
target_link_libraries(test_DiffScrollSync PRIVATE ${wxWidgets_LIBRARIES})
add_test(DiffScrollSync test_DiffScrollSync)

# The worksheet's virtual-size arithmetic (extracted from Worksheet::AdjustSize)
# is GUI-free (see WorksheetSizeMath.h), so it is tested directly.
add_executable(test_WorksheetSizeMath test_WorksheetSizeMath.cpp ${WXM_TEST_SETUP} ${WXM_TEST_MANIFEST})
target_link_libraries(test_WorksheetSizeMath PRIVATE ${wxWidgets_LIBRARIES})
add_test(WorksheetSizeMath test_WorksheetSizeMath)

# The parser for the <variables> XML Maxima sends (extracted from
# wxMaxima::ReadVariables) is GUI-free, so it is tested directly too.
add_executable(test_MaximaVariableUpdates
    test_MaximaVariableUpdates.cpp ${WXM_TEST_SETUP} ${WXM_TEST_MANIFEST})
target_link_libraries(test_MaximaVariableUpdates PRIVATE ${wxWidgets_LIBRARIES})
add_test(MaximaVariableUpdates test_MaximaVariableUpdates)

# The sbcl-LDB recognition helpers (LdbSupport) are GUI-free, so they are tested
# directly against captured stdout/stderr text.
add_executable(test_LdbSupport test_LdbSupport.cpp ${WXM_TEST_SETUP} ${WXM_TEST_MANIFEST})
target_include_directories(test_LdbSupport PRIVATE ${CMAKE_SOURCE_DIR}/src)
target_link_libraries(test_LdbSupport PRIVATE ${wxWidgets_LIBRARIES})
add_test(LdbSupport test_LdbSupport)

# The Maxima-protocol helpers (MaximaProtocol) - prompt classification and the
# blank-command predicate - are GUI-free, so they are tested directly against
# captured prompt/command strings.
add_executable(test_MaximaProtocol test_MaximaProtocol.cpp ${WXM_TEST_SETUP} ${WXM_TEST_MANIFEST})
target_include_directories(test_MaximaProtocol PRIVATE ${CMAKE_SOURCE_DIR}/src)
target_link_libraries(test_MaximaProtocol PRIVATE ${wxWidgets_LIBRARIES})
add_test(MaximaProtocol test_MaximaProtocol)

# Guards the wxWidgets 3.3 "item" assert that wxMenu::GetHelpString() trips when
# a highlighted menu id is not an item of that menu (seen mainly via the Windows
# native menu code). MenuHelpString() is a tiny GUI-object helper, so it is
# tested directly -- and, crucially, the Windows CI's `ctest -L unittest` step
# runs this even though it drives no GUI, giving Gunter (who has no Windows box)
# a check for a Windows-surfacing bug.
add_executable(test_MenuHelpString
    test_MenuHelpString.cpp ${CMAKE_SOURCE_DIR}/src/MenuHelpString.cpp ${WXM_TEST_SETUP} ${WXM_TEST_MANIFEST})
target_link_libraries(test_MenuHelpString PRIVATE ${wxWidgets_LIBRARIES})
add_test(NAME MenuHelpString
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_MenuHelpString>")

# Guards the Greek/symbol sidebars' Buttonwrapsizer (a wxWrapSizer subclass that
# uses the protected m_availSize -- fragile across wx versions, broke wrapping on
# wx 3.0 and again on wx 3.3). Small, standalone: compiles the class directly.
add_executable(test_ButtonWrapSizer
    test_ButtonWrapSizer.cpp ${CMAKE_SOURCE_DIR}/src/sidebars/ButtonWrapSizer.cpp
    ${WXM_TEST_SETUP} ${WXM_TEST_MANIFEST})
target_link_libraries(test_ButtonWrapSizer PRIVATE ${wxWidgets_LIBRARIES})
add_test(NAME ButtonWrapSizer
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_ButtonWrapSizer>")

# Layout/recalculation regression test. Unlike the tests above (which stub out
# GroupCell via TestStubs.cpp), this links the real application code through the
# wxmTestApp object library so it can exercise the genuine GroupCell/EditorCell
# recalculation pipeline.
if(TARGET wxmTestApp)
    # wxmTestApp is an OBJECT library: linking it via $<TARGET_OBJECTS:...>
    # (below) pulls in its object files but not its own target_link_libraries()
    # requirements, so anything one of those objects needs - currently just
    # libfribidi, for Bidi.cpp, see src/CMakeLists.txt - has to be linked into
    # each executable below directly too.
    set(WXM_TESTAPP_EXTRA_LIBS "")
    if(TARGET PkgConfig::FRIBIDI)
        list(APPEND WXM_TESTAPP_EXTRA_LIBS PkgConfig::FRIBIDI)
    endif()

    add_executable(test_GroupCellLayout
        test_GroupCellLayout.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_GroupCellLayout PUBLIC cxx_std_20)
    target_link_libraries(test_GroupCellLayout PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${WXM_TESTAPP_EXTRA_LIBS})
    add_test(NAME GroupCellLayout
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_GroupCellLayout>")

    # Drives the full layout pipeline (RequestRecalculation ->
    # RecalculateIfNeeded -> virtual size pushed to a mock view) with real
    # GroupCells and a memory-DC Configuration but NO Worksheet window - the
    # payoff of the WorksheetLayout split.
    add_executable(test_WorksheetLayout
        test_WorksheetLayout.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_WorksheetLayout PUBLIC cxx_std_20)
    target_link_libraries(test_WorksheetLayout PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${WXM_TESTAPP_EXTRA_LIBS})
    add_test(NAME WorksheetLayout
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_WorksheetLayout>")

    # Line-wrapping inside EditorCells: soft breaks must be derived layout
    # state that never alters or leaks out of the cell's content, for the
    # prose path and (once enabled) the code path.
    add_executable(test_EditorCellWrapping
        test_EditorCellWrapping.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_EditorCellWrapping PUBLIC cxx_std_20)
    target_link_libraries(test_EditorCellWrapping PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${WXM_TESTAPP_EXTRA_LIBS})
    add_test(NAME EditorCellWrapping
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_EditorCellWrapping>")

    # Configuration::GetAsciiArtColumns() tells Maxima's own ASCII-art 2D/1D
    # printer ($linel) how many columns of the worksheet's monospace
    # ASCII-math font currently fit into the window (#1608).
    add_executable(test_AsciiArtLinel
        test_AsciiArtLinel.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_AsciiArtLinel PUBLIC cxx_std_20)
    target_link_libraries(test_AsciiArtLinel PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${WXM_TESTAPP_EXTRA_LIBS})
    add_test(NAME AsciiArtLinel
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_AsciiArtLinel>")

    # What the caret does inside bidirectional text: a position in a Hebrew word
    # has to map to a point in the word's *drawn* order, which is the reverse of
    # the order it is stored in.
    add_executable(test_EditorCellBidi
        test_EditorCellBidi.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_EditorCellBidi PUBLIC cxx_std_20)
    target_link_libraries(test_EditorCellBidi PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${WXM_TESTAPP_EXTRA_LIBS})
    # Started through the headless wrapper rather than spawning an Xvfb of its
    # own, as the other GUI tests still do. The wrapper picks a free display
    # instead of a fixed :99 two tests can collide on, kills the server it
    # started, waits for it to be ready rather than sleeping, and refuses to
    # fall back to the developer's own desktop. It also keeps a system() call
    # out of the test, which is what flawfinder rightly objects to.
    add_test(NAME EditorCellBidi
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_EditorCellBidi>")

    # Real tab-character support: a '\t' is a single character in m_text,
    # expanded to the next 4-column stop only at draw/measure time
    # (NextTabStop()/GetLineWidth()), plus the Tab-key and Backspace behavior
    # that changed once a tab stopped being 1-4 literal spaces.
    add_executable(test_EditorCellTabs
        test_EditorCellTabs.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_EditorCellTabs PUBLIC cxx_std_20)
    target_link_libraries(test_EditorCellTabs PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${WXM_TESTAPP_EXTRA_LIBS})
    add_test(NAME EditorCellTabs
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_EditorCellTabs>")

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
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${WXM_TESTAPP_EXTRA_LIBS})
    add_test(NAME Accessibility
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_Accessibility>")

    # Guards everything a .wxm file (= the copy-to-clipboard path) can hold --
    # images in several formats, every text cell type, code cell answers and the
    # send-known-answers flag -- surviving the TreeToWXM/TreeFromWXM round-trip.
    add_executable(test_WXMRoundtrip
        test_WXMRoundtrip.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_WXMRoundtrip PUBLIC cxx_std_20)
    target_link_libraries(test_WXMRoundtrip PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${WXM_TESTAPP_EXTRA_LIBS})
    add_test(NAME WXMRoundtrip
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_WXMRoundtrip>")

    # Guards a plain .mac file (Format::TreeToWXM(cell, wxm=false) /
    # Format::ParseMACContents(), the code ExportToMAC()/OpenMACFile() use)
    # surviving wxMaxima's open/save round-trip byte-for-byte, focused on tab
    # characters -- the concrete case that used to get silently rewritten to
    # spaces.
    add_executable(test_MACRoundtrip
        test_MACRoundtrip.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_MACRoundtrip PUBLIC cxx_std_20)
    target_link_libraries(test_MACRoundtrip PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${WXM_TESTAPP_EXTRA_LIBS})
    add_test(NAME MACRoundtrip
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_MACRoundtrip>")

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
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${WXM_TESTAPP_EXTRA_LIBS})
    add_test(NAME WXMXRoundtrip
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_WXMXRoundtrip>")

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
        WXM_CORPUS_DIR="${CMAKE_SOURCE_DIR}/test/fuzz/corpus_mathparser"
        WXM_TESTFILES_DIR="${CMAKE_SOURCE_DIR}/test/automatic_test_files")
    target_link_libraries(test_WorksheetExport PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${WXM_TESTAPP_EXTRA_LIBS})
    add_test(NAME WorksheetExport
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_WorksheetExport>")

    # Safety net for the multi-format clipboard / drag-export payload: builds
    # the real Copy()/CopyCells() data objects over a live document and pins
    # that their advertised formats are distinct (no wxDataFormat collision -
    # the class of bug that trips the clipboard assertion) and each round-trips.
    add_executable(test_WorksheetClipboard
        test_WorksheetClipboard.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_WorksheetClipboard PUBLIC cxx_std_20)
    target_compile_definitions(test_WorksheetClipboard PRIVATE
        WXM_CORPUS_DIR="${CMAKE_SOURCE_DIR}/test/fuzz/corpus_mathparser")
    target_link_libraries(test_WorksheetClipboard PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${WXM_TESTAPP_EXTRA_LIBS})
    add_test(NAME WorksheetClipboard
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_WorksheetClipboard>")

    # Pins the right-click context menu (WorksheetContextMenu, extracted from
    # Worksheet::OnMouseRightDown): drives the main right-click states on a
    # real Worksheet and checks the expected menu entries appear.
    add_executable(test_WorksheetContextMenu
        test_WorksheetContextMenu.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_WorksheetContextMenu PUBLIC cxx_std_20)
    target_link_libraries(test_WorksheetContextMenu PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${WXM_TESTAPP_EXTRA_LIBS})
    add_test(NAME WorksheetContextMenu
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_WorksheetContextMenu>")

    # Pins MaximaTokenizer's handling of an identifier split by an escaped
    # newline (GH #898): both halves must stay one logical name for
    # function/variable/operator/keyword classification, even though a
    # newline still has to render as its own isolated token.
    add_executable(test_MaximaTokenizer
        test_MaximaTokenizer.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_MaximaTokenizer PUBLIC cxx_std_20)
    target_link_libraries(test_MaximaTokenizer PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${WXM_TESTAPP_EXTRA_LIBS})
    add_test(NAME MaximaTokenizer
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_MaximaTokenizer>")

    # Pins the worksheet's two-cursor model (h-caret between cells XOR editor
    # cursor inside a cell; neither while cells are selected) ahead of the
    # WorksheetCursor extraction.
    add_executable(test_WorksheetCursor
        test_WorksheetCursor.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_WorksheetCursor PUBLIC cxx_std_20)
    target_link_libraries(test_WorksheetCursor PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${WXM_TESTAPP_EXTRA_LIBS})
    add_test(NAME WorksheetCursor
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_WorksheetCursor>")

    # Pins the worksheet's find/replace behavior (wrap-around group loop,
    # prompt/editor/output order, start-from-cursor skip rules) ahead of the
    # WorksheetSearch extraction.
    add_executable(test_WorksheetFind
        test_WorksheetFind.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_WorksheetFind PUBLIC cxx_std_20)
    target_link_libraries(test_WorksheetFind PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${WXM_TESTAPP_EXTRA_LIBS})
    add_test(NAME WorksheetFind
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_WorksheetFind>")

    # Pins Worksheet::TOCdnd() (the table of contents sidebar's drag-and-drop
    # reorder, GH #1524) and TableOfContents::ClampDropIndex(): a chapter
    # moves as a whole (taking its own content and nested sub-headings with
    # it), with no pre-existing worksheet selection required, and dropping
    # near the end of the list lands at the end rather than snapping back
    # near the drag's own start.
    add_executable(test_TableOfContentsDnD
        test_TableOfContentsDnD.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_TableOfContentsDnD PUBLIC cxx_std_20)
    target_link_libraries(test_TableOfContentsDnD PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${WXM_TESTAPP_EXTRA_LIBS})
    add_test(NAME TableOfContentsDnD
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_TableOfContentsDnD>")

    # Pins Worksheet::AnonymizeCodeCells() (GH #1339): non-builtin
    # variable/function names in the selected code cells get replaced with a
    # random, collision-free, reproducible-within-the-run name, while Maxima
    # builtins, the tokenizer's hardcoded keywords, non-code cells and cells
    # outside the selection are left alone, all as a single undo step.
    add_executable(test_AnonymizeCodeCells
        test_AnonymizeCodeCells.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_AnonymizeCodeCells PUBLIC cxx_std_20)
    target_link_libraries(test_AnonymizeCodeCells PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${WXM_TESTAPP_EXTRA_LIBS})
    add_test(NAME AnonymizeCodeCells
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_AnonymizeCodeCells>")

    # Pins the "add cells to the evaluation queue" behavior (which visible code
    # cells each Add*ToEvaluationQueue enqueues, and where the h-caret lands)
    # ahead of the WorksheetEvalQueue extraction.
    add_executable(test_WorksheetEvalQueue
        test_WorksheetEvalQueue.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_WorksheetEvalQueue PUBLIC cxx_std_20)
    target_link_libraries(test_WorksheetEvalQueue PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${WXM_TESTAPP_EXTRA_LIBS})
    add_test(NAME WorksheetEvalQueue
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_WorksheetEvalQueue>")

    # Command-level behavior of the EvaluationQueue: how a cell's input is split
    # into the commands sent to maxima one at a time, and how lisp/maxima mode
    # affects that. The GUI-free safety net for the planned prompt-driven queue
    # rewrite (drives GetCommand/RemoveFirst with a scriptable InLispMode).
    add_executable(test_EvalQueueCommands
        test_EvalQueueCommands.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_EvalQueueCommands PUBLIC cxx_std_20)
    target_link_libraries(test_EvalQueueCommands PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${WXM_TESTAPP_EXTRA_LIBS})
    add_test(NAME EvalQueueCommands
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_EvalQueueCommands>")

    # Hardening/fuzz test for EditorCell's text-editing core: drives the real
    # key-event paths and checks the cursor/selection invariants after every op.
    add_executable(test_EditorCellInvariants
        test_EditorCellInvariants.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_EditorCellInvariants PUBLIC cxx_std_20)
    target_link_libraries(test_EditorCellInvariants PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${WXM_TESTAPP_EXTRA_LIBS})
    add_test(NAME EditorCellInvariants
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_EditorCellInvariants>")

    # The directory-scanning half of the bash-like file-name completion:
    # openr()/load()/demo() file arguments complete against the directory the
    # partial currently points into, descending level by level.
    add_executable(test_Autocomplete
        test_Autocomplete.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_Autocomplete PUBLIC cxx_std_20)
    target_link_libraries(test_Autocomplete PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${WXM_TESTAPP_EXTRA_LIBS})
    add_test(NAME Autocomplete
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_Autocomplete>")

    # Layout idempotency invariants: after zoom / canvas-size changes, the
    # converged layout must equal a fresh layout of the same content - catches
    # any cell that stays marked "recalculated" while holding stale geometry
    # (the MatrCell-after-deadline / ParenCell-spacing bug family).
    add_executable(test_LayoutInvariants
        test_LayoutInvariants.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_LayoutInvariants PUBLIC cxx_std_20)
    target_link_libraries(test_LayoutInvariants PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${WXM_TESTAPP_EXTRA_LIBS})
    add_test(NAME LayoutInvariants
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_LayoutInvariants>")

    # An integral's "d" (dx/dtheta/...) must render upright in LaTeX export,
    # not in math mode's default italic (GH #972).
    add_executable(test_IntegralToTeX
        test_IntegralToTeX.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_IntegralToTeX PUBLIC cxx_std_20)
    target_link_libraries(test_IntegralToTeX PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${WXM_TESTAPP_EXTRA_LIBS})
    add_test(NAME IntegralToTeX
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_IntegralToTeX>")

    # Round-trip test for the mechanical scalar settings: guards that
    # ReadConfig() and the settings-write side share one key<->member table
    # (Configuration::ScalarConfigSettings) so a setting can never be written
    # and read under mismatched keys or types.
    add_executable(test_ConfigRoundtrip
        test_ConfigRoundtrip.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_ConfigRoundtrip PUBLIC cxx_std_20)
    target_link_libraries(test_ConfigRoundtrip PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${WXM_TESTAPP_EXTRA_LIBS})
    add_test(NAME ConfigRoundtrip
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_ConfigRoundtrip>")

    # Round-trip test for the worksheet styles: guards that ReadStyles() and
    # WriteStyles() share one TextStyle -> config-key mapping so styles can never
    # be written and read under mismatched keys.
    add_executable(test_StyleConfigRoundtrip
        test_StyleConfigRoundtrip.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_StyleConfigRoundtrip PUBLIC cxx_std_20)
    target_link_libraries(test_StyleConfigRoundtrip PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${WXM_TESTAPP_EXTRA_LIBS})
    add_test(NAME StyleConfigRoundtrip
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_StyleConfigRoundtrip>")

    # Direct parser test for MaximaManual's help anchors: batch mode no longer
    # compiles anchors (interactive-only), so they need their own regression net.
    add_executable(test_ManualAnchors
        test_ManualAnchors.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_ManualAnchors PUBLIC cxx_std_20)
    target_link_libraries(test_ManualAnchors PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${WXM_TESTAPP_EXTRA_LIBS})
    add_test(NAME ManualAnchors
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_ManualAnchors>")

    # Regression test for the cell-tree undo/redo (TreeUndo_*); the safety net for
    # extracting that subsystem out of the Worksheet god class.
    add_executable(test_TreeUndo
        test_TreeUndo.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_TreeUndo PUBLIC cxx_std_20)
    target_link_libraries(test_TreeUndo PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${WXM_TESTAPP_EXTRA_LIBS})
    add_test(NAME TreeUndo
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_TreeUndo>")

    # Drives a real GreekSidebar to guard its line wrapping: when too small to show
    # every letter it must grow its virtual height so the wrapped rows stay
    # reachable by the vertical scrollbar (broke on wx 3.3 -- OnSize clamped it).
    add_executable(test_GreekSidebar
        test_GreekSidebar.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_GreekSidebar PUBLIC cxx_std_20)
    target_link_libraries(test_GreekSidebar PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${WXM_TESTAPP_EXTRA_LIBS})
    add_test(NAME GreekSidebar
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_GreekSidebar>")

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
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${WXM_TESTAPP_EXTRA_LIBS})
    add_test(NAME LastActiveTextCtrl
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_LastActiveTextCtrl>")

    # Guards the declarative variable-to-menu-items table (MaximaMenuSync) that
    # keeps menu check marks in sync with the option variables Maxima reports -
    # it replaced a dozen hand-written VariableAction* handlers in wxMaxima.
    # Builds real menus from the table's own ids and asserts on the check marks.
    add_executable(test_MaximaMenuSync
        test_MaximaMenuSync.cpp groupcell_test_stubs.cpp ${WXM_TEST_SETUP}
        $<TARGET_OBJECTS:wxmTestApp>)
    target_compile_features(test_MaximaMenuSync PUBLIC cxx_std_20)
    target_link_libraries(test_MaximaMenuSync PRIVATE
        ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${WXM_TESTAPP_EXTRA_LIBS})
    add_test(NAME MaximaMenuSync
        COMMAND ${WXM_TEST_LAUNCHER} "$<TARGET_FILE:test_MaximaMenuSync>")
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
