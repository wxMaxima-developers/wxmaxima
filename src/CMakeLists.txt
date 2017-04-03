find_package(wxWidgets REQUIRED std xml html adv aui core net base)

include(${wxWidgets_USE_FILE})

set(SOURCE_FILES
        AbsCell.cpp
        AbsCell.h
        AtCell.cpp
        AtCell.h
        Autocomplete.cpp
        Autocomplete.h
        AutocompletePopup.cpp
        AutocompletePopup.h
        BC2Wiz.cpp
        BC2Wiz.h
        Bitmap.cpp
        Bitmap.h
        BTextCtrl.cpp
        BTextCtrl.h
        CellPointers.cpp
        CellPointers.h
        ConfigDialogue.cpp
        ConfigDialogue.h
        Configuration.cpp
        Configuration.h
        ConjugateCell.cpp
        ConjugateCell.h
        ContentAssistantPopup.cpp
        ContentAssistantPopup.h
        DiffCell.cpp
        DiffCell.h
        Dirstructure.cpp
        Dirstructure.h
        EditorCell.cpp
        EditorCell.h
        EvaluationQueue.cpp
        EvaluationQueue.h
        ExptCell.cpp
        ExptCell.h
        FindReplaceDialog.cpp
        FindReplaceDialog.h
        FindReplacePane.cpp
        FindReplacePane.h
        FracCell.cpp
        FracCell.h
        FunCell.cpp
        FunCell.h
        Gen1Wiz.cpp
        Gen1Wiz.h
        Gen2Wiz.cpp
        Gen2Wiz.h
        Gen3Wiz.cpp
        Gen3Wiz.h
        Gen4Wiz.cpp
        Gen4Wiz.h
        GroupCell.cpp
        GroupCell.h
        History.cpp
        History.h
        Image.cpp
        Image.h
        ImgCell.cpp
        ImgCell.h
        IntCell.cpp
        IntCell.h
        IntegrateWiz.cpp
        IntegrateWiz.h
        LimitCell.cpp
        LimitCell.h
        LimitWiz.cpp
        LimitWiz.h
        main.cpp
        MarkDown.cpp
        MarkDown.h
        MathCell.cpp
        MathCell.h
        MathCtrl.cpp
        MathCtrl.h
        MathParser.cpp
        MathParser.h
        MathPrintout.cpp
        MathPrintout.h
        MatrCell.cpp
        MatrCell.h
        MatWiz.cpp
        MatWiz.h
        MyTipProvider.cpp
        MyTipProvider.h
        ParenCell.cpp
        ParenCell.h
        Plot2dWiz.cpp
        Plot2dWiz.h
        Plot3dWiz.cpp
        Plot3dWiz.h
        PlotFormatWiz.cpp
        PlotFormatWiz.h
        SeriesWiz.cpp
        SeriesWiz.h
        Setup.h
        SlideShowCell.cpp
        SlideShowCell.h
        SqrtCell.cpp
        SqrtCell.h
        StatusBar.cpp
        StatusBar.h
        SubCell.cpp
        SubCell.h
        SubstituteWiz.cpp
        SubstituteWiz.h
        SubSupCell.cpp
        SubSupCell.h
        SumCell.cpp
        SumCell.h
        SumWiz.cpp
        SumWiz.h
        SystemWiz.cpp
        SystemWiz.h
        TableOfContents.cpp
        TableOfContents.h
        TextCell.cpp
        TextCell.h
        TextStyle.h
        ToolBar.cpp
        ToolBar.h
        wxMaxima.cpp
        wxMaxima.h
        wxMaximaFrame.cpp
        wxMaximaFrame.h
        XmlInspector.cpp
        XmlInspector.h)

IF (WINDOWS)
  SET(SOURCE_FILES Resources.rc ${SOURCE_FILES})
ENDIF()

# We put Setup.h into binary dir
include_directories("${CMAKE_CURRENT_BINARY_DIR}")

IF (WIN32)
  add_executable(wxmaxima WIN32 ${SOURCE_FILES})
ELSE ()
  add_executable(wxmaxima ${SOURCE_FILES})
ENDIF()

if (APPLE OR WIN32)
    set_target_properties(wxmaxima PROPERTIES
            RUNTIME_OUTPUT_DIRECTORY_DEBUG ${CMAKE_CURRENT_BINARY_DIR}
            RUNTIME_OUTPUT_DIRECTORY_RELEASE ${CMAKE_CURRENT_BINARY_DIR})
endif()

target_link_libraries(wxmaxima ${wxWidgets_CXX_FLAGS} ${wxWidgets_LIBRARIES})

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Setup.h.cin ${CMAKE_CURRENT_BINARY_DIR}/Setup.h)

install(TARGETS wxmaxima RUNTIME DESTINATION bin)

if (APPLE)
    set(CMAKE_CXX_FLAGS -mmacosx-version-min=10.7)
    set(CMAKE_EXE_LINKER_FLAGS -mmacosx-version-min=10.7)
endif()
