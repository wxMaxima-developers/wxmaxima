cmake_minimum_required(VERSION 3.7)
project(wxmaxima)

set(VERSION 17.04.0)
set(PACKAGE_VERSION ${VERSION})
set(PACKAGE wxMaxima)

find_package(wxWidgets REQUIRED std xml html adv aui core net base)

include(${wxWidgets_USE_FILE})

find_package(gettext)

if (GETTEXT_FOUND)
    add_subdirectory(locales)
endif()

set(SOURCE_FILES
        src/AbsCell.cpp
        src/AbsCell.h
        src/AtCell.cpp
        src/AtCell.h
        src/Autocomplete.cpp
        src/Autocomplete.h
        src/AutocompletePopup.cpp
        src/AutocompletePopup.h
        src/BC2Wiz.cpp
        src/BC2Wiz.h
        src/Bitmap.cpp
        src/Bitmap.h
        src/BTextCtrl.cpp
        src/BTextCtrl.h
        src/CellPointers.cpp
        src/CellPointers.h
        src/ConfigDialogue.cpp
        src/ConfigDialogue.h
        src/Configuration.cpp
        src/Configuration.h
        src/ConjugateCell.cpp
        src/ConjugateCell.h
        src/ContentAssistantPopup.cpp
        src/ContentAssistantPopup.h
        src/DiffCell.cpp
        src/DiffCell.h
        src/Dirstructure.cpp
        src/Dirstructure.h
        src/EditorCell.cpp
        src/EditorCell.h
        src/EvaluationQueue.cpp
        src/EvaluationQueue.h
        src/ExptCell.cpp
        src/ExptCell.h
        src/FindReplaceDialog.cpp
        src/FindReplaceDialog.h
        src/FindReplacePane.cpp
        src/FindReplacePane.h
        src/FracCell.cpp
        src/FracCell.h
        src/FunCell.cpp
        src/FunCell.h
        src/Gen1Wiz.cpp
        src/Gen1Wiz.h
        src/Gen2Wiz.cpp
        src/Gen2Wiz.h
        src/Gen3Wiz.cpp
        src/Gen3Wiz.h
        src/Gen4Wiz.cpp
        src/Gen4Wiz.h
        src/GroupCell.cpp
        src/GroupCell.h
        src/History.cpp
        src/History.h
        src/Image.cpp
        src/Image.h
        src/ImgCell.cpp
        src/ImgCell.h
        src/IntCell.cpp
        src/IntCell.h
        src/IntegrateWiz.cpp
        src/IntegrateWiz.h
        src/LimitCell.cpp
        src/LimitCell.h
        src/LimitWiz.cpp
        src/LimitWiz.h
        src/main.cpp
        src/MarkDown.cpp
        src/MarkDown.h
        src/MathCell.cpp
        src/MathCell.h
        src/MathCtrl.cpp
        src/MathCtrl.h
        src/MathParser.cpp
        src/MathParser.h
        src/MathPrintout.cpp
        src/MathPrintout.h
        src/MatrCell.cpp
        src/MatrCell.h
        src/MatWiz.cpp
        src/MatWiz.h
        src/MyTipProvider.cpp
        src/MyTipProvider.h
        src/ParenCell.cpp
        src/ParenCell.h
        src/Plot2dWiz.cpp
        src/Plot2dWiz.h
        src/Plot3dWiz.cpp
        src/Plot3dWiz.h
        src/PlotFormatWiz.cpp
        src/PlotFormatWiz.h
        src/SeriesWiz.cpp
        src/SeriesWiz.h
        src/Setup.h
        src/SlideShowCell.cpp
        src/SlideShowCell.h
        src/SqrtCell.cpp
        src/SqrtCell.h
        src/StatusBar.cpp
        src/StatusBar.h
        src/SubCell.cpp
        src/SubCell.h
        src/SubstituteWiz.cpp
        src/SubstituteWiz.h
        src/SubSupCell.cpp
        src/SubSupCell.h
        src/SumCell.cpp
        src/SumCell.h
        src/SumWiz.cpp
        src/SumWiz.h
        src/SystemWiz.cpp
        src/SystemWiz.h
        src/TableOfContents.cpp
        src/TableOfContents.h
        src/TextCell.cpp
        src/TextCell.h
        src/TextStyle.h
        src/ToolBar.cpp
        src/ToolBar.h
        src/wx_inc.h
        src/wxMaxima.cpp
        src/wxMaxima.h
        src/wxMaximaFrame.cpp
        src/wxMaximaFrame.h
        src/XmlInspector.cpp
        src/XmlInspector.h)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/data/wxmathml.lisp.in ${CMAKE_CURRENT_SOURCE_DIR}/data/wxmathml.lisp)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/src/Setup.h.cin ${CMAKE_CURRENT_SOURCE_DIR}/src/Setup.h)

add_executable(wxmaxima ${SOURCE_FILES})

if (APPLE)
    set(CMAKE_CXX_FLAGS -mmacosx-version-min=10.7)
    set(CMAKE_EXE_LINKER_FLAGS -mmacosx-version-min=10.7)
endif()

target_link_libraries(wxmaxima ${wxWidgets_CXX_FLAGS} ${wxWidgets_LIBRARIES})

add_custom_target(wxMaxima.app
        DEPENDS wxmaxima
        COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/make_app_bundle)
