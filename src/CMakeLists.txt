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

if(WIN32)
    include_directories(${CMAKE_SOURCE_DIR}/data/winrc)
    set(SOURCE_FILES Resources.rc ${SOURCE_FILES})
endif()

# We put Setup.h into binary dir
include_directories("${CMAKE_CURRENT_BINARY_DIR}")

if(WIN32)
    add_executable(wxmaxima WIN32 ${SOURCE_FILES})
    set_target_properties(wxmaxima PROPERTIES
              RUNTIME_OUTPUT_DIRECTORY_DEBUG   ${CMAKE_BINARY_DIR}/wxMaxima
              RUNTIME_OUTPUT_DIRECTORY_RELEASE ${CMAKE_BINARY_DIR}/wxMaxima)
    add_dependencies(wxmaxima wxMaxima.win)
elseif(APPLE)
    add_executable(wxmaxima ${SOURCE_FILES})
    set_target_properties(wxmaxima PROPERTIES
              RUNTIME_OUTPUT_DIRECTORY_DEBUG   ${CMAKE_BINARY_DIR}/wxMaxima.app/Contents/MacOS
              RUNTIME_OUTPUT_DIRECTORY_RELEASE ${CMAKE_BINARY_DIR}/wxMaxima.app/Contents/MacOS)
    add_dependencies(wxmaxima wxMaxima.app)
else()
    add_executable(wxmaxima ${SOURCE_FILES})
endif()

target_link_libraries(wxmaxima ${wxWidgets_CXX_FLAGS} ${wxWidgets_LIBRARIES})

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Setup.h.cin ${CMAKE_CURRENT_BINARY_DIR}/Setup.h)

install(TARGETS wxmaxima RUNTIME DESTINATION bin)

# OSX and Windows binary folders
if(APPLE OR WIN32)
    # DEFINE FILES
    file(GLOB RESOURCES_FILES
            ${CMAKE_BINARY_DIR}/data/wxmathml.lisp
            ${CMAKE_SOURCE_DIR}/test/testbench_simple.wxmx
            ${CMAKE_SOURCE_DIR}/data/wxmaxima.png
            ${CMAKE_SOURCE_DIR}/data/wxmaxima.svg
            ${CMAKE_SOURCE_DIR}/data/autocomplete.txt
            ${CMAKE_SOURCE_DIR}/data/tips*.txt)
    file(GLOB RESOURCES_FILES_OSX
            ${CMAKE_SOURCE_DIR}/art/wxmac.icns
            ${CMAKE_SOURCE_DIR}/art/wxmac-doc.icns
            ${CMAKE_SOURCE_DIR}/art/wxmac-doc-wxm.icns
            ${CMAKE_SOURCE_DIR}/art/wxmac-doc-wxmx.icns)
    file(GLOB HELP_FILES
            ${CMAKE_SOURCE_DIR}/info/*.html
            ${CMAKE_SOURCE_DIR}/info/*.jpg
            ${CMAKE_SOURCE_DIR}/info/*.png
            ${CMAKE_SOURCE_DIR}/info/wxmaxima.hhp)
    file(GLOB CONTENT_FILES
            ${CMAKE_BINARY_DIR}/data/Info.plist
            ${CMAKE_SOURCE_DIR}/data/PkgInfo)
    file(GLOB TOOLBAR_FILES
            ${CMAKE_SOURCE_DIR}/art/toolbar/*.png)
    file(GLOB STATUSBAR_FILES
            ${CMAKE_SOURCE_DIR}/art/statusbar/*.png)
    file(GLOB CONFIG_FILES
            ${CMAKE_SOURCE_DIR}/art/config/*.png)
    file(GLOB FONT_FILES
            ${CMAKE_SOURCE_DIR}/data/fonts/*.ttf)

    if(APPLE)
        # DEFINE DIRECTORIES
        set(APP_DIR ${CMAKE_BINARY_DIR}/wxMaxima.app/Contents)
        set(APP_MACOSX_DIR ${APP_DIR}/MacOS)
        set(APP_RESOURCES_DIR ${APP_DIR}/Resources)
        set(APP_HELP_DIR ${APP_RESOURCES_DIR}/help)
        set(APP_FONTS_DIR ${APP_RESOURCES_DIR}/fonts)
        # CUSTOM TARGET
        add_custom_target(wxMaxima.app)
        add_custom_command(TARGET wxMaxima.app PRE_BUILD
                COMMAND ${CMAKE_COMMAND} -E make_directory ${APP_MACOSX_DIR})
        add_custom_command(TARGET wxMaxima.app PRE_BUILD
                COMMAND ${CMAKE_COMMAND} -E make_directory ${APP_HELP_DIR})
        add_custom_command(TARGET wxMaxima.app PRE_BUILD
                COMMAND ${CMAKE_COMMAND} -E make_directory ${APP_RESOURCES_DIR}/toolbar)
        add_custom_command(TARGET wxMaxima.app PRE_BUILD
                COMMAND ${CMAKE_COMMAND} -E make_directory ${APP_RESOURCES_DIR}/statusbar)
        add_custom_command(TARGET wxMaxima.app PRE_BUILD
                COMMAND ${CMAKE_COMMAND} -E make_directory ${APP_RESOURCES_DIR}/config)
        add_custom_command(TARGET wxMaxima.app
                COMMAND ${CMAKE_COMMAND} -E make_directory ${APP_FONTS_DIR})
        foreach(f ${RESOURCE_FILES})
            add_custom_command(TARGET wxMaxima.app
                COMMAND ${CMAKE_COMMAND} -E copy ${f}  ${APP_RESOURCES_DIR})
        endforeach()
        foreach(f ${RESOURCES_FILES_OSX})
            add_custom_command(TARGET wxMaxima.app
                COMMAND ${CMAKE_COMMAND} -E copy ${f}  ${APP_RESOURCES_DIR})
        endforeach()
        foreach(f ${CONTENT_FILES})
            add_custom_command(TARGET wxMaxima.app
                    COMMAND ${CMAKE_COMMAND} -E copy ${f}  ${APP_DIR})
        endforeach()
        foreach(f ${STATUSBAR_FILES})
            add_custom_command(TARGET wxMaxima.app
                    COMMAND ${CMAKE_COMMAND} -E copy ${f}  ${APP_RESOURCES_DIR}/statusbar)
        endforeach()
        foreach(f ${TOOLBAR_FILES})
            add_custom_command(TARGET wxMaxima.app
                    COMMAND ${CMAKE_COMMAND} -E copy ${f}  ${APP_RESOURCES_DIR}/toolbar)
        endforeach()
        foreach(f ${CONFIG_FILES})
            add_custom_command(TARGET wxMaxima.app
                    COMMAND ${CMAKE_COMMAND} -E copy ${f}  ${APP_RESOURCES_DIR}/config)
        endforeach()
        foreach(f ${RESOURCES_FILES})
            add_custom_command(TARGET wxMaxima.app
                    COMMAND ${CMAKE_COMMAND} -E copy ${f}  ${APP_RESOURCES_DIR})
        endforeach()
        foreach(f ${HELP_FILES})
            add_custom_command(TARGET wxMaxima.app
                    COMMAND ${CMAKE_COMMAND} -E copy ${f}  ${APP_HELP_DIR})
        endforeach()
        foreach(f ${FONT_FILES})
            add_custom_command(TARGET wxMaxima.app
                    COMMAND ${CMAKE_COMMAND} -E copy ${f} ${APP_FONTS_DIR})
        endforeach()
    endif()

    if(WIN32)
        # DEFINE DIRECTORIES
        set(APP_DIR ${CMAKE_BINARY_DIR}/wxMaxima)
        set(APP_DATA_DIR ${APP_DIR}/data)
        set(APP_HELP_DIR ${APP_DIR}/help)
        set(APP_ART_DIR ${APP_DIR}/art)
        # CUSTOM TARGET
        add_custom_target(wxMaxima.win)
        add_custom_command(TARGET wxMaxima.win PRE_BUILD
                COMMAND ${CMAKE_COMMAND} -E make_directory ${APP_DIR})
        add_custom_command(TARGET wxMaxima.win PRE_BUILD
                COMMAND ${CMAKE_COMMAND} -E make_directory ${APP_DATA_DIR})
        add_custom_command(TARGET wxMaxima.win PRE_BUILD
                COMMAND ${CMAKE_COMMAND} -E make_directory ${APP_HELP_DIR})
        add_custom_command(TARGET wxMaxima.win PRE_BUILD
                COMMAND ${CMAKE_COMMAND} -E make_directory ${APP_ART_DIR}/toolbar)
        add_custom_command(TARGET wxMaxima.win PRE_BUILD
                COMMAND ${CMAKE_COMMAND} -E make_directory ${APP_ART_DIR}/statusbar)
        add_custom_command(TARGET wxMaxima.win PRE_BUILD
                COMMAND ${CMAKE_COMMAND} -E make_directory ${APP_ART_DIR}/config)
        foreach(f ${RESOURCE_FILES})
            add_custom_command(TARGET wxMaxima.win
                COMMAND ${CMAKE_COMMAND} -E copy ${f}  ${APP_DATA_DIR})
        endforeach()
        foreach(f ${STATUSBAR_FILES})
            add_custom_command(TARGET wxMaxima.win
                    COMMAND ${CMAKE_COMMAND} -E copy ${f}  ${APP_ART_DIR}/statusbar)
        endforeach()
        foreach(f ${TOOLBAR_FILES})
            add_custom_command(TARGET wxMaxima.win
                    COMMAND ${CMAKE_COMMAND} -E copy ${f}  ${APP_ART_DIR}/toolbar)
        endforeach()
        foreach(f ${CONFIG_FILES})
            add_custom_command(TARGET wxMaxima.win
                    COMMAND ${CMAKE_COMMAND} -E copy ${f}  ${APP_ART_DIR}/config)
        endforeach()
        foreach(f ${RESOURCES_FILES})
            add_custom_command(TARGET wxMaxima.win
                    COMMAND ${CMAKE_COMMAND} -E copy ${f}  ${APP_DATA_DIR})
        endforeach()
        foreach(f ${HELP_FILES})
            add_custom_command(TARGET wxMaxima.win
                    COMMAND ${CMAKE_COMMAND} -E copy ${f}  ${APP_HELP_DIR})
        endforeach()
    endif()
endif()
