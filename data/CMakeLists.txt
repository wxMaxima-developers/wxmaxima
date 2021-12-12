# -*- mode: CMake; cmake-tab-width: 4; -*-

set(DATAFILES
        ${CMAKE_CURRENT_SOURCE_DIR}/io.github.wxmaxima_developers.wxMaxima.png
        ${CMAKE_CURRENT_SOURCE_DIR}/io.github.wxmaxima_developers.wxMaxima.svg)

set(PIXMAPS
        ${CMAKE_CURRENT_SOURCE_DIR}/text-x-wxmathml.svg
        ${CMAKE_CURRENT_SOURCE_DIR}/text-x-wxmaxima-batch.svg
        ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima-16.xpm
        ${CMAKE_CURRENT_SOURCE_DIR}/io.github.wxmaxima_developers.wxMaxima.png
        ${CMAKE_CURRENT_SOURCE_DIR}/io.github.wxmaxima_developers.wxMaxima.svg
        ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima-32.xpm)

if(WXM_INCLUDE_FONTS)
    add_subdirectory(fonts)
endif()

if(NOT WIN32)
    string(TIMESTAMP CURRENTDATE "%Y-%m-%d" UTC)
    # the config file is ~/.wxMaxima if wxWidgets < 3.1.1
    #                    ~/.config/wxMaxima.conf if wxWidgets >= 3.1.1 (XDG Specification)
    if(wxWidgets_VERSION_STRING VERSION_GREATER_EQUAL "3.1.1")
        set(WXMAXIMA_CONFIGFILE_PATH "~/.config/wxMaxima.conf")
    else()
        set(WXMAXIMA_CONFIGFILE_PATH "~/.wxMaxima")
    endif()
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.1.in ${CMAKE_CURRENT_BINARY_DIR}/wxmaxima.1)
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/de/wxmaxima.1.in ${CMAKE_CURRENT_BINARY_DIR}/de/wxmaxima.1)
    install(FILES ${PIXMAPS}                                                                     DESTINATION share/pixmaps)
    install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima                                           DESTINATION share/bash-completion/completions)
    install(FILES ${CMAKE_CURRENT_BINARY_DIR}/wxmaxima.1                                         DESTINATION share/man/man1)
    install(FILES ${CMAKE_CURRENT_BINARY_DIR}/de/wxmaxima.1                                      DESTINATION share/man/de/man1)
    install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/io.github.wxmaxima_developers.wxMaxima.appdata.xml DESTINATION share/metainfo)
    install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/io.github.wxmaxima_developers.wxMaxima.desktop     DESTINATION share/applications)
    install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/x-wxmathml.xml                                     DESTINATION share/mime/packages)
    install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/x-wxmaxima-batch.xml                               DESTINATION share/mime/packages)
endif()
install(FILES ${DATAFILES} DESTINATION share/wxMaxima)

# allow local execution (./wxmaxima-local) from the build directory without installation
if(UNIX)
    file(COPY ${DATAFILES} DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
endif()
