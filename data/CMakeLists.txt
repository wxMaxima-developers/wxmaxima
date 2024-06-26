# -*- mode: CMake; cmake-tab-width: 4; -*-


include("${CMAKE_SOURCE_DIR}/cmake-bin2h/bin2h.cmake")

set(DATAFILES
        ${CMAKE_CURRENT_SOURCE_DIR}/io.github.wxmaxima_developers.wxMaxima.png
        ${CMAKE_CURRENT_SOURCE_DIR}/io.github.wxmaxima_developers.wxMaxima.svg)

set(RESOLUTIONS 8 16 22 24 32 36 42 44 48 64 72 96 128 150 192 256 310 512 1024)

foreach(resolution ${RESOLUTIONS})
        install(
            FILES ${CMAKE_CURRENT_SOURCE_DIR}/io.github.wxmaxima_developers.wxMaxima_${resolution}x${resolution}.png
            DESTINATION share/icons/hicolor/${resolution}x${resolution}/apps/
            RENAME io.github.wxmaxima_developers.wxMaxima.png)
endforeach()
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/io.github.wxmaxima_developers.wxMaxima.svg
        DESTINATION share/icons/hicolor/scalable/apps/)

set(PIXMAPS
        ${CMAKE_CURRENT_SOURCE_DIR}/text-x-wxmathml.svg
        ${CMAKE_CURRENT_SOURCE_DIR}/text-x-wxmaxima-batch.svg
        ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima-16.xpm
        ${CMAKE_CURRENT_SOURCE_DIR}/io.github.wxmaxima_developers.wxMaxima.png
        ${CMAKE_CURRENT_SOURCE_DIR}/io.github.wxmaxima_developers.wxMaxima.svg
        ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima-32.xpm)

if(NOT WIN32)
    # the config file is ~/.wxMaxima if wxWidgets < 3.1.1
    #                    ~/.config/wxMaxima.conf if wxWidgets >= 3.1.1 (XDG Specification)
    if(wxWidgets_VERSION_STRING VERSION_GREATER_EQUAL "3.1.1")
        set(WXMAXIMA_CONFIGFILE_PATH "~/.config/wxMaxima.conf")
    else()
        set(WXMAXIMA_CONFIGFILE_PATH "~/.wxMaxima")
    endif()
    file(TIMESTAMP ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.1.in MANPAGEDATE "%Y-%m-%d" UTC)
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.1.in wxmaxima.1)
    file(TIMESTAMP ${CMAKE_CURRENT_SOURCE_DIR}/de/wxmaxima.1.in MANPAGEDATE "%Y-%m-%d" UTC)
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/de/wxmaxima.1.in de/wxmaxima.1)
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
    file(COPY ${DATAFILES} DESTINATION .)
endif()

# Copy the sample worksheet file to the binary dir so it can be used as test case without
# being afraid that we change the test cases somehow.
file(
    COPY
    sampleWorksheet.wxmx
    DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    FILE_PERMISSIONS OWNER_WRITE OWNER_READ
)

add_custom_command(
        OUTPUT ${CMAKE_BINARY_DIR}/builtin_commands.h
        COMMAND ${CMAKE_COMMAND} ARGS
        -DBIN2H_SOURCE_FILE="${CMAKE_CURRENT_SOURCE_DIR}/builtin_commands.txt"
        -DBIN2H_VARIABLE_NAME=builtin_commands
        -DBIN2H_HEADER_FILE="${CMAKE_BINARY_DIR}/builtin_commands.h"
        -DBIN2H="${CMAKE_SOURCE_DIR}/cmake-bin2h/bin2h.cmake"
        -P "${CMAKE_SOURCE_DIR}/src/bin2h.cmake"
        COMMENT "Embedding builtin_commands.txt"
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/builtin_commands.txt ${CMAKE_SOURCE_DIR}/src/bin2h.cmake
)
add_custom_target(build_builtin_commands.h DEPENDS ${CMAKE_BINARY_DIR}/builtin_commands.h)
add_dependencies(wxmaxima build_builtin_commands.h)
