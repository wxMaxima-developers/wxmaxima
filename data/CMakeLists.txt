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
        ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima-32.xpm)

if(NOT WIN32)
    # the config file is ~/.wxMaxima if wxWidgets < 3.1.1
    #                    ~/.config/wxMaxima.conf if wxWidgets >= 3.1.1 (XDG Specification)
    if(wxWidgets_VERSION_STRING VERSION_GREATER_EQUAL "3.1.1")
        set(WXMAXIMA_CONFIGFILE_PATH "~/.config/wxMaxima.conf")
    else()
        set(WXMAXIMA_CONFIGFILE_PATH "~/.wxMaxima")
    endif()
		set(MANPAGEVERSION ${WXMAXIMA_VERSION})
    string(TIMESTAMP MANPAGEDATE "%Y-%m-%d" UTC)
    # The English man page's option list and the shell completions are
    # written from src/CommandLineOptions.h, the table wxMaxima itself parses
    # its command line with, so they cannot drift apart from it. The generator
    # is a plain C++ program without wxWidgets: wxMaxima itself starts its GUI
    # before parsing its arguments, so it can't be run during a headless build.
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.1.in wxmaxima.1.template)
    add_executable(wxmaxima-genopts ${CMAKE_SOURCE_DIR}/src/genopts.cpp)
    target_include_directories(wxmaxima-genopts PRIVATE ${CMAKE_SOURCE_DIR}/src)
    set(WXM_COMPLETIONS_DIR ${CMAKE_CURRENT_BINARY_DIR}/completions)
    set(WXM_COMMAND_LINE_DOCS
        ${CMAKE_CURRENT_BINARY_DIR}/wxmaxima.1
        ${WXM_COMPLETIONS_DIR}/wxmaxima
        ${WXM_COMPLETIONS_DIR}/_wxmaxima
        ${WXM_COMPLETIONS_DIR}/wxmaxima.fish)
    add_custom_command(
        OUTPUT ${WXM_COMMAND_LINE_DOCS}
        COMMAND ${CMAKE_COMMAND} -E make_directory ${WXM_COMPLETIONS_DIR}
        COMMAND wxmaxima-genopts man ${CMAKE_CURRENT_BINARY_DIR}/wxmaxima.1.template
                                     ${CMAKE_CURRENT_BINARY_DIR}/wxmaxima.1
        COMMAND wxmaxima-genopts bash ${WXM_COMPLETIONS_DIR}/wxmaxima
        COMMAND wxmaxima-genopts zsh ${WXM_COMPLETIONS_DIR}/_wxmaxima
        COMMAND wxmaxima-genopts fish ${WXM_COMPLETIONS_DIR}/wxmaxima.fish
        DEPENDS wxmaxima-genopts ${CMAKE_CURRENT_BINARY_DIR}/wxmaxima.1.template
        COMMENT "Writing the man page's options and the shell completions")
    add_custom_target(command_line_docs ALL DEPENDS ${WXM_COMMAND_LINE_DOCS})
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/de/wxmaxima.1.in de/wxmaxima.1)

    # Install man pages UNCOMPRESSED. Distributions compress manpages with their
    # own tooling (e.g. Debian's dh_compress); shipping pre-gzipped .1.gz files
    # collides with that and breaks the downstream maintainer's packaging.
    install(FILES ${PIXMAPS}                                                                     DESTINATION share/pixmaps)
    install(FILES ${WXM_COMPLETIONS_DIR}/wxmaxima                                                DESTINATION share/bash-completion/completions)
    install(FILES ${WXM_COMPLETIONS_DIR}/_wxmaxima                                               DESTINATION share/zsh/site-functions)
    install(FILES ${WXM_COMPLETIONS_DIR}/wxmaxima.fish                                           DESTINATION share/fish/vendor_completions.d)
    install(FILES ${CMAKE_CURRENT_BINARY_DIR}/wxmaxima.1                                         DESTINATION share/man/man1)
    install(FILES ${CMAKE_CURRENT_BINARY_DIR}/de/wxmaxima.1                                      DESTINATION share/man/de/man1)
    install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/io.github.wxmaxima_developers.wxMaxima.appdata.xml DESTINATION share/metainfo)
    install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/io.github.wxmaxima_developers.wxMaxima.desktop     DESTINATION share/applications)
    install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/x-wxmathml.xml                                     DESTINATION share/mime/packages)
    install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/x-wxmaxima-batch.xml                               DESTINATION share/mime/packages)

    # create symlinks for the duplicate icons
    install(CODE "
        macro(create_install_symlink TARGET_PATH LINK_PATH)
            get_filename_component(link_dir \"\${LINK_PATH}\" DIRECTORY)
            set(dest_dir \"\$ENV{DESTDIR}\${CMAKE_INSTALL_PREFIX}/\${link_dir}\")
            set(dest_link \"\$ENV{DESTDIR}\${CMAKE_INSTALL_PREFIX}/\${LINK_PATH}\")
            execute_process(COMMAND \${CMAKE_COMMAND} -E make_directory \${dest_dir})
            execute_process(COMMAND chmod 755 \${dest_dir})
            message(STATUS \"Creating symlink: \${dest_link} -> \${TARGET_PATH}\")
            execute_process(COMMAND \${CMAKE_COMMAND} -E create_symlink \${TARGET_PATH} \${dest_link})
        endmacro()
        
        create_install_symlink(../icons/hicolor/scalable/apps/io.github.wxmaxima_developers.wxMaxima.svg share/pixmaps/io.github.wxmaxima_developers.wxMaxima.svg)
        create_install_symlink(../icons/hicolor/1024x1024/apps/io.github.wxmaxima_developers.wxMaxima.png share/pixmaps/io.github.wxmaxima_developers.wxMaxima.png)
        create_install_symlink(../icons/hicolor/scalable/apps/io.github.wxmaxima_developers.wxMaxima.svg share/wxMaxima/io.github.wxmaxima_developers.wxMaxima.svg)
        create_install_symlink(../icons/hicolor/1024x1024/apps/io.github.wxmaxima_developers.wxMaxima.png share/wxMaxima/io.github.wxmaxima_developers.wxMaxima.png)
        create_install_symlink(wxmaxima.1 share/man/man1/wxmxdiff.1)
        create_install_symlink(wxmaxima.1 share/man/de/man1/wxmxdiff.1)
        # bash and fish load a completion by the command's name; zsh's
        # \"#compdef wxmaxima wxmxdiff\" line covers both already.
        create_install_symlink(wxmaxima share/bash-completion/completions/wxmxdiff)
        create_install_symlink(wxmaxima.fish share/fish/vendor_completions.d/wxmxdiff.fish)
    ")
endif()

if(WIN32)
    install(FILES ${DATAFILES} DESTINATION share/wxMaxima)
endif()

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
