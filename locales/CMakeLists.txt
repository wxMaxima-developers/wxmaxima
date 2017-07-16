find_package(Gettext)

# which languages are supported?
file(GLOB TRANSLATION_FILES *.po)

# which sourcecode and data files can contain translatable text?
file(GLOB POT_SOURCE_FILES ${CMAKE_SOURCE_DIR}/src/*.cpp ${CMAKE_SOURCE_DIR}/src/*.h ${CMAKE_SOURCE_DIR}/data/tips.txt)

if(GETTEXT_FOUND)
    find_program(XGETTEXT xgettext)
    # generate a wxMaxima.pot in the build dir - don't touch the existing source (for now)
    # if that is okay, one may copy it to the source dir.
    # Currently the full source paths are used in the comments.
    message(STATUS "Create POT-file from current sourcecode and tips file")
    execute_process(
        COMMAND ${XGETTEXT} -C -k_ -s -o "wxMaxima.pot" ${POT_SOURCE_FILES})

    foreach(POFILE ${TRANSLATION_FILES})
        string(REGEX REPLACE ".*locales/(.*).po$" "\\1" LANG ${POFILE})

        # convert the po to mo files and install them
        # (FIXME: different destination directories on Windows and Unix, should probably be unified.)
        gettext_process_po_files(${LANG} ALL PO_FILES ${POFILE})
        if(WIN32)
            install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${LANG}.gmo"
                    DESTINATION "wxMaxima/locale/${LANG}/LC_MESSAGES/"
                    RENAME "wxMaxima.mo")
        else()
            install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${LANG}.gmo"
                    DESTINATION "share/locale/${LANG}/LC_MESSAGES/"
                    RENAME "wxMaxima.mo")
        endif()
        # Call msgmerge for each language and create a merged language file
        message(STATUS "Create PO-file for language ${LANG}")
        execute_process(
            COMMAND ${GETTEXT_MSGMERGE_EXECUTABLE} ${CMAKE_SOURCE_DIR}/locales/${LANG}.po ${CMAKE_BINARY_DIR}/wxMaxima.pot
            OUTPUT_FILE "${CMAKE_CURRENT_BINARY_DIR}/${LANG}.po.new"
            ERROR_QUIET)
        file(RENAME "${CMAKE_CURRENT_BINARY_DIR}/${LANG}.po.new" "${CMAKE_CURRENT_BINARY_DIR}/${LANG}.po")
    endforeach()
    message(STATUS "The new POT and PO files were generated in the build directory.")
    message(STATUS "Call 'make update-locale' to modify the wxMaxima sourcecode.")


    file(GLOB NEW_PO_FILES "${CMAKE_BINARY_DIR}/locales/*.po")
    add_custom_target(update-locale
        COMMAND ${CMAKE_COMMAND} -E copy
                "${CMAKE_BINARY_DIR}/wxMaxima.pot"
                "${CMAKE_SOURCE_DIR}/locales/wxMaxima.pot"
        COMMAND sh -c "cp *.po ${CMAKE_SOURCE_DIR}/locales/" # FIXME: Convert to a CMake copy command...
        COMMENT "Installing wxMaxima.pot and po-files in the source directory ${CMAKE_SOURCE_DIR}")
else()
    message(STATUS "Gettext not found - translations will be disabled")
endif()
