# -*- mode: CMake; cmake-tab-width: 4; -*-

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/po4a.cfg.in ${CMAKE_CURRENT_BINARY_DIR}/po4a.cfg)

find_program(PO4A-UPDATEPO po4a-updatepo)

# which languages are supported?
file(GLOB TRANSLATION_FILES *.po)

if(PO4A-UPDATEPO)
    add_custom_target(update-locale-manual-in-source) # update all po files for the manual *in the source tree*.
    foreach(POFILE ${TRANSLATION_FILES})
        string(REGEX REPLACE ".*/(.*).po$" "\\1" LANG ${POFILE})
        file(RELATIVE_PATH WXMAXIMA_MD_RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}" "${CMAKE_SOURCE_DIR}/info/wxmaxima.md")
        add_custom_target(wxmaxima-manual-locale-${LANG} ALL DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${LANG}.po")
        add_custom_command(
            DEPENDS ${CMAKE_SOURCE_DIR}/info/wxmaxima.md
            OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${LANG}.po
            COMMAND ${PO4A-UPDATEPO} -f text -o markdown -m "${WXMAXIMA_MD_RELATIVE}" -p "${CMAKE_CURRENT_BINARY_DIR}/${LANG}.po" -M UTF-8
            COMMAND touch "${CMAKE_CURRENT_BINARY_DIR}/${LANG}.po"
            COMMENT "Created and updated ${LANG}.po in order to sync the file with the changes in ${CMAKE_SOURCE_DIR}/info/wxmaxima.md"
        )

        # modifies the po file *in the source tree*.
        add_custom_target(manual_${LANG}_po
            COMMAND ${PO4A-UPDATEPO} -f text -o markdown -m "${WXMAXIMA_MD_RELATIVE}" -p "${CMAKE_CURRENT_SOURCE_DIR}/${LANG}.po" -M UTF-8
            COMMENT "Created and updated ${CMAKE_CURRENT_SOURCE_DIR}/${LANG}.po in order to sync the file with the changes in ${CMAKE_SOURCE_DIR}/info/wxmaxima.md")
        add_dependencies(update-locale-manual-in-source manual_${LANG}_po)
    endforeach()
    message(STATUS "Call 'make update-locale-manual-in-source' to update the language specific .po files for the manual IN THE SOURCE TREE.")
else()
    message(${NONCRIT_WARNING} "po4a-updatepo not found. Language specific .po files cannot be automatically updated with changes from the untranslated manual.")
endif()
