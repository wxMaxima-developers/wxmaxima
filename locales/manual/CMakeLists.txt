# -*- mode: CMake; cmake-tab-width: 4; -*-

# create at target which will update the manual specific locale files IN THE SOURCE TREE.
add_custom_target(update-locale-manual)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/po4a.cfg.in ${CMAKE_CURRENT_BINARY_DIR}/po4a.cfg)

find_program(PO4A-UPDATEPO po4a-updatepo)

# which languages are supported?
file(GLOB TRANSLATION_FILES *.po)

if(PO4A-UPDATEPO)
    foreach(POFILE ${TRANSLATION_FILES})
        string(REGEX REPLACE ".*/(.*).po$" "\\1" LANG ${POFILE})

        add_custom_target(manual_${LANG}_po
            COMMAND ${PO4A-UPDATEPO} -f text -m ${CMAKE_SOURCE_DIR}/info/wxmaxima.md -p "${CMAKE_CURRENT_SOURCE_DIR}/${LANG}.po" -M UTF-8
            COMMENT "Created and updated ${CMAKE_CURRENT_SOURCE_DIR}/${LANG}.po in order to sync the file with the changes in ${CMAKE_SOURCE_DIR}/info/wxmaxima.md"
        )
        add_dependencies(update-locale-manual manual_${LANG}_po)
    endforeach()
    message(STATUS "Call 'make update-locale-manual' to update the language specific .po files for the manual IN THE SOURCE TREE.")
else()
    message(WARNING "po4a-updatepo not found. Language specific .po files cannot be automatically updated with changes from the untranslated manual.")
endif()
