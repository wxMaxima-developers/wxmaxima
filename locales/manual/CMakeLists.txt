# -*- mode: CMake; cmake-tab-width: 4; -*-


find_program(PO4A po4a)

# which languages are supported?
file(GLOB TRANSLATION_FILES *.po)
file(GLOB LANGUAGES *.po)
list(TRANSFORM LANGUAGES REPLACE ".*/(.*).po$" "\\1")
string(REPLACE ";" " " LANGUAGES_SPACESEPARATED "${LANGUAGES}")
file(RELATIVE_PATH WXMAXIMA_MD_RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}" "${CMAKE_SOURCE_DIR}/info/wxmaxima.md")


configure_file("po4a.cfg.in" "po4a.cfg")

if(PO4A)
    # update all po files for the manual *in the source tree*.
    add_custom_target(
        update-locale-manual-in-source
        COMMAND ${PO4A} po4a.cfg
    )
else()
    message(STATUS "po4a-updatepo not found. Language specific .po files cannot be automatically updated with changes from the untranslated manual.")
endif()
