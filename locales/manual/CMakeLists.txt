# -*- mode: CMake; cmake-tab-width: 4; -*-


include(${CMAKE_SOURCE_DIR}/CheckPo4aVersion.cmake)

# which languages are supported? info/CMakeLists.txt expects a translated
# info/wxmaxima.<lang>.md to already exist for every language in this list
# (it doesn't generate a fresh empty one), so this must stay scoped to the
# languages that actually have a manual translation - not every language
# locales/wxMaxima/*.po covers (most of which have no manual translation at
# all yet, only UI strings). po4a keeps writing its own po file here (see
# ../wxMaxima/merge_manual_po.cmake for why it can't write directly into the
# combined locales/wxMaxima/*.po).
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
    message(STATUS "po4a not found (or too old, see CheckPo4aVersion.cmake). Language specific .po files cannot be automatically updated with changes from the untranslated manual.")
endif()
