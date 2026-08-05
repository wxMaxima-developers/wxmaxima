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
    find_program(MSGMERGE msgmerge)
    find_program(MSGCAT msgcat)
    if(MSGMERGE AND MSGCAT)
        # Pull each already-manual-translated language's manual-content
        # translations back out of the combined locales/wxMaxima/<lang>.po
        # (the file Crowdin actually writes to) before po4a regenerates
        # info/wxmaxima.<lang>.md, so translations contributed through
        # Crowdin for the manual's own strings reach the manual instead of
        # sitting unused in the combined file. See
        # ../wxMaxima/split_manual_po.cmake. Scoped to LANGUAGES (languages
        # that already have a locales/manual/<lang>.po), same as the rest of
        # this file -- silently creating a fresh locales/manual/<lang>.po
        # for a language that has none yet would make po4a.cfg's language
        # list grow on the next reconfigure, and info/CMakeLists.txt expects
        # a matching info/wxmaxima.<lang>.md to already exist for every
        # entry in that list, which it wouldn't for a language nobody has
        # deliberately set up manual translation for.
        add_custom_target(pull-manual-translations-from-combined)
        foreach(LANG ${LANGUAGES})
            add_custom_command(
                TARGET pull-manual-translations-from-combined
                COMMAND "${CMAKE_COMMAND}"
                    "-DLANG_PO=${CMAKE_SOURCE_DIR}/locales/wxMaxima/${LANG}.po"
                    "-DMANUAL_POT=${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.md.pot"
                    "-DMANUAL_PO=${CMAKE_CURRENT_SOURCE_DIR}/${LANG}.po"
                    "-DMSGMERGE=${MSGMERGE}"
                    "-DMSGCAT=${MSGCAT}"
                    -P "${CMAKE_SOURCE_DIR}/locales/wxMaxima/split_manual_po.cmake")
        endforeach()
    else()
        message(STATUS "msgmerge/msgcat not found. Manual translations contributed through Crowdin's combined .po files cannot be pulled back into locales/manual/.")
    endif()

    # update all po files for the manual *in the source tree*.
    add_custom_target(
        update-locale-manual-in-source
        COMMAND ${PO4A} po4a.cfg
    )
    if(TARGET pull-manual-translations-from-combined)
        add_dependencies(update-locale-manual-in-source pull-manual-translations-from-combined)
    endif()
else()
    message(STATUS "po4a not found (or too old, see CheckPo4aVersion.cmake). Language specific .po files cannot be automatically updated with changes from the untranslated manual.")
endif()
