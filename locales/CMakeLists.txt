find_package(Gettext)

file(GLOB TRANSLATION_FILES *.po)

if (GETTEXT_FOUND)
    gettext_create_translations(wxMaxima.pot ${TRANSLATION_FILES})
else()
    MESSAGE(STATUS "Gettext not found - translations will be disabled")
endif()


file(GLOB POT_SOURCE_FILES ${CMAKE_SOURCE_DIR}/src/*.cpp ${CMAKE_SOURCE_DIR}/src/*.h ${CMAKE_SOURCE_DIR}/data/tips.txt)

find_program(XGETTEXT xgettext)
# generate a wxMaxima.pot in the build dir - don't touch the existing source (for now)
# if that is okay, one may copy it to the source dir.
# Currently the full source paths are used in the comments.
if(NOT "${XGETTEXT}" STREQUAL "XGETTEXT-NOTFOUND")
    add_custom_command(OUTPUT wxMaxima.pot
        COMMAND ${XGETTEXT} -C -k_ -s -o "wxMaxima.pot" ${POT_SOURCE_FILES}
        DEPENDS ${POT_SOURCE_FILES})

    add_custom_target(generate-pot DEPENDS "wxMaxima.pot")
endif()