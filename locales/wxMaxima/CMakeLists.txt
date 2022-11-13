# -*- mode: CMake; cmake-tab-width: 4; -*-

find_package(Gettext)

add_custom_target(update-locale)
set_target_properties(update-locale
    PROPERTIES OUTPUT_NAME update-locale)

# which languages are supported?
file(GLOB TRANSLATION_FILES *.po)

# which sourcecode and data files can contain translatable text?
file(GLOB POT_SOURCE_FILES_REL LIST_DIRECTORIES false RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_SOURCE_DIR}/src/*.cpp ${CMAKE_SOURCE_DIR}/src/*.h ${CMAKE_SOURCE_DIR}/examples/examples_gettext.list)
file(GLOB POT_SOURCE_FILES ${CMAKE_SOURCE_DIR}/src/*.cpp ${CMAKE_SOURCE_DIR}/src/*.h ${CMAKE_SOURCE_DIR}/examples/examples_gettext.h)

if(GETTEXT_FOUND AND GETTEXT_MSGFMT_EXECUTABLE AND GETTEXT_MSGMERGE_EXECUTABLE)
    find_program(XGETTEXT xgettext)
    # generate a wxMaxima.pot in the build dir - don't touch the existing source (for now)
    # if that is okay, one may copy it to the source dir.
    # Currently the full source paths are used in the comments.
    add_custom_command(
	OUTPUT wxMaxima.pot
	DEPENDS ${POT_SOURCE_FILES}
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	COMMAND ${XGETTEXT} -C --from-code=UTF-8 -k_ -s -o "${CMAKE_CURRENT_BINARY_DIR}/wxMaxima.pot" ${POT_SOURCE_FILES_REL}
	COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_BINARY_DIR}/wxMaxima.pot" "${CMAKE_CURRENT_SOURCE_DIR}/wxMaxima.pot"
	COMMENT "Updating the POT-file from the current sourcecode")

    foreach(POFILE ${TRANSLATION_FILES})
	string(REGEX REPLACE ".*locales/wxMaxima/(.*).po$" "\\1" LANG ${POFILE})

	# convert the po to mo files and install them
	gettext_process_po_files(${LANG} ALL PO_FILES ${POFILE})
	install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${LANG}.gmo"
	    DESTINATION "share/locale/${LANG}/LC_MESSAGES/"
	    RENAME "wxMaxima.mo")

	# 'install' message files in the directory "${CMAKE_BINARY_DIR}/share/locale/..."
	# too (at build time, not at install time), so that running ./wxmaxima-local
	# (without installing it) can find the message files and i18n works.
	file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/share/locale/${LANG}/LC_MESSAGES")
	add_custom_target(wxmaxima-local-${LANG} ALL
            COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_BINARY_DIR}/${LANG}.gmo" "${CMAKE_BINARY_DIR}/share/locale/${LANG}/LC_MESSAGES/wxMaxima.mo"
            DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${LANG}.gmo")

	add_custom_target(
	    ${LANG}_po
	    DEPENDS ${LANG}.po
	    DEPENDS wxMaxima.pot
	    )
	add_custom_command(
	    TARGET ${LANG}_po
	    DEPENDS ${LANG}.po
	    COMMENT "Creating and installing the PO- and GMO-file for language ${LANG}"
	    COMMAND ${GETTEXT_MSGMERGE_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/${LANG}.po wxMaxima.pot -U
	    COMMAND ${GETTEXT_MSGFMT_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/${LANG}.po -o "${CMAKE_CURRENT_BINARY_DIR}/${LANG}.gmo"
	    COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_BINARY_DIR}/${LANG}.gmo" "${CMAKE_BINARY_DIR}/locale/${LANG}/LC_MESSAGES/wxMaxima.mo")
	# Call msgmerge for each language and create a merged language file
	add_dependencies(update-locale ${LANG}_po)
    endforeach()

    message(STATUS "The updated POT and PO files were generated in the build directory.")
    message(STATUS "Call 'make update-locale' to update the .po files in the source tree, too.")
    file(GLOB NEW_PO_FILES "${CMAKE_BINARY_DIR}/locales/*.po")
else()
    message(STATUS "Gettext not found - translations of wxMaxima will be disabled")
endif()
