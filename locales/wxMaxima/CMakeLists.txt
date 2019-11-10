# -*- mode: CMake; cmake-tab-width: 4; -*-

find_package(Gettext)

add_custom_target(update-locale)
set_target_properties(update-locale
    PROPERTIES OUTPUT_NAME update-locale)

# which languages are supported?
file(GLOB TRANSLATION_FILES *.po)

# which sourcecode and data files can contain translatable text?
file(GLOB POT_SOURCE_FILES ${CMAKE_SOURCE_DIR}/src/*.cpp ${CMAKE_SOURCE_DIR}/src/*.h)

if(GETTEXT_FOUND)
    find_program(XGETTEXT xgettext)
    # generate a wxMaxima.pot in the build dir - don't touch the existing source (for now)
    # if that is okay, one may copy it to the source dir.
    # Currently the full source paths are used in the comments.
    message(STATUS "Create POT-file from current sourcecode")
    add_custom_target(build-pot ALL
	COMMAND ${XGETTEXT} -C -k_ -s -o "wxMaxima.pot" ${POT_SOURCE_FILES})

    add_custom_command(
	TARGET update-locale
	COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_BINARY_DIR}/wxMaxima.pot" "${CMAKE_SOURCE_DIR}/locales/wxMaxima/wxMaxima.pot"
	COMMAND ${CMAKE_COMMAND} -E copy "*.po" "${CMAKE_SOURCE_DIR}/locales/wxMaxima/"
	COMMENT "Installing wxMaxima.pot and po-files in the source directory ${CMAKE_SOURCE_DIR}/locales/wxMaxima")
    
    foreach(POFILE ${TRANSLATION_FILES})
	string(REGEX REPLACE ".*locales/wxMaxima/(.*).po$" "\\1" LANG ${POFILE})

	# convert the po to mo files and install them
	gettext_process_po_files(${LANG} ALL PO_FILES ${POFILE})
	install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${LANG}.gmo"
	    DESTINATION "share/locale/${LANG}/LC_MESSAGES/"
	    RENAME "wxMaxima.mo")

	# 'install' message files in the directory "${CMAKE_BINARY_DIR}/locale/..."
	# too (at build time, not at install time), so that running ./wxmaxima-local
	# (without installing it) can find the message files and i18n works.
	file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/locale/${LANG}")
	add_custom_target(build-locale-${LANG} ALL
	    DEPENDS ${POT_SOURCE_FILES}
	    DEPENDS build-pot
	    COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_BINARY_DIR}/${LANG}.gmo" "${CMAKE_BINARY_DIR}/locale/${LANG}/LC_MESSAGES/wxMaxima.mo")
	# Call msgmerge for each language and create a merged language file
	message(STATUS "Create PO-file for language ${LANG}")
	add_custom_command(
	    TARGET update-locale
	    COMMAND ${CMAKE_COMMAND} -E copy "*.po" "${CMAKE_BINARY_DIR}/locales/wxMaxima/"
	    COMMAND ${GETTEXT_MSGMERGE_EXECUTABLE} -N ${CMAKE_SOURCE_DIR}/locales/wxMaxima/${LANG}.po ${CMAKE_BINARY_DIR}/wxMaxima.pot -U)
	add_dependencies(update-locale build-locale-${LANG})
    endforeach()

    message(STATUS "The updated POT and PO files were generated in the build directory.")
    message(STATUS "Call 'make update-locale' to update the .po files in the source tree, too.")
    file(GLOB NEW_PO_FILES "${CMAKE_BINARY_DIR}/locales/*.po")
else()
    message(STATUS "Gettext not found - translations of wxMaxima will be disabled")
endif()
