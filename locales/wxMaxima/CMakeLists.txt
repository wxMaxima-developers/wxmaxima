# -*- mode: CMake; cmake-tab-width: 4; -*-

find_package(Gettext)

add_custom_target(update-locale)
set_target_properties(update-locale
    PROPERTIES OUTPUT_NAME update-locale)

# which languages are supported?
file(GLOB TRANSLATION_FILES *.po)

# which sourcecode and data files can contain translatable text?
# Note the src/*/* patterns: translatable strings also live in the source
# subdirectories (src/cells, src/sidebars, src/wizards, src/worksheet, ...);
# a flat src/* glob silently drops them from the POT file.
file(GLOB POT_SOURCE_FILES_REL LIST_DIRECTORIES false RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_SOURCE_DIR}/src/*.cpp ${CMAKE_SOURCE_DIR}/src/*.h ${CMAKE_SOURCE_DIR}/src/*/*.cpp ${CMAKE_SOURCE_DIR}/src/*/*.h ${CMAKE_SOURCE_DIR}/examples/examples_gettext.list)
file(GLOB POT_SOURCE_FILES ${CMAKE_SOURCE_DIR}/src/*.cpp ${CMAKE_SOURCE_DIR}/src/*.h ${CMAKE_SOURCE_DIR}/src/*/*.cpp ${CMAKE_SOURCE_DIR}/src/*/*.h ${CMAKE_SOURCE_DIR}/examples/examples_gettext.h)

if(GETTEXT_FOUND AND GETTEXT_MSGFMT_EXECUTABLE AND GETTEXT_MSGMERGE_EXECUTABLE)
    find_program(XGETTEXT xgettext)
    find_program(MSGCAT msgcat)
    # generate a wxMaxima.pot in the build dir - don't touch the existing source (for now)
    # if that is okay, one may copy it to the source dir.
    # Currently the full source paths are used in the comments.
    #
    # The manual's translations live merged into these same .po files (see
    # locales/README.md), so the strings extracted from the manual
    # (../manual/wxmaxima.md.pot, kept current by update-locale-manual-in-source)
    # need to stay part of this pot too, or the next 'make update-locale' would
    # mark every manual-only msgid obsolete and drop it from every language's
    # .po file. msgcat --use-first prefers wxMaxima.pot's own (source-derived)
    # header over the manual pot's when both define the same field, so the
    # combined file's placeholder header stays in wxMaxima.pot's usual shape.
    add_custom_command(
	OUTPUT wxMaxima.pot
	DEPENDS ${POT_SOURCE_FILES} ${CMAKE_SOURCE_DIR}/locales/manual/wxmaxima.md.pot
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	COMMAND ${XGETTEXT} -C --from-code=UTF-8 -k_ -s -o "${CMAKE_CURRENT_BINARY_DIR}/wxMaxima_sourcecode.pot" ${POT_SOURCE_FILES_REL}
	COMMAND ${MSGCAT} --use-first --no-wrap "${CMAKE_CURRENT_BINARY_DIR}/wxMaxima_sourcecode.pot" "${CMAKE_SOURCE_DIR}/locales/manual/wxmaxima.md.pot" -o "${CMAKE_CURRENT_BINARY_DIR}/wxMaxima.pot"
	COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_BINARY_DIR}/wxMaxima.pot" "${CMAKE_CURRENT_SOURCE_DIR}/wxMaxima.pot"
	COMMENT "Updating the POT-file from the current sourcecode and the manual")
    # A plain file (as opposed to a target) can only be a custom command's
    # OUTPUT for one "owner" target under Xcode's build system - having all
    # ${LANG}_po targets below list wxMaxima.pot itself in their DEPENDS
    # made CMake's Xcode generator refuse to configure ("is attached to
    # multiple targets ... none of these is a common dependency of the
    # other(s)", confirmed by CI). Routing every language's dependency
    # through this one shared target instead avoids that: many targets can
    # depend on the same target, just not the same bare custom-command
    # output file.
    add_custom_target(wxMaxima_pot DEPENDS wxMaxima.pot)

    foreach(POFILE ${TRANSLATION_FILES})
	string(REGEX REPLACE ".*locales/wxMaxima/(.*).po$" "\\1" LANG ${POFILE})

	# convert the po to mo files and install them
	gettext_process_po_files(${LANG} ALL PO_FILES ${POFILE})
	install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${LANG}.gmo"
	    DESTINATION "share/locale/${LANG}/LC_MESSAGES/"
	    RENAME "wxMaxima.mo")

  if(NOT APPLE)
    # 'install' message files in the directory "${CMAKE_BINARY_DIR}/share/locale/..."
    # too (at build time, not at install time), so that running ./wxmaxima-local
    # (without installing it) can find the message files and i18n works.
    #
    # This does not work with Apple XCode - they only support a 'new' build system, and I have NO idea how to fix it.
    # ./wxmaxima-local will probably work, but maybe only with English locale.
    # if someone knows, how to solve the issue, we are happy to hear from you.
    file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/share/locale/${LANG}/LC_MESSAGES")
    add_custom_target(copy_mo_file_${LANG}_for_wxmaxima_local ALL DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${LANG}.gmo"
              COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_BINARY_DIR}/${LANG}.gmo" "${CMAKE_BINARY_DIR}/share/locale/${LANG}/LC_MESSAGES/wxMaxima.mo"
              COMMENT "Copying ${LANG}.gmo to ${CMAKE_BINARY_DIR}/share/locale/${LANG}/LC_MESSAGES/wxMaxima.mo for ./wxmaxima_local")
  endif()

	add_custom_target(
	    ${LANG}_po
	    DEPENDS ${LANG}.po wxMaxima_pot)
	add_custom_command(
	    TARGET ${LANG}_po
	    PRE_BUILD
	    COMMENT "Creating and installing the PO- and GMO-file for language ${LANG}"
	    # --previous records, on every entry msgmerge marks fuzzy, the msgid
	    # the translation was written against ("#| msgid ..."). Without it a
	    # fuzzy entry says only that something changed, not what: a trailing
	    # full stop and a complete rewording look exactly alike, so deciding
	    # whether the old translation still fits needs someone who reads the
	    # language. With it, the cosmetic changes can be told apart from the
	    # real ones without reading a word of it. The catalogues currently
	    # hold ~9400 fuzzy entries and not one of them says what it used to
	    # be; this stops that from being true of the next ones.
	    # Fold locales/manual/${LANG}.po's translations (if any - po4a keeps
	    # writing its own file there, see merge_manual_po.cmake for why) into
	    # this file BEFORE msgmerge, so the combined catalog picks up the
	    # manual's current translations too.
	    COMMAND "${CMAKE_COMMAND}"
		"-DLANG_PO=${CMAKE_CURRENT_SOURCE_DIR}/${LANG}.po"
		"-DMANUAL_PO=${CMAKE_SOURCE_DIR}/locales/manual/${LANG}.po"
		"-DMSGCAT=${MSGCAT}"
		-P "${CMAKE_CURRENT_SOURCE_DIR}/merge_manual_po.cmake"
	    COMMAND "${GETTEXT_MSGMERGE_EXECUTABLE}" "${CMAKE_CURRENT_SOURCE_DIR}/${LANG}.po" "${CMAKE_CURRENT_SOURCE_DIR}/wxMaxima.pot" -U --previous
	    COMMAND "${GETTEXT_MSGFMT_EXECUTABLE}" "${CMAKE_CURRENT_SOURCE_DIR}/${LANG}.po" -o "${CMAKE_CURRENT_BINARY_DIR}/${LANG}.gmo"
	    COMMAND "${CMAKE_COMMAND}" -E copy "${CMAKE_CURRENT_BINARY_DIR}/${LANG}.gmo" "${CMAKE_BINARY_DIR}/locale/${LANG}/LC_MESSAGES/wxMaxima.mo")
	# Call msgmerge for each language and create a merged language file
	add_dependencies(update-locale ${LANG}_po)
    endforeach()

    message(STATUS "The updated POT and PO files were generated in the build directory.")
    message(STATUS "Call 'make update-locale' to update the .po files in the source tree, too.")
    message(STATUS "Call 'make update-locale-manual-in-source' to do the same with the manual.")
else()
    message(STATUS "Gettext not found - translations of wxMaxima will be disabled")
endif()
