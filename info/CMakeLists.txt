# -*- mode: CMake; cmake-tab-width: 4; -*-

include(${CMAKE_SOURCE_DIR}/CheckPo4aVersion.cmake)
if(PO4A)
	# Translate the manual for this build. This never writes to the source
	# tree: the translated manuals go into this build directory, and
	# --no-update leaves the .po/.pot files alone.
	#
	# It used to write info/wxmaxima.<lang>.md in the source tree (and, with
	# an option, the .po files too), on every configure. Those files are
	# tracked, so every developer whose po4a version or manual differed from
	# the last commit got a dirty tree after a plain build, and then a merge
	# conflict on the next pull. Refreshing the committed copies is now the
	# job of the update-locale-manual-in-source target, which the weekly
	# update_translations workflow runs (or a developer, by hand).
	file(GLOB MANUAL_PO_FILES "${CMAKE_SOURCE_DIR}/locales/manual/*.po")
	set(MANUAL_LANGUAGES ${MANUAL_PO_FILES})
	list(TRANSFORM MANUAL_LANGUAGES REPLACE ".*/(.*).po$" "\\1")
	string(REPLACE ";" " " MANUAL_LANGUAGES_SPACESEPARATED "${MANUAL_LANGUAGES}")
	configure_file(po4a-build.cfg.in po4a-build.cfg)
	# A translation below po4a's --keep threshold gets no file, so don't let
	# one from an earlier configure linger and pose as current.
	foreach(MANUAL_LANGUAGE ${MANUAL_LANGUAGES})
		file(REMOVE "${CMAKE_CURRENT_BINARY_DIR}/wxmaxima.${MANUAL_LANGUAGE}.md")
	endforeach()
	# Re-run this (and with it po4a) when the manual or a translation changes.
	set_property(DIRECTORY APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS
		"${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.md" ${MANUAL_PO_FILES})
	execute_process(
		COMMAND ${PO4A} --verbose --no-update ${CMAKE_CURRENT_BINARY_DIR}/po4a-build.cfg
		WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
else()
	message(STATUS "po4a not found (or too old, see CheckPo4aVersion.cmake). Disable the generation of localized documentation.")
endif()

find_program(PANDOC pandoc)
if(PANDOC)
	execute_process(
		COMMAND ${PANDOC} --version
		OUTPUT_VARIABLE PANDOC_VERSION_TEXT)
	string(REGEX REPLACE "^pandoc(.exe)? ([0-9.]*).*" "\\2" PANDOC_VERSION ${PANDOC_VERSION_TEXT})
	message(STATUS "Found pandoc: ${PANDOC} (found version \"${PANDOC_VERSION}\")")

	# "make pdf" creates the manual in PDF form. The other targets that now
	# follow do the same for other formats
	add_custom_target(pdf)
	add_custom_target(epub)
	add_custom_target(html ALL)
	function(generate_wxmaxima_documentation_html FILEBASENAME)
		add_custom_command(
			OUTPUT ${FILEBASENAME}.html
			COMMAND ${PANDOC} ${FILEBASENAME}.md -t html5 "--include-in-header=${CMAKE_CURRENT_SOURCE_DIR}/header.html" -o ${FILEBASENAME}.html --number-sections --table-of-contents --standalone --css=wxmaxima.css --metadata title="WxMaxima"
			COMMAND ${REMOVE_HTML5SHIV}
			COMMENT "Generating ${FILEBASENAME}.html")
		add_custom_target(build_${FILEBASENAME}.html DEPENDS ${FILEBASENAME}.html)
		add_dependencies(html build_${FILEBASENAME}.html)
		install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${FILEBASENAME}.html" DESTINATION share/doc/wxmaxima OPTIONAL)
		# create a symlink from wxmaxima.html to wxmaxima-en.html for consistency (Suggested in issue #2275).
		install(CODE "execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink wxmaxima.html  \"\$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/share/doc/wxmaxima/wxmaxima-en.html\")" OPTIONAL)

	endfunction()

	function(generate_wxmaxima_documentation_pdf FILEBASENAME)
		set(PDFLANG "${LANG}")
		# if LANG is empty, we are generating the default (english) manual.
		if("${PDFLANG}" STREQUAL "")
			set(PDFLANG "en")
		endif()
		# For Chinese use "zh_Hans", which is "Simplified Chinese". At least keywords like "Figure" or
		# "Contents" have a translation now (hopefully a correct one).
		# "zh_Hant" (Traditional Chinese) might also be possible, don't know which is 'correct'.
		if("${PDFLANG}" STREQUAL "zh_CN")
			set(PDFLANG "zh_Hans")
		endif()

		# GH #2271: -V CJKmainfont used to be passed unconditionally, for every
		# language's PDF, not just the CJK ones. That makes pandoc emit
		# \usepackage{xeCJK} into every manual's LaTeX regardless of whether it
		# is actually CJK content, so composing ANY language's PDF -- Hungarian
		# included, despite it having no CJK text at all -- hard-depends on
		# texlive's (large, easy to not have installed) CJK support, and fails
		# outright with "File `xeCJK.sty' not found" if that one specific
		# package is missing, even though the rest of the LaTeX toolchain is
		# perfectly capable of composing the document. Only the languages that
		# actually need a CJK font should pay for requiring one.
		set(CJK_ARGS "")
		if(PDFLANG MATCHES "^zh")
			set(CJK_ARGS -V 'CJKmainfont:NotoSerifCJK-Regular.ttc')
		endif()

		# GH #2271, second half: "-t latex-smart", not "-t latex". With smart
		# punctuation on, pandoc writes quotation marks as TeX ligatures
		# (``...''). Hungarian's babel module makes ` an active character, and
		# pandoc >= 3.2 loads babel with shorthands=off, which leaves that
		# character undefined -- so every quote broke the Hungarian PDF
		# ("Undefined control sequence"), while older pandoc, which loads the
		# language via \babelprovide instead, composed it fine. Turning the
		# extension off makes pandoc write the Unicode quotation marks
		# themselves, which xelatex/lualatex and the Noto fonts handle
		# directly. test/check-manual-backticks.cmake guards the other way a
		# literal ` reaches the LaTeX: an unpaired backtick in the manual.
		add_custom_command(
			OUTPUT ${FILEBASENAME}.pdf
			COMMAND ${PANDOC} ${FILEBASENAME}.md -t latex-smart -o ${FILEBASENAME}.pdf -M lang=${PDFLANG} -V 'mainfont:NotoSerif-Regular.ttf' -V 'sansfont:NotoSans-Regular.ttf' -V 'monofont:NotoSansMono-Regular.ttf' -V 'mathfont:NotoSansMath-Regular.ttf' ${CJK_ARGS} --number-sections --table-of-contents --standalone --metadata title="wxMaxima" --pdf-engine=${WXMAXIMA_LATEX_COMMAND}
			COMMENT "Generating ${FILEBASENAME}.pdf")
		add_custom_target(build_${FILEBASENAME}.pdf DEPENDS ${FILEBASENAME}.pdf)
		add_dependencies(pdf build_${FILEBASENAME}.pdf)
		install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${FILEBASENAME}.pdf" DESTINATION share/doc/wxmaxima OPTIONAL)
		# create a symlink from wxmaxima.pdf to wxmaxima-en.pdf for consistency (Suggested in issue #2275).
		install(CODE "execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink wxmaxima.pdf  \"\$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/share/doc/wxmaxima/wxmaxima-en.pdf\")" OPTIONAL)
	endfunction()

	function(generate_wxmaxima_documentation_epub FILEBASENAME)
		add_custom_command(
			OUTPUT ${FILEBASENAME}.epub
			COMMAND ${PANDOC} ${FILEBASENAME}.md -t epub -o ${FILEBASENAME}.epub --number-sections --table-of-contents --standalone --css=wxmaxima.css --metadata title="wxMaxima"
			COMMENT "Generating ${FILEBASENAME}.epub")
		add_custom_target(build_${FILEBASENAME}.epub DEPENDS ${FILEBASENAME}.epub)
		add_dependencies(epub build_${FILEBASENAME}.epub)
		install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${FILEBASENAME}.epub" DESTINATION share/doc/wxmaxima OPTIONAL)
		# create a symlink from wxmaxima.epub to wxmaxima-en.epub for consistency (Suggested in issue #2275).
		install(CODE "execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink wxmaxima.epub  \"\$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/share/doc/wxmaxima/wxmaxima-en.epub\")" OPTIONAL)
	endfunction()

	# Create a list of the common part of the name of all the translated manuals.
	# The "list(TRANSFORM" type of commands only is available in very new cmake
	# versions and therefore (in 2019) of only limited use so we'll have to do
	# this by hand.
	set(BASENAMES "")
	file(GLOB POFILES ${CMAKE_SOURCE_DIR}/locales/manual/*.po)
	foreach(POFILE ${POFILES})
		string(REGEX REPLACE ".*/(.*).po$" "wxmaxima.\\1" BASENAME ${POFILE})
		list(APPEND BASENAMES ${BASENAME})
	endforeach()

	# Find a suitable LaTeX installation
	find_package(LATEX COMPONENTS XELATEX LUALATEX)
        if(LATEX_XELATEX_FOUND)
            set(WXMAXIMA_LATEX_COMMAND "${XELATEX_COMPILER}")
        elseif(LATEX_LUALATEX_FOUND)
            set(WXMAXIMA_LATEX_COMMAND "${LUALATEX_COMPILER}")
        else()
            message(STATUS "Xelatex or Lualatex not found. PDF documentation can not be converted from Markdown.")
	endif()

	file(GLOB IMAGEFILES ${CMAKE_CURRENT_SOURCE_DIR}/*.png)
	file(COPY ${IMAGEFILES} DESTINATION .)
        configure_file(wxmaxima.md wxmaxima.md COPYONLY)
	foreach(BASENAME ${BASENAMES})
		if(NOT EXISTS "${CMAKE_CURRENT_BINARY_DIR}/${BASENAME}.md")
			# po4a (run above, into this directory) only writes a
			# translated ${BASENAME}.md once that language crosses its
			# --keep threshold (po4a-build.cfg.in sets 5%); a just-seeded
			# language with no translated manual strings yet has none.
			# Fall back to the English original so configure doesn't
			# hard-fail until someone crosses that threshold.
			configure_file(wxmaxima.md ${BASENAME}.md COPYONLY)
		endif()
	endforeach()
	generate_wxmaxima_documentation_html("wxmaxima")
	generate_wxmaxima_documentation_epub("wxmaxima")
	if(WXMAXIMA_LATEX_COMMAND)
		generate_wxmaxima_documentation_pdf("wxmaxima")
    endif()

	if(PO4A)
		foreach(BASENAME ${BASENAMES})
			string(REGEX REPLACE "wxmaxima.(.*)$" "\\1" LANG ${BASENAME})
			string(REGEX REPLACE "wxmaxima$" "" LANG ${LANG})
			generate_wxmaxima_documentation_html("${BASENAME}")
			generate_wxmaxima_documentation_epub("${BASENAME}")
			if(WXMAXIMA_LATEX_COMMAND)
				generate_wxmaxima_documentation_pdf("${BASENAME}")
			endif()
		endforeach()
	else()
		foreach(BASENAME ${BASENAMES})
			install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/${BASENAME}.html DESTINATION share/doc/wxmaxima OPTIONAL)
		endforeach()
	endif()
else()
    message(STATUS "Pandoc not found. Documentation can not be converted from Markdown. Included HTML documentation will be installed.")
	file(GLOB WXMAXIMA_HTML_HELP "${CMAKE_CURRENT_SOURCE_DIR}/*.html")
	install(FILES ${WXMAXIMA_HTML_HELP} DESTINATION share/doc/wxmaxima)
	file(COPY ${WXMAXIMA_HTML_HELP} DESTINATION .)
endif()

file(GLOB HTMLHELP_ADDITIONAL_FILES *.png *.svg)
if(NOT WIN32)
    list(REMOVE_ITEM HTMLHELP_ADDITIONAL_FILES "${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.svg")
endif()
install(FILES ${HTMLHELP_ADDITIONAL_FILES} DESTINATION share/doc/wxmaxima)
file(COPY ${HTMLHELP_ADDITIONAL_FILES} DESTINATION .)

if(NOT WIN32)
    install(CODE "
        macro(create_install_symlink TARGET_PATH LINK_PATH)
            get_filename_component(link_dir \"\${LINK_PATH}\" DIRECTORY)
            set(dest_dir \"\$ENV{DESTDIR}\${CMAKE_INSTALL_PREFIX}/\${link_dir}\")
            set(dest_link \"\$ENV{DESTDIR}\${CMAKE_INSTALL_PREFIX}/\${LINK_PATH}\")
            execute_process(COMMAND \${CMAKE_COMMAND} -E make_directory \${dest_dir})
            execute_process(COMMAND chmod 755 \${dest_dir})
            message(STATUS \"Creating symlink: \${dest_link} -> \${TARGET_PATH}\")
            execute_process(COMMAND \${CMAKE_COMMAND} -E create_symlink \${TARGET_PATH} \${dest_link})
        endmacro()
        
        create_install_symlink(../../icons/hicolor/scalable/apps/io.github.wxmaxima_developers.wxMaxima.svg share/doc/wxmaxima/wxmaxima.svg)
    ")
endif()


find_package(ImageMagick COMPONENTS identify)
file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/wxmaxima.css" "/* wxMaxima CSS generated by CMake */\n\n")
if(ImageMagick_identify_FOUND)
  file(GLOB HTMLHELP_PNG_FILES *.png)
  foreach(i ${HTMLHELP_PNG_FILES})
    get_filename_component(comp ${i} NAME_WE)
    execute_process(COMMAND ${ImageMagick_identify_EXECUTABLE} -format "#img_${comp} {width: %wpx; height: %hpx;}" ${i}
                    OUTPUT_VARIABLE css)
                    file(APPEND "${CMAKE_CURRENT_BINARY_DIR}/wxmaxima.css" "${css}\n")
  endforeach()
endif()

install(FILES "${CMAKE_CURRENT_BINARY_DIR}/wxmaxima.css" DESTINATION share/doc/wxmaxima)
