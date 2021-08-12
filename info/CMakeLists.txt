# -*- mode: CMake; cmake-tab-width: 4; -*-


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
		if(UNIX)
			# Remove workaround for IE9
			set(REMOVE_HTML5SHIV cp ${FILEBASENAME}.html sedinput && sed /html5shiv-printshiv.min.js/d <sedinput >${FILEBASENAME}.html)
		endif()
		add_custom_command(
			OUTPUT ${FILEBASENAME}.html
			DEPENDS ${FILEBASENAME}.md
			COMMAND ${PANDOC} ${FILEBASENAME}.md -t html5 "--include-in-header=${CMAKE_CURRENT_SOURCE_DIR}/header.html" -o ${FILEBASENAME}.html --number-sections --table-of-contents --standalone --css=wxmaxima.css --metadata title="wxMaxima"
			COMMAND ${REMOVE_HTML5SHIV}
			COMMENT "Generating ${FILEBASENAME}.html")
		add_custom_target(build_${FILEBASENAME}.html DEPENDS ${FILEBASENAME}.html)
		add_dependencies(html build_${FILEBASENAME}.html)
		install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${FILEBASENAME}.html" DESTINATION share/doc/wxmaxima OPTIONAL)
	endfunction()

	function(generate_wxmaxima_documentation_pdf FILEBASENAME PANDOC_TEXSELECT_OPTION)
		set(PDFLANG "${LANG}")
		# if LANG is empty, we are generating the default (english) manual.
		if("${PDFLANG}" STREQUAL "")
			set(PDFLANG "en")
		endif()
		# For Chinese use English, I have not found out, what must be
		# set for chinese (at least on my system, nothing worked).
		if("${PDFLANG}" STREQUAL "zh_CN")
			set(PDFLANG "en")
		endif()

		add_custom_command(
			OUTPUT ${FILEBASENAME}.pdf
			DEPENDS ${FILEBASENAME}.md
			COMMAND ${PANDOC} ${FILEBASENAME}.md -t latex -o ${FILEBASENAME}.pdf -M lang=${PDFLANG} -V 'mainfont:NotoSerif-Regular.ttf' -V 'sansfont:NotoSans-Regular.ttf' -V 'monofont:NotoSansMono-Regular.ttf' -V 'mathfont:NotoSansMath-Regular.ttf' -V 'CJKmainfont:NotoSerifCJK-Regular.ttc' --number-sections --table-of-contents --standalone --metadata title="wxMaxima" ${PANDOC_TEXSELECT_OPTION}
			COMMENT "Generating ${FILEBASENAME}.pdf")
		add_custom_target(build_${FILEBASENAME}.pdf DEPENDS ${FILEBASENAME}.pdf)
		add_dependencies(pdf build_${FILEBASENAME}.pdf)
		install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${FILEBASENAME}.pdf" DESTINATION share/doc/wxmaxima OPTIONAL)
	endfunction()

	function(generate_wxmaxima_documentation_epub FILEBASENAME)
		add_custom_command(
			OUTPUT ${FILEBASENAME}.epub
			DEPENDS ${FILEBASENAME}.md
			COMMAND ${PANDOC} ${FILEBASENAME}.md -t epub -o ${FILEBASENAME}.epub --number-sections --table-of-contents --standalone --css=wxmaxima.css --metadata title="wxMaxima"
			COMMENT "Generating ${FILEBASENAME}.epub")
		add_custom_target(build_${FILEBASENAME}.epub DEPENDS ${FILEBASENAME}.epub)
		add_dependencies(epub build_${FILEBASENAME}.epub)
		install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${FILEBASENAME}.epub" DESTINATION share/doc/wxmaxima OPTIONAL)
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

	find_program(PO4A_TRANSLATE po4a-translate)
	if(PO4A_TRANSLATE)
		# Get po4a-translate version number, only >= 0.55 works for wxMaxima.
		execute_process(COMMAND ${PO4A_TRANSLATE} --version
			OUTPUT_VARIABLE PO4A_TRANSLATE_VERSION_TEXT)
		string(REGEX REPLACE "^po4a-translate version ([^\n]*).*" "\\1"  PO4A_TRANSLATE_VERSION "${PO4A_TRANSLATE_VERSION_TEXT}")
		string(REGEX REPLACE "\\.$" ""  PO4A_TRANSLATE_VERSION "${PO4A_TRANSLATE_VERSION}") # remove trailing dot.
		message(STATUS "Found po4a-translate: ${PO4A_TRANSLATE} (found version \"${PO4A_TRANSLATE_VERSION}\")")
		if(PO4A_TRANSLATE_VERSION VERSION_LESS 0.55)
			message(WARNING "po4a-translate Version < 0.55 found. Please upgrade po4a, that version does not work. "
							"Disable po4a and the generation of localized documentation.")
			set(PO4A_TRANSLATE "")
		endif()
	else()
		message(STATUS "po4a-translate not found. Disable the generation of localized documentation.")
	endif()


	file(GLOB IMAGEFILES ${CMAKE_CURRENT_SOURCE_DIR}/*.png)
	file(COPY ${IMAGEFILES} DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
	file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.md DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

	generate_wxmaxima_documentation_html("wxmaxima")
	generate_wxmaxima_documentation_epub("wxmaxima")
	if(WXMAXIMA_LATEX_COMMAND)
		if(PANDOC_VERSION LESS_EQUAL 1)
			message(STATUS "pandoc version < 2 found - using option --latex-engine=${WXMAXIMA_LATEX_COMMAND}")
			set(PANDOC_TEXSELECT_OPTION "--latex-engine=${WXMAXIMA_LATEX_COMMAND}")
		else()
			message(STATUS "pandoc version >= 2 found - using option --pdf-engine=${WXMAXIMA_LATEX_COMMAND}")
			set(PANDOC_TEXSELECT_OPTION "--pdf-engine=${WXMAXIMA_LATEX_COMMAND}")
		endif()
		generate_wxmaxima_documentation_pdf("wxmaxima" "${PANDOC_TEXSELECT_OPTION}")
    endif()

	if(PO4A_TRANSLATE)
		foreach(BASENAME ${BASENAMES})
			string(REGEX REPLACE "wxmaxima.(.*)$" "\\1" LANG ${BASENAME})
			string(REGEX REPLACE "wxmaxima$" "" LANG ${LANG})

			add_custom_command(
				OUTPUT ${BASENAME}.md
				DEPENDS ${CMAKE_SOURCE_DIR}/locales/manual/${LANG}.po ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.md
				COMMAND ${PO4A_TRANSLATE} -k 0 -f text -o markdown -m ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.md -p ${CMAKE_SOURCE_DIR}/locales/manual/${LANG}.po -l ${BASENAME}.md -M UTF-8)

			generate_wxmaxima_documentation_html("${BASENAME}")
			generate_wxmaxima_documentation_epub("${BASENAME}")
			if(WXMAXIMA_LATEX_COMMAND)
				generate_wxmaxima_documentation_pdf("${BASENAME}" "${PANDOC_TEXSELECT_OPTION}")
			endif()
		endforeach()
	else()
		foreach(BASENAME ${BASENAMES})
			install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/${BASENAME}.html DESTINATION share/doc/wxmaxima)
		endforeach()
	endif()
else()
    message(STATUS "Pandoc not found. Documentation can not be converted from Markdown. Included HTML documentation will be installed.")
	file(GLOB WXMAXIMA_HTML_HELP "${CMAKE_CURRENT_SOURCE_DIR}/*.html")
	install(FILES ${WXMAXIMA_HTML_HELP} DESTINATION share/doc/wxmaxima)
	file(COPY ${WXMAXIMA_HTML_HELP} DESTINATION "${CMAKE_CURRENT_BINARY_DIR}")
endif()

set(HTMLHELP_ADDITIONAL_FILES wxmaxima.css
                              BatchImage.png cell-example.png ezUnits.png InputCell.png MatrixTableExample.png
                              maxima_screenshot.png sbclMemory.png SecondTableExample.png SidePanes.png
                              wxMaxima_configuration_001.png wxMaximaLogo.png wxMaximaWindow.png wxmaxima.svg)
foreach(CURRENTFILE ${HTMLHELP_ADDITIONAL_FILES})
	install(FILES "${CMAKE_CURRENT_SOURCE_DIR}/${CURRENTFILE}" DESTINATION share/doc/wxmaxima)
	file(COPY "${CMAKE_CURRENT_SOURCE_DIR}/${CURRENTFILE}" DESTINATION "${CMAKE_CURRENT_BINARY_DIR}")
endforeach()



