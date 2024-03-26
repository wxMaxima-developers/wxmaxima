# -*- mode: CMake; cmake-tab-width: 4; -*-

find_program(PO4A po4a)
if(PO4A)
	# Get po4a version number
	execute_process(COMMAND ${PO4A} --version OUTPUT_VARIABLE PO4A_VERSION_TEXT)
	string(REGEX REPLACE "^po4a version ([^\n]*).*" "\\1"  PO4A_VERSION "${PO4A_VERSION_TEXT}")
	string(REGEX REPLACE "\\.$" ""  PO4A_VERSION "${PO4A_VERSION}") # remove trailing dot.
	message(STATUS "Found po4a: ${PO4A} (found version \"${PO4A_VERSION}\")")
	execute_process(COMMAND ${PO4A} --verbose --no-update ${CMAKE_BINARY_DIR}/po4a.cfg)
	message(STATUS "Calling po4a with the option --no-update - not updating the po/pot files in the source tree.")
else()
	message(STATUS "po4a not found. Disable the generation of localized documentation.")
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
	endfunction()

	function(generate_wxmaxima_documentation_pdf FILEBASENAME)
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
			COMMAND ${PANDOC} ${FILEBASENAME}.md -t latex -o ${FILEBASENAME}.pdf -M lang=${PDFLANG} -V 'mainfont:NotoSerif-Regular.ttf' -V 'sansfont:NotoSans-Regular.ttf' -V 'monofont:NotoSansMono-Regular.ttf' -V 'mathfont:NotoSansMath-Regular.ttf' -V 'CJKmainfont:NotoSerifCJK-Regular.ttc' --number-sections --table-of-contents --standalone --metadata title="wxMaxima" --pdf-engine=${WXMAXIMA_LATEX_COMMAND}
			COMMENT "Generating ${FILEBASENAME}.pdf")
		add_custom_target(build_${FILEBASENAME}.pdf DEPENDS ${FILEBASENAME}.pdf)
		add_dependencies(pdf build_${FILEBASENAME}.pdf)
		install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${FILEBASENAME}.pdf" DESTINATION share/doc/wxmaxima OPTIONAL)
	endfunction()

	function(generate_wxmaxima_documentation_epub FILEBASENAME)
		add_custom_command(
			OUTPUT ${FILEBASENAME}.epub
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

	file(GLOB IMAGEFILES ${CMAKE_CURRENT_SOURCE_DIR}/*.png)
	file(COPY ${IMAGEFILES} DESTINATION .)
	file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.md DESTINATION .)

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
			install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/${BASENAME}.html DESTINATION share/doc/wxmaxima)
		endforeach()
	endif()
else()
    message(STATUS "Pandoc not found. Documentation can not be converted from Markdown. Included HTML documentation will be installed.")
	file(GLOB WXMAXIMA_HTML_HELP "${CMAKE_CURRENT_SOURCE_DIR}/*.html")
	install(FILES ${WXMAXIMA_HTML_HELP} DESTINATION share/doc/wxmaxima)
	file(COPY ${WXMAXIMA_HTML_HELP} DESTINATION .)
endif()

file(GLOB HTMLHELP_ADDITIONAL_FILES *.png *.css *.svg)
install(FILES ${HTMLHELP_ADDITIONAL_FILES} DESTINATION share/doc/wxmaxima)
file(COPY ${HTMLHELP_ADDITIONAL_FILES} DESTINATION .)



