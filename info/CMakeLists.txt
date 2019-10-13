# -*- mode: CMake; cmake-tab-width: 4; -*-

# "make pdf" creates the manual in PDF form. The other of the targets that now
# follow do the same for other formats
add_custom_target(pdf)
add_custom_target(docx)
add_custom_target(odt)
add_custom_target(epub)

# Create a list of the common part of the name of all the translated manuals.
# The "list(TRANSFORM" type of commands only is available in very new cmake
# versions and therefore (in 2019) of only limited use so we'll have to do
# this by hand.
set(BASENAMES "wxmaxima")
file(GLOB POFILES ${CMAKE_SOURCE_DIR}/locales/manual/*.po)
foreach(POFILE ${POFILES})
    string(REGEX REPLACE ".*/(.*).po$" "wxmaxima.\\1" BASENAME ${POFILE})
    list(APPEND BASENAMES ${BASENAME})
endforeach()

# Find a suitable LaTeX installation
find_program(LATEX NAMES xelatex lualatex)

find_program(PO4ATRANSLATE po4a-translate)

# Generate the manuals
find_program(PANDOC pandoc)
if(PANDOC AND PO4ATRANSLATE)
    execute_process(
        COMMAND ${PANDOC} --version
        OUTPUT_VARIABLE PANDOC_VERSION_TEXT
	)
    # get pandoc major version number, if >2 use --pdf-engine else --latex-engine (they changed the name of the command line option)
    string(REGEX REPLACE "^pandoc ([0-9]).*$" "\\1"  PANDOC_VERSION ${PANDOC_VERSION_TEXT})
    if(PANDOC_VERSION LESS_EQUAL 1)
        message(STATUS "pandoc version < 2 found - using option --latex-engine")
        set(PANDOC_TEXSELECT_OPTION "--latex-engine")
    else()
        message(STATUS "pandoc version >= 2 found - using option --pdf-engine")
        set(PANDOC_TEXSELECT_OPTION "--pdf-engine")
    endif()
    set(PANDOC_ARGS --number-sections --table-of-contents -s --metadata title="wxMaxima")
    if(NOT LATEX)
        message(STATUS "lualatex or xelatex required for building the pdf manual.")
    endif()
    file(GLOB IMAGEFILES ${CMAKE_CURRENT_SOURCE_DIR}/*.png)
    file(COPY ${IMAGEFILES} DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
    file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.md DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
    foreach(BASENAME ${BASENAMES})
        if(LATEX)
            add_custom_command(
		TARGET  pdf
		DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${BASENAME}.md
		COMMAND ${PANDOC} ${CMAKE_CURRENT_BINARY_DIR}/${BASENAME}.md -o ${BASENAME}.pdf ${PANDOC_ARGS} ${PANDOC_TEXSELECT_OPTION}=${LATEX}
		COMMENT "Generating ${BASENAME}.pdf ...")
        endif()
        add_custom_command(
	    TARGET  odt
	    DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${BASENAME}.md
            COMMAND ${PANDOC} ${CMAKE_CURRENT_BINARY_DIR}/${BASENAME}.md -o ${BASENAME}.odt ${PANDOC_ARGS}
	    COMMENT "Generating ${BASENAME}.odt ...")
        add_custom_command(
	    TARGET  epub
	    DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${BASENAME}.md
            COMMAND ${PANDOC} ${CMAKE_CURRENT_BINARY_DIR}/${BASENAME}.md -o ${BASENAME}.epub ${PANDOC_ARGS}
	    COMMENT "Generating ${BASENAME}.epub ...")
        add_custom_command(
	    TARGET  docx
	    DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${BASENAME}.md
            COMMAND ${PANDOC} ${CMAKE_CURRENT_BINARY_DIR}/${BASENAME}.md -o ${BASENAME}.docx ${PANDOC_ARGS}
	    COMMENT "Generating ${BASENAME}.docx ...")
	string(REGEX REPLACE "wxmaxima.(.*)$" "\\1" LANG ${BASENAME})
	if(PO4ATRANSLATE)
	    if(NOT BASENAME STREQUAL "wxmaxima")
		add_custom_command(
		    OUTPUT ${BASENAME}.md
		    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/../locales/manual/${LANG}.po ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.md
		    COMMAND po4a-translate -k 0 -f text -m ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.md -p ${CMAKE_CURRENT_SOURCE_DIR}/../locales/manual/${LANG}.po -l ${BASENAME}.md -M UTF-8)
	    endif()
	endif()
	add_custom_command(
	    OUTPUT ${BASENAME}.html
	    DEPENDS ${BASENAME}.md
	    COMMAND ${PANDOC} ${CMAKE_CURRENT_BINARY_DIR}/${BASENAME}.md -o ${CMAKE_CURRENT_BINARY_DIR}/${BASENAME}.html ${PANDOC_ARGS})
	add_custom_target(
	    ${BASENAME}.html.stamp ALL
	    DEPENDS ${BASENAME}.html
	    )
	configure_file(${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.hhp.in ${BASENAME}.hhp)
    endforeach()
else()
    message(WARNING "Pandoc or po4a-translate not found. Documentation can not converted from Markdown.")
    # allow local execution (./wxmaxima-local) from the build directory without installation
    # and ensure that the images will be found by pandoc
    file(COPY ${WXMAXIMA_HELP_FILES} DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
endif()

file(GLOB WXMAXIMA_HELP_FILES *.jpg *.png *.html *.hhp *.pdf)
install(FILES ${WXMAXIMA_HELP_FILES} DESTINATION share/doc/wxmaxima)


