# -*- mode: CMake; cmake-tab-width: 4; -*-

# Create a list of the common part of the name of all the translated manuals
file(GLOB BASENAMES ${CMAKE_SOURCE_DIR}/locales/manual/*.po)
list(TRANSFORM BASENAMES REPLACE ".*/(.*).po$" "wxmaxima.\\1")
list(APPEND BASENAMES "wxmaxima")

add_custom_target(pdf)
add_custom_target(odt)
add_custom_target(epub)

# Find a suitable LaTeX installation
find_program(LATEX NAMES xelatex lualatex)

# Generate the manuals
find_program(PANDOC pandoc)
if(NOT PANDOC)
    message(WARNING "Pandoc not found. Documentation can not converted from Markdown.")
else()
    execute_process(
        COMMAND ${PANDOC} --version
        OUTPUT_VARIABLE PANDOC_VERSION_TEXT
	)
    # get pandoc major version number, if >2 use --pdf-engine else --latex-engine (they changed the name of the command line option)
    string(REGEX REPLACE "^pandoc ([0-9]).*$" "\\1"  PANDOC_VERSION ${PANDOC_VERSION_TEXT})
    if(PANDOC_VERSION GREATER 1)
        message(STATUS "pandoc version >= 2 found - using option --pdf-engine")
        set(PANDOC_TEXSELECT_OPTION "--pdf-engine")
    else()
        message(STATUS "pandoc version < 2 found - using option --latex-engine")
        set(PANDOC_TEXSELECT_OPTION "--latex-engine")
    endif()
    set(PANDOC_ARGS --number-sections --table-of-contents -s --metadata title="wxMaxima" ${PANDOC_TEXSELECT_OPTION}=${LATEX})
    if(NOT LATEX)
        message(STATUS "lualatex or xelatex required for building the pdf manual.")
    endif()
    foreach(BASENAME ${BASENAMES})
        if(LATEX)
            add_custom_command(
		TARGET  pdf
		DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${BASENAME}.md
		COMMAND ${PANDOC} ${CMAKE_CURRENT_SOURCE_DIR}/${BASENAME}.md -o ${BASENAME}.pdf ${PANDOC_ARGS}
		COMMENT "Generating ${BASENAME}.pdf ...")
        endif()
        add_custom_command(
	    TARGET  odt
	    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${BASENAME}.md
            COMMAND ${PANDOC} ${CMAKE_CURRENT_SOURCE_DIR}/${BASENAME}.md -o ${BASENAME}.odt ${PANDOC_ARGS}
	    COMMENT "Generating ${BASENAME}.odt ...")
        add_custom_command(
	    TARGET  epub
	    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${BASENAME}.md
            COMMAND ${PANDOC} ${CMAKE_CURRENT_SOURCE_DIR}/${BASENAME}.md -o ${BASENAME}.epub ${PANDOC_ARGS}
	    COMMENT "Generating ${BASENAME}.epub ...")
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
endif()

file(GLOB WXMAXIMA_HELP_FILES *.jpg *.png *.html *.hhp *.pdf)
install(FILES ${WXMAXIMA_HELP_FILES} DESTINATION share/doc/wxmaxima)

# allow local execution (./wxmaxima-local) from the build directory without installation
# and ensure that the images will be found by pandoc
file(COPY ${WXMAXIMA_HELP_FILES} DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
