# -*- mode: CMake; cmake-tab-width: 4; -*-

include(FindPerl)

# "make pdf" creates the manual in PDF form. The other targets that now
# follow do the same for other formats
add_custom_target(pdf)
add_custom_target(docx)
add_custom_target(odt)
add_custom_target(epub)
add_custom_target(html ALL)

function(generate_wxmaxima_documentation FILEEXTENSION PANDOC_TEXSELECT_OPTION)
    set(FORMAT "${FILEEXTENSION}")
    # Set the format to "html5" to generate html with pandoc.
    if(FILEEXTENSION STREQUAL "html")
        set(FORMAT "html5")
    endif()
    # Set the format to "latex" to generate pdf with pandoc.
    if(FILEEXTENSION STREQUAL "pdf")
        set(FORMAT "latex")
    endif()
    add_custom_command(
        OUTPUT ${BASENAME}.${FILEEXTENSION}
        DEPENDS ${BASENAME}.md
        COMMAND ${PANDOC} ${BASENAME}.md -t ${FORMAT} -o ${BASENAME}.${FILEEXTENSION} ${PANDOC_ARGS} ${PANDOC_TEXSELECT_OPTION}
        COMMENT "Generating ${BASENAME}.${FILEEXTENSION}")
    add_custom_target(build_${BASENAME}.${FILEEXTENSION} DEPENDS ${BASENAME}.${FILEEXTENSION})
    add_dependencies(${FILEEXTENSION} build_${BASENAME}.${FILEEXTENSION})
    install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${BASENAME}.${FILEEXTENSION} DESTINATION share/doc/wxmaxima OPTIONAL)
endfunction()



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
if(NOT LATEX)
    message(WARNING "Xelatex or Lualatex not found. PDF documentation can not be converted from Markdown.")
endif()

find_program(PO4A_TRANSLATE po4a-translate)
if(PO4A_TRANSLATE)
    message(STATUS "Found po4a-translate at: ${PO4A_TRANSLATE}")
else()
    message(WARNING "po4a-translate not found. Localized documentation can not be converted from Markdown.")
    message(STATUS "No po4a-translate binary")
    if(NOT PO4A_TRANSLATE)
        find_package(Perl)
        if(PERL_FOUND)
            message(STATUS "Found perl")
            find_file(PO4A_TRANSLATE po4a-translate)
            if(PO4A_TRANSLATE)
                message(STATUS "Found po4a-translate file")
                get_filename_component(PO4A_TRANSLATE_DIR ${PO4A_TRANSLATE} DIRECTORY)
                set(PO4A-TRANSLATE ${PERL_EXECUTABLE} -I${PO4A_TRANSLATE_DIR}/lib ${PO4A_TRANSLATE})
                message(STATUS "PO4A_TRANSLATE = ${PO4A_TRANSLATE}")
            endif()
        endif()
    endif()
endif()

# Generate the manuals
find_program(PANDOC pandoc)
if(NOT PANDOC)
    message(WARNING "Pandoc not found. Documentation can not be converted from Markdown.")
endif()

if(PANDOC AND PO4A_TRANSLATE)
    execute_process(
        COMMAND ${PANDOC} --version
        OUTPUT_VARIABLE PANDOC_VERSION_TEXT)
    # get pandoc major version number, if >=2 use --pdf-engine else --latex-engine (they changed the name of the command line option)
    string(REGEX REPLACE "^pandoc ([0-9]).*$" "\\1"  PANDOC_VERSION ${PANDOC_VERSION_TEXT})
    if(PANDOC_VERSION LESS_EQUAL 1)
        message(STATUS "pandoc version < 2 found - using option --latex-engine=${LATEX}")
        set(PANDOC_TEXSELECT_OPTION "--latex-engine=${LATEX}")
    else()
        message(STATUS "pandoc version >= 2 found - using option --pdf-engine=${LATEX}")
        set(PANDOC_TEXSELECT_OPTION "--pdf-engine=${LATEX}")
    endif()
    set(PANDOC_ARGS --number-sections --table-of-contents --standalone --css=wxmaxima.css --metadata title="wxMaxima")
    if(NOT LATEX)
        message(STATUS "lualatex or xelatex required for building the pdf manual.")
    endif()
    file(GLOB IMAGEFILES ${CMAKE_CURRENT_SOURCE_DIR}/*.png)
    file(COPY ${IMAGEFILES} DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
    file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.md DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
    foreach(BASENAME ${BASENAMES})
        string(REGEX REPLACE "wxmaxima.(.*)$" "\\1" LANG ${BASENAME})
        string(REGEX REPLACE "wxmaxima$" "" LANG ${LANG})

        if(PO4A_TRANSLATE)
            if(NOT BASENAME STREQUAL "wxmaxima")
                add_custom_command(
                    OUTPUT ${BASENAME}.md
                    DEPENDS ${CMAKE_SOURCE_DIR}/locales/manual/${LANG}.po ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.md
                    COMMAND ${PO4A_TRANSLATE} -k 0 -f text -o markdown -m ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.md -p ${CMAKE_SOURCE_DIR}/locales/manual/${LANG}.po -l ${BASENAME}.md -M UTF-8)
            endif()
        endif()

        generate_wxmaxima_documentation(html "")
        generate_wxmaxima_documentation(odt "")
        generate_wxmaxima_documentation(docx "")
        generate_wxmaxima_documentation(epub "")
        if(LATEX)
            generate_wxmaxima_documentation(pdf "${PANDOC_TEXSELECT_OPTION}")
        endif()

        install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/${BASENAME}.html DESTINATION share/doc/wxmaxima)
    endforeach()
else()
    foreach(BASENAME ${BASENAMES})
        install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/${BASENAME}.html DESTINATION share/doc/wxmaxima)
    endforeach()
endif()

file(COPY wxmaxima.css DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
file(GLOB WXMAXIMA_ADDITIONAL_HELP_FILES *.jpg *.png *.css)
install(FILES ${WXMAXIMA_ADDITIONAL_HELP_FILES} DESTINATION share/doc/wxmaxima)


