file(GLOB WXMAXIMA_HELP_FILES *.jpg *.png *.html *.hhp *.pdf)

#find_program(MAKEINFO makeinfo)
#if(NOT MAKEINFO)
#    message(WARNING "Makeinfo not found. Old wxmaxima.html will be installed.")
#else()
#    message(STATUS "Makeinfo found at ${MAKEINFO}")
#    add_custom_command(OUTPUT "wxmaxima.info"
#        COMMAND ${MAKEINFO} -o wxmaxima.info --no-split ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.texi)
#    add_custom_target(wxmaxima.info ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/wxmaxima.info)
#    # where should the info file be installed? Is it used anywhere?
#
#    add_custom_command(OUTPUT "wxmaxima.html"
#        COMMAND ${MAKEINFO} -o wxmaxima.html --html --force --no-split ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.texi)
#    add_custom_target(wxmaxima.html ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/wxmaxima.html)
#    install(FILES ${CMAKE_CURRENT_BINARY_DIR}/wxmaxima.html DESTINATION share/doc/wxmaxima)
#
#    if(BUILD_PDF_DOCUMENTATION)
#        add_custom_command(OUTPUT "wxmaxima.pdf"
#            COMMAND ${MAKEINFO} -o wxmaxima.pdf --pdf --force --no-split ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.texi)
#        add_custom_target(wxmaxima.pdf  ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/wxmaxima.pdf)
#        install(FILES ${CMAKE_CURRENT_BINARY_DIR}/wxmaxima.pdf DESTINATION share/doc/wxmaxima)
#    endif()
#endif()

install(FILES ${WXMAXIMA_HELP_FILES} DESTINATION share/doc/wxmaxima)

# allow local execution (./wxmaxima-local) from the build directory without installation
if(UNIX)
    file(COPY ${WXMAXIMA_HELP_FILES} DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
endif()

file(GLOB TRANSLATION_FILES ${CMAKE_SOURCE_DIR}/locales/manual/*.po)

find_program(LATEX lualatex xelatex)
find_program(PANDOC pandoc)
if(NOT PANDOC)
    message(FATAL_ERROR "Pandoc not found. Documentation can not converted from Markdown.")
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
    # the english manual has no special LANG extension
    if(LATEX)
      add_custom_target(wxmaxima.pdf DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.md
        COMMAND ${PANDOC} ${PANDOC_TEXSELECT_OPTION}=${LATEX} ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.md -o wxmaxima.pdf --number-sections --table-of-contents)
    else()
        message(STATUS "lualatex or xelatex required for building the pdf manual.")
    endif()
    foreach(POFILE ${TRANSLATION_FILES})
        string(REGEX REPLACE ".*locales/manual/(.*).po$" "\\1" LANG ${POFILE})
	if(LATEX)
          # it seems, ${PANDOC} --pdf-engine=xelatex is required for unicode handling.
          add_custom_target(wxmaxima.${LANG}.pdf DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.${LANG}.md
            COMMAND ${PANDOC} ${PANDOC_TEXSELECT_OPTION}=${LATEX} ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.${LANG}.md -o wxmaxima.${LANG}.pdf --number-sections --table-of-contents)
	endif()
        add_custom_target(wxmaxima.${LANG}.odt DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.${LANG}.md
            COMMAND ${PANDOC} ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.md -o wxmaxima.${LANG}.odt --number-sections --table-of-contents)
        add_custom_target(wxmaxima.${LANG}.epub DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.${LANG}.md
            COMMAND ${PANDOC} ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.md -o wxmaxima.${LANG}.epub --number-sections --table-of-contents --metadata title="wxMaxima")
        add_custom_target(wxmaxima.${LANG}.html DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.${LANG}.md
            COMMAND ${PANDOC} ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.md -o wxmaxima.${LANG}.html --number-sections --table-of-contents)
    endforeach()
endif()


foreach(POFILE ${TRANSLATION_FILES})
    string(REGEX REPLACE ".*locales/manual/(.*).po$" "\\1" LANG ${POFILE})
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.hhp.in ${CMAKE_CURRENT_BINARY_DIR}/wxmaxima.${LANG}.hhp)
endforeach()


