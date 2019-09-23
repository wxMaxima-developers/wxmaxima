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

find_program(PANDOC pandoc)
if(NOT PANDOC)
    message(WARNING "Pandoc not found. Documentation can not converted from Markdown.")
else()
    add_custom_target(wxmaxima.pdf ALL
        COMMAND ${PANDOC} ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.md -o wxmaxima.pdf --number-sections --table-of-contents
    )
    add_custom_target(wxmaxima.docx ALL
        COMMAND ${PANDOC} ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.md -o wxmaxima.docx --number-sections --table-of-contents
    )
    add_custom_target(wxmaxima.epub ALL
        COMMAND ${PANDOC} ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.md -o wxmaxima.epub --number-sections --table-of-contents
    )
    add_custom_target(wxmaxima.html ALL
        COMMAND ${PANDOC} ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.md -o wxmaxima.html --number-sections --table-of-contents
    )
endif()

file(GLOB TRANSLATION_FILES ${CMAKE_SOURCE_DIR}/locales/manual/*.po)

foreach(POFILE ${TRANSLATION_FILES})
  string(REGEX REPLACE ".*locales/manual/(.*).po$" "\\1" LANG ${POFILE})
  message(WARNING "POFILE = ${POFILE}")
  message(WARNING "LANG = ${LANG}")
  configure_file(${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.hhp.in ${CMAKE_CURRENT_BINARY_DIR}/wxmaxima.${LANG}.hhp)
endforeach()


