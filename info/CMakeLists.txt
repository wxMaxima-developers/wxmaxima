file(GLOB IMAGE_FILES *.jpg *.png)
        
find_program(MAKEINFO makeinfo)
if(NOT MAKEINFO)
    message(WARNING "Makeinfo not found. Old wxmaxima.html will be installed.")
    install(FILES wxmaxima.html DESTINATION share/doc/wxmaxima)
else()
    add_custom_command(OUTPUT "wxmaxima.info"
        COMMAND ${MAKEINFO} -o ${file}.info --no-split ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.texi)
    add_custom_command(OUTPUT "wxmaxima.html"
        COMMAND ${MAKEINFO} -o ${file}.info --html --no-split ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.texi)
    add_custom_command(OUTPUT "wxmaxima.pdf"
        COMMAND ${MAKEINFO} -o ${file}.info --pdf --no-split ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.texi)
    add_custom_target(wxmaxima.info ALL DEPENDS wxmaxima.info)
    add_custom_target(wxmaxima.html ALL DEPENDS wxmaxima.html)
    add_custom_target(wxmaxima.pdf  ALL DEPENDS wxmaxima.pdf)
    # where should the info file be installed? Is it used anywhere?
    install(FILES ${CMAKE_CURRENT_BINARY_DIR}/wxmaxima.html ${CMAKE_CURRENT_BINARY_DIR}/wxmaxima.pdf DESTINATION share/doc/wxmaxima)
endif()
        
install(FILES ${IMAGE_FILES} DESTINATION share/doc/wxmaxima)
