file(GLOB IMAGE_FILES *.jpg *.png)
        
find_program(MAKEINFO makeinfo)
if(NOT MAKEINFO)
    message(WARNING "Makeinfo not found. Old wxmaxima.html will be installed.")
    install(FILES wxmaxima.html DESTINATION share/doc/wxmaxima)
else()
    message(STATUS "Makeinfo found at ${MAKEINFO}")
    add_custom_command(OUTPUT "wxmaxima.info"
        COMMAND ${MAKEINFO} -o wxmaxima.info --no-split ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.texi)
    add_custom_target(wxmaxima.info ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/wxmaxima.info)
    # where should the info file be installed? Is it used anywhere?
    
    add_custom_command(OUTPUT "wxmaxima.html"
        COMMAND ${MAKEINFO} -o wxmaxima.html --html --force --no-split ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.texi)
    add_custom_target(wxmaxima.html ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/wxmaxima.html)
    install(FILES ${CMAKE_CURRENT_BINARY_DIR}/wxmaxima.html DESTINATION share/doc/wxmaxima)
    
    # makeinfo on MacOS does not know the option "--pdf"??? Do not build the PDF there.
    if(NOT APPLE)
        add_custom_command(OUTPUT "wxmaxima.pdf"
            COMMAND ${MAKEINFO} -o wxmaxima.pdf --pdf --force --no-split ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.texi)
        add_custom_target(wxmaxima.pdf  ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/wxmaxima.pdf)
        install(FILES ${CMAKE_CURRENT_BINARY_DIR}/wxmaxima.pdf DESTINATION share/doc/wxmaxima)
    endif()
endif()
        
install(FILES ${IMAGE_FILES} DESTINATION share/doc/wxmaxima)
