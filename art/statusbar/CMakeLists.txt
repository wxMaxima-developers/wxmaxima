file(GLOB pngfiles ${CMAKE_CURRENT_SOURCE_DIR}/*.png)

if(WIN32)
    install(FILES ${pngfiles} DESTINATION wxMaxima/art/statusbar)
else()
    install(FILES ${pngfiles} DESTINATION share/wxMaxima)
endif()
