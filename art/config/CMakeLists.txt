file(GLOB CONFIG_PNGFILES ${CMAKE_CURRENT_SOURCE_DIR}/*.png)

if(WIN32)
    install(FILES ${CONFIG_PNGFILES} DESTINATION wxMaxima/art/config)
else()
    install(FILES ${CONFIG_PNGFILES} DESTINATION share/wxMaxima)
endif()
