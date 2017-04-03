file(GLOB pngfiles ${CMAKE_CURRENT_SOURCE_DIR}/*.png)

install(FILES ${pngfiles} DESTINATION share/wxMaxima)
