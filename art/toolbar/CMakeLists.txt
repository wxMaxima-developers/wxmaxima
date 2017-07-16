file(GLOB pngfiles ${CMAKE_CURRENT_SOURCE_DIR}/*.png)

# Is needed on Windows and on Linux if the current theme lacks standard images.
install(FILES ${pngfiles} DESTINATION wxMaxima/art/toolbar)
