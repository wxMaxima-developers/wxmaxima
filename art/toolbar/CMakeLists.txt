file(GLOB pngfiles ${CMAKE_CURRENT_SOURCE_DIR}/*.png)

# need only be installed on Windows.
install(FILES ${pngfiles} DESTINATION wxMaxima/art/toolbar)
