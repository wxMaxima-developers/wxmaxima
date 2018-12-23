file(GLOB fontfiles ${CMAKE_CURRENT_SOURCE_DIR}/*.ttf)

# need only be installed on Windows.
if(WIN32)
install(FILES ${fontfiles} DESTINATION wxMaxima/fonts)
endif()
