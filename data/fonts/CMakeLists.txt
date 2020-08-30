# -*- mode: CMake; cmake-tab-width: 4; -*-

file(GLOB fontfiles ${CMAKE_CURRENT_SOURCE_DIR}/*.ttf)

# need only be installed on Windows.
if(WIN32)
install(FILES ${fontfiles} DESTINATION share/fonts)
endif()
