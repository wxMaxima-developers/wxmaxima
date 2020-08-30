# -*- mode: CMake; cmake-tab-width: 4; -*-

add_subdirectory(wxMaxima)
if(PANDOC)
	add_subdirectory(manual)
endif()
