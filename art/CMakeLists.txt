add_subdirectory(config)
add_subdirectory(statusbar)
# For Windows install these icons - for Unix/Linux they are usually installed with GTK
if(WIN32)
    add_subdirectory(toolbar)
endif()
