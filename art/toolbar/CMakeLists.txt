file(GLOB pngfiles ${CMAKE_CURRENT_SOURCE_DIR}/*.png)

if(WIN32)
    install(FILES ${pngfiles} DESTINATION wxMaxima/art/toolbar)
else()
    install(FILES ${pngfiles} DESTINATION share/wxMaxima)
endif()

# allow local execution (./wxmaxima-local) from the build directory without installation
if(UNIX)
    file(COPY ${pngfiles} DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
endif()  
