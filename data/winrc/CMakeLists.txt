set(RESSOURCES ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.manifest)

# need only be installed on Windows.
if(WIN32)
    install(FILES ${RESSOURCES} DESTINATION share/wxMaxima)
endif()
