file(GLOB INFO_FILES
        ${CMAKE_CURRENT_SOURCE_DIR}/*.jpg
        ${CMAKE_CURRENT_SOURCE_DIR}/*.png
        ${CMAKE_CURRENT_SOURCE_DIR}/*.html)

install(FILES ${INFO_FILES} DESTINATION share/doc/wxmaxima)
