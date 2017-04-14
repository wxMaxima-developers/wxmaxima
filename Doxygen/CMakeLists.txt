find_package(Doxygen)

if(DOXYGEN_FOUND)
    # maybe these string commands may be changed to configure_file
    file(READ ${CMAKE_SOURCE_DIR}/Doxygen/Doxyfile DOXYFILE)
    file(COPY ${CMAKE_SOURCE_DIR}/Doxygen/wxMaxima.gif DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/html/)
    string(REPLACE "INPUT                  = ../src .." "INPUT = ${CMAKE_SOURCE_DIR}/src" DOXYFILE "${DOXYFILE}")
    string(REPLACE "PROJECT_LOGO           = ../data/wxmaxima.png" "PROJECT_LOGO ${CMAKE_SOURCE_DIR}/Doxygen/wxMaxima.gif" DOXYFILE "${DOXYFILE}")
    file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile.real "${DOXYFILE}")
    message(STATUS "Copied and patched ${CMAKE_SOURCE_DIR}/Doxygen/Doxyfile to ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile.real")
    # html should probably be renamed to 'doxygen', 'sourcedocumentation' or something like that
    add_custom_target(html
        COMMAND ${DOXYGEN_EXECUTABLE} Doxyfile.real)
else()
    message(STATUS "Doxygen not found. Source documentation will not be build.")
endif()
