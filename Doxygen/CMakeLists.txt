# -*- mode: CMake; cmake-tab-width: 4; -*-

find_package(Doxygen)

if(DOXYGEN_FOUND)
    file(COPY ${CMAKE_SOURCE_DIR}/Doxygen/wxMaxima.gif DESTINATION html/)
    set(DOXYGEN_PROJECT_LOGO ${CMAKE_SOURCE_DIR}/data/io.github.wxmaxima_developers.wxMaxima.png)
    file(GLOB WXM_GENERATED_FILES ${CMAKE_BINARY_DIR}/*.h)
    doxygen_add_docs(Doxygen ${CMAKE_SOURCE_DIR} ${WXM_GENERATED_FILES})
endif()
