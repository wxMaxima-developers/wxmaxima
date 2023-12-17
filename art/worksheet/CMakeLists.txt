include("${CMAKE_SOURCE_DIR}/cmake-bin2h/bin2h.cmake")

set(HEADER_FILE "${CMAKE_BINARY_DIR}/wxm_worksheet_images.h")
message(STATUS "Embedding following files into header file ${HEADER_FILE}")

file(WRITE "${HEADER_FILE}"
    "#ifndef WXM_WORKSHEET_IMAGES_H\n"
    "#define WXM_WORKSHEET_IMAGES_H\n")

file(GLOB WXM_FILES *.svg)
foreach(file ${WXM_FILES})
    message(STATUS "   Processing: ${file}")
    get_filename_component(variableName ${file} NAME_WE)
    set(HEADER_FILE "${CMAKE_BINARY_DIR}/${variableName}_svg.h")
    message(STATUS "Embedding following files into header file ${HEADER_FILE}")

    file(WRITE "${HEADER_FILE}"
        "#ifndef WXM_${variableName}_svg_H\n"
        "#define WXM_${variableName}_svg_H\n")

    file(APPEND "${HEADER_FILE}" "/* Data for the file: ${variableName} */\n")
    bin2h(SOURCE_FILE ${file} HEADER_FILE "${HEADER_FILE}" VARIABLE_NAME ${variableName} APPEND NULL_TERMINATE)
    file(APPEND "${HEADER_FILE}" "\n")
    file(APPEND "${HEADER_FILE}" "#endif\n")
endforeach()


