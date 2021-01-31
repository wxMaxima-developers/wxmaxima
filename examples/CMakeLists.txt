include("${CMAKE_SOURCE_DIR}/cmake-bin2h/bin2h.cmake")

set(HEADER_FILE "${CMAKE_CURRENT_BINARY_DIR}/examples.h")
message(STATUS "Embedding following files into header file ${HEADER_FILE}")

file(REMOVE "${HEADER_FILE}")
file(GLOB WXM_FILES *.wxm)
foreach(file ${WXM_FILES})
    message(STATUS "   Processing: ${file}")

    get_filename_component(variableName ${file} NAME)
    file(APPEND "${HEADER_FILE}" "/* Data for the example: ${variableName} */\n")

    bin2h(SOURCE_FILE ${file} HEADER_FILE "${HEADER_FILE}" VARIABLE_NAME ${variableName} APPEND NULL_TERMINATE)
    file(APPEND "${HEADER_FILE}" "\n")
endforeach()

