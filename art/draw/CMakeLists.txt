include("${CMAKE_SOURCE_DIR}/cmake-bin2h/bin2h.cmake")

set(HEADER_FILE "${CMAKE_BINARY_DIR}/wxm_draw_images.h")
message(STATUS "Embedding following files into header file ${HEADER_FILE}")

file(WRITE "${HEADER_FILE}.tmp"
    "#ifndef WXM_DRAW_IMAGES_H\n"
    "#define WXM_DRAW_IMAGES_H\n")

file(GLOB WXM_FILES *.svg.gz)
foreach(file ${WXM_FILES})
    message(STATUS "   Processing: ${file}")
    get_filename_component(variableName ${file} NAME)
    file(APPEND "${HEADER_FILE}.tmp" "/* Data for the file: ${variableName} */\n")
    bin2h(SOURCE_FILE ${file} HEADER_FILE "${HEADER_FILE}.tmp" VARIABLE_NAME ${variableName} APPEND NULL_TERMINATE)
    file(APPEND "${HEADER_FILE}.tmp" "\n")
endforeach()

file(APPEND "${HEADER_FILE}.tmp" "#endif // WXM_DRAW_IMAGES_H\n")
# write to a *.tmp file first and then 'copy_if_different' prevents rebuilding source files, if nothing (but a timestamp) changed.
execute_process(COMMAND ${CMAKE_COMMAND} -E copy_if_different "${HEADER_FILE}.tmp" "${HEADER_FILE}")
