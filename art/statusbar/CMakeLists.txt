include("${CMAKE_SOURCE_DIR}/cmake-bin2h/bin2h.cmake")

set(HEADER_FILE "${CMAKE_BINARY_DIR}/wxm_statusbar_images.h")
message(STATUS "Embedding following files into header file ${HEADER_FILE}")

file(WRITE "${HEADER_FILE}"
    "#ifndef WXM_STATUSBAR_IMAGES_H\n"
    "#define WXM_STATUSBAR_IMAGES_H\n")

set(IMAGE_FILES
  network-idle.svg.gz
  Dialog-accept.svg.gz
  network-offline.svg.gz
  Emblem-equal-defined.svg.gz
  network-receive.svg.gz
  Emblem-important.svg.gz
  network-transmit-receive.svg.gz
  Go-previous.svg.gz
  network-transmit.svg.gz
  Go-up.svg.gz
  System-lock-screen.svg.gz
  network-error.svg.gz
  Waiting.svg.gz)

foreach(file ${IMAGE_FILES})
    message(STATUS "   Processing: ${file}")
    get_filename_component(variableName ${file} NAME)
    file(APPEND "${HEADER_FILE}" "/* Data for the file: ${variableName} */\n")
    bin2h(SOURCE_FILE ${file} HEADER_FILE "${HEADER_FILE}" VARIABLE_NAME ${variableName} APPEND NULL_TERMINATE)
    file(APPEND "${HEADER_FILE}" "\n")
endforeach()

file(APPEND "${HEADER_FILE}" "#endif // WXM_STATUSBAR_IMAGES_H\n")
