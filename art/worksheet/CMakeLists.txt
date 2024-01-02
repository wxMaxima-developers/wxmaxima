add_custom_command(
        OUTPUT ${CMAKE_BINARY_DIR}/sumSign_svg.h
        COMMAND ${CMAKE_COMMAND} ARGS
        -DBIN2H_SOURCE_FILE="${CMAKE_CURRENT_SOURCE_DIR}/sumSign.svg"
        -DBIN2H_VARIABLE_NAME=sumSign
        -DBIN2H_HEADER_FILE="${CMAKE_BINARY_DIR}/sumSign_svg.h"
        -DBIN2H="${CMAKE_SOURCE_DIR}/cmake-bin2h/bin2h.cmake"
        -P "${CMAKE_SOURCE_DIR}/src/bin2h.cmake"
        COMMENT "Embedding sumSign.svg"
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/sumSign.svg ${CMAKE_SOURCE_DIR}/src/bin2h.cmake
)
add_custom_target(build_sumSign.h DEPENDS ${CMAKE_BINARY_DIR}/sumSign_svg.h)
add_dependencies(wxmaxima build_sumSign.h)

add_custom_command(
        OUTPUT ${CMAKE_BINARY_DIR}/prodSign_svg.h
        COMMAND ${CMAKE_COMMAND} ARGS
        -DBIN2H_SOURCE_FILE="${CMAKE_CURRENT_SOURCE_DIR}/prodSign.svg"
        -DBIN2H_VARIABLE_NAME=prodSign
        -DBIN2H_HEADER_FILE="${CMAKE_BINARY_DIR}/prodSign_svg.h"
        -DBIN2H="${CMAKE_SOURCE_DIR}/cmake-bin2h/bin2h.cmake"
        -P "${CMAKE_SOURCE_DIR}/src/bin2h.cmake"
        COMMENT "Embedding prodSign.svg"
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/prodSign.svg ${CMAKE_SOURCE_DIR}/src/bin2h.cmake
)
add_custom_target(build_prodSign.h DEPENDS ${CMAKE_BINARY_DIR}/prodSign_svg.h)
add_dependencies(wxmaxima build_prodSign.h)

add_custom_command(
        OUTPUT ${CMAKE_BINARY_DIR}/intSign_svg.h
        COMMAND ${CMAKE_COMMAND} ARGS
        -DBIN2H_SOURCE_FILE="${CMAKE_CURRENT_SOURCE_DIR}/intSign.svg"
        -DBIN2H_VARIABLE_NAME=intSign
        -DBIN2H_HEADER_FILE="${CMAKE_BINARY_DIR}/intSign_svg.h"
        -DBIN2H="${CMAKE_SOURCE_DIR}/cmake-bin2h/bin2h.cmake"
        -P "${CMAKE_SOURCE_DIR}/src/bin2h.cmake"
        COMMENT "Embedding intSign.svg"
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/intSign.svg ${CMAKE_SOURCE_DIR}/src/bin2h.cmake
)
add_custom_target(build_intSign.h DEPENDS ${CMAKE_BINARY_DIR}/intSign_svg.h)
add_dependencies(wxmaxima build_intSign.h)
