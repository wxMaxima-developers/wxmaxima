include("${CMAKE_SOURCE_DIR}/cmake-bin2h/bin2h.cmake")

set(IMAGE_FILES
        arrow-up-square
        dialog-information
        gtk-preferences
        eye-slash
        go-bottom
        gtk-select-all
        go-jump
        gtk-stop
        go-last
        go-next
        input
        media-playback-stop
        software-update-urgent
        text
        view-refresh1
)

foreach(file ${IMAGE_FILES})
        add_custom_command(
	    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${file}.h
	    COMMAND ${CMAKE_COMMAND} ARGS
	    -DBIN2H_SOURCE_FILE="${CMAKE_CURRENT_SOURCE_DIR}/${file}.svg.gz"
	    -DBIN2H_VARIABLE_NAME=${file}_SVG_GZ
	    -DBIN2H_HEADER_FILE="${CMAKE_CURRENT_BINARY_DIR}/${file}.h"
	    -DBIN2H="${CMAKE_SOURCE_DIR}/cmake-bin2h/bin2h.cmake"
	    -P "${CMAKE_SOURCE_DIR}/src/bin2h.cmake"
	    COMMENT "Embedding ${file}.svg.gz"
	    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${file}.svg.gz ${CMAKE_SOURCE_DIR}/src/bin2h.cmake)
	add_custom_target(build_${file}.h DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${file}.h)
	add_dependencies(wxmaxima build_${file}.h)
endforeach()

# For the regular wxMaximaArtProvider (the "wxWidgets-way") use the uncompressed SVG.
set(SVG_FILES
        eye-slash
)

# Embed the .svg files into .h files
foreach(file ${SVG_FILES})
        add_custom_command(
            OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${file}.svg.h
            COMMAND ${CMAKE_COMMAND} ARGS
            -DBIN2H_SOURCE_FILE="${CMAKE_CURRENT_SOURCE_DIR}/${file}.svg"
            -DBIN2H_VARIABLE_NAME=${file}_SVG
            -DBIN2H_HEADER_FILE="${CMAKE_CURRENT_BINARY_DIR}/${file}.svg.h"
            -DBIN2H="${CMAKE_SOURCE_DIR}/cmake-bin2h/bin2h.cmake"
            -P "${CMAKE_SOURCE_DIR}/src/bin2h.cmake"
            COMMENT "Embedding ${file}.svg"
            DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${file}.svg ${CMAKE_SOURCE_DIR}/src/bin2h.cmake)
        add_custom_target(build_${file}.svg.h DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${file}.svg.h)
        add_dependencies(wxmaxima build_${file}.svg.h)
endforeach()
