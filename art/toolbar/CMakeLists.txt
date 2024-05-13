include("${CMAKE_SOURCE_DIR}/cmake-bin2h/bin2h.cmake")

set(IMAGE_FILES
        arrow-up-square
        gtk-new
        gtk-open
        dialog-information
        gtk-paste
        edit-copy
        gtk-preferences
        edit-cut
        gtk-print
        edit-find
        gtk-redo
        eye-slash
        gtk-save
        go-bottom
        gtk-select-all
        go-jump
        gtk-stop
        go-last
        gtk-undo
        go-next
        input
        gtk-copy
        media-playback-stop
        gtk-cut
        software-update-urgent
        gtk-find
        text
        gtk-help
        view-refresh1

)

foreach(file ${IMAGE_FILES})
        add_custom_command(
	    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${file}.h
	    COMMAND ${CMAKE_COMMAND} ARGS
	    -DBIN2H_SOURCE_FILE="${CMAKE_CURRENT_SOURCE_DIR}/${file}.svg.gz"
	    -DBIN2H_VARIABLE_NAME=${file}_WXM
	    -DBIN2H_HEADER_FILE="${CMAKE_CURRENT_BINARY_DIR}/${file}.h"
	    -DBIN2H="${CMAKE_SOURCE_DIR}/cmake-bin2h/bin2h.cmake"
	    -P "${CMAKE_SOURCE_DIR}/src/bin2h.cmake"
	    COMMENT "Embedding ${file}.svg.gz"
	    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${file}.svg.gz ${CMAKE_SOURCE_DIR}/src/bin2h.cmake)
	add_custom_target(build_${file}.h DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${file}.h)
	add_dependencies(wxmaxima build_${file}.h)
endforeach()
