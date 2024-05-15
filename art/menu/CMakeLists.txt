set(SVG_FILES
	"Text-questionmark"
	"cell-divide"
	"watchlist"
)

# Embed the .svg files into .h files
foreach(file ${SVG_FILES})
        add_custom_command(
	    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${file}.h
	    COMMAND ${CMAKE_COMMAND} ARGS
	    -DBIN2H_SOURCE_FILE="${CMAKE_CURRENT_SOURCE_DIR}/${file}.svg"
	    -DBIN2H_VARIABLE_NAME=${file}_SVG
	    -DBIN2H_HEADER_FILE="${CMAKE_CURRENT_BINARY_DIR}/${file}.h"
	    -DBIN2H="${CMAKE_SOURCE_DIR}/cmake-bin2h/bin2h.cmake"
	    -P "${CMAKE_SOURCE_DIR}/src/bin2h.cmake"
	    COMMENT "Embedding ${file}.svg"
	    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${file}.svg ${CMAKE_SOURCE_DIR}/src/bin2h.cmake)
	add_custom_target(build_${file}.h DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${file}.h)
	add_dependencies(wxmaxima build_${file}.h)
endforeach()

