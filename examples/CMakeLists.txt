include("${CMAKE_SOURCE_DIR}/cmake-bin2h/bin2h.cmake")


set(WXM_FILES
	"CASvsProgrammingLanguage"
	"diffEquations"
	"displaying3DCurves"
	"fastListAccess"
	"fittingEquations"
	"memoizing"
	"numberFormats"
	"solvingEquations"
	"toleranceCalculations"
	"variableNames"
)

# Copy the .wxm files to the binary dir so they can be used as test case without
# being afraid that we change the test cases somehow.

foreach(file ${WXM_FILES})
	file(
	    COPY
	    ${file}.wxm
	    DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
	    FILE_PERMISSIONS OWNER_WRITE OWNER_READ
        )
endforeach()

foreach(file ${WXM_FILES})
	add_custom_command(
	        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${file}.h
	        COMMAND ${CMAKE_COMMAND} ARGS
	        -DBIN2H_SOURCE_FILE="${CMAKE_CURRENT_SOURCE_DIR}/${file}.wxm"
	        -DBIN2H_VARIABLE_NAME=${file}_WXM
	        -DBIN2H_HEADER_FILE="${CMAKE_CURRENT_BINARY_DIR}/${file}.h"
	        -DBIN2H="${CMAKE_SOURCE_DIR}/cmake-bin2h/bin2h.cmake"
	        -P "${CMAKE_SOURCE_DIR}/src/bin2h.cmake"
	        COMMENT "Embedding ${file}.wxm"
	        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${file}.wxm ${CMAKE_SOURCE_DIR}/src/bin2h.cmake
	)
	add_custom_target(build${file}h DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${file}.h)
	add_dependencies(wxmaxima build${file}h)
endforeach()

