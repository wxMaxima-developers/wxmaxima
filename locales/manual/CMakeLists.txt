# -*- mode: CMake; cmake-tab-width: 4; -*-

add_custom_target(update-locale-manual)
set_target_properties(update-locale-manual
    PROPERTIES OUTPUT_NAME update-locale)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/po4a.cfg.in ${CMAKE_CURRENT_BINARY_DIR}/po4a.cfg)

find_program(PO4A po4a)

find_program(TOUCH touch)

file(GLOB POT_SOURCE_FILES ${CMAKE_CURRENT_BINARY_DIR}/*.po)

if(PO4A)
    add_custom_command(
	OUTPUT ${CMAKE_SOURCE_DIR}/info/wxmaxima.de.md
	DEPENDS ${CMAKE_SOURCE_DIR}/info/wxmaxima.md
	DEPENDS ${POT_SOURCE_FILES}
	COMMAND ${PO4A} -k 0 po4a.cfg
	COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_BINARY_DIR}/wxMaxima.pot" "${CMAKE_SOURCE_DIR}/locales/manual/wxmaxima.md.pot"
	COMMAND ${CMAKE_COMMAND} -E copy "*.md" "${CMAKE_SOURCE_DIR}/info"
	COMMAND ${TOUCH} ${CMAKE_SOURCE_DIR}/info/wxmaxima.de.md
	COMMENT "Updating wxmaxima.md.pot and the po-files in the source tree in order to keep the source files in sync.")
else()
    message(STATUS "po4a not found - translations of the manual won't be updated on running \"make update-locale\"")
endif()


add_custom_target(
    po4a.stamp ALL
    DEPENDS ${CMAKE_SOURCE_DIR}/info/wxmaxima.de.md)
