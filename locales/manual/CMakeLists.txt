configure_file(${CMAKE_CURRENT_SOURCE_DIR}/po4a.cfg.in ${CMAKE_CURRENT_BINARY_DIR}/po4a.cfg)

find_program(PO4A po4a)
  
if(PO4A)
  message(STATUS "Updating the manual translations.")
  add_custom_command(
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMAND ${PO4A} -k 0 po4a.cfg
    OUTPUT en.po
    DEPENDS ${CMAKE_SOURCE_DIR}/info/wxmaxima.md)
  add_custom_target(TARGET update-locale
    COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_BINARY_DIR}/wxMaxima.pot" "${CMAKE_SOURCE_DIR}/locales/manual/wxmaxima.md.pot"
    COMMAND ${CMAKE_COMMAND} -E copy "*.po" "${CMAKE_SOURCE_DIR}/locales/manual"
    COMMAND ${CMAKE_COMMAND} -E copy "*.md" "${CMAKE_SOURCE_DIR}/locales/manual"
    COMMENT "Installing wxmaxima.md.pot and po-files in the source directory ${CMAKE_SOURCE_DIR}/locales/manual")
else()
  message(STATUS "po4a not found - translations of the manual won't be updated")
endif()
