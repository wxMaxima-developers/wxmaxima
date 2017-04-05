add_custom_target(html)
add_custom_command(TARGET html PRE_BUILD
  COMMAND cat ${CMAKE_SOURCE_DIR}/Doxygen/Doxyfile >Doxyfile.real)
add_custom_command(TARGET html PRE_BUILD
  COMMAND echo INPUT = ${CMAKE_SOURCE_DIR}/src ${CMAKE_SOURCE_DIR}>>Doxyfile.real)
add_custom_command(TARGET html PRE_BUILD
  COMMAND echo PROJECT_LOGO = ${CMAKE_SOURCE_DIR}/data/wxmaxima.png>>Doxyfile.real)
add_custom_command(TARGET html
  COMMAND doxygen Doxyfile.real)
add_custom_command(TARGET html POST_BUILD
  COMMAND rm Doxyfile.real)
