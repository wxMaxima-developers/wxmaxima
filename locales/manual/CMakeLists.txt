configure_file(${CMAKE_CURRENT_SOURCE_DIR}/po4a.cfg.in ${CMAKE_CURRENT_BINARY_DIR}/po4a.cfg)

find_program(PO4A po4a)

if(PO4A)
  execute_process(
    COMMAND ${po4a} po4a.cfg)
  foreach(POFILE ${TRANSLATION_FILES})
    string(REGEX REPLACE ".*locales/(.*).po$" "\\1" LANG ${POFILE})
    execute_process(COMMAND po4a-updatepo --master-charset utf8 -f text -m ${CMAKE_SOURCE_DIR}/info/wxmaxima.md -p ${POFILE} )
    execute_process(COMMAND po4a-translate --master-charset utf8 -f text -m ${CMAKE_SOURCE_DIR}/info/wxmaxima.md -p ${POFILE} ${CMAKE_SOURCE_DIR}/info/wxmaxima.${LANG}.md )
  endforeach()
endif()
