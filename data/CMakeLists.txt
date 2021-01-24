# -*- mode: CMake; cmake-tab-width: 4; -*-


# prepares a include file, which can be included in a string, e.g.
# const char License_text[] =
# #include "char-License.h"
# ;
# the length of the content ist written as lengthvariable to the length_output_file
function(make_includable input_file output_file length_output_file lengthvariable)
    file(READ ${input_file} content)
    string(LENGTH "${content}" contentlength)
    file(APPEND "${length_output_file}" "#define ${lengthvariable} ${contentlength}\n")
    set(delim "for_c++_include")
    set(content "R\"${delim}(\n${content})${delim}\"")
    file(WRITE ${output_file} "${content}")
endfunction(make_includable)

file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/arraylength.h" "#ifndef ARRAYLENGTH_H\n#define ARRAYLENGTH_H\n")
make_includable("${CMAKE_CURRENT_SOURCE_DIR}/UnicodeData.txt" "${CMAKE_CURRENT_BINARY_DIR}/char-UnicodeData.h" "${CMAKE_CURRENT_BINARY_DIR}/arraylength.h" "UnicodeData_txt_len")
make_includable("${CMAKE_CURRENT_SOURCE_DIR}/wxMathML.lisp" "${CMAKE_CURRENT_BINARY_DIR}/char-wxMathML.h" "${CMAKE_CURRENT_BINARY_DIR}/arraylength.h" "wxMathML_lisp_len")
make_includable("${CMAKE_CURRENT_SOURCE_DIR}/manual_anchors.xml" "${CMAKE_CURRENT_BINARY_DIR}/char-manual_anchors.xml.h" "${CMAKE_CURRENT_BINARY_DIR}/arraylength.h" "manual_anchors_xml_len")
make_includable("${CMAKE_SOURCE_DIR}/GPL.txt" "${CMAKE_CURRENT_BINARY_DIR}/char-License.h" "${CMAKE_CURRENT_BINARY_DIR}/arraylength.h" "License_len")
file(APPEND "${CMAKE_CURRENT_BINARY_DIR}/arraylength.h" "#endif // ARRAYLENGTH_H\n")
set(DATAFILES
        ${CMAKE_CURRENT_SOURCE_DIR}/io.github.wxmaxima_developers.wxMaxima.png
        ${CMAKE_CURRENT_SOURCE_DIR}/io.github.wxmaxima_developers.wxMaxima.svg)

set(PIXMAPS
        ${CMAKE_CURRENT_SOURCE_DIR}/text-x-wxmathml.svg
        ${CMAKE_CURRENT_SOURCE_DIR}/text-x-wxmaxima-batch.svg
        ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima-16.xpm
        ${CMAKE_CURRENT_SOURCE_DIR}/io.github.wxmaxima_developers.wxMaxima.png
        ${CMAKE_CURRENT_SOURCE_DIR}/io.github.wxmaxima_developers.wxMaxima.svg
        ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima-32.xpm)

if(WIN32)
    add_subdirectory(fonts)
else()
    string(TIMESTAMP CURRENTDATE "%Y-%m-%d")
    # the config file is ~/.wxMaxima if wxWidgets < 3.1.1
    #                    ~/.config/wxMaxima.conf if wxWidgets >= 3.1.1 (XDG Specification)
    if(wxWidgets_VERSION_STRING VERSION_GREATER_EQUAL "3.1.1")
        set(WXMAXIMA_CONFIGFILE_PATH "~/.config/wxMaxima.conf")
    else()
        set(WXMAXIMA_CONFIGFILE_PATH "~/.wxMaxima")
    endif()
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.1.in ${CMAKE_CURRENT_BINARY_DIR}/wxmaxima.1)
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/de/wxmaxima.1.in ${CMAKE_CURRENT_BINARY_DIR}/de/wxmaxima.1)
    install(FILES ${PIXMAPS}                                                                     DESTINATION share/pixmaps)
    install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima                                           DESTINATION share/bash-completion/completions)
    install(FILES ${CMAKE_CURRENT_BINARY_DIR}/wxmaxima.1                                         DESTINATION share/man/man1)
    install(FILES ${CMAKE_CURRENT_BINARY_DIR}/de/wxmaxima.1                                      DESTINATION share/man/de/man1)
    install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/io.github.wxmaxima_developers.wxMaxima.appdata.xml DESTINATION share/metainfo)
    install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/io.github.wxmaxima_developers.wxMaxima.desktop     DESTINATION share/applications)
    install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/x-wxmathml.xml                                     DESTINATION share/mime/packages)
    install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/x-wxmaxima-batch.xml                               DESTINATION share/mime/packages)
endif()
install(FILES ${DATAFILES} DESTINATION share/wxMaxima)

# allow local execution (./wxmaxima-local) from the build directory without installation
if(UNIX)
    file(COPY ${DATAFILES} DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
endif()
