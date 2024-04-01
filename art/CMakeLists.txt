add_subdirectory(statusbar)
add_subdirectory(config)
add_subdirectory(draw)
add_subdirectory(toolbar)
add_subdirectory(worksheet)

include("${CMAKE_SOURCE_DIR}/cmake-bin2h/bin2h.cmake")
file(WRITE "${CMAKE_BINARY_DIR}/wxm_media_playback_start_svg.h.tmp"
    "#ifndef WXM_MEDIA_PLAYBACK_START_SVG_H\n"
    "#define WXM_MEDIA_PLAYBACK_START_SVG_H\n")

bin2h(SOURCE_FILE "media-playback-start.svg.gz" HEADER_FILE "${CMAKE_BINARY_DIR}/wxm_media_playback_start_svg.h.tmp" VARIABLE_NAME "MEDIA_PLAYBACK_START_SVG_GZ" APPEND NULL_TERMINATE)
file(APPEND "${CMAKE_BINARY_DIR}/wxm_media_playback_start_svg.h.tmp" "\n#endif\n")
# write to a *.tmp file first and then 'copy_if_different' prevents rebuilding source files, if nothing (but a timestamp) changed.
execute_process(COMMAND ${CMAKE_COMMAND} -E copy_if_different "${CMAKE_BINARY_DIR}/wxm_media_playback_start_svg.h.tmp" "${CMAKE_BINARY_DIR}/wxm_media_playback_start_svg.h")

file(WRITE "${CMAKE_BINARY_DIR}/wxm_media_playback_reverse_svg.h.tmp"
    "#ifndef WXM_MEDIA_PLAYBACK_REVERSE_SVG_H\n"
    "#define WXM_MEDIA_PLAYBACK_REVERSE_SVG_H\n")
bin2h(SOURCE_FILE "media-playback-reverse.svg.gz" HEADER_FILE "${CMAKE_BINARY_DIR}/wxm_media_playback_reverse_svg.h.tmp" VARIABLE_NAME "MEDIA_PLAYBACK_REVERSE_SVG_GZ" APPEND NULL_TERMINATE)
file(APPEND "${CMAKE_BINARY_DIR}/wxm_media_playback_reverse_svg.h.tmp" "\n#endif\n")
# write to a *.tmp file first and then 'copy_if_different' prevents rebuilding source files, if nothing (but a timestamp) changed.
execute_process(COMMAND ${CMAKE_COMMAND} -E copy_if_different "${CMAKE_BINARY_DIR}/wxm_media_playback_reverse_svg.h.tmp" "${CMAKE_BINARY_DIR}/wxm_media_playback_reverse_svg.h")
