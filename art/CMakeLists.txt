add_subdirectory(statusbar)
add_subdirectory(config)
add_subdirectory(draw)
add_subdirectory(toolbar)
add_subdirectory(worksheet)

include("${CMAKE_SOURCE_DIR}/cmake-bin2h/bin2h.cmake")
bin2h(SOURCE_FILE "media-playback-start.svg.gz" HEADER_FILE "${CMAKE_BINARY_DIR}/wxm_media_playback_start_svg.h" VARIABLE_NAME "MEDIA_PLAYBACK_START_SVG_GZ" APPEND NULL_TERMINATE)
bin2h(SOURCE_FILE "media-playback-reverse.svg.gz" HEADER_FILE "${CMAKE_BINARY_DIR}/wxm_media_playback_reverse_svg.h" VARIABLE_NAME "MEDIA_PLAYBACK_REVERSE_SVG_GZ" APPEND NULL_TERMINATE)

