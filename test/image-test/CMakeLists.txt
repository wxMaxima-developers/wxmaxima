# -*- mode: CMake; cmake-tab-width: 4; -*-

set(IMAGE_FILES
330px-Maxima.svg.bmp
330px-Maxima.svg.gif
330px-Maxima.svg.jpg
330px-Maxima.svg.png
330px-Maxima.svg.pnm
330px-Maxima.svg.tif
330px-Maxima.svg.webp
330px-Maxima.svg.xpm
Maxima_pathonly.svg
Maxima_pathonly.svg.gz
Maxima.svg
Maxima.svgz
)

file(
    COPY
    ${IMAGE_FILES}
    DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    FILE_PERMISSIONS OWNER_READ
    )

file(
    COPY
    image-test.wxmx
    DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    FILE_PERMISSIONS OWNER_WRITE OWNER_READ
)
