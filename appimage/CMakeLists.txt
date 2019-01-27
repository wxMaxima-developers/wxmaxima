
add_custom_target(appimage DEPENDS wxmaxima-x86_64.AppImage)

add_custom_command(OUTPUT wxmaxima-x86_64.AppImage
  COMMAND docker build -t wxmaxima .
  COMMAND docker run maxima cat wxmaxima-x86_64.AppImage > wxmaxima-x86_64.AppImage;chmod +x wxmaxima-x86_64.AppImage
  COMMAND chmod +x wxmaxima-x86_64.AppImage;chmod +x wxmaxima-x86_64.AppImage
 )
