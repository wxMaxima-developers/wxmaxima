configure_file(wxmathml.lisp.in wxmathml.lisp)

if (APPLE)
    configure_file(Info.plist.in Info.plist)
endif()

set(DATAFILES
        tips.txt
        wxmathml.lisp
        wxmaxima.png
        wxmaxima.svg
        autocomplete.txt)

install(FILES ${DATAFILES}         DESTINATION share/wxmaxima)

set(PIXMAPS
        text-x-wxmathml.svg
        text-x-wxmaxima-batch.svg)

install(FILES ${PIXMAPS}           DESTINATION share/wxmaxima/pixmaps)

install(FILES wxmaxima.1           DESTINATION share/man/man1)
install(FILES wxmaxima-16.xpm      DESTINATION share/pixmaps)
install(FILES wxmaxima-32.xpm      DESTINATION share/pixmaps)
install(FILES wxMaxima.appdata.xml DESTINATION share/appdata)
install(FILES wxMaxima.desktop     DESTINATION share/applications)
install(FILES x-wxmathml.xml       DESTINATION share/mime/packages)
install(FILES x-wxmaxima-batch.xml DESTINATION share/mime/packages)
