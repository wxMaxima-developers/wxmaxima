install(DIRECTORY . DESTINATION share/doc/wxaaxima
        FILES_MATCHING PATTERN *.html
            PATTERN *.jpg
            PATTERN *.png)

set(EXTRADOCFILES
        index.hhk
        contents.hhc)

install(FILES ${EXTRADOCFILES} DESTINATION share/doc/wxmaxima)
