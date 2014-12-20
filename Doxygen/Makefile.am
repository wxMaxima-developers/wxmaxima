html: html_local
html_local: html/index.html

html/index.html: FORCE index.html
	doxygen Doxyfile

index.html:
	echo "<head>">index.html
	echo "<meta http-equiv=\"refresh\" content=\"0; URL=html/index.xhtml\">">>index.html
	echo "</head>">>index.html


all-am: html

FORCE:
