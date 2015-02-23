html: html_local
html_local: html/index.html

html/index.html latex/index.tex: FORCE index.html
	doxygen Doxyfile

index.html:
	echo "<head>">index.html
	echo "<meta http-equiv=\"refresh\" content=\"0; URL=html/index.xhtml\">">>index.html
	echo "</head>">>index.html


EXTRA_DIST =

pdf-am:
	cd latex&&pdflatex refman.tex
	cd latex&&pdflatex refman.tex
	cd latex&&pdflatex refman.tex

FORCE:
