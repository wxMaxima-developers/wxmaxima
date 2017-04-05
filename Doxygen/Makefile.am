html: html_local
html_local: html/index.html

html/index.html latex/index.tex: FORCE index.html
	cat ${srcdir}/../Doxygen/Doxyfile >Doxyfile.real
	echo "INPUT = ${srcdir}/../src ${srcdir}/..">>Doxyfile.real
	echo "PROJECT_LOGO = ${srcdir}/../data/wxmaxima.png">>Doxyfile.real
	doxygen Doxyfile.real

index.html:
	echo "<head>">index.html
	echo "<meta http-equiv=\"refresh\" content=\"0; URL=html/index.xhtml\">">>index.html
	echo "</head>">>index.html


EXTRA_DIST = wxmaxima.png

pdf-am: html/index.html
	cd latex&&pdflatex refman.tex
	cd latex&&pdflatex refman.tex
	cd latex&&pdflatex refman.tex

FORCE:
