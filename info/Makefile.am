SUBDIRS = $(DE_DIR)

info_TEXINFOS = wxmaxima.texi
wxmaxima_TEXINFOS = wxmaxima.texi

figurefiles =  NoiseFilter.jpg maxima_screenshot.jpg InputCell.jpg
figurefiles += wxMaximaWindow.jpg wxMaximaLogo.jpg SidePanes.jpg ezUnits.jpg
figurefiles += BatchImage.jpg wxMaxima_configuration_001.jpg wxMaxima_configuration_002.jpg

# install-data-local:install-html

pdffigures =  NoiseFilter.pdf maxima_screenshot.pdf InputCell.pdf
pdffigures += wxMaximaWindow.pdf wxMaximaLogo.pdf SidePanes.pdf ezUnits.pdf
pdffigures += BatchImage.pdf wxMaxima_configuration_001.pdf wxMaxima_configuration_002.pdf

# %.jpg: %.png
# 	pngtopnm $<|cjpeg > $@

# %.eps: %.png
# 	cat $<|pngtopnm  -background white -mix|ppmtopgm|pnmtops -scale .125 -noturn -rle> $@

# %.pdf:	%.eps
# 	gs -dNOPAUSE -dNOCACHE -dBATCH -sDEVICE=eps2write -sPAPERSIZE=a0 -sOutputFile=$<.eps $< -c q

if CHM
WXMAXIMA_CHM = wxMaxima.chm
INSTALL_CHM = install-chm
UNINSTALL_CHM = uninstall-chm
CLEAN_CHM = clean-chm
endif

all-local: wxmaxima.info $(WXMAXIMA_CHM)

AM_MAKEINFOFLAGS = --no-split --css-include=manual.css

html_DATA = $(figurefiles) wxmaxima.hhp wxmaxima.html

dist-hook: html

install-data-local: $(INSTALL_CHM)
uninstall-local: $(UNINSTALL_CHM)


clean-local: clean-info clean-html $(CLEAN_CHM)

clean-info:
	rm -f wxmaxima.info*

clean-html:
	rm -f wxmaxima.html

if CHM
install-chm: wxMaxima.chm
	test -z "$(DESTDIR)$(docchmdir)" || mkdir -p -- "$(DESTDIR)$(docchmdir)"
	$(INSTALL_DATA) $< "$(DESTDIR)$(docchmdir)/wxMaxima.chm"

uninstall-chm:
	rm -f "$(DESTDIR)$(docchmdir)"

clean-chm:
	rm -f wxMaxima.chm
	rm -rf chm
endif


EXTRA_DIST = manual.css $(html_DATA)\
    $(genericdirDATA) $(pdffigures) \
    wxmaximaicon.ico \
    wxmaxima.info \
    $(html_DATA)

.chm:.hhp
	-"$(HHC)" $<
