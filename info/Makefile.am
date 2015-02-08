SUBDIRS = $(DE_DIR)

info_TEXINFOS = wxmaxima.texi
wxmaxima_TEXINFOS = wxmaxima.texi

figurefiles =  NoiseFilter.jpg maxima_screenshot.jpg InputCell.jpg
figurefiles += wxMaximaWindow.jpg wxMaximaLogo.jpg SidePanes.jpg

# install-data-local:install-html

pdffigures=NoiseFilter.pdf maxima_screenshot.pdf InputCell.pdf wxMaximaLogo.pdf
pdffigures+=wxMaximaWindow.pdf SidePanes.pdf

if CHM
WXMAXIMA_CHM = wxMaxima.chm
INSTALL_CHM = install-chm
UNINSTALL_CHM = uninstall-chm
CLEAN_CHM = clean-chm
endif

all-local: wxmaxima.info $(WXMAXIMA_CHM)

wxmaxima.info : wxmaxima.texi
	makeinfo --split-size=1000000 $<

wxmaxima.html: wxmaxima.texi $(wxmaxima_TEXINFOS) manual.css texi2html.init
	texi2html -split_chapter --lang=en --output=. \
          --css-include="manual.css" --init-file "texi2html.init" wxmaxima.texi

wxmaxima_1.html: wxmaxima.html
wxmaxima_2.html: wxmaxima.html
wxmaxima_3.html: wxmaxima.html
wxmaxima_4.html: wxmaxima.html

wxmaxima.pdf: wxmaxima.texi $(wxmaxima_TEXINFOS) $(pdffigures) $(figurefiles)
	$(TEXI2PDF) $(AM_V_texinfo) -o wxmaxima.pdf wxmaxima.texi

contents.hhc index.hhk: wxmaxima.html
	perl ./create_index

# Squash error on exit from hhc
wxMaxima.chm: wxmaxima.hhp contents.hhc index.hhk
	-"$(HHC)" wxmaxima.hhp

html_DATA = $(figurefiles) contents.hhc index.hhk wxmaxima.hhp wxmaxima.html
html_DATA+= wxmaxima_1.html wxmaxima_2.html wxmaxima_3.html wxmaxima_4.html wxmaxima_5.html
html_DATA+= wxmaxima_abt.html wxmaxima_ovr.html wxmaxima_toc.html
#dist_html_data=$(html_DATA)

wxmaxima_abt.html: wxmaxima.html
wxmaxima_ovr.html: wxmaxima.html
wxmaxima_toc.html: wxmaxima.html

dist-hook: html

install-data-local: $(INSTALL_CHM)
uninstall-local: $(UNINSTALL_CHM)


clean-local: clean-info clean-html $(CLEAN_CHM)

clean-info:
	rm -f wxmaxima.info*

clean-html:
	rm -f wxmaxima*.html
	rm -f contents.hhc
	rm -f index.hhk

if CHM
install-chm: wxMaxima.chm
	test -z "$(DESTDIR)$(docchmdir)" || mkdir -p -- "$(DESTDIR)$(docchmdir)"
	$(INSTALL_DATA) wxMaxima.chm "$(DESTDIR)$(docchmdir)/wxMaxima.chm"

uninstall-chm:
	rm -f "$(DESTDIR)$(docchmdir)"

clean-chm:
	rm -f wxMaxima.chm
	rm -rf chm
endif


EXTRA_DIST = texi2html manual.css texi2html.init $(html_DATA)\
    $(genericdirDATA) $(pdffigures) \
    wxmaximaicon.ico \
    wxmaxima.info wxmaxima.hhc wxMaxima.chm

