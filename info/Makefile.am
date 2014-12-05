SUBDIRS = $(DE_DIR)

info_TEXINFOS = wxmaxima.texi
wxmaxima_TEXINFOS = wxmaxima.texi

figurefiles = figures/NoiseFilter.png figures/maxima.png figures/InputCell.png
figurefiles +=figures/wxMaximaWindow.png

pdffigures=figures/NoiseFilter.pdf figures/maxima.pdf figures/InputCell.pdf
pdffigures+=figures/wxMaximaWindow.pdf
.png.ps:
	pngtopnm $<|pnmtops -noturn >$@

.ps.pdf:
	gs -sOutputFile=$@ -sDEVICE=pdfwrite -dNOPAUSE $< -c quit 

if CHM
WXMAXIMA_CHM = wxmaxima.chm
INSTALL_CHM = install-chm
UNINSTALL_CHM = uninstall-chm
CLEAN_CHM = clean-chm
endif

all-local: wxmaxima.info $(WXMAXIMA_CHM)

wxmaxima.info : wxmaxima.texi
	makeinfo --split-size=1000000 $<

html: wxmaxima.html contents.hhc

wxmaxima.html: wxmaxima.texi $(wxmaxima_TEXINFOS)
	sh extract_categories.sh wxmaxima

wxmaxima.pdf: wxmaxima.texi $(wxmaxima_TEXINFOS) $(pdffigures) $(figurefiles)
	$(TEXI2PDF) $(AM_V_texinfo) -o wxmaxima.pdf wxmaxima.texi
	-rm wxmaxima.tp
	-rm wxmaxima.pg
	-rm wxmaxima.ky
	-rm wxmaxima.cp
	-rm wxmaxima.toc
	-rm wxmaxima.fn
	-rm wxmaxima.aux
	-rm wxmaxima.log

contents.hhc index.hhk: wxmaxima.html
	perl ./create_index

# Squash error on exit from hhc
wxmaxima.chm: wxmaxima.hhp contents.hhc index.hhk
	-"$(HHC)" wxmaxima.hhp

htmldir = $(dochtmldir)
html_DATA = $(figurefiles) contents.hhc index.hhk wxmaxima.hhp wxmaxima*.html

all-local: warn_texinfo
dist-hook: check_texinfo html

install-data-local: $(INSTALL_CHM)
uninstall-local: $(UNINSTALL_CHM)


clean-local: clean-info clean-html $(CLEAN_CHM)

clean-info:
	rm -f wxmaxima.info*

clean-html:
	rm -f wxmaxima*.html
	rm -f contents.hhc
	rm -f index.hhk
	rm -f wxmaxima.chm

if CHM
install-chm: wxmaxima.chm
	test -z "$(DESTDIR)$(docchmdir)" || mkdir -p -- "$(DESTDIR)$(docchmdir)"
	$(INSTALL_DATA) wxmaxima.chm "$(DESTDIR)$(docchmdir)/wxmaxima.chm"

uninstall-chm:
	rm -f "$(DESTDIR)$(docchmdir)"

clean-chm:
	rm -f wxmaxima.chm
	rm -rf chm
endif


EXTRA_DIST = texi2html manual.css texi2html.init \
    $(genericdirDATA) $(pdffigures) \
    check_tab fix_tab build_index.pl \
    extract_categories1.awk  extract_categories1.sed  extract_categories.sh

check_texinfo:
	pattern=`printf "\t"` ; \
	bad_files=`find . -name '*.texi' -print | xargs $(EGREP) -l -e "$$pattern"` ; \
	[ -z  "$$bad_files" ] || ( echo "ERROR: The following files have unexpanded Tabs: $$bad_files" ; \
	echo "Run make distclean and then /doc/info/fix_tab to fix the problem." ; exit 1 )

warn_texinfo:
	pattern=`printf "\t"` ; \
	bad_files=`find . -name '*.texi' -print | xargs $(EGREP) -l -e "$$pattern"` ; \
	[ -z  "$$bad_files" ] || ( echo "WARNING: The following files have unexpanded Tabs: $$bad_files" ; \
	echo "Run /doc/info/fix_tab to fix the problem." )

.PHONY: check_texinfo warn_texinfo

