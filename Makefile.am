SUBDIRS = src locales data info test Doxygen

EXTRA_DIST = bootstrap wxmaxima.spec.in     \
             art/readme.txt art/license.txt                  \
             art/toolbar/gtk-preferences.png art/toolbar/gtk-cut.png   \
             art/toolbar/input.png art/toolbar/gtk-print.png     \
             art/toolbar/gtk-stop.png art/toolbar/gtk-copy.png       \
             art/toolbar/gtk-help.png art/toolbar/gtk-open.png   \
             art/toolbar/gtk-save.png art/toolbar/text.png       \
             art/toolbar/media-playback-start.png            \
	     art/toolbar/gtk-select-all.png		     \
	     art/toolbar/view-refresh.png		     \
             art/toolbar/media-playback-stop.png             \
	     art/toolbar/gtk-paste.png                       \
	     art/toolbar/gtk-find.png                        \
	     art/toolbar/gtk-new.png                         \
	     art/toolbar/weather-clear.png                   \
	     art/toolbar/software-update-urgent.png          \
	     art/config/options.png                          \
	     art/config/maxima.png                           \
	     art/config/styles.png                           \
	     art/config/editing.png                          \
             art/wxmac-doc-wxm.icns art/wxmac-doc-wxmx.icns  \
             art/wxmac-doc.icns art/wxmac.icns               \
             art/wxmac-doc.ico art/maximaicon.ico

docdir = ${datadir}/wxMaxima
doc_DATA = README README.md COPYING art/config/options.png art/config/maxima.png \
	art/config/styles.png art/config/editing.png

LANGS = fr es it de pt_BR ru hu uk pl zh_TW da cs el ja ca gl zh_CN tr nb

wxMaxima.app: all
	mkdir -p wxMaxima.app/Contents/MacOS
	mkdir -p wxMaxima.app/Contents/Resources
	cp data/wxmathml.lisp wxMaxima.app/Contents/Resources
	cp data/wxmaxima.png wxMaxima.app/Contents/Resources
	cp data/wxmaxima.svg wxMaxima.app/Contents/Resources
	cp data/autocomplete.txt wxMaxima.app/Contents/Resources
	cp data/tips*.txt wxMaxima.app/Contents/Resources
	cp art/wxmac.icns wxMaxima.app/Contents/Resources
	cp art/wxmac-doc.icns wxMaxima.app/Contents/Resources
	cp art/wxmac-doc-wxm.icns wxMaxima.app/Contents/Resources
	cp art/wxmac-doc-wxmx.icns wxMaxima.app/Contents/Resources
	cp data/Info.plist wxMaxima.app/Contents
	cp data/PkgInfo wxMaxima.app/Contents
	cp src/wxmaxima wxMaxima.app/Contents/MacOS
	for i in $(LANGS) ; do \
		mkdir -p wxMaxima.app/Contents/Resources/locale/$$i/LC_MESSAGES ; \
		cp locales/$$i.mo wxMaxima.app/Contents/Resources/locale/$$i/LC_MESSAGES/wxMaxima.mo ; \
		cp locales/wxwin/$$i.mo wxMaxima.app/Contents/Resources/locale/$$i/LC_MESSAGES/wxMaxima-wxstd.mo ; \
	done
	mkdir -p wxMaxima.app/Contents/Resources/toolbar
	mkdir -p wxMaxima.app/Contents/Resources/config
	cp art/toolbar/*.png wxMaxima.app/Contents/Resources/toolbar
	cp art/config/*.png wxMaxima.app/Contents/Resources/config
	mkdir -p wxMaxima.app/Contents/Resources/help
	cp info/*.html wxMaxima.app/Contents/Resources/help
	cp info/*.jpg  wxMaxima.app/Contents/Resources/help
	cp info/*.png  wxMaxima.app/Contents/Resources/help
	cp info/index.hhk wxMaxima.app/Contents/Resources/help
	cp info/contents.hhc wxMaxima.app/Contents/Resources/help
	cp info/wxmaxima.hhp wxMaxima.app/Contents/Resources/help

wxMaxima.win: all
	mkdir -p wxMaxima/art
	mkdir -p wxMaxima/data
	cp data/wxmathml.lisp wxMaxima/data
	cp data/wxmaxima.png wxMaxima/data
	cp data/wxmaxima.svg wxMaxima/data
	cp data/autocomplete.txt wxMaxima/data
	cp data/tips*.txt wxMaxima/data
	mkdir -p wxMaxima/help
	cp info/*.jpg  wxMaxima/help
	cp info/*.png  wxMaxima/help
if CHM
	cp info/*.chm  wxMaxima/help
else
	-cp info/*.html wxMaxima/help
	-cp info/index.hhk wxMaxima/help
	-cp info/contents.hhc wxMaxima/help
	-cp info/wxmaxima.hhp wxMaxima/help
endif
	mkdir -p wxMaxima/art/toolbar
	cp art/toolbar/*.png wxMaxima/art/toolbar
	mkdir -p wxMaxima/art/config
	cp art/config/*.png wxMaxima/art/config
	for i in $(LANGS) ; do \
		mkdir -p wxMaxima/locale/$$i/LC_MESSAGES ; \
		cp locales/$$i.mo wxMaxima/locale/$$i/LC_MESSAGES/wxMaxima.mo ; \
		cp locales/wxwin/$$i.mo wxMaxima/locale/$$i/LC_MESSAGES/wxMaxima-wxstd.mo ; \
	done
	cp src/wxmaxima.exe wxMaxima/

allmo:
	    cd locales&&$(MAKE) allmo

Doxygen: FORCE
	    cd Doxygen&&$(MAKE)

.PHONY:	allmo FORCE
FORCE:
