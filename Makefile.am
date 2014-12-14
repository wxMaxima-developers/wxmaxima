SUBDIRS = src locales data info

EXTRA_DIST = bootstrap wxmaxima.spec.in wxmaxima.desktop     \
             art/readme.txt art/license.txt                  \
             art/toolbar/configure.png art/toolbar/cut.png   \
             art/toolbar/input.png art/toolbar/print.png     \
             art/toolbar/stop.png art/toolbar/copy.png       \
             art/toolbar/help.png art/toolbar/open.png       \
             art/toolbar/save.png art/toolbar/text.png       \
             art/toolbar/playback-start.png                  \
             art/toolbar/playback-stop.png                   \
	     art/toolbar/paste.png                           \
	     art/toolbar/find.png                            \
	     art/toolbar/new.png                             \
	     art/config/options.png                          \
	     art/config/maxima.png                           \
	     art/config/styles.png                           \
             art/wxmac-doc-wxm.icns art/wxmac-doc-wxmx.icns  \
             art/wxmac-doc.icns art/wxmac.icns               \
             art/wxmac-doc.ico art/maximaicon.ico

docdir = ${datadir}/wxMaxima
doc_DATA = README COPYING art/config/options.png art/config/maxima.png \
	art/config/styles.png

LANGS = fr es it de pt_BR ru hu uk pl zh_TW da cs el ja ca gl zh_CN tr nb

wxMaxima.app: all
	mkdir -p wxMaxima.app/Contents/MacOS
	mkdir -p wxMaxima.app/Contents/Resources
	cp data/wxmathml.lisp wxMaxima.app/Contents/Resources
	cp data/wxmaxima.png wxMaxima.app/Contents/Resources
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

wxMaxima.win: all
	mkdir -p wxMaxima/art
	mkdir -p wxMaxima/data
	cp data/wxmathml.lisp wxMaxima/data
	cp data/wxmaxima.png wxMaxima/data
	cp data/autocomplete.txt wxMaxima/data
	cp data/tips*.txt wxMaxima/data
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
