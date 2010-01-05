SUBDIRS = src locales data

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
             art/wxmac-doc-wxm.icns art/wxmac-doc-wxmx.icns  \
             art/wxmac-doc.icns art/wxmac.icns               \
             art/maximaicon.ico

docdir = ${datadir}/wxMaxima
doc_DATA = README COPYING

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
	mkdir -p wxMaxima.app/Contents/Resources/locale/es/LC_MESSAGES
	mkdir -p wxMaxima.app/Contents/Resources/locale/fr/LC_MESSAGES
	mkdir -p wxMaxima.app/Contents/Resources/locale/it/LC_MESSAGES
	mkdir -p wxMaxima.app/Contents/Resources/locale/de/LC_MESSAGES
	mkdir -p wxMaxima.app/Contents/Resources/locale/pt_BR/LC_MESSAGES
	mkdir -p wxMaxima.app/Contents/Resources/locale/ru/LC_MESSAGES
	mkdir -p wxMaxima.app/Contents/Resources/locale/hu/LC_MESSAGES
	mkdir -p wxMaxima.app/Contents/Resources/locale/uk/LC_MESSAGES
	mkdir -p wxMaxima.app/Contents/Resources/locale/pl/LC_MESSAGES
	mkdir -p wxMaxima.app/Contents/Resources/locale/zh_TW/LC_MESSAGES
	mkdir -p wxMaxima.app/Contents/Resources/locale/da/LC_MESSAGES
	mkdir -p wxMaxima.app/Contents/Resources/locale/cs/LC_MESSAGES
	mkdir -p wxMaxima.app/Contents/Resources/locale/el/LC_MESSAGES
	mkdir -p wxMaxima.app/Contents/Resources/toolbar
	cp locales/es.mo wxMaxima.app/Contents/Resources/locale/es/LC_MESSAGES/wxMaxima.mo
	cp locales/fr.mo wxMaxima.app/Contents/Resources/locale/fr/LC_MESSAGES/wxMaxima.mo
	cp locales/it.mo wxMaxima.app/Contents/Resources/locale/it/LC_MESSAGES/wxMaxima.mo
	cp locales/de.mo wxMaxima.app/Contents/Resources/locale/de/LC_MESSAGES/wxMaxima.mo
	cp locales/pt_BR.mo wxMaxima.app/Contents/Resources/locale/pt_BR/LC_MESSAGES/wxMaxima.mo
	cp locales/ru.mo wxMaxima.app/Contents/Resources/locale/ru/LC_MESSAGES/wxMaxima.mo
	cp locales/hu.mo wxMaxima.app/Contents/Resources/locale/hu/LC_MESSAGES/wxMaxima.mo
	cp locales/uk.mo wxMaxima.app/Contents/Resources/locale/uk/LC_MESSAGES/wxMaxima.mo
	cp locales/pl.mo wxMaxima.app/Contents/Resources/locale/pl/LC_MESSAGES/wxMaxima.mo
	cp locales/zh_TW.mo wxMaxima.app/Contents/Resources/locale/zh_TW/LC_MESSAGES/wxMaxima.mo
	cp locales/da.mo wxMaxima.app/Contents/Resources/locale/da/LC_MESSAGES/wxMaxima.mo
	cp locales/cs.mo wxMaxima.app/Contents/Resources/locale/cs/LC_MESSAGES/wxMaxima.mo
	cp locales/el.mo wxMaxima.app/Contents/Resources/locale/el/LC_MESSAGES/wxMaxima.mo
	cp locales/wxwin/es.mo wxMaxima.app/Contents/Resources/locale/es/LC_MESSAGES/wxMaxima-wxstd.mo
	cp locales/wxwin/fr.mo wxMaxima.app/Contents/Resources/locale/fr/LC_MESSAGES/wxMaxima-wxstd.mo
	cp locales/wxwin/it.mo wxMaxima.app/Contents/Resources/locale/it/LC_MESSAGES/wxMaxima-wxstd.mo
	cp locales/wxwin/de.mo wxMaxima.app/Contents/Resources/locale/de/LC_MESSAGES/wxMaxima-wxstd.mo
	cp locales/wxwin/pt_BR.mo wxMaxima.app/Contents/Resources/locale/pt_BR/LC_MESSAGES/wxMaxima-wxstd.mo
	cp locales/wxwin/ru.mo wxMaxima.app/Contents/Resources/locale/ru/LC_MESSAGES/wxMaxima-wxstd.mo
	cp locales/wxwin/hu.mo wxMaxima.app/Contents/Resources/locale/hu/LC_MESSAGES/wxMaxima-wxstd.mo
	cp locales/wxwin/uk.mo wxMaxima.app/Contents/Resources/locale/uk/LC_MESSAGES/wxMaxima-wxstd.mo
	cp locales/wxwin/pl.mo wxMaxima.app/Contents/Resources/locale/pl/LC_MESSAGES/wxMaxima-wxstd.mo
	cp locales/wxwin/zh_TW.mo wxMaxima.app/Contents/Resources/locale/zh_TW/LC_MESSAGES/wxMaxima-wxstd.mo
	cp locales/wxwin/da.mo wxMaxima.app/Contents/Resources/locale/da/LC_MESSAGES/wxMaxima-wxstd.mo
	cp locales/wxwin/cs.mo wxMaxima.app/Contents/Resources/locale/cs/LC_MESSAGES/wxMaxima-wxstd.mo
	cp locales/wxwin/el.mo wxMaxima.app/Contents/Resources/locale/el/LC_MESSAGES/wxMaxima-wxstd.mo
	cp art/toolbar/*.png wxMaxima.app/Contents/Resources/toolbar

