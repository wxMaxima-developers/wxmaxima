SUBDIRS = src locales data

EXTRA_DIST = bootstrap wxmaxima.spec.in wxmaxima.desktop		\
             maxima-new.png art/ok.xpm art/multiline.xpm		\
             art/readme.txt art/license.txt art/big/copy.xpm		\
             art/big/cut.xpm art/big/help.xpm art/big/open.xpm		\
             art/big/pref.xpm art/big/print.xpm art/big/stop.xpm	\
             art/big/save.xpm art/small/copy.xpm art/small/cut.xpm	\
             art/small/help.xpm art/small/open.xpm art/small/pref.xpm	\
             art/small/print.xpm art/small/stop.xpm art/small/save.xpm	\
             art/icons.h art/small/icons.h art/big/icons.h		\
             art/maximaicon.xpm art/maximaicon.ico                      \
             art/wxmac.icns

docdir = ${datadir}/wxMaxima
doc_DATA = README COPYING


wxMaxima.app:
	mkdir -p wxMaxima.app/Contents/MacOS
	mkdir -p wxMaxima.app/Contents/Resources
	cp data/docs.zip wxMaxima.app/Contents/Resources
	cp data/intro.zip wxMaxima.app/Contents/Resources
	cp data/tips.txt wxMaxima.app/Contents/Resources
	cp data/wxmathml.lisp wxMaxima.app/Contents/Resources
	cp art/wxmac.icns wxMaxima.app/Contents/Resources
	cp data/Info.plist wxMaxima.app/Contents
	cp data/PkgInfo wxMaxima.app/Contents
	cp src/wxmaxima wxMaxima.app/Contents/MacOS
	mkdir -p wxMaxima.app/Contents/Resources/locale/es/LC_MESSAGES
	mkdir -p wxMaxima.app/Contents/Resources/locale/fr/LC_MESSAGES
	mkdir -p wxMaxima.app/Contents/Resources/locale/it/LC_MESSAGES
	cp locales/es.mo wxMaxima.app/Contents/Resources/locale/es/LC_MESSAGES/wxMaxima.mo
	cp locales/fr.mo wxMaxima.app/Contents/Resources/locale/fr/LC_MESSAGES/wxMaxima.mo
	cp locales/it.mo wxMaxima.app/Contents/Resources/locale/it/LC_MESSAGES/wxMaxima.mo
	cp locales/wxwin/es.mo wxMaxima.app/Contents/Resources/locale/es/LC_MESSAGES/wxMaxima-wxstd.mo
	cp locales/wxwin/fr.mo wxMaxima.app/Contents/Resources/locale/fr/LC_MESSAGES/wxMaxima-wxstd.mo
	cp locales/wxwin/it.mo wxMaxima.app/Contents/Resources/locale/it/LC_MESSAGES/wxMaxima-wxstd.mo