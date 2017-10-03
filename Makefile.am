SUBDIRS = src locales data info test Doxygen art

DISTCLEAN_FILES = $(distdir).tar.gz.asc
EXTRA_DIST = README.md bootstrap wxmaxima.spec.in     \
             art/readme.txt art/license.txt                  \
             art/toolbar/gtk-preferences.png art/toolbar/gtk-cut.png   \
             art/toolbar/input.png art/toolbar/gtk-print.png     \
             art/toolbar/gtk-stop.png art/toolbar/gtk-copy.png       \
             art/toolbar/gtk-help.png art/toolbar/gtk-open.png   \
             art/toolbar/gtk-save.png art/toolbar/text.png   \
             art/toolbar/media-playback-start.png            \
             art/toolbar/media-playback-startstop.png        \
	     art/toolbar/go-bottom.png			     \
	     art/toolbar/gtk-select-all.png		     \
	     art/toolbar/view-refresh.png		     \
             art/toolbar/media-playback-stop.png             \
	     art/toolbar/gtk-paste.png                       \
	     art/toolbar/gtk-find.png                        \
	     art/toolbar/gtk-new.png                         \
	     art/toolbar/weather-clear.png                   \
	     art/toolbar/weather-few-clouds.png              \
	     art/toolbar/software-update-urgent.png          \
	     art/config/media-playback-start.png	     \
	     art/config/options.png                          \
	     art/config/maxima.png                           \
	     art/config/styles.png                           \
	     art/config/editing.png                          \
	     art/config/Document-export.png		     \
	     art/statusbar/network-error.png		     \
	     art/statusbar/network-transmit.png		     \
	     art/statusbar/network-idle.png		     \
	     art/statusbar/network-offline.png		     \
	     art/statusbar/network-receive.png		     \
	     art/statusbar/network-transmit.png		     \
	     art/statusbar/network-transmit-receive.png	     \
             art/wxmac-doc-wxm.icns art/wxmac-doc-wxmx.icns  \
             art/wxmac-doc.icns art/wxmac.icns               \
	     art/wxmac-doc.png				     \
	     art/toolbar/showCurrentCell.png       	     \
	     art/toolbar/dialog-information.png

docdir = ${datadir}/wxMaxima
doc_DATA = README README.md COPYING art/config/options.png art/config/maxima.png \
	art/config/styles.png art/config/editing.png art/config/Document-export.png


wxLANGS =  fr es it de pt_BR ru hu uk pl zh_TW da cs el fi ja ca gl zh_CN tr nb
LANGS = $(wxLANGS) en kab

wxMaxima.app: all
	mkdir -p wxMaxima.app/Contents/MacOS
	mkdir -p wxMaxima.app/Contents/Resources
	cp $(srcdir)/test/testbench_simple.wxmx wxMaxima.app/Contents/Resources
	cp data/wxmathml.lisp wxMaxima.app/Contents/Resources
	cp $(srcdir)/data/wxmaxima.png wxMaxima.app/Contents/Resources
	cp $(srcdir)/data/wxmaxima.svg wxMaxima.app/Contents/Resources
	cp $(srcdir)/data/autocomplete.txt wxMaxima.app/Contents/Resources
	cp $(srcdir)/data/tips*.txt wxMaxima.app/Contents/Resources
	cp $(srcdir)/art/wxmac.icns wxMaxima.app/Contents/Resources
	cp $(srcdir)/art/wxmac-doc.icns wxMaxima.app/Contents/Resources
	cp $(srcdir)/art/wxmac-doc-wxm.icns wxMaxima.app/Contents/Resources
	cp $(srcdir)/art/wxmac-doc-wxmx.icns wxMaxima.app/Contents/Resources
	cp $(srcdir)/data/Info.plist wxMaxima.app/Contents
	cp $(srcdir)/data/PkgInfo wxMaxima.app/Contents
	cp $(srcdir)/src/wxmaxima wxMaxima.app/Contents/MacOS
	for i in $(LANGS) ; do \
		mkdir -p wxMaxima.app/Contents/Resources/locale/$$i/LC_MESSAGES ; \
		cp $(srcdir)/locales/$$i.mo wxMaxima.app/Contents/Resources/locale/$$i/LC_MESSAGES/wxMaxima.mo ; \
	done
	for i in $(wxLANGS) ; do \
		mkdir -p wxMaxima.app/Contents/Resources/locale/$$i/LC_MESSAGES ; \
		cp $(srcdir)/locales/wxwin/$$i.mo wxMaxima.app/Contents/Resources/locale/$$i/LC_MESSAGES/wxMaxima-wxstd.mo ; \
	done
	mkdir -p wxMaxima.app/Contents/Resources/toolbar
	mkdir -p wxMaxima.app/Contents/Resources/statusbar
	mkdir -p wxMaxima.app/Contents/Resources/config
	cp $(srcdir)/art/toolbar/*.png wxMaxima.app/Contents/Resources/toolbar
	cp $(srcdir)/art/statusbar/*.png wxMaxima.app/Contents/Resources/statusbar
	cp $(srcdir)/art/config/*.png wxMaxima.app/Contents/Resources/config
	mkdir -p wxMaxima.app/Contents/Resources/help
	cp info/*.html wxMaxima.app/Contents/Resources/help
	cp $(srcdir)/info/*.jpg  wxMaxima.app/Contents/Resources/help
	cp $(srcdir)/info/*.png  wxMaxima.app/Contents/Resources/help
	cp $(srcdir)/info/wxmaxima.hhp wxMaxima.app/Contents/Resources/help
	@echo "The following dylibs will need to be included in wxMaxima.app"
	@echo "in order to make it run on systems that haven't them installed"
	@echo "(see README.md)":
	otool -l src/wxmaxima|cut -f 2|grep "name"|grep "/usr/"|grep "\.dylib"|grep -v libc++.1.dylib|grep -v libSystem.B.dylib|sed -e "s#.*\(/usr/[^ ]*\).*#\1#g"|tee dylibs.txt
	@echo "Will generate a script named dylibs.sh which (with a little bit"
	@echo "of luck) will do this automatically."
	echo "#!/bin/sh">dylibs.sh
	chmod +x dylibs.sh
	cat dylibs.txt|sed -f data/dylibs.sed >> dylibs.sh
	@echo "Executing the script..."
	-mkdir ./wxMaxima.app/Contents/MacOS/lib
	./dylibs.sh
	@echo "Done."

wxMaxima.win: all
	mkdir -p wxMaxima/art
	mkdir -p wxMaxima/data
	cp data/wxmathml.lisp wxMaxima/data
	cp $(srcdir)/test/testbench_simple.wxmx wxMaxima/data
	cp $(srcdir)/data/wxmaxima.png wxMaxima/data
	cp $(srcdir)/data/wxmaxima.svg wxMaxima/data
	cp $(srcdir)/data/autocomplete.txt wxMaxima/data
	cp $(srcdir)/data/tips*.txt wxMaxima/data
	mkdir -p wxMaxima/help
	cp $(srcdir)/info/*.jpg  wxMaxima/help
	cp $(srcdir)/info/*.png  wxMaxima/help
# Try to autodetect which libraries to ship with wxMaxima.
#	-cp `gcc -print-libgcc-file-name` wxMaxima
#	-cp `gcc -print-file-name=libstdc++.dll` wxMaxima
if CHM
	cp $(srcdir)/info/*.chm  wxMaxima/help
	-cp $(srcdir)/info/wxmaxima.hhp wxMaxima/help
endif
	-cp $(srcdir)/info/*.html wxMaxima/help
	mkdir -p wxMaxima/art/toolbar
	cp $(srcdir)/art/toolbar/*.png wxMaxima/art/toolbar
	mkdir -p wxMaxima/art/statusbar
	cp $(srcdir)/art/statusbar/*.png wxMaxima/art/statusbar
	mkdir -p wxMaxima/art/config
	cp $(srcdir)/art/config/*.png wxMaxima/art/config
	for i in $(LANGS) ; do \
		mkdir -p wxMaxima/locale/$$i/LC_MESSAGES ; \
		cp $(srcdir)/locales/$$i.mo wxMaxima/locale/$$i/LC_MESSAGES/wxMaxima.mo ; \
	done
	for i in $(wxLANGS) ; do \
		mkdir -p wxMaxima/locale/$$i/LC_MESSAGES ; \
		cp $(srcdir)/locales/wxwin/$$i.mo wxMaxima/locale/$$i/LC_MESSAGES/wxMaxima-wxstd.mo ; \
	done
	cp src/wxmaxima.exe wxMaxima/

wxMaxima.win.zip: wxMaxima.win
	zip -r wxMaxima.win.zip wxMaxima

allmo:
	cd locales&&$(MAKE) allmo

signeddist: dist-gzip
	gpg --armor --detach-sign $(distdir).tar.gz



allpo:
	    cd locales&&$(MAKE) allpo

all: locales/en.mo

locales/en.mo: allmo

Doxygen: FORCE
	    cd Doxygen&&$(MAKE)

.PHONY:	allmo FORCE
FORCE:
