
figurefiles =  NoiseFilter.jpg maxima_screenshot.jpg InputCell.jpg
figurefiles += wxMaximaWindow.jpg wxMaximaLogo.jpg wxMaximaLogo.png SidePanes.jpg ezUnits.jpg
figurefiles += BatchImage.jpg wxMaxima_configuration_001.jpg wxMaxima_configuration_002.jpg
figurefiles += wxMaxima_configuration_001.png wxMaxima_configuration_002.png sbclMemory.png
figurefiles += sbclMemory.jpg ezUnits.png BatchImage.png Autocomplete1.jpg Autocomplete2.jpg
figurefiles += SecondTableExample.png MatrixTableExample.png


EXTRA_DIST = manual.css $(html_DATA)\
    $(genericdirDATA) $(figurefiles) \
    wxmaximaicon.ico \
    $(html_DATA) wxMaximaWindow.wxmx


if CHM
WXMAXIMA_CHM = wxMaxima.chm
INSTALL_CHM = install-chm
UNINSTALL_CHM = uninstall-chm
CLEAN_CHM = clean-chm

all-local: $(WXMAXIMA_CHM)

install-chm: wxMaxima.chm
	test -z "$(DESTDIR)$(docchmdir)" || mkdir -p -- "$(DESTDIR)$(docchmdir)"
	$(INSTALL_DATA) $< "$(DESTDIR)$(docchmdir)/wxMaxima.chm"

uninstall-chm:
	rm -f "$(DESTDIR)$(docchmdir)"

clean-chm:
	rm -f wxMaxima.chm
	rm -rf chm
endif


.chm:.hhp
	-"$(HHC)" $<
