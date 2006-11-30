#
# This file is based on the corresponding file for the poedit program
#

WXMAXIMA_LINGUAS = fr es it de pt_BR ru
WXWIN_LINGUAS = fr es it de pt_BR ru

localedir = $(datadir)/locale

install-data-local: @CATALOGS_TO_INSTALL@

install-wxmaxima-catalogs:
	for i in $(WXMAXIMA_LINGUAS) ; do \
		$(mkinstalldirs) $(DESTDIR)$(localedir)/$$i/LC_MESSAGES ; \
		$(INSTALL_DATA) $(srcdir)/$$i.mo $(DESTDIR)$(localedir)/$$i/LC_MESSAGES/wxMaxima.mo ; \
	done

install-wxstd-catalogs:
	for i in $(WXWIN_LINGUAS) ; do \
		$(mkinstalldirs) $(DESTDIR)$(localedir)/$$i/LC_MESSAGES ; \
		$(INSTALL_DATA) $(srcdir)/wxwin/$$i.mo $(DESTDIR)$(localedir)/$$i/LC_MESSAGES/wxMaxima-wxstd.mo ; \
	done



# ----------------------------------------------------------------------------
# Logic for catalogs updating follows
# (shamelessly stolen from wxWindows makefile):
# ----------------------------------------------------------------------------


# the programs we use (TODO: use configure to detect them)
MSGFMT=msgfmt --verbose
MSGMERGE=msgmerge
XGETTEXT=xgettext
XARGS=xargs

# common xgettext args: C++ syntax, use the specified macro names as markers
XGETTEXT_ARGS=-C -k_ -s -j

# implicit rules
%.mo: %.po
	$(MSGFMT) -o $@ $<

# a PO file must be updated to include new translations
%.po: $(srcdir)/wxMaxima.pot
	if [ -f $@ ]; then \
		$(MSGMERGE) $@ $(srcdir)/wxMaxima.pot > $@.new && mv $@.new $@;\
	else \
		cp $(srcdir)/wxMaxima.pot $@; \
	fi

$(srcdir)/wxMaxima.pot:
	touch $@
	(cd $(srcdir) ; find ../src -name "*.cpp" | $(XARGS) $(XGETTEXT) $(XGETTEXT_ARGS) -o wxMaxima.pot)
	(cd $(srcdir) ; find ../data -name "*.txt" | $(XARGS) $(XGETTEXT) $(XGETTEXT_ARGS) -o wxMaxima.pot)

allpo: force-update
	@-for t in $(WXMAXIMA_LINGUAS); do $(MAKE) $(srcdir)/$$t.po; done

allmo:
	@for t in $(WXMAXIMA_LINGUAS); do $(MAKE) $(srcdir)/$$t.mo; done

force-update:
	$(RM) $(srcdir)/wxMaxima.pot

# print out the percentage of the translated strings
stats:
	@for i in $(WXMAXIMA_LINGUAS); do \
		x=`$(MSGFMT) -o /dev/null "$(srcdir)/$$i.po" 2>&1 | sed -e 's/[,\.]//g' \
			-e 's/\([0-9]\+\) translated messages\?/TR=\1/' \
			-e 's/\([0-9]\+\) fuzzy translations\?/FZ=\1/' \
			-e 's/\([0-9]\+\) untranslated messages\?/UT=\1/'`; \
		TR=0 FZ=0 UT=0; \
		eval $$x; \
		TOTAL=`expr $$TR + $$FZ + $$UT`; \
		echo "\"$$i\" => \"`expr 100 "*" $$TR / $$TOTAL`\", /* $$TOTAL strings */"; \
	done

#echo "$$i.po `expr 100 "*" $$TR / $$TOTAL`% of $$TOTAL strings"; 

# transcoded locales

transcoded:
	mkdir -p transcoded
	cat de.po | sed -e "s/UTF-8/ISO-8859-15/" | iconv -f utf-8 -t iso8859-15 > transcoded/de.po
	$(MSGFMT) transcoded/de.po -o transcoded/de.mo
	cp es.po transcoded
	$(MSGFMT) transcoded/es.po -o transcoded/es.mo
	cp fr.po transcoded
	$(MSGFMT) transcoded/fr.po -o transcoded/fr.mo
	cat it.po | sed -e "s/UTF-8/ISO-8859-1/" | iconv -f utf-8 -t iso8859-1 > transcoded/it.po
	$(MSGFMT) transcoded/it.po -o transcoded/it.mo
	cp pt_BR.po transcoded
	$(MSGFMT) transcoded/pt_BR.po -o transcoded/fr.mo
	cp ru.po transcoded
	$(MSGFMT) transcoded/ru.po -o transcoded/ru.mo


EXTRA_DIST = fr.po fr.mo wxwin/fr.mo \
             es.po es.mo wxwin/es.mo  \
             it.po it.mo wxwin/it.mo  \
             de.po de.mo wxwin/de.mo  \
	     pt_BR.po pt_BR.mo wxwin/pt_BR.mo \
	     ru.po ru.mo wxwin/ru.mo \
             wxMaxima.pot ChangeLog

.PHONY: allpo allmo force-update stats FORCE
