#
# This file is based on the corresponding file for the poedit program
#

WXWIN_LINGUAS =    fr es it de pt_BR ru hu uk pl zh_TW da cs el fi ja ca gl zh_CN tr nb
WXMAXIMA_LINGUAS = $(WXWIN_LINGUAS) kab

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

uninstall-local:
	for i in $(WXMAXIMA_LINGUAS) ; do \
		rm -f $(DESTDIR)$(localedir)/$$i/LC_MESSAGES/wxMaxima.mo ; \
	done
	for i in $(WXWIN_LINGUAS) ; do \
		rm -f $(DESTDIR)$(localedir)/$$i/LC_MESSAGES/wxMaxima-wxstd.mo ; \
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
.po.mo:
	$(MSGFMT) -o $@ $<

$(srcdir)/wxMaxima.pot:
	touch $@
	(cd $(srcdir) ; find ../src -name "*.cpp" | $(XARGS) $(XGETTEXT) $(XGETTEXT_ARGS) -o wxMaxima.pot)
	$(XGETTEXT) $(XGETTEXT_ARGS) -o wxMaxima.pot

# The PO file must be updated by to include new translations by running this make target
# every time a new string is introduced.
allpo: force-update wxMaxima.pot
	@-for t in $(WXMAXIMA_LINGUAS); do \
		echo Creating $$t.po; \
		$(MSGMERGE) $$t.po $(srcdir)/wxMaxima.pot > $$t.new && mv $$t.new $$t.po;\
		done

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
	cat da.po | sed -e "s/utf-8/ISO-8859-1/" | iconv -f utf-8 -t iso8859-1 > transcoded/da.po
	$(MSGFMT) transcoded/da.po -o transcoded/da.mo
	cat de.po | sed -e "s/UTF-8/ISO-8859-15/" | iconv -f utf-8 -t iso8859-15 > transcoded/de.po
	$(MSGFMT) transcoded/de.po -o transcoded/de.mo
	cp es.po transcoded
	$(MSGFMT) transcoded/es.po -o transcoded/es.mo
	cp fr.po transcoded
	$(MSGFMT) transcoded/fr.po -o transcoded/fr.mo
	cat it.po | sed -e "s/UTF-8/ISO-8859-1/" | iconv -f utf-8 -t iso8859-1 > transcoded/it.po
	$(MSGFMT) transcoded/it.po -o transcoded/it.mo
	cat pt_BR.po | sed -e "s/UTF-8/ISO-8859-1/" | iconv -f utf-8 -t iso8859-1 > transcoded/pt_br.po
	$(MSGFMT) transcoded/pt_BR.po -o transcoded/pt_BR.mo
	cp ru.po transcoded
	$(MSGFMT) transcoded/ru.po -o transcoded/ru.mo
	cp uk.po transcoded
	$(MSGFMT) transcoded/uk.po -o transcoded/uk.mo
	cat hu.po | sed -e "s/UTF-8/ISO-8859-2/" | iconv -f utf-8 -t iso8859-2 > transcoded/hu.po
	$(MSGFMT) transcoded/hu.po -o transcoded/hu.mo
	cp pl.po transcoded
	$(MSGFMT) transcoded/pl.po -o transcoded/pl.mo
	cp zh_TW.po transcoded
	$(MSGFMT) transcoded/zh_TW.po -o transcoded/zh_TW.mo
	cp cs.po transcoded
	$(MSGFMT) transcoded/cs.po -o transcoded/cs.mo
	cp el.po transcoded
	$(MSGFMT) transcoded/el.po -o transcoded/el.mo
	cp fi.po transcoded
	$(MSGFMT) transcoded/fi.po -o transcoded/fi.mo
	cp ja.po transcoded
	$(MSGFMT) transcoded/ja.po -o transcoded/ja.mo
	cp ca.po transcoded
	$(MSGFMT) transcoded/ca.po -o transcoded/ca.mo
	cp gl.po transcoded
	$(MSGFMT) transcoded/gl.po -o transcoded/gl.mo
	cp zh_CN.po transcoded
	$(MSGFMT) transcoded/zh_CN.po -o transcoded/zh_CN.mo
	cp tr.po transcoded
	$(MSGFMT) transcoded/tr.po -o transcoded/tr.mo


EXTRA_DIST = fr.po fr.mo wxwin/fr.mo \
             es.po es.mo wxwin/es.mo  \
             it.po it.mo wxwin/it.mo  \
             de.po de.mo wxwin/de.mo  \
	     pt_BR.po pt_BR.mo wxwin/pt_BR.mo \
	     ru.po ru.mo wxwin/ru.mo \
	     hu.po hu.mo wxwin/hu.mo \
	     uk.po uk.mo wxwin/uk.mo \
	     pl.po pl.mo wxwin/pl.mo \
	     zh_TW.po zh_TW.mo wxwin/zh_TW.mo \
	     da.po da.mo wxwin/da.mo \
	     cs.po cs.mo wxwin/cs.mo \
	     el.po el.mo wxwin/el.mo \
	     fi.po fi.mo wxwin/fi.mo \
	     ja.po ja.mo wxwin/ja.mo \
	     ca.po ca.mo wxwin/ca.mo \
	     gl.po gl.mo wxwin/gl.mo \
             zh_CN.po zh_CN.mo wxwin/zh_CN.mo \
             tr.po tr.mo wxwin/tr.mo \
             nb.po nb.mo wxwin/nb.mo \
	     kab.po kab.mo \
             wxMaxima.pot ChangeLog

.PHONY: allpo allmo force-update stats FORCE
SUFFIXES=.po .mo
