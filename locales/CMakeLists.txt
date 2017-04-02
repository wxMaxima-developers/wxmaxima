find_package(gettext)

set(TRANSLATION_FILES
        fr.po
        es.po
        it.po
        de.po
        pt_BR.po
        ru.po
        hu.po
        uk.po
        pl.po
        zh_TW.po
        da.po
        cs.po
        el.po
        fi.po
        ja.po
        ca.po
        gl.po
        zh_CN.po
        tr.po
        nb.po
        en.po
        kab.po)

if (GETTEXT_FOUND)
    gettext_create_translations(wxMaxima.pot ${TRANSLATION_FILES})
else()
    MESSAGE("Gettext not found - translations will be disabled")
endif()
