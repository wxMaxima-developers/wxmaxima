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

gettext_create_translations(wxMaxima.pot ${TRANSLATION_FILES})
