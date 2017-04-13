find_package(Gettext)

file(GLOB TRANSLATION_FILES *.po)

if (GETTEXT_FOUND)
    gettext_create_translations(wxMaxima.pot ${TRANSLATION_FILES})
else()
    MESSAGE(STATUS "Gettext not found - translations will be disabled")
endif()
