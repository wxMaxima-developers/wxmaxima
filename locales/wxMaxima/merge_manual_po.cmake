# -*- mode: CMake; cmake-tab-width: 4; -*-
#
# Combines locales/manual/<lang>.po's translations into
# locales/wxMaxima/<lang>.po (if a manual translation for that language
# exists) before wxMaxima's own msgmerge -U picks up the result.
#
# po4a treats the po file it is pointed at as its own: running po4a on a
# file that also holds wxMaxima's own UI strings does not just add/update
# the manual's entries, it *rewrites the whole file to contain only the
# entries po4a itself extracted from info/wxmaxima.md* - silently dropping
# every UI string in it (confirmed live: a language with 1000 translated UI
# strings and 69 translated manual strings dropped to 69 after one po4a
# run). So po4a must keep writing its own separate locales/manual/<lang>.po
# (as it always did), and this script is the *only* place that folds that
# content into the combined locales/wxMaxima/<lang>.po - never let po4a
# write directly into the shared file.
#
# --use-first prefers locales/wxMaxima/<lang>.po's own header (Crowdin
# metadata, more current) over the manual po's; there is no msgid overlap
# between the two catalogs (confirmed empirically) for this to have to
# arbitrate beyond that.
#
# Required variables (-D on the cmake -P invocation):
#   LANG_PO    - path to locales/wxMaxima/<lang>.po (updated in place)
#   MANUAL_PO  - path to locales/manual/<lang>.po (read-only; may not exist)
#   MSGCAT     - path to the msgcat executable

if(EXISTS "${MANUAL_PO}")
    execute_process(
        COMMAND "${MSGCAT}" --use-first --no-wrap "${LANG_PO}" "${MANUAL_PO}" -o "${LANG_PO}.merging"
        RESULT_VARIABLE _result
    )
    if(NOT _result EQUAL 0)
        message(FATAL_ERROR "msgcat failed to merge ${MANUAL_PO} into ${LANG_PO}")
    endif()
    file(RENAME "${LANG_PO}.merging" "${LANG_PO}")
endif()
