# -*- mode: CMake; cmake-tab-width: 4; -*-
#
# Reverse of merge_manual_po.cmake: pulls a language's manual-content
# translations back OUT of the combined locales/wxMaxima/<lang>.po -- the
# only file Crowdin actually writes to -- into locales/manual/<lang>.po, so
# translations contributed through Crowdin for the manual's own strings
# reach info/wxmaxima.<lang>.md via the po4a pipeline. Without this step
# those translations sit in the combined file but nothing ever reads them
# back out for the manual to use: a silent dead end for anyone translating
# the manual's strings through Crowdin instead of po4a-updatepo directly.
#
# msgmerge's def/ref-template model does the filtering this needs in one
# step: given a translated file (def) and a template (ref), it emits the
# ref's own entries with translations pulled from def wherever the msgid
# matches (near-misses included, via its normal fuzzy matching). Using
# MANUAL_POT as ref restricts the output to exactly the manual's own
# msgids. The result is then folded into the existing locales/manual/<lang>.po
# the same way merge_manual_po.cmake folds in the other direction --
# msgcat --use-first prefers whichever side has a non-blank translation,
# and prefers the freshly-extracted (Crowdin-sourced) side on a genuine
# conflict between two non-blank translations, since the combined file is
# the single point of truth a translator actually edits now.
#
# Required variables (-D on the cmake -P invocation):
#   LANG_PO     - path to locales/wxMaxima/<lang>.po (read-only)
#   MANUAL_POT  - path to locales/manual/wxmaxima.md.pot (read-only)
#   MANUAL_PO   - path to locales/manual/<lang>.po (updated in place; created if missing)
#   MSGMERGE    - path to the msgmerge executable
#   MSGCAT      - path to the msgcat executable

execute_process(
    COMMAND "${MSGMERGE}" --quiet --no-wrap "${LANG_PO}" "${MANUAL_POT}" -o "${MANUAL_PO}.extracted"
    RESULT_VARIABLE _result
)
if(NOT _result EQUAL 0)
    message(FATAL_ERROR "msgmerge failed to extract manual translations from ${LANG_PO}")
endif()

if(EXISTS "${MANUAL_PO}")
    execute_process(
        COMMAND "${MSGCAT}" --use-first --no-wrap "${MANUAL_PO}.extracted" "${MANUAL_PO}" -o "${MANUAL_PO}.merging"
        RESULT_VARIABLE _result
    )
    if(NOT _result EQUAL 0)
        message(FATAL_ERROR "msgcat failed to fold extracted translations into ${MANUAL_PO}")
    endif()
    file(REMOVE "${MANUAL_PO}.extracted")
    file(RENAME "${MANUAL_PO}.merging" "${MANUAL_PO}")
else()
    file(RENAME "${MANUAL_PO}.extracted" "${MANUAL_PO}")
endif()
