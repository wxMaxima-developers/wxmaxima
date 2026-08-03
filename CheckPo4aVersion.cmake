# -*- mode: CMake; cmake-tab-width: 4; -*-
#
# Finds po4a and rejects a version too old to trust with non-ASCII text.
#
# po4a < 0.70 parses text encodings loosely, relying on Perl's default
# latin-1/UTF-8 auto-detection, instead of po4a >= 0.70's PerlIO-based
# handling (see po4a's own NEWS file, the "Cassini" release). This can
# silently corrupt translated text that isn't plain ASCII: observed directly
# with po4a 0.69 (Ubuntu 24.04's own package) turning a German manual
# paragraph into English text with mangled UTF-8 (e.g. "'" became "â"),
# without printing any warning or error of its own. info/CMakeLists.txt
# calls po4a on every configure - not just on an explicit "update the
# translations" step - and writes its output straight into
# info/wxmaxima.<lang>.md in the source tree, so this is not a rare, opt-in
# failure mode for anyone who happens to have both po4a and pandoc
# installed.
#
# Included (not add_subdirectory()'d) from info/CMakeLists.txt and
# locales/manual/CMakeLists.txt, the two places that call po4a, so it runs in
# each includer's own directory scope and PO4A ends up set there directly -
# the same contract find_program() itself has, so callers can keep writing
# plain if(PO4A) checks.

set(WXM_PO4A_MINIMUM_VERSION "0.70")

find_program(PO4A po4a)
if(PO4A)
    execute_process(COMMAND ${PO4A} --version OUTPUT_VARIABLE PO4A_VERSION_TEXT)
    string(REGEX REPLACE "^po4a version ([^\n]*).*" "\\1" PO4A_VERSION "${PO4A_VERSION_TEXT}")
    string(REGEX REPLACE "\\.$" "" PO4A_VERSION "${PO4A_VERSION}") # remove trailing dot.

    if(PO4A_VERSION VERSION_LESS WXM_PO4A_MINIMUM_VERSION)
        message(WARNING
            "Found po4a ${PO4A_VERSION}, but it is older than ${WXM_PO4A_MINIMUM_VERSION} and will not be used: "
            "po4a < 0.70 parses text encodings loosely (Perl's default latin-1/UTF-8 "
            "auto-detection) instead of po4a >= 0.70's PerlIO-based handling, and can "
            "silently corrupt non-ASCII translated text with no error or warning of its "
            "own - observed directly turning a German manual paragraph into mangled "
            "English. Upgrade po4a to translate the manual; everything else about "
            "building and running wxMaxima is unaffected by this.")
        set(PO4A "PO4A-NOTFOUND")
    else()
        message(STATUS "Found po4a: ${PO4A} (found version \"${PO4A_VERSION}\")")
    endif()
endif()
