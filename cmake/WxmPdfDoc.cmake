# -*- mode: CMake; cmake-tab-width: 4; -*-
#
# wxPdfDocument (https://github.com/wxMaxima-developers/wxpdfdoc, a fork of
# utelle/wxpdfdoc) is what the "Export to PDF" feature draws the worksheet
# with. The fork adds what a worksheet needs and upstream doesn't have yet:
# font fallback (Greek letters and maths symbols that the worksheet's text
# font lacks are taken from another installed font, as on screen), embedding
# every font, and tagged (accessible) PDF with MathML attached to formulas.
#
# It is a git submodule in thirdparty/wxpdfdoc, compiled into a static
# library here: wxpdfdoc's own build systems (autotools, premake, Visual
# Studio projects) can't be driven from a CMake build, and a system-wide
# wxPdfDocument would lack the fork's features.
#
# Sets WXM_USE_WXPDFDOC (ON if the library is built) and defines the target
# wxm_wxpdfdoc.

option(WXM_USE_WXPDFDOC "Build the PDF export (needs the thirdparty/wxpdfdoc git submodule)" ON)

set(WXM_WXPDFDOC_DIR "${CMAKE_SOURCE_DIR}/thirdparty/wxpdfdoc")
# For the feature summary: why there is no PDF export, if there is none.
set(WXM_WXPDFDOC_MISSING_REASON "")

if(WXM_USE_WXPDFDOC AND NOT EXISTS "${WXM_WXPDFDOC_DIR}/include/wx/pdfstructure.h")
    if(EXISTS "${WXM_WXPDFDOC_DIR}/include/wx/pdfdocument.h")
        set(WXM_WXPDFDOC_MISSING_REASON "thirdparty/wxpdfdoc is a wxPdfDocument without tagged PDF support. Run git submodule update in the source tree.")
    else()
        set(WXM_WXPDFDOC_MISSING_REASON "The git submodule thirdparty/wxpdfdoc is not checked out. Run git submodule update --init in the source tree.")
    endif()
    message(STATUS "PDF export disabled: ${WXM_WXPDFDOC_MISSING_REASON}")
    set(WXM_USE_WXPDFDOC OFF)
endif()

# wxGTK's wxPdfDocument (Linux, the BSDs, Cygwin) finds installed fonts (and
# fallback fonts) through fontconfig, the library Pango uses for the same
# purpose on screen. Other ports (MSW, macOS, Qt) don't use it.
if(WXM_USE_WXPDFDOC AND wxWidgets_DEFINITIONS MATCHES "__WXGTK__")
    find_package(PkgConfig QUIET)
    if(PKG_CONFIG_FOUND)
        pkg_check_modules(WXM_FONTCONFIG IMPORTED_TARGET fontconfig)
    endif()
    if(NOT WXM_FONTCONFIG_FOUND)
        set(WXM_WXPDFDOC_MISSING_REASON "fontconfig was not found. Install libfontconfig-dev (or your distribution's equivalent).")
        message(STATUS "PDF export disabled: ${WXM_WXPDFDOC_MISSING_REASON}")
        set(WXM_USE_WXPDFDOC OFF)
    endif()
endif()

if(WXM_USE_WXPDFDOC)
    # The bundled brotli decompressor is written in C.
    enable_language(C)
    set(WXM_WXPDFDOC_SOURCES
        pdfannotation.cpp pdfcffdecoder.cpp pdfcffindex.cpp pdfcolour.cpp
        pdfdc.cpp pdfdecode.cpp pdfdocument.cpp pdfencoding.cpp pdfencrypt.cpp
        pdffont.cpp pdffontdata.cpp pdffontdatacore.cpp
        pdffontdataopentype.cpp pdffontdatatruetype.cpp pdffontdatatype0.cpp
        pdffontdatatype1.cpp pdffontdescription.cpp pdffontdetails.cpp
        pdffontextended.cpp pdffontmanager.cpp pdffontparser.cpp
        pdffontparsertruetype.cpp pdffontparsertype1.cpp pdffontsubsetcff.cpp
        pdffontsubsettruetype.cpp pdffontvolt.cpp pdfform.cpp pdfgc.cpp
        pdfgradient.cpp pdfgraphics.cpp pdfgridctrl.cpp pdfimage.cpp
        pdfkernel.cpp pdflayer.cpp pdflistctrl.cpp pdfobjects.cpp pdfocg.cpp
        pdfparser.cpp pdfpattern.cpp pdfprint.cpp pdfrijndael.cpp
        pdfstructure.cpp pdftemplate.cpp pdfutility.cpp pdfxml.cpp
        crypto/random.cpp crypto/saslprep.cpp crypto/sha256.cpp
        crypto/sha384.cpp crypto/sha512.cpp crypto/unicode_norm.cpp
        woff/woff2converter.cpp woff/woffconverter.cpp)
    # Left out: pdfbarcode.cpp and pdfbarcodezint.cpp, which need the zint
    # barcode library and nothing else uses.
    list(TRANSFORM WXM_WXPDFDOC_SOURCES PREPEND "${WXM_WXPDFDOC_DIR}/src/")
    # The WOFF2 decoder (and the brotli decompressor it needs) is bundled
    # with wxpdfdoc.
    set(WXM_WOFF2_SOURCES
        src/woff2_dec.cc src/woff2_common.cc src/woff2_out.cc
        src/table_tags.cc src/variable_length.cc
        brotli/dec/bit_reader.c brotli/dec/decode.c brotli/dec/huffman.c
        brotli/dec/state.c brotli/common/constants.c brotli/common/context.c
        brotli/common/dictionary.c brotli/common/platform.c
        brotli/common/shared_dictionary.c brotli/common/transform.c)
    list(TRANSFORM WXM_WOFF2_SOURCES PREPEND "${WXM_WXPDFDOC_DIR}/thirdparty/woff2/")

    add_library(wxm_wxpdfdoc STATIC ${WXM_WXPDFDOC_SOURCES} ${WXM_WOFF2_SOURCES})
    target_include_directories(wxm_wxpdfdoc SYSTEM PUBLIC "${WXM_WXPDFDOC_DIR}/include")
    target_include_directories(wxm_wxpdfdoc PRIVATE
        "${WXM_WXPDFDOC_DIR}/src"
        "${WXM_WXPDFDOC_DIR}/thirdparty/woff2/include"
        "${WXM_WXPDFDOC_DIR}/thirdparty/woff2/src"
        "${WXM_WXPDFDOC_DIR}/thirdparty/woff2/brotli/include")
    # A static library: WXMAKINGLIB/WXUSINGLIB keep pdfdocdef.h from adding
    # dllimport/dllexport on Windows.
    target_compile_definitions(wxm_wxpdfdoc PRIVATE WXMAKINGLIB_PDFDOC)
    target_compile_definitions(wxm_wxpdfdoc INTERFACE WXUSINGLIB_PDFDOC)
    # Third-party code: build it as its authors do. Our CI compiles with
    # -Werror -Wpedantic, which is for our own code, and the library's
    # warnings are not ours to fix here.
    target_compile_options(wxm_wxpdfdoc PRIVATE
        $<$<OR:$<CXX_COMPILER_ID:GNU>,$<CXX_COMPILER_ID:Clang>,$<CXX_COMPILER_ID:AppleClang>>:-w -Wno-error>
        $<$<CXX_COMPILER_ID:MSVC>:/W0>)
    # Neither a unity build nor our precompiled header fits code that
    # wasn't written for them (anonymous namespaces and static helpers of
    # the same name in different files).
    set_target_properties(wxm_wxpdfdoc PROPERTIES
        UNITY_BUILD OFF
        POSITION_INDEPENDENT_CODE ON)
    target_link_libraries(wxm_wxpdfdoc PUBLIC ${wxWidgets_LIBRARIES})
    if(TARGET PkgConfig::WXM_FONTCONFIG)
        target_link_libraries(wxm_wxpdfdoc PUBLIC PkgConfig::WXM_FONTCONFIG)
    endif()
    if(APPLE)
        # Random numbers for the encryption code
        target_link_libraries(wxm_wxpdfdoc PUBLIC "-framework Security")
    endif()
    if(WIN32)
        target_link_libraries(wxm_wxpdfdoc PUBLIC gdi32)
    endif()
    message(STATUS "PDF export: using wxPdfDocument from ${WXM_WXPDFDOC_DIR}")
endif()
