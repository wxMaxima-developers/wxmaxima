# -*- mode: CMake; cmake-tab-width: 4; -*-

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Info.plist.in ${CMAKE_CURRENT_BINARY_DIR}/Info.plist)


find_package(Threads REQUIRED)

set(CELL_SOURCE_FILES
    AbsCell.cpp
    AtCell.cpp
    BoxCell.cpp
    NamedBoxCell.cpp
    Cell.cpp
    CellList.cpp
    CellPtr.cpp
    ConjugateCell.cpp
    DiffCell.cpp
    DigitCell.cpp
    EditorCell.cpp
    ExptCell.cpp
    FracCell.cpp
    FunCell.cpp
    GroupCell.cpp
    ImgCell.cpp
    ImgCellBase.cpp
    IntCell.cpp
    IntervalCell.cpp
    LabelCell.cpp
    LimitCell.cpp
    ListCell.cpp
    LongNumberCell.cpp
    MatrCell.cpp
    ParenCell.cpp
    SetCell.cpp
    AnimationCell.cpp
    SqrtCell.cpp
    SubCell.cpp
    SubSupCell.cpp
    SumCell.cpp
    TextCell.cpp
    ProductCell.cpp
    VisiblyInvalidCell.cpp
    FontAttribs.cpp
    TextStyle.cpp
    FontVariantCache.cpp
)
list(TRANSFORM CELL_SOURCE_FILES PREPEND cells/)

set(WIZARD_SOURCE_FILES
    ActualValuesStorageWiz.cpp
    BC2Wiz.cpp
    CsvWiz.cpp
    DrawWiz.cpp
    Gen1Wiz.cpp
    Gen2Wiz.cpp
    Gen3Wiz.cpp
    Gen4Wiz.cpp
    Gen5Wiz.cpp
    GenWizPanel.cpp
    ScrollingGenWizPanel.cpp
    GenWiz.cpp
    IntegrateWiz.cpp
    LimitWiz.cpp
    ListSortWiz.cpp
    MatWiz.cpp
    Plot2dWiz.cpp
    Plot3dWiz.cpp
    PlotFormatWiz.cpp
    SeriesWiz.cpp
    SubstituteWiz.cpp
    SumWiz.cpp
    SystemWiz.cpp
    WizardHelp.cpp
)
list(TRANSFORM WIZARD_SOURCE_FILES PREPEND wizards/)

set(SIDEBAR_SOURCE_FILES
    GreekSidebar.cpp
    CharButton.cpp
    UnicodeSidebar.cpp
    SymbolsSidebar.cpp
    History.cpp
    TableOfContents.cpp
    VariablesPane.cpp
    XmlInspector.cpp
    ButtonWrapSizer.cpp
    StatSidebar.cpp
    DrawSidebar.cpp
    MathSidebar.cpp
    FormatSidebar.cpp
    RegexCtrl.cpp
    HelpBrowser.cpp
    PerformanceSidebar.cpp
)
list(TRANSFORM SIDEBAR_SOURCE_FILES PREPEND sidebars/)

set(GRAPHICAL_IO_SOURCE_FILES
    BitmapOut.cpp
    EMFout.cpp
    OutCommon.cpp
    Printout.cpp
    SVGout.cpp
)
list(TRANSFORM GRAPHICAL_IO_SOURCE_FILES PREPEND graphical_io/)

set(DIALOG_SOURCE_FILES
    AboutDialog.cpp
    ChangeLogDialog.cpp
    FindReplaceDialog.cpp
    FindReplacePane.cpp
    LicenseDialog.cpp
    LoggingMessageDialog.cpp
    MaxSizeChooser.cpp
    ResolutionChooser.cpp
    TipOfTheDay.cpp
    ConfigDialogue.cpp
    MaximaNotStartingDialog.cpp
    BinaryNameCtrl.cpp
    DiffFrame.cpp
    DiffScrollSync.cpp
)
list(TRANSFORM DIALOG_SOURCE_FILES PREPEND dialogs/)

set(SOURCE_FILES
    ArtProvider.cpp
    Autocomplete.cpp
    AutocompletePopup.cpp
    BTextCtrl.cpp
    CellPointers.cpp
    CompositeDataObject.cpp
    Configuration.cpp
    DiffAlgorithm.cpp
    Dirstructure.cpp
    BackgroundQueue.cpp
    EvaluationQueue.cpp
    EventIDs.cpp
    Image.cpp
    MainMenuBar.cpp
    MarkDown.cpp
    MathParser.cpp
    Maxima.cpp
    MaximaIPC.cpp
    MaximaTokenizer.cpp
    MaximaManual.cpp
    nanoSVG.cpp
    Notification.cpp
    RecentDocuments.cpp
    RegexSearch.cpp
    StatusBar.cpp
    StringUtils.cpp
    SvgBitmap.cpp
    SvgPanel.cpp
    ToolBar.cpp
    Worksheet.cpp
    WrappingStaticText.cpp
    WXMformat.cpp
    WXMXformat.cpp
    levenshtein/levenshtein.cpp
    main.cpp
    wxMathml.cpp
    wxMaxima.cpp
    wxMaximaFrame.cpp
    wxMaximaIcon.cpp
    wxMaximaArtProvider.cpp
    wxMaximaOSDescription.cpp
)

if(NOT CYGWIN)
    list(APPEND SOURCE_FILES ${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.rc)
endif()

list(APPEND SOURCE_FILES
  ${CELL_SOURCE_FILES}
  ${GRAPHICAL_IO_SOURCE_FILES}
  ${WIZARD_SOURCE_FILES}
  ${SIDEBAR_SOURCE_FILES}
  ${DIALOG_SOURCE_FILES}
)

# The git hash lives in its own tiny generated TU (see GitHash.h): keeping the
# per-commit-volatile value out of Version.h (which is in the precompiled header)
# means a new commit only recompiles this one file instead of the whole tree.
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/GitHash.cpp.cin ${CMAKE_BINARY_DIR}/GitHash.cpp)
list(APPEND SOURCE_FILES ${CMAKE_BINARY_DIR}/GitHash.cpp)

include_directories(
  "${CMAKE_SOURCE_DIR}/src"
  "${CMAKE_BINARY_DIR}"
)



if(WXM_USE_CPPCHECK)
    find_program(CMAKE_CXX_CPPCHECK NAMES cppcheck)
    if(CMAKE_CXX_CPPCHECK)
        message(STATUS "cppcheck found at ${CMAKE_CXX_CPPCHECK}. Enable cppcheck tests.")
	list(APPEND CMAKE_CXX_CPPCHECK
	    "--enable=warning,style,performance,portability"
	    "--quiet"
	    "--std=c++20"
	    "--force"
	    #"--inconclusive"
	    "--inline-suppr"
	    "--language=c++"
	    "--template=gcc"
	    "--suppressions-list=${CMAKE_CURRENT_SOURCE_DIR}/CppCheckSuppressions.txt")
    else()
	message(STATUS "cppcheck not found.")
        set(CMAKE_CXX_CPPCHECK "")
    endif()
endif()

include_directories(${CMAKE_SOURCE_DIR}/data/winrc)

if(APPLE)
  file(GLOB RESOURCE_FILES
      ${CMAKE_SOURCE_DIR}/data/io.github.wxmaxima_developers.wxMaxima.png
      ${CMAKE_SOURCE_DIR}/data/io.github.wxmaxima_developers.wxMaxima.svg
      ${CMAKE_SOURCE_DIR}/art/wxmac.icns
      ${CMAKE_SOURCE_DIR}/art/wxmac-doc.icns
      ${CMAKE_SOURCE_DIR}/art/wxmac-doc-wxm.icns
      ${CMAKE_SOURCE_DIR}/art/wxmac-doc-wxmx.icns
      ${CMAKE_SOURCE_DIR}/info/*.html
      ${CMAKE_SOURCE_DIR}/info/*.png
      ${CMAKE_SOURCE_DIR}/data/PkgInfo
      ${CMAKE_SOURCE_DIR}/locales/wxwin/*.mo
      ${CMAKE_BINARY_DIR}/locale/*.mo
      )
endif()

if(WIN32)
  add_executable(wxmaxima WIN32 ${SOURCE_FILES})
  set_target_properties(wxmaxima PROPERTIES VS_DPI_AWARE "PerMonitor")
elseif(APPLE)
    set(MACOSX_BUNDLE_NAME wxmaxima)
    set(MACOSX_BUNDLE_ICON_FILE wxmac.icns)
    set(MACOSX_BUNDLE TRUE)
    list(APPEND SOURCE_FILES ${RESOURCE_FILES})
    add_executable(wxmaxima ${SOURCE_FILES})
    set_target_properties(wxmaxima PROPERTIES
        MACOSX_BUNDLE TRUE
	MACOSX_BUNDLE_INFO_PLIST ${CMAKE_CURRENT_BINARY_DIR}/Info.plist
        RESOURCE "${RESOURCE_FILES}"
    )

    install(
        # The bundle utilities need to be run in a
        # separate step, meaning from within a string.
        # they provide the fixup command that makes the
        # .dylibs run from within the app bundle.
        CODE "include(BundleUtilities)
        # the separate step doesn't know about
        # the values our variables had in the
        # cmakefile so we need to set them again.
        set(BU_CHMOD_BUNDLE_ITEMS TRUE)
        fixup_bundle(\"${CMAKE_BINARY_DIR}/src/wxmaxima.app\"   \"\"   \"\")"
        COMPONENT Runtime)
else()
    add_executable(wxmaxima ${SOURCE_FILES})
endif()
target_compile_features(wxmaxima PUBLIC cxx_std_20)

if(WXM_INTERPROCEDURAL_OPTIMIZATION)
    # Enable interprocedural optimization (IPO/LTO), if supported.
    cmake_policy(SET CMP0069 NEW)
    include(CheckIPOSupported)
    # Optional IPO. Do not use IPO if it's not supported by compiler.
    check_ipo_supported(RESULT IPOresult OUTPUT IPOoutput)
    if(IPOresult)
        set_property(TARGET wxmaxima PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
    else()
        message(WARNING "IPO is not supported: ${IPOoutput}")
    endif()
endif()

if(WXM_DISABLE_WEBVIEW)
  set(USE_WEBVIEW "0")
else()
  set(USE_WEBVIEW "1")
endif()
if(WXM_DISABLE_QA)
  set(USE_QA "0")
else()
  set(USE_QA "1")
endif()

if(WXM_ENABLE_PRECOMPILED_HEADERS)
  target_precompile_headers(wxmaxima PUBLIC "precomp.h")
else()
  set(USE_PRECOMP_HEADER "0")
endif()

file(RELATIVE_PATH TRANSLATIONS_DIR ${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_SOURCE_DIR}/locales/wxMaxima/)

configure_file("wxmaxima.exe.manifest.in" wxmaxima.exe.manifest)

# allow local execution (./wxmaxima-local) from the build directory without installation
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima-local.bat.in" ../wxmaxima-local.bat)
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima-local.in" ../wxmaxima-local)

target_link_libraries(wxmaxima ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})

if(COMPILE_SUCCEEDED)
  message("Success!")
endif()

if(WIN32)
    target_link_libraries(wxmaxima ws2_32)
endif()

# ---------------------------------------------------------------------------
# Coverage-guided libFuzzer fuzz targets for the parsers (see test/fuzz/).
# Built only with -DWXM_FUZZ=ON and a Clang compiler. They reuse the whole app
# (minus main.cpp, which holds libFuzzer's rival main()) so the parser runs with
# its real cell/Configuration/Worksheet dependencies.
# ---------------------------------------------------------------------------
if(WXM_FUZZ)
    if(NOT CMAKE_CXX_COMPILER_ID MATCHES "Clang")
        message(FATAL_ERROR "WXM_FUZZ requires Clang (libFuzzer). Re-run cmake with -DCMAKE_CXX_COMPILER=clang++.")
    endif()
    # Two build modes:
    #  - OSS-Fuzz drives the build via the environment: the coverage
    #    instrumentation and the sanitizer are already in $CXXFLAGS (picked up by
    #    CMAKE_CXX_FLAGS), and the fuzzing engine to link is in $LIB_FUZZING_ENGINE.
    #  - Standalone (developer / our own CI): we add the instrumentation
    #    (-fsanitize=fuzzer-no-link,address) and link libFuzzer ourselves.
    if(DEFINED ENV{LIB_FUZZING_ENGINE})
        set(WXM_FUZZ_INSTRUMENT "")
        separate_arguments(WXM_FUZZ_ENGINE NATIVE_COMMAND "$ENV{LIB_FUZZING_ENGINE}")
    else()
        set(WXM_FUZZ_INSTRUMENT -fsanitize=fuzzer-no-link,address -fno-omit-frame-pointer)
        set(WXM_FUZZ_ENGINE -fsanitize=fuzzer,address)
    endif()
    set(FUZZ_APP_SOURCES ${SOURCE_FILES})
    list(REMOVE_ITEM FUZZ_APP_SOURCES main.cpp)
    add_library(wxmFuzzApp OBJECT ${FUZZ_APP_SOURCES})
    target_compile_features(wxmFuzzApp PUBLIC cxx_std_20)
    target_compile_options(wxmFuzzApp PRIVATE ${WXM_FUZZ_INSTRUMENT})
    set(FUZZ_DIR ${CMAKE_SOURCE_DIR}/test/fuzz)
    foreach(FT wxm mathparser)
        add_executable(fuzz_${FT}
            ${FUZZ_DIR}/fuzz_${FT}.cpp ${FUZZ_DIR}/fuzz_stubs.cpp
            $<TARGET_OBJECTS:wxmFuzzApp>)
        target_compile_features(fuzz_${FT} PUBLIC cxx_std_20)
        target_compile_options(fuzz_${FT} PRIVATE ${WXM_FUZZ_INSTRUMENT})
        target_link_options(fuzz_${FT} PRIVATE ${WXM_FUZZ_ENGINE})
        target_link_libraries(fuzz_${FT} ${wxWidgets_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
    endforeach()
endif()

# ---------------------------------------------------------------------------
# Object library that lets unit tests (test/unit_tests, built with
# -DWXM_UNIT_TESTS=ON) link the *real* application code -- GroupCell,
# EditorCell, Worksheet, Configuration, ... -- instead of the lightweight stubs
# in TestStubs.cpp. Needed for tests that exercise the layout/recalculation
# pipeline. main.cpp is excluded (it holds the app's real main(); the test
# provides its own). Mirrors the wxmFuzzApp object library above.
# ---------------------------------------------------------------------------
if(WXM_UNIT_TESTS)
    set(TEST_APP_SOURCES ${SOURCE_FILES})
    list(REMOVE_ITEM TEST_APP_SOURCES main.cpp)
    add_library(wxmTestApp OBJECT ${TEST_APP_SOURCES})
    target_compile_features(wxmTestApp PUBLIC cxx_std_20)
endif()

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Version.h.cin ${CMAKE_BINARY_DIR}/Version.h)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/BuildConfig.h.cin ${CMAKE_BINARY_DIR}/BuildConfig.h)

if(APPLE)
    install(TARGETS wxmaxima
	     BUNDLE  DESTINATION .
	     RUNTIME DESTINATION bin
	     COMPONENT Runtime
	     RESOURCE DESTINATION resource)
else()
    install(TARGETS wxmaxima RUNTIME DESTINATION bin)
endif()

if(NOT WIN32)
    # Create symlink wxmxdiff -> wxmaxima in bin/
    install(CODE "
        macro(create_install_symlink TARGET_PATH LINK_PATH)
            get_filename_component(link_dir \"\${LINK_PATH}\" DIRECTORY)
            set(dest_dir \"\$ENV{DESTDIR}\${CMAKE_INSTALL_PREFIX}/\${link_dir}\")
            set(dest_link \"\$ENV{DESTDIR}\${CMAKE_INSTALL_PREFIX}/\${LINK_PATH}\")
            execute_process(COMMAND \${CMAKE_COMMAND} -E make_directory \${dest_dir})
            execute_process(COMMAND chmod 755 \${dest_dir})
            message(STATUS \"Creating symlink: \${dest_link} -> \${TARGET_PATH}\")
            execute_process(COMMAND \${CMAKE_COMMAND} -E create_symlink \${TARGET_PATH} \${dest_link})
        endmacro()
        
        create_install_symlink(wxmaxima bin/wxmxdiff)
    ")
endif()
# Windows: Install the manifest file as wxmaxima.exe.manifest, see
# https://learn.microsoft.com/en-us/windows/win32/sbscs/application-manifests
# We do (currently) not try to include it in the wxmaxima binary,
# an extra file is valid too and might help to test various entries in that file.
if(WIN32)
    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/wxmaxima.exe.manifest" DESTINATION bin)
endif()

# Build Packages
if(WIN32)
    set(CPACK_GENERATOR "ZIP;NSIS")
    set(CPACK_NSIS_DEFINES "ManifestDPIAware true")
    include(InstallRequiredSystemLibraries)
    install(FILES ${CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS} DESTINATION bin)
elseif(APPLE)
    # If we have generated an .apk bundle we can package this in a DMG image
    set(CPACK_GENERATOR "DragNDrop")
    set(CPACK_BINARY_DragNDrop "ON")
else()
    set(CPACK_GENERATOR "DEB;TGZ;TBZ2")
    # build RPMs only if rpmbuild is installed
    find_program(RPMBUILD_EXECUTABLE rpmbuild)
    if(NOT RPMBUILD_EXECUTABLE)
	message(STATUS "rpmbuild not found - no RPM package will be build with: cmake --build . -- package")
    else()
	message(STATUS "rpmbuild found - RPM package can be build with: cmake --build . -- package")
	list(APPEND CPACK_GENERATOR "RPM")
	list(APPEND CPACK_SOURCE_GENERATOR "RPM")
    endif()
endif()

set(CPACK_PACKAGE_VENDOR "The wxMaxima Team")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Document-based interface for the computer algebra system Maxima")
set(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_SOURCE_DIR}/data/description.txt")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/COPYING")
set(CPACK_PACKAGE_CONTACT "The wxMaxima Team <wxmaxima-devel@lists.sourceforge.net>")
set(CPACK_DEBIAN_PACKAGE_SECTION "math")

set(CPACK_RPM_PACKAGE_LICENSE "GPLv2+")
set(CPACK_RPM_PACKAGE_GROUP "Productivity/Scientific/Math")
list(APPEND CPACK_RPM_EXCLUDE_FROM_AUTO_FILELIST_ADDITION
  "/usr/share/icons"
  "/usr/share/man"
  "/usr/share/man/man1"
)
if(WIN32)
    set(CPACK_PACKAGE_ICON "${CMAKE_SOURCE_DIR}/data\\\\wxmaxima.bmp")
else()
    set(CPACK_PACKAGE_ICON "${CMAKE_SOURCE_DIR}/data/io.github.wxmaxima_developers.wxMaxima.png")
endif()
set(CPACK_PACKAGE_VERSION "${PROJECT_VERSION}")
set(CPACK_DEBIAN_PACKAGE_SHLIBDEPS ON)
set(CPACK_DEBIAN_PACKAGE_DEPENDS "maxima, maxima-doc")

# Build a tarball
set(CPACK_SOURCE_IGNORE_FILES
  "build/"
  ".git/"
  ".gitignore"
  ".gitattributes"
  "Doxygen/html/"
  "\\\\.gmo$"
  "\\\\.mo$"
  "~$"
  "CPackConfig.cmake"
  "CPackSourceConfig.cmake"
  "CMakeCache.txt"
  "CMakeFiles"
  "_CPack_Packages"
  "wxmaxima-.*\\\\.tgz$"
  "wxmaxima-.*\\\\.deb$"
  "wxmaxima-.*\\\\.rpm$"
  "wxmaxima-.*\\\\.bz2$"
  "wxmaxima-.*\\\\.xz$"
  "wxmaxima-.*\\\\.Z$"
  "wxmaxima-.*\\\\.gz$"
  "${CPACK_SOURCE_IGNORE_FILES}")

add_custom_command(
        OUTPUT ${CMAKE_BINARY_DIR}/nanosvg_private.h
        COMMAND ${CMAKE_COMMAND} ARGS
        -DINPUT_FILE="${CMAKE_CURRENT_SOURCE_DIR}/nanoSVG/nanosvg.h"
        -DOUTPUT_FILE=${CMAKE_BINARY_DIR}/nanosvg_private.h
        -P "${CMAKE_CURRENT_SOURCE_DIR}/privateNanoSVG.cmake"
        COMMENT "Generating our private copy of nanosvg.h"
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/nanoSVG/nanosvg.h ${CMAKE_CURRENT_SOURCE_DIR}/privateNanoSVG.cmake
)
add_custom_target(build_nanosvg_private.h DEPENDS ${CMAKE_BINARY_DIR}/nanosvg_private.h)
add_dependencies(wxmaxima build_nanosvg_private.h)

add_custom_command(
        OUTPUT ${CMAKE_BINARY_DIR}/nanosvgrast_private.h
        COMMAND ${CMAKE_COMMAND} ARGS
        -DINPUT_FILE="${CMAKE_CURRENT_SOURCE_DIR}/nanoSVG/nanosvgrast.h"
        -DOUTPUT_FILE=${CMAKE_BINARY_DIR}/nanosvgrast_private.h
        -P "${CMAKE_CURRENT_SOURCE_DIR}/privateNanoSVG.cmake"
        COMMENT "Generating our private copy of nanosvgrast.h"
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/nanoSVG/nanosvgrast.h ${CMAKE_CURRENT_SOURCE_DIR}/privateNanoSVG.cmake
)
add_custom_target(build_nanosvgrast_private.h DEPENDS ${CMAKE_BINARY_DIR}/nanosvgrast_private.h)
add_dependencies(wxmaxima build_nanosvgrast_private.h)


add_custom_command(
        OUTPUT ${CMAKE_BINARY_DIR}/sampleWorksheet.h
        COMMAND ${CMAKE_COMMAND} ARGS
        -DBIN2H_SOURCE_FILE="${CMAKE_SOURCE_DIR}/data/sampleWorksheet.wxmx"
        -DBIN2H_VARIABLE_NAME=sampleWorksheet_wxmx
        -DBIN2H_HEADER_FILE="${CMAKE_BINARY_DIR}/sampleWorksheet.h"
        -DBIN2H="${CMAKE_SOURCE_DIR}/cmake-bin2h/bin2h.cmake"
        -P "${CMAKE_CURRENT_SOURCE_DIR}/bin2h.cmake"
        COMMENT "Embedding the example worksheet for the config dialogue"
        DEPENDS ${CMAKE_SOURCE_DIR}/data/sampleWorksheet.wxmx ${CMAKE_CURRENT_SOURCE_DIR}/bin2h.cmake
)
add_custom_target(build_sampleWorksheet.h DEPENDS ${CMAKE_BINARY_DIR}/sampleWorksheet.h)
add_dependencies(wxmaxima build_sampleWorksheet.h)

add_custom_command(
        OUTPUT ${CMAKE_BINARY_DIR}/wxMathML_lisp.h
        COMMAND ${CMAKE_COMMAND} ARGS
        -DBIN2H_SOURCE_FILE="${CMAKE_CURRENT_SOURCE_DIR}/wxMathML.lisp"
        -DBIN2H_VARIABLE_NAME=wxMathML_lisp
        -DBIN2H_HEADER_FILE="${CMAKE_BINARY_DIR}/wxMathML_lisp.h"
        -DBIN2H="${CMAKE_SOURCE_DIR}/cmake-bin2h/bin2h.cmake"
        -P "${CMAKE_CURRENT_SOURCE_DIR}/bin2h.cmake"
        COMMENT "Embedding wxMathML.lisp"
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/wxMathML.lisp ${CMAKE_CURRENT_SOURCE_DIR}/bin2h.cmake
)
add_custom_target(build_wxMathML.h DEPENDS ${CMAKE_BINARY_DIR}/wxMathML_lisp.h)
add_dependencies(wxmaxima build_wxMathML.h)

add_custom_command(
        OUTPUT ${CMAKE_BINARY_DIR}/news_md.h
        COMMAND ${CMAKE_COMMAND} ARGS
        -DBIN2H_SOURCE_FILE="${CMAKE_SOURCE_DIR}/NEWS.md"
        -DBIN2H_VARIABLE_NAME=news_md
        -DBIN2H_HEADER_FILE="${CMAKE_BINARY_DIR}/news_md.h"
        -DBIN2H="${CMAKE_SOURCE_DIR}/cmake-bin2h/bin2h.cmake"
        -P "${CMAKE_CURRENT_SOURCE_DIR}/bin2h.cmake"
        COMMENT "Embedding NEWS.md"
        DEPENDS ${CMAKE_SOURCE_DIR}/NEWS.md ${CMAKE_CURRENT_SOURCE_DIR}/bin2h.cmake
)
add_custom_target(build_news_md.h DEPENDS ${CMAKE_BINARY_DIR}/news_md.h)
add_dependencies(wxmaxima build_news_md.h)

add_custom_command(
        OUTPUT ${CMAKE_BINARY_DIR}/wxm_license.h
        COMMAND ${CMAKE_COMMAND} ARGS
        -DBIN2H_SOURCE_FILE="${CMAKE_SOURCE_DIR}/COPYING"
        -DBIN2H_VARIABLE_NAME=wxm_license
        -DBIN2H_HEADER_FILE="${CMAKE_BINARY_DIR}/wxm_license.h"
        -DBIN2H="${CMAKE_SOURCE_DIR}/cmake-bin2h/bin2h.cmake"
        -P "${CMAKE_CURRENT_SOURCE_DIR}/bin2h.cmake"
        COMMENT "Embedding the license information"
        DEPENDS ${CMAKE_SOURCE_DIR}/COPYING ${CMAKE_CURRENT_SOURCE_DIR}/bin2h.cmake
)
add_custom_target(build_license.h DEPENDS ${CMAKE_BINARY_DIR}/wxm_license.h)
add_dependencies(wxmaxima build_license.h)

add_custom_command(
        OUTPUT ${CMAKE_BINARY_DIR}/wxm_manual_anchors_xml.h
        COMMAND ${CMAKE_COMMAND} ARGS
        -DBIN2H_SOURCE_FILE="${CMAKE_SOURCE_DIR}/data/manual_anchors.xml"
        -DBIN2H_VARIABLE_NAME=manual_anchors_xml
        -DBIN2H_HEADER_FILE="${CMAKE_BINARY_DIR}/wxm_manual_anchors_xml.h"
        -DBIN2H="${CMAKE_SOURCE_DIR}/cmake-bin2h/bin2h.cmake"
        -P "${CMAKE_CURRENT_SOURCE_DIR}/bin2h.cmake"
        COMMENT "Embedding the anchors of the maxima manual"
        DEPENDS ${CMAKE_SOURCE_DIR}/data/manual_anchors.xml ${CMAKE_CURRENT_SOURCE_DIR}/bin2h.cmake
)
add_custom_target(build_manualanchors.h DEPENDS ${CMAKE_BINARY_DIR}/wxm_manual_anchors_xml.h)
add_dependencies(wxmaxima build_manualanchors.h)


# Debian wants to be able to download a signature of the source package from the
# project's download directory. If it cannot find it it will still work, but will
# issue a warning. For details see
# https://lintian.debian.org/tags/debian-watch-may-check-gpg-signature.html
find_program(gpg NAMES gpg pgp)
add_custom_target(gpg DEPENDS dist
  COMMAND ${gpg} --armor --detach-sign ${CPACK_SOURCE_PACKAGE_FILE_NAME}.tar.bz2
  COMMAND ${gpg} --armor --detach-sign ${CPACK_SOURCE_PACKAGE_FILE_NAME}.tar.gz
  COMMAND ${gpg} --armor --detach-sign ${CPACK_SOURCE_PACKAGE_FILE_NAME}.tar.xz
  COMMAND ${gpg} --armor --detach-sign ${CPACK_SOURCE_PACKAGE_FILE_NAME}.tar.Z)

if(NOT WIN32)
    # 1. Generate and compress Debian changelog to changelog.gz
    configure_file(
        "${CMAKE_SOURCE_DIR}/data/changelog.Debian.in"
        "${CMAKE_CURRENT_BINARY_DIR}/changelog.Debian"
        @ONLY
    )
    add_custom_command(
        OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/changelog.gz"
        COMMAND gzip -cn9 "${CMAKE_CURRENT_BINARY_DIR}/changelog.Debian" > "${CMAKE_CURRENT_BINARY_DIR}/changelog.gz"
        DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/changelog.Debian"
        COMMENT "Compressing Debian changelog"
    )
    # Compress upstream ChangeLog to changelog.upstream.gz
    add_custom_command(
        OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/changelog.upstream.gz"
        COMMAND gzip -cn9 "${CMAKE_SOURCE_DIR}/ChangeLog" > "${CMAKE_CURRENT_BINARY_DIR}/changelog.upstream.gz"
        DEPENDS "${CMAKE_SOURCE_DIR}/ChangeLog"
        COMMENT "Compressing upstream changelog"
    )
    add_custom_target(generate_changelogs ALL DEPENDS
        "${CMAKE_CURRENT_BINARY_DIR}/changelog.gz"
        "${CMAKE_CURRENT_BINARY_DIR}/changelog.upstream.gz"
    )
    install(FILES
        "${CMAKE_CURRENT_BINARY_DIR}/changelog.gz"
        "${CMAKE_CURRENT_BINARY_DIR}/changelog.upstream.gz"
        DESTINATION share/doc/wxmaxima
    )

    # 2. Install copyright file
    install(FILES "${CMAKE_SOURCE_DIR}/data/copyright" DESTINATION share/doc/wxmaxima)

    # 3. Strip binaries
    set(CPACK_STRIP_FILES ON)

    # 4. Standard directory permissions to prevent 0775 permissions warnings
    set(CPACK_INSTALL_DEFAULT_DIRECTORY_PERMISSIONS
        OWNER_READ OWNER_WRITE OWNER_EXECUTE
        GROUP_READ GROUP_EXECUTE
        WORLD_READ WORLD_EXECUTE
    )

    # 5. Install lintian overrides
    install(FILES "${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima.lintian-overrides" DESTINATION share/lintian/overrides RENAME wxmaxima)
endif()

include(CPack)
