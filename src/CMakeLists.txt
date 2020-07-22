# -*- mode: CMake; cmake-tab-width: 4; -*-

set(SOURCE_FILES
    AbsCell.cpp
    ActualValuesStorageWiz.cpp
    AtCell.cpp
    Autocomplete.cpp
    AutocompletePopup.cpp
    Autocomplete_Builtins.cpp
    BC2Wiz.cpp
    BTextCtrl.cpp
    BitmapOut.cpp
    Cell.cpp
    CellPointers.cpp
    CellPtr.cpp
    CharButton.cpp
    CompositeDataObject.cpp
    ConfigDialogue.cpp
    Configuration.cpp
    ConjugateCell.cpp
    DiffCell.cpp
    Dirstructure.cpp
    DrawWiz.cpp
    EMFout.cpp
    EditorCell.cpp
    ErrorRedirector.cpp
    EvaluationQueue.cpp
    ExptCell.cpp
    FindReplaceDialog.cpp
    FindReplacePane.cpp
    FontAttribs.cpp
    FontCache.cpp
    FracCell.cpp
    FunCell.cpp
    Gen1Wiz.cpp
    Gen2Wiz.cpp
    Gen3Wiz.cpp
    Gen4Wiz.cpp
    Gen5Wiz.cpp
    GroupCell.cpp
    History.cpp
    Image.cpp
    ImgCell.cpp
    IntCell.cpp
    IntegrateWiz.cpp
    LicenseDialog.cpp
    LimitCell.cpp
    LimitWiz.cpp
    ListSortWiz.cpp
    LongNumberCell.cpp
    LogPane.cpp
    LoggingMessageDialog.cpp
    MainMenuBar.cpp
    MarkDown.cpp
    MatWiz.cpp
    MathParser.cpp
    MatrCell.cpp
    MaxSizeChooser.cpp
    MaximaIPC.cpp
    MaximaTokenizer.cpp
    Notification.cpp
    OutCommon.cpp
    ParenCell.cpp
    ListCell.cpp
    Plot2dWiz.cpp
    Plot3dWiz.cpp
    PlotFormatWiz.cpp
    Printout.cpp
    RecentDocuments.cpp
    SVGout.cpp
    SeriesWiz.cpp
    SlideShowCell.cpp
    SqrtCell.cpp
    StatusBar.cpp
    StringUtils.cpp
    SubCell.cpp
    SubSupCell.cpp
    SubstituteWiz.cpp
    SumCell.cpp
    SumWiz.cpp
    SvgBitmap.cpp
    SvgPanel.cpp
    SystemWiz.cpp
    TableOfContents.cpp
    TextCell.cpp
    TextStyle.cpp
    TipOfTheDay.cpp
    ToolBar.cpp
    UnicodeSidebar.cpp
    VariablesPane.cpp
    VisiblyInvalidCell.cpp
    WXMformat.cpp
    Worksheet.cpp
    XmlInspector.cpp
    levenshtein/levenshtein.cpp
    main.cpp
    wxImagePanel.cpp
    wxMathml.cpp
    wxMaxima.cpp
    wxMaximaFrame.cpp
    wxMaximaIcon.cpp
    Resources.rc
)

if(CMAKE_VERSION VERSION_GREATER_EQUAL "3.9.4")
    include(CheckIPOSupported)
    check_ipo_supported(RESULT lposupported)
endif()

if(CMAKE_VERSION VERSION_GREATER_EQUAL "3.10.0" AND WXM_USE_CPPCHECK)
    find_program(CMAKE_CXX_CPPCHECK NAMES cppcheck)
    if(CMAKE_CXX_CPPCHECK)
        message(STATUS "cppcheck found at ${CMAKE_CXX_CPPCHECK}. Enable cppcheck tests.")
	list(APPEND CMAKE_CXX_CPPCHECK
	    "--enable=warning,style,performance,portability"
	    "--quiet"
	    "--std=c++14"
	    "--force"
	    #"--inconclusive"
	    "--inline-suppr"
	    "--language=c++"
	    "--template=gcc"
	    "--suppressions-list=${CMAKE_CURRENT_SOURCE_DIR}/CppCheckSuppressions.txt"
	    "-i${CMAKE_CURRENT_SOURCE_DIR}/nanoSVG/"
	)
    else()
	message(STATUS "cppcheck not found.")
        set(CMAKE_CXX_CPPCHECK "")
    endif()
endif()

# We provide our own manifest. MSVC seems not to autodetect that.
if(MSVC)
  message(STATUS "MSVC detected => Trying to disable the inclusion of the automatic manifest hat suppresses high-dpi support.")
  set(CMAKE_EXE_LINKER_FLAGS_RELEASE    "${CMAKE_EXE_LINKER_FLAGS_RELEASE} /MANIFEST:NO")
  set(CMAKE_MODULE_LINKER_FLAGS_RELEASE "${CMAKE_MODULE_LINKER_FLAGS_RELEASE} /MANIFEST:NO")
  set(CMAKE_SHARED_LINKER_FLAGS_RELEASE "${CMAKE_SHARED_LINKER_FLAGS_RELEASE} /MANIFEST:NO")
  set(CMAKE_EXE_LINKER_FLAGS_DEBUG    "${CMAKE_EXE_LINKER_FLAGS_DEBUG} /MANIFEST:NO")
  set(CMAKE_MODULE_LINKER_FLAGS_DEBUG "${CMAKE_MODULE_LINKER_FLAGS_DEBUG} /MANIFEST:NO")
  set(CMAKE_SHARED_LINKER_FLAGS_DEBUG "${CMAKE_SHARED_LINKER_FLAGS_DEBUG} /MANIFEST:NO")
endif()

# Include our Resources file that contains our manifest
if(WIN32)
  include_directories(${CMAKE_SOURCE_DIR}/data/winrc)
  list(APPEND SOURCE_FILES ${CMAKE_CURRENT_BINARY_DIR}/Resources.rc)
endif()

# Version.h is generated in the binary dir
include_directories("${CMAKE_CURRENT_BINARY_DIR}")

file(GLOB RESOURCE_FILES
    ${CMAKE_SOURCE_DIR}/data/io.github.wxmaxima_developers.wxMaxima.png
    ${CMAKE_SOURCE_DIR}/data/io.github.wxmaxima_developers.wxMaxima.svg
    ${CMAKE_SOURCE_DIR}/art/wxmac.icns
    ${CMAKE_SOURCE_DIR}/art/wxmac-doc.icns
    ${CMAKE_SOURCE_DIR}/art/wxmac-doc-wxm.icns
    ${CMAKE_SOURCE_DIR}/art/wxmac-doc-wxmx.icns
    ${CMAKE_SOURCE_DIR}/info/*.html
    ${CMAKE_SOURCE_DIR}/info/*.jpg
    ${CMAKE_SOURCE_DIR}/info/*.png
    ${CMAKE_SOURCE_DIR}/data/PkgInfo
    ${CMAKE_SOURCE_DIR}/data/fonts/*.ttf
    ${CMAKE_SOURCE_DIR}/locales/wxwin/*.mo)

if(WIN32)
    add_executable(wxmaxima WIN32 ${SOURCE_FILES})
elseif(APPLE)
    set(MACOSX_BUNDLE_NAME wxmaxima)
    set(MACOSX_BUNDLE_ICON_FILE wxmac.icns)
    list(APPEND SOURCE_FILES ${RESOURCE_FILES})
    add_executable(wxmaxima ${SOURCE_FILES})
    set_target_properties(wxmaxima PROPERTIES RESOURCE "${RESOURCE_FILES}")
    if(APPLE)
        set_target_properties(wxmaxima PROPERTIES
            MACOSX_BUNDLE TRUE
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
        set_target_properties(wxmaxima PROPERTIES
            RESOURCE "${RESOURCE_FILES}"
    )
    endif()
else()
    add_executable(wxmaxima ${SOURCE_FILES})
endif()

if(WXM_ENABLE_PRECOMPILED_HEADERS AND CMAKE_VERSION VERSION_GREATER_EQUAL "3.16")
    target_precompile_headers(wxmaxima PUBLIC "precomp.h")
else()
    set(USE_PRECOMP_HEADER "0")
endif()

if(wxWidgets_VERSION_STRING VERSION_GREATER_EQUAL "3.1.3")
    set(MANIFEST_FILE "wx/msw/wx_dpi_aware.manifest")
else()
    set(MANIFEST_FILE "${CMAKE_SOURCE_DIR}/data/winrc/wxmaxima.manifest")
endif()

file(RELATIVE_PATH WINDOWS_ICON_DIR ${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/../data/winrc/)

configure_file("${CMAKE_CURRENT_SOURCE_DIR}/Resources.rc.in" Resources.rc)

# allow local execution (./wxmaxima-local) from the build directory without installation
if(WIN32)
    configure_file("${CMAKE_CURRENT_SOURCE_DIR}/wxmaxima-local.bat" ../wxmaxima-local.bat)
else()
    configure_file("${CMAKE_SOURCE_DIR}/src/wxmaxima-local" ../wxmaxima-local)
endif()

if(WXM_USE_OPENMP AND OpenMP_CXX_FOUND)
    target_link_libraries(wxmaxima OpenMP::OpenMP_CXX ${wxWidgets_LIBRARIES})
else()
    target_link_libraries(wxmaxima ${wxWidgets_LIBRARIES})
endif()

if(MINGW)
    target_link_libraries(wxmaxima -lws2_32)
endif()

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Version.h.cin ${CMAKE_CURRENT_BINARY_DIR}/Version.h)

if(APPLE)
    install(TARGETS wxmaxima
	     BUNDLE  DESTINATION .
	     RUNTIME DESTINATION bin
	     COMPONENT Runtime)
else()
    install(TARGETS wxmaxima RUNTIME DESTINATION bin)
endif()

# Build Packages
if(WIN32)
    set(CPACK_GENERATOR "ZIP;NSIS")
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
	message(STATUS "rpmbuild not found - no RPM package will be build with make package.")
    else()
	message(STATUS "rpmbuild found - RPM package can be build with make package.")
	list(APPEND CPACK_GENERATOR "RPM")
    endif()
endif()

set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "wxMaxima is a GUI for the CAS Maxima")
set(CPACK_PACKAGE_VENDOR "The wxMaxima Team")
set(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_SOURCE_DIR}/README.md")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/COPYING")
set(CPACK_SOURCE_PACKAGE_FILE_NAME "${CMAKE_PROJECT_NAME}-${VERSION}")
set(CPACK_PACKAGE_CONTACT "The wxMaxima Team <wxmaxima-devel@lists.sourceforge.net>")
set(CPACK_DEBIAN_PACKAGE_MAINTAINER "The wxMaxima Team <wxmaxima-devel@lists.sourceforge.net>")
set(CPACK_DEBIAN_PACKAGE_SECTION "math")
if(WIN32)
    set(CPACK_PACKAGE_ICON "${CMAKE_SOURCE_DIR}/data\\\\wxmaxima.bmp")
else()
    set(CPACK_PACKAGE_ICON "${CMAKE_SOURCE_DIR}/data/io.github.wxmaxima_developers.wxMaxima.png")
endif()
set(CPACK_PACKAGE_VERSION "${VERSION}")
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


include(CPack)

