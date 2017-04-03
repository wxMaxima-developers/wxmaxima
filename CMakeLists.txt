cmake_minimum_required(VERSION 3.7)
project(wxmaxima)

set(VERSION 17.04.0)
set(PACKAGE_VERSION ${VERSION})
set(GITVERSION ${VERSION})
set(PACKAGE wxMaxima)

# PREFIX should be defined, since it is used in the sourcecode and
# defined by the automake build system, but not defined by CMake by default.
add_definitions (-DPREFIX=\"${CMAKE_INSTALL_PREFIX}\")

add_subdirectory(locales)
add_subdirectory(src)
add_subdirectory(data)
add_subdirectory(info)
add_subdirectory(art)

if (APPLE)
    add_custom_target(wxMaxima.app
        DEPENDS wxmaxima
            COMMAND mkdir -p wxMaxima.app/Contents/MacOS
            COMMAND mkdir -p wxMaxima.app/Contents/Resources
            COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/test/testbench_simple.wxmx wxMaxima.app/Contents/Resources
            COMMAND cp ${CMAKE_CURRENT_BINARY_DIR}/data/wxmathml.lisp wxMaxima.app/Contents/Resources
            COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/data/wxmaxima.png wxMaxima.app/Contents/Resources
            COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/data/wxmaxima.svg wxMaxima.app/Contents/Resources
            COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/data/autocomplete.txt wxMaxima.app/Contents/Resources
            COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/data/tips*.txt wxMaxima.app/Contents/Resources
            COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/art/wxmac.icns wxMaxima.app/Contents/Resources
            COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/art/wxmac-doc.icns wxMaxima.app/Contents/Resources
            COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/art/wxmac-doc-wxm.icns wxMaxima.app/Contents/Resources
            COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/art/wxmac-doc-wxmx.icns wxMaxima.app/Contents/Resources
            COMMAND cp ${CMAKE_CURRENT_BINARY_DIR}/data/Info.plist wxMaxima.app/Contents
            COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/data/PkgInfo wxMaxima.app/Contents
            COMMAND cp ${CMAKE_CURRENT_BINARY_DIR}/src/${CMAKE_BUILD_TYPE}/wxmaxima wxMaxima.app/Contents/MacOS
            COMMAND mkdir -p wxMaxima.app/Contents/Resources/toolbar
            COMMAND mkdir -p wxMaxima.app/Contents/Resources/statusbar
            COMMAND mkdir -p wxMaxima.app/Contents/Resources/config
            COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/art/toolbar/*.png wxMaxima.app/Contents/Resources/toolbar
            COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/art/statusbar/*.png wxMaxima.app/Contents/Resources/statusbar
            COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/art/config/*.png wxMaxima.app/Contents/Resources/config
            COMMAND mkdir -p wxMaxima.app/Contents/Resources/help
            COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/info/*.html wxMaxima.app/Contents/Resources/help
            COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/info/*.jpg  wxMaxima.app/Contents/Resources/help
            COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/info/*.png  wxMaxima.app/Contents/Resources/help
            COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/info/wxmaxima.hhp wxMaxima.app/Contents/Resources/help)

endif()

install(FILES COPYING README README.md DESTINATION share/wxMaxima)
install(FILES test/testbench_simple.wxmx DESTINATION share/wxMaxima)

# Build Packages
set(CPACK_GENERATOR "TGZ;TBZ2;DEB;RPM")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "wxMaxima is a GUI for the CAS Maxima")
set(CPACK_PACKAGE_VENDOR "The wxMaxima Team")
set(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_SOURCE_DIR}/README.md")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/COPYING")
set(CPACK_SOURCE_PACKAGE_FILE_NAME "${CMAKE_PROJECT_NAME}-${VERSION}")
set(CPACK_PACKAGE_CONTACT "The wxMaxima Team <wxmaxima-devel@lists.sourceforge.net>")
set(CPACK_PACKAGE_SECTION "science")
set(CPACK_PACKAGE_VERSION "${VERSION}")
include(CPack)
