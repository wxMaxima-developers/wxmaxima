cmake_minimum_required(VERSION 2.8)
project(wxmaxima)

set(VERSION 17.04.0)
set(PACKAGE_VERSION ${VERSION})
set(GITVERSION ${VERSION})
set(PACKAGE wxMaxima)

# PREFIX should be defined, since it is used in the sourcecode and
# defined by the automake build system, but not defined by CMake by default.
add_definitions(-DPREFIX=\"${CMAKE_INSTALL_PREFIX}\")

add_subdirectory(locales)
add_subdirectory(src)
add_subdirectory(data)
add_subdirectory(info)
add_subdirectory(art)

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
