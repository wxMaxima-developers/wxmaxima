cmake_minimum_required(VERSION 2.8)
project(wxmaxima)

set(VERSION 17.04.0)
set(PACKAGE_VERSION ${VERSION})
set(GITVERSION ${VERSION})
set(PACKAGE wxMaxima)

# should the PDF doku be build from the texinfo source? (requires a TeX installation)
option(BUILD_PDF_DOCUMENTATION "Build the PDF documentation." NO)

# PREFIX should be defined, since it is used in the sourcecode and
# defined by the automake build system, but not defined by CMake by default.
add_definitions(-DPREFIX=\"${CMAKE_INSTALL_PREFIX}\")

# MacOSX version-min compiler settings
if(MACOSX_VERSION_MIN)
    message(STATUS "Compiling with min macosx version ${MACOSX_VERSION_MIN}")
    set(CMAKE_CXX_FLAGS -mmacosx-version-min=${MACOSX_VERSION_MIN})
    set(CMAKE_EXE_LINKER_FLAGS -mmacosx-version-min=${MACOSX_VERSION_MIN})
endif()

add_subdirectory(locales)
add_subdirectory(src)
add_subdirectory(Doxygen)
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
set(CPACK_PACKAGE_ICON ${CMAKE_SOURCE_DIR}/data/wxmaxima.png)
set(CPACK_PACKAGE_VERSION "${VERSION}")

set(CPACK_DEBIAN_PACKAGE_SHLIBDEPS ON)
set(CPACK_DEBIAN_PACKAGE_DEPENDS "maxima, maxima-doc, imagemagick")

# Build a tarball
set(CPACK_SOURCE_IGNORE_FILES
  "build/"
  ".git/"
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
add_custom_target(dist COMMAND ${CMAKE_MAKE_PROGRAM} package_source)

add_custom_target(checksums COMMAND test -f wxmaxima-${VERSION}-${CMAKE_SYSTEM_NAME}.tar.gz && ${CMAKE_COMMAND} -E md5sum wxmaxima-${VERSION}-${CMAKE_SYSTEM_NAME}.tar.gz >checksums.txt
                            COMMAND test -f wxmaxima-${VERSION}-${CMAKE_SYSTEM_NAME}.tar.bz2 && ${CMAKE_COMMAND} -E md5sum wxmaxima-${VERSION}-${CMAKE_SYSTEM_NAME}.tar.bz2 >>checksums.txt
                            COMMAND test -f wxmaxima-${VERSION}-${CMAKE_SYSTEM_NAME}.deb && ${CMAKE_COMMAND} -E md5sum wxmaxima-${VERSION}-${CMAKE_SYSTEM_NAME}.deb >>checksums.txt
                            COMMAND test -f wxmaxima-${VERSION}-${CMAKE_SYSTEM_NAME}.rpm && ${CMAKE_COMMAND} -E md5sum wxmaxima-${VERSION}-${CMAKE_SYSTEM_NAME}.rpm >>checksums.txt)
include(CPack)
