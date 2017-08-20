cmake_minimum_required(VERSION 2.8)
project(wxmaxima)

set(VERSION 17.07.0)
set(PACKAGE_VERSION ${VERSION})
set(GITVERSION ${VERSION})
set(PACKAGE wxMaxima)


# Set a default build type if none was specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to 'Debug' as none was specified.")
  set(CMAKE_BUILD_TYPE Debug CACHE STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
    "MinSizeRel" "RelWithDebInfo")
endif()


if("${PROJECT_SOURCE_DIR}" STREQUAL "${PROJECT_BINARY_DIR}")
    message(WARNING "In-source builds are strongly discouraged. Please make an out-of-source-build instead (which means: run cmake and build the program from a different directory than the one the source package is in).")
endif()

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

if(WIN32)
    install(FILES COPYING README README.md DESTINATION wxMaxima/doc)
    install(FILES test/testbench_simple.wxmx test/a.png test/b.png test/c.png test/d.png DESTINATION wxMaxima/data)
else()
    install(FILES COPYING README README.md DESTINATION share/wxMaxima)
    install(FILES test/testbench_simple.wxmx test/a.png test/b.png test/c.png test/d.png DESTINATION share/wxMaxima)
endif()


# Build Packages
set(CPACK_GENERATOR "TGZ;TBZ2;DEB")

# build RPMs only if rpmbuild is installed
find_program(RPMBUILD_EXECUTABLE rpmbuild)
if(NOT RPMBUILD_EXECUTABLE)
    message(STATUS "rpmbuild not found - no RPM package will be build with make package.")
else()
    message(STATUS "rpmbuild found - RPM package can be build with make package.")
    list(APPEND  CPACK_GENERATOR "RPM")
endif()

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
set(CPACK_DEBIAN_PACKAGE_DEPENDS "maxima, maxima-doc")

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

find_program(sha1sum   NAMES sha1sum)
find_program(sha256sum NAMES sha256sum)
find_program(sha512sum NAMES sha512sum)
file(WRITE "${CMAKE_BINARY_DIR}/create-checksums.sh" "#!/bin/sh\n")
# create checksums for binary packages
# "make package" must be run before "make checksums" to get them.
foreach(ext "tar.gz" "tar.bz2" "deb" "rpm")
    file(APPEND "${CMAKE_BINARY_DIR}/create-checksums.sh" "test -f wxmaxima-${VERSION}-${CMAKE_SYSTEM_NAME}.${ext} && ${CMAKE_COMMAND} -E md5sum wxmaxima-${VERSION}-${CMAKE_SYSTEM_NAME}.${ext} >> checksums-md5.txt\n")
    file(APPEND "${CMAKE_BINARY_DIR}/create-checksums.sh" "test -f wxmaxima-${VERSION}-${CMAKE_SYSTEM_NAME}.${ext} && ${sha1sum} wxmaxima-${VERSION}-${CMAKE_SYSTEM_NAME}.${ext} >> checksums-sha1.txt\n")
    file(APPEND "${CMAKE_BINARY_DIR}/create-checksums.sh" "test -f wxmaxima-${VERSION}-${CMAKE_SYSTEM_NAME}.${ext} && ${sha256sum} wxmaxima-${VERSION}-${CMAKE_SYSTEM_NAME}.${ext} >> checksums-sha256.txt\n")
    file(APPEND "${CMAKE_BINARY_DIR}/create-checksums.sh" "test -f wxmaxima-${VERSION}-${CMAKE_SYSTEM_NAME}.${ext} && ${sha512sum} wxmaxima-${VERSION}-${CMAKE_SYSTEM_NAME}.${ext} >> checksums-sha512.txt\n")
endforeach()

# create checksums for source packages
# "make package_source" must be run before "make checksums" to get them.
foreach(ext "tar.gz" "tar.bz2" "tar.xz" "tar.Z")
    file(APPEND "${CMAKE_BINARY_DIR}/create-checksums.sh" "test -f wxmaxima-${VERSION}.${ext} && ${CMAKE_COMMAND} -E md5sum wxmaxima-${VERSION}.${ext} >> checksums-md5.txt\n")
    file(APPEND "${CMAKE_BINARY_DIR}/create-checksums.sh" "test -f wxmaxima-${VERSION}.${ext} && ${sha1sum} wxmaxima-${VERSION}.${ext} >> checksums-sha1.txt\n")
    file(APPEND "${CMAKE_BINARY_DIR}/create-checksums.sh" "test -f wxmaxima-${VERSION}.${ext} && ${sha256sum} wxmaxima-${VERSION}.${ext} >> checksums-sha256.txt\n")
    file(APPEND "${CMAKE_BINARY_DIR}/create-checksums.sh" "test -f wxmaxima-${VERSION}.${ext} && ${sha512sum} wxmaxima-${VERSION}.${ext} >> checksums-sha512.txt\n")
endforeach()
file(APPEND "${CMAKE_BINARY_DIR}/create-checksums.sh" "exit 0\n")

add_custom_target(checksums COMMAND chmod +x ${CMAKE_BINARY_DIR}/create-checksums.sh
                            COMMAND ${CMAKE_BINARY_DIR}/create-checksums.sh)

include(CPack)
