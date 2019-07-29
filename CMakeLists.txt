project(wxmaxima)
set(VERSION 19.07.0-DevelopmentSnapshot)
set(PACKAGE_VERSION ${VERSION})
set(GITVERSION ${VERSION})
set(PACKAGE wxMaxima)

#SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pg")
#SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -pg")
#SET(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -pg")

# Avoid a warning by deciding which version of this policy we use in case we can
# decide this.
if(POLICY CMP0066)
    cmake_policy(SET CMP0066 NEW)
endif()
 
cmake_minimum_required(VERSION 3.4)

# Turn off many warnings wxWidgets triggers on MSVC
if(MSVC)
    add_definitions(-D_CRT_SECURE_NO_WARNINGS)
endif()

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

# MacOSX version-min compiler settings
if(MACOSX_VERSION_MIN)
    message(STATUS "Compiling with min macosx version ${MACOSX_VERSION_MIN}")
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -mmacosx-version-min=${MACOSX_VERSION_MIN}")
    set(CMAKE_EXE_LINKER_FLAGS_DEBUG "${CMAKE_LINKER_FLAGS_DEBUG} -mmacosx-version-min=${MACOSX_VERSION_MIN}")
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -mmacosx-version-min=${MACOSX_VERSION_MIN}")
    set(CMAKE_EXE_LINKER_FLAGS_RELEASE "${CMAKE_LINKER_FLAGS_RELEASE} -mmacosx-version-min=${MACOSX_VERSION_MIN}")
endif()

# Get the git version, if available.
find_package(Git)
if(Git_FOUND)
    if(EXISTS "${CMAKE_SOURCE_DIR}/.git")
        execute_process(COMMAND ${GIT_EXECUTABLE} rev-parse --short HEAD
                        WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
                        OUTPUT_VARIABLE WXMAXIMA_GIT_VERSION
                        OUTPUT_STRIP_TRAILING_WHITESPACE)
        message(STATUS "Building from git development tree, revision: ${WXMAXIMA_GIT_VERSION}")
        add_definitions("-DWXMAXIMA_GIT_VERSION=\"${WXMAXIMA_GIT_VERSION}\"")
    endif()
endif()


add_subdirectory(locales)
add_subdirectory(Doxygen)
add_subdirectory(data)
add_subdirectory(info)
add_subdirectory(src)

if(WIN32)
    install(FILES COPYING README README.md DESTINATION wxMaxima/doc)
else()
    install(FILES COPYING README README.md DESTINATION share/wxMaxima)
endif()


# allow local execution (./wxmaxima-local) from the build directory without installation
if(UNIX)
    configure_file("${CMAKE_SOURCE_DIR}/src/wxmaxima-local" wxmaxima-local)
endif()
# Build Packages
set(CPACK_GENERATOR "TGZ;TBZ2")
if(UNIX AND NOT APPLE)
    list(APPEND CPACK_GENERATOR "DEB") # Don't build DEB packages on MacOS
endif()

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
set(CPACK_PACKAGE_ICON "${CMAKE_SOURCE_DIR}/data/io.github.wxmaxima_developers.wxMaxima.png")
set(CPACK_PACKAGE_VERSION "${VERSION}")


set(CPACK_DEBIAN_PACKAGE_SHLIBDEPS ON)
set(CPACK_DEBIAN_PACKAGE_DEPENDS "maxima, maxima-doc")
set(CPACK_DEBIAN_PACKAGE_SUGGESTS "jsmath-fonts")

set(CPACK_RPM_PACKAGE_SUGGESTS "jsmath-fonts")

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


add_custom_target(checksums
    ${CMAKE_COMMAND} -DVERSION=${VERSION} -DCMAKE_SYSTEM_NAME=${CMAKE_SYSTEM_NAME} -P ${CMAKE_SOURCE_DIR}/checksums.cmake)

include(CPack)

enable_testing()
add_subdirectory(test)
