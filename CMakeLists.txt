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

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/data/wxmathml.lisp.in ${CMAKE_CURRENT_SOURCE_DIR}/data/wxmathml.lisp)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/src/Setup.h.cin ${CMAKE_CURRENT_SOURCE_DIR}/src/Setup.h)

if (APPLE)
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/data/Info.plist.in ${CMAKE_CURRENT_SOURCE_DIR}/data/Info.plist)
    add_custom_target(wxMaxima.app
        DEPENDS wxmaxima
        COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/make_app_bundle)
endif()
