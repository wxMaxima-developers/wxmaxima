# -*- mode: CMake; cmake-tab-width: 4; -*-

file(GLOB SOURCE_FILES *.cpp *.h)

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
  set(SOURCE_FILES Resources.rc ${SOURCE_FILES})
endif()

# We put Version.h into binary dir
include_directories("${CMAKE_CURRENT_BINARY_DIR}")

# OSX binary folders
if(APPLE)
  # DEFINE FILES
  file(GLOB RESOURCES_FILES
    ${CMAKE_BINARY_DIR}/data/wxmathml.lisp
    ${CMAKE_SOURCE_DIR}/test/testbench_simple.wxmx
    ${CMAKE_SOURCE_DIR}/data/io.github.wxmaxima_developers.wxMaxima.png
    ${CMAKE_SOURCE_DIR}/data/io.github.wxmaxima_developers.wxMaxima.svg)
  file(GLOB RESOURCES_FILES_OSX
    ${CMAKE_SOURCE_DIR}/art/wxmac.icns
    ${CMAKE_SOURCE_DIR}/art/wxmac-doc.icns
    ${CMAKE_SOURCE_DIR}/art/wxmac-doc-wxm.icns
    ${CMAKE_SOURCE_DIR}/art/wxmac-doc-wxmx.icns)
  file(GLOB HELP_FILES
    ${CMAKE_SOURCE_DIR}/info/*.html
    ${CMAKE_SOURCE_DIR}/info/*.jpg
    ${CMAKE_SOURCE_DIR}/info/*.png
    ${CMAKE_SOURCE_DIR}/info/*.hhp)
  file(GLOB CONTENT_FILES
    ${CMAKE_BINARY_DIR}/data/Info.plist
    ${CMAKE_SOURCE_DIR}/data/PkgInfo)
  file(GLOB FONT_FILES
    ${CMAKE_SOURCE_DIR}/data/fonts/*.ttf)
  file(GLOB WXSTD_LOCALE_FILES
    ${CMAKE_SOURCE_DIR}/locales/wxwin/*.mo)
    # DEFINE DIRECTORIES
    set(APP_DIR ${CMAKE_BINARY_DIR}/wxMaxima.app/Contents)
    set(APP_MACOSX_DIR ${APP_DIR}/MacOS)
    set(APP_RESOURCES_DIR ${APP_DIR}/Resources)
    set(APP_HELP_DIR ${APP_RESOURCES_DIR}/help)
    set(APP_FONTS_DIR ${APP_RESOURCES_DIR}/fonts)
    set(APP_LOCALE_DIR ${APP_RESOURCES_DIR}/locale)
    # CUSTOM TARGET
    add_custom_target(wxMaxima.app)
    add_custom_command(TARGET wxMaxima.app PRE_BUILD
      COMMAND ${CMAKE_COMMAND} -E make_directory ${APP_MACOSX_DIR})
    add_custom_command(TARGET wxMaxima.app PRE_BUILD
      COMMAND ${CMAKE_COMMAND} -E make_directory ${APP_HELP_DIR})
    add_custom_command(TARGET wxMaxima.app
      COMMAND ${CMAKE_COMMAND} -E make_directory ${APP_FONTS_DIR})
    foreach(f ${CONTENT_FILES})
      add_custom_command(TARGET wxMaxima.app
        COMMAND ${CMAKE_COMMAND} -E copy ${f}  ${APP_DIR})
    endforeach()
    foreach(f ${RESOURCES_FILES})
      add_custom_command(TARGET wxMaxima.app
        COMMAND ${CMAKE_COMMAND} -E copy ${f}  ${APP_RESOURCES_DIR})
    endforeach()
    foreach(f ${RESOURCES_FILES_OSX})
      add_custom_command(TARGET wxMaxima.app
        COMMAND ${CMAKE_COMMAND} -E copy ${f}  ${APP_RESOURCES_DIR})
    endforeach()
    foreach(f ${HELP_FILES})
      add_custom_command(TARGET wxMaxima.app
        COMMAND ${CMAKE_COMMAND} -E copy ${f}  ${APP_HELP_DIR})
    endforeach()
    foreach(f ${FONT_FILES})
      add_custom_command(TARGET wxMaxima.app
        COMMAND ${CMAKE_COMMAND} -E copy ${f} ${APP_FONTS_DIR})
    endforeach()
    add_custom_command(TARGET wxMaxima.app
      COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_BINARY_DIR}/locale ${APP_LOCALE_DIR})
    foreach(f ${WXSTD_LOCALE_FILES})
      string(REGEX REPLACE ".*locales/wxwin/(.*).mo$" "\\1" LANG ${f})
      add_custom_command(TARGET wxMaxima.app
        COMMAND ${CMAKE_COMMAND} -E copy ${f} ${APP_LOCALE_DIR}/${LANG}/LC_MESSAGES/wxMaxima-wxstd.mo)
    endforeach()
endif()

if(WIN32)
    add_executable(wxmaxima WIN32 ${SOURCE_FILES})
elseif(APPLE)
  add_executable(wxmaxima ${SOURCE_FILES})

    install(TARGETS wxmaxima
      BUNDLE DESTINATION . COMPONENT Runtime
      RUNTIME DESTINATION bin COMPONENT Runtime
      )
    set_target_properties(wxmaxima PROPERTIES
              RUNTIME_OUTPUT_DIRECTORY_DEBUG   ${CMAKE_BINARY_DIR}/wxMaxima.app/Contents/MacOS
              RUNTIME_OUTPUT_DIRECTORY_RELEASE ${CMAKE_BINARY_DIR}/wxMaxima.app/Contents/MacOS
              MACOSX_BUNDLE_INFO_PLIST ${CMAKE_BINARY_DIR}/Info.plist
              RESOURCE "${ALLRERESOURCES}"
              )
    # make wxmathml.lisp ship with the Mac App
    add_dependencies(wxmaxima wxMaxima.app)
else()
    add_executable(wxmaxima ${SOURCE_FILES})
endif()

get_target_property(EXEPATH wxmaxima BINARY_DIR)
get_target_property(RUNTIME wxmaxima RUNTIME_OUTPUT_DIRECTORY)
get_target_property(OUTPUTNAME wxmaxima OUTPUT_NAME)
get_target_property(MACOSX_BUNDLE_INFO_PLIST wxmaxima ${CMAKE_SOURCE_DIR}/data/info.plist)

# allow local execution (./wxmaxima-local) from the build directory without installation
configure_file("${CMAKE_SOURCE_DIR}/src/wxmaxima-local" ../wxmaxima-local)
if(WIN32)
    configure_file("${CMAKE_SOURCE_DIR}/src/wxmaxima-local.bat" ../wxmaxima-local.bat)
endif()


target_link_libraries(wxmaxima ${wxWidgets_LIBRARIES})

include(CheckIncludeFiles)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Version.h.cin ${CMAKE_CURRENT_BINARY_DIR}/Version.h)

install(TARGETS wxmaxima RUNTIME DESTINATION bin)
