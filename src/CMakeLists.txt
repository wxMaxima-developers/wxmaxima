# -*- mode: CMake; cmake-tab-width: 4; -*-

# The result of the following command is only re-generated on
# CMakeLists.txt changes
file(GLOB SOURCE_FILES *.cpp *.h nanoSVG/*.h)

# VERSION_GREATER_EQUAL would normally be the way to go here.
# But in Nov 2012 some Ubuntu versions still use a cmake that
# doesn't support it.
# if(${CMAKE_VERSION} VERSION_GREATER_EQUAL "3.10.0")
# As the top-level cmakelists.txt refuses to configure on cmake <3.0.0
# The version test can be simple.
if(CMAKE_MAJOR_VERSION GREATER 3 OR CMAKE_MINOR_VERSION GREATER 9)
    find_program(CMAKE_CXX_CPPCHECK NAMES cppcheck)
    if(CMAKE_CXX_CPPCHECK)
        message(STATUS "cppcheck found at ${CMAKE_CXX_CPPCHECK}. Enable cppcheck tests.")
	list(APPEND CMAKE_CXX_CPPCHECK
	    "--enable=warning,style,performance,portability"
	    "--quiet"
	    "--std=c++11"
	    "--force"
	    #"--inconclusive"
	    "--inline-suppr"
	    "--language=c++"
	    "--template=gcc"
	    "--suppressions-list=${CMAKE_CURRENT_SOURCE_DIR}/CppCheckSuppressions.txt"
	)
    else()
	message(STATUS "cppcheck not found.")
        set(CMAKE_CXX_CPPCHECK "")
    endif()
else()
    message(STATUS "Cmake too old to support cppcheck.")
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
  set(SOURCE_FILES Resources.rc ${SOURCE_FILES})
endif()

# We put Version.h into binary dir
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
    ${CMAKE_SOURCE_DIR}/info/*.hhp
    ${CMAKE_SOURCE_DIR}/data/PkgInfo
    ${CMAKE_SOURCE_DIR}/data/fonts/*.ttf
    ${CMAKE_SOURCE_DIR}/locales/wxwin/*.mo)


SET(MACOSX_BUNDLE_BUNDLE_NAME wxMaxima)

if(WIN32)
    add_executable(wxmaxima WIN32 ${SOURCE_FILES})
    install(TARGETS wxmaxima RUNTIME DESTINATION bin)
elseif(APPLE)
  list(APPEND SOURCE_FILES ${MACOS_RESOURCES_FILES})
  add_executable(wxmaxima MACOSX_BUNDLE ${SOURCE_FILES})
  set_target_properties(wxmaxima PROPERTIES
      RESOURCE "${RESOURCE_FILES}"
      MACOSX_BUNDLE TRUE
      MACOSX_FRAMEWORK_IDENTIFIER com.github.wxMaxima-developers.wxmaxima
      )
    install(
	TARGETS wxmaxima
	BUNDLE  DESTINATION . COMPONENT Runtime
	RUNTIME DESTINATION bin COMPONENT Runtime
	)
  set(APPS "\${CMAKE_CURRENT_BINARY_DIR}/wxmaxima.app")
  install(CODE "
   include(BundleUtilities)
   fixup_bundle(\"${APPS}\"   \"\"   \"\")
   " COMPONENT Runtime)
else()
    add_executable(wxmaxima ${SOURCE_FILES})
    install(TARGETS wxmaxima RUNTIME DESTINATION bin)
endif()

get_target_property(EXEPATH wxmaxima BINARY_DIR)
get_target_property(RUNTIME wxmaxima RUNTIME_OUTPUT_DIRECTORY)
get_target_property(OUTPUTNAME wxmaxima OUTPUT_NAME)

# allow local execution (./wxmaxima-local) from the build directory without installation
configure_file("${CMAKE_SOURCE_DIR}/src/wxmaxima-local" ../wxmaxima-local)
if(WIN32)
    configure_file("${CMAKE_SOURCE_DIR}/src/wxmaxima-local.bat" ../wxmaxima-local.bat)
endif()


target_link_libraries(wxmaxima ${wxWidgets_LIBRARIES})

include(CheckIncludeFiles)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Version.h.cin ${CMAKE_CURRENT_BINARY_DIR}/Version.h)
