# Checks that every string meant for translation can reach the translators.
#
# xgettext only extracts a string literal that is the direct argument of a
# keyword macro. _(wxS("...")) compiles and runs, but its argument is a call,
# so xgettext silently skips it: thirteen such strings, among them menu
# entries and error messages, never made it into wxMaxima.pot. A literal
# with characters outside ASCII goes through TR_() (StringUtils.h) instead,
# which is only extracted while xgettext is told about that keyword.
# A CMake script so it runs on every platform without a build.
cmake_minimum_required(VERSION 3.16)

file(GLOB_RECURSE sources "${SOURCE_DIR}/src/*.cpp" "${SOURCE_DIR}/src/*.h")
set(failures "")
foreach(f IN LISTS sources)
  file(STRINGS "${f}" lines REGEX "_\\((wxS|wxT)\\(")
  foreach(line IN LISTS lines)
    file(RELATIVE_PATH rel "${SOURCE_DIR}" "${f}")
    string(STRIP "${line}" line)
    string(APPEND failures "  ${rel}: ${line}\n")
  endforeach()
endforeach()
if(failures)
  message(FATAL_ERROR
    "xgettext doesn't extract _(wxS(...)) or _(wxT(...)), so these strings "
    "are never translated. Write _(\"...\"), or TR_(\"...\") for a literal "
    "with characters outside ASCII:\n${failures}")
endif()

file(READ "${SOURCE_DIR}/locales/wxMaxima/CMakeLists.txt" localesCMake)
if(NOT localesCMake MATCHES "-kTR_")
  message(FATAL_ERROR
    "locales/wxMaxima/CMakeLists.txt no longer passes -kTR_ to xgettext, so "
    "no TR_(\"...\") string would reach the translators.")
endif()
message(STATUS "Every translatable literal is visible to xgettext.")
