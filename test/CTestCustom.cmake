find_program(DESKTOP_FILE_VALIDATE_FOUND desktop-file-validate)
if(NOT DESKTOP_FILE_VALIDATE_FOUND)
  list(APPEND CTEST_CUSTOM_TESTS_IGNORE check_desktop_file)
endif()

find_program(APPSTREAM_UTIL_FOUND appstream-util)
if(NOT APPSTREAM_UTIL_FOUND)
  list(APPEND CTEST_CUSTOM_TESTS_IGNORE check_appstream_file)
endif()
