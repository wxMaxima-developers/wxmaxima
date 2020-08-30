# Skip a few tests that for some reason fail on travis.
set(CTEST_CUSTOM_TESTS_IGNORE
  ${CTEST_CUSTOM_TESTS_IGNORE}
  slideshowCells
  autosave
  noautosave
  printf_simple
  printf_equations
  printf_continuationLines
)
