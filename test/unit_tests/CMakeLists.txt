# -*- mode: CMake; cmake-tab-width: 4; -*-

add_compile_options("$<$<CXX_COMPILER_ID:MSVC>:/utf-8>")

add_executable(test_stringutils test_stringutils.cpp)
target_link_libraries(test_stringutils PRIVATE ${wxWidgets_LIBRARIES})
add_test(stringutils test_stringutils)

add_executable(test_markdown test_markdown.cpp)
target_link_libraries(test_markdown PRIVATE ${wxWidgets_LIBRARIES})
add_test(markdown test_markdown)
