These files were downloaded from: https://github.com/sivachandran/cmake-bin2h
(Git commit: 3777f88)

# bin2h.cmake

Pure CMake function to convert any file into C/C++ header, implemented with only CMake commands.

## Example Usage

```cmake
include(bin2h.cmake)
...
message("Embedding following files into header file ${HEADER_FILE}")
foreach(file ${SOURCE_FILES})
    message("   ${file}")

    get_filename_component(variableName ${file} NAME)

    bin2h(SOURCE_FILE ${file} HEADER_FILE ${HEADER_FILE} VARIABLE_NAME ${variableName} APPEND NULL_TERMINATE)
    file(APPEND ${HEADER_FILE} "\n")
endforeach()
```
