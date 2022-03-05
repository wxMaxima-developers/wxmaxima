include("${BIN2H}")

set(HEADER_FILE "wxMathML_lisp.cpp")
message(STATUS "Embedding wxMathML.lisp into header file ${HEADER_FILE}")
file(WRITE "${HEADER_FILE}"
  "/** \\file wxMathML_lisp.cpp\n"
  " * This header file was generated from wxMathML.lisp.\n"
  " */\n"
  " #include<wx/wx.h>\n"
  " #include <cstddef>\n"
  )
get_filename_component(variableName ${HEADER_FILE} NAME)
bin2h(SOURCE_FILE "${SRC}" HEADER_FILE ${HEADER_FILE} VARIABLE_NAME "wxMathML_lisp" APPEND NULL_TERMINATE)
