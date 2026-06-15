
file(READ "${INPUT_FILE}" FILE_CONTENTS)
string(REPLACE "nsvg" "wxm_nsvg" FILE_CONTENTS "${FILE_CONTENTS}")
string(REPLACE "NSVG" "wxm_NSVG" FILE_CONTENTS "${FILE_CONTENTS}")
string(REPLACE "nanosvg.h" "nanosvg_private.h" FILE_CONTENTS "${FILE_CONTENTS}")

# This is vendored, symbol-renamed third-party code (a private copy of nanoSVG so
# it can coexist with the nanoSVG bundled inside wxWidgets). Its compiler
# warnings are not wxMaxima bugs and would otherwise reappear in every
# translation unit that includes the generated header. Silence them: GCC/Clang
# treat the rest of the file as a system header; MSVC drops to warning level 0
# until the matching pop appended below.
set(WARNING_GUARD_TOP
"#if defined(__GNUC__) || defined(__clang__)
#pragma GCC system_header
#endif
#if defined(_MSC_VER)
#pragma warning(push, 0)
#endif
")
set(WARNING_GUARD_BOTTOM
"
#if defined(_MSC_VER)
#pragma warning(pop)
#endif
")
set(FILE_CONTENTS "${WARNING_GUARD_TOP}${FILE_CONTENTS}${WARNING_GUARD_BOTTOM}")
# string(REPLACE "nsvg__parseXML" "wxm_nsvg__parseXML" FILE_CONTENTS "${FILE_CONTENTS}")
# string(REPLACE "nsvgCreate" "wxm_nsvgCreate" FILE_CONTENTS "${FILE_CONTENTS}")
# string(REPLACE "nsvgDelete" "wxm_nsvgDelete" FILE_CONTENTS "${FILE_CONTENTS}")
# string(REPLACE "nsvgDuplicatePath" "wxm_nsvgDuplicatePath" FILE_CONTENTS "${FILE_CONTENTS}")
# string(REPLACE "nsvgParse" "wxm_nsvgParse" FILE_CONTENTS "${FILE_CONTENTS}")
# string(REPLACE "NSVGimage" "wxm_NSVGimage" FILE_CONTENTS "${FILE_CONTENTS}")
# string(REPLACE "nsvgRasterize" "wxm_nsvgRasterize" FILE_CONTENTS "${FILE_CONTENTS}")
# string(REPLACE "NSVGRasterize" "wxm_NSVGRasterize" FILE_CONTENTS "${FILE_CONTENTS}")
# string(REPLACE "NSVGNamedColor" "wxm_NSVGNamedColor" FILE_CONTENTS "${FILE_CONTENTS}")
file(WRITE ${OUTPUT_FILE} "${FILE_CONTENTS}")
