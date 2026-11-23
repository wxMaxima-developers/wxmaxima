# Checks wxm_localise_manual_images() (info/LocaliseManualImages.cmake), which
# points a translated manual at its own screenshots in info/<lang>/.
#
# Run as: cmake -DSOURCE_DIR=<source dir> -DWORK_DIR=<scratch dir> -P <this file>
cmake_minimum_required(VERSION 3.16)
include("${SOURCE_DIR}/info/LocaliseManualImages.cmake")

set(dir "${WORK_DIR}/localised-manual-images")
file(REMOVE_RECURSE "${dir}")
file(MAKE_DIRECTORY "${dir}/de")
# The German set has two of the images; Logo.png stays English.
file(WRITE "${dir}/de/Window.png" "")
file(WRITE "${dir}/de/Input-Cell.png" "")

set(md "${dir}/wxmaxima.de.md")
# Like po4a's output: one id on the same line, one wrapped onto the next,
# one reference without an id, and an image the set doesn't have.
file(WRITE "${md}" "![Fenster](./Window.png){ id=img_Window }
![Eingabezelle](./Input-Cell.png){
id=img_Input-Cell }
Siehe ![](./Window.png) und ![Logo](./Logo.png){ id=img_Logo }
Nicht ./WindowXpng: ![x](./WindowXpng)
")

set(expected "![Fenster](de/Window.png){ id=de_img_Window }
![Eingabezelle](de/Input-Cell.png){
id=de_img_Input-Cell }
Siehe ![](de/Window.png) und ![Logo](./Logo.png){ id=img_Logo }
Nicht ./WindowXpng: ![x](./WindowXpng)
")

wxm_localise_manual_images("${md}" de "${dir}/de")
file(READ "${md}" result)
if(NOT result STREQUAL expected)
  message(FATAL_ERROR "Rewritten manual:\n${result}\nexpected:\n${expected}")
endif()

# Running it a second time (every configure does) must change nothing.
wxm_localise_manual_images("${md}" de "${dir}/de")
file(READ "${md}" result)
if(NOT result STREQUAL expected)
  message(FATAL_ERROR "A second run changed the manual again:\n${result}")
endif()

# A language without a screenshot set is left alone.
file(WRITE "${md}" "![Fenster](./Window.png){ id=img_Window }\n")
wxm_localise_manual_images("${md}" fr "${dir}/fr")
file(READ "${md}" result)
if(NOT result STREQUAL "![Fenster](./Window.png){ id=img_Window }\n")
  message(FATAL_ERROR "A language without images was changed:\n${result}")
endif()

file(REMOVE_RECURSE "${dir}")
message(STATUS "Localised manual images: OK")
