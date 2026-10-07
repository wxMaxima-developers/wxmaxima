# Checks that every dialog that has its size and position remembered by
# wxPersistenceManager sends its destroy event from its own destructor.
#
# The persistence manager saves the geometry when the window's destroy event
# arrives, treating the window as a wxTopLevelWindow. wxWidgets sends that
# event from a base class destructor -- on wxQt (3.3) from ~wxWindow, when the
# object isn't a wxTopLevelWindow any more -- so saving called a function
# through the wrong vtable and crashed every such dialog on closing. Frames
# are fine: ~wxFrameBase sends the event itself. See
# FindReplaceDialog::~FindReplaceDialog().
#
# The check is textual: a source file that calls RegisterAndRestore(this) and
# whose header declares N classes derived from a dialog class must contain at
# least N calls to SendDestroyEvent() in the header and source together.
# A CMake script so it runs on every platform without a build.
cmake_minimum_required(VERSION 3.16)

file(GLOB_RECURSE sources "${SOURCE_DIR}/src/*.cpp")
set(failures "")
foreach(cpp IN LISTS sources)
  file(READ "${cpp}" cppText)
  if(NOT cppText MATCHES "RegisterAndRestore\\(this\\)")
    continue()
  endif()
  string(REGEX REPLACE "\\.cpp$" ".h" header "${cpp}")
  if(NOT EXISTS "${header}")
    continue()
  endif()
  file(READ "${header}" headerText)
  string(REGEX MATCHALL "public (wxDialog|wxPropertySheetDialog|wxScrolled<wxDialog>)[^a-zA-Z_]"
         dialogs "${headerText}")
  list(LENGTH dialogs dialogCount)
  if(dialogCount EQUAL 0)
    continue()
  endif()
  string(REGEX MATCHALL "SendDestroyEvent\\(\\)" sends "${headerText}${cppText}")
  list(LENGTH sends sendCount)
  if(sendCount LESS dialogCount)
    string(APPEND failures
      "  ${header}: ${dialogCount} dialog class(es), "
      "${sendCount} SendDestroyEvent() call(s)\n")
  endif()
endforeach()

if(failures)
  message(FATAL_ERROR
    "Dialogs registered with wxPersistenceManager whose destructor doesn't "
    "call SendDestroyEvent() (they crash on closing with wxQt; see "
    "FindReplaceDialog::~FindReplaceDialog()):\n${failures}")
endif()
message(STATUS "Every persistent dialog sends its destroy event itself.")
