# Checks that every ID a MaximaCommandMenus handler reacts to is bound to
# some handler.
#
# MaximaCommandMenus' handlers are long chains of
# "if(event.GetId() == EventIDs::<id>)". A branch whose ID nothing Bind()s
# is never reached: the menu entry or keyboard shortcut silently does
# nothing. That is how "Factor Expression" and "Find Root..." in the
# worksheet's context menu, and the Ctrl+Shift+Esc / Ctrl+Shift+1...6
# shortcuts that convert a cell to another type, came to do nothing.
#
# The check is textual: each such ID has to appear in a Bind() call
# somewhere under src/, or in the wizard catalog (whose IDs wxMaxima binds in
# a loop). A CMake script so it runs on every platform without a build.
cmake_minimum_required(VERSION 3.16)

file(READ "${SOURCE_DIR}/src/MaximaCommandMenus.cpp" handlers)
string(REGEX MATCHALL "GetId\\(\\) *== *EventIDs::[A-Za-z0-9_]+" handled
       "${handlers}")
list(TRANSFORM handled REPLACE ".*EventIDs::" "")
list(REMOVE_DUPLICATES handled)

file(GLOB_RECURSE sources "${SOURCE_DIR}/src/*.cpp")
set(bound "")
foreach(cpp IN LISTS sources)
  file(READ "${cpp}" text)
  # A Bind() call can span lines; match up to the semicolon that ends it.
  string(REGEX MATCHALL "Bind\\([^;]*;" binds "${text}")
  foreach(bind IN LISTS binds)
    string(REGEX MATCHALL "EventIDs::[A-Za-z0-9_]+" ids "${bind}")
    list(APPEND bound ${ids})
  endforeach()
endforeach()
file(READ "${SOURCE_DIR}/src/wizards/WizardCatalog.cpp" catalog)
string(REGEX MATCHALL "EventIDs::[A-Za-z0-9_]+" catalogIds "${catalog}")
list(APPEND bound ${catalogIds})
list(TRANSFORM bound REPLACE "EventIDs::" "")
list(REMOVE_DUPLICATES bound)

set(failures "")
foreach(id IN LISTS handled)
  if(NOT id IN_LIST bound)
    string(APPEND failures "  EventIDs::${id}\n")
  endif()
endforeach()

if(failures)
  message(FATAL_ERROR
    "MaximaCommandMenus.cpp handles these IDs, but nothing binds them to a "
    "handler, so their menu entries or shortcuts do nothing (add a Bind() "
    "in wxMaxima.cpp):\n${failures}")
endif()
list(LENGTH handled count)
message(STATUS "All ${count} IDs MaximaCommandMenus handles are bound.")
