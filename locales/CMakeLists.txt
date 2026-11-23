# -*- mode: CMake; cmake-tab-width: 4; -*-

add_subdirectory(wxMaxima)
# Unconditionally: manual/ only needs po4a and gettext, not pandoc. It used to
# be added only if pandoc was found, which silently removed the
# update-locale-manual-in-source target from the weekly update_translations
# job (its container has no pandoc), so manual translations made in
# locales/wxMaxima/*.po never reached locales/manual/*.po.
add_subdirectory(manual)
