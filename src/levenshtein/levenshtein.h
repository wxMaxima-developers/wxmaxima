// This file was originally posted to
// https://forums.wxwidgets.org/viewtopic.php?t=35124
// from mjuergens (mjx). It is under the wxWidgets license
// (https://www.wxwidgets.org/about/licence/) that is GPL-compatible.
//
// Added some modifications that remove MSVC code scanning warnings
#ifndef _LEVENSHTEIN_
#define _LEVENSHTEIN_

#include <wx/wx.h>

size_t LevenshteinDistance(const wxString &s1, const wxString &s2);

#endif
