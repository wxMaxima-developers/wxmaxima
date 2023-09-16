// This file was originally posted to
// https://forums.wxwidgets.org/viewtopic.php?t=35124
// from mjuergens (mjx). It is under the wxWidgets license
// (https://www.wxwidgets.org/about/licence/) that is GPL-compatible.
//
// Added some modifications that remove MSVC code scanning warnings

#include "levenshtein.h"
#include <vector>

size_t LevenshteinDistance(const wxString &s1, const wxString &s2) {
  const size_t m = s1.Len();
  const size_t n = s2.Len();

  if (m == 0)
    return n;
  if (n == 0)
    return m;

  std::vector<size_t> costs;
  costs.resize(wxMax(m, n) + 1);
  
  for (size_t k = 0; k <= wxMax(m, n) + 1; k++)
    costs[k] = k;

  size_t i = 0;
  for (wxString::const_iterator it1 = s1.begin(); it1 != s1.end(); ++it1, ++i) {
    costs[0] = i + 1;
    size_t corner = i;

    size_t j = 0;
    for (wxString::const_iterator it2 = s2.begin(); it2 != s2.end();
         ++it2, ++j) {
      size_t upper = costs[j + 1];
      if (*it1 == *it2)
        costs[j + 1] = corner;
      else
        costs[j + 1] = wxMin(costs[j], wxMin(upper, corner)) + 1;

      corner = upper;
    }
  }

  size_t result = costs[n];

  return result;
}
