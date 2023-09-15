// This file was originally posted to
// https://forums.wxwidgets.org/viewtopic.php?t=35124
// from mjuergens (mjx). It is under the wxWidgets license
// (https://www.wxwidgets.org/about/licence/) that is GPL-compatible.
//
// Added some modifications that remove MSVC code scanning warnings

#include "levenshtein.h"

size_t LevenshteinDistance(const wxString &s1, const wxString &s2) {
  const size_t m = s1.Len();
  const size_t n = s2.Len();

  if (m == 0)
    return n;
  if (n == 0)
    return m;

  size_t *costs = new size_t[wxMax(m, n) + 1];

  for (int k = 0; k <= n; k++)
    costs[k] = k;

  int i = 0;
  for (wxString::const_iterator it1 = s1.begin(); it1 != s1.end(); ++it1, ++i) {
    costs[0] = i + 1;
    int corner = i;

    int j = 0;
    for (wxString::const_iterator it2 = s2.begin(); it2 != s2.end();
         ++it2, ++j) {
      int upper = costs[j + 1];
      if (*it1 == *it2)
        costs[j + 1] = corner;
      else
        costs[j + 1] = wxMin(costs[j], wxMin(upper, corner)) + 1;

      corner = upper;
    }
  }

  int result = costs[n];
  delete[] costs;

  return result;
}
