// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2026 Gunter Königsmann <wxMaxima@physikbuch.de>
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  Implements the link detection declared in UrlDetection.h.
*/

#include "UrlDetection.h"
#include <wx/crt.h>
#include <cwchar>
#include <string>

namespace wxm {

namespace {

//! The link prefixes we recognize, lower case.
const wchar_t *const kUrlSchemes[] = {L"https://", L"http://", L"mailto:"};

//! Can this character be part of a link?
bool IsUrlChar(wchar_t ch) {
  if (ch <= 0x20 || ch == 0x7F || ch == 0xA0 || wxIsspace(ch))
    return false;
  switch (ch) {
  case L'<': case L'>': case L'"': case L'{': case L'}':
  case L'|': case L'\\': case L'^': case L'`':
    return false;
  default:
    return true;
  }
}

//! Does text contain scheme (lower case) at pos, ignoring case?
bool UrlStartsWithSchemeAt(const std::wstring &text, size_t pos,
                        const wchar_t *scheme) {
  for (size_t i = 0; scheme[i]; ++i) {
    if (pos + i >= text.size())
      return false;
    if (static_cast<wchar_t>(wxTolower(text[pos + i])) != scheme[i])
      return false;
  }
  return true;
}

//! Is ch a letter or digit, so that a scheme right after it isn't a word start?
bool IsUrlWordChar(wchar_t ch) { return wxIsalnum(ch) || ch == L'_'; }

/*! Drops the characters from the end of a link candidate that belong to the
  sentence around it rather than to the link. */
size_t TrimUrlEnd(const std::wstring &text, size_t start, size_t end) {
  bool changed = true;
  while (changed && end > start) {
    changed = false;
    const wchar_t last = text[end - 1];
    if (wcschr(L".,;:!?'*", last)) {
      --end;
      changed = true;
    } else if (last == L')' || last == L']') {
      const wchar_t open = (last == L')') ? L'(' : L'[';
      long balance = 0;
      for (size_t i = start; i < end; ++i) {
        if (text[i] == open)
          ++balance;
        else if (text[i] == last)
          --balance;
      }
      // More closing than opening: the last one closes something outside.
      if (balance < 0) {
        --end;
        changed = true;
      }
    }
  }
  return end;
}

// The placeholder ProtectUrls() writes: a private-use character, the link's
// index in decimal, another private-use character. 0xE000 is avoided since
// GroupCell's TeX escaping already parks backslashes there.
const wxUniChar kUrlPlaceholderStart(0xE001);
const wxUniChar kUrlPlaceholderEnd(0xE002);

} // namespace

std::vector<UrlSpan> FindUrls(const wxString &text) {
  std::vector<UrlSpan> result;
  // Every scheme we know contains a colon: most text has none and is done.
  if (text.Find(wxS(':')) == wxNOT_FOUND)
    return result;

  const std::wstring w = text.ToStdWstring();
  size_t pos = 0;
  while (pos < w.size()) {
    // Only a word start can begin a link.
    if ((pos > 0 && IsUrlWordChar(w[pos - 1])) ||
        (wxTolower(w[pos]) != L'h' && wxTolower(w[pos]) != L'm')) {
      ++pos;
      continue;
    }
    const wchar_t *matched = nullptr;
    for (const wchar_t *scheme : kUrlSchemes)
      if (UrlStartsWithSchemeAt(w, pos, scheme)) {
        matched = scheme;
        break;
      }
    if (!matched) {
      ++pos;
      continue;
    }
    size_t end = pos + wcslen(matched);
    while (end < w.size() && IsUrlChar(w[end]))
      ++end;
    const size_t trimmedEnd = TrimUrlEnd(w, pos, end);
    // A bare "https://" or "mailto:" with nothing after it links nowhere.
    if (trimmedEnd > pos + wcslen(matched))
      result.push_back({pos, trimmedEnd - pos});
    pos = end;
  }
  return result;
}

bool IsLaunchableUrl(const wxString &url) {
  const std::wstring w = url.ToStdWstring();
  for (const wchar_t *scheme : kUrlSchemes)
    if (UrlStartsWithSchemeAt(w, 0, scheme) && w.size() > wcslen(scheme))
      return true;
  return false;
}

wxString ProtectUrls(const wxString &text, std::vector<wxString> &urls) {
  const std::vector<UrlSpan> spans = FindUrls(text);
  if (spans.empty())
    return text;
  wxString result;
  size_t copied = 0;
  for (const auto &span : spans) {
    result += text.Mid(copied, span.start - copied);
    result += kUrlPlaceholderStart;
    result += wxString::Format(wxS("%lu"), static_cast<unsigned long>(urls.size()));
    result += kUrlPlaceholderEnd;
    urls.push_back(text.Mid(span.start, span.length));
    copied = span.start + span.length;
  }
  result += text.Mid(copied);
  return result;
}

wxString RestoreUrls(const wxString &text, const std::vector<wxString> &urls,
                     const std::function<wxString(const wxString &)> &format) {
  if (urls.empty())
    return text;
  wxString result;
  wxString::const_iterator it = text.begin();
  while (it != text.end()) {
    if (*it != kUrlPlaceholderStart) {
      result += *it;
      ++it;
      continue;
    }
    // Read the index up to the closing marker.
    wxString::const_iterator digits = it;
    ++digits;
    unsigned long index = 0;
    bool valid = false;
    while (digits != text.end() && *digits >= wxS('0') && *digits <= wxS('9')) {
      index = index * 10 + static_cast<unsigned long>(*digits - wxS('0'));
      valid = true;
      ++digits;
    }
    if (valid && digits != text.end() && *digits == kUrlPlaceholderEnd &&
        index < urls.size()) {
      result += format(urls[index]);
      it = digits;
      ++it;
    } else {
      // Not one of ours after all: keep it as it is.
      result += *it;
      ++it;
    }
  }
  return result;
}

} // namespace wxm
