// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2020 Kuba Ober <kuba@bertec.com>
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

#ifndef FONTCACHE_H
#define FONTCACHE_H

#include "precomp.h"
#include <wx/font.h>
#include <functional>
#include <unordered_map>

/*! \file
 * This file implements the wxFont cache system.
 */

namespace std {

template <>
struct hash<wxSize> {
  std::size_t operator()(wxSize size) const;
};

template <>
struct hash<wxFontInfo> {
  std::size_t operator()(const wxFontInfo &fi) const;
};

template <>
struct equal_to<wxFontInfo> {
  bool operator()(const wxFontInfo &l, const wxFontInfo &r) const;
};

} // namespace std

/*! Provides extension methods for the wxFontInfo class.
 */
namespace FontInfo
{
  wxFontInfo GetFor(const wxFont &font);
  void CopyWithoutSize(const wxFontInfo &src, wxFontInfo &dst);
  void CopyWithoutSize(const wxFont *font, wxFontInfo &dst);

  void SetPointSize(wxFontInfo &info, int p);
  void SetPointSize(wxFontInfo &info, float p);
  void SetPointSize(wxFontInfo &info, double p);
  void SetPixelSize(wxFontInfo &info, wxSize size);
}

class FontCache final
{
  FontCache(const FontCache &) = delete;
  FontCache &operator=(const FontCache &) = delete;
  std::unordered_map<wxFontInfo, wxFont> m_cache;
  bool m_enabled = true;
  int m_hits = 0;
  int m_misses = 0;
public:
  FontCache() = default;
  ~FontCache();
  wxFont GetFont(const wxFontInfo &request);
  wxFontInfo AddFont(wxFontInfo info, const wxFont &font);
  wxFontInfo AddFont(const wxFont &font);
  bool IsOk(const wxFontInfo &request);
  int GetHits() const { return m_hits; }
  int GetMisses() const { return m_misses; }
  void SetEnabled(bool enabled = true) { m_enabled = enabled; }
  void Clear();
  static FontCache &Get()
  {
#ifdef _WIN32
    static thread_local FontCache globalCache;
    // Windows allows font access from multiple threads, as long as each font
    // is built separately.
#else
    static FontCache globalCache;
#endif // _WIN32
    return globalCache;
  }
  static wxFont GetAFont(const wxFontInfo &request)
  {
    return Get().GetFont(request);
  }
  static wxFont GetAFont(const wxFont &font)
  {
    Get().AddFont(font);
    return font;
  }
  static wxFontInfo AddAFont(const wxFont &font)
  {
    return Get().AddFont(font);
  }
  static wxFontInfo AddAFont(const wxFontInfo &info, const wxFont &font)
  {
    return Get().AddFont(info, font);
  }
};

#endif  // FONTCACHE_H
