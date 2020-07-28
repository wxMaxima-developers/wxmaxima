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
#include "TextStyle.h"
#include <wx/font.h>
#include <functional>
#include <list>
#include <unordered_map>

/*! \file
 * This file implements the wxFont cache system.
 */

class FontCache final
{
  static constexpr size_t tempFontCount = 8;
  using TempFonts = std::list<std::pair<const Style, wxFont>>;
  FontCache(const FontCache &) = delete;
  FontCache &operator=(const FontCache &) = delete;
  std::unordered_map<Style, wxFont, StyleFontHasher, StyleFontEquals> m_cache;
  //! Used to store the last few font instances when the cache is disabled
  TempFonts m_temporaryFonts;
  int m_hits = 0;
  int m_misses = 0;
  const bool m_enabled = true;
  const std::pair<const Style, wxFont> &GetStyleFont(const Style &style, const wxFont &withFont = {});
  const std::pair<const Style, wxFont> &GetStyleFontUncached(const Style &style, const wxFont &withFont = {});
public:
  FontCache();
  ~FontCache();
  const wxFont &GetFont(const Style &style);
  const Style &AddFont(const wxFont &font);
  bool IsEnabled() const { return m_enabled; }
  int GetHits() const { return m_hits; }
  int GetMisses() const { return m_misses; }
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
  static const std::pair<const Style, wxFont> &GetAStyleFont(const Style &style)
  { return Get().GetStyleFont(style); }
  static const wxFont &GetAFont(const Style &style) { return Get().GetFont(style); }
  static const Style &AddAFont(const wxFont &font) { return Get().AddFont(font); }
};

#endif  // FONTCACHE_H
