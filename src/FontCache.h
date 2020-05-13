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

#include "TextStyle.h"
#include <functional>
#include <map>

/*! \file
 * This file implements the wxFont cache system.
 */

class FontCache final
{
  FontCache(const FontCache &) = delete;
  FontCache &operator=(const FontCache &) = delete;
  std::map<Style, wxFont, StyleFontLess> m_cache;
  bool m_enabled = true;
  int m_hits = 0;
  int m_misses = 0;
  const std::pair<const Style, wxFont> &GetFont2(Style style);
public:
  FontCache() = default;
  ~FontCache();
  const wxFont &GetFont(const Style &style);
  const Style &AddFont(const wxFont &font);
  bool IsOk(const Style &style);
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
  static const wxFont &GetAFont(const Style &style) { return Get().GetFont(style); }
  static const Style &AddAFont(const wxFont &font) { return Get().AddFont(font); }
  static bool IsAOk(const Style &style) { return GetAFont(style).IsOk(); }
};

#endif  // FONTCACHE_H
