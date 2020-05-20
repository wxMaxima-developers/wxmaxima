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

#include "FontCache.h"
#include <wx/log.h>

FontCache::~FontCache()
{
  wxLogMessage("~FontCache: hits=%d misses=%d h:m ratio=%.2f",
               m_hits, m_misses, double(m_hits)/m_misses);
}

FontCache::FontCache()
{}

#ifdef _MSC_VER
#pragma warning( push )
#pragma warning( disable: 4172 )
#endif
const std::pair<const Style, wxFont> &FontCache::GetFont2(const Style &style)
{
  if (style.m.isNotOK)
  {
    static const std::pair<const Style, wxFont> badStyle{Style::Data::NotOK, {}};
    return badStyle;
  }
  const std::pair<const Style, wxFont> *cacheEntry = {};
  if (m_enabled)
  {
    auto it = m_cache.find(style);
    if (it != m_cache.end())
    {
      ++ m_hits;
      return *it;
    }
    auto font = m_cache.emplace(style, style.GetAsFontInfo());
    wxASSERT(font.first != m_cache.end());
    wxASSERT(font.second);
    cacheEntry = &*font.first;
    wxLogDebug("FontCache Miss: %5.2fpt %c%c%c%c \"%s\" ",
               style.GetFontSize(),
               style.IsBold() ? 'B' : style.IsLight() ? 'L' : '-',
               style.IsItalic() ? 'I' : style.IsSlant() ? 'S' : '-',
               style.IsUnderlined() ? 'U' : '-',
               style.IsStrikethrough() ? 'T' : '-',
               style.GetFontName().GetAsString());
    ++ m_misses;
  }
  else
  {
    wxFontInfo request = style.GetAsFontInfo();
    if (m_temporaryFonts.size() >= tempFontCount)
      m_temporaryFonts.pop_front();
    m_temporaryFonts.emplace_back(style, request);
    cacheEntry = &m_temporaryFonts.back();
    ++ m_misses;
  }
  cacheEntry->first.GetFontHash();
  cacheEntry->first.m.font = &cacheEntry->second;
  return *cacheEntry;
}
#ifdef _MSC_VER
#pragma warning( pop )
#endif

const wxFont &FontCache::GetFont(const Style &style)
{
  return GetFont2(style).second;
}

const Style &FontCache::AddFont(const wxFont &font)
{
  auto style = Style::FromFontNoCache(font);
  return GetFont2(style).first;
}

void FontCache::Clear()
{
  m_temporaryFonts.clear();
  m_cache.clear();
  m_hits = 0;
  m_misses = 0;
}
