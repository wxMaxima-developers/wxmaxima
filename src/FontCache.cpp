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

#ifdef _MSC_VER
#pragma warning( push )
#pragma warning( disable: 4172 )
#endif
const std::pair<const Style, wxFont> &FontCache::GetFont2(Style style)
{
  style.DetachFromHierarchy();
  wxFontInfo request = style.GetAsFontInfo();
  if (!m_enabled)
  {
#ifdef __WINDOWS__
    static thread_local std::pair<Style, wxFont> result;
#else
    static std::pair<Style, wxFont> result;
#endif
    m_misses ++;
    result.first = std::move(style);
    result.second = {request};
    return result;
  }
  auto font = m_cache.emplace(std::move(style), request);
  wxASSERT(font.first != m_cache.end());
  m_misses += font.second ? 1 : 0;
  m_hits += font.second ? 0 : 1;
  return *font.first;
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
  Style style;
  style.SetFromFontNoCache(font);
  return GetFont2(style).first;
}

bool FontCache::IsOk(const Style &style)
{
  return GetFont2(style).second.IsOk();
}

void FontCache::Clear()
{
  m_cache.clear();
  m_hits = 0;
  m_misses = 0;
}
