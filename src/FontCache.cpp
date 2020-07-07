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

const std::pair<const Style, wxFont> &FontCache::GetStyleFontUncached(const Style &style, const wxFont &withFont)
{
  wxASSERT(!m_enabled);
  if (m_temporaryFonts.size() >= tempFontCount)
    m_temporaryFonts.pop_front();

  wxFont font = withFont.IsOk() ? withFont : style.GetAsFontInfo();
  m_temporaryFonts.emplace_back(
    font.IsOk() ? Style().FromFontNoCache(font) : style, font);
  auto &entry = m_temporaryFonts.back();
  ++ m_misses;
  entry.first.GetFontHash();
  entry.first.m.font = &entry.second;
  return entry;
}

const std::pair<const Style, wxFont> &FontCache::GetStyleFont(const Style &style, const wxFont &withFont)
{
  if (style.m.isNotOK)
  {
    static const std::pair<const Style, wxFont> badStyle{Style::Data::NotOK, {}};
    return badStyle;
  }
  if (!m_enabled)
    return GetStyleFontUncached(style, withFont);

  auto it = m_cache.find(style);
  if (it != m_cache.end())
  {
    ++ m_hits;
    return *it;
  }

  ++ m_misses;
  wxFont font = withFont.IsOk() ? withFont : style.GetAsFontInfo();

  // Cache the font under the style that yielded it
  auto styleFont = m_cache.emplace(style, font); // Key with raw font request
  wxASSERT(styleFont.second);
  wxASSERT(styleFont.first->first.m.fontHash);
  styleFont.first->first.m.font = &styleFont.first->second;
  wxLogDebug("FontCache Raw Miss: %s", styleFont.first->first.GetDump());
  if (!font.IsOk())
    return *styleFont.first;

  // Cache it also under the style that was read back from the resolved font, if needed
  auto updatedStyle = style;
  updatedStyle.SetFromFontNoCache(font);
  if (!updatedStyle.IsFontEqualTo(style))
  {
    it = m_cache.find(updatedStyle);
    if (it == m_cache.end())
    {
      // Cache the font also under the resolved style
      styleFont = m_cache.emplace(updatedStyle, std::move(font));
      wxASSERT(styleFont.second);
      wxASSERT(styleFont.first->first.m.fontHash);
      styleFont.first->first.m.font = &styleFont.first->second;
      wxLogDebug("FontCache Resolved: %s", updatedStyle.GetDump());
    }
    else
    {
      // We've already cached this font - we now have two instances.
      // Let's replace the instance keyed with raw font request with the one already
      // present, and drop the instance we've just constructed
      auto rawStyleFont = m_cache.emplace(style, it->second);
      wxASSERT(!rawStyleFont.second);
      wxASSERT(rawStyleFont.first->first.m.fontHash);
      rawStyleFont.first->first.m.font = &rawStyleFont.first->second;
      wxLogDebug("FontCache Rekeyed:  %s", updatedStyle.GetDump());
      styleFont.first = it;
    }
  }

  wxASSERT(styleFont.first != m_cache.end());
  return *styleFont.first;
}

const wxFont &FontCache::GetFont(const Style &style)
{
  return GetStyleFont(style).second;
}

const Style &FontCache::AddFont(const wxFont &font)
{
  auto style = Style().FromFontNoCache(font);
  return GetStyleFont(style, font).first;
}

void FontCache::Clear()
{
  m_temporaryFonts.clear();
  m_cache.clear();
  m_hits = 0;
  m_misses = 0;
}
