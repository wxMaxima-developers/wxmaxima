// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2023 Gunter Königsmann <wxMaxima@peterpall.de>
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

#include <FontVariantCache.h>
#include <wx/intl.h>
#include <wx/log.h>
#include <iostream>
FontVariantCache::FontVariantCache(wxString fontName):
  m_fontName(fontName)
{
}


void FontVariantCache::ClearCache(){
  bool cleared = false;
  for (auto &i: m_fontCaches)
  {
    if(!i.empty())
    {
      cleared = true;
      i.clear();
    }
  }
  if(cleared)
    wxLogMessage(_("Cleared font cache for font %s"), m_fontName.mb_str());
}

std::shared_ptr<wxFont> FontVariantCache::GetFont (double size,
                                                   bool isItalic,
                                                   bool isBold,
                                                   bool isUnderlined,
                                                   bool isSlanted,
                                                   bool isStrikeThrough
  )
{
  int index = GetIndex(isItalic,
                       isBold,
                       isUnderlined,
                       isSlanted,
                       isStrikeThrough);
  auto cachedFont = m_fontCaches[index].find(size);
  if(cachedFont == m_fontCaches[index].end())
  {
    wxFontStyle style;
      style = wxFONTSTYLE_NORMAL;
    if(isItalic)
      style = wxFONTSTYLE_ITALIC;
    if(isSlanted)
      style = wxFONTSTYLE_SLANT;
    wxFontWeight weight;
    if(isBold)
      weight = wxFONTWEIGHT_BOLD;
    else
      weight = wxFONTWEIGHT_NORMAL;
    auto font = std::shared_ptr<wxFont>(
      new
      wxFont (
        size,
        wxFONTFAMILY_DEFAULT,
        style,
        weight, isUnderlined,
        m_fontName));
    if(!font->IsOk())
    {
      wxLogMessage(_("Cannot create a font based on %s. Falling back to a default font."), m_fontName.mb_str());
      font = std::shared_ptr<wxFont>(new wxFont(*wxNORMAL_FONT));
    }
    if(isStrikeThrough)
      font->MakeStrikethrough();
#if wxCHECK_VERSION(3, 1, 2)
    font->SetFractionalPointSize(size);
#else
    font->SetPointSize(size);
#endif
    m_fontCaches[index][size] = font;
    wxLogMessage(_("Caching font variant: %s"), font->GetNativeFontInfoDesc().mb_str());
    return font;
  }
  return cachedFont->second;
}
