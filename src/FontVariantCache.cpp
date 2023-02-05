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

FontVariantCache::FontVariantCache(wxString fontName):
  m_fontName(fontName)
{
}

std::shared_ptr<wxFont> FontVariantCache::GetFont (double size,
                                                   bool isItalic,
                                                   bool isBold,
                                                   bool isUnderlined)
{
  int index = GetIndex(isItalic,
                       isBold,
                       isUnderlined);
  auto cachedFont = m_fontCaches[index].find(size);
  if(cachedFont == m_fontCaches[index].end())
  {
    wxFontStyle style;
    if(isItalic)
      style = wxFONTSTYLE_SLANT;
    else
      style = wxFONTSTYLE_NORMAL;
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
      font = std::shared_ptr<wxFont>(new wxFont(*wxNORMAL_FONT));
#if wxCHECK_VERSION(3, 1, 2)
    font->SetFractionalPointSize(size);
#else
    font->SetPointSize(size);
#endif
    m_fontCaches[index][size] = font;
    wxLogMessage(_("Caching font variant: %s"), font->GetNativeFontInfoDesc());
    return font;
  }
  return cachedFont->second;
}
