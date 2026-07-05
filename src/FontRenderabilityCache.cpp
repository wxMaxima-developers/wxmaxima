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
  Implements FontRenderabilityCache: which chars can each font render?
*/

#include "FontRenderabilityCache.h"
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/image.h>
#include <wx/log.h>

bool FontRenderabilityCache::FontRendersChar(wxUniChar ch, const wxFont &font) {
  wxString fontName = font.GetNativeFontInfoDesc();
  fontName.Replace("/", "_");
  if (m_renderableChars[fontName].Contains(ch))
    return true;
  if (m_nonRenderableChars[fontName].Contains(ch))
    return false;

  bool retval = FontDisplaysChar(ch, font) &&
    CharVisiblyDifferent(ch, wxS('\1'), font) &&
    CharVisiblyDifferent(ch, L'\uF299', font) &&
    CharVisiblyDifferent(ch, L'\uF000', font);

  if (retval)
    m_renderableChars[fontName] += ch;
  else
    m_nonRenderableChars[fontName] += ch;

  return retval;
}

void FontRenderabilityCache::ClearValues() {
  for (auto &i : m_renderableChars)
    i.second.Clear();
  for (auto &i : m_nonRenderableChars)
    i.second.Clear();
}

void FontRenderabilityCache::ReadFrom(wxConfigBase *config) {
  wxString str;
  long dummy;
  config->SetPath("/renderability/good");
  bool bCont = config->GetFirstEntry(str, dummy);
  while (bCont) {
    wxString chars;
    config->Read(str, &chars);
    m_renderableChars[str] = chars;
    bCont = config->GetNextEntry(str, dummy);
  }
  config->SetPath("/renderability/bad");
  bCont = config->GetFirstEntry(str, dummy);
  while (bCont) {
    wxString chars;
    config->Read(str, &chars);
    m_nonRenderableChars[str] = chars;
    bCont = config->GetNextEntry(str, dummy);
  }
  config->SetPath("/");
}

void FontRenderabilityCache::WriteTo(wxConfigBase *config) const {
  for (const auto &[fontName, chars] : m_renderableChars)
    config->Write(wxS("renderability/good/") + fontName, chars);
  for (const auto &[fontName, chars] : m_nonRenderableChars)
    config->Write(wxS("renderability/bad/") + fontName, chars);
}

bool FontRenderabilityCache::FontDisplaysChar(wxUniChar ch, const wxFont &font) {
  int width = 200;
  int height = 200;

  // Prepare two identical device contexts that create identical bitmaps
  wxBitmap characterBitmap =
    wxBitmap(wxSize(width, height), wxBITMAP_SCREEN_DEPTH);
  wxBitmap referenceBitmap =
    wxBitmap(wxSize(width, height), wxBITMAP_SCREEN_DEPTH);
  wxMemoryDC characterDC;
  wxMemoryDC referenceDC;
  characterDC.SetFont(font);
  referenceDC.SetFont(font);
  characterDC.SelectObject(characterBitmap);
  referenceDC.SelectObject(referenceBitmap);
  characterDC.SetBrush(*wxWHITE_BRUSH);
  referenceDC.SetBrush(*wxWHITE_BRUSH);
  characterDC.DrawRectangle(0, 0, 200, 200);
  referenceDC.DrawRectangle(0, 0, 200, 200);
  characterDC.SetPen(*wxBLACK_PEN);
  referenceDC.SetPen(*wxBLACK_PEN);

  // Now draw the character our button shows into one of these bitmaps and see
  // if that changed any aspect of the bitmap
  characterDC.DrawText(ch, 100, 100);
  wxImage characterImage = characterBitmap.ConvertToImage();
  wxImage referenceImage = referenceBitmap.ConvertToImage();
  for (int x = 0; x < width; x++)
    for (int y = 0; y < height; y++) {
      if (characterImage.GetRed(x, y) != referenceImage.GetRed(x, y))
        return true;
      if (characterImage.GetGreen(x, y) != referenceImage.GetGreen(x, y))
        return true;
      if (characterImage.GetBlue(x, y) != referenceImage.GetBlue(x, y))
        return true;
    }
  wxLogMessage(wxS("Char '%s' seems not to be displayed."), wxString(ch).mb_str());

  // characterImage.SaveFile(wxString(m_char)+".png");

  return false;
}

bool FontRenderabilityCache::CharVisiblyDifferent(wxChar ch, wxChar otherChar,
                                                  const wxFont &font) {
  int width = 200;
  int height = 200;

  // Prepare two identical device contexts that create identical bitmaps
  wxBitmap characterBitmap =
    wxBitmap(wxSize(width, height), wxBITMAP_SCREEN_DEPTH);
  wxBitmap referenceBitmap =
    wxBitmap(wxSize(width, height), wxBITMAP_SCREEN_DEPTH);
  wxMemoryDC characterDC;
  wxMemoryDC referenceDC;
  characterDC.SetFont(font);
  referenceDC.SetFont(font);
  characterDC.SelectObject(characterBitmap);
  referenceDC.SelectObject(referenceBitmap);
  characterDC.SetBrush(*wxWHITE_BRUSH);
  referenceDC.SetBrush(*wxWHITE_BRUSH);
  characterDC.DrawRectangle(0, 0, 200, 200);
  referenceDC.DrawRectangle(0, 0, 200, 200);
  characterDC.SetPen(*wxBLACK_PEN);
  referenceDC.SetPen(*wxBLACK_PEN);
  characterDC.DrawText(wxString(ch), 100, 100);
  referenceDC.DrawText(wxString(otherChar), 100, 100);
  wxImage characterImage = characterBitmap.ConvertToImage();
  wxImage referenceImage = referenceBitmap.ConvertToImage();
  for (int x = 0; x < width; x++)
    for (int y = 0; y < height; y++) {
      if (characterImage.GetRed(x, y) != referenceImage.GetRed(x, y))
        return true;
      if (characterImage.GetGreen(x, y) != referenceImage.GetGreen(x, y))
        return true;
      if (characterImage.GetBlue(x, y) != referenceImage.GetBlue(x, y))
        return true;
    }
  wxLogMessage(wxS("Char '%s' looks identical to '%s'."),
               wxString(ch).mb_str(),
               wxString(otherChar).mb_str());
  return false;
}
