// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
//            (C) 2020 Kuba Ober <kuba@mareimbrium.org>
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

/*!
 * \file
 * This file implements the text style system.
 */

#include "TextStyle.h"
#include <array>
#include <list>
#include <vector>
#include <wx/fontenum.h>
#include <wx/log.h>
#include <wx/thread.h>
#include <wx/translation.h>

/*
 * Style
 */

constexpr wxFontFamily Style::Default_Family;
constexpr wxFontEncoding Style::Default_Encoding;
constexpr wxFontWeight Style::Default_Weight;
constexpr wxFontStyle Style::Default_FontStyle;
constexpr bool Style::Default_Underlined;
constexpr bool Style::Default_Strikethrough;
constexpr AFontSize Style::Default_FontSize;
constexpr uint32_t Style::Default_ColorRGB;

Style::Style()
{
  SetFontName(wxNORMAL_FONT->GetFaceName());
  wxASSERT(m.fontCache != NULL);
}

Style::Style(AFontSize fontSize)
{
  SetFontSize(fontSize);
  SetFontName(wxNORMAL_FONT->GetFaceName());
  wxASSERT(m.fontCache != NULL);
}

Style::Style(const Style &o) : m(o.m) {
  wxASSERT(m.fontCache != NULL);
}

Style &Style::operator=(const Style &o) {
  if (&o != this)
    {
      m = o.m;
      SetFontName(o.GetFontName());
    }
  wxASSERT(m.fontCache != NULL);

  return *this;
}

wxFontFamily Style::GetFamily() const { return m.family; }

wxFontEncoding Style::GetEncoding() const { return m.encoding; }

wxFontWeight Style::GetWeight() const { return m.weight; }

wxFontStyle Style::GetFontStyle() const { return m.fontStyle; }

bool Style::IsUnderlined() const { return m.underlined; }

bool Style::IsStrikethrough() const { return m.strikethrough; }

const wxString &Style::GetFontName() const {
  if(m.fontCache)
    return m.fontCache->GetFaceName();
  else
    return m_emptyString;
}

//! The size of this style's font, asserted to be valid.
AFontSize Style::GetFontSize() const {
  wxASSERT(m.fontSize.IsValid());
  return m.fontSize;
}

uint32_t Style::GetRGBColor() const { return m.rgbColor; }

using did_change = Style::did_change;

did_change Style::SetFamily(wxFontFamily family) {
  if (m.family == family)
    return false;
  m.family = family;
  return true;
}

did_change Style::SetEncoding(wxFontEncoding encoding) {
  if (m.encoding == encoding)
    return false;
  m.encoding = encoding;
  return true;
}

did_change Style::SetFontStyle(wxFontStyle style) {
  if (m.fontStyle == style)
    return false;
  m.fontStyle = style;
  return true;
}

did_change Style::SetWeight(wxFontWeight weight) {
  if (m.weight == weight)
    return false;
  m.weight = weight;
  return true;
}

did_change Style::SetBold(bool bold) {
  return SetWeight(bold ? wxFONTWEIGHT_BOLD : wxFONTWEIGHT_NORMAL);
}

did_change Style::SetLight(bool light) {
  return SetWeight(light ? wxFONTWEIGHT_LIGHT : wxFONTWEIGHT_NORMAL);
}

did_change Style::SetItalic(bool italic) {
  return SetFontStyle(italic ? wxFONTSTYLE_ITALIC : wxFONTSTYLE_NORMAL);
}

did_change Style::SetSlant(bool slant) {
  return SetFontStyle(slant ? wxFONTSTYLE_SLANT : wxFONTSTYLE_NORMAL);
}

did_change Style::SetUnderlined(bool underlined) {
  if (m.underlined == underlined)
    return false;
  m.underlined = underlined;
  return true;
}

did_change Style::SetStrikethrough(bool strikethrough) {
  if (m.strikethrough == strikethrough)
    return false;
  m.strikethrough = strikethrough;
  return true;
}

did_change Style::SetFontName(wxString fontName) {
  if ((m.fontCache != NULL) && (GetFontName() == fontName))
    {
      return false;
    }
  auto fontCache = m_fontCaches.find(fontName);
  if(fontCache == m_fontCaches.end())
    {
      auto newfontCache = std::shared_ptr<FontVariantCache>(new FontVariantCache(fontName));
      m_fontCaches[fontName] = newfontCache;
      m.fontCache = newfontCache;
    }
  else
    m.fontCache = fontCache->second;
  wxASSERT(m.fontCache != NULL);

  return true;
}

did_change Style::SetFontSize(AFontSize fontSize) {
  if (m.fontSize == fontSize)
    return false;
  m.fontSize = fontSize;
  return true;
}

did_change Style::SetRGBColor(uint32_t rgb) {
  if (m.rgbColor == rgb)
    return false;
  m.rgbColor = rgb;
  return true;
}

did_change Style::SetColor(const wxColor &color) {
  return SetRGBColor(color.GetRGB());
}

did_change Style::SetColor(wxSystemColour sysColour) {
  return SetColor(wxSystemSettings::GetColour(sysColour));
}

did_change Style::SetFontFrom(const Style &o) {
  bool changed = SetFontFaceAndSizeFrom(o) |       //-V792
    SetFontStyle(o.GetFontStyle()) |  //-V792
    SetWeight(o.GetWeight()) |        //-V792
    SetUnderlined(o.IsUnderlined()) | //-V792
    SetStrikethrough(o.IsStrikethrough());
  return changed;
}

did_change Style::SetFontFaceFrom(const Style &o) {
  return SetFontName(o.GetFontName()) | //-V792
    SetEncoding(o.GetEncoding()) | //-V792
    SetFamily(o.GetFamily());
}

did_change Style::SetFontFaceAndSizeFrom(const Style &o) {
  return SetFontFaceFrom(o) | SetFontSize(o.m.fontSize); //-V792
}

bool Style::IsFontOk() { return GetFont().IsOk(); }

did_change Style::SetFromFont(const wxFont &font) {
  if (font.IsOk()) {
    m.encoding = font.GetEncoding();
    m.family = font.GetFamily();
    m.fontStyle = font.GetStyle();
    m.underlined = font.GetUnderlined();
    m.strikethrough = font.GetStrikethrough();
    m.weight = font.GetWeight();
    m.fontSize = GetFontSize(font);
    SetFontName(wxString(font.GetFaceName()));
  } else
    {
      SetFontName(wxNORMAL_FONT->GetFaceName());
    }
  return this;
}

AFontSize Style::GetFontSize(const wxFont &font) {
#if wxCHECK_VERSION(3, 1, 2)
  return AFontSize(font.GetFractionalPointSize());
#endif
  return AFontSize(font.GetPointSize());
}

void Style::SetFontSize(wxFont &font, AFontSize fontSize) {
#if wxCHECK_VERSION(3, 1, 2)
  return font.SetFractionalPointSize(fontSize.Get());
#endif
  return font.SetPointSize(fontSize.GetAsLong());
}

wxFontInfo Style::GetAsFontInfo() const {
  wxFontInfo result(GetFontSize().GetForWX());

  result.Family(GetFamily())
    .FaceName(GetFontName())
    .Underlined(IsUnderlined())
    .Strikethrough(IsStrikethrough())
    .Encoding(GetEncoding());

  // This pattern is used to ensure that the legacy variant
  // still compiles (doesn't bitrot).
#if wxCHECK_VERSION(3, 1, 2)
  return result.Style(GetFontStyle()).Weight(GetWeight());
#endif
  return result.Slant(IsSlant())
    .Italic(IsItalic())
    .Bold(IsBold())
    .Light(IsLight());
}

const wxColor &Style::Default_Color() {
  using colorULong = unsigned long;
  static const wxColor color{colorULong(Default_ColorRGB)};
  return color;
}

static const wxString k_color = wxT("%s/color");
static const wxString k_bold = wxT("%s/bold");
static const wxString k_light = wxT("%s/light");
static const wxString k_italic = wxT("%s/italic");
static const wxString k_slant = wxT("%s/slant");
static const wxString k_underlined = wxT("%s/underlined");
static const wxString k_strikethrough = wxT("%s/strikethrough");
static const wxString k_fontsize_float = wxT("%s/Style/Text/fontsize_float");
static const wxString k_fontsize_legacy = wxT("%s/Style/Text/fontsize");
static const wxString k_fontname = wxT("%s/Style/Text/fontname");

Style &Style::Read(wxConfigBase *config, const wxString &where) {
  wxString tmpStr;
  bool tmpBool;
  long tmpLong;
  double tmpDouble;

  if (config->Read(wxString::Format(k_color, where), &tmpStr)) {
    wxColor color = wxColor(tmpStr);
    if (color.IsOk())
      SetColor(color);
  }
  if (config->Read(wxString::Format(k_bold, where), &tmpBool) && tmpBool)
    SetBold(true);
  else if (config->Read(wxString::Format(k_light, where), &tmpBool) && tmpBool)
    SetLight(true);
  if (config->Read(wxString::Format(k_italic, where), &tmpBool) && tmpBool)
    SetItalic(true);
  else if (config->Read(wxString::Format(k_slant, where), &tmpBool) && tmpBool)
    SetSlant(true);
  if (config->Read(wxString::Format(k_underlined, where), &tmpBool))
    SetUnderlined(tmpBool);
  if (config->Read(wxString::Format(k_strikethrough, where), &tmpBool))
    SetStrikethrough(tmpBool);
  if (config->Read(wxString::Format(k_fontsize_float, where), &tmpDouble))
    SetFontSize(AFontSize(tmpDouble));
  else if (config->Read(wxString::Format(k_fontsize_legacy, where), &tmpLong))
    SetFontSize(AFontSize(tmpLong));
  if (config->Read(wxString::Format(k_fontname, where), &tmpStr) &&
      !tmpStr.empty())
    SetFontName(tmpStr);
  else
    SetFontName(wxNORMAL_FONT->GetFaceName());

  // Validation is deferred to the point of first use, etc.
  return *this;
}

void Style::Write(wxConfigBase *config, const wxString &where) const {
  config->Write(wxString::Format(k_color, where), GetColor().GetAsString());
  config->Write(wxString::Format(k_bold, where), IsBold());
  config->Write(wxString::Format(k_italic, where), IsItalic());
  config->Write(wxString::Format(k_underlined, where), IsUnderlined());
  config->Write(wxString::Format(k_fontsize_float, where), GetFontSize().Get());
  config->Write(wxString::Format(k_fontname, where), GetFontName());

  // We don't write the slant, light nor strikethrough attributes so as not to
  // grow the configuration compared to the previous releases. The slant and
  // strikethrough are only emitted when set or when previously set.
  auto const optWrite = [](wxConfigBase *config, const wxString &keyFormat,
                           const wxString &where, auto condition) {
    decltype(condition) tempVal;
    auto path = wxString::Format(keyFormat, where);
    if (condition != decltype(condition){} || config->Read(path, &tempVal))
      config->Write(path, condition);
  };

  optWrite(config, k_slant, where, IsSlant());
  optWrite(config, k_light, where, IsLight());
  optWrite(config, k_strikethrough, where, IsStrikethrough());
}

wxFont Style::GetFont() const {
  return *(m.fontCache->GetFont(GetFontSize().Get(), IsItalic(), IsBold(), IsUnderlined()));
}

const Style &Style::FromStockFont(wxStockGDI::Item font) {
  static Style retval;
  switch (font) {
  case wxStockGDI::FONT_ITALIC: {
    retval.SetFromFont(*wxITALIC_FONT);
    return retval;
  }
  case wxStockGDI::FONT_NORMAL: {
    retval.SetFromFont(*wxNORMAL_FONT);
    return retval;
  }
  case wxStockGDI::FONT_SMALL: {
    retval.SetFromFont(*wxSMALL_FONT);
    return retval;
  }
  case wxStockGDI::FONT_SWISS: {
    retval.SetFromFont(*wxSWISS_FONT);
    return retval;
  }
  default: {
    retval.SetFromFont(*wxNORMAL_FONT);
    return retval;
  }
  }
}

Style::FontVariantCachesMap Style::m_fontCaches;
wxString Style::m_emptyString = wxEmptyString;
