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
#include <wx/hashmap.h>
#include <cmath>
#include <functional>
#include <string>
#include <type_traits>

FontCache::~FontCache()
{
  wxLogMessage("~FontCache: hits=%d misses=%d h:m ratio=%.2f",
             m_hits, m_misses, double(m_hits)/m_misses);
}

wxFont FontCache::GetFont(const wxFontInfo &request)
{
  if (!m_enabled)
  {
    m_misses ++;
    return {request};
  }
  auto font = m_cache.emplace(request, request);
  assert(font.first != m_cache.end());
  m_misses += font.second ? 1 : 0;
  m_hits += font.second ? 0 : 1;
  return font.first->second;
}

wxFontInfo FontCache::AddFont(wxFontInfo info, const wxFont &font)
{
  m_cache.emplace(info, font);
  return info;
}

wxFontInfo FontCache::AddFont(const wxFont &font)
{
  return AddFont(FontInfo::GetFor(font), font);
}

bool FontCache::IsOk(const wxFontInfo &request)
{
  return GetFont(request).IsOk();
}

void FontCache::Clear()
{
  m_cache.clear();
  m_hits = 0;
  m_misses = 0;
}

template <typename T>
static wxFontInfo &SetSize(wxFontInfo &info, T size)
{
#if wxCHECK_VERSION(3, 1, 2)
  wxFontInfo newInfo{size};
#else
  if (!std::is_integral<T>())
    size = round(size);
  wxFontInfo newInfo{int(size)};
#endif
  FontInfo::CopyWithoutSize(info, newInfo);
  return (info = newInfo);
}

template<>
wxFontInfo &SetSize(wxFontInfo &info, wxSize size)
{
  wxFontInfo newInfo{size};
  FontInfo::CopyWithoutSize(info, newInfo);
  return (info = newInfo);
}

namespace FontInfo
{

void CopyWithoutSize(const wxFontInfo &src, wxFontInfo &dst)
{
  dst
    .Family(src.GetFamily())
    .FaceName(src.GetFaceName())
    .Underlined(src.IsUnderlined())
    .Strikethrough(src.IsStrikethrough())
    .Encoding(src.GetEncoding())
#if wxCHECK_VERSION(3, 1, 2)
    .Style(src.GetStyle())
    .Weight(src.GetNumericWeight())
#else
    .Slant(src.GetStyle() == wxFONTSTYLE_SLANT)
    .Italic(src.GetStyle() == wxFONTSTYLE_ITALIC)
    .Bold(src.GetWeight() == wxFONTWEIGHT_BOLD)
    .Light(src.GetWeight() == wxFONTWEIGHT_LIGHT)
#endif
    ;
}

void CopyWithoutSize(const wxFont *font, wxFontInfo &dst)
{
  auto req = GetFor(*font);
  FontCache::AddAFont(req, *font);
  CopyWithoutSize(req, dst);
}

wxFontInfo GetFor(const wxFont &font)
{
  wxFontInfo request;
  if (font.IsUsingSizeInPixels())
    request = wxFontInfo(font.GetPixelSize());
  else
#if wxCHECK_VERSION(3, 1, 2)
    request = wxFontInfo(font.GetFractionalPointSize());
#else
    request = wxFontInfo(font.GetPointSize());
#endif
  return request
#if wxCHECK_VERSION(3, 1, 2)
    .Style(font.GetStyle())
    .Weight(font.GetNumericWeight())
#else
    .Slant(font.GetStyle() == wxFONTSTYLE_SLANT)
    .Italic(font.GetStyle() == wxFONTSTYLE_ITALIC)
    .Bold(font.GetWeight() == wxFONTWEIGHT_BOLD)
    .Light(font.GetWeight() == wxFONTWEIGHT_LIGHT)
#endif
    .Family(font.GetFamily())
    .Encoding(font.GetEncoding())
    .FaceName(font.GetFaceName())
    .Underlined(font.GetUnderlined())
    .Strikethrough(font.GetStrikethrough());
}

void SetPointSize(wxFontInfo &info, int p)
{
  if (info.GetPointSize() != p) SetSize(info, p);
}

void SetPointSize(wxFontInfo &info, float p)
{
#if wxCHECK_VERSION(3, 1, 2)
  if (info.GetFractionalPointSize() != p) SetSize(info, p);
#else
  p = roundf(p);
  if (info.GetPointSize() != p) SetSize(info, p);
#endif // wx version > 3.1.2
}

void SetPointSize(wxFontInfo &info, double p)
{
#if wxCHECK_VERSION(3, 1, 2)
  if (info.GetFractionalPointSize() != p) SetSize(info, p);
#else
  p = round(p);
  if (info.GetPointSize() != p) SetSize(info, p);
#endif // wx version > 3.1.2
}

void SetPixelSize(wxFontInfo &info, wxSize size)
{
  if (!info.IsUsingSizeInPixels() || info.GetPixelSize() != size)
    SetSize(info, size);
}

} // namespace FontInfo

template <typename T>
static std::size_t mixHash(std::size_t seed, const T &value)
{
  std::hash<T> hsh;
  seed ^= hsh(value) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
  return seed;
}

namespace std {

std::size_t hash<wxSize>::operator()(wxSize size) const
{
  return mixHash(mixHash(0, size.x), size.y);
}

std::size_t hash<wxFontInfo>::operator()(const wxFontInfo &fi) const
{
  std::size_t h = 0;
  h = mixHash(h, fi.GetEncoding());
  h = mixHash(h, fi.GetFamily());
#ifdef wxNEEDS_WX_HASH_MAP
  h = mixHash(h, wxStringHash()(fi.GetFaceName()));
#else
  h = mixHash(h, fi.GetFaceName());
#endif
  h = mixHash(h, fi.GetStyle());
#if wxCHECK_VERSION(3, 1, 2)
  h = mixHash(h, fi.GetNumericWeight());
#else
  h = mixHash(h, fi.GetWeight() == wxFONTWEIGHT_BOLD);
  h = mixHash(h, fi.GetWeight() == wxFONTWEIGHT_LIGHT);
#endif
  h = mixHash(h, fi.IsUnderlined() ? wxFONTFLAG_UNDERLINED : 0);
  h = mixHash(h, fi.IsStrikethrough() ? wxFONTFLAG_STRIKETHROUGH : 0);
  if (fi.IsUsingSizeInPixels())
    h = mixHash(h, fi.GetPixelSize());
  else
#if wxCHECK_VERSION(3, 1, 2)
    h = mixHash(h, fi.GetFractionalPointSize());
#else
    h = mixHash(h, fi.GetPointSize());
#endif
  return h;
}

bool equal_to<wxFontInfo>::operator()(const wxFontInfo &l, const wxFontInfo &r) const
{
  return
    l.IsUsingSizeInPixels() == r.IsUsingSizeInPixels() &&
    ((l.IsUsingSizeInPixels() && l.GetPixelSize() == r.GetPixelSize()) ||
     (!l.IsUsingSizeInPixels() &&

#if wxCHECK_VERSION(3, 1, 2)
      (l.GetFractionalPointSize() == r.GetFractionalPointSize())
#else
      (l.GetPointSize() == r.GetPointSize())
#endif
       )) &&
    l.GetFamily() == r.GetFamily() &&
    l.GetFaceName() == r.GetFaceName() &&
    l.GetWeight() == r.GetWeight() &&
    l.IsUnderlined() == r.IsUnderlined() &&
    l.IsStrikethrough() == r.IsStrikethrough() &&
    l.GetEncoding() == r.GetEncoding();
}

} // namespace std

