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
  Reads the size an SVG image says it has, see SvgSize.h.
*/

#include "SvgSize.h"
#include <algorithm>
#include <map>
#include <optional>

namespace {
//! The attributes of the root svg element, or nothing if there is none.
std::optional<std::map<wxString, wxString>> RootSvgAttributes(const wxString &svg) {
  size_t pos = 0;
  const size_t len = svg.length();
  while (true) {
    pos = svg.find(wxS('<'), pos);
    if (pos == wxString::npos)
      return std::nullopt;
    // Skip comments: one may well mention "<svg" itself
    if (svg.compare(pos, 4, wxS("<!--")) == 0) {
      pos = svg.find(wxS("-->"), pos + 4);
      if (pos == wxString::npos)
        return std::nullopt;
      continue;
    }
    // Skip <?xml ...?>, <!DOCTYPE ...> and the like
    if ((pos + 1 < len) &&
        ((svg[pos + 1] == wxS('?')) || (svg[pos + 1] == wxS('!')))) {
      pos++;
      continue;
    }
    break;
  }
  // The first element has to be the svg one.
  if ((svg.compare(pos, 4, wxS("<svg")) != 0) || (pos + 4 >= len) ||
      !(wxIsspace(svg[pos + 4]) || (svg[pos + 4] == wxS('>')) ||
        (svg[pos + 4] == wxS('/'))))
    return std::nullopt;

  std::map<wxString, wxString> attributes;
  pos += 4;
  while (pos < len) {
    while ((pos < len) && wxIsspace(svg[pos]))
      pos++;
    if ((pos >= len) || (svg[pos] == wxS('>')) || (svg[pos] == wxS('/')))
      break;
    wxString name;
    while ((pos < len) && !wxIsspace(svg[pos]) && (svg[pos] != wxS('=')) &&
           (svg[pos] != wxS('>')))
      name += svg[pos++];
    while ((pos < len) && wxIsspace(svg[pos]))
      pos++;
    if ((pos >= len) || (svg[pos] != wxS('='))) {
      // An attribute without a value: not valid XML, but not ours to judge.
      attributes[name] = wxEmptyString;
      continue;
    }
    pos++;
    while ((pos < len) && wxIsspace(svg[pos]))
      pos++;
    if ((pos >= len) || ((svg[pos] != wxS('"')) && (svg[pos] != wxS('\''))))
      return attributes;
    const wxUniChar quote = svg[pos++];
    const size_t end = svg.find(quote, pos);
    if (end == wxString::npos)
      return attributes;
    attributes[name] = svg.SubString(pos, end - 1);
    pos = end + 1;
  }
  return attributes;
}

/*! A length attribute in pixels, or nothing if it is missing, relative or
  not a length at all.
*/
std::optional<double> LengthInPixels(wxString value, double ppi) {
  value.Trim(true);
  value.Trim(false);
  if (value.IsEmpty() || value.EndsWith(wxS("%")))
    return std::nullopt;
  // The unit is whatever follows the number
  size_t unitStart = value.length();
  while ((unitStart > 0) && wxIsalpha(value[unitStart - 1]))
    unitStart--;
  const wxString unit = value.Mid(unitStart).Lower();
  double number;
  if (!value.Left(unitStart).Strip(wxString::both).ToCDouble(&number))
    return std::nullopt;

  double factor;
  if (unit.IsEmpty() || (unit == wxS("px")))
    factor = 1;
  else if (unit == wxS("in"))
    factor = ppi;
  else if (unit == wxS("cm"))
    factor = ppi / 2.54;
  else if (unit == wxS("mm"))
    factor = ppi / 25.4;
  else if (unit == wxS("pt"))
    factor = ppi / 72;
  else if (unit == wxS("pc"))
    factor = ppi / 6;
  else if (unit == wxS("em"))
    factor = 16;  // The CSS default font size
  else if (unit == wxS("ex"))
    factor = 8;
  else
    return std::nullopt;
  if (number * factor <= 0)
    return std::nullopt;
  return number * factor;
}
} // namespace

namespace wxm {
wxSize SvgIntrinsicSize(const wxString &svg, double ppi) {
  const auto attributes = RootSvgAttributes(svg);
  if (!attributes)
    return wxDefaultSize;

  std::optional<double> width, height;
  if (auto w = attributes->find(wxS("width")); w != attributes->end())
    width = LengthInPixels(w->second, ppi);
  if (auto h = attributes->find(wxS("height")); h != attributes->end())
    height = LengthInPixels(h->second, ppi);

  // A missing or relative width or height comes from the viewBox, keeping
  // its aspect ratio if the other one is known.
  double viewBoxWidth = 0, viewBoxHeight = 0;
  if (auto vb = attributes->find(wxS("viewBox")); vb != attributes->end()) {
    wxString numbers = vb->second;
    numbers.Replace(wxS(","), wxS(" "));
    wxString rest = numbers.Strip(wxString::both);
    double values[4];
    int found = 0;
    while (found < 4 && !rest.IsEmpty()) {
      const wxString token = rest.BeforeFirst(wxS(' '));
      rest = rest.AfterFirst(wxS(' ')).Strip(wxString::leading);
      if (!token.ToCDouble(&values[found]))
        break;
      found++;
    }
    // values[0] and values[1] are where the box starts, which doesn't
    // change its size.
    if (found == 4) {
      viewBoxWidth = values[2];
      viewBoxHeight = values[3];
    }
  }
  if ((viewBoxWidth > 0) && (viewBoxHeight > 0)) {
    if (width && !height)
      height = *width * viewBoxHeight / viewBoxWidth;
    else if (height && !width)
      width = *height * viewBoxWidth / viewBoxHeight;
    else if (!width && !height) {
      width = viewBoxWidth;
      height = viewBoxHeight;
    }
  }
  if (!width || !height)
    return wxDefaultSize;
  return wxSize(std::max(1, static_cast<int>(*width + .5)),
                std::max(1, static_cast<int>(*height + .5)));
}
} // namespace wxm
