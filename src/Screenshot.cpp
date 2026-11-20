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
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  Screenshots for the manual, made by wxMaxima itself (--screenshot).
*/

#include "Screenshot.h"
#include <wx/bitmap.h>
#include <wx/dcmemory.h>
#include <wx/dcscreen.h>
#include <wx/image.h>
#include <wx/tokenzr.h>
#include <algorithm>

std::optional<wxm::ScreenshotTarget>
wxm::ParseScreenshotTarget(const wxString &text) {
  ScreenshotTarget target;
  wxString rest;
  if (text.IsEmpty() || (text == wxS("window")))
    return target;
  if (text == wxS("worksheet")) {
    target.kind = ScreenshotTarget::Kind::Worksheet;
    return target;
  }
  if (text == wxS("cells")) {
    target.kind = ScreenshotTarget::Kind::Cells;
    return target;
  }
  if (text.StartsWith(wxS("pane:"), &rest)) {
    if (rest.IsEmpty())
      return std::nullopt;
    target.kind = ScreenshotTarget::Kind::Pane;
    target.pane = rest;
    return target;
  }
  if (text.StartsWith(wxS("config:"), &rest)) {
    long page = 0;
    if (!rest.ToLong(&page) || (page < 0))
      return std::nullopt;
    target.kind = ScreenshotTarget::Kind::Config;
    target.page = static_cast<int>(page);
    return target;
  }
  return std::nullopt;
}

wxImage wxm::ScreenRect(const wxRect &rect) {
  if ((rect.GetWidth() <= 0) || (rect.GetHeight() <= 0))
    return wxImage();
  wxBitmap bitmap(rect.GetSize());
  {
    wxScreenDC screen;
    wxMemoryDC memory(bitmap);
    if (!memory.Blit(0, 0, rect.GetWidth(), rect.GetHeight(), &screen,
                     rect.GetX(), rect.GetY()))
      return wxImage();
  }
  return bitmap.ConvertToImage();
}

wxImage wxm::TrimToContent(const wxImage &image, int padding) {
  if (!image.IsOk())
    return image;
  const int width = image.GetWidth();
  const int height = image.GetHeight();
  const unsigned char red = image.GetRed(0, 0);
  const unsigned char green = image.GetGreen(0, 0);
  const unsigned char blue = image.GetBlue(0, 0);
  int left = width;
  int right = -1;
  int top = height;
  int bottom = -1;
  for (int y = 0; y < height; ++y)
    for (int x = 0; x < width; ++x)
      if ((image.GetRed(x, y) != red) || (image.GetGreen(x, y) != green) ||
          (image.GetBlue(x, y) != blue)) {
        left = std::min(left, x);
        right = std::max(right, x);
        top = std::min(top, y);
        bottom = std::max(bottom, y);
      }
  if (right < 0)
    return image;
  left = std::max(0, left - padding);
  top = std::max(0, top - padding);
  right = std::min(width - 1, right + padding);
  bottom = std::min(height - 1, bottom + padding);
  return image.GetSubImage(wxRect(left, top, right - left + 1, bottom - top + 1));
}

std::optional<wxm::ScreenshotScene>
wxm::ReadScreenshotScene(const wxConfigBase &config) {
  ScreenshotScene scene;
  const auto target =
    ParseScreenshotTarget(config.Read(wxS("Screenshot/show"), wxEmptyString));
  if (!target)
    return std::nullopt;
  scene.target = *target;
  wxString sidebars;
  if (config.Read(wxS("Screenshot/sidebars"), &sidebars)) {
    scene.setSidebars = true;
    wxStringTokenizer names(sidebars, wxS(","));
    while (names.HasMoreTokens()) {
      wxString name = names.GetNextToken();
      name.Trim(true).Trim(false);
      if (!name.IsEmpty())
        scene.sidebars.Add(name);
    }
  }
  long sidebarWidth = 0;
  if (config.Read(wxS("Screenshot/sidebarWidth"), &sidebarWidth) &&
      (sidebarWidth > 0))
    scene.sidebarWidth = static_cast<int>(sidebarWidth);
  long width = -1;
  long height = -1;
  config.Read(wxS("Screenshot/width"), &width);
  config.Read(wxS("Screenshot/height"), &height);
  if ((width > 0) && (height > 0))
    scene.size = wxSize(static_cast<int>(width), static_cast<int>(height));
  return scene;
}
