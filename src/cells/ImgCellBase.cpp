// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class ImgCellBase.

  Pointers to this class can both point to images or to animations.
*/

#include "ImgCellBase.h"
#include "CellImpl.h"
#include "CellPointers.h"
#include "Image.h"
#include "StringUtils.h"
#include <wx/clipbrd.h>
#include <wx/file.h>
#include <wx/filename.h>
#include <wx/filesys.h>
#include <wx/fs_mem.h>
#include <wx/mstream.h>

ImgCellBase::ImgCellBase(GroupCell *group, Configuration *config)
  : Cell(group, config) {}

ImgCellBase::~ImgCellBase() = default;

void ImgCellBase::DrawImageBackdrop(wxDC *dc, const wxRect &rect) const {
  if (!m_configuration->DrawImageBackdrop())
    return;
  dc->SetPen(*wxTRANSPARENT_PEN);
  dc->SetBrush(*(wxTheBrushList->FindOrCreateBrush(
                   m_configuration->GetColor(TS_IMAGE_BACKDROP))));
  dc->DrawRectangle(rect);
}

wxString ImgCellBase::UserScaleXMLFlags(double scale, bool fitToWindow) {
  if (!fitToWindow)
    return wxS(" scale=\"native\"");
  // FromCDouble(): MathParser reads it back with ToCDouble(), whatever the
  // locale.
  if (scale != 1.0)
    return wxS(" scale=\"") + wxString::FromCDouble(scale) + wxS("\"");
  return wxEmptyString;
}

wxString ImgCellBase::ImageToRTF(Image &image) {
  wxString picture;
  wxMemoryBuffer imgdata;
  const wxString extension = image.GetExtension().Lower();
  if (extension == wxS("png")) {
    imgdata = image.GetCompressedImage();
    picture = wxS("{\\pict\\pngblip");
  } else if ((extension == wxS("jpg")) || (extension == wxS("jpeg"))) {
    imgdata = image.GetCompressedImage();
    picture = wxS("{\\pict\\jpegblip");
  } else {
    picture = wxS("{\\pict\\pngblip");
    wxImage imagedata = image.GetUnscaledBitmap().ConvertToImage();
    wxMemoryOutputStream stream;
    imagedata.SaveFile(stream, wxBITMAP_TYPE_PNG);
    imgdata.AppendData(stream.GetOutputStreamBuffer()->GetBufferStart(),
                       stream.GetOutputStreamBuffer()->GetBufferSize());
  }

  picture += wxString::Format(wxS("\\picw%lu\\pich%lu\\picwgoal%lu\\pichgoal%lu "),
                              (unsigned long)image.GetOriginalWidth(),
                              (unsigned long)image.GetOriginalHeight(),
                              (unsigned long)image.GetOriginalWidth() * 1440 / image.GetPPI(),
                              (unsigned long)image.GetOriginalHeight() * 1440 / image.GetPPI());

  // The image data, as hexadecimal digits
  static constexpr char hexDigits[] = "0123456789abcdef";
  const auto *const data = static_cast<const unsigned char *>(imgdata.GetData());
  const size_t length = imgdata.GetDataLen();
  picture.reserve(picture.length() + 2 * length + 2);
  for (size_t i = 0; i < length; i++) {
    picture += hexDigits[data[i] >> 4];
    picture += hexDigits[data[i] & 0xF];
  }
  picture += wxS("}\n");
  return picture;
}
