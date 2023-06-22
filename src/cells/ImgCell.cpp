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
  This file defines the class ImgCell

  ImgCell is the Cell type that represents still images in maxima's output
  or in user-provided images.
*/

// 72 points per inch / 96 pixels per inch
#define PRINT_SIZE_MULTIPLIER (72.0 / 96.0)
#define SELECTION_BORDER_WDTH Scale_Px(3)

#include "ImgCell.h"
#include "CellImpl.h"
#include "CellPointers.h"
#include "StringUtils.h"
#include <memory>
#include <wx/clipbrd.h>
#include <wx/file.h>
#include <wx/filename.h>
#include <wx/filesys.h>
#include <wx/fs_mem.h>
#include <wx/mstream.h>

ImgCell::ImgCell(GroupCell *group, Configuration *config)
  : ImgCellBase(group, config), m_imageBorderWidth(1) {
  InitBitFields();
  m_type = MC_TYPE_IMAGE;
}

ImgCell::ImgCell(GroupCell *group, Configuration *config,
                 const wxMemoryBuffer &image, const wxString &type)
  : ImgCellBase(group, config),
    m_image(new Image(m_configuration, image, type)), m_imageBorderWidth(1) {
  InitBitFields();
  m_type = MC_TYPE_IMAGE;
}

ImgCell::ImgCell(GroupCell *group, Configuration *config,
                 const wxBitmap &bitmap)
  : ImgCellBase(group, config), m_image(new Image(m_configuration, bitmap)),
    m_imageBorderWidth(1) {
  InitBitFields();
  m_type = MC_TYPE_IMAGE;
}

int ImgCell::s_counter = 0;

// constructor which load image
ImgCell::ImgCell(GroupCell *group, Configuration *config, const wxString &image,
                 std::shared_ptr<wxFileSystem> filesystem, bool remove)
  : ImgCellBase(group, config), m_imageBorderWidth(1) {
  InitBitFields();
  m_type = MC_TYPE_IMAGE;
  if (image != wxEmptyString) {
    m_image =
      std::make_shared<Image>(m_configuration, image, filesystem, remove);
  } else
    m_image = std::make_shared<Image>(m_configuration);
  m_drawBoundingBox = false;
}

void ImgCell::SetConfiguration(Configuration *config) {
  m_configuration = config;
  m_image->SetConfiguration(config);
}

ImgCell::ImgCell(GroupCell *group, const ImgCell &cell)
  : ImgCellBase(group, cell.m_configuration), m_imageBorderWidth(1) {
  InitBitFields();
  m_type = MC_TYPE_IMAGE;
  m_image = std::make_shared<Image>(cell.m_configuration, *cell.m_image);
  m_drawBoundingBox = cell.m_drawBoundingBox;
}

DEFINE_CELL(ImgCell)

void ImgCell::ReloadImage(const wxString &image,
                          std::shared_ptr<wxFileSystem> filesystem) {
  // Store old size limits
  double width = m_image->GetMaxWidth();
  double height = m_image->GetHeightList();

  // Load new image
  m_image = std::make_shared<Image>(m_configuration, image, filesystem, false);

  // Restore size limits
  m_image->SetMaxHeight(height);
  m_image->SetMaxWidth(width);
}

void ImgCell::LoadImage(wxString image, bool remove) {
  m_image = std::make_shared<Image>(m_configuration, remove, image);
}

void ImgCell::SetBitmap(const wxBitmap &bitmap) {
  m_width = m_height = -1;
  m_image = std::make_shared<Image>(m_configuration, bitmap);
}

// ImgCell::ImgCell(GroupCell *group, const ImgCell &cell):
//     ImgImgCellBase(group, cell.m_configuration
// {
//   InitBitFields();
//   CopyCommonData(cell);
//   m_drawRectangle = cell.m_drawRectangle;
//   m_drawBoundingBox = false;
//   m_image = cell.m_image;
// }

ImgCell::~ImgCell() { ImgCell::ClearCache(); }

const wxString &ImgCell::GetToolTip(const wxPoint point) const {
  if (!ContainsPoint(point))
    return wxm::emptyString;

  m_cellPointers->m_cellUnderPointer = const_cast<ImgCell *>(this);
  if (!m_image->IsOk())
    return Image::GetBadImageToolTip();

  return GetLocalToolTip();
}

void ImgCell::Recalculate(AFontSize fontsize) {
  if (m_image) {
    // Here we recalculate the height, as well:
    //  - This doesn't cost much time and
    //  - as image cell's sizes might change when the resolution does
    //    we might have intermittent calculation issues otherwise
    if (m_configuration->GetPrinting()) {
      m_image->Recalculate(m_configuration->GetZoomFactor() *
                           PRINT_SIZE_MULTIPLIER);
      m_imageBorderWidth = Scale_Px(1);
    } else
      m_image->Recalculate();
    m_width = m_image->m_width + 2 * m_imageBorderWidth;
    if (m_configuration->GetPrinting()) {
      m_image->Recalculate(m_configuration->GetZoomFactor() *
                           PRINT_SIZE_MULTIPLIER);
      m_imageBorderWidth = Scale_Px(1);
    } else
      m_image->Recalculate();
    m_height = m_image->m_height + 2 * m_imageBorderWidth;
    m_center = m_height / 2;
  }
  Cell::Recalculate(fontsize);
}

void ImgCell::Draw(wxPoint point, wxDC *dc, wxDC *antialiassingDC) {
  Cell::Draw(point, dc, antialiassingDC);
  if (DrawThisCell(point) && (m_image != NULL)) {
    if (!InUpdateRegion())
      return;

    wxMemoryDC bitmapDC;

    if (m_drawBoundingBox)
      {
	std::lock_guard<std::mutex> guard(Configuration::m_refcount_mutex);
	dc->SetBrush(*(wxTheBrushList->FindOrCreateBrush(
							 m_configuration->GetColor(TS_SELECTION))));
      }
    else
      SetPen(dc);

    if (m_drawRectangle || m_drawBoundingBox)
      dc->DrawRectangle(wxRect(point.x, point.y - m_center, m_width, m_height));

    wxBitmap bitmap =
      (m_configuration->GetPrinting() ? m_image->GetUnscaledBitmap()
       : m_image->GetBitmap());
    bitmapDC.SelectObject(bitmap);

    int xDst = point.x + m_imageBorderWidth;
    int yDst = point.y - m_center + m_imageBorderWidth;
    int widthDst = m_width - 2 * m_imageBorderWidth;
    int heightDst = m_height - 2 * m_imageBorderWidth;
    int xSrc = 0;
    int ySrc = 0;
    if (m_drawBoundingBox && m_imageBorderWidth == 0) {
      xDst += SELECTION_BORDER_WDTH;
      yDst += SELECTION_BORDER_WDTH;
      widthDst -= 2 * SELECTION_BORDER_WDTH;
      heightDst -= 2 * SELECTION_BORDER_WDTH;
      xSrc += SELECTION_BORDER_WDTH;
      ySrc += SELECTION_BORDER_WDTH;
    }
    if (m_configuration->GetPrinting()) {
      dc->StretchBlit(xDst, yDst, widthDst, heightDst, &bitmapDC, xSrc, ySrc,
                      bitmap.GetWidth(), bitmap.GetHeight());
    } else
      dc->Blit(xDst, yDst, widthDst, heightDst, &bitmapDC, xSrc, ySrc);
  } else
    // The cell isn't drawn => No need to keep it's image cache for now.
    ClearCache();

  // The next time we need to draw a bounding box we will be informed again.
  m_drawBoundingBox = false;
}

wxString ImgCell::ToString() const { return _(" (Graphics) "); }

wxString ImgCell::ToMatlab() const { return _(" (Graphics) "); }

wxString ImgCell::ToTeX() const { return _(" (Graphics) "); }

wxSize ImgCell::ToImageFile(wxString filename) {
  return m_image->ToImageFile(filename);
}

static void writeHex(void *data, size_t length, wxStringBuffer::CharType *out) {
  using char_t = std::remove_reference_t<decltype(*out)>;
  char *const end = static_cast<char *>(data) + length;
  for (char *in = static_cast<char *>(data); in != end; in++) {
    char c = *in;
    unsigned char const h = (c >> 4) & 0xF, l = c & 0xF;
    if (h > 9)
      *out++ = char_t('a' + h - 0xA);
    else
      *out++ = char_t('0' + h - 0);
    if (l > 9)
      *out++ = char_t('a' + l - 0xA);
    else
      *out++ = char_t('0' + l - 0);
  }
}

wxString ImgCell::ToRTF() const {
  // Lines that are common to all types of images
  wxString header = wxS("{\\pict");
  wxString footer = wxS("}\n");

  // Extract the description of the image data
  wxString image;
  wxMemoryBuffer imgdata;
  if (m_image->GetExtension().Lower() == wxS("png")) {
    imgdata = GetCompressedImage();
    image = wxS("\\pngblip");
  } else if ((m_image->GetExtension().Lower() == wxS("jpg")) ||
             (m_image->GetExtension().Lower() == wxS("jpeg"))) {
    imgdata = GetCompressedImage();
    image = wxS("\\jpegblip");
  } else {
    // Convert any non-rtf-enabled format to .png before adding it to the .rtf
    // file.
    image = wxS("\\pngblip");
    wxImage imagedata = m_image->GetUnscaledBitmap().ConvertToImage();
    wxMemoryOutputStream stream;
    imagedata.SaveFile(stream, wxBITMAP_TYPE_PNG);
    imgdata.AppendData(stream.GetOutputStreamBuffer()->GetBufferStart(),
                       stream.GetOutputStreamBuffer()->GetBufferSize());
  }

  image += wxString::Format(wxS("\\picw%lu\\pich%lu "),
                            (unsigned long)m_image->GetOriginalWidth(),
                            (unsigned long)m_image->GetOriginalHeight());

  // Convert the data into a hexadecimal string
  wxString hexString;
  writeHex(imgdata.GetData(), imgdata.GetDataLen(),
           wxStringBuffer(hexString, 2 * imgdata.GetDataLen()));

  wxString result;
  result.reserve(header.size() + image.size() + hexString.size() +
                 footer.size() + 10);
  result << header << image << hexString << footer;
  return result;
}

wxString ImgCell::ToXML() const {
  wxString basename = m_cellPointers->WXMXGetNewFileName();

  // add the file to memory
  if (m_image) {
    m_configuration->PushFileToSave(basename + m_image->GetExtension(),
				    m_image->GetCompressedImage());
  }

  wxString flags;
  if (HasHardLineBreak())
    flags += wxS(" breakline=\"true\"");

  flags += wxString::Format(wxS(" ppi=\"%i\""), m_image->GetPPI());

  if (!m_drawRectangle)
    flags += wxS(" rect=\"false\"");

  if (m_image->GetMaxWidth() > 0)
    flags += wxString::Format(wxS(" maxWidth=\"%f\""), m_image->GetMaxWidth());

  if (m_image->GetHeightList() > 0)
    flags +=
      wxString::Format(wxS(" maxHeight=\"%f\""), m_image->GetHeightList());

  if (m_origImageFile != wxEmptyString) {
    if (m_configuration->SaveImgFileName()) {
      flags += wxString::Format(wxS(" origImageFile=\"%s\""),
                                XMLescape(m_origImageFile));
    }
  }

  if (m_image) {
    // Anonymize the name of our temp directory for saving
    if (m_image->GnuplotData() != wxEmptyString) {
      wxFileName gnuplotDataFile(m_image->GnuplotData());
      wxString gnuplotData = gnuplotDataFile.GetFullName() + wxS(".gz");
      m_configuration->PushFileToSave(gnuplotData,
				      m_image->GetCompressedGnuplotData());
      flags += wxS(" gnuplotdata_gz=\"") + gnuplotData + wxS("\"");
    }
    if (m_image->GnuplotSource() != wxEmptyString) {
      wxFileName gnuplotSourceFile(m_image->GnuplotSource());
      wxString gnuplotSource = gnuplotSourceFile.GetFullName() + wxS(".gz");
      m_configuration->PushFileToSave(gnuplotSource,
				      m_image->GetCompressedGnuplotSource());
      flags += wxS(" gnuplotsource_gz=\"") + gnuplotSource + wxS("\"");
    }

    return (wxS("<img") + flags + wxS(">") + basename + m_image->GetExtension() +
	    wxS("</img>"));
  }
  else
    return  (wxS("<img") + flags + wxS(">") +
          wxS("</img>"));
}

void ImgCell::DrawBoundingBox(wxDC &WXUNUSED(dc), bool WXUNUSED(all)) {
  m_drawBoundingBox = true;
}

bool ImgCell::CopyToClipboard() const {
  wxASSERT_MSG(!wxTheClipboard->IsOpened(),
               _("Bug: The clipboard is already opened"));
  if (wxTheClipboard->Open()) {
    bool res = wxTheClipboard->SetData(
				       new wxBitmapDataObject(m_image->GetUnscaledBitmap()));
    wxTheClipboard->Close();
    return res;
  }
  return false;
}
