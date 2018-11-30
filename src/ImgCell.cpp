// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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

#include <wx/file.h>
#include <wx/filename.h>
#include <wx/filesys.h>
#include <wx/fs_mem.h>
#include <wx/clipbrd.h>
#include <wx/mstream.h>

ImgCell::ImgCell(Cell *parent, Configuration **config, CellPointers *cellPointers) : Cell(parent, config)
{
  m_cellPointers = cellPointers;
  m_image = NULL;
  m_type = MC_TYPE_IMAGE;
  m_drawRectangle = true;
  m_imageBorderWidth = 1;
  m_drawBoundingBox = false;
}

ImgCell::ImgCell(Cell *parent, Configuration **config, CellPointers *cellpointers, wxMemoryBuffer image, wxString type) : Cell(parent,
                                                                                                           config)
{
  m_cellPointers = cellpointers;
  m_image = new Image(m_configuration, image, type);
  m_type = MC_TYPE_IMAGE;
  m_drawRectangle = true;
  m_imageBorderWidth = 1;
  m_drawBoundingBox = false;
}

ImgCell::ImgCell(Cell *parent, Configuration **config, CellPointers *cellpointers, const wxBitmap &bitmap) : Cell(parent, config)
{
  m_cellPointers = cellpointers;
  m_image = new Image(m_configuration, bitmap);
  m_type = MC_TYPE_IMAGE;
  m_drawRectangle = true;
  m_imageBorderWidth = 1;
  m_drawBoundingBox = false;
}

int ImgCell::s_counter = 0;

// constructor which load image
ImgCell::ImgCell(Cell *parent, Configuration **config, CellPointers *cellpointers, wxString image, bool remove, wxFileSystem *filesystem)
        : Cell(parent, config)
{
  m_cellPointers = cellpointers;
  m_type = MC_TYPE_IMAGE;
  m_drawRectangle = true;
  if (image != wxEmptyString)
    m_image = new Image(m_configuration, image, remove, filesystem);
  else
    m_image = new Image(m_configuration);
  m_drawBoundingBox = false;
}

void ImgCell::LoadImage(wxString image, bool remove)
{
  wxDELETE(m_image);
  m_image = new Image(m_configuration, image, remove);
}

void ImgCell::SetBitmap(const wxBitmap &bitmap)
{
  wxDELETE(m_image);

  m_width = m_height = -1;
  m_image = new Image(m_configuration, bitmap);
}

Cell *ImgCell::Copy()
{
  ImgCell *tmp = new ImgCell(m_group, m_configuration, m_cellPointers);
  CopyData(this, tmp);
  tmp->m_drawRectangle = m_drawRectangle;

  Image *img = new Image(m_configuration);
  *img = *m_image;
  tmp->m_image = img;

  return tmp;
}

ImgCell::~ImgCell()
{
  wxDELETE(m_image);
  MarkAsDeleted();
}

void ImgCell::MarkAsDeleted()
{
  ClearCache();
  Cell::MarkAsDeleted();
}

std::list<Cell *> ImgCell::GetInnerCells()
{
  std::list<Cell *> innerCells;
  return innerCells;
}

wxString ImgCell::GetToolTip(const wxPoint &point)
{
  if(ContainsPoint(point))
  {
    m_cellPointers->m_cellUnderPointer = this;
    if(!m_image->IsOk())
      return(_("The image could not be displayed. It may be broken, in a wrong format or "
               "be the result of gnuplot not being able to write the image or not being "
               "able to understand what maxima wanted to plot.\n"
               "One example of the latter would be: Gnuplot refuses to plot entirely "
               "empty images"));
    else
      return m_toolTip;
  }
  else
    return wxEmptyString;
}

void ImgCell::RecalculateWidths(int fontsize)
{
  Cell::RecalculateWidths(fontsize);
  // Here we recalculate the height, as well:
  //  - This doesn't cost much time and
  //  - as image cell's sizes might change when the resolution does
  //    we might have intermittent calculation issues otherwise
  Configuration *configuration = (*m_configuration);
  if (configuration->GetPrinter()) {
    m_image->Recalculate(configuration->GetZoomFactor() * PRINT_SIZE_MULTIPLIER);
  } else {
    m_image->Recalculate();
  }

  m_width = m_image->m_width + 2 * m_imageBorderWidth;
  m_height = m_image->m_height + 2 * m_imageBorderWidth;
  m_center = m_height / 2;
}

void ImgCell::RecalculateHeight(int fontsize)
{
  Cell::RecalculateHeight(fontsize);
  // Here we recalculate the width, as well:
  //  - This doesn't cost much time and
  //  - as image cell's sizes might change when the resolution does
  //    we might have intermittent calculation issues otherwise
  RecalculateWidths(fontsize);
}

void ImgCell::Draw(wxPoint point)
{
  if (DrawThisCell(point) && (m_image != NULL))
  {
    Configuration *configuration = (*m_configuration);
    if (configuration->GetPrinter()) {
      m_image->Recalculate(configuration->GetZoomFactor() * PRINT_SIZE_MULTIPLIER);
    } else {
      m_image->Recalculate();
    }

    Cell::Draw(point);
    
    if (!InUpdateRegion()) return;
    
    wxDC *dc = configuration->GetDC();
    wxMemoryDC bitmapDC;

    if (m_drawBoundingBox)
      dc->SetBrush(*(wxTheBrushList->FindOrCreateBrush(configuration->GetColor(TS_SELECTION))));
    else
      SetPen();

    if (m_drawRectangle || m_drawBoundingBox)
      dc->DrawRectangle(wxRect(point.x, point.y - m_center, m_width, m_height));

    // Use printing-scale while in printing-mode.
    wxBitmap bitmap = (configuration->GetPrinter() ? m_image->GetBitmap(configuration->GetZoomFactor() * PRINT_SIZE_MULTIPLIER) : m_image->GetBitmap());
    bitmapDC.SelectObject(bitmap);

    if ((m_drawBoundingBox == false) || (m_imageBorderWidth > 0))
      dc->Blit(point.x + m_imageBorderWidth, point.y - m_center + m_imageBorderWidth,
               m_width - 2 * m_imageBorderWidth, m_height - 2 * m_imageBorderWidth,
               &bitmapDC,
               0, 0);
    else
      dc->Blit(point.x + m_imageBorderWidth + SELECTION_BORDER_WDTH, point.y - m_center + m_imageBorderWidth + SELECTION_BORDER_WDTH,
               m_width - 2 * m_imageBorderWidth - 2*SELECTION_BORDER_WDTH,
               m_height - 2 * m_imageBorderWidth - 2*SELECTION_BORDER_WDTH,
               &bitmapDC,
               SELECTION_BORDER_WDTH, SELECTION_BORDER_WDTH);
  }
  else
    // The cell isn't drawn => No need to keep it's image cache for now.
    ClearCache();

  // The next time we need to draw a bounding box we will be informed again.
  m_drawBoundingBox = false;
}

wxString ImgCell::ToString()
{
  return _(" (Graphics) ");
}

wxString ImgCell::ToTeX()
{
  return _(" (Graphics) ");
}

wxSize ImgCell::ToImageFile(wxString file)
{
  return m_image->ToImageFile(file);
}

wxString ImgCell::ToRTF()
{
  // Lines that are common to all types of images
  wxString header = wxT("{\\pict");
  wxString footer = wxT("}\n");

  // Extract the description of the image data
  wxString image;
  wxMemoryBuffer imgdata;
  if (m_image->GetExtension().Lower() == wxT("png"))
  {
    imgdata = GetCompressedImage();
    image = wxT("\\pngblip");
  }
  else if (
          (m_image->GetExtension().Lower() == wxT("jpg")) ||
          (m_image->GetExtension().Lower() == wxT("jpeg"))
          )
  {
    imgdata = GetCompressedImage();
    image = wxT("\\jpegblip");
  }
  else
  {
    // Convert any non-rtf-enabled format to .png before adding it to the .rtf file.
    image = wxT("\\pngblip");
    wxImage imagedata = m_image->GetUnscaledBitmap().ConvertToImage();
    wxMemoryOutputStream stream;
    imagedata.SaveFile(stream, wxBITMAP_TYPE_PNG);
    imgdata.AppendData(stream.GetOutputStreamBuffer()->GetBufferStart(),
                       stream.GetOutputStreamBuffer()->GetBufferSize());
  }

  image += wxString::Format(wxT("\\picw%lu\\pich%lu "),
                            (unsigned long)m_image->GetOriginalWidth(),
                            (unsigned long)m_image->GetOriginalHeight()
  );

  // Convert the data into a hexadecimal string
  for (size_t i = 0; i <= imgdata.GetDataLen(); i++)
    image += wxString::Format("%02x", ((unsigned char *) imgdata.GetData())[i]);

  return header + image + footer;
}

wxString ImgCell::ToXML()
{
  wxString basename = m_cellPointers->WXMXGetNewFileName();

  // add the file to memory
  if (m_image)
  {
    if (m_image->GetCompressedImage())
      wxMemoryFSHandler::AddFile(basename + m_image->GetExtension(),
                                 m_image->GetCompressedImage().GetData(),
                                 m_image->GetCompressedImage().GetDataLen()
      );
  }

  wxString flags;
  if (m_forceBreakLine)
    flags += wxT(" breakline=\"true\"");

  if(!m_drawRectangle)
    flags += wxT(" rect=\"false\"");

  if(m_image->GetMaxWidth() > 0)
    flags += wxString::Format(wxT(" maxWidth=\"%f\""), m_image->GetMaxWidth());

  if(m_image->GetMaxHeight() > 0)
    flags += wxString::Format(wxT(" maxHeight=\"%f\""), m_image->GetMaxHeight());

  if (m_image)
  {
    // Anonymize the name of our temp directory for saving
    wxString gnuplotSource;
    wxString gnuplotData;
    if(m_image->GnuplotData() != wxEmptyString)
    {
      wxFileName gnuplotDataFile(m_image->GnuplotData());
      gnuplotData = gnuplotDataFile.GetFullName();
    }
    if(m_image->GnuplotSource() != wxEmptyString)
    {
      wxFileName gnuplotSourceFile(m_image->GnuplotSource());
      gnuplotSource = gnuplotSourceFile.GetFullName();
    }

    // Save the gnuplot source, if necessary.
    if(gnuplotSource != wxEmptyString)
    {
      flags += " gnuplotsource=\"" + gnuplotSource + "\"";
      wxMemoryBuffer data = m_image->GetGnuplotSource();
      if(data.GetDataLen() > 0)
      {
        wxMemoryFSHandler::AddFile(gnuplotSource,
                                   data.GetData(),
                                   data.GetDataLen()
          );
      }
    }
    if(gnuplotData != wxEmptyString)
    {
      flags += " gnuplotdata=\"" + gnuplotData + "\"";
      wxMemoryBuffer data = m_image->GetGnuplotData();
      if(data.GetDataLen() > 0)
      {
        wxMemoryFSHandler::AddFile(gnuplotData,
                                   data.GetData(),
                                   data.GetDataLen()
          );
      }
    }
  }
  
  return (wxT("<img") + flags + wxT(">") +
          basename + m_image->GetExtension() + wxT("</img>"));
}

bool ImgCell::CopyToClipboard()
{
  wxASSERT_MSG(!wxTheClipboard->IsOpened(),_("Bug: The clipboard is already opened"));
  if (wxTheClipboard->Open())
  {
    bool res = wxTheClipboard->SetData(new wxBitmapDataObject(m_image->GetUnscaledBitmap()));
    wxTheClipboard->Close();
    return res;
  }
  return false;
}
