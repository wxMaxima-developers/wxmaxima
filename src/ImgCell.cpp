// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2015 Gunter Königsmann <wxMaxima@physikbuch.de>
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

#include "ImgCell.h"

#include <wx/file.h>
#include <wx/filename.h>
#include <wx/filesys.h>
#include <wx/fs_mem.h>
#include <wx/clipbrd.h>

ImgCell::ImgCell() : MathCell()
{
  m_image = NULL;
  m_type = MC_TYPE_IMAGE;
  m_drawRectangle = true;
  m_imageBorderWidth = 1;
}

int ImgCell::s_counter = 0;

// constructor which load image
ImgCell::ImgCell(wxString image, bool remove, wxFileSystem *filesystem) : MathCell()
{
  m_type = MC_TYPE_IMAGE;
  m_drawRectangle = true;
  m_image = new Image(image,remove,filesystem);
}

ImgCell::~ImgCell()
{
  wxDELETE(m_image);
  wxDELETE(m_next);
}

void ImgCell::LoadImage(wxString image, bool remove)
{
  wxDELETE(m_image);
  m_image = new Image(image, remove);
}

void ImgCell::SetBitmap(const wxBitmap &bitmap)
{
  wxDELETE(m_image);

  m_width = m_height = -1;
  m_image = new Image(bitmap);
}

MathCell* ImgCell::Copy()
{
  ImgCell* tmp = new ImgCell;
  CopyData(this, tmp);
  tmp->m_drawRectangle = m_drawRectangle;

  Image *img = new Image();
  *img = *m_image;
  tmp->m_image = img;
  
  return tmp;
}

void ImgCell::Destroy()
{
  wxDELETE(m_image);
  m_image = NULL;
  m_next = NULL;
}

void ImgCell::RecalculateWidths(CellParser& parser, int fontsize)
{
  double scale = parser.GetScale();
  m_image->ViewportSize(m_canvasSize.x,m_canvasSize.y,scale);
  
  m_width = (scale * m_image->m_width) + 2 * m_imageBorderWidth;
  ResetData();
}

void ImgCell::RecalculateSize(CellParser& parser, int fontsize)
{
  double scale = parser.GetScale();
  m_image->ViewportSize(m_canvasSize.x,m_canvasSize.y,scale);
  
  m_height = (scale * m_image->m_height) + 2 * m_imageBorderWidth;
  ResetData();

  m_center = m_height / 2;
}

void ImgCell::Draw(CellParser& parser, wxPoint point, int fontsize)
{
  wxDC& dc = parser.GetDC();

  if (DrawThisCell(parser, point) && m_image != NULL)
  {
    wxMemoryDC bitmapDC;
    double scale = parser.GetScale();
    m_image->ViewportSize(m_canvasSize.x,m_canvasSize.y,scale);
  
    m_height = (m_image->m_height) + 2 * m_imageBorderWidth;
    m_width  = (m_image->m_width)  + 2 * m_imageBorderWidth;

    SetPen(parser);
    if (m_drawRectangle)
      
      dc.DrawRectangle(wxRect(point.x, point.y - m_center, m_width, m_height));  

    wxBitmap bitmap = m_image->GetBitmap();
    bitmapDC.SelectObject(bitmap);
      
    dc.Blit(point.x + m_imageBorderWidth, point.y - m_center + m_imageBorderWidth, m_width - 2 * m_imageBorderWidth, m_height - 2 * m_imageBorderWidth, &bitmapDC, 0, 0);
  }
  else
    // The cell isn't drawn => No need to keep it's image cache for now.
    ClearCache();

  MathCell::Draw(parser, point, fontsize);
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

wxString ImgCell::ToXML()
{
  wxString basename = ImgCell::WXMXGetNewFileName();

  // add the file to memory
  if(m_image)
  {
    if(m_image->GetCompressedImage())
      wxMemoryFSHandler::AddFile(basename+m_image -> GetExtension(),
                                 m_image->GetCompressedImage().GetData(),
                                 m_image->GetCompressedImage().GetDataLen()
        );
  }
  return (m_drawRectangle ? wxT("<img>") : wxT("<img rect=\"false\">")) +
    basename + m_image -> GetExtension()+ wxT("</img>");
}

wxString ImgCell::WXMXGetNewFileName()
{
   wxString file(wxT("image"));
   file << (++s_counter) << wxT(".");
   return file;
}

bool ImgCell::CopyToClipboard()
{
  if (wxTheClipboard->Open())
  {
    bool res = wxTheClipboard->SetData(new wxBitmapDataObject(m_image->GetUnscaledBitmap()));
    wxTheClipboard->Close();
    return res;
  }
  return false;
}
