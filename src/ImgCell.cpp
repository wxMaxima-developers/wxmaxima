// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2016 Gunter Königsmann <wxMaxima@physikbuch.de>
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

/*! \file
  This file defines the class ImgCell

  ImgCell is the MathCell type that represents still images in maxima's output
  or in user-provided images.
 */

#include "ImgCell.h"

#include <wx/file.h>
#include <wx/filename.h>
#include <wx/filesys.h>
#include <wx/fs_mem.h>
#include <wx/clipbrd.h>
#include <wx/mstream.h>

ImgCell::ImgCell() : MathCell()
{
  m_image = NULL;
  m_type = MC_TYPE_IMAGE;
  m_drawRectangle = true;
  m_imageBorderWidth = 1;
  m_drawBoundingBox = false;
}

ImgCell::ImgCell(wxMemoryBuffer image,wxString type) : MathCell()
{
  m_image = new Image(image,type);
  m_type = MC_TYPE_IMAGE;
  m_drawRectangle = true;
  m_imageBorderWidth = 1;
  m_drawBoundingBox = false;
}

ImgCell::ImgCell(const wxBitmap &bitmap) : MathCell()
{
  m_image = new Image(bitmap);
  m_type = MC_TYPE_IMAGE;
  m_drawRectangle = true;
  m_imageBorderWidth = 1;
  m_drawBoundingBox = false;
}

int ImgCell::s_counter = 0;

// constructor which load image
ImgCell::ImgCell(wxString image, bool remove, wxFileSystem *filesystem) : MathCell()
{
  m_type = MC_TYPE_IMAGE;
  m_drawRectangle = true;
  if(image != wxEmptyString)
    m_image = new Image(image,remove,filesystem);
  else
    m_image = new Image();
  m_drawBoundingBox = false;
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
  // Here we recalculate the height, as well:
  //  - This doesn't cost much time and
  //  - as image cell's sizes might change when the resolution does
  //    we might have intermittent calculation issues otherwise
  double scale = parser.GetScale();
  m_image->ViewportSize(m_canvasSize.x,m_canvasSize.y,scale);
  
  m_width  = (scale * m_image->m_width)  + 2 * m_imageBorderWidth;
  m_height = (scale * m_image->m_height) + 2 * m_imageBorderWidth;
  m_center = m_height / 2;
}

void ImgCell::RecalculateSize(CellParser& parser, int fontsize)
{
  // Here we recalculate the width, as well:
  //  - This doesn't cost much time and
  //  - as image cell's sizes might change when the resolution does
  //    we might have intermittent calculation issues otherwise
  RecalculateWidths(parser,fontsize);
}

void ImgCell::Draw(CellParser& parser, wxPoint point, int fontsize)
{
  MathCell::Draw(parser, point, fontsize);

  // TODO: Enable this when unselecting text updates the right region.
  //if (!InUpdateRegion()) return;

  wxDC& dc = parser.GetDC();
  if (DrawThisCell(parser, point) && (m_image != NULL))
  {
    wxMemoryDC bitmapDC;

    if(m_drawBoundingBox)
      dc.SetBrush( *(wxTheBrushList->FindOrCreateBrush(parser.GetColor(TS_SELECTION))));
    else
      SetPen(parser);
    
    if (m_drawRectangle || m_drawBoundingBox)
      dc.DrawRectangle(wxRect(point.x, point.y - m_center, m_width, m_height));
    
    wxBitmap bitmap = m_image->GetBitmap();
    bitmapDC.SelectObject(bitmap);

    if((m_drawBoundingBox == false) or (m_imageBorderWidth > 0))
      dc.Blit(point.x + m_imageBorderWidth, point.y - m_center + m_imageBorderWidth, m_width - 2 * m_imageBorderWidth, m_height - 2 * m_imageBorderWidth, &bitmapDC, 0, 0);
    else
      dc.StretchBlit(point.x + 5, point.y - m_center + 5, m_width - 2 * 5, m_height - 2 * 5, &bitmapDC, 0, 0, m_width, m_height);
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
  wxString header=wxT("{\\pict");
  wxString footer=wxT("}\n");
  
  // Extract the description of the image data
  wxString image;
  wxMemoryBuffer imgdata;
  if(m_image->GetExtension().Lower() == wxT("png"))
  {
    imgdata = GetCompressedImage();
    image=wxT("\\pngblip");
  } else if(
    (m_image->GetExtension().Lower() == wxT("jpg"))||
    (m_image->GetExtension().Lower() == wxT("jpeg"))
    )
  {
    imgdata = GetCompressedImage();
    image=wxT("\\jpegblip");
  }
    else
    {
      // Convert any non-rtf-enabled format to .png before adding it to the .rtf file.
      image = wxT("\\pngblip");
      wxImage imagedata = m_image->GetUnscaledBitmap().ConvertToImage();
      wxMemoryOutputStream stream;
      imagedata.SaveFile(stream,wxBITMAP_TYPE_PNG);
      imgdata.AppendData(stream.GetOutputStreamBuffer()->GetBufferStart(),
                         stream.GetOutputStreamBuffer()->GetBufferSize());
    }

  image +=wxString::Format(wxT("\\picw%li\\pich%li "),
                           m_image->GetOriginalWidth(),
                           m_image->GetOriginalHeight()
    );
  
  // Convert the data into a hexadecimal string
  for(size_t i=0;i<= imgdata.GetDataLen();i++)
    image += wxString::Format("%02x",((unsigned char *)imgdata.GetData())[i]);

  return header+image+footer;
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
