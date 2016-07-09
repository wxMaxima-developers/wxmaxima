// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2007-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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

#include "SlideShowCell.h"
#include "ImgCell.h"

#include <wx/file.h>
#include <wx/filename.h>
#include <wx/filesys.h>
#include <wx/fs_mem.h>
#include <wx/utils.h>
#include <wx/clipbrd.h>
#include <wx/config.h>
#include "wx/config.h"

SlideShow::SlideShow(wxFileSystem *filesystem,int framerate) : MathCell()
{
  m_size = m_displayed = 0;
  m_type = MC_TYPE_SLIDE;
  m_fileSystem = filesystem; // NULL when not loading from wxmx
  m_framerate = framerate;
  m_imageBorderWidth = 1;
  m_drawBoundingBox = false;
}

SlideShow::~SlideShow()
{
  for (int i=0; i<m_size; i++)
    wxDELETE(m_images[i]);
  wxDELETE(m_next);
}


int SlideShow::GetFrameRate()
{
  int framerate=2;

  if(m_framerate>-1)
    framerate=m_framerate;
  else
    {
      wxConfigBase *config = wxConfig::Get();
      
      config->Read(wxT("DefaultFramerate"),&framerate);
    }
  return(framerate);
}

int SlideShow::SetFrameRate(int Freq)
{

  m_framerate=Freq;
  
  if(Freq<0)
     m_framerate=-1;
  else{
    if(Freq<1)
      m_framerate=1;
    if(Freq>200)
      m_framerate=200;
    }

  return m_framerate;
}

void SlideShow::LoadImages(wxArrayString images)
{
  m_size = images.GetCount();

  if (m_fileSystem) {
    for (int i=0; i<m_size; i++)
    {
      Image *image =new Image(images[i],false,m_fileSystem);
      m_images.push_back(image);
    }
    m_fileSystem = NULL;
  }
  else
    for (int i=0; i<m_size; i++)
    {

      Image *image = new Image(images[i]);
        m_images.push_back(image);
    }
  m_displayed = 0;
}

MathCell* SlideShow::Copy()
{
  SlideShow* tmp = new SlideShow;
  CopyData(this, tmp);

  for(int i=0;i<m_images.size();i++)
  {
    Image *image = new Image(*m_images[i]);
    tmp->m_images.push_back(image);
  }

  tmp->m_size = m_size;
  
  return tmp;
}

void SlideShow::Destroy()
{
  for (int i=0; i<m_size; i++)
    if (m_images[i] != NULL)
    {
      wxDELETE(m_images[i]);
      m_images[i] = NULL;
    }
  m_next = NULL;
}

void SlideShow::SetDisplayedIndex(int ind)
{
  if (ind >= 0 && ind < m_size)
    m_displayed = ind;
  else
    m_displayed = m_size - 1;
}

void SlideShow::RecalculateWidths(CellParser& parser, int fontsize)
{
  double scale = parser.GetScale();
  m_images[m_displayed]->ViewportSize(m_canvasSize.x,m_canvasSize.y,scale);
  
  m_width = (scale * m_images[m_displayed]->m_width) + 2 * m_imageBorderWidth;
}

void SlideShow::RecalculateSize(CellParser& parser, int fontsize)
{
  double scale = parser.GetScale();
  m_images[m_displayed]->ViewportSize(m_canvasSize.x,m_canvasSize.y,scale);
  
  m_height = (scale * m_images[m_displayed]->m_height) + 2 * m_imageBorderWidth;

  m_center = m_height / 2;
}

void SlideShow::Draw(CellParser& parser, wxPoint point, int fontsize)
{
  MathCell::Draw(parser, point, fontsize);
  wxDC& dc = parser.GetDC();

  // Todo: Find a way to redraw only the parts of the Cells that are
  // inside the draw region.
  if (DrawThisCell(parser, point) && (m_images[m_displayed] != NULL))
  {
    wxMemoryDC bitmapDC;
    double scale = parser.GetScale();
    m_images[m_displayed]->ViewportSize(m_canvasSize.x,m_canvasSize.y,scale);
  
    m_height = (m_images[m_displayed]->m_height) + 2 * m_imageBorderWidth;
    m_width  = (m_images[m_displayed]->m_width)  + 2 * m_imageBorderWidth;
    m_center = m_height / 2;

    // Slide show cells have a red border except if they are selected
    if(m_drawBoundingBox)
      dc.SetBrush( *(wxTheBrushList->FindOrCreateBrush(parser.GetColor(TS_SELECTION))));
    else
      dc.SetPen(*wxRED_PEN);

    // If we need a selection border on another redraw we will be informed by OnPaint() again.
    m_drawBoundingBox = false;

    dc.DrawRectangle(wxRect(point.x, point.y - m_center, m_width, m_height));  

    wxBitmap bitmap = m_images[m_displayed]->GetBitmap();
    bitmapDC.SelectObject(bitmap);
      
    dc.Blit(point.x + m_imageBorderWidth, point.y - m_center + m_imageBorderWidth, m_width - 2 * m_imageBorderWidth, m_height - 2 * m_imageBorderWidth, &bitmapDC, 0, 0);
  }
  else
    // The cell isn't drawn => No need to keep it's image cache for now.
    ClearCache();
}

wxString SlideShow::ToString()
{
  return wxT(" << Graphics >> ");
}

wxString SlideShow::ToTeX()
{
  return wxT(" << Graphics >> ");
}

wxString SlideShow::ToXML()
{
  wxString images;

  for (int i=0; i<m_size; i++) {
    wxString basename = ImgCell::WXMXGetNewFileName();
    // add the file to memory
    if(m_images[i])
    {
      if(m_images[i]->GetCompressedImage())
        wxMemoryFSHandler::AddFile(basename+m_images[i] -> GetExtension(),
                                   m_images[i]->GetCompressedImage().GetData(),
                                   m_images[i]->GetCompressedImage().GetDataLen()
          );
    }

    images += basename + m_images[i] -> GetExtension()+wxT(";");
  }

  if(m_framerate<0)
    return wxT("\n<slide>") + images + wxT("</slide>");
  else
    return wxT("\n<slide fr=\"")+ wxString::Format(wxT("%i\">"),GetFrameRate()) + images + wxT("</slide>");
}

wxSize SlideShow::ToImageFile(wxString file)
{
  return m_images[m_displayed]->ToImageFile(file);
}

wxSize SlideShow::ToGif(wxString file)
{
  wxArrayString which;
  bool success = true;

  wxString convert(wxT("convert -delay "+wxString::Format(wxT("%i"),100/GetFrameRate())));
  wxString convertArgs;

  wxString tmpdir = wxFileName::GetTempDir();

  for (int i=0; i<m_size; i++)
  {
    wxFileName imgname(tmpdir, wxString::Format(wxT("wxm_anim%d.png"), i));

    wxImage image = m_images[i]->ToImageFile(imgname.GetFullPath());

    convert << wxT(" \"") << imgname.GetFullPath() << wxT("\"");
  }

  convert << wxT(" \"") << file << wxT("\"");

#if defined __WXMSW__
  if (!wxShell(convert))
#else
  if (wxExecute(convert, wxEXEC_SYNC) != 0)
#endif
  {
    success = false;
    wxMessageBox(_("There was an error during GIF export!\n\nMake sure ImageMagick is installed and wxMaxima can find the convert program."),
        wxT("Error"), wxICON_ERROR);
  }

  for (int i=0; i<m_size; i++)
  {
    wxFileName imgname(tmpdir, wxString::Format(wxT("wxm_anim%d.png"), i));
    wxRemoveFile(imgname.GetFullPath());
  }

  if(success)
  {
    if(m_size>0)
      return wxSize(m_images[1]->GetOriginalWidth(),m_images[1]->GetOriginalHeight());
    else
    {
      wxSize retval;
      retval.x=retval.y=0;
      return retval;
    }
  }
  else
  {
    wxSize retval;
    retval.x=retval.y=-1;
    return retval;
  }
}

void SlideShow::ClearCache()
{
    for (int i=0; i<m_size; i++)
      m_images[i]->ClearCache();
}

bool SlideShow::CopyToClipboard()
{
  if (wxTheClipboard->Open())
  {
    bool res = wxTheClipboard->SetData(new wxBitmapDataObject(m_images[m_displayed]->GetUnscaledBitmap()));
    wxTheClipboard->Close();
    return res;
  }
  return false;
}

