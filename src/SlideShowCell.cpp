// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2007-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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
  This file defines the class SlideShowCell

  SlideShowCell is the MathCell type that represents animations.
*/

#include "SlideShowCell.h"
#include "ImgCell.h"

#include <wx/quantize.h>
#include <wx/imaggif.h>
#include <wx/file.h>
#include <wx/filename.h>
#include <wx/filesys.h>
#include <wx/fs_mem.h>
#include <wx/utils.h>
#include <wx/clipbrd.h>
#include <wx/config.h>
#include "wx/config.h"
#include <wx/mstream.h>
#include <wx/wfstream.h>
#include <wx/anidecod.h>

SlideShow::SlideShow(MathCell *parent, Configuration **config, CellPointers *cellPointers, wxFileSystem *filesystem, int framerate) : MathCell(
        parent, config)
{
  m_cellPointers = cellPointers;
  m_timer = new wxTimer(m_cellPointers->GetMathCtrl(), wxNewId());
  // Tell MathCtrl about our timer.
  m_cellPointers->m_slideShowTimers[this] = m_timer->GetId();
  m_animationRunning = true;
  m_size = m_displayed = 0;
  m_type = MC_TYPE_SLIDE;
  m_fileSystem = filesystem; // NULL when not loading from wxmx
  m_framerate = framerate;
  m_imageBorderWidth = 1;
  m_drawBoundingBox = false;
}
int SlideShow::GetFrameRate()
{
  int framerate = 2;

  if (m_framerate > -1)
    framerate = m_framerate;
  else
  {
    wxConfigBase *config = wxConfig::Get();

    config->Read(wxT("DefaultFramerate"), &framerate);
  }
  if (framerate > 30)
    framerate = 30;
  if (framerate < 0)
    framerate = 0;
  return (framerate);
}

wxTimer *SlideShow::AnimationRunning(bool run)
{
  if(!run)
    m_timer->Stop();
  else
  {
    if(!m_timer->IsRunning())
      m_timer->StartOnce(1000 / GetFrameRate());
  }
  m_animationRunning = run;
}


int SlideShow::SetFrameRate(int Freq)
{

  m_framerate = Freq;

  if (Freq < 0)
    m_framerate = -1;
  else
  {
    if (Freq < 1)
      m_framerate = 1;
    if (Freq > 200)
      m_framerate = 200;
  }

  return m_framerate;
}

void SlideShow::LoadImages(wxArrayString images)
{
  m_size = images.GetCount();

  if (m_fileSystem)
  {
    for (int i = 0; i < m_size; i++)
    {
      Image *image = new Image(m_configuration, images[i], false, m_fileSystem);
      m_images.push_back(image);
    }
    m_fileSystem = NULL;
  }
  else
    for (int i = 0; i < m_size; i++)
    {

      Image *image = new Image(m_configuration, images[i]);
      m_images.push_back(image);
    }
  m_displayed = 0;
}

MathCell *SlideShow::Copy()
{
  SlideShow *tmp = new SlideShow(m_group, m_configuration, m_cellPointers);
  CopyData(this, tmp);

  for (size_t i = 0; i < m_images.size(); i++)
  {
    Image *image = new Image(*m_images[i]);
    tmp->m_images.push_back(image);
  }

  tmp->m_size = m_size;

  return tmp;
}

SlideShow::~SlideShow()
{
  // Stop and unregister the timer.
  m_timer->Stop();
  m_cellPointers->m_slideShowTimers.erase(this);
  delete m_timer;

  for (int i = 0; i < m_size; i++)
    if (m_images[i] != NULL)
    {
      wxDELETE(m_images[i]);
      m_images[i] = NULL;
    }
  MarkAsDeleted();
}

void SlideShow::MarkAsDeleted()
{
  if((this == m_cellPointers->m_selectionStart) || (this == m_cellPointers->m_selectionEnd))
    m_cellPointers->m_selectionStart = m_cellPointers->m_selectionEnd = NULL;
  if(this == m_cellPointers->m_cellUnderPointer)
    m_cellPointers->m_cellUnderPointer = NULL;
  ClearCache();
}

void SlideShow::SetDisplayedIndex(int ind)
{
  if (ind >= 0 && ind < m_size)
    m_displayed = ind;
  else
    m_displayed = m_size - 1;
}

void SlideShow::RecalculateWidths(int fontsize)
{
  // Here we recalculate the height, as well:
  //  - This doesn't cost much time and
  //  - as image cell's sizes might change when the resolution does
  //    we might have intermittent calculation issues otherwise
  m_images[m_displayed]->Recalculate();

  m_width = m_images[m_displayed]->m_width + 2 * m_imageBorderWidth;
  m_height = m_images[m_displayed]->m_height + 2 * m_imageBorderWidth;
  m_center = m_height / 2;
}

void SlideShow::RecalculateHeight(int fontsize)
{
  // Here we recalculate the width, as well:
  //  - This doesn't cost much time and
  //  - as image cell's sizes might change when the resolution does
  //    we might have intermittent calculation issues otherwise
  RecalculateWidths(fontsize);
}

void SlideShow::Draw(wxPoint point, int fontsize)
{
  if (DrawThisCell(point) && (m_images[m_displayed] != NULL))
  {
    // Start the timer once the animation appears on the screen.
    // But start it only once: Else the animation could be refreshed
    // more frequent than it can be drawn. Each update of the animation
    // will trigger this function and will trigger the animation to be
    // restarted anyway.
    //
    // If the animation leaves the screen the timer is stopped automatically.
    if((!m_timer->IsRunning()) && m_animationRunning)
      m_timer->StartOnce(1000 / GetFrameRate());
    MathCell::Draw(point, fontsize);
    m_images[m_displayed]->Recalculate();
    
    if (!InUpdateRegion()) return;
    
    Configuration *configuration = (*m_configuration);
    wxDC &dc = configuration->GetDC();
    wxMemoryDC bitmapDC;
    m_images[m_displayed]->Recalculate();

    m_height = (m_images[m_displayed]->m_height) + 2 * m_imageBorderWidth;
    m_width = (m_images[m_displayed]->m_width) + 2 * m_imageBorderWidth;
    m_center = m_height / 2;

    // Slide show cells have a red border except if they are selected
    if (m_drawBoundingBox)
      dc.SetBrush(*(wxTheBrushList->FindOrCreateBrush(configuration->GetColor(TS_SELECTION))));
    else
      dc.SetPen(*wxRED_PEN);

    // If we need a selection border on another redraw we will be informed by OnPaint() again.
    m_drawBoundingBox = false;

    dc.DrawRectangle(wxRect(point.x, point.y - m_center, m_width, m_height));

    wxBitmap bitmap = m_images[m_displayed]->GetBitmap();
    bitmapDC.SelectObject(bitmap);

    dc.Blit(point.x + m_imageBorderWidth, point.y - m_center + m_imageBorderWidth, m_width - 2 * m_imageBorderWidth,
            m_height - 2 * m_imageBorderWidth, &bitmapDC, 0, 0);
  }
  else
    // The cell isn't drawn => No need to keep it's image cache for now.
    ClearCache();
}

wxString SlideShow::ToString()
{
  return wxT(" << Animation >> ");
}

wxString SlideShow::ToTeX()
{
  return wxT(" << Graphics >> ");
}

wxString SlideShow::ToXML()
{
  wxString images;

  for (int i = 0; i < m_size; i++)
  {
    wxString basename = ImgCell::WXMXGetNewFileName();
    // add the file to memory
    if (m_images[i])
    {
      if (m_images[i]->GetCompressedImage())
        wxMemoryFSHandler::AddFile(basename + m_images[i]->GetExtension(),
                                   m_images[i]->GetCompressedImage().GetData(),
                                   m_images[i]->GetCompressedImage().GetDataLen()
        );
    }

    images += basename + m_images[i]->GetExtension() + wxT(";");
  }

  wxString flags;
  if (m_forceBreakLine)
    flags += wxT(" breakline=\"true\"");
  if (m_animationRunning)
    flags += wxT(" running=\"true\"");
  else
    flags += wxT(" running=\"false\"");
  if (m_framerate < 0)
    flags +=  wxString::Format(wxT(" fr=\"%i\""), GetFrameRate());
  return wxT("\n<slide") + flags + wxT(">") + images + wxT("</slide>");
}

wxSize SlideShow::ToImageFile(wxString file)
{
  return m_images[m_displayed]->ToImageFile(file);
}

wxString SlideShow::ToRTF()
{
  // Animations aren't supported by RTF so we just export the currently shown
  // image.

  // Lines that are common to all types of images
  wxString header = wxT("{\\pict");
  wxString footer = wxT("}\n");

  // Extract the description of the image data
  wxString image;
  wxMemoryBuffer imgdata;
  if (m_images[m_displayed]->GetExtension().Lower() == wxT("png"))
  {
    imgdata = m_images[m_displayed]->GetCompressedImage();
    image = wxT("\\pngblip\n");
  }
  else if (
          (m_images[m_displayed]->GetExtension().Lower() == wxT("jpg")) ||
          (m_images[m_displayed]->GetExtension().Lower() == wxT("jpeg"))
          )
  {
    imgdata = m_images[m_displayed]->GetCompressedImage();
    image = wxT("\\jpegblip\n");
  }
  else
  {
    // Convert any non-rtf-enabled format to .png before adding it to the .rtf file.
    image = wxT("\\pngblip\n");
    wxImage imagedata = m_images[m_displayed]->GetUnscaledBitmap().ConvertToImage();
    wxMemoryOutputStream stream;
    imagedata.SaveFile(stream, wxBITMAP_TYPE_PNG);
    imgdata.AppendData(stream.GetOutputStreamBuffer()->GetBufferStart(),
                       stream.GetOutputStreamBuffer()->GetBufferSize());
  }

  image += wxString::Format(wxT("\\picw%lu\\pich%lu "),
                            (unsigned long)m_images[m_displayed]->GetOriginalWidth(),
                            (unsigned long)m_images[m_displayed]->GetOriginalHeight()
  );

  // Convert the data into a hexadecimal string
  for (size_t i = 0; i <= imgdata.GetDataLen(); i++)
    image += wxString::Format("%02x", ((unsigned char *) imgdata.GetData())[i]);

  return header + image + footer;
}


wxString SlideShow::GetToolTip(const wxPoint &point)
{
  if(ContainsPoint(point))
  {
    m_cellPointers->m_cellUnderPointer = this;
    if(!IsOk())
      return(_("The image could not be displayed. It may be broken, in a wrong format or "
               "be the result of gnuplot not being able to write the image or not being "
               "able to understand what maxima wanted to plot."));
    else
      return m_toolTip;
  }
  else
    return wxEmptyString;
}


wxSize SlideShow::ToGif(wxString file)
{
  // Show a busy cursor as long as we export a .gif file (which might be a lengthy
  // action).
  wxBusyCursor crs;

  wxArrayString which;

  wxImageArray gifFrames;

  for (int i = 0; i < m_size; i++)
  {
    wxImage frame;
    // Reduce the frame to at most 256 colors
    wxQuantize::Quantize(m_images[m_displayed]->GetUnscaledBitmap().ConvertToImage(),frame);
    // Gif supports only fully transparent or not transparent at all.
    frame.ConvertAlphaToMask();
    gifFrames.Add(frame);
  }

  wxFile fl(file, wxFile::write);
  if(fl.IsOpened())
  {
    wxFileOutputStream outStream(fl);
    if(outStream.IsOk())
    {
      wxGIFHandler gif;
      
      if(gif.SaveAnimation(gifFrames, &outStream, false, 1000 / GetFrameRate()))
        return wxSize(m_images[1]->GetOriginalWidth(), m_images[1]->GetOriginalHeight());
    }
  }
  return wxSize(-1,-1);
}

void SlideShow::ClearCache()
{
  for (int i = 0; i < m_size; i++)
    if(m_images[i] != NULL)
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

