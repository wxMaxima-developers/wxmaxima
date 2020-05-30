// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2007-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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
  This file defines the class SlideShowCell

  SlideShowCell is the Cell type that represents animations.
*/

// 72 points per inch / 96 pixels per inch
#define PRINT_SIZE_MULTIPLIER (72.0 / 96.0)

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
#include <wx/mstream.h>
#include <wx/wfstream.h>
#include <wx/anidecod.h>

// filesystem cannot be passed by const reference as we want to keep the
// pointer to the file system alive in a background task
// cppcheck-suppress performance symbolName=filesystem
SlideShow::SlideShow(Cell *parent, Configuration **config, CellPointers *cellPointers, std::shared_ptr <wxFileSystem> filesystem, int framerate) :
    Cell(parent, config, cellPointers),
    m_fileSystem(filesystem)
{
  m_animationRunning = true;
  m_size = m_displayed = 0;
  m_type = MC_TYPE_SLIDE;
  m_framerate = framerate;
  m_imageBorderWidth = Scale_Px(1);
  m_drawBoundingBox = false;
  if (m_animationRunning)
    ReloadTimer();
  m_width = m_height = -1;
}

SlideShow::SlideShow(Cell *parent, Configuration **config, CellPointers *cellPointers, int framerate) :
    Cell(parent, config, cellPointers)
{
  m_width = m_height = -1;
  m_animationRunning = true;
  m_size = m_displayed = 0;
  m_type = MC_TYPE_SLIDE;
  m_framerate = framerate;
  m_imageBorderWidth = Scale_Px(1);
  m_drawBoundingBox = false;
  if (m_animationRunning)
    ReloadTimer();
}

SlideShow::SlideShow(Cell *parent, Configuration **config, CellPointers *cellPointers, wxMemoryBuffer image, wxString WXUNUSED(type)):
    SlideShow(parent, config, cellPointers)
{
  LoadImages(image);
}

SlideShow::SlideShow(Cell *parent, Configuration **config, CellPointers *cellPointers, wxString image, bool remove):
    SlideShow(parent, config, cellPointers)
{
  LoadImages(image);
  if (remove)
    wxRemoveFile(image);
}


int SlideShow::GetFrameRate() const
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
  if (framerate < 1)
    framerate = 1;
  return (framerate);
}

void SlideShow::ReloadTimer()
{
  if (!m_timer)
  {
    // Tell MathCtrl about our timer.
    m_timer = std::make_shared<wxTimer>(m_cellPointers->GetMathCtrl(), wxNewId());
    m_cellPointers->m_slideShowTimers[this] = m_timer->GetId();
  }
  
  if(m_timer)
  {
    if(!m_timer->IsRunning())
      m_timer->StartOnce(1000 / GetFrameRate());
  }
}

void SlideShow::StopTimer()
{
    if (m_timer)
    {
      m_timer->Stop();
      m_cellPointers->m_slideShowTimers.erase(this);
      m_timer.reset();
    }
}

void SlideShow::AnimationRunning(bool run)
{
  if(run)
    ReloadTimer();
  else
    StopTimer();
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

void SlideShow::LoadImages(wxMemoryBuffer imageData)
{
  wxImage images;
  wxMemoryInputStream istream(imageData.GetData(), imageData.GetDataLen());
  size_t count = wxImage::GetImageCount(istream);

  m_size = 0;
  for (size_t i = 0; i < count; i++)
  {
    wxMemoryInputStream istream2(imageData.GetData(), imageData.GetDataLen());
    wxImage image;
    image.LoadFile(istream2, wxBITMAP_TYPE_ANY, i);
    m_images.push_back(std::make_shared<Image>(m_configuration, wxBitmap(image)));
    m_size++;
  }
}

void SlideShow::LoadImages(wxString imageFile)
{
  wxImage images;
  size_t count = wxImage::GetImageCount(imageFile);

  m_size = 0;
  for (size_t i = 0; i < count; i++)
  {
    wxImage image;
    image.LoadFile(imageFile, wxBITMAP_TYPE_ANY, i);
    m_images.push_back(std::make_shared<Image>(m_configuration, wxBitmap(image)));
    m_size++;
  }
}

void SlideShow::LoadImages(wxArrayString images, bool deleteRead)
{
  wxString gnuplotFilename;
  wxString dataFilename;

  if(images.GetCount() == 1)
  {
    LoadImages(images[0]);
  }
  else
    for (size_t i = 0; i < images.GetCount(); i++)
    {
      if(images[i].EndsWith(".gnuplot"))
        gnuplotFilename = images[i];
      else
      {
        if(images[i].EndsWith(".data"))
          dataFilename = images[i];
        else
        {
          m_images.push_back(
            std::make_shared<Image>(m_configuration, images[i], m_fileSystem, deleteRead));
          if(gnuplotFilename != wxEmptyString)
          {
            if(m_images.back())
              m_images.back()->GnuplotSource(gnuplotFilename, dataFilename);
          }
          m_size++;
        }
      }
    }
  m_fileSystem = NULL;
  m_displayed = 0;
}

SlideShow::SlideShow(const SlideShow &cell):
  SlideShow(cell.m_group, cell.m_configuration, cell.m_cellPointers)
{
  CopyCommonData(cell);
  AnimationRunning(false);

  m_images.reserve(cell.m_images.size());
  std::copy(cell.m_images.begin(), cell.m_images.end(), std::back_inserter(m_images));

  m_framerate = cell.m_framerate;
  m_displayed = true;
  m_size = cell.m_size;
  m_drawBoundingBox = cell.m_drawBoundingBox;
}

SlideShow::~SlideShow()
{
  SlideShow::MarkAsDeleted();
}

void SlideShow::MarkAsDeleted()
{
  // Stop and unregister the timer.
  StopTimer();
  ClearCache();
  Cell::MarkAsDeleted();
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

  Configuration *configuration = *m_configuration;

  // Assuming a minimum size maybe isn't that bad.
  m_height = m_width = 10;

  // Make the cell as big as the biggest image plus its border.
  for (int i = 0; i < m_size; i++)
  {
    if(m_images[i] != NULL)
    {
      if(configuration->GetPrinting()) {
        m_images[i]->Recalculate(configuration->GetZoomFactor() * PRINT_SIZE_MULTIPLIER);
      } else {
        m_images[i]->Recalculate();
      }
      if(m_width < m_images[i]->m_width + 2 * m_imageBorderWidth)
        m_width = m_images[i]->m_width + 2 * m_imageBorderWidth;
      if(m_height < m_images[i]->m_height + 2 * m_imageBorderWidth)
        m_height = m_images[i]->m_height + 2 * m_imageBorderWidth;
    }
  }       
  m_center = m_height / 2;
  Cell::RecalculateWidths(fontsize);
}

void SlideShow::RecalculateHeight(int fontsize)
{
  Cell::RecalculateHeight(fontsize);
  // The rest is already done on recalculating the width.
}

void SlideShow::Draw(wxPoint point)
{
  Cell::Draw(point);
  // If the animation leaves the screen the timer is stopped automatically.
  if(m_animationRunning)
    ReloadTimer();
  
  if (DrawThisCell(point) && (m_images[m_displayed] != NULL))
  {
    // Start the timer once the animation appears on the screen.
    // But start it only once: Else the animation could be refreshed
    // more frequent than it can be drawn. Each update of the animation
    // will trigger this function and will trigger the animation to be
    // restarted anyway.
    //
    Configuration *configuration = (*m_configuration);
    if(configuration->GetPrinting()) {
        m_images[m_displayed]->Recalculate(configuration->GetZoomFactor() * PRINT_SIZE_MULTIPLIER);
    } else {
      m_images[m_displayed]->Recalculate();
    }
    
    if (!InUpdateRegion()) return;
    
    wxDC *dc = configuration->GetDC();
    wxMemoryDC bitmapDC;

    // Slide show cells have a red border except if they are selected
    if (m_drawBoundingBox)
      dc->SetPen(*(wxThePenList->FindOrCreatePen(configuration->GetColor(TS_SELECTION))));
    else
      dc->SetPen(*wxRED_PEN);

    dc->DrawRectangle(wxRect(point.x, point.y - m_center, m_width, m_height));

    wxBitmap bitmap = (configuration->GetPrinting() ? m_images[m_displayed]->GetBitmap(configuration->GetZoomFactor() * PRINT_SIZE_MULTIPLIER) : m_images[m_displayed]->GetBitmap());
    bitmapDC.SelectObject(bitmap);

    int imageBorderWidth = m_imageBorderWidth;
    if (m_drawBoundingBox)
    {
      imageBorderWidth = Scale_Px(3);
      dc->SetBrush(*(wxTheBrushList->FindOrCreateBrush(configuration->GetColor(TS_SELECTION))));
      dc->DrawRectangle(wxRect(point.x, point.y - m_center, m_width, m_height));
    }

    dc->Blit(point.x + imageBorderWidth, point.y - m_center + imageBorderWidth,
             m_width - 2 * imageBorderWidth, m_height - 2 * imageBorderWidth,
             &bitmapDC,
             imageBorderWidth - m_imageBorderWidth, imageBorderWidth - m_imageBorderWidth);

  }
  else
    // The cell isn't drawn => No need to keep it's image cache for now.
    ClearCache();

  // If we need a selection border on another redraw we will be informed by OnPaint() again.
  m_drawBoundingBox = false;
}

wxString SlideShow::ToString()
{
  return wxT(" << Animation >> ");
}

wxString SlideShow::ToMatlab()
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
  wxString gnuplotSourceFiles;
  wxString gnuplotDataFiles;

  for (int i = 0; i < m_size; i++)
  {
    wxString basename = m_cellPointers->WXMXGetNewFileName();
    // add the file to memory
    if (m_images[i])
    {
      // Anonymize the name of our temp directory for saving
      wxString gnuplotSource;
      wxString gnuplotData;
      if(m_images[i]->GnuplotData() != wxEmptyString)
      {
        wxFileName gnuplotDataFile(m_images[i]->GnuplotData());
        gnuplotData = gnuplotDataFile.GetFullName();
      }
      if(m_images[i]->GnuplotSource() != wxEmptyString)
      {
        wxFileName gnuplotSourceFile(m_images[i]->GnuplotSource());
        gnuplotSource = gnuplotSourceFile.GetFullName();
      }

      // Save the gnuplot source, if necessary.
      if(gnuplotSource != wxEmptyString)
      {
        gnuplotSourceFiles += gnuplotSource + ";";
        wxMemoryBuffer data = m_images[i]->GetGnuplotSource();
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
        gnuplotDataFiles += gnuplotData + ";";
        wxMemoryBuffer data = m_images[i]->GetGnuplotData();
        if(data.GetDataLen() > 0)
        {
          wxMemoryFSHandler::AddFile(gnuplotData,
                                     data.GetData(),
                                     data.GetDataLen()
            );
        }
      }
      
      if (m_images[i]->GetCompressedImage())
        wxMemoryFSHandler::AddFile(basename + m_images[i]->GetExtension(),
                                   m_images[i]->GetCompressedImage().GetData(),
                                   m_images[i]->GetCompressedImage().GetDataLen()
        );
    }

    images += basename + m_images[i]->GetExtension() + wxT(";");
  }

  wxString flags;
  flags = " gnuplotSources=\"" + gnuplotSourceFiles + "\"";
  flags += " gnuplotData=\"" + gnuplotDataFiles + "\"";
  if (m_forceBreakLine)
    flags += wxT(" breakline=\"true\"");
  if (m_animationRunning)
    flags += wxT(" running=\"true\"");
  else
    flags += wxT(" running=\"false\"");
  flags += wxString::Format(wxT(" frame=\"%i\""),m_displayed);

  if (m_framerate > 0)
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
               "able to understand what maxima wanted to plot.\n"
               "One example of the latter would be: Gnuplot refuses to plot entirely "
               "empty images"));
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

  wxImageArray gifFrames;

  for (int i = 0; i < m_size; i++)
  {
    wxImage frame;
    // Reduce the frame to at most 256 colors
    wxQuantize::Quantize(m_images[i]->GetUnscaledBitmap().ConvertToImage(),frame);
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
      
      if(gif.SaveAnimation(gifFrames, &outStream, true, 1000 / GetFrameRate()))
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

SlideShow::GifDataObject::GifDataObject(const wxMemoryOutputStream &str) : wxCustomDataObject(m_gifFormat)
{
  SetData(str.GetOutputStreamBuffer()->GetBufferSize(),
          str.GetOutputStreamBuffer()->GetBufferStart());
}

bool SlideShow::CopyToClipboard()
{
  wxASSERT_MSG(!wxTheClipboard->IsOpened(),_("Bug: The clipboard is already opened"));
  if (wxTheClipboard->Open())
  {
    bool res = wxTheClipboard->SetData(new wxBitmapDataObject(m_images[m_displayed]->GetUnscaledBitmap()));
    wxTheClipboard->Close();
    return res;
  }
  return false;
}

bool SlideShow::CopyAnimationToClipboard()
{
  if (wxTheClipboard->Open())
  {
    // Show a busy cursor as long as we export a .gif file (which might be a lengthy
    // action).
    wxBusyCursor crs;
    
    wxImageArray gifFrames;
    
    for (int i = 0; i < m_size; i++)
    {
      wxImage frame;
      // Reduce the frame to at most 256 colors
      wxQuantize::Quantize(m_images[i]->GetUnscaledBitmap().ConvertToImage(),frame);
      // Gif supports only fully transparent or not transparent at all.
      frame.ConvertAlphaToMask();
      gifFrames.Add(frame);
    }

    wxMemoryOutputStream stream;
    wxGIFHandler gif;
    if(!gif.SaveAnimation(gifFrames, &stream, true, 1000 / GetFrameRate()))
      return false;

    GifDataObject *clpbrdObj = new GifDataObject(stream);
    bool res = wxTheClipboard->SetData(clpbrdObj);
    wxTheClipboard->Close();

    return res;
  }
  return false;
}

wxDataFormat SlideShow::m_gifFormat(wxT("image/gif"));
