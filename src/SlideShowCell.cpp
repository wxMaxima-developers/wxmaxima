///
///  Copyright (C) 2007-2011 Andrej Vodopivec <andrej.vodopivec@gmail.com>
///
///  This program is free software; you can redistribute it and/or modify
///  it under the terms of the GNU General Public License as published by
///  the Free Software Foundation; either version 2 of the License, or
///  (at your option) any later version.
///
///  This program is distributed in the hope that it will be useful,
///  but WITHOUT ANY WARRANTY; without even the implied warranty of
///  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
///  GNU General Public License for more details.
///
///
///  You should have received a copy of the GNU General Public License
///  along with this program; if not, write to the Free Software
///  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
///

#include "SlideShowCell.h"
#include "ImgCell.h"

#include <wx/file.h>
#include <wx/filename.h>
#include <wx/filesys.h>
#include <wx/fs_mem.h>
#include <wx/utils.h>
#include <wx/clipbrd.h>

SlideShow::SlideShow(wxFileSystem *filesystem) : MathCell()
{
  m_size = m_displayed = 0;
  m_type = MC_TYPE_SLIDE;
  m_fileSystem = filesystem; // NULL when not loading from wxmx
}

SlideShow::~SlideShow()
{
  for (int i=0; i<m_size; i++)
    delete m_bitmaps[i];
  if (m_next != NULL)
    delete m_next;
}

void SlideShow::LoadImages(wxArrayString images)
{
  m_size = images.GetCount();

  if (m_fileSystem) {
    for (int i=0; i<m_size; i++)
    {
      bool loadedImage = false;

      wxFSFile *fsfile = m_fileSystem->OpenFile(images[i]);
      if (fsfile) { // open successful

        wxInputStream *istream = fsfile->GetStream();
        wxImage pngImage(*istream);
        if (pngImage.Ok())
        {
          loadedImage = true;
          m_bitmaps.push_back(new wxBitmap(pngImage));
        }
        delete fsfile;
      }

      if (!loadedImage)
      {
        wxBitmap *bitmap = new wxBitmap;
        bitmap->Create(400, 250);

        wxString error = wxString::Format(_("Error %d"), i);

        wxMemoryDC dc;
        dc.SelectObject(*bitmap);

        int width = 0, height = 0;
        dc.GetTextExtent(error, &width, &height);

        dc.DrawRectangle(0, 0, 400, 250);
        dc.DrawLine(0, 0,   400, 250);
        dc.DrawLine(0, 250, 400, 0);
        dc.DrawText(error, 200 - width/2, 125 - height/2);

        m_bitmaps.push_back(bitmap);
      }
    }

    m_fileSystem = NULL;
  }
  else
    for (int i=0; i<m_size; i++)
    {
      bool loadedImage = false;

      if (wxFileExists(images[i]))
      {
        wxBitmap *bitmap = new wxBitmap;
        if (bitmap->LoadFile(images[i], wxBITMAP_TYPE_PNG))
        {
          loadedImage = true;
          m_bitmaps.push_back(bitmap);
        }
        else
          delete bitmap;

        wxRemoveFile(images[i]);
      }

      if (!loadedImage)
      {
        wxBitmap *bitmap = new wxBitmap;
        bitmap->Create(400, 250);

        wxString error = wxString::Format(_("Error %d"), i);

        wxMemoryDC dc;
        dc.SelectObject(*bitmap);

        int width = 0, height = 0;
        dc.GetTextExtent(error, &width, &height);

        dc.DrawRectangle(0, 0, 400, 250);
        dc.DrawLine(0, 0,   400, 250);
        dc.DrawLine(0, 250, 400, 0);
        dc.DrawText(error, 200 - width/2, 125 - height/2);

        m_bitmaps.push_back(bitmap);
      }
    }

  m_displayed = 0;
}

MathCell* SlideShow::Copy(bool all)
{
  ImgCell* tmp = new ImgCell;
  CopyData(this, tmp);

  tmp->m_bitmap = new wxBitmap(*m_bitmaps[m_displayed]);

  if (all && m_next != NULL)
    tmp->AppendCell(m_next->Copy(all));
  return tmp;
}

void SlideShow::Destroy()
{
  for (int i=0; i<m_size; i++)
    if (m_bitmaps[i] != NULL)
    {
      delete m_bitmaps[i];
      m_bitmaps[i] = NULL;
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

void SlideShow::RecalculateWidths(CellParser& parser, int fontsize, bool all)
{
  if (m_bitmaps[m_displayed] != NULL)
    m_width = m_bitmaps[m_displayed]->GetWidth() + 2;
  else
    m_width = 0;

  double scale = parser.GetScale();
  scale = MAX(scale, 1.0);

  m_width = (int) (scale * m_width);
  MathCell::RecalculateWidths(parser, fontsize, all);
}

void SlideShow::RecalculateSize(CellParser& parser, int fontsize, bool all)
{
  if (m_bitmaps[m_displayed] != NULL)
    m_height = m_bitmaps[m_displayed]->GetHeight() + 2;
  else
    m_height = 0;

  double scale = parser.GetScale();
  scale = MAX(scale, 1.0);

  m_height= (int) (scale * m_height);

  m_center = m_height / 2;
  MathCell::RecalculateSize(parser, fontsize, all);
}

void SlideShow::Draw(CellParser& parser, wxPoint point, int fontsize, bool all)
{
  if (DrawThisCell(parser, point) && m_bitmaps[m_displayed] != NULL)
  {
    wxDC& dc = parser.GetDC();
    wxMemoryDC bitmapDC;
    double scale = parser.GetScale();
    scale = MAX(scale, 1.0);

    dc.DrawRectangle(wxRect(point.x, point.y - m_center, m_width, m_height));

    if (scale != 1.0)
    {
      wxImage img = m_bitmaps[m_displayed]->ConvertToImage();
      img.Rescale(m_width, m_height);

      wxBitmap bmp = img;
      bitmapDC.SelectObject(bmp);
    }
    else
      bitmapDC.SelectObject(*m_bitmaps[m_displayed]);

    dc.Blit(point.x + 1, point.y - m_center + 1, m_width, m_height, &bitmapDC, 0, 0);
  }
  MathCell::Draw(parser, point, fontsize, all);
}

wxString SlideShow::ToString(bool all)
{
  return wxT(" << Graphics >> ") +
         MathCell::ToString(all);
}

wxString SlideShow::ToTeX(bool all)
{
  return wxT(" << Graphics >> ") +
         MathCell::ToTeX(all);
}

wxString SlideShow::ToXML(bool all)
{
  wxString images;

  for (int i=0; i<m_size; i++) {
    wxImage image = m_bitmaps[i]->ConvertToImage();
    wxString basename = ImgCell::WXMXGetNewFileName();

    // add to memory
    wxMemoryFSHandler::AddFile(basename, image, wxBITMAP_TYPE_PNG);

    images += basename + wxT(";");
  }

  return wxT("\n<slide>") + images + wxT("</slide>") +
         MathCell::ToXML(all);
}

bool SlideShow::ToImageFile(wxString file)
{
  wxImage image = m_bitmaps[m_displayed]->ConvertToImage();

  return image.SaveFile(file, wxBITMAP_TYPE_PNG);
}

bool SlideShow::ToGif(wxString file)
{
  wxArrayString which;
  bool retval = true;

  wxString convert(wxT("convert -delay 40 "));
  wxString convertArgs;

  wxString tmpdir = wxFileName::GetTempDir();

  for (int i=0; i<m_size; i++)
  {
    wxFileName imgname(tmpdir, wxString::Format(wxT("wxm_anim%d.png"), i));

    wxImage image = m_bitmaps[i]->ConvertToImage();
    image.SaveFile(imgname.GetFullPath(), wxBITMAP_TYPE_PNG);

    convert << wxT(" \"") << imgname.GetFullPath() << wxT("\"");
  }

  convert << wxT(" \"") << file << wxT("\"");

#if defined __WXMSW__
  if (!wxShell(convert))
#else
  if (wxExecute(convert, wxEXEC_SYNC) != 0)
#endif
  {
    retval = false;
    wxMessageBox(_("There was an error during GIF export!\n\nMake sure ImageMagick is installed and wxMaxima can find the convert program."),
        wxT("Error"), wxICON_ERROR);
  }

  for (int i=0; i<m_size; i++)
  {
    wxFileName imgname(tmpdir, wxString::Format(wxT("wxm_anim%d.png"), i));
    wxRemoveFile(imgname.GetFullPath());
  }

  return retval;
}

bool SlideShow::CopyToClipboard()
{
  if (wxTheClipboard->Open())
  {
    bool res = wxTheClipboard->SetData(new wxBitmapDataObject(*m_bitmaps[m_displayed]));
    wxTheClipboard->Close();
    return res;
  }
  return false;
}

