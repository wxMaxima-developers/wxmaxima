///
///  Copyright (C) 2007 Andrej Vodopivec <andrejv@users.sourceforge.net>
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

SlideShow::SlideShow() : MathCell()
{
  m_size = m_displayed = 0;
  m_bitmap = NULL;
  m_images = NULL;
  m_type = MC_TYPE_SLIDE;
}

SlideShow::~SlideShow()
{
  if (m_bitmap != NULL)
    delete m_bitmap;
  if (m_images != NULL)
    delete m_images;
  if (m_next != NULL)
    delete m_next;
}

void SlideShow::LoadImages(wxArrayString images)
{
  printf("Reading images (%d)\n", images.GetCount());
  if (m_bitmap != NULL)
    delete m_bitmap;
  if (m_images != NULL)
    delete m_images;

  m_size = images.GetCount();
  m_images = new wxImage[m_size];

  for (int i=0; i<m_size; i++)
  {
    printf("  -> image", i);
    bool loadedImage = false;

    if (wxFileExists(images[i]))
    {
      printf(" OK\n");
      wxFile imageFile(images[i]);
	
      if (imageFile.Length())
      {
	wxImage pngImage(images[i], wxBITMAP_TYPE_PNG);

	if (pngImage.Ok())
	{
	  loadedImage = true;
	  m_images[i] = pngImage;
	}
      }

      imageFile.Close();
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
      m_images[i] = bitmap->ConvertToImage();
      delete bitmap;
    }
  }

  m_displayed = 0;
  m_bitmap = new wxBitmap(m_images[m_displayed]);
}

MathCell* SlideShow::Copy(bool all)
{
  ImgCell* tmp = new ImgCell;
  CopyData(this, tmp);

  tmp->m_bitmap = new wxBitmap(*m_bitmap);

  if (all && m_next != NULL)
    tmp->AppendCell(m_next->Copy(all));
  return tmp;
}

void SlideShow::Destroy()
{
  if (m_bitmap != NULL)
    delete m_bitmap;
  m_bitmap = NULL;
  m_next = NULL;
}

void SlideShow::SetDisplayedIndex(int ind)
{
  if (ind >= 0 && ind < m_size)
  {
    if (m_bitmap != NULL)
      delete m_bitmap;
    m_displayed = ind;
    m_bitmap = new wxBitmap(m_images[ind]);
  }
}

void SlideShow::RecalculateWidths(CellParser& parser, int fontsize, bool all)
{
  if (m_bitmap != NULL)
    m_width = m_bitmap->GetWidth() + 2;
  else
    m_width = 0;

  double scale = parser.GetScale();
  scale = MAX(scale, 1.0);

  m_width = (int) (scale * m_width);
}

void SlideShow::RecalculateSize(CellParser& parser, int fontsize, bool all)
{
  if (m_bitmap != NULL)
    m_height = m_bitmap->GetHeight() + 2;
  else
    m_height = 0;

  double scale = parser.GetScale();
  scale = MAX(scale, 1.0);

  m_height= (int) (scale * m_height);

  m_center = m_height / 2;
}

void SlideShow::Draw(CellParser& parser, wxPoint point, int fontsize, bool all)
{
  double scale = parser.GetScale();
  wxDC& dc = parser.GetDC();
  if (DrawThisCell(parser, point) && m_bitmap != NULL)
  {
    wxMemoryDC bitmapDC;
    double scale = parser.GetScale();
    scale = MAX(scale, 1.0);

    dc.DrawRectangle(wxRect(point.x, point.y - m_center, m_width, m_height));

    if (scale != 1.0)
    {
      wxImage img = m_bitmap->ConvertToImage();
      img.Rescale(m_width, m_height);

      wxBitmap bmp = img;
      bitmapDC.SelectObject(bmp);
    }
    else
      bitmapDC.SelectObject(*m_bitmap);

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
