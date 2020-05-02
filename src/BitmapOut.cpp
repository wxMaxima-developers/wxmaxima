// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2015-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class BitMap that renders math as bitmap.
 */

#include "BitmapOut.h"
#include "Configuration.h"

#include <wx/config.h>
#include <wx/clipbrd.h>

#define BM_FULL_WIDTH 1000

BitmapOut::BitmapOut(Configuration **configuration, int scale):
  m_dc(new wxMemoryDC())
{
  m_scale = scale;

  m_configuration = configuration;
  m_oldconfig = *m_configuration;
  m_bmp.CreateScaled(m_width = 10, m_height= 10, 24, scale);
  m_dc->SelectObject(m_bmp);
  m_dc->SetUserScale(m_scale, m_scale);
  m_dc->SetPen(wxNullPen);
  
  *m_configuration = new Configuration(m_dc.get());
  (*m_configuration)->ShowCodeCells(m_oldconfig->ShowCodeCells());
  (*m_configuration)->SetZoomFactor_temporarily(1.0);
  (*m_configuration)->SetClientWidth(BM_FULL_WIDTH);
  (*m_configuration)->SetClientHeight(BM_FULL_WIDTH);
  (*m_configuration)->RecalculationForce(true);
}

BitmapOut::~BitmapOut()
{
  wxDELETE(*m_configuration);
  *m_configuration = m_oldconfig;
  (*m_configuration)->FontChanged(true);
  (*m_configuration)->RecalculationForce(true);
}

bool BitmapOut::SetData(Cell *tree, long int maxSize)
{
  m_tree = std::unique_ptr<Cell>(tree);
  m_tree->ResetSize();
  return Layout(maxSize);
}

bool BitmapOut::Layout(long int maxSize)
{
  if(m_tree == NULL)
    return false;
  
  if (m_tree->GetType() != MC_TYPE_GROUP)
  {
    RecalculateWidths();
    BreakUpCells();
    BreakLines();
    RecalculateHeight();
  }
  else
  {
    GroupCell *tmp = dynamic_cast<GroupCell *>(m_tree.get());
    while (tmp != NULL)
    {
      tmp->Recalculate();
      tmp = tmp->GetNext();
    }
  }

  GetMaxPoint(&m_width, &m_height);

  // Too big bitmaps or bitmaps that are too wide or high can crash windows
  // or the X server.
  if ((maxSize < 0) ||
      (
              (m_width * m_height * m_scale * m_scale < maxSize) &&
              (m_width * m_scale < 20000) &&
              (m_height * m_scale < 20000)
      )
          )
  {
    // The depth 24 hinders wxWidgets from creating rgb0 bitmaps that some
    // windows applications will interpret as rgba if they appear on
    // the clipboards and therefore render them all-transparent.
    m_bmp.CreateScaled(m_width, m_height, 24, m_scale);
    if(!m_bmp.IsOk())
    {
      m_bmp = wxNullBitmap;
      return false;
    }
    else
    {
      m_dc = std::unique_ptr<wxMemoryDC>(new wxMemoryDC());
      m_dc->SelectObject(m_bmp);
      if(m_dc->IsOk())
      {
        m_dc->SetUserScale(m_scale, m_scale);
        (*m_configuration)->SetContext(*m_dc);
        m_dc->SetPen(wxNullPen);
        Draw();
        return true;
      }
      else
      {
        m_bmp = wxNullBitmap;
        return false;
      }
    }
  }
  else
  {
    m_bmp = wxNullBitmap;
    return false;
  }
}

double BitmapOut::GetRealWidth() const
{
  return m_width * m_scale;
}

double BitmapOut::GetRealHeight() const
{
  return m_height * m_scale;
}

void BitmapOut::RecalculateHeight()
{
  int fontsize = 12;
  wxConfig::Get()->Read(wxT("fontSize"), &fontsize);
  int mfontsize = fontsize;
  wxConfig::Get()->Read(wxT("mathfontsize"), &mfontsize);
  Cell *tmp = m_tree.get();

  while (tmp != NULL)
  {
    tmp->RecalculateHeight(tmp->IsMath() ? mfontsize : fontsize);
    tmp = tmp->m_next;
  }
}

// cppcheck-suppress functionStatic
// cppcheck-suppress functionConst
void BitmapOut::RecalculateWidths()
{
  int fontsize = 12;
  wxConfig::Get()->Read(wxT("fontSize"), &fontsize);
  int mfontsize = fontsize;
  wxConfig::Get()->Read(wxT("mathfontsize"), &mfontsize);

  Cell *tmp = m_tree.get();

  while (tmp != NULL)
  {
    tmp->RecalculateWidths(tmp->IsMath() ? mfontsize : fontsize);
    tmp = tmp->m_next;
  }
}

void BitmapOut::BreakLines()
{
  int fullWidth = BM_FULL_WIDTH * m_scale;
  int currentWidth = 0;

  Cell *tmp = m_tree.get();

  while (tmp != NULL)
  {
    if (!tmp->m_isBrokenIntoLines)
    {
      if(tmp->SoftLineBreak(false))
      {
        tmp->ResetData();
      }
      if (tmp->BreakLineHere() ||
          (currentWidth + tmp->GetWidth() >= fullWidth))
      {
        currentWidth = tmp->GetWidth();
        tmp->SoftLineBreak(true);
      }
      else
        currentWidth += (tmp->GetWidth());
    }
    tmp = tmp->GetNextToDraw();
  }
}

void BitmapOut::GetMaxPoint(int *width, int *height) const
{
  Cell *tmp = m_tree.get();
  int currentHeight = 0;
  int currentWidth = 0;
  *width = 0;
  *height = 0;
  bool firstCell = true;
  while (tmp != NULL)
  {
    if (!tmp->m_isBrokenIntoLines)
    {
      if (tmp->BreakLineHere() || firstCell)
      {
        firstCell = false;
        currentHeight += tmp->GetHeightList();
        *height = currentHeight;
        currentWidth = tmp->GetWidth();
        *width = wxMax(currentWidth, *width);
      }
      else
      {
        currentWidth += (tmp->GetWidth());
        *width = wxMax(currentWidth, *width);
      }
    }
    tmp = tmp->GetNextToDraw();
  }
}

void BitmapOut::Draw()
{
  (*m_configuration)->ClipToDrawRegion(false);
  Cell *tmp = m_tree.get();

  wxString bgColStr = wxT("white");
  wxConfig::Get()->Read(wxT("Style/Background/color"), &bgColStr);
  m_dc->SetBackground(*(wxTheBrushList->FindOrCreateBrush(bgColStr, wxBRUSHSTYLE_SOLID)));
  m_dc->Clear();

  if (tmp != NULL)
  {
    wxPoint point;
    point.x = 0;
    point.y = tmp->GetCenterList();
    int fontsize = 12;
    int drop = tmp->GetMaxDrop();

    wxConfig::Get()->Read(wxT("fontSize"), &fontsize);
    int mfontsize = fontsize;
    wxConfig::Get()->Read(wxT("mathfontsize"), &mfontsize);

    while (tmp != NULL)
    {
      if (!tmp->m_isBrokenIntoLines)
      {
        tmp->Draw(point);
        if ((tmp->m_next != NULL) && (tmp->m_next->BreakLineHere()))
        {
          point.x = 0;
          point.y += drop + tmp->m_next->GetCenterList();
          drop = tmp->m_next->GetMaxDrop();
        }
        else
          point.x += (tmp->GetWidth());
      }
      else
      {
        if ((tmp->m_next != NULL) && (tmp->m_next->BreakLineHere()))
        {
          point.x = 0;
          point.y += drop + tmp->m_next->GetCenterList();
          drop = tmp->m_next->GetMaxDrop();
        }
      }
      tmp = tmp->GetNextToDraw();
    }
  }
  // Update the bitmap's size information.
  m_ppi = m_dc->GetPPI();
  m_ppi.x *= m_scale;
  m_ppi.y *= m_scale;
}

wxSize BitmapOut::ToFile(wxString file)
{
  // Assign a resolution to the bitmap.
  wxImage img = m_bmp.ConvertToImage();
  int resolution = img.GetOptionInt(wxIMAGE_OPTION_RESOLUTION);
  if (resolution <= 0)
    resolution = 75;
  img.SetOption(wxIMAGE_OPTION_RESOLUTION, resolution * m_scale);

  bool success = false;
  if (file.Right(4) == wxT(".bmp"))
    success = img.SaveFile(file, wxBITMAP_TYPE_BMP);
  else if (file.Right(4) == wxT(".xpm"))
    success = img.SaveFile(file, wxBITMAP_TYPE_XPM);
  else if (file.Right(4) == wxT(".jpg"))
    success = img.SaveFile(file, wxBITMAP_TYPE_JPEG);
  else
  {
    if (file.Right(4) != wxT(".png"))
      file = file + wxT(".png");
    success = img.SaveFile(file, wxBITMAP_TYPE_PNG);
  }

  wxSize retval;
  if (success)
  {
    retval.x = GetRealWidth();
    retval.y = GetRealHeight();
    return retval;
  }
  else
  {
    retval.x = -1;
    retval.y = -1;
    return retval;
  };
}

bool BitmapOut::ToClipboard()
{
  wxASSERT_MSG(!wxTheClipboard->IsOpened(),_("Bug: The clipboard is already opened"));
  if (wxTheClipboard->Open())
  {
    bool res = wxTheClipboard->SetData(new wxBitmapDataObject(m_bmp));
    wxTheClipboard->Close();
    return res;
  }
  return false;
}

void BitmapOut::BreakUpCells()
{
  Cell *tmp = m_tree.get();
  int fontsize = 12;
  wxConfig::Get()->Read(wxT("fontSize"), &fontsize);
  int mfontsize = fontsize;
  wxConfig::Get()->Read(wxT("mathfontsize"), &mfontsize);

  while (tmp != NULL)
  {
    if (tmp->GetWidth() > BM_FULL_WIDTH * m_scale)
    {
      if (tmp->BreakUp())
      {
        tmp->RecalculateWidths(tmp->IsMath() ? mfontsize : fontsize);
        tmp->RecalculateHeight(tmp->IsMath() ? mfontsize : fontsize);
      }
    }
    tmp = tmp->GetNextToDraw();
  }
}
