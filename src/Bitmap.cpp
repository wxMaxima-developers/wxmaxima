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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//

/*! \file
  This file defines the class BitMap that renders math as bitmap.
 */

#include "Bitmap.h"
#include "Configuration.h"
#include "GroupCell.h"

#include <wx/config.h>
#include <wx/clipbrd.h>

#define BM_FULL_WIDTH 1000

Bitmap::Bitmap(Configuration **configuration, int scale)
{
  m_scale = scale;

  m_configuration = configuration;
  m_oldconfig = *m_configuration;
  m_tree = NULL;

  m_dc = new wxMemoryDC();
  m_bmp.CreateScaled(m_width = 10, m_height= 10, 24, scale);
  m_dc->SelectObject(m_bmp);
  m_dc->SetUserScale(m_scale, m_scale);
  m_dc->SetPen(wxNullPen);
  
  *m_configuration = new Configuration(*m_dc);
  (*m_configuration)->ShowCodeCells(m_oldconfig->ShowCodeCells());
  (*m_configuration)->SetZoomFactor_temporarily(1.0);
  (*m_configuration)->SetClientWidth(BM_FULL_WIDTH);
  (*m_configuration)->SetClientHeight(BM_FULL_WIDTH);
  (*m_configuration)->SetForceUpdate(true);
}

Bitmap::~Bitmap()
{
  wxDELETE(m_tree);
  wxDELETE(m_dc);
  wxDELETE(*m_configuration);
  *m_configuration = m_oldconfig;
  (*m_configuration)->SetForceUpdate(false);
}

bool Bitmap::SetData(MathCell *tree, long int maxSize)
{
  wxDELETE(m_tree);
  m_tree = tree;
  m_tree->ResetSize();
  return Layout(maxSize);
}

bool Bitmap::Layout(long int maxSize)
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
    GroupCell *tmp = dynamic_cast<GroupCell *>(m_tree);
    while (tmp != NULL)
    {
      tmp->Recalculate();
      tmp = dynamic_cast<GroupCell *>(tmp->m_next);
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
    wxDELETE(m_dc);
    m_bmp.CreateScaled(m_width, m_height, 24, m_scale);
    if(!m_bmp.IsOk())
    {
      m_bmp = wxNullBitmap;
      return false;
    }
    else
    {
      m_dc = new wxMemoryDC();
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

double Bitmap::GetRealWidth()
{
  return m_width * m_scale;
}

double Bitmap::GetRealHeight()
{
  return m_height * m_scale;
}

void Bitmap::RecalculateHeight()
{
  int fontsize = 12;
  wxConfig::Get()->Read(wxT("fontSize"), &fontsize);
  int mfontsize = fontsize;
  wxConfig::Get()->Read(wxT("mathfontsize"), &mfontsize);
  MathCell *tmp = m_tree;

  while (tmp != NULL)
  {
    tmp->RecalculateHeight(tmp->IsMath() ? mfontsize : fontsize);
    tmp = tmp->m_next;
  }
}

void Bitmap::RecalculateWidths()
{
  int fontsize = 12;
  wxConfig::Get()->Read(wxT("fontSize"), &fontsize);
  int mfontsize = fontsize;
  wxConfig::Get()->Read(wxT("mathfontsize"), &mfontsize);

  MathCell *tmp = m_tree;

  while (tmp != NULL)
  {
    tmp->RecalculateWidths(tmp->IsMath() ? mfontsize : fontsize);
    tmp = tmp->m_next;
  }
}

void Bitmap::BreakLines()
{
  int fullWidth = BM_FULL_WIDTH * m_scale;
  int currentWidth = 0;

  MathCell *tmp = m_tree;

  while (tmp != NULL)
  {
    if (!tmp->m_isBroken)
    {
      tmp->BreakLine(false);
      tmp->ResetData();
      if (tmp->BreakLineHere() ||
          (currentWidth + tmp->GetWidth() >= fullWidth))
      {
        currentWidth = tmp->GetWidth();
        tmp->BreakLine(true);
      }
      else
        currentWidth += (tmp->GetWidth() + MC_CELL_SKIP);
    }
    tmp = tmp->m_nextToDraw;
  }
}

void Bitmap::GetMaxPoint(int *width, int *height)
{
  MathCell *tmp = m_tree;
  int currentHeight = 0;
  int currentWidth = 0;
  *width = 0;
  *height = 0;
  bool bigSkip = false;
  bool firstCell = true;
  while (tmp != NULL)
  {
    if (!tmp->m_isBroken)
    {
      if (tmp->BreakLineHere() || firstCell)
      {
        firstCell = false;
        currentHeight += tmp->GetMaxHeight();
        if (bigSkip)
          currentHeight += MC_LINE_SKIP;
        *height = currentHeight;
        currentWidth = tmp->GetWidth();
        *width = MAX(currentWidth, *width);
      }
      else
      {
        currentWidth += (tmp->GetWidth() + MC_CELL_SKIP);
        *width = MAX(currentWidth - MC_CELL_SKIP, *width);
      }
      bigSkip = tmp->m_bigSkip;
    }
    tmp = tmp->m_nextToDraw;
  }
}

void Bitmap::Draw()
{
  MathCell::ClipToDrawRegion(false);
  MathCell *tmp = m_tree;

  wxString bgColStr = wxT("white");
  wxConfig::Get()->Read(wxT("Style/Background/color"), &bgColStr);
  m_dc->SetBackground(*(wxTheBrushList->FindOrCreateBrush(bgColStr, wxBRUSHSTYLE_SOLID)));
  m_dc->Clear();

  if (tmp != NULL)
  {
    wxPoint point;
    point.x = 0;
    point.y = tmp->GetMaxCenter();
    int fontsize = 12;
    int drop = tmp->GetMaxDrop();

    wxConfig::Get()->Read(wxT("fontSize"), &fontsize);
    int mfontsize = fontsize;
    wxConfig::Get()->Read(wxT("mathfontsize"), &mfontsize);

    while (tmp != NULL)
    {
      if (!tmp->m_isBroken)
      {
        tmp->Draw(point, tmp->IsMath() ? mfontsize : fontsize);
        if ((tmp->m_next != NULL) && (tmp->m_next->BreakLineHere()))
        {
          point.x = 0;
          point.y += drop + tmp->m_next->GetMaxCenter();
          if (tmp->m_bigSkip)
            point.y += MC_LINE_SKIP;
          drop = tmp->m_next->GetMaxDrop();
        }
        else
          point.x += (tmp->GetWidth() + MC_CELL_SKIP);
      }
      else
      {
        if ((tmp->m_next != NULL) && (tmp->m_next->BreakLineHere()))
        {
          point.x = 0;
          point.y += drop + tmp->m_next->GetMaxCenter();
          if (tmp->m_bigSkip)
            point.y += MC_LINE_SKIP;
          drop = tmp->m_next->GetMaxDrop();
        }
      }
      tmp = tmp->m_nextToDraw;
    }
  }
  // Update the bitmap's size information.
  m_ppi = m_dc->GetPPI();
  m_ppi.x *= m_scale;
  m_ppi.y *= m_scale;
  MathCell::ClipToDrawRegion(true);
}

wxSize Bitmap::ToFile(wxString file)
{
  // Assign an resolution to the bitmap.
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

bool Bitmap::ToClipboard()
{
  if (wxTheClipboard->Open())
  {
    bool res = wxTheClipboard->SetData(new wxBitmapDataObject(m_bmp));
    wxTheClipboard->Close();
    return res;
  }
  wxTheClipboard->Close();
  return false;
}

void Bitmap::BreakUpCells()
{
  MathCell *tmp = m_tree;
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
    tmp = tmp->m_nextToDraw;
  }
}
