///
///  Copyright (C) 2004-2009 Andrej Vodopivec <andrejv@users.sourceforge.net>
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

#include "Bitmap.h"
#include "CellParser.h"
#include "GroupCell.h"

#include <wx/config.h>
#include <wx/clipbrd.h>

#define BM_FULL_WIDTH 600

Bitmap::Bitmap()
{
  m_tree = NULL;
  m_bmp.Create(10,10);
}

Bitmap::~Bitmap()
{
  if (m_tree != NULL)
    DestroyTree();
}

void Bitmap::SetData(MathCell* tree)
{
  if (m_tree != NULL)
    delete m_tree;
  m_tree = tree;
  Layout();
}

void Bitmap::Layout()
{
  if (m_tree->GetType() != MC_TYPE_GROUP)
  {
    RecalculateWidths();
    BreakUpCells();
    BreakLines();
    RecalculateSize();
  }
  else {
    int fontsize = 12;
    wxConfig::Get()->Read(wxT("fontSize"), &fontsize);
    int mfontsize = fontsize;
    wxConfig::Get()->Read(wxT("mathfontsize"), &mfontsize);
    GroupCell* tmp = (GroupCell *)m_tree;

    wxMemoryDC dc;
    dc.SelectObject(m_bmp);
    CellParser parser(dc);

    while (tmp != NULL)
    {
      tmp->Recalculate(parser, fontsize, mfontsize);
      tmp = (GroupCell *)tmp->m_next;
    }
  }

  int width, height;
  GetMaxPoint(&width, &height);
  m_bmp.Create(width, height);

  Draw();
}

void Bitmap::RecalculateSize()
{
  int fontsize = 12;
  wxConfig::Get()->Read(wxT("fontSize"), &fontsize);
  int mfontsize = fontsize;
  wxConfig::Get()->Read(wxT("mathfontsize"), &mfontsize);
  MathCell* tmp = m_tree;

  wxMemoryDC dc;
  dc.SelectObject(m_bmp);
  CellParser parser(dc);

  while (tmp != NULL)
  {
    tmp->RecalculateSize(parser, tmp->IsMath() ? mfontsize : fontsize, false);
    tmp = tmp->m_next;
  }
}

void Bitmap::RecalculateWidths()
{
  int fontsize = 12;
  wxConfig::Get()->Read(wxT("fontSize"), &fontsize);
  int mfontsize = fontsize;
  wxConfig::Get()->Read(wxT("mathfontsize"), &mfontsize);

  MathCell* tmp = m_tree;

  wxMemoryDC dc;
  dc.SelectObject(m_bmp);
  CellParser parser(dc);
  parser.SetClientWidth(BM_FULL_WIDTH);

  while (tmp != NULL)
  {
    tmp->RecalculateWidths(parser,  tmp->IsMath() ? mfontsize : fontsize, false);
    tmp = tmp->m_next;
  }
}

void Bitmap::BreakLines()
{
  int fullWidth = BM_FULL_WIDTH;
  int currentWidth = 0;

  MathCell* tmp = m_tree;

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

void Bitmap::GetMaxPoint(int* width, int* height)
{
  MathCell* tmp = m_tree;
  int currentHeight = MC_BASE_INDENT;
  int currentWidth = MC_BASE_INDENT;
  *width = MC_BASE_INDENT;
  *height = MC_BASE_INDENT;
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
        currentWidth = MC_BASE_INDENT + tmp->GetWidth();
        *width = MAX(currentWidth + MC_BASE_INDENT, *width);
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
  MathCell* tmp = m_tree;
  wxMemoryDC dc;
  dc.SelectObject(m_bmp);

  wxString bgColStr = wxT("white");
  wxConfig::Get()->Read(wxT("Style/Background/color"), &bgColStr);
  dc.SetBackground(*(wxTheBrushList->FindOrCreateBrush(bgColStr, wxSOLID)));
  dc.Clear();

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

    CellParser parser(dc);

    while (tmp != NULL)
    {
      if (!tmp->m_isBroken)
      {
        tmp->Draw(parser, point, tmp->IsMath() ? mfontsize : fontsize, false);
        if (tmp->m_next != NULL && tmp->m_next->BreakLineHere())
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
        if (tmp->m_next != NULL && tmp->m_next->BreakLineHere())
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
  dc.SelectObject(wxNullBitmap);
}

bool Bitmap::ToFile(wxString file)
{
  bool res = false;
  if (file.Right(4) == wxT(".bmp"))
    res = m_bmp.SaveFile(file, wxBITMAP_TYPE_BMP);
  else if (file.Right(4) == wxT(".xpm"))
    res = m_bmp.SaveFile(file, wxBITMAP_TYPE_XPM);
  else if (file.Right(4) == wxT(".jpg"))
    res = m_bmp.SaveFile(file, wxBITMAP_TYPE_JPEG);
  else
  {
    if (file.Right(4) != wxT(".png"))
      file = file + wxT(".png");
    res = m_bmp.SaveFile(file, wxBITMAP_TYPE_PNG);
  }
  return res;
}

bool Bitmap::ToClipboard()
{
  if (wxTheClipboard->Open())
  {
    bool res = wxTheClipboard->SetData(new wxBitmapDataObject(m_bmp));
    wxTheClipboard->Close();
    return res;
  }
  return false;
}

void Bitmap::DestroyTree()
{
  if (m_tree != NULL)
  {
    MathCell *tmp1, *tmp = m_tree;
    while (tmp != NULL)
    {
      tmp1 = tmp;
      tmp = tmp->m_next;
      tmp1->Destroy();
      delete tmp1;
    }
  }
  m_tree = NULL;
}

void Bitmap::BreakUpCells()
{
  MathCell *tmp = m_tree;
  int fontsize = 12;
  wxConfig::Get()->Read(wxT("mathfontsize"), &fontsize);
  wxMemoryDC dc;
  CellParser parser(dc);

  while (tmp != NULL)
  {
    if (tmp->GetWidth() > BM_FULL_WIDTH)
    {
      if (tmp->BreakUp())
      {
        tmp->RecalculateWidths(parser, fontsize, false);
        tmp->RecalculateSize(parser, fontsize, false);
      }
    }
    tmp = tmp->m_nextToDraw;
  }
}
