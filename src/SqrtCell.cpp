///
///  Copyright (C) 2004-2011 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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

#include "SqrtCell.h"
#include "TextCell.h"

#define SIGN_FONT_SCALE 2.0

SqrtCell::SqrtCell() : MathCell()
{
  m_innerCell = NULL;
  m_open = new TextCell(wxT("sqrt("));
  m_close = new TextCell(wxT(")"));
}


SqrtCell::~SqrtCell()
{
  if (m_innerCell != NULL)
    delete m_innerCell;
  if (m_next != NULL)
    delete m_next;
  delete m_open;
  delete m_close;
}

void SqrtCell::SetParent(MathCell *parent, bool all)
{
  if (m_innerCell != NULL)
    m_innerCell->SetParent(parent, true);
  if (m_open != NULL)
    m_open->SetParent(parent, true);
  if (m_close != NULL)
    m_close->SetParent(parent, true);

  MathCell::SetParent(parent, all);
}

MathCell* SqrtCell::Copy(bool all)
{
  SqrtCell* tmp = new SqrtCell;
  CopyData(this, tmp);
  tmp->SetInner(m_innerCell->Copy(true));
  if (all && m_next != NULL)
    tmp->AppendCell(m_next->Copy(all));
  return tmp;
}

void SqrtCell::Destroy()
{
  if (m_innerCell != NULL)
    delete m_innerCell;
  m_innerCell = NULL;
  m_next = NULL;
}

void SqrtCell::SetInner(MathCell *inner)
{
  if (inner == NULL)
    return ;
  if (m_innerCell != NULL)
    delete m_innerCell;
  m_innerCell = inner;

  m_last = inner;
  while (m_last->m_next != NULL)
    m_last = m_last->m_next;
}

void SqrtCell::RecalculateWidths(CellParser& parser, int fontsize, bool all)
{
  double scale = parser.GetScale();
  m_innerCell->RecalculateWidths(parser, fontsize, true);
  if (parser.CheckTeXFonts())
  {
    wxDC& dc = parser.GetDC();
    double scale = parser.GetScale();
    m_innerCell->RecalculateSize(parser, fontsize, true);

    m_signFontScale = 1.0;
    int fontsize1 = (int)(SIGN_FONT_SCALE*scale*fontsize*m_signFontScale + 0.5);

    dc.SetFont(wxFont(fontsize1, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false, parser.GetTeXCMEX()));
    dc.GetTextExtent(wxT("s"), &m_signWidth, &m_signSize);
    m_signTop = m_signSize / 5;
    m_width = m_innerCell->GetFullWidth(scale) + m_signWidth;

    int size = m_innerCell->GetMaxHeight();

    if  (size <= (m_signSize) / 5)  {
      m_signType = 1;
      m_signFontScale = (5.0 * size) / (1.5 * m_signSize);
    }
    else if (size <= (2*m_signSize) / 5) {
      m_signType = 2;
      m_signFontScale = (5.0 * size) / (2.2 * m_signSize);
    }
    else if (size <= (3*m_signSize) / 5) {
      m_signType = 3;
      m_signFontScale = (5.0 * size) / (3.0 * m_signSize);
    }
    else if (size <= (4*m_signSize) / 5 ) {
      m_signType = 4;
      m_signFontScale = (5.0 * size) / (3.8 * m_signSize);
    }
    else {
      m_signType = 5;
      m_signFontScale = 1.0;
    }

    fontsize1 = (int)(SIGN_FONT_SCALE*scale*fontsize*m_signFontScale + 0.5);
    dc.SetFont(wxFont(fontsize1, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false, parser.GetTeXCMEX()));
    dc.GetTextExtent(wxT("s"), &m_signWidth, &m_signSize);
    m_signTop = m_signSize / 5;
    m_width = m_innerCell->GetFullWidth(scale) + m_signWidth;
  }
  else
    m_width = m_innerCell->GetFullWidth(scale) + SCALE_PX(10, scale) +
              3 * SCALE_PX(1, scale) + 1;
  m_open->RecalculateWidths(parser, fontsize, all);
  m_close->RecalculateWidths(parser, fontsize, all);
  MathCell::RecalculateWidths(parser, fontsize, all);
}

void SqrtCell::RecalculateSize(CellParser& parser, int fontsize, bool all)
{
  double scale = parser.GetScale();
  m_innerCell->RecalculateSize(parser, fontsize, true);
  m_height = m_innerCell->GetMaxHeight() + SCALE_PX(3, scale);
  m_center = m_innerCell->GetMaxCenter() + SCALE_PX(3, scale);
  m_open->RecalculateSize(parser, fontsize, all);
  m_close->RecalculateSize(parser, fontsize, all);
  MathCell::RecalculateSize(parser, fontsize, all);
}

void SqrtCell::Draw(CellParser& parser, wxPoint point, int fontsize, bool all)
{
  if (DrawThisCell(parser, point))
  {
    wxDC& dc = parser.GetDC();
    double scale = parser.GetScale();

    wxPoint in(point);

    if (parser.CheckTeXFonts())
    {
      SetPen(parser);

      in.x += m_signWidth;
      double scale = parser.GetScale();

      int fontsize1 = (int)(SIGN_FONT_SCALE*scale*fontsize*m_signFontScale + 0.5);

      dc.SetFont(wxFont(fontsize1, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false, parser.GetTeXCMEX()));
      SetForeground(parser);
      if (m_signType < 4) {
        dc.DrawText(
            m_signType == 1 ? wxT("p") :
              m_signType == 2 ? wxT("q") :
                m_signType == 3 ? wxT("r") : wxT("s"),
          point.x,
          point.y - m_innerCell->GetMaxCenter() - m_signTop);
      }
      else {
        int yBottom = point.y + m_innerCell->GetMaxDrop() - 3.2*m_signTop;
        int yTop = point.y - m_innerCell->GetMaxCenter() - m_signTop;
        int dy = m_signSize / 10;
        dc.DrawText(wxT("t"),
            point.x,
            yBottom);
        dc.DrawText(wxT("v"),
            point.x,
            yTop);
        while (yTop < yBottom) {
          yTop += dy;
          dc.DrawText(wxT("u"),
                      point.x,
                      yTop);
        }
      }

      dc.DrawLine(point.x + m_signWidth,
                  point.y - m_innerCell->GetMaxCenter(),
                  point.x + m_signWidth + m_innerCell->GetFullWidth(scale),
                  point.y - m_innerCell->GetMaxCenter());

      UnsetPen(parser);
    }
    else
    {
      in.x += SCALE_PX(10, scale) + SCALE_PX(1, scale) + 1;

      SetPen(parser);
      dc.DrawLine(point.x,
                  point.y,
                  point.x + SCALE_PX(3, scale),
                  point.y - SCALE_PX(1, scale));
      dc.DrawLine(point.x + SCALE_PX(3, scale),
                  point.y - SCALE_PX(1, scale),
                  point.x + SCALE_PX(7, scale),
                  point.y + m_height - m_center - SCALE_PX(4, scale));
      dc.DrawLine(point.x + SCALE_PX(3, scale) + 1,
                  point.y - SCALE_PX(1, scale),
                  point.x + SCALE_PX(7, scale) + 1,
                  point.y + m_height - m_center - SCALE_PX(4, scale));
      dc.DrawLine(point.x + SCALE_PX(7, scale) + 1,
                  point.y + m_height - m_center - SCALE_PX(4, scale),
                  point.x + SCALE_PX(10, scale),
                  point.y - m_center + SCALE_PX(2, scale));
      dc.DrawLine(point.x + SCALE_PX(10, scale),
                  point.y - m_center + SCALE_PX(2, scale),
                  point.x + m_width - SCALE_PX(1, scale),
                  point.y - m_center + SCALE_PX(2, scale));
      dc.DrawLine(point.x + m_width - SCALE_PX(1, scale),
                  point.y - m_center + SCALE_PX(2, scale),
                  point.x + m_width - SCALE_PX(1, scale),
                  point.y - m_center + SCALE_PX(6, scale));
      UnsetPen(parser);
    }

    m_innerCell->Draw(parser, in, fontsize, true);
  }
  MathCell::Draw(parser, point, fontsize, all);
}

wxString SqrtCell::ToString(bool all)
{
  if (m_isBroken)
    return wxEmptyString;
  return wxT("sqrt(") + m_innerCell->ToString(true) + wxT(")") +
         MathCell::ToString(all);
}

wxString SqrtCell::ToTeX(bool all)
{
  if (m_isBroken)
    return wxEmptyString;
  return wxT("\\sqrt{") + m_innerCell->ToTeX(true) + wxT("}") +
         MathCell::ToTeX(all);
}

wxString SqrtCell::ToXML(bool all)
{
//  if (m_isBroken)
//    return wxEmptyString;
  return _T("<q>") + m_innerCell->ToXML(true) + _T("</q>") +
    MathCell::ToXML(all);
}

void SqrtCell::SelectInner(wxRect& rect, MathCell **first, MathCell **last)
{
  *first = NULL;
  *last = NULL;

  if (m_innerCell->ContainsRect(rect))
    m_innerCell->SelectRect(rect, first, last);

  if (*first == NULL || *last == NULL)
  {
    *first = this;
    *last = this;
  }
}

bool SqrtCell::BreakUp()
{
  if (!m_isBroken)
  {
    m_isBroken = true;
    m_open->m_nextToDraw = m_innerCell;
    m_innerCell->m_previousToDraw = m_open;
    m_last->m_nextToDraw = m_close;
    m_close->m_previousToDraw = m_last;
    m_close->m_nextToDraw = m_nextToDraw;
    if (m_nextToDraw != NULL)
      m_nextToDraw->m_previousToDraw = m_close;
    m_nextToDraw = m_open;
    return true;
  }
  return false;
}

void SqrtCell::Unbreak(bool all)
{
  if (m_isBroken)
    m_innerCell->Unbreak(true);
  MathCell::Unbreak(all);
}
