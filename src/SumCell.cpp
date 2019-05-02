// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  This file defines the class SumCell

  SumCell is the Cell type that represents maxima's <code>sum()</code>, 
  <code>lsum</code> and <code>product()</code> 
  commands.
*/

#include "SumCell.h"
#include "TextCell.h"

#define SUM_SIGN "\x58"
#define PROD_SIGN "\x59"
#define SUM_DEC 2

SumCell::SumCell(Cell *parent, Configuration **config, CellPointers *cellPointers) : Cell(parent, config)
{
  m_cellPointers = cellPointers;
  m_base = NULL;
  m_under = NULL;
  m_over = NULL;
  m_signSize = 50;
  m_signTop = (2 * m_signSize) / 5;
  m_signWidth = 30;
  m_signWCenter = 15;
  m_sumStyle = SM_SUM;
}

Cell *SumCell::Copy()
{
  SumCell *tmp = new SumCell(m_group, m_configuration, m_cellPointers);
  CopyData(this, tmp);
  tmp->SetBase(m_base->CopyList());
  tmp->SetUnder(m_under->CopyList());
  tmp->SetOver(m_over->CopyList());
  tmp->m_sumStyle = m_sumStyle;

  return tmp;
}

SumCell::~SumCell()
{
  wxDELETE(m_base);
  wxDELETE(m_under);
  wxDELETE(m_over);
  m_base = NULL;
  m_under = NULL;
  m_over = NULL;
  MarkAsDeleted();
}

std::list<Cell *> SumCell::GetInnerCells()
{
  std::list<Cell *> innerCells;
  if(m_base)
    innerCells.push_back(m_base);
  if(m_under)
    innerCells.push_back(m_under);
  if(m_over)
    innerCells.push_back(m_over);
  return innerCells;
}

void SumCell::SetOver(Cell *over)
{
  if (over == NULL)
    return;
  wxDELETE(m_over);
  m_over = over;
}

void SumCell::SetBase(Cell *base)
{
  if (base == NULL)
    return;
  wxDELETE(m_base);
  m_base = base;
}

void SumCell::SetUnder(Cell *under)
{
  if (under == NULL)
    return;
  wxDELETE(m_under);
  m_under = under;
}

void SumCell::RecalculateWidths(int fontsize)
{
  Cell::RecalculateWidths(fontsize);
  Configuration *configuration = (*m_configuration);

  m_signSize = Scale_Px(50) * configuration->GetZoomFactor();
  m_signWidth = Scale_Px(30) * configuration->GetZoomFactor();
  m_signWCenter = Scale_Px(15) * configuration->GetZoomFactor();

  m_base->RecalculateWidthsList(fontsize);
  m_under->RecalculateWidthsList(MAX(MC_MIN_SIZE, fontsize - SUM_DEC));
  if (m_over == NULL)
    m_over = new TextCell(m_group, m_configuration, m_cellPointers);
  m_over->RecalculateWidthsList(MAX(MC_MIN_SIZE, fontsize - SUM_DEC));

  if (configuration->CheckTeXFonts())
  {
    wxDC *dc = configuration->GetDC();
    double fontsize1 = Scale_Px(configuration->GetMathFontSize());
    wxFont font(fontsize1, wxFONTFAMILY_MODERN,
                wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false,
                configuration->GetTeXCMEX());
    if (!font.IsOk())
      font = *wxNORMAL_FONT;
    wxASSERT(fontsize1 > 0);
#if wxCHECK_VERSION(3, 1, 2)
    font.SetFractionalPointSize(fontsize1);
#else
    font.SetPointSize(fontsize1);
#endif
    dc->SetFont(font);
    dc->GetTextExtent(m_sumStyle == SM_SUM ? wxT(SUM_SIGN) : wxT(PROD_SIGN), &m_signWidth, &m_signSize);
    m_signWCenter = m_signWidth / 2;
    m_signTop = (2 * m_signSize) / 5;
    m_signSize = (2 * m_signSize) / 5;
  }
  m_signWCenter = MAX(m_signWCenter, m_under->GetFullWidth() / 2);
  m_signWCenter = MAX(m_signWCenter, m_over->GetFullWidth() / 2);
  m_width = 2 * m_signWCenter + m_base->GetFullWidth() + Scale_Px(4);

  ResetData();
}

void SumCell::RecalculateHeight(int fontsize)
{
  Cell::RecalculateHeight(fontsize);
  m_under->RecalculateHeightList(MAX(MC_MIN_SIZE, fontsize - SUM_DEC));
  m_over->RecalculateHeightList(MAX(MC_MIN_SIZE, fontsize - SUM_DEC));
  m_base->RecalculateHeightList(fontsize);

  m_center = MAX(m_over->GetMaxHeight() + Scale_Px(4) + m_signSize / 2,
                 m_base->GetMaxCenter());
  m_height = m_center +
             MAX(m_under->GetMaxHeight() + Scale_Px(4) + m_signSize / 2,
                 m_base->GetMaxDrop());
}

void SumCell::Draw(wxPoint point)
{
  Cell::Draw(point);

  if (DrawThisCell(point))
  {
    Configuration *configuration = (*m_configuration);
    wxDC *dc = configuration->GetDC();

    wxPoint base(point), under(point), over(point), sign(point);

    under.x += m_signWCenter - m_under->GetFullWidth() / 2;
    under.y = point.y + m_signSize / 2 + m_under->GetMaxCenter() + Scale_Px(2);
    m_under->DrawList(under);

    over.x += m_signWCenter - m_over->GetFullWidth() / 2;
    over.y = point.y - m_signSize / 2 - m_over->GetMaxDrop() - Scale_Px(2);
    m_over->DrawList(over);

    if (configuration->CheckTeXFonts())
    {
      SetForeground();
      double fontsize1 = Scale_Px(configuration->GetMathFontSize());
      wxFont font(fontsize1, wxFONTFAMILY_MODERN,
                  wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false,
                  configuration->GetTeXCMEX());
      if (!font.IsOk())
        font = *wxNORMAL_FONT;
      wxASSERT(fontsize1 > 0);
#if wxCHECK_VERSION(3, 1, 2)
      font.SetFractionalPointSize(fontsize1);
#else
      font.SetPointSize(fontsize1);
#endif
      dc->SetFont(font);
      dc->DrawText(m_sumStyle == SM_SUM ? wxT(SUM_SIGN) : wxT(PROD_SIGN),
                  sign.x + m_signWCenter - m_signWidth / 2,
                  sign.y - m_signTop);
    }
    else
    {
      SetPen(1.5);
      if (m_sumStyle == SM_SUM)
      {
        wxDC *adc = configuration->GetAntialiassingDC();
        //DRAW SUM SIGN
        // Upper part
        adc->DrawLine(point.x + m_signWCenter + m_signWidth / 6,
                     point.y,
                     point.x + m_signWCenter - m_signWidth / 2,
                     point.y - m_signSize / 2 + 1);
        adc->DrawLine(point.x + m_signWCenter - m_signWidth / 2,
                     point.y - m_signSize / 2,
                     point.x + m_signWCenter + m_signWidth / 2,
                     point.y - m_signSize / 2);
        adc->DrawLine(point.x + m_signWCenter - m_signWidth / 2,
                     point.y - m_signSize / 2 + 1,
                     point.x + m_signWCenter + m_signWidth / 2,
                     point.y - m_signSize / 2 + 1);
        adc->DrawLine(point.x + m_signWCenter + m_signWidth / 2,
                     point.y - m_signSize / 2,
                     point.x + m_signWCenter + m_signWidth / 2,
                     point.y - m_signSize / 2 + Scale_Px(5));
        // Lower part
        adc->DrawLine(point.x + m_signWCenter + m_signWidth / 6,
                     point.y,
                     point.x + m_signWCenter - m_signWidth / 2,
                     point.y + m_signSize / 2 - 1);
        adc->DrawLine(point.x + m_signWCenter - m_signWidth / 2,
                     point.y + m_signSize / 2,
                     point.x + m_signWCenter + m_signWidth / 2,
                     point.y + m_signSize / 2);
        adc->DrawLine(point.x + m_signWCenter - m_signWidth / 2,
                     point.y + m_signSize / 2 - 1,
                     point.x + m_signWCenter + m_signWidth / 2,
                     point.y + m_signSize / 2 - 1);
        adc->DrawLine(point.x + m_signWCenter + m_signWidth / 2,
                     point.y + m_signSize / 2,
                     point.x + m_signWCenter + m_signWidth / 2,
                     point.y + m_signSize / 2 - Scale_Px(5));
      }
      else
      {
        // DRAW PRODUCT SIGN
        // Vertical lines
        dc->DrawLine(point.x + m_signWCenter + m_signWidth / 6,
                    point.y + m_signSize / 2,
                    point.x + m_signWCenter + m_signWidth / 6,
                    point.y - m_signSize / 2 + Scale_Px(4));
        dc->DrawLine(point.x + m_signWCenter - m_signWidth / 6,
                    point.y + m_signSize / 2,
                    point.x + m_signWCenter - m_signWidth / 6,
                    point.y - m_signSize / 2 + Scale_Px(4));
        // Horizonral line (double)
        dc->DrawLine(point.x + m_signWCenter - m_signWidth / 2,
                    point.y - m_signSize / 2,
                    point.x + m_signWCenter + m_signWidth / 2,
                    point.y - m_signSize / 2);
        dc->DrawLine(point.x + m_signWCenter - m_signWidth / 2,
                    point.y - m_signSize / 2 + 1,
                    point.x + m_signWCenter + m_signWidth / 2,
                    point.y - m_signSize / 2 + 1);
        // Ticks on horizontal line
        dc->DrawLine(point.x + m_signWCenter - m_signWidth / 2,
                    point.y - m_signSize / 2,
                    point.x + m_signWCenter - m_signWidth / 2,
                    point.y - m_signSize / 2 + Scale_Px(5));
        dc->DrawLine(point.x + m_signWCenter + m_signWidth / 2,
                    point.y - m_signSize / 2,
                    point.x + m_signWCenter + m_signWidth / 2,
                    point.y - m_signSize / 2 + Scale_Px(5));
      }
      UnsetPen();
    }
    base.x += (2 * m_signWCenter + Scale_Px(4));
    m_base->DrawList(base);
  }
}

wxString SumCell::ToString()
{
  wxString s;
  if (m_sumStyle == SM_SUM)
    s = wxT("sum(");
  else
    s = wxT("product(");
  s += m_base->ListToString();

  Cell *tmp = m_under;
  wxString var = tmp->ToString();
  wxString from;
  tmp = tmp->m_next;
  if (tmp != NULL)
  {
    tmp = tmp->m_next;
    if (tmp != NULL)
      from = tmp->ListToString();
  }
  wxString to = m_over->ListToString();
  s += wxT(",") + var + wxT(",") + from;
  if (to != wxEmptyString)
    s += wxT(",") + to + wxT(")");
  else
    s = wxT("l") + s + wxT(")");
  return s;
}

wxString SumCell::ToMatlab()
{
  wxString s;
  if (m_sumStyle == SM_SUM)
	s = wxT("sum(");
  else
	s = wxT("product(");
  s += m_base->ListToMatlab();

  Cell *tmp = m_under;
  wxString var = tmp->ToMatlab();
  wxString from;
  tmp = tmp->m_next;
  if (tmp != NULL)
  {
	tmp = tmp->m_next;
	if (tmp != NULL)
	  from = tmp->ListToMatlab();
  }
  wxString to = m_over->ListToMatlab();
  s += wxT(",") + var + wxT(",") + from;
  if (to != wxEmptyString)
	s += wxT(",") + to + wxT(")");
  else
	s = wxT("l") + s + wxT(")");
  return s;
}

wxString SumCell::ToTeX()
{
  wxString s;
  if (m_sumStyle == SM_SUM)
    s = wxT("\\sum");
  else
    s = wxT("\\prod");


  s += wxT("_{") + m_under->ListToTeX() + wxT("}");
  wxString to = m_over->ListToTeX();
  if (to.Length())
    s += wxT("^{") + to + wxT("}");


  s += wxT("{\\left. ");
  s += m_base->ListToTeX();
  s += wxT("\\right.}");
  return s;
}

wxString SumCell::ToOMML()
{
  wxString base = m_base->ListToOMML();

  wxString from;
  if (m_under) from = m_under->ListToOMML();

  wxString to;
  if (m_over) to = m_over->ListToOMML();

  wxString retval;

  retval = wxT("<m:nary><m:naryPr><m:chr>");
  if (m_sumStyle == SM_SUM)
    retval += wxT("\x2211");
  else
    retval += wxT("\x220F");

  retval += wxT("</m:chr></m:naryPr>");
  if (from != wxEmptyString)
    retval += wxT("<m:sub>") + from + wxT("</m:sub>");
  if (to != wxEmptyString)
    retval += wxT("<m:sup>") + to + wxT("</m:sup>");
  retval += wxT("<m:e>") + base + wxT("</m:e></m:nary>");

  return retval;
}


wxString SumCell::ToXML()
{
  wxString type(wxT("sum"));

  if (m_sumStyle == SM_PROD)
    type = wxT("prod");
  else if (m_over->ListToString() == wxEmptyString)
    type = wxT("lsum");


  wxString flags;
  if (m_forceBreakLine)
    flags += wxT(" breakline=\"true\"");
    
  return wxT("<sm type=\"") + flags + type + wxT("\"><r>") + m_under->ListToXML() + _T("</r><r>") +
         m_over->ListToXML() + _T("</r><r>") +
         m_base->ListToXML() + _T("</r></sm>");
}

wxString SumCell::ToMathML()
{
  wxString base = m_base->ListToMathML();

  wxString from;
  if (m_under) from = m_under->ListToMathML();

  wxString to;
  if (m_over) to = m_over->ListToMathML();

  wxString retval;

  if (m_sumStyle == SM_SUM)
  {
//    retval = wxT("<apply><sum/>");
//    if(!from.IsEmpty())
//      retval += wxT("<lowlimit>") + m_under->ListToMathML() + wxT("</lowlimit>");
//    if(!to.IsEmpty())
//      retval += wxT("<uplimit>") + m_over->ListToMathML() + wxT("</uplimit>");
//    retval += m_base->ListToMathML() + wxT("</apply>");
    if (from.IsEmpty() && to.IsEmpty())
      retval = wxT("<mo>&#x2211;</mo>") + base;
    if (from.IsEmpty() && !to.IsEmpty())
      retval = wxT("<mover><mo>&#x2211;</mo>") + to + wxT("</mover>") + base;
    if (!from.IsEmpty() && to.IsEmpty())
      retval = wxT("<munder><mo>&#x2211;</mo>") + from + wxT("</munder>") + base;
    if (!from.IsEmpty() && !to.IsEmpty())
      retval = wxT("<munderover><mo>&#x2211;</mo>") + from + to + wxT("</munderover>") + base;
  }
  else
  {
    // A product
    if (from.IsEmpty() && to.IsEmpty())
      retval = wxT("<mo>&#x220F;</mo>") + base;
    if (from.IsEmpty() && !to.IsEmpty())
      retval = wxT("<mover><mo>&#x220F;</mo>") + to + wxT("</mover>") + base;
    if (!from.IsEmpty() && to.IsEmpty())
      retval = wxT("<munder><mo>&#x220F;</mo>") + from + wxT("</munder>") + base;
    if (!from.IsEmpty() && !to.IsEmpty())
      retval = wxT("<munderover><mo>&#x220F;</mo>") + from + to + wxT("</munderover>") + base;
  }
  return (wxT("<mrow>") + retval + wxT("</mrow>"));
}
