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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
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
#include "FontCache.h"

SumCell::SumCell(Cell *parent, Configuration **config, CellPointers *cellPointers) :
    Cell(parent, config, cellPointers),
    m_base(new TextCell(parent, config, cellPointers)),
    m_under(new TextCell(parent, config, cellPointers)),
    m_over(new TextCell(parent, config, cellPointers)),
    m_paren(new ParenCell(parent, config, cellPointers))
{
  m_nextToDraw = NULL;
  m_signHeight = 50;
  m_signTop = (2 * m_signHeight) / 5;
  m_signWidth = 30;
  m_signWCenter = 15;
  m_sumStyle = SM_SUM;
}

// cppcheck-suppress uninitMemberVar symbolName=SumCell::m_signHeight
// cppcheck-suppress uninitMemberVar symbolName=SumCell::m_signWidth
// cppcheck-suppress uninitMemberVar symbolName=SumCell::m_signWCenter
SumCell::SumCell(const SumCell &cell) :
    SumCell(cell.m_group, cell.m_configuration, cell.m_cellPointers)
{
  m_nextToDraw = NULL;
  CopyCommonData(cell);
  if (cell.m_base)
    SetBase(cell.m_base->CopyList());
  if (cell.m_under)
  SetUnder(cell.m_under->CopyList());
  if (cell.m_over)
    SetOver(cell.m_over->CopyList());
  m_sumStyle = cell.m_sumStyle;
}

SumCell::~SumCell()
{
  MarkAsDeleted();
}

void SumCell::SetOver(Cell *over)
{
  if (!over)
    return;
  m_over.reset(over);
}

void SumCell::SetBase(Cell *base)
{
  if (!base)
    return;
  m_base.reset(base);
  static_cast<ParenCell&>(*m_paren).SetInner(m_base);
  m_displayedBase = m_paren;
}

void SumCell::SetUnder(Cell *under)
{
  if (!under)
    return;
  m_under.reset(under);
}

void SumCell::RecalculateWidths(int fontsize)
{
  if (!NeedsRecalculation(fontsize))
    return;

  m_displayedBase->RecalculateWidthsList(fontsize);
  m_signHeight = m_displayedBase->GetHeightList();
  if (m_sumStyle == SM_SUM)
    m_signWidth = 3.0 * m_signHeight / 5.0;
  else
    m_signWidth = 4.0 * m_signHeight / 5.0;
  m_signWCenter = m_signWidth / 2.0;
  m_under->RecalculateWidthsList(wxMax(MC_MIN_SIZE, fontsize - SUM_DEC));
  if (!m_over)
    m_over.reset(new TextCell(m_group, m_configuration, m_cellPointers));
  m_over->RecalculateWidthsList(wxMax(MC_MIN_SIZE, fontsize - SUM_DEC));

  if (false)
  {
    Configuration *configuration = *m_configuration;
    if (configuration->CheckTeXFonts())
    {
      wxDC *dc = configuration->GetDC();
      double fontsize1 = Scale_Px(configuration->GetMathFontSize());

      wxFont font =
        FontCache::GetAFont(wxFontInfo(fontsize1)
                              .Family(wxFONTFAMILY_MODERN)
                              .Italic(false)
                              .Bold(false)
                              .Underlined(false)
                              .FaceName(configuration->GetTeXCMEX()));

      if (!font.IsOk())
        configuration->CheckTeXFonts(false);

      dc->SetFont(font);
#if 0
      dc->GetTextExtent(m_sumStyle == SM_SUM ? wxT(SUM_SIGN) : wxT(PROD_SIGN), &m_signWidth, &m_signHeight);
      m_signWCenter = m_signWidth / 2;
      m_signTop = (2 * m_signHeight) / 5;
      m_signHeight = (2 * m_signHeight) / 5;
#endif
    }
  } // if (false)

  m_signWCenter = wxMax(m_signWCenter, m_under->GetFullWidth() / 2);
  m_signWCenter = wxMax(m_signWCenter, m_over->GetFullWidth() / 2);
  m_width = 2 * m_signWCenter + m_displayedBase->GetFullWidth() + Scale_Px(4);

  ResetData();
  Cell::RecalculateWidths(fontsize);
}

void SumCell::RecalculateHeight(int fontsize)
{
  if(!NeedsRecalculation(fontsize))
    return;

  m_under->RecalculateHeightList(wxMax(MC_MIN_SIZE, fontsize - SUM_DEC));
  m_over->RecalculateHeightList(wxMax(MC_MIN_SIZE, fontsize - SUM_DEC));
  m_displayedBase->RecalculateHeightList(fontsize);

  m_center = wxMax(m_over->GetHeightList() + Scale_Px(4) + m_signHeight / 2,
                 m_displayedBase->GetCenterList());
  m_height = m_center +
             wxMax(m_under->GetHeightList() + Scale_Px(4) + m_signHeight / 2,
                 m_displayedBase->GetMaxDrop());
  Cell::RecalculateHeight(fontsize);
}

void SumCell::Draw(wxPoint point)
{
  Cell::Draw(point);

  if (DrawThisCell(point))
  {
    Configuration *configuration = (*m_configuration);
    wxDC *dc = configuration->GetDC();

    wxPoint base(point), under(point), over(point);

    under.x += m_signWCenter - m_under->GetFullWidth() / 2;
    under.y = point.y + m_signHeight / 2 + m_under->GetCenterList() + Scale_Px(2);
    m_under->DrawList(under);

    over.x += m_signWCenter - m_over->GetFullWidth() / 2;
    over.y = point.y - m_signHeight / 2 - m_over->GetMaxDrop() - Scale_Px(2);
    m_over->DrawList(over);

    if (false /*this code is disabled*/ && configuration->CheckTeXFonts())
    {
      /*this code is disabled*/
      SetForeground();
      double fontsize1 = Scale_Px(configuration->GetMathFontSize());
      wxASSERT(fontsize1 > 0);

      auto req = wxFontInfo(fontsize1)
                   .Family(wxFONTFAMILY_MODERN)
                   .Italic(false)
                   .Bold(false)
                   .Underlined(false)
                   .FaceName(configuration->GetTeXCMEX());

      wxFont font = FontCache::GetAFont(req);

      if (!font.IsOk()) {
        FontInfo::CopyWithoutSize(wxNORMAL_FONT, req);
        font = FontCache::GetAFont(req);
      }

      dc->SetFont(font);
#if 0
      dc->DrawText(m_sumStyle == SM_SUM ? wxT(SUM_SIGN) : wxT(PROD_SIGN),
                   sign.x + m_signWCenter - m_signWidth / 2,
                   sign.y - m_signTop);
#endif
    }
    else
    {
      SetPen(1.5);
      if (m_sumStyle == SM_SUM)
      {
        wxDC *adc = configuration->GetAntialiassingDC();
        //DRAW SUM SIGN
        // Upper part
        wxPointList points;
        points.Append(new wxPoint(
                        point.x + m_signWCenter + m_signWidth / 2,
                        point.y - (m_signHeight / 2)
                        ));
        points.Append(new wxPoint(
                        point.x + m_signWCenter - m_signWidth / 2,
                        point.y - (m_signHeight / 2)
                        ));
        points.Append(new wxPoint(
                        point.x + m_signWCenter + m_signWidth / 5,
                        point.y
                        ));
        points.Append(new wxPoint(
                        point.x + m_signWCenter - m_signWidth / 2,
                        point.y + (m_signHeight / 2)
                        ));
        points.Append(new wxPoint(
                        point.x + m_signWCenter + m_signWidth / 2,
                        point.y + (m_signHeight / 2)
                        ));
        adc->DrawLines(&points);
      }
      else
      {
        // DRAW PRODUCT SIGN
        // Vertical lines
        dc->DrawLine(point.x + m_signWCenter + m_signWidth / 6,
                    point.y + m_signHeight / 2,
                    point.x + m_signWCenter + m_signWidth / 6,
                    point.y - m_signHeight / 2);
        dc->DrawLine(point.x + m_signWCenter - m_signWidth / 6,
                    point.y + m_signHeight / 2,
                    point.x + m_signWCenter - m_signWidth / 6,
                    point.y - m_signHeight / 2);
        // Horizontal line
        dc->DrawLine(point.x + m_signWCenter - m_signWidth / 2,
                    point.y - m_signHeight / 2,
                    point.x + m_signWCenter + m_signWidth / 2,
                    point.y - m_signHeight / 2);
      }
      UnsetPen();
    }
    base.x += (2 * m_signWCenter + Scale_Px(4));
    m_displayedBase->DrawList(base);
  }
}

wxString SumCell::ToString()
{
  if (m_altCopyText != wxEmptyString)
    return m_altCopyText;

  wxString s;
  if (m_sumStyle == SM_SUM)
    s = wxT("sum(");
  else
    s = wxT("product(");
  s += m_base->ListToString();

  Cell *tmp = m_under.get();
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

  Cell *tmp = m_under.get();
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
  wxString from = m_under ? m_under->ListToOMML() : wxString{};
  wxString to = m_over ? m_over->ListToOMML() : wxString{};

  wxString retval;

  retval = wxT("<m:nary><m:naryPr><m:chr>");
  if (m_sumStyle == SM_SUM)
    retval += wxT("\u2211");
  else
    retval += wxT("\u220F");

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
