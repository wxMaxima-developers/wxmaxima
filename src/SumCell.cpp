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

SumCell::SumCell(GroupCell *group, Configuration **config) :
    Cell(group, config),
    m_under(new TextCell(group, config)),
    m_start(new TextCell(group, config)),
    m_var(new TextCell(group, config)),
    m_end(new TextCell(group, config)),
    m_comma1(new TextCell(group, config, wxT(","))),
    m_comma2(new TextCell(group, config, wxT(","))),
    m_comma3(new TextCell(group, config, wxT(","))),
    m_open(new TextCell(group, config, wxT("lsum("))),
    m_close(new TextCell(group, config, wxT(")"))),
    m_paren(new ParenCell(group, config))
{
  InitBitFields();
  wxASSERT(Base()); // m_paren constructs its inner cell by default
}

// cppcheck-suppress uninitMemberVar symbolName=SumCell::m_signHeight
// cppcheck-suppress uninitMemberVar symbolName=SumCell::m_signWidth
// cppcheck-suppress uninitMemberVar symbolName=SumCell::m_signWCenter
SumCell::SumCell(const SumCell &cell) :
    SumCell(cell.m_group, cell.m_configuration)
{
  CopyCommonData(cell);
  m_altCopyText = cell.m_altCopyText;
  if (cell.Base())
    SetBase(cell.Base()->CopyList());
  if (cell.m_under)
  SetUnder(cell.m_under->CopyList());
  if (cell.m_end)
    SetOver(cell.m_end->CopyList());
  SetSumStyle(cell.m_sumStyle);
}

std::unique_ptr<Cell> SumCell::Copy() const
{
  return std::make_unique<SumCell>(*this);
}

void SumCell::SetOver(Cell *over)
{
  if (!over)
    return;
  m_end.reset(over);
  if(m_sumStyle == SM_SUM)
  {
    if(!m_end->ToString().IsEmpty())
      m_open.reset(new TextCell(m_group, m_configuration, wxT("sum(")));
  }
  else
    m_open.reset(new TextCell(m_group, m_configuration, wxT("prod(")));
  ResetSize();
}

void SumCell::SetBase(Cell *base)
{
  if (!base)
    return;
  m_baseWithoutParen = base;
  Paren()->SetInner(base);
  wxASSERT(Base() == base);
  m_displayedBase = m_paren;
}

void SumCell::SetUnder(Cell *under)
{
  if (!under)
    return;
  m_under.reset(under);
  
  // m_under consists of a list of cells:
  //  The variable name, that can be more than one cell if there is a subscript.
  //  1 cell containing the text "in" or "=" (TODO: That's heuristics. Is there a better, but
  //                                          backwards-compatible way for this?)
  //  And the rest contains the lower limit.
  m_var = m_under->Copy();
  Cell *start = m_under.get();
  while (start != NULL &&
         ((start->GetValue() != wxT("in")) && (start->GetValue() != wxT("="))))
    start = start->GetNext();
  if(start != NULL)  
    start = start->GetNext();
  if(start != NULL)  
    m_start.reset(start->CopyList());
  ResetSize();
}

void SumCell::RecalculateWidths(AFontSize fontsize)
{
  if (!NeedsRecalculation(fontsize))
    return;

  m_displayedBase->RecalculateWidthsList(fontsize);

  m_comma1->RecalculateWidthsList(fontsize);
  m_comma2->RecalculateWidthsList(fontsize);
  m_comma3->RecalculateWidthsList(fontsize);
  m_open->RecalculateWidthsList(fontsize);
  m_start->RecalculateWidthsList(fontsize);
  m_var->RecalculateWidthsList(fontsize);
  m_open->RecalculateWidthsList(fontsize);
  m_close->RecalculateWidthsList(fontsize);

  if (m_sumStyle == SM_SUM)
    m_signWidth = 3.0 * m_signHeight / 5.0;
  else
    m_signWidth = 4.0 * m_signHeight / 5.0;
  m_signWCenter = m_signWidth / 2.0;
  if(m_isBrokenIntoLines)
    m_under->RecalculateWidthsList(fontsize);
  else
    m_under->RecalculateWidthsList({ MC_MIN_SIZE, fontsize - SUM_DEC });
  if (!m_end)
    m_end.reset(new TextCell(m_group, m_configuration));
  if(m_isBrokenIntoLines)
    m_end->RecalculateWidthsList(fontsize);
  else
    m_end->RecalculateWidthsList({ MC_MIN_SIZE, fontsize - SUM_DEC });

  m_signWCenter = wxMax(m_signWCenter, m_under->GetFullWidth() / 2);
  m_signWCenter = wxMax(m_signWCenter, m_end->GetFullWidth() / 2);
  m_width = 2 * m_signWCenter + m_displayedBase->GetFullWidth() + Scale_Px(4);
  ResetCellListSizes();
  Cell::RecalculateWidths(fontsize);
}

void SumCell::RecalculateHeight(AFontSize fontsize)
{
  if(!NeedsRecalculation(fontsize))
    return;

  if(m_isBrokenIntoLines)
    m_under->RecalculateHeightList(fontsize);
  else
    m_under->RecalculateHeightList({ MC_MIN_SIZE, fontsize - SUM_DEC });
  if(m_isBrokenIntoLines)
    m_end->RecalculateHeightList(fontsize);
  else
    m_end->RecalculateHeightList({ MC_MIN_SIZE, fontsize - SUM_DEC });
  m_start->RecalculateHeightList(fontsize);
  m_var->RecalculateHeightList(fontsize);
  m_displayedBase->RecalculateHeightList(fontsize);

  m_center = wxMax(m_end->GetHeightList() + Scale_Px(2) + m_signHeight / 2,
                 m_displayedBase->GetCenterList());
  m_height = m_center +
             wxMax(m_under->GetHeightList() + Scale_Px(4) + m_signHeight / 2,
                 m_displayedBase->GetMaxDrop());
  m_signHeight = m_displayedBase->GetHeightList();
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

    over.x += m_signWCenter - m_end->GetFullWidth() / 2;
    over.y = point.y - m_signHeight / 2 - m_end->GetMaxDrop() - Scale_Px(2);
    m_end->DrawList(over);

    SetPen(1.5);
    if (m_sumStyle == SM_SUM)
    {
      wxDC *adc = configuration->GetAntialiassingDC();
      //DRAW SUM SIGN
      // Upper part
      const wxPoint points[5] = {
        {m_signWCenter + int(m_signWidth / 2),
         -(m_signHeight / 2)},
        {m_signWCenter - int(m_signWidth / 2),
         -(m_signHeight / 2)},
        {m_signWCenter + int(m_signWidth / 5),
         0},
        {m_signWCenter - int(m_signWidth / 2),
         (m_signHeight / 2)},
        {m_signWCenter + int(m_signWidth / 2),
         (m_signHeight / 2)}
      };
      adc->DrawLines(5, points, point.x, point.y);
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
    base.x += (2 * m_signWCenter + Scale_Px(4));
    m_displayedBase->DrawList(base);
  }
}

wxString SumCell::ToString() const
{
  if (m_altCopyText != wxEmptyString)
    return m_altCopyText;

  wxString s;
  if (m_sumStyle == SM_SUM)
    s = wxT("sum(");
  else
    s = wxT("product(");
  s += Base()->ListToString();

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
  wxString to = m_end->ListToString();
  s += wxT(",") + var + wxT(",") + from;
  if (to != wxEmptyString)
    s += wxT(",") + to + wxT(")");
  else
    s = wxT("l") + s + wxT(")");
  return s;
}

wxString SumCell::ToMatlab() const
{
  wxString s;
  if (m_sumStyle == SM_SUM)
	s = wxT("sum(");
  else
	s = wxT("product(");
  s += Base()->ListToMatlab();

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
  wxString to = m_end->ListToMatlab();
  s += wxT(",") + var + wxT(",") + from;
  if (to != wxEmptyString)
	s += wxT(",") + to + wxT(")");
  else
	s = wxT("l") + s + wxT(")");
  return s;
}

wxString SumCell::ToTeX() const
{
  wxString s;
  if (m_sumStyle == SM_SUM)
    s = wxT("\\sum");
  else
    s = wxT("\\prod");


  s += wxT("_{") + m_under->ListToTeX() + wxT("}");
  wxString to = m_end->ListToTeX();
  if (to.Length())
    s += wxT("^{") + to + wxT("}");


  s += wxT("{\\left. ");
  s += Base()->ListToTeX();
  s += wxT("\\right.}");
  return s;
}

wxString SumCell::ToOMML() const
{
  wxString base = Base()->ListToOMML();
  wxString from = m_under ? m_under->ListToOMML() : wxString{};
  wxString to = m_end ? m_end->ListToOMML() : wxString{};

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


wxString SumCell::ToXML() const
{
  wxString type(wxT("sum"));

  if (m_sumStyle == SM_PROD)
    type = wxT("prod");
  else if (m_end->ListToString() == wxEmptyString)
    type = wxT("lsum");


  wxString flags;
  if (m_forceBreakLine)
    flags += wxT(" breakline=\"true\"");
    
  return wxT("<sm type=\"") + flags + type + wxT("\"><r>") + m_under->ListToXML() + _T("</r><r>") +
         m_end->ListToXML() + _T("</r><r>") +
         Base()->ListToXML() + _T("</r></sm>");
}

wxString SumCell::ToMathML() const
{
  wxString base = Base()->ListToMathML();

  wxString from;
  if (m_under) from = m_under->ListToMathML();

  wxString to;
  if (m_end) to = m_end->ListToMathML();

  wxString retval;

  if (m_sumStyle == SM_SUM)
  {
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

void SumCell::SetSumStyle(sumStyle style)
{
  m_sumStyle = style;
  if(style == SM_PROD)
  {
    m_open.reset(new TextCell(m_group, m_configuration, wxT("prod(")));
  }
  else
  {
    if(!m_end->ToString().IsEmpty())
      m_open.reset(new TextCell(m_group, m_configuration, wxT("sum(")));
    else
      m_open.reset(new TextCell(m_group, m_configuration, wxT("lsum(")));
  }
}


void SumCell::Unbreak()
{
  m_displayedBase = m_paren;
  Cell::Unbreak();
}

bool SumCell::BreakUp()
{
  if (!m_isBrokenIntoLines)
  {
    Cell::BreakUp();
    m_displayedBase = m_baseWithoutParen;
    m_isBrokenIntoLines = true;

    m_close->SetNextToDraw(m_nextToDraw);
    m_nextToDraw = m_open;
    m_open->last()->SetNextToDraw(m_displayedBase);
    m_displayedBase->last()->SetNextToDraw(m_comma1);
    m_comma1->last()->SetNextToDraw(m_var);
    m_var->last()->SetNextToDraw(m_comma2);
    m_comma2->last()->SetNextToDraw(m_start);
    // The first cell of m_var should normally be a "d"
    if(m_end->ToString().IsEmpty())
      m_start->last()->SetNextToDraw(m_close);
    else
    {
      m_start->last()->SetNextToDraw(m_comma3);
      m_comma3->last()->SetNextToDraw(m_end);
      m_end->last()->SetNextToDraw(m_close);
    }
    ResetCellListSizes();
    m_height = 0;
    m_center = 0;
    return true;
  }
  return false;
}

void SumCell::SetNextToDraw(Cell *next)
{
  if (m_isBrokenIntoLines)
    m_close->SetNextToDraw(next);
  else
    m_nextToDraw = next;
}
