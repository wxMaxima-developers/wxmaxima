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
#include "CellImpl.h"
#include "TextCell.h"

SumCell::SumCell(GroupCell *group, Configuration **config, sumStyle style,
                 std::unique_ptr<Cell> &&under, std::unique_ptr<Cell> &&over,
                 std::unique_ptr<Cell> &&base)
    : Cell(group, config),
      m_paren(std::make_unique<ParenCell>(group, config, std::move(base))),
      m_var(under->Copy(group)),
      m_start(MakeStart(under.get())),
      m_over(std::move(over)),
      m_under(std::move(under)),
      m_sumStyle(style)
{
  InitBitFields();
  if (!m_over)
    m_over = std::make_unique<TextCell>(group, config);
  wxASSERT(Base());
}

// cppcheck-suppress uninitMemberVar symbolName=SumCell::m_signHeight
// cppcheck-suppress uninitMemberVar symbolName=SumCell::m_signWidth
// cppcheck-suppress uninitMemberVar symbolName=SumCell::m_signWCenter
SumCell::SumCell(GroupCell *group, const SumCell &cell)
    : SumCell(group, cell.m_configuration, cell.m_sumStyle,
              CopyList(group, cell.m_under.get()),
              CopyList(group, cell.m_over.get()),
              CopyList(group, cell.Base()))
{
  CopyCommonData(cell);
  m_altCopyText = cell.m_altCopyText;
}

DEFINE_CELL(SumCell)

void SumCell::MakeBreakUpCells()
{
  if (m_open) return;

  bool overStringEmpty = m_over->ToString().empty();
  const wxString &openText =
    (m_sumStyle == SM_SUM &&  overStringEmpty) ? S_("lsum(") :
    (m_sumStyle == SM_SUM && !overStringEmpty) ? S_("sum(") :
    /* (m_sumstyle == SM_PROD) */                S_("prod(");

  m_comma1 = std::make_unique<TextCell>(m_group, m_configuration, wxT(","));
  m_comma2 = std::make_unique<TextCell>(m_group, m_configuration, wxT(","));
  m_comma3 = std::make_unique<TextCell>(m_group, m_configuration, wxT(","));
  m_open = std::make_unique<TextCell>(m_group, m_configuration, openText);
  m_close = std::make_unique<TextCell>(m_group, m_configuration, wxT(")"));
}

ParenCell *SumCell::Paren() const
{
  return static_cast<ParenCell *>(m_paren.get());
}

Cell *SumCell::Base() const { return Paren() ? Paren()->GetInner() : nullptr; }

Cell *SumCell::DisplayedBase() const
{
  return m_displayParen ? m_paren.get() : Paren()->GetInner();
}

std::unique_ptr<Cell> SumCell::MakeStart(Cell *under) const
{
  std::unique_ptr<Cell> newStart;
  // m_under consists of a list of cells:
  //  The variable name, that can be more than one cell if there is a subscript.
  //  1 cell containing the text "in" or "=" (TODO: That's heuristics. Is there a better, but
  //                                          backwards-compatible way for this?)
  //  And the rest contains the lower limit.

  bool prevFound = false;
  for (auto &start : OnList(under))
  {
    auto const &value = start.GetValue();
    if (prevFound)
    {
      newStart = start.CopyList(GetGroup());
      break;
    }
    prevFound = (value == wxT("in")) || (value == wxT("="));
  }

  return newStart ? std::move(newStart) : std::make_unique<TextCell>(m_group, m_configuration);
}

void SumCell::Recalculate(AFontSize fontsize)
{
  DisplayedBase()->RecalculateList(fontsize);
  m_start->RecalculateList(fontsize);
  m_var->RecalculateList(fontsize);

  if (IsBrokenIntoLines())
  {
    m_over->RecalculateList(fontsize);
    m_comma1->RecalculateList(fontsize);
    m_comma2->RecalculateList(fontsize);
    m_comma3->RecalculateList(fontsize);
    m_open->RecalculateList(fontsize);
    m_close->RecalculateList(fontsize);
  }
  else
    m_over->RecalculateList({ MC_MIN_SIZE, fontsize - SUM_DEC });

  if (m_sumStyle == SM_SUM)
    m_signWidth = 3.0 * m_signHeight / 5.0;
  else
    m_signWidth = 4.0 * m_signHeight / 5.0;
  m_signWCenter = m_signWidth / 2.0;
  if(IsBrokenIntoLines())
    m_under->RecalculateList(fontsize);
  else
    m_under->RecalculateList({ MC_MIN_SIZE, fontsize - SUM_DEC });

  m_signWCenter = wxMax(m_signWCenter, m_under->GetFullWidth() / 2);
  m_signWCenter = wxMax(m_signWCenter, m_over->GetFullWidth() / 2);
  m_width = 2 * m_signWCenter + DisplayedBase()->GetFullWidth() + Scale_Px(4);

  m_center = wxMax(m_over->GetHeightList() + Scale_Px(2) + m_signHeight / 2,
                 DisplayedBase()->GetCenterList());
  m_height = m_center +
             wxMax(m_under->GetHeightList() + Scale_Px(4) + m_signHeight / 2,
                 DisplayedBase()->GetMaxDrop());
  m_signHeight = DisplayedBase()->GetHeightList();
  Cell::Recalculate(fontsize);
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
    DisplayedBase()->DrawList(base);
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
  
  if (m_over->ListToString() == wxEmptyString)
  {
    if(m_sumStyle == SM_PROD)
      s = wxT("lprod(");
    else
      s = wxT("lsum(");
  }
  
  s += Base()->ListToString();

  Cell *tmp = m_under.get();
  wxString var = tmp->ToString();
  wxString from;
  tmp = tmp->GetNext();
  if (tmp != NULL)
  {
    tmp = tmp->GetNext();
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
  tmp = tmp->GetNext();
  if (tmp != NULL)
  {
	tmp = tmp->GetNext();
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

wxString SumCell::ToTeX() const
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
  s += Base()->ListToTeX();
  s += wxT("\\right.}");
  return s;
}

wxString SumCell::ToOMML() const
{
  wxString base = Base()->ListToOMML();
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


wxString SumCell::ToXML() const
{
  wxString type(wxT("sum"));

  if (m_sumStyle == SM_PROD)
    type = wxT("prod");
  if (m_over->ListToString() == wxEmptyString)
  {
    if(m_sumStyle == SM_PROD)
      type = wxT("lprod");
    else
      type = wxT("lsum");
  }

  wxString flags;
  if (HasHardLineBreak())
    flags += wxT(" breakline=\"true\"");
    
  return wxT("<sm type=\"") + flags + type + wxT("\"><r>") + m_under->ListToXML() + _T("</r><r>") +
         m_over->ListToXML() + _T("</r><r>") +
         Base()->ListToXML() + _T("</r></sm>");
}

wxString SumCell::ToMathML() const
{
  wxString base = Base()->ListToMathML();

  wxString from;
  if (m_under) from = m_under->ListToMathML();

  wxString to;
  if (m_over) to = m_over->ListToMathML();

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

void SumCell::Unbreak()
{
  m_displayParen = true;
  Cell::Unbreak();
}

bool SumCell::BreakUp()
{
  if (IsBrokenIntoLines())
    return false;

  MakeBreakUpCells();
  Cell::BreakUpAndMark();
  m_displayParen = false;

  m_close->SetNextToDraw(m_nextToDraw);
  m_nextToDraw = m_open;
  m_open->last()->SetNextToDraw(DisplayedBase());
  DisplayedBase()->last()->SetNextToDraw(m_comma1);
  m_comma1->last()->SetNextToDraw(m_var);
  m_var->last()->SetNextToDraw(m_comma2);
  m_comma2->last()->SetNextToDraw(m_start);
  // The first cell of m_var should normally be a "d"
  if(m_over->ToString().IsEmpty())
    m_start->last()->SetNextToDraw(m_close);
  else
  {
    m_start->last()->SetNextToDraw(m_comma3);
    m_comma3->last()->SetNextToDraw(m_over);
    m_over->last()->SetNextToDraw(m_close);
  }
  ResetCellListSizes();
  m_height = 0;
  m_center = 0;
  return true;
}

void SumCell::SetNextToDraw(Cell *next)
{
  if (IsBrokenIntoLines())
    m_close->SetNextToDraw(next);
  else
    m_nextToDraw = next;
}
