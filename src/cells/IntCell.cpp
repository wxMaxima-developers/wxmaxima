// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
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
  This file defines the class IntCell

  IntCell is the Cell type that represents maxima's <code>integrate()</code>
  command.
*/

#include "IntCell.h"
#include "CellImpl.h"
#include "TextCell.h"
#include <memory>
#include <utility>

#if defined __WXMSW__
#define INTEGRAL_TOP "\xF3"
#define INTEGRAL_BOTTOM "\xF5"
#define INTEGRAL_EXTEND "\xF4"
static constexpr AFontSize INTEGRAL_FONT_SIZE{12.0f};
#endif

IntCell::IntCell(GroupCell *group, Configuration *config,
                 std::unique_ptr<Cell> &&base, std::unique_ptr<Cell> &&under,
                 std::unique_ptr<Cell> &&over, std::unique_ptr<Cell> &&var)
  : Cell(group, config), m_base(std::move(base)), m_var(std::move(var)),
    m_under(std::move(under)), m_over(std::move(over)) {
  InitBitFields();
  SetStyle(TS_VARIABLE);
}

IntCell::IntCell(GroupCell *group, Configuration *config,
                 std::unique_ptr<Cell> &&base, std::unique_ptr<Cell> &&var)
  : IntCell(group, config, std::move(base),
	    std::make_unique<TextCell>(group, config),
	    std::make_unique<TextCell>(group, config), std::move(var)) {}

// Old cppcheck bugs:
// cppcheck-suppress uninitMemberVar symbolName=IntCell::m_signHeight
// cppcheck-suppress uninitMemberVar symbolName=IntCell::m_signWidth
// cppcheck-suppress uninitMemberVar symbolName=IntCell::m_signTop
// cppcheck-suppress uninitMemberVar symbolName=IntCell::m_charHeight
// cppcheck-suppress uninitMemberVar symbolName=IntCell::m_charWidth
IntCell::IntCell(GroupCell *group, const IntCell &cell)
  : IntCell(group, cell.m_configuration, CopyList(group, cell.m_base.get()),
	    CopyList(group, cell.m_under.get()),
	    CopyList(group, cell.m_over.get()),
	    CopyList(group, cell.m_var.get())) {
  CopyCommonData(cell);
  m_intStyle = cell.m_intStyle;
}

DEFINE_CELL(IntCell)

void IntCell::MakeBreakUpCells() {
  if (m_open)
    return;
  m_open = std::make_unique<TextCell>(m_group, m_configuration, "integrate(");
  m_close = std::make_unique<TextCell>(m_group, m_configuration, ")");
  m_comma1 = std::make_unique<TextCell>(m_group, m_configuration, ",");
  m_comma2 = std::make_unique<TextCell>(m_group, m_configuration, ",");
  m_comma3 = std::make_unique<TextCell>(m_group, m_configuration, ",");
}

void IntCell::Recalculate(AFontSize fontsize) {
  wxASSERT(fontsize.IsValid());

  m_signHeight = Scale_Px(35);
  m_signWidth = Scale_Px(18);
  if (m_signWidth < 4)
    m_signWidth = 4;

  m_base->RecalculateList(fontsize);
  m_var->RecalculateList(fontsize);
  if (IsBrokenIntoLines()) {
    m_open->RecalculateList(fontsize);
    m_comma1->RecalculateList(fontsize);
    m_comma2->RecalculateList(fontsize);
    m_under->RecalculateList(fontsize);
    m_comma3->RecalculateList(fontsize);
    m_over->RecalculateList(fontsize);
    m_close->RecalculateList(fontsize);
  } else {
    m_under->RecalculateList({MC_MIN_SIZE, fontsize - 5});
    m_over->RecalculateList({MC_MIN_SIZE, fontsize - 5});
    if (m_intStyle == INT_DEF) {
      m_signHeight = Scale_Px(35) + m_over->GetHeightList() + m_under->GetHeightList();
    }
  }

  if (IsBrokenIntoLines()) {
    m_center = 0;
    m_height = 0;
    m_width = 0;
  } else {
    m_width = m_signWidth + m_base->GetFullWidth() +
      wxMax(m_over->GetFullWidth(), m_under->GetFullWidth()) +
      m_var->GetFullWidth() + Scale_Px(4);
    // cppcheck-suppress duplicateBranch
    if (m_intStyle == INT_DEF) {
      m_center = wxMax(m_signHeight / 2,
                       m_base->GetCenterList());
      m_height = m_center + wxMax(m_signHeight / 2,
                                  m_base->GetMaxDrop());
    } else {
      m_center = wxMax(m_signHeight / 2, m_base->GetCenterList());
      m_height = m_center + wxMax(m_signHeight / 2, m_base->GetMaxDrop());
    }
  }
}

void IntCell::Draw(wxPoint point, wxDC *dc, wxDC *antialiassingDC) {
  Cell::Draw(point, dc, antialiassingDC);
  if (DrawThisCell(point)) {
    wxPoint base(point), under(point), over(point), var(point), sign(point);

    SetPen(antialiassingDC, 1.5);
    // top decoration
    int m_signWCenter = m_signWidth / 2;
    wxPoint points[7] = {
      {sign.x + m_signWCenter + 2 * (m_signWidth / 4),
       sign.y - (m_signHeight - Scale_Px(1)) / 2 + m_signWidth / 4},
      {sign.x + m_signWCenter + m_signWidth / 4,
       sign.y - (m_signHeight - Scale_Px(1)) / 2},
      {sign.x + m_signWCenter, sign.y - (m_signHeight - Scale_Px(1)) / 2 +
       2 * (m_signWidth / 4) + Scale_Px(.35)},

      // The line
      {sign.x + m_signWCenter + Scale_Px(.5), sign.y},

      // Bottom Decoration
      {sign.x + m_signWCenter, sign.y + (m_signHeight - Scale_Px(1)) / 2 -
       2 * (m_signWidth / 4) + Scale_Px(.35)},
      {sign.x + m_signWCenter - m_signWidth / 4,
       sign.y + (m_signHeight - Scale_Px(1)) / 2},
      {sign.x + m_signWCenter - 2 * (m_signWidth / 4),
       sign.y + (m_signHeight - Scale_Px(1)) / 2 - m_signWidth / 4}};

    antialiassingDC->DrawSpline(7, points);
    // line

    if (m_intStyle == INT_DEF) {
      under.x += m_signWidth;
      under.y = point.y + m_signHeight / 2 - m_under->GetMaxDrop();
      m_under->DrawList(under, dc, antialiassingDC);

      over.x += m_signWidth;
      
      over.y = point.y - m_signHeight / 2 + m_over->GetHeightList() - m_over->GetCenterList();
      m_over->DrawList(over, dc, antialiassingDC);

      base.x += m_signWidth +
	wxMax(m_over->GetFullWidth(), m_under->GetFullWidth());
    }
    else
      base.x += m_signWidth;

    m_base->DrawList(base, dc, antialiassingDC);

    var.x = base.x + m_base->GetFullWidth();
    m_var->DrawList(var, dc, antialiassingDC);
  }
}

wxString IntCell::ToString() const {
  wxString s = wxS("integrate(");

  s += m_base->ListToString();

  Cell *tmp = m_var.get();
  wxString var;
  tmp = tmp->GetNext();
  if (tmp != NULL) {
    var = tmp->ListToString();
  }

  wxString to = m_over->ListToString();
  wxString from = m_under->ListToString();

  s += wxS(",") + var;
  if (m_intStyle == INT_DEF)
    s += wxS(",") + from + wxS(",") + to;

  s += wxS(")");
  return s;
}

wxString IntCell::ToMatlab() const {
  wxString s = wxS("integrate(");

  s += m_base->ListToMatlab();

  Cell *tmp = m_var.get();
  wxString var;
  tmp = tmp->GetNext();
  if (tmp != NULL)
    var = tmp->ListToMatlab();

  wxString to = m_over->ListToMatlab();
  wxString from = m_under->ListToMatlab();

  s += wxS(",") + var;
  if (m_intStyle == INT_DEF)
    s += wxS(",") + from + wxS(",") + to;

  s += wxS(")");
  return s;
}

wxString IntCell::ToTeX() const {
  wxString s = wxS("\\int");

  wxString to = m_over->ListToTeX();
  wxString from = m_under->ListToTeX();

  if (m_intStyle == INT_DEF)
    s += wxS("_{") + from + wxS("}^{") + to + wxS("}");
  else
    s += wxS(" ");

  s += wxS("{\\left. ");
  s += m_base->ListToTeX();
  s += m_var->ListToTeX();
  s += wxS("\\right.}");

  return s;
}

wxString IntCell::ToMathML() const {
  wxString base = m_base->ListToMathML();

  wxString var;
  if (m_var)
    var = m_var->ListToMathML();

  wxString from;
  if (m_under)
    from = m_under->ListToMathML();

  wxString to;
  if (m_over)
    to = m_over->ListToMathML();

  wxString retval;
  if (from.IsEmpty() && to.IsEmpty())
    retval = wxS("<mo>&#x222B;</mo>") + base;
  if (from.IsEmpty() && !to.IsEmpty())
    retval = wxS("<mover><mo>&#x222B;</mo>") + to + wxS("</mover>") + base;
  if (!from.IsEmpty() && to.IsEmpty())
    retval = wxS("<munder><mo>&#x222B;</mo>") + from + wxS("</munder>") + base;
  if (!from.IsEmpty() && !to.IsEmpty())
    retval = wxS("<munderover><mo>&#x222B;</mo>") + from + to +
      wxS("</munderover>\n") + base;
  if (!var.IsEmpty())
    retval = retval + var;

  return (wxS("<mrow>") + retval + wxS("</mrow>"));
}

wxString IntCell::ToOMML() const {
  wxString base = m_base->ListToOMML();

  wxString var;
  if (m_var)
    var = m_var->ListToOMML();

  wxString from;
  if (m_under)
    from = m_under->ListToOMML();

  wxString to;
  if (m_over)
    to = m_over->ListToOMML();

  wxString retval;

  retval = wxS("<m:nary><m:naryPr><m:chr>\u222b</m:chr></m:naryPr>");
  if (from != wxEmptyString)
    retval += wxS("<m:sub>") + from + wxS("</m:sub>");
  if (to != wxEmptyString)
    retval += wxS("<m:sup>") + to + wxS("</m:sup>");
  retval += wxS("<m:e><m:r>") + base + var + wxS("</m:r></m:e></m:nary>");

  return retval;
}

wxString IntCell::ToXML() const {
  wxString from;
  if (m_under != NULL)
    from = m_under->ListToXML();
  from = wxS("<r>") + from + wxS("</r>");

  wxString to;
  if (m_over != NULL)
    to = m_over->ListToXML();
  to = wxS("<r>") + to + wxS("</r>");

  wxString base;
  if (m_base != NULL)
    base = m_base->ListToXML();
  base = wxS("<r>") + base + wxS("</r>");

  wxString var;
  if (m_var != NULL)
    var = m_var->ListToXML();
  var = wxS("<r>") + var + wxS("</r>");

  wxString flags;
  if (HasHardLineBreak())
    flags += wxS(" breakline=\"true\"");

  if (m_intStyle != INT_DEF) {
    flags += wxS(" def=\"false\">");
    return wxS("<in") + flags + wxS(">") + base + var + wxS("</in>");
  } else
    return wxS("<in") + flags + wxS(">") + from + to + base + var +
      wxS("</in>");
}

bool IntCell::BreakUp() {
  if (IsBrokenIntoLines())
    return false;

  MakeBreakUpCells();
  Cell::BreakUpAndMark();
  m_close->SetNextToDraw(m_nextToDraw);
  m_nextToDraw = m_open;
  m_open->last()->SetNextToDraw(m_base);
  m_base->last()->SetNextToDraw(m_comma1);
  // The first cell of m_var should normally be a "d"
  if (m_var->GetNext() != NULL)
    m_comma1->last()->SetNextToDraw(m_var->GetNext());
  else
    m_comma1->last()->SetNextToDraw(m_var);
  if (m_intStyle != INT_DEF)
    m_var->last()->SetNextToDraw(m_close);
  else {
    m_var->last()->SetNextToDraw(m_comma2);
    m_comma2->last()->SetNextToDraw(m_under);
    m_under->last()->SetNextToDraw(m_comma3);
    m_comma3->last()->SetNextToDraw(m_over);
    m_over->last()->SetNextToDraw(m_close);
  }
  ResetCellListSizes();
  m_height = 0;
  m_center = 0;
  return true;
}

void IntCell::SetNextToDraw(Cell *next) {
  if (IsBrokenIntoLines())
    m_close->SetNextToDraw(next);
  else
    m_nextToDraw = next;
}
