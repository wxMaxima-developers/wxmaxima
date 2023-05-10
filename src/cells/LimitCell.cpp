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
  This file defines the class LimitCell

  LimitCell is the Cell type that represents maxima's <code>limit()</code>
  command.
*/

#include "LimitCell.h"
#include "CellImpl.h"

static constexpr AFontSize MIN_LIMIT_FONT_SIZE{8.0f};
static constexpr float LIMIT_FONT_SIZE_DECREASE{1.0f};

LimitCell::LimitCell(GroupCell *group, Configuration *config,
                     std::unique_ptr<Cell> &&base,
                     std::unique_ptr<Cell> &&under,
                     std::unique_ptr<Cell> &&name)
  : Cell(group, config), m_name(std::move(name)), m_base(std::move(base)),
    m_under(std::move(under)) {
  InitBitFields();
  SetStyle(TS_VARIABLE);
}

// cppcheck-suppress uninitMemberVar symbolName=LimitCell::m_open
// cppcheck-suppress uninitMemberVar symbolName=LimitCell::m_comma
// cppcheck-suppress uninitMemberVar symbolName=LimitCell::m_close
LimitCell::LimitCell(GroupCell *group, const LimitCell &cell)
  : LimitCell(group, cell.m_configuration, CopyList(group, cell.m_base.get()),
	      CopyList(group, cell.m_under.get()),
	      CopyList(group, cell.m_name.get())) {
  CopyCommonData(cell);
}

DEFINE_CELL(LimitCell)

void LimitCell::MakeBreakUpCells() {
  if (m_open)
    return;
  m_open = std::make_unique<TextCell>(m_group, m_configuration, wxS("("));
  m_open->SetStyle(TS_FUNCTION);
  m_comma = std::make_unique<TextCell>(m_group, m_configuration, wxS(","));
  m_comma->SetStyle(TS_FUNCTION);
  m_close = std::make_unique<TextCell>(m_group, m_configuration, wxS(")"));
  m_close->SetStyle(TS_FUNCTION);
}

void LimitCell::Recalculate(AFontSize fontsize) {
  m_base->RecalculateList(fontsize);
  m_under->RecalculateList(
			   {MIN_LIMIT_FONT_SIZE, fontsize - LIMIT_FONT_SIZE_DECREASE});
  m_name->RecalculateList(fontsize);
  if (!IsBrokenIntoLines()) {
    m_width = wxMax(m_name->GetFullWidth(), m_under->GetFullWidth()) +
      m_base->GetFullWidth();
    m_center = wxMax(m_base->GetCenterList(), m_name->GetCenterList());
    m_height = m_center + wxMax(m_name->GetMaxDrop() + m_under->GetHeightList(),
                                m_base->GetMaxDrop());
  } else {
    m_width = m_height = m_center = 0;
    m_open->RecalculateList(fontsize);
    m_comma->RecalculateList(fontsize);
    m_close->RecalculateList(fontsize);
  }

  Cell::Recalculate(fontsize);
}

void LimitCell::Draw(wxPoint point) {
  Cell::Draw(point);
  if (DrawThisCell(point)) {
    wxPoint base(point), under(point), name(point);

    name.x = point.x +
      wxMax(m_name->GetFullWidth(), m_under->GetFullWidth()) / 2 -
      m_name->GetFullWidth() / 2;
    m_name->DrawList(name);

    under.x = point.x +
      wxMax(m_name->GetFullWidth(), m_under->GetFullWidth()) / 2 -
      m_under->GetFullWidth() / 2;

    under.y = point.y + m_name->GetMaxDrop() + m_under->GetCenterList();
    m_under->DrawList(under);

    base.x += wxMax(m_name->GetFullWidth(), m_under->GetFullWidth());
    m_base->DrawList(base);
  }
}

wxString LimitCell::ToString() const {
  wxString s(wxS("limit"));
  wxString under = m_under->ListToString();
  wxString base = m_base->ListToString();
  wxString var = under.SubString(0, under.Find(wxS("->")) - 1);
  wxString to = under.SubString(under.Find(wxS("->")) + 2, under.Length() - 1);
  if (to.Right(1) == wxS("+"))
    to = to.Left(to.Length() - 1) + wxS(",plus");
  if (to.Right(1) == wxS("-"))
    to = to.Left(to.Length() - 1) + wxS(",minus");

  s += wxS("(") + base + wxS(",") + var + wxS(",") + to + wxS(")");
  return s;
}

wxString LimitCell::ToMatlab() const {
  wxString s(wxS("limit"));
  wxString under = m_under->ListToMatlab();
  wxString base = m_base->ListToMatlab();
  wxString var = under.SubString(0, under.Find(wxS("->")) - 1);
  wxString to = under.SubString(under.Find(wxS("->")) + 2, under.Length() - 1);
  if (to.Right(1) == wxS("+"))
    to = to.Left(to.Length() - 1) + wxS(",plus");
  if (to.Right(1) == wxS("-"))
    to = to.Left(to.Length() - 1) + wxS(",minus");

  s += wxS("(") + base + wxS(",") + var + wxS(",") + to + wxS(")");
  return s;
}

wxString LimitCell::ToTeX() const {
  wxString under = m_under->ListToTeX();
  wxString base = m_base->ListToTeX();
  int varEnd = under.Find(wxS("->"));
  int toStart = 0;
  if (varEnd == wxNOT_FOUND) {
    varEnd = under.Find(wxS("\\mbox{\\rightarrow }"));
    if (varEnd != wxNOT_FOUND) {
      toStart = varEnd + 19;
      varEnd -= 1;
    }
  } else {
    toStart = varEnd + 2;
    varEnd -= 1;
  }

  wxString var = under.SubString(0, varEnd);
  wxString to = under.SubString(toStart, under.Length() - 1);
  wxString s =
    wxS("\\lim_{") + var + wxS("\\to ") + to + wxS("}{") + base + wxS("}");
  return s;
}

wxString LimitCell::ToMathML() const {
  wxString base = m_base->ListToMathML();

  wxString from;
  if (m_under)
    from = m_under->ListToMathML();

  wxString retval;
  if (from.IsEmpty())
    retval = wxS("<mo>lim</mo>") + base;
  else
    retval = wxS("<munder><mo>lim</mo>") + from + wxS("</munder>\n");
  return (retval);
}

wxString LimitCell::ToXML() const {
  wxString flags;
  if (HasHardLineBreak())
    flags += wxS(" breakline=\"true\"");

  return _T("<lm") + flags + wxS("><r>") + m_name->ListToXML() + _T("</r><r>") +
    m_under->ListToXML() + _T("</r><r>") + m_base->ListToXML() +
    _T("</r></lm>");
}

wxString LimitCell::ToOMML() const {
  wxString under = m_under->ListToOMML();
  under.Replace(wxS("->"), wxS("\u2192"));

  return wxS("<m:func><m:fName><m:limLow><m:e><m:r>lim</m:r></m:e><m:lim>") +
    under + wxS("</m:lim></m:limLow></m:fName><m:e>") +
    m_base->ListToOMML() + wxS("</m:e></m:func>");
}

bool LimitCell::BreakUp() {
  if (IsBrokenIntoLines())
    return false;

  MakeBreakUpCells();
  Cell::BreakUpAndMark();
  m_name->last()->SetNextToDraw(m_open);
  m_open->SetNextToDraw(m_base);
  m_base->last()->SetNextToDraw(m_comma);
  m_comma->SetNextToDraw(m_under);
  m_under->last()->SetNextToDraw(m_close);
  m_close->SetNextToDraw(m_nextToDraw);
  m_nextToDraw = m_name;
  ResetCellListSizes();
  m_height = 0;
  m_center = 0;
  return true;
}

void LimitCell::SetNextToDraw(Cell *next) {
  if (IsBrokenIntoLines())
    m_close->SetNextToDraw(next);
  else
    m_nextToDraw = next;
}
