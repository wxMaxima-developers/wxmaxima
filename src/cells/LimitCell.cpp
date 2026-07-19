// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2016 Gunter Königsmann <wxMaxima@physikbuch.de>
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
*/

#include "LimitCell.h"
#include "CellImpl.h"
#include "TextCell.h"

LimitCell::LimitCell(GroupCell *group, Configuration *config,
                     std::unique_ptr<Cell> &&base,
                     std::unique_ptr<Cell> &&under,
                     std::unique_ptr<Cell> &&name)
  : Cell(group, config), m_name(std::move(name)), m_base(std::move(base)),
    m_under(std::move(under)) {
  InitBitFields_LimitCell();
  SetStyle(TS_VARIABLE);
  MakeBreakUpCells();
}

LimitCell::LimitCell(GroupCell *group, const LimitCell &cell)
  : LimitCell(group, cell.m_configuration, CopyList(group, cell.m_base.get()),
              CopyList(group, cell.m_under.get()),
              CopyList(group, cell.m_name.get())) {
  CopyCommonData(cell);
}

DEFINE_CELL(LimitCell)

void LimitCell::SetCurrentPoint(wxPoint point) const {
  Cell::SetCurrentPoint(point);
}

void LimitCell::MakeBreakUpCells() {
  m_open = std::make_unique<TextCell>(GetGroup(), m_configuration, wxS("limit("));
  m_comma = std::make_unique<TextCell>(GetGroup(), m_configuration, wxS(","));
  m_close = std::make_unique<TextCell>(GetGroup(), m_configuration, wxS(")"));
}

static constexpr int LIMIT_FONT_SIZE_DECREASE = 2;
static constexpr AFontSize MIN_LIMIT_FONT_SIZE = MC_MIN_SIZE;

void LimitCell::Recalculate(AFontSize fontsize) const {
  bool changed = false;
  changed |= m_base->RecalculateList(fontsize);
  changed |= m_name->RecalculateList(fontsize);

  changed |= m_open->RecalculateList(fontsize);
  changed |= m_comma->RecalculateList(fontsize);
  changed |= m_close->RecalculateList(fontsize);

  if (!IsBrokenIntoLines())
    changed |= m_under->RecalculateList(
        {MIN_LIMIT_FONT_SIZE, fontsize - LIMIT_FONT_SIZE_DECREASE});
  else
    changed |= m_under->RecalculateList(fontsize);

  if (changed || NeedsRecalculation(fontsize)) {
    Cell::Recalculate(fontsize);

    if (!IsBrokenIntoLines()) {
      m_width = std::max(m_name->SumOfWidths(), m_under->SumOfWidths()) +
                m_base->SumOfWidths();
      m_center = std::max(m_base->GetCenterList(), m_name->GetCenterList());
      m_height = m_center + std::max(m_name->GetMaxDrop() + m_under->GetHeightList(),
                                     m_base->GetMaxDrop());
    } else {
      m_width = m_height = m_center = 0;
    }
  }
}

void LimitCell::Draw(wxDC *dc, wxDC *antialiassingDC) {
  Cell::Draw(dc, antialiassingDC);
  if (DrawThisCell()) {
    wxPoint point = m_currentPoint;
    wxPoint base(point), under(point), name(point);

    name.x = point.x +
      std::max(m_name->SumOfWidths(), m_under->SumOfWidths()) / 2 -
      m_name->SumOfWidths() / 2;
    m_name->DrawList(dc, antialiassingDC);

    under.x = point.x +
      std::max(m_name->SumOfWidths(), m_under->SumOfWidths()) / 2 -
      m_under->SumOfWidths() / 2;

    under.y = point.y + m_name->GetMaxDrop() + m_under->GetCenterList();
    m_under->DrawList(dc, antialiassingDC);

    base.x += std::max(m_name->SumOfWidths(), m_under->SumOfWidths());
    m_base->DrawList(dc, antialiassingDC);
  }
}

wxString LimitCell::ToString() const {
  wxString s(wxS("limit"));
  wxString under = m_under->ListToString();
  wxString base = m_base->ListToString();
  auto arrowpos = under.Find(wxS("->"));
  wxString var;
  wxString to;
  if(arrowpos >= 0)
    {
      var = under.Left(arrowpos);
      to = under.Right(under.Length() - arrowpos - 2);
    }
  else
    {
      var = under;
      to = wxEmptyString;
    }
  s += wxS("(") + base + wxS(",") + var + wxS(",") + to + wxS(")");
  return s;
}

wxString LimitCell::ToMatlab() const {
  wxString s(wxS("limit("));
  s += m_base->ListToMatlab() + wxS(",") + m_under->ListToMatlab() + wxS(")");
  return s;
}

wxString LimitCell::ToTeX() const {
  wxString s(wxS("\\lim_{"));
  s += m_under->ListToTeX() + wxS("} ") + m_base->ListToTeX();
  return s;
}

wxString LimitCell::ToMathML() const {
  wxString s(wxS("<munder><mo>lim</mo>"));
  s += m_under->ListToMathML() + wxS("</munder>");
  s += m_base->ListToMathML();
  return s;
}

wxString LimitCell::ToOMML() const {
  wxString s(wxS("<m:limLow><m:lim>"));
  s += m_under->ListToOMML() + wxS("</m:lim>");
  s += wxS("<m:e>") + m_base->ListToOMML() + wxS("</m:e></m:limLow>");
  return s;
}

wxString LimitCell::ToXML() const {
  wxString s = wxS("<lm") + GetXMLFlags() + wxS(">");
  s += wxS("<r>") + m_name->ListToXML() + wxS("</r>");
  s += wxS("<r>") + m_under->ListToXML() + wxS("</r>");
  s += wxS("<r>") + m_base->ListToXML() + wxS("</r>");
  s += wxS("</lm>");
  return s;
}

bool LimitCell::BreakUp() const {
  if (IsBrokenIntoLines())
    return false;

  BreakUpAndMark();
  m_open->SetNextToDraw(m_base.get());
  m_base->last()->SetNextToDraw(m_comma.get());
  m_comma->SetNextToDraw(m_under.get());
  m_under->last()->SetNextToDraw(m_close.get());
  m_close->SetNextToDraw(m_nextToDraw);
  m_nextToDraw = m_open.get();
  return true;
}

void LimitCell::SetNextToDraw(Cell *next) const {
  if (IsBrokenIntoLines()) {
    m_close->SetNextToDraw(next);
  } else
    m_nextToDraw = next;
}
