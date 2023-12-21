// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2023 Gunter Königsmann <wxMaxima@physikbuch.de>
//            (C)      2023 Wolfgang Dautermann
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
  If the wxWidgets version is at least 3.1.6, then the integral sign is rendered as SVG,
  otherwise as as spline.
*/

#include "IntCell.h"
#include "CellImpl.h"
#include "TextCell.h"
#include "SvgBitmap.h"
#include <memory>
#include <utility>
#include "intSign_svg.h"
#if wxCHECK_VERSION(3, 1, 6)
#include <wx/bmpbndl.h>
#endif

#if defined __WXMSW__
#define INTEGRAL_TOP "\xF3"
#define INTEGRAL_BOTTOM "\xF5"
#define INTEGRAL_EXTEND "\xF4"
static constexpr AFontSize INTEGRAL_FONT_SIZE{12.0f};
#endif


IntCell::IntCell(GroupCell *group, Configuration *config,
                 std::unique_ptr<Cell> &&base, std::unique_ptr<Cell> &&lowerLimit,
                 std::unique_ptr<Cell> &&upperLimit, std::unique_ptr<Cell> &&var)
  : Cell(group, config), m_base(std::move(base)), m_var(std::move(var)),
    m_lowerLimit(std::move(lowerLimit)), m_upperLimit(std::move(upperLimit)) {
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
            CopyList(group, cell.m_lowerLimit.get()),
            CopyList(group, cell.m_upperLimit.get()),
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

  m_base->RecalculateList(fontsize);
  m_var->RecalculateList(fontsize);
  if (IsBrokenIntoLines()) {
    // Cell is being displayed in its linear form. That means: All of its components
    // are printed in the current font size and the 2D object this cell can print
    // isn't displayed at all.
    m_open->RecalculateList(fontsize);
    m_comma1->RecalculateList(fontsize);
    m_comma2->RecalculateList(fontsize);
    m_lowerLimit->RecalculateList(fontsize);
    m_comma3->RecalculateList(fontsize);
    m_upperLimit->RecalculateList(fontsize);
    m_close->RecalculateList(fontsize);
    m_center = 0;
    m_height = 0;
    m_width = 0;
  } else {
    // Make the font used for the limits a bit smaller than the font used for the
    // contents  
    m_lowerLimit->RecalculateList({MC_MIN_SIZE, fontsize - 5});
    m_upperLimit->RecalculateList({MC_MIN_SIZE, fontsize - 5});
    if (UseSvgIntSign())
      {
        // The integral sign is displayed as an SVG graphics. As line thickness scales
        // with the sign we need to make it a constant height.
        m_signHeight = Scale_Px(40.0);
        if (m_signHeight < 6)
          m_signHeight = 6;
        // The sign width in this case is defined by the sign height and the sign's aspect
        // ratio
        m_signWidth = m_signHeight * 19.879 / 51.781;
      }
    else
      {
        // The integral sign is hand-drawn by wxWidgets and can therefore adapt
        // to it's content's height
        m_signWidth = Scale_Px(14);
        if (HasLimits())
          {
            m_signHeight = wxMax(Scale_Px(35) + m_upperLimit->GetHeightList() +
                                 m_lowerLimit->GetHeightList(),
                                 m_base->GetHeightList());
          }
        else
          {
            m_signHeight = Scale_Px(35) + m_base->GetHeightList();
          }
          
      }
    m_width = m_signWidth + m_base->GetFullWidth() +
      wxMax(m_upperLimit->GetFullWidth(), m_lowerLimit->GetFullWidth()) +
      m_var->GetFullWidth() + Scale_Px(4);
    if (HasLimits()) {
      /* The height of our whole integral needs to be
           - either high enough for the integral's contents, or
           - enough for the "upperLimit" and "lowerLimit" plus a MC_LINE_SKIP between them, or
           - enough for the integral sign.
      */
      m_center = wxMax(wxMax(m_signHeight / 2,
                             m_base->GetCenterList()), m_upperLimit->GetMaxDrop() + MC_LINE_SKIP / 2);
      m_height = m_center + wxMax(wxMax(m_signHeight / 2,
                                        m_base->GetMaxDrop()), m_upperLimit->GetMaxDrop()
                                  + MC_LINE_SKIP + m_lowerLimit->GetMaxDrop());
    } else {
      m_center = wxMax(m_signHeight / 2, m_base->GetCenterList());
      m_height = m_center + wxMax(m_signHeight / 2, m_base->GetMaxDrop());
    }
  }

}

void IntCell::Draw(wxPoint point, wxDC *dc, wxDC *antialiassingDC) {
  Cell::Draw(point, dc, antialiassingDC);
  if (DrawThisCell(point)) {
    wxPoint base(point), lowerLimit(point), upperLimit(point), var(point);

    SetPen(antialiassingDC, 1.5);

    if(UseSvgIntSign())
      DrawSvgSign(antialiassingDC, point);
    else
      DrawHanddrawnSign(antialiassingDC, point);

    if (HasLimits()) {
      lowerLimit.x += m_signWidth;
      lowerLimit.y = point.y + m_signHeight / 2 - m_lowerLimit->GetMaxDrop();
      m_lowerLimit->DrawList(lowerLimit, dc, antialiassingDC);

      upperLimit.x += m_signWidth;

      upperLimit.y = point.y - m_signHeight / 2 + m_upperLimit->GetHeightList() - m_upperLimit->GetCenterList();
      m_upperLimit->DrawList(upperLimit, dc, antialiassingDC);

      base.x += m_signWidth +
        wxMax(m_upperLimit->GetFullWidth(), m_lowerLimit->GetFullWidth());
    }
    else
      base.x += m_signWidth;

    m_base->DrawList(base, dc, antialiassingDC);

    var.x = base.x + m_base->GetFullWidth();
    m_var->DrawList(var, dc, antialiassingDC);
  }
}

void IntCell::DrawSvgSign(wxDC *dc, wxPoint pos)
{
  pos.y -= .5 * m_signHeight;
  dc->DrawBitmap(BitmapFromSVG(m_svgIntegralSign, wxSize(m_signWidth, m_signHeight)),
                pos.x, pos.y, true);
}

void IntCell::DrawHanddrawnSign(wxDC *dc, wxPoint pos)
{
  // top decoration
  int signCenter = m_signWidth / 2;
  wxPoint points[7] = {
    {pos.x + signCenter + 2 * (m_signWidth / 4),
     pos.y - (m_signHeight - Scale_Px(1)) / 2 + m_signWidth / 4},
    {pos.x + signCenter + m_signWidth / 4,
     pos.y - (m_signHeight - Scale_Px(1)) / 2},
    {pos.x + signCenter, pos.y - (m_signHeight - Scale_Px(1)) / 2 +
     2 * (m_signWidth / 4) + Scale_Px(.35)},
    
    // The line
    {pos.x + signCenter + Scale_Px(.5), pos.y},
    
    // Bottom Decoration
    {pos.x + signCenter, pos.y + (m_signHeight - Scale_Px(1)) / 2 -
     2 * (m_signWidth / 4) + Scale_Px(.35)},
    {pos.x + signCenter - m_signWidth / 4,
     pos.y + (m_signHeight - Scale_Px(1)) / 2},
    {pos.x + signCenter - 2 * (m_signWidth / 4),
     pos.y + (m_signHeight - Scale_Px(1)) / 2 - m_signWidth / 4}};
  
  dc->DrawSpline(7, points);
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

  wxString to = m_upperLimit->ListToString();
  wxString from = m_lowerLimit->ListToString();

  s += wxS(",") + var;
  if (HasLimits())
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

  wxString to = m_upperLimit->ListToMatlab();
  wxString from = m_lowerLimit->ListToMatlab();

  s += wxS(",") + var;
  if (HasLimits())
    s += wxS(",") + from + wxS(",") + to;

  s += wxS(")");
  return s;
}

wxString IntCell::ToTeX() const {
  wxString s = wxS("\\int");

  wxString to = m_upperLimit->ListToTeX();
  wxString from = m_lowerLimit->ListToTeX();

  if (HasLimits())
    s += wxS("_{") + from + wxS("}^{") + to + wxS("}");
  else
    s += wxS(" ");

  s += m_base->ListToTeX();
  s += wxS("{\\, ");
  s += m_var->ListToTeX();
  s += wxS("}");

  return s;
}

wxString IntCell::ToMathML() const {
  wxString base = m_base->ListToMathML();

  wxString var;
  if (m_var)
    var = m_var->ListToMathML();

  wxString from;
  if (m_lowerLimit)
    from = m_lowerLimit->ListToMathML();

  wxString to;
  if (m_upperLimit)
    to = m_upperLimit->ListToMathML();

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
  if (m_lowerLimit)
    from = m_lowerLimit->ListToOMML();

  wxString to;
  if (m_upperLimit)
    to = m_upperLimit->ListToOMML();

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
  if (m_lowerLimit != NULL)
    from = m_lowerLimit->ListToXML();
  from = wxS("<r>") + from + wxS("</r>");

  wxString to;
  if (m_upperLimit != NULL)
    to = m_upperLimit->ListToXML();
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

  if (HasLimits()) {
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
  if (HasLimits())
    m_var->last()->SetNextToDraw(m_close);
  else {
    m_var->last()->SetNextToDraw(m_comma2);
    m_comma2->last()->SetNextToDraw(m_lowerLimit);
    m_lowerLimit->last()->SetNextToDraw(m_comma3);
    m_comma3->last()->SetNextToDraw(m_upperLimit);
    m_upperLimit->last()->SetNextToDraw(m_close);
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

const wxString IntCell::m_svgIntegralSign(reinterpret_cast<const char*>(INTSIGN));
