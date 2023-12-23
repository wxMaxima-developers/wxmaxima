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
  This file defines the class SumCell

  SumCell is the Cell type that represents maxima's <code>sum()</code>,
  <code>lsum</code> and <code>product()</code>
  commands.
*/

#include "SumCell.h"
#include "CellImpl.h"
#include "TextCell.h"
#include "sumSign_svg.h"
#include "prodSign_svg.h"

SumCell::SumCell(GroupCell *group, Configuration *config, sumStyle style,
                 std::unique_ptr<Cell> &&under, std::unique_ptr<Cell> &&over,
                 std::unique_ptr<Cell> &&base)
  : Cell(group, config),
    m_paren(std::make_unique<ParenCell>(group, config, std::move(base))),
    m_var(under->Copy(group)), m_start(MakeStart(under.get())),
    m_over(std::move(over)), m_under(std::move(under)), m_sumStyle(style) {
  InitBitFields_SumCell();
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
            CopyList(group, cell.Base())) {
  CopyCommonData(cell);
  m_altCopyText = cell.m_altCopyText;
}

DEFINE_CELL(SumCell)

void SumCell::MakeBreakUpCells() {
  if (m_open)
    return;

  bool overStringEmpty = m_over->ToString().empty();
  const wxString &openText = (m_sumStyle == SM_SUM && overStringEmpty)
    ? wxS("lsum(")
    : (m_sumStyle == SM_SUM && !overStringEmpty)
    ? wxS("sum(")
    :
    /* (m_sumstyle == SM_PROD) */ wxS("prod(");

  m_comma1 = std::make_unique<TextCell>(m_group, m_configuration, wxS(","));
  m_comma2 = std::make_unique<TextCell>(m_group, m_configuration, wxS(","));
  m_comma3 = std::make_unique<TextCell>(m_group, m_configuration, wxS(","));
  m_open = std::make_unique<TextCell>(m_group, m_configuration, openText);
  m_close = std::make_unique<TextCell>(m_group, m_configuration, wxS(")"));
}

ParenCell *SumCell::Paren() const {
  return static_cast<ParenCell *>(m_paren.get());
}

Cell *SumCell::Base() const { return Paren() ? Paren()->GetInner() : nullptr; }

Cell *SumCell::DisplayedBase() const {
  return m_displayParen ? m_paren.get() : Paren()->GetInner();
}

std::unique_ptr<Cell> SumCell::MakeStart(Cell *under) const {
  std::unique_ptr<Cell> newStart;
  // m_under consists of a list of cells:
  //  The variable name, that can be more than one cell if there is a subscript.
  //  1 cell containing the text "in" or "=" (TODO: That's heuristics. Is there
  //  a better, but
  //                                          backwards-compatible way for
  //                                          this?)
  //  And the rest contains the lower limit.

  bool prevFound = false;
  for (auto &start : OnList(under)) {
    auto const &value = start.GetValue();
    if (prevFound) {
      newStart = start.CopyList(GetGroup());
      break;
    }
    prevFound = (value == wxS("in")) || (value == wxS("="));
  }

  return newStart ? std::move(newStart)
    : std::make_unique<TextCell>(m_group, m_configuration);
}

void SumCell::Recalculate(AFontSize fontsize) {
  DisplayedBase()->RecalculateList(fontsize);
  m_start->RecalculateList(fontsize);
  m_var->RecalculateList(fontsize);

  if (IsBrokenIntoLines()) {
    m_over->RecalculateList(fontsize);
    m_comma1->RecalculateList(fontsize);
    m_comma2->RecalculateList(fontsize);
    m_comma3->RecalculateList(fontsize);
    m_open->RecalculateList(fontsize);
    m_close->RecalculateList(fontsize);
  } else
    m_over->RecalculateList({MC_MIN_SIZE, fontsize - SUM_DEC});

  bool useSVGsign;
  if (m_sumStyle == SM_SUM)
    useSVGsign = true;
  else
    useSVGsign = false;
  // tell compilers and static analysis tools not to worry if they
  // believe that useSVGsign does never change value
  (void) useSVGsign;
  if(useSVGsign)
    {
      m_signHeight = Scale_Px(40.0);
      m_signWidth = 13 * m_signHeight / 15;
    }
  else
    {
      m_signHeight = DisplayedBase()->GetHeightList();
      if (m_sumStyle == SM_SUM)
        m_signWidth = 3.0 * m_signHeight / 5.0;
      else
        m_signWidth = 4.0 * m_signHeight / 5.0;
    }
  m_signWCenter = m_signWidth / 2.0;
  if (IsBrokenIntoLines())
    m_under->RecalculateList(fontsize);
  else
    m_under->RecalculateList({MC_MIN_SIZE, fontsize - SUM_DEC});

  m_signWCenter = std::max(m_signWCenter, m_under->GetFullWidth() / 2);
  m_signWCenter = std::max(m_signWCenter, m_over->GetFullWidth() / 2);
  m_width = 2 * m_signWCenter + DisplayedBase()->GetFullWidth() + Scale_Px(4);

  m_center = std::max(m_over->GetHeightList() + Scale_Px(2) + m_signHeight / 2,
                   DisplayedBase()->GetCenterList());
  m_height = m_center +
    std::max(m_under->GetHeightList() + Scale_Px(4) + m_signHeight / 2,
          DisplayedBase()->GetMaxDrop());
  Cell::Recalculate(fontsize);
}

void SumCell::Draw(wxPoint point, wxDC *dc, wxDC *antialiassingDC) {
  Cell::Draw(point, dc, antialiassingDC);

  if (DrawThisCell(point)) {

    wxPoint base(point), under(point), over(point);

    under.x += m_signWCenter - m_under->GetFullWidth() / 2;
    under.y = point.y + m_signHeight / 2 + m_under->GetCenterList() + Scale_Px(2);
    m_under->DrawList(under, dc, antialiassingDC);

    over.x += m_signWCenter - m_over->GetFullWidth() / 2;
    over.y = point.y - m_signHeight / 2 - m_over->GetMaxDrop() - Scale_Px(2);
    m_over->DrawList(over, dc, antialiassingDC);

    SetPen(antialiassingDC, 1.5);
    if (m_sumStyle == SM_SUM) {
      // FIXME: The sum sign look ok now, but the position/size can be improved!!
      if(UseSvgSign()) {
        antialiassingDC->DrawBitmap(BitmapFromSVG(m_svgSumSign, wxSize(m_signWidth, m_signHeight)),
                                    base.x, over.y+m_signHeight/3, true);
      } else {
        // DRAW SUM SIGN
        //  Upper part
        const wxPoint points[5] = {
          {m_signWCenter + int(m_signWidth / 2), -(m_signHeight / 2)},
          {m_signWCenter - int(m_signWidth / 2), -(m_signHeight / 2)},
          {m_signWCenter + int(m_signWidth / 5), 0},
          {m_signWCenter - int(m_signWidth / 2), (m_signHeight / 2)},
          {m_signWCenter + int(m_signWidth / 2), (m_signHeight / 2)}};
        antialiassingDC->DrawLines(5, points, point.x, point.y);
      }
    } else {
      // DRAW PRODUCT SIGN
      // FIXME: The product sign look ok now, but the position/size can be improved!!
      if(UseSvgSign()) {
        antialiassingDC->DrawBitmap(BitmapFromSVG(m_svgProdSign, wxSize(m_signWidth, m_signHeight)),
                                    base.x, over.y+m_signHeight/3, true);
      } else {
        // Vertical lines
        antialiassingDC->DrawLine(point.x + m_signWCenter + m_signWidth / 6,
                                  point.y + m_signHeight / 2,
                                  point.x + m_signWCenter + m_signWidth / 6,
                                  point.y - m_signHeight / 2);
        antialiassingDC->DrawLine(point.x + m_signWCenter - m_signWidth / 6,
                                  point.y + m_signHeight / 2,
                                  point.x + m_signWCenter - m_signWidth / 6,
                                  point.y - m_signHeight / 2);
        // Horizontal line
        antialiassingDC->DrawLine(point.x + m_signWCenter - m_signWidth / 2,
                                  point.y - m_signHeight / 2,
                                  point.x + m_signWCenter + m_signWidth / 2,
                                  point.y - m_signHeight / 2);
      }
    }
    base.x += (2 * m_signWCenter + Scale_Px(4));
    DisplayedBase()->DrawList(base, dc, antialiassingDC);
  }
}

wxString SumCell::ToString() const {
  if (m_altCopyText != wxEmptyString)
    return m_altCopyText;

  wxString s;
  if (m_sumStyle == SM_SUM)
    s = wxS("sum(");
  else
    s = wxS("product(");

  if (m_over->ListToString() == wxEmptyString) {
    if (m_sumStyle == SM_PROD)
      s = wxS("lprod(");
    else
      s = wxS("lsum(");
  }

  s += Base()->ListToString();

  Cell *tmp = m_under.get();
  wxString var = tmp->ToString();
  wxString from;
  tmp = tmp->GetNext();
  if (tmp != NULL) {
    tmp = tmp->GetNext();
    if (tmp != NULL)
      from = tmp->ListToString();
  }
  wxString to = m_over->ListToString();
  s += wxS(",") + var + wxS(",") + from;
  if (to != wxEmptyString)
    s += wxS(",") + to + wxS(")");
  else
    s = wxS("l") + s + wxS(")");
  return s;
}

wxString SumCell::ToMatlab() const {
  wxString s;
  if (m_sumStyle == SM_SUM)
    s = wxS("sum(");
  else
    s = wxS("product(");
  s += Base()->ListToMatlab();

  Cell *tmp = m_under.get();
  wxString var = tmp->ToMatlab();
  wxString from;
  tmp = tmp->GetNext();
  if (tmp != NULL) {
    tmp = tmp->GetNext();
    if (tmp != NULL)
      from = tmp->ListToMatlab();
  }
  wxString to = m_over->ListToMatlab();
  s += wxS(",") + var + wxS(",") + from;
  if (to != wxEmptyString)
    s += wxS(",") + to + wxS(")");
  else
    s = wxS("l") + s + wxS(")");
  return s;
}

wxString SumCell::ToTeX() const {
  wxString s;
  if (m_sumStyle == SM_SUM)
    s = wxS("\\sum");
  else
    s = wxS("\\prod");

  s += wxS("_{") + m_under->ListToTeX() + wxS("}");
  wxString to = m_over->ListToTeX();
  if (to.Length())
    s += wxS("^{") + to + wxS("}");

  s += wxS("{\\left. ");
  s += Base()->ListToTeX();
  s += wxS("\\right.}");
  return s;
}

wxString SumCell::ToOMML() const {
  wxString base = Base()->ListToOMML();
  wxString from = m_under ? m_under->ListToOMML() : wxString{};
  wxString to = m_over ? m_over->ListToOMML() : wxString{};

  wxString retval;

  retval = wxS("<m:nary><m:naryPr><m:chr>");
  if (m_sumStyle == SM_SUM)
    retval += wxS("\u2211");
  else
    retval += wxS("\u220F");

  retval += wxS("</m:chr></m:naryPr>");
  if (from != wxEmptyString)
    retval += wxS("<m:sub>") + from + wxS("</m:sub>");
  if (to != wxEmptyString)
    retval += wxS("<m:sup>") + to + wxS("</m:sup>");
  retval += wxS("<m:e>") + base + wxS("</m:e></m:nary>");

  return retval;
}

wxString SumCell::ToXML() const {
  wxString type(wxS("sum"));

  if (m_sumStyle == SM_PROD)
    type = wxS("prod");
  if (m_over->ListToString() == wxEmptyString) {
    if (m_sumStyle == SM_PROD)
      type = wxS("lprod");
    else
      type = wxS("lsum");
  }

  wxString flags;
  if (HasHardLineBreak())
    flags += wxS(" breakline=\"true\"");

  return wxS("<sm type=\"") + type + "\"" + flags + wxS("><r>") +
    m_under->ListToXML() + _T("</r><r>") + m_over->ListToXML() +
    _T("</r><r>") + Base()->ListToXML() + _T("</r></sm>");
}

wxString SumCell::ToMathML() const {
  wxString base = Base()->ListToMathML();

  wxString from;
  if (m_under)
    from = m_under->ListToMathML();

  wxString to;
  if (m_over)
    to = m_over->ListToMathML();

  wxString retval;

  if (m_sumStyle == SM_SUM) {
    if (from.IsEmpty() && to.IsEmpty())
      retval = wxS("<mo>&#x2211;</mo>") + base;
    if (from.IsEmpty() && !to.IsEmpty())
      retval = wxS("<mover><mo>&#x2211;</mo>") + to + wxS("</mover>") + base;
    if (!from.IsEmpty() && to.IsEmpty())
      retval =
        wxS("<munder><mo>&#x2211;</mo>") + from + wxS("</munder>") + base;
    if (!from.IsEmpty() && !to.IsEmpty())
      retval = wxS("<munderover><mo>&#x2211;</mo>") + from + to +
        wxS("</munderover>") + base;
  } else {
    // A product
    if (from.IsEmpty() && to.IsEmpty())
      retval = wxS("<mo>&#x220F;</mo>") + base;
    if (from.IsEmpty() && !to.IsEmpty())
      retval = wxS("<mover><mo>&#x220F;</mo>") + to + wxS("</mover>") + base;
    if (!from.IsEmpty() && to.IsEmpty())
      retval =
        wxS("<munder><mo>&#x220F;</mo>") + from + wxS("</munder>") + base;
    if (!from.IsEmpty() && !to.IsEmpty())
      retval = wxS("<munderover><mo>&#x220F;</mo>") + from + to +
        wxS("</munderover>") + base;
  }
  return (wxS("<mrow>") + retval + wxS("</mrow>"));
}

void SumCell::Unbreak() {
  m_displayParen = true;
  Cell::Unbreak();
}

bool SumCell::BreakUp() {
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
  if (m_over->ToString().IsEmpty())
    m_start->last()->SetNextToDraw(m_close);
  else {
    m_start->last()->SetNextToDraw(m_comma3);
    m_comma3->last()->SetNextToDraw(m_over);
    m_over->last()->SetNextToDraw(m_close);
  }
  ResetCellListSizes();
  m_height = 0;
  m_center = 0;
  return true;
}

void SumCell::SetNextToDraw(Cell *next) {
  if (IsBrokenIntoLines())
    m_close->SetNextToDraw(next);
  else
    m_nextToDraw = next;
}

const wxString SumCell::m_svgSumSign(reinterpret_cast<const char*>(SUMSIGN));
const wxString SumCell::m_svgProdSign(reinterpret_cast<const char*>(PRODSIGN));
