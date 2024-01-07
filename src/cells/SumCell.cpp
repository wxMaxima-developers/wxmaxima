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

SumCell::SumCell(GroupCell *group, Configuration *config, 
                 std::unique_ptr<Cell> &&under, std::unique_ptr<Cell> &&over,
                 std::unique_ptr<Cell> &&base)
  : Cell(group, config),
    m_paren(std::make_unique<ParenCell>(group, config, std::move(base))),
    m_var(under->Copy(group)), m_start(MakeStart(under.get())),
    m_over(std::move(over)), m_under(std::move(under)) {
  InitBitFields_SumCell();
  if (!m_over)
    m_over = std::make_unique<TextCell>(group, config);
  wxASSERT(Base());
}

SumCell::SumCell(GroupCell *group, const SumCell &cell)
  : SumCell(group, cell.m_configuration,
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

  m_comma1 = std::make_unique<TextCell>(m_group, m_configuration, wxS(","));
  m_comma2 = std::make_unique<TextCell>(m_group, m_configuration, wxS(","));
  m_comma3 = std::make_unique<TextCell>(m_group, m_configuration, wxS(","));
  m_open = std::make_unique<TextCell>(m_group, m_configuration, GetMaximaCommandName());
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

const wxSize SumCell::GetSymbolSize() const
{
  wxSize signSize;
  // A sane height for the sum sign
  signSize.y = Scale_Px(40.0);
  // The width of the sum sign is defined by its height and aspect ratio
  signSize.x = 13 * signSize.y / 15;
  return signSize;
}

const wxString SumCell::GetMaximaCommandName() const {
  wxString s = wxS("sum(");
  if (m_over->ListToString() == wxEmptyString)
    s = wxS("lsum(");
  
  return s;
}

const wxString SumCell::GetSvgSymbolData() const
{
  return(m_svgSumSign);
}

//! What maxima command name corresponds to this cell?
const wxString SumCell::GetMatlabCommandName() const
{
  return wxS("sum(");
}

const wxString SumCell::GetLaTeXCommandName() const
{
  return wxS("\\sum");
}

const wxString SumCell::GetUnicodeSymbol() const
{
    return wxS("\u2211");
}

const wxString SumCell::GetXMLType() const
{
  wxString type(wxS("sum"));
  if (m_over->ListToString() == wxEmptyString)
      type = wxS("lsum");
  return type;
}

void SumCell::Recalculate(AFontSize fontsize) {
  if (NeedsRecalculation(fontsize)) {
    DisplayedBase()->RecalculateList(fontsize);
    m_start->RecalculateList(fontsize);
    m_var->RecalculateList(fontsize);
    
    m_signSize = GetSymbolSize();
    if (IsBrokenIntoLines()) {
      m_over->RecalculateList(fontsize);
      m_under->RecalculateList(fontsize);
      m_comma1->RecalculateList(fontsize);
      m_comma2->RecalculateList(fontsize);
      m_comma3->RecalculateList(fontsize);
      m_open->RecalculateList(fontsize);
      m_close->RecalculateList(fontsize);
      m_width = 0;
      m_center = 0;
      m_height = 0;
    } else {
      m_over->RecalculateList({MC_MIN_SIZE, fontsize - SUM_DEC});
      m_under->RecalculateList({MC_MIN_SIZE, fontsize - SUM_DEC});
      m_width = std::max(std::max(m_signSize.x, m_over->GetFullWidth()),
                         m_under->GetFullWidth()) + DisplayedBase()->GetFullWidth();
    
      m_center = std::max(m_signSize.y / 2 + Scale_Px(2)
                          + m_over->GetHeightList(),
                          DisplayedBase()->GetCenterList());
      m_height = m_center +
        std::max(m_under->GetHeightList() +  Scale_Px(2) + m_signSize.y / 2,
                 DisplayedBase()->GetMaxDrop());
    }    
    Cell::Recalculate(fontsize);
  } 
}

void SumCell::Draw(wxPoint point, wxDC *dc, wxDC *antialiassingDC) {
  Cell::Draw(point, dc, antialiassingDC);

  if (DrawThisCell(point)) {

    wxPoint base(point), under(point), over(point), sign(point);

    wxCoord signCenter_horizontal = std::max(std::max(m_signSize.x, m_over->GetFullWidth()),
                                             m_under->GetFullWidth()) / 2;

    under.x += signCenter_horizontal - m_under->GetFullWidth() / 2;
    under.y += + m_signSize.y / 2 + Scale_Px(2) + m_under->GetCenterList();
    m_under->DrawList(under, dc, antialiassingDC);

    over.x += signCenter_horizontal - m_over->GetFullWidth() / 2;
    over.y -= m_signSize.y / 2 + m_over->GetMaxDrop() + Scale_Px(2);
    m_over->DrawList(over, dc, antialiassingDC);

    sign.x += signCenter_horizontal - m_signSize.x / 2;
    sign.y -= .5 * m_signSize.y;
    antialiassingDC->DrawBitmap(BitmapFromSVG(GetSvgSymbolData(),
                                              wxSize(m_signSize.x, m_signSize.y)),
                                sign.x, sign.y, true);    
    base.x += std::max(std::max(m_signSize.x, m_over->GetFullWidth()),
                         m_under->GetFullWidth());
    DisplayedBase()->DrawList(base, dc, antialiassingDC);
  }
}

wxString SumCell::ToString() const {
  if (m_altCopyText != wxEmptyString)
    return m_altCopyText;

  wxString s = GetMaximaCommandName();

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
  wxString s = GetMatlabCommandName();
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
  wxString s = GetLaTeXCommandName();
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
  retval += GetUnicodeSymbol();

  retval += wxS("</m:chr></m:naryPr>");
  if (from != wxEmptyString)
    retval += wxS("<m:sub>") + from + wxS("</m:sub>");
  if (to != wxEmptyString)
    retval += wxS("<m:sup>") + to + wxS("</m:sup>");
  retval += wxS("<m:e>") + base + wxS("</m:e></m:nary>");

  return retval;
}
  
wxString SumCell::ToXML() const {
  wxString type = GetXMLType(); 
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

  if (from.IsEmpty() && to.IsEmpty())
    retval = wxS("<mo>" + GetUnicodeSymbol() + "</mo>") + base;
  if (from.IsEmpty() && !to.IsEmpty())
    retval = wxS("<mover><mo>" + GetUnicodeSymbol() + "</mo>") + to + wxS("</mover>") + base;
  if (!from.IsEmpty() && to.IsEmpty())
    retval =
      wxS("<munder><mo>" + GetUnicodeSymbol() + "</mo>") + from + wxS("</munder>") + base;
  if (!from.IsEmpty() && !to.IsEmpty())
    retval = wxS("<munderover><mo>" + GetUnicodeSymbol() + "</mo>") + from + to +
      wxS("</munderover>") + base;
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
  return true;
}

void SumCell::SetNextToDraw(Cell *next) {
  if (IsBrokenIntoLines())
    m_close->SetNextToDraw(next);
  else
    m_nextToDraw = next;
}

const wxString SumCell::m_svgSumSign(reinterpret_cast<const char*>(SUMSIGN));
