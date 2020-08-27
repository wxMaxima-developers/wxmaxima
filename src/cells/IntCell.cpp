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
  This file defines the class IntCell

  IntCell is the Cell type that represents maxima's <code>integrate()</code> command.
*/

#include "IntCell.h"
#include "CellImpl.h"
#include "TextCell.h"

#if defined __WXMSW__
#define INTEGRAL_TOP "\xF3"
#define INTEGRAL_BOTTOM "\xF5"
#define INTEGRAL_EXTEND "\xF4"
static constexpr AFontSize INTEGRAL_FONT_SIZE{ 12.0f };
#endif

IntCell::IntCell(GroupCell *parent, Configuration **config,
                 std::unique_ptr<Cell> &&base, std::unique_ptr<Cell> &&under,
                 std::unique_ptr<Cell> &&over, std::unique_ptr<Cell> &&var)
    : Cell(parent, config),
    m_base(std::move(base)),
    m_under(std::move(under)),
    m_over(std::move(over)),
    m_var(std::move(var))
{
  InitBitFields();
  SetStyle(TS_VARIABLE);
}

IntCell::IntCell(GroupCell *parent, Configuration **config,
                 std::unique_ptr<Cell> &&base, std::unique_ptr<Cell> &&var)
    : IntCell(parent, config, std::move(base),
              std::make_unique<TextCell>(parent, config),
              std::make_unique<TextCell>(parent, config), std::move(var))
{}

// Old cppcheck bugs:
// cppcheck-suppress uninitMemberVar symbolName=IntCell::m_signHeight
// cppcheck-suppress uninitMemberVar symbolName=IntCell::m_signWidth
// cppcheck-suppress uninitMemberVar symbolName=IntCell::m_signTop
// cppcheck-suppress uninitMemberVar symbolName=IntCell::m_charHeight
// cppcheck-suppress uninitMemberVar symbolName=IntCell::m_charWidth
IntCell::IntCell(const IntCell &cell)
    : IntCell(cell.m_group, cell.m_configuration, CopyList(cell.m_base.get()),
              CopyList(cell.m_under.get()), CopyList(cell.m_over.get()),
              CopyList(cell.m_var.get()))
{
  CopyCommonData(cell);
  m_intStyle = cell.m_intStyle;
}

DEFINE_CELL(IntCell)

void IntCell::MakeBreakUpCells()
{
  if (m_open) return;
  m_open = std::make_unique<TextCell>(m_group, m_configuration, "integrate(");
  m_close = std::make_unique<TextCell>(m_group, m_configuration, ")");
  m_comma1 = std::make_unique<TextCell>(m_group, m_configuration, ",");
  m_comma2 = std::make_unique<TextCell>(m_group, m_configuration, ",");
  m_comma3 = std::make_unique<TextCell>(m_group, m_configuration, ",");
}

void IntCell::Recalculate(AFontSize fontsize)
{
  if (!NeedsRecalculation(fontsize))
    return;

  wxASSERT(fontsize.IsValid());
  Configuration *configuration = (*m_configuration);
  
  m_signHeight = Scale_Px(35 * configuration->GetZoomFactor());
  m_signWidth = Scale_Px(18 * configuration->GetZoomFactor());
  if(m_signWidth < 4)
    m_signWidth = 4;
  
  if (IsBrokenIntoLines())
  {
    m_base->RecalculateList(fontsize);
    m_open->RecalculateList(fontsize);
    m_comma1->RecalculateList(fontsize);
    m_var->RecalculateList(fontsize);
    m_comma2->RecalculateList(fontsize);
    m_under->RecalculateList(fontsize);
    m_comma3->RecalculateList(fontsize);
    m_over->RecalculateList(fontsize);
    m_close->RecalculateList(fontsize);
  }
  else
  {
    m_base->RecalculateList(fontsize);
    m_var->RecalculateList(fontsize);
    m_under->RecalculateList({ MC_MIN_SIZE, fontsize - 5 });
    m_over->RecalculateList({ MC_MIN_SIZE, fontsize - 5 });
  }

  if(IsBrokenIntoLines())
  {
    m_center = 0;
    m_height = 0;
    m_width = 0;
  }
  else
  {
    if (configuration->CheckTeXFonts())
    {
      wxDC *dc = configuration->GetDC();
      auto fontsize1 = AFontSize(Scale_Px(fontsize * 1.5));
      wxASSERT(fontsize1.IsValid());
      
      Style style = Style(fontsize1)
        .FontName(configuration->GetTeXCMEX());
      if (!style.IsFontOk())
      {
        style = Style::FromStockFont(wxStockGDI::FONT_NORMAL);
        style.SetFontSize(fontsize1);
      }
      
      dc->SetFont(style.GetFont());
      dc->GetTextExtent(wxT("\u005A"), &m_signWidth, &m_signHeight);
      
#if defined __WXMSW__
      m_signWidth = m_signWidth / 2;
#endif
      m_signTop = m_signHeight / 2;
      m_signHeight = (85 * m_signHeight) / 100;
      
      m_width = m_signWidth +
        wxMax(m_over->GetFullWidth() + m_signWidth, m_under->GetFullWidth()) +
        m_base->GetFullWidth() +
        m_var->GetFullWidth() +
        Scale_Px(4);
    }
    else
    {
#if defined __WXMSW__
      wxDC *dc = configuration->GetDC();
      auto fontsize1 = Scale_Px(INTEGRAL_FONT_SIZE);
      wxASSERT(fontsize1.IsValid());
      
      Style style = Style(fontsize1)
        .FontName(configuration->GetSymbolFontName());
      
      if (!style.IsFontOk())
      {
        style = Style::FromStockFont(wxStockGDI::FONT_NORMAL);
        style.SetFontSize(fontsize1);
      }
      
      dc->SetFont(style.GetFont());
      dc->GetTextExtent(INTEGRAL_TOP, &m_charWidth, &m_charHeight);
      
      m_width = m_signWidth +
        m_base->GetFullWidth() +
        wxMax(m_over->GetFullWidth(), m_under->GetFullWidth()) +
        m_var->GetFullWidth() +
        Scale_Px(4);
#else
      m_width = m_signWidth +
        m_base->GetFullWidth() +
        wxMax(m_over->GetFullWidth(), m_under->GetFullWidth()) +
        m_var->GetFullWidth() +
        Scale_Px(4);
      if(m_signHeight < Scale_Px(35))
        m_signHeight = Scale_Px(35);
#endif
    }
    
    if (m_intStyle == INT_DEF)
    {
      m_center = wxMax(m_over->GetHeightList() + Scale_Px(4) + m_signHeight / 2 - m_signHeight / 3,
                       m_base->GetCenterList());
      m_height = m_center +
        wxMax(m_under->GetHeightList() + Scale_Px(4) + m_signHeight / 2 - m_signHeight / 3,
              m_base->GetMaxDrop());
    }
    else
    {
      m_center = wxMax(m_signHeight / 2, m_base->GetCenterList());
      m_height = m_center +
        wxMax(m_signHeight / 2, m_base->GetMaxDrop());
    }
    
  }
}

void IntCell::Draw(wxPoint point)
{
  Cell::Draw(point);
  if (DrawThisCell(point))
  {
    Configuration *configuration = (*m_configuration);
    
    wxDC *dc = configuration->GetDC();
    
    wxPoint base(point), under(point), over(point), var(point), sign(point);

    if (configuration->CheckTeXFonts())
    {
      SetForeground();
      auto fontsize1 = AFontSize(Scale_Px(m_fontSize * 1.5));
      wxASSERT(fontsize1.IsValid());

      Style style = Style(fontsize1)
        .FontName(configuration->GetTeXCMEX());

      if (!style.IsFontOk())
        configuration->CheckTeXFonts(false);

      dc->SetFont(style.GetFont());
      dc->DrawText(wxT("\u005A"),
                  sign.x,
                  sign.y - m_signTop);
    }
    else
    {
#if defined __WXMSW__
      SetForeground();
      auto fontsize1 = AFontSize(Scale_Px(INTEGRAL_FONT_SIZE));
      int m_signWCenter = m_signWidth / 2;
      wxASSERT(fontsize1.IsValid());

      Style style = Style(fontsize1)
                      .FontName(configuration->GetSymbolFontName());

      dc->SetFont(style.GetFont());
      dc->DrawText(INTEGRAL_TOP,
                  sign.x + m_signWCenter - m_charWidth / 2,
                  sign.y - (m_signHeight + 1) / 2);
      dc->DrawText(INTEGRAL_BOTTOM,
                  sign.x + m_signWCenter - m_charWidth / 2,
                  sign.y + (m_signHeight + 1) / 2 - m_charHeight);

      int top, bottom;
      top = sign.y - (m_signHeight + 1) / 2 + m_charHeight / 2;
      bottom = sign.y + (m_signHeight + 1) / 2 - (3 * m_charHeight) / 2;
      if (top <= bottom)
      {
        wxASSERT_MSG(m_charHeight>=2,_("Font issue: The char height is too small!"));
        if(m_charHeight <= 2)
          m_charHeight = 2;

        while (top < bottom)
        {
          dc->DrawText(INTEGRAL_EXTEND,
          point.x + m_signWCenter - m_charWidth / 2,
          top);
          top += (2*m_charHeight)/3;
        }
        dc->DrawText(INTEGRAL_EXTEND,
        point.x + m_signWCenter - m_charWidth / 2,
        sign.y + (m_signHeight + 1) / 2 - (3 * m_charHeight) / 2);
      }
#else
      SetPen(1.5);
      // top decoration
      int m_signWCenter = m_signWidth / 2;
      wxPoint points[7] =
        {
          {sign.x + m_signWCenter + 2 * (m_signWidth / 4),
           sign.y - (m_signHeight - Scale_Px(1)) / 2 + m_signWidth / 4},
          {sign.x + m_signWCenter + m_signWidth / 4,
           sign.y - (m_signHeight - Scale_Px(1)) / 2},
          {sign.x + m_signWCenter,
           sign.y - (m_signHeight - Scale_Px(1)) / 2 + 2* (m_signWidth / 4)
             + Scale_Px(.35)},

          // The line
          {sign.x + m_signWCenter + Scale_Px(.5),
           sign.y},

          // Bottom Decoration
          {sign.x + m_signWCenter,
           sign.y + (m_signHeight - Scale_Px(1)) / 2 - 2* (m_signWidth / 4)
             + Scale_Px(.35)},
          {sign.x + m_signWCenter - m_signWidth / 4,
           sign.y + (m_signHeight - Scale_Px(1)) / 2},
          {sign.x + m_signWCenter - 2 * (m_signWidth / 4),
           sign.y + (m_signHeight - Scale_Px(1)) / 2 - m_signWidth / 4}
          };

      configuration->GetAntialiassingDC()->DrawSpline(7, points);
      points[1] = {sign.x + m_signWCenter + m_signWidth / 4,
                   sign.y - (m_signHeight - Scale_Px(1.25)) / 2};
      points[2] = {sign.x + m_signWCenter,
                   sign.y - (m_signHeight - Scale_Px(1)) / 2 + 2* (m_signWidth / 4)
                     - Scale_Px(.35)};
      points[3] = {sign.x + m_signWCenter - Scale_Px(.5),
                   sign.y};
      points[4] = {sign.x + m_signWCenter,
                   sign.y + (m_signHeight - Scale_Px(1)) / 2 - 2* (m_signWidth / 4)
                     + Scale_Px(.35)};
      points[5] = {sign.x + m_signWCenter - m_signWidth / 4,
                   sign.y + (m_signHeight - Scale_Px(1.25)) / 2};
      configuration->GetAntialiassingDC()->DrawSpline(7, points);
      // line
#endif
    }

    if (m_intStyle == INT_DEF)
    {
      under.x += m_signWidth;
      under.y = point.y + m_signHeight / 2 + m_under->GetCenterList() + Scale_Px(2) -
                m_signHeight / 3;
      m_under->DrawList(under);

      if (configuration->CheckTeXFonts())
        over.x += 2 * m_signWidth;
      else
        over.x += m_signWidth;

      over.y = point.y - m_signHeight / 2 - m_over->GetMaxDrop() - Scale_Px(2) +
               m_signHeight / 3;
      m_over->DrawList(over);

      if (configuration->CheckTeXFonts())
      {
        base.x += m_signWidth +
                  wxMax(m_over->GetFullWidth() + m_signWidth, m_under->GetFullWidth());
      }
      else
        base.x += m_signWidth +
                  wxMax(m_over->GetFullWidth(), m_under->GetFullWidth());
    }

    else if (configuration->CheckTeXFonts())
      base.x += 2 * m_signWidth;
    else
      base.x += m_signWidth;

    m_base->DrawList(base);

    var.x = base.x + m_base->GetFullWidth();
    m_var->DrawList(var);
  }
}

wxString IntCell::ToString() const
{
  wxString s = wxT("integrate(");

  s += m_base->ListToString();

  Cell *tmp = m_var.get();
  wxString var;
  tmp = tmp->GetNext();
  if (tmp != NULL)
  {
    var = tmp->ListToString();
  }

  wxString to = m_over->ListToString();
  wxString from = m_under->ListToString();

  s += wxT(",") + var;
  if (m_intStyle == INT_DEF)
    s += wxT(",") + from + wxT(",") + to;

  s += wxT(")");
  return s;
}

wxString IntCell::ToMatlab() const
{
  wxString s = wxT("integrate(");

  s += m_base->ListToMatlab();

  Cell *tmp = m_var.get();
  wxString var;
  tmp = tmp->GetNext();
  if (tmp != NULL)
    var = tmp->ListToMatlab();

  wxString to = m_over->ListToMatlab();
  wxString from = m_under->ListToMatlab();

  s += wxT(",") + var;
  if (m_intStyle == INT_DEF)
	s += wxT(",") + from + wxT(",") + to;

  s += wxT(")");
  return s;
}

wxString IntCell::ToTeX() const
{
  wxString s = wxT("\\int");

  wxString to = m_over->ListToTeX();
  wxString from = m_under->ListToTeX();

  if (m_intStyle == INT_DEF)
    s += wxT("_{") + from + wxT("}^{") + to + wxT("}");
  else
    s += wxT(" ");

  s += wxT("{\\left. ");
  s += m_base->ListToTeX();
  s += m_var->ListToTeX();
  s += wxT("\\right.}");

  return s;
}

wxString IntCell::ToMathML() const
{
  wxString base = m_base->ListToMathML();

  wxString var;
  if (m_var) var = m_var->ListToMathML();

  wxString from;
  if (m_under) from = m_under->ListToMathML();

  wxString to;
  if (m_over) to = m_over->ListToMathML();

  wxString retval;
  if (from.IsEmpty() && to.IsEmpty())
    retval = wxT("<mo>&#x222B;</mo>") + base;
  if (from.IsEmpty() && !to.IsEmpty())
    retval = wxT("<mover><mo>&#x222B;</mo>") + to + wxT("</mover>") + base;
  if (!from.IsEmpty() && to.IsEmpty())
    retval = wxT("<munder><mo>&#x222B;</mo>") + from + wxT("</munder>") + base;
  if (!from.IsEmpty() && !to.IsEmpty())
    retval = wxT("<munderover><mo>&#x222B;</mo>") + from + to + wxT("</munderover>\n") + base;
  if (!var.IsEmpty())
    retval = retval + var;

  return (wxT("<mrow>") + retval + wxT("</mrow>"));
}

wxString IntCell::ToOMML() const
{
  wxString base = m_base->ListToOMML();

  wxString var;
  if (m_var) var = m_var->ListToOMML();

  wxString from;
  if (m_under) from = m_under->ListToOMML();

  wxString to;
  if (m_over) to = m_over->ListToOMML();

  wxString retval;

  retval = wxT("<m:nary><m:naryPr><m:chr>\u222b</m:chr></m:naryPr>");
  if (from != wxEmptyString)
    retval += wxT("<m:sub>") + from + wxT("</m:sub>");
  if (to != wxEmptyString)
    retval += wxT("<m:sup>") + to + wxT("</m:sup>");
  retval += wxT("<m:e><m:r>") + base + var + wxT("</m:r></m:e></m:nary>");

  return retval;
}

wxString IntCell::ToXML() const
{
  wxString from;
  if (m_under != NULL)
    from = m_under->ListToXML();
  from = wxT("<r>") + from + wxT("</r>");

  wxString to;
  if (m_over != NULL)
    to = m_over->ListToXML();
  to = wxT("<r>") + to + wxT("</r>");

  wxString base;
  if (m_base != NULL)
    base = m_base->ListToXML();
  base = wxT("<r>") + base + wxT("</r>");

  wxString var;
  if (m_var != NULL)
  {
    var = m_var->ToXML();
  }
  if (m_var->GetNext() != NULL)
  {
    var = m_var->GetNext()->ListToXML();
  }
  var = wxT("<r>") + var + wxT("</r>");

  wxString flags;
  if (HasHardLineBreak())
    flags += wxT(" breakline=\"true\"");

  if (m_intStyle != INT_DEF)
  {
    flags += wxT(" def=\"false\">");
    return wxT("<in") + flags + wxT(">") + base + var + wxT("</in>");
  }
  else
    return wxT("<in") + flags + wxT(">") + from + to + base + var + wxT("</in>");
}

bool IntCell::BreakUp()
{
  if (IsBrokenIntoLines())
    return false;

  MakeBreakUpCells();
  Cell::BreakUpAndMark();
  m_close->SetNextToDraw(m_nextToDraw);
  m_nextToDraw = m_open;
  m_open->last()->SetNextToDraw(m_base);
  m_base->last()->SetNextToDraw(m_comma1);
  // The first cell of m_var should normally be a "d"
  if(m_var->GetNext() != NULL)
    m_comma1->last()->SetNextToDraw(m_var->GetNext());
  else
    m_comma1->last()->SetNextToDraw(m_var);
  if (m_intStyle != INT_DEF)
    m_var->last()->SetNextToDraw(m_close);
  else{
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

void IntCell::SetNextToDraw(Cell *next)
{
  if (IsBrokenIntoLines())
    m_close->SetNextToDraw(next);
  else
    m_nextToDraw = next;
}
