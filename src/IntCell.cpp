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
#include "FontCache.h"
#include "TextCell.h"

#if defined __WXMSW__
#define INTEGRAL_TOP "\xF3"
#define INTEGRAL_BOTTOM "\xF5"
#define INTEGRAL_EXTEND "\xF4"
#define INTEGRAL_FONT_SIZE 12
#endif

IntCell::IntCell(Cell *parent, Configuration **config, CellPointers *cellPointers) :
    Cell(parent, config, cellPointers),
    m_base(new TextCell(parent, config, cellPointers)),
    m_under(new TextCell(parent, config, cellPointers)),
    m_over(new TextCell(parent, config, cellPointers)),
    m_var(new TextCell(parent, config, cellPointers))
{
  m_nextToDraw = NULL;
  m_signHeight = 35;
  m_signWidth = 18;
  m_signTop = m_signHeight / 2;
  m_intStyle = INT_IDEF;
  m_charWidth = 12;
  m_charHeight = 12;
}

// Old cppcheck bugs:
// cppcheck-suppress uninitMemberVar symbolName=IntCell::m_signHeight
// cppcheck-suppress uninitMemberVar symbolName=IntCell::m_signWidth
// cppcheck-suppress uninitMemberVar symbolName=IntCell::m_signTop
// cppcheck-suppress uninitMemberVar symbolName=IntCell::m_charHeight
// cppcheck-suppress uninitMemberVar symbolName=IntCell::m_charWidth
IntCell::IntCell(const IntCell &cell):
    IntCell(cell.m_group, cell.m_configuration, cell.m_cellPointers)
{
  m_nextToDraw = NULL;
  CopyCommonData(cell);
  if(cell.m_base)
    SetBase(cell.m_base->CopyList());
  if(cell.m_under)
    SetUnder(cell.m_under->CopyList());
  if(cell.m_over)
    SetOver(cell.m_over->CopyList());
  if(cell.m_var)
    SetVar(cell.m_var->CopyList());
  m_intStyle = cell.m_intStyle;
}

IntCell::~IntCell()
{
  MarkAsDeleted();
}

void IntCell::SetOver(Cell *name)
{
  if (!name)
    return;
  m_over.reset(name);
}

void IntCell::SetBase(Cell *base)
{
  if (!base)
    return;
  m_base.reset(base);
}

void IntCell::SetUnder(Cell *under)
{
  if (!under)
    return;
  m_under.reset(under);
}

void IntCell::SetVar(Cell *var)
{
  if (var)
    return;
  m_var.reset(var);
}

void IntCell::RecalculateWidths(int fontsize)
{
  if (!NeedsRecalculation(fontsize))
    return;

  wxASSERT(fontsize >= 1);
  Configuration *configuration = (*m_configuration);

  m_signHeight = Scale_Px(35 * configuration->GetZoomFactor());
  m_signWidth = Scale_Px(18 * configuration->GetZoomFactor());
  if(m_signWidth < 4)
    m_signWidth = 4;

  m_base->RecalculateWidthsList(fontsize);
  m_var->RecalculateWidthsList(fontsize);
  m_under->RecalculateWidthsList(wxMax(MC_MIN_SIZE, fontsize - 5));
  m_over->RecalculateWidthsList(wxMax(MC_MIN_SIZE, fontsize - 5));

  if (configuration->CheckTeXFonts())
  {
    wxDC *dc = configuration->GetDC();
    double fontsize1 = Scale_Px(fontsize * 1.5);
    wxASSERT(fontsize1 > 0);

    wxFont font =
      FontCache::GetAFont(wxFontInfo(fontsize1)
                            .Family(wxFONTFAMILY_MODERN)
                            .Italic(false)
                            .Bold(false)
                            .Underlined(false)
                            .FaceName(configuration->GetTeXCMEX()));
    if (!font.IsOk())
    {
      auto req = wxFontInfo(fontsize1);
      FontInfo::CopyWithoutSize(wxNORMAL_FONT, req);
      font = FontCache::GetAFont(req);
    }

    dc->SetFont(font);
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
    double fontsize1 = Scale_Px(INTEGRAL_FONT_SIZE);
    wxASSERT(fontsize1 > 0);

    wxFont font =
      FontCache::GetAFont(wxFontInfo(fontsize1)
                            .Family(wxFONTFAMILY_MODERN)
                            .Style(wxFONTSTYLE_NORMAL)
                            .Weight(wxFONTWEIGHT_NORMAL)
                            .Underlined(false)
                            .FaceName(configuration->GetSymbolFontName()));

    if(!font.IsOk())
    {
      auto req = wxFontInfo(fontsize1);
      FontInfo::CopyWithoutSize(wxNORMAL_FONT, req);
      font = FontCache::GetAFont(req);
    }

    dc->SetFont(font);
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
  Cell::RecalculateWidths(fontsize);
}

void IntCell::RecalculateHeight(int fontsize)
{

  if(!NeedsRecalculation(fontsize))
    return;

  Cell::RecalculateHeight(fontsize);

  m_under->RecalculateHeightList(wxMax(MC_MIN_SIZE, fontsize - 5));
  m_over->RecalculateHeightList(wxMax(MC_MIN_SIZE, fontsize - 5));
  m_base->RecalculateHeightList(fontsize);
  m_var->RecalculateHeightList(fontsize);

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
      double fontsize1 = Scale_Px(m_fontSize * 1.5);
      wxASSERT(fontsize1 > 0);

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
      dc->DrawText(wxT("\u005A"),
                  sign.x,
                  sign.y - m_signTop);
    }
    else
    {
#if defined __WXMSW__
      SetForeground();
      double fontsize1 = Scale_Px(INTEGRAL_FONT_SIZE);
      int m_signWCenter = m_signWidth / 2;
      wxASSERT(fontsize1 > 0);

      wxFont font =
        FontCache::GetAFont(wxFontInfo(fontsize1)
                              .Family(wxFONTFAMILY_MODERN)
                              .Style(wxFONTSTYLE_NORMAL)
                              .Weight(wxFONTWEIGHT_NORMAL)
                              .Underlined(false)
                              .FaceName(configuration->GetSymbolFontName()));

      dc->SetFont(font);
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
        wxASSERT_MSG(m_charHeight>=2,_("Font issue: The char height is too small! Installing http://www.math.union.edu/~dpvc/jsmath/download/jsMath-fonts.html and checking \"Use JSmath fonts\" in the configuration dialogue should be a workaround."));
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
      wxPoint pointList[10];
      pointList[0] = wxPoint(sign.x + m_signWCenter + 2 * (m_signWidth / 4),
                             sign.y - (m_signHeight - Scale_Px(1)) / 2 + m_signWidth / 4);
      pointList[1] = wxPoint(sign.x + m_signWCenter + m_signWidth / 4,
                             sign.y - (m_signHeight - Scale_Px(1)) / 2);
      pointList[2] = wxPoint(sign.x + m_signWCenter,
                             sign.y - (m_signHeight - Scale_Px(1)) / 2 + 2* (m_signWidth / 4)
                             + Scale_Px(.35));

      // The line
      pointList[3] = wxPoint(sign.x + m_signWCenter + Scale_Px(.5),
                               sign.y);
      
      // Bottom Decoration
      pointList[4] = wxPoint(sign.x + m_signWCenter,
                             sign.y + (m_signHeight - Scale_Px(1)) / 2 - 2* (m_signWidth / 4)
                             + Scale_Px(.35));
      pointList[5] = wxPoint(sign.x + m_signWCenter - m_signWidth / 4,
                             sign.y + (m_signHeight - Scale_Px(1)) / 2);
      pointList[6] = wxPoint(sign.x + m_signWCenter - 2 * (m_signWidth / 4),
                             sign.y + (m_signHeight - Scale_Px(1)) / 2 - m_signWidth / 4);

      configuration->GetAntialiassingDC()->DrawSpline(7,pointList);
      pointList[1] = wxPoint(sign.x + m_signWCenter + m_signWidth / 4,
                             sign.y - (m_signHeight - Scale_Px(1.25)) / 2);
      pointList[2] = wxPoint(sign.x + m_signWCenter,
                             sign.y - (m_signHeight - Scale_Px(1)) / 2 + 2* (m_signWidth / 4)
                             - Scale_Px(.35));
      pointList[3] = wxPoint(sign.x + m_signWCenter - Scale_Px(.5),
                               sign.y);
      pointList[4] = wxPoint(sign.x + m_signWCenter,
                             sign.y + (m_signHeight - Scale_Px(1)) / 2 - 2* (m_signWidth / 4)
                             + Scale_Px(.35));
      pointList[5] = wxPoint(sign.x + m_signWCenter - m_signWidth / 4,
                             sign.y + (m_signHeight - Scale_Px(1.25)) / 2);
      configuration->GetAntialiassingDC()->DrawSpline(7,pointList);
      // line
      UnsetPen();
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

wxString IntCell::ToString()
{
  wxString s = wxT("integrate(");

  s += m_base->ListToString();

  Cell *tmp = m_var.get();
  wxString var;
  tmp = tmp->m_next;
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

wxString IntCell::ToMatlab()
{
  wxString s = wxT("integrate(");

  s += m_base->ListToMatlab();

  Cell *tmp = m_var.get();
  wxString var;
  tmp = tmp->m_next;
  if (tmp != NULL)
  {
	var = tmp->ListToMatlab();
  }

  wxString to = m_over->ListToMatlab();
  wxString from = m_under->ListToMatlab();

  s += wxT(",") + var;
  if (m_intStyle == INT_DEF)
	s += wxT(",") + from + wxT(",") + to;

  s += wxT(")");
  return s;
}

wxString IntCell::ToTeX()
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

wxString IntCell::ToMathML()
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

wxString IntCell::ToOMML()
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

wxString IntCell::ToXML()
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
    var = m_var->ListToXML();
  var = wxT("<r>") + var + wxT("</r>");

  wxString flags;
  if (m_forceBreakLine)
    flags += wxT(" breakline=\"true\"");

  if (m_intStyle != INT_DEF)
    flags += wxT(" def=\"false\">");
  
  return wxT("<in") + flags + wxT(">") + from + to + base + var + wxT("</in>");
}
