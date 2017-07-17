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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//

/*! \file
  This file defines the class IntCell

  IntCell is the MathCell type that represents maxima's <code>integrate()</code> command.
*/

#include "IntCell.h"
#include "TextCell.h"

#if defined __WXMSW__
#define INTEGRAL_TOP "\xF3"
#define INTEGRAL_BOTTOM "\xF5"
#define INTEGRAL_EXTEND "\xF4"
#define INTEGRAL_FONT_SIZE 12
#endif

IntCell::IntCell(MathCell *parent, Configuration **config, CellPointers *cellPointers) : MathCell(parent, config)
{
  m_base = NULL;
  m_under = NULL;
  m_over = NULL;
  m_var = NULL;
  m_signSize = 50;
  m_signWidth = 18;
  m_signTop = m_signSize / 2;
  m_intStyle = INT_IDEF;
  m_charWidth = 12;
  m_charHeight = 12;
  m_cellPointers = cellPointers;
}

void IntCell::SetParent(MathCell *parent)
{
  m_group = parent;
  if (m_base != NULL)
    m_base->SetParentList(parent);
  if (m_under != NULL)
    m_under->SetParentList(parent);
  if (m_over != NULL)
    m_over->SetParentList(parent);
  if (m_var != NULL)
    m_var->SetParentList(parent);
}

MathCell *IntCell::Copy()
{
  IntCell *tmp = new IntCell(m_group, m_configuration, m_cellPointers);
  CopyData(this, tmp);
  tmp->SetBase(m_base->CopyList());
  tmp->SetUnder(m_under->CopyList());
  tmp->SetOver(m_over->CopyList());
  tmp->SetVar(m_var->CopyList());
  tmp->m_intStyle = m_intStyle;

  return tmp;
}

IntCell::~IntCell()
{
  wxDELETE(m_base);
  wxDELETE(m_under);
  wxDELETE(m_over);
  wxDELETE(m_var);
  m_base = m_under = m_over = m_var = NULL;
  MarkAsDeleted();
}

void IntCell::MarkAsDeleted()
{
  MarkAsDeletedList(m_base, m_under, m_over, m_var);
  if((this == m_cellPointers->m_selectionStart) || (this == m_cellPointers->m_selectionEnd))
    m_cellPointers->m_selectionStart = m_cellPointers->m_selectionEnd = NULL;
  if(this == m_cellPointers->m_cellUnderPointer)
    m_cellPointers->m_cellUnderPointer = NULL;
}

void IntCell::SetOver(MathCell *over)
{
  if (over == NULL)
    return;
  wxDELETE(m_over);
  m_over = over;
}

void IntCell::SetBase(MathCell *base)
{
  if (base == NULL)
    return;
  wxDELETE(m_base);
  m_base = base;
}

void IntCell::SetUnder(MathCell *under)
{
  if (under == NULL)
    return;
  wxDELETE(m_under);
  m_under = under;
}

void IntCell::SetVar(MathCell *var)
{
  if (var == NULL)
    return;
  wxDELETE(m_var);
  m_var = var;
}

void IntCell::RecalculateWidths(int fontsize)
{
  Configuration *configuration = (*m_configuration);
  double scale = configuration->GetScale();

  m_signSize = Scale_Px(50, scale) * configuration->GetZoomFactor();
  m_signWidth = Scale_Px(18, scale) * configuration->GetZoomFactor();

  m_base->RecalculateWidthsList(fontsize);
  m_var->RecalculateWidthsList(fontsize);
  if (m_under == NULL)
    m_under = new TextCell(m_group, m_configuration, m_cellPointers);
  m_under->RecalculateWidthsList(MAX(MC_MIN_SIZE, fontsize - 5));
  if (m_over == NULL)
    m_over = new TextCell(m_group, m_configuration, m_cellPointers);
  m_over->RecalculateWidthsList(MAX(MC_MIN_SIZE, fontsize - 5));

  if (configuration->CheckTeXFonts())
  {
    wxDC &dc = configuration->GetDC();
    int fontsize1 = Scale_Px(fontsize * 1.5, scale);
    wxFont font(fontsize1, wxFONTFAMILY_MODERN,
                wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false,
                configuration->GetTeXCMEX());
    if (!font.IsOk())
      font = *wxNORMAL_FONT;
    font.SetPointSize(fontsize1);
    dc.SetFont(font);
    dc.GetTextExtent(wxT("\x5A"), &m_signWidth, &m_signSize);

#if defined __WXMSW__
    m_signWidth = m_signWidth / 2;
#endif
    m_signTop = m_signSize / 2;
    m_signSize = (85 * m_signSize) / 100;

    m_width = m_signWidth +
              MAX(m_over->GetFullWidth(scale) + m_signWidth, m_under->GetFullWidth(scale)) +
              m_base->GetFullWidth(scale) +
              m_var->GetFullWidth(scale) +
              Scale_Px(4, scale);
  }
  else
  {
#if defined __WXMSW__
    wxDC& dc = configuration->GetDC();
    int fontsize1 = Scale_Px(INTEGRAL_FONT_SIZE, scale);
    wxFont font(fontsize1, wxFONTFAMILY_MODERN,
                wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL,
                false,
                configuration->GetSymbolFontName());
    if(!font.IsOk())
      font = *wxNORMAL_FONT;
    font.SetPointSize(fontsize1);
    dc.SetFont(font);
    dc.GetTextExtent(INTEGRAL_TOP, &m_charWidth, &m_charHeight);

    m_width = m_signWidth +
              m_base->GetFullWidth(scale) +
              MAX(m_over->GetFullWidth(scale), m_under->GetFullWidth(scale)) +
              m_var->GetFullWidth(scale) +
              Scale_Px(4, scale);
#else
    m_width = m_signWidth +
              m_base->GetFullWidth(scale) +
              MAX(m_over->GetFullWidth(scale), m_under->GetFullWidth(scale)) +
              m_var->GetFullWidth(scale) +
              Scale_Px(4, scale);
#endif
  }
  ResetData();
}

void IntCell::RecalculateHeight(int fontsize)
{
  Configuration *configuration = (*m_configuration);
  double scale = configuration->GetScale();

  m_under->RecalculateHeightList(MAX(MC_MIN_SIZE, fontsize - 5));
  m_over->RecalculateHeightList(MAX(MC_MIN_SIZE, fontsize - 5));
  m_base->RecalculateHeightList(fontsize);
  m_var->RecalculateHeightList(fontsize);

  if (m_intStyle == INT_DEF)
  {
    if (configuration->CheckTeXFonts())
    {
      m_center = MAX(m_over->GetMaxHeight() + Scale_Px(4, scale) + m_signSize / 2 - m_signSize / 3,
                     m_base->GetMaxCenter());
      m_height = m_center +
                 MAX(m_under->GetMaxHeight() + Scale_Px(4, scale) + m_signSize / 2 - m_signSize / 3,
                     m_base->GetMaxDrop());
    }
    else
    {
      m_center = MAX(m_over->GetMaxHeight() + Scale_Px(4, scale) + m_signSize / 2 - m_signSize / 3,
                     m_base->GetMaxCenter());
      m_height = m_center +
                 MAX(m_under->GetMaxHeight() + Scale_Px(4, scale) + m_signSize / 2 - m_signSize / 3,
                     m_base->GetMaxDrop());
    }
  }
  else
  {
    m_center = MAX(m_signSize / 2, m_base->GetMaxCenter());
    m_height = m_center +
               MAX(m_signSize / 2, m_base->GetMaxDrop());
  }
}

void IntCell::Draw(wxPoint point, int fontsize)
{
  if (DrawThisCell(point) && InUpdateRegion())
  {
    MathCell::Draw(point, fontsize);
    Configuration *configuration = (*m_configuration);
    
    wxDC &dc = configuration->GetDC();
    double scale = configuration->GetScale();
    
    wxPoint base(point), under(point), over(point), var(point), sign(point);

    if (configuration->CheckTeXFonts())
    {
      SetForeground();
      int fontsize1 = Scale_Px(fontsize * 1.5, scale);
      wxFont font(fontsize1, wxFONTFAMILY_MODERN,
                  wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false,
                  configuration->GetTeXCMEX());
      if (!font.IsOk())
        font = *wxNORMAL_FONT;
      font.SetPointSize(fontsize1);
      dc.SetFont(font);
      dc.DrawText(wxT("\x5A"),
                  sign.x,
                  sign.y - m_signTop);
    }
    else
    {
#if defined __WXMSW__
      SetForeground();
      int fontsize1 = Scale_Px(INTEGRAL_FONT_SIZE, scale);
      int m_signWCenter = m_signWidth / 2;

      dc.SetFont(wxFont(fontsize1, wxFONTFAMILY_MODERN,
      wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL,
      false,
                        configuration->GetSymbolFontName()));
      dc.DrawText(INTEGRAL_TOP,
                  sign.x + m_signWCenter - m_charWidth / 2,
                  sign.y - (m_signSize + 1) / 2);
      dc.DrawText(INTEGRAL_BOTTOM,
                  sign.x + m_signWCenter - m_charWidth / 2,
                  sign.y + (m_signSize + 1) / 2 - m_charHeight);

      int top, bottom;
      top = sign.y - (m_signSize + 1) / 2 + m_charHeight / 2;
      bottom = sign.y + (m_signSize + 1) / 2 - (3 * m_charHeight) / 2;
      if (top <= bottom)
      {
        wxASSERT_MSG(m_charHeight>=2,_("Font issue: The char height is too small! Installing http://www.math.union.edu/~dpvc/jsmath/download/jsMath-fonts.html and checking \"Use JSmath fonts\" in the configuration dialogue should be a workaround."));
        if(m_charHeight <= 2)
          m_charHeight = 2;

        while (top < bottom)
        {
          dc.DrawText(INTEGRAL_EXTEND,
          point.x + m_signWCenter - m_charWidth / 2,
          top);
          top += (2*m_charHeight)/3;
        }
        dc.DrawText(INTEGRAL_EXTEND,
        point.x + m_signWCenter - m_charWidth / 2,
        sign.y + (m_signSize + 1) / 2 - (3 * m_charHeight) / 2);
      }
#else
      SetPen(1.5);
      dc.SetBrush(*wxTRANSPARENT_BRUSH);
      // top decoration
      int m_signWCenter = m_signWidth / 2;
      dc.DrawEllipticArc(sign.x + m_signWCenter - Scale_Px(0,scale),
                         sign.y - (m_signSize + 1) / 2 + Scale_Px(1.5, scale),
                         Scale_Px(12, scale),
                         Scale_Px(19, scale),90,180);
      dc.DrawEllipticArc(sign.x + m_signWCenter + Scale_Px(5, scale),
                         sign.y - (m_signSize + 1) / 2 + Scale_Px(1.5, scale),
                         Scale_Px(8, scale),
                         Scale_Px(12, scale),20,90);

      // bottom decoration
      dc.DrawEllipticArc(sign.x + m_signWCenter - Scale_Px(13, scale),
                         sign.y + (m_signSize + 1) / 2 - Scale_Px(23.5, scale),
                         Scale_Px(12, scale),
                         Scale_Px(19, scale),270,360);
      dc.DrawEllipticArc(sign.x + m_signWCenter - Scale_Px(13, scale),
                         sign.y + (m_signSize + 1) / 2 - Scale_Px(17.5, scale),
                         Scale_Px(8, scale),
                         Scale_Px(12, scale),200,270);
      // line
      dc.DrawLine(sign.x + m_signWCenter,
                  sign.y - (m_signSize + 1) / 2 + Scale_Px(12, scale),
                  sign.x + m_signWCenter,
                  sign.y + (m_signSize + 1) / 2 - Scale_Px(15, scale));
      UnsetPen();
#endif
    }

    if (m_intStyle == INT_DEF)
    {
      under.x += m_signWidth;
      under.y = point.y + m_signSize / 2 + m_under->GetMaxCenter() + Scale_Px(2, scale) -
                m_signSize / 3;
      m_under->DrawList(under, MAX(MC_MIN_SIZE, fontsize - 5));

      if (configuration->CheckTeXFonts())
        over.x += 2 * m_signWidth;
      else
        over.x += m_signWidth;

      over.y = point.y - m_signSize / 2 - m_over->GetMaxDrop() - Scale_Px(2, scale) +
               m_signSize / 3;
      m_over->DrawList(over, MAX(MC_MIN_SIZE, fontsize - 5));

      if (configuration->CheckTeXFonts())
      {
        base.x += m_signWidth +
                  MAX(m_over->GetFullWidth(scale) + m_signWidth, m_under->GetFullWidth(scale));
      }
      else
        base.x += m_signWidth +
                  MAX(m_over->GetFullWidth(scale), m_under->GetFullWidth(scale));
    }

    else if (configuration->CheckTeXFonts())
      base.x += 2 * m_signWidth;
    else
      base.x += m_signWidth;

    m_base->DrawList(base, fontsize);

    var.x = base.x + m_base->GetFullWidth(scale);
    m_var->DrawList(var, fontsize);
  }
}

wxString IntCell::ToString()
{
  wxString s = wxT("integrate(");

  s += m_base->ListToString();

  MathCell *tmp = m_var;
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

  retval = wxT("<m:nary><m:naryPr><m:chr>\x222b</m:chr></m:naryPr>");
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

  if (m_intStyle == INT_DEF)
    return wxT("<in>") + from + to + base + var + wxT("</in>");
  else
    return wxT("<in def=\"false\">") + base + var + wxT("</in>");
}

void IntCell::SelectInner(wxRect &rect, MathCell **first, MathCell **last)
{
  *first = NULL;
  *last = NULL;
  if (m_over->ContainsRect(rect))
    m_over->SelectRect(rect, first, last);
  else if (m_under->ContainsRect(rect))
    m_under->SelectRect(rect, first, last);
  else if (m_base->ContainsRect(rect))
    m_base->SelectRect(rect, first, last);
  if (*first == NULL || *last == NULL)
  {
    *first = this;
    *last = this;
  }
}
