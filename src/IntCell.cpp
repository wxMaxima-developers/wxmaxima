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

#include "IntCell.h"
#include "TextCell.h"

#if defined __WXMSW__
  #define INTEGRAL_TOP "\xF3"
  #define INTEGRAL_BOTTOM "\xF5"
  #define INTEGRAL_EXTEND "\xF4"
  #define INTEGRAL_FONT_SIZE 12
#endif

IntCell::IntCell() : MathCell()
{
  m_base = NULL;
  m_under = NULL;
  m_over = NULL;
  m_var = NULL;
  m_signSize = 50;
  m_signWidth = 18;
  m_intStyle = INT_IDEF;
}

IntCell::~IntCell()
{
  if (m_base != NULL)
    delete m_base;
  if (m_under != NULL)
    delete m_under;
  if (m_over != NULL)
    delete m_over;
  if (m_var != NULL)
    delete m_var;
  if (m_next != NULL)
    delete m_next;
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

MathCell* IntCell::Copy()
{
  IntCell *tmp = new IntCell;
  CopyData(this, tmp);
  tmp->SetBase(m_base->CopyList());
  tmp->SetUnder(m_under->CopyList());
  tmp->SetOver(m_over->CopyList());
  tmp->SetVar(m_var->CopyList());
  tmp->m_intStyle = m_intStyle;

  return tmp;
}

void IntCell::Destroy()
{
  if (m_base != NULL)
    delete m_base;
  if (m_under != NULL)
    delete m_under;
  if (m_over != NULL)
    delete m_over;
  if (m_var != NULL)
    delete m_var;
  m_base = NULL;
  m_under = NULL;
  m_over = NULL;
  m_var = NULL;
  m_next = NULL;
}

void IntCell::SetOver(MathCell* over)
{
  if (over == NULL)
    return ;
  if (m_over != NULL)
    delete m_over;
  m_over = over;
}

void IntCell::SetBase(MathCell* base)
{
  if (base == NULL)
    return ;
  if (m_base != NULL)
    delete m_base;
  m_base = base;
}

void IntCell::SetUnder(MathCell *under)
{
  if (under == NULL)
    return ;
  if (m_under != NULL)
    delete m_under;
  m_under = under;
}

void IntCell::SetVar(MathCell *var)
{
  if (var == NULL)
    return ;
  if (m_var != NULL)
    delete m_var;
  m_var = var;
}

void IntCell::RecalculateWidths(CellParser& parser, int fontsize)
{
  double scale = parser.GetScale();

  m_signSize = SCALE_PX(50, scale);
  m_signWidth = SCALE_PX(18, scale);

  m_base->RecalculateWidthsList(parser, fontsize);
  m_var->RecalculateWidthsList(parser, fontsize);
  if (m_under == NULL)
    m_under = new TextCell;
  m_under->RecalculateWidthsList(parser, MAX(MC_MIN_SIZE, fontsize - 5));
  if (m_over == NULL)
    m_over = new TextCell;
  m_over->RecalculateWidthsList(parser, MAX(MC_MIN_SIZE, fontsize - 5));

  if (parser.CheckTeXFonts()) {
    wxDC& dc = parser.GetDC();
    int fontsize1 = (int) ((fontsize * scale * 1.5 + 0.5));
    dc.SetFont( wxFont(fontsize1, wxFONTFAMILY_MODERN,
		       wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false,
		       parser.GetTeXCMEX()));
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
              SCALE_PX(4, scale);
  }
  else {
#if defined __WXMSW__
    wxDC& dc = parser.GetDC();
    int fontsize1 = (int) ((INTEGRAL_FONT_SIZE * scale + 0.5));
    dc.SetFont(wxFont(fontsize1, wxFONTFAMILY_MODERN,
		      wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL,
		      false,
                      parser.GetSymbolFontName()));
    dc.GetTextExtent(INTEGRAL_TOP, &m_charWidth, &m_charHeight);

    m_width = m_signWidth +
              m_base->GetFullWidth(scale) +
              MAX(m_over->GetFullWidth(scale), m_under->GetFullWidth(scale)) +
              m_var->GetFullWidth(scale) +
              SCALE_PX(4, scale);
#else
    m_width = m_signWidth +
              m_base->GetFullWidth(scale) +
              MAX(m_over->GetFullWidth(scale), m_under->GetFullWidth(scale)) +
              m_var->GetFullWidth(scale) +
              SCALE_PX(4, scale);
#endif
  }
  ResetData();
}

void IntCell::RecalculateSize(CellParser& parser, int fontsize)
{
  double scale = parser.GetScale();

  m_under->RecalculateSizeList(parser, MAX(MC_MIN_SIZE, fontsize - 5));
  m_over->RecalculateSizeList(parser, MAX(MC_MIN_SIZE, fontsize - 5));
  m_base->RecalculateSizeList(parser, fontsize);
  m_var->RecalculateSizeList(parser, fontsize);

  if (m_intStyle == INT_DEF)
  {
    if (parser.CheckTeXFonts())
    {
      m_center = MAX(m_over->GetMaxHeight() + SCALE_PX(4, scale) + m_signSize / 2 - m_signSize / 3,
                     m_base->GetMaxCenter());
      m_height = m_center +
                 MAX(m_under->GetMaxHeight() + SCALE_PX(4, scale) + m_signSize / 2 - m_signSize / 3,
                     m_base->GetMaxDrop());
    }
    else
    {
      m_center = MAX(m_over->GetMaxHeight() + SCALE_PX(4, scale) + m_signSize / 2 - m_signSize / 3,
                     m_base->GetMaxCenter());
      m_height = m_center +
                 MAX(m_under->GetMaxHeight() + SCALE_PX(4, scale) + m_signSize / 2 - m_signSize / 3,
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

void IntCell::Draw(CellParser& parser, wxPoint point, int fontsize)
{
  MathCell::Draw(parser, point, fontsize);

  if (DrawThisCell(parser, point) && InUpdateRegion())
  {
    wxDC& dc = parser.GetDC();
    double scale = parser.GetScale();

    wxPoint base(point), under(point), over(point), var(point), sign(point);

    if (parser.CheckTeXFonts())
    {
      SetForeground(parser);
      int fontsize1 = (int) ((fontsize * scale * 1.5 + 0.5));
      dc.SetFont(wxFont(fontsize1, wxFONTFAMILY_MODERN,
			wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false,
                        parser.GetTeXCMEX()));
      dc.DrawText(wxT("\x5A"),
                  sign.x,
                  sign.y - m_signTop);
    }
    else
    {
#if defined __WXMSW__
      SetForeground(parser);
      int fontsize1 = (int) ((INTEGRAL_FONT_SIZE * scale + 0.5));
      int m_signWCenter = m_signWidth / 2;

      dc.SetFont(wxFont(fontsize1, wxFONTFAMILY_MODERN,
			wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL,
			false,
                        parser.GetSymbolFontName()));
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
      SetPen(parser);
      // top decoration
      int m_signWCenter = m_signWidth / 2;
      dc.DrawLine(sign.x + m_signWCenter,
                  sign.y - (m_signSize + 1) / 2 + SCALE_PX(12, scale) - 1,
                  sign.x + m_signWCenter + SCALE_PX(3, scale),
                  sign.y - (m_signSize + 1) / 2 + SCALE_PX(3, scale));
      dc.DrawLine(sign.x + m_signWCenter + SCALE_PX(3, scale),
                  sign.y - (m_signSize + 1) / 2 + SCALE_PX(3, scale),
                  sign.x + m_signWCenter + SCALE_PX(6, scale),
                  sign.y - (m_signSize + 1) / 2);
      dc.DrawLine(sign.x + m_signWCenter + SCALE_PX(6, scale),
                  sign.y - (m_signSize + 1) / 2,
                  sign.x + m_signWCenter + SCALE_PX(9, scale),
                  sign.y - (m_signSize + 1) / 2 + SCALE_PX(3, scale));
      // bottom decoration
      dc.DrawLine(sign.x + m_signWCenter,
                  sign.y + (m_signSize + 1) / 2 - SCALE_PX(12, scale) + 1,
                  sign.x + m_signWCenter - SCALE_PX(3, scale),
                  sign.y + (m_signSize + 1) / 2 - SCALE_PX(3, scale));
      dc.DrawLine(sign.x + m_signWCenter - SCALE_PX(3, scale),
                  sign.y + (m_signSize + 1) / 2 - SCALE_PX(3, scale),
                  sign.x + m_signWCenter - SCALE_PX(6, scale),
                  sign.y + (m_signSize + 1) / 2);
      dc.DrawLine(sign.x + m_signWCenter - SCALE_PX(6, scale),
                  sign.y + (m_signSize + 1) / 2,
                  sign.x + m_signWCenter - SCALE_PX(9, scale),
                  sign.y + (m_signSize + 1) / 2 - SCALE_PX(3, scale));
      // line
      dc.DrawLine(sign.x + m_signWCenter,
                  sign.y - (m_signSize + 1) / 2 + SCALE_PX(12, scale) - 1,
                  sign.x + m_signWCenter,
                  sign.y + (m_signSize + 1) / 2 - SCALE_PX(12, scale) + 1);
      UnsetPen(parser);
#endif
    }

    if (m_intStyle == INT_DEF)
    {
      under.x += m_signWidth;
      under.y = point.y + m_signSize / 2 + m_under->GetMaxCenter() + SCALE_PX(2, scale) -
                m_signSize / 3;
      m_under->DrawList(parser, under, MAX(MC_MIN_SIZE, fontsize - 5));

      if (parser.CheckTeXFonts())
        over.x += 2*m_signWidth;
      else
        over.x += m_signWidth;

      over.y = point.y - m_signSize / 2 - m_over->GetMaxDrop() - SCALE_PX(2, scale) +
               m_signSize / 3;
      m_over->DrawList(parser, over, MAX(MC_MIN_SIZE, fontsize - 5));

      if (parser.CheckTeXFonts())
      {
        base.x += m_signWidth +
                  MAX(m_over->GetFullWidth(scale) + m_signWidth, m_under->GetFullWidth(scale));
      }
      else
        base.x += m_signWidth +
                  MAX(m_over->GetFullWidth(scale), m_under->GetFullWidth(scale));
    }

    else if (parser.CheckTeXFonts())
      base.x += 2*m_signWidth;
    else
      base.x += m_signWidth;

    m_base->DrawList(parser, base, fontsize);

    var.x = base.x + m_base->GetFullWidth(scale);
    m_var->DrawList(parser, var, fontsize);
  }
}

wxString IntCell::ToString()
{
  wxString s = wxT("integrate(");

  s += m_base->ListToString();

  MathCell* tmp = m_var;
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
  if(m_var) var = m_var->ListToMathML();
  
  wxString from;
  if(m_under) from = m_under->ListToMathML();
  
  wxString to;
  if(m_over) to = m_over->ListToMathML();

  wxString retval;
  if(from.IsEmpty() && to.IsEmpty())
    retval = wxT("<mo>&#x222B;</mo>") + base;
  if(from.IsEmpty() && !to.IsEmpty())
    retval = wxT("<mover><mo>&#x222B;</mo>") + to + wxT("</mover>") + base;
  if(!from.IsEmpty() && to.IsEmpty())
    retval = wxT("<munder><mo>&#x222B;</mo>") + from + wxT("</munder>") + base;
  if(!from.IsEmpty() && !to.IsEmpty())
    retval = wxT("<munderover><mo>&#x222B;</mo>") + from + to + wxT("</munderover>\n")+ base;
  if(!var.IsEmpty())
    retval = retval + var;

  return(wxT("<mrow>") + retval + wxT("</mrow>"));
}

wxString IntCell::ToOMML()
{
  wxString base = m_base->ListToMathML();

  wxString var;
  if(m_var) var = m_var->ListToMathML();
  
  wxString from;
  if(m_under) from = m_under->ListToMathML();
  
  wxString to;
  if(m_over) to = m_over->ListToMathML();

  wxString retval;
  if(from.IsEmpty() && to.IsEmpty())
    retval = wxT("<mo>&#x222B;</mo>") + base;
  if(from.IsEmpty() && !to.IsEmpty())
    retval = wxT("<mover><mo>&#x222B;</mo>") + to + wxT("</mover>") + base;
  if(!from.IsEmpty() && to.IsEmpty())
    retval = wxT("<munder><mo>&#x222B;</mo>") + from + wxT("</munder>") + base;
  if(!from.IsEmpty() && !to.IsEmpty())
    retval = wxT("<munderover><mo>&#x222B;</mo>") + from + to + wxT("</munderover>\n")+ base;
  if(!var.IsEmpty())
    retval = retval + var;

  return(wxT("<mrow>") + retval + wxT("</mrow>"));
}

wxString IntCell::ToXML()
{
  wxString from;
  if(m_under != NULL)
    from = m_under->ListToXML();
  from = wxT("<r>")+from+wxT("</r>");

  wxString to;
  if(m_over != NULL)
    to = m_over->ListToXML();
  to = wxT("<r>")+to+wxT("</r>");

  wxString base;
  if(m_base != NULL)
    base = m_base->ListToXML();
  base = wxT("<r>")+base+wxT("</r>");

  wxString var;
  if(m_var != NULL)
    var = m_var->ListToXML();
  var = wxT("<r>")+var+wxT("</r>");

  if (m_intStyle == INT_DEF)
    return wxT("<in>") + from + to + base + var + wxT("</in>");
  else
    return wxT("<in def=\"false\">") + base + var  + wxT("</in>");
}

void IntCell::SelectInner(wxRect& rect, MathCell** first, MathCell** last)
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
