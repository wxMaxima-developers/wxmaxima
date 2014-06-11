///
///  Copyright (C) 2004-2011 Andrej Vodopivec <andrej.vodopivec@gmail.com>
///
///  This program is free software; you can redistribute it and/or modify
///  it under the terms of the GNU General Public License as published by
///  the Free Software Foundation; either version 2 of the License, or
///  (at your option) any later version.
///
///  This program is distributed in the hope that it will be useful,
///  but WITHOUT ANY WARRANTY; without even the implied warranty of
///  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
///  GNU General Public License for more details.
///
///
///  You should have received a copy of the GNU General Public License
///  along with this program; if not, write to the Free Software
///  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
///

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

void IntCell::SetParent(MathCell *parent, bool all)
{
  if (m_base != NULL)
    m_base->SetParent(parent, true);
  if (m_under != NULL)
    m_under->SetParent(parent, true);
  if (m_over != NULL)
    m_over->SetParent(parent, true);
  if (m_var != NULL)
    m_var->SetParent(parent, true);

  MathCell::SetParent(parent, all);
}

MathCell* IntCell::Copy(bool all)
{
  IntCell *tmp = new IntCell;
  CopyData(this, tmp);
  tmp->SetBase(m_base->Copy(true));
  tmp->SetUnder(m_under->Copy(true));
  tmp->SetOver(m_over->Copy(true));
  tmp->SetVar(m_var->Copy(true));
  tmp->m_intStyle = m_intStyle;
  if (all && m_next != NULL)
    tmp->AppendCell(m_next->Copy(all));
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

void IntCell::RecalculateWidths(CellParser& parser, int fontsize, bool all)
{
  double scale = parser.GetScale();

  m_signSize = SCALE_PX(50, scale);
  m_signWidth = SCALE_PX(18, scale);

  m_base->RecalculateWidths(parser, fontsize, true);
  m_var->RecalculateWidths(parser, fontsize, true);
  if (m_under == NULL)
    m_under = new TextCell;
  m_under->RecalculateWidths(parser, MAX(MC_MIN_SIZE, fontsize - 5), true);
  if (m_over == NULL)
    m_over = new TextCell;
  m_over->RecalculateWidths(parser, MAX(MC_MIN_SIZE, fontsize - 5), true);

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
                      false, false, false,
                      parser.GetSymbolFontName()));
    dc.GetTextExtent(wxT(INTEGRAL_TOP), &m_charWidth, &m_charHeight);

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


  MathCell::RecalculateWidths(parser, fontsize, all);
}

void IntCell::RecalculateSize(CellParser& parser, int fontsize, bool all)
{
  double scale = parser.GetScale();

  m_under->RecalculateSize(parser, MAX(MC_MIN_SIZE, fontsize - 5), true);
  m_over->RecalculateSize(parser, MAX(MC_MIN_SIZE, fontsize - 5), true);
  m_base->RecalculateSize(parser, fontsize, true);
  m_var->RecalculateSize(parser, fontsize, true);

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

  MathCell::RecalculateSize(parser, fontsize, all);
}

void IntCell::Draw(CellParser& parser, wxPoint point, int fontsize, bool all)
{
  if (DrawThisCell(parser, point))
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
                        false, false, false,
                        parser.GetSymbolFontName()));
      dc.DrawText(wxT(INTEGRAL_TOP),
                  sign.x + m_signWCenter - m_charWidth / 2,
                  sign.y - (m_signSize + 1) / 2);
      dc.DrawText(wxT(INTEGRAL_BOTTOM),
                  sign.x + m_signWCenter - m_charWidth / 2,
                  sign.y + (m_signSize + 1) / 2 - m_charHeight);

      int top, bottom;
      top = sign.y - (m_signSize + 1) / 2 + m_charHeight / 2;
      bottom = sign.y + (m_signSize + 1) / 2 - (3 * m_charHeight) / 2;
      if (top <= bottom)
      {
        while (top < bottom)
        {
          dc.DrawText(wxT(INTEGRAL_EXTEND),
                        point.x + m_signWCenter - m_charWidth / 2,
                        top);
          top += (2*m_charHeight)/3;
        }
        dc.DrawText(wxT(INTEGRAL_EXTEND),
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
      m_under->Draw(parser, under, MAX(MC_MIN_SIZE, fontsize - 5), true);

      if (parser.CheckTeXFonts())
        over.x += 2*m_signWidth;
      else
        over.x += m_signWidth;

      over.y = point.y - m_signSize / 2 - m_over->GetMaxDrop() - SCALE_PX(2, scale) +
               m_signSize / 3;
      m_over->Draw(parser, over, MAX(MC_MIN_SIZE, fontsize - 5), true);

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

    m_base->Draw(parser, base, fontsize, true);

    var.x = base.x + m_base->GetFullWidth(scale);
    m_var->Draw(parser, var, fontsize, true);
  }

  MathCell::Draw(parser, point, fontsize, all);
}

wxString IntCell::ToString(bool all)
{
  wxString s = wxT("integrate(");

  s += m_base->ToString(true);

  MathCell* tmp = m_var;
  wxString var;
  tmp = tmp->m_next;
  if (tmp != NULL)
  {
    var = tmp->ToString(true);
  }

  wxString to = m_over->ToString(true);
  wxString from = m_under->ToString(true);

  s += wxT(",") + var;
  if (m_intStyle == INT_DEF)
    s += wxT(",") + from + wxT(",") + to;

  s += wxT(")");
  s += MathCell::ToString(all);
  return s;
}

wxString IntCell::ToTeX(bool all)
{
  wxString s = wxT("\\int");

  wxString to = m_over->ToTeX(true);
  wxString from = m_under->ToTeX(true);

  if (m_intStyle == INT_DEF)
    s += wxT("_{") + from + wxT("}^{") + to + wxT("}");
  else
    s += wxT(" ");

  s += m_base->ToTeX(true);
  s += m_var->ToTeX(true);

  s += MathCell::ToTeX(all);
  return s;
}

wxString IntCell::ToXML(bool all)
{
  MathCell* tmp = m_base;
  wxString base = _T("<r>") + tmp->ToXML(true) + _T("</r>");

  tmp = m_var;
  wxString var = ( tmp == NULL )? wxEmptyString : _T("<r>");
  var += tmp->ToXML(true);
  var += ( var == wxEmptyString )? wxEmptyString : _T("</r>");

  tmp = m_under;
  wxString from = _T("<r>") + tmp->ToXML(true) + _T("</r>");

  tmp = m_over;
  wxString to = _T("<r>") + tmp->ToXML(true) + _T("</r>");

  if (m_intStyle == INT_DEF)
    return wxT("<in>") + from + to + base + var + wxT("</in>") + MathCell::ToXML(all);
  else
    return wxT("<in def=\"false\">") + base + var  + wxT("</in>") +
      MathCell::ToXML(all);
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
