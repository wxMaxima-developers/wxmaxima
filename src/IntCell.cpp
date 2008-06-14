///
///  Copyright (C) 2004-2007 Andrej Vodopivec <andrejv@users.sourceforge.net>
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
#elif (wxUSE_UNICODE && WXM_UNICODE_GLYPHS)
  #define INTEGRAL_TOP "\x2320"
  #define INTEGRAL_BOTTOM "\x2321"
  #define INTEGRAL_EXTEND "\x23AE"
  #define INTEGRAL_FONT_SIZE fontsize
#endif


IntCell::IntCell() : MathCell()
{
  m_base = NULL;
  m_under = NULL;
  m_over = NULL;
  m_var = NULL;
  m_signSize = 20;
  m_signWidth = 18;
  m_signMiddle = 9;
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

  m_signSize = SCALE_PX(20, scale);
  m_signWidth = SCALE_PX(18, scale);
  m_signMiddle = SCALE_PX(9, scale);

  m_base->RecalculateWidths(parser, fontsize, true);
  m_var->RecalculateWidths(parser, fontsize, true);
  if (m_under == NULL)
    m_under = new TextCell;
  m_under->RecalculateWidths(parser, MAX(8, fontsize - 5), true);
  if (m_over == NULL)
    m_over = new TextCell;
  m_over->RecalculateWidths(parser, MAX(8, fontsize - 5), true);

  m_signMiddle = MAX(m_signMiddle,
                     m_under->GetFullWidth(scale) / 2 + SCALE_PX(5, scale));

#if defined __WXMSW__ || (wxUSE_UNICODE && WXM_UNICODE_GLYPHS)
  wxDC& dc = parser.GetDC();
  int fontsize1 = (int) ((INTEGRAL_FONT_SIZE * scale + 0.5));
  dc.SetFont(wxFont(fontsize1, wxMODERN,
                    false, false, false,
                    parser.GetSymbolFontName()));
  dc.GetTextExtent(wxT(INTEGRAL_TOP), &m_charWidth, &m_charHeight);
#endif
  int signOver = MAX(SCALE_PX(10, scale),
                     m_over->GetFullWidth(scale) / 2 + SCALE_PX(5, scale));
  m_signWidth = m_signMiddle + signOver;
  m_width = m_signWidth + m_base->GetFullWidth(scale) +
            m_var->GetFullWidth(scale) - SCALE_PX(2, scale);

  MathCell::RecalculateWidths(parser, fontsize, all);
}

void IntCell::RecalculateSize(CellParser& parser, int fontsize, bool all)
{
  double scale = parser.GetScale();

  m_under->RecalculateSize(parser, MAX(8, fontsize - 5), true);
  m_over->RecalculateSize(parser, MAX(8, fontsize - 5), true);
  m_base->RecalculateSize(parser, fontsize, true);
  m_var->RecalculateSize(parser, fontsize, true);

  m_signSize = MAX(m_signSize, m_base->GetMaxHeight()); // + SCALE_PX(30, scale));
  m_signSize = MAX(m_signSize, m_var->GetMaxHeight()); // + SCALE_PX(30, scale));
  if (m_intStyle == INT_DEF)
  {
    m_height = m_signSize + m_under->GetMaxHeight() + m_over->GetMaxHeight();
    m_center = MAX((m_signSize + 1) / 2,
                   m_base->GetMaxCenter()) + // SCALE_PX(15, scale)) +
               m_over->GetMaxHeight();
  }
  else
  {
    m_height = m_signSize + SCALE_PX(2, scale);
    m_center = MAX((m_signSize + 1) / 2,
                   m_base->GetMaxCenter()) + // SCALE_PX(15, scale)) +
               SCALE_PX(1, scale);
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

    if (m_intStyle == INT_DEF)
    {
      under.x += m_signMiddle - m_under->GetFullWidth(scale) / 2 -
                 SCALE_PX(5, scale);
      under.y = point.y + (m_height - m_center) - m_under->GetMaxDrop();
      m_under->Draw(parser, under, MAX(8, fontsize - 5), true);

      over.x += m_signMiddle - m_over->GetFullWidth(scale) / 2 +
                SCALE_PX(5, scale);
      over.y = point.y - m_center + m_over->GetMaxCenter();
      m_over->Draw(parser, over, MAX(8, fontsize - 5), true);
    }

    sign.y = sign.y - m_center + (m_signSize + 1) / 2;
    if (m_intStyle == INT_DEF)
      sign.y += m_over->GetMaxHeight();
    else
      sign.y += SCALE_PX(3, scale);

#if defined __WXMSW__ || (wxUSE_UNICODE && WXM_UNICODE_GLYPHS)
    SetForeground(parser);
    int fontsize1 = (int) ((INTEGRAL_FONT_SIZE * scale + 0.5));
    dc.SetFont(wxFont(fontsize1, wxMODERN,
               false, false, false,
               parser.GetSymbolFontName()));
    dc.DrawText(wxT(INTEGRAL_TOP),
                sign.x + m_signMiddle - m_charWidth / 2,
                sign.y - (m_signSize + 1) / 2);
    dc.DrawText(wxT(INTEGRAL_BOTTOM),
                sign.x + m_signMiddle - m_charWidth / 2,
                sign.y + (m_signSize + 1) / 2 - m_charHeight);
    int top, bottom;
    top = sign.y - (m_signSize + 1) / 2 + m_charHeight / 2;
    bottom = sign.y + (m_signSize + 1) / 2 - (3 * m_charHeight) / 2;
    if (top <= bottom)
    {
      while (top < bottom)
      {
        dc.DrawText(wxT(INTEGRAL_EXTEND),
                      point.x + m_signMiddle - m_charWidth / 2,
                      top);
        top += (2*m_charHeight)/3;
      }
      dc.DrawText(wxT(INTEGRAL_EXTEND),
                      point.x + m_signMiddle - m_charWidth / 2,
                      sign.y + (m_signSize + 1) / 2 - (3 * m_charHeight) / 2);
    }
#else
    SetPen(parser);
    // top decoration
    dc.DrawLine(sign.x + m_signMiddle,
                sign.y - (m_signSize + 1) / 2 + SCALE_PX(12, scale) - 1,
                sign.x + m_signMiddle + SCALE_PX(3, scale),
                sign.y - (m_signSize + 1) / 2 + SCALE_PX(3, scale));
    dc.DrawLine(sign.x + m_signMiddle + SCALE_PX(3, scale),
                sign.y - (m_signSize + 1) / 2 + SCALE_PX(3, scale),
                sign.x + m_signMiddle + SCALE_PX(6, scale),
                sign.y - (m_signSize + 1) / 2);
    dc.DrawLine(sign.x + m_signMiddle + SCALE_PX(6, scale),
                sign.y - (m_signSize + 1) / 2,
                sign.x + m_signMiddle + SCALE_PX(9, scale),
                sign.y - (m_signSize + 1) / 2 + SCALE_PX(3, scale));
    // bottom decoration
    dc.DrawLine(sign.x + m_signMiddle,
                sign.y + (m_signSize + 1) / 2 - SCALE_PX(12, scale) + 1,
                sign.x + m_signMiddle - SCALE_PX(3, scale),
                sign.y + (m_signSize + 1) / 2 - SCALE_PX(3, scale));
    dc.DrawLine(sign.x + m_signMiddle - SCALE_PX(3, scale),
                sign.y + (m_signSize + 1) / 2 - SCALE_PX(3, scale),
                sign.x + m_signMiddle - SCALE_PX(6, scale),
                sign.y + (m_signSize + 1) / 2);
    dc.DrawLine(sign.x + m_signMiddle - SCALE_PX(6, scale),
                sign.y + (m_signSize + 1) / 2,
                sign.x + m_signMiddle - SCALE_PX(9, scale),
                sign.y + (m_signSize + 1) / 2 - SCALE_PX(3, scale));
    // line
    dc.DrawLine(sign.x + m_signMiddle,
                sign.y - (m_signSize + 1) / 2 + SCALE_PX(12, scale) - 1,
                sign.x + m_signMiddle,
                sign.y + (m_signSize + 1) / 2 - SCALE_PX(12, scale) + 1);
    UnsetPen(parser);
#endif
    base.x += m_signWidth - SCALE_PX(6, scale);
    m_base->Draw(parser, base, fontsize, true);

    var.x += m_signWidth + m_base->GetFullWidth(scale) - SCALE_PX(2, scale);
    var.y += m_var->GetMaxCenter() - m_var->GetMaxHeight() / 2;
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
