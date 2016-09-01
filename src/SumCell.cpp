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
  This file defines the class SumCell

  SumCell is the MathCell type that represents maxima's <code>sum()</code>, 
  <code>lsum</code> and <code>product()</code> 
  commands.
*/

#include "SumCell.h"
#include "TextCell.h"

#define SUM_SIGN "\x58"
#define PROD_SIGN "\x59"
#define SUM_DEC 2

SumCell::SumCell() : MathCell()
{
  m_base = NULL;
  m_under = NULL;
  m_over = NULL;
  m_signSize = 50;
  m_signWidth = 30;
  m_signWCenter = 15;
  m_sumStyle = SM_SUM;
}

SumCell::~SumCell()
{
  if (m_base != NULL)
    delete m_base;
  if (m_under != NULL)
    delete m_under;
  if (m_over != NULL)
    delete m_over;
  if (m_next != NULL)
    delete m_next;
}

void SumCell::SetParent(MathCell *parent)
{
  m_group=parent;
  if (m_base != NULL)
    m_base->SetParentList(parent);
  if (m_under != NULL)
    m_under->SetParentList(parent);
  if (m_over != NULL)
    m_over->SetParentList(parent);
}

MathCell* SumCell::Copy()
{
  SumCell *tmp = new SumCell;
  CopyData(this, tmp);
  tmp->SetBase(m_base->CopyList());
  tmp->SetUnder(m_under->CopyList());
  tmp->SetOver(m_over->CopyList());
  tmp->m_sumStyle = m_sumStyle;
  
  return tmp;
}

void SumCell::Destroy()
{
  if (m_base != NULL)
    delete m_base;
  if (m_under != NULL)
    delete m_under;
  if (m_over != NULL)
    delete m_over;
  m_next = NULL;
  m_base = NULL;
  m_under = NULL;
  m_over = NULL;
}

void SumCell::SetOver(MathCell* over)
{
  if (over == NULL)
    return ;
  if (m_over != NULL)
    delete m_over;
  m_over = over;
}

void SumCell::SetBase(MathCell* base)
{
  if (base == NULL)
    return ;
  if (m_base != NULL)
    delete m_base;
  m_base = base;
}

void SumCell::SetUnder(MathCell *under)
{
  if (under == NULL)
    return ;
  if (m_under != NULL)
    delete m_under;
  m_under = under;
}

void SumCell::RecalculateWidths(CellParser& parser, int fontsize)
{
  double scale = parser.GetScale();

  m_signSize = SCALE_PX(50, scale);
  m_signWidth = SCALE_PX(30, scale);
  m_signWCenter = SCALE_PX(15, scale);

  m_base->RecalculateWidthsList(parser, fontsize);
  m_under->RecalculateWidthsList(parser, MAX(MC_MIN_SIZE, fontsize - SUM_DEC));
  if (m_over == NULL)
    m_over = new TextCell;
  m_over->RecalculateWidthsList(parser, MAX(MC_MIN_SIZE, fontsize - SUM_DEC));

  if (parser.CheckTeXFonts())
  {
    wxDC& dc = parser.GetDC();
    int fontsize1 = (int) ((fontsize * 1.5 * scale + 0.5));
    dc.SetFont(wxFont(fontsize1, wxFONTFAMILY_MODERN,
    		          wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false,
                      parser.GetTeXCMEX()));
    dc.GetTextExtent(m_sumStyle == SM_SUM ? wxT(SUM_SIGN) : wxT(PROD_SIGN), &m_signWidth, &m_signSize);
    m_signWCenter = m_signWidth / 2;
    m_signTop = (2* m_signSize) / 5;
    m_signSize = (2 * m_signSize) / 5;
  }
  m_signWCenter = MAX(m_signWCenter, m_under->GetFullWidth(scale) / 2);
  m_signWCenter = MAX(m_signWCenter, m_over->GetFullWidth(scale) / 2);
  m_width = 2 * m_signWCenter + m_base->GetFullWidth(scale) + SCALE_PX(4, scale);

  ResetData();
}

void SumCell::RecalculateSize(CellParser& parser, int fontsize)
{
  double scale = parser.GetScale();

  m_under->RecalculateSizeList(parser, MAX(MC_MIN_SIZE, fontsize - SUM_DEC));
  m_over->RecalculateSizeList(parser, MAX(MC_MIN_SIZE, fontsize - SUM_DEC));
  m_base->RecalculateSizeList(parser, fontsize);

  m_center = MAX(m_over->GetMaxHeight() + SCALE_PX(4, scale) + m_signSize / 2,
                 m_base->GetMaxCenter());
  m_height = m_center +
             MAX(m_under->GetMaxHeight() + SCALE_PX(4, scale) + m_signSize / 2,
                 m_base->GetMaxDrop());
}

void SumCell::Draw(CellParser& parser, wxPoint point, int fontsize)
{
  if (DrawThisCell(parser, point))
  {
    wxDC& dc = parser.GetDC();
    double scale = parser.GetScale();

    wxPoint base(point), under(point), over(point), sign(point);

    under.x += m_signWCenter - m_under->GetFullWidth(scale) / 2;
    under.y = point.y + m_signSize / 2 + m_under->GetMaxCenter() + SCALE_PX(2, scale);
    m_under->DrawList(parser, under, MAX(MC_MIN_SIZE, fontsize - SUM_DEC));

    over.x += m_signWCenter - m_over->GetFullWidth(scale) / 2;
    over.y = point.y - m_signSize / 2 - m_over->GetMaxDrop() - SCALE_PX(2, scale);
    m_over->DrawList(parser, over, MAX(MC_MIN_SIZE, fontsize - SUM_DEC));

    if (parser.CheckTeXFonts())
    {
      SetForeground(parser);
      int fontsize1 = (int) ((fontsize * 1.5 * scale + 0.5));
      dc.SetFont(wxFont(fontsize1, wxFONTFAMILY_MODERN,
    		            wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false,
                        parser.GetTeXCMEX()));
      dc.DrawText(m_sumStyle == SM_SUM ? wxT(SUM_SIGN) : wxT(PROD_SIGN),
                  sign.x + m_signWCenter - m_signWidth / 2,
                  sign.y - m_signTop);
    }
    else
    {
      SetPen(parser);
      if (m_sumStyle == SM_SUM)
      {
        //DRAW SUM SIGN
        // Upper part
        dc.DrawLine(point.x + m_signWCenter + m_signWidth / 6,
                    point.y,
                    point.x + m_signWCenter - m_signWidth / 2,
                    point.y - m_signSize / 2 + 1);
        dc.DrawLine(point.x + m_signWCenter - m_signWidth / 2,
                    point.y - m_signSize / 2,
                    point.x + m_signWCenter + m_signWidth / 2,
                    point.y - m_signSize / 2);
        dc.DrawLine(point.x + m_signWCenter - m_signWidth / 2,
                    point.y - m_signSize / 2 + 1,
                    point.x + m_signWCenter + m_signWidth / 2,
                    point.y - m_signSize / 2 + 1);
        dc.DrawLine(point.x + m_signWCenter + m_signWidth / 2,
                    point.y - m_signSize / 2,
                    point.x + m_signWCenter + m_signWidth / 2,
                    point.y - m_signSize / 2 + SCALE_PX(5, scale));
        // Lower part
        dc.DrawLine(point.x + m_signWCenter + m_signWidth / 6,
                    point.y,
                    point.x + m_signWCenter - m_signWidth / 2,
                    point.y + m_signSize / 2 - 1);
        dc.DrawLine(point.x + m_signWCenter - m_signWidth / 2,
                    point.y + m_signSize / 2,
                    point.x + m_signWCenter + m_signWidth / 2,
                    point.y + m_signSize / 2);
        dc.DrawLine(point.x + m_signWCenter - m_signWidth / 2,
                    point.y + m_signSize / 2 - 1,
                    point.x + m_signWCenter + m_signWidth / 2,
                    point.y + m_signSize / 2 - 1);
        dc.DrawLine(point.x + m_signWCenter + m_signWidth / 2,
                    point.y + m_signSize / 2,
                    point.x + m_signWCenter + m_signWidth / 2,
                    point.y + m_signSize / 2 - SCALE_PX(5, scale));
      }
      else
      {
        // DRAW PRODUCT SIGN
        // Vertical lines
        dc.DrawLine(point.x + m_signWCenter + m_signWidth / 6,
                    point.y + m_signSize / 2,
                    point.x + m_signWCenter + m_signWidth / 6,
                    point.y - m_signSize / 2 + SCALE_PX(4, scale));
        dc.DrawLine(point.x + m_signWCenter - m_signWidth / 6,
                    point.y + m_signSize / 2,
                    point.x + m_signWCenter - m_signWidth / 6,
                    point.y - m_signSize / 2 + SCALE_PX(4, scale));
        // Horizonral line (double)
        dc.DrawLine(point.x + m_signWCenter - m_signWidth / 2,
                    point.y - m_signSize / 2,
                    point.x + m_signWCenter + m_signWidth / 2,
                    point.y - m_signSize / 2);
        dc.DrawLine(point.x + m_signWCenter - m_signWidth / 2,
                    point.y - m_signSize / 2 + 1,
                    point.x + m_signWCenter + m_signWidth / 2,
                    point.y - m_signSize / 2 + 1);
        // Ticks on horizontal line
        dc.DrawLine(point.x + m_signWCenter - m_signWidth / 2,
                    point.y - m_signSize / 2,
                    point.x + m_signWCenter - m_signWidth / 2,
                    point.y - m_signSize / 2 + SCALE_PX(5, scale));
        dc.DrawLine(point.x + m_signWCenter + m_signWidth / 2,
                    point.y - m_signSize / 2,
                    point.x + m_signWCenter + m_signWidth / 2,
                    point.y - m_signSize / 2 + SCALE_PX(5, scale));
      }
      UnsetPen(parser);
    }
    base.x += (2 * m_signWCenter + SCALE_PX(4, scale));
    m_base->DrawList(parser, base, fontsize);
  }

  MathCell::Draw(parser, point, fontsize);
}

wxString SumCell::ToString()
{
  wxString s;
  if (m_sumStyle == SM_SUM)
    s = wxT("sum(");
  else
    s = wxT("product(");
  s += m_base->ListToString();

  MathCell* tmp = m_under;
  wxString var = tmp->ToString();
  wxString from;
  tmp = tmp->m_next;
  if (tmp != NULL)
  {
    tmp = tmp->m_next;
    if (tmp != NULL)
      from = tmp->ListToString();
  }
  wxString to = m_over->ListToString();
  s += wxT(",") + var + wxT(",") + from;
  if (to != wxEmptyString)
    s += wxT(",") + to + wxT(")");
  else
    s = wxT("l") + s + wxT(")");
  return s;
}

wxString SumCell::ToTeX()
{
  wxString s;
  if (m_sumStyle == SM_SUM)
    s = wxT("\\sum");
  else
    s = wxT("\\prod");


  s += wxT("_{") + m_under->ListToTeX() + wxT("}");
  wxString to = m_over->ListToTeX();
  if (to.Length())
    s += wxT("^{") + to + wxT("}");


  s += wxT("{\\left. ");
  s += m_base->ListToTeX();
  s += wxT("\\right.}");
  return s;
}

wxString SumCell::ToOMML()
{
  wxString base = m_base->ListToOMML();

  wxString from;
  if(m_under) from = m_under->ListToOMML();
  
  wxString to;
  if(m_over) to = m_over->ListToOMML();

  wxString retval;

  retval = wxT("<m:nary><m:naryPr><m:chr>");
  if (m_sumStyle == SM_SUM)
    retval += wxT("\x2211");
  else
    retval += wxT("\x220F");
    
  retval += wxT("</m:chr></m:naryPr>");
  if(from != wxEmptyString)
    retval += wxT("<m:sub>") + from + wxT("</m:sub>");
  if(to != wxEmptyString)
    retval += wxT("<m:sup>") + to + wxT("</m:sup>");
  retval += wxT("<m:e>") + base + wxT("</m:e></m:nary>");

  return retval;
}


wxString SumCell::ToXML()
{
  wxString type(wxT("sum"));

  if (m_sumStyle == SM_PROD)
    type = wxT("prod");
  else if (m_over->ListToString() == wxEmptyString)
    type = wxT("lsum");

  return _T("<sm type=\"") + type + wxT("\"><r>") + m_under->ListToXML() + _T("</r><r>") +
    m_over->ListToXML() + _T("</r><r>") +
    m_base->ListToXML() + _T("</r></sm>");
}

wxString SumCell::ToMathML()
{
  wxString base = m_base->ListToMathML();

  wxString from;
  if(m_under) from = m_under->ListToMathML();
  
  wxString to;
  if(m_over) to = m_over->ListToMathML();

  wxString retval;

  if (m_sumStyle == SM_SUM)
  {
//    retval = wxT("<apply><sum/>");
//    if(!from.IsEmpty())
//      retval += wxT("<lowlimit>") + m_under->ListToMathML() + wxT("</lowlimit>");
//    if(!to.IsEmpty())
//      retval += wxT("<uplimit>") + m_over->ListToMathML() + wxT("</uplimit>");
//    retval += m_base->ListToMathML() + wxT("</apply>");
      if(from.IsEmpty() && to.IsEmpty())
        retval = wxT("<mo>&#x2211;</mo>") + base;
      if(from.IsEmpty() && !to.IsEmpty())
        retval = wxT("<mover><mo>&#x2211;</mo>") + to + wxT("</mover>") + base;
      if(!from.IsEmpty() && to.IsEmpty())
        retval = wxT("<munder><mo>&#x2211;</mo>") + from + wxT("</munder>") + base;
      if(!from.IsEmpty() && !to.IsEmpty())
        retval = wxT("<munderover><mo>&#x2211;</mo>") + from + to + wxT("</munderover>") + base;
  }
  else
  {
    // A product
      if(from.IsEmpty() && to.IsEmpty())
        retval = wxT("<mo>&#x220F;</mo>") + base;
      if(from.IsEmpty() && !to.IsEmpty())
        retval = wxT("<mover><mo>&#x220F;</mo>") + to + wxT("</mover>") + base;
      if(!from.IsEmpty() && to.IsEmpty())
        retval = wxT("<munder><mo>&#x220F;</mo>") + from + wxT("</munder>") + base;
      if(!from.IsEmpty() && !to.IsEmpty())
        retval = wxT("<munderover><mo>&#x220F;</mo>") + from + to + wxT("</munderover>") + base;
  }
  return(wxT("<mrow>")+retval+wxT("</mrow>"));
}


void SumCell::SelectInner(wxRect& rect, MathCell** first, MathCell** last)
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
