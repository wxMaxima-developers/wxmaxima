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

#include "FunCell.h"

FunCell::FunCell() : MathCell()
{
  m_nameCell = NULL;
  m_argCell = NULL;
}

FunCell::~FunCell()
{
  if (m_nameCell != NULL)
    delete m_nameCell;
  if (m_argCell != NULL)
    delete m_argCell;
  if (m_next != NULL)
    delete m_next;
}

void FunCell::SetParent(MathCell *parent, bool all)
{
  if (m_nameCell != NULL)
    m_nameCell->SetParent(parent, true);
  if (m_argCell != NULL)
    m_argCell->SetParent(parent, true);

  MathCell::SetParent(parent, all);
}

MathCell* FunCell::Copy(bool all)
{
  FunCell* tmp = new FunCell;
  CopyData(this, tmp);
  tmp->SetName(m_nameCell->Copy(true));
  tmp->SetArg(m_argCell->Copy(true));
  if (all && m_next != NULL)
    tmp->AppendCell(m_next->Copy(true));
  return tmp;
}

void FunCell::Destroy()
{
  if (m_nameCell != NULL)
    delete m_nameCell;
  if (m_argCell != NULL)
    delete m_argCell;
  m_nameCell = NULL;
  m_argCell = NULL;
  m_next = NULL;
}

void FunCell::SetName(MathCell *name)
{
  if (name == NULL)
    return ;
  if (m_nameCell != NULL)
    delete m_nameCell;
  m_nameCell = name;
}

void FunCell::SetArg(MathCell *arg)
{
  if (arg == NULL)
    return ;
  if (m_argCell != NULL)
    delete m_argCell;
  m_argCell = arg;
}

void FunCell::RecalculateWidths(CellParser& parser, int fontsize, bool all)
{
  double scale = parser.GetScale();
  m_argCell->RecalculateWidths(parser, fontsize, true);
  m_nameCell->RecalculateWidths(parser, fontsize, true);
  m_width = m_nameCell->GetFullWidth(scale) + m_argCell->GetFullWidth(scale) -
            SCALE_PX(1, scale);
  MathCell::RecalculateWidths(parser, fontsize, all);
}

void FunCell::RecalculateSize(CellParser& parser, int fontsize, bool all)
{
  m_nameCell->RecalculateSize(parser, fontsize, true);
  m_argCell->RecalculateSize(parser, fontsize, true);
  m_center = MAX(m_nameCell->GetMaxCenter(), m_argCell->GetMaxCenter());
  m_height = m_center + MAX(m_nameCell->GetMaxDrop(), m_argCell->GetMaxDrop());
  MathCell::RecalculateSize(parser, fontsize, all);
}

void FunCell::Draw(CellParser& parser, wxPoint point, int fontsize, bool all)
{
  if (DrawThisCell(parser, point))
  {
    double scale = parser.GetScale();

    wxPoint name(point), arg(point);
    m_nameCell->Draw(parser, name, fontsize, true);

    arg.x += m_nameCell->GetFullWidth(scale) - SCALE_PX(1, scale);
    m_argCell->Draw(parser, arg, fontsize, true);
  }

  MathCell::Draw(parser, point, fontsize, all);
}

wxString FunCell::ToString(bool all)
{
  if (m_isBroken)
    return wxEmptyString;
  if (m_altCopyText != wxEmptyString)
    return m_altCopyText + MathCell::ToString(all);
  wxString s = m_nameCell->ToString(true) + m_argCell->ToString(true);
  s += MathCell::ToString(all);
  return s;
}

wxString FunCell::ToTeX(bool all)
{
  if (m_isBroken)
    return wxEmptyString;
  wxString s = m_nameCell->ToTeX(true) + m_argCell->ToTeX(true);
  s += MathCell::ToTeX(all);
  return s;
}

wxString FunCell::ToXML(bool all)
{
//  if (m_isBroken)
//    return wxEmptyString;
  return _T("<fn>") + m_nameCell->ToXML(true) +
    m_argCell->ToXML(true) + _T("</fn>") + MathCell::ToXML(all);
}

void FunCell::SelectInner(wxRect& rect, MathCell** first, MathCell** last)
{
  *first = NULL;
  *last = NULL;
  if (m_nameCell->ContainsRect(rect))
    m_nameCell->SelectRect(rect, first, last);
  else if (m_argCell->ContainsRect(rect))
    m_argCell->SelectRect(rect, first, last);
  if (*first == NULL || *last == NULL)
  {
    *first = this;
    *last = this;
  }
}

bool FunCell::BreakUp()
{
  if (!m_isBroken)
  {
    m_isBroken = true;
    m_nameCell->m_previousToDraw = this;
    m_nameCell->m_nextToDraw = m_argCell;
    m_argCell->m_previousToDraw = m_nameCell;
    m_argCell->m_nextToDraw = m_nextToDraw;
    if (m_nextToDraw != NULL)
      m_nextToDraw->m_previousToDraw = m_argCell;
    m_nextToDraw = m_nameCell;
    return true;
  }
  return false;
}

void FunCell::Unbreak(bool all)
{
  if (m_isBroken)
  {
    m_nameCell->Unbreak(true);
    m_argCell->Unbreak(true);
  }
  MathCell::Unbreak(all);
}
