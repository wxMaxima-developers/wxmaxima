//
//  Copyright (C) 2004-2014 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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

void FunCell::SetParent(MathCell *parent)
{
  m_group = parent;
  if (m_nameCell != NULL)
    m_nameCell->SetParentList(parent);
  if (m_argCell != NULL)
    m_argCell->SetParentList(parent);
}

MathCell* FunCell::Copy()
{
  FunCell* tmp = new FunCell;
  CopyData(this, tmp);
  tmp->SetName(m_nameCell->CopyList());
  tmp->SetArg(m_argCell->CopyList());

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

wxString FunCell::ToString()
{
  if (m_isBroken)
    return wxEmptyString;
  if (m_altCopyText != wxEmptyString)
    return m_altCopyText + MathCell::ListToString();
  wxString s = m_nameCell->ListToString() + m_argCell->ListToString();
  return s;
}

wxString FunCell::ToTeX()
{
  if (m_isBroken)
    return wxEmptyString;
  wxString s = m_nameCell->ListToTeX() + m_argCell->ListToTeX();
  return s;
}

wxString FunCell::ToXML()
{
//  if (m_isBroken)
//    return wxEmptyString;
  return _T("<fn>") + m_nameCell->ListToXML() +
    m_argCell->ListToXML() + _T("</fn>");
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
