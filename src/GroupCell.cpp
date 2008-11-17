///
///  Copyright (C) 2008 Andrej Vodopivec <andrejv@users.sourceforge.net>
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

#include "GroupCell.h"
#include "TextCell.h"  m_indent = parser.GetIndent();

GroupCell::GroupCell() : MathCell()
{
  m_input = new TextCell(wxT(">>"));
  m_output = NULL;
  m_outputRect.x = -1;
  m_outputRect.y = -1;
  m_outputRect.width = 0;
  m_outputRect.height = 0;
  m_group = this;
  m_forceBreakLine = true;
  m_special = false;
  m_type = MC_TYPE_GROUP;
  m_indent = MC_BASE_INDENT;
  m_hide = false;
}

GroupCell::~GroupCell()
{
  if (m_input != NULL)
    delete m_input;
  DestroyOutput();
}

void GroupCell::SetParent(MathCell *parent, bool all)
{
  if (m_input != NULL)
    m_input->SetParent(parent, true);

  MathCell *tmp = m_output;
  while (tmp != NULL) {
    tmp->SetParent(parent, false);
    tmp = tmp->m_next;
  }
}

void GroupCell::DestroyOutput() {
  MathCell *tmp = m_output, *tmp1;
  while (tmp != NULL) {
    tmp1 = tmp;
    tmp = tmp->m_next;
    tmp1->Destroy();
    delete tmp1;
  }
  m_output = NULL;
}

MathCell* GroupCell::Copy(bool all)
{
  GroupCell* tmp = new GroupCell;
  CopyData(this, tmp);
  tmp->SetInput(m_input->Copy(true));
  if (m_output != NULL)
    tmp->SetOutput(m_output->Copy(true));
  if (all && m_next != NULL)
    tmp->AppendCell(m_next->Copy(all));
  return tmp;
}

void GroupCell::Destroy()
{
  if (m_input != NULL)
    delete m_input;
  m_input = NULL;
  if (m_output != NULL)
    DestroyOutput();
  m_output = NULL;
  m_next = NULL;
}

void GroupCell::SetInput(MathCell *input)
{
  if (input == NULL)
    return ;
  if (m_input != NULL)
    delete m_input;
  m_input = input;
  m_input->m_group = this;
}

void GroupCell::AppendInput(MathCell *cell)
{
  m_hide = false;
  if (m_input == NULL) {
    m_input = cell;
    m_input->m_group = this;
  }
  else {
    if (m_input->m_next == NULL)
      m_input->AppendCell(cell);
    else
      AppendOutput(cell);
  }
}

void GroupCell::SetOutput(MathCell *output)
{
  m_hide = false;
  if (output == NULL)
    return ;
  if (m_output != NULL)
    DestroyOutput();
  m_output = output;
  m_output->m_group = this;
}

void GroupCell::RemoveOutput()
{
  DestroyOutput();
  m_output = NULL;
  m_hide = false;
}

void GroupCell::AppendOutput(MathCell *cell) {
  m_hide = false;
  if (m_output == NULL) {
    m_output = cell;
    m_output->m_group = this;
  }
  else {
    MathCell *tmp = m_output;
    while (tmp->m_next != NULL)
      tmp = tmp->m_next;
    tmp->AppendCell(cell);
  }
}

void GroupCell::RecalculateWidths(CellParser& parser, int fontsize, bool all)
{
  double scale = parser.GetScale();
  m_input->RecalculateWidths(parser, fontsize, true);

  if (m_output == NULL || m_hide) {
    m_width = m_input->GetFullWidth(scale);
  }

  else {
    MathCell *tmp = m_output;
    while (tmp != NULL) {
      tmp->RecalculateWidths(parser, fontsize, false);
      tmp = tmp->m_next;
    }
    // This is not correct, m_width will be computed correctly in RecalculateSize!
    m_width = MAX(m_input->GetFullWidth(scale), m_output->GetFullWidth(scale));
  }
  MathCell::RecalculateWidths(parser, fontsize, all);
}

void GroupCell::RecalculateSize(CellParser& parser, int fontsize, bool all)
{
  double scale = parser.GetScale();
  m_input->RecalculateSize(parser, fontsize, true);
  m_center = m_input->GetMaxCenter();
  m_height = m_input->GetMaxHeight();
  m_indent = parser.GetIndent();

  if (m_output != NULL && !m_hide) {
    m_output->RecalculateSize(parser, fontsize, true);
    m_outputRect.x = m_currentPoint.x;
    m_outputRect.y = m_currentPoint.y - m_output->GetMaxCenter();
    m_outputRect.width = 0;
    m_outputRect.height = 0;
    m_height = m_input->GetMaxHeight();
    m_width = m_input->GetFullWidth(scale);

    MathCell *tmp = m_output;
    while (tmp != NULL) {
      if (tmp->BreakLineHere() || tmp == m_output) {
        m_width = MAX(m_width, tmp->GetLineWidth(scale));
        m_outputRect.width = MAX(m_outputRect.width, tmp->GetLineWidth(scale));
        m_height += tmp->GetMaxHeight();
        if (tmp->m_bigSkip)
          m_height += MC_LINE_SKIP;
        m_outputRect.height += tmp->GetMaxHeight() + MC_LINE_SKIP;
      }
      tmp = tmp->m_nextToDraw;
    }
  }

  MathCell::RecalculateSize(parser, fontsize, all);
}

void GroupCell::Draw(CellParser& parser, wxPoint point, int fontsize, bool all)
{
  double scale = parser.GetScale();
  wxDC& dc = parser.GetDC();
  if (m_width == -1 || m_height == -1) {
    RecalculateWidths(parser, fontsize, false);
    RecalculateSize(parser, fontsize, false);
  }
  if (DrawThisCell(parser, point))
  {
    SetPen(parser);
    wxPoint in(point);
    m_input->Draw(parser, in, fontsize, true);

    if (m_output != NULL && !m_hide) {
      MathCell *tmp = m_output;
      int drop = tmp->GetMaxDrop();
      in.y += m_input->GetMaxDrop() + m_output->GetMaxCenter();
      m_outputRect.y = in.y - m_output->GetMaxCenter();
      m_outputRect.x = in.x;

      while (tmp != NULL) {

        if (!tmp->m_isBroken) {
          tmp->m_currentPoint.x = in.x;
          tmp->m_currentPoint.y = in.y;
          if (tmp->DrawThisCell(parser, in))
            tmp->Draw(parser, in, MAX(fontsize, MC_MIN_SIZE), false);
          if (tmp->m_nextToDraw != NULL) {
            if (tmp->m_nextToDraw->BreakLineHere()) {
              in.x = m_indent;
              in.y += drop + tmp->m_nextToDraw->GetMaxCenter();
              if (tmp->m_bigSkip)
                in.y += MC_LINE_SKIP;
              drop = tmp->m_nextToDraw->GetMaxDrop();
            } else
              in.x += (tmp->GetWidth() + MC_CELL_SKIP);
          }

        } else {
          if (tmp->m_nextToDraw != NULL && tmp->m_nextToDraw->BreakLineHere()) {
            in.x = m_indent;
            in.y += drop + tmp->m_nextToDraw->GetMaxCenter();
            if (tmp->m_bigSkip)
              in.y += MC_LINE_SKIP;
            drop = tmp->m_nextToDraw->GetMaxDrop();
          }
        }

        tmp = tmp->m_nextToDraw;
      }
    }

    MathCell *editable = GetEditable();
    if (editable != NULL && editable->IsActive()) {
      wxPen active(wxT("coral"), 2);
      dc.SetPen(active);
    }
    else if (m_hide) {
      wxPen hidden(wxT("grey"), 2);
      dc.SetPen(hidden);
    }
    else
      dc.SetPen(*wxGREY_PEN);
    // top horizontal
    dc.DrawLine(point.x, point.y - m_center - 1,
                point.x - SCALE_PX(10, scale), point.y - m_center - 1);
    dc.DrawLine(point.x, point.y - m_center - 1,
                point.x - SCALE_PX(10, scale), point.y - m_center + SCALE_PX(8, scale));
    // vertical
    dc.DrawLine(point.x - SCALE_PX(10, scale), point.y - m_center - 1,
                point.x - SCALE_PX(10, scale), point.y - m_center + m_height);
    // bottom horizontal
    dc.DrawLine(point.x - SCALE_PX(10, scale), point.y - m_center + m_height,
                point.x - SCALE_PX(4, scale), point.y - m_center + m_height);
    // middle horizontal
    if (!IsSpecial() && m_output != NULL && !m_hide) {
      dc.DrawLine(point.x - SCALE_PX(6, scale),
                  point.y - m_center + m_input->GetMaxHeight(),
                  point.x - SCALE_PX(10, scale),
                  point.y - m_center + m_input->GetMaxHeight());
    }

    UnsetPen(parser);
  }
  MathCell::Draw(parser, point, fontsize, all);
}

wxRect GroupCell::HideRect()
{
  return wxRect(m_currentPoint.x - 10, m_currentPoint.y - m_center, 10, 10);
}

wxString GroupCell::ToString(bool all)
{
  wxString str = m_input->ToString(true);
  if (m_output != NULL) {
    MathCell *tmp = m_output;
    while (tmp != NULL) {
      if (tmp->ForceBreakLineHere() && str.Length()>0)
        str += wxT("\n");
      str += tmp->ToString(false);
      tmp = tmp->m_nextToDraw;
    }
  }
  return str + MathCell::ToString(all);
}

wxString GroupCell::ToTeX(bool all)
{
  wxString str;
  if (!IsSpecial()) {
    str = wxT("\n\\begin{verbatim}\n") + m_input->ToString(true) + wxT("\n\\end{verbatim}\n");
  }
  if (m_output != NULL) {
    if (IsSpecial()) {
      str = m_output->ToString(true);
      switch (m_output->GetType()) {
        case MC_TYPE_TITLE:
          str = wxT("\n\\section{") + str + wxT("}\n");
          break;
        case MC_TYPE_SECTION:
          str = wxT("\n\\subsection{") + str + wxT("}\n");
          break;
        default:
          if (!str.StartsWith(wxT("TeX:")))
            str = wxT("\n\\begin{verbatim}\n") + str + wxT("\n\\end{verbatim}\n");
          else
            str = wxT("\n") + str.SubString(5, str.Length()-1) + wxT("\n");
          break;
      }
    }
    else {
      str += wxT("$$");
      wxString label;
      MathCell *tmp = m_output;
      while (tmp != NULL) {
        if (tmp->GetType() == MC_TYPE_LABEL)
          label = tmp->ToTeX(false);
        else
          str += tmp->ToTeX(false);
        tmp = tmp->m_nextToDraw;
      }
      str += label + wxT("$$\n");
    }
  }
  return str + MathCell::ToTeX(all);
}

void GroupCell::SelectRectGroup(wxRect& rect, wxPoint& one, wxPoint& two,
    MathCell **first, MathCell **last)
{
  *first = NULL;
  *last = NULL;

  if (m_input->ContainsRect(rect))
    m_input->SelectRect(rect, first, last);
  else if (m_output != NULL && m_outputRect.Contains(rect))
    SelectRectInOutput(rect, one, two, first, last);

  if (*first == NULL || *last == NULL)
  {
    *first = this;
    *last = this;
  }
}

void GroupCell::SelectInner(wxRect& rect, MathCell **first, MathCell **last)
{
  *first = NULL;
  *last = NULL;

  if (m_input->ContainsRect(rect))
    m_input->SelectRect(rect, first, last);
  else if (m_output != NULL  && !m_hide && m_outputRect.Contains(rect))
    m_output->SelectRect(rect, first, last);

  if (*first == NULL || *last == NULL)
  {
    *first = this;
    *last = this;
  }
}

void GroupCell::SelectPoint(wxPoint& point, MathCell **first, MathCell **last)
{
  *first = NULL;
  *last = NULL;

  wxRect rect(point.x, point.y, 1, 1);

  if (m_input->ContainsRect(rect))
    m_input->SelectInner(rect, first, last);
}

void GroupCell::SelectRectInOutput(wxRect& rect, wxPoint& one, wxPoint& two,
    MathCell **first, MathCell **last)
{
  if (m_hide)
    return;

  MathCell* tmp;
  wxPoint start, end;

  if (one.y < two.y || (one.y == two.y && one.x < two.x)) {
    start = one;
    end = two;
  } else {
    start = two;
    end = one;
  }

  // Lets select a rectangle
  tmp = m_output;
  *first = *last = NULL;

  while (tmp != NULL && !rect.Intersects(tmp->GetRect()))
    tmp = tmp->m_nextToDraw;
  *first = tmp;
  *last = tmp;
  while (tmp != NULL) {
    if (rect.Intersects(tmp->GetRect()))
      *last = tmp;
    tmp = tmp->m_nextToDraw;
  }

  if (*first != NULL && *last != NULL) {

    // If selection is on multiple lines, we need to correct it
    if ((*first)->GetCurrentY() != (*last)->GetCurrentY()) {
      tmp = *last;
      MathCell *curr;

      // Find the first cell in selection
      while (*first != tmp &&
             ((*first)->GetCurrentX() + (*first)->GetWidth() < start.x
              || (*first)->GetCurrentY() + (*first)->GetDrop() < start.y))
        *first = (*first)->m_nextToDraw;

      // Find the last cell in selection
      curr = *last = *first;
       while (1) {
        curr = curr->m_nextToDraw;
        if (curr == NULL)
          break;
        if (curr->GetCurrentX() <= end.x &&
            curr->GetCurrentY() - curr->GetMaxCenter() <= end.y)
          *last = curr;
        if (curr == tmp)
          break;
      }
    }

    if (*first == *last)
      (*first)->SelectInner(rect, first, last);
  }
}

MathCell *GroupCell::GetEditable() {
  if (IsSpecial())
    return GetLabel();
  return GetInput();
}

void GroupCell::BreakLines(int fullWidth) {

  int currentWidth = m_indent;

  MathCell *tmp = m_output;

  while (tmp != NULL && !m_hide) {
    tmp->ResetData();
    tmp->BreakLine(false);
    if (!tmp->m_isBroken) {
      if (tmp->BreakLineHere() || (currentWidth + tmp->GetWidth() >= fullWidth)) {
        currentWidth = m_indent + tmp->GetWidth();
        tmp->BreakLine(true);
      } else
        currentWidth += (tmp->GetWidth() + MC_CELL_SKIP);
    }
    tmp = tmp->m_nextToDraw;
  }
}

void GroupCell::SelectOutput(MathCell **start, MathCell **end) {

  if (m_hide)
    return;

  *start = m_output;

  while (*start != NULL && (*start)->GetType() != MC_TYPE_LABEL)
    *start = (*start)->m_nextToDraw;

  if (*start != NULL)
    *start = (*start)->m_nextToDraw;

  *end = *start;

  while (*end != NULL && (*end)->GetType() == MC_TYPE_TEXT && (*end)->m_nextToDraw != NULL)
    *end = (*end)->m_nextToDraw;

  if (*end == NULL || *start == NULL)
    *end = *start = NULL;
}

void GroupCell::BreakUpCells(wxDC &dc, CellParser parser, int fontsize, int clientWidth) {
  MathCell *tmp = m_output;

  while (tmp != NULL && !m_hide) {
    if (tmp->GetWidth() > clientWidth) {
      if (tmp->BreakUp()) {
        tmp->RecalculateWidths(parser, fontsize, false);
        tmp->RecalculateSize(parser, fontsize, false);
      }
    }
    tmp = tmp->m_nextToDraw;
  }
}

void GroupCell::UnBreakUpCells() {
  MathCell *tmp = m_output;
  while (tmp != NULL) {
    if (tmp->m_isBroken) {
      tmp->Unbreak(false);
    }
    tmp = tmp->m_next;
  }
}
