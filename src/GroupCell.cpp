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
#include "TextCell.h"

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
}

GroupCell::~GroupCell()
{
  if (m_input != NULL)
    delete m_input;
  if (m_output != NULL)
    delete m_output;
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
    delete m_output;
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

void GroupCell::AppendInput(MathCell *cell) {
  if (m_input == NULL) {
    m_input = cell;
    m_input->m_group = this;
  }
  else
    m_input->AppendCell(cell);
}

void GroupCell::SetOutput(MathCell *output)
{
  if (output == NULL)
    return ;
  if (m_output != NULL)
    delete m_output;
  m_output = output;
  m_output->m_group = this;
}

void GroupCell::RemoveOutput()
{
  if (m_output == NULL)
    return;
  delete m_output;
  m_output = NULL;
}

void GroupCell::AppendOutput(MathCell *cell) {
  if (m_output == NULL) {
    m_output = cell;
    m_output->m_group = this;
  }
  else
    m_output->AppendCell(cell);
}

void GroupCell::RecalculateWidths(CellParser& parser, int fontsize, bool all)
{
  double scale = parser.GetScale();
  m_input->RecalculateWidths(parser, fontsize, true);
  if (m_output == NULL) {
    m_width = m_input->GetFullWidth(scale);
  }
  else {
    m_output->RecalculateWidths(parser, fontsize, true);
    m_width = MAX(m_input->GetFullWidth(scale), m_output->GetFullWidth(scale));
  }
  MathCell::RecalculateWidths(parser, fontsize, all);
}

void GroupCell::RecalculateSize(CellParser& parser, int fontsize, bool all)
{
  // TODO: Break lines in m_output
  double scale = parser.GetScale();
  m_input->RecalculateSize(parser, fontsize, true);
  m_center = m_input->GetMaxCenter();
  if (m_output == NULL) {
    m_height = m_input->GetMaxHeight();
  }
  else {
    m_output->RecalculateSize(parser, fontsize, true);
    m_outputRect.x = m_currentPoint.x - m_output->GetMaxCenter();
    m_outputRect.width = 0;
    m_outputRect.height = 0;
    m_height = m_input->GetMaxHeight();
    m_width = m_input->GetFullWidth(scale);

    MathCell *tmp = m_output;
    while (tmp != NULL) {
      if (tmp->BreakLineHere()) {
        m_width = MAX(m_width, tmp->GetLineWidth(scale));
        m_outputRect.width = MAX(m_outputRect.width, tmp->GetLineWidth(scale));
        m_height += tmp->GetMaxHeight() + MC_LINE_SKIP;
        m_outputRect.height += tmp->GetMaxHeight() + MC_LINE_SKIP;
      }
      tmp = tmp->m_next;
    }

  }
  MathCell::RecalculateSize(parser, fontsize, all);
}

void GroupCell::Draw(CellParser& parser, wxPoint point, int fontsize, bool all)
{
  double scale = parser.GetScale();
  wxDC& dc = parser.GetDC();
  if (DrawThisCell(parser, point))
  {
    SetPen(parser);
    wxPoint in(point);
    m_input->Draw(parser, in, fontsize, true);

    if (m_output != NULL) {
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
          if (tmp->m_next != NULL) {
            if (tmp->m_next->BreakLineHere()) {
              in.x = MC_BASE_INDENT;
              in.y += drop + tmp->m_next->GetMaxCenter();
              if (tmp->m_bigSkip)
                in.y += MC_LINE_SKIP;
              drop = tmp->m_next->GetMaxDrop();
            } else
              in.x += (tmp->GetWidth() + MC_CELL_SKIP);
          }
        } else {
          if (tmp->m_next != NULL && tmp->m_next->BreakLineHere()) {
            in.x = MC_BASE_INDENT;
            in.y += drop + tmp->m_next->GetMaxCenter();
            if (tmp->m_bigSkip)
              in.y += MC_LINE_SKIP;
            drop = tmp->m_next->GetMaxDrop();
          }
        }
        tmp = tmp->m_next;
      }
    }

    dc.DrawLine(point.x - SCALE_PX(2, scale),
                point.y - m_center + SCALE_PX(2, scale),
                point.x - SCALE_PX(2, scale),
                point.y - m_center + m_height - SCALE_PX(2, scale));
    UnsetPen(parser);
  }
  MathCell::Draw(parser, point, fontsize, all);

  //dc.DrawRectangle(m_outputRect);
}

wxString GroupCell::ToString(bool all)
{
  // TODO: finish
  return wxT("group") +
         MathCell::ToString(all);
}

wxString GroupCell::ToTeX(bool all)
{
  // TODO: finish
  return wxT("\\left|ddd\\right| ") +
         MathCell::ToTeX(all);
}

void GroupCell::SelectInner(wxRect& rect, MathCell **first, MathCell **last)
{
  *first = NULL;
  *last = NULL;

  if (m_input->ContainsRect(rect))
    m_input->SelectRect(rect, first, last);
  else if (m_output != NULL && m_outputRect.Contains(rect))
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

void GroupCell::BreakLines(int fullWidth) {

  int currentWidth = MC_BASE_INDENT;

  MathCell *tmp = m_output;

  while (tmp != NULL) {
    tmp->ResetData();
    tmp->BreakLine(false);
    if (!tmp->m_isBroken) {
      if (tmp->BreakLineHere() || (currentWidth + tmp->GetWidth() >= fullWidth)) {
        currentWidth = MC_BASE_INDENT + tmp->GetWidth();
        tmp->BreakLine(true);
      } else
        currentWidth += (tmp->GetWidth() + MC_CELL_SKIP);
    }
    tmp = tmp->m_next;
  }
}
