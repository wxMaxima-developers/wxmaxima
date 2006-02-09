/*
 *  Copyright (C) 2004-2006 Andrej Vodopivec <andrejv@users.sourceforge.net>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#include "MathCell.h"

MathCell::MathCell()
{
  m_next = NULL;
  m_previous = NULL;
  m_previousToDraw = NULL;
  m_nextToDraw = NULL;
  m_fullWidth = -1;
  m_lineWidth = -1;
  m_maxCenter = -1;
  m_maxDrop = -1;
  m_width = -1;
  m_height = -1;
  m_breakLine = false;
  m_breakPage = false;
  m_forceBreakLine = false;
  m_bigSkip = true;
  m_isFolded = false;
  m_isHidden = false;
  m_isBroken = false;
  m_highlight = false;
  m_type = MC_TYPE_TEXT;
}

/***
 * Derived classes must test if m_next if not NULL end delete it!!!
 */
MathCell::~MathCell()
{
}

/***
 * Append new cell to the end of group.
 */
void MathCell::AppendCell(MathCell *p_next)
{
  if (p_next == NULL) return;
  if (m_next == NULL) {
    m_next = p_next;
    m_next->m_previous = this;
    MathCell *tmp = this;
    while (tmp->m_nextToDraw != NULL)
      tmp = tmp->m_nextToDraw;
    tmp->m_nextToDraw = p_next;
    p_next->m_previousToDraw = tmp;
  }
  else m_next->AppendCell(p_next);
};

/***
 * Get the maximum drop of the center.
 */
int MathCell::GetMaxCenter()
{
  int center = m_isBroken ? 0 : m_center;
  if (m_maxCenter == -1) {
    if (m_nextToDraw == NULL)
      m_maxCenter = center;
    else {
      // If the next cell is on a new line, maxCenter is m_center
      if (m_nextToDraw->m_breakLine && !m_nextToDraw->m_isBroken) {
        m_maxCenter = center;
      }
      else {
        m_maxCenter = MAX(center, m_nextToDraw->GetMaxCenter());
      }
    }
  }
  return m_maxCenter;
}

/***
 * Get the maximum drop of cell.
 */
int MathCell::GetMaxDrop()
{
  int drop = m_isBroken ? 0 : (m_height - m_center);
  if (m_maxDrop == -1) {
    if (m_nextToDraw == NULL)
      m_maxDrop = drop;
    else {
      if (m_nextToDraw->m_breakLine && !m_nextToDraw->m_isBroken) {
        m_maxDrop = drop;
      }
      else {
        m_maxDrop = MAX(drop, m_nextToDraw->GetMaxDrop());
      }
    }
  }
  return m_maxDrop;
}

/***
 * Get the maximum hight of cells in line.
 */
int MathCell::GetMaxHeight()
{
  return GetMaxCenter() + GetMaxDrop();
}

/***
 * Get full width of this group.
 */
int MathCell::GetFullWidth(double scale)
{
  if (m_fullWidth == -1) {
    if (m_next == NULL)
      m_fullWidth = m_width;
    else
      m_fullWidth = m_width + m_next->GetFullWidth(scale) +
                    SCALE_PX(MC_CELL_SKIP, scale);
  }
  return m_fullWidth;
}

/***
 * Get the width of this line.
 */
int MathCell::GetLineWidth(double scale)
{
  int width = m_isBroken ? 0 : m_width;
  if (m_lineWidth == -1) {
    if (m_nextToDraw == NULL || m_nextToDraw->m_breakLine)
      m_lineWidth = width;
    else
      m_lineWidth = width + m_nextToDraw->GetLineWidth(scale) +
                    SCALE_PX(MC_CELL_SKIP, scale);
  }
  return m_lineWidth;
}

/***
 * Draw this cell to dc (each derived class must draw the content of the cell
 * and then call MathCall::Draw(...).
 */
void MathCell::Draw(CellParser& parser, wxPoint point, int fontsize, bool all)
{
  m_currentPoint.x = point.x;
  m_currentPoint.y = point.y;
  if (m_nextToDraw != NULL && all) {
    double scale = parser.GetScale();
    point.x += m_width + SCALE_PX(MC_CELL_SKIP, scale);
    m_nextToDraw->Draw(parser, point, fontsize, true);
  }
}

/***
 * Calculate the size of cell, only needed once. Each derived class must call
 * MathCell::RecalculateSize(...).
 *
 * Should set: m_height, m_center.
 */
void MathCell::RecalculateSize(CellParser& parser, int fontsize, bool all)
{
  if (m_next != NULL && all)
    m_next->RecalculateSize(parser, fontsize, all);
}

/***
 * Recalculate widths of cells. (Used for changing font size - must recalculate
 * all size information).
 *
 * Should set: set m_width.
 */
void MathCell::RecalculateWidths(CellParser& parser, int fontsize, bool all)
{
  ResetData();
  if (m_next != NULL && all)
    m_next->RecalculateWidths(parser, fontsize, all);
}

/***
 * Is this cell visible in the window.
 */
bool MathCell::DrawThisCell(CellParser& parser, wxPoint point)
{
  int top = parser.GetTop();
  int bottom = parser.GetBottom();
  if (top == -1 || bottom == -1)
    return true;
  if (point.y - GetMaxCenter() > bottom || point.y + GetMaxDrop() < top)
    return false;
  return true;
}

/***
 * Get the rectangle around this cell - if all is true then return the rectangle
 * around the whole line.
 */
wxRect MathCell::GetRect(bool all)
{
  if (m_isBroken)
    return wxRect(-1,-1,0,0);
  if (all)
    return wxRect(m_currentPoint.x, m_currentPoint.y - GetMaxCenter(),
                  GetLineWidth(1.0), GetMaxHeight());
  return wxRect(m_currentPoint.x, m_currentPoint.y-m_center,
                                  m_width, m_height);
}

/***
 * Draws a box around this cell - if all is true draws a box around the whole
 * line.
 */
void MathCell::DrawBoundingBox(wxDC& dc, bool all)
{
  wxRect rect = GetRect(all);
  int x = rect.GetX(), y = rect.GetY();
  int width = rect.GetWidth(), height = rect.GetHeight();
  dc.SetLogicalFunction(wxINVERT);
  dc.DrawRectangle(x-1, y-1, width+2, height+2);
}

/***
 * Do we have an operator in this line - draw () in frac...
 */
bool MathCell::IsCompound()
{
  if (IsOperator()) return true;
  if (m_next == NULL) return false;
  return m_next->IsCompound();
}

/***
 * Is operator - draw () in frac...
 */
bool MathCell::IsOperator()
{
  return false;
}

/***
 * Return the string representation of cell.
 */
wxString MathCell::ToString(bool all)
{
  if (all && m_next != NULL)
    return m_next->ToString(all);
  return wxEmptyString;
}

/***
 * Get the part for diff tag support - only ExpTag overvrides this.
 */
wxString MathCell::GetDiffPart()
{
  return wxEmptyString;
}

/***
 * Find the first and last cell in rectangle rect in this line.
 */

void MathCell::SelectRect(wxRect& rect, MathCell** first, MathCell** last)
{
  SelectFirst(rect, first);
  if (*first != NULL) {
    *last = *first;
    (*first)->SelectLast(rect, last);
    if (*last == *first)
      (*first)->SelectInner(rect, first, last);
  }
  else
    *last = NULL;
}

/***
 * Find the first cell in rectangle rect in this line.
 */
void MathCell::SelectFirst(wxRect& rect, MathCell** first)
{
  if (rect.Intersects(GetRect(false)))
    *first = this;
  else if (m_nextToDraw != NULL)
    m_nextToDraw->SelectFirst(rect, first);
  else
    *first = NULL;
}

/***
 * Find the last cell in rectangle rect in this line.
 */
void MathCell::SelectLast(wxRect& rect, MathCell** last)
{
  if (rect.Intersects(GetRect(false)))
    *last = this;
  if (m_nextToDraw != NULL)
    m_nextToDraw->SelectLast(rect, last);
}

/***
 * Select rectangle in deeper cell - derived classes should override this
 */
void MathCell::SelectInner(wxRect& rect, MathCell** first, MathCell** last)
{
  *first = this;
  *last = this;
}

/***
 * Break line when the cell is not broken only.
 */
bool MathCell::BreakLineHere()
{
  return (!m_isBroken && (m_breakLine || m_forceBreakLine));
}

/***
 * Does this cell contain a rectangle sm
 */
bool MathCell::ContainsRect(wxRect& sm, bool all)
{
  wxRect big = GetRect(all);
  if (big.x <= sm.x &&
      big.y <= sm.y &&
      big.x + big.width >= sm.x + sm.width &&
      big.y + big.height >= sm.y + sm.height)
    return true;
  return false;
}

/***
 * Resets remembered data.
 */
void MathCell::ResetData()
{
  m_fullWidth = -1;
  m_lineWidth = -1;
  m_maxCenter = -1;
  m_maxDrop = -1;
  m_currentPoint.x = -1;
  m_currentPoint.y = -1;
  m_breakLine = false;
}

/***
 * Unbreaks broken cells
 */
void MathCell::Unbreak(bool all)
{
  ResetData();
  m_isBroken = false;
  if (!m_isFolded) {
    m_nextToDraw = m_next;
    if (m_nextToDraw != NULL)
      m_nextToDraw->m_previousToDraw = this;
  }
  if (all && m_next != NULL)
    m_next->Unbreak(all);
}

/***
 * Set the pen in device context accordint to the style of the cell.
 */
void MathCell::SetPen(CellParser& parser)
{
  wxDC& dc = parser.GetDC();
  if (m_highlight)
    dc.SetPen(*(wxThePenList->FindOrCreatePen(parser.GetColor(TS_HIGHLIGHT),
                                              1, wxSOLID)));
  else if (m_type == MC_TYPE_PROMPT)
    dc.SetPen(*(wxThePenList->FindOrCreatePen(parser.GetColor(TS_OTHER_PROMPT),
                                              1, wxSOLID)));
  else if (m_type == MC_TYPE_INPUT)
    dc.SetPen(*(wxThePenList->FindOrCreatePen(parser.GetColor(TS_INPUT),
                                              1, wxSOLID)));
}

/***
 * Reset the pen in the device context.
 */
void MathCell::UnsetPen(CellParser& parser)
{
  wxDC& dc = parser.GetDC();
  if (m_type == MC_TYPE_PROMPT || m_type == MC_TYPE_INPUT || m_highlight)
    dc.SetPen(*(wxThePenList->FindOrCreatePen(parser.GetColor(TS_NORMAL_TEXT),
                                              1, wxSOLID)));
}
