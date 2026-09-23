// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  This file defines the class MatrCell

  MatrCell is the Cell type that represents matrices and matrix-like
  elements like the table_form command.
*/

#include "MatrCell.h"
#include "CellImpl.h"
#include "MatrixScrollHost.h"

#include <algorithm>
#include <optional>

MatrCell::MatrCell(GroupCell *group, Configuration *config)
  : Cell(group, config) {
}

MatrCell::MatrCell(GroupCell *group, const MatrCell &cell)
  : MatrCell(group, cell.m_configuration) {
  CopyCommonData(cell);
  m_specialMatrix = cell.m_specialMatrix;
  m_inferenceMatrix = cell.m_inferenceMatrix;
  m_parenType = cell.m_parenType;
  m_rowNames = cell.m_rowNames;
  m_colNames = cell.m_colNames;
  m_nestedInMatrix = cell.m_nestedInMatrix;
  m_matWidth = cell.m_matWidth;
  m_matHeight = cell.m_matHeight;
  for (size_t i = 0; i < cell.m_matWidth * cell.m_matHeight; i++)
    if (i < cell.m_cells.size())
      m_cells.emplace_back(cell.m_cells.at(i)->CopyList(group));

  for (size_t i = 0; i < m_matHeight; i++)
    m_dropCenters.emplace_back(-1, -1);

  for (size_t i = 0; i < m_matWidth; i++)
    m_widths.emplace_back(-1);
}

DEFINE_CELL(MatrCell)

void MatrCell::Recalculate(AFontSize const fontsize) const {
  AFontSize const fontsize_entry{MC_MIN_SIZE, fontsize - 2};
  bool changed = false;
  for (size_t i = 0; i < m_cells.size(); i++) {
    if (m_configuration->IsLayoutCancelled()) {
      // The layout deadline fired before we finished measuring the entries, so
      // the m_widths / m_width / m_height computation below never ran. Make
      // sure the guard below can't consider this cell up to date and leave the
      // matrix frozen with stale (or first-pass garbage) column widths once
      // the group is re-laid-out later (GroupCell::RecalculateOutput clears
      // the cancel flag and may drop m_layoutSuppressed): invalidate the size
      // so we are forced to recompute on the next, non-deadline-bound pass.
      m_width.Invalidate();
      return;
    }
    changed |= m_cells.at(i)->RecalculateList(fontsize_entry);
  }

  if (changed || NeedsRecalculation(fontsize)) {
    Cell::Recalculate(fontsize);

    // The extent of each column and row, the gaps between them included
    std::vector<wxCoord> colSizes;
    m_widths.clear();
    for (size_t i = 0; i < m_matWidth; i++) {
      wxCoord width = 0;
      for (size_t j = 0; j < m_matHeight; j++) {
        if ((m_matWidth * j + i) < m_cells.size())
          width = std::max(width, GetInnerCell(j, i)->SumOfWidths());
      }
      m_widths.emplace_back(width);
      colSizes.emplace_back(width + Scale_Px(10));
    }

    std::vector<wxCoord> rowSizes;
    m_dropCenters.clear();
    for (size_t i = 0; i < m_matHeight; i++) {
      wxCoord center = 0, drop = 0;
      for (size_t j = 0; j < m_matWidth; j++)
        if (m_matWidth * i + j < m_cells.size()) {
          center = std::max(center, GetInnerCell(i, j)->GetCenterList());
          drop = std::max(drop, GetInnerCell(i, j)->GetMaxDrop());
        }
      m_dropCenters.emplace_back(drop, center);
      rowSizes.emplace_back(center + drop + Scale_Px(10));
    }

    wxCoord fullWidth = 0;
    for (wxCoord size : colSizes)
      fullWidth += size;
    wxCoord fullHeight = 0;
    for (wxCoord size : rowSizes)
      fullHeight += size;
    m_scrollableSize = wxSize(fullWidth, fullHeight);

    // Which way to deal with a matrix too large for the window. Scrolling
    // needs a window to put scrollbars in; where there is none (printing, a
    // context without a worksheet) the matrix is elided instead, since paper
    // can't scroll. (The graphical exporters switch all this off; see
    // OutCommon.)
    Configuration::OversizedMatrices mode = m_configuration->GetOversizedMatrices();
    MatrixScrollHost *const host = m_configuration->GetMatrixScrollHost();
    if ((mode == Configuration::OversizedMatrices::scroll) && (host == nullptr))
      mode = Configuration::OversizedMatrices::elide;
    // Only the outermost matrix scrolls. A viewport inside a viewport would
    // mean two sets of scrollbars for one thing -- and the inner ones, being
    // real windows, couldn't even be clipped to the outer viewport. Shown in
    // full instead, a nested matrix simply makes the outer one wider, and
    // that one scrolls over all of it.
    if ((mode == Configuration::OversizedMatrices::scroll) && m_nestedInMatrix)
      mode = Configuration::OversizedMatrices::showInFull;

    // How much room there is. The canvas size is part of the configuration,
    // and changing it forces a recalculation (see
    // Configuration::SetCanvasSize()), so this is redone whenever the window
    // or the printed page changes size. A context that never set a canvas
    // size gets the whole matrix, rather than one cut down to nothing.
    const wxSize canvas = m_configuration->GetCanvasSize();
    // What Cell::BreakLines_List() lets a line of maths use, less the label
    // column that usually sits to the left of the matrix.
    const wxCoord widthBudget = std::max(
      static_cast<wxCoord>(canvas.x - m_configuration->GetIndent() -
                           Scale_Px(m_configuration->GetLabelWidth()) -
                           Scale_Px(5)),
      Scale_Px(150));
    // A matrix taller than most of the window can never be seen whole
    // without scrolling past it, which is where the 80% comes from.
    const wxCoord heightBudget =
      std::max(static_cast<wxCoord>(canvas.y * 8 / 10), Scale_Px(100));

    const wxCoord dotsGap = DotsExtent() + Scale_Px(10);
    m_colElision = {};
    m_rowElision = {};
    m_hasHorizontalScrollbar = false;
    m_hasVerticalScrollbar = false;
    m_scrollbarThickness = 0;
    if (mode == Configuration::OversizedMatrices::elide) {
      if (canvas.x > 0)
        m_colElision = ChooseElision(colSizes, dotsGap, widthBudget);
      if (canvas.y > 0)
        m_rowElision = ChooseElision(rowSizes, dotsGap, heightBudget);
    } else if (mode == Configuration::OversizedMatrices::scroll) {
      const wxCoord thickness = host->ScrollbarThickness();
      // A vertical scrollbar takes room from the columns, so it can make a
      // horizontal one necessary. Not the other way round: the height budget
      // is for the viewport alone.
      m_hasVerticalScrollbar = (canvas.y > 0) && (fullHeight > heightBudget);
      m_hasHorizontalScrollbar =
        (canvas.x > 0) &&
        (fullWidth > widthBudget - (m_hasVerticalScrollbar ? thickness : 0));
      if (IsScrolling())
        m_scrollbarThickness = thickness;
    }

    // The box between the brackets
    wxCoord contentWidth = m_colElision.Active() ? dotsGap : 0;
    for (size_t i = 0; i < colSizes.size(); i++)
      if (!m_colElision.Hides(i))
        contentWidth += colSizes.at(i);
    if (m_hasHorizontalScrollbar)
      contentWidth = widthBudget - (m_hasVerticalScrollbar ? m_scrollbarThickness : 0);
    if (contentWidth < Scale_Px(14))
      contentWidth = Scale_Px(14);

    wxCoord contentHeight = m_rowElision.Active() ? dotsGap : 0;
    for (size_t i = 0; i < rowSizes.size(); i++)
      if (!m_rowElision.Hides(i))
        contentHeight += rowSizes.at(i);
    if (m_hasVerticalScrollbar)
      contentHeight = heightBudget;
    if (contentHeight == 0)
      contentHeight = fontsize + Scale_Px(10);

    m_contentSize = wxSize(contentWidth, contentHeight);
    // Keep the scroll position the reader chose across a relayout, as far as
    // the new size allows it.
    ClampScrollPosition();

    // The scrollbars sit to the right of and below the box, outside the
    // brackets. The centre line stays that of the box, so the matrix still
    // lines up with its label; a horizontal scrollbar just adds to the drop.
    m_width = contentWidth + (m_hasVerticalScrollbar ? m_scrollbarThickness : 0);
    m_height = contentHeight + (m_hasHorizontalScrollbar ? m_scrollbarThickness : 0);
    m_center = contentHeight / 2;
  }
}

void MatrCell::ClampScrollPosition() const {
  const wxCoord maxX = std::max(0, m_scrollableSize.x - m_contentSize.x);
  const wxCoord maxY = std::max(0, m_scrollableSize.y - m_contentSize.y);
  m_scroll.x = m_hasHorizontalScrollbar ? std::clamp(m_scroll.x, 0, maxX) : 0;
  m_scroll.y = m_hasVerticalScrollbar ? std::clamp(m_scroll.y, 0, maxY) : 0;
}

bool MatrCell::ScrollTo(wxPoint position) {
  const wxPoint old = m_scroll;
  m_scroll = position;
  ClampScrollPosition();
  if (m_scroll == old)
    return false;
  // Nothing about the layout changes but where the entries sit, so reposition
  // just them rather than asking for a recalculation.
  if (m_currentPoint != wxPoint(-1, -1))
    SetCurrentPoint(m_currentPoint);
  return true;
}

wxRect MatrCell::ViewportRect() const {
  // Inside the brackets' margins, which is where the entries of a matrix
  // that isn't scrolled begin and end, too.
  return wxRect(m_currentPoint.x + Scale_Px(5),
                m_currentPoint.y - m_center + Scale_Px(5),
                m_contentSize.x - 2 * Scale_Px(5),
                m_contentSize.y - 2 * Scale_Px(5));
}

wxRect MatrCell::HorizontalScrollbarRect() const {
  if (!m_hasHorizontalScrollbar)
    return {};
  return wxRect(m_currentPoint.x, m_currentPoint.y - m_center + m_contentSize.y,
                m_contentSize.x, m_scrollbarThickness);
}

wxRect MatrCell::VerticalScrollbarRect() const {
  if (!m_hasVerticalScrollbar)
    return {};
  return wxRect(m_currentPoint.x + m_contentSize.x, m_currentPoint.y - m_center,
                m_scrollbarThickness, m_contentSize.y);
}

bool MatrCell::IsEntryShown(size_t row, size_t col) const {
  if (IsElided(row, col))
    return false;
  if ((row * m_matWidth + col) >= m_cells.size())
    return false;
  if (!IsScrolling())
    return true;
  return GetInnerCell(static_cast<int>(row), static_cast<int>(col))
    ->GetRect(true).Intersects(ViewportRect());
}

MatrCell::Elision MatrCell::ChooseElision(const std::vector<wxCoord> &sizes,
                                          wxCoord gapSize, wxCoord budget) {
  const size_t n = sizes.size();
  // With fewer than three there is nothing between the first and the last to
  // leave out.
  if (n < 3)
    return {};
  wxCoord total = 0;
  for (wxCoord size : sizes)
    total += size;
  if (total <= budget)
    return {};

  // The first and the last are kept even if they alone don't fit: they are
  // what tells the reader where the matrix begins and ends, and in a
  // table_form they are the headings.
  size_t left = 1, right = 1;
  wxCoord used = sizes.front() + sizes.back() + gapSize;
  // Then alternate between the two ends, so what is left out is the middle
  // and both edges of the matrix stay in view, stopping at the first one that
  // doesn't fit so neither end gets far ahead of the other.
  bool fromLeft = true;
  while (left + right < n) {
    const size_t next = fromLeft ? left : n - 1 - right;
    if (used + sizes.at(next) > budget)
      break;
    used += sizes.at(next);
    if (fromLeft)
      left++;
    else
      right++;
    fromLeft = !fromLeft;
  }
  if (left + right >= n)
    return {};
  Elision elision;
  elision.first = left;
  elision.count = n - left - right;
  return elision;
}

wxCoord MatrCell::DotPitch() const {
  // Roughly the spacing of TeX's \cdots, measured in the entries' font size.
  const double em = Scale_Px(AFontSize{MC_MIN_SIZE, m_fontSize - 2}).Get();
  return std::max(Scale_Px(3), static_cast<wxCoord>(0.4 * em + 0.5));
}

wxCoord MatrCell::DotRadius() const {
  const double em = Scale_Px(AFontSize{MC_MIN_SIZE, m_fontSize - 2}).Get();
  return std::max(static_cast<wxCoord>(1), static_cast<wxCoord>(0.07 * em + 0.5));
}

void MatrCell::DrawDots(wxDC *dc, wxPoint start, wxPoint step) const {
  const wxCoord radius = DotRadius();
  for (int dot = 0; dot < 3; dot++)
    dc->DrawCircle(start + wxPoint(dot * step.x, dot * step.y), radius);
}

void MatrCell::DrawElisionMarks(wxDC *dc) const {
  SetPen(dc, 1);
  SetBrush(dc);
  const wxCoord dotsGap = DotsExtent() + Scale_Px(10);
  const wxCoord pitch = DotPitch();
  const wxCoord radius = DotRadius();

  // Walk the columns and rows the way SetCurrentPoint() does, noting where
  // the gap for the dots begins and the middle of every column and row that
  // is shown.
  wxCoord gapX = 0;
  std::vector<wxCoord> colMiddles;
  wxCoord x = m_currentPoint.x + Scale_Px(5);
  for (size_t i = 0; i < m_matWidth; i++) {
    if (m_colElision.Active() && (i == m_colElision.first)) {
      gapX = x;
      x += dotsGap;
    }
    if (m_colElision.Hides(i))
      continue;
    colMiddles.emplace_back(x + m_widths.at(i) / 2);
    x += m_widths.at(i) + Scale_Px(10);
  }
  wxCoord gapY = 0;
  std::vector<wxCoord> rowMiddles;
  wxCoord y = m_currentPoint.y - m_center + Scale_Px(5);
  for (size_t j = 0; j < m_matHeight; j++) {
    if (m_rowElision.Active() && (j == m_rowElision.first)) {
      gapY = y;
      y += dotsGap;
    }
    if (m_rowElision.Hides(j))
      continue;
    rowMiddles.emplace_back(y + m_dropCenters.at(j).center);
    y += m_dropCenters.at(j).Sum() + Scale_Px(10);
  }

  // ⋯ in every row that is shown, at the height the row's entries are
  // centred on; ⋮ in every column that is shown; ⋱ where the two gaps cross.
  if (m_colElision.Active())
    for (wxCoord middle : rowMiddles)
      DrawDots(dc, {gapX + radius, middle}, {pitch, 0});
  if (m_rowElision.Active())
    for (wxCoord middle : colMiddles)
      DrawDots(dc, {middle, gapY + radius}, {0, pitch});
  if (m_colElision.Active() && m_rowElision.Active())
    DrawDots(dc, {gapX + radius, gapY + radius}, {pitch, pitch});
}

void MatrCell::SetCurrentPoint(wxPoint point) const {
  Cell::SetCurrentPoint(point);
  // The entries that are left out are not positioned at all. They keep
  // whatever position they had before, which is why GetToolTip() and
  // GetInnerCellsInRect() skip them rather than trust it.
  // A scrolling matrix positions every entry, including those scrolled out
  // of the viewport: Draw() clips them, and IsEntryShown() keeps them out of
  // hit-testing.
  const wxCoord dotsGap = DotsExtent() + Scale_Px(10);
  wxPoint mp;
  mp.x = point.x + Scale_Px(5) - m_scroll.x;
  for (size_t i = 0; i < m_matWidth; i++) {
    if (m_colElision.Active() && (i == m_colElision.first))
      mp.x += dotsGap;
    if (m_colElision.Hides(i))
      continue;
    mp.y = point.y - m_center + Scale_Px(5) - m_scroll.y;
    for (size_t j = 0; j < m_matHeight; j++) {
      if (m_rowElision.Active() && (j == m_rowElision.first))
        mp.y += dotsGap;
      if (m_rowElision.Hides(j))
        continue;
      if ((j * m_matWidth + i) < m_cells.size()) {
        mp.y += m_dropCenters.at(j).center;
        wxPoint mp1(mp);
        mp1.x = mp.x + (m_widths.at(i) - GetInnerCell(j, i)->SumOfWidths()) / 2;
        GetInnerCell(j, i)->SetCurrentPointList(mp1);
        mp.y += (m_dropCenters.at(j).drop + Scale_Px(10));
      }
    }
    mp.x += (m_widths.at(i) + Scale_Px(10));
  }
}

const wxString MatrCell::GetToolTip(const wxPoint point) const {
  if (!ContainsPoint(point))
    return wxm::emptyString;

  // Same as Cell::GetToolTip(), but asking only the entries that are shown:
  // an elided one still carries the position it had before it was left out,
  // and one scrolled out of the viewport sits somewhere outside the matrix.
  for (size_t j = 0; j < m_matHeight; j++)
    for (size_t i = 0; i < m_matWidth; i++) {
      if (!IsEntryShown(j, i))
        continue;
      for (const Cell &tmp : OnList(GetInnerCell(j, i))) {
        auto &toolTip = tmp.GetToolTip(point);
        if (!toolTip.empty())
          return toolTip;
      }
    }

  // Anywhere else on an elided matrix -- including its entries, which have no
  // tooltip of their own, just as Cell::GetToolTip() lets a parent's tooltip
  // cover its children -- say what is missing, and that nothing is lost by it.
  const unsigned long firstRow = m_rowElision.first + 1;
  const unsigned long lastRow = m_rowElision.first + m_rowElision.count;
  const unsigned long firstCol = m_colElision.first + 1;
  const unsigned long lastCol = m_colElision.first + m_colElision.count;
  if (m_rowElision.Active() && m_colElision.Active())
    return wxString::Format(
      _("Rows %lu to %lu and columns %lu to %lu of this matrix are not shown, "
        "so that it fits the window. Copying the matrix copies all of it."),
      firstRow, lastRow, firstCol, lastCol);
  if (m_rowElision.Active())
    return wxString::Format(
      _("Rows %lu to %lu of this matrix are not shown, so that it fits the "
        "window. Copying the matrix copies all of it."),
      firstRow, lastRow);
  if (m_colElision.Active())
    return wxString::Format(
      _("Columns %lu to %lu of this matrix are not shown, so that it fits the "
        "window. Copying the matrix copies all of it."),
      firstCol, lastCol);

  return GetLocalToolTip();
}

Cell::Range MatrCell::GetInnerCellsInRect(const wxRect &rect) const {
  // Cell::GetInnerCellsInRect(), minus the entries that are left out or
  // scrolled out of view.
  Range retval = {const_cast<MatrCell *>(this), const_cast<MatrCell *>(this)};
  for (size_t j = 0; j < m_matHeight; j++)
    for (size_t i = 0; i < m_matWidth; i++) {
      if (!IsEntryShown(j, i))
        continue;
      for (Cell const &tmp : OnList(GetInnerCell(j, i)))
        if (tmp.ContainsRect(rect)) {
          auto r = tmp.GetCellsInRect(rect);
          if (r.first)
            retval = r;
        }
    }
  return retval;
}

void MatrCell::Draw(wxDC *dc, wxDC *antialiassingDC) {
  Cell::Draw(dc, antialiassingDC);
  SetBrush(dc);
  if (DrawThisCell()) {
    wxPoint point = m_currentPoint;
    // The box between the brackets. For a scrolling matrix m_width and
    // m_height also hold the scrollbars, which the brackets must stay clear of.
    const wxCoord width = m_contentSize.x;
    const wxCoord height = m_contentSize.y;
    {
      // A scrolling matrix's entries, and the heading lines that scroll with
      // them, are cut off where they leave the viewport.
      std::optional<wxDCClipper> clip, antialiassedClip;
      if (IsScrolling()) {
        clip.emplace(*dc, ViewportRect());
        if (antialiassingDC != dc)
          antialiassedClip.emplace(*antialiassingDC, ViewportRect());
      }
      for (size_t i = 0; i < m_matWidth; i++) {
        for (size_t j = 0; j < m_matHeight; j++) {
          if (IsEntryShown(j, i))
            GetInnerCell(j, i)->DrawList(dc, antialiassingDC);
        }
      }
      if (m_colElision.Active() || m_rowElision.Active())
        DrawElisionMarks(antialiassingDC);
      SetPen(antialiassingDC, 1.5);
      // m_rowNames/m_colNames come straight from the .wxmx XML attributes and
      // are set independently of whether the matrix actually has any
      // columns/rows (MathParser sets them unconditionally). A special matrix
      // with rownames/colnames but zero columns/rows leaves m_widths /
      // m_dropCenters empty, so guard the at(0) accesses - .at() would throw
      // and, uncaught, terminate wxMaxima on such a (corrupt) document.
      if (m_specialMatrix && !m_inferenceMatrix) {
        if (m_rowNames && !m_widths.empty())
          antialiassingDC->DrawLine(
            point.x + m_widths.at(0) + 2 * Scale_Px(5) - m_scroll.x,
            point.y - m_center + Scale_Px(2),
            point.x + m_widths.at(0) + 2 * Scale_Px(5) - m_scroll.x,
            point.y + m_center - Scale_Px(2));
        if (m_colNames && !m_dropCenters.empty())
          antialiassingDC->DrawLine(
            point.x + Scale_Px(1),
            point.y - m_center + m_dropCenters.at(0).Sum() + 2 * Scale_Px(5) - m_scroll.y,
            point.x + Scale_Px(1) + width,
            point.y - m_center + m_dropCenters.at(0).Sum() + 2 * Scale_Px(5) - m_scroll.y);
      }
    }
    SetPen(antialiassingDC, 1.5);
    if (m_specialMatrix) {
      if (m_inferenceMatrix)
        antialiassingDC->DrawLine(point.x + Scale_Px(1), point.y - m_center + Scale_Px(2),
                                  point.x + Scale_Px(1), point.y + m_center - Scale_Px(2));
    } else {
      switch (m_parenType) {
      case paren_rounded: {
        SetPen(dc, 1);
        wxCoord signWidth = Scale_Px(4);
        if (height <= signWidth / 3)
          signWidth = height / 3;

        // Left bracket
        wxPoint pointsL[5] = {
          {point.x + Scale_Px(1) + signWidth, point.y - m_center},
          {point.x + Scale_Px(1) + signWidth / 2,
           point.y - m_center + signWidth / 2},
          {point.x + Scale_Px(1), point.y},
          {point.x + Scale_Px(1) + signWidth / 2,
           point.y + m_center - signWidth / 2},
          {point.x + Scale_Px(1) + signWidth, point.y + m_center}};
        antialiassingDC->DrawSpline(5, pointsL);
        pointsL[2] = {point.x + Scale_Px(1.5), point.y};
        antialiassingDC->DrawSpline(5, pointsL);

        // Right bracket
        wxPoint pointsR[5] = {
          {point.x + width - Scale_Px(1) - signWidth, point.y - m_center},
          {point.x + width - Scale_Px(1) - signWidth / 2,
           point.y - m_center + signWidth / 2},
          {point.x + width - Scale_Px(1.5), point.y},
          {point.x + width - Scale_Px(1) - signWidth / 2,
           point.y + m_center - signWidth / 2},
          {point.x + width - Scale_Px(1) - signWidth, point.y + m_center}};
        antialiassingDC->DrawSpline(5, pointsR);
        pointsR[2] = {point.x + width - Scale_Px(1), point.y};
        antialiassingDC->DrawSpline(5, pointsR);
        break;
      }
      case paren_angled: {
        SetPen(dc, 1);
        wxCoord signWidth = Scale_Px(4);
        if (height <= signWidth / 3)
          signWidth = height / 3;

        // Left bracket
        wxPoint pointsL[3] = {
          {point.x + Scale_Px(1) + signWidth, point.y - m_center},
          {point.x + Scale_Px(1), point.y},
          {point.x + Scale_Px(1) + signWidth, point.y + m_center}};
        antialiassingDC->DrawLines(3, pointsL);

        // Right bracket
        wxPoint pointsR[3] = {
          {point.x + width - Scale_Px(1) - signWidth, point.y - m_center},
          {point.x + width - Scale_Px(1.5), point.y},
          {point.x + width - Scale_Px(1) - signWidth, point.y + m_center}};
        antialiassingDC->DrawLines(3, pointsR);
        break;
      }
      case paren_straight: {
        SetPen(dc, 1);
        wxCoord signWidth = Scale_Px(4);
        if (height <= signWidth / 3)
          signWidth = height / 3;

        // Left bracket
        wxPoint pointsL[2] = {
          {point.x + Scale_Px(1) + signWidth / 2, point.y - m_center},
          {point.x + Scale_Px(1) + signWidth / 2, point.y + m_center}};
        antialiassingDC->DrawLines(2, pointsL);

        // Right bracket
        wxPoint pointsR[2] = {{point.x + width - Scale_Px(1) - signWidth / 2,
                                 point.y - m_center},
                              {point.x + width - Scale_Px(1) - signWidth / 2,
                               point.y + m_center}};
        antialiassingDC->DrawLines(2, pointsR);
        break;
      }
      case paren_brackets: {
        SetPen(dc, 1.5);
        // left bracket
        const wxPoint pointsL[4] = {{Scale_Px(5), -m_center + Scale_Px(2)},
                                    {Scale_Px(1), -m_center + Scale_Px(2)},
                                    {Scale_Px(1), m_center - Scale_Px(2)},
                                    {Scale_Px(5), m_center - Scale_Px(2)}};
        antialiassingDC->DrawLines(4, pointsL, point.x, point.y);

        // right bracket
        const wxPoint pointsR[4] = {{-Scale_Px(5), -m_center + Scale_Px(2)},
                                    {-Scale_Px(1), -m_center + Scale_Px(2)},
                                    {-Scale_Px(1), m_center - Scale_Px(2)},
                                    {-Scale_Px(5), m_center - Scale_Px(2)}};
        antialiassingDC->DrawLines(4, pointsR, point.x + width - 1, point.y);
        break;
      }
      case paren_none:
        break;
      }
    }

    // The scrollbars aren't ours to draw: they are real windows the host
    // owns. Tell it this matrix needs them, and where.
    if (IsScrolling())
      if (MatrixScrollHost *host = m_configuration->GetMatrixScrollHost())
        host->MatrixDrawn(this);
  }
}

void MatrCell::AddNewCell(std::unique_ptr<Cell> &&cell) {
  MarkNestedMatrices(cell.get());
  m_cells.emplace_back(std::move(cell));
}

void MatrCell::MarkNestedMatrices(Cell *list) {
  // Every way a matrix ends up inside another goes through AddNewCell()
  // (MathParser builds matrices bottom-up) or through copying one that
  // already did (the copy constructor carries the flag over), so marking
  // here, once, is enough -- and needs no parent pointers, which cells don't
  // have.
  for (Cell &cell : OnList(list)) {
    if (auto *matrix = dynamic_cast<MatrCell *>(&cell))
      matrix->m_nestedInMatrix = true;
    for (Cell &inner : OnInner(&cell))
      MarkNestedMatrices(&inner);
  }
}

wxString MatrCell::ToString() const {
  wxString s = wxS("matrix(\n");
  for (size_t i = 0; i < m_matHeight; i++) {
    s += wxS("\t\t[");
    for (size_t j = 0; j < m_matWidth; j++) {
      s += GetInnerCell(i, j)->ListToString();
      if (j < m_matWidth - 1)
        s += wxS(",\t");
    }
    s += wxS("]");
    if (i < m_matHeight - 1)
      s += wxS(",");
    s += wxS("\n");
  }
  s += wxS("\t)");
  return s;
}

wxString MatrCell::ToMatlab() const {
  // ToDo: We ignore colNames and rowNames here. Are they currently in use?
  wxString s;

  s = wxS("[");
  for (size_t i = 0; i < m_matHeight; i++) {
    for (size_t j = 0; j < m_matWidth; j++) {
      s += GetInnerCell(i, j)->ListToMatlab();
      if (j < m_matWidth - 1)
        s += wxS(", ");
    }
    if (i < m_matHeight - 1)
      s += wxS(";\n");
  }

  s += wxS("];");

  return s;
}

wxString MatrCell::ToTeX() const {
  // ToDo: We ignore colNames and rowNames here. Are they currently in use?
  wxString s;

  if (!m_specialMatrix) {
    switch (m_parenType) {
    case paren_rounded:
      s = wxS("\\begin{pmatrix}");
      break;
    case paren_brackets:
    case paren_angled:
    case paren_straight:
    case paren_none:
      s = wxS("\\begin{bmatrix}");
      break;
    }
  } else {
    s = wxS("\\begin{array}{");
    for (size_t j = 0; j < m_matWidth; j++)
      s += wxS("c");
    s += wxS("}");
  }
  for (size_t i = 0; i < m_matHeight; i++) {
    for (size_t j = 0; j < m_matWidth; j++) {
      s += GetInnerCell(i, j)->ListToTeX();
      if (j < m_matWidth - 1)
        s += wxS(" & ");
    }
    if (i < m_matHeight - 1)
      s += wxS("\\\\\n");
  }
  if (!m_specialMatrix) {
    switch (m_parenType) {
    case paren_rounded:
      s += wxS("\\end{pmatrix}");
      break;
    case paren_brackets:
    case paren_angled:
    case paren_straight:
    case paren_none:
      s += wxS("\\end{bmatrix}");
      break;
    }
  } else
    s += wxS("\\end{array}");
  return s;
}

wxString MatrCell::ToMathML() const {
  wxString retval;
  if (!m_specialMatrix) {
    wxString openchar;
    switch (m_parenType) {
    case paren_rounded:
      openchar = wxS("(");
      break;
    case paren_brackets:
      openchar = wxS("[");
      break;
    case paren_angled:
      openchar = wxS("&#x27E8;");
      break;
    case paren_straight:
      openchar = wxS("|");
      break;
    default:
      break;
    }
    if (m_parenType != paren_none)
      retval = wxS("<mrow><mo>") + openchar + wxS("</mo><mrow>");
  }
  retval += wxS("<mtable>");

  for (size_t i = 0; i < m_matHeight; i++) {
    retval += wxS("<mtr>");
    for (size_t j = 0; j < m_matWidth; j++)
      retval += wxS("<mtd>") + GetInnerCell(i, j)->ListToMathML() + wxS("</mtd>");
    retval += wxS("</mtr>");
  }
  retval += wxS("</mtable>\n");
  if (!m_specialMatrix && m_parenType != paren_none) {
    wxString closechar;
    switch (m_parenType) {
    case paren_rounded:
      closechar = wxS(")");
      break;
    case paren_brackets:
      closechar = wxS("]");
      break;
    case paren_angled:
      closechar = wxS("&#x27E9;");
      break;
    case paren_straight:
      closechar = wxS("|");
      break;
    default:
      break;
    }
    retval += wxS("</mrow><mo>") + closechar + wxS("</mo></mrow>\n");
  }
  return retval;
}

wxString MatrCell::ToOMML() const {
  wxString retval;

  retval = wxS("<m:d>");
  if (!m_specialMatrix) {
    // Same m:begChr="..." m:endChr="..." m:grow="1" attribute form
    // ParenCell/ListCell/IntervalCell::ToOMML() already use -- not the
    // <m:begChr>...</m:begChr><m:grow>"1"</m:grow> child-element form this
    // used to have, which put literal quote characters into m:grow's value
    // (OMML2RTF() turns element text content into raw RTF, so
    // "<m:grow>\"1\"</m:grow>" produced the RTF math control word
    // "{\mgrow "1"}" instead of the well-formed "{\mgrow 1}" every other
    // delimiter-emitting cell type here already produces) -- Word/LibreOffice
    // silently ignored the malformed grow flag and fell back to a small,
    // non-stretchy delimiter regardless of the matrix's actual height (GH #1457).
    switch (m_parenType) {
    case paren_rounded:
      retval += wxS("<m:dPr m:begChr=\"(\" m:endChr=\")\" m:grow=\"1\"></m:dPr>");
      break;
    case paren_brackets:
      retval += wxS("<m:dPr m:begChr=\"[\" m:endChr=\"]\" m:grow=\"1\"></m:dPr>");
      break;
    case paren_angled:
      retval += wxS("<m:dPr m:begChr=\"&lt;\" m:endChr=\"&gt;\" m:grow=\"1\"></m:dPr>");
      break;
    case paren_straight:
      retval += wxS("<m:dPr m:begChr=\"|\" m:endChr=\"|\" m:grow=\"1\"></m:dPr>");
      break;
    case paren_none:
      retval += wxS("<m:dPr m:begChr=\" \" m:endChr=\" \" m:grow=\"1\"></m:dPr>");
      break;
    }
  }

  retval += wxS("<m:e><m:m>");

  for (size_t i = 0; i < m_matHeight; i++) {
    retval += wxS("<m:mr>");
    for (size_t j = 0; j < m_matWidth; j++)
      retval += wxS("<m:e>") + GetInnerCell(i, j)->ListToOMML() +
        wxS("</m:e>");
    retval += wxS("</m:mr>");
  }

  retval += wxS("</m:m></m:e></m:d>");
  return retval;
}

wxString MatrCell::ToXML() const {
  wxString flags = GetXMLFlags();
  switch (m_parenType) {
  case paren_rounded:
    flags += wxS(" roundedParens=\"true\"");
    break;
  case paren_brackets:
    flags += wxS(" roundedParens=\"false\" bracketParens=\"true\"");
    break;
  case paren_angled:
    flags += wxS(" roundedParens=\"false\" angledParens=\"true\"");
    break;
  case paren_straight:
    flags += wxS(" roundedParens=\"false\" straightParens=\"true\"");
    break;
  case paren_none:
    flags += wxS(" roundedParens=\"false\" noneParens=\"true\"");
    break;
  }

  wxString s = wxS("<tb") + flags;
  if (m_specialMatrix) {
    s += wxS(" special=\"true\"");
    s += wxString::Format(wxS(" inference=\"%s\""),
                          m_inferenceMatrix ? "true" : "false");
    s += wxString::Format(wxS(" rownames=\"%s\""), m_rowNames ? "true" : "false");
    s += wxString::Format(wxS(" colnames=\"%s\""), m_colNames ? "true" : "false");
  }
  s += wxS(">");

  for (size_t i = 0; i < m_matHeight; i++) {
    s += wxS("<mtr>");
    for (size_t j = 0; j < m_matWidth; j++)
      s += wxS("<mtd>") + GetInnerCell(i, j)->ListToXML() + wxS("</mtd>");
    s += wxS("</mtr>");
  }
  s += wxS("</tb>");

  return s;
}

void MatrCell::SetDimension() {
  if (m_matHeight != 0)
    m_matWidth = m_matWidth / m_matHeight;
}
