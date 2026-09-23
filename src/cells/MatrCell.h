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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

#ifndef MATRCELL_H
#define MATRCELL_H

#include "Cell.h"

#include <vector>

/*! A Cell that displays a matrix.

  Matrices can be displayed with various bracket styles and optional internal 
  lines for row/column headings. While standard Maxima `matrix()` commands use 
  default formatting, wxMaxima provides a specialized `wx_matrix()` command for 
  advanced layout:

  **`wx_matrix(matrix, [options])`**
  - **`lines=true`**: Draws internal separator lines (required for headings).
  - **`rownames=true`**: Treats the first column as labels.
  - **`colnames=true`**: Treats the first row as labels.
  - **`parenstyle=style`**: Sets the bracket type. Supported styles:
    `round` (), `square` [], `angled` <>, `straight` ||, or `none`.

  Example: `wx_matrix(matrix([1,2],[3,4]), lines=true, rownames=true, parenstyle=square);`

  **Matrices too large for the window.** A matrix has no linear form to fall
  back on the way a fraction does, so BreakUp() cannot help it. If
  Configuration::GetOversizedMatrices() asks for it, Recalculate() instead
  leaves out a contiguous run of middle columns (and/or rows) and draws the
  gap the way a mathematician would write it: ⋯ in each shown row, ⋮ in each
  shown column and ⋱ where the two gaps cross. The first and the last column
  and row are always kept, which also keeps the headings of a `table_form`
  or `wx_matrix(..., rownames=true, colnames=true)` in view.

  Elision is a matter of display only: it applies to the worksheet and to
  printing, whose pages are a real limit. Every To...() export and the
  clipboard still write the whole matrix, and the graphical exporters (see
  OutCommon) switch elision off, since their canvas size is only nominal.
  The left-out entries are simply not positioned or drawn, and are skipped
  when looking up what is under the mouse.

  **Or scrolling.** Configuration::OversizedMatrices::scroll instead shows a
  window-sized viewport onto the whole matrix, with a native scrollbar to
  its right and/or below it. The brackets frame the viewport (m_contentSize)
  and the scrollbars sit outside them, adding to m_width/m_height; the
  centre line stays the viewport's, so the matrix still lines up with its
  label. Every entry is positioned, offset by the scroll position; Draw()
  clips them to ViewportRect(), and IsEntryShown() keeps the ones outside it
  out of hit-testing. The cell owns no window: the scrollbars belong to a
  MatrixScrollHost, which only the worksheet provides, so a matrix laid out
  anywhere else -- for printing, say -- is elided instead.

  **Nesting.** Only the outermost matrix scrolls: one nested in another
  (MarkNestedMatrices() flags it as it is added) is shown in full, and the
  outer one scrolls over all of it. A viewport inside a viewport would mean
  two sets of scrollbars for one thing, and the inner ones -- real windows --
  couldn't even be clipped to the outer viewport. Elision does nest: each
  matrix elides itself to the window, so an outer one can still overshoot by
  the width of the columns it always keeps.

  \image html MatrCellGeometry.svg
  \image html MatrCellVariations.svg
  \image html MatrCellElisionGeometry.svg
  \image html MatrCellScrollGeometry.svg
*/
class MatrCell final : public Cell
{
public:
  MatrCell(GroupCell *group, Configuration *config);
  MatrCell(GroupCell *group, const MatrCell &cell);
  std::unique_ptr<Cell> Copy(GroupCell *group) const override;
  const CellTypeInfo &GetInfo() override;

  size_t GetInnerCellCount() const override { return m_cells.size(); }
  Cell *GetInnerCell(size_t index) const override { return m_cells.at(index).get(); }
  Cell *GetInnerCell(int x, int y) const {
    return m_cells.at(static_cast<size_t>(x) * m_matWidth + y).get(); }

  void Recalculate(const AFontSize fontsize) const override;

  using Cell::SetCurrentPoint;
  void SetCurrentPoint(wxPoint point) const override;
  void Draw(wxDC *dc, wxDC *antialiassingDC) override;

  const wxString GetToolTip(wxPoint point) const override;
  Range GetInnerCellsInRect(const wxRect &rect) const override;

  //! Is the entry in this row and column left out of the display?
  bool IsElided(size_t row, size_t col) const
    { return m_rowElision.Hides(row) || m_colElision.Hides(col); }
  //! How many columns are left out of the display (0 = none)
  size_t ElidedColumns() const { return m_colElision.count; }
  //! How many rows are left out of the display (0 = none)
  size_t ElidedRows() const { return m_rowElision.count; }

  /*! \name Scrolling (Configuration::OversizedMatrices::scroll)

    A scrolling matrix shows a window-sized viewport onto the whole matrix,
    with a native scrollbar below it and/or to its right. Everything here is
    in worksheet (unscrolled) coordinates; the scrollbars themselves belong
    to the MatrixScrollHost.
    @{
  */
  //! Does this matrix show a horizontal scrollbar?
  bool HasHorizontalScrollbar() const { return m_hasHorizontalScrollbar; }
  //! Does this matrix show a vertical scrollbar?
  bool HasVerticalScrollbar() const { return m_hasVerticalScrollbar; }
  //! Where the horizontal scrollbar belongs; empty if there is none
  wxRect HorizontalScrollbarRect() const;
  //! Where the vertical scrollbar belongs; empty if there is none
  wxRect VerticalScrollbarRect() const;
  //! The part of the worksheet the matrix's entries are shown in
  wxRect ViewportRect() const;
  //! The size of the whole matrix, as it would be drawn without scrolling
  wxSize ScrollableSize() const { return m_scrollableSize; }
  //! The size of the viewport onto it, brackets' margins included
  wxSize ViewportSize() const { return m_contentSize; }
  //! How far the matrix is scrolled
  wxPoint ScrollPosition() const { return m_scroll; }
  /*! Scrolls the matrix, clamped to its scrollable range

    Repositions the entries at once, so the matrix can be redrawn without a
    new layout. Returns true if the position actually changed.
  */
  bool ScrollTo(wxPoint position);
  //! Does this matrix sit inside another matrix? Only the outermost one scrolls.
  bool IsNestedInMatrix() const { return m_nestedInMatrix; }
  /*! @} */

  void AddNewCell(std::unique_ptr<Cell> &&cell);

  void NewRow() { m_matHeight++; m_dropCenters.emplace_back(-1, -1);}
  void NewColumn() { m_matWidth++; m_widths.emplace_back(-1);}

  void SetDimension();

  wxString ToMathML() const override;
  wxString ToMatlab() const override;
  wxString ToOMML() const override;
  wxString ToString() const override;
  wxString ToTeX() const override;
  wxString ToXML() const override;

  void SetSpecialFlag(bool special) { m_specialMatrix = special; }

  void SetInferenceFlag(bool inference) { m_inferenceMatrix = inference; }

  void RowNames(bool rn) { m_rowNames = rn; }

  void ColNames(bool cn) { m_colNames = cn; }

  void RoundedParens()  { m_parenType = paren_rounded;}
  void BracketParens()  { m_parenType = paren_brackets;}
  void StraightParens() { m_parenType = paren_straight;}
  void AngledParens()   { m_parenType = paren_angled;}
  void NoParens()       { m_parenType = paren_none;}

private:
  struct DropCenter
  {
    int drop = {}, center = {};
    constexpr int Sum() const { return drop + center; }
    constexpr DropCenter() = default;
    constexpr DropCenter(int drop, int center) : drop(drop), center(center) {}
  };

  //! A contiguous run of rows or columns left out of the display
  struct Elision
  {
    //! The index of the first left-out row/column
    size_t first = 0;
    //! How many rows/columns are left out; 0 means none are
    size_t count = 0;
    constexpr bool Active() const { return count > 0; }
    constexpr bool Hides(size_t index) const
      { return (index >= first) && (index < first + count); }
  };

  /*! Chooses which of these rows/columns to leave out so the rest fit

    \param sizes   the extent of each row/column, gaps included
    \param gapSize the extent of the ⋯ or ⋮ that marks the left-out run,
                   gaps included
    \param budget  the space available

    Keeps rows/columns from both ends alternately, as long as they fit, and
    always keeps the first and the last one. Returns an inactive Elision if
    everything fits.
  */
  static Elision ChooseElision(const std::vector<wxCoord> &sizes,
                               wxCoord gapSize, wxCoord budget);

  //! The distance between the centres of two neighbouring dots of ⋯ ⋮ ⋱
  wxCoord DotPitch() const;
  //! The radius of one dot of ⋯ ⋮ ⋱
  wxCoord DotRadius() const;
  //! The extent of a ⋯, ⋮ or ⋱ along the direction of its dots
  wxCoord DotsExtent() const { return 2 * DotPitch() + 2 * DotRadius(); }
  //! Draws three dots, starting at start and each step further along
  void DrawDots(wxDC *dc, wxPoint start, wxPoint step) const;
  //! Draws the ⋯ ⋮ ⋱ that mark where rows or columns are left out
  void DrawElisionMarks(wxDC *dc) const;
  /*! Tints every other row and every other column of an oversized matrix

    Only a matrix that is elided or scrolls gets bands: that is where losing
    track of a row or column is a real risk. One that fits is left plain, as
    shading it would distract more than it helps.

    The bands are a translucent tint of the text colour, so they need a
    graphics context to blend on (the antialiassing DC): the worksheet has
    already painted any selection highlight underneath, and an opaque band
    would hide it. Where a row band crosses a column band the tint doubles.
    Rows and columns are counted from 0 and the odd ones are tinted, so the
    first row and column -- the headings of a table_form -- stay plain.
  */
  void DrawBands(wxDC *dc) const;
public:
  //! Does this matrix get alternating row/column bands? See DrawBands().
  bool IsBanded() const
    { return m_colElision.Active() || m_rowElision.Active() || IsScrolling(); }
private:
  //! Is this matrix shown in a scrolling viewport right now?
  bool IsScrolling() const
    { return m_hasHorizontalScrollbar || m_hasVerticalScrollbar; }
  //! Is (any part of) this entry visible, i.e. neither elided nor scrolled out?
  bool IsEntryShown(size_t row, size_t col) const;
  //! Clamps m_scroll to what m_scrollableSize and m_contentSize allow
  void ClampScrollPosition() const;
  //! Flags every matrix in this list, and anywhere inside it, as nested
  static void MarkNestedMatrices(Cell *list);

  //! Collection of pointers to inner cells.
  std::vector<std::unique_ptr<Cell>> m_cells;

  mutable std::vector<wxCoord> m_widths;
  mutable std::vector<DropCenter> m_dropCenters;

  /*! The size of the box between the brackets, margins included

    The whole matrix when it is shown in full, what is left of it when it is
    elided, and the viewport when it scrolls. m_width and m_height add the
    scrollbars, if there are any, to the right of and below this.
  */
  mutable wxSize m_contentSize;
  //! The size of the whole matrix, as it would be drawn without scrolling
  mutable wxSize m_scrollableSize;
  //! How far the viewport is scrolled into the matrix; kept across layouts
  mutable wxPoint m_scroll;
  //! How thick the scrollbars are, as the MatrixScrollHost said at layout
  mutable wxCoord m_scrollbarThickness = 0;

  //! The columns left out of the display, if any
  mutable Elision m_colElision;
  //! The rows left out of the display, if any
  mutable Elision m_rowElision;

  size_t m_matWidth = 0;
  size_t m_matHeight = 0;

  enum parenType : int8_t
  {
    paren_rounded = 0,
    paren_brackets = 1,
    paren_angled = 2,
    paren_straight = 3,
    paren_none = 4
  };
//** Bitfield objects (1 bytes)
//**
  uint8_t m_parenType : 3 = paren_rounded;
  bool m_specialMatrix : 1 = false;
  bool m_inferenceMatrix : 1 = false;
  bool m_rowNames : 1 = false;
  bool m_colNames : 1 = false;
  //! Does this matrix sit inside another one? See MarkNestedMatrices().
  bool m_nestedInMatrix : 1 = false;
  mutable bool m_hasHorizontalScrollbar : 1 = false;
  mutable bool m_hasVerticalScrollbar : 1 = false;
};

#endif // MATRCELL_H
