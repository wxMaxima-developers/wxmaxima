// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
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
  This file declares the class SumCell

  SumCell is the Cell type that represents maxima's <code>sum()</code>,
  <code>lsum</code> and <code>product()</code>
  commands.
*/

#ifndef SUMCELL_H
#define SUMCELL_H

#include "precomp.h"
#include "Cell.h"
#include "ParenCell.h"

class TextCell;

//cppcheck-suppress ctuOneDefinitionRuleViolation
class SumCell : public Cell
{
public:
  /*! \image html OperatorCellGeometry.svg
      \image html SumCellLinearGeometry.svg */
  SumCell(GroupCell *group, Configuration *config,
          std::unique_ptr<Cell> &&under, std::unique_ptr<Cell> &&over,
          std::unique_ptr<Cell> &&base);
  SumCell(GroupCell *group, const SumCell &cell);
  std::unique_ptr<Cell> Copy(GroupCell *group) const override;
  const CellTypeInfo &GetInfo() override;

  size_t GetInnerCellCount() const override { return 10; }
  Cell *GetInnerCell(size_t index) const override {
    switch (index) {
    case 0:
      return m_open.get();
    case 1:
      return m_paren.get();
    case 2:
      return m_comma1.get();
    case 3:
      return m_var.get();
    case 4:
      return m_comma2.get();
    case 5:
      return m_start.get();
    case 6:
      return m_comma3.get();
    case 7:
      return m_close.get();
    case 8:
      return m_over.get();
    case 9:
      return m_under.get();
    default:
      return nullptr;
    }
  }

  void Recalculate(const AFontSize fontsize) const override;

  using Cell::SetCurrentPoint;
  void SetCurrentPoint(wxPoint point) const override;
  void Draw(wxDC *dc, wxDC *antialiassingDC) override;

  wxString ToMathML() const override;
  wxString ToMatlab() const override;
  wxString ToOMML() const override;
  wxString ToString() const override;
  wxString ToTeX() const override;
  wxString ToXML() const override;

  void SetAltCopyText(const wxString &text) override { m_altCopyText = text; }

  /*! Whether the summand genuinely needs the disambiguating parentheses
    around it (GH #1536), e.g. because it is a sum of terms like "k+k^2"
    that could otherwise be misread as extending past the sum sign into
    whatever follows it -- a bare summand like "k" doesn't. Must be set
    right after construction, before the first layout pass; defaults to
    true (the old, always-parenthesize behavior) if never called.
  */
  void NeedsParen(bool needsParen) {
    m_baseNeedsParen = needsParen;
    m_displayParen = needsParen;
  }
  const wxString &GetAltCopyText() const override { return m_altCopyText; }

  bool BreakUp() const override;

  /*! ORDER MATTERS, and this is NOT the same set as GetInnerCellCount()/
    GetInnerCell() above: index 1 there is m_paren (the ParenCell wrapper),
    but the linear form shows Base() -- the wrapper's bare inner content,
    parenthesis-free -- since BreakUp() clears m_displayParen before
    building this sequence. m_under (index 9 above) never appears at all;
    m_start (derived from m_under at construction) is shown instead. The
    upper-limit pieces (comma3, over) are only present when m_over actually
    has content (mirrors BreakUp()'s own `m_over->ToString().IsEmpty()`
    check precisely -- don't derive this independently).
  */
  size_t GetBrokenCellCount() const override {
    return m_over->ToString().IsEmpty() ? 7 : 9;
  }
  Cell *GetBrokenCell(size_t index) const override {
    const bool hasOver = !m_over->ToString().IsEmpty();
    switch (index) {
    case 0:
      return m_open.get();
    case 1:
      return Base();
    case 2:
      return m_comma1.get();
    case 3:
      return m_var.get();
    case 4:
      return m_comma2.get();
    case 5:
      return m_start.get();
    case 6:
      return hasOver ? m_comma3.get() : m_close.get();
    case 7:
      return hasOver ? m_over.get() : nullptr;
    case 8:
      return hasOver ? m_close.get() : nullptr;
    default:
      return nullptr;
    }
  }

  void Unbreak() const override final;

protected:
  /*! Re-applies GetMaximaCommandName() to the already-created m_open cell.

    MakeBreakUpCells() (called from SumCell's own constructor) can only
    ever see SumCell::GetMaximaCommandName(): a virtual call made during a
    base class's constructor never reaches a derived class's override,
    since the derived part of the object doesn't exist yet. Every
    subclass that overrides GetMaximaCommandName() (currently only
    ProductCell) MUST call this once from its own constructor body, after
    SumCell's constructor has finished running.
  */
  void RefreshBreakUpCommandName() const;
  //! What maxima command name corresponds to this cell?
  virtual const wxString GetMaximaCommandName() const;
  //! What matlab command name corresponds to this cell?
  virtual const wxString GetMatlabCommandName() const;
  //! What LaTeX command name corresponds to this cell?
  virtual const wxString GetLaTeXCommandName() const;
  //! What unicode symbol name corresponds to this cell?
  virtual const wxString GetUnicodeSymbol() const;
  //! Returns the data that creates our SVG symbol
  virtual const wxString GetSvgSymbolData() const;
  //! Returns the type our cell has when saving it to .wxmx
  virtual const wxString GetXMLType() const;
  //! How big do we want our svg symbol to be?
  virtual const wxSize GetSymbolSize() const;
  //! The base cell owned by the paren (it's without the paren)
  Cell *Base() const;
  Cell *Over() const {return m_over.get();}
  Cell *Under() const {return m_under.get();}

private:
  std::unique_ptr<Cell> MakeStart(Cell *under) const;
  void MakeBreakUpCells();
  const static wxString m_svgSumSign;

  ParenCell *Paren() const;
  //! The displayed base
  Cell *DisplayedBase() const;

  //! Text that should end up on the clipboard if this cell is copied as text.
  wxString m_altCopyText;
  // The pointers below point to inner cells and must be kept contiguous.
  // ** This is the partial draw list order. All pointers must be the same:
  // ** either Cell * or std::unique_ptr<Cell>. NO OTHER TYPES are allowed.
  std::unique_ptr<Cell> m_open;
  std::unique_ptr<Cell> m_paren;
  std::unique_ptr<Cell> m_comma1;
  std::unique_ptr<Cell> m_var;
  std::unique_ptr<Cell> m_comma2;
  std::unique_ptr<Cell> m_start;
  std::unique_ptr<Cell> m_comma3;
  std::unique_ptr<Cell> m_close;
  std::unique_ptr<Cell> m_over;
  std::unique_ptr<Cell> m_under;
  // The pointers above point to inner cells and must be kept contiguous.

  mutable wxSize m_signSize;

//** Bitfield objects (1 bytes)
//**
  /*! Display m_paren if true, or Base() if false.

    Unlike m_baseNeedsParen this is transient view state, forced to false
    whenever BreakUp() linearizes the cell (the linear form never shows a
    sum sign to disambiguate against, so it never needs the parens either,
    regardless of m_baseNeedsParen) and restored from m_baseNeedsParen by
    Unbreak().
  */
  mutable bool m_displayParen : 1 = true;
  /*! Whether the summand structurally needs the disambiguating parentheses
    (GH #1536) -- see NeedsParen(). Persists across BreakUp()/Unbreak(),
    unlike m_displayParen.
  */
  bool m_baseNeedsParen : 1 = true;
};

#endif // SUMCELL_H
