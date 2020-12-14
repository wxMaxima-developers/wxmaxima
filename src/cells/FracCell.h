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

/*! \file
  This file declares the class FracCell

  FracCell is the Cell type that represents fractions.
*/

#ifndef FRACCELL_H
#define FRACCELL_H

#include "Cell.h"
#include "TextCell.h"
#include "ParenCell.h"

/* This class represents fractions.

   Fractions can be drawn in 2 ways:
     - As a 2D fraction (\f$ \frac{a}{b} \f$) if the fraction is narrow enough to fit
       on the screen, or
     - as a linear division (\f$ a/b \f$) if it doesn't. 
 */
class FracCell final : public Cell
{
public:
  FracCell(GroupCell *parent, Configuration **config,
           std::unique_ptr<Cell> &&num, std::unique_ptr<Cell> &&denom);
  FracCell(const FracCell &cell);
  std::unique_ptr<Cell> Copy() const override;
  const CellTypeInfo &GetInfo() override;

  int GetInnerCellCount() const override { return 3; }
  Cell *GetInnerCell(int index) const override { return (&m_divide)[index]; }

  //! All types of fractions we support
  enum FracType : int8_t
  {
    FC_NORMAL,
    FC_CHOOSE,
    FC_DIFF
  };

  void Recalculate(AFontSize fontsize) override;

  void Draw(wxPoint point) override;

  void SetFracStyle(FracType style) { m_fracStyle = style; }

  //! Answers the question if this is an operator by returning "true".
  bool IsOperator() const override { return true; }

  wxString ToMathML() const override;
  wxString ToMatlab() const override;
  wxString ToOMML() const override;
  wxString ToString() const override;
  wxString ToTeX() const override;
  wxString ToXML() const override;

  //! Fractions in exponents are shown in their linear form.
  void SetExponentFlag() override;

  bool BreakUp() override;

  void SetupBreakUps();

  void SetNextToDraw(Cell *next) override;

private:
  //! Makes the division sign cell, used in linear form - whether when broken
  //! into lines, or when the exponent flag is set.
  void MakeDivideCell();

  //! The numerator
  Cell *Num() const { return m_numParenthesis->GetInner(); }
  //! The denominator
  Cell *Denom() const { return m_denomParenthesis->GetInner(); }

  //! A parenthesis around the numerator, owns the numerator
  std::unique_ptr<ParenCell> const m_numParenthesis;
  //! A parenthesis around the denominator, owns the denominaotr
  std::unique_ptr<ParenCell> const m_denomParenthesis;
  //! The owner of the "/" sign
  std::unique_ptr<TextCell> m_divideOwner;

  // The pointers below point to inner cells and must be kept contiguous.
  // ** All pointers must be the same: either Cell * [const] or std::unique_ptr<Cell>.
  // ** NO OTHER TYPES are allowed.
  //! The "/" sign
  Cell* m_divide = {};
  //! The displayed version of the numerator, if needed with parenthesis
  Cell* m_displayedNum = {};
  //! The displayed version of the denominator, if needed with parenthesis
  Cell* m_displayedDenom = {};
  // The pointers above point to inner cells and must be kept contiguous.

  //! How much wider should the horizontal line be on both ends than num or denom?
  int m_protrusion = 0;
  /*! The horizontal gap between this frac and any minus before it
  
    This gap hinders avoids the horizontal rule of a fraction from building a straight 
    nearly-uninterrupted horizontal line together with a minus. It is only introduced
    if there is an actual minus.
  */
  int m_horizontalGapLeft = 0;
  /*! The horizontal gap between this frac and any minus that follows it
  
    This gap hinders avoids the horizontal rule of a fraction from building a straight 
    nearly-uninterrupted horizontal line together with a minus. It is only introduced
    if there is an actual minus.
  */
  int m_horizontalGapRight = 0;

  //! The way the fraction should be displayed
  FracType m_fracStyle = FC_NORMAL;

//** Bitfield objects (1 bytes)
//**
  void InitBitFields()
  { // Keep the initailization order below same as the order
    // of bit fields in this class!
    m_inExponent = false;
  }
  //! Fractions in exponents are shown in their linear form.
  bool m_inExponent : 1 /* InitBitFields */;
};

#endif // FRACCELL_H
