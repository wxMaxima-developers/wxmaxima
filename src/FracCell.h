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
  FracCell(Cell *parent, Configuration **config, CellPointers *cellPointers);
  FracCell(const FracCell &cell);
  Cell *Copy() override { return new FracCell(*this); }
  ~FracCell();

  InnerCellIterator InnerBegin() const override { return InnerCellIterator(&m_divide); }
  InnerCellIterator InnerEnd() const override { return ++InnerCellIterator(&m_displayedDenom); }

  //! All types of fractions we support
  enum FracType
  {
    FC_NORMAL,
    FC_CHOOSE,
    FC_DIFF
  };

  void RecalculateHeight(int fontsize) override;

  void RecalculateWidths(int fontsize) override;

  void Draw(wxPoint point) override;

  void SetFracStyle(int style) { m_fracStyle = style; }

  //! Set the numerator for the fraction
  void SetNum(Cell *num);

  //! Set the denominator of the fraction
  void SetDenom(Cell *denom);

  //! Answers the question if this is an operator by returning "true".
  bool IsOperator() const override { return true; }

  wxString ToString() override;

  wxString ToMatlab() override;

  wxString ToTeX() override;

  wxString ToMathML() override;

  wxString ToOMML() override;

  wxString ToXML() override;

  //! Fractions in exponents are shown in their linear form.
  void SetExponentFlag() override;

  bool BreakUp() override;

  void SetupBreakUps();

  void SetNextToDraw(Cell *next) override;
  Cell *GetNextToDraw() const override { return m_nextToDraw; }

private:
  Cell *m_nextToDraw;

  //! The numerator
  std::shared_ptr<Cell> m_num;
  //! The denominator
  std::shared_ptr<Cell> m_denom;
  //! A parenthesis around the numerator
  std::shared_ptr<ParenCell> m_numParenthesis;
  //! A parenthesis around the denominator
  std::shared_ptr<ParenCell> m_denomParenthesis;
  //! The last element of the numerator
  Cell *m_num_Last;
  //! The last element of the denominator
  Cell *m_denom_Last;
  //! Fractions in exponents are shown in their linear form.
  bool m_exponent;
  // The pointers below point to inner cells and must be kept contiguous.
  //! The "/" sign
  std::shared_ptr<Cell> m_divide;
  //! The displayed version of the numerator, if needed with parenthesis
  std::shared_ptr<Cell> m_displayedNum;
  //! The displayed version of the denominator, if needed with parenthesis
  std::shared_ptr<Cell> m_displayedDenom;
  //! The way the fraction should be displayed
  int m_fracStyle;
  //! How much wider should the horizontal line be on both ends than num or denom?
  int m_protrusion;
  /*! The horizontal gap between this frac and any minus before it
  
    This gap hinders avoids the horizontal rule of a fraction from building a straight 
    nearly-uninterrupted horizontal line together with a minus. It is only introduced
    if there is an actual minus.
  */
  int m_horizontalGapLeft;
  /*! The horizontal gap between this frac and any minus that follows it
  
    This gap hinders avoids the horizontal rule of a fraction from building a straight 
    nearly-uninterrupted horizontal line together with a minus. It is only introduced
    if there is an actual minus.
  */
  int m_horizontalGapRight;
};

#endif // FRACCELL_H
