// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2014-2016 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file declares the class IntCell

  IntCell is the Cell type that represents maxima's <code>integrate()</code> command.
*/


#ifndef INTCELL_H
#define INTCELL_H

#include "Cell.h"

/*! This class represents an integral

  This class represents an integral including the integral sign and its contents.
 */
class IntCell final : public Cell
{
public:
  IntCell(Cell *parent, Configuration **config, CellPointers *cellPointers);
  IntCell(const IntCell &cell);
  Cell *Copy() override {return new IntCell(*this);}
  ~IntCell();

  InnerCellIterator InnerBegin() const override { return InnerCellIterator(&m_base); }
  InnerCellIterator InnerEnd() const override { return ++InnerCellIterator(&m_var); }

  void RecalculateHeight(int fontsize) override;

  void RecalculateWidths(int fontsize) override;

  void Draw(wxPoint point) override;

  void SetBase(Cell *base);

  //! Set the lower limit of the integral
  void SetUnder(Cell *under);

  //! Set the higher limit of the integral
  void SetOver(Cell *name);

  //! Set the integration variable
  void SetVar(Cell *var);

  enum IntegralType
  {
    INT_DEF, //!< A definite integral, meaning an integral with limits.
    INT_IDEF //!> An indefinite integral, meaning an integral without limits
  };

  //! Choose between definite and indefinite integrals
  void SetIntStyle(IntegralType style)
  {
    m_intStyle = style;
  }

  wxString ToString() override;

  wxString ToMatlab() override;

  wxString ToTeX() override;

  wxString ToMathML() override;

  wxString ToOMML() override;

  wxString ToXML() override;

  void SetNextToDraw(Cell *next) override;

  Cell *GetNextToDraw() const override {return m_nextToDraw;}

private:
  Cell *m_nextToDraw;

  // The pointers below point to inner cells and must be kept contiguous.
  //! The part of the formula that is to be integrated.
  std::shared_ptr<Cell> m_base;
  //! The lower limit of the integral
  std::shared_ptr<Cell> m_under;
  //! The upper limit of the integral
  std::shared_ptr<Cell> m_over;
  //! The integration variable
  std::shared_ptr<Cell> m_var;
  //! The height of the integral sign
  int m_signHeight;
  //! The width of the integral sign
  int m_signWidth;
  //! Is this integral definitive?
  IntegralType m_intStyle;
  //! How far is the integral sign's center from the top of this cell?
  int m_signTop;
  int m_charHeight, m_charWidth;
};

#endif  // INTCELL_H
