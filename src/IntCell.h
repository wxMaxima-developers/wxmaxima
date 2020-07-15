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
  IntCell(GroupCell *parent, Configuration **config);
  IntCell(const IntCell &cell);
  std::unique_ptr<Cell> Copy() const override;

  InnerCellIterator InnerBegin() const override { return InnerCellIterator(&m_base); }
  InnerCellIterator InnerEnd() const override { return ++InnerCellIterator(&m_var); }

  void RecalculateHeight(AFontSize fontsize) override;
  void RecalculateWidths(AFontSize fontsize) override;

  void Draw(wxPoint point) override;

  void SetBase(std::unique_ptr<Cell> &&base);
  //! Set the lower limit of the integral
  void SetUnder(std::unique_ptr<Cell> &&under);
  //! Set the higher limit of the integral
  void SetOver(std::unique_ptr<Cell> &&name);
  //! Set the integration variable
  void SetVar(std::unique_ptr<Cell> &&var);

  enum IntegralType : int8_t
  {
    INT_DEF, //!< A definite integral, meaning an integral with limits.
    INT_IDEF //!> An indefinite integral, meaning an integral without limits
  };

  //! Choose between definite and indefinite integrals
  void SetIntStyle(IntegralType style) { m_intStyle = style; }

  wxString ToMathML() const override;
  wxString ToMatlab() const override;
  wxString ToOMML() const override;
  wxString ToString() const override;
  wxString ToTeX() const override;
  wxString ToXML() const override;

  bool BreakUp() override;
  void SetNextToDraw(Cell *next) override;

  Cell *GetNextToDraw() const override { return m_nextToDraw; }

private:
  CellPtr<Cell> m_nextToDraw;

  // The pointers below point to inner cells and must be kept contiguous.
  // ** All pointers must be the same: either Cell * or std::unique_ptr<Cell>.
  // ** NO OTHER TYPES are allowed.
  //! The part of the formula that is to be integrated.
  std::unique_ptr<Cell> m_base;
  //! The lower limit of the integral
  std::unique_ptr<Cell> m_under;
  //! The upper limit of the integral
  std::unique_ptr<Cell> m_over;
  //! A text cell reading "integrate("
  std::unique_ptr<Cell> m_open;
  //! A text cell reading ")"
  std::unique_ptr<Cell> m_close;
  std::unique_ptr<Cell> m_comma1;
  std::unique_ptr<Cell> m_comma2;
  std::unique_ptr<Cell> m_comma3;
  //! The integration variable
  std::unique_ptr<Cell> m_var;
  // The pointers above point to inner cells and must be kept contiguous.

  //! The height of the integral sign
  int m_signHeight = 35;
  //! The width of the integral sign
  int m_signWidth = 18;
  //! How far is the integral sign's center from the top of this cell?
  int m_signTop = m_signHeight / 2;
#if defined __WXMSW__
  int m_charHeight = 12;
  int m_charWidth = 12;
#endif

  //! Is this integral definitive?
  IntegralType m_intStyle = INT_IDEF;

//** Bitfield objects (0 bytes)
//**
  void InitBitFields()
  { // Keep the initailization order below same as the order
    // of bit fields in this class!
  }
};

#endif  // INTCELL_H
