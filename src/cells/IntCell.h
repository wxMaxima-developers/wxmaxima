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
// cppcheck-suppress ctuOneDefinitionRuleViolation
class IntCell final : public Cell
{
public:
  IntCell(GroupCell *group, Configuration *config,
          std::unique_ptr<Cell> &&base, std::unique_ptr<Cell> &&lowerLimit,
          std::unique_ptr<Cell> &&upperLimit, std::unique_ptr<Cell> &&var);
  IntCell(GroupCell *group, Configuration *config,
          std::unique_ptr<Cell> &&base, std::unique_ptr<Cell> &&var);
  IntCell(GroupCell *group, const IntCell &cell);
  std::unique_ptr<Cell> Copy(GroupCell *group) const override;
  const CellTypeInfo &GetInfo() override;

  size_t GetInnerCellCount() const override { return 9; }
  // cppcheck-suppress objectIndex
  Cell *GetInnerCell(size_t index) const override { return (&m_open)[index].get(); }

  void Recalculate(AFontSize fontsize) override;

  void Draw(wxPoint point, wxDC *dc, wxDC *antialiassingDC) override;

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

  //! Does this integral have limits?
  bool HasLimits() const {return (m_intStyle == INT_DEF) &&
      (m_upperLimit != NULL) && (m_lowerLimit != NULL); }
  
private:
  void DrawSvgSign(wxDC *dc, wxPoint pos);
  void DrawHanddrawnSign(wxDC *dc, wxPoint pos);
  /*! Do we want to use a SVG integral sign?

    Is constexpr, which means: This is evaluated at compile time.
   */
  constexpr bool UseSvgSign() const {
#ifdef WXM_WITHOUT_SVG_SIGNS
    return false;
#else
    return true;
#endif    
  }
  void MakeBreakUpCells();
  const static wxString m_svgIntegralSign;
  // The pointers below point to inner cells and must be kept contiguous.
  // ** This is the draw list order. All pointers must be the same:
  // ** either Cell * or std::unique_ptr<Cell>. NO OTHER TYPES are allowed.
  //! A text cell reading "integrate("
  std::unique_ptr<Cell> m_open;
  //! The part of the formula that is to be integrated.
  std::unique_ptr<Cell> m_base;
  std::unique_ptr<Cell> m_comma1;
  //! The integration variable
  std::unique_ptr<Cell> m_var;
  std::unique_ptr<Cell> m_comma2;
  //! The lower limit of the integral
  std::unique_ptr<Cell> m_lowerLimit;
  std::unique_ptr<Cell> m_comma3;
  //! The upper limit of the integral
  std::unique_ptr<Cell> m_upperLimit;
  //! A text cell reading ")"
  std::unique_ptr<Cell> m_close;
  // The pointers above point to inner cells and must be kept contiguous.

  //! The height of the integral sign
  wxCoord m_signHeight = 35;
  //! The width of the integral sign
  wxCoord m_signWidth = 18;
  //! How far is the integral sign's center from the top of this cell?
  wxCoord m_signTop = m_signHeight / 2;
#if defined __WXMSW__
  wxCoord m_charHeight = 12;
  wxCoord m_charWidth = 12;
#endif

  //! Is this integral definitive?
  IntegralType m_intStyle = INT_IDEF;

//** Bitfield objects (0 bytes)
//**
  static void InitBitFields()
    { // Keep the initialization order below same as the order
      // of bit fields in this class!
    }
};

#endif  // INTCELL_Hs.
