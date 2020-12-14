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

#ifndef FUNCELL_H
#define FUNCELL_H

#include "Cell.h"
/*! \file

  This file declares the class FunCell() that represents a maxima function.
 */

/*! FunCell represents a maxima function no special visual representation exists for

  Examples of functions with special visual representation would be:
   - SqrtCell
   - ExptCell
   - AbsCell
   - ConjugateCell

  In the case that this cell is broken into two lines in the order of
  m_nextToDraw this cell is represented by the following individual 
  cells:
  
   - The FunCell itself
   - The function name"
   - The ParenCell containing its contents
   - The closing ")".
   
  If it isn't broken into multiple cells m_nextToDraw points to the 
  cell that follows this Cell. 
*/
class FunCell final : public Cell
{
public:
  FunCell(GroupCell *parent, Configuration **config,
          std::unique_ptr<Cell> &&name, std::unique_ptr<Cell> &&arg);
  FunCell(const FunCell &cell);
  std::unique_ptr<Cell> Copy() const override;
  const CellTypeInfo &GetInfo() override;

  int GetInnerCellCount() const override { return 2; }
  Cell *GetInnerCell(int index) const override { return (&m_nameCell)[index].get(); }

  void Recalculate(AFontSize fontsize) override;

  void Draw(wxPoint point) override;

  wxString ToMathML() const override;
  wxString ToMatlab() const override;
  wxString ToOMML() const override;
  wxString ToString() const override;
  wxString ToTeX() const override;
  wxString ToXML() const override;

  void SetAltCopyText(const wxString &text) override { m_altCopyText = text; }
  const wxString &GetAltCopyText() const override { return m_altCopyText; }

  bool BreakUp() override;

  void SetNextToDraw(Cell *next) override;

private:
  //! Text that should end up on the clipboard if this cell is copied as text.
  wxString m_altCopyText;

  // The pointers below point to inner cells and must be kept contiguous.
  // ** All pointers must be the same: either Cell * or std::unique_ptr<Cell>.
  // ** NO OTHER TYPES are allowed.
  std::unique_ptr<Cell> m_nameCell;
  std::unique_ptr<Cell> m_argCell;
  // The pointers above point to inner cells and must be kept contiguous.

//** Bitfield objects (0 bytes)
//**
  void InitBitFields()
  { // Keep the initailization order below same as the order
    // of bit fields in this class!
  }
};

#endif // FUNCELL_H
