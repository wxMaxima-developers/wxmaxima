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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//  SPDX-License-Identifier: GPL-2.0+

#ifndef FUNCELL_H
#define FUNCELL_H

#include "Cell.h"
/*! \file

  This file declares the class FunCell() that represents a maxima function.
 */

/*! FunCell represents a maxiam function no special visual representation exists for 

  Examples of functions with special visual representation would be:
   - SqurtCell
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
class FunCell : public Cell
{
public:
  FunCell(Cell *parent, Configuration **config, CellPointers *cellPointers);

  ~FunCell();

  std::list<Cell *> GetInnerCells();

  Cell *Copy();

  void SetName(Cell *base);

  void SetArg(Cell *index);

  void RecalculateHeight(int fontsize);

  void RecalculateWidths(int fontsize);

  virtual void Draw(wxPoint point);

  wxString ToString();

  wxString ToMatlab();

  wxString ToTeX();

  wxString ToMathML();

  wxString ToXML();

  wxString ToOMML();

  bool BreakUp();

  void Unbreak();

protected:
  Cell *m_nameCell;
  Cell *m_argCell;
};


#endif // FUNCELL_H
