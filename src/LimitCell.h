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

/*! \file
  This file declares the class LimitCell

  LimitCell is the Cell type that represents maxima's <code>limit()</code> command.
*/

#ifndef LIMITCELL_H
#define LIMITCELL_H

#include "Cell.h"
#include "TextCell.h"

class LimitCell : public Cell
{
public:
  LimitCell(Cell *parent, Configuration **config, CellPointers *cellPointers);

  ~LimitCell();

  std::list<Cell *> GetInnerCells();

  Cell *Copy();

  void RecalculateHeight(int fontsize);

  void RecalculateWidths(int fontsize);

  virtual void Draw(wxPoint point);

  void SetBase(Cell *base);

  void SetUnder(Cell *under);

  void SetName(Cell *name);

  wxString ToString();

  wxString ToMatlab();

  wxString ToTeX();

  wxString ToXML();

  wxString ToOMML();

  wxString ToMathML();

  void Unbreak();
  bool BreakUp();

protected:
  Cell *m_name;
  TextCell *m_open;
  Cell *m_base;
  TextCell *m_comma;
  Cell *m_under;
  TextCell *m_close;
  Cell *m_name_last;
  Cell *m_base_last;
  Cell *m_under_last;
};

#endif // LIMITCELL_H
