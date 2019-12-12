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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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

#include "Cell.h"

enum
{
  SM_SUM,
  SM_PROD
};

class SumCell : public Cell
{
public:
  SumCell(Cell *parent, Configuration **config, CellPointers *cellPointers);
  SumCell(const SumCell &cell);
  Cell *Copy() override {return new SumCell(*this);}

  ~SumCell();

    //! This class can be derived from wxAccessible which has no copy constructor
  SumCell operator=(const SumCell&) = delete;

  std::list<Cell *> GetInnerCells() override;
  
  void RecalculateHeight(int fontsize) override;
  void RecalculateWidths(int fontsize) override;

  virtual void Draw(wxPoint point) override;

  void SetBase(Cell *base);

  void SetUnder(Cell *under);

  void SetOver(Cell *over);

  void SetSumStyle(int style)
  {
    m_sumStyle = style;
  }

  wxString ToString() override;

  wxString ToMatlab() override;

  wxString ToTeX() override;

  wxString ToMathML() override;

  wxString ToXML() override;

  wxString ToOMML() override;

protected:
  std::unique_ptr<Cell> m_base;
  std::unique_ptr<Cell> m_under;
  std::unique_ptr<Cell> m_over;
  int m_signSize;
  int m_signWidth;
  int m_sumStyle;
  int m_signWCenter;
  int m_signTop;
};

#endif // SUMCELL_H
