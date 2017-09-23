// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2007-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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

#ifndef SUBSUPCELL_H
#define SUBSUPCELL_H

#include "MathCell.h"

class SubSupCell : public MathCell
{
public:
  SubSupCell(MathCell *parent, Configuration **config, CellPointers *cellPointers);

  ~SubSupCell();

  std::list<MathCell *> GetInnerCells();
  void MarkAsDeleted();

  virtual wxString GetToolTip(const wxPoint &point){
    if(ContainsPoint(point))
      {
        // Default assumption: will be overwritten by the next command,
        // if there is a more accurate solution.
        m_cellPointers->m_cellUnderPointer = this;
      }
    return GetToolTipList(point, m_baseCell, m_exptCell, m_indexCell);
  }

  MathCell *Copy();

  void SetBase(MathCell *base);

  void SetIndex(MathCell *index);

  void SetExponent(MathCell *expt);

  void RecalculateHeight(int fontsize);

  void RecalculateWidths(int fontsize);

  void Draw(wxPoint point, int fontsize);

  wxString ToString();

  wxString ToTeX();

  wxString ToXML();

  wxString ToOMML();

  wxString ToMathML();

  void SelectInner(wxRect &rect, MathCell **first, MathCell **last);

  void SetGroup(MathCell *parent);

protected:
  MathCell *m_baseCell;
  MathCell *m_exptCell;
  MathCell *m_indexCell;

private:
  CellPointers *m_cellPointers;
};

#endif // SUBSUPCELL_H
