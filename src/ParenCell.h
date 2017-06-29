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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//

/*! \file
  This file declares the class ParenCell

  ParenCell is the MathCell type that represents a math element that is kept
  between parenthesis.
 */

#ifndef PARENCELL_H
#define PARENCELL_H

#include "MathCell.h"
#include "CellPointers.h"
#include "Setup.h"

/*! The class that represents parenthesis that are wrapped around text

  In the case that this cell is broken into two lines in the order of
  m_nextToDraw this cell is represented by the following individual 
  cells:
  
   - The ParenCell itself
   - The opening "("
   - The contents
   - The closing ")".
   
  If it isn't broken into multiple cells m_nextToDraw points to the 
  cell that follows this Cell.
 */
class ParenCell : public MathCell
{
public:
  ParenCell(MathCell *parent, Configuration **config, CellPointers *cellPointers);

  ~ParenCell();

  void MarkAsDeleted();

  virtual wxString GetToolTip(const wxPoint &point){
    if(ContainsPoint(point))
      {
        // Default assumption: will be overwritten by the next command,
        // if there is a more accurate solution.
        m_cellPointers->m_cellUnderPointer = this;
      }
    return GetToolTipList(point,m_innerCell);
  }

  MathCell *Copy();

  void SetInner(MathCell *inner, int style);

  void SetPrint(bool print)
  {
    m_print = print;
  }

  void SelectInner(wxRect &rect, MathCell **first, MathCell **last);

  void RecalculateHeight(int fontsize);

  void RecalculateWidths(int fontsize);

  void Draw(wxPoint point, int fontsize);

  bool BreakUp();

  void Unbreak();

  wxString ToString();

  wxString ToTeX();

  wxString ToMathML();

  wxString ToOMML();

  wxString ToXML();

  void SetParent(MathCell *parent);

protected:
  int m_fontSize;
  void SetFont(int fontsize);
  MathCell *m_innerCell, *m_open, *m_close;
  MathCell *m_last1;
  bool m_print;
  int m_numberOfExtensions;
  int m_charWidth, m_charHeight;
  int m_charWidth1, m_charHeight1;
  int m_parenFontSize, m_signWidth, m_signHeight, m_signTopHeight, m_signBotHeight, m_extendHeight;
  enum parenthesisStyle
  {
    ascii,              //!< An ordinary parenthesis sign created using ascii characters
    assembled_unicode,  //!< A "big parenthesis" assembled of unicode characters
    assembled_unicode_fallbackfont,  //!< A "big parenthesis" assembled of unicode characters of the fallback font, Freetype version
    assembled_unicode_fallbackfont2,  //!< A "big parenthesis" assembled of unicode characters of the fallback font, Opentype version
    handdrawn           //!< A  parenthesis sign that was created using draw commands
  };

  /* How to create a big parenthesis sign?
     - 0 = 
   */
  parenthesisStyle m_bigParenType;

private:
  CellPointers *m_cellPointers;
};

#endif // PARENCELL_H
