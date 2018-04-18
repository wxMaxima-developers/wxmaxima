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
  This file declares the class FracCell

  FracCell is the MathCell type that represents fractions.
*/

#ifndef FRACCELL_H
#define FRACCELL_H

#include "MathCell.h"
#include "TextCell.h"

/* This class represents fractions.

   Fractions can be drawn in 2 ways:
     - As a 2D fraction (\f$ \frac{a}{b} \f$) if the fraction is narrow enough to fit
       on the screen, or
     - as a linear division (\f$ a/b \f$) if it doesn't. 
 */
class FracCell : public MathCell
{
public:
  FracCell(MathCell *parent, Configuration **config, CellPointers *cellpointers);

  ~FracCell();
  
  std::list<MathCell *> GetInnerCells();

  //! All types of fractions we supportx
  enum FracType
  {
    FC_NORMAL,
    FC_CHOOSE,
    FC_DIFF
  };

  MathCell *Copy();

  void RecalculateHeight(int fontsize);

  void RecalculateWidths(int fontsize);

  void Draw(wxPoint point, int fontsize);

  void SetFracStyle(int style)
  {
    m_fracStyle = style;
  }

  //! Set the nummerator for the fraction
  void SetNum(MathCell *num);

  //! Set the denominator of the fraction
  void SetDenom(MathCell *denom);

  //! Answers the question if this is an operator by returning "true".
  bool IsOperator()
  {
    return true;
  }

  wxString ToString();

  wxString ToTeX();

  wxString ToMathML();

  wxString ToOMML();

  wxString ToXML();

  void SetExponentFlag();

  bool BreakUp();

  void SetupBreakUps();

  void Unbreak();

  void SetGroup(MathCell *parent);

protected:
  //! The "/".
  TextCell *m_divSign;
  //! The nummerator
  MathCell *m_num;
  //! The denominator
  MathCell *m_denom;
  MathCell *m_open1, *m_open2, *m_close1, *m_close2, *m_divide;
  MathCell *m_last1, *m_last2;
  bool m_exponent;
  int m_fracStyle;
  //! How much wider should the horizontal line be on both ends than num or denom?
  int m_protrusion;
  int m_expDivideWidth;
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
