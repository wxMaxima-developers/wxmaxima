///
///  Copyright (C) 2004-2014 Andrej Vodopivec <andrej.vodopivec@gmail.com>
///
///  This program is free software; you can redistribute it and/or modify
///  it under the terms of the GNU General Public License as published by
///  the Free Software Foundation; either version 2 of the License, or
///  (at your option) any later version.
///
///  This program is distributed in the hope that it will be useful,
///  but WITHOUT ANY WARRANTY; without even the implied warranty of
///  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
///  GNU General Public License for more details.
///
///
///  You should have received a copy of the GNU General Public License
///  along with this program; if not, write to the Free Software
///  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
///

#ifndef INTCELL_H
#define INTCELL_H

#include "MathCell.h"
#include "Setup.h"

/*! This class represents an integral

  This class represents an integral including the integral sign and its contents.
 */
class IntCell : public MathCell
{
public:
  IntCell();
  ~IntCell();
  MathCell* Copy(bool all);
  void Destroy();
  void RecalculateSize(CellParser& parser, int fontsize, bool all);
  void RecalculateWidths(CellParser& parser, int fontsize, bool all);
  void Draw(CellParser& parser, wxPoint point, int fontsize, bool all);
  void SetBase(MathCell* base);
  //! Set the lower limit of the integral
  void SetUnder(MathCell* under);
  //! Set the higher limit of the integral
  void SetOver(MathCell* name);
  //! Set the integration variable 
  void SetVar(MathCell* var);
  enum IntegralType{
    INT_DEF, //!< An definite integral, meaning an integral with limits.
    INT_IDEF //!> An indefinite integral, meaning an integral without limits
  };
  //! Choose between definite and indefinite integrals
  void SetIntStyle(IntegralType style)
  {
    m_intStyle = style;
  }
  wxString ToString(bool all);
  wxString ToTeX(bool all);
  wxString ToXML(bool all);
  void SelectInner(wxRect& rect, MathCell** first, MathCell** last);
  void SetParent(MathCell *parent, bool all);

 protected:
  //! The part of the formula that is to be integrated.
  MathCell *m_base;
  //! The lower limit of the integral
  MathCell *m_under;
  //! The upper limit of the integral
  MathCell *m_over;
  //! The integration variable
  MathCell *m_var;
  //! The height of the integral sign
  int m_signSize;
  //! The width of the integral sign
  int m_signWidth;
  //! Is this integral definitive?
  IntegralType m_intStyle;
  //! How far is the integral sign's center from the top of this cell?
  int m_signTop;
#if defined __WXMSW__
  int m_charHeight, m_charWidth;
#endif

private:

};

#endif  // INTCELL_H
