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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

#ifndef TEXTCELL_H
#define TEXTCELL_H

#include "wx/regex.h"
#include "Cell.h"

/*! A Text cell

  Everything on the worksheet that is composed of characters with the eception
  of input cells: Input cells are handled by EditorCell instead.
 */
class TextCell final : public Cell
{
private:
  //! Is an ending "(" of a function name the opening parenthesis of the function?
  bool m_dontEscapeOpeningParenthesis;

public:
  TextCell(Cell *parent, Configuration **config, CellPointers *cellPointers, wxString text = {}, TextStyle style = TS_FUNCTION);
  TextCell(const TextCell &cell);
  Cell *Copy() override { return new TextCell(*this); }  
  ~TextCell();  

  double GetScaledTextSize() const;
  
  void SetStyle(TextStyle style) override;
  
  //! Set the text contained in this cell
  void SetValue(const wxString &text) override;

  //! Set the automatic label maxima has assigned the current equation
  void SetUserDefinedLabel(wxString userDefinedLabel){m_userDefinedLabel = userDefinedLabel;}

  void RecalculateWidths(int fontsize) override;

  void Draw(wxPoint point) override;

  void SetFont(int fontsize);

  /*! Calling this function signals that the "(" this cell ends in isn't part of the function name

    The "(" is the opening parenthesis of a function instead.
   */
  void DontEscapeOpeningParenthesis() { m_dontEscapeOpeningParenthesis = true; }

  wxString ToString() override;

  wxString ToMatlab() override;

  wxString ToTeX() override;

  wxString ToMathML() override;

  wxString ToOMML() override;

  wxString ToRTF() override;

  wxString ToXML() override;

  wxString GetDiffPart() override;

  bool IsOperator() const override;

  wxString GetValue() const override { return m_text; }

  wxString GetGreekStringTeX() const;

  wxString GetSymbolTeX() const;

  wxString GetGreekStringUnicode() const;

  wxString GetSymbolUnicode(bool keepPercent) const;

  bool IsShortNum() override;

  void SetType(CellType type) override;

private:
  wxSize GetTextSize(wxString const &text);
  void SetAltText();

  void FontsChanged() override
    {
      ResetSize();
      ResetData();
      m_widths.clear();
    }

  //! Resets the font size to label size
  void SetFontSizeForLabel(wxDC *dc);

  bool NeedsRecalculation(int fontSize) override;
  static wxRegEx m_unescapeRegEx;
  static wxRegEx m_roundingErrorRegEx1;
  static wxRegEx m_roundingErrorRegEx2;
  static wxRegEx m_roundingErrorRegEx3;
  static wxRegEx m_roundingErrorRegEx4;

  //! The text we keep inside this cell
  wxString m_text;
  //! The text we keep inside this cell
  wxString m_userDefinedLabel;
  //! The text we display: m_text might be a number that is longer than we want to display
  wxString m_displayedText;
  wxString m_altText, m_altJsText;
  wxString m_fontname, m_texFontname;

  int m_realCenter;
  /*! The font size we had the last time we were recalculating this cell

    If a fraction or similar is broken into two lines this changes \f$ \frac{a}{b}\f$ to 
    \f$ a/b\f$. \f$ \Longrightarrow\f$ we need a mechanism that tells us that the font 
    size has changed and we need to re-calculate the text width.
   */
  double m_lastCalculationFontSize;
  //! The actual font size for labels (that have a fixed width)
  double m_fontSizeLabel;
  void SetNextToDraw(Cell *next) override { m_nextToDraw = next; }
  Cell *GetNextToDraw() const override { return m_nextToDraw; }


  Cell *m_nextToDraw = {};
  class SizeHash_internals
  {
  public:
    SizeHash_internals() { }
    unsigned long operator()( const double& k ) const
      {
        return k * 1000000;
      }
    SizeHash_internals& operator=(const SizeHash_internals&) { return *this; }
  };
  // comparison operator
  class DoubleEqual
  {
  public:
    DoubleEqual() { }
    bool operator()( const double& a, const double& b ) const
      {
        return fabs(a-b) < .001;
      }
    DoubleEqual& operator=(const DoubleEqual&) { return *this; }
  };
  WX_DECLARE_HASH_MAP(
    double, wxSize, SizeHash_internals, DoubleEqual, SizeHash);
  //! Remembers all widths of the full text we already have configured
  SizeHash m_widths;
  //! The size of the first few digits
  SizeHash m_numstartWidths;
  wxSize m_numStartWidth;
  wxString m_numStart;
  //! The size of the "not all digits displayed" message.
  SizeHash m_ellipsisWidths;
  wxString m_ellipsis;
  wxSize m_ellipsisWidth;
  //! The size of the last few digits
  SizeHash m_numEndWidths;
  wxString m_numEnd;
  wxSize m_numEndWidth;

  //! Produces a text sample that determines the label width
  wxString m_initialToolTip;
  //! The number of digits we did display the last time we displayed a number.
  int m_displayedDigits_old;
};

#endif // TEXTCELL_H
