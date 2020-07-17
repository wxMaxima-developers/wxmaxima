// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
//            (C) 2020      Kuba Ober <kuba@bertec.com>
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

#include "precomp.h"
#include <wx/regex.h>
#include "Cell.h"

/*! A Text cell

  Everything on the worksheet that is composed of characters with the eception
  of input cells: Input cells are handled by EditorCell instead.
 */
// 568 bytes <- 744 bytes
class TextCell : public Cell
{
public:
  TextCell(GroupCell *parent, Configuration **config, const wxString &text = {}, TextStyle style = TS_FUNCTION);
  TextCell(const TextCell &cell);
  std::unique_ptr<Cell> Copy() const override;  

  AFontSize GetScaledTextSize() const;
  
  void SetStyle(TextStyle style) override;
  
  //! Set the text contained in this cell
  void SetValue(const wxString &text) override;

  //! Set the automatic label maxima has assigned the current equation
  void SetUserDefinedLabel(const wxString &userDefinedLabel) { m_userDefinedLabel() = userDefinedLabel; }

  void RecalculateWidths(AFontSize fontsize) override;

  void Draw(wxPoint point) override;

  void SetFont(AFontSize fontsize);

  /*! Calling this function signals that the "(" this cell ends in isn't part of the function name

    The "(" is the opening parenthesis of a function instead.
   */
  void DontEscapeOpeningParenthesis() { m_dontEscapeOpeningParenthesis = true; }

  wxString ToMatlab() const override;
  wxString ToMathML() const override;
  wxString ToOMML() const override;
  wxString ToRTF() const override;
  wxString ToString() const override;
  wxString ToTeX() const override;
  wxString ToXML() const override;

  wxString GetDiffPart() const override;

  bool IsOperator() const override;

  const wxString &GetValue() const override { return m_text; }

  wxString GetGreekStringTeX() const;

  wxString GetSymbolTeX() const;

  wxString GetGreekStringUnicode() const;

  wxString GetSymbolUnicode(bool keepPercent) const;

  bool IsShortNum() const override;

  void SetType(CellType type) override;

  void SetAltCopyText(const wxString &text) { m_altCopyText = text; }

  void SetPromptTooltip(bool use) { m_promptTooltip = use; }

  //! The actual font size for labels (that have a fixed width)
  void SetNextToDraw(Cell *next) override { m_nextToDraw = next; }
  Cell *GetNextToDraw() const override { return m_nextToDraw; }

private:
  //! The text we actually display depends on many factors, unfortunately
  void UpdateDisplayedText();
  //! Update the tooltip for this cell
  void UpdateToolTip();

  void FontsChanged() override
  {
    ResetSize();
    ResetData();
    m_sizeCache.clear();
  }

  bool NeedsRecalculation(AFontSize fontSize) const override;

  enum TextIndex : int8_t
  {
    noText,
    displayedText,
    userLabelText,
    numStart,
    ellipsis,
    numEnd
  };

  struct SizeEntry {
    wxSize textSize;
    AFontSize fontSize;
    TextIndex index;
    SizeEntry(wxSize textSize, AFontSize fontSize, TextIndex index) :
      textSize(textSize), fontSize(fontSize), index(index) {}
    SizeEntry() = default;
  };

  TextIndex GetLabelIndex() const;
  wxString GetTextFor(TextIndex text) const;
  wxSize GetTextSizeFor(wxDC *dc, TextIndex index);

  static wxRegEx m_unescapeRegEx;
  static wxRegEx m_roundingErrorRegEx1;
  static wxRegEx m_roundingErrorRegEx2;
  static wxRegEx m_roundingErrorRegEx3;
  static wxRegEx m_roundingErrorRegEx4;

  //! The user-defined label for this label cell. Reuses m_numEnd since
  //! otherwise it'd be unused for labels.
  wxString &m_userDefinedLabel() { return m_numEnd; }
  const wxString &m_userDefinedLabel() const { return m_numEnd; }

  //! Text that should end up on the clipboard if this cell is copied as text.
  wxString m_altCopyText;
  //! The text we keep inside this cell
  wxString m_text;
  //! The text we display: m_text might be a number that is longer than we want to display
  wxString m_displayedText;

  //! The first few digits
  wxString m_numStart;
  //! The "not all digits displayed" message.
  wxString m_ellipsis;
  //! Last few digits (also used for user defined label)
  wxString m_numEnd;

  std::vector<SizeEntry> m_sizeCache;

  CellPtr<Cell> m_nextToDraw;

  int m_realCenter = -1;

  //! The number of digits we did display the last time we displayed a number.
  int m_displayedDigits_old = -1;

  Configuration::showLabels m_labelChoice_Last = {};

//** Bitfield objects (1 bytes)
//**
  void InitBitFields()
  { // Keep the initailization order below same as the order
    // of bit fields in this class!
    m_dontEscapeOpeningParenthesis = false;
    m_promptTooltip = false;
  }

  //! Is an ending "(" of a function name the opening parenthesis of the function?
  bool m_dontEscapeOpeningParenthesis : 1 /* InitBitFields */;
  //! Default to a special tooltip for prompts?
  bool m_promptTooltip : 1 /* InitBitFields */;
};

#endif // TEXTCELL_H
