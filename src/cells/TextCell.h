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

#include <wx/regex.h>
#include "Cell.h"

/*! A Text cell

  Everything on the worksheet that is composed of characters with the exception
  of input cells: Input cells are handled by EditorCell instead.
*/
// 304 bytes <- 744 bytes
// cppcheck-suppress ctuOneDefinitionRuleViolation
class TextCell : public Cell
{
public:
  TextCell(GroupCell *group, Configuration *config, const wxString &text = {}, TextStyle style = TS_FUNCTION);
  TextCell(GroupCell *group, const TextCell &cell);
  virtual const CellTypeInfo &GetInfo() override;
  std::unique_ptr<Cell> Copy(GroupCell *group) const override;

  AFontSize GetScaledTextSize() const;

  void SetStyle(TextStyle style) override;

  //! Set the text contained in this cell
  void SetValue(const wxString &text) override;

  virtual void Recalculate(AFontSize fontsize) override;

  void Draw(wxPoint point, wxDC *dc, wxDC *antialiassingDC) override;
  const wxFont &GetFont(AFontSize fontsize) const {
    return m_configuration->GetStyle(GetTextStyle())->GetFont(fontsize);
  }
  //cppcheck-suppress functionConst
  void SetFont(wxDC *dc, AFontSize fontsize);

  /*! Calling this function signals that the "(" this cell ends in isn't part of the function name

    The "(" is the opening parenthesis of a function instead.
  */
  void DontEscapeOpeningParenthesis() { m_dontEscapeOpeningParenthesis = true; }

  wxString ToMatlab() const override;
  wxString ToMathML() const override;
  wxString ToOMML() const override;
  wxString ToRTF() const override;
  virtual wxString ToString() const override;
  wxString ToTeX() const override;
  wxString ToXML() const override;

  bool IsOperator() const override;

  const wxString &GetValue() const override { return m_text; }

  wxString GetGreekStringTeX() const;

  wxString GetSymbolTeX() const;

  wxString GetGreekStringUnicode() const;

  wxString GetSymbolUnicode(bool keepPercent) const;

  bool IsShortNum() const override;

  void SetType(CellType type) override;

  void SetAltCopyText(const wxString &text) override {m_altCopyText = text;}

  void SetPromptTooltip(bool use) { m_promptTooltip = use; }

protected:
  mutable wxString m_altCopyText;
  //! Returns the XML flags this cell needs in wxMathML
  virtual wxString GetXMLFlags() const;
  //! The text we actually display depends on many factors, unfortunately
  virtual void UpdateDisplayedText();
  //! Update the tooltip for this cell
  void UpdateToolTip();
  const wxString &GetAltCopyText() const override { return m_altCopyText; }

  void FontsChanged() override
    {
      ResetSize();
      ResetData();
      m_sizeCache.clear();
    }

  virtual bool NeedsRecalculation(AFontSize fontSize) const override;

  enum TextIndex : int8_t
  {
    noText,
    cellText,
    userLabelText,
    numberStart,
    ellipsis,
    numberEnd
  };

  struct SizeEntry {
    wxSize textSize;
    AFontSize fontSize;
    TextIndex index = cellText;
    SizeEntry(wxSize textSize, AFontSize fontSize, TextIndex index) :
      textSize(textSize), fontSize(fontSize), index(index) {}
    SizeEntry() = default;
  };

  wxSize CalculateTextSize(wxDC *dc, const wxString &text, TextCell::TextIndex const index);

  static wxRegEx m_unescapeRegEx;
  static wxRegEx m_roundingErrorRegEx1;
  static wxRegEx m_roundingErrorRegEx2;
  static wxRegEx m_roundingErrorRegEx3;
  static wxRegEx m_roundingErrorRegEx4;

//** Large objects (120 bytes)
//**
  //! The text we keep inside this cell
  wxString m_text;
  //! The text we display: We might want to convert some characters or do similar things
  wxString m_displayedText;
  std::vector<SizeEntry> m_sizeCache;

//** Bitfield objects (1 bytes)
//**
  void InitBitFields()
    { // Keep the initialization order below same as the order
      // of bit fields in this class!
      m_dontEscapeOpeningParenthesis = false;
      m_promptTooltip = false;
      m_keepPercent_last = m_configuration->CheckKeepPercent();
    }

  //! Is an ending "(" of a function name the opening parenthesis of the function?
  bool m_dontEscapeOpeningParenthesis : 1 /* InitBitFields */;
  //! Default to a special tooltip for prompts?
  bool m_promptTooltip : 1 /* InitBitFields */;
  //! The last known value of CheckKeepPercent
  bool m_keepPercent_last : 1;

};

#endif // TEXTCELL_H
