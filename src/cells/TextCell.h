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
#include "UrlDetection.h"
#include <vector>

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
  virtual ~TextCell(){}
  virtual const CellTypeInfo &GetInfo() override;
  std::unique_ptr<Cell> Copy(GroupCell *group) const override;

  AFontSize GetScaledTextSize() const;

  void SetStyle(TextStyle style) override;

  //! Set the text contained in this cell
  void SetValue(const wxString &text) override;

  virtual void Recalculate(const AFontSize fontsize) const override;

  using Cell::SetCurrentPoint;
  void SetCurrentPoint(wxPoint point) const override;
  void Draw(wxDC *dc, wxDC *antialiassingDC) override;
  const wxFont &GetFont(AFontSize fontsize) const {
    return m_configuration->GetStyle(GetTextStyle())->GetFont(fontsize);
  }
  //cppcheck-suppress functionConst
  void SetFont(wxDC *dc, AFontSize fontsize) const;

  wxCoord GetWidthAtLineBreak() const override;

  /*! Calling this function signals that the "(" this cell ends in isn't part of the function name

    The "(" is the opening parenthesis of a function instead.
  */
  void DontEscapeOpeningParenthesis() { m_dontEscapeOpeningParenthesis = true; }

  wxString ToMatlab() const override;
  wxString ToMathML() const override;
  wxString ToOMML() const override;
  wxString ToRTF() const override;
  virtual wxString ToString() const override;
  virtual const wxString GetDisplayedString() const override {return m_displayedText;}
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

  /*! The link drawn at point, if any (GH #2396).

    A string Maxima prints -- print("See https://...") or an error message
    naming a web page -- shows its http://, https:// and mailto: addresses as
    links, just as a text cell does. Free for a cell without links, which is
    known from the moment its text is set.
  */
  wxString GetLinkAt(wxPoint point) override;

protected:
  mutable wxString m_altCopyText;
  //! Returns the XML flags this cell needs in wxMathML
  wxString GetXMLFlags() const override;
  //! The text we actually display depends on many factors, unfortunately
  virtual void UpdateDisplayedText() const;
  /*! The links in m_displayedText.

    Only those also found in m_text verbatim count: UpdateDisplayedText()
    replaces "->" by an arrow, and a link has to open what Maxima printed,
    not what it looks like on screen.
  */
  std::vector<wxm::UrlSpan> LinkSpans() const;
  /*! Walks m_displayedText as Draw() paints it: plain runs and links.

    func(text, x, width, isLink) is called for each run from left to right,
    x being where the run starts. Draw() and GetLinkAt() both use this, so
    what is painted as a link and what is found under the pointer can't
    disagree.
  */
  template <typename RunFunc> void WalkTextRuns(wxDC *dc, wxCoord x, RunFunc &&func) const;
  //! Update the tooltip for this cell
  void UpdateToolTip();
  const wxString &GetAltCopyText() const override { return m_altCopyText; }

  void FontsChanged() const override
    {
      m_sizeCache.clear();
    }

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

  wxSize CalculateTextSize(wxDC *dc, const wxString &text, TextCell::TextIndex const index) const;

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
  mutable wxString m_displayedText;
  mutable std::vector<SizeEntry> m_sizeCache;
  //! The width this cell has if it is at a line break, see GetWidthAtLineBreak()
  mutable wxCoord m_widthAtLineBreak = 0;

//** Bitfield objects (1 bytes)
//**
  //! Is an ending "(" of a function name the opening parenthesis of the function?
  bool m_dontEscapeOpeningParenthesis : 1 = false;
  //! Default to a special tooltip for prompts?
  bool m_promptTooltip : 1 = false;
  //! Does m_displayedText contain a link? Keeps Draw() a single DrawText() if not.
  mutable bool m_hasLinks : 1 = false;
};

#endif // TEXTCELL_H
