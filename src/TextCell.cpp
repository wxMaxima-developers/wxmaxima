﻿// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
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
  This file defines the class TextCell

  TextCell is the Cell type that is used in order to display text that is
  contained in maxima's output.
 */

#include "TextCell.h"
#include "wx/config.h"

TextCell::TextCell(Cell *parent, Configuration **config, CellPointers *cellPointers, wxString text) : Cell(parent, config)
{
  m_cellPointers = cellPointers;
  m_displayedDigits_old = -1;
  m_height = -1;
  m_realCenter = m_center = -1;
  m_lastCalculationFontSize = -1;
  m_fontSize = -1;
  m_fontSizeLabel = -1;
  m_lastZoomFactor = -1;
  SetValue(text);
  m_highlight = false;
  m_dontEscapeOpeningParenthesis = false;
  m_initialToolTip = (*m_configuration)->GetDefaultCellToolTip();
  m_fontsize_old = -1;
}

TextCell::~TextCell()
{
  MarkAsDeleted();
}

std::list<Cell *> TextCell::GetInnerCells()
{
  std::list<Cell *> innerCells;
  return innerCells;
}

void TextCell::SetStyle(TextStyle style)
{
  Cell::SetStyle(style);
  if ((m_text == wxT("gamma")) && (m_textStyle == TS_FUNCTION))
    m_displayedText = wxT("\x0393");
  if ((m_text == wxT("psi")) && (m_textStyle == TS_FUNCTION))
    m_displayedText = wxT("\x03A8");
  if((style == TS_LABEL) || (style == TS_USERLABEL)||
     (style == TS_MAIN_PROMPT) || (style == TS_OTHER_PROMPT))
    HardLineBreak();
  ResetSize();
}

void TextCell::SetType(CellType type)
{
  Cell::SetType(type);
  SetFont((*m_configuration)->GetDefaultFontSize());
}

void TextCell::SetValue(const wxString &text)
{
  m_toolTip = m_initialToolTip;
  m_displayedDigits_old = (*m_configuration)->GetDisplayedDigits();
  m_text = text;
  ResetSize();
  m_text.Replace(wxT("\n"), wxEmptyString);
  m_text.Replace(wxT("-->"), wxT("\x2794"));
  m_text.Replace(wxT(" -->"), wxT("\x2794"));
  m_text.Replace(wxT(" \x2212\x2192 "), wxT("\x2794"));
  m_text.Replace(wxT("->"), wxT("\x2192"));
  m_text.Replace(wxT("\x2212>"), wxT("\x2192"));

  m_displayedText = m_text;
  if (m_textStyle == TS_FUNCTION)
  {
    if (m_text == wxT("ilt"))
      m_toolTip = _("The inverse laplace transform.");
    
    if (m_text == wxT("gamma"))
      m_displayedText = wxT("\x0393");
    if (m_text == wxT("psi"))
      m_displayedText = wxT("\x03A8");
  }      

  if (m_textStyle == TS_VARIABLE)
  {
    if (m_text == wxT("pnz"))
      m_toolTip = _("Either positive, negative or zero.\n"
                    "Normally the result of sign() if the sign cannot be determined."
        );

    if (m_text == wxT("pz"))
      m_toolTip = _("Either positive or zero.\n"
                    "A possible result of sign()."
        );
  
    if (m_text == wxT("nz"))
      m_toolTip = _("Either negative or zero.\n"
                    "A possible result of sign()."
        );

    if (m_text == wxT("und"))
      m_toolTip = _("The result was undefined.");

        if (m_text == wxT("ind"))
      m_toolTip = _("The result was indefinite.");

    if (m_text == wxT("zeroa"))
      m_toolTip = _("Infinitesimal above zero.");

    if (m_text == wxT("zerob"))
      m_toolTip = _("Infinitesimal below zero.");

    if (m_text == wxT("inf"))
      m_toolTip = wxT("+∞.");

    if (m_text == wxT("infinity"))
      m_toolTip = _("Complex infinity.");
        
    if (m_text == wxT("inf"))
      m_toolTip = wxT("-∞.");

    if(m_text.StartsWith("%r"))
    {
      wxString number;

      number = m_text.Right(m_text.Length()-2);

      bool isrnum = (number != wxEmptyString);
     
      for (wxString::iterator it = number.begin(); it != number.end(); ++it)
        if(!wxIsdigit(*it))
        {
          isrnum = false;
          break;
        }

      if(isrnum)
        m_toolTip = _("A variable that can be assigned a number to.\n"
          "Often used by solve() and algsys(), if there is an infinite number of results.");
    }

  
    if(m_text.StartsWith("%i"))
    {
      wxString number;

      number = m_text.Right(m_text.Length()-2);

      bool isinum = (number != wxEmptyString);
     
      for (wxString::iterator it = number.begin(); it != number.end(); ++it)
        if(!wxIsdigit(*it))
        {
          isinum = false;
          break;
        }
      
      if(isinum)
        m_toolTip = _("An integration constant.");
    }
  }
  
  if (m_textStyle == TS_NUMBER)
  {
    unsigned int displayedDigits = (*m_configuration)->GetDisplayedDigits();
    if (m_displayedText.Length() > displayedDigits)
    {
      int left = displayedDigits / 3;
      if (left > 30) left = 30;

      m_displayedText = m_displayedText.Left(left) +
                        wxString::Format(_("[%i digits]"), (int) m_displayedText.Length() - 2 * left) +
                        m_displayedText.Right(left);
      m_toolTip = _("The maximum number of displayed digits can be changed in the configuration dialogue");
    }
    else
    {
      if(
        (m_roundingErrorRegEx1.Matches(m_displayedText)) ||
        (m_roundingErrorRegEx2.Matches(m_displayedText)) ||
        (m_roundingErrorRegEx3.Matches(m_displayedText)) ||
        (m_roundingErrorRegEx4.Matches(m_displayedText))
        )
        m_toolTip = _("As calculating 0.1^12 demonstrates maxima by default doesn't tend to "
                      "hide what looks like being the small error using floating-point "
                      "numbers introduces.\n"
                      "If this seems to be the case here the error can be avoided by using "
                      "exact numbers like 1/10, 1*10^-1 or rat(.1).\n"
                      "It also can be hidden by setting fpprintprec to an appropriate value. "
                      "But be aware in this case that even small errors can add up.");
    }
  }
  else
  {
    if((text.Contains(wxT("LINE SEARCH FAILED. SEE")))||
       (text.Contains(wxT("DOCUMENTATION OF ROUTINE MCSRCH"))) ||
       (text.Contains(wxT("ERROR RETURN OF LINE SEARCH:"))) ||
       text.Contains(wxT("POSSIBLE CAUSES: FUNCTION OR GRADIENT ARE INCORRECT")))
      m_toolTip = _("This message can appear when trying to numerically find an optimum. "
                    "In this case it might indicate that a starting point lies in a local "
                    "optimum that fits the data best if one parameter is increased to "
                    "infinity or decreased to -infinity. It also can indicate that an "
                    "attempt was made to fit data to an equation that actually matches "
                    "the data best if one parameter is set to +/- infinity.");
    if(text.StartsWith(wxT("incorrect syntax")) && (text.Contains(wxT("is not an infix operator"))))
      m_toolTip = _("A command or number wasn't preceded by a \":\", a \"$\", a \";\" or a \",\".\n"
                    "Most probable cause: A missing comma between two list items.");
    if(text.StartsWith(wxT("incorrect syntax")) && (text.Contains(wxT("Found LOGICAL expression where ALGEBRAIC expression expected"))))
      m_toolTip = _("Most probable cause: A dot instead a comma between two list items containing assignments.");
    if(text.StartsWith(wxT("incorrect syntax")) && (text.Contains(wxT("is not a prefix operator"))))
      m_toolTip = _("Most probable cause: Two commas or similar separators in a row.");
    if(text.Contains(wxT("Illegal use of delimiter")))
      m_toolTip = _("Most probable cause: an operator was directly followed by a closing parenthesis.");
    
    if(text.StartsWith(wxT("part: fell off the end.")))
      m_toolTip = _("part() or the [] operator was used in order to extract the nth element "
                    "of something that was less than n elements long.");
    if(text.StartsWith(wxT("rest: fell off the end.")))
      m_toolTip = _("rest() tried to drop more entries from a list than the list was long.");
    if(text.StartsWith(wxT("assignment: cannot assign to")))
      m_toolTip = _("The value of few special variables is assigned by Maxima and cannot be changed by the user. Also a few constructs aren't variable names and therefore cannot be written to.");
    if(text.StartsWith(wxT("rat: replaced ")))
      m_toolTip = _("Normally computers use floating-point numbers that can be handled "
                    "incredibly fast while being accurate to dozens of digits. "
                    "They will, though, introduce a small error into some common numbers. "
                    "For example 0.1 is represented as 3602879701896397/36028797018963968.\n"
                    "As mathematics is based on the fact that numbers that are exactly "
                    "equal cancel each other out small errors can quickly add up to big errors "
                    "(see Wilkinson's Polynomials or Rump's Polynomials). Some maxima "
                    "commands therefore use rat() in order to automatically convert floats to "
                    "exact numbers (like 1/10 or sqrt(2)/2) where floating-point errors might "
                    "add up.\n\n"
                    "This error message doesn't occur if exact numbers (1/10 instead of 0.1) "
                    "are used.\n"
                    "The info that numbers have automatically been converted can be suppressed "
                    "by setting ratprint to false.");
    if(text.StartsWith(wxT("expt: undefined: 0 to a negative exponent.")))
      m_toolTip = _("Division by 0.");
    if(text.Contains(wxT("arithmetic error DIVISION-BY-ZERO signalled")))
      m_toolTip = _("Besides a division by 0 the reason for this error message can be a "
                    "calculation that returns +/-infinity.");
    if(text.Contains(wxT("isn't in the domain of")))
      m_toolTip = _("Most probable cause: A function was called with a parameter that causes "
                    "it to return infinity and/or -infinity.");
    if(text.StartsWith(wxT("Only symbols can be bound")))
      m_toolTip = _("This error message is most probably caused by a try to assign "
                    "a value to a number instead of a variable name.\n"
                    "One probable cause is using a variable that already has a numeric "
                    "value as a loop counter.");
    if(text.StartsWith(wxT("append: operators of arguments must all be the same.")))
      m_toolTip = _("Most probably it was attempted to append something to a list "
                    "that isn't a list.\n"
                    "Enclosing the new element for the list in brackets ([]) "
                    "converts it to a list and makes it appendable.");
    if(text.StartsWith(wxT("part: invalid index of list or matrix.")))
      m_toolTip = _("The [] or the part() command tried to access a list or matrix "
                    "element that doesn't exist.");
    if(text.StartsWith(wxT("apply: subscript must be an integer; found:")))
      m_toolTip = _("the [] operator tried to extract an element of a list, a matrix, "
                    "an equation or an array. But instead of an integer number "
                    "something was used whose numerical value is unknown or not an "
                    "integer.\n"
                    "Floating-point numbers are bound to contain small rounding errors "
                    "and aren't allowed as an array index.");
    if(text.StartsWith(wxT(": improper argument: ")))
    {
      if((m_previous) && (m_previous->ToString() == wxT("at")))
        m_toolTip = _("The second argument of at() isn't an equation or a list of "
                      "equations. Most probably it was lacking an \"=\".");
      else if((m_previous) && (m_previous->ToString() == wxT("subst")))
        m_toolTip = _("The first argument of subst() isn't an equation or a list of "
                      "equations. Most probably it was lacking an \"=\".");
      else
        m_toolTip = _("The argument of a function was of the wrong type. Most probably "
                      "an equation was expected but was lacking an \"=\".");
    }
  }
  m_alt = m_altJs = false;
  ResetSize();
}

Cell *TextCell::Copy()
{
  TextCell *retval = new TextCell(m_group, m_configuration, m_cellPointers, wxEmptyString);
  CopyData(this, retval);
  retval->m_text = wxString(m_text);
  retval->m_displayedText = wxString(m_displayedText);
  retval->m_forceBreakLine = m_forceBreakLine;
  retval->m_bigSkip = m_bigSkip;
  retval->m_isHidden = m_isHidden;
  retval->m_textStyle = m_textStyle;
  retval->m_highlight = m_highlight;
  retval->m_userDefinedLabel = m_userDefinedLabel;
  retval->m_dontEscapeOpeningParenthesis = m_dontEscapeOpeningParenthesis;

  return retval;
}

void TextCell::RecalculateWidths(int fontsize)
{
  Cell::RecalculateWidths(fontsize);
  if(fontsize != m_fontsize_old)
    ResetSize();
  m_fontsize_old = fontsize;
  Configuration *configuration = (*m_configuration);

  bool recalculateNeeded = false;

  if(m_lastZoomFactor != configuration->GetZoomFactor())
  {
    m_lastZoomFactor = configuration->GetZoomFactor();
    recalculateNeeded = true;
  }
  
  // If the setting has changed and we want to show a user-defined label
  // instead of an automatic one or vice versa we decide that here.
  if(
    (m_textStyle == TS_USERLABEL) &&
    (!configuration->UseUserLabels())
    )
  {
    m_textStyle = TS_LABEL;
    recalculateNeeded = true;
  }
  if(
    (m_textStyle == TS_LABEL) &&
    (configuration->UseUserLabels()) &&
    (m_userDefinedLabel != wxEmptyString)
    )
  {
    m_textStyle = TS_USERLABEL;
    recalculateNeeded = true;
  }
  
  SetAltText();

  // If the config settings about how many digits to display has changed we
  // need to regenerate the info which number to show.
  if (
          (m_textStyle == TS_NUMBER) &&
          (m_displayedDigits_old != (*m_configuration)->GetDisplayedDigits())
          )
  {
    SetValue(m_text);
    recalculateNeeded = true;
  }

  if ((m_height < 0) || (m_width < 0) || configuration->FontChanged())
    recalculateNeeded = true;

  if(recalculateNeeded)
  {
    m_lastCalculationFontSize = fontsize;
    wxDC *dc = configuration->GetDC();
    SetFont(fontsize);

    // Labels and prompts are fixed width - adjust font size so that
    // they fit in
    if ((m_textStyle == TS_LABEL) || (m_textStyle == TS_USERLABEL) || (m_textStyle == TS_MAIN_PROMPT) || (m_textStyle == TS_OTHER_PROMPT))
    {
      wxString text = m_text;

      if(m_textStyle == TS_USERLABEL)
      {
        text = wxT("(") + m_userDefinedLabel + wxT(")");
        m_unescapeRegEx.ReplaceAll(&text,wxT("\\1"));
      }

      wxFont font = dc->GetFont();
      double fontsize1 = Scale_Px(configuration->GetDefaultFontSize());
      if(fontsize1 < 4)
        fontsize1 = 4;
#if wxCHECK_VERSION(3, 1, 2)
      font.SetFractionalPointSize(fontsize1);
#else
      font.SetPointSize(fontsize1);
#endif
      dc->SetFont(font);

      
      m_width = Scale_Px(configuration->GetLabelWidth());
      // We will decrease it before use
      m_fontSizeLabel = m_fontSize + 1;
      int labelWidth,labelHeight;
      dc->GetTextExtent(text, &labelWidth, &labelHeight);
      wxASSERT_MSG((labelWidth > 0) || (m_displayedText == wxEmptyString),
                   _("Seems like something is broken with the maths font. Installing http://www.math.union.edu/~dpvc/jsmath/download/jsMath-fonts.html and checking \"Use JSmath fonts\" in the configuration dialogue should fix it."));
      font = dc->GetFont();
      do
      {
#if wxCHECK_VERSION(3, 1, 2)
        font.SetFractionalPointSize(Scale_Px(--m_fontSizeLabel));
#else
        font.SetPointSize(Scale_Px(--m_fontSizeLabel));
#endif
        dc->SetFont(font);
        dc->GetTextExtent(text, &labelWidth, &labelHeight);
      } while ((labelWidth >= m_width) && (m_fontSizeLabel > 2));
      m_height = labelHeight;
      m_center = m_height / 2;
    }
    // Check if we are using jsMath and have jsMath character
    else if (m_altJs && configuration->CheckTeXFonts())
    {
      dc->GetTextExtent(m_altJsText, &m_width, &m_height);

      if (m_texFontname == wxT("jsMath-cmsy10"))
        m_height = m_height / 2;
    }

      /// We are using a special symbol
    else if (m_alt)
    {
      dc->GetTextExtent(m_altText, &m_width, &m_height);
    }

      /// Empty string has height of X
    else if (m_displayedText == wxEmptyString)
    {
      dc->GetTextExtent(wxT("gXÄy"), &m_width, &m_height);
      m_width = 0;
    }

      /// This is the default.
    else
      dc->GetTextExtent(m_displayedText, &m_width, &m_height);

    m_width = m_width + 2 * MC_TEXT_PADDING;
    m_height = m_height + 2 * MC_TEXT_PADDING;

    /// Hidden cells (multiplication * is not displayed)
    if (m_isHidden)
    {
      m_height = 0;
      m_width = m_width / 4;
    }
  }
  if(m_height < Scale_Px(4)) m_height = Scale_Px(4);
  m_realCenter = m_center = m_height / 2;
  ResetData();
}

void TextCell::Draw(wxPoint point)
{
  Cell::Draw(point);
  if (DrawThisCell(point) && !m_isHidden)
  {
    
    Configuration *configuration = (*m_configuration);
    wxDC *dc = configuration->GetDC();
    
    if (m_width == -1 || m_height == -1 || m_fontSize != m_lastCalculationFontSize)
      RecalculateWidths(m_fontSize);
    
    if (InUpdateRegion())
    {
      SetFont(m_fontSize);
      SetForeground();
      /// Labels and prompts have special fontsize
      if ((m_textStyle == TS_LABEL) || (m_textStyle == TS_USERLABEL) || (m_textStyle == TS_MAIN_PROMPT))
      {
        if ((m_textStyle == TS_USERLABEL || configuration->ShowAutomaticLabels()) &&
            configuration->ShowLabels())
        {
          SetFontSizeForLabel(dc);
          // Draw the label
          if(m_textStyle == TS_USERLABEL)
          {
            wxString text = m_userDefinedLabel;
            SetToolTip(m_text);
            m_unescapeRegEx.ReplaceAll(&text,wxT("\\1"));
            dc->DrawText(wxT("(") + text + wxT(")"),
                         point.x + MC_TEXT_PADDING,
                         point.y - m_realCenter + MC_TEXT_PADDING);
          }
          else
          {
//            SetToolTip(m_userDefinedLabel);
            SetToolTip(wxEmptyString);
            dc->DrawText(m_displayedText,
                         point.x + MC_TEXT_PADDING,
                         point.y - m_realCenter + MC_TEXT_PADDING);
          }
        }
      }

        /// Check if we are using jsMath and have jsMath character
      else if (m_altJs && configuration->CheckTeXFonts())
        dc->DrawText(m_altJsText,
                    point.x + MC_TEXT_PADDING,
                    point.y - m_realCenter + MC_TEXT_PADDING);

        /// We are using a special symbol
      else if (m_alt)
        dc->DrawText(m_altText,
                    point.x + MC_TEXT_PADDING,
                    point.y - m_realCenter + MC_TEXT_PADDING);

        /// Change asterisk
      else if (configuration->GetChangeAsterisk() && m_displayedText == wxT("*"))
        dc->DrawText(wxT("\xB7"),
                    point.x + MC_TEXT_PADDING,
                    point.y - m_realCenter + MC_TEXT_PADDING);

      else if (m_displayedText == wxT("#"))
        dc->DrawText(wxT("\x2260"),
                    point.x + MC_TEXT_PADDING,
                    point.y - m_realCenter + MC_TEXT_PADDING);
        /// This is the default.
      else
      {
        switch (GetType())
        {
          case MC_TYPE_TEXT:
            // TODO: Add markdown formatting for bold, italic and underlined here.
            dc->DrawText(m_displayedText,
                        point.x + MC_TEXT_PADDING,
                        point.y - m_realCenter + MC_TEXT_PADDING);
            break;
          case MC_TYPE_INPUT:
            // This cell has already been drawn as an EditorCell => we don't repeat this action here.
            break;
          default:
            dc->DrawText(m_displayedText,
                        point.x + MC_TEXT_PADDING,
                        point.y - m_realCenter + MC_TEXT_PADDING);
        }
      }
    }
  }
}

void TextCell::SetFontSizeForLabel(wxDC *dc)
{
  wxFont font(dc->GetFont());
#if wxCHECK_VERSION(3, 1, 2)
  font.SetFractionalPointSize(Scale_Px(m_fontSizeLabel));
#else
  font.SetPointSize(Scale_Px(m_fontSizeLabel));
#endif
  dc->SetFont(font);
}

void TextCell::SetFont(int fontsize)
{
  Configuration *configuration = (*m_configuration);
  wxDC *dc = configuration->GetDC();
  m_fontSize = configuration->GetDefaultFontSize();

  if ((m_textStyle == TS_TITLE) ||
      (m_textStyle == TS_SECTION) ||
      (m_textStyle == TS_SUBSECTION) ||
      (m_textStyle == TS_SUBSUBSECTION) ||
      (m_textStyle == TS_HEADING5) || 
      (m_textStyle == TS_HEADING6))
  {
    // Titles have a fixed font size 
    m_fontSize = configuration->GetFontSize(m_textStyle);
  }
  else
  {
    // Font within maths has a dynamic font size that might be reduced for example
    // within fractions, subscripts or superscripts.
    if (
      (m_textStyle != TS_MAIN_PROMPT) &&
      (m_textStyle != TS_OTHER_PROMPT) &&
      (m_textStyle != TS_ERROR) &&
      (m_textStyle != TS_WARNING)
      )
      m_fontSize = fontsize;
  }
    
  wxFont font = configuration->GetFont(m_textStyle,fontsize);

  // Use jsMath
  if (m_altJs && configuration->CheckTeXFonts())
    font.SetFaceName(m_texFontname);
  
  if (!font.IsOk())
  {
    font.SetFamily(wxFONTFAMILY_MODERN);
    font.SetFaceName(wxEmptyString);
  }
  
  if (!font.IsOk())
    font = *wxNORMAL_FONT;

  if(m_fontSize < 4)
    m_fontSize = 4;
  
  // Mark special variables that are printed as ordinary letters as being special.
  if ((!(*m_configuration)->CheckKeepPercent()) &&
      ((m_text == wxT("%e")) || (m_text == wxT("%i"))))
  {
    if((*m_configuration)->IsItalic(TS_VARIABLE) != wxFONTSTYLE_NORMAL)
    {
      font.SetStyle(wxFONTSTYLE_NORMAL);
    }
    else
    {
      font.SetStyle(wxFONTSTYLE_ITALIC);
    }
  }

  wxASSERT(Scale_Px(m_fontSize) > 0);
#if wxCHECK_VERSION(3, 1, 2)
  font.SetFractionalPointSize(Scale_Px(m_fontSize));
#else
  font.SetPointSize(Scale_Px(m_fontSize));
#endif

  wxASSERT_MSG(font.IsOk(),
               _("Seems like something is broken with a font. Installing http://www.math.union.edu/~dpvc/jsmath/download/jsMath-fonts.html and checking \"Use JSmath fonts\" in the configuration dialogue should fix it."));
  dc->SetFont(font);
  
  // A fallback if we have been completely unable to set a working font
  if (!dc->GetFont().IsOk())
  {
    dc->SetFont(wxFontInfo(10));
  }
}

bool TextCell::IsOperator()
{
  if (wxString(wxT("+*/-")).Find(m_text) >= 0)
    return true;
  if (m_text == wxT("\x2212"))
    return true;
  return false;
}

wxString TextCell::ToString()
{
  wxString text;
  if (m_altCopyText != wxEmptyString)
    text = m_altCopyText;
  else
  {
    text = m_text;
    if(((*m_configuration)->UseUserLabels())&&(m_userDefinedLabel != wxEmptyString))
      text = wxT("(") + m_userDefinedLabel + wxT(")");
    text.Replace(wxT("\x2212"), wxT("-")); // unicode minus sign
    text.Replace(wxT("\x2794"), wxT("-->"));
    text.Replace(wxT("\x2192"), wxT("->"));
  }
  switch (m_textStyle)
  {
    case TS_VARIABLE:
    case TS_FUNCTION:
      // The only way for variable or function names to contain quotes and
      // characters that clearly represent operators is that these chars
      // are quoted by a backslash: They cannot be quoted by quotation
      // marks since maxima would'nt allow strings here.
    {
      // TODO: We could escape the - char inside a variable name.
      // But we get false positives, then.
      wxString charsNeedingQuotes("\\'\"()[]{}^+*/&§?:;=#<>$");
      bool isOperator = true;
      for (size_t i = 0; i < m_text.Length(); i++)
      {
        if ((m_text[i] == wxT(' ')) || (charsNeedingQuotes.Find(m_text[i]) == wxNOT_FOUND))
        {
          isOperator = false;
          break;
        }
      }

      if (!isOperator)
      {
        wxString lastChar;
        if ((m_dontEscapeOpeningParenthesis) && (text.Length() > 0) && (text[text.Length() - 1] == wxT('(')))
        {
          lastChar = text[text.Length() - 1];
          text = text.Left(text.Length() - 1);
        }
        for (size_t i = 0; i < charsNeedingQuotes.Length(); i++)
          text.Replace(charsNeedingQuotes[i], wxT("\\") + wxString(charsNeedingQuotes[i]));
        text += lastChar;
      }
      break;
    }
    case TS_STRING:
      text = wxT("\"") + text + wxT("\"");
      break;

      // Labels sometimes end with a few spaces. But if they are long they don't do
      // that any more => Add a TAB to the end of any label replacing trailing
      // whitespace. But don't do this if we copy only the label.
    case TS_LABEL:
    case TS_USERLABEL:
    case TS_MAIN_PROMPT:
    case TS_OTHER_PROMPT:
      {
        text.Trim();
        text += wxT("\t");
        break;
      }
  default:
  {}
  }
  if((m_next != NULL) && (m_next->BreakLineHere()))
    text += "\n";
  
  return text;
}

wxString TextCell::ToMatlab()
{
	wxString text;
	if (m_altCopyText != wxEmptyString)
	  text = m_altCopyText;
	else
	{
	  text = m_text;
	  if(((*m_configuration)->UseUserLabels())&&(m_userDefinedLabel != wxEmptyString))
		text = wxT("(") + m_userDefinedLabel + wxT(")");
	  text.Replace(wxT("\x2212"), wxT("-")); // unicode minus sign
	  text.Replace(wxT("\x2794"), wxT("-->"));
	  text.Replace(wxT("\x2192"), wxT("->"));

	  if (text == wxT("%e"))
		text = wxT("e");
	  else if (text == wxT("%i"))
		text = wxT("i");
	  else if (text == wxT("%pi"))
		text = wxString(wxT("pi"));
	}
	switch (m_textStyle)
	{
	  case TS_VARIABLE:
	  case TS_FUNCTION:
		// The only way for variable or function names to contain quotes and
		// characters that clearly represent operators is that these chars
		// are quoted by a backslash: They cannot be quoted by quotation
		// marks since maxima would'nt allow strings here.
	  {
		// TODO: We could escape the - char inside a variable name.
		// But we get false positives, then.
		wxString charsNeedingQuotes("\\'\"()[]{}^+*/&§?:;=#<>$");
		bool isOperator = true;
		for (size_t i = 0; i < m_text.Length(); i++)
		{
		  if ((m_text[i] == wxT(' ')) || (charsNeedingQuotes.Find(m_text[i]) == wxNOT_FOUND))
		  {
			isOperator = false;
			break;
		  }
		}

		if (!isOperator)
		{
		  wxString lastChar;
		  if ((m_dontEscapeOpeningParenthesis) && (text.Length() > 0) && (text[text.Length() - 1] == wxT('(')))
		  {
			lastChar = text[text.Length() - 1];
			text = text.Left(text.Length() - 1);
		  }
		  for (size_t i = 0; i < charsNeedingQuotes.Length(); i++)
			text.Replace(charsNeedingQuotes[i], wxT("\\") + wxString(charsNeedingQuotes[i]));
		  text += lastChar;
		}
		break;
	  }
	  case TS_STRING:
		text = wxT("\"") + text + wxT("\"");
		break;

		// Labels sometimes end with a few spaces. But if they are long they don't do
		// that any more => Add a TAB to the end of any label replacing trailing
		// whitespace. But don't do this if we copy only the label.
	  case TS_LABEL:
	  case TS_USERLABEL:
	  case TS_MAIN_PROMPT:
	  case TS_OTHER_PROMPT:
		{
		  text.Trim();
		  text += wxT("\t");
		  break;
		}
	default:
	{}
	}
	if((m_next != NULL) && (m_next->BreakLineHere()))
	  text += "\n";

	return text;
}

wxString TextCell::ToTeX()
{
  wxString text = m_displayedText;

  if(((*m_configuration)->UseUserLabels())&&(m_userDefinedLabel != wxEmptyString))
    text = wxT("(") + m_userDefinedLabel + wxT(")");

  if (!(*m_configuration)->CheckKeepPercent())
  {
    if (text == wxT("%e"))
      text = wxT("e");
    else if (text == wxT("%i"))
      text = wxT("i");
    else if (text == wxT("%pi"))
      text = wxString(wxT("\x03C0"));
  }

  // The string needed in order to ensure we are in math mode. Most TextCells contain names of
  // math objects and therefore can leave this string blank.
  wxString mathModeStart;
  // The string needed in order to close the command that ensures we are in math mode.
  wxString mathModeEnd = wxT(" ");

  if (
          (GetStyle() == TS_ERROR) ||
          (GetStyle() == TS_WARNING) ||
          (GetStyle() == TS_LABEL) ||
          (GetStyle() == TS_USERLABEL) ||
          (GetStyle() == TS_MAIN_PROMPT) ||
          (GetStyle() == TS_OTHER_PROMPT)
          )
  {
    mathModeStart = wxT("\\ensuremath{");
    mathModeEnd = wxT("}");
    text.Replace(wxT("\\"), mathModeStart + wxT("\\backslash") + mathModeEnd);
    text.Replace(wxT("{"), wxT("\\{"));
    text.Replace(wxT("}"), wxT("\\}"));
  }
  else
  {
    text.Replace(wxT("\\"), mathModeStart + wxT("\\backslash") + mathModeEnd);
    text.Replace(wxT("{"), wxT("\\{"));
    text.Replace(wxT("}"), wxT("\\}"));

    // Babel replaces Umlaute by constructs like \"a - and \" isn't allowed in
    // math mode. Fortunately amsTeX provides the \text command that allows to
    // switch to plain text mode again - but with the math font size.
    text.Replace(wxT("ä"), wxT("\\text{ä}"));
    text.Replace(wxT("ö"), wxT("\\text{ö}"));
    text.Replace(wxT("ü"), wxT("\\text{ü}"));
    text.Replace(wxT("Ä"), wxT("\\text{Ä}"));
    text.Replace(wxT("Ö"), wxT("\\text{Ö}"));
    text.Replace(wxT("Ü"), wxT("\\text{Ü}"));
  }

  // If we don't want to show automatic labels the following "if" empties the label.
  if ((m_textStyle == TS_LABEL) && (((!(*m_configuration)->ShowAutomaticLabels())) ||
                                    !(*m_configuration)->ShowLabels())
    )
    text = wxT("");

  text.Replace(wxT("<"), mathModeStart + wxT("<") + mathModeEnd);
  text.Replace(wxT(">"), mathModeStart + wxT(">") + mathModeEnd);
  text.Replace(wxT("\x2212"), wxT("-")); // unicode minus sign
  text.Replace(L"\x00B1", mathModeStart + wxT("\\pm") + mathModeEnd);
  text.Replace(L"\x03B1", mathModeStart + wxT("\\alpha") + mathModeEnd);
  text.Replace(L"\x00B2", mathModeStart + wxT("^2") + mathModeEnd);
  text.Replace(L"\x00B3", mathModeStart + wxT("^3") + mathModeEnd);
  text.Replace(L"\x221A", mathModeStart + wxT("\\sqrt{}") + mathModeEnd);
  text.Replace(L"\x2148", mathModeStart + wxT("\\mathbbm{i}") + mathModeEnd);
  text.Replace(L"\x2147", mathModeStart + wxT("\\mathbbm{e}") + mathModeEnd);
  text.Replace(L"\x210f", mathModeStart + wxT("\\hbar") + mathModeEnd);
  text.Replace(L"\x2203", mathModeStart + wxT("\\exists") + mathModeEnd);
  text.Replace(L"\x2204", mathModeStart + wxT("\\nexists") + mathModeEnd);
  text.Replace(L"\x2208", mathModeStart + wxT("\\in") + mathModeEnd);
  text.Replace(L"\x21D2", mathModeStart + wxT("\\Longrightarrow") + mathModeEnd);
  text.Replace(L"\x221e", mathModeStart + wxT("\\infty") + mathModeEnd);
  text.Replace(L"\x22C0", mathModeStart + wxT("\\wedge") + mathModeEnd);
  text.Replace(L"\x22C1", mathModeStart + wxT("\\vee") + mathModeEnd);
  text.Replace(L"\x22bb", mathModeStart + wxT("\\oplus") + mathModeEnd);
  text.Replace(L"\x22BC", mathModeStart + wxT("\\overline{\\wedge}") + mathModeEnd);
  text.Replace(L"\x22BB", mathModeStart + wxT("\\overline{\\vee}") + mathModeEnd);
  text.Replace(L"\x00AC", mathModeStart + wxT("\\setminus") + mathModeEnd);
  text.Replace(L"\x22C3", mathModeStart + wxT("\\cup") + mathModeEnd);
  text.Replace(L"\x22C2", mathModeStart + wxT("\\cap") + mathModeEnd);
  text.Replace(L"\x2286", mathModeStart + wxT("\\subseteq") + mathModeEnd);
  text.Replace(L"\x2282", mathModeStart + wxT("\\subset") + mathModeEnd);
  text.Replace(L"\x2288", mathModeStart + wxT("\\not\\subseteq") + mathModeEnd);
  text.Replace(L"\x0127", mathModeStart + wxT("\\hbar") + mathModeEnd);
  text.Replace(L"\x0126", mathModeStart + wxT("\\Hbar") + mathModeEnd);
  text.Replace(L"\x2205", mathModeStart + wxT("\\emptyset") + mathModeEnd);
  text.Replace(L"\x00BD", mathModeStart + wxT("\\frac{1}{2}") + mathModeEnd);
  text.Replace(L"\x03B2", mathModeStart + wxT("\\beta") + mathModeEnd);
  text.Replace(L"\x03B3", mathModeStart + wxT("\\gamma") + mathModeEnd);
  text.Replace(L"\x03B4", mathModeStart + wxT("\\delta") + mathModeEnd);
  text.Replace(L"\x03B5", mathModeStart + wxT("\\epsilon") + mathModeEnd);
  text.Replace(L"\x03B6", mathModeStart + wxT("\\zeta") + mathModeEnd);
  text.Replace(L"\x03B7", mathModeStart + wxT("\\eta") + mathModeEnd);
  text.Replace(L"\x03B8", mathModeStart + wxT("\\theta") + mathModeEnd);
  text.Replace(L"\x03B9", mathModeStart + wxT("\\iota") + mathModeEnd);
  text.Replace(L"\x03BA", mathModeStart + wxT("\\kappa") + mathModeEnd);
  text.Replace(L"\x03BB", mathModeStart + wxT("\\lambda") + mathModeEnd);
  text.Replace(L"\x03BC", mathModeStart + wxT("\\mu") + mathModeEnd);
  text.Replace(L"\x03BD", mathModeStart + wxT("\\nu") + mathModeEnd);
  text.Replace(L"\x03BE", mathModeStart + wxT("\\xi") + mathModeEnd);
  text.Replace(L"\x03BF", mathModeStart + wxT("\\omicron") + mathModeEnd);
  text.Replace(L"\x03C0", mathModeStart + wxT("\\pi") + mathModeEnd);
  text.Replace(L"\x03C1", mathModeStart + wxT("\\rho") + mathModeEnd);
  text.Replace(L"\x03C3", mathModeStart + wxT("\\sigma") + mathModeEnd);
  text.Replace(L"\x03C4", mathModeStart + wxT("\\tau") + mathModeEnd);
  text.Replace(L"\x03C5", mathModeStart + wxT("\\upsilon") + mathModeEnd);
  text.Replace(L"\x03C6", mathModeStart + wxT("\\phi") + mathModeEnd);
  text.Replace(L"\x03C7", mathModeStart + wxT("\\chi") + mathModeEnd);
  text.Replace(L"\x03C8", mathModeStart + wxT("\\psi") + mathModeEnd);
  text.Replace(L"\x03C9", mathModeStart + wxT("\\omega") + mathModeEnd);
  text.Replace(L"\x0391", mathModeStart + wxT("\\Alpha") + mathModeEnd);
  text.Replace(L"\x0392", mathModeStart + wxT("\\Beta") + mathModeEnd);
  text.Replace(L"\x0393", mathModeStart + wxT("\\Gamma") + mathModeEnd);
  text.Replace(L"\x0394", mathModeStart + wxT("\\Delta") + mathModeEnd);
  text.Replace(L"\x0395", mathModeStart + wxT("\\Epsilon") + mathModeEnd);
  text.Replace(L"\x0396", mathModeStart + wxT("\\Zeta") + mathModeEnd);
  text.Replace(L"\x0397", mathModeStart + wxT("\\Eta") + mathModeEnd);
  text.Replace(L"\x0398", mathModeStart + wxT("\\Theta") + mathModeEnd);
  text.Replace(L"\x0399", mathModeStart + wxT("\\Iota") + mathModeEnd);
  text.Replace(L"\x039A", mathModeStart + wxT("\\Kappa") + mathModeEnd);
  text.Replace(L"\x039B", mathModeStart + wxT("\\Lambda") + mathModeEnd);
  text.Replace(L"\x039C", mathModeStart + wxT("\\Mu") + mathModeEnd);
  text.Replace(L"\x039D", mathModeStart + wxT("\\Nu") + mathModeEnd);
  text.Replace(L"\x039E", mathModeStart + wxT("\\Xi") + mathModeEnd);
  text.Replace(L"\x039F", mathModeStart + wxT("\\Omicron") + mathModeEnd);
  text.Replace(L"\x03A0", mathModeStart + wxT("\\Pi") + mathModeEnd);
  text.Replace(L"\x03A1", mathModeStart + wxT("\\Rho") + mathModeEnd);
  text.Replace(L"\x03A3", mathModeStart + wxT("\\Sigma") + mathModeEnd);
  text.Replace(L"\x03A4", mathModeStart + wxT("\\Tau") + mathModeEnd);
  text.Replace(L"\x03A5", mathModeStart + wxT("\\Upsilon") + mathModeEnd);
  text.Replace(L"\x03A6", mathModeStart + wxT("\\Phi") + mathModeEnd);
  text.Replace(L"\x03A7", mathModeStart + wxT("\\Chi") + mathModeEnd);
  text.Replace(L"\x03A8", mathModeStart + wxT("\\Psi") + mathModeEnd);
  text.Replace(L"\x03A9", mathModeStart + wxT("\\Omega") + mathModeEnd);
  text.Replace(L"\x2202", mathModeStart + wxT("\\partial") + mathModeEnd);
  text.Replace(L"\x222b", mathModeStart + wxT("\\int") + mathModeEnd);
  text.Replace(L"\x2245", mathModeStart + wxT("\\approx") + mathModeEnd);
  text.Replace(L"\x221d", mathModeStart + wxT("\\propto") + mathModeEnd);
  text.Replace(L"\x2260", mathModeStart + wxT("\\neq") + mathModeEnd);
  text.Replace(L"\x2264", mathModeStart + wxT("\\leq") + mathModeEnd);
  text.Replace(L"\x2265", mathModeStart + wxT("\\geq") + mathModeEnd);
  text.Replace(L"\x226A", mathModeStart + wxT("\\ll") + mathModeEnd);
  text.Replace(L"\x226B", mathModeStart + wxT("\\gg") + mathModeEnd);
  text.Replace(L"\x220e", mathModeStart + wxT("\\blacksquare") + mathModeEnd);
  text.Replace(L"\x2263", mathModeStart + wxT("\\equiv") + mathModeEnd);
  text.Replace(L"\x2211", mathModeStart + wxT("\\sum") + mathModeEnd);
  text.Replace(L"\x220F", mathModeStart + wxT("\\prod") + mathModeEnd);
  text.Replace(L"\x2225", mathModeStart + wxT("\\parallel") + mathModeEnd);
  text.Replace(L"\x27C2", mathModeStart + wxT("\\bot") + mathModeEnd);
  text.Replace(wxT("~"), mathModeStart + wxT("\\sim ") + mathModeEnd);
  text.Replace(wxT("_"), wxT("\\_ "));
  text.Replace(wxT("$"), wxT("\\$ "));
  text.Replace(wxT("%"), wxT("\\% "));
  text.Replace(wxT("&"), wxT("\\& "));
  text.Replace(wxT("@"), mathModeStart + wxT("@") + mathModeEnd);
  text.Replace(wxT("#"), mathModeStart + wxT("\\neq") + mathModeEnd);
  text.Replace(wxT("\xDCB6"), wxT("~")); // A non-breakable space
  text.Replace(wxT("<"), mathModeStart + wxT("<") + mathModeEnd);
  text.Replace(wxT(">"), mathModeStart + wxT(">") + mathModeEnd);
  text.Replace(wxT("\x219D"), mathModeStart + wxT("\\leadsto") + mathModeEnd);
  text.Replace(wxT("\x2192"), mathModeStart + wxT("\\rightarrow") + mathModeEnd);
  text.Replace(wxT("\x2794"), mathModeStart + wxT("\\longrightarrow") + mathModeEnd);

  // m_IsHidden is set for multiplication signs and parenthesis that
  // don't need to be shown
  if (m_isHidden)
  {
    // Normally in TeX the spacing between variable names following each other directly
    // is chosen to show that this is a multiplication.
    // But any use of \mathit{} will change to ordinary text spacing which means we need
    // to add a \, to show that we want to multiply the two long variable names.
    if ((text == wxT("*")) || (text == wxT("\xB7")))
    {
      // We have a hidden multiplication sign
      if (
        // This multiplication sign is between 2 cells
              ((m_previous != NULL) && (m_next != NULL)) &&
              // These cells are two variable names
              ((m_previous->GetStyle() == TS_VARIABLE) && (m_next->GetStyle() == TS_VARIABLE)) &&
              // The variable name prior to this cell has no subscript
              (!(m_previous->ToString().Contains(wxT('_')))) &&
              // we will be using \mathit{} for the TeX outout.
              ((m_next->ToString().Length() > 1) || (m_next->ToString().Length() > 1))
              )
        text = wxT("\\, ");
      else
        text = wxT(" ");
    }
    else
      text = wxEmptyString;
  }
  else
  {
    /*
      Normally we want to draw a centered dot in this case. But if we
      are in the denominator of a d/dt or are drawing the "dx" or
      similar of an integral a centered dot looks stupid and will be
      replaced by a short space ("\,") instead. Likewise we don't want
      to begin a parenthesis with a centered dot even if this
      parenthesis does contain a product.
    */

    if (m_SuppressMultiplicationDot)
    {
      text.Replace(wxT("*"), wxT("\\, "));
      text.Replace(wxT("\xB7"), wxT("\\, "));
    }
    else
    {
      // If we want to know if the last element was a "d" we first have to
      // look if there actually is a last element.
      if (m_previous)
      {
        if (m_previous->GetStyle() == TS_SPECIAL_CONSTANT && m_previous->ToTeX() == wxT("d"))
        {
          text.Replace(wxT("*"), wxT("\\, "));
          text.Replace(wxT("\xB7"), wxT("\\, "));
        }
        else
        {
          text.Replace(wxT("*"), wxT("\\cdot "));
          text.Replace(wxT("\xB7"), wxT("\\cdot "));
        }
      }
    }
  }

  if (GetStyle() == TS_GREEK_CONSTANT)
  {
    if (text == wxT("\\% alpha"))
      return wxT("\\alpha ");
    else if (text == wxT("\\% beta"))
      return wxT("\\beta ");
    else if (text == wxT("\\% gamma"))
      return wxT("\\gamma ");
    else if (text == wxT("\\% delta"))
      return wxT("\\delta ");
    else if (text == wxT("\\% epsilon"))
      return wxT("\\epsilon ");
    else if (text == wxT("\\% zeta"))
      return wxT("\\zeta ");
    else if (text == wxT("\\% eta"))
      return wxT("\\eta ");
    else if (text == wxT("\\% theta"))
      return wxT("\\theta ");
    else if (text == wxT("\\% iota"))
      return wxT("\\iota ");
    else if (text == wxT("\\% kappa"))
      return wxT("\\kappa ");
    else if (text == wxT("\\% lambda"))
      return wxT("\\lambda ");
    else if (text == wxT("\\% mu"))
      return wxT("\\mu ");
    else if (text == wxT("\\% nu"))
      return wxT("\\nu ");
    else if (text == wxT("\\% xi"))
      return wxT("\\xi ");
    else if (text == wxT("\\% omicron"))
      return wxT("\\omicron ");
    else if (text == wxT("\\% pi"))
      return wxT("\\pi ");
    else if (text == wxT("\\% rho"))
      return wxT("\\rho ");
    else if (text == wxT("\\% sigma"))
      return wxT("\\sigma ");
    else if (text == wxT("\\% tau"))
      return wxT("\\tau ");
    else if (text == wxT("\\% upsilon"))
      return wxT("\\upsilon ");
    else if (text == wxT("\\% phi"))
      return wxT("\\phi ");
    else if (text == wxT("\\% chi"))
      return wxT("\\chi ");
    else if (text == wxT("\\% psi"))
      return wxT("\\psi ");
    else if (text == wxT("\\% omega"))
      return wxT("\\omega ");
    else if (text == wxT("\\% Alpha"))
      return wxT("\\Alpha ");
    else if (text == wxT("\\% Beta"))
      return wxT("\\Beta ");
    else if (text == wxT("\\% Gamma"))
      return wxT("\\Gamma ");
    else if (text == wxT("\\% Delta"))
      return wxT("\\Delta ");
    else if (text == wxT("\\% Epsilon"))
      return wxT("\\Epsilon ");
    else if (text == wxT("\\% Zeta"))
      return wxT("\\Zeta ");
    else if (text == wxT("\\% Eta"))
      return wxT("\\Eta ");
    else if (text == wxT("\\% Theta"))
      return wxT("\\Theta ");
    else if (text == wxT("\\% Iota"))
      return wxT("\\Iota ");
    else if (text == wxT("\\% Kappa"))
      return wxT("\\Kappa ");
    else if (text == wxT("\\% Lambda"))
      return wxT("\\Lambda ");
    else if (text == wxT("\\% Mu"))
      return wxT("\\Mu ");
    else if (text == wxT("\\% Nu"))
      return wxT("\\Nu ");
    else if (text == wxT("\\% Xi"))
      return wxT("\\Xi ");
    else if (text == wxT("\\% Omicron"))
      return wxT("\\Omicron ");
    else if (text == wxT("\\% Pi"))
      return wxT("\\Pi ");
    else if (text == wxT("\\% Rho"))
      return wxT("\\Rho ");
    else if (text == wxT("\\% Sigma"))
      return wxT("\\Sigma ");
    else if (text == wxT("\\% Tau"))
      return wxT("\\Tau ");
    else if (text == wxT("\\% Upsilon"))
      return wxT("\\Upsilon ");
    else if (text == wxT("\\% Phi"))
      return wxT("\\Phi ");
    else if (text == wxT("\\% Chi"))
      return wxT("\\Chi ");
    else if (text == wxT("\\% Psi"))
      return wxT("\\Psi ");
    else if (text == wxT("\\% Omega"))
      return wxT("\\Omega ");

    return text;
  }

  if (GetStyle() == TS_SPECIAL_CONSTANT)
  {
    if (text == wxT("inf"))
      return wxT("\\infty ");
    else if (text == wxT("%e"))
      return wxT("e");
    else if (text == wxT("%i"))
      return wxT("i");
    else if (text == wxT("\\% pi"))
      return wxT("\\ensuremath{\\pi} ");
    else
      return text;
  }

  if ((GetStyle() == TS_LABEL) || (GetStyle() == TS_USERLABEL))
  {
    wxString conditionalLinebreak;
    if (m_previous) conditionalLinebreak = wxT("\\]\n\\[");
    text.Trim(true);
    wxString label = text.SubString(1, text.Length() - 2);
    text = conditionalLinebreak + wxT("\\tag{") + label + wxT("}");
    label.Replace(wxT("\\% "), wxT(""));
    // Would be a good idea, but apparently breaks mathJaX
    // text += wxT("\\label{") + label + wxT("}");
  }
  else
  {
    if (GetStyle() == TS_FUNCTION)
    {
      if (text != wxEmptyString)
        text = wxT("\\operatorname{") + text + wxT("}");
    }
    else if (GetStyle() == TS_VARIABLE)
    {
      if ((m_displayedText.Length() > 1) && (text[1] != wxT('_')))
        text = wxT("\\mathit{") + text + wxT("}");
      if (text == wxT("\\% pi"))
        text = wxT("\\ensuremath{\\pi} ");
      text.Replace(wxT("\\text{ä}"), wxT("\\text{\\textit{ä}}"));
      text.Replace(wxT("\\text{ö}"), wxT("\\text{\\textit{ö}}"));
      text.Replace(wxT("\\text{ü}"), wxT("\\text{\\textit{ü}}"));
      text.Replace(wxT("\\text{Ä}"), wxT("\\text{\\textit{Ä}}"));
      text.Replace(wxT("\\text{Ö}"), wxT("\\text{\\textit{Ö}}"));
      text.Replace(wxT("\\text{Ü}"), wxT("\\text{\\textit{Ü}}"));
    }
    else if ((GetStyle() == TS_ERROR) || (GetStyle() == TS_WARNING))
    {
      if (text.Length() > 1)
        text = wxT("\\mbox{") + text + wxT("}");
    }
    else if (GetStyle() == TS_DEFAULT)
    {
      if ((text.Length() > 2) && (text != wxT("\\,")) && (text != wxT("\\, ")))
        text = wxT("\\mbox{") + text + wxT("}");
    }
  }

  if (
          (GetStyle() != TS_FUNCTION) &&
          (GetStyle() != TS_OUTDATED) &&
          (GetStyle() != TS_VARIABLE) &&
          (GetStyle() != TS_NUMBER) &&
          (GetStyle() != TS_GREEK_CONSTANT) &&
          (GetStyle() != TS_SPECIAL_CONSTANT)
          )
    text.Replace(wxT("^"), wxT("\\textasciicircum"));

  if ((GetStyle() == TS_DEFAULT) || (GetStyle() == TS_STRING))
  {
    if (text.Length() > 1)
    {
      if (((m_forceBreakLine) || (m_breakLine)))
        //text=wxT("\\ifhmode\\\\fi\n")+text;
        text = wxT("\\mbox{}\\\\") + text;
/*      if(GetStyle() != TS_DEFAULT)
        text.Replace(wxT(" "), wxT("\\, "));*/
    }
  }

  if ((GetStyle() == TS_LABEL) || (GetStyle() == TS_LABEL))
    text = text + wxT(" ");

  return text;
}

wxString TextCell::ToMathML()
{
  if(m_displayedText == wxEmptyString)
    return wxEmptyString;
  wxString text = XMLescape(m_displayedText);

  if(((*m_configuration)->UseUserLabels())&&(m_userDefinedLabel != wxEmptyString))
    text = XMLescape(wxT("(") + m_userDefinedLabel + wxT(")"));

  // If we didn't display a multiplication dot we want to do the same in MathML.
  if (m_isHidden)
  {
    text.Replace(wxT("*"), wxT("&#8290;"));
    text.Replace(wxT("\xB7"), wxT("&#8290;"));
    if (text != wxT ("&#8290;"))
      text = wxEmptyString;
  }
  text.Replace(wxT("*"), wxT("\xB7"));

  switch (GetStyle())
  {
    case TS_GREEK_CONSTANT:
      text = GetGreekStringUnicode();
      break;
    case TS_SPECIAL_CONSTANT:
    {
      // The "d" from d/dt can be written as a special unicode symbol. But firefox doesn't
      // support this currently => Commenting it out.
      // if((GetStyle() == TS_SPECIAL_CONSTANT) && (text == wxT("d")))
      //   text = wxT("&#2146;");
      bool keepPercent = (*m_configuration)->CheckKeepPercent();
      if (!keepPercent)
      {
        if (text == wxT("%e"))
          text = wxT("e");
        else if (text == wxT("%i"))
          text = wxT("i");
      }
    }
    /* FALLTHRU */
  case TS_VARIABLE:
    {
      bool keepPercent = (*m_configuration)->CheckKeepPercent();

      if (!keepPercent)
      {
        if (text == wxT("%pi"))
          text = wxT("\x03C0");
      }
    }
    /* FALLTHRU */
  case TS_FUNCTION:
      text = GetGreekStringUnicode();
      if (text == wxT("inf"))
        text = wxT("\x221e");
      if((text == wxT("+")) || (text == wxT("-")) || (text == wxT("*")) || (text == wxT("/")))
        return wxT("<mo>") + text + wxT("</mo>\n");
      else
        return wxT("<mi>") + text + wxT("</mi>\n");
      break;
    case TS_NUMBER:
      return wxT("<mn>") + text + wxT("</mn>\n");
      break;

    case TS_LABEL:
    case TS_USERLABEL:
      return wxT("<mtext>") + text + wxT("</mtext></mtd><mtd>\n");
      break;

    case TS_STRING:
    default:
      if (text.StartsWith(wxT("\"")))
        return wxT("<ms>") + text + wxT("</ms>\n");
      else
        return wxT("<mo>") + text + wxT("</mo>\n");
  }

  return wxT("<mo>") + text + wxT("</mo>\n");
}

wxString TextCell::ToOMML()
{
  //Text-only lines are better handled in RTF.
  if (
          ((m_previous != NULL) && (m_previous->GetStyle() != TS_LABEL) && (!m_previous->HardLineBreak())) &&
          (HardLineBreak())
          )
    return wxEmptyString;

  // Labels are text-only.
  if ((GetStyle() == TS_LABEL) || (GetStyle() == TS_USERLABEL))
    return wxEmptyString;

  wxString text = XMLescape(m_displayedText);

  // If we didn't display a multiplication dot we want to do the same in MathML.
  if (m_isHidden)
  {
    text.Replace(wxT("*"), wxT("&#8290;"));
    text.Replace(wxT("\xB7"), wxT("&#8290;"));
    if (text != wxT ("&#8290;"))
      text = wxEmptyString;
  }
  text.Replace(wxT("*"), wxT("\xB7"));

  switch (GetStyle())
  {
    case TS_GREEK_CONSTANT:
    case TS_SPECIAL_CONSTANT:
    {
      // The "d" from d/dt can be written as a special unicode symbol. But firefox doesn't
      // support this currently => Commenting it out.
      // if((GetStyle() == TS_SPECIAL_CONSTANT) && (text == wxT("d")))
      //   text = wxT("&#2146;");
      bool keepPercent = (*m_configuration)->CheckKeepPercent();
      if (!keepPercent)
      {
        if (text == wxT("%e"))
          text = wxT("e");
        else if (text == wxT("%i"))
          text = wxT("i");
      }
    }
    /* FALLTHRU */
  case TS_VARIABLE:
    {
      bool keepPercent = (*m_configuration)->CheckKeepPercent();

      if (!keepPercent)
      {
        if (text == wxT("%pi"))
          text = wxT("\x03C0");
      }
    }
    /* FALLTHRU */
  case TS_FUNCTION:
      text = GetGreekStringUnicode();
      if (text == wxT("inf"))
        text = wxT("\x221e");
      break;
    case TS_NUMBER:
      break;

    case TS_LABEL:
    case TS_USERLABEL:
      return wxEmptyString;
      break;

    case TS_STRING:
    default:
    {
    }
  }
  text = wxT("<m:t>") + text + wxT("</m:t>\n");
  return text;
}

wxString TextCell::ToRTF()
{
  wxString retval;
  wxString text = m_displayedText;

  if (m_displayedText == wxEmptyString)
    return(wxT(" "));
  
  if(((*m_configuration)->UseUserLabels())&&(m_userDefinedLabel != wxEmptyString))
    text = wxT("(") + m_userDefinedLabel + wxT(")");
  
  text.Replace(wxT("-->"), wxT("\x2192"));
  // Needed for the output of let(a/b,a+1);
  text.Replace(wxT(" --> "), wxT("\x2192"));
  if ((GetStyle() == TS_LABEL) || (GetStyle() == TS_USERLABEL))
  {
    retval += wxString::Format(wxT("\\cf%i{"), (int) GetStyle());
    retval += RTFescape(text);
    retval += wxT("}\\cf0");
  }
  return retval;
}

wxString TextCell::ToXML()
{
  wxString tag;
  wxString flags;
  if (m_isHidden)tag = _T("h");
  else
    switch (GetStyle())
    {
      case TS_GREEK_CONSTANT:
        tag = _T("g");
        break;
      case TS_SPECIAL_CONSTANT:
        tag = _T("s");
        break;
      case TS_VARIABLE:
        tag = _T("v");
        break;
      case TS_FUNCTION:
        tag = _T("fnm");
        break;
      case TS_NUMBER:
        tag = _T("n");
        break;
      case TS_STRING:
        tag = _T("st");
        break;
      case TS_LABEL:
        tag = _T("lbl");
        break;
      case TS_USERLABEL:
        tag = _T("lbl");
        flags += wxT(" userdefined=\"yes\"");
        break;
      default:
        tag = _T("t");
    }

  if ((m_forceBreakLine) && (GetStyle() != TS_LABEL) && (GetStyle() != TS_USERLABEL))
    flags += wxT(" breakline=\"true\"");

  if (GetStyle() == TS_ERROR)
    flags += wxT(" type=\"error\"");

  if (GetStyle() == TS_WARNING)
    flags += wxT(" type=\"warning\"");
  
  wxString xmlstring = XMLescape(m_displayedText);
  // convert it, so that the XML configuration doesn't fail
  if(m_userDefinedLabel != wxEmptyString)
    flags += wxT(" userdefinedlabel=\"") + XMLescape(m_userDefinedLabel) + wxT("\"");

  if(m_toolTip != wxEmptyString)
    flags += wxT(" tooltip=\"") + XMLescape(m_toolTip) + wxT("\"");

  return wxT("<") + tag + flags + wxT(">") + xmlstring + wxT("</") + tag + wxT(">");
}

wxString TextCell::GetDiffPart()
{
  return wxT(",") + m_text + wxT(",1");
}

bool TextCell::IsShortNum()
{
  if (m_next != NULL)
    return false;
  else if (m_text.Length() < 4)
    return true;
  return false;
}

void TextCell::SetAltText()
{
  m_altJs = m_alt = false;
  if ((GetStyle() == TS_DEFAULT) && m_text.StartsWith("\""))
    return;

  /// Greek characters are defined in jsMath, Windows and Unicode
  if (GetStyle() == TS_GREEK_CONSTANT)
  {
    m_altJs = true;
    m_altJsText = GetGreekStringTeX();
    m_texFontname = CMMI10;

    m_alt = true;
    m_altText = GetGreekStringUnicode();
  }

    /// Check for other symbols
  else
  {
    m_altJsText = GetSymbolTeX();
    if (m_altJsText != wxEmptyString)
    {
      if (m_text == wxT("+") || m_text == wxT("="))
        m_texFontname = CMR10;
      else if (m_text == wxT("%pi"))
        m_texFontname = CMMI10;
      else
        m_texFontname = CMSY10;
      m_altJs = true;
    }
    m_altText = GetSymbolUnicode((*m_configuration)->CheckKeepPercent());
    if (m_altText != wxEmptyString)
      m_alt = true;
// #if defined __WXMSW__
//     m_altText = GetSymbolSymbol(configuration->CheckKeepPercent());
//     if (m_altText != wxEmptyString)
//     {
//       m_alt = true;
//       m_fontname = wxT("Symbol");
//     }
// #endif
  }
}

wxString TextCell::GetGreekStringUnicode()
{
  wxString txt(m_text);

  if (txt[0] != '%')
    txt = wxT("%") + txt;

  if (txt == wxT("%alpha"))
    return wxT("\x03B1");
  else if (txt == wxT("%beta"))
    return wxT("\x03B2");
  else if (txt == wxT("%gamma"))
    return wxT("\x03B3");
  else if (txt == wxT("%delta"))
    return wxT("\x03B4");
  else if (txt == wxT("%epsilon"))
    return wxT("\x03B5");
  else if (txt == wxT("%zeta"))
    return wxT("\x03B6");
  else if (txt == wxT("%eta"))
    return wxT("\x03B7");
  else if (txt == wxT("%theta"))
    return wxT("\x03B8");
  else if (txt == wxT("%iota"))
    return wxT("\x03B9");
  else if (txt == wxT("%kappa"))
    return wxT("\x03BA");
  else if (txt == wxT("%lambda"))
    return wxT("\x03BB");
  else if (txt == wxT("%mu"))
    return wxT("\x03BC");
  else if (txt == wxT("%nu"))
    return wxT("\x03BD");
  else if (txt == wxT("%xi"))
    return wxT("\x03BE");
  else if (txt == wxT("%omicron"))
    return wxT("\x03BF");
  else if (txt == wxT("%pi"))
    return wxT("\x03C0");
  else if (txt == wxT("%rho"))
    return wxT("\x03C1");
  else if (txt == wxT("%sigma"))
    return wxT("\x03C3");
  else if (txt == wxT("%tau"))
    return wxT("\x03C4");
  else if (txt == wxT("%upsilon"))
    return wxT("\x03C5");
  else if (txt == wxT("%phi"))
    return wxT("\x03C6");
  else if (txt == wxT("%chi"))
    return wxT("\x03C7");
  else if (txt == wxT("%psi"))
    return wxT("\x03C8");
  else if (txt == wxT("%omega"))
    return wxT("\x03C9");
  else if (txt == wxT("%Alpha"))
    return wxT("\x0391");
  else if (txt == wxT("%Beta"))
    return wxT("\x0392");
  else if (txt == wxT("%Gamma"))
    return wxT("\x0393");
  else if (txt == wxT("%Delta"))
    return wxT("\x0394");
  else if (txt == wxT("%Epsilon"))
    return wxT("\x0395");
  else if (txt == wxT("%Zeta"))
    return wxT("\x0396");
  else if (txt == wxT("%Eta"))
    return wxT("\x0397");
  else if (txt == wxT("%Theta"))
    return wxT("\x0398");
  else if (txt == wxT("%Iota"))
    return wxT("\x0399");
  else if (txt == wxT("%Kappa"))
    return wxT("\x039A");
  else if (txt == wxT("%Lambda"))
    return wxT("\x039B");
  else if (txt == wxT("%Mu"))
    return wxT("\x039C");
  else if (txt == wxT("%Nu"))
    return wxT("\x039D");
  else if (txt == wxT("%Xi"))
    return wxT("\x039E");
  else if (txt == wxT("%Omicron"))
    return wxT("\x039F");
  else if (txt == wxT("%Pi"))
    return wxT("\x03A0");
  else if (txt == wxT("%Rho"))
    return wxT("\x03A1");
  else if (txt == wxT("%Sigma"))
    return wxT("\x03A3");
  else if (txt == wxT("%Tau"))
    return wxT("\x03A4");
  else if (txt == wxT("%Upsilon"))
    return wxT("\x03A5");
  else if (txt == wxT("%Phi"))
    return wxT("\x03A6");
  else if (txt == wxT("%Chi"))
    return wxT("\x03A7");
  else if (txt == wxT("%Psi"))
    return wxT("\x03A8");
  else if (txt == wxT("%Omega"))
    return wxT("\x03A9");

  return wxEmptyString;
}

wxString TextCell::GetSymbolUnicode(bool keepPercent)
{
  if (m_text == wxT("+"))
    return wxT("+");
  else if (m_text == wxT("="))
    return wxT("=");
  else if (m_text == wxT("inf"))
    return wxT("\x221E");
  else if (m_text == wxT("%pi"))
    return wxT("\x03C0");
  else if (m_text == wxT("<="))
    return wxT("\x2264");
  else if (m_text == wxT(">="))
    return wxT("\x2265");
  #ifndef __WXMSW__
  else if (m_text == wxT(" and "))
    return wxT(" \x22C0 ");
  else if (m_text == wxT(" or "))
    return wxT(" \x22C1 ");
  else if (m_text == wxT(" xor "))
    return wxT(" \x22BB ");
  else if (m_text == wxT(" nand "))
    return wxT(" \x22BC ");
  else if (m_text == wxT(" nor "))
    return wxT(" \x22BD ");
  else if (m_text == wxT(" implies "))
    return wxT(" \x21D2 ");
  else if (m_text == wxT(" equiv "))
    return wxT(" \x21D4 ");
  else if (m_text == wxT("not"))
    return wxT("\x00AC");
  #endif
  else if (m_text == wxT("->"))
    return wxT("\x2192");
  else if (m_text == wxT("-->"))
    return wxT("\x2794");
  // The next two ones are needed for the output of let(a/b,a+1);
  else if (m_text == wxT(" --> "))
    return wxT("\x2794");
  else if (m_text == wxT(" \x2212\x2192 "))
    return wxT("\x2794");
  /*
   else if (GetStyle() == TS_SPECIAL_CONSTANT && m_text == wxT("d"))
     return wxT("\x2202");
   */

  if (!keepPercent)
  {
    if (m_text == wxT("%e"))
      return wxT("e");
    else if (m_text == wxT("%i"))
      return wxT("i");
    else if (m_text == wxT("%pi"))
      return wxString(wxT("\x03C0"));
  }

  return wxEmptyString;
}

wxString TextCell::GetGreekStringTeX()
{
  if (m_text == wxT("gamma"))
    return wxT("\xC0");
  else if (m_text == wxT("zeta"))
    return wxT("\xB0");
  else if (m_text == wxT("psi"))
    return wxT("\xC9");

  wxString txt(m_text);
  if (txt[0] != '%')
    txt = wxT("%") + txt;

  if (txt == wxT("%alpha"))
    return wxT("\xCB");
  else if (txt == wxT("%beta"))
    return wxT("\xCC");
  else if (txt == wxT("%gamma"))
    return wxT("\xCD");
  else if (txt == wxT("%delta"))
    return wxT("\xCE");
  else if (txt == wxT("%epsilon"))
    return wxT("\xCF");
  else if (txt == wxT("%zeta"))
    return wxT("\xB0");
  else if (txt == wxT("%eta"))
    return wxT("\xD1");
  else if (txt == wxT("%theta"))
    return wxT("\xD2");
  else if (txt == wxT("%iota"))
    return wxT("\xD3");
  else if (txt == wxT("%kappa"))
    return wxT("\xD4");
  else if (txt == wxT("%lambda"))
    return wxT("\xD5");
  else if (txt == wxT("%mu"))
    return wxT("\xD6");
  else if (txt == wxT("%nu"))
    return wxT("\xB7");
  else if (txt == wxT("%xi"))
    return wxT("\xD8");
  else if (txt == wxT("%omicron"))
    return wxT("o");
  else if (txt == wxT("%pi"))
    return wxT("\xD9");
  else if (txt == wxT("%rho"))
    return wxT("\xDA");
  else if (txt == wxT("%sigma"))
    return wxT("\xDB");
  else if (txt == wxT("%tau"))
    return wxT("\xDC");
  else if (txt == wxT("%upsilon"))
    return wxT("\xB5");
  else if (txt == wxT("%chi"))
    return wxT("\xDF");
  else if (txt == wxT("%psi"))
    return wxT("\xEF");
  else if (txt == wxT("%phi"))
    return wxT("\x27");
  else if (txt == wxT("%omega"))
    return wxT("\x21");
  else if (txt == wxT("%Alpha"))
    return wxT("A");
  else if (txt == wxT("%Beta"))
    return wxT("B");
  else if (txt == wxT("%Gamma"))
    return wxT("\xC0");
  else if (txt == wxT("%Delta"))
    return wxT("\xC1");
  else if (txt == wxT("%Epsilon"))
    return wxT("E");
  else if (txt == wxT("%Zeta"))
    return wxT("Z");
  else if (txt == wxT("%Eta"))
    return wxT("H");
  else if (txt == wxT("%Theta"))
    return wxT("\xC2");
  else if (txt == wxT("%Iota"))
    return wxT("I");
  else if (txt == wxT("%Kappa"))
    return wxT("K");
  else if (txt == wxT("%Lambda"))
    return wxT("\xC3");
  else if (txt == wxT("%Mu"))
    return wxT("M");
  else if (txt == wxT("%Nu"))
    return wxT("N");
  else if (txt == wxT("%Xi"))
    return wxT("\xC4");
  else if (txt == wxT("%Omicron"))
    return wxT("O");
  else if (txt == wxT("%Pi"))
    return wxT("\xC5");
  else if (txt == wxT("%Rho"))
    return wxT("P");
  else if (txt == wxT("%Sigma"))
    return wxT("\xC6");
  else if (txt == wxT("%Tau"))
    return wxT("T");
  else if (txt == wxT("%Upsilon"))
    return wxT("Y");
  else if (txt == wxT("%Phi"))
    return wxT("\xC8");
  else if (txt == wxT("%Chi"))
    return wxT("X");
  else if (txt == wxT("%Psi"))
    return wxT("\xC9");
  else if (txt == wxT("%Omega"))
    return wxT("\xCA");

  return wxEmptyString;
}

wxString TextCell::GetSymbolTeX()
{
  if (m_text == wxT("inf"))
    return wxT("\x31");
  else if (m_text == wxT("+"))
    return wxT("+");
  else if (m_text == wxT("%pi"))
    return wxT("\xD9");
  else if (m_text == wxT("="))
    return wxT("=");
  else if (m_text == wxT("->"))
    return wxT("\x21");
  else if (m_text == wxT(">="))
    return wxT("\xD5");
  else if (m_text == wxT("<="))
    return wxT("\xD4");
/*
  else if (m_text == wxT(" and "))
    return wxT(" \x5E ");
  else if (m_text == wxT(" or "))
    return wxT(" \x5F ");
  else if (m_text == wxT(" nand "))
    return wxT(" \x22 ");
  else if (m_text == wxT(" nor "))
    return wxT(" \x23 ");
  else if (m_text == wxT(" eq "))
    return wxT(" \x2C ");
  else if (m_text == wxT(" implies "))
    return wxT(" \x29 ");
  else if (m_text == wxT("not"))
    return wxT("\x3A");
  else if (m_text == wxT(" xor "))
    return wxT("\xC8");
*/

  return wxEmptyString;
}


// RegExes all TextCells share.
wxRegEx TextCell::m_unescapeRegEx(wxT("\\\\(.)"));
wxRegEx TextCell::m_roundingErrorRegEx1(wxT("\\.000000000000[0-9]+$"));
wxRegEx TextCell::m_roundingErrorRegEx2(wxT("\\.999999999999[0-9]+$"));
wxRegEx TextCell::m_roundingErrorRegEx3(wxT("\\.000000000000[0-9]+e"));
wxRegEx TextCell::m_roundingErrorRegEx4(wxT("\\.999999999999[0-9]+e"));
