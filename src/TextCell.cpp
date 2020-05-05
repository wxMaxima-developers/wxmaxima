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

/*! \file
  This file defines the class TextCell

  TextCell is the Cell type that is used in order to display text that is
  contained in maxima's output.
 */

#include "TextCell.h"
#include "FontCache.h"
#include "wx/config.h"

TextCell::TextCell(Cell *parent, Configuration **config, CellPointers *cellPointers,
                   wxString text, TextStyle style) : Cell(parent, config, cellPointers)
{
  m_nextToDraw = NULL;
  switch(m_textStyle = style)
  {
  case TS_DEFAULT: m_type = MC_TYPE_DEFAULT; break;
  case TS_VARIABLE: m_type = MC_TYPE_DEFAULT; break;
  case TS_NUMBER: m_type = MC_TYPE_DEFAULT; break;
  case TS_FUNCTION: m_type = MC_TYPE_DEFAULT; break;
  case TS_SPECIAL_CONSTANT: m_type = MC_TYPE_DEFAULT; break;
  case TS_GREEK_CONSTANT: m_type = MC_TYPE_DEFAULT; break;
  case TS_STRING: m_type = MC_TYPE_DEFAULT; break;
  case TS_INPUT: m_type = MC_TYPE_INPUT; break;
  case TS_MAIN_PROMPT: m_type = MC_TYPE_MAIN_PROMPT; break;
  case TS_OTHER_PROMPT: m_type = MC_TYPE_PROMPT; break;
  case TS_LABEL: m_type = MC_TYPE_LABEL; break;
  case TS_USERLABEL: m_type = MC_TYPE_LABEL; break;
  case TS_HIGHLIGHT: m_type = MC_TYPE_DEFAULT; break;
  case TS_WARNING: m_type = MC_TYPE_WARNING; break;
  case TS_ERROR: m_type = MC_TYPE_ERROR; break;
  case TS_TEXT: m_type = MC_TYPE_TEXT; break;
  case TS_HEADING6: m_type = MC_TYPE_HEADING6; break;
  case TS_HEADING5: m_type = MC_TYPE_HEADING5; break;
  case TS_SUBSUBSECTION: m_type = MC_TYPE_SUBSUBSECTION; break;
  case TS_SUBSECTION: m_type = MC_TYPE_SUBSECTION; break;
  case TS_SECTION: m_type = MC_TYPE_SECTION; break;
  case TS_TITLE: m_type = MC_TYPE_TITLE; break;
  default:
    wxLogMessage(wxString::Format(_("Unexpected text style %i for TextCell"),style));
    m_type = MC_TYPE_DEFAULT;
  }
  m_displayedDigits_old = -1;
  m_height = -1;
  m_realCenter = m_center = -1;
  m_lastCalculationFontSize = -1;
  m_fontSize = 10;
  m_fontSizeLabel = 10;
  m_lastZoomFactor = -1;
  TextCell::SetValue(text);
  m_highlight = false;
  m_dontEscapeOpeningParenthesis = false;
  m_initialToolTip = (*m_configuration)->GetDefaultCellToolTip();
  m_fontsize_old = -1;
}

TextCell::~TextCell()
{
  MarkAsDeleted();
}

void TextCell::SetStyle(TextStyle style)
{
  m_widths.clear();
  Cell::SetStyle(style);
  if ((m_text == "gamma") && (m_textStyle == TS_FUNCTION))
    m_displayedText = wxT("\u0393");
  if ((m_text == "psi") && (m_textStyle == TS_FUNCTION))
    m_displayedText = wxT("\u03A8");
  if((style == TS_LABEL) || (style == TS_USERLABEL)||
     (style == TS_MAIN_PROMPT) || (style == TS_OTHER_PROMPT))
    HardLineBreak();
  ResetSize();
}

void TextCell::SetType(CellType type)
{
  m_widths.clear();
  ResetSize();
  ResetData();
  Cell::SetType(type);
}

void TextCell::SetValue(const wxString &text)
{
  m_widths.clear();
  SetToolTip(m_initialToolTip);
  m_displayedDigits_old = (*m_configuration)->GetDisplayedDigits();
  m_text = text;
  ResetSize();
  m_text.Replace(wxT("\xDCB6"), wxT("\u00A0")); // A non-breakable space
  m_text.Replace("\n", wxEmptyString);
  m_text.Replace("-->", wxT("\u2794"));
  m_text.Replace(" -->", wxT("\u2794"));
  m_text.Replace(wxT(" \u2212\u2192 "), wxT("\u2794"));
  m_text.Replace("->", wxT("\u2192"));
  m_text.Replace(wxT("\u2212>"), wxT("\u2192"));

  m_displayedText = m_text;
  if (m_textStyle == TS_FUNCTION)
  {
    if (m_text == "ilt")
      SetToolTip(_("The inverse laplace transform."));
    
    if (m_text == "gamma")
      m_displayedText = wxT("\u0393");
    if (m_text == "psi")
      m_displayedText = wxT("\u03A8");
  }      

  if (m_textStyle == TS_VARIABLE)
  {
    if (m_text == "pnz")
      SetToolTip( _("Either positive, negative or zero.\n"
                    "Normally the result of sign() if the sign cannot be determined."
                    ));

    if (m_text == "pz")
      SetToolTip(_("Either positive or zero.\n"
                    "A possible result of sign()."
                   ));
  
    if (m_text == "nz")
      SetToolTip(_("Either negative or zero.\n"
                   "A possible result of sign()."
                   ));

    if (m_text == "und")
      SetToolTip( _("The result was undefined."));

    if (m_text == "ind")
      SetToolTip( _("The result was indefinite."));

    if (m_text == "zeroa")
      SetToolTip( _("Infinitesimal above zero."));

    if (m_text == "zerob")
      SetToolTip( _("Infinitesimal below zero."));

    if (m_text == "inf")
      SetToolTip( wxT("+∞."));

    if (m_text == "infinity")
      SetToolTip( _("Complex infinity."));
        
    if (m_text == "inf")
      SetToolTip( wxT("-∞."));

    if(m_text.StartsWith("%r"))
    {
      wxString number;

      number = m_text.Right(m_text.Length()-2);

      bool isrnum = (number != wxEmptyString);
     
      for (wxString::const_iterator it = number.begin(); it != number.end(); ++it)
        if(!wxIsdigit(*it))
        {
          isrnum = false;
          break;
        }

      if(isrnum)
        SetToolTip( _("A variable that can be assigned a number to.\n"
                      "Often used by solve() and algsys(), if there is an infinite number of results."));
    }

  
    if(m_text.StartsWith("%i"))
    {
      wxString number;

      number = m_text.Right(m_text.Length()-2);

      bool isinum = (number != wxEmptyString);
     
      for (wxString::const_iterator it = number.begin(); it != number.end(); ++it)
        if(!wxIsdigit(*it))
        {
          isinum = false;
          break;
        }
      
      if(isinum)
        SetToolTip( _("An integration constant."));
    }
  }
  
  if (m_textStyle == TS_NUMBER)
  {
    m_numStart = wxEmptyString;
    m_numEnd = wxEmptyString;
    m_ellipsis = wxEmptyString;
    unsigned int displayedDigits = (*m_configuration)->GetDisplayedDigits();
    if (m_displayedText.Length() > displayedDigits)
    {
      int left = displayedDigits / 3;
      if (left > 30) left = 30;      
      m_numStart = m_displayedText.Left(left);
      m_ellipsis = wxString::Format(_("[%i digits]"), (int) m_displayedText.Length() - 2 * left);
      m_numEnd = m_displayedText.Right(left);
    }
    else
    {
      m_numStart = wxEmptyString;
      m_numEnd = wxEmptyString;
      m_ellipsis = wxEmptyString;
      if(
        (m_roundingErrorRegEx1.Matches(m_displayedText)) ||
        (m_roundingErrorRegEx2.Matches(m_displayedText)) ||
        (m_roundingErrorRegEx3.Matches(m_displayedText)) ||
        (m_roundingErrorRegEx4.Matches(m_displayedText))
        )
        SetToolTip( _("As calculating 0.1^12 demonstrates maxima by default doesn't tend to "
                      "hide what looks like being the small error using floating-point "
                      "numbers introduces.\n"
                      "If this seems to be the case here the error can be avoided by using "
                      "exact numbers like 1/10, 1*10^-1 or rat(.1).\n"
                      "It also can be hidden by setting fpprintprec to an appropriate value. "
                      "But be aware in this case that even small errors can add up."));
    }
  }
  else
  {
    if((text.Contains("LINE SEARCH FAILED. SEE"))||
       (text.Contains("DOCUMENTATION OF ROUTINE MCSRCH")) ||
       (text.Contains("ERROR RETURN OF LINE SEARCH:")) ||
       text.Contains("POSSIBLE CAUSES: FUNCTION OR GRADIENT ARE INCORRECT"))
      SetToolTip( _("This message can appear when trying to numerically find an optimum. "
                    "In this case it might indicate that a starting point lies in a local "
                    "optimum that fits the data best if one parameter is increased to "
                    "infinity or decreased to -infinity. It also can indicate that an "
                    "attempt was made to fit data to an equation that actually matches "
                    "the data best if one parameter is set to +/- infinity."));
    if(text.StartsWith("incorrect syntax") && (text.Contains("is not an infix operator")))
      SetToolTip( _("A command or number wasn't preceded by a \":\", a \"$\", a \";\" or a \",\".\n"
                    "Most probable cause: A missing comma between two list items."));
    if(text.StartsWith("incorrect syntax") && (text.Contains("Found LOGICAL expression where ALGEBRAIC expression expected")))
      SetToolTip( _("Most probable cause: A dot instead a comma between two list items containing assignments."));
    if(text.StartsWith("incorrect syntax") && (text.Contains("is not a prefix operator")))
      SetToolTip( _("Most probable cause: Two commas or similar separators in a row."));
    if(text.Contains("Illegal use of delimiter"))
      SetToolTip( _("Most probable cause: an operator was directly followed by a closing parenthesis."));
    
    if(text.StartsWith("part: fell off the end."))
      SetToolTip( _("part() or the [] operator was used in order to extract the nth element "
                    "of something that was less than n elements long."));
    if(text.StartsWith("rest: fell off the end."))
      SetToolTip( _("rest() tried to drop more entries from a list than the list was long."));
    if(text.StartsWith("assignment: cannot assign to"))
      SetToolTip( _("The value of few special variables is assigned by Maxima and cannot be changed by the user. Also a few constructs aren't variable names and therefore cannot be written to."));
    if(text.StartsWith("rat: replaced "))
      SetToolTip( _("Normally computers use floating-point numbers that can be handled "
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
                    "by setting ratprint to false."));
    if(text.StartsWith("desolve: can't handle this case."))
      SetToolTip( _("The list of time-dependent variables to solve to doesn't match the time-dependent variables the list of dgls contains."));      
    if(text.StartsWith("expt: undefined: 0 to a negative exponent."))
      SetToolTip( _("Division by 0."));
    if(text.StartsWith("incorrect syntax: parser: incomplete number; missing exponent?"))
      SetToolTip( _("Might also indicate a missing multiplication sign (\"*\")."));
    if(text.Contains("arithmetic error DIVISION-BY-ZERO signalled"))
      SetToolTip( _("Besides a division by 0 the reason for this error message can be a "
                    "calculation that returns +/-infinity."));
    if(text.Contains("isn't in the domain of"))
      SetToolTip( _("Most probable cause: A function was called with a parameter that causes "
                    "it to return infinity and/or -infinity."));
    if(text.StartsWith("Only symbols can be bound"))
      SetToolTip( _("This error message is most probably caused by a try to assign "
                    "a value to a number instead of a variable name.\n"
                    "One probable cause is using a variable that already has a numeric "
                    "value as a loop counter."));
    if(text.StartsWith("append: operators of arguments must all be the same."))
      SetToolTip( _("Most probably it was attempted to append something to a list "
                    "that isn't a list.\n"
                    "Enclosing the new element for the list in brackets ([]) "
                    "converts it to a list and makes it appendable."));
    if(text.Contains(": invalid index"))
      SetToolTip( _("The [] or the part() command tried to access a list or matrix "
                    "element that doesn't exist."));
    if(text.StartsWith("apply: subscript must be an integer; found:"))
      SetToolTip( _("the [] operator tried to extract an element of a list, a matrix, "
                    "an equation or an array. But instead of an integer number "
                    "something was used whose numerical value is unknown or not an "
                    "integer.\n"
                    "Floating-point numbers are bound to contain small rounding errors "
                    "and therefore in most cases don't work as an array index that"
                    "needs to be an exact integer number."));
    if(text.StartsWith(": improper argument: "))
    {
      if((m_previous) && (m_previous->ToString() == "at"))
        SetToolTip( _("The second argument of at() isn't an equation or a list of "
                      "equations. Most probably it was lacking an \"=\"."));
      else if((m_previous) && (m_previous->ToString() == "subst"))
        SetToolTip( _("The first argument of subst() isn't an equation or a list of "
                      "equations. Most probably it was lacking an \"=\"."));
      else
        SetToolTip( _("The argument of a function was of the wrong type. Most probably "
                      "an equation was expected but was lacking an \"=\"."));
    }
  }
  SetAltText();
  ResetSize();
}

// cppcheck-suppress uninitMemberVar symbolName=TextCell::m_altText
// cppcheck-suppress uninitMemberVar symbolName=TextCell::m_altJsText
// cppcheck-suppress uninitMemberVar symbolName=TextCell::m_fontname
// cppcheck-suppress uninitMemberVar symbolName=TextCell::m_texFontname
// cppcheck-suppress uninitMemberVar symbolName=TextCell::m_alt
// cppcheck-suppress uninitMemberVar symbolName=TextCell::m_altJs
// cppcheck-suppress uninitMemberVar symbolName=TextCell::m_initialToolTip
TextCell::TextCell(const TextCell &cell):
  Cell(cell.m_group, cell.m_configuration, cell.m_cellPointers),
  m_text(cell.m_text),
  m_userDefinedLabel(cell.m_userDefinedLabel),
  m_displayedText(cell.m_displayedText)
{
  m_nextToDraw = NULL;
  CopyCommonData(cell);
  m_forceBreakLine = cell.m_forceBreakLine;
  m_bigSkip = cell.m_bigSkip;
  m_lastZoomFactor = -1;
  m_fontSizeLabel = -1;
  m_displayedDigits_old = -1;
  m_lastCalculationFontSize = -1;
  m_realCenter = -1;
  m_fontsize_old = -1;
  m_textStyle = cell.m_textStyle;
  m_highlight = cell.m_highlight;
  m_dontEscapeOpeningParenthesis = cell.m_dontEscapeOpeningParenthesis;
}

double TextCell::GetScaledTextSize() const
{
  if((m_textStyle == TS_LABEL) || (m_textStyle == TS_USERLABEL) || (m_textStyle == TS_MAIN_PROMPT))
    return Scale_Px(m_fontSizeLabel);
  else
    return Scale_Px(m_fontSize);

}

wxSize TextCell::GetTextSize(wxString const &text)
{
  wxDC *dc = (*m_configuration)->GetDC();
  double fontSize = GetScaledTextSize();
 
  SizeHash::const_iterator it = m_widths.find(fontSize);

  // If we already know this text piece's size we return the cached value
  if(it != m_widths.end())
    return it->second;

  // Ask wxWidgets to return this text piece's size (slow, but the only way if
  // there is no cached size).
  wxSize sz = dc->GetTextExtent(text);
  m_widths[fontSize] = sz;
  return sz;
}

bool TextCell::NeedsRecalculation(int fontSize)
{
  return Cell::NeedsRecalculation(fontSize) ||
    (
      (m_textStyle == TS_USERLABEL) &&
      (!(*m_configuration)->UseUserLabels())
      ) ||
    (
      (m_textStyle == TS_LABEL) &&
      ((*m_configuration)->UseUserLabels()) &&
    (m_userDefinedLabel != wxEmptyString)
      ) ||
    (
      (m_textStyle == TS_NUMBER) &&
      (m_displayedDigits_old != (*m_configuration)->GetDisplayedDigits())
      );
}

void TextCell::RecalculateWidths(int fontsize)
{
  if(fontsize < 1)
    fontsize = m_fontSize;
  Configuration *configuration = (*m_configuration);
  
  if(NeedsRecalculation(fontsize))
  {      
    m_fontSize = m_fontsize_old = fontsize;
    wxDC *dc = configuration->GetDC();
    SetFont(fontsize);

    // If the setting has changed and we want to show a user-defined label
    // instead of an automatic one or vice versa we decide that here.
    if(
      (m_textStyle == TS_USERLABEL) &&
      (!configuration->UseUserLabels())
      )
      m_textStyle = TS_LABEL;
    if(
      (m_textStyle == TS_LABEL) &&
      (configuration->UseUserLabels()) &&
      (m_userDefinedLabel != wxEmptyString)
      )
      m_textStyle = TS_USERLABEL;
        
    // If the config settings about how many digits to display has changed we
    // need to regenerate the info which number to show.
    if (
      (m_textStyle == TS_NUMBER) &&
      (m_displayedDigits_old != (*m_configuration)->GetDisplayedDigits())
        )
    {
      SetValue(m_text);
      m_numstartWidths.clear();
      m_ellipsisWidths.clear();
      m_numEndWidths.clear();
    }
    
    m_lastCalculationFontSize = fontsize;

    if(m_numStart != wxEmptyString)
    {      
      double fontSize = GetScaledTextSize();
      {
        SizeHash::const_iterator it = m_numstartWidths.find(fontSize);    
        if(it != m_numstartWidths.end())
          m_numStartWidth = it->second;
        else
        {
          wxSize sz = dc->GetTextExtent(m_numStart);
          m_numstartWidths[fontSize] = sz;
          m_numStartWidth = sz;
        }
      }
      {
        SizeHash::const_iterator it = m_numEndWidths.find(fontSize);    
        if(it != m_numEndWidths.end())
          m_numEndWidth = it->second;
        else
        {
          wxSize sz = dc->GetTextExtent(m_numEnd);
          m_numEndWidths[fontSize] = sz;
          m_numEndWidth = sz;
        }
      }
      {
        SizeHash::const_iterator it = m_ellipsisWidths.find(fontSize);    
        if(it != m_ellipsisWidths.end())
          m_ellipsisWidth = it->second;
        else
        {
          wxSize sz = dc->GetTextExtent(m_ellipsis);
          m_ellipsisWidths[fontSize] = sz;
          m_ellipsisWidth = sz;
        }
      }
      m_width = m_numStartWidth.GetWidth() + m_numEndWidth.GetWidth() +
        m_ellipsisWidth.GetWidth();
      m_height = wxMax(
        wxMax(m_numStartWidth.GetHeight(), m_numEndWidth.GetHeight()),
        m_ellipsisWidth.GetHeight());
    }
    else
    {    
      // Labels and prompts are fixed width - adjust font size so that
      // they fit in
      if ((m_textStyle == TS_LABEL) || (m_textStyle == TS_USERLABEL) || (m_textStyle == TS_MAIN_PROMPT))
      {
        wxString text = m_text;
        if(!m_altText.IsEmpty())
          text = m_altText;

        if(m_textStyle == TS_USERLABEL)
        {
          text = "(" + m_userDefinedLabel + ")";
          m_unescapeRegEx.ReplaceAll(&text,"\\1");
        }


        wxFont font = configuration->GetFont(m_textStyle, configuration->GetDefaultFontSize());
      
        m_width = Scale_Px(configuration->GetLabelWidth());
        // We will decrease it before use
        m_fontSizeLabel = m_fontSize + 1;
        wxSize labelSize = GetTextSize(text);
        wxASSERT_MSG((labelSize.GetWidth() > 0) || (m_displayedText == wxEmptyString),
                     _("Seems like something is broken with the maths font. Installing http://www.math.union.edu/~dpvc/jsmath/download/jsMath-fonts.html and checking \"Use JSmath fonts\" in the configuration dialogue should fix it."));

        while ((labelSize.GetWidth() >= m_width) && (m_fontSizeLabel > 2))
        {
#if wxCHECK_VERSION(3, 1, 2)
          m_fontSizeLabel -= .3 + 3 * (m_width - labelSize.GetWidth()) / labelSize.GetWidth() / 4;
          font.SetFractionalPointSize(Scale_Px(m_fontSizeLabel));
#else
          m_fontSizeLabel -= 1 + 3 * (m_width - labelSize.GetWidth()) / labelSize.GetWidth() / 4;
          font.SetPointSize(Scale_Px(m_fontSizeLabel));
#endif
          dc->SetFont(font);
          labelSize = GetTextSize(text);
        } 
        m_width = wxMax(m_width + MC_TEXT_PADDING, Scale_Px(configuration->GetLabelWidth()) + MC_TEXT_PADDING);
        m_height = labelSize.GetHeight();
        m_center = m_height / 2;
      }
      // Check if we are using jsMath and have jsMath character
      else if ((!m_altJsText.IsEmpty()) && configuration->CheckTeXFonts())
      {      
        wxSize sz = GetTextSize(m_altJsText);
        m_width = sz.GetWidth();
        m_height = sz.GetHeight();
        if (m_texFontname == "jsMath-cmsy10")
          m_height = m_height / 2;
      }

      /// We are using a special symbol
      else if (!m_altText.IsEmpty())
      {
        wxSize sz = GetTextSize(m_altText);
        m_width = sz.GetWidth();
        m_height = sz.GetHeight();
      }
      /// This is the default.
      else
      {
        wxSize sz = GetTextSize(m_displayedText);
        m_width = sz.GetWidth();
        m_height = sz.GetHeight();
      }
    
      m_width += 2 * MC_TEXT_PADDING;
      m_height += 2 * MC_TEXT_PADDING;

      /// Hidden cells (multiplication * is not displayed)
      if ((m_isHidden) || ((configuration->HidemultiplicationSign()) && m_isHidableMultSign))
      {
        m_height = 0;
        m_width = Scale_Px(fontsize) / 4;
      }
    }
    if(m_height < Scale_Px(4)) m_height = Scale_Px(4);
    m_realCenter = m_center = m_height / 2;
  }
  Cell::RecalculateWidths(fontsize);
}

void TextCell::Draw(wxPoint point)
{
  Cell::Draw(point);
  Configuration *configuration = (*m_configuration);
  if (DrawThisCell(point) &&
      !(m_isHidden || ((configuration->HidemultiplicationSign()) && m_isHidableMultSign)))
  {
    wxDC *dc = configuration->GetDC();
    
    if (NeedsRecalculation(m_fontsize_old))
      RecalculateWidths(m_fontSize);
    
    if (InUpdateRegion())
    {
      SetFont(m_fontSize);
      // Sets the foreground color
      SetForeground();
      /// Labels and prompts have special fontsize
      if ((m_textStyle == TS_LABEL) || (m_textStyle == TS_USERLABEL) || (m_textStyle == TS_MAIN_PROMPT))
      {
        SetFontSizeForLabel(dc);
        if ((m_textStyle == TS_USERLABEL || configuration->ShowAutomaticLabels()) &&
            configuration->ShowLabels())
        {
          // Draw the label
          if(m_textStyle == TS_USERLABEL)
          {
            wxString text = m_userDefinedLabel;
            SetToolTip(m_text);
            m_unescapeRegEx.ReplaceAll(&text,"\\1");
            dc->DrawText("(" + text + ")",
                         point.x + MC_TEXT_PADDING,
                         point.y - m_realCenter + MC_TEXT_PADDING);
          }
          else
          {
            SetToolTip(m_userDefinedLabel);
            dc->DrawText(m_displayedText,
                         point.x + MC_TEXT_PADDING,
                         point.y - m_realCenter + MC_TEXT_PADDING);
          }
        }
      }
      else if (!m_numStart.IsEmpty())
      {
        dc->DrawText(m_numStart,
                     point.x + MC_TEXT_PADDING,
                     point.y - m_realCenter + MC_TEXT_PADDING);
        dc->DrawText(m_numEnd,
                     point.x + MC_TEXT_PADDING + m_numStartWidth.GetWidth() +
                     m_ellipsisWidth.GetWidth(),
                     point.y - m_realCenter + MC_TEXT_PADDING);
        wxColor textColor = dc->GetTextForeground();
        wxColor backgroundColor = dc->GetTextBackground();
        dc->SetTextForeground(
          wxColor(
            (textColor.Red() + backgroundColor.Red()) / 2,
            (textColor.Green() + backgroundColor.Green()) / 2,
            (textColor.Blue() + backgroundColor.Blue()) / 2
            )
          );
        dc->DrawText(m_ellipsis,
                     point.x + MC_TEXT_PADDING + m_numStartWidth.GetWidth(),
                     point.y - m_realCenter + MC_TEXT_PADDING);
      }
        /// Check if we are using jsMath and have jsMath character
      else if ((!m_altJsText.IsEmpty()) && configuration->CheckTeXFonts())
        dc->DrawText(m_altJsText,
                    point.x + MC_TEXT_PADDING,
                    point.y - m_realCenter + MC_TEXT_PADDING);

        /// We are using a special symbol
      else if (!m_altText.IsEmpty())
        dc->DrawText(m_altText,
                    point.x + MC_TEXT_PADDING,
                    point.y - m_realCenter + MC_TEXT_PADDING);

        /// Change asterisk
      else if (configuration->GetChangeAsterisk() && m_displayedText == "*")
        dc->DrawText("\u00B7",
                    point.x + MC_TEXT_PADDING,
                    point.y - m_realCenter + MC_TEXT_PADDING);

      else if (m_displayedText == "#")
        dc->DrawText("\u2260",
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
  wxFont font = (*m_configuration)->GetFont(m_textStyle, GetScaledTextSize());
  font.SetPointSize(GetScaledTextSize());
  dc->SetFont(font);
}

void TextCell::SetFont(int fontsize)
{
  Configuration *configuration = (*m_configuration);
  wxDC *dc = configuration->GetDC();

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

  wxFont font = configuration->GetFont(m_textStyle, fontsize);
  auto req = FontInfo::GetFor(font);

  // Use jsMath
  if ((!m_altJsText.IsEmpty()) && configuration->CheckTeXFonts())
  {
    req.FaceName(m_texFontname);
    font = FontCache::GetAFont(req);
  }
  
  if (!font.IsOk())
  {
    req.Family(wxFONTFAMILY_MODERN).FaceName(wxEmptyString);
    font = FontCache::GetAFont(req);
  }
  
  if (!font.IsOk())
  {
    font = *wxNORMAL_FONT;
    req = FontInfo::GetFor(font);
  }

  if(m_fontSize < 4)
    m_fontSize = 4;
  
  // Mark special variables that are printed as ordinary letters as being special.
  if ((!(*m_configuration)->CheckKeepPercent()) &&
      ((m_text == "%e") || (m_text == "%i")))
  {
    if((*m_configuration)->IsItalic(TS_VARIABLE) != wxFONTSTYLE_NORMAL)
    {
      req.Italic(false);
    }
    else
    {
      req.Italic(true);
    }
  }

  wxASSERT(Scale_Px(m_fontSize) > 0);
  FontInfo::SetPointSize(req, Scale_Px(m_fontSize));
  font = FontCache::GetAFont(req);
  font.SetPointSize(Scale_Px(m_fontSize));

  wxASSERT_MSG(font.IsOk(),
               _("Seems like something is broken with a font. Installing http://www.math.union.edu/~dpvc/jsmath/download/jsMath-fonts.html and checking \"Use JSmath fonts\" in the configuration dialogue should fix it."));
  dc->SetFont(font);
  
  // A fallback if we have been completely unable to set a working font
  if (!dc->GetFont().IsOk())
  {
    req = wxFontInfo(10);
    font = FontCache::GetAFont(req);
    font.SetPointSize(Scale_Px(m_fontSize));
    dc->SetFont(font);
  }
}

bool TextCell::IsOperator() const
{
  if (wxString("+*/-").Find(m_text) >= 0)
    return true;
  if (m_text == "\u2212")
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
      text = "(" + m_userDefinedLabel + ")";
    text.Replace(wxT("\u2212"), "-"); // unicode minus sign
    text.Replace(wxT("\u2794"), "-->");
    text.Replace(wxT("\u2192"), "->");
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
      wxString charsNeedingQuotes("\\'\"()[]-{}^+*/&§?:;=#<>$");
      bool isOperator = true;
      if(m_text.Length() > 1)
      {
        for (size_t i = 0; i < m_text.Length(); i++)
        {
          if ((m_text[i] == ' ') || (charsNeedingQuotes.Find(m_text[i]) == wxNOT_FOUND))
          {
            isOperator = false;
            break;
          }
        }
      }

      if (!isOperator)
      {
        wxString lastChar;
        if ((m_dontEscapeOpeningParenthesis) && (text.Length() > 0) && (text[text.Length() - 1] == '('))
        {
          lastChar = text[text.Length() - 1];
          text = text.Left(text.Length() - 1);
        }
        for (size_t i = 0; i < charsNeedingQuotes.Length(); i++)
          text.Replace(charsNeedingQuotes[i], "\\" + wxString(charsNeedingQuotes[i]));
        text += lastChar;
      }
      break;
    }
    case TS_STRING:
      text = "\"" + text + "\"";
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
        text += "\t";
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
		text = "(" + m_userDefinedLabel + ")";
	  text.Replace(wxT("\u2212"), "-"); // unicode minus sign
	  text.Replace(wxT("\u2794"), "-->");
	  text.Replace(wxT("\u2192"), "->");

	  if (text == "%e")
		text = "e";
	  else if (text == "%i")
		text = "i";
	  else if (text == "%pi")
		text = wxString("pi");
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
		wxString charsNeedingQuotes("\\'\"()[]{}^+*/&§?:;=#<>$");
		bool isOperator = true;
		for (size_t i = 0; i < m_text.Length(); i++)
		{
		  if ((m_text[i] == ' ') || (charsNeedingQuotes.Find(m_text[i]) == wxNOT_FOUND))
		  {
			isOperator = false;
			break;
		  }
		}

		if (!isOperator)
		{
		  wxString lastChar;
		  if ((m_dontEscapeOpeningParenthesis) && (text.Length() > 0) && (text[text.Length() - 1] == '('))
		  {
			lastChar = text[text.Length() - 1];
			text = text.Left(text.Length() - 1);
		  }
		  for (size_t i = 0; i < charsNeedingQuotes.Length(); i++)
			text.Replace(charsNeedingQuotes[i], "\\" + wxString(charsNeedingQuotes[i]));
		  text += lastChar;
		}
		break;
	  }
	  case TS_STRING:
		text = "\"" + text + "\"";
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
		  text += "\t";
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
    text = "(" + m_userDefinedLabel + ")";

  if (!(*m_configuration)->CheckKeepPercent())
  {
    if (text == "%e")
      text = "e";
    else if (text == "%i")
      text = "i";
    else if (text == "%pi")
      text = wxString(wxT("\u03C0"));
  }

  // The string needed in order to ensure we are in math mode. Most TextCells contain names of
  // math objects and therefore can leave this string blank.
  wxString mathModeStart;
  // The string needed in order to close the command that ensures we are in math mode.
  wxString mathModeEnd = " ";

  if (
          (GetStyle() == TS_ERROR) ||
          (GetStyle() == TS_WARNING) ||
          (GetStyle() == TS_LABEL) ||
          (GetStyle() == TS_USERLABEL) ||
          (GetStyle() == TS_MAIN_PROMPT) ||
          (GetStyle() == TS_OTHER_PROMPT)
          )
  {
    mathModeStart = "\\ensuremath{";
    mathModeEnd = "}";
    text.Replace("\\", mathModeStart + "\\backslash" + mathModeEnd);
    text.Replace("{", "\\{");
    text.Replace("}", "\\}");
  }
  else
  {
    text.Replace("\\", mathModeStart + "\\backslash" + mathModeEnd);
    text.Replace("{", "\\{");
    text.Replace("}", "\\}");

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
  
  text.Replace("<", mathModeStart + "<" + mathModeEnd);
  text.Replace(">", mathModeStart + ">" + mathModeEnd);
  text.Replace(wxT("\u2212"), "-"); // unicode minus sign
  text.Replace(wxT("\u00B1"), mathModeStart + "\\pm" + mathModeEnd);
  text.Replace(wxT("\u03B1"), mathModeStart + "\\alpha" + mathModeEnd);
  text.Replace(wxT("\u00B2"), mathModeStart + "^2" + mathModeEnd);
  text.Replace(wxT("\u00B3"), mathModeStart + "^3" + mathModeEnd);
  text.Replace(wxT("\u221A"), mathModeStart + "\\sqrt{}" + mathModeEnd);
  text.Replace(wxT("\u2148"), mathModeStart + "\\mathbbm{i}" + mathModeEnd);
  text.Replace(wxT("\u2147"), mathModeStart + "\\mathbbm{e}" + mathModeEnd);
  text.Replace(wxT("\u210f"), mathModeStart + "\\hbar" + mathModeEnd);
  text.Replace(wxT("\u2203"), mathModeStart + "\\exists" + mathModeEnd);
  text.Replace(wxT("\u2204"), mathModeStart + "\\nexists" + mathModeEnd);
  text.Replace(wxT("\u2208"), mathModeStart + "\\in" + mathModeEnd);
  text.Replace(wxT("\u21D2"), mathModeStart + "\\Longrightarrow" + mathModeEnd);
  text.Replace(wxT("\u221e"), mathModeStart + "\\infty" + mathModeEnd);
  text.Replace(wxT("\u22C0"), mathModeStart + "\\wedge" + mathModeEnd);
  text.Replace(wxT("\u22C1"), mathModeStart + "\\vee" + mathModeEnd);
  text.Replace(wxT("\u22bb"), mathModeStart + "\\oplus" + mathModeEnd);
  text.Replace(wxT("\u22BC"), mathModeStart + "\\overline{\\wedge}" + mathModeEnd);
  text.Replace(wxT("\u22BB"), mathModeStart + "\\overline{\\vee}" + mathModeEnd);
  text.Replace(wxT("\u00AC"), mathModeStart + "\\setminus" + mathModeEnd);
  text.Replace(wxT("\u22C3"), mathModeStart + "\\cup" + mathModeEnd);
  text.Replace(wxT("\u22C2"), mathModeStart + "\\cap" + mathModeEnd);
  text.Replace(wxT("\u2286"), mathModeStart + "\\subseteq" + mathModeEnd);
  text.Replace(wxT("\u2282"), mathModeStart + "\\subset" + mathModeEnd);
  text.Replace(wxT("\u2288"), mathModeStart + "\\not\\subseteq" + mathModeEnd);
  text.Replace(wxT("\u0127"), mathModeStart + "\\hbar" + mathModeEnd);
  text.Replace(wxT("\u0126"), mathModeStart + "\\Hbar" + mathModeEnd);
  text.Replace(wxT("\u2205"), mathModeStart + "\\emptyset" + mathModeEnd);
  text.Replace(wxT("\u00BD"), mathModeStart + "\\frac{1}{2}" + mathModeEnd);
  text.Replace(wxT("\u03B2"), mathModeStart + "\\beta" + mathModeEnd);
  text.Replace(wxT("\u03B3"), mathModeStart + "\\gamma" + mathModeEnd);
  text.Replace(wxT("\u03B4"), mathModeStart + "\\delta" + mathModeEnd);
  text.Replace(wxT("\u03B5"), mathModeStart + "\\epsilon" + mathModeEnd);
  text.Replace(wxT("\u03B6"), mathModeStart + "\\zeta" + mathModeEnd);
  text.Replace(wxT("\u03B7"), mathModeStart + "\\eta" + mathModeEnd);
  text.Replace(wxT("\u03B8"), mathModeStart + "\\theta" + mathModeEnd);
  text.Replace(wxT("\u03B9"), mathModeStart + "\\iota" + mathModeEnd);
  text.Replace(wxT("\u03BA"), mathModeStart + "\\kappa" + mathModeEnd);
  text.Replace(wxT("\u03BB"), mathModeStart + "\\lambda" + mathModeEnd);
  text.Replace(wxT("\u03BC"), mathModeStart + "\\mu" + mathModeEnd);
  text.Replace(wxT("\u03BD"), mathModeStart + "\\nu" + mathModeEnd);
  text.Replace(wxT("\u03BE"), mathModeStart + "\\xi" + mathModeEnd);
  text.Replace(wxT("\u03BF"), "o");
  text.Replace(wxT("\u03C0"), mathModeStart + "\\pi" + mathModeEnd);
  text.Replace(wxT("\u03C1"), mathModeStart + "\\rho" + mathModeEnd);
  text.Replace(wxT("\u03C3"), mathModeStart + "\\sigma" + mathModeEnd);
  text.Replace(wxT("\u03C4"), mathModeStart + "\\tau" + mathModeEnd);
  text.Replace(wxT("\u03C5"), mathModeStart + "\\upsilon" + mathModeEnd);
  text.Replace(wxT("\u03C6"), mathModeStart + "\\phi" + mathModeEnd);
  text.Replace(wxT("\u03C7"), mathModeStart + "\\chi" + mathModeEnd);
  text.Replace(wxT("\u03C8"), mathModeStart + "\\psi" + mathModeEnd);
  text.Replace(wxT("\u03C9"), mathModeStart + "\\omega" + mathModeEnd);
  text.Replace(wxT("\u0391"), "A");
  text.Replace(wxT("\u0392"), "B");
  text.Replace(wxT("\u0393"), mathModeStart + "\\Gamma" + mathModeEnd);
  text.Replace(wxT("\u0394"), mathModeStart + "\\Delta" + mathModeEnd);
  text.Replace(wxT("\u0395"), "E");
  text.Replace(wxT("\u0396"), "Z");
  text.Replace(wxT("\u0397"), "H");
  text.Replace(wxT("\u0398"), mathModeStart + "\\Theta" + mathModeEnd);
  text.Replace(wxT("\u0399"), "I");
  text.Replace(wxT("\u039A"), "K");
  text.Replace(wxT("\u039B"), mathModeStart + "\\Lambda" + mathModeEnd);
  text.Replace(wxT("\u039C"), "M");
  text.Replace(wxT("\u039D"), "N");
  text.Replace(wxT("\u039E"), mathModeStart + "\\Xi" + mathModeEnd);
  text.Replace(wxT("\u039F"), "O");
  text.Replace(wxT("\u03A0"), mathModeStart + "\\Pi" + mathModeEnd);
  text.Replace(wxT("\u03A1"), "P");
  text.Replace(wxT("\u03A3"), mathModeStart + "\\Sigma" + mathModeEnd);
  text.Replace(wxT("\u03A4"), "T");
  text.Replace(wxT("\u03A5"), mathModeStart + "\\Upsilon" + mathModeEnd);
  text.Replace(wxT("\u03A6"), mathModeStart + "\\Phi" + mathModeEnd);
  text.Replace(wxT("\u03A7"), "X");
  text.Replace(wxT("\u03A8"), mathModeStart + "\\Psi" + mathModeEnd);
  text.Replace(wxT("\u03A9"), mathModeStart + "\\Omega" + mathModeEnd);
  text.Replace(wxT("\u2202"), mathModeStart + "\\partial" + mathModeEnd);
  text.Replace(wxT("\u222b"), mathModeStart + "\\int" + mathModeEnd);
  text.Replace(wxT("\u2245"), mathModeStart + "\\approx" + mathModeEnd);
  text.Replace(wxT("\u221d"), mathModeStart + "\\propto" + mathModeEnd);
  text.Replace(wxT("\u2260"), mathModeStart + "\\neq" + mathModeEnd);
  text.Replace(wxT("\u2264"), mathModeStart + "\\leq" + mathModeEnd);
  text.Replace(wxT("\u2265"), mathModeStart + "\\geq" + mathModeEnd);
  text.Replace(wxT("\u226A"), mathModeStart + "\\ll" + mathModeEnd);
  text.Replace(wxT("\u226B"), mathModeStart + "\\gg" + mathModeEnd);
  text.Replace(wxT("\u220e"), mathModeStart + "\\blacksquare" + mathModeEnd);
  text.Replace(wxT("\u2263"), mathModeStart + "\\equiv" + mathModeEnd);
  text.Replace(wxT("\u2211"), mathModeStart + "\\sum" + mathModeEnd);
  text.Replace(wxT("\u220F"), mathModeStart + "\\prod" + mathModeEnd);
  text.Replace(wxT("\u2225"), mathModeStart + "\\parallel" + mathModeEnd);
  text.Replace(wxT("\u27C2"), mathModeStart + "\\bot" + mathModeEnd);
  text.Replace("~", mathModeStart + "\\sim " + mathModeEnd);
  text.Replace("_", "\\_ ");
  text.Replace("$", "\\$ ");
  text.Replace("%", "\\% ");
  text.Replace("&", "\\& ");
  text.Replace("@", mathModeStart + "@" + mathModeEnd);
  text.Replace("#", mathModeStart + "\\neq" + mathModeEnd);
  text.Replace(wxT("\u00A0"), "~"); // A non-breakable space
  text.Replace("<", mathModeStart + "<" + mathModeEnd);
  text.Replace(">", mathModeStart + ">" + mathModeEnd);
  text.Replace(wxT("\u219D"), mathModeStart + "\\leadsto" + mathModeEnd);
  text.Replace(wxT("\u2192"), mathModeStart + "\\rightarrow" + mathModeEnd);
  text.Replace(wxT("\u2794"), mathModeStart + "\\longrightarrow" + mathModeEnd);

  // m_IsHidden is set for parenthesis that don't need to be shown
  if (m_isHidden || (((*m_configuration)->HidemultiplicationSign()) && m_isHidableMultSign))
  {
    // Normally in TeX the spacing between variable names following each other directly
    // is chosen to show that this is a multiplication.
    // But any use of \mathit{} will change to ordinary text spacing which means we need
    // to add a \, to show that we want to multiply the two long variable names.
    if ((text == "*") || (text == wxT("\u00B7")))
    {
      // We have a hidden multiplication sign
      if (
        // This multiplication sign is between 2 cells
              ((m_previous != NULL) && (m_next != NULL)) &&
              // These cells are two variable names
              ((m_previous->GetStyle() == TS_VARIABLE) && (m_next->GetStyle() == TS_VARIABLE)) &&
              // The variable name prior to this cell has no subscript
              (!(m_previous->ToString().Contains('_'))) &&
              // we will be using \mathit{} for the TeX outout.
              ((m_next->ToString().Length() > 1) || (m_next->ToString().Length() > 1))
              )
        text = "\\, ";
      else
        text = " ";
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
      text.Replace("*", "\\, ");
      text.Replace(wxT("\u00B7"), "\\, ");
    }
    else
    {
      // If we want to know if the last element was a "d" we first have to
      // look if there actually is a last element.
      if (m_previous)
      {
        if (m_previous->GetStyle() == TS_SPECIAL_CONSTANT && m_previous->ToTeX() == "d")
        {
          text.Replace("*", "\\, ");
          text.Replace(wxT("\u00B7"), "\\, ");
        }
        else
        {
          text.Replace("*", "\\cdot ");
          text.Replace(wxT("\u00B7"), "\\cdot ");
        }
      }
    }
  }

  if (GetStyle() == TS_GREEK_CONSTANT)
  {
    if (text == "\\% alpha")
      return "\\alpha ";
    else if (text == "\\% beta")
      return "\\beta ";
    else if (text == "\\% gamma")
      return "\\gamma ";
    else if (text == "\\% delta")
      return "\\delta ";
    else if (text == "\\% epsilon")
      return "\\epsilon ";
    else if (text == "\\% zeta")
      return "\\zeta ";
    else if (text == "\\% eta")
      return "\\eta ";
    else if (text == "\\% theta")
      return "\\theta ";
    else if (text == "\\% iota")
      return "\\iota ";
    else if (text == "\\% kappa")
      return "\\kappa ";
    else if (text == "\\% lambda")
      return "\\lambda ";
    else if (text == "\\% mu")
      return "\\mu ";
    else if (text == "\\% nu")
      return "\\nu ";
    else if (text == "\\% xi")
      return "\\ui ";
    else if (text == "\\% omicron")
      return "\\omicron ";
    else if (text == "\\% pi")
      return "\\pi ";
    else if (text == "\\% rho")
      return "\\rho ";
    else if (text == "\\% sigma")
      return "\\sigma ";
    else if (text == "\\% tau")
      return "\\tau ";
    else if (text == "\\% upsilon")
      return "\\upsilon ";
    else if (text == "\\% phi")
      return "\\phi ";
    else if (text == "\\% chi")
      return "\\chi ";
    else if (text == "\\% psi")
      return "\\psi ";
    else if (text == "\\% omega")
      return "\\omega ";
    else if (text == "\\% Alpha")
      return "A";
    else if (text == "\\% Beta")
      return "B";
    else if (text == "\\% Gamma")
      return "\\Gamma ";
    else if (text == "\\% Delta")
      return "\\Delta ";
    else if (text == "\\% Epsilon")
      return "\\Epsilon ";
    else if (text == "\\% Zeta")
      return "\\Zeta ";
    else if (text == "\\% Eta")
      return "\\Eta ";
    else if (text == "\\% Theta")
      return "\\Theta ";
    else if (text == "\\% Iota")
      return "\\Iota ";
    else if (text == "\\% Kappa")
      return "\\Kappa ";
    else if (text == "\\% Lambda")
      return "\\Lambda ";
    else if (text == "\\% Mu")
      return "\\Mu ";
    else if (text == "\\% Nu")
      return "\\Nu ";
    else if (text == "\\% Xi")
      return "\\ui ";
    else if (text == "\\% Omicron")
      return "\\Omicron ";
    else if (text == "\\% Pi")
      return "\\Pi ";
    else if (text == "\\% Rho")
      return "\\Rho ";
    else if (text == "\\% Sigma")
      return "\\Sigma ";
    else if (text == "\\% Tau")
      return "\\Tau ";
    else if (text == "\\% Upsilon")
      return "\\Upsilon ";
    else if (text == "\\% Phi")
      return "\\Phi ";
    else if (text == "\\% Chi")
      return "\\Chi ";
    else if (text == "\\% Psi")
      return "\\Psi ";
    else if (text == "\\% Omega")
      return "\\Omega ";

    return text;
  }

  if (GetStyle() == TS_SPECIAL_CONSTANT)
  {
    if (text == "inf")
      return "\\infty ";
    else if (text == "%e")
      return "e";
    else if (text == "%i")
      return "i";
    else if (text == "\\% pi")
      return "\\ensuremath{\\pi} ";
    else
      return text;
  }

  if ((GetStyle() == TS_LABEL) || (GetStyle() == TS_USERLABEL))
  {
    wxString conditionalLinebreak;
    if (m_previous) conditionalLinebreak = "\\]\n\\[";
    text.Trim(true);
    wxString label = text.SubString(1, text.Length() - 2);
    text = conditionalLinebreak + "\\tag{" + label + "}";
    label.Replace("\\% ", "");
    // Would be a good idea, but apparently breaks mathJaX
    // text += "\\label{" + label + "}";
  }
  else
  {
    if (GetStyle() == TS_FUNCTION)
    {
      if (text != wxEmptyString)
        text = "\\operatorname{" + text + "}";
    }
    else if (GetStyle() == TS_VARIABLE)
    {
      if ((m_displayedText.Length() > 1) && (text[1] != '_'))
        text = "\\mathit{" + text + "}";
      if (text == "\\% pi")
        text = "\\ensuremath{\\pi} ";
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
        text = "\\mbox{" + text + "}";
    }
    else if (GetStyle() == TS_DEFAULT)
    {
      if ((text.Length() > 2) && (text != "\\,") && (text != "\\, "))
        text = "\\mbox{" + text + "}";
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
    text.Replace("^", "\\textasciicircum");

  if ((GetStyle() == TS_DEFAULT) || (GetStyle() == TS_STRING))
  {
    if (text.Length() > 1)
    {
      if (((m_forceBreakLine) || (m_breakLine)))
        //text="\\ifhmode\\\\fi\n"+text;
        text = "\\mbox{}\\\\" + text;
/*      if(GetStyle() != TS_DEFAULT)
        text.Replace(" ", "\\, ");*/
    }
  }

  if ((GetStyle() == TS_LABEL) || (GetStyle() == TS_USERLABEL))
    text = text + " ";

  return text;
}

wxString TextCell::ToMathML()
{
  if(m_displayedText == wxEmptyString)
    return wxEmptyString;
  wxString text = XMLescape(m_displayedText);

  if(((*m_configuration)->UseUserLabels())&&(m_userDefinedLabel != wxEmptyString))
    text = XMLescape("(" + m_userDefinedLabel + ")");

  // If we didn't display a multiplication dot we want to do the same in MathML.
  if (m_isHidden || (((*m_configuration)->HidemultiplicationSign()) && m_isHidableMultSign))
  {
    text.Replace("*", "&#8290;");
    text.Replace(wxT("\u00B7"), "&#8290;");
    if (text != wxT ("&#8290;"))
      text = wxEmptyString;
  }
  text.Replace("*", wxT("\u00B7"));

  switch (GetStyle())
  {
    case TS_GREEK_CONSTANT:
      text = GetGreekStringUnicode();
      break;
    case TS_SPECIAL_CONSTANT:
    {
      text = GetGreekStringUnicode();
      // The "d" from d/dt can be written as a special unicode symbol. But firefox doesn't
      // support this currently => Commenting it out.
      // if((GetStyle() == TS_SPECIAL_CONSTANT) && (text == "d"))
      //   text = "&#2146;";
      bool keepPercent = (*m_configuration)->CheckKeepPercent();
      if (!keepPercent)
      {
        if (text == "%e")
          text = "e";
        else if (text == "%i")
          text = "i";
      }
    }
    break;
  case TS_VARIABLE:
    {
      text = GetGreekStringUnicode();

      bool keepPercent = (*m_configuration)->CheckKeepPercent();

      if (!keepPercent)
      {
        if (text == "%pi")
          text = wxT("\u03C0");
      }
    }
    break;
  case TS_FUNCTION:
      text = GetGreekStringUnicode();
      if (text == "inf")
        text = wxT("\u221e");
      if((text == "+") || (text == "-") || (text == "*") || (text == "/"))
        return "<mo>" + text + "</mo>\n";
      else
        return "<mi>" + text + "</mi>\n";
    case TS_NUMBER:
      return "<mn>" + text + "</mn>\n";

    case TS_LABEL:
    case TS_USERLABEL:
      return "<mtext>" + text + "</mtext></mtd><mtd>\n";

    case TS_STRING:
    default:
      if (text.StartsWith("\""))
        return "<ms>" + text + "</ms>\n";
      else
        return "<mo>" + text + "</mo>\n";
  }

  return "<mo>" + text + "</mo>\n";
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
  if (m_isHidden || (((*m_configuration)->HidemultiplicationSign()) && m_isHidableMultSign))
  {
    text.Replace("*", "&#8290;");
    text.Replace(wxT("\u00B7"), "&#8290;");
    if (text != wxT ("&#8290;"))
      text = wxEmptyString;
  }
  text.Replace("*", wxT("\u00B7"));

  switch (GetStyle())
  {
    case TS_GREEK_CONSTANT:
    case TS_SPECIAL_CONSTANT:
    {
      // The "d" from d/dt can be written as a special unicode symbol. But firefox doesn't
      // support this currently => Commenting it out.
      // if((GetStyle() == TS_SPECIAL_CONSTANT) && (text == "d"))
      //   text = "&#2146;";
      bool keepPercent = (*m_configuration)->CheckKeepPercent();
      if (!keepPercent)
      {
        if (text == "%e")
          text = "e";
        else if (text == "%i")
          text = "i";
      }
    }
    /* FALLTHRU */
  case TS_VARIABLE:
    {
      if (!(*m_configuration)->CheckKeepPercent())
      {
        if (text == "%pi")
          text = wxT("\u03C0");
      }
    }
    break;
  case TS_FUNCTION:
      text = GetGreekStringUnicode();
      if (text == "inf")
        text = wxT("\u221e");
      break;
    case TS_NUMBER:
      break;

    case TS_LABEL:
    case TS_USERLABEL:
      return wxEmptyString;

    case TS_STRING:
    default:
    {
    }
  }
  text = "<m:r>" + text + "</m:r>\n";
  return text;
}

wxString TextCell::ToRTF()
{
  wxString retval;
  wxString text = m_displayedText;

  if (m_displayedText == wxEmptyString)
    return(" ");
  
  if(((*m_configuration)->UseUserLabels())&&(m_userDefinedLabel != wxEmptyString))
    text = "(" + m_userDefinedLabel + ")";
  
  text.Replace("-->", wxT("\u2192"));
  // Needed for the output of let(a/b,a+1);
  text.Replace(" --> ", wxT("\u2192"));
  if ((GetStyle() == TS_LABEL) || (GetStyle() == TS_USERLABEL))
  {
    retval += wxString::Format("\\cf%i{", (int) GetStyle());
    retval += RTFescape(text);
    retval += wxT("}\\cf0");
  }
  return retval;
}

wxString TextCell::ToXML()
{
  wxString tag;
  wxString flags;
  if (m_isHidden || (m_isHidableMultSign))
    tag = _T("h");
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
        flags += " userdefined=\"yes\"";
        break;
      default:
        tag = _T("t");
    }

  if ((m_forceBreakLine) && (GetStyle() != TS_LABEL) && (GetStyle() != TS_USERLABEL))
    flags += " breakline=\"true\"";

  if (GetStyle() == TS_ERROR)
    flags += " type=\"error\"";

  if (GetStyle() == TS_WARNING)
    flags += " type=\"warning\"";
  
  wxString xmlstring = XMLescape(m_displayedText);
  // convert it, so that the XML configuration doesn't fail
  if(m_userDefinedLabel != wxEmptyString)
    flags += " userdefinedlabel=\"" + XMLescape(m_userDefinedLabel) + "\"";

  if(m_altCopyText != wxEmptyString)
    flags += " altCopy=\"" + XMLescape(m_altCopyText) + "\"";

  if(m_toolTip != wxEmptyString)
    flags += " tooltip=\"" + XMLescape(m_toolTip) + "\"";

  return "<" + tag + flags + ">" + xmlstring + "</" + tag + ">";
}

wxString TextCell::GetDiffPart()
{
  return "," + m_text + ",1";
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
  if ((GetStyle() == TS_DEFAULT) && m_text.StartsWith("\""))
    return;

  /// Greek characters are defined in jsMath, Windows and Unicode
  if (GetStyle() == TS_GREEK_CONSTANT)
  {
    if((*m_configuration)->Latin2Greek())
    {
      m_altJsText = GetGreekStringTeX();
      m_texFontname = CMMI10;      
      m_altText = GetGreekStringUnicode();
    }
  }

    /// Check for other symbols
  else
  {
    m_altJsText = GetSymbolTeX();
    if (m_altJsText != wxEmptyString)
    {
      if (m_text == "+" || m_text == "=")
        m_texFontname = CMR10;
      else if (m_text == "%pi")
        m_texFontname = CMMI10;
      else
        m_texFontname = CMSY10;
    }
    m_altText = GetSymbolUnicode((*m_configuration)->CheckKeepPercent());
// #if defined __WXMSW__
//     m_altText = GetSymbolSymbol(configuration->CheckKeepPercent());
//     if (m_altText != wxEmptyString)
//     {
//       m_alt = true;
//       m_fontname = "Symbol";
//     }
// #endif
  }
}

wxString TextCell::GetGreekStringUnicode() const
{
  wxString txt(m_text);

  if (txt[0] != '%')
    txt = "%" + txt;

  if (txt == "%alpha")
    return wxT("\u03B1");
  else if (txt == "%beta")
    return wxT("\u03B2");
  else if (txt == "%gamma")
    return wxT("\u03B3");
  else if (txt == "%delta")
    return wxT("\u03B4");
  else if (txt == "%epsilon")
    return wxT("\u03B5");
  else if (txt == "%zeta")
    return wxT("\u03B6");
  else if (txt == "%eta")
    return wxT("\u03B7");
  else if (txt == "%theta")
    return wxT("\u03B8");
  else if (txt == "%iota")
    return wxT("\u03B9");
  else if (txt == "%kappa")
    return wxT("\u03BA");
  else if (txt == "%lambda")
    return wxT("\u03BB");
  else if (txt == "%mu")
    return wxT("\u03BC");
  else if (txt == "%nu")
    return wxT("\u03BD");
  else if (txt == "%xi")
    return wxT("\u03BE");
  else if (txt == "%omicron")
    return wxT("\u03BF");
  else if (txt == "%pi")
    return wxT("\u03C0");
  else if (txt == "%rho")
    return wxT("\u03C1");
  else if (txt == "%sigma")
    return wxT("\u03C3");
  else if (txt == "%tau")
    return wxT("\u03C4");
  else if (txt == "%upsilon")
    return wxT("\u03C5");
  else if (txt == "%phi")
    return wxT("\u03C6");
  else if (txt == "%chi")
    return wxT("\u03C7");
  else if (txt == "%psi")
    return wxT("\u03C8");
  else if (txt == "%omega")
    return wxT("\u03C9");
  else if (txt == "%Alpha")
    return wxT("\u0391");
  else if (txt == "%Beta")
    return wxT("\u0392");
  else if (txt == "%Gamma")
    return wxT("\u0393");
  else if (txt == "%Delta")
    return wxT("\u0394");
  else if (txt == "%Epsilon")
    return wxT("\u0395");
  else if (txt == "%Zeta")
    return wxT("\u0396");
  else if (txt == "%Eta")
    return wxT("\u0397");
  else if (txt == "%Theta")
    return wxT("\u0398");
  else if (txt == "%Iota")
    return wxT("\u0399");
  else if (txt == "%Kappa")
    return wxT("\u039A");
  else if (txt == "%Lambda")
    return wxT("\u039B");
  else if (txt == "%Mu")
    return wxT("\u039C");
  else if (txt == "%Nu")
    return wxT("\u039D");
  else if (txt == "%Xi")
    return wxT("\u039E");
  else if (txt == "%Omicron")
    return wxT("\u039F");
  else if (txt == "%Pi")
    return wxT("\u03A0");
  else if (txt == "%Rho")
    return wxT("\u03A1");
  else if (txt == "%Sigma")
    return wxT("\u03A3");
  else if (txt == "%Tau")
    return wxT("\u03A4");
  else if (txt == "%Upsilon")
    return wxT("\u03A5");
  else if (txt == "%Phi")
    return wxT("\u03A6");
  else if (txt == "%Chi")
    return wxT("\u03A7");
  else if (txt == "%Psi")
    return wxT("\u03A8");
  else if (txt == "%Omega")
    return wxT("\u03A9");

  return m_text;
}

wxString TextCell::GetSymbolUnicode(bool keepPercent) const
{
  if (m_text == "+")
    return "+";
  else if (m_text == "=")
    return "=";
  else if (m_text == "inf")
    return wxT("\u221E");
  else if (m_text == "%pi")
    return wxT("\u03C0");
  else if (m_text == "<=")
    return wxT("\u2264");
  else if (m_text == ">=")
    return wxT("\u2265");
  #ifndef __WXMSW__
  else if (m_text == " and ")
    return wxT(" \u22C0 ");
  else if (m_text == " or ")
    return wxT(" \u22C1 ");
  else if (m_text == " xor ")
    return wxT(" \u22BB ");
  else if (m_text == " nand ")
    return wxT(" \u22BC ");
  else if (m_text == " nor ")
    return wxT(" \u22BD ");
  else if (m_text == " implies ")
    return wxT(" \u21D2 ");
  else if (m_text == " equiv ")
    return wxT(" \u21D4 ");
  else if (m_text == "not")
    return wxT("\u00AC");
  #endif
  else if (m_text == "->")
    return wxT("\u2192");
  else if (m_text == "-->")
    return wxT("\u2794");
  // The next two ones are needed for the output of let(a/b,a+1);
  else if (m_text == " --> ")
    return wxT("\u2794");
  else if (m_text == wxT(" \u2212\u2192 "))
    return wxT("\u2794");
  /*
   else if (GetStyle() == TS_SPECIAL_CONSTANT && m_text == "d")
     return wxT("\u2202");
   */

  if (!keepPercent)
  {
    if (m_text == "%e")
      return "e";
    else if (m_text == "%i")
      return "i";
    else if (m_text == "%pi")
      return wxString(wxT("\u03C0"));
  }

  return wxEmptyString;
}

wxString TextCell::GetGreekStringTeX() const
{
  if (m_text == "gamma")
    return wxT("\u00C0");
  else if (m_text == "zeta")
    return wxT("\u00B0");
  else if (m_text == "psi")
    return wxT("\u00C9");

  wxString txt(m_text);
  if (txt[0] != '%')
    txt = "%" + txt;

  if (txt == "%alpha")
    return wxT("\u00CB");
  else if (txt == "%beta")
    return wxT("\u00CC");
  else if (txt == "%gamma")
    return wxT("\u00CD");
  else if (txt == "%delta")
    return wxT("\u00CE");
  else if (txt == "%epsilon")
    return wxT("\u00CF");
  else if (txt == "%zeta")
    return wxT("\u00B0");
  else if (txt == "%eta")
    return wxT("\u00D1");
  else if (txt == "%theta")
    return wxT("\u00D2");
  else if (txt == "%iota")
    return wxT("\u00D3");
  else if (txt == "%kappa")
    return wxT("\u00D4");
  else if (txt == "%lambda")
    return wxT("\u00D5");
  else if (txt == "%mu")
    return wxT("\u00D6");
  else if (txt == "%nu")
    return wxT("\u00B7");
  else if (txt == "%xi")
    return wxT("\u00D8");
  else if (txt == "%omicron")
    return wxT("o");
  else if (txt == "%pi")
    return wxT("\u00D9");
  else if (txt == "%rho")
    return wxT("\u00DA");
  else if (txt == "%sigma")
    return wxT("\u00DB");
  else if (txt == "%tau")
    return wxT("\u00DC");
  else if (txt == "%upsilon")
    return wxT("\u00B5");
  else if (txt == "%chi")
    return wxT("\u00DF");
  else if (txt == "%psi")
    return wxT("\u00EF");
  else if (txt == "%phi")
    return wxT("\u0027");
  else if (txt == "%omega")
    return wxT("\u0021");
  else if (txt == "%Alpha")
    return "A";
  else if (txt == "%Beta")
    return "B";
  else if (txt == "%Gamma")
    return wxT("\u00C0");
  else if (txt == "%Delta")
    return wxT("\u00C1");
  else if (txt == "%Epsilon")
    return "E";
  else if (txt == "%Zeta")
    return "Z";
  else if (txt == "%Eta")
    return "H";
  else if (txt == "%Theta")
    return wxT("\u00C2");
  else if (txt == "%Iota")
    return "I";
  else if (txt == "%Kappa")
    return "K";
  else if (txt == "%Lambda")
    return wxT("\u00C3");
  else if (txt == "%Mu")
    return "M";
  else if (txt == "%Nu")
    return "N";
  else if (txt == "%Xi")
    return wxT("\u00C4");
  else if (txt == "%Omicron")
    return "O";
  else if (txt == "%Pi")
    return wxT("\u00C5");
  else if (txt == "%Rho")
    return "P";
  else if (txt == "%Sigma")
    return wxT("\u00C6");
  else if (txt == "%Tau")
    return "T";
  else if (txt == "%Upsilon")
    return "Y";
  else if (txt == "%Phi")
    return wxT("\u00C8");
  else if (txt == "%Chi")
    return "X";
  else if (txt == "%Psi")
    return wxT("\u00C9");
  else if (txt == "%Omega")
    return wxT("\u00CA");

  return wxEmptyString;
}

wxString TextCell::GetSymbolTeX() const
{
  if (m_text == "inf")
    return wxT("\u0031");
  else if (m_text == "+")
    return "+";
  else if (m_text == "%pi")
    return wxT("\u00D9");
  else if (m_text == "=")
    return "=";
  else if (m_text == "->")
    return wxT("\u0021");
  else if (m_text == ">=")
    return wxT("\u00D5");
  else if (m_text == "<=")
    return wxT("\u00D4");
/*
  else if (m_text == " and ")
    return wxT(" \u005E ");
  else if (m_text == " or ")
    return wxT(" \u005F ");
  else if (m_text == " nand ")
    return wxT(" \u0022 ");
  else if (m_text == " nor ")
    return wxT(" \u0023 ");
  else if (m_text == " eq ")
    return wxT(" \u002C ");
  else if (m_text == " implies ")
    return wxT(" \u0029 ");
  else if (m_text == "not")
    return wxT("\u003A");
  else if (m_text == " xor ")
    return wxT("\u00C8");
*/

  return wxEmptyString;
}

void TextCell::SetNextToDraw(Cell *next)
{
  m_nextToDraw = next;
}

// RegExes all TextCells share.
wxRegEx TextCell::m_unescapeRegEx("\\\\(.)");
wxRegEx TextCell::m_roundingErrorRegEx1(wxT("\\.000000000000[0-9]+$"));
wxRegEx TextCell::m_roundingErrorRegEx2(wxT("\\.999999999999[0-9]+$"));
wxRegEx TextCell::m_roundingErrorRegEx3(wxT("\\.000000000000[0-9]+e"));
wxRegEx TextCell::m_roundingErrorRegEx4(wxT("\\.999999999999[0-9]+e"));
