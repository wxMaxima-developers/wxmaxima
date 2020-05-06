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
#include "StringUtils.h"
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
  if ((m_text == wxT("gamma")) && (m_textStyle == TS_FUNCTION))
    m_displayedText = wxT("\u0393");
  if ((m_text == wxT("psi")) && (m_textStyle == TS_FUNCTION))
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
  m_text.Replace(stR("\xDCB6"), stR("\u00A0")); // A non-breakable space
    // FIXME: this replaces "\u00DCB6" ie. "ÜB6" with NBSP - this is a typo
  m_text.Replace(stR("\n"), {});
  m_text.Replace(stR("-->"), stR("\u2794"));
  m_text.Replace(stR(" -->"), stR("\u2794"));
  m_text.Replace(stR(" \u2212\u2192 "), stR("\u2794"));
  m_text.Replace(stR("->"), stR("\u2192"));
  m_text.Replace(stR("\u2212>"), stR("\u2192"));

  m_displayedText = m_text;
  if (m_textStyle == TS_FUNCTION)
  {
    if (m_text == wxT("ilt"))
      SetToolTip(_("The inverse laplace transform."));
    
    if (m_text == wxT("gamma"))
      m_displayedText = wxT("\u0393");
    if (m_text == wxT("psi"))
      m_displayedText = wxT("\u03A8");
  }      

  if (m_textStyle == TS_VARIABLE)
  {
    if (m_text == wxT("pnz"))
      SetToolTip( _("Either positive, negative or zero.\n"
                    "Normally the result of sign() if the sign cannot be determined."
                    ));

    if (m_text == wxT("pz"))
      SetToolTip(_("Either positive or zero.\n"
                    "A possible result of sign()."
                   ));
  
    if (m_text == wxT("nz"))
      SetToolTip(_("Either negative or zero.\n"
                   "A possible result of sign()."
                   ));

    if (m_text == wxT("und"))
      SetToolTip( _("The result was undefined."));

    if (m_text == wxT("ind"))
      SetToolTip( _("The result was indefinite."));

    if (m_text == wxT("zeroa"))
      SetToolTip( _("Infinitesimal above zero."));

    if (m_text == wxT("zerob"))
      SetToolTip( _("Infinitesimal below zero."));

    if (m_text == wxT("inf"))
      SetToolTip( stR("+∞."));

    if (m_text == wxT("infinity"))
      SetToolTip( _("Complex infinity."));
        
    if (m_text == wxT("inf"))
      SetToolTip( stR("-∞."));

    if (m_text.StartsWith(stR("%r")))
    {
      const wxString number = m_text.Right(m_text.Length()-2);

      bool isrnum =
        !number.IsEmpty() && std::all_of(number.begin(), number.end(), wxIsdigit);

      if (isrnum)
        SetToolTip( _("A variable that can be assigned a number to.\n"
                      "Often used by solve() and algsys(), if there is an infinite number of results."));
    }


    if (m_text.StartsWith(stR("%i")))
    {
      const wxString number = m_text.Right(m_text.Length()-2);

      bool isinum =
        !number.IsEmpty() && std::all_of(number.begin(), number.end(), wxIsdigit);

      if (isinum)
        SetToolTip( _("An integration constant."));
    }
  }
  
  if (m_textStyle == TS_NUMBER)
  {
    m_numStart.clear();
    m_numEnd.clear();
    m_ellipsis.clear();
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
      m_numStart.clear();
      m_numEnd.clear();
      m_ellipsis.clear();
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
    if((text.Contains(stR("LINE SEARCH FAILED. SEE")))||
       (text.Contains(stR("DOCUMENTATION OF ROUTINE MCSRCH"))) ||
       (text.Contains(stR("ERROR RETURN OF LINE SEARCH:"))) ||
       text.Contains(stR("POSSIBLE CAUSES: FUNCTION OR GRADIENT ARE INCORRECT")))
      SetToolTip( _("This message can appear when trying to numerically find an optimum. "
                    "In this case it might indicate that a starting point lies in a local "
                    "optimum that fits the data best if one parameter is increased to "
                    "infinity or decreased to -infinity. It also can indicate that an "
                    "attempt was made to fit data to an equation that actually matches "
                    "the data best if one parameter is set to +/- infinity."));
    if(text.StartsWith(stR("incorrect syntax")) && (text.Contains(stR("is not an infix operator"))))
      SetToolTip( _("A command or number wasn't preceded by a \":\", a \"$\", a \";\" or a \",\".\n"
                    "Most probable cause: A missing comma between two list items."));
    if(text.StartsWith(stR("incorrect syntax")) && (text.Contains(stR("Found LOGICAL expression where ALGEBRAIC expression expected"))))
      SetToolTip( _("Most probable cause: A dot instead a comma between two list items containing assignments."));
    if(text.StartsWith(stR("incorrect syntax")) && (text.Contains(stR("is not a prefix operator"))))
      SetToolTip( _("Most probable cause: Two commas or similar separators in a row."));
    if(text.Contains(stR("Illegal use of delimiter")))
      SetToolTip( _("Most probable cause: an operator was directly followed by a closing parenthesis."));
    
    if(text.StartsWith(stR("part: fell off the end.")))
      SetToolTip( _("part() or the [] operator was used in order to extract the nth element "
                    "of something that was less than n elements long."));
    if(text.StartsWith(stR("rest: fell off the end.")))
      SetToolTip( _("rest() tried to drop more entries from a list than the list was long."));
    if(text.StartsWith(stR("assignment: cannot assign to")))
      SetToolTip( _("The value of few special variables is assigned by Maxima and cannot be changed by the user. Also a few constructs aren't variable names and therefore cannot be written to."));
    if(text.StartsWith(stR("rat: replaced ")))
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
    if(text.StartsWith(stR("desolve: can't handle this case.")))
      SetToolTip( _("The list of time-dependent variables to solve to doesn't match the time-dependent variables the list of dgls contains."));      
    if(text.StartsWith(stR("expt: undefined: 0 to a negative exponent.")))
      SetToolTip( _("Division by 0."));
    if(text.StartsWith(stR("incorrect syntax: parser: incomplete number; missing exponent?")))
      SetToolTip( _("Might also indicate a missing multiplication sign (\"*\")."));
    if(text.Contains(stR("arithmetic error DIVISION-BY-ZERO signalled")))
      SetToolTip( _("Besides a division by 0 the reason for this error message can be a "
                    "calculation that returns +/-infinity."));
    if(text.Contains(stR("isn't in the domain of")))
      SetToolTip( _("Most probable cause: A function was called with a parameter that causes "
                    "it to return infinity and/or -infinity."));
    if(text.StartsWith(stR("Only symbols can be bound")))
      SetToolTip( _("This error message is most probably caused by a try to assign "
                    "a value to a number instead of a variable name.\n"
                    "One probable cause is using a variable that already has a numeric "
                    "value as a loop counter."));
    if(text.StartsWith(stR("append: operators of arguments must all be the same.")))
      SetToolTip( _("Most probably it was attempted to append something to a list "
                    "that isn't a list.\n"
                    "Enclosing the new element for the list in brackets ([]) "
                    "converts it to a list and makes it appendable."));
    if(text.Contains(stR(": invalid index")))
      SetToolTip( _("The [] or the part() command tried to access a list or matrix "
                    "element that doesn't exist."));
    if(text.StartsWith(stR("apply: subscript must be an integer; found:")))
      SetToolTip( _("the [] operator tried to extract an element of a list, a matrix, "
                    "an equation or an array. But instead of an integer number "
                    "something was used whose numerical value is unknown or not an "
                    "integer.\n"
                    "Floating-point numbers are bound to contain small rounding errors "
                    "and therefore in most cases don't work as an array index that"
                    "needs to be an exact integer number."));
    if(text.StartsWith(stR(": improper argument: ")))
    {
      if((m_previous) && (m_previous->ToString() == wxT("at")))
        SetToolTip( _("The second argument of at() isn't an equation or a list of "
                      "equations. Most probably it was lacking an \"=\"."));
      else if((m_previous) && (m_previous->ToString() == wxT("subst")))
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
    (!m_userDefinedLabel.IsEmpty())
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
      (!m_userDefinedLabel.IsEmpty())
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

    if(!m_numStart.IsEmpty())
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
          text = wxT('(') + m_userDefinedLabel + wxT(')');
          m_unescapeRegEx.ReplaceAll(&text, stR("\\1"));
        }

        wxFont font = configuration->GetFont(m_textStyle, configuration->FontSize());
      
        m_width = Scale_Px(configuration->GetLabelWidth());
        // We will decrease it before use
        m_fontSizeLabel = m_fontSize + 1;
        wxSize labelSize = GetTextSize(text);
        wxASSERT_MSG(labelSize.GetWidth() > 0 || m_displayedText.IsEmpty(),
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
        if (m_texFontname == wxT("jsMath-cmsy10"))
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
            m_unescapeRegEx.ReplaceAll(&text, stR("\\1"));
            dc->DrawText(wxT('(') + text + wxT(')'),
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
      else if (configuration->GetChangeAsterisk() && m_displayedText == wxT('*'))
        dc->DrawText(stR("\u00B7"),
                    point.x + MC_TEXT_PADDING,
                    point.y - m_realCenter + MC_TEXT_PADDING);

      else if (m_displayedText == wxT('#'))
        dc->DrawText(stR("\u2260"),
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
    req.Family(wxFONTFAMILY_MODERN).FaceName({});
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
      ((m_text == wxT("%e")) || (m_text == wxT("%i"))))
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
  if (stR("+*/-").Find(m_text) >= 0)
    return true;
  if (m_text == wxT("\u2212"))
    return true;
  return false;
}

wxString TextCell::ToString()
{
  wxString text;
  if (!m_altCopyText.IsEmpty())
    text = m_altCopyText;
  else
  {
    text = m_text;
    if ((*m_configuration)->UseUserLabels() && !m_userDefinedLabel.IsEmpty())
      text = wxT('(') + m_userDefinedLabel + wxT(')');
    text.Replace(stR("\u2212"), stR("-")); // unicode minus sign
    text.Replace(stR("\u2794"), stR("-->"));
    text.Replace(stR("\u2192"), stR("->"));
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
          if ((m_text[i] == wxT(' ')) || (charsNeedingQuotes.Find(m_text[i]) == wxNOT_FOUND))
          {
            isOperator = false;
            break;
          }
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
          text.Replace(charsNeedingQuotes[i], stR("\\") + charsNeedingQuotes[i]);
        text += lastChar;
      }
      break;
    }
    case TS_STRING:
      text = wxT('"') + text + wxT('"');
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
        text += wxT('\t');
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
    if (!m_altCopyText.IsEmpty())
	  text = m_altCopyText;
	else
	{
	  text = m_text;
      if ((*m_configuration)->UseUserLabels() && !m_userDefinedLabel.IsEmpty())
        text = wxT('(') + m_userDefinedLabel + wxT(')');
      text.Replace(stR("\u2212"), stR("-")); // unicode minus sign
      text.Replace(stR("\u2794"), stR("-->"));
      text.Replace(stR("\u2192"), stR("->"));

	  if (text == wxT("%e"))
        text = wxT('e');
	  else if (text == wxT("%i"))
        text = wxT('i');
	  else if (text == wxT("%pi"))
        text = stR("pi");
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
            text.Replace(charsNeedingQuotes[i], stR("\\") + charsNeedingQuotes[i]);
		  text += lastChar;
		}
		break;
	  }
	  case TS_STRING:
        text = wxT('"') + text + wxT('"');
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
          text += wxT('\t');
		  break;
		}
	default:
	{}
	}
    if((m_next != NULL) && (m_next->BreakLineHere()))
      text += wxT('\n');

	return text;
}

wxString TextCell::ToTeX()
{
  wxString text = m_displayedText;

  if ((*m_configuration)->UseUserLabels() && !m_userDefinedLabel.IsEmpty())
    text = wxT('(') + m_userDefinedLabel + wxT(')');

  if (!(*m_configuration)->CheckKeepPercent())
  {
    if (text == wxT("%e"))
      text = stR("e");
    else if (text == wxT("%i"))
      text = stR("i");
    else if (text == wxT("%pi"))
      text = stR("\u03C0");
  }

  // The string needed in order to ensure we are in math mode. Most TextCells contain names of
  // math objects and therefore can leave this string blank.
  wxString mathModeStart;
  // The string needed in order to close the command that ensures we are in math mode.
  wxString mathModeEnd = wxT(' ');

  if (
          (GetStyle() == TS_ERROR) ||
          (GetStyle() == TS_WARNING) ||
          (GetStyle() == TS_LABEL) ||
          (GetStyle() == TS_USERLABEL) ||
          (GetStyle() == TS_MAIN_PROMPT) ||
          (GetStyle() == TS_OTHER_PROMPT)
          )
  {
    mathModeStart = stR("\\ensuremath{");
    mathModeEnd = wxT('}');
    text.Replace(stR("\\"), mathModeStart + stR("\\backslash") + mathModeEnd);
    text.Replace(stR("{"), stR("\\{"));
    text.Replace(stR("}"), stR("\\}"));
  }
  else
  {
    text.Replace(stR("\\"), mathModeStart + stR("\\backslash") + mathModeEnd);
    text.Replace(stR("{"), stR("\\{"));
    text.Replace(stR("}"), stR("\\}"));

    // Babel replaces Umlaute by constructs like \"a - and \" isn't allowed in
    // math mode. Fortunately amsTeX provides the \text command that allows to
    // switch to plain text mode again - but with the math font size.
    text.Replace(stR("ä"), stR("\\text{ä}"));
    text.Replace(stR("ö"), stR("\\text{ö}"));
    text.Replace(stR("ü"), stR("\\text{ü}"));
    text.Replace(stR("Ä"), stR("\\text{Ä}"));
    text.Replace(stR("Ö"), stR("\\text{Ö}"));
    text.Replace(stR("Ü"), stR("\\text{Ü}"));
  }
  
  text.Replace(stR("<"), mathModeStart + wxT('<') + mathModeEnd);
  text.Replace(stR(">"), mathModeStart + wxT('>') + mathModeEnd);
  text.Replace(stR("\u2212"), stR("-")); // unicode minus sign
  text.Replace(stR("\u00B1"), mathModeStart + stR("\\pm") + mathModeEnd);
  text.Replace(stR("\u03B1"), mathModeStart + stR("\\alpha") + mathModeEnd);
  text.Replace(stR("\u00B2"), mathModeStart + stR("^2") + mathModeEnd);
  text.Replace(stR("\u00B3"), mathModeStart + stR("^3") + mathModeEnd);
  text.Replace(stR("\u221A"), mathModeStart + stR("\\sqrt{}") + mathModeEnd);
  text.Replace(stR("\u2148"), mathModeStart + stR("\\mathbbm{i}") + mathModeEnd);
  text.Replace(stR("\u2147"), mathModeStart + stR("\\mathbbm{e}") + mathModeEnd);
  text.Replace(stR("\u210f"), mathModeStart + stR("\\hbar") + mathModeEnd);
  text.Replace(stR("\u2203"), mathModeStart + stR("\\exists") + mathModeEnd);
  text.Replace(stR("\u2204"), mathModeStart + stR("\\nexists") + mathModeEnd);
  text.Replace(stR("\u2208"), mathModeStart + stR("\\in") + mathModeEnd);
  text.Replace(stR("\u21D2"), mathModeStart + stR("\\Longrightarrow") + mathModeEnd);
  text.Replace(stR("\u221e"), mathModeStart + stR("\\infty") + mathModeEnd);
  text.Replace(stR("\u22C0"), mathModeStart + stR("\\wedge") + mathModeEnd);
  text.Replace(stR("\u22C1"), mathModeStart + stR("\\vee") + mathModeEnd);
  text.Replace(stR("\u22bb"), mathModeStart + stR("\\oplus") + mathModeEnd);
  text.Replace(stR("\u22BC"), mathModeStart + stR("\\overline{\\wedge}") + mathModeEnd);
  text.Replace(stR("\u22BB"), mathModeStart + stR("\\overline{\\vee}") + mathModeEnd);
  text.Replace(stR("\u00AC"), mathModeStart + stR("\\setminus") + mathModeEnd);
  text.Replace(stR("\u22C3"), mathModeStart + stR("\\cup") + mathModeEnd);
  text.Replace(stR("\u22C2"), mathModeStart + stR("\\cap") + mathModeEnd);
  text.Replace(stR("\u2286"), mathModeStart + stR("\\subseteq") + mathModeEnd);
  text.Replace(stR("\u2282"), mathModeStart + stR("\\subset") + mathModeEnd);
  text.Replace(stR("\u2288"), mathModeStart + stR("\\not\\subseteq") + mathModeEnd);
  text.Replace(stR("\u0127"), mathModeStart + stR("\\hbar") + mathModeEnd);
  text.Replace(stR("\u0126"), mathModeStart + stR("\\Hbar") + mathModeEnd);
  text.Replace(stR("\u2205"), mathModeStart + stR("\\emptyset") + mathModeEnd);
  text.Replace(stR("\u00BD"), mathModeStart + stR("\\frac{1}{2}") + mathModeEnd);
  text.Replace(stR("\u03B2"), mathModeStart + stR("\\beta") + mathModeEnd);
  text.Replace(stR("\u03B3"), mathModeStart + stR("\\gamma") + mathModeEnd);
  text.Replace(stR("\u03B4"), mathModeStart + stR("\\delta") + mathModeEnd);
  text.Replace(stR("\u03B5"), mathModeStart + stR("\\epsilon") + mathModeEnd);
  text.Replace(stR("\u03B6"), mathModeStart + stR("\\zeta") + mathModeEnd);
  text.Replace(stR("\u03B7"), mathModeStart + stR("\\eta") + mathModeEnd);
  text.Replace(stR("\u03B8"), mathModeStart + stR("\\theta") + mathModeEnd);
  text.Replace(stR("\u03B9"), mathModeStart + stR("\\iota") + mathModeEnd);
  text.Replace(stR("\u03BA"), mathModeStart + stR("\\kappa") + mathModeEnd);
  text.Replace(stR("\u03BB"), mathModeStart + stR("\\lambda") + mathModeEnd);
  text.Replace(stR("\u03BC"), mathModeStart + stR("\\mu") + mathModeEnd);
  text.Replace(stR("\u03BD"), mathModeStart + stR("\\nu") + mathModeEnd);
  text.Replace(stR("\u03BE"), mathModeStart + stR("\\xi") + mathModeEnd);
  text.Replace(stR("\u03BF"), stR("o"));
  text.Replace(stR("\u03C0"), mathModeStart + stR("\\pi") + mathModeEnd);
  text.Replace(stR("\u03C1"), mathModeStart + stR("\\rho") + mathModeEnd);
  text.Replace(stR("\u03C3"), mathModeStart + stR("\\sigma") + mathModeEnd);
  text.Replace(stR("\u03C4"), mathModeStart + stR("\\tau") + mathModeEnd);
  text.Replace(stR("\u03C5"), mathModeStart + stR("\\upsilon") + mathModeEnd);
  text.Replace(stR("\u03C6"), mathModeStart + stR("\\phi") + mathModeEnd);
  text.Replace(stR("\u03C7"), mathModeStart + stR("\\chi") + mathModeEnd);
  text.Replace(stR("\u03C8"), mathModeStart + stR("\\psi") + mathModeEnd);
  text.Replace(stR("\u03C9"), mathModeStart + stR("\\omega") + mathModeEnd);
  text.Replace(stR("\u0391"), stR("A"));
  text.Replace(stR("\u0392"), stR("B"));
  text.Replace(stR("\u0393"), mathModeStart + stR("\\Gamma") + mathModeEnd);
  text.Replace(stR("\u0394"), mathModeStart + stR("\\Delta") + mathModeEnd);
  text.Replace(stR("\u0395"), stR("E"));
  text.Replace(stR("\u0396"), stR("Z"));
  text.Replace(stR("\u0397"), stR("H"));
  text.Replace(stR("\u0398"), mathModeStart + stR("\\Theta") + mathModeEnd);
  text.Replace(stR("\u0399"), stR("I"));
  text.Replace(stR("\u039A"), stR("K"));
  text.Replace(stR("\u039B"), mathModeStart + stR("\\Lambda") + mathModeEnd);
  text.Replace(stR("\u039C"), stR("M"));
  text.Replace(stR("\u039D"), stR("N"));
  text.Replace(stR("\u039E"), mathModeStart + stR("\\Xi") + mathModeEnd);
  text.Replace(stR("\u039F"), stR("O"));
  text.Replace(stR("\u03A0"), mathModeStart + stR("\\Pi") + mathModeEnd);
  text.Replace(stR("\u03A1"), stR("P"));
  text.Replace(stR("\u03A3"), mathModeStart + stR("\\Sigma") + mathModeEnd);
  text.Replace(stR("\u03A4"), stR("T"));
  text.Replace(stR("\u03A5"), mathModeStart + stR("\\Upsilon") + mathModeEnd);
  text.Replace(stR("\u03A6"), mathModeStart + stR("\\Phi") + mathModeEnd);
  text.Replace(stR("\u03A7"), stR("X"));
  text.Replace(stR("\u03A8"), mathModeStart + stR("\\Psi") + mathModeEnd);
  text.Replace(stR("\u03A9"), mathModeStart + stR("\\Omega") + mathModeEnd);
  text.Replace(stR("\u2202"), mathModeStart + stR("\\partial") + mathModeEnd);
  text.Replace(stR("\u222b"), mathModeStart + stR("\\int") + mathModeEnd);
  text.Replace(stR("\u2245"), mathModeStart + stR("\\approx") + mathModeEnd);
  text.Replace(stR("\u221d"), mathModeStart + stR("\\propto") + mathModeEnd);
  text.Replace(stR("\u2260"), mathModeStart + stR("\\neq") + mathModeEnd);
  text.Replace(stR("\u2264"), mathModeStart + stR("\\leq") + mathModeEnd);
  text.Replace(stR("\u2265"), mathModeStart + stR("\\geq") + mathModeEnd);
  text.Replace(stR("\u226A"), mathModeStart + stR("\\ll") + mathModeEnd);
  text.Replace(stR("\u226B"), mathModeStart + stR("\\gg") + mathModeEnd);
  text.Replace(stR("\u220e"), mathModeStart + stR("\\blacksquare") + mathModeEnd);
  text.Replace(stR("\u2263"), mathModeStart + stR("\\equiv") + mathModeEnd);
  text.Replace(stR("\u2211"), mathModeStart + stR("\\sum") + mathModeEnd);
  text.Replace(stR("\u220F"), mathModeStart + stR("\\prod") + mathModeEnd);
  text.Replace(stR("\u2225"), mathModeStart + stR("\\parallel") + mathModeEnd);
  text.Replace(stR("\u27C2"), mathModeStart + stR("\\bot") + mathModeEnd);
  text.Replace(stR("~"), mathModeStart + stR("\\sim ") + mathModeEnd);
  text.Replace(stR("_"), stR("\\_ "));
  text.Replace(stR("$"), stR("\\$ "));
  text.Replace(stR("%"), stR("\\% "));
  text.Replace(stR("&"), stR("\\& "));
  text.Replace(stR("@"), mathModeStart + stR("@") + mathModeEnd);
  text.Replace(stR("#"), mathModeStart + stR("\\neq") + mathModeEnd);
  text.Replace(stR("\u00A0"), stR("~")); // A non-breakable space
  text.Replace(stR("<"), mathModeStart + stR("<") + mathModeEnd);
  text.Replace(stR(">"), mathModeStart + stR(">") + mathModeEnd);
  text.Replace(stR("\u219D"), mathModeStart + stR("\\leadsto") + mathModeEnd);
  text.Replace(stR("\u2192"), mathModeStart + stR("\\rightarrow") + mathModeEnd);
  text.Replace(stR("\u2794"), mathModeStart + stR("\\longrightarrow") + mathModeEnd);

  // m_IsHidden is set for parenthesis that don't need to be shown
  if (m_isHidden || (((*m_configuration)->HidemultiplicationSign()) && m_isHidableMultSign))
  {
    // Normally in TeX the spacing between variable names following each other directly
    // is chosen to show that this is a multiplication.
    // But any use of \mathit{} will change to ordinary text spacing which means we need
    // to add a \, to show that we want to multiply the two long variable names.
    if ((text == wxT("*")) || (text == wxT("\u00B7")))
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
        text = stR("\\, ");
      else
        text = wxT(' ');
    }
    else
      text.clear();
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
      text.Replace(stR("*"), stR("\\, "));
      text.Replace(stR("\u00B7"), stR("\\, "));
    }
    else
    {
      // If we want to know if the last element was a "d" we first have to
      // look if there actually is a last element.
      if (m_previous)
      {
        if (m_previous->GetStyle() == TS_SPECIAL_CONSTANT && m_previous->ToTeX() == wxT("d"))
        {
          text.Replace(stR("*"), stR("\\, "));
          text.Replace(stR("\u00B7"), stR("\\, "));
        }
        else
        {
          text.Replace(stR("*"), stR("\\cdot "));
          text.Replace(stR("\u00B7"), stR("\\cdot "));
        }
      }
    }
  }

  if (GetStyle() == TS_GREEK_CONSTANT)
  {
    if (text == wxT("\\% alpha"))
      return stR("\\alpha ");
    else if (text == wxT("\\% beta"))
      return stR("\\beta ");
    else if (text == wxT("\\% gamma"))
      return stR("\\gamma ");
    else if (text == wxT("\\% delta"))
      return stR("\\delta ");
    else if (text == wxT("\\% epsilon"))
      return stR("\\epsilon ");
    else if (text == wxT("\\% zeta"))
      return stR("\\zeta ");
    else if (text == wxT("\\% eta"))
      return stR("\\eta ");
    else if (text == wxT("\\% theta"))
      return stR("\\theta ");
    else if (text == wxT("\\% iota"))
      return stR("\\iota ");
    else if (text == wxT("\\% kappa"))
      return stR("\\kappa ");
    else if (text == wxT("\\% lambda"))
      return stR("\\lambda ");
    else if (text == wxT("\\% mu"))
      return stR("\\mu ");
    else if (text == wxT("\\% nu"))
      return stR("\\nu ");
    else if (text == wxT("\\% xi"))
      return stR("\\ui ");
    else if (text == wxT("\\% omicron"))
      return stR("\\omicron ");
    else if (text == wxT("\\% pi"))
      return stR("\\pi ");
    else if (text == wxT("\\% rho"))
      return stR("\\rho ");
    else if (text == wxT("\\% sigma"))
      return stR("\\sigma ");
    else if (text == wxT("\\% tau"))
      return stR("\\tau ");
    else if (text == wxT("\\% upsilon"))
      return stR("\\upsilon ");
    else if (text == wxT("\\% phi"))
      return stR("\\phi ");
    else if (text == wxT("\\% chi"))
      return stR("\\chi ");
    else if (text == wxT("\\% psi"))
      return stR("\\psi ");
    else if (text == wxT("\\% omega"))
      return stR("\\omega ");
    else if (text == wxT("\\% Alpha"))
      return stR("A");
    else if (text == wxT("\\% Beta"))
      return stR("B");
    else if (text == wxT("\\% Gamma"))
      return stR("\\Gamma ");
    else if (text == wxT("\\% Delta"))
      return stR("\\Delta ");
    else if (text == wxT("\\% Epsilon"))
      return stR("\\Epsilon ");
    else if (text == wxT("\\% Zeta"))
      return stR("\\Zeta ");
    else if (text == wxT("\\% Eta"))
      return stR("\\Eta ");
    else if (text == wxT("\\% Theta"))
      return stR("\\Theta ");
    else if (text == wxT("\\% Iota"))
      return stR("\\Iota ");
    else if (text == wxT("\\% Kappa"))
      return stR("\\Kappa ");
    else if (text == wxT("\\% Lambda"))
      return stR("\\Lambda ");
    else if (text == wxT("\\% Mu"))
      return stR("\\Mu ");
    else if (text == wxT("\\% Nu"))
      return stR("\\Nu ");
    else if (text == wxT("\\% Xi"))
      return stR("\\ui ");
    else if (text == wxT("\\% Omicron"))
      return stR("\\Omicron ");
    else if (text == wxT("\\% Pi"))
      return stR("\\Pi ");
    else if (text == wxT("\\% Rho"))
      return stR("\\Rho ");
    else if (text == wxT("\\% Sigma"))
      return stR("\\Sigma ");
    else if (text == wxT("\\% Tau"))
      return stR("\\Tau ");
    else if (text == wxT("\\% Upsilon"))
      return stR("\\Upsilon ");
    else if (text == wxT("\\% Phi"))
      return stR("\\Phi ");
    else if (text == wxT("\\% Chi"))
      return stR("\\Chi ");
    else if (text == wxT("\\% Psi"))
      return stR("\\Psi ");
    else if (text == wxT("\\% Omega"))
      return stR("\\Omega ");

    return text;
  }

  if (GetStyle() == TS_SPECIAL_CONSTANT)
  {
    if (text == wxT("inf"))
      return stR("\\infty ");
    else if (text == wxT("%e"))
      return stR("e");
    else if (text == wxT("%i"))
      return stR("i");
    else if (text == wxT("\\% pi"))
      return stR("\\ensuremath{\\pi} ");
    else
      return text;
  }

  if ((GetStyle() == TS_LABEL) || (GetStyle() == TS_USERLABEL))
  {
    wxString conditionalLinebreak;
    if (m_previous) conditionalLinebreak = stR("\\]\n\\[");
    text.Trim(true);
    wxString label = text.SubString(1, text.Length() - 2);
    text = conditionalLinebreak + stR("\\tag{") + label + wxT('}');
    label.Replace(stR("\\% "), {});
    // Would be a good idea, but apparently breaks mathJaX
    // text += stR("\\label{") + label + wxT('}');
  }
  else
  {
    if (GetStyle() == TS_FUNCTION)
    {
      if (!text.IsEmpty())
        text = stR("\\operatorname{") + text + wxT('}');
    }
    else if (GetStyle() == TS_VARIABLE)
    {
      if ((m_displayedText.Length() > 1) && (text[1] != wxT('_')))
        text = stR("\\mathit{") + text + wxT('}');
      if (text == wxT("\\% pi"))
        text = stR("\\ensuremath{\\pi} ");
      text.Replace(stR("\\text{ä}"), stR("\\text{\\textit{ä}}"));
      text.Replace(stR("\\text{ö}"), stR("\\text{\\textit{ö}}"));
      text.Replace(stR("\\text{ü}"), stR("\\text{\\textit{ü}}"));
      text.Replace(stR("\\text{Ä}"), stR("\\text{\\textit{Ä}}"));
      text.Replace(stR("\\text{Ö}"), stR("\\text{\\textit{Ö}}"));
      text.Replace(stR("\\text{Ü}"), stR("\\text{\\textit{Ü}}"));
    }
    else if ((GetStyle() == TS_ERROR) || (GetStyle() == TS_WARNING))
    {
      if (text.Length() > 1)
        text = stR("\\mbox{") + text + wxT('}');
    }
    else if (GetStyle() == TS_DEFAULT)
    {
      if ((text.Length() > 2) && (text != wxT("\\,")) && (text != wxT("\\, ")))
        text = stR("\\mbox{") + text + wxT('}');
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
    text.Replace(stR("^"), stR("\\textasciicircum"));

  if ((GetStyle() == TS_DEFAULT) || (GetStyle() == TS_STRING))
  {
    if (text.Length() > 1)
    {
      if (((m_forceBreakLine) || (m_breakLine)))
        //text=stR("\\ifhmode\\\\fi\n")+text;
        text = stR("\\mbox{}\\\\") + text;
/*      if(GetStyle() != TS_DEFAULT)
        text.Replace(stR(" "), stR("\\, "));*/
    }
  }

  if ((GetStyle() == TS_LABEL) || (GetStyle() == TS_USERLABEL))
    text += wxT(' ');

  return text;
}

wxString TextCell::ToMathML()
{
  if(m_displayedText.IsEmpty())
    return {};
  wxString text = XMLescape(m_displayedText);

  if ((*m_configuration)->UseUserLabels() && !m_userDefinedLabel.IsEmpty())
    text = XMLescape(wxT('(') + m_userDefinedLabel + wxT(')'));

  // If we didn't display a multiplication dot we want to do the same in MathML.
  if (m_isHidden || (((*m_configuration)->HidemultiplicationSign()) && m_isHidableMultSign))
  {
    text.Replace(stR("*"), stR("&#8290;"));
    text.Replace(stR("\u00B7"), stR("&#8290;"));
    if (text != wxT("&#8290;"))
      text.clear();
  }
  text.Replace(stR("*"), stR("\u00B7"));

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
      // if((GetStyle() == TS_SPECIAL_CONSTANT) && (text == wxT("d")))
      //   text = stR("&#2146;");
      bool keepPercent = (*m_configuration)->CheckKeepPercent();
      if (!keepPercent)
      {
        if (text == wxT("%e"))
          text = wxT('e');
        else if (text == wxT("%i"))
          text = wxT('i');
      }
    }
    break;
  case TS_VARIABLE:
    {
      text = GetGreekStringUnicode();

      bool keepPercent = (*m_configuration)->CheckKeepPercent();

      if (!keepPercent)
      {
        if (text == wxT("%pi"))
          text = stR("\u03C0");
      }
    }
    break;
  case TS_FUNCTION:
      text = GetGreekStringUnicode();
      if (text == wxT("inf"))
        text = stR("\u221e");
      if((text == wxT("+")) || (text == wxT("-")) || (text == wxT("*")) || (text == wxT("/")))
        return stR("<mo>") + text + stR("</mo>\n");
      else
        return stR("<mi>") + text + stR("</mi>\n");
    case TS_NUMBER:
      return stR("<mn>") + text + stR("</mn>\n");

    case TS_LABEL:
    case TS_USERLABEL:
      return stR("<mtext>") + text + stR("</mtext></mtd><mtd>\n");

    case TS_STRING:
    default:
      if (text.StartsWith(stR("\"")))
        return stR("<ms>") + text + stR("</ms>\n");
      else
        return stR("<mo>") + text + stR("</mo>\n");
  }

  return stR("<mo>") + text + stR("</mo>\n");
}

wxString TextCell::ToOMML()
{
  //Text-only lines are better handled in RTF.
  if (
          ((m_previous != NULL) && (m_previous->GetStyle() != TS_LABEL) && (!m_previous->HardLineBreak())) &&
          (HardLineBreak())
          )
    return {};

  // Labels are text-only.
  if ((GetStyle() == TS_LABEL) || (GetStyle() == TS_USERLABEL))
    return {};

  wxString text = XMLescape(m_displayedText);

  // If we didn't display a multiplication dot we want to do the same in MathML.
  if (m_isHidden || (((*m_configuration)->HidemultiplicationSign()) && m_isHidableMultSign))
  {
    text.Replace(stR("*"), stR("&#8290;"));
    text.Replace(stR("\u00B7"), stR("&#8290;"));
    if (text != wxT ("&#8290;"))
      text.clear();
  }
  text.Replace(stR("*"), stR("\u00B7"));

  switch (GetStyle())
  {
    case TS_GREEK_CONSTANT:
    case TS_SPECIAL_CONSTANT:
    {
      // The "d" from d/dt can be written as a special unicode symbol. But firefox doesn't
      // support this currently => Commenting it out.
      // if((GetStyle() == TS_SPECIAL_CONSTANT) && (text == wxT("d")))
      //   text = stR("&#2146;");
      bool keepPercent = (*m_configuration)->CheckKeepPercent();
      if (!keepPercent)
      {
        if (text == wxT("%e"))
          text = wxT('e');
        else if (text == wxT("%i"))
          text = wxT('i');
      }
    }
    /* FALLTHRU */
  case TS_VARIABLE:
    {
      if (!(*m_configuration)->CheckKeepPercent())
      {
        if (text == wxT("%pi"))
          text = stR("\u03C0");
      }
    }
    break;
  case TS_FUNCTION:
      text = GetGreekStringUnicode();
      if (text == wxT("inf"))
        text = stR("\u221e");
      break;
    case TS_NUMBER:
      break;

    case TS_LABEL:
    case TS_USERLABEL:
      return {};

    case TS_STRING:
    default:
    {
    }
  }
  text = stR("<m:r>") + text + stR("</m:r>\n");
  return text;
}

wxString TextCell::ToRTF()
{
  wxString retval;
  wxString text = m_displayedText;

  if (m_displayedText.IsEmpty())
    return L' ';
  
  if(((*m_configuration)->UseUserLabels())&&(!m_userDefinedLabel.IsEmpty()))
    text = wxT('(') + m_userDefinedLabel + wxT(')');
  
  text.Replace(stR("-->"), stR("\u2192"));
  // Needed for the output of let(a/b,a+1);
  text.Replace(stR(" --> "), stR("\u2192"));
  if ((GetStyle() == TS_LABEL) || (GetStyle() == TS_USERLABEL))
  {
    retval += wxString::Format(stR("\\cf%i{"), (int) GetStyle());
    retval += RTFescape(text);
    retval += stR("}\\cf0");
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
        flags += stR(" userdefined=\"yes\"");
        break;
      default:
        tag = _T("t");
    }

  if ((m_forceBreakLine) && (GetStyle() != TS_LABEL) && (GetStyle() != TS_USERLABEL))
    flags += stR(" breakline=\"true\"");

  if (GetStyle() == TS_ERROR)
    flags += stR(" type=\"error\"");

  if (GetStyle() == TS_WARNING)
    flags += stR(" type=\"warning\"");
  
  wxString xmlstring = XMLescape(m_displayedText);
  // convert it, so that the XML configuration doesn't fail
  if(!m_userDefinedLabel.IsEmpty())
    flags += stR(" userdefinedlabel=\"") + XMLescape(m_userDefinedLabel) + wxT('"');

  if(!m_altCopyText.IsEmpty())
    flags += stR(" altCopy=\"") + XMLescape(m_altCopyText) + wxT('"');

  if (!m_toolTip.IsEmpty()    )
    flags += stR(" tooltip=\"") + XMLescape(m_toolTip) + wxT('"');

  return wxT('<') + tag + flags + wxT('>') + xmlstring + wxT("</") + tag + wxT('>');
}

wxString TextCell::GetDiffPart()
{
  return wxT(',') + m_text + stR(",1");
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
    if (!m_altJsText.IsEmpty())
    {
      if (m_text == wxT("+") || m_text == wxT("="))
        m_texFontname = CMR10;
      else if (m_text == wxT("%pi"))
        m_texFontname = CMMI10;
      else
        m_texFontname = CMSY10;
    }
    m_altText = GetSymbolUnicode((*m_configuration)->CheckKeepPercent());
// #if defined __WXMSW__
//     m_altText = GetSymbolSymbol(configuration->CheckKeepPercent());
//     if (!m_altText.IsEmpty())
//     {
//       m_alt = true;
//       m_fontname = wxT("Symbol");
//     }
// #endif
  }
}

wxString TextCell::GetGreekStringUnicode() const
{
  wxString txt(m_text);

  if (txt[0] != '%')
    txt = wxT('%') + txt;

  if (txt == wxT("%alpha"))
    return L'\u03B1';
  else if (txt == wxT("%beta"))
    return L'\u03B2';
  else if (txt == wxT("%gamma"))
    return L'\u03B3';
  else if (txt == wxT("%delta"))
    return L'\u03B4';
  else if (txt == wxT("%epsilon"))
    return L'\u03B5';
  else if (txt == wxT("%zeta"))
    return L'\u03B6';
  else if (txt == wxT("%eta"))
    return L'\u03B7';
  else if (txt == wxT("%theta"))
    return L'\u03B8';
  else if (txt == wxT("%iota"))
    return L'\u03B9';
  else if (txt == wxT("%kappa"))
    return L'\u03BA';
  else if (txt == wxT("%lambda"))
    return L'\u03BB';
  else if (txt == wxT("%mu"))
    return L'\u03BC';
  else if (txt == wxT("%nu"))
    return L'\u03BD';
  else if (txt == wxT("%xi"))
    return L'\u03BE';
  else if (txt == wxT("%omicron"))
    return L'\u03BF';
  else if (txt == wxT("%pi"))
    return L'\u03C0';
  else if (txt == wxT("%rho"))
    return L'\u03C1';
  else if (txt == wxT("%sigma"))
    return L'\u03C3';
  else if (txt == wxT("%tau"))
    return L'\u03C4';
  else if (txt == wxT("%upsilon"))
    return L'\u03C5';
  else if (txt == wxT("%phi"))
    return L'\u03C6';
  else if (txt == wxT("%chi"))
    return L'\u03C7';
  else if (txt == wxT("%psi"))
    return L'\u03C8';
  else if (txt == wxT("%omega"))
    return L'\u03C9';
  else if (txt == wxT("%Alpha"))
    return L'\u0391';
  else if (txt == wxT("%Beta"))
    return L'\u0392';
  else if (txt == wxT("%Gamma"))
    return L'\u0393';
  else if (txt == wxT("%Delta"))
    return L'\u0394';
  else if (txt == wxT("%Epsilon"))
    return L'\u0395';
  else if (txt == wxT("%Zeta"))
    return L'\u0396';
  else if (txt == wxT("%Eta"))
    return L'\u0397';
  else if (txt == wxT("%Theta"))
    return L'\u0398';
  else if (txt == wxT("%Iota"))
    return L'\u0399';
  else if (txt == wxT("%Kappa"))
    return L'\u039A';
  else if (txt == wxT("%Lambda"))
    return L'\u039B';
  else if (txt == wxT("%Mu"))
    return L'\u039C';
  else if (txt == wxT("%Nu"))
    return L'\u039D';
  else if (txt == wxT("%Xi"))
    return L'\u039E';
  else if (txt == wxT("%Omicron"))
    return L'\u039F';
  else if (txt == wxT("%Pi"))
    return L'\u03A0';
  else if (txt == wxT("%Rho"))
    return L'\u03A1';
  else if (txt == wxT("%Sigma"))
    return L'\u03A3';
  else if (txt == wxT("%Tau"))
    return L'\u03A4';
  else if (txt == wxT("%Upsilon"))
    return L'\u03A5';
  else if (txt == wxT("%Phi"))
    return L'\u03A6';
  else if (txt == wxT("%Chi"))
    return L'\u03A7';
  else if (txt == wxT("%Psi"))
    return L'\u03A8';
  else if (txt == wxT("%Omega"))
    return L'\u03A9';

  return m_text;
}

wxString TextCell::GetSymbolUnicode(bool keepPercent) const
{
  if (m_text == wxT('+'))
    return L'+';
  else if (m_text == wxT('='))
    return L'=';
  else if (m_text == wxT("inf"))
    return L'\u221E';
  else if (m_text == wxT("%pi"))
    return L'\u03C0';
  else if (m_text == wxT("<="))
    return L'\u2264';
  else if (m_text == wxT(">="))
    return L'\u2265';
  #ifndef __WXMSW__
  else if (m_text == wxT(" and "))
    return stR(" \u22C0 ");
  else if (m_text == wxT(" or "))
    return stR(" \u22C1 ");
  else if (m_text == wxT(" xor "))
    return stR(" \u22BB ");
  else if (m_text == wxT(" nand "))
    return stR(" \u22BC ");
  else if (m_text == wxT(" nor "))
    return stR(" \u22BD ");
  else if (m_text == wxT(" implies "))
    return stR(" \u21D2 ");
  else if (m_text == wxT(" equiv "))
    return stR(" \u21D4 ");
  else if (m_text == wxT("not"))
    return L'\u00AC';
  #endif
  else if (m_text == wxT("->"))
    return L'\u2192';
  else if (m_text == wxT("-->"))
    return L'\u2794';
  // The next two ones are needed for the output of let(a/b,a+1);
  else if (m_text == wxT(" --> "))
    return L'\u2794';
  else if (m_text == wxT(" \u2212\u2192 "))
    return L'\u2794';
  /*
   else if (GetStyle() == TS_SPECIAL_CONSTANT && m_text == wxT('d'))
     return L'\u2202';
   */

  if (!keepPercent)
  {
    if (m_text == wxT("%e"))
      return L'e';
    else if (m_text == wxT("%i"))
      return L'i';
    else if (m_text == wxT("%pi"))
      return L'\u03C0';
  }

  return {};
}

wxString TextCell::GetGreekStringTeX() const
{
  if (m_text == wxT("gamma"))
    return L'\u00C0';
  else if (m_text == wxT("zeta"))
    return L'\u00B0';
  else if (m_text == wxT("psi"))
    return L'\u00C9';

  wxString txt(m_text);
  if (txt[0] != '%')
    txt = wxT('%') + txt;

  if (txt == wxT("%alpha"))
    return L'\u00CB';
  else if (txt == wxT("%beta"))
    return L'\u00CC';
  else if (txt == wxT("%gamma"))
    return L'\u00CD';
  else if (txt == wxT("%delta"))
    return L'\u00CE';
  else if (txt == wxT("%epsilon"))
    return L'\u00CF';
  else if (txt == wxT("%zeta"))
    return L'\u00B0';
  else if (txt == wxT("%eta"))
    return L'\u00D1';
  else if (txt == wxT("%theta"))
    return L'\u00D2';
  else if (txt == wxT("%iota"))
    return L'\u00D3';
  else if (txt == wxT("%kappa"))
    return L'\u00D4';
  else if (txt == wxT("%lambda"))
    return L'\u00D5';
  else if (txt == wxT("%mu"))
    return L'\u00D6';
  else if (txt == wxT("%nu"))
    return L'\u00B7';
  else if (txt == wxT("%xi"))
    return L'\u00D8';
  else if (txt == wxT("%omicron"))
    return L'o';
  else if (txt == wxT("%pi"))
    return L'\u00D9';
  else if (txt == wxT("%rho"))
    return L'\u00DA';
  else if (txt == wxT("%sigma"))
    return L'\u00DB';
  else if (txt == wxT("%tau"))
    return L'\u00DC';
  else if (txt == wxT("%upsilon"))
    return L'\u00B5';
  else if (txt == wxT("%chi"))
    return L'\u00DF';
  else if (txt == wxT("%psi"))
    return L'\u00EF';
  else if (txt == wxT("%phi"))
    return L'\u0027';
  else if (txt == wxT("%omega"))
    return L'\u0021';
  else if (txt == wxT("%Alpha"))
    return L'A';
  else if (txt == wxT("%Beta"))
    return L'B';
  else if (txt == wxT("%Gamma"))
    return L'\u00C0';
  else if (txt == wxT("%Delta"))
    return L'\u00C1';
  else if (txt == wxT("%Epsilon"))
    return L'E';
  else if (txt == wxT("%Zeta"))
    return L'Z';
  else if (txt == wxT("%Eta"))
    return L'H';
  else if (txt == wxT("%Theta"))
    return L'\u00C2';
  else if (txt == wxT("%Iota"))
    return L'I';
  else if (txt == wxT("%Kappa"))
    return L'K';
  else if (txt == wxT("%Lambda"))
    return L'\u00C3';
  else if (txt == wxT("%Mu"))
    return L'M';
  else if (txt == wxT("%Nu"))
    return L'N';
  else if (txt == wxT("%Xi"))
    return L'\u00C4';
  else if (txt == wxT("%Omicron"))
    return L'O';
  else if (txt == wxT("%Pi"))
    return L'\u00C5';
  else if (txt == wxT("%Rho"))
    return L'P';
  else if (txt == wxT("%Sigma"))
    return L'\u00C6';
  else if (txt == wxT("%Tau"))
    return L'T';
  else if (txt == wxT("%Upsilon"))
    return L'Y';
  else if (txt == wxT("%Phi"))
    return L'\u00C8';
  else if (txt == wxT("%Chi"))
    return L'X';
  else if (txt == wxT("%Psi"))
    return L'\u00C9';
  else if (txt == wxT("%Omega"))
    return L'\u00CA';

  return {};
}

wxString TextCell::GetSymbolTeX() const
{
  if (m_text == wxT("inf"))
    return L'\u0031';
  else if (m_text == wxT("+"))
    return L'+';
  else if (m_text == wxT("%pi"))
    return L'\u00D9';
  else if (m_text == wxT("="))
    return L'=';
  else if (m_text == wxT("->"))
    return L'\u0021';
  else if (m_text == wxT(">="))
    return L'\u00D5';
  else if (m_text == wxT("<="))
    return L'\u00D4';
/*
  else if (m_text == wxT(" and "))
    return wxT(" \u005E ");
  else if (m_text == wxT(" or "))
    return wxT(" \u005F ");
  else if (m_text == wxT(" nand "))
    return wxT(" \u0022 ");
  else if (m_text == wxT(" nor "))
    return wxT(" \u0023 ");
  else if (m_text == wxT(" eq "))
    return wxT(" \u002C ");
  else if (m_text == wxT(" implies "))
    return wxT(" \u0029 ");
  else if (m_text == wxT("not"))
    return L'\u003A';
  else if (m_text == wxT(" xor "))
    return L'\u00C8';
*/

  return {};
}

void TextCell::SetNextToDraw(Cell *next)
{
  m_nextToDraw = next;
}

// RegExes all TextCells share.
wxRegEx TextCell::m_unescapeRegEx(wxT("\\\\(.)"));
wxRegEx TextCell::m_roundingErrorRegEx1(wxT("\\.000000000000[0-9]+$"));
wxRegEx TextCell::m_roundingErrorRegEx2(wxT("\\.999999999999[0-9]+$"));
wxRegEx TextCell::m_roundingErrorRegEx3(wxT("\\.000000000000[0-9]+e"));
wxRegEx TextCell::m_roundingErrorRegEx4(wxT("\\.999999999999[0-9]+e"));
