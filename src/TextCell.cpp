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

/*! \file
  This file defines the class TextCell

  TextCell is the Cell type that is used in order to display text that is
  contained in maxima's output.
 */

#include "TextCell.h"
#include "StringUtils.h"
#include "wx/config.h"

TextCell::TextCell(GroupCell *parent, Configuration **config,
                   const wxString &text, TextStyle style) :
  Cell(parent, config)
{
  InitBitFields();
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
  m_fontSize.Set(10.0f);
  TextCell::SetValue(text);
}

std::unique_ptr<Cell> TextCell::Copy() const
{
  return std::make_unique<TextCell>(*this);
}

void TextCell::SetStyle(TextStyle style)
{
  m_sizeCache.clear();
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
  m_sizeCache.clear();
  ResetSize();
  ResetData();
  Cell::SetType(type);
}

void TextCell::SetAltCopyText(const wxString &text)
{
  // Numbers with altCopyText are scary
  wxASSERT((m_textStyle != TS_NUMBER) || text.empty());
  m_altCopyText = text;
}

void TextCell::UpdateToolTip()
{
  if (m_promptTooltip)
    SetToolTip(
          &T_("Most questions can be avoided using the assume() and the "
            "declare() command. If that isn't possible the \"Automatically "
            "answer questions\" button makes wxMaxima automatically fill in "
            "all answers it still remembers from a previous run."));

  if (m_text.empty())
    return;

  auto const &c_text = m_text;

  if (m_textStyle == TS_VARIABLE)
  {
    if (m_text == wxT("pnz"))
      SetToolTip(&T_(
          "Either positive, negative or zero.\n"
          "Normally the result of sign() if the sign cannot be determined."));

    else if (m_text == wxT("pz"))
      SetToolTip(&T_("Either positive or zero.\n"
                     "A possible result of sign()."));

    else if (m_text == wxT("nz"))
      SetToolTip(&T_("Either negative or zero.\n"
                     "A possible result of sign()."));

    else if (m_text == wxT("und"))
      SetToolTip(&T_("The result was undefined."));

    else if (m_text == wxT("ind"))
      SetToolTip(&T_("The result was indefinite, which might be infinity, both "
                     "plus or minus infinity or something additionally "
                     "potentially involving a complex infinity."));

    else if (m_text == wxT("zeroa"))
      SetToolTip(&T_("Infinitesimal above zero."));

    else if (m_text == wxT("zerob"))
      SetToolTip(&T_("Infinitesimal below zero."));

    else if (m_text == wxT("inf"))
      SetToolTip(&S_("+∞."));

    else if (m_text == wxT("infinity"))
      SetToolTip(&T_("Complex infinity."));
        
    else if (m_text == wxT("inf"))
      SetToolTip(&S_("-∞."));

    else if (m_text.StartsWith(S_("%r")))
    {
      if (std::all_of(std::next(c_text.begin(), 2), c_text.end(), wxIsdigit))
        SetToolTip(&T_("A variable that can be assigned a number to.\n"
                       "Often used by solve() and algsys(), if there is an "
                       "infinite number of results."));
    }
    else if (m_text.StartsWith(S_("%i")))
    {
      if (std::all_of(std::next(c_text.begin(), 2), c_text.end(), wxIsdigit))
        SetToolTip(&T_("An integration constant."));
    }
  }
  
  else if (m_textStyle == TS_NUMBER)
  {
    if(
      (m_roundingErrorRegEx1.Matches(m_text)) ||
      (m_roundingErrorRegEx2.Matches(m_text)) ||
      (m_roundingErrorRegEx3.Matches(m_text)) ||
      (m_roundingErrorRegEx4.Matches(m_text))
      )
      SetToolTip(&T_("As calculating 0.1^12 demonstrates maxima by default doesn't tend to "
                       "hide what looks like being the small error using floating-point "
                     "numbers introduces.\n"
                     "If this seems to be the case here the error can be avoided by using "
                     "exact numbers like 1/10, 1*10^-1 or rat(.1).\n"
                     "It also can be hidden by setting fpprintprec to an appropriate value. "
                     "But be aware in this case that even small errors can add up."));
  }

  else
  {
    if (m_text.Contains(S_("LINE SEARCH FAILED. SEE")) ||
        m_text.Contains(S_("DOCUMENTATION OF ROUTINE MCSRCH")) ||
        m_text.Contains(S_("ERROR RETURN OF LINE SEARCH:")) ||
        m_text.Contains(S_("POSSIBLE CAUSES: FUNCTION OR GRADIENT ARE INCORRECT")))
      SetToolTip(&T_("This message can appear when trying to numerically find an optimum. "
                     "In this case it might indicate that a starting point lies in a local "
                     "optimum that fits the data best if one parameter is increased to "
                     "infinity or decreased to -infinity. It also can indicate that an "
                     "attempt was made to fit data to an equation that actually matches "
                     "the data best if one parameter is set to +/- infinity."));

    else if (m_text.StartsWith(S_("incorrect syntax")) &&
             m_text.Contains(S_("is not an infix operator")))
      SetToolTip(&T_("A command or number wasn't preceded by a \":\", a \"$\", a \";\" or a \",\".\n"
                     "Most probable cause: A missing comma between two list items."));
    else if (m_text.StartsWith(S_("incorrect syntax")) &&
             m_text.Contains(S_("Found LOGICAL expression where ALGEBRAIC expression expected")))
      SetToolTip(&T_("Most probable cause: A dot instead a comma between two list items containing assignments."));
    else if (m_text.StartsWith(S_("incorrect syntax")) &&
             m_text.Contains(S_("is not a prefix operator")))
      SetToolTip(&T_("Most probable cause: Two commas or similar separators in a row."));
    else if (m_text.Contains(S_("Illegal use of delimiter")))
      SetToolTip(&T_("Most probable cause: an operator was directly followed by a closing parenthesis."));
    else if (m_text.StartsWith(S_("find_root: function has same sign at endpoints: ")))
      SetToolTip(&T_("find_root only works if the function the solution is searched for crosses the solution exactly once in the given range."));
    else if (m_text.StartsWith(S_("part: fell off the end.")))
      SetToolTip(&T_("part() or the [] operator was used in order to extract the nth element "
                     "of something that was less than n elements long."));
    else if (m_text.StartsWith(S_("rest: fell off the end.")))
      SetToolTip(&T_("rest() tried to drop more entries from a list than the list was long."));
    else if (m_text.StartsWith(S_("assignment: cannot assign to")))
      SetToolTip(&T_("The value of few special variables is assigned by Maxima and "
                     "cannot be changed by the user. Also a few constructs aren't "
                     "variable names and therefore cannot be written to."));
    else if (m_text.StartsWith(S_("rat: replaced ")))
      SetToolTip(&T_("Normally computers use floating-point numbers that can be handled "
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
    else if (m_text.StartsWith(S_("desolve: can't handle this case.")))
      SetToolTip(&T_("The list of time-dependent variables to solve to doesn't match "
                     "the time-dependent variables the list of dgls contains."));
    else if (m_text.StartsWith(S_("expt: undefined: 0 to a negative exponent.")))
      SetToolTip(&T_("Division by 0."));
    else if (m_text.StartsWith(S_("incorrect syntax: parser: incomplete number; missing exponent?")))
      SetToolTip(&T_("Might also indicate a missing multiplication sign (\"*\")."));
    else if (m_text.Contains(S_("arithmetic error DIVISION-BY-ZERO signalled")))
      SetToolTip(&T_("Besides a division by 0 the reason for this error message can be a "
                     "calculation that returns +/-infinity."));
    else if (m_text.Contains(S_("isn't in the domain of")))
      SetToolTip(&T_("Most probable cause: A function was called with a parameter that causes "
                     "it to return infinity and/or -infinity."));
    else if (m_text.StartsWith(S_("Only symbols can be bound")))
      SetToolTip(&T_("This error message is most probably caused by a try to assign "
                     "a value to a number instead of a variable name.\n"
                     "One probable cause is using a variable that already has a numeric "
                     "value as a loop counter."));
    else if (m_text.StartsWith(S_("append: operators of arguments must all be the same.")))
      SetToolTip(&T_("Most probably it was attempted to append something to a list "
                     "that isn't a list.\n"
                     "Enclosing the new element for the list in brackets ([]) "
                     "converts it to a list and makes it appendable."));
    else if (m_text.Contains(S_(": invalid index")))
      SetToolTip(&T_("The [] or the part() command tried to access a list or matrix "
                     "element that doesn't exist."));
    else if (m_text.StartsWith(S_("apply: subscript must be an integer; found:")))
      SetToolTip(&T_("the [] operator tried to extract an element of a list, a matrix, "
                     "an equation or an array. But instead of an integer number "
                     "something was used whose numerical value is unknown or not an "
                     "integer.\n"
                     "Floating-point numbers are bound to contain small rounding errors "
                     "and therefore in most cases don't work as an array index that"
                     "needs to be an exact integer number."));
    else if (m_text.StartsWith(S_(": improper argument: ")))
    {
      auto const prevString = m_previous ? m_previous->ToString() : wxm::emptyString;
      if (prevString == wxT("at"))
        SetToolTip(&T_("The second argument of at() isn't an equation or a list of "
                       "equations. Most probably it was lacking an \"=\"."));
      else if (prevString == wxT("subst"))
        SetToolTip(&T_("The first argument of subst() isn't an equation or a list of "
                       "equations. Most probably it was lacking an \"=\"."));
      else
        SetToolTip(&T_("The argument of a function was of the wrong type. Most probably "
                       "an equation was expected but was lacking an \"=\"."));
    }
  }
}

void TextCell::SetValue(const wxString &text)
{
  m_sizeCache.clear();
  m_text = text;
  ResetSize();
  UpdateDisplayedText();
  UpdateToolTip();
  ResetSize();
}

// cppcheck-suppress uninitMemberVar symbolName=TextCell::m_alt
// cppcheck-suppress uninitMemberVar symbolName=TextCell::m_altJs
// cppcheck-suppress uninitMemberVar symbolName=TextCell::m_initialToolTip
TextCell::TextCell(const TextCell &cell):
    Cell(cell.m_group, cell.m_configuration),
    m_text(cell.m_text),
    m_displayedText(cell.m_displayedText)
{
  InitBitFields();
  CopyCommonData(cell);
  if (!cell.GetAltCopyText().empty())
    SetAltCopyText(cell.GetAltCopyText());
  m_bigSkip = cell.m_bigSkip;
  m_highlight = cell.m_highlight;
  m_dontEscapeOpeningParenthesis = cell.m_dontEscapeOpeningParenthesis;
}

AFontSize TextCell::GetScaledTextSize() const
{
    return Scale_Px(m_fontSize);
}

bool TextCell::NeedsRecalculation(AFontSize fontSize) const
{
  return Cell::NeedsRecalculation(fontSize);
}

wxSize TextCell::GetTextSize(wxDC *const dc, const wxString &text, TextCell::TextIndex const index)
{
  AFontSize const fontSize = GetScaledTextSize();
  if (text.empty())
    return {};

  auto const size = dc->GetTextExtent(text);
  m_sizeCache.emplace_back(size, fontSize, index);
  return size;
}

void TextCell::UpdateDisplayedText()
{
  m_displayedText = m_text;

  Configuration *configuration = (*m_configuration);
  
  m_displayedText.Replace(wxT("\xDCB6"), wxT("\u00A0")); // A non-breakable space
  m_displayedText.Replace(wxT("\n"), wxEmptyString);
  m_displayedText.Replace(wxT("-->"), wxT("\u2794"));
  m_displayedText.Replace(wxT(" -->"), wxT("\u2794"));
  m_displayedText.Replace(wxT(" \u2212\u2192 "), wxT("\u2794"));
  m_displayedText.Replace(wxT("->"), wxT("\u2192"));
  m_displayedText.Replace(wxT("\u2212>"), wxT("\u2192"));
  
  if (m_textStyle == TS_FUNCTION)
  {
    if (m_text == wxT("ilt"))
      SetToolTip(&T_("The inverse laplace transform."));
    
    if (m_text == wxT("gamma"))
      m_displayedText = wxT("\u0393");
    if (m_text == wxT("psi"))
      m_displayedText = wxT("\u03A8");
  }  

  if ((GetStyle() == TS_DEFAULT) && m_text.StartsWith("\""))
    return;
  
  if ((GetStyle() == TS_GREEK_CONSTANT) && (*m_configuration)->Latin2Greek())
    m_displayedText = GetGreekStringUnicode();

  wxString unicodeSym = GetSymbolUnicode((*m_configuration)->CheckKeepPercent());
  if(!unicodeSym.IsEmpty())
    m_displayedText = unicodeSym;

  /// Change asterisk to a multiplication dot, if applicable
  if (configuration->GetChangeAsterisk())
  {
    if(m_displayedText == wxT("*"))
      m_displayedText = wxT("\u00B7");
    if (m_displayedText == wxT("#"))
      m_displayedText = wxT("\u2260");
  }
}

void TextCell::Recalculate(AFontSize fontsize)
{
  Configuration *configuration = (*m_configuration);
  if(NeedsRecalculation(fontsize))
  {      
    Cell::Recalculate(fontsize);
    m_fontSize = m_fontsize_old = fontsize;
    wxDC *dc = configuration->GetDC();
    SetFont(fontsize);


    wxSize sz = GetTextSize((*m_configuration)->GetDC(), m_displayedText, cellText);
    m_width = sz.GetWidth();
    m_height = sz.GetHeight();
    
    m_width += 2 * MC_TEXT_PADDING;
    m_height += 2 * MC_TEXT_PADDING;
    
    /// Hidden cells (multiplication * is not displayed)
    if ((m_isHidden) || ((configuration->HidemultiplicationSign()) && m_isHidableMultSign))
    {
      m_height = 0;
      m_width = Scale_Px(fontsize) / 4;
    }
    if(m_height < Scale_Px(4)) m_height = Scale_Px(4);
    m_center = m_height / 2;
  }
}

void TextCell::Draw(wxPoint point)
{
  Cell::Draw(point);
  Configuration *configuration = (*m_configuration);
  if (DrawThisCell(point) &&
      !(m_isHidden || ((configuration->HidemultiplicationSign()) && m_isHidableMultSign)))
  {
    wxDC *dc = configuration->GetDC();
    
    if (NeedsRecalculation(m_fontSize))
      Recalculate(m_fontSize);
    
    if (InUpdateRegion())
    {
      SetForeground();
      SetFont(m_fontSize);
      dc->DrawText(m_displayedText,
                   point.x + MC_TEXT_PADDING,
                   point.y - m_center + MC_TEXT_PADDING);
    }
  }
}

void TextCell::SetFont(AFontSize fontsize)
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
      (m_textStyle != TS_OTHER_PROMPT)
      )
      m_fontSize = fontsize;
  }

  auto style = configuration->GetStyle(m_textStyle, fontsize);
  
  // Mark special variables that are printed as ordinary letters as being special.
  if ((!(*m_configuration)->CheckKeepPercent()) &&
      ((m_text == wxT("%e")) || (m_text == wxT("%i"))))
  {
    if((*m_configuration)->IsItalic(TS_VARIABLE) != wxFONTSTYLE_NORMAL)
      style.SetItalic(false);
    else
      style.SetItalic(true);
  }

  wxASSERT(m_fontSize.IsValid());
  style.SetFontSize(Scale_Px(m_fontSize));

  dc->SetFont(style.GetFont());
}

bool TextCell::IsOperator() const
{
  if (wxString(wxT("+*/-")).Find(m_text) >= 0)
    return true;
  if (m_text == wxT("\u2212"))
    return true;
  return false;
}

wxString TextCell::ToString() const
{
  wxString text;
  if (!GetAltCopyText().empty())
    text = GetAltCopyText();
  else
  {
    text = m_text;
    text.Replace(wxT("\u2212"), wxT("-")); // unicode minus sign
    text.Replace(wxT("\u2794"), wxT("-->"));
    text.Replace(wxT("\u2192"), wxT("->"));
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
  default:
  {}
  }
  if((m_next != NULL) && (m_next->BreakLineHere()))
    text += "\n";
  
  return text;
}

wxString TextCell::ToMatlab() const
{
  wxString text = ToString();
  if (text == wxT("%e"))
    text = wxT("e");
  else if (text == wxT("%i"))
    text = wxT("i");
  else if (text == wxT("%pi"))
    text = wxString(wxT("pi"));
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

wxString TextCell::ToTeX() const
{
  wxString text = ToString();

  if (!(*m_configuration)->CheckKeepPercent())
  {
    if (text == wxT("%e"))
      text = wxT("e");
    else if (text == wxT("%i"))
      text = wxT("i");
    else if (text == wxT("%pi"))
      text = wxString(wxT("\u03C0"));
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
  
  text.Replace(wxT("<"), mathModeStart + wxT("<") + mathModeEnd);
  text.Replace(wxT(">"), mathModeStart + wxT(">") + mathModeEnd);
  text.Replace(wxT("\u2212"), wxT("-")); // unicode minus sign
  text.Replace(wxT("\u00B1"), mathModeStart + wxT("\\pm") + mathModeEnd);
  text.Replace(wxT("\u03B1"), mathModeStart + wxT("\\alpha") + mathModeEnd);
  text.Replace(wxT("\u00B2"), mathModeStart + wxT("^2") + mathModeEnd);
  text.Replace(wxT("\u00B3"), mathModeStart + wxT("^3") + mathModeEnd);
  text.Replace(wxT("\u221A"), mathModeStart + wxT("\\sqrt{}") + mathModeEnd);
  text.Replace(wxT("\u2148"), mathModeStart + wxT("\\mathbbm{i}") + mathModeEnd);
  text.Replace(wxT("\u2147"), mathModeStart + wxT("\\mathbbm{e}") + mathModeEnd);
  text.Replace(wxT("\u210f"), mathModeStart + wxT("\\hbar") + mathModeEnd);
  text.Replace(wxT("\u2203"), mathModeStart + wxT("\\exists") + mathModeEnd);
  text.Replace(wxT("\u2204"), mathModeStart + wxT("\\nexists") + mathModeEnd);
  text.Replace(wxT("\u2208"), mathModeStart + wxT("\\in") + mathModeEnd);
  text.Replace(wxT("\u21D2"), mathModeStart + wxT("\\Longrightarrow") + mathModeEnd);
  text.Replace(wxT("\u221e"), mathModeStart + wxT("\\infty") + mathModeEnd);
  text.Replace(wxT("\u22C0"), mathModeStart + wxT("\\wedge") + mathModeEnd);
  text.Replace(wxT("\u22C1"), mathModeStart + wxT("\\vee") + mathModeEnd);
  text.Replace(wxT("\u22bb"), mathModeStart + wxT("\\oplus") + mathModeEnd);
  text.Replace(wxT("\u22BC"), mathModeStart + wxT("\\overline{\\wedge}") + mathModeEnd);
  text.Replace(wxT("\u22BB"), mathModeStart + wxT("\\overline{\\vee}") + mathModeEnd);
  text.Replace(wxT("\u00AC"), mathModeStart + wxT("\\setminus") + mathModeEnd);
  text.Replace(wxT("\u22C3"), mathModeStart + wxT("\\cup") + mathModeEnd);
  text.Replace(wxT("\u22C2"), mathModeStart + wxT("\\cap") + mathModeEnd);
  text.Replace(wxT("\u2286"), mathModeStart + wxT("\\subseteq") + mathModeEnd);
  text.Replace(wxT("\u2282"), mathModeStart + wxT("\\subset") + mathModeEnd);
  text.Replace(wxT("\u2288"), mathModeStart + wxT("\\not\\subseteq") + mathModeEnd);
  text.Replace(wxT("\u0127"), mathModeStart + wxT("\\hbar") + mathModeEnd);
  text.Replace(wxT("\u0126"), mathModeStart + wxT("\\Hbar") + mathModeEnd);
  text.Replace(wxT("\u2205"), mathModeStart + wxT("\\emptyset") + mathModeEnd);
  text.Replace(wxT("\u00BD"), mathModeStart + wxT("\\frac{1}{2}") + mathModeEnd);
  text.Replace(wxT("\u03B2"), mathModeStart + wxT("\\beta") + mathModeEnd);
  text.Replace(wxT("\u03B3"), mathModeStart + wxT("\\gamma") + mathModeEnd);
  text.Replace(wxT("\u03B4"), mathModeStart + wxT("\\delta") + mathModeEnd);
  text.Replace(wxT("\u03B5"), mathModeStart + wxT("\\epsilon") + mathModeEnd);
  text.Replace(wxT("\u03B6"), mathModeStart + wxT("\\zeta") + mathModeEnd);
  text.Replace(wxT("\u03B7"), mathModeStart + wxT("\\eta") + mathModeEnd);
  text.Replace(wxT("\u03B8"), mathModeStart + wxT("\\theta") + mathModeEnd);
  text.Replace(wxT("\u03B9"), mathModeStart + wxT("\\iota") + mathModeEnd);
  text.Replace(wxT("\u03BA"), mathModeStart + wxT("\\kappa") + mathModeEnd);
  text.Replace(wxT("\u03BB"), mathModeStart + wxT("\\lambda") + mathModeEnd);
  text.Replace(wxT("\u03BC"), mathModeStart + wxT("\\mu") + mathModeEnd);
  text.Replace(wxT("\u03BD"), mathModeStart + wxT("\\nu") + mathModeEnd);
  text.Replace(wxT("\u03BE"), mathModeStart + wxT("\\xi") + mathModeEnd);
  text.Replace(wxT("\u03BF"), wxT("o"));
  text.Replace(wxT("\u03C0"), mathModeStart + wxT("\\pi") + mathModeEnd);
  text.Replace(wxT("\u03C1"), mathModeStart + wxT("\\rho") + mathModeEnd);
  text.Replace(wxT("\u03C3"), mathModeStart + wxT("\\sigma") + mathModeEnd);
  text.Replace(wxT("\u03C4"), mathModeStart + wxT("\\tau") + mathModeEnd);
  text.Replace(wxT("\u03C5"), mathModeStart + wxT("\\upsilon") + mathModeEnd);
  text.Replace(wxT("\u03C6"), mathModeStart + wxT("\\phi") + mathModeEnd);
  text.Replace(wxT("\u03C7"), mathModeStart + wxT("\\chi") + mathModeEnd);
  text.Replace(wxT("\u03C8"), mathModeStart + wxT("\\psi") + mathModeEnd);
  text.Replace(wxT("\u03C9"), mathModeStart + wxT("\\omega") + mathModeEnd);
  text.Replace(wxT("\u0391"), wxT("A"));
  text.Replace(wxT("\u0392"), wxT("B"));
  text.Replace(wxT("\u0393"), mathModeStart + wxT("\\Gamma") + mathModeEnd);
  text.Replace(wxT("\u0394"), mathModeStart + wxT("\\Delta") + mathModeEnd);
  text.Replace(wxT("\u0395"), wxT("E"));
  text.Replace(wxT("\u0396"), wxT("Z"));
  text.Replace(wxT("\u0397"), wxT("H"));
  text.Replace(wxT("\u0398"), mathModeStart + wxT("\\Theta") + mathModeEnd);
  text.Replace(wxT("\u0399"), wxT("I"));
  text.Replace(wxT("\u039A"), wxT("K"));
  text.Replace(wxT("\u039B"), mathModeStart + wxT("\\Lambda") + mathModeEnd);
  text.Replace(wxT("\u039C"), wxT("M"));
  text.Replace(wxT("\u039D"), wxT("N"));
  text.Replace(wxT("\u039E"), mathModeStart + wxT("\\Xi") + mathModeEnd);
  text.Replace(wxT("\u039F"), wxT("O"));
  text.Replace(wxT("\u03A0"), mathModeStart + wxT("\\Pi") + mathModeEnd);
  text.Replace(wxT("\u03A1"), wxT("P"));
  text.Replace(wxT("\u03A3"), mathModeStart + wxT("\\Sigma") + mathModeEnd);
  text.Replace(wxT("\u03A4"), wxT("T"));
  text.Replace(wxT("\u03A5"), mathModeStart + wxT("\\Upsilon") + mathModeEnd);
  text.Replace(wxT("\u03A6"), mathModeStart + wxT("\\Phi") + mathModeEnd);
  text.Replace(wxT("\u03A7"), wxT("X"));
  text.Replace(wxT("\u03A8"), mathModeStart + wxT("\\Psi") + mathModeEnd);
  text.Replace(wxT("\u03A9"), mathModeStart + wxT("\\Omega") + mathModeEnd);
  text.Replace(wxT("\u2202"), mathModeStart + wxT("\\partial") + mathModeEnd);
  text.Replace(wxT("\u222b"), mathModeStart + wxT("\\int") + mathModeEnd);
  text.Replace(wxT("\u2245"), mathModeStart + wxT("\\approx") + mathModeEnd);
  text.Replace(wxT("\u221d"), mathModeStart + wxT("\\propto") + mathModeEnd);
  text.Replace(wxT("\u2260"), mathModeStart + wxT("\\neq") + mathModeEnd);
  text.Replace(wxT("\u2264"), mathModeStart + wxT("\\leq") + mathModeEnd);
  text.Replace(wxT("\u2265"), mathModeStart + wxT("\\geq") + mathModeEnd);
  text.Replace(wxT("\u226A"), mathModeStart + wxT("\\ll") + mathModeEnd);
  text.Replace(wxT("\u226B"), mathModeStart + wxT("\\gg") + mathModeEnd);
  text.Replace(wxT("\u220e"), mathModeStart + wxT("\\blacksquare") + mathModeEnd);
  text.Replace(wxT("\u2263"), mathModeStart + wxT("\\equiv") + mathModeEnd);
  text.Replace(wxT("\u2211"), mathModeStart + wxT("\\sum") + mathModeEnd);
  text.Replace(wxT("\u220F"), mathModeStart + wxT("\\prod") + mathModeEnd);
  text.Replace(wxT("\u2225"), mathModeStart + wxT("\\parallel") + mathModeEnd);
  text.Replace(wxT("\u27C2"), mathModeStart + wxT("\\bot") + mathModeEnd);
  text.Replace(wxT("~"), mathModeStart + wxT("\\sim ") + mathModeEnd);
  text.Replace(wxT("_"), wxT("\\_ "));
  text.Replace(wxT("$"), wxT("\\$ "));
  text.Replace(wxT("%"), wxT("\\% "));
  text.Replace(wxT("&"), wxT("\\& "));
  text.Replace(wxT("@"), mathModeStart + wxT("@") + mathModeEnd);
  text.Replace(wxT("#"), mathModeStart + wxT("\\neq") + mathModeEnd);
  text.Replace(wxT("\u00A0"), wxT("~")); // A non-breakable space
  text.Replace(wxT("<"), mathModeStart + wxT("<") + mathModeEnd);
  text.Replace(wxT(">"), mathModeStart + wxT(">") + mathModeEnd);
  text.Replace(wxT("\u219D"), mathModeStart + wxT("\\leadsto") + mathModeEnd);
  text.Replace(wxT("\u2192"), mathModeStart + wxT("\\rightarrow") + mathModeEnd);
  text.Replace(wxT("\u2794"), mathModeStart + wxT("\\longrightarrow") + mathModeEnd);

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
              (m_previous && m_next) &&
              // These cells are two variable names
              ((m_previous->GetStyle() == TS_VARIABLE) && (m_next->GetStyle() == TS_VARIABLE)) &&
              // The variable name prior to this cell has no subscript
              (!(m_previous->ToString().Contains(wxT('_')))) &&
              // we will be using \mathit{} for the TeX outout.
              ((ToString().Length() > 1) || ((m_next == NULL) || (m_next->ToString().Length() > 1)))
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

    if (m_suppressMultiplicationDot)
    {
      text.Replace(wxT("*"), wxT("\\, "));
      text.Replace(wxT("\u00B7"), wxT("\\, "));
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
          text.Replace(wxT("\u00B7"), wxT("\\, "));
        }
        else
        {
          text.Replace(wxT("*"), wxT("\\cdot "));
          text.Replace(wxT("\u00B7"), wxT("\\cdot "));
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
      return wxT("\\ui ");
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
      return wxT("A");
    else if (text == wxT("\\% Beta"))
      return wxT("B");
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
      return wxT("\\ui ");
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

  if ((GetStyle() == TS_LABEL) || (GetStyle() == TS_USERLABEL))
    text = text + wxT(" ");

  return text;
}

wxString TextCell::ToMathML() const
{
  if(m_displayedText == wxEmptyString)
    return wxEmptyString;
  wxString text = XMLescape(ToString());

  // If we didn't display a multiplication dot we want to do the same in MathML.
  if (m_isHidden || (((*m_configuration)->HidemultiplicationSign()) && m_isHidableMultSign))
  {
    text.Replace(wxT("*"), wxT("&#8290;"));
    text.Replace(wxT("\u00B7"), wxT("&#8290;"));
    if (text != wxT ("&#8290;"))
      text = wxEmptyString;
  }
  text.Replace(wxT("*"), wxT("\u00B7"));

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
    break;
  case TS_VARIABLE:
    {
      text = GetGreekStringUnicode();

      bool keepPercent = (*m_configuration)->CheckKeepPercent();

      if (!keepPercent)
      {
        if (text == wxT("%pi"))
          text = wxT("\u03C0");
      }
    }
    break;
  case TS_FUNCTION:
      text = GetGreekStringUnicode();
      if (text == wxT("inf"))
        text = wxT("\u221e");
      if((text == wxT("+")) || (text == wxT("-")) || (text == wxT("*")) || (text == wxT("/")))
        return wxT("<mo>") + text + wxT("</mo>\n");
      else
        return wxT("<mi>") + text + wxT("</mi>\n");
    case TS_NUMBER:
      return wxT("<mn>") + text + wxT("</mn>\n");

    case TS_LABEL:
    case TS_USERLABEL:
      return wxT("<mtext>") + text + wxT("</mtext></mtd><mtd>\n");

    case TS_STRING:
    default:
      if (text.StartsWith(wxT("\"")))
        return wxT("<ms>") + text + wxT("</ms>\n");
      else
        return wxT("<mo>") + text + wxT("</mo>\n");
  }

  return wxT("<mo>") + text + wxT("</mo>\n");
}

wxString TextCell::ToOMML() const
{
  //Text-only lines are better handled in RTF.
  if (
          (m_previous && (m_previous->GetStyle() != TS_LABEL) && (!m_previous->HardLineBreak())) &&
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
    text.Replace(wxT("*"), wxT("&#8290;"));
    text.Replace(wxT("\u00B7"), wxT("&#8290;"));
    if (text != wxT ("&#8290;"))
      text = wxEmptyString;
  }
  text.Replace(wxT("*"), wxT("\u00B7"));

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
      if (!(*m_configuration)->CheckKeepPercent())
      {
        if (text == wxT("%pi"))
          text = wxT("\u03C0");
      }
    }
    break;
  case TS_FUNCTION:
      text = GetGreekStringUnicode();
      if (text == wxT("inf"))
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
  text = wxT("<m:r>") + text + wxT("</m:r>\n");
  return text;
}

wxString TextCell::ToRTF() const
{
  wxString retval;
  wxString text = ToString();

  if (m_displayedText == wxEmptyString)
    return(wxT(" "));
    
  text.Replace(wxT("-->"), wxT("\u2192"));
  // Needed for the output of let(a/b,a+1);
  text.Replace(wxT(" --> "), wxT("\u2192"));
  if ((GetStyle() == TS_LABEL) || (GetStyle() == TS_USERLABEL))
  {
    retval += wxString::Format(wxT("\\cf%i{"), (int) GetStyle());
    retval += RTFescape(text);
    retval += wxT("}\\cf0");
  }
  return retval;
}

wxString TextCell::GetXMLFlags() const
{
  wxString flags;
  if ((m_forceBreakLine) && (GetStyle() != TS_LABEL) && (GetStyle() != TS_USERLABEL))
    flags += wxT(" breakline=\"true\"");

  if (GetStyle() == TS_ERROR)
    flags += wxT(" type=\"error\"");

  if (GetStyle() == TS_WARNING)
    flags += wxT(" type=\"warning\"");
  
  if(!GetAltCopyText().empty())
    flags += wxT(" altCopy=\"") + XMLescape(GetAltCopyText()) + wxT("\"");

  if (!GetLocalToolTip().empty())
    flags += wxT(" tooltip=\"") + XMLescape(GetLocalToolTip()) + wxT("\"");

  if(GetStyle() == TS_USERLABEL)
    flags += wxT(" userdefined=\"yes\"");

  return flags;
}

wxString TextCell::ToXML() const
{
  wxString tag;
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
        break;
      default:
        tag = _T("t");
    }

  wxString xmlstring = XMLescape(m_displayedText);
  // convert it, so that the XML configuration doesn't fail

  return wxT("<") + tag + GetXMLFlags() + wxT(">") + xmlstring + wxT("</") + tag + wxT(">");
}

wxString TextCell::GetDiffPart() const
{
  return wxT(",") + m_text + wxT(",1");
}

bool TextCell::IsShortNum() const
{
  if (m_next != NULL)
    return false;
  else if (m_text.Length() < 4)
    return true;
  return false;
}

wxString TextCell::GetGreekStringUnicode() const
{
  wxString txt(m_text);

  if (!txt.empty() && txt[0] != '%')
    txt.Prepend(wxT("%"));

  if (txt == wxT("%alpha"))
    return wxT("\u03B1");
  else if (txt == wxT("%beta"))
    return wxT("\u03B2");
  else if (txt == wxT("%gamma"))
    return wxT("\u03B3");
  else if (txt == wxT("%delta"))
    return wxT("\u03B4");
  else if (txt == wxT("%epsilon"))
    return wxT("\u03B5");
  else if (txt == wxT("%zeta"))
    return wxT("\u03B6");
  else if (txt == wxT("%eta"))
    return wxT("\u03B7");
  else if (txt == wxT("%theta"))
    return wxT("\u03B8");
  else if (txt == wxT("%iota"))
    return wxT("\u03B9");
  else if (txt == wxT("%kappa"))
    return wxT("\u03BA");
  else if (txt == wxT("%lambda"))
    return wxT("\u03BB");
  else if (txt == wxT("%mu"))
    return wxT("\u03BC");
  else if (txt == wxT("%nu"))
    return wxT("\u03BD");
  else if (txt == wxT("%xi"))
    return wxT("\u03BE");
  else if (txt == wxT("%omicron"))
    return wxT("\u03BF");
  else if (txt == wxT("%pi"))
    return wxT("\u03C0");
  else if (txt == wxT("%rho"))
    return wxT("\u03C1");
  else if (txt == wxT("%sigma"))
    return wxT("\u03C3");
  else if (txt == wxT("%tau"))
    return wxT("\u03C4");
  else if (txt == wxT("%upsilon"))
    return wxT("\u03C5");
  else if (txt == wxT("%phi"))
    return wxT("\u03C6");
  else if (txt == wxT("%chi"))
    return wxT("\u03C7");
  else if (txt == wxT("%psi"))
    return wxT("\u03C8");
  else if (txt == wxT("%omega"))
    return wxT("\u03C9");
  else if (txt == wxT("%Alpha"))
    return wxT("\u0391");
  else if (txt == wxT("%Beta"))
    return wxT("\u0392");
  else if (txt == wxT("%Gamma"))
    return wxT("\u0393");
  else if (txt == wxT("%Delta"))
    return wxT("\u0394");
  else if (txt == wxT("%Epsilon"))
    return wxT("\u0395");
  else if (txt == wxT("%Zeta"))
    return wxT("\u0396");
  else if (txt == wxT("%Eta"))
    return wxT("\u0397");
  else if (txt == wxT("%Theta"))
    return wxT("\u0398");
  else if (txt == wxT("%Iota"))
    return wxT("\u0399");
  else if (txt == wxT("%Kappa"))
    return wxT("\u039A");
  else if (txt == wxT("%Lambda"))
    return wxT("\u039B");
  else if (txt == wxT("%Mu"))
    return wxT("\u039C");
  else if (txt == wxT("%Nu"))
    return wxT("\u039D");
  else if (txt == wxT("%Xi"))
    return wxT("\u039E");
  else if (txt == wxT("%Omicron"))
    return wxT("\u039F");
  else if (txt == wxT("%Pi"))
    return wxT("\u03A0");
  else if (txt == wxT("%Rho"))
    return wxT("\u03A1");
  else if (txt == wxT("%Sigma"))
    return wxT("\u03A3");
  else if (txt == wxT("%Tau"))
    return wxT("\u03A4");
  else if (txt == wxT("%Upsilon"))
    return wxT("\u03A5");
  else if (txt == wxT("%Phi"))
    return wxT("\u03A6");
  else if (txt == wxT("%Chi"))
    return wxT("\u03A7");
  else if (txt == wxT("%Psi"))
    return wxT("\u03A8");
  else if (txt == wxT("%Omega"))
    return wxT("\u03A9");

  return m_text;
}

wxString TextCell::GetSymbolUnicode(bool keepPercent) const
{
  if (m_text == wxT("+"))
    return wxT("+");
  else if (m_text == wxT("="))
    return wxT("=");
  else if (m_text == wxT("inf"))
    return wxT("\u221E");
  else if (m_text == wxT("%pi"))
    return wxT("\u03C0");
  else if (m_text == wxT("<="))
    return wxT("\u2264");
  else if (m_text == wxT(">="))
    return wxT("\u2265");
  #ifndef __WXMSW__
  else if (m_text == wxT(" and "))
    return wxT(" \u22C0 ");
  else if (m_text == wxT(" or "))
    return wxT(" \u22C1 ");
  else if (m_text == wxT(" xor "))
    return wxT(" \u22BB ");
  else if (m_text == wxT(" nand "))
    return wxT(" \u22BC ");
  else if (m_text == wxT(" nor "))
    return wxT(" \u22BD ");
  else if (m_text == wxT(" implies "))
    return wxT(" \u21D2 ");
  else if (m_text == wxT(" equiv "))
    return wxT(" \u21D4 ");
  else if (m_text == wxT("not"))
    return wxT("\u00AC");
  #endif
  else if (m_text == wxT("->"))
    return wxT("\u2192");
  else if (m_text == wxT("-->"))
    return wxT("\u2794");
  // The next two ones are needed for the output of let(a/b,a+1);
  else if (m_text == wxT(" --> "))
    return wxT("\u2794");
  else if (m_text == wxT(" \u2212\u2192 "))
    return wxT("\u2794");
  /*
   else if (GetStyle() == TS_SPECIAL_CONSTANT && m_text == wxT("d"))
     return wxT("\u2202");
   */

  if (!keepPercent)
  {
    if (m_text == wxT("%e"))
      return wxT("e");
    else if (m_text == wxT("%i"))
      return wxT("i");
    else if (m_text == wxT("%pi"))
      return wxString(wxT("\u03C0"));
  }

  return wxEmptyString;
}

wxString TextCell::GetGreekStringTeX() const
{
  if (m_text == wxT("gamma"))
    return wxT("\u00C0");
  else if (m_text == wxT("zeta"))
    return wxT("\u00B0");
  else if (m_text == wxT("psi"))
    return wxT("\u00C9");

  wxString txt(m_text);
  if (txt[0] != '%')
    txt = wxT("%") + txt;

  if (txt == wxT("%alpha"))
    return wxT("\u00CB");
  else if (txt == wxT("%beta"))
    return wxT("\u00CC");
  else if (txt == wxT("%gamma"))
    return wxT("\u00CD");
  else if (txt == wxT("%delta"))
    return wxT("\u00CE");
  else if (txt == wxT("%epsilon"))
    return wxT("\u00CF");
  else if (txt == wxT("%zeta"))
    return wxT("\u00B0");
  else if (txt == wxT("%eta"))
    return wxT("\u00D1");
  else if (txt == wxT("%theta"))
    return wxT("\u00D2");
  else if (txt == wxT("%iota"))
    return wxT("\u00D3");
  else if (txt == wxT("%kappa"))
    return wxT("\u00D4");
  else if (txt == wxT("%lambda"))
    return wxT("\u00D5");
  else if (txt == wxT("%mu"))
    return wxT("\u00D6");
  else if (txt == wxT("%nu"))
    return wxT("\u00B7");
  else if (txt == wxT("%xi"))
    return wxT("\u00D8");
  else if (txt == wxT("%omicron"))
    return wxT("o");
  else if (txt == wxT("%pi"))
    return wxT("\u00D9");
  else if (txt == wxT("%rho"))
    return wxT("\u00DA");
  else if (txt == wxT("%sigma"))
    return wxT("\u00DB");
  else if (txt == wxT("%tau"))
    return wxT("\u00DC");
  else if (txt == wxT("%upsilon"))
    return wxT("\u00B5");
  else if (txt == wxT("%chi"))
    return wxT("\u00DF");
  else if (txt == wxT("%psi"))
    return wxT("\u00EF");
  else if (txt == wxT("%phi"))
    return wxT("\u0027");
  else if (txt == wxT("%omega"))
    return wxT("\u0021");
  else if (txt == wxT("%Alpha"))
    return wxT("A");
  else if (txt == wxT("%Beta"))
    return wxT("B");
  else if (txt == wxT("%Gamma"))
    return wxT("\u00C0");
  else if (txt == wxT("%Delta"))
    return wxT("\u00C1");
  else if (txt == wxT("%Epsilon"))
    return wxT("E");
  else if (txt == wxT("%Zeta"))
    return wxT("Z");
  else if (txt == wxT("%Eta"))
    return wxT("H");
  else if (txt == wxT("%Theta"))
    return wxT("\u00C2");
  else if (txt == wxT("%Iota"))
    return wxT("I");
  else if (txt == wxT("%Kappa"))
    return wxT("K");
  else if (txt == wxT("%Lambda"))
    return wxT("\u00C3");
  else if (txt == wxT("%Mu"))
    return wxT("M");
  else if (txt == wxT("%Nu"))
    return wxT("N");
  else if (txt == wxT("%Xi"))
    return wxT("\u00C4");
  else if (txt == wxT("%Omicron"))
    return wxT("O");
  else if (txt == wxT("%Pi"))
    return wxT("\u00C5");
  else if (txt == wxT("%Rho"))
    return wxT("P");
  else if (txt == wxT("%Sigma"))
    return wxT("\u00C6");
  else if (txt == wxT("%Tau"))
    return wxT("T");
  else if (txt == wxT("%Upsilon"))
    return wxT("Y");
  else if (txt == wxT("%Phi"))
    return wxT("\u00C8");
  else if (txt == wxT("%Chi"))
    return wxT("X");
  else if (txt == wxT("%Psi"))
    return wxT("\u00C9");
  else if (txt == wxT("%Omega"))
    return wxT("\u00CA");

  return wxEmptyString;
}

wxString TextCell::GetSymbolTeX() const
{
  if (m_text == wxT("inf"))
    return wxT("\u0031");
  else if (m_text == wxT("+"))
    return wxT("+");
  else if (m_text == wxT("%pi"))
    return wxT("\u00D9");
  else if (m_text == wxT("="))
    return wxT("=");
  else if (m_text == wxT("->"))
    return wxT("\u0021");
  else if (m_text == wxT(">="))
    return wxT("\u00D5");
  else if (m_text == wxT("<="))
    return wxT("\u00D4");
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
    return wxT("\u003A");
  else if (m_text == wxT(" xor "))
    return wxT("\u00C8");
*/

  return wxEmptyString;
}

// RegExes all TextCells share.
wxRegEx TextCell::m_unescapeRegEx(wxT("\\\\(.)"));
wxRegEx TextCell::m_roundingErrorRegEx1(wxT("\\.000000000000[0-9]+$"));
wxRegEx TextCell::m_roundingErrorRegEx2(wxT("\\.999999999999[0-9]+$"));
wxRegEx TextCell::m_roundingErrorRegEx3(wxT("\\.000000000000[0-9]+e"));
wxRegEx TextCell::m_roundingErrorRegEx4(wxT("\\.999999999999[0-9]+e"));
