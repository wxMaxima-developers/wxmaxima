// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
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
#include "CellImpl.h"
#include "StringUtils.h"
#include <wx/config.h>

TextCell::TextCell(GroupCell *group, Configuration *config,
                   const wxString &text, TextStyle style)
  : Cell(group, config)
{
  InitBitFields();
  switch (style) {
  case TS_MATH:
    m_type = MC_TYPE_TEXT;
    break;
  case TS_VARIABLE:
    m_type = MC_TYPE_TEXT;
    break;
  case TS_NUMBER:
    m_type = MC_TYPE_TEXT;
    break;
  case TS_FUNCTION:
    m_type = MC_TYPE_TEXT;
    break;
  case TS_SPECIAL_CONSTANT:
    m_type = MC_TYPE_TEXT;
    break;
  case TS_GREEK_CONSTANT:
    m_type = MC_TYPE_TEXT;
    break;
  case TS_STRING:
    m_type = MC_TYPE_TEXT;
    break;
  case TS_CODE_DEFAULT:
    m_type = MC_TYPE_INPUT;
    break;
  case TS_MAIN_PROMPT:
    m_type = MC_TYPE_MAIN_PROMPT;
    break;
  case TS_OTHER_PROMPT:
    m_type = MC_TYPE_PROMPT;
    break;
  case TS_LABEL:
    m_type = MC_TYPE_LABEL;
    break;
  case TS_USERLABEL:
    m_type = MC_TYPE_LABEL;
    break;
  case TS_HIGHLIGHT:
    m_type = MC_TYPE_TEXT;
    break;
  case TS_WARNING:
    m_type = MC_TYPE_WARNING;
    break;
  case TS_ASCIIMATHS:
    m_type = MC_TYPE_ASCIIMATHS;
    break;
  case TS_ERROR:
    m_type = MC_TYPE_ERROR;
    break;
  case TS_TEXT:
    m_type = MC_TYPE_TEXT;
    break;
  case TS_HEADING6:
    m_type = MC_TYPE_HEADING6;
    break;
  case TS_HEADING5:
    m_type = MC_TYPE_HEADING5;
    break;
  case TS_SUBSUBSECTION:
    m_type = MC_TYPE_SUBSUBSECTION;
    break;
  case TS_SUBSECTION:
    m_type = MC_TYPE_SUBSECTION;
    break;
  case TS_SECTION:
    m_type = MC_TYPE_SECTION;
    break;
  case TS_TITLE:
    m_type = MC_TYPE_TITLE;
    break;
  case TS_OPERATOR:
    m_type = MC_TYPE_TEXT;
    break;
  default:
    wxLogMessage(_("Unexpected text style %li for TextCell"), (long)style);
    m_type = MC_TYPE_TITLE;
  }
  TextCell::SetValue(text);
  TextCell::SetStyle(style);
}

// cppcheck-suppress uninitMemberVar symbolName=TextCell::m_alt
// cppcheck-suppress uninitMemberVar symbolName=TextCell::m_altJs
// cppcheck-suppress uninitMemberVar symbolName=TextCell::m_initialToolTip
TextCell::TextCell(GroupCell *group, const TextCell &cell)
  : Cell(group, cell.m_configuration), m_text(cell.m_text),
    m_displayedText(cell.m_displayedText) {
  InitBitFields();
  m_type = cell.m_type;
  CopyCommonData(cell);
  SetBigSkip(cell.HasBigSkip());
  SetHighlight(cell.GetHighlight());
  m_dontEscapeOpeningParenthesis = cell.m_dontEscapeOpeningParenthesis;
}

DEFINE_CELL(TextCell)

void TextCell::SetStyle(TextStyle style) {
  m_sizeCache.clear();
  Cell::SetStyle(style);
  if ((m_text == wxS("gamma")) && (GetTextStyle() == TS_FUNCTION))
    m_displayedText = wxS("\u0393");
  if ((m_text == wxS("psi")) && (GetTextStyle() == TS_FUNCTION))
    m_displayedText = wxS("\u03A8");
  if ((style == TS_LABEL) || (style == TS_USERLABEL) ||
      (style == TS_MAIN_PROMPT) || (style == TS_OTHER_PROMPT))
    HasHardLineBreak();
  ResetSize();
}

void TextCell::SetType(CellType type) {
  m_sizeCache.clear();
  Cell::SetType(type);
}

void TextCell::UpdateToolTip() {
  if (m_promptTooltip)
    SetToolTip(
	       &T_("Most questions can be avoided using the assume() and the "
		   "declare() command. If that isn't possible the \"Automatically "
		   "answer questions\" button makes wxMaxima automatically fill in "
		   "all answers it still remembers from a previous run."));

  if (m_text.empty())
    return;

  auto const &c_text = m_text;

  if (GetTextStyle() == TS_VARIABLE) {
    if (m_text == wxS("pnz"))
      SetToolTip(&T_(
		     "Either positive, negative or zero.\n"
		     "Normally the result of sign() if the sign cannot be determined."));

    else if (m_text == wxS("pz"))
      SetToolTip(&T_("Either positive or zero.\n"
                     "A possible result of sign()."));

    else if (m_text == wxS("nz"))
      SetToolTip(&T_("Either negative or zero.\n"
                     "A possible result of sign()."));

    else if (m_text == wxS("und"))
      SetToolTip(&T_("The result was undefined."));

    else if (m_text == wxS("ind"))
      SetToolTip(&T_("The result was indefinite, which might be infinity, both "
                     "plus or minus infinity or something additionally "
                     "potentially involving a complex infinity."));

    else if (m_text == wxS("zeroa"))
      SetToolTip(&T_("Infinitesimal above zero."));

    else if (m_text == wxS("zerob"))
      SetToolTip(&T_("Infinitesimal below zero."));

    else if (m_text == wxS("inf"))
      SetToolTip(&S_("+∞."));

    else if (m_text == wxS("infinity"))
      SetToolTip(&T_("Complex infinity."));

    else if (m_text == wxS("minf"))
      SetToolTip(&S_("-∞."));

    else if (m_text.StartsWith(S_("%r"))) {
      if (std::all_of(std::next(c_text.begin(), 2), c_text.end(), wxIsdigit))
        SetToolTip(&T_("A variable that can be assigned a number to.\n"
                       "Often used by solve() and algsys(), if there is an "
                       "infinite number of results."));
    } else if (m_text.StartsWith(S_("%i"))) {
      if (std::all_of(std::next(c_text.begin(), 2), c_text.end(), wxIsdigit))
        SetToolTip(&T_("An integration constant."));
    }
  }

  else if (GetTextStyle() == TS_NUMBER) {
    if ((m_roundingErrorRegEx1.Matches(m_text)) ||
        (m_roundingErrorRegEx2.Matches(m_text)) ||
        (m_roundingErrorRegEx3.Matches(m_text)) ||
        (m_roundingErrorRegEx4.Matches(m_text)))
      SetToolTip(&T_(
		     "As calculating 0.1^12 demonstrates maxima by default doesn't tend "
		     "to "
		     "hide what looks like being the small error using floating-point "
		     "numbers introduces.\n"
		     "If this seems to be the case here the error can be avoided by using "
		     "exact numbers like 1/10, 1*10^-1 or rat(.1).\n"
		     "It also can be hidden by setting fpprintprec to an appropriate "
		     "value. "
		     "But be aware in this case that even small errors can add up."));
  }

  else {
    if (m_text.Contains(S_("LINE SEARCH FAILED. SEE")) ||
        m_text.Contains(S_("DOCUMENTATION OF ROUTINE MCSRCH")) ||
        m_text.Contains(S_("ERROR RETURN OF LINE SEARCH:")) ||
        m_text.Contains(
			S_("POSSIBLE CAUSES: FUNCTION OR GRADIENT ARE INCORRECT")))
      SetToolTip(&T_(
		     "This message can appear when trying to numerically find an optimum. "
		     "In this case it might indicate that a starting point lies in a "
		     "local "
		     "optimum that fits the data best if one parameter is increased to "
		     "infinity or decreased to -infinity. It also can indicate that an "
		     "attempt was made to fit data to an equation that actually matches "
		     "the data best if one parameter is set to +/- infinity."));

    else if (m_text.StartsWith(S_("incorrect syntax")) &&
             m_text.Contains(S_("is not an infix operator")))
      SetToolTip(
		 &T_("A command or number wasn't preceded by a \":\", a \"$\", a "
		     "\";\" or a \",\".\n"
		     "Most probable cause: A missing comma between two list items."));
    else if (m_text.StartsWith(S_("incorrect syntax")) &&
             m_text.Contains(S_("Found LOGICAL expression where ALGEBRAIC "
                                "expression expected")))
      SetToolTip(&T_("Most probable cause: A dot instead a comma between two "
                     "list items containing assignments."));
    else if (m_text.StartsWith(S_("incorrect syntax")) &&
             m_text.Contains(S_("is not a prefix operator")))
      SetToolTip(&T_(
		     "Most probable cause: Two commas or similar separators in a row."));
    else if (m_text.Contains(S_("Illegal use of delimiter")))
      SetToolTip(&T_("Most probable cause: an operator was directly followed "
                     "by a closing parenthesis."));
    else if (m_text.StartsWith(
			       S_("find_root: function has same sign at endpoints: ")))
      SetToolTip(&T_(
		     "Maxima tried to find out where between two points a curve crosses "
		     "the zero line. Since the curve is on the same side of the zero line "
		     "in both points its algorithms fails here. Set find_root_error to "
		     "false if you want it to return false instead of an error."));
    else if (m_text.StartsWith(S_("part: fell off the end.")))
      SetToolTip(&T_("part() or the [] operator was used in order to extract "
                     "the nth element "
                     "of something that was less than n elements long."));
    else if (m_text.StartsWith(S_("rest: fell off the end.")))
      SetToolTip(&T_("rest() tried to drop more entries from a list than the "
                     "list was long."));
    else if (m_text.StartsWith(S_("assignment: cannot assign to")))
      SetToolTip(
		 &T_("The value of few special variables is assigned by Maxima and "
		     "cannot be changed by the user. Also a few constructs aren't "
		     "variable names and therefore cannot be written to."));
    else if (m_text.StartsWith(S_("rat: replaced ")))
      SetToolTip(&T_(
		     "Normally computers use floating-point numbers that can be handled "
		     "incredibly fast while being accurate to dozens of digits. "
		     "They will, though, introduce a small error into some common "
		     "numbers. "
		     "For example 0.1 is represented as "
		     "3602879701896397/36028797018963968.\n"
		     "As mathematics is based on the fact that numbers that are exactly "
		     "equal cancel each other out small errors can quickly add up to big "
		     "errors "
		     "(see Wilkinson's Polynomials or Rump's Polynomials). Some maxima "
		     "commands therefore use rat() in order to automatically convert "
		     "floats to "
		     "exact numbers (like 1/10 or sqrt(2)/2) where floating-point errors "
		     "might "
		     "add up.\n\n"
		     "This error message doesn't occur if exact numbers (1/10 instead of "
		     "0.1) "
		     "are used.\n"
		     "The info that numbers have automatically been converted can be "
		     "suppressed "
		     "by setting ratprint to false."));
    else if (m_text.StartsWith(S_("desolve: can't handle this case.")))
      SetToolTip(
		 &T_("The list of time-dependent variables to solve to doesn't match "
		     "the time-dependent variables the list of dgls contains."));
    else if (m_text.StartsWith(
			       S_("expt: undefined: 0 to a negative exponent.")))
      SetToolTip(&T_("Division by 0."));
    else if (m_text.StartsWith(S_("incorrect syntax: parser: incomplete "
                                  "number; missing exponent?")))
      SetToolTip(
		 &T_("Might also indicate a missing multiplication sign (\"*\")."));
    else if (m_text.Contains(S_("arithmetic error DIVISION-BY-ZERO signalled")))
      SetToolTip(&T_(
		     "Besides a division by 0 the reason for this error message can be a "
		     "calculation that returns +/-infinity."));
    else if (m_text.Contains(S_("isn't in the domain of")))
      SetToolTip(&T_("Most probable cause: A function was called with a "
                     "parameter that causes "
                     "it to return infinity and/or -infinity."));
    else if (m_text.StartsWith(S_("Only symbols can be bound")))
      SetToolTip(&T_(
		     "This error message is most probably caused by a try to assign "
		     "a value to a number instead of a variable name.\n"
		     "One probable cause is using a variable that already has a numeric "
		     "value as a loop counter."));
    else if (m_text.StartsWith(
			       S_("append: operators of arguments must all be the same.")))
      SetToolTip(
		 &T_("Most probably it was attempted to append something to a list "
		     "that isn't a list.\n"
		     "Enclosing the new element for the list in brackets ([]) "
		     "converts it to a list and makes it appendable."));
    else if (m_text.Contains(S_("matrix: all rows must be the same length")))
      SetToolTip(&T_(
		     "Might be caused by reading an csv file with an empty last line:\n"
		     "Technically that line can be described as having the length 0 "
		     "which differs from the other lines of this file."));
    else if (m_text.Contains(S_("expected a polynomial")))
      SetToolTip(
		 &T_("If the thing maxima complains about actually looks like a "
		     "polynomial "
		     "you can try running it through ratdisrep() in order to fix that "
		     "problem."));
    else if (m_text.Contains(S_("Control stack exhausted")))
      SetToolTip(&T_("Often caused by recursive function calls. Some lisps "
                     "allow to increase "
                     "the control stack size using command-line arguments."));
    else if (m_text.Contains(S_(": invalid index")))
      SetToolTip(
		 &T_("The [] or the part() command tried to access a list or matrix "
		     "element that doesn't exist."));
    else if (m_text.StartsWith(
			       S_("apply: subscript must be an integer; found:")))
      SetToolTip(&T_(
		     "the [] operator tried to extract an element of a list, a matrix, "
		     "an equation or an array. But instead of an integer number "
		     "something was used whose numerical value is unknown or not an "
		     "integer.\n"
		     "Floating-point numbers are bound to contain small rounding errors "
		     "and therefore in most cases don't work as an array index that"
		     "needs to be an exact integer number."));
    else if (m_text.StartsWith(S_(": improper argument: "))) {
      auto const prevString =
	GetPrevious() ? GetPrevious()->ToString() : wxm::emptyString;
      if (prevString == wxS("at"))
        SetToolTip(
		   &T_("The second argument of at() isn't an equation or a list of "
		       "equations. Most probably it was lacking an \"=\"."));
      else if (prevString == wxS("subst"))
        SetToolTip(
		   &T_("The first argument of subst() isn't an equation or a list of "
		       "equations. Most probably it was lacking an \"=\"."));
      else
        SetToolTip(&T_(
		       "The argument of a function was of the wrong type. Most probably "
		       "an equation was expected but was lacking an \"=\"."));
    }
  }
}

void TextCell::SetValue(const wxString &text) {
  m_sizeCache.clear();
  m_text = text;
  ResetSize();
  UpdateDisplayedText();
  UpdateToolTip();
}

AFontSize TextCell::GetScaledTextSize() const { return m_fontSize_Scaled; }

bool TextCell::NeedsRecalculation(AFontSize fontSize) const {
  return Cell::NeedsRecalculation(fontSize) ||
    (m_keepPercent_last != m_configuration->CheckKeepPercent());
}

wxSize TextCell::CalculateTextSize(wxDC *const dc, const wxString &text,
                                   TextCell::TextIndex const index) {
  AFontSize const fontSize = GetScaledTextSize();
  if (text.empty())
    return {};

  auto const size = dc->GetTextExtent(text);
  m_sizeCache.emplace_back(size, fontSize, index);
  return size;
}

void TextCell::UpdateDisplayedText() {
  m_displayedText = m_text;

  m_displayedText.Replace(wxS("\xDCB6"),
                          wxS("\u00A0")); // A non-breakable space
  m_displayedText.Replace(wxS("\n"), wxEmptyString);
  m_displayedText.Replace(wxS("-->"), wxS("\u2794"));
  m_displayedText.Replace(wxS(" -->"), wxS("\u2794"));
  m_displayedText.Replace(wxS(" \u2212\u2192 "), wxS("\u2794"));
  m_displayedText.Replace(wxS("->"), wxS("\u2192"));
  m_displayedText.Replace(wxS("\u2212>"), wxS("\u2192"));

  if (GetTextStyle() == TS_FUNCTION) {
    if (m_text == wxS("ilt"))
      SetToolTip(&T_("The inverse laplace transform."));

    if (m_text == wxS("gamma"))
      m_displayedText = wxS("\u0393");
    if (m_text == wxS("psi"))
      m_displayedText = wxS("\u03A8");
  }

  if ((GetTextStyle() == TS_MATH) && m_text.StartsWith("\""))
    return;

  if ((GetTextStyle() == TS_GREEK_CONSTANT) && m_configuration->Latin2Greek())
    m_displayedText = GetGreekStringUnicode();

  wxString unicodeSym = GetSymbolUnicode(m_configuration->CheckKeepPercent());
  if (!unicodeSym.IsEmpty())
    m_displayedText = unicodeSym;

  /// Change asterisk to a multiplication dot, if applicable
  if (m_configuration->GetChangeAsterisk()) {
    if (m_displayedText == wxS("*"))
      m_displayedText = wxS("\u00B7");
    if (m_displayedText == wxS("#"))
      m_displayedText = wxS("\u2260");
  }
}

void TextCell::Recalculate(AFontSize fontsize) {
  if (GetTextStyle() == TS_ASCIIMATHS)
    ForceBreakLine(true);
  if (m_keepPercent_last != m_configuration->CheckKeepPercent())
    UpdateDisplayedText();
  if (NeedsRecalculation(fontsize)) {
    Cell::Recalculate(fontsize);
    m_keepPercent_last = m_configuration->CheckKeepPercent();
    SetFont(m_fontSize_Scaled);

    wxSize sz =
      CalculateTextSize(m_configuration->GetDC(), m_displayedText, cellText);
    m_width = sz.GetWidth();
    m_height = sz.GetHeight();

    m_width += 2 * MC_TEXT_PADDING;
    m_height += 2 * MC_TEXT_PADDING;

    /// Hidden cells (multiplication * is not displayed)
    if (IsHidden() ||
        (GetHidableMultSign() && (m_configuration->HidemultiplicationSign()))) {
      m_height = m_fontSize_Scaled.Get();
      m_width = m_fontSize_Scaled.Get() / 4;
    }
    if (m_height < Scale_Px(4))
      m_height = Scale_Px(4);
    m_center = m_height / 2;
  }
}

void TextCell::Draw(wxPoint point) {
  Cell::Draw(point);

  if (DrawThisCell(point) &&
      !(IsHidden() ||
        (GetHidableMultSign() && m_configuration->HidemultiplicationSign()))) {

    wxDC *dc = m_configuration->GetDC();
    int padding = 0;
    if (GetTextStyle() != TS_ASCIIMATHS)
      padding = MC_TEXT_PADDING;

    SetForeground();
    SetFont(m_fontSize_Scaled);
    dc->DrawText(m_displayedText, point.x + padding,
                 point.y - m_center + MC_TEXT_PADDING);
  }
}

void TextCell::SetFont(AFontSize fontsize) {
  wxDC *dc = m_configuration->GetDC();
  if(dc == NULL)
    {
      wxLogMessage(_("Bug: dc == NULL"));
      return;
    }
  
  const wxFont &font = GetFont(fontsize);
  if(m_configuration->GetLastFontUsed() != &font)
    {
      m_configuration->SetLastFontUsed(&font);
      dc->SetFont(font);
    }
}

bool TextCell::IsOperator() const {
  if (wxString(wxS("+*/-\u2212\u00B7")).Find(m_text) >= 0)
    return true;
  if (GetHidableMultSign())
    return true;
  if (IsHidden())
    return true;
  return false;
}

wxString TextCell::ToString() const {
  wxString text;
  if (!GetAltCopyText().empty())
    text = GetAltCopyText();
  else {
    text = m_text;
    text.Replace(wxS("\u2212"), wxS("-")); // unicode minus sign
    text.Replace(wxS("\u2794"), wxS("-->"));
    text.Replace(wxS("\u2192"), wxS("->"));
  }
  switch (GetTextStyle()) {
  case TS_VARIABLE:
  case TS_FUNCTION:
    // The only way for variable or function names to contain quotes and
    // characters that clearly represent operators is that these chars
    // are quoted by a backslash: They cannot be quoted by quotation
    // marks since maxima wouldn't allow strings here.
    {
      wxString charsNeedingQuotes("\\'\"()[]-{}^+*/&§?:;=#<>$");
      bool isOperator = true;
      if (m_text.Length() > 1) {
        for (size_t i = 0; i < m_text.Length(); i++) {
          if ((m_text[i] == wxS(' ')) ||
              (charsNeedingQuotes.Find(m_text[i]) == wxNOT_FOUND)) {
            isOperator = false;
            break;
          }
        }
      }

      if (!isOperator) {
        wxString lastChar;
        if ((m_dontEscapeOpeningParenthesis) && (text.Length() > 0) &&
            (text[text.Length() - 1] == wxS('('))) {
          lastChar = text[text.Length() - 1];
          text = text.Left(text.Length() - 1);
        }
        for (size_t i = 0; i < charsNeedingQuotes.Length(); i++)
          text.Replace(charsNeedingQuotes[i],
                       wxS("\\") + wxString(charsNeedingQuotes[i]));
        text += lastChar;
      }
      break;
    }
  case TS_STRING:
    text = wxS("\"") + text + wxS("\"");
    break;

    // Labels sometimes end with a few spaces. But if they are long they don't
    // do that any more => Add a TAB to the end of any label replacing trailing
    // whitespace. But don't do this if we copy only the label.
  default: {
  }
  }
  if (GetNext() && GetNext()->BreakLineHere())
    text += "\n";

  return text;
}

wxString TextCell::ToMatlab() const {
  wxString text = ToString();
  if (text == wxS("%e"))
    text = wxS("e");
  else if (text == wxS("%i"))
    text = wxS("i");
  else if (text == wxS("%pi"))
    text = wxString(wxS("pi"));
  switch (GetTextStyle()) {
  case TS_VARIABLE:
  case TS_FUNCTION:
    // The only way for variable or function names to contain quotes and
    // characters that clearly represent operators is that these chars
    // are quoted by a backslash: They cannot be quoted by quotation
    // marks since maxima wouldn't allow strings here.
    {
      wxString charsNeedingQuotes("\\'\"()[]{}^+*/&§?:;=#<>$");
      bool isOperator = true;
      for (size_t i = 0; i < m_text.Length(); i++) {
        if ((m_text[i] == wxS(' ')) ||
            (charsNeedingQuotes.Find(m_text[i]) == wxNOT_FOUND)) {
          isOperator = false;
          break;
        }
      }

      if (!isOperator) {
        wxString lastChar;
        if ((m_dontEscapeOpeningParenthesis) && (text.Length() > 0) &&
            (text[text.Length() - 1] == wxS('('))) {
          lastChar = text[text.Length() - 1];
          text = text.Left(text.Length() - 1);
        }
        for (size_t i = 0; i < charsNeedingQuotes.Length(); i++)
          text.Replace(charsNeedingQuotes[i],
                       wxS("\\") + wxString(charsNeedingQuotes[i]));
        text += lastChar;
      }
      break;
    }
  case TS_STRING:
    text = wxS("\"") + text + wxS("\"");
    break;

    // Labels sometimes end with a few spaces. But if they are long they don't
    // do that any more => Add a TAB to the end of any label replacing trailing
    // whitespace. But don't do this if we copy only the label.
  case TS_LABEL:
  case TS_USERLABEL:
  case TS_MAIN_PROMPT:
  case TS_OTHER_PROMPT: {
    text.Trim();
    text += wxS("\t");
    break;
  }
  default: {
  }
  }
  if (GetNext() && GetNext()->BreakLineHere())
    text += "\n";

  return text;
}

wxString TextCell::ToTeX() const {
  wxString text = ToString();

  if (!m_configuration->CheckKeepPercent()) {
    if (text == wxS("%e"))
      text = wxS("e");
    else if (text == wxS("%i"))
      text = wxS("i");
    else if (text == wxS("%pi"))
      text = wxString(wxS("\u03C0"));
  }

  // The string needed in order to ensure we are in math mode. Most TextCells
  // contain names of math objects and therefore can leave this string blank.
  wxString mathModeStart;
  // The string needed in order to close the command that ensures we are in math
  // mode.
  wxString mathModeEnd = wxS(" ");

  if ((GetTextStyle() == TS_ERROR) || (GetTextStyle() == TS_WARNING) ||
      (GetTextStyle() == TS_LABEL) || (GetTextStyle() == TS_USERLABEL) ||
      (GetTextStyle() == TS_MAIN_PROMPT) || (GetTextStyle() == TS_OTHER_PROMPT)) {
    mathModeStart = wxS("\\ensuremath{");
    mathModeEnd = wxS("}");
    if ((GetTextStyle() == TS_LABEL) || (GetTextStyle() == TS_USERLABEL) ||
        (GetTextStyle() == TS_MAIN_PROMPT) || (GetTextStyle() == TS_OTHER_PROMPT))
      text.Replace(wxS("\\"), wxEmptyString);
    else
      text.Replace(wxS("\\"), mathModeStart + wxS("\\backslash") + mathModeEnd);
    text.Replace(wxS("{"), wxS("\\{"));
    text.Replace(wxS("}"), wxS("\\}"));
  } else {
    text.Replace(wxS("\\"), mathModeStart + wxS("\\backslash") + mathModeEnd);
    text.Replace(wxS("{"), wxS("\\{"));
    text.Replace(wxS("}"), wxS("\\}"));

    // Babel replaces Umlaute by constructs like \"a - and \" isn't allowed in
    // math mode. Fortunately amsTeX provides the \text command that allows to
    // switch to plain text mode again - but with the math font size.
    text.Replace(wxS("ä"), wxS("\\text{ä}"));
    text.Replace(wxS("ö"), wxS("\\text{ö}"));
    text.Replace(wxS("ü"), wxS("\\text{ü}"));
    text.Replace(wxS("Ä"), wxS("\\text{Ä}"));
    text.Replace(wxS("Ö"), wxS("\\text{Ö}"));
    text.Replace(wxS("Ü"), wxS("\\text{Ü}"));
  }

  text.Replace(wxS("<"), mathModeStart + wxS("<") + mathModeEnd);
  text.Replace(wxS(">"), mathModeStart + wxS(">") + mathModeEnd);
  text.Replace(wxS("\u2212"), wxS("-")); // unicode minus sign
  text.Replace(wxS("\u00B1"), mathModeStart + wxS("\\pm") + mathModeEnd);
  text.Replace(wxS("\u03B1"), mathModeStart + wxS("\\alpha") + mathModeEnd);
  text.Replace(wxS("\u00B2"), mathModeStart + wxS("^2") + mathModeEnd);
  text.Replace(wxS("\u00B3"), mathModeStart + wxS("^3") + mathModeEnd);
  text.Replace(wxS("\u221A"), mathModeStart + wxS("\\sqrt{}") + mathModeEnd);
  text.Replace(wxS("\u2148"),
               mathModeStart + wxS("\\mathbbm{i}") + mathModeEnd);
  text.Replace(wxS("\u2147"),
               mathModeStart + wxS("\\mathbbm{e}") + mathModeEnd);
  text.Replace(wxS("\u210f"), mathModeStart + wxS("\\hbar") + mathModeEnd);
  text.Replace(wxS("\u2203"), mathModeStart + wxS("\\exists") + mathModeEnd);
  text.Replace(wxS("\u2204"), mathModeStart + wxS("\\nexists") + mathModeEnd);
  text.Replace(wxS("\u2208"), mathModeStart + wxS("\\in") + mathModeEnd);
  text.Replace(wxS("\u21D2"),
               mathModeStart + wxS("\\Longrightarrow") + mathModeEnd);
  text.Replace(wxS("\u221e"), mathModeStart + wxS("\\infty") + mathModeEnd);
  text.Replace(wxS("\u22C0"), mathModeStart + wxS("\\wedge") + mathModeEnd);
  text.Replace(wxS("\u22C1"), mathModeStart + wxS("\\vee") + mathModeEnd);
  text.Replace(wxS("\u22bb"), mathModeStart + wxS("\\oplus") + mathModeEnd);
  text.Replace(wxS("\u22BC"),
               mathModeStart + wxS("\\overline{\\wedge}") + mathModeEnd);
  text.Replace(wxS("\u22BB"),
               mathModeStart + wxS("\\overline{\\vee}") + mathModeEnd);
  text.Replace(wxS("\u00AC"), mathModeStart + wxS("\\setminus") + mathModeEnd);
  text.Replace(wxS("\u22C3"), mathModeStart + wxS("\\cup") + mathModeEnd);
  text.Replace(wxS("\u22C2"), mathModeStart + wxS("\\cap") + mathModeEnd);
  text.Replace(wxS("\u2286"), mathModeStart + wxS("\\subseteq") + mathModeEnd);
  text.Replace(wxS("\u2282"), mathModeStart + wxS("\\subset") + mathModeEnd);
  text.Replace(wxS("\u2288"),
               mathModeStart + wxS("\\not\\subseteq") + mathModeEnd);
  text.Replace(wxS("\u0127"), mathModeStart + wxS("\\hbar") + mathModeEnd);
  text.Replace(wxS("\u0126"), mathModeStart + wxS("\\Hbar") + mathModeEnd);
  text.Replace(wxS("\u2205"), mathModeStart + wxS("\\emptyset") + mathModeEnd);
  text.Replace(wxS("\u00BD"),
               mathModeStart + wxS("\\frac{1}{2}") + mathModeEnd);
  text.Replace(wxS("\u03B2"), mathModeStart + wxS("\\beta") + mathModeEnd);
  text.Replace(wxS("\u03B3"), mathModeStart + wxS("\\gamma") + mathModeEnd);
  text.Replace(wxS("\u03B4"), mathModeStart + wxS("\\delta") + mathModeEnd);
  text.Replace(wxS("\u03B5"), mathModeStart + wxS("\\epsilon") + mathModeEnd);
  text.Replace(wxS("\u03B6"), mathModeStart + wxS("\\zeta") + mathModeEnd);
  text.Replace(wxS("\u03B7"), mathModeStart + wxS("\\eta") + mathModeEnd);
  text.Replace(wxS("\u03B8"), mathModeStart + wxS("\\theta") + mathModeEnd);
  text.Replace(wxS("\u03B9"), mathModeStart + wxS("\\iota") + mathModeEnd);
  text.Replace(wxS("\u03BA"), mathModeStart + wxS("\\kappa") + mathModeEnd);
  text.Replace(wxS("\u03BB"), mathModeStart + wxS("\\lambda") + mathModeEnd);
  text.Replace(wxS("\u03BC"), mathModeStart + wxS("\\mu") + mathModeEnd);
  text.Replace(wxS("\u03BD"), mathModeStart + wxS("\\nu") + mathModeEnd);
  text.Replace(wxS("\u03BE"), mathModeStart + wxS("\\xi") + mathModeEnd);
  text.Replace(wxS("\u03BF"), wxS("o"));
  text.Replace(wxS("\u03C0"), mathModeStart + wxS("\\pi") + mathModeEnd);
  text.Replace(wxS("\u03C1"), mathModeStart + wxS("\\rho") + mathModeEnd);
  text.Replace(wxS("\u03C3"), mathModeStart + wxS("\\sigma") + mathModeEnd);
  text.Replace(wxS("\u03C4"), mathModeStart + wxS("\\tau") + mathModeEnd);
  text.Replace(wxS("\u03C5"), mathModeStart + wxS("\\upsilon") + mathModeEnd);
  text.Replace(wxS("\u03C6"), mathModeStart + wxS("\\phi") + mathModeEnd);
  text.Replace(wxS("\u03C7"), mathModeStart + wxS("\\chi") + mathModeEnd);
  text.Replace(wxS("\u03C8"), mathModeStart + wxS("\\psi") + mathModeEnd);
  text.Replace(wxS("\u03C9"), mathModeStart + wxS("\\omega") + mathModeEnd);
  text.Replace(wxS("\u0391"), wxS("A"));
  text.Replace(wxS("\u0392"), wxS("B"));
  text.Replace(wxS("\u0393"), mathModeStart + wxS("\\Gamma") + mathModeEnd);
  text.Replace(wxS("\u0394"), mathModeStart + wxS("\\Delta") + mathModeEnd);
  text.Replace(wxS("\u0395"), wxS("E"));
  text.Replace(wxS("\u0396"), wxS("Z"));
  text.Replace(wxS("\u0397"), wxS("H"));
  text.Replace(wxS("\u0398"), mathModeStart + wxS("\\Theta") + mathModeEnd);
  text.Replace(wxS("\u0399"), wxS("I"));
  text.Replace(wxS("\u039A"), wxS("K"));
  text.Replace(wxS("\u039B"), mathModeStart + wxS("\\Lambda") + mathModeEnd);
  text.Replace(wxS("\u039C"), wxS("M"));
  text.Replace(wxS("\u039D"), wxS("N"));
  text.Replace(wxS("\u039E"), mathModeStart + wxS("\\Xi") + mathModeEnd);
  text.Replace(wxS("\u039F"), wxS("O"));
  text.Replace(wxS("\u03A0"), mathModeStart + wxS("\\Pi") + mathModeEnd);
  text.Replace(wxS("\u03A1"), wxS("P"));
  text.Replace(wxS("\u03A3"), mathModeStart + wxS("\\Sigma") + mathModeEnd);
  text.Replace(wxS("\u03A4"), wxS("T"));
  text.Replace(wxS("\u03A5"), mathModeStart + wxS("\\Upsilon") + mathModeEnd);
  text.Replace(wxS("\u03A6"), mathModeStart + wxS("\\Phi") + mathModeEnd);
  text.Replace(wxS("\u03A7"), wxS("X"));
  text.Replace(wxS("\u03A8"), mathModeStart + wxS("\\Psi") + mathModeEnd);
  text.Replace(wxS("\u03A9"), mathModeStart + wxS("\\Omega") + mathModeEnd);
  text.Replace(wxS("\u2202"), mathModeStart + wxS("\\partial") + mathModeEnd);
  text.Replace(wxS("\u222b"), mathModeStart + wxS("\\int") + mathModeEnd);
  text.Replace(wxS("\u2245"), mathModeStart + wxS("\\approx") + mathModeEnd);
  text.Replace(wxS("\u221d"), mathModeStart + wxS("\\propto") + mathModeEnd);
  text.Replace(wxS("\u2260"), mathModeStart + wxS("\\neq") + mathModeEnd);
  text.Replace(wxS("\u2264"), mathModeStart + wxS("\\leq") + mathModeEnd);
  text.Replace(wxS("\u2265"), mathModeStart + wxS("\\geq") + mathModeEnd);
  text.Replace(wxS("\u226A"), mathModeStart + wxS("\\ll") + mathModeEnd);
  text.Replace(wxS("\u226B"), mathModeStart + wxS("\\gg") + mathModeEnd);
  text.Replace(wxS("\u220e"),
               mathModeStart + wxS("\\blacksquare") + mathModeEnd);
  text.Replace(wxS("\u2263"), mathModeStart + wxS("\\equiv") + mathModeEnd);
  text.Replace(wxS("\u2211"), mathModeStart + wxS("\\sum") + mathModeEnd);
  text.Replace(wxS("\u220F"), mathModeStart + wxS("\\prod") + mathModeEnd);
  text.Replace(wxS("\u2225"), mathModeStart + wxS("\\parallel") + mathModeEnd);
  text.Replace(wxS("\u27C2"), mathModeStart + wxS("\\bot") + mathModeEnd);
  text.Replace(wxS("~"), mathModeStart + wxS("\\sim ") + mathModeEnd);
  text.Replace(wxS("_"), wxS("\\_ "));
  text.Replace(wxS("$"), wxS("\\$ "));
  text.Replace(wxS("%"), wxS("\\% "));
  text.Replace(wxS("&"), wxS("\\& "));
  text.Replace(wxS("@"), mathModeStart + wxS("@") + mathModeEnd);
  text.Replace(wxS("#"), mathModeStart + wxS("\\neq") + mathModeEnd);
  text.Replace(wxS("\u00A0"), wxS("~")); // A non-breakable space
  text.Replace(wxS("<"), mathModeStart + wxS("<") + mathModeEnd);
  text.Replace(wxS(">"), mathModeStart + wxS(">") + mathModeEnd);
  text.Replace(wxS("\u219D"), mathModeStart + wxS("\\leadsto") + mathModeEnd);
  text.Replace(wxS("\u2192"),
               mathModeStart + wxS("\\rightarrow") + mathModeEnd);
  text.Replace(wxS("\u2794"),
               mathModeStart + wxS("\\longrightarrow") + mathModeEnd);

  // IsHidden() is set for parenthesis that don't need to be shown
  if (IsHidden() ||
      ((m_configuration->HidemultiplicationSign()) && GetHidableMultSign())) {
    // Normally in TeX the spacing between variable names following each other
    // directly is chosen to show that this is a multiplication. But any use of
    // \mathit{} will change to ordinary text spacing which means we need to add
    // a \, to show that we want to multiply the two long variable names.
    if ((text == wxS("*")) || (text == wxS("\u00B7"))) {
      // We have a hidden multiplication sign
      if (
          // This multiplication sign is between 2 cells
          (GetPrevious() && GetNext()) &&
          // These cells are two variable names
          ((GetPrevious()->GetTextStyle() == TS_VARIABLE) &&
           (GetNext()->GetTextStyle() == TS_VARIABLE)) &&
          // The variable name prior to this cell has no subscript
          (!(GetPrevious()->ToString().Contains(wxS('_')))) &&
          // we will be using \mathit{} for the TeX output.
          ((ToString().Length() > 1) || (GetNext()->ToString().Length() > 1)))
        text = wxS("\\, ");
      else
        text = wxS(" ");
    } else
      text = wxEmptyString;
  } else {
    /*
      Normally we want to draw a centered dot in this case. But if we
      are in the denominator of a d/dt or are drawing the "dx" or
      similar of an integral a centered dot looks stupid and will be
      replaced by a short space ("\,") instead. Likewise we don't want
      to begin a parenthesis with a centered dot even if this
      parenthesis does contain a product.
    */

    if (GetSuppressMultiplicationDot()) {
      text.Replace(wxS("*"), wxS("\\, "));
      text.Replace(wxS("\u00B7"), wxS("\\, "));
    } else {
      // If we want to know if the last element was a "d" we first have to
      // look if there actually is a last element.
      if (GetPrevious()) {
        if (GetPrevious()->GetTextStyle() == TS_SPECIAL_CONSTANT &&
            GetPrevious()->ToTeX() == wxS("d")) {
          text.Replace(wxS("*"), wxS("\\, "));
          text.Replace(wxS("\u00B7"), wxS("\\, "));
        } else {
          text.Replace(wxS("*"), wxS("\\ensuremath{\\cdot}"));
          text.Replace(wxS("\u00B7"), wxS("\\ensuremath{\\cdot}}"));
        }
      }
    }
  }

  if (GetTextStyle() == TS_GREEK_CONSTANT) {
    if (text == wxS("\\% alpha"))
      return wxS("\\alpha ");
    else if (text == wxS("\\% beta"))
      return wxS("\\beta ");
    else if (text == wxS("\\% gamma"))
      return wxS("\\gamma ");
    else if (text == wxS("\\% delta"))
      return wxS("\\delta ");
    else if (text == wxS("\\% epsilon"))
      return wxS("\\epsilon ");
    else if (text == wxS("\\% zeta"))
      return wxS("\\zeta ");
    else if (text == wxS("\\% eta"))
      return wxS("\\eta ");
    else if (text == wxS("\\% theta"))
      return wxS("\\theta ");
    else if (text == wxS("\\% iota"))
      return wxS("\\iota ");
    else if (text == wxS("\\% kappa"))
      return wxS("\\kappa ");
    else if (text == wxS("\\% lambda"))
      return wxS("\\lambda ");
    else if (text == wxS("\\% mu"))
      return wxS("\\mu ");
    else if (text == wxS("\\% nu"))
      return wxS("\\nu ");
    else if (text == wxS("\\% xi"))
      return wxS("\\ui ");
    else if (text == wxS("\\% omicron"))
      return wxS("\\omicron ");
    else if (text == wxS("\\% pi"))
      return wxS("\\pi ");
    else if (text == wxS("\\% rho"))
      return wxS("\\rho ");
    else if (text == wxS("\\% sigma"))
      return wxS("\\sigma ");
    else if (text == wxS("\\% tau"))
      return wxS("\\tau ");
    else if (text == wxS("\\% upsilon"))
      return wxS("\\upsilon ");
    else if (text == wxS("\\% phi"))
      return wxS("\\phi ");
    else if (text == wxS("\\% chi"))
      return wxS("\\chi ");
    else if (text == wxS("\\% psi"))
      return wxS("\\psi ");
    else if (text == wxS("\\% omega"))
      return wxS("\\omega ");
    else if (text == wxS("\\% Alpha"))
      return wxS("A");
    else if (text == wxS("\\% Beta"))
      return wxS("B");
    else if (text == wxS("\\% Gamma"))
      return wxS("\\Gamma ");
    else if (text == wxS("\\% Delta"))
      return wxS("\\Delta ");
    else if (text == wxS("\\% Epsilon"))
      return wxS("\\Epsilon ");
    else if (text == wxS("\\% Zeta"))
      return wxS("\\Zeta ");
    else if (text == wxS("\\% Eta"))
      return wxS("\\Eta ");
    else if (text == wxS("\\% Theta"))
      return wxS("\\Theta ");
    else if (text == wxS("\\% Iota"))
      return wxS("\\Iota ");
    else if (text == wxS("\\% Kappa"))
      return wxS("\\Kappa ");
    else if (text == wxS("\\% Lambda"))
      return wxS("\\Lambda ");
    else if (text == wxS("\\% Mu"))
      return wxS("\\Mu ");
    else if (text == wxS("\\% Nu"))
      return wxS("\\Nu ");
    else if (text == wxS("\\% Xi"))
      return wxS("\\ui ");
    else if (text == wxS("\\% Omicron"))
      return wxS("\\Omicron ");
    else if (text == wxS("\\% Pi"))
      return wxS("\\Pi ");
    else if (text == wxS("\\% Rho"))
      return wxS("\\Rho ");
    else if (text == wxS("\\% Sigma"))
      return wxS("\\Sigma ");
    else if (text == wxS("\\% Tau"))
      return wxS("\\Tau ");
    else if (text == wxS("\\% Upsilon"))
      return wxS("\\Upsilon ");
    else if (text == wxS("\\% Phi"))
      return wxS("\\Phi ");
    else if (text == wxS("\\% Chi"))
      return wxS("\\Chi ");
    else if (text == wxS("\\% Psi"))
      return wxS("\\Psi ");
    else if (text == wxS("\\% Omega"))
      return wxS("\\Omega ");

    return text;
  }

  if (GetTextStyle() == TS_SPECIAL_CONSTANT) {
    if (text == wxS("inf"))
      return wxS("\\infty ");
    else if (text == wxS("%e"))
      return wxS("e");
    else if (text == wxS("%i"))
      return wxS("i");
    else if (text == wxS("\\% pi"))
      return wxS("\\ensuremath{\\pi} ");
    else
      return text;
  }

  if ((GetTextStyle() == TS_LABEL) || (GetTextStyle() == TS_USERLABEL)) {
    wxString conditionalLinebreak;
    if (GetPrevious())
      conditionalLinebreak = wxS("\\]\n\\[");
    text.Trim(true);
    wxString label = text.SubString(1, text.Length() - 2);
    text = conditionalLinebreak + wxS("\\tag{") + label + wxS("}");
    label.Replace(wxS("\\% "), wxS(""));
    // Would be a good idea, but apparently breaks mathJaX
    // text += wxS("\\label{") + label + wxS("}");
  } else {
    if (GetTextStyle() == TS_FUNCTION) {
      if (text != wxEmptyString) {
        text.Replace(wxS("^"), wxS("\\hat{} "));
        text = wxS("\\operatorname{") + text + wxS("}");
      }
    } else if ((GetTextStyle() == TS_VARIABLE) ||
               (GetTextStyle() == TS_GREEK_CONSTANT) ||
               (GetTextStyle() == TS_SPECIAL_CONSTANT)) {
      if ((m_displayedText.Length() > 1) && (text[1] != wxS('_')))
        text = wxS("\\ensuremath{\\mathrm{") + text + wxS("}}");
      if (text == wxS("\\% pi"))
        text = wxS("\\ensuremath{\\pi} ");
      text.Replace(wxS("\\text{ä}"), wxS("\\text{\\textit{ä}}"));
      text.Replace(wxS("\\text{ö}"), wxS("\\text{\\textit{ö}}"));
      text.Replace(wxS("\\text{ü}"), wxS("\\text{\\textit{ü}}"));
      text.Replace(wxS("\\text{Ä}"), wxS("\\text{\\textit{Ä}}"));
      text.Replace(wxS("\\text{Ö}"), wxS("\\text{\\textit{Ö}}"));
      text.Replace(wxS("\\text{Ü}"), wxS("\\text{\\textit{Ü}}"));
    } else if ((GetTextStyle() == TS_ERROR) || (GetTextStyle() == TS_WARNING)) {
      if (text.Length() > 1)
        text = wxS("\\mbox{%error\n") + text + wxS("}");
    } else if (GetTextStyle() == TS_MATH) {
      if ((text.Length() > 2) && (text != wxS("\\,")) && (text != wxS("\\, ")))
        text = wxS("\\mbox{%default\n") + text + wxS("}");
    }
  }

  if ((GetTextStyle() != TS_FUNCTION) && (GetTextStyle() != TS_OUTDATED) &&
      (GetTextStyle() != TS_VARIABLE) && (GetTextStyle() != TS_NUMBER) &&
      (GetTextStyle() != TS_GREEK_CONSTANT) && (GetTextStyle() != TS_SPECIAL_CONSTANT))
    text.Replace(wxS("^"), wxS("\\textasciicircum"));

  if ((GetTextStyle() == TS_MATH) || (GetTextStyle() == TS_STRING)) {
    if (text.Length() > 1) {
      if (BreakLineHere())
        // text=wxS("\\ifhmode\\\\fi\n")+text;
        text = wxS("\\mbox{}\\\\") + text;
      /*      if(GetTextStyle() != TS_MATH)
              text.Replace(wxS(" "), wxS("\\, "));*/
    }
  }

  if ((GetTextStyle() == TS_LABEL) || (GetTextStyle() == TS_USERLABEL))
    text = text + wxS(" ");

  return text;
}

wxString TextCell::ToMathML() const {
  if (m_displayedText == wxEmptyString)
    return wxEmptyString;
  wxString text = XMLescape(ToString());

  // If we didn't display a multiplication dot we want to do the same in MathML.
  if (IsHidden() ||
      ((m_configuration->HidemultiplicationSign()) && GetHidableMultSign())) {
    text.Replace(wxS("*"), wxS("&#8290;"));
    text.Replace(wxS("\u00B7"), wxS("&#8290;"));
    if (text != wxS("&#8290;"))
      text = wxEmptyString;
  }
  text.Replace(wxS("*"), wxS("\u00B7"));

  switch (GetTextStyle()) {
  case TS_GREEK_CONSTANT:
    text = GetGreekStringUnicode();
    break;
  case TS_SPECIAL_CONSTANT: {
    text = GetGreekStringUnicode();
    // The "d" from d/dt can be written as a special unicode symbol. But firefox
    // doesn't support this currently => Commenting it out. if((GetTextStyle() ==
    // TS_SPECIAL_CONSTANT) && (text == wxS("d")))
    //   text = wxS("&#2146;");
    bool keepPercent = m_configuration->CheckKeepPercent();
    if (!keepPercent) {
      if (text == wxS("%e"))
        text = wxS("e");
      else if (text == wxS("%i"))
        text = wxS("i");
    }
  } break;
  case TS_VARIABLE: {
    text = GetGreekStringUnicode();

    bool keepPercent = m_configuration->CheckKeepPercent();

    if (!keepPercent) {
      if (text == wxS("%pi"))
        text = wxS("\u03C0");
    }
  } break;
  case TS_FUNCTION:
    text = GetGreekStringUnicode();
    if (text == wxS("inf"))
      text = wxS("\u221e");
    if ((text == wxS("+")) || (text == wxS("-")) || (text == wxS("*")) ||
        (text == wxS("/")))
      return wxS("<mo>") + text + wxS("</mo>\n");
    else
      return wxS("<mi>") + text + wxS("</mi>\n");
  case TS_NUMBER:
    return wxS("<mn>") + text + wxS("</mn>\n");

  case TS_LABEL:
  case TS_USERLABEL:
    return wxS("<mtext>") + text + wxS("</mtext></mtd><mtd>\n");

  case TS_STRING:
  default:
    if (text.StartsWith(wxS("\"")))
      return wxS("<ms>") + text + wxS("</ms>\n");
    else
      return wxS("<mo>") + text + wxS("</mo>\n");
  }

  return wxS("<mo>") + text + wxS("</mo>\n");
}

wxString TextCell::ToOMML() const {
  // Text-only lines are better handled in RTF.
  if ((GetPrevious() && (GetPrevious()->GetTextStyle() != TS_LABEL) &&
       (!GetPrevious()->HasHardLineBreak())) &&
      (HasHardLineBreak()))
    return wxEmptyString;

  // Labels are text-only.
  if ((GetTextStyle() == TS_LABEL) || (GetTextStyle() == TS_USERLABEL))
    return wxEmptyString;

  wxString text = XMLescape(m_displayedText);

  // If we didn't display a multiplication dot we want to do the same in MathML.
  if (IsHidden() ||
      ((m_configuration->HidemultiplicationSign()) && GetHidableMultSign())) {
    text.Replace(wxS("*"), wxS("&#8290;"));
    text.Replace(wxS("\u00B7"), wxS("&#8290;"));
    if (text != wxS("&#8290;"))
      text = wxEmptyString;
  }
  text.Replace(wxS("*"), wxS("\u00B7"));

  switch (GetTextStyle()) {
  case TS_GREEK_CONSTANT:
  case TS_SPECIAL_CONSTANT: {
    // The "d" from d/dt can be written as a special unicode symbol. But firefox
    // doesn't support this currently => Commenting it out. if((GetTextStyle() ==
    // TS_SPECIAL_CONSTANT) && (text == wxS("d")))
    //   text = wxS("&#2146;");
    bool keepPercent = m_configuration->CheckKeepPercent();
    if (!keepPercent) {
      if (text == wxS("%e"))
        text = wxS("e");
      else if (text == wxS("%i"))
        text = wxS("i");
    }
  }
    /* FALLTHRU */
  case TS_VARIABLE: {
    if (!m_configuration->CheckKeepPercent()) {
      if (text == wxS("%pi"))
        text = wxS("\u03C0");
    }
  } break;
  case TS_FUNCTION:
    text = GetGreekStringUnicode();
    if (text == wxS("inf"))
      text = wxS("\u221e");
    break;
  case TS_NUMBER:
    break;

  case TS_LABEL:
  case TS_USERLABEL:
    return wxEmptyString;

  case TS_STRING:
  default: {
  }
  }
  text = wxS("<m:r>") + text + wxS("</m:r>\n");
  return text;
}

wxString TextCell::ToRTF() const {
  wxString retval;
  wxString text = ToString();

  if (m_displayedText == wxEmptyString)
    return (wxS(" "));

  text.Replace(wxS("-->"), wxS("\u2192"));
  // Needed for the output of let(a/b,a+1);
  text.Replace(wxS(" --> "), wxS("\u2192"));
  if ((GetTextStyle() == TS_LABEL) || (GetTextStyle() == TS_USERLABEL)) {
    retval += wxString::Format(wxS("\\cf%i{"), (int)GetTextStyle());
    retval += RTFescape(text);
    retval += wxS("}\\cf0");
  }
  return retval;
}

wxString TextCell::GetXMLFlags() const {
  wxString flags;
  if (HasHardLineBreak() && (GetTextStyle() != TS_LABEL) &&
      (GetTextStyle() != TS_USERLABEL))
    flags += wxS(" breakline=\"true\"");
  
  if (GetTextStyle() == TS_ASCIIMATHS)
    flags += wxS(" type=\"ASCII-Art\"");
  
  if (GetTextStyle() == TS_OPERATOR)
    flags += wxS(" type=\"Operator\"");
  
  if (GetTextStyle() == TS_ERROR)
    flags += wxS(" type=\"error\"");

  if (GetTextStyle() == TS_WARNING)
    flags += wxS(" type=\"warning\"");

  if (!GetAltCopyText().empty())
    flags += wxS(" altCopy=\"") + XMLescape(GetAltCopyText()) + wxS("\"");

  if (!GetLocalToolTip().empty())
    flags += wxS(" tooltip=\"") + XMLescape(GetLocalToolTip()) + wxS("\"");

  if (GetTextStyle() == TS_USERLABEL)
    flags += wxS(" userdefined=\"yes\"");

  return flags;
}

wxString TextCell::ToXML() const {
  wxString tag;
  if (IsHidden() || GetHidableMultSign())
    tag = wxS("h");
  else
    switch (GetTextStyle()) {
    case TS_GREEK_CONSTANT:
      tag = wxS("g");
      break;
    case TS_SPECIAL_CONSTANT:
      tag = wxS("s");
      break;
    case TS_VARIABLE:
      tag = wxS("v");
      break;
    case TS_FUNCTION:
      tag = wxS("fnm");
      break;
    case TS_NUMBER:
      tag = wxS("n");
      break;
    case TS_STRING:
      tag = wxS("st");
      break;
    case TS_LABEL:
      tag = wxS("lbl");
      break;
    case TS_USERLABEL:
      tag = wxS("lbl");
      break;
    case TS_MAIN_PROMPT:
      tag = wxS("lbl");
      break;
    case TS_OTHER_PROMPT:
      tag = wxS("lbl");
      break;
    default:
      tag = wxS("t");
    }

  wxString xmlstring = XMLescape(m_displayedText);
  // convert it, so that the XML configuration doesn't fail

  return wxS("<") + tag + GetXMLFlags() + wxS(">") + xmlstring + wxS("</") +
    tag + wxS(">");
}

bool TextCell::IsShortNum() const {
  return (!GetNext()) && (m_text.Length() < 4);
}

wxString TextCell::GetGreekStringUnicode() const {
  wxString txt(m_text);

  if (!txt.empty() && txt[0] != '%')
    txt.Prepend(wxS("%"));

  if (txt == wxS("%alpha"))
    return wxS("\u03B1");
  else if (txt == wxS("%beta"))
    return wxS("\u03B2");
  else if (txt == wxS("%gamma"))
    return wxS("\u03B3");
  else if (txt == wxS("%delta"))
    return wxS("\u03B4");
  else if (txt == wxS("%epsilon"))
    return wxS("\u03B5");
  else if (txt == wxS("%zeta"))
    return wxS("\u03B6");
  else if (txt == wxS("%eta"))
    return wxS("\u03B7");
  else if (txt == wxS("%theta"))
    return wxS("\u03B8");
  else if (txt == wxS("%iota"))
    return wxS("\u03B9");
  else if (txt == wxS("%kappa"))
    return wxS("\u03BA");
  else if (txt == wxS("%lambda"))
    return wxS("\u03BB");
  else if (txt == wxS("%mu"))
    return wxS("\u03BC");
  else if (txt == wxS("%nu"))
    return wxS("\u03BD");
  else if (txt == wxS("%xi"))
    return wxS("\u03BE");
  else if (txt == wxS("%omicron"))
    return wxS("\u03BF");
  else if (txt == wxS("%pi"))
    return wxS("\u03C0");
  else if (txt == wxS("%rho"))
    return wxS("\u03C1");
  else if (txt == wxS("%sigma"))
    return wxS("\u03C3");
  else if (txt == wxS("%tau"))
    return wxS("\u03C4");
  else if (txt == wxS("%upsilon"))
    return wxS("\u03C5");
  else if (txt == wxS("%phi"))
    return wxS("\u03C6");
  else if (txt == wxS("%chi"))
    return wxS("\u03C7");
  else if (txt == wxS("%psi"))
    return wxS("\u03C8");
  else if (txt == wxS("%omega"))
    return wxS("\u03C9");
  else if (txt == wxS("%Alpha"))
    return wxS("\u0391");
  else if (txt == wxS("%Beta"))
    return wxS("\u0392");
  else if (txt == wxS("%Gamma"))
    return wxS("\u0393");
  else if (txt == wxS("%Delta"))
    return wxS("\u0394");
  else if (txt == wxS("%Epsilon"))
    return wxS("\u0395");
  else if (txt == wxS("%Zeta"))
    return wxS("\u0396");
  else if (txt == wxS("%Eta"))
    return wxS("\u0397");
  else if (txt == wxS("%Theta"))
    return wxS("\u0398");
  else if (txt == wxS("%Iota"))
    return wxS("\u0399");
  else if (txt == wxS("%Kappa"))
    return wxS("\u039A");
  else if (txt == wxS("%Lambda"))
    return wxS("\u039B");
  else if (txt == wxS("%Mu"))
    return wxS("\u039C");
  else if (txt == wxS("%Nu"))
    return wxS("\u039D");
  else if (txt == wxS("%Xi"))
    return wxS("\u039E");
  else if (txt == wxS("%Omicron"))
    return wxS("\u039F");
  else if (txt == wxS("%Pi"))
    return wxS("\u03A0");
  else if (txt == wxS("%Rho"))
    return wxS("\u03A1");
  else if (txt == wxS("%Sigma"))
    return wxS("\u03A3");
  else if (txt == wxS("%Tau"))
    return wxS("\u03A4");
  else if (txt == wxS("%Upsilon"))
    return wxS("\u03A5");
  else if (txt == wxS("%Phi"))
    return wxS("\u03A6");
  else if (txt == wxS("%Chi"))
    return wxS("\u03A7");
  else if (txt == wxS("%Psi"))
    return wxS("\u03A8");
  else if (txt == wxS("%Omega"))
    return wxS("\u03A9");

  return m_text;
}

wxString TextCell::GetSymbolUnicode(bool keepPercent) const {
  if (m_text == wxS("+"))
    return wxS("+");
  else if (m_text == wxS("="))
    return wxS("=");
  else if (m_text == wxS("inf"))
    return wxS("\u221E");
  else if (m_text == wxS("%pi"))
    return wxS("\u03C0");
  else if (m_text == wxS("<="))
    return wxS("\u2264");
  else if (m_text == wxS(">="))
    return wxS("\u2265");
#ifndef __WXMSW__
  else if (m_text == wxS(" and "))
    return wxS(" \u22C0 ");
  else if (m_text == wxS(" or "))
    return wxS(" \u22C1 ");
  else if (m_text == wxS(" xor "))
    return wxS(" \u22BB ");
  else if (m_text == wxS(" nand "))
    return wxS(" \u22BC ");
  else if (m_text == wxS(" nor "))
    return wxS(" \u22BD ");
  else if (m_text == wxS(" implies "))
    return wxS(" \u21D2 ");
  else if (m_text == wxS(" equiv "))
    return wxS(" \u21D4 ");
  else if (m_text == wxS("not"))
    return wxS("\u00AC");
#endif
  else if (m_text == wxS("->"))
    return wxS("\u2192");
  else if (m_text == wxS("-->"))
    return wxS("\u2794");
  // The next two ones are needed for the output of let(a/b,a+1);
  else if (m_text == wxS(" --> "))
    return wxS("\u2794");
  else if (m_text == wxS(" \u2212\u2192 "))
    return wxS("\u2794");
  /*
    else if (GetTextStyle() == TS_SPECIAL_CONSTANT && m_text == wxS("d"))
    return wxS("\u2202");
  */

  if (!keepPercent) {
    if (m_text == wxS("%e"))
      return wxS("e");
    else if (m_text == wxS("%i"))
      return wxS("i");
    else if (m_text == wxS("%pi"))
      return wxString(wxS("\u03C0"));
  }

  return wxEmptyString;
}

wxString TextCell::GetGreekStringTeX() const {
  if (m_text == wxS("gamma"))
    return wxS("\u00C0");
  else if (m_text == wxS("zeta"))
    return wxS("\u00B0");
  else if (m_text == wxS("psi"))
    return wxS("\u00C9");

  wxString txt(m_text);
  if (txt[0] != '%')
    txt = wxS("%") + txt;

  if (txt == wxS("%alpha"))
    return wxS("\u00CB");
  else if (txt == wxS("%beta"))
    return wxS("\u00CC");
  else if (txt == wxS("%gamma"))
    return wxS("\u00CD");
  else if (txt == wxS("%delta"))
    return wxS("\u00CE");
  else if (txt == wxS("%epsilon"))
    return wxS("\u00CF");
  else if (txt == wxS("%zeta"))
    return wxS("\u00B0");
  else if (txt == wxS("%eta"))
    return wxS("\u00D1");
  else if (txt == wxS("%theta"))
    return wxS("\u00D2");
  else if (txt == wxS("%iota"))
    return wxS("\u00D3");
  else if (txt == wxS("%kappa"))
    return wxS("\u00D4");
  else if (txt == wxS("%lambda"))
    return wxS("\u00D5");
  else if (txt == wxS("%mu"))
    return wxS("\u00D6");
  else if (txt == wxS("%nu"))
    return wxS("\u00B7");
  else if (txt == wxS("%xi"))
    return wxS("\u00D8");
  else if (txt == wxS("%omicron"))
    return wxS("o");
  else if (txt == wxS("%pi"))
    return wxS("\u00D9");
  else if (txt == wxS("%rho"))
    return wxS("\u00DA");
  else if (txt == wxS("%sigma"))
    return wxS("\u00DB");
  else if (txt == wxS("%tau"))
    return wxS("\u00DC");
  else if (txt == wxS("%upsilon"))
    return wxS("\u00B5");
  else if (txt == wxS("%chi"))
    return wxS("\u00DF");
  else if (txt == wxS("%psi"))
    return wxS("\u00EF");
  else if (txt == wxS("%phi"))
    return wxS("\u0027");
  else if (txt == wxS("%omega"))
    return wxS("\u0021");
  else if (txt == wxS("%Alpha"))
    return wxS("A");
  else if (txt == wxS("%Beta"))
    return wxS("B");
  else if (txt == wxS("%Gamma"))
    return wxS("\u00C0");
  else if (txt == wxS("%Delta"))
    return wxS("\u00C1");
  else if (txt == wxS("%Epsilon"))
    return wxS("E");
  else if (txt == wxS("%Zeta"))
    return wxS("Z");
  else if (txt == wxS("%Eta"))
    return wxS("H");
  else if (txt == wxS("%Theta"))
    return wxS("\u00C2");
  else if (txt == wxS("%Iota"))
    return wxS("I");
  else if (txt == wxS("%Kappa"))
    return wxS("K");
  else if (txt == wxS("%Lambda"))
    return wxS("\u00C3");
  else if (txt == wxS("%Mu"))
    return wxS("M");
  else if (txt == wxS("%Nu"))
    return wxS("N");
  else if (txt == wxS("%Xi"))
    return wxS("\u00C4");
  else if (txt == wxS("%Omicron"))
    return wxS("O");
  else if (txt == wxS("%Pi"))
    return wxS("\u00C5");
  else if (txt == wxS("%Rho"))
    return wxS("P");
  else if (txt == wxS("%Sigma"))
    return wxS("\u00C6");
  else if (txt == wxS("%Tau"))
    return wxS("T");
  else if (txt == wxS("%Upsilon"))
    return wxS("Y");
  else if (txt == wxS("%Phi"))
    return wxS("\u00C8");
  else if (txt == wxS("%Chi"))
    return wxS("X");
  else if (txt == wxS("%Psi"))
    return wxS("\u00C9");
  else if (txt == wxS("%Omega"))
    return wxS("\u00CA");

  return wxEmptyString;
}

wxString TextCell::GetSymbolTeX() const {
  if (m_text == wxS("inf"))
    return wxS("\u0031");
  else if (m_text == wxS("+"))
    return wxS("+");
  else if (m_text == wxS("%pi"))
    return wxS("\u00D9");
  else if (m_text == wxS("="))
    return wxS("=");
  else if (m_text == wxS("->"))
    return wxS("\u0021");
  else if (m_text == wxS(">="))
    return wxS("\u00D5");
  else if (m_text == wxS("<="))
    return wxS("\u00D4");
  /*
    else if (m_text == wxS(" and "))
    return wxS(" \u005E ");
    else if (m_text == wxS(" or "))
    return wxS(" \u005F ");
    else if (m_text == wxS(" nand "))
    return wxS(" \u0022 ");
    else if (m_text == wxS(" nor "))
    return wxS(" \u0023 ");
    else if (m_text == wxS(" eq "))
    return wxS(" \u002C ");
    else if (m_text == wxS(" implies "))
    return wxS(" \u0029 ");
    else if (m_text == wxS("not"))
    return wxS("\u003A");
    else if (m_text == wxS(" xor "))
    return wxS("\u00C8");
  */

  return wxEmptyString;
}

// RegExes all TextCells share.
wxRegEx TextCell::m_unescapeRegEx(wxS("\\\\(.)"));
wxRegEx TextCell::m_roundingErrorRegEx1(wxS("\\.000000000000[0-9]+$"));
wxRegEx TextCell::m_roundingErrorRegEx2(wxS("\\.999999999999[0-9]+$"));
wxRegEx TextCell::m_roundingErrorRegEx3(wxS("\\.000000000000[0-9]+e"));
wxRegEx TextCell::m_roundingErrorRegEx4(wxS("\\.999999999999[0-9]+e"));
