// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2006-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2012 Doug Ilijev <doug.ilijev@gmail.com>
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
//  Foundation, Inc., 59 Temple Placet, Suite 330, Boston, MA  02111-1307  USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  This file defines the class EditorCell

  EditorCell is the Cell type that represents the field that contains user input.
 */

#include <wx/clipbrd.h>
#include <wx/regex.h>

#include "EditorCell.h"
#include "wxMaxima.h"
#include "MarkDown.h"
#include "wxMaximaFrame.h"
#include <wx/tokenzr.h>
#include "MaximaTokenizer.h"

#define ESC_CHAR wxT('\xA6')

EditorCell::EditorCell(Cell *parent, Configuration **config,
                       CellPointers *cellPointers, wxString text) : Cell(parent, config)
{
  m_errorIndex = -1;
  m_autoAnswer = false;
  m_cellPointers = cellPointers;
  m_numberOfLines = 1;
  m_charHeight = 12;
  m_selectionChanged = false;
  m_oldSelectionStart = -1;
  m_oldSelectionEnd = -1;
  m_lastSelectionStart = -1;
  m_displayCaret = false;
  m_text = wxEmptyString;
  m_fontSize = -1;
  m_fontSize_Last = -1;
  m_positionOfCaret = 0;
  m_caretColumn = -1; // used when moving up/down between lines
  m_selectionStart = -1;
  m_selectionEnd = -1;
  m_paren1 = m_paren2 = -1;
  m_isDirty = false;
  m_hasFocus = false;
  m_underlined = false;
  m_fontWeight = wxFONTWEIGHT_NORMAL;
  m_fontStyle = wxFONTSTYLE_NORMAL;
  m_fontEncoding = wxFONTENCODING_DEFAULT;
  m_saveValue = false;
  m_containsChanges = false;
  m_containsChangesCheck = false;
  m_firstLineOnly = false;
  m_historyPosition = -1;
  SetValue(TabExpand(text, 0));
  ResetSize();  
}

wxString EditorCell::EscapeHTMLChars(wxString input)
{
  input.Replace(wxT("&"), wxT("&amp;"));
  input.Replace(wxT("\""), wxT("&quot;"));
  input.Replace(wxT("<"), wxT("&lt;"));
  input.Replace(wxT(">"), wxT("&gt;"));
  input.Replace(wxT("\n"), wxT("<br/>\n"));
  input.Replace(wxT("\r"), wxT(" "));
  return input;
}

void EditorCell::AddDrawParameter(wxString param)
{
  SaveValue();
  if(m_positionOfCaret < 0)
    return;

  if(param == wxEmptyString)
    return;

  wxString paramTrimmed = param;
  paramTrimmed.Trim();
  if(paramTrimmed == wxEmptyString)
    return;

  int pos = 1;

  // Insert a comma in front of the parameter, if necessary
  wxString::iterator ch = m_text.begin();
  bool commaNeededBefore = false;
  bool commaNeededAfter = false;
  while (ch < m_text.end())
  {
    if(
      (*ch == wxT('(')) ||
      (*ch == wxT('[')) ||
      (*ch == wxT(','))
      )
      commaNeededBefore = false;
    else
    {
      if(!(
           (*ch == wxT(' ')) ||
           (*ch == wxT('\n')) ||
           (*ch == wxT('\r')) ||
           (*ch == wxT('\t'))
           )
        )
        commaNeededBefore = true;
    }

    if(pos > m_positionOfCaret)
      break;
    else
    {
      ++ch;++pos;
    }
  }

  // if(ch < m_text.end())
  //  ++ch;

  while (ch < m_text.end())
  {
    if(
      (*ch == wxT(')')) ||
      (*ch == wxT(']')) ||
      (*ch == wxT(','))
      )
    {
      commaNeededAfter = false;
      break;
    }
    if(
      (*ch != wxT(' ' )) &&
      (*ch != wxT('\n')) &&
      (*ch != wxT('\r')) &&
      (*ch != wxT('\t'))
      )
    {
      commaNeededAfter = true;
      break;
    }

    ++ch;++pos;
  }

  if(commaNeededAfter)
    param += ",";

  wxString textAfterParameter = m_text.Right(m_text.Length() - m_positionOfCaret);
  m_text = m_text.Left(m_positionOfCaret);
  m_text.Trim();
  if(commaNeededBefore)
  {
    m_text += wxT(",");
    m_positionOfCaret ++;
  }

  wxStringTokenizer lines(param, wxT("\n"), wxTOKEN_RET_EMPTY_ALL);
  while(lines.HasMoreTokens())
  {
    // Todo: Don't insert a newline if we are at the beginning of a line.
    ProcessNewline(false);
    wxString line = lines.GetNextToken();
    line.Trim(false);
    m_text += line;
    m_positionOfCaret += line.Length();
  }
  m_text += textAfterParameter;
  StyleText();
  ResetSize();
  if (m_group != NULL)
    m_group->ResetSize();
}

wxString EditorCell::GetFullCommandUnderCursor()
{
  if(!IsActive())
    return wxEmptyString;

  if(m_text == wxEmptyString)
    return wxEmptyString;

  wxString result;
  int pos = 1;

  wxString::iterator ch = m_text.begin();
  while (ch < m_text.end())
  {
    result += *ch;
    if(*ch == wxT('\\'))
    {
      ++ch;++pos;
      if(ch < m_text.end())
        result += *ch;
    }
    else
    {
      if((*ch == ';') || (*ch == '$'))
      {
        if(m_positionOfCaret < pos)
          return result;
        result = wxEmptyString;
      }
    }

    if(ch < m_text.end())
    {
      ++ch;++pos;
    }
  }
  return result;
}

wxString EditorCell::PrependNBSP(wxString input)
{
  bool firstSpace = true;;
  wxString retval;

  input.Replace(wxT("\r"), wxT(" "));

  for (size_t i = 0; i < input.Length(); i++)
  {
    wxChar ch = input.GetChar(i);
    if (ch == wxT('\n'))
      firstSpace = true;

    if (ch == wxT(' '))
    {
      if (firstSpace)
      {
        firstSpace = false;
        retval += ch;
      }
      else
        retval += wxT("\xA0");
    }
    else
    {
      retval += ch;
      firstSpace = true;
    }
  }
  return retval;
}

Cell *EditorCell::Copy()
{
  EditorCell *tmp = new EditorCell(m_group, m_configuration, m_cellPointers);
  // We cannot use SetValue() here, since SetValue() sometimes has the task to change
  //  the cell's contents
  tmp->m_text = m_text;
  tmp->m_containsChanges = m_containsChanges;
  CopyData(this, tmp);
  tmp->m_styledText = m_styledText;

  return tmp;
}

wxString EditorCell::ToString()
{
  return ToString(false);
}

wxString EditorCell::ToString(bool dontLimitToSelection)
{
  wxString text = m_text;
  // Remove all soft line breaks
  text.Replace(wxT('\r'), wxT(' '));
  // Convert non-breakable spaces to breakable ones
  text.Replace(wxT("\xa0"), wxT(" "));

  if (SelectionActive() && (!dontLimitToSelection))
  {
    long start = wxMin(m_selectionStart, m_selectionEnd);
    long end = wxMax(m_selectionStart, m_selectionEnd) - 1;
    if (end >= (signed)m_text.Length()) end = m_text.Length() - 1;
    if (start < 0) start = 0;
    text = m_text.SubString(start, end);
  }
  return text;
}

wxString EditorCell::ToMatlab()
{
  return ToMatlab(false);
}

wxString EditorCell::ToMatlab(bool dontLimitToSelection)
{
  wxString text = m_text;
  // Remove all soft line breaks
  text.Replace(wxT('\r'), wxT(' '));
  // Convert non-breakable spaces to breakable ones
  text.Replace(wxT("\xa0"), wxT(" "));

  if (SelectionActive() && (!dontLimitToSelection))
  {
	long start = wxMin(m_selectionStart, m_selectionEnd);
	long end = wxMax(m_selectionStart, m_selectionEnd) - 1;
	if (end >= (signed)m_text.Length()) end = m_text.Length() - 1;
	if (start < 0) start = 0;
	text = m_text.SubString(start, end);
  }
  return text;
}

wxString EditorCell::ToRTF()
{
  wxString retval;

  switch (m_type)
  {
    case MC_TYPE_TITLE:
      retval += wxT("\\pard\\s16\\b\\f0\\fs56 ") + RTFescape(m_text) + wxT("\n");
      break;
    case MC_TYPE_SECTION:
      retval += wxT("\\pard\\s1\\b\\f0\\fs40 ") + RTFescape(m_text) + wxT("\n");
      break;
    case MC_TYPE_SUBSECTION:
      retval += wxT("\\pard\\s2\\b\\f0\\fs36 ") + RTFescape(m_text) + wxT("\n");
      break;
    case MC_TYPE_SUBSUBSECTION:
      retval += wxT("\\pard\\s3\\b\\f0\\fs32 ") + RTFescape(m_text) + wxT("\n");
      break;
    case MC_TYPE_HEADING5:
      retval += wxT("\\pard\\s4\\b\\f0\\fs32 ") + RTFescape(m_text) + wxT("\n");
      break;
    case MC_TYPE_HEADING6:
      retval += wxT("\\pard\\s5\\b\\f0\\fs32 ") + RTFescape(m_text) + wxT("\n");
      break;
    case MC_TYPE_PROMPT:
      retval += wxString::Format(wxT("\\cf%i"), GetStyle()) +
                wxT("\\pard\\s22\\li1105\\lin1105\\fi-1105\\f0\\fs24 ") + RTFescape(m_text) + wxT("\n");
      break;
    case MC_TYPE_INPUT:
    {
      retval += wxT(" ");
      for (std::vector<StyledText>::iterator textSnippet = m_styledText.begin();
           textSnippet != m_styledText.end(); ++textSnippet)
      {

        wxString text = RTFescape(textSnippet->GetText());

        if (textSnippet->StyleSet())
        {
          retval += wxString::Format(wxT("\\cf%i "), (int) textSnippet->GetStyle());
          retval += RTFescape(textSnippet->GetText());
        }
        else
        {
          retval += wxString::Format(wxT("\\cf%i "), (int) TS_DEFAULT);
          retval += wxT("{") + RTFescape(textSnippet->GetText()) + wxT("}\n");
        }
        if (textSnippet->GetText().Contains(wxT("\n")))
        {
          retval += wxT("\\pard\\s21\\li1105\\lin1105\\f0\\fs24 ");
        }
      }
      retval += wxString::Format(wxT("\\cf%i "), (int) TS_DEFAULT);
      break;
    }
    default:
      retval += wxT("\\pard\\s0 ") + RTFescape(m_text);
      break;
  }
  return retval;
}

EditorCell::~EditorCell()
{
  MarkAsDeleted();
}

void EditorCell::MarkAsDeleted()
{
  if (m_cellPointers->m_cellMouseSelectionStartedIn == this)
    m_cellPointers->m_cellMouseSelectionStartedIn = NULL;
  if (m_cellPointers->m_cellKeyboardSelectionStartedIn == this)
    m_cellPointers->m_cellKeyboardSelectionStartedIn = NULL;
  if (m_cellPointers->m_cellSearchStartedIn == this)
  {
    m_cellPointers->m_cellSearchStartedIn = NULL;
    m_cellPointers->m_indexSearchStartedAt = -1;
  }
  if (m_cellPointers->m_activeCell == this)
    m_cellPointers->m_activeCell = NULL;

  Cell::MarkAsDeleted();
}

std::list<Cell *> EditorCell::GetInnerCells()
{
  std::list<Cell *> innerCells;
  return innerCells;
}

wxString EditorCell::ToTeX()
{
  wxString text = m_text;
  if (!text.StartsWith(wxT("TeX:")))
  {
    text.Replace(wxT("\xa0"), wxT("~"));
    text.Replace(wxT("\\"), wxT("\\ensuremath{\\backslash}"));
    text.Replace(wxT("\r"), wxEmptyString);
    text.Replace(wxT("^"), wxT("\\^{}"));
    text.Replace(wxT("°"), wxT("\\ensuremath{^\\circ}"));
    text.Replace(wxT("\x2212"), wxT("-")); // unicode minus sign
    text.Replace(wxT("\x03B1"), wxT("\\ensuremath{\\alpha}"));
    text.Replace(wxT("\x00B1"), wxT("\\ensuremath{\\pm}"));
    text.Replace(wxT("\x00B2"), wxT("\\ensuremath{^2}"));
    text.Replace(wxT("\x00B3"), wxT("\\ensuremath{^3}"));
    text.Replace(wxT("\x221A"), wxT("\\ensuremath{\\sqrt{}}"));
    text.Replace(wxT("\x2148"), wxT("\\ensuremath{\\mathbbm{i}}"));
    text.Replace(wxT("\x2147"), wxT("\\ensuremath{\\mathbbm{e}}"));
    text.Replace(wxT("\x210f"), wxT("\\ensuremath{\\hbar}"));
    text.Replace(wxT("\x2203"), wxT("\\ensuremath{\\exists}"));
    text.Replace(wxT("\x2204"), wxT("\\ensuremath{\\nexists}"));
    text.Replace(wxT("\x2208"), wxT("\\ensuremath{\\in}"));
    text.Replace(wxT("\x21D2"), wxT("\\ensuremath{\\Longrightarrow}"));
    text.Replace(wxT("\x221e"), wxT("\\ensuremath{\\infty}"));
    text.Replace(wxT("\x22C0"), wxT("\\ensuremath{\\wedge}"));
    text.Replace(wxT("\x22C1"), wxT("\\ensuremath{\\vee}"));
    text.Replace(wxT("\x22bb"), wxT("\\ensuremath{\\oplus}"));
    text.Replace(wxT("\x22BC"), wxT("\\ensuremath{\\overline{\\wedge}}"));
    text.Replace(wxT("\x22BB"), wxT("\\ensuremath{\\overline{\\vee}}"));
    text.Replace(wxT("\x00AC"), wxT("\\ensuremath{\\setminus}"));
    text.Replace(wxT("\x22C3"), wxT("\\ensuremath{\\cup}"));
    text.Replace(wxT("\x22C2"), wxT("\\ensuremath{\\cap}"));
    text.Replace(wxT("\x2286"), wxT("\\ensuremath{\\subseteq}"));
    text.Replace(wxT("\x2282"), wxT("\\ensuremath{\\subset}"));
    text.Replace(wxT("\x2288"), wxT("\\ensuremath{\\not\\subseteq}"));
    text.Replace(wxT("\x0127"), wxT("\\ensuremath{\\hbar}"));
    text.Replace(wxT("\x0126"), wxT("\\ensuremath{\\Hbar}"));
    text.Replace(wxT("\x2205"), wxT("\\ensuremath{\\emptyset}"));
    text.Replace(wxT("\x00BD"), wxT("\\ensuremath{\\frac{1}{2}}"));
    text.Replace(wxT("\x03B2"), wxT("\\ensuremath{\\beta}"));
    text.Replace(wxT("\x03B3"), wxT("\\ensuremath{\\gamma}"));
    text.Replace(wxT("\x03B4"), wxT("\\ensuremath{\\delta}"));
    text.Replace(wxT("\x03B5"), wxT("\\ensuremath{\\epsilon}"));
    text.Replace(wxT("\x03B6"), wxT("\\ensuremath{\\zeta}"));
    text.Replace(wxT("\x03B7"), wxT("\\ensuremath{\\eta}"));
    text.Replace(wxT("\x03B8"), wxT("\\ensuremath{\\theta}"));
    text.Replace(wxT("\x03B9"), wxT("\\ensuremath{\\iota}"));
    text.Replace(wxT("\x03BA"), wxT("\\ensuremath{\\kappa}"));
    text.Replace(wxT("\x03BB"), wxT("\\ensuremath{\\lambda}"));
    text.Replace(wxT("\x03BC"), wxT("\\ensuremath{\\mu}"));
    text.Replace(wxT("\x03BD"), wxT("\\ensuremath{\\nu}"));
    text.Replace(wxT("\x03BE"), wxT("x"));
    text.Replace(wxT("\x03BF"), wxT("o"));
    text.Replace(wxT("\x03C0"), wxT("\\ensuremath{\\pi}"));
    text.Replace(wxT("\x03C1"), wxT("\\ensuremath{\\rho}"));
    text.Replace(wxT("\x03C3"), wxT("\\ensuremath{\\sigma}"));
    text.Replace(wxT("\x03C4"), wxT("\\ensuremath{\\tau}"));
    text.Replace(wxT("\x03C5"), wxT("\\ensuremath{\\upsilon}"));
    text.Replace(wxT("\x03C6"), wxT("\\ensuremath{\\phi}"));
    text.Replace(wxT("\x03C7"), wxT("\\ensuremath{\\chi}"));
    text.Replace(wxT("\x03C8"), wxT("\\ensuremath{\\psi}"));
    text.Replace(wxT("\x03C9"), wxT("\\ensuremath{\\omega}"));
    text.Replace(wxT("\x0391"), wxT("A"));
    text.Replace(wxT("\x0392"), wxT("B"));
    text.Replace(wxT("\x0393"), wxT("\\ensuremath{\\Gamma}"));
    text.Replace(wxT("\x0394"), wxT("\\ensuremath{\\Delta}"));
    text.Replace(wxT("\x0395"), wxT("E"));
    text.Replace(wxT("\x0396"), wxT("Z"));
    text.Replace(wxT("\x0397"), wxT("H"));
    text.Replace(wxT("\x0398"), wxT("\\ensuremath{\\Theta}"));
    text.Replace(wxT("\x0399"), wxT("I"));
    text.Replace(wxT("\x039A"), wxT("K"));
    text.Replace(wxT("\x039B"), wxT("\\ensuremath{\\Lambda}"));
    text.Replace(wxT("\x039C"), wxT("M"));
    text.Replace(wxT("\x039D"), wxT("N"));
    text.Replace(wxT("\x039E"), wxT("\\ensuremath{\\Xi}"));
    text.Replace(wxT("\x039F"), wxT("O"));
    text.Replace(wxT("\x03A0"), wxT("\\ensuremath{\\Pi}"));
    text.Replace(wxT("\x03A1"), wxT("P"));
    text.Replace(wxT("\x03A3"), wxT("\\ensuremath{\\Sigma}"));
    text.Replace(wxT("\x03A4"), wxT("T"));
    text.Replace(wxT("\x03A5"), wxT("\\ensuremath{\\Upsilon}"));
    text.Replace(wxT("\x03A6"), wxT("\\ensuremath{\\Phi}"));
    text.Replace(wxT("\x03A7"), wxT("X"));
    text.Replace(wxT("\x03A8"), wxT("\\ensuremath{\\Psi}"));
    text.Replace(wxT("\x03A9"), wxT("\\ensuremath{\\Omega}"));
    text.Replace(wxT("\x2202"), wxT("\\ensuremath{\\partial}"));
    text.Replace(wxT("\x222b"), wxT("\\ensuremath{\\int}"));
    text.Replace(wxT("\x2245"), wxT("\\ensuremath{\\approx}"));
    text.Replace(wxT("\x221d"), wxT("\\ensuremath{\\propto}"));
    text.Replace(wxT("\x2260"), wxT("\\ensuremath{\\neq}"));
    text.Replace(wxT("\x2264"), wxT("\\ensuremath{\\leq}"));
    text.Replace(wxT("\x2265"), wxT("\\ensuremath{\\geq}"));
    text.Replace(wxT("\x226A"), wxT("\\ensuremath{\\ll}"));
    text.Replace(wxT("\x226B"), wxT("\\ensuremath{\\gg}"));
    text.Replace(wxT("\x220e"), wxT("\\ensuremath{\\blacksquare}"));
    text.Replace(wxT("\x2263"), wxT("\\ensuremath{\\equiv}"));
    text.Replace(wxT("\x2211"), wxT("\\ensuremath{\\sum}"));
    text.Replace(wxT("\x220F"), wxT("\\ensuremath{\\prod}"));
    text.Replace(wxT("\x2225"), wxT("\\ensuremath{\\parallel}"));
    text.Replace(wxT("\x27C2"), wxT("\\ensuremath{\\bot}"));
    text.Replace(wxT("~"), wxT("\\ensuremath{\\sim }"));
    text.Replace(wxT("_"), wxT("\\_"));
    text.Replace(wxT("$"), wxT("\\$"));
    text.Replace(wxT("%"), wxT("\\%"));
    text.Replace(wxT("&"), wxT("\\&"));
    text.Replace(wxT("@"), wxT("\\ensuremath{@}"));
    text.Replace(wxT("#"), wxT("\\ensuremath{\\neq}"));
    text.Replace(wxT("\xDCB6"), wxT("~")); // A non-breakable space
    text.Replace(wxT("<"), wxT("\\ensuremath{<}"));
    text.Replace(wxT(">"), wxT("\\ensuremath{>}"));
    text.Replace(wxT("\x219D"), wxT("\\ensuremath{\\leadsto}"));
    text.Replace(wxT("\x2192"), wxT("\\ensuremath{\\rightarrow}"));
    text.Replace(wxT("\x27F6"), wxT("\\ensuremath{\\longrightarrow}"));
    // Now we might want to introduce some markdown:
    MarkDownTeX MarkDown(*m_configuration);
    text = MarkDown.MarkDown(text);
  }
  else
  {
    text=text.Mid(5, text.Length());
  }
  return text;
}

wxString EditorCell::ToXML()
{
  wxString xmlstring = m_text;
  // convert it, so that the XML parser doesn't fail
  xmlstring.Replace(wxT("&"), wxT("&amp;"));
  xmlstring.Replace(wxT("<"), wxT("&lt;"));
  xmlstring.Replace(wxT(">"), wxT("&gt;"));
  xmlstring.Replace(wxT("'"), wxT("&apos;"));
  xmlstring.Replace(wxT("\""), wxT("&quot;"));
  xmlstring.Replace(wxT("\n"), wxT("</line>\n<line>"));
  xmlstring.Replace(wxT("\r"), wxT(" "));
  xmlstring = wxT("<line>") + xmlstring + wxT("</line>\n");
  wxString head = wxT("<editor");
  switch (m_type)
  {
    case MC_TYPE_TEXT:
      head += wxT(" type=\"text\"");
      break;
    case MC_TYPE_TITLE:
      head += wxT(" type=\"title\" sectioning_level=\"1\"");
      break;
    case MC_TYPE_SECTION:
      head += wxT(" type=\"section\" sectioning_level=\"2\"");
      break;
    case MC_TYPE_SUBSECTION:
      head += wxT(" type=\"subsection\" sectioning_level=\"3\"");
      break;
    case MC_TYPE_SUBSUBSECTION:
      // We save subsubsections as subsections with a higher sectioning level:
      // This makes them backwards-compatible in the way that they are displayed
      // as subsections on old wxMaxima installations.
      head += wxT(" type=\"subsection\" sectioning_level=\"4\"");
      break;
    case MC_TYPE_HEADING5:
      head += wxT(" type=\"subsection\" sectioning_level=\"5\"");
      break;
    case MC_TYPE_HEADING6:
      head += wxT(" type=\"subsection\" sectioning_level=\"6\"");
      break;
    case MC_TYPE_INPUT:
    default:
      head += wxT(" type=\"input\"");
      break;
  }
  head += wxT(">\n");

  return head + xmlstring + wxT("</editor>\n");
}

void EditorCell::RecalculateWidths(int fontsize)
{
  Configuration *configuration = (*m_configuration);

  m_isDirty = false;
  if (NeedsRecalculation())
  {
    StyleText();
    m_fontSize_Last = Scale_Px(fontsize);
    wxDC *dc = configuration->GetDC();
    SetFont();

    // Measure the text hight using characters that might extend below or above the region
    // ordinary characters move in.
    int charWidth;
    dc->GetTextExtent(wxT("äXÄgy"), &charWidth, &m_charHeight);

    // We want a little bit of vertical space between two text lines (and between two labels).
    m_charHeight += 2 * MC_TEXT_PADDING;
    int width = 0, tokenwidth, tokenheight, linewidth = 0;

    m_numberOfLines = 1;

    std::vector<StyledText>::iterator textSnippet;

    for (
            textSnippet = m_styledText.begin();
            textSnippet != m_styledText.end();
            ++textSnippet
            )
    {
      if ((textSnippet->GetText().StartsWith(wxT('\n')) || (textSnippet->GetText().StartsWith(wxT('\r')))))
      {
        m_numberOfLines++;
        linewidth = textSnippet->GetIndentPixels();
      }
      else
      {
        dc->GetTextExtent(textSnippet->GetText(), &tokenwidth, &tokenheight);
        linewidth += tokenwidth;
        width = wxMax(width, linewidth);
      }
    }

    // Handle folding
    if (m_firstLineOnly)
      m_numberOfLines = 1;

    // Assign empty lines a minimum width
    if (m_text == wxEmptyString)
      width = charWidth;

    // Add a line border
    m_width = width + 2 * Scale_Px(2);

    // Calculate the cell height
    m_height = m_numberOfLines * (m_charHeight) + 2 * Scale_Px(2);

    // The center lies in the middle of the 1st line
    m_center = m_charHeight / 2;
  }
  Cell::RecalculateWidths(fontsize);
}

wxString EditorCell::ToHTML()
{
  EditorCell *tmp = this;
  wxString retval;

  while (tmp != NULL)
  {
    for (std::vector<StyledText>::iterator textSnippet = m_styledText.begin();
         textSnippet != m_styledText.end(); ++textSnippet)
    {
      wxString text = PrependNBSP(EscapeHTMLChars(textSnippet->GetText()));
/*      wxString tmp = EscapeHTMLChars(textSnippet->GetText());
        wxString text = tmp);*/

      if (textSnippet->StyleSet())
      {
        switch (textSnippet->GetStyle())
        {
          case TS_CODE_COMMENT:
            retval += wxT("<span class=\"code_comment\">") + text + wxT("</span>");
            break;
          case TS_CODE_VARIABLE:
            retval += wxT("<span class=\"code_variable\">") + text + wxT("</span>");
            break;
          case TS_CODE_FUNCTION:
            retval += wxT("<span class=\"code_function\">") + text + wxT("</span>");
            break;
          case TS_CODE_NUMBER:
            retval += wxT("<span class=\"code_number\">") + text + wxT("</span>");
            break;
          case TS_CODE_STRING:
            retval += wxT("<span class=\"code_string\">") + text + wxT("</span>");
            break;
          case TS_CODE_OPERATOR:
            retval += wxT("<span class=\"code_operator\">") + text + wxT("</span>");
            break;
          case TS_CODE_LISP:
            retval += wxT("<span class=\"code_lisp\">") + text + wxT("</span>");
            break;
          case TS_CODE_ENDOFLINE:
          default:
            retval += wxT("<span class=\"code_endofline\">") + text + wxT("</span>");
            break;
        }
      }
      else
        retval += text;
    }
    tmp = dynamic_cast<EditorCell *>(tmp->m_next);
  }
  return retval;
}

void EditorCell::MarkSelection(long start, long end, TextStyle style, int fontsize)
{
  Configuration *configuration = (*m_configuration);
  if ((start < 0) || (end < 0)) return;
  wxPoint point, point1;
  long pos1 = start, pos2 = start;

#if defined(__WXOSX__)
  configuration->GetDC()->SetPen(wxNullPen); // no border on rectangles
#else
      configuration->GetDC()->SetPen(*(wxThePenList->FindOrCreatePen(configuration->GetColor(style), 1, wxPENSTYLE_SOLID)) );
// window linux, set a pen
#endif
      configuration->GetDC()->SetBrush(*(wxTheBrushList->FindOrCreateBrush(configuration->GetColor(style)))); //highlight c.


  while (pos1 < end) // go through selection, draw a rect for each line of selection
  {
    while (pos1 < end && m_text.GetChar(pos1) != '\n' && m_text.GetChar(pos1) != '\r')
      pos1++;

    point = PositionToPoint(fontsize, pos2);  // left  point
    point1 = PositionToPoint(fontsize, pos1); // right point
    long selectionWidth = point1.x - point.x;
    wxRect rect;
#if defined(__WXOSX__)
    rect = GetRect(); // rectangle representing the cell
    if (pos1 != end) // we have a \n, draw selection to the right border (mac behaviour)
      selectionWidth = rect.GetRight() - point.x;
#endif

    rect = wxRect(point.x,
                  point.y + Scale_Px(1) - m_center,
                  selectionWidth,
                  m_charHeight);
    // draw the rectangle if it is in the region that is to be updated.
    if (InUpdateRegion(rect))
      (*m_configuration)->GetDC()->DrawRectangle(CropToUpdateRegion(rect));
    pos1++;
    pos2 = pos1;
  }
}

/* Draws the editor cell including selection and cursor

The order this cell is drawn is:
 1. draw selection (wxCOPY), TS_SELECTION color
 2. mark matching parenthesis (wxCOPY), TS_SELECTION color
 3. draw all text (wxCOPY)
 4. draw the caret (wxCOPY), TS_CURSOR color

 The text is not taken from m_text but from the list of styled text snippets
 StyleText() converts m_text into. This way the decisions needed for styling
 text are cached for later use.
*/
void EditorCell::Draw(wxPoint point1)
{
  Cell::Draw(point1);
  
  if ((!m_isHidden) && (DrawThisCell()))
  {
    wxRect rect = GetRect();
    int y = rect.GetY();
    
    Configuration *configuration = (*m_configuration);
    wxDC *dc = configuration->GetDC();

    // Set the background to the cell's background color
    if (m_height > 0 && m_width > 0 && y >= 0)
    {
      wxBrush *br;
      wxPen *pen;
      if(GetStyle() == TS_TEXT)
      {
        br = wxTheBrushList->FindOrCreateBrush(
          configuration->GetColor(TS_TEXT_BACKGROUND));
        pen = wxThePenList->FindOrCreatePen(
          configuration->GetColor(TS_TEXT_BACKGROUND),
          0,
          wxPENSTYLE_SOLID);
      }
      else
      {
        br = wxTheBrushList->FindOrCreateBrush(
          configuration->DefaultBackgroundColor());
        pen = wxThePenList->FindOrCreatePen(
          configuration->DefaultBackgroundColor(),
          0,
          wxPENSTYLE_SOLID);
      }
      dc->SetBrush(*br);
      dc->SetPen(*pen);
      rect.SetWidth((*m_configuration)->GetCanvasSize().GetWidth());
      if (InUpdateRegion(rect))
        dc->DrawRectangle(CropToUpdateRegion(rect));
    }
    dc->SetPen(*wxBLACK_PEN);
    SetFont();

    m_selectionChanged = false;
    wxPoint point(point1);

    //
    // Mark text that coincides with the selection
    //
    if (m_cellPointers->m_selectionString != wxEmptyString)
    {
      long start = 0;
      wxString text(m_text);
      text.Replace(wxT('\r'), wxT(' '));
      while ((start = text.find(m_cellPointers->m_selectionString, start)) != wxNOT_FOUND)
      {
        long end = start + m_cellPointers->m_selectionString.Length();

        // Mark only text that won't be marked in the next step:
        // This would not only be unnecessary but also could cause
        // selections to flicker in very long texts
        if ((!IsActive()) || (start != wxMin(m_selectionStart, m_selectionEnd)))
          MarkSelection(start, end, TS_EQUALSSELECTION, m_fontSize);
        if(m_cellPointers->m_selectionString.Length() == 0)
          end++;
        start = end;
      }
    }

    if (IsActive()) // draw selection or matching parens
    {
      //
      // Mark selection
      //
      if (m_selectionStart >= 0)
        MarkSelection(wxMin(m_selectionStart, m_selectionEnd),
                      wxMax(m_selectionStart, m_selectionEnd),
                      TS_SELECTION, m_fontSize);

        //
        // Matching parens - draw only if we don't have selection
        //
      else if (m_paren1 != -1 && m_paren2 != -1)
      {
#if defined(__WXOSX__)
        configuration->GetDC()->SetPen(wxNullPen); // no border on rectangles
#else
        dc->SetPen(*(wxThePenList->FindOrCreatePen(configuration->GetColor(TS_SELECTION), 1, wxPENSTYLE_SOLID))); // window linux, set a pen
#endif
        dc->SetBrush(*(wxTheBrushList->FindOrCreateBrush(configuration->GetColor(TS_SELECTION)))); //highlight c.

        wxPoint matchPoint = PositionToPoint(m_fontSize, m_paren1);
        int width, height;
        dc->GetTextExtent(m_text.GetChar(m_paren1), &width, &height);
        wxRect rect(matchPoint.x + 1,
                    matchPoint.y + Scale_Px(2) - m_center + 1,
                    width - 1, height - 1);
        if (InUpdateRegion(rect))
          dc->DrawRectangle(CropToUpdateRegion(rect));
        matchPoint = PositionToPoint(m_fontSize, m_paren2);
        dc->GetTextExtent(m_text.GetChar(m_paren1), &width, &height);
        rect = wxRect(matchPoint.x + 1,
                      matchPoint.y + Scale_Px(2) - m_center + 1,
                      width - 1, height - 1);
        if (InUpdateRegion(rect))
          dc->DrawRectangle(CropToUpdateRegion(rect));
      } // else if (m_paren1 != -1 && m_paren2 != -1)
    } // if (IsActive())

    //
    // Draw the text
    //
    SetPen();

    wxPoint TextStartingpoint = point;
    wxPoint TextCurrentPoint = TextStartingpoint;
    int lastStyle = -1;
    int lastIndent = 0;
    for (std::vector<StyledText>::iterator textSnippet = m_styledText.begin();
         textSnippet != m_styledText.end(); ++textSnippet)
    {
      wxString TextToDraw = textSnippet->GetText();
      int width, height;

      // A newline is a separate token.
      if ((TextToDraw == wxT("\n")) || (TextToDraw == wxT("\r")))
      {
        if ((TextToDraw == wxT("\n")))
          lastIndent = textSnippet->GetIndentPixels();

        // A newline =>
        // set the point to the beginning of the next line.
        TextCurrentPoint.x = TextStartingpoint.x;
        TextCurrentPoint.y += m_charHeight;
        TextCurrentPoint.x += textSnippet->GetIndentPixels();
      }
      else
      {
        // We need to draw some text.

        // Grab a pen of the right color.
        if (textSnippet->StyleSet())
        {
          wxDC *dc = configuration->GetDC();
          if (lastStyle != textSnippet->GetStyle())
          {
            dc->SetTextForeground(configuration->GetColor(textSnippet->GetStyle()));
            lastStyle = textSnippet->GetStyle();
          }
        }
        else
        {
          lastStyle = -1;
          SetForeground();
        }

        // replace "*" with centerdot and "-" by a Minus if requested
//        if (configuration->GetChangeAsterisk())
//        {
//          TextToDraw.Replace(wxT("*"), wxT("\xB7"));
//          if (m_type == MC_TYPE_INPUT)
//            TextToDraw.Replace(wxT("-"), wxT("\x2212"));
//        }

        // Draw a char that shows we continue an indentation - if this is needed.
        if (textSnippet->GetIndentChar() != wxEmptyString)
          dc->DrawText(textSnippet->GetIndentChar(),
                      TextStartingpoint.x + lastIndent,
                      TextCurrentPoint.y - m_center);

        dc->DrawText(TextToDraw,
                    TextCurrentPoint.x,
                    TextCurrentPoint.y - m_center);
        /*
        dc->DrawLine(TextCurrentPoint.x + Scale_Px(2),
                    TextCurrentPoint.y - m_center,
                    TextCurrentPoint.x + Scale_Px(2),
                    TextCurrentPoint.y); */

        dc->GetTextExtent(TextToDraw, &width, &height);
        TextCurrentPoint.x += width;
      }
    }
    //
    // Draw the caret
    //

    if (m_displayCaret && m_hasFocus && IsActive())
    {
      unsigned int caretInLine = 0;
      unsigned int caretInColumn = 0;

      PositionToXY(m_positionOfCaret, &caretInColumn, &caretInLine);

      int lineWidth = GetLineWidth(caretInLine, caretInColumn);

      dc->SetPen(*(wxThePenList->FindOrCreatePen(configuration->GetColor(TS_CURSOR), 1, wxPENSTYLE_SOLID)));
      dc->SetBrush(*(wxTheBrushList->FindOrCreateBrush(configuration->GetColor(TS_CURSOR), wxBRUSHSTYLE_SOLID)));
#if defined(__WXOSX__)
      // draw 1 pixel shorter caret than on windows
      dc->DrawRectangle(point.x  + lineWidth - (*m_configuration)->GetCursorWidth(),
                       point.y + Scale_Px(1) - m_center + caretInLine * m_charHeight,
                       (*m_configuration)->GetCursorWidth(),
                       m_charHeight - Scale_Px(5));
#else
      dc->DrawRectangle(point.x + + lineWidth-(*m_configuration)->GetCursorWidth()/2,
                       point.y + Scale_Px(2) - m_center + caretInLine * m_charHeight,
                       (*m_configuration)->GetCursorWidth(),
                       m_charHeight- Scale_Px(3));
#endif
    }

    UnsetPen();

  }
}

void EditorCell::SetFont()
{
  Configuration *configuration = (*m_configuration);
  wxDC *dc = configuration->GetDC();

  m_fontSize = configuration->GetFontSize(m_textStyle);
  if (m_fontSize < 4)
    m_fontSize = configuration->GetDefaultFontSize();

  m_fontSize = Scale_Px(m_fontSize);

  m_fontName = configuration->GetFontName(m_textStyle);
  // Cells that save answers are displayed differently to
  // ordinary cells in order to make transparent that this cell is special.
  if(!m_autoAnswer)
    m_fontStyle = configuration->IsItalic(m_textStyle);
  else
  {
    if(configuration->IsItalic(m_textStyle) != wxFONTSTYLE_SLANT)
      m_fontStyle = wxFONTSTYLE_SLANT;
    else
      m_fontStyle = wxFONTSTYLE_NORMAL;
  }
  m_fontWeight = configuration->IsBold(m_textStyle);
  m_underlined = configuration->IsUnderlined(m_textStyle);
  m_fontEncoding = configuration->GetFontEncoding();

  wxFont font;
  font.SetFamily(wxFONTFAMILY_MODERN);
  font.SetFaceName(m_fontName);
  font.SetEncoding(m_fontEncoding);
  font.SetStyle(m_fontStyle);
  font.SetWeight(m_fontWeight);
  font.SetUnderlined(m_underlined);
  font.SetEncoding(m_fontEncoding);
  if (!font.IsOk())
  {
    wxLogMessage(_("EditorCell Ignoring the font name as the selected font didn't work"));
    font.SetFamily(wxFONTFAMILY_MODERN);
    font.SetEncoding(m_fontEncoding);
    font.SetStyle(m_fontStyle);
    font.SetWeight(m_fontWeight);
    font.SetUnderlined(m_underlined);
  }

  if (!font.IsOk())
    font = *wxNORMAL_FONT;

  wxASSERT(m_fontSize >= 0);
  if(m_fontSize < 4)
    m_fontSize = 4;
#if wxCHECK_VERSION(3, 1, 2)
  font.SetFractionalPointSize(m_fontSize);
#else
  font.SetPointSize(m_fontSize);
#endif
  wxASSERT_MSG(font.IsOk(),
               _("Seems like something is broken with a font. Installing http://www.math.union.edu/~dpvc/jsmath/download/jsMath-fonts.html and checking \"Use JSmath fonts\" in the configuration dialogue should fix it."));
  dc->SetFont(font);
}

void EditorCell::SetForeground()
{
  Configuration *configuration = (*m_configuration);
  wxDC *dc = configuration->GetDC();
  dc->SetTextForeground(configuration->GetColor(m_textStyle));
}

wxString EditorCell::GetCurrentCommand()
{
  // Discard all chars behind the cursor.
  wxString lineTillCursor = m_text.Left(m_positionOfCaret);

  wxString command;
  wxString possibleCommand;
  wxString::iterator it = lineTillCursor.begin();
  while(it != lineTillCursor.end())
  {
    if(wxIsalpha(*it) || (*it == wxT('_')) || (*it == wxT('\\')))
    {
      if(*it == '\\')
      {
        *it;++it;
      }
      if(it != lineTillCursor.end())
      {
        possibleCommand += *it;
        ++it;
      }
      while((it != lineTillCursor.end()) && ((wxIsalnum(*it) ||
                                              (*it == wxT('_')) ||
                                              (*it == wxT('\\')))))
      {
        if(*it == '\\')
        {
          *it;++it;
        }
        if(it != lineTillCursor.end())
        {
          possibleCommand += *it;++it;
        }
      }
    }
    else
      switch(wxChar(*it))
      {
      case ' ':
      case '\t':
      case '\n':
      case '\r':
        while((it != lineTillCursor.end()) && ((*it == wxT(' ')) ||
                                               (*it == wxT('\t')) ||
                                               (*it == wxT('\n')) ||
                                               (*it == wxT('\r'))))
          ++it;
        if ((it != lineTillCursor.end()) && (*it == wxT('(')))
        {
          command = possibleCommand;
          possibleCommand = wxEmptyString;
          ++it;
        }
        break;
      case '(':
        if((possibleCommand != wxEmptyString))
          command = possibleCommand;
        ++it;
        break;
      case '$':
      case ';':
      {
        command = wxEmptyString;
        possibleCommand = wxEmptyString;
        ++it;
        break;
      }
      default:
        possibleCommand = wxEmptyString;
        ++it;
        break;
      }
  }
  return command;
}

wxString EditorCell::TabExpand(wxString input, long posInLine)
{
  if (posInLine < 0) posInLine = 0;
  wxString retval;
  // Convert the text to our line endings.
  input.Replace(wxT("\r\n"), wxT("\n"));

  wxString::iterator ch = input.begin();
  while (ch < input.end())
  {
    if ((*ch == wxT('\n')))
    {
      posInLine = 0;
      retval += *ch;
      ++ch;
      continue;
    }

    if (*ch == wxT('\t'))
    {
      switch (posInLine - (posInLine / 4) * 4)
      {
        case 0:
          retval += wxT("    ");
          break;
        case 1:
          retval += wxT("   ");
          break;
        case 2:
          retval += wxT("  ");
          break;
        case 3:
          retval += wxT(" ");
          break;
      }
      posInLine = 0;
      ++ch;
      continue;
    }
    else
      retval += *ch;
    if(ch < input.end())
      {
        ++ch;++posInLine;
      }
  }
  // TODO: Implement the actual TAB expansion
  return retval;
}

size_t EditorCell::BeginningOfLine(long pos)
{
  if (pos > 0)
    pos--;
  if (pos < 0) pos = 0;

  while (pos > 0)
  {
    if ((m_text[pos] == wxT('\n')) || (m_text[pos] == wxT('\r')))
      break;
    pos--;
  }
  if ((m_text[pos] == wxT('\n')) || (m_text[pos] == wxT('\r')))
    pos++;
  return pos;
}

size_t EditorCell::EndOfLine(long pos)
{
  if (pos < 0) pos = 0;
  while (pos < (long) m_text.length() && m_text[pos] != wxT('\n') && m_text[pos] != wxT('\r'))
    pos++;

  return pos;
}

#if defined __WXOSX__

bool EditorCell::HandleCtrlCommand(wxKeyEvent &ev)
{
  int code = ev.GetKeyCode();
  bool done = true;

  if (code >= 32)
    return false;

  code = code + 'A' - 1;

  switch (code)
  {
    case 'K':
    {
      ClearSelection();
      SaveValue();
      size_t end = EndOfLine(m_positionOfCaret);
      if (end == (size_t) m_positionOfCaret)
        end++;
      m_text = m_text.SubString(0, m_positionOfCaret - 1) + m_text.SubString(end, m_text.length());
      m_isDirty = true;
      break;
    }

    case 'E':
    {
      ClearSelection();
      int end = EndOfLine(m_positionOfCaret);
      if (ev.ShiftDown())
      {
        m_selectionStart = m_positionOfCaret;
        m_selectionEnd = end;
      }
      m_positionOfCaret = end;
      m_displayCaret = true;
      break;
    }

    case 'A':
    {
      ClearSelection();
      int start = BeginningOfLine(m_positionOfCaret);
      if (ev.ShiftDown())
      {
        m_selectionStart = start;
        m_selectionEnd = m_positionOfCaret;
      }
      m_positionOfCaret = start;
      m_displayCaret = true;
      break;
    }

    default:
      done = false;
      break;
  }

  return done;
}

#endif // __WXOSX__

void EditorCell::ProcessEvent(wxKeyEvent &event)
{
  bool done = false;

#if defined __WXOSX__
  done = HandleCtrlCommand(event);
#endif

  if(!done)
    done = HandleSpecialKey(event);

  if (!done)
    HandleOrdinaryKey(event);

  if (m_type == MC_TYPE_INPUT)
    FindMatchingParens();

  if (m_isDirty)
    ResetSize();
  m_displayCaret = true;
}

int EditorCell::GetIndentDepth(wxString text, int positionOfCaret)
{

  // Don't indent parenthesis that aren't part of code cells.
  if (m_type != MC_TYPE_INPUT)
    return 0;

  // A list of by how many chars we need to indent the current line.
  std::list<int> indentChars;
  indentChars.push_back(0);

  wxString::const_iterator it = m_text.begin();

  // Determine how many parenthesis this cell opens or closes before the point
  long pos = 0;
  while ((pos < positionOfCaret) && (it < m_text.end()))
  {
    wxChar ch = *it;
    if (ch == wxT('\\'))
    {
      ++pos;++it;
      continue;
    }

    if (ch == wxT('\"'))
    {
      ++pos;++it;
      while (
              (it < m_text.end()) &&
              (pos < positionOfCaret) &&
              (*it != wxT('\"'))
              )
      {
        ++pos;++it;
      }
    }

    if (
            (ch == wxT('(')) ||
            (ch == wxT('[')) ||
            (ch == wxT('{'))
            )
    {
      if (indentChars.empty())
        indentChars.push_back(4);
      else
        indentChars.push_back(indentChars.back() + 4);
    }

    if (
            (ch == wxT(')')) ||
            (ch == wxT(']')) ||
            (ch == wxT('}'))
            )
    {
      if (!indentChars.empty())
        indentChars.pop_back();
    }

    // A comma removes all extra indentation from a "do" or an "if".
    if (ch == wxT(','))
    {
      // Discard any extra indentation from a "then" or a "do" from the last item
      // of indentChars.
      if (!indentChars.empty())
      {
        int lst = indentChars.back();
        indentChars.pop_back();
        if (!indentChars.empty())
          lst = indentChars.back() + 4;
        else
          lst = 0;
        indentChars.push_back(lst);
      }
    }

    // A semicolon or a dollar sign restarts indentation completely.
    if (
            (ch == wxT(';')) ||
            (ch == wxT('$'))
            )
    {
      // Discard any indentation data
      while (!indentChars.empty())
        indentChars.pop_back();

      // Start fresh with zero indentation.
      indentChars.push_back(0);
    }

    // A "do" or an "if" increases the current indentation level by a tab.
    if ((!wxIsalnum(ch)) || (pos == 0))
    {
      // Concatenate the current with the following two characters
      wxString::const_iterator it2(it);
      wxString rest(*it2);
      ++it2;
      if(it2 < m_text.end())
      {
        rest += wxString(*it2);
        ++it2;
        if(it2 < m_text.end())
          rest += wxString(*it2);
      }
      // Handle a "do"
      if (rest.StartsWith(wxT("do")) && ((rest.Length() < 3) || (!wxIsalnum(rest[2]))))
      {
        int lst = 0;
        if (!indentChars.empty())
        {
          lst = indentChars.back();
          indentChars.pop_back();
        }
        indentChars.push_back(lst + 4);
      }

      // Handle a "if"
      if (rest.StartsWith(wxT("if")) && ((rest.Length() < 3) || (!wxIsalnum(rest[2]))))
      {
        int lst = 0;
        if (!indentChars.empty())
        {
          lst = indentChars.back();
          indentChars.pop_back();
        }
        indentChars.push_back(lst + 4);
      }
    }

    if(it < m_text.end())
    {
      ++pos;++it;
    }
  }

  if (it < m_text.end())
  {
    if (
            (text[positionOfCaret] == wxT(')')) ||
            (text[positionOfCaret] == wxT(']')) ||
            (text[positionOfCaret] == wxT('}'))
            )
    {
      if (!indentChars.empty())
        indentChars.pop_back();
    }
  }

  int retval;
  if (indentChars.empty())
    retval = 0;
  else
    retval = indentChars.back();

  // A fast way to get the next 5 characters
  wxString rightOfCursor;
  for(int i=0; i<5; i++)
  {
    if (it >= m_text.end())
      break;

    rightOfCursor += *it;
    ++it;
  }

  rightOfCursor.Trim();
  rightOfCursor.Trim(false);
  if (
          (
                  (rightOfCursor.StartsWith(wxT("else"))) ||
                  (rightOfCursor.StartsWith(wxT("then")))
          ) &&
          (rightOfCursor.Length() > 4) &&
          (!(wxIsalnum(rightOfCursor[4])))
          )
    retval -= 4;

  if (retval < 0) retval = 0;

  return retval;
}

void EditorCell::ProcessNewline(bool keepCursorAtStartOfLine)
{
        if (m_selectionStart != -1) // we have a selection, delete it, then proceed
      {
        SaveValue();
        long start = wxMin(m_selectionEnd, m_selectionStart);
        long end = wxMax(m_selectionEnd, m_selectionStart);
        m_text = m_text.SubString(0, start - 1) +
                 m_text.SubString(end, m_text.Length());
        m_positionOfCaret = start;
        ClearSelection();
      }

      {
        bool autoIndent = (*m_configuration)->GetAutoIndent();
        // If the cursor is at the beginning of a line we will move it there again after
        // indenting.
        bool cursorAtStartOfLine = keepCursorAtStartOfLine &&
          (m_positionOfCaret == (long) BeginningOfLine(m_positionOfCaret));

        // If the cursor is part of the whitespace at the beginning of the line
        // we move it to its end if this makes sense.
        if (autoIndent)
        {
          int i = BeginningOfLine(m_positionOfCaret);
          while ((m_text[i] == wxT(' ')) && (i < m_positionOfCaret))
            ++i;
          if (i == m_positionOfCaret)
            while ((m_text[m_positionOfCaret] == wxT(' ')) && (m_positionOfCaret < (long) m_text.Length() - 1))
              ++m_positionOfCaret;
        }

        int indentChars = GetIndentDepth(m_text, m_positionOfCaret);

        // The string we indent with.
        wxString indentString;
        if (autoIndent && (indentChars > 0))
          for (int i = 0; i < indentChars; i++)
            indentString += wxT(" ");

        m_text = m_text.SubString(0, m_positionOfCaret - 1) +
                 wxT("\n") + indentString +
                 m_text.SubString(m_positionOfCaret, m_text.Length());
        m_positionOfCaret++;
        if ((indentChars > 0) && (autoIndent))
        {
          m_positionOfCaret = BeginningOfLine(m_positionOfCaret);
          m_positionOfCaret += indentChars;
        }
        m_isDirty = true;
        m_containsChanges = true;
        bool cursorJump = true;
        wxConfig::Get()->Read(wxT("cursorJump"), &cursorJump);

        if ((!cursorJump) || ((cursorAtStartOfLine) && (!autoIndent)))
          m_positionOfCaret = BeginningOfLine(m_positionOfCaret);
      }
}

bool EditorCell::HandleSpecialKey(wxKeyEvent &event)
{
  bool done = true;

  if ((event.GetKeyCode() != WXK_DOWN) &&
      (event.GetKeyCode() != WXK_PAGEDOWN) &&
      (event.GetKeyCode() != WXK_PAGEUP) &&
      #ifdef WXK_NUMPAD_PRIOR
      (event.GetKeyCode() != WXK_NUMPAD_PRIOR) &&
      #endif
      #ifdef WXK_PRIOR
      (event.GetKeyCode() != WXK_PRIOR) &&
      #endif
      #ifdef WXK_NEXT
      (event.GetKeyCode() != WXK_NEXT) &&
      #endif
      #ifdef WXK_NUMPAD_NEXT
      (event.GetKeyCode() != WXK_NUMPAD_NEXT) &&
      #endif
      (event.GetKeyCode() != WXK_UP)
          )
    m_caretColumn = -1; // make caretColumn invalid

  switch (event.GetKeyCode())
  {
    case WXK_LEFT:
      SaveValue();
      if (event.ShiftDown())
      {
        if (m_selectionStart == -1)
          SetSelection(m_positionOfCaret, m_positionOfCaret);
      }
      else
        ClearSelection();

      if (event.ControlDown())
      {
        int lastpos = m_positionOfCaret;

        while (
                (m_positionOfCaret > 0) &&
                (
                        wxIsalnum(m_text[m_positionOfCaret - 1]) ||
                        m_text[m_positionOfCaret - 1] == wxT('_') ||
                        ((m_positionOfCaret > 1) && (m_text[m_positionOfCaret - 2] == wxT('\\')))
                )
                )
        {
          if ((m_positionOfCaret > 1) && (m_text[m_positionOfCaret - 2] == wxT('\\')))
            m_positionOfCaret--;
          m_positionOfCaret--;
        }

        while ((m_positionOfCaret > 0) && (wxIsspace(m_text[m_positionOfCaret - 1])))
          m_positionOfCaret--;

        if ((lastpos == m_positionOfCaret) && (m_positionOfCaret > 0))
          m_positionOfCaret--;
      }
      else if (event.AltDown())
      {
        int count = 0;

        while (m_positionOfCaret > 0 && count >= 0)
        {
          m_positionOfCaret--;
          if (m_text[m_positionOfCaret] == '(' || m_text[m_positionOfCaret] == '[')
            count--;
          else if (m_text[m_positionOfCaret] == ')' || m_text[m_positionOfCaret] == ']')
            count++;
        }
      }
      else if (m_positionOfCaret > 0)
        m_positionOfCaret--;

      if (event.ShiftDown())
        SetSelection(m_selectionStart, m_positionOfCaret);

      break;

    case WXK_RIGHT:
      SaveValue();
      if (event.ShiftDown())
      {
        if (m_selectionStart == -1)
          SetSelection(m_positionOfCaret, m_positionOfCaret);
      }
      else
        ClearSelection();

      if (event.ControlDown())
      {
        int lastpos = m_positionOfCaret;

        while ((m_positionOfCaret < (long) m_text.Length()) &&
               (
                       wxIsalnum(m_text[m_positionOfCaret]) ||
                       m_text[m_positionOfCaret] == wxT('_') ||
                       m_text[m_positionOfCaret] == wxT('\\')
               )
                )
        {
          if (m_text[m_positionOfCaret] == wxT('\\'))
            m_positionOfCaret++;
          if (m_positionOfCaret < (long) m_text.Length())
            m_positionOfCaret++;
        }

        while ((m_positionOfCaret < (long) m_text.Length()) && (wxIsspace(m_text[m_positionOfCaret])))
          m_positionOfCaret++;

        if ((m_positionOfCaret < (long) m_text.Length()) && (lastpos == m_positionOfCaret))
          m_positionOfCaret++;
      }
      else if (event.AltDown())
      {
        int count = 0;

        while (m_positionOfCaret < (signed) m_text.Length() && count >= 0)
        {
          m_positionOfCaret++;
          if ((m_text[m_positionOfCaret - 1] == '(') || (m_text[m_positionOfCaret - 1] == '['))
            count++;
          else if ((m_text[m_positionOfCaret - 1] == ')') || (m_text[m_positionOfCaret - 1] == ']'))
            count--;
        }
      }

      else if (m_positionOfCaret < (signed) m_text.Length())
        m_positionOfCaret++;

      if (event.ShiftDown())
        SetSelection(m_selectionStart, m_positionOfCaret);

      break;

    case WXK_END:
      SaveValue();
      if (event.ShiftDown())
      {
        if (m_selectionStart == -1)
          m_selectionStart = m_positionOfCaret;
      }
      else
        ClearSelection();

      if (event.ControlDown())
        m_positionOfCaret = (signed) m_text.Length();
      else
      {
        while (m_positionOfCaret < (signed) m_text.Length() &&
               m_text.GetChar(m_positionOfCaret) != '\n' &&
               m_text.GetChar(m_positionOfCaret) != '\r')
          m_positionOfCaret++;
      }

      if (event.ShiftDown())
      {
        SetSelection(m_selectionStart, m_positionOfCaret);
      }
      break;

    case WXK_HOME:
      SaveValue();
      {
        if (event.ShiftDown())
        {
          if (m_selectionStart == -1)
            SetSelection(m_positionOfCaret, m_positionOfCaret);
        }
        else
          ClearSelection();

        if (event.ControlDown())
          m_positionOfCaret = 0;
        else
        {
          unsigned int col, lin;
          PositionToXY(m_positionOfCaret, &col, &lin);
          m_positionOfCaret = XYToPosition(0, lin);
        }

        if (event.ShiftDown())
          SetSelection(m_selectionStart, m_positionOfCaret);
      }
      break;

    case WXK_PAGEDOWN:
#ifdef WXK_NEXT
      case WXK_NEXT:
#endif
#ifdef WXK_NUMPAD_NEXT
      case WXK_NUMPAD_NEXT:
#endif
      SaveValue();
      {
        if (event.ShiftDown())
        {
          if (m_selectionStart == -1)
          {
            SetSelection(m_positionOfCaret, m_positionOfCaret);
            m_lastSelectionStart = m_positionOfCaret;
          }
        }
        else
          ClearSelection();
        unsigned int column;
        unsigned int line;
        PositionToXY(m_positionOfCaret, &column, &line); // get current line
        if (m_caretColumn > -1)
          column = m_caretColumn;
        else
          m_caretColumn = column;

        if (line < m_numberOfLines - 1) // can we go down ?
        {
          int scrolllength = (*m_configuration)->GetCanvasSize().y - m_charHeight;

          while ((line < m_numberOfLines - 1) && (scrolllength > 0))
          {
            line++;
            m_positionOfCaret = XYToPosition(column, line);
            scrolllength -= m_charHeight;
          }
        }
        else
        { // we can't go down. move caret to the end
          m_positionOfCaret = (signed) m_text.Length();
          m_caretColumn = -1; // make caretColumn invalid
        }

        if (event.ShiftDown())
          SetSelection(m_selectionStart, m_positionOfCaret);
      }
      break;

    case WXK_DOWN:
      SaveValue();
      {
        if (event.ShiftDown())
        {
          if (m_selectionStart == -1)
          {
            SetSelection(m_positionOfCaret, m_positionOfCaret);
            m_lastSelectionStart = m_positionOfCaret;
          }
        }
        else
          ClearSelection();
        unsigned int column, line;
        PositionToXY(m_positionOfCaret, &column, &line); // get current line
        if (m_caretColumn > -1)
          column = m_caretColumn;
        else
          m_caretColumn = column;

        if (line < m_numberOfLines - 1) // can we go down ?
          m_positionOfCaret = XYToPosition(column, line + 1);
        else
        { // we can't go down. move caret to the end
          m_positionOfCaret = (signed) m_text.Length();
          m_caretColumn = -1; // make caretColumn invalid
        }

        if (event.ShiftDown())
          SetSelection(m_selectionStart, m_positionOfCaret);
      }
      break;

    case WXK_PAGEUP:
#ifdef WXK_PRIOR
      case WXK_PRIOR:
#endif
#ifdef WXK_NUMPAD_PRIOR
      case WXK_NUMPAD_PRIOR:
#endif
      SaveValue();
      {
        if (event.ShiftDown())
        {
          if (m_selectionStart == -1)
          {
            SetSelection(m_positionOfCaret, m_positionOfCaret);
            m_lastSelectionStart = m_positionOfCaret;
          }
        }
        else
          ClearSelection();

        unsigned int column, line;
        PositionToXY(m_positionOfCaret, &column, &line); // get current line
        if (m_caretColumn > -1)
          column = m_caretColumn;
        else
          m_caretColumn = column;

        if (line > 0) // can we go up?
        {
          int scrolllength = (*m_configuration)->GetCanvasSize().y - m_charHeight;

          while ((line > 0) && (scrolllength > 0))
          {
            line--;
            m_positionOfCaret = XYToPosition(column, line);
            scrolllength -= m_charHeight;
          }
        }
        else
        { // we can't move up, move to the beginning
          m_positionOfCaret = 0;
          m_caretColumn = -1; // make caretColumn invalid
        }

        if (event.ShiftDown())
          SetSelection(m_selectionStart, m_positionOfCaret);
      }
      break;

    case WXK_UP:
      SaveValue();
      {
        if (event.ShiftDown())
        {
          if (m_selectionStart == -1)
          {
            SetSelection(m_positionOfCaret, m_positionOfCaret);
            m_lastSelectionStart = m_positionOfCaret;
          }
        }
        else
          ClearSelection();

        unsigned int column, line;
        PositionToXY(m_positionOfCaret, &column, &line); // get current line
        if (m_caretColumn > -1)
          column = m_caretColumn;
        else
          m_caretColumn = column;

        if (line > 0) // can we go up?
          m_positionOfCaret = XYToPosition(column, line - 1);
        else
        { // we can't move up, move to the beginning
          m_positionOfCaret = 0;
          m_caretColumn = -1; // make caretColumn invalid
        }

        if (event.ShiftDown())
          SetSelection(m_selectionStart, m_positionOfCaret);
      }
      break;

    case WXK_RETURN:
      SaveValue();
      ProcessNewline();
      StyleText();
      break;

    case WXK_DELETE:
      // On windows CMD+WXK_BACK is passed to us as CMD+WXK_DELETE.
      if (!event.CmdDown())
      {
        SaveValue();
        if (m_selectionStart == -1)
        {
          if (m_positionOfCaret < (signed) m_text.Length())
          {
            m_isDirty = true;
            m_containsChanges = true;
            m_text = m_text.SubString(0, m_positionOfCaret - 1) +
                     m_text.SubString(m_positionOfCaret + 1, m_text.Length());
          }
        }
        else
        {
          m_isDirty = true;
          m_containsChanges = true;
          SaveValue();
          m_saveValue = true;
          long start = wxMin(m_selectionEnd, m_selectionStart);
          long end = wxMax(m_selectionEnd, m_selectionStart);
          m_text = m_text.SubString(0, start - 1) +
                   m_text.SubString(end, m_text.Length());
          m_positionOfCaret = start;
          ClearSelection();
        }
      }
      else
      {
        // Ctrl+Backspace is pressed.

        m_containsChanges = true;
        m_isDirty = true;


        int lastpos = m_positionOfCaret;
        // Delete characters until the end of the current word or number
        while ((wxIsalnum(m_text[m_positionOfCaret - 1])) && (m_positionOfCaret > 0))
        {
          m_positionOfCaret--;
          m_text = m_text.SubString(0, m_positionOfCaret - 1) +
                   m_text.SubString(m_positionOfCaret + 1, m_text.Length());
        }
        // Delete Spaces, Tabs and Newlines until the next printable character
        while ((wxIsspace(m_text[m_positionOfCaret - 1])) && (m_positionOfCaret > 0))
        {
          m_positionOfCaret--;
          m_text = m_text.SubString(0, m_positionOfCaret - 1) +
                   m_text.SubString(m_positionOfCaret + 1, m_text.Length());
        }

        // If we didn't delete anything till now delete one single character.
        if (lastpos == m_positionOfCaret)
        {
          m_positionOfCaret--;
          m_text = m_text.SubString(0, m_positionOfCaret - 1) +
                   m_text.SubString(m_positionOfCaret + 1, m_text.Length());
        }
      }
      StyleText();
      break;

    case WXK_BACK:
      SaveValue();
      if (SelectionActive())
      {
        SaveValue();
        m_saveValue = true;
        m_containsChanges = true;
        m_isDirty = true;
        long start = wxMin(m_selectionEnd, m_selectionStart);
        long end = wxMax(m_selectionEnd, m_selectionStart);
        m_text = m_text.SubString(0, start - 1) +
                 m_text.SubString(end, m_text.Length());
        m_positionOfCaret = start;
        ClearSelection();
        StyleText();
        break;
      }
      else
      {
        if (!event.CmdDown())
        {
          // Backspace without Ctrl => Delete one character if there are characters to delete.
          if (m_positionOfCaret > 0)
          {
            m_containsChanges = true;
            m_isDirty = true;

            if (m_text.SubString(0, m_positionOfCaret - 1).Right(4) == wxT("    "))
            {
              m_text = m_text.SubString(0, m_positionOfCaret - 5) +
                       m_text.SubString(m_positionOfCaret, m_text.Length());
              m_positionOfCaret -= 4;
            }
            else
            {
              /// If deleting ( in () then delete both.
              int right = m_positionOfCaret;
              if (m_positionOfCaret < (long) m_text.Length() && (*m_configuration)->GetMatchParens() &&
                  ((m_text.GetChar(m_positionOfCaret - 1) == '[' && m_text.GetChar(m_positionOfCaret) == ']') ||
                   (m_text.GetChar(m_positionOfCaret - 1) == '(' && m_text.GetChar(m_positionOfCaret) == ')') ||
                   (m_text.GetChar(m_positionOfCaret - 1) == '{' && m_text.GetChar(m_positionOfCaret) == '}') ||
                   (m_text.GetChar(m_positionOfCaret - 1) == '"' && m_text.GetChar(m_positionOfCaret) == '"')))
                right++;
              m_text = m_text.SubString(0, m_positionOfCaret - 2) +
                       m_text.SubString(right, m_text.Length());
              m_positionOfCaret--;
            }
          }

        }
        else
        {
          // Ctrl+Backspace is pressed.

          m_containsChanges = true;
          m_isDirty = true;


          int lastpos = m_positionOfCaret;
          // Delete characters until the end of the current word or number
          while ((wxIsalnum(m_text[m_positionOfCaret - 1])) && (m_positionOfCaret > 0))
          {
            m_positionOfCaret--;
            m_text = m_text.SubString(0, m_positionOfCaret - 1) +
                     m_text.SubString(m_positionOfCaret + 1, m_text.Length());
          }
          // Delete Spaces, Tabs and Newlines until the next printable character
          while ((wxIsspace(m_text[m_positionOfCaret - 1])) && (m_positionOfCaret > 0))
          {
            m_positionOfCaret--;
            m_text = m_text.SubString(0, m_positionOfCaret - 1) +
                     m_text.SubString(m_positionOfCaret + 1, m_text.Length());
          }

          // If we didn't delete anything till now delete one single character.
          if (lastpos == m_positionOfCaret)
          {
            m_positionOfCaret--;
            m_text = m_text.SubString(0, m_positionOfCaret - 1) +
                     m_text.SubString(m_positionOfCaret + 1, m_text.Length());
          }
        }
      }
      StyleText();
      break;

    case WXK_TAB:
      m_isDirty = true;
      if (!FindNextTemplate(event.ShiftDown()))
      {
        m_containsChanges = true;
        {
          if (SelectionActive())
          {
            // Selection active and Tab
            SaveValue();

            long start = wxMin(m_selectionStart, m_selectionEnd);
            long end = wxMax(m_selectionStart, m_selectionEnd);
            long newLineIndex = wxMin(m_text.find(wxT('\n'), start), m_text.find(wxT('\r'), start));

            if (((newLineIndex != wxNOT_FOUND) && (newLineIndex < end)) ||
                (m_text.SubString(newLineIndex, start).Trim() == wxEmptyString)
                    )
            {
              start = BeginningOfLine(start);
              long pos = start;

              if ((m_text[end] == wxT('\n')))
                end++;

              if (end > (long) m_text.Length())
                end = m_text.Length();

              while (pos < end)
              {
                if (event.ShiftDown())
                {
                  for (int i = 0; i < 4; i++)
                    if (m_text[pos] == wxT(' '))
                    {
                      m_text =
                              m_text.SubString(0, pos - 1) +
                              m_text.SubString(pos + 1, m_text.Length());
                      if (end > 0)
                        end--;
                    }
                }
                else
                {
                  m_text =
                          m_text.SubString(0, pos - 1) +
                          wxT("    ") +
                          m_text.SubString(pos, m_text.Length());
                  end += 4;
                  pos += 4;
                }
                while ((pos < end) && (m_text[pos] != wxT('\n')) && (m_text[pos] != wxT('\r')))
                  pos++;
                if ((pos < end) && ((m_text[pos] == wxT('\n')) || (m_text[pos] == wxT('\r'))))
                  pos++;
              }
              SetSelection(start, end);
            }
            else
            {
              m_text = m_text.SubString(0, start - 1) +
                       m_text.SubString(end, m_text.Length());
              ClearSelection();
            }
            m_positionOfCaret = start;
            StyleText();
            break;
          }
          else
          {
            if (!event.ShiftDown())
            {
              // Selection active and Tab was pressed without Shift
              unsigned int col, line;
              PositionToXY(m_positionOfCaret, &col, &line);
              wxString ins;
              do
              {
                col++;
                ins += wxT(" ");
              } while (col % 4 != 0);

              m_text = m_text.SubString(0, m_positionOfCaret - 1) +
                       ins +
                       m_text.SubString(m_positionOfCaret, m_text.Length());
              m_positionOfCaret += ins.Length();
            }
            else
            {
              // Selection active and Shift+Tab
              long start = BeginningOfLine(m_positionOfCaret);
              if (m_text.SubString(start, start + 3) == wxT("    "))
              {
                m_text =
                        m_text.SubString(0, start - 1) +
                        m_text.SubString(start + 4, m_text.Length());
                if (m_positionOfCaret > start)
                {
                  m_positionOfCaret = start;
                  while ((m_positionOfCaret < (long) m_text.Length()) && (m_text[m_positionOfCaret] == wxT(' ')))
                    m_positionOfCaret++;
                }
              }
            }
          }
        }
      }
      StyleText();
      break;
/*
  case WXK_SPACE:
    if (event.ShiftDown())
      m_text = m_text.SubString(0, m_positionOfCaret - 1) + wxT("*") + // wxT("\x00B7")
               m_text.SubString(m_positionOfCaret, m_text.Length());
    else
      m_text = m_text.SubString(0, m_positionOfCaret - 1) + wxT(" ") +
               m_text.SubString(m_positionOfCaret, m_text.Length());
    m_isDirty = true;
    m_containsChanges = true;
    m_positionOfCaret++;
    break;
*/
    case WXK_ESCAPE:
      if (m_selectionStart != -1)
      {
        m_positionOfCaret = m_selectionEnd;
        ClearSelection();
      }
      else
      {
        bool insertescchar = false;
        int esccharpos = m_text.Left(m_positionOfCaret).Find(ESC_CHAR, true);
        if (esccharpos > -1)
        { // we have a match, check for insertion
          wxString greek = InterpretEscapeString(m_text.SubString(esccharpos + 1, m_positionOfCaret - 1));
          if (greek.Length() > 0)
          {
            m_text = m_text.SubString(0, esccharpos - 1) + greek +
                     m_text.SubString(m_positionOfCaret, m_text.Length());
            m_positionOfCaret = esccharpos + greek.Length();
            m_isDirty = true;
            m_containsChanges = true;
          }
          else
            insertescchar = true;
        }
        else
          insertescchar = true;

        if (insertescchar)
        {
          m_text = m_text.SubString(0, m_positionOfCaret - 1) + ESC_CHAR +
                   m_text.SubString(m_positionOfCaret, m_text.Length());
          m_isDirty = true;
          m_containsChanges = true;
          m_positionOfCaret++;
        }
      }
      StyleText();
      break;

      /* Ignored keys */
    case WXK_WINDOWS_LEFT:
    case WXK_WINDOWS_RIGHT:
    case WXK_WINDOWS_MENU:
    case WXK_COMMAND:
    case WXK_START:
      break;

    default:
      done = false;
      break;
  }

  return done;
}

bool EditorCell::HandleOrdinaryKey(wxKeyEvent &event)
{
  if (event.ControlDown() && !event.AltDown())
    return false;

  m_isDirty = true;
  m_containsChanges = true;
  bool insertLetter = true;

  if (m_saveValue)
  {
    SaveValue();
    m_saveValue = false;
  }


  wxChar keyCode;
  keyCode = event.GetUnicodeKey();

  // If we got passed a non-printable character we have to send it back to the
  // hotkey management.
  if (keyCode == WXK_NONE)
  {
    event.Skip();
    return false;
  }

  if (m_historyPosition != -1)
  {
    int len = m_textHistory.GetCount() - m_historyPosition;
    m_textHistory.RemoveAt(m_historyPosition + 1, len - 1);
    m_startHistory.erase(m_startHistory.begin() + m_historyPosition + 1, m_startHistory.end());
    m_endHistory.erase(m_endHistory.begin() + m_historyPosition + 1, m_endHistory.end());
    m_positionHistory.erase(m_positionHistory.begin() + m_historyPosition + 1, m_positionHistory.end());
    m_historyPosition = -1;
  }

  // if we have a selection either put parens around it (and don't write the letter afterwards)
  // or delete selection and write letter (insertLetter = true).
  if (m_selectionStart > -1)
  {
    SaveValue();
    long start = wxMin(m_selectionEnd, m_selectionStart);
    long end = wxMax(m_selectionEnd, m_selectionStart);

    switch (keyCode)
    {
      case '(':
        m_text = m_text.SubString(0, start - 1) + wxT("(") +
                 m_text.SubString(start, end - 1) + wxT(")") +
                 m_text.SubString(end, m_text.Length());
        m_positionOfCaret = start;
        insertLetter = false;
        break;
      case '\"':
        m_text = m_text.SubString(0, start - 1) + wxT("\"") +
                 m_text.SubString(start, end - 1) + wxT("\"") +
                 m_text.SubString(end, m_text.Length());
        m_positionOfCaret = start;
        insertLetter = false;
        break;
      case '{':
        m_text = m_text.SubString(0, start - 1) + wxT("{") +
                 m_text.SubString(start, end - 1) + wxT("}") +
                 m_text.SubString(end, m_text.Length());
        m_positionOfCaret = start;
        insertLetter = false;
        break;
      case '[':
        m_text = m_text.SubString(0, start - 1) + wxT("[") +
                 m_text.SubString(start, end - 1) + wxT("]") +
                 m_text.SubString(end, m_text.Length());
        m_positionOfCaret = start;
        insertLetter = false;
        break;
      case ')':
        m_text = m_text.SubString(0, start - 1) + wxT("(") +
                 m_text.SubString(start, end - 1) + wxT(")") +
                 m_text.SubString(end, m_text.Length());
        m_positionOfCaret = end + 2;
        insertLetter = false;
        break;
      case '}':
        m_text = m_text.SubString(0, start - 1) + wxT("{") +
                 m_text.SubString(start, end - 1) + wxT("}") +
                 m_text.SubString(end, m_text.Length());
        m_positionOfCaret = end + 2;
        insertLetter = false;
        break;
      case ']':
        m_text = m_text.SubString(0, start - 1) + wxT("[") +
                 m_text.SubString(start, end - 1) + wxT("]") +
                 m_text.SubString(end, m_text.Length());
        m_positionOfCaret = end + 2;
        insertLetter = false;
        break;
      default: // delete selection
        m_text = m_text.SubString(0, start - 1) +
                 m_text.SubString(end, m_text.Length());
        m_positionOfCaret = start;
        break;
    }
    ClearSelection();
    StyleText();
  } // end if (m_selectionStart > -1)

  // insert letter if we didn't insert brackets around selection
  if (insertLetter)
  {
    wxString chr;

    chr = event.GetUnicodeKey();

    if (event.ShiftDown())
      chr.Replace(wxT(" "), wxT("\xa0"));

    m_text = m_text.SubString(0, m_positionOfCaret - 1) +
             chr +
             m_text.SubString(m_positionOfCaret, m_text.Length());

    m_positionOfCaret++;

    if ((*m_configuration)->GetMatchParens())
    {
      switch (keyCode)
      {
        case '(':
          m_text = m_text.SubString(0, m_positionOfCaret - 1) +
                   wxT(")") +
                   m_text.SubString(m_positionOfCaret, m_text.Length());
          break;
        case '[':
          m_text = m_text.SubString(0, m_positionOfCaret - 1) +
                   wxT("]") +
                   m_text.SubString(m_positionOfCaret, m_text.Length());
          break;
        case '{':
          m_text = m_text.SubString(0, m_positionOfCaret - 1) +
                   wxT("}") +
                   m_text.SubString(m_positionOfCaret, m_text.Length());
          break;
        case '"':
          if (m_positionOfCaret < (long) m_text.Length() &&
              m_text.GetChar(m_positionOfCaret) == '"')
            m_text = m_text.SubString(0, m_positionOfCaret - 2) +
                     m_text.SubString(m_positionOfCaret, m_text.Length());
          else
            m_text = m_text.SubString(0, m_positionOfCaret - 1) +
                     wxT("\"") + m_text.SubString(m_positionOfCaret, m_text.Length());
          break;
        case ')': // jump over ')'
          if (m_positionOfCaret < (long) m_text.Length() &&
              m_text.GetChar(m_positionOfCaret) == ')')
            m_text = m_text.SubString(0, m_positionOfCaret - 2) +
                     m_text.SubString(m_positionOfCaret, m_text.Length());
          break;
        case ']': // jump over ']'
          if (m_positionOfCaret < (long) m_text.Length() &&
              m_text.GetChar(m_positionOfCaret) == ']')
            m_text = m_text.SubString(0, m_positionOfCaret - 2) +
                     m_text.SubString(m_positionOfCaret, m_text.Length());
          break;
        case '}': // jump over '}'
          if (m_positionOfCaret < (long) m_text.Length() &&
              m_text.GetChar(m_positionOfCaret) == '}')
            m_text = m_text.SubString(0, m_positionOfCaret - 2) +
                     m_text.SubString(m_positionOfCaret, m_text.Length());
          break;
        case '+':
          // case '-': // this could mean negative.
        case '*':
        case '/':
        case '^':
        case '=':
        case ',':
          size_t len = m_text.Length();
          if ((*m_configuration)->GetInsertAns())
          {
            // Insert an "%" before an operator that begins this cell
            if(len == 1 && m_positionOfCaret == 1)
            {
              m_text = m_text.SubString(0, m_positionOfCaret - 2) + wxT("%") +
                m_text.SubString(m_positionOfCaret - 1, m_text.Length());
              m_positionOfCaret += 1;
            }

            // If this operator happens to be the first letter of an comment start sign
            // we remove the "%" again as the unability to begin a code cell with a
            // comment in the obvious way tends to surprise users.
            if((len == 3) && (m_positionOfCaret == 3) && (m_text.StartsWith(wxT("%/*"))))
            {
              m_text = m_text.SubString(m_positionOfCaret - 2, m_text.Length());
              m_positionOfCaret -= 1;
            }

          }
          break;
      }
    }
  } // end if (insertLetter)

  StyleText();
  return true;
}

/**
 * For a given quotation mark ("), find a matching quote.
 * Since there are no nested quotes, an odd-numbered, non-escaped quote
 * is an opening quote, and an even-numbered non-escaped quote
 * is a closing quote.
 *
 * @return true if matching quotation marks were found; false otherwise
 */
bool EditorCell::FindMatchingQuotes()
{
  int pos = m_positionOfCaret;
  if (pos < 0)
  {
    m_paren1 = m_paren2 = -1;
    return false;
  }

  if(pos >= (long)m_text.Length())
    pos = m_text.Length()-1;
  if ((pos >= (long) m_text.Length() - 1)||
      (wxString(wxT("\"")).Find(m_text.GetChar(pos)) == -1))
  {
    pos--;
    if (pos < 0 ||
        wxString(wxT("\"")).Find(m_text.GetChar(pos)) == -1)
    {
      m_paren1 = m_paren2 = -1;
      return false;
    }
  }

  int count = 0;
  for (int i = 0; i < (int) m_text.Length(); ++i)
  {
    if (m_text.GetChar(i) == '"' &&
        ((i == 0) ||
         (i >= 1 && m_text.GetChar(i - 1) != '\\')))
    {
      ++count;
      if (count & 1)
      {
        m_paren1 = i;  // open quote here
      }
      else
      {
        m_paren2 = i;  // close quote here
        if (m_paren1 == pos || m_paren2 == pos)
        {
          // found the pair of quotes under the cursor
          return true;
        }
      }
    }
  }

  // didn't find matching quotes; do not highlight quotes
  m_paren1 = m_paren2 = -1;
  return false;
}

void EditorCell::FindMatchingParens()
{
  if (FindMatchingQuotes())
  {
    return;
  }

  m_paren2 = m_positionOfCaret;
  if (m_paren2 < 0)
  {
    m_paren1 = m_paren2 = -1;
    return;
  }

  if(m_paren2 >= (long)m_text.Length())
    m_paren2 = m_text.Length() - 1;
  if ((m_paren2 >= (long) m_text.Length())||
      (wxString(wxT("([{}])")).Find(m_text.GetChar(m_paren2)) == -1))
  {
    m_paren2--;
    if (m_paren2 < 0 ||
        wxString(wxT("([{}])")).Find(m_text.GetChar(m_paren2)) == -1)
    {
      m_paren1 = m_paren2 = -1;
      return;
    }
  }

  wxChar first = m_text.GetChar(m_paren2);
  wxChar second;
  int dir;

  switch (first)
  {
    case '(':
      second = ')';
      dir = 1;
      break;
    case '[':
      second = ']';
      dir = 1;
      break;
    case '{':
      second = '}';
      dir = 1;
      break;
    case ')':
      second = '(';
      dir = -1;
      break;
    case ']':
      second = '[';
      dir = -1;
      break;
    case '}':
      second = '{';
      dir = -1;
      break;
    default:
      return;
  }

  m_paren1 = m_paren2 + dir;
  int depth = 1;

  while (m_paren1 >= 0 && m_paren1 < (int) m_text.Length())
  {
    if (m_text.GetChar(m_paren1) == second)
      depth--;
    else if (m_text.GetChar(m_paren1) == first)
      depth++;

    if (depth == 0)
      break;
    m_paren1 += dir;
  }

  if (m_paren1 < 0 || m_paren1 >= (int) m_text.Length())
    m_paren1 = m_paren2 = -1;
}

wxString EditorCell::InterpretEscapeString(wxString txt)
{
  wxString retval = (*m_configuration)->m_escCodes[txt];

  if(retval == wxEmptyString)
  {
    long int unicodeval = -1;
    if (txt.ToLong(&unicodeval, 16))
    {
      if (unicodeval >= 32)
        retval = wxString(wxChar(unicodeval));
      else
        retval = wxT(" ");
    }
  }
  return retval;
}


void EditorCell::DeactivateCursor()
{
  if (m_cellPointers->m_activeCell != NULL)
  {
    dynamic_cast<EditorCell *>(m_cellPointers->m_activeCell)->ClearSelection();
    dynamic_cast<EditorCell *>(m_cellPointers->m_activeCell)->m_paren1 =
    dynamic_cast<EditorCell *>(m_cellPointers->m_activeCell)->m_paren2 = -1;
  }
  m_cellPointers->m_activeCell = NULL;
}

void EditorCell::ActivateCursor()
{
  if (m_cellPointers->m_activeCell != NULL)
    DeactivateCursor();

  SaveValue();
  m_displayCaret = true;
  m_hasFocus = true;
  m_cellPointers->m_activeCell = this;

  ClearSelection();
  m_paren1 = m_paren2 = -1;

  // upon activation unhide the parent groupcell
  m_firstLineOnly = false;
  dynamic_cast<GroupCell *>(GetGroup())->Hide(false);
  if (GetType() == MC_TYPE_INPUT)
    FindMatchingParens();
}

bool EditorCell::AddEnding()
{
  if (m_text.StartsWith(wxT(":lisp")))
    return false;

  wxString text;

  size_t index = 0;
  // Copy the text, but not comments or whitespace
  while (index < m_text.Length())
  {
    if (
            (m_text[index] != wxT(' ')) ||
            (m_text[index] != wxT('\t')) ||
            (m_text[index] != wxT('\n')) ||
            (m_text[index] != wxT('\r'))
            )
    {
      // Did we encounter a comment start?
      if (
              (index < m_text.Length() - 1) &&
              (m_text[index] == wxT('/')) &&
              (m_text[index + 1] == wxT('*'))
              )
      {
        // Comment start.
        //
        // Skip all text until the end of the comment
        while (index < m_text.Length())
        {
          if (
                  (index < m_text.Length() - 1) &&
                  (m_text[index] == wxT('*')) &&
                  (m_text[index + 1] == wxT('/'))
                  )
          {
            index++;
            break;
          }
          index++;
        }
      }
      else
        text += m_text[index];
    }
    index++;
  }
  text.Trim();

  bool endingNeeded = true;

  // Cells ending in ";" or in "$" don't require us to add an ending.
  if (text.EndsWith(wxT(";")))
    endingNeeded = false;
  if (text.EndsWith(wxT("$")))
    endingNeeded = false;

  // Cells ending in "(to-maxima)" (with optional spaces around the "to-maxima")
  // don't require us to add an ending, neither.
  if(text.EndsWith(wxT(")")))
  {
    text = text.SubString(0,text.Length()-2);
    text.Trim();
      if (text.EndsWith(wxT("to-maxima")))
        endingNeeded = false;
  }

  if(endingNeeded)
  {
    m_text += wxT(";");
    m_paren1 = m_paren2 = m_width = -1;
    StyleText();
    return true;
  }
  return false;
}

//
// lines and columns are counted from zero
// position of caret is pos if caret is just before the character
//   at position pos in m_text.
//
void EditorCell::PositionToXY(int position, unsigned int *x, unsigned int *y)
{
  int col = 0, lin = 0;
  int pos = 0;

  wxString::const_iterator it = m_text.begin();
  while ((pos < position) && (it < m_text.end()))
  {
    if ((*it == '\n') || (*it == '\r'))
    {
      col = 0;
      lin++;
    }
    else
      col++;

    ++it;
    ++pos;
  }

  *x = col;
  *y = lin;
}

int EditorCell::XYToPosition(int x, int y)
{
  int col = 0, lin = 0, pos = 0;

  wxString::const_iterator it = m_text.begin();
  while ((it < m_text.end()) && (lin < y))
  {
    if ((*it == '\n') || (*it == '\r'))
      lin++;

    ++it;++pos;
  }

  while ((it < m_text.end()) && (pos < (int) m_text.Length()) && (col < x))
  {
    if ((*it == '\n') || (*it == '\r'))
      break;
    ++pos;
    ++col;
    ++it;
  }

  return pos;
}

wxPoint EditorCell::PositionToPoint(int WXUNUSED(fontsize), int pos)
{
  SetFont();

  int x = m_currentPoint.x, y = m_currentPoint.y;
  if (x == -1 || y == -1)
    return wxPoint(-1, -1);

  int width;
  unsigned int cX, cY;

  if (pos < 0)
    pos = m_positionOfCaret;

  PositionToXY(pos, &cX, &cY);

  width = GetLineWidth(cY, cX);

  x += width;
  y += m_charHeight * cY;

  return wxPoint(x, y);
}

void EditorCell::SelectPointText(wxPoint &point)
{
  wxString s;
  SetFont();

  ClearSelection();
  wxPoint posInCell(point);

  wxASSERT_MSG(m_currentPoint.x >= 0, _("Bug: x position of cell is unknown!"));
  wxASSERT_MSG(m_currentPoint.y >= 0, _("Bug: y position of cell is unknown!"));
  posInCell -= m_currentPoint;
//  posInCell -= wxPoint(Scale_Px(m_fontSize), 2);
  posInCell.y -= m_center;

  int lin = posInCell.y / m_charHeight + 1;
  if (posInCell.y < 0)
    lin = 0;
  int width, height;
  int lineStart = XYToPosition(0, lin);
  m_positionOfCaret = lineStart;
  // Find the text snippet the line we search for begins with
  int currentLine = 1;
  int indentPixels = 0;
  std::vector<StyledText>::iterator textSnippet;
  for (textSnippet = m_styledText.begin();
       ((textSnippet != m_styledText.end()) && (currentLine <= lin)); ++textSnippet)
  {
      if ((textSnippet->GetText() == '\n') || (textSnippet->GetText() == '\r'))
      {
        indentPixels = textSnippet->GetIndentPixels();
        currentLine++;
      }
  }

  if (GetType() == MC_TYPE_INPUT)
  {
    // Code cell

    int xpos = 0;
    // Find the text snippet the cursor is in
    while ((textSnippet != m_styledText.end()) && (xpos < posInCell.x))
    {
      wxString txt = textSnippet->GetText();
      int firstCharWidth;
      (*m_configuration)->GetDC()->GetTextExtent(txt.Left(1), &firstCharWidth, &height);

       if((txt == wxT("\n")) || (txt == wxT("\r")))
         break;

       wxCoord width = 0, height = 0;
       (*m_configuration)->GetDC()->GetTextExtent(txt, &width, &height);
       if(xpos + width + firstCharWidth / 2 < posInCell.x)
       {
         xpos += width;
         m_positionOfCaret += txt.Length();
       }
       else
         break;

       ++textSnippet;
    }

    int lastwidth = 0;
    wxString snippet;
    if(textSnippet != m_styledText.end())
      snippet = textSnippet->GetText();

    (*m_configuration)->GetDC()->GetTextExtent(snippet.Left(1), &lastwidth, &height);
    lastwidth = -lastwidth;

    // Now determine which char inside this text snippet the cursor is at
    if(
      (snippet != wxT("\r")) &&
      (snippet != wxT("\n")))
    {
      for (unsigned int i = 0; i < snippet.Length(); i++)
      {
        (*m_configuration)->GetDC()->GetTextExtent(snippet.Left(i), &width, &height);
        if(xpos + width
           + (width - lastwidth)/2
           < posInCell.x)
          m_positionOfCaret++;
        else
          break;
        lastwidth = width;
      }
    }
    m_displayCaret = true;
    m_caretColumn = -1;
    FindMatchingParens();
    // The line that now follows is pure paranoia.
    m_positionOfCaret = wxMin(m_positionOfCaret, (signed) m_text.Length());
  }
  else
  {
    // Text cell

    wxString text = m_text;
    if ((*m_configuration)->GetChangeAsterisk())
    {
      text.Replace(wxT("*"), wxT("\xB7"));
      if (m_type == MC_TYPE_INPUT)
        text.Replace(wxT("-"), wxT("\x2212"));
    }

    // Handle indentation.
    posInCell.x -= indentPixels;

    while (m_positionOfCaret < (signed) text.Length() && text.GetChar(m_positionOfCaret) != '\n' &&
           text.GetChar(m_positionOfCaret) != '\r')
    {
      s = text.SubString(lineStart, m_positionOfCaret);
      (*m_configuration)->GetDC()->GetTextExtent(text.SubString(lineStart, m_positionOfCaret),
                                                 &width, &height);
      if (width > posInCell.x)
        break;

      m_positionOfCaret++;
    }
    m_positionOfCaret = wxMin(m_positionOfCaret, (signed) text.Length());


    m_displayCaret = true;
    m_caretColumn = -1;
  }
}

void EditorCell::SelectRectText(wxPoint &one, wxPoint &two)
{
  SelectPointText(one);
  long start = m_positionOfCaret;
  SelectPointText(two);
  SetSelection(start, m_positionOfCaret);
  m_paren2 = m_paren1 = -1;
  m_caretColumn = -1;
  if (m_selectionStart == m_selectionEnd)
  {
    ClearSelection();
  }
}

// IsPointInSelection
// Return true if coordinates "point" fall into selection
// If they don't or there is no selection it returns false
bool EditorCell::IsPointInSelection(wxPoint point)
{
  if ((m_selectionStart == -1) || (m_selectionEnd == -1) || !IsActive())
    return false;

  wxRect rect = GetRect();
  if (!rect.Contains(point))
    return false;

  wxString s;
  wxString text = m_text;
  if ((*m_configuration)->GetChangeAsterisk())
  {
    text.Replace(wxT("*"), wxT("\xB7"));
    if (m_type == MC_TYPE_INPUT)
      text.Replace(wxT("-"), wxT("\x2212"));
  }
  SetFont();
  // Determine the line the point would be in
  wxPoint posInCell(point);
  posInCell.x -= m_currentPoint.x - 2;
  posInCell.y -= m_currentPoint.y - 2 - m_center;
  unsigned int lin = posInCell.y / m_charHeight;
  int width, height;
  int lineStart = XYToPosition(0, lin);
  int positionOfCaret = lineStart;

  // Find the text snippet the line we search for begins with for determining
  // the indentation needed.
  unsigned int currentLine = 1;
  int indentPixels = 0;
  std::vector<StyledText>::iterator textSnippet;
  for (textSnippet = m_styledText.begin();
       ((textSnippet < m_styledText.end()) && (currentLine < lin)); ++textSnippet)
  {
    if ((textSnippet->GetText() == '\n') || (textSnippet->GetText() == '\r'))
    {
      indentPixels = textSnippet->GetIndentPixels();
      currentLine++;
    }
  }


  // Handle indentation
  posInCell.x -= indentPixels;

  while (positionOfCaret < (signed) text.Length() && text.GetChar(positionOfCaret) != '\n' &&
         text.GetChar(positionOfCaret) != '\r')
  {
    s = text.SubString(lineStart, positionOfCaret);
    (*m_configuration)->GetDC()->GetTextExtent(text.SubString(lineStart, positionOfCaret),
                                               &width, &height);
    if (width > posInCell.x)
      break;
    positionOfCaret++;
  }
  positionOfCaret = wxMin(positionOfCaret, (signed) text.Length());

  return !((m_selectionStart >= positionOfCaret) || (m_selectionEnd <= positionOfCaret));

}

wxString EditorCell::DivideAtCaret()
{
  wxString original = m_text;
  m_containsChanges = true;
  wxString newText = m_text.SubString(0, m_positionOfCaret - 1);

  // Remove an eventual newline from the end of the old cell
  // that would appear if the cell is divided at the beginning of a line.
  if (newText.Length() > 0)
  {
    // Search for the end of whitespace at the end of the new cell
    size_t whiteSpaceEnd = newText.Length() - 1;
    while ((whiteSpaceEnd < newText.Length()) &&
           (
                   (newText[whiteSpaceEnd] == wxT(' ')) ||
                   (newText[whiteSpaceEnd] == wxT('\t'))
           )
            )
      whiteSpaceEnd++;

    if ((newText[whiteSpaceEnd] == wxT('\n')) || (newText[whiteSpaceEnd] == wxT('\r')))
      newText = newText.SubString(0, whiteSpaceEnd - 1);
  }

  SetValue(newText);
  ResetSize();
  GetGroup()->ResetSize();
  wxString retval = original.SubString(m_positionOfCaret, original.Length());
  // Remove an eventual newline from the beginning of a new cell
  // that would appear if the cell is divided at the end of a line.
  if (retval.Length() > 0)
  {
    // Search for the end of whitespace at the beginning of the new cell
    size_t whiteSpaceEnd = 0;
    while ((whiteSpaceEnd < retval.Length()) &&
           (
                   (retval[whiteSpaceEnd] == wxT(' ')) ||
                   (retval[whiteSpaceEnd] == wxT('\t'))
           )
            )
      whiteSpaceEnd++;

    if ((retval[whiteSpaceEnd] == wxT('\n')) || (retval[whiteSpaceEnd] == wxT('\r')))
      retval = retval.SubString(whiteSpaceEnd + 1, retval.Length());
  }
  return retval;
}


void EditorCell::SetSelection(int start, int end)
{
  if ((start != m_oldSelectionStart) || (end != m_oldSelectionEnd))
  {
    m_oldSelectionStart = start;
    m_oldSelectionEnd = end;
    m_selectionChanged = true;
    m_selectionStart = start;
    m_positionOfCaret = m_selectionEnd = end;
    if (m_selectionStart == -1 || m_selectionEnd == -1)
      m_cellPointers->m_selectionString = wxEmptyString;
    else
      m_cellPointers->m_selectionString = m_text.SubString(
              wxMin(m_selectionStart, m_selectionEnd),
              wxMax(m_selectionStart, m_selectionEnd) - 1
      );
    m_cellPointers->m_selectionString.Replace(wxT('\r'), wxT(' '));
  }
}

void EditorCell::CommentSelection()
{
  if ((m_selectionStart == -1) || (m_selectionEnd == -1))
    return;
  m_containsChanges = true;
  m_isDirty = true;
  SetValue(m_text.SubString(0, m_selectionStart - 1) + wxT("/*")
           + m_text.SubString(m_selectionStart, m_selectionEnd - 1) + wxT("*/")
           + m_text.SubString(m_selectionEnd, m_text.Length()));
  m_positionOfCaret = wxMin(m_selectionEnd + 4, (signed long) m_text.Length());
  ClearSelection();
}

wxString EditorCell::GetWordUnderCaret()
{
  long start = m_positionOfCaret;
  if(start < 0)
    return wxEmptyString;
  if(start >= m_text.Length())
    start = m_text.Length();

  wxString retval;
  long pos = 0;
  for (wxString::iterator it = m_text.begin(); it != m_text.end(); ++it)
  {
    if(*it == '\\')
    {
      *it++;
      if(it != m_text.end())
      {
        retval += *it;
        pos++;   
      }
    }        

    if(!wxIsalnum(*it) && !(*it == '\\') && !(*it == '_') && !(*it == '&'))
    {
      if(pos >= start)
        return retval;
      else
        retval = wxEmptyString;
    }
    else
      retval += *it;

    pos++;   
  }
  return retval;
}
    
/***
 * SelectWordUnderCaret
 * - called from MathCtrl::OnDoubleClick, MathCtrl::Autocomplete and wxMaxima::HelpMenu
 * Selects word under cursor (aA-zZ, 0-9, %, _, count) or
 * the inside of brackets using m_paren1 and m_paren2 if available and selectParens is true.
 * Returns the selected string if selected a word successfully - used for F1 help and
 * MathCtrl::Autocomplete.
 */
wxString EditorCell::SelectWordUnderCaret(bool WXUNUSED(selectParens), bool toRight, bool includeDoubleQuotes)
{
  if(m_positionOfCaret < 0)
    return wxEmptyString;
  
  long start = 0;
  long pos = 0;
  for (wxString::iterator it = m_text.begin(); it != m_text.end(); ++it)
  {
    if(*it == '\\')
    {
      pos++;
      if(it != m_text.end())
      {
        *it++;
        pos++;
      }
      continue;
    }        
    if(!wxIsalnum(*it) && !(*it == '\\') && !(*it == '_') && !(*it == '_') &&
       !((*it == '\"') && includeDoubleQuotes))
    {
      // !!toRight is 0, if toRight is false or guaranteed to be 1, if toRight is true
      if(pos >= m_positionOfCaret + !!toRight)
        break;
      else
        start = pos + 1;   
    }
    pos++;   
  }
  if(pos > 0)
    SetSelection(start, pos);
  m_positionOfCaret = pos;
  
  if (left != right)
    return m_cellPointers->m_selectionString;
  else
    return wxString(wxT("%"));
  
}

bool EditorCell::CopyToClipboard()
{
  if (m_selectionStart == -1)
    return false;
  wxASSERT_MSG(!wxTheClipboard->IsOpened(),_("Bug: The clipboard is already opened"));
  if (wxTheClipboard->Open())
  {
    long start = wxMin(m_selectionStart, m_selectionEnd);
    long end = wxMax(m_selectionStart, m_selectionEnd) - 1;
    wxString s = m_text.SubString(start, end);

    // For some reason wxMaxima sometimes hangs when putting string on the
    // clipboard. Also Valgrind tells me that if I don't add a null byte to my string
    // one byte too much is accessed.
    //
    // Another hope is that using a wxDataObjectComposite uses a different code path:
    // Valgrind tells me that the clipboard uses an uninitialized 64 bit value
    // in this case when using a 64 bit linux box instead.
    wxDataObjectComposite *data = new wxDataObjectComposite;
    data->Add(new wxTextDataObject(s + wxT('\0')));
    wxTheClipboard->SetData(data);
    wxTheClipboard->Close();
  }
  return true;
}

bool EditorCell::CutToClipboard()
{
  if (m_selectionStart == -1)
    return false;

  SaveValue();
  m_saveValue = true;
  m_containsChanges = true;
  CopyToClipboard();

  long start = wxMin(m_selectionStart, m_selectionEnd);
  long end = wxMax(m_selectionStart, m_selectionEnd);
  m_positionOfCaret = start;

  // We cannot use SetValue() here, since SetValue() tends to move the cursor.
  m_text = m_text.SubString(0, start - 1) +
           m_text.SubString(end, m_text.Length());
  StyleText();

  ClearSelection();
  m_paren1 = m_paren2 = -1;
  m_width = m_height = m_maxDrop = m_center = -1;

  return true;
}

void EditorCell::InsertText(wxString text)
{
  SaveValue();
  m_saveValue = true;
  m_containsChanges = true;

  if (!SelectionActive())
    SetSelection(m_positionOfCaret, m_positionOfCaret);

  text = TabExpand(text, m_positionOfCaret - BeginningOfLine(m_positionOfCaret));

  ReplaceSelection(
          GetSelectionString(),
          text
  );

  if (GetType() == MC_TYPE_INPUT)
    FindMatchingParens();

//  m_width = m_height = m_maxDrop = m_center = -1;
  StyleText();
}

void EditorCell::PasteFromClipboard(bool primary)
{
  if (primary)
    wxTheClipboard->UsePrimarySelection(true);
  wxASSERT_MSG(wxTheClipboard->IsOpened(),_("Bug: The clipboard isn't open on pasting into an editor cell"));
  if (wxTheClipboard->IsSupported(wxDF_TEXT))
  {
    wxTextDataObject obj;
    wxTheClipboard->GetData(obj);
    InsertText(obj.GetText());
    m_containsChanges = true;
    StyleText();
  }
  if (primary)
    wxTheClipboard->UsePrimarySelection(false);
}

int EditorCell::GetLineWidth(unsigned int line, int pos)
{
  // Find the text snippet the line we search for begins with for determining
  // the indentation needed.
  unsigned int currentLine = 1;
  int indentPixels = 0;
  std::vector<StyledText>::iterator textSnippet;
  for (textSnippet = m_styledText.begin();
       ((textSnippet < m_styledText.end()) && (currentLine <= line)); ++textSnippet)
  {
    if ((textSnippet->GetText() == '\n') || (textSnippet->GetText() == '\r'))
    {
      indentPixels = textSnippet->GetIndentPixels();
      currentLine++;
    }
  }

  if (pos == 0)
  {
    return indentPixels;
  }

  unsigned int i = 0;

  for (textSnippet = m_styledText.begin(); (textSnippet < m_styledText.end()) && (i < line); ++textSnippet)
  {
    wxString text = textSnippet->GetText();
    if ((text.Right(1) == '\n') || (text.Right(1) == '\r'))
      i++;
  }

  if (i < line)
    return 0;

  SetFont();
  int width = 0;
  wxString text;
  int textWidth = 0, textHeight = 0;
  pos--;
  for (; (textSnippet < m_styledText.end()) && (pos >= 0); ++textSnippet)
  {
    text = textSnippet->GetText();
    (*m_configuration)->GetDC()->GetTextExtent(text, &textWidth, &textHeight);
    width += textWidth;
    pos -= text.Length();
  }

  if (pos < 0)
  {
    width -= textWidth;
    (*m_configuration)->GetDC()->GetTextExtent(text.SubString(0, text.Length() + pos), &textWidth, &textHeight);
    width += textWidth;
  }

  // Handle indentation
  width += indentPixels;

  return width;
}


bool EditorCell::CanUndo()
{
  return m_textHistory.GetCount() > 0 && m_historyPosition != 0;
}

void EditorCell::Undo()
{
  if (m_historyPosition == -1)
  {
    m_historyPosition = m_textHistory.GetCount() - 1;
    m_textHistory.Add(m_text);
    m_startHistory.push_back(m_selectionStart);
    m_endHistory.push_back(m_selectionEnd);
    m_positionHistory.push_back(m_positionOfCaret);
  }
  else
    m_historyPosition--;

  if (m_historyPosition == -1)
    return;

  // We cannot use SetValue() here, since SetValue() tends to move the cursor.
  m_text = m_textHistory.Item(m_historyPosition);
  StyleText();

  m_positionOfCaret = m_positionHistory[m_historyPosition];
  SetSelection(m_startHistory[m_historyPosition], m_endHistory[m_historyPosition]);

  m_paren1 = m_paren2 = -1;
  m_isDirty = true;
  m_width = m_height = m_maxDrop = m_center = -1;
}


bool EditorCell::CanRedo()
{
  return m_textHistory.GetCount() > 0 &&
         m_historyPosition >= 0 &&
         m_historyPosition < ((long) m_textHistory.GetCount()) - 1;
}

void EditorCell::Redo()
{
  if (m_historyPosition == -1)
    return;

  m_historyPosition++;

  if (m_historyPosition >= (long) m_textHistory.GetCount())
    return;

  // We cannot use SetValue() here, since SetValue() tends to move the cursor.
  m_text = m_textHistory.Item(m_historyPosition);
  StyleText();

  m_positionOfCaret = m_positionHistory[m_historyPosition];
  SetSelection(m_startHistory[m_historyPosition], m_endHistory[m_historyPosition]);

  m_paren1 = m_paren2 = -1;
  m_isDirty = true;
  m_width = m_height = m_maxDrop = m_center = -1;
}


void EditorCell::SaveValue()
{
  if (m_textHistory.GetCount() > 0)
  {
    if (m_textHistory.Last() == m_text)
      return;
  }

  if (m_historyPosition != -1)
  {
    int len = m_textHistory.GetCount() - m_historyPosition;
    m_textHistory.RemoveAt(m_historyPosition, len);
    m_startHistory.erase(m_startHistory.begin() + m_historyPosition, m_startHistory.end());
    m_endHistory.erase(m_endHistory.begin() + m_historyPosition, m_endHistory.end());
    m_positionHistory.erase(m_positionHistory.begin() + m_historyPosition, m_positionHistory.end());
  }

  m_textHistory.Add(m_text);
  m_startHistory.push_back(m_selectionStart);
  m_endHistory.push_back(m_selectionEnd);
  m_positionHistory.push_back(m_positionOfCaret);
  m_historyPosition = -1;
}

void EditorCell::ClearUndo()
{
  m_textHistory.Clear();
  m_startHistory.clear();
  m_endHistory.clear();
  m_positionHistory.clear();
  m_historyPosition = -1;
}

void EditorCell::HandleSoftLineBreaks_Code(StyledText *&lastSpace, int &lineWidth, const wxString &token,
                                           unsigned int charInCell, wxString &text, size_t &lastSpacePos,
                                           int &indentationPixels)
{
  // If we don't want to autowrap code we don't do nothing here.
  if (!(*m_configuration)->GetAutoWrapCode())
    return;

  // If this token contains spaces and is followed by a space we will do the line break
  // in the next token.
  if ((charInCell + 1 < text.Length()) && (token.StartsWith(wxT(" "))) && (text[charInCell + 1] == ' '))
    return;

  SetFont();

  int width, height;
  //  Does the line extend too much to the right to fit on the screen /
  //   // to be easy to read?
  Configuration *configuration = (*m_configuration);
  configuration->GetDC()->GetTextExtent(token, &width, &height);
  lineWidth += width;

  // Normally the cell begins at the x position m_currentPoint.x - but sometimes
  // m_currentPoint is 0 so we need to determine our own value for the x position.
  int xmargin = Scale_Px(configuration->GetLabelWidth() +
                         configuration->GetCellBracketWidth());

  if (
          (lineWidth + xmargin + indentationPixels >= configuration->GetLineWidth()) &&
          (lastSpace != NULL) && (lastSpace->GetText() != "\r"))
  {
    int charWidth;
    configuration->GetDC()->GetTextExtent(wxT(" "), &charWidth, &height);
    indentationPixels = charWidth * GetIndentDepth(m_text, lastSpacePos);
    lineWidth = width + indentationPixels;
    lastSpace->SetText("\r");
    lastSpace->SetIndentation(indentationPixels);
    text[lastSpacePos] = '\r';
    lastSpace = NULL;
  }
}

void EditorCell::StyleTextCode()
{
  Configuration *configuration = (*m_configuration);

  // We have to style code
  StyledText *lastSpace = NULL;
  size_t lastSpacePos = 0;
  // If a space is part of the initial spaces that do the indentation of a cell it is
  // not eligible for soft line breaks: It would add a soft line break that causes
  // the same indentation to be introduced in the new line again and therefore would not
  // help at all.
  int indentationPixels = 0;
  wxString textToStyle = m_text;
  SetFont();
  if (configuration->GetChangeAsterisk())
  {
    textToStyle.Replace(wxT("*"), wxT("\xB7"));
    if (m_type == MC_TYPE_INPUT)
      textToStyle.Replace(wxT("-"), wxT("\x2212"));
  }

  // Handle folding of EditorCells
  if (m_firstLineOnly)
  {
    long newlinepos = textToStyle.Find(wxT("\n"));
    if (newlinepos != wxNOT_FOUND)
    {
      int lines = textToStyle.Freq(wxT('\n'));
      if(lines > 1)
        textToStyle = textToStyle.Left(newlinepos) +
          wxString::Format(_(" ... + %i hidden lines"), lines);
      else
        textToStyle = textToStyle.Left(newlinepos) +
          _(" ... + 1 hidden line");
    }
  }

  // Split the line into commands, numbers etc.
  MaximaTokenizer::TokenList tokens = MaximaTokenizer(textToStyle).GetTokens();

  // Now handle the text pieces one by one
  wxString lastTokenWithText;
  int pos = 0;
  int lineWidth = 0;
  MaximaTokenizer::Token token;

  for(MaximaTokenizer::TokenList::iterator it = tokens.begin(); it != tokens.end(); ++it)
  {
    pos += token.GetText().Length();
    token = *(*it);
    if (token.GetText().Length() < 1)
      continue;
    wxChar Ch = token.GetText()[0];
    
    // Handle Spaces
    if (Ch == wxT(' '))
    {
      // All spaces except the last one (that could cause a line break)
      // share the same token
      if (token.GetText().Length() > 1)
        m_styledText.push_back(StyledText(token.GetText().Right(token.GetText().Length()-1)));
      
      // Now we push the last space to the list of tokens and remember this
      // space as the space that potentially serves as the next point to
      // introduce a soft line break.
      m_styledText.push_back(StyledText(wxT(" ")));
      if (!m_styledText.empty())
      {
        lastSpace = &m_styledText.back();
        lastSpacePos = pos + token.GetText().Length() - 1;
      }
      else
      {
        lastSpace = NULL;
        lastSpacePos = -1;
      }
      
      continue;
    }
    else
      // Most things can contain Newlines - that we want as separate tokens
    {
      wxString txt = token.GetText();
      wxString line;      
      for (wxString::iterator it2 = txt.begin(); it2 < txt.end(); ++it2)
      {
        if(*it2 != '\n')
          line +=wxString(*it2);
        else
        {
          if(line != wxEmptyString)
            m_styledText.push_back(StyledText(token.GetStyle(), line));
          m_styledText.push_back(StyledText(token.GetStyle(), "\n"));
          line = wxEmptyString;
        }
      }
      if(line != wxEmptyString)
        m_styledText.push_back(StyledText(token.GetStyle(), line));
      HandleSoftLineBreaks_Code(lastSpace, lineWidth, token, pos, m_text, lastSpacePos,
                                indentationPixels);
      continue;
    }
    // End of a command
    if (token.GetStyle() == TS_CODE_ENDOFLINE)
    {
      m_styledText.push_back(StyledText(TS_CODE_ENDOFLINE, token));
      continue;
    }
    
    // Numbers
    if (token.GetStyle() == TS_CODE_NUMBER)
    {
      m_styledText.push_back(StyledText(TS_CODE_NUMBER, token));
      HandleSoftLineBreaks_Code(lastSpace, lineWidth, token, pos, m_text, lastSpacePos,
                                indentationPixels);
      if ((token.GetStyle() == TS_CODE_VARIABLE) || (token.GetStyle() == TS_CODE_FUNCTION))
        m_wordList.Add(token);
      continue;
    }
  }
  m_wordList.Sort();
}

void EditorCell::StyleTextTexts()
{
  Configuration *configuration = (*m_configuration);

  // Normally the cell begins at the x position m_currentPoint.x - but sometimes
  // m_currentPoint is 0 so we need to determine our own value for the x position.
  int xmargin =
  Scale_Px(configuration->GetLabelWidth()) +
  configuration->GetCellBracketWidth();

  // Remove all bullets of item lists as we will introduce them again in the next
  // step, as well.
  m_text.Replace(wxT("\x2022"), wxT("*"));

  // Insert new soft line breaks where we hit the right border of the worksheet, if
  // this has been requested in the config dialogue
  if (configuration->GetAutoWrap())
  {
    SetFont();
    wxString line;
    int lastSpacePos = -1;
    wxString::const_iterator lastSpaceIt;
    int indentation = 0;
    int lastLineStart = 0;
    int width, height;

    // Is this a new line - or the remainder of the line after a soft break?
    bool newLine = true;
    std::list<wxString> prefixes;
    std::list<int> indentPixels;
    wxString indentChar;

    unsigned int i = 0;
    wxString::const_iterator it = m_text.begin();
    while (it < m_text.end())
    {
      // Extract a line inserting a soft linebreak if necessary
      while (it < m_text.end())
      {
        wxString::const_iterator nextChar(it);
        ++nextChar;
        // Handle hard linebreaks or indent a soft linebreak if necessary
        if ((*it == '\n') || (nextChar >= m_text.end()))
        {
          // Can we introduce a soft line break?
          // One of the next questions will be: Do we need to?
          if (lastSpacePos >= 0)
          {
            // How far has the current line to be indented?
            if ((!indentPixels.empty()) && (!newLine))
              indentation = indentPixels.back();
            else
              indentation = 0;

            // How long is the current line already?
            configuration->GetDC()->GetTextExtent(
                                                 m_text.SubString(lastLineStart, i),
                                                 &width, &height);
            // Do we need to introduce a soft line break?
            if (width + xmargin + indentation >= configuration->GetLineWidth())
            {
              // We need a line break in front of the last space
              m_text[lastSpacePos] = wxT('\r');
              line = m_text.SubString(lastLineStart, lastSpacePos - 1);
              i = lastSpacePos;
              it = lastSpaceIt;
              lastLineStart = i + 1;
              lastSpacePos = -1;
              break;
            }
          }
          if ((*it == '\n') || (*it == '\r'))
          {
            if (i > 0)
              line = m_text.SubString(lastLineStart, i - 1);
            else
              line = wxEmptyString;
          }
          else
            line = m_text.SubString(lastLineStart, i);

          lastLineStart = i + 1;
          lastSpacePos = -1;
          break;
        }
        else
        {
          // We cannot introduce soft linebreaks since there were no spaces we
          // could break at.
          //
          // TODO: If we handled spaces before we handled soft line breaks this
          // branch would be unnecessary, right?

          // Spaces, newlines and reaching the end of the text all trigger
          // auto-wrapping
          if ((*it == ' ') || (*it == '\n') || (nextChar >= m_text.end()))
          {
            // Determine the current line's length
            configuration->GetDC()->GetTextExtent(m_text.SubString(lastLineStart, i), &width, &height);
            // Determine the current indentation
            if ((!indentPixels.empty()) && (!newLine))
              indentation = indentPixels.back();
            else
              indentation = 0;

            // Does the line extend too much to the right to fit on the screen /
            // to be easy to read?
            if (width + m_currentPoint.x + indentation >= configuration->GetLineWidth())
            {
              // We need a line break. Does the current line contain a space we can
              // break the line at?
              if (lastSpacePos >= 0)
              {
                // Introduce a soft line break
                m_text[lastSpacePos] = wxT('\r');
                line = m_text.SubString(lastLineStart, lastSpacePos - 1);
                i = lastSpacePos + 1;
                it = lastSpaceIt;
                ++it;
                lastLineStart = i;
                lastSpacePos = -1;
                break;
              }
              else
              {
                if (*it == wxT(' '))
                {
                  m_text[i] = wxT('\r');
                  line = m_text.SubString(lastLineStart, i - 1);
                  lastLineStart = i + 1;
                  lastSpacePos = -1;
                  break;
                }
              }
            }
          }
        }

        // Remember the current space as a point we potentially can break lines at
        if (*it == ' ')
        {
          lastSpacePos = i;
          lastSpaceIt = it;
        }

        // Go to the next character if we actually had a string in front of this
        // newline.
        if ((i > 0) || (*it != wxT('\n')))
        {
          ++it;
          ++i;
        }
      }

      // If this is the last line of the text we still need to extract it.
      if (i == m_text.Length())
        line = m_text.SubString(lastLineStart, i - 1);

      // If we fold the cell we only show the first line of text.
      if (m_firstLineOnly)
      {
        m_styledText.push_back(
                               StyledText(
                                          line +
                                          wxString::Format(_(" ... + %i hidden lines"), m_text.Freq(wxT('\n')))
                                          )
                               );
        line = wxEmptyString;
        break;
      }


      // Determine how much which line has to be indented for bullet lists
      // or citations

      // Handle the start of new lines
      if (newLine)
      {
        // Let's see if the line begins with a "begin indenting" marker:
        wxString line_trimmed(line);
        line_trimmed.Trim(false);
        if (
            (line_trimmed.StartsWith(wxT("* "))) ||
            (line_trimmed.StartsWith(wxT("\x2022 "))) ||
            (line_trimmed.StartsWith(wxT("\xB7 "))) ||
            (line_trimmed.StartsWith(wxT("> ")))
            )
        {
          // An "begin indenting" marker

          // Remember what a line that is part of this indentation level has to
          // begin with
          int width, height;

          indentChar = line.Left(line.Length() - line_trimmed.Length() + 2);

          // Remember how far to indent subsequent lines
          (*m_configuration)->GetDC()->GetTextExtent(indentChar, &width, &height);

          // Every line of a Quote begins with a ">":
          if (!line_trimmed.StartsWith(wxT("> ")))
            indentChar = wxEmptyString;

          // Equip bullet lists with real bullets
          if (line_trimmed.StartsWith(wxT("* ")))
            line[line.find("*")] = wxT('\x2022');
          if (line_trimmed.StartsWith(wxT("\xB7 ")))
            line[line.find("\xB7")] = wxT('\x2022');

          // Remember what a continuation for this indenting object would begin with
          prefixes.push_back(wxT("  ") + line.Left(line.Length() - line_trimmed.Length()));
          indentPixels.push_back(width);
        }
        else
        {
          // No "begin indenting" marker => Let's see if this is a continuation
          // of a indentation
          if (!prefixes.empty())
          {
            while (!prefixes.empty())
            {
              if (line.StartsWith(prefixes.back()))
                break;
              prefixes.pop_back();
              indentPixels.pop_back();
            }
          }
          // We don't need indentation as this line was indented
          // by spaces already.
        }
      }

      if (prefixes.empty())
        indentChar = wxEmptyString;

      int indentation;
      if ((!indentPixels.empty()) && (!newLine))
        indentation = indentPixels.back();
      else
        indentation = 0;

      // Equip the last soft linebreak with indentation.
      if (m_styledText.size() > 0)
      {
        if (m_styledText.back().GetText() == wxT("\r"))
          m_styledText.back().SetIndentation(indentation);
      }
      // Store the indented line in the list of styled text snippets
      m_styledText.push_back(StyledText(line, 0, indentChar));

      if(it < m_text.end())
      {

        // If the cell doesn't end with the last char of this line we have to
        // add a line ending to the list of styled text snippets
        if ((i + 1 < m_text.Length()) || (m_text[i] == wxT('\n')))
        {
          // Store the line ending in the list of styled text snippets
          if (*it == wxT('\n'))
            m_styledText.push_back(StyledText(wxT("\n"), 0, indentChar));
          else
            m_styledText.push_back(StyledText(wxT("\r"), 0, indentChar));
        }
      }

      // Is this a real new line of comment - or did we insert a soft linebreak?
      newLine = ((i + 1 >= m_text.Length()) || (*it == wxT('\n')));

      ++i;
      ++it;
    } // The loop that loops over all lines
  } // Do we want to autowrap lines?
  else
  {
    m_text.Replace(wxT("\r"),wxT("\n"));
    wxStringTokenizer lines(m_text, wxT("\n"), wxTOKEN_RET_EMPTY_ALL);
    while(lines.HasMoreTokens())
    {
      wxString line = lines.GetNextToken();
      if (m_firstLineOnly)
      {
        m_styledText.push_back(
          StyledText(
            line +
            wxString::Format(
              _(" ... + %i hidden lines"), m_text.Freq(wxT('\n'))),0,wxEmptyString)
          );
        break;
      }

      m_styledText.push_back(StyledText(line, 0, wxEmptyString));
      if((lines.HasMoreTokens()))
        m_styledText.push_back(StyledText(wxT("\n"), 0, wxEmptyString));
    }
  }
  ResetSize();
} // Style text, not code?

void EditorCell::StyleText()
{
  // We will need to determine the width of text and therefore need to set
  // the font type and size.
  Configuration *configuration = (*m_configuration);
  SetFont();


  m_wordList.Clear();
  m_styledText.clear();

  if(m_text == wxEmptyString)
    return;

  // Remove all soft line breaks. They will be re-added in the right places
  // in the next step
  m_text.Replace(wxT("\r"), wxT(" "));
  // Do we need to style code or text?
  if (m_type == MC_TYPE_INPUT)
    StyleTextCode();
  else
    StyleTextTexts();
}


void EditorCell::SetValue(const wxString &text)
{
  if (m_type == MC_TYPE_INPUT)
  {
    if ((*m_configuration)->GetMatchParens())
    {
      if (text == wxT("("))
      {
        m_text = wxT("()");
        m_positionOfCaret = 1;
      }
      else if (text == wxT("["))
      {
        m_text = wxT("[]");
        m_positionOfCaret = 1;
      }
      else if (text == wxT("{"))
      {
        m_text = wxT("{}");
        m_positionOfCaret = 1;
      }
      else if (text == wxT("\""))
      {
        m_text = wxT("\"\"");
        m_positionOfCaret = 1;
      }
      else
      {
        m_text = text;
        m_positionOfCaret = m_text.Length() ;
      }
    }
    else
    {
      m_text = text;
      m_positionOfCaret = m_text.Length() ;
    }

    if ((*m_configuration)->GetInsertAns())
    {
      if (m_text == wxT("+") ||
          m_text == wxT("*") ||
          m_text == wxT("/") ||
          m_text == wxT("^") ||
          m_text == wxT("=") ||
          m_text == wxT(","))
      {
        m_text = wxT("%") + m_text;
        m_positionOfCaret = m_text.Length() ;
      }
    }
  }
  else
  {
    m_text = text;
    m_positionOfCaret = m_text.Length() ;
  }

  if(m_positionOfCaret < 0)
    m_positionOfCaret = 0;

  FindMatchingParens();
  m_containsChanges = true;

  // Style the text.
  StyleText();
  if (m_group != NULL)
    m_group->ResetSize();
  ResetData();
}

bool EditorCell::CheckChanges()
{
  if (m_containsChanges != m_containsChangesCheck)
  {
    m_containsChangesCheck = m_containsChanges;
    return true;
  }

  return false;
}

int EditorCell::ReplaceAll(wxString oldString, wxString newString, bool IgnoreCase)
{
  if (oldString == wxEmptyString)
    return 0;

  SaveValue();
  wxString newText;
  int count = 0;
  if(!IgnoreCase)
  {
    newText = m_text;
    newText.Replace(wxT("\r"), wxT(" "));
    count = newText.Replace(oldString, newString);
  }
  else
  {
    int pos;
    wxString src = m_text;
    src.Replace(wxT("\r"), wxT(" "));
    wxString src_LowerCase = src;
    src_LowerCase.MakeLower();
    oldString.MakeLower();
    do{
      pos = src_LowerCase.Find(oldString);
      if(pos == wxNOT_FOUND)
        newText += src;
      else
      {
        newText += src.Left(pos);
        newText += newString;
        src_LowerCase = src_LowerCase.Right(src_LowerCase.Length()-pos-newString.Length());
        src = src.Right(src.Length()-pos-oldString.Length());
        count ++;
      }
    } while((pos != wxNOT_FOUND) && (src != wxEmptyString));
  }
  if (count > 0)
  {
    m_text = newText;
    m_containsChanges = true;
    ClearSelection();
    StyleText();
  }

  // If text is selected setting the selection again updates m_selectionString
  if (m_selectionStart > 0)
    SetSelection(m_selectionStart, m_selectionEnd);

  return count;
}

bool EditorCell::FindNext(wxString str, bool down, bool ignoreCase)
{
  int start = down ? 0 : m_text.Length();
  wxString text(m_text);

  text.Replace(wxT('\r'), wxT(' '));

  if (ignoreCase)
  {
    str.MakeLower();
    text.MakeLower();
  }

  if (m_selectionStart >= 0)
  {
    if (down)
      start = m_selectionStart + 1;
    else
      start = m_selectionStart ;
  }
  else if (IsActive())
    start = m_positionOfCaret;

  if (!down && m_selectionStart == 0)
    return false;

  int strStart = wxNOT_FOUND;
  if (down)
    strStart = text.find(str, start);
  else
    strStart = text.rfind(str, start);

  if (strStart != wxNOT_FOUND)
  {
    SetSelection(strStart, strStart + str.Length());
    return true;
  }
  return false;
}

bool EditorCell::ReplaceSelection(wxString oldStr, wxString newStr, bool keepSelected, bool IgnoreCase, bool replaceMaximaString)
{
  wxString text(m_text);
  text.Replace(wxT("\r"), wxT(" "));

  long start = wxMin(m_selectionStart, m_selectionEnd);
  long end = wxMax(m_selectionStart, m_selectionEnd);
  if (m_selectionStart < 0)
  {
    if (oldStr == wxEmptyString)
      SetSelection(m_positionOfCaret, m_positionOfCaret);
    else
      return false;
  }

  if (IgnoreCase)
  {
    if (text.SubString(start, end - 1).Upper() !=
        wxString(oldStr).Upper()
            )
      return false;
  }
  else
  {
    if (text.SubString(start, end - 1) != oldStr)
      return false;
  }

  {
    // We cannot use SetValue() here, since SetValue() tends to move the cursor.
    wxString text_left = text.SubString(0, start - 1);
    wxString text_right = text.SubString(end, text.Length());
    m_text = text_left+
             newStr +
             text_right;
    StyleText();

    m_containsChanges = true;
    m_positionOfCaret = start + newStr.Length();

    if(replaceMaximaString)
    {
      if((newStr.EndsWith("\"") || (text_right.StartsWith("\""))))
      {
        if(!((newStr.EndsWith("\"") && (text_right.StartsWith("\"")))))
          m_positionOfCaret--;
      }
    }

    if (keepSelected)
    {
      SetSelection(start, m_positionOfCaret);
    }
    else
    {
      ClearSelection();
    }

    if (GetType() == MC_TYPE_INPUT)
      FindMatchingParens();

    StyleText();
    return true;
  }
  return false;
}

wxString EditorCell::GetSelectionString()
{
  if (m_selectionStart >= 0)
    return m_cellPointers->m_selectionString;
  else
    return wxEmptyString;
}

void EditorCell::ClearSelection()
{
  if (SelectionActive())
  {
    m_selectionChanged = true;
    m_cellPointers->m_selectionString = wxEmptyString;
    m_oldSelectionStart = m_oldSelectionEnd = m_selectionStart = m_selectionEnd = -1;
  }
}

/***
 * FindNextTemplate selects the next template
 * of moves the cursor behind the first closing
 * paren in the current line.
 */
bool EditorCell::FindNextTemplate(bool left)
{
  wxRegEx varsRegex;

  if (left)
    wxASSERT(varsRegex.Compile(wxT("(<[^> \n]+>)[^>]*$")));
  else
    wxASSERT(varsRegex.Compile(wxT("(<[^> \n]+>)")));

  int positionOfCaret = m_positionOfCaret;
  if (!left && m_selectionEnd != -1)
    positionOfCaret = m_selectionEnd;


  // Splits the string into first (from caret in the direction of search)
  // and second (the rest of the string)
  wxString first, second;
  if (left)
  {
    first = m_text.Mid(0, positionOfCaret);
    second = m_text.Mid(positionOfCaret);
  }
  else
  {
    first = m_text.Mid(positionOfCaret);
    second = m_text.Mid(0, positionOfCaret);
  }

  size_t start, length;

  // First search in the direction of search
  if (varsRegex.Matches(first))
  {
    varsRegex.GetMatch(&start, &length, 1);
    if (left)
    {
      m_positionOfCaret = start;
      SetSelection(start, m_selectionEnd);
    }
    else
      m_positionOfCaret = m_selectionStart = positionOfCaret + start;
    SetSelection(m_selectionStart, m_selectionStart + length);
    return true;
  }

  // Then in the rest of the string
  if (varsRegex.Matches(second))
  {
    varsRegex.GetMatch(&start, &length, 1);
    if (!left)
    {
      m_positionOfCaret = start;
      SetSelection(start, m_selectionEnd);
    }
    else
      m_positionOfCaret = m_selectionStart = positionOfCaret + start;
    SetSelection(m_selectionStart, m_selectionStart + length);
    return true;
  }

  return false;
}

void EditorCell::CaretToEnd()
{
  m_positionOfCaret = m_text.Length();
  if (GetType() == MC_TYPE_INPUT)
    FindMatchingParens();
}

void EditorCell::CaretToStart()
{
  m_positionOfCaret = 0;
  if (GetType() == MC_TYPE_INPUT)
    FindMatchingParens();
}

void EditorCell::CaretToPosition(int pos)
{
  m_positionOfCaret = pos;
  if (GetType() == MC_TYPE_INPUT)
    FindMatchingParens();
}


#if wxUSE_ACCESSIBILITY
wxAccStatus EditorCell::GetDescription(int childId, wxString *description)
{
  if (childId != 0)
    return wxACC_FAIL;

  if (description == NULL)
    return wxACC_FAIL;

  switch (GetType())
  {
  case MC_TYPE_INPUT:
    *description = _("Maxima code");
    break;
    //  case MC_TYPE_CHAPTER:
    //    *description = _("A chapter heading");
    //    break;
  case MC_TYPE_SECTION:
    *description = _("A section heading");
    break;
  case MC_TYPE_SUBSECTION:
    *description = _("A subsection heading");
    break;
  case MC_TYPE_SUBSUBSECTION:
    *description = _("A sub-subsection heading");
    break;
  case MC_TYPE_HEADING5:
    *description = _("A sub-sub-subsection heading");
    break;
  case MC_TYPE_HEADING6:
    *description = _("A sub-sub-sub-subsection heading");
    break;
  case MC_TYPE_TEXT:
    *description = _("Comment (ordinary worksheet text that isn't fed to maxima)");
    break;
  default:
    *description = _("Bug: Unknown type of text");
    break;
  }
  return wxACC_OK;
}

wxAccStatus EditorCell::GetDefaultAction (int WXUNUSED(childId), wxString *actionName)
{
  if(actionName != NULL)
  {
    *actionName = _("Type in text");
  return wxACC_OK;
  }
  return wxACC_FAIL;
}

wxAccStatus EditorCell::GetValue (int WXUNUSED(childId), wxString *strValue)
{
  wxString retval = ToString();

  // If the blinking caret is currently visible we hide the char under the caret
  if((m_displayCaret) && (m_positionOfCaret > 0))
  {
    if((unsigned(m_positionOfCaret) < retval.Length()))
    {
      if(retval[m_positionOfCaret] == wxT(' '))
        retval[m_positionOfCaret] = wxT('%');
      else
        retval[m_positionOfCaret] = wxT(' ');
    }
    else
      retval += wxT("%");
  }
  *strValue = retval;
  return wxACC_OK;
}

wxAccStatus EditorCell::GetFocus (int *childId, EditorCell **child)
{
  if(IsActive())
  {
    if(child != NULL)
      *child   = this;
    if(childId != NULL)
      *childId = 0;
    return wxACC_OK;
  }
  else
  {
    if(child != NULL)
      *child   = NULL;
    if(childId != NULL)
      *childId = 0;
    return wxACC_FAIL;
  }
}

wxAccStatus EditorCell::GetRole (int childId, wxAccRole *role)
{
  if((childId == 0) && (role != NULL))
  {
    *role = wxROLE_SYSTEM_TEXT;
    return wxACC_OK;
  }
  else
  {
    return wxACC_FAIL;
  }
}

#endif
