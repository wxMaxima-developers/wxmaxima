// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2006-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2012 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2014-2015 Gunter Königsmann <wxMaxima@physikbuch.de>
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

#include <wx/clipbrd.h>
#include <wx/regex.h>

#include "EditorCell.h"
#include "wxMaxima.h"
#include "wxMaximaFrame.h"
#include <wx/tokenzr.h>

#define ESC_CHAR wxT('\xA6')

const wxString operators = wxT("+-*/^:=#'!\";$");

wxString EditorCell::m_selectionString;

EditorCell::EditorCell(wxString text) : MathCell()
{
  m_selectionChanged = false;
  m_lastSelectionStart = -1;
  m_displayCaret = false;
  m_text = wxEmptyString;
  m_fontSize = -1;
  m_positionOfCaret = 0;
  m_caretColumn = -1; // used when moving up/down between lines
  m_selectionStart = -1;
  m_selectionEnd = -1;
  m_isActive = false;
  m_matchParens = true;
  m_paren1 = m_paren2 = -1;
  m_insertAns = true;
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
  m_text = TabExpand(text,0);
}

EditorCell::~EditorCell()
{
  if (m_next != NULL)
    delete m_next;
}

wxString EditorCell::EscapeHTMLChars(wxString input)
{
  input.Replace(wxT("&"), wxT("&amp;"));  
  input.Replace(wxT("\""), wxT("&quot;"));  
  input.Replace(wxT("<"), wxT("&lt;"));
  input.Replace(wxT(">"), wxT("&gt;"));
  input.Replace(wxT("\n"), wxT("<BR>\n"));
  return input;
}

wxString EditorCell::PrependNBSP(wxString input)
{
  bool     firstSpace = true;;
  wxString retval;
  
  for(size_t i=0;i<input.Length();i++)
  {
    wxChar ch = input.GetChar(i);
    if(ch == wxT('\n'))
      firstSpace = true;

    if(ch == wxT(' '))
    {
      if(firstSpace)
      {
        firstSpace = false;
        retval += ch;
      }
      else
        retval += wxT("&nbsp;");
    }
    else 
    {
      retval += ch;
      firstSpace = true;
    }
  }
  return retval;
}

MathCell *EditorCell::Copy()
{
  EditorCell *tmp = new EditorCell();
  // We cannot use SetValue() here, since SetValue() sometimes has the task to change
  //  the cell's contents
  tmp->m_text = m_text;
  tmp->m_containsChanges = m_containsChanges;
  CopyData(this, tmp);
  tmp->m_styledText = m_styledText;

  return tmp;
}

void EditorCell::Destroy()
{
  m_next = NULL;
}

wxString EditorCell::ToString()
{
  wxString text = m_text;

  if (SelectionActive())
  {
    long start = MIN(m_selectionStart, m_selectionEnd);
    long end = MAX(m_selectionStart, m_selectionEnd) - 1;
    text = m_text.SubString(start, end);
  }

  return text;
}

wxString EditorCell::ToTeX()
{
  wxString text = m_text;
  text.Replace(wxT("\x2212"), wxT("-")); // unicode minus sign
  text.Replace(wxT("\\"), wxT("\\verb|\\|"));
  text.Replace(L"\x03B1",wxT("\\ensuremath{\\alpha}"));
  text.Replace(L"\x00B2",wxT("\\ensuremath{^2}"));
  text.Replace(L"\x00B3",wxT("\\ensuremath{^3}"));
  text.Replace(L"\x221A",wxT("\\ensuremath{\\sqrt{}}"));
  text.Replace(L"\x2148",wxT("\\ensuremath{\\mathbbm{i}}"));
  text.Replace(L"\x2147",wxT("\\ensuremath{\\mathbbm{e}}"));
  text.Replace(L"\x210f",wxT("\\ensuremath{\\hbar}"));
  text.Replace(L"\x2208",wxT("\\ensuremath{\\in}"));
  text.Replace(L"\x21D2",wxT("\\ensuremath{\\Longrightarrow}"));
  text.Replace(L"\x221e",wxT("\\ensuremath{\\infty}"));
  text.Replace(L"\x22C0",wxT("\\ensuremath{\\wedge}"));
  text.Replace(L"\x22C1",wxT("\\ensuremath{\\vee}"));
  text.Replace(L"\x22bb",wxT("\\ensuremath{\\oplus}"));
  text.Replace(L"\x22BC",wxT("\\ensuremath{\\overline{\\wedge}}"));
  text.Replace(L"\x22BB",wxT("\\ensuremath{\\overline{\\vee}}"));
  text.Replace(L"\x00AC",wxT("\\ensuremath{\\setminus}"));
  text.Replace(L"\x22C3",wxT("\\ensuremath{\\cup}"));
  text.Replace(L"\x22C2",wxT("\\ensuremath{\\cap}"));
  text.Replace(L"\x2286",wxT("\\ensuremath{\\subseteq}"));
  text.Replace(L"\x2282",wxT("\\ensuremath{\\subset}"));
  text.Replace(L"\x2288",wxT("\\ensuremath{\\not\\subseteq}"));
  text.Replace(L"\x0127",wxT("\\ensuremath{\\hbar}"));
  text.Replace(L"\x0126",wxT("\\ensuremath{\\Hbar}"));
  text.Replace(L"\x2205",wxT("\\ensuremath{\\emptyset}"));
  text.Replace(L"\x00BD",wxT("\\ensuremath{\\frac{1}{2}}"));
  text.Replace(L"\x03B2",wxT("\\ensuremath{\\beta}"));
  text.Replace(L"\x03B3",wxT("\\ensuremath{\\gamma}"));
  text.Replace(L"\x03B4",wxT("\\ensuremath{\\delta}"));
  text.Replace(L"\x03B5",wxT("\\ensuremath{\\epsilon}"));
  text.Replace(L"\x03B6",wxT("\\ensuremath{\\zeta}"));
  text.Replace(L"\x03B7",wxT("\\ensuremath{\\eta}"));
  text.Replace(L"\x03B8",wxT("\\ensuremath{\\theta}"));
  text.Replace(L"\x03B9",wxT("\\ensuremath{\\iota}"));
  text.Replace(L"\x03BA",wxT("\\ensuremath{\\kappa}"));
  text.Replace(L"\x03BB",wxT("\\ensuremath{\\lambda}"));
  text.Replace(L"\x03BC",wxT("\\ensuremath{\\mu}"));
  text.Replace(L"\x03BD",wxT("\\ensuremath{\\nu}"));
  text.Replace(L"\x03BE",wxT("\\ensuremath{\\xi}"));
  text.Replace(L"\x03BF",wxT("\\ensuremath{\\omnicron}"));
  text.Replace(L"\x03C0",wxT("\\ensuremath{\\pi}"));
  text.Replace(L"\x03C1",wxT("\\ensuremath{\\rho}"));
  text.Replace(L"\x03C3",wxT("\\ensuremath{\\sigma}"));
  text.Replace(L"\x03C4",wxT("\\ensuremath{\\tau}"));
  text.Replace(L"\x03C5",wxT("\\ensuremath{\\upsilon}"));
  text.Replace(L"\x03C6",wxT("\\ensuremath{\\phi}"));
  text.Replace(L"\x03C7",wxT("\\ensuremath{\\chi}"));
  text.Replace(L"\x03C8",wxT("\\ensuremath{\\psi}"));
  text.Replace(L"\x03C9",wxT("\\ensuremath{\\omega}"));
  text.Replace(L"\x0391",wxT("\\ensuremath{\\Alpha}"));
  text.Replace(L"\x0392",wxT("\\ensuremath{\\Beta}"));
  text.Replace(L"\x0393",wxT("\\ensuremath{\\Gamma}"));
  text.Replace(L"\x0394",wxT("\\ensuremath{\\Delta}"));
  text.Replace(L"\x0395",wxT("\\ensuremath{\\Epsilon}"));
  text.Replace(L"\x0396",wxT("\\ensuremath{\\Zeta}"));
  text.Replace(L"\x0397",wxT("\\ensuremath{\\Eta}"));
  text.Replace(L"\x0398",wxT("\\ensuremath{\\Theta}"));
  text.Replace(L"\x0399",wxT("\\ensuremath{\\Iota}"));
  text.Replace(L"\x039A",wxT("\\ensuremath{\\Kappa}"));
  text.Replace(L"\x039B",wxT("\\ensuremath{\\Lambda}"));
  text.Replace(L"\x039C",wxT("\\ensuremath{\\Mu}"));
  text.Replace(L"\x039D",wxT("\\ensuremath{\\Nu}"));
  text.Replace(L"\x039E",wxT("\\ensuremath{\\Xi}"));
  text.Replace(L"\x039F",wxT("\\ensuremath{\\Omnicron}"));
  text.Replace(L"\x03A0",wxT("\\ensuremath{\\Pi}"));
  text.Replace(L"\x03A1",wxT("\\ensuremath{\\Rho}"));
  text.Replace(L"\x03A3",wxT("\\ensuremath{\\Sigma}"));
  text.Replace(L"\x03A4",wxT("\\ensuremath{\\Tau}"));
  text.Replace(L"\x03A5",wxT("\\ensuremath{\\Upsilon}"));
  text.Replace(L"\x03A6",wxT("\\ensuremath{\\Phi}"));
  text.Replace(L"\x03A7",wxT("\\ensuremath{\\Chi}"));
  text.Replace(L"\x03A8",wxT("\\ensuremath{\\Psi}"));
  text.Replace(L"\x03A9",wxT("\\ensuremath{\\Omega}"));
  text.Replace(L"\x2202",wxT("\\ensuremath{\\partial}"));
  text.Replace(L"\x222b",wxT("\\ensuremath{\\int}"));
  text.Replace(L"\x2245",wxT("\\ensuremath{\\approx}"));
  text.Replace(L"\x221d",wxT("\\ensuremath{\\propto}"));
  text.Replace(L"\x2260",wxT("\\ensuremath{\\neq}"));
  text.Replace(L"\x220e",wxT("\\ensuremath{\\blacksquare}"));
  text.Replace(L"\x2263",wxT("\\ensuremath{\\equiv}"));
  text.Replace(wxT("~"),wxT("\\ensuremath{\\sim }"));
  text.Replace(wxT("_"), wxT("\\_"));
  text.Replace(wxT("$"), wxT("\\$"));
  text.Replace(wxT("%"), wxT("\\%"));
  text.Replace(wxT("#"), wxT("\\ensuremath{\\neq}"));
  text.Replace(wxT("\xDCB6"), wxT("~")); // A non-breakable space
  return text;
}

wxString EditorCell::ToXML()
{
  wxString xmlstring = m_text;
  // convert it, so that the XML parser doesn't fail
  xmlstring.Replace(wxT("&"),  wxT("&amp;"));
  xmlstring.Replace(wxT("<"),  wxT("&lt;"));
  xmlstring.Replace(wxT(">"),  wxT("&gt;"));
  xmlstring.Replace(wxT("'"),  wxT("&apos;"));
  xmlstring.Replace(wxT("\""), wxT("&quot;"));
  xmlstring.Replace(wxT("\n"), wxT("</line>\n<line>"));
  xmlstring = wxT("<line>") + xmlstring + wxT("</line>\n");
  wxString head = wxT("<editor");
  switch (m_type) {
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
  case MC_TYPE_INPUT:
    default:
      head += wxT(" type=\"input\"");
      break;
  }
  head += wxT(">\n");

  return head + xmlstring + wxT("</editor>\n");
}

void EditorCell::RecalculateWidths(CellParser& parser, int fontsize)
{
  int charWidth;

  m_isDirty = false;
  if (m_height == -1 || m_width == -1 || fontsize != m_fontSize || parser.ForceUpdate())
  {
    m_fontSize = fontsize;
    wxDC& dc = parser.GetDC();
    double scale = parser.GetScale();
    SetFont(parser, fontsize);

    dc.GetTextExtent(wxT("X"), &charWidth, &m_charHeight);

    unsigned int newLinePos = 0, prevNewLinePos = 0;
    int width = 0, width1, height1;

    m_numberOfLines = 1;

    while (newLinePos < m_text.Length())
    {
      while (newLinePos < m_text.Length())
      {
        if (m_text.GetChar(newLinePos) == '\n')
          break;
        newLinePos++;
      }

      dc.GetTextExtent(m_text.SubString(prevNewLinePos, newLinePos), &width1, &height1);
      width = MAX(width, width1);

      while (newLinePos < m_text.Length() && m_text.GetChar(newLinePos) == '\n')
      {
        newLinePos++;
        m_numberOfLines++;
      }

      prevNewLinePos = newLinePos;
    }

    // new
    if (m_firstLineOnly)
      m_numberOfLines = 1;

    if (m_text == wxEmptyString)
      width = charWidth;

    m_width = width + 2 * SCALE_PX(2, scale);
    m_height = m_numberOfLines * m_charHeight + 2 * SCALE_PX(2, scale);

    m_center = m_charHeight / 2 + SCALE_PX(2, scale);
  }
  ResetData();
}

wxString EditorCell::ToHTML()
{
  EditorCell *tmp = this;
  wxString retval;

  while(tmp != NULL)
  {
    std::list<StyledText> styledText = tmp->m_styledText;
    while(!styledText.empty())
    {
      // Grab a portion of text from the list.
      StyledText TextSnippet = styledText.front();
      styledText.pop_front();

      wxString text =  PrependNBSP(EscapeHTMLChars(TextSnippet.GetText()));
/*      wxString tmp = EscapeHTMLChars(TextSnippet.GetText());
        wxString text = tmp);*/
      
      if(TextSnippet.StyleSet())
      {
        switch(TextSnippet.GetStyle())
        {
        case TS_CODE_COMMENT:
          retval+=wxT("<span class=\"code_comment\">")+text+wxT("</span>");
          break;
        case TS_CODE_VARIABLE:
          retval+=wxT("<span class=\"code_variable\">")+text+wxT("</span>");
          break;
        case TS_CODE_FUNCTION:
          retval+=wxT("<span class=\"code_function\">")+text+wxT("</span>");
          break;
        case TS_CODE_NUMBER:
          retval+=wxT("<span class=\"code_number\">")+text+wxT("</span>");
          break;
        case TS_CODE_STRING:
          retval+=wxT("<span class=\"code_string\">")+text+wxT("</span>");
          break;
        case TS_CODE_OPERATOR:
          retval+=wxT("<span class=\"code_operator\">")+text+wxT("</span>");
          break;
        case TS_CODE_ENDOFLINE:
        default:
          retval+=wxT("<span class=\"code_endofline\">")+text+wxT("</span>");
          break;
        }
      } else
        retval+=text;
    }
    tmp = dynamic_cast<EditorCell*>(tmp->m_next);
  }
  return retval; 
}

void EditorCell::MarkSelection(size_t start, size_t end,CellParser& parser,double scale, wxDC& dc, TextStyle style)
{
  wxRect rect = GetRect(); // rectangle representing the cell
  wxPoint point, point1;
  long pos1 = start, pos2 = start;

#if defined(__WXMAC__)
  dc.SetPen(wxNullPen); // no border on rectangles
#else
  dc.SetPen(*(wxThePenList->FindOrCreatePen(parser.GetColor(style), 1, wxPENSTYLE_SOLID)) );
// window linux, set a pen
#endif
  dc.SetBrush( *(wxTheBrushList->FindOrCreateBrush(parser.GetColor(style))) ); //highlight c.
  
  
  while (pos1 < end) // go through selection, draw a rect for each line of selection
  {
    while (pos1 < end && m_text.GetChar(pos1) != '\n')
      pos1++;
    
    point = PositionToPoint(parser, pos2);  // left  point
    point1 = PositionToPoint(parser, pos1); // right point
    long selectionWidth = point1.x - point.x;
#if defined(__WXMAC__)
    if (pos1 != end) // we have a \n, draw selection to the right border (mac behaviour)
      selectionWidth = rect.GetRight() - point.x - SCALE_PX(2,scale);
#endif
    dc.DrawRectangle(point.x + SCALE_PX(2, scale), // draw the rectangle
                     point.y + SCALE_PX(2, scale) - m_center,
                     selectionWidth,
                     m_charHeight);
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
void EditorCell::Draw(CellParser& parser, wxPoint point1, int fontsize)
{
  m_selectionChanged = false;
  double scale = parser.GetScale();
  wxDC& dc = parser.GetDC();
  wxPoint point(point1);

  if (m_width == -1 || m_height == -1)
    RecalculateWidths(parser, fontsize);

  if (DrawThisCell(parser, point) && !m_isHidden)
  {
    dc.SetLogicalFunction(wxCOPY); // opaque (for everything except the caret)

    // Need correct m_currentPoint before we call MathCell::Draw!
    m_currentPoint.x = point.x;
    m_currentPoint.y = point.y;

    //
    // Mark text that coincides with the selection
    //
    if (m_selectionString != wxEmptyString)
    {
      size_t start = 0;
      while((start = m_text.find(m_selectionString,start)) != wxNOT_FOUND)
      {
        size_t end = start + m_selectionString.Length();
        MarkSelection(start,end,parser,scale,dc,TS_EQUALSSELECTION);
        start = end;
      }
    }
    
    if (m_isActive) // draw selection or matching parens
    {
      //
      // Mark selection
      //
      if (m_selectionStart >= 0)
        MarkSelection(MIN(m_selectionStart, m_selectionEnd),
                      MAX(m_selectionStart, m_selectionEnd),
                      parser,scale,dc,TS_SELECTION);

      //
      // Matching parens - draw only if we dont have selection
      //
      else if (m_paren1 != -1 && m_paren2 != -1)
      {
#if defined(__WXMAC__)
        wxRect rect = GetRect(); // rectangle representing the cell
        dc.SetPen(wxNullPen); // no border on rectangles
#else
        dc.SetPen(*(wxThePenList->FindOrCreatePen(parser.GetColor(TS_SELECTION), 1, wxPENSTYLE_SOLID))); // window linux, set a pen
#endif
        dc.SetBrush( *(wxTheBrushList->FindOrCreateBrush(parser.GetColor(TS_SELECTION))) ); //highlight c.

        wxPoint point = PositionToPoint(parser, m_paren1);
        int width, height;
        dc.GetTextExtent(m_text.GetChar(m_paren1), &width, &height);
        dc.DrawRectangle(point.x + SCALE_PX(2, scale) + 1,
                         point.y  + SCALE_PX(2, scale) - m_center + 1,
                         width - 1, height - 1);
        point = PositionToPoint(parser, m_paren2);
        dc.GetTextExtent(m_text.GetChar(m_paren1), &width, &height);
        dc.DrawRectangle(point.x + SCALE_PX(2, scale) + 1,
                         point.y  + SCALE_PX(2, scale) - m_center + 1,
                         width - 1, height - 1);
      } // else if (m_paren1 != -1 && m_paren2 != -1)
    } // if (m_isActive)

    //
    // Draw the text
    //
    SetPen(parser);
    SetFont(parser, fontsize);

    wxPoint TextStartingpoint = point;
    int labelwidth, labelheight;
    // TextStartingpoint.x -= SCALE_PX(MC_TEXT_PADDING, scale);
    TextStartingpoint.x += SCALE_PX(2, scale);
    TextStartingpoint.y += SCALE_PX(2, scale);
    wxPoint TextCurrentPoint = TextStartingpoint;
    std::list<StyledText> styledText = m_styledText;
    int lastStyle = -1;
    while(!styledText.empty())
    {
      // Grab a portion of text from the list.
      StyledText TextSnippet = styledText.front();
      styledText.pop_front();
      wxString TextToDraw = TextSnippet.GetText();
      int width, height;
      
      // A newline is a separate token.
      if(TextToDraw == wxT("\n"))
      {
        // A newline =>
        // set the point to the beginning of the next line.
        TextCurrentPoint.x = TextStartingpoint.x;
        TextCurrentPoint.y += m_charHeight;
      }
      else
      {
        // We need to draw some text.
        
        // Grab a pen of the right color.
        if(TextSnippet.StyleSet())
        {
          wxDC& dc = parser.GetDC();
          if(lastStyle != TextSnippet.GetStyle())
          {
            dc.SetTextForeground(parser.GetColor(TextSnippet.GetStyle()));
            lastStyle = TextSnippet.GetStyle();
          }
        }
        else
        {
          lastStyle = -1;
          SetForeground(parser);
        }

#if defined __WXMSW__ || wxUSE_UNICODE
        // replace "*" with centerdot if requested
        if (parser.GetChangeAsterisk())  
          TextToDraw.Replace(wxT("*"), wxT("\xB7"));
#endif
        
        dc.DrawText(TextToDraw,
                    TextCurrentPoint.x,
                    TextCurrentPoint.y - m_center);
        /*
        dc.DrawLine(TextCurrentPoint.x + SCALE_PX(2, scale),
                    TextCurrentPoint.y - m_center,
                    TextCurrentPoint.x + SCALE_PX(2, scale),
                    TextCurrentPoint.y); */
        
        dc.GetTextExtent(TextToDraw, &width, &height);
        TextCurrentPoint.x += width;
      }
    }
    //
    // Draw the caret
    //

    if (m_displayCaret && m_hasFocus && m_isActive)
    {
      int caretInLine = 0;
      int caretInColumn = 0;

      PositionToXY(m_positionOfCaret, &caretInColumn, &caretInLine);

      int lineWidth = GetLineWidth(dc, caretInLine, caretInColumn);

      dc.SetPen(*(wxThePenList->FindOrCreatePen(parser.GetColor(TS_CURSOR), 1, wxPENSTYLE_SOLID))); //TODO is there more efficient way to do this?
#if defined(__WXMAC__)
      // draw 1 pixel shorter caret than on windows
      dc.DrawLine(point.x + SCALE_PX(2, scale) + lineWidth,
                  point.y + SCALE_PX(2, scale) - m_center + caretInLine * m_charHeight,
                  point.x + SCALE_PX(2, scale) + lineWidth,
                  point.y + SCALE_PX(1, scale) - m_center + (caretInLine + 1) * m_charHeight);
#else
      dc.DrawLine(point.x + SCALE_PX(2, scale) + lineWidth,
                  point.y + SCALE_PX(2, scale) - m_center + caretInLine * m_charHeight,
                  point.x + SCALE_PX(2, scale) + lineWidth,
                  point.y + SCALE_PX(2, scale) - m_center + (caretInLine + 1) * m_charHeight);
#endif
    }

    UnsetPen(parser);

  } // if (DrawThisCell(parser, point) && !m_isHidden)
  MathCell::Draw(parser, point1, fontsize);
}

void EditorCell::SetFont(CellParser& parser, int fontsize)
{
  wxDC& dc = parser.GetDC();
  double scale = parser.GetScale();

  int fontsize1 = parser.GetFontSize(m_textStyle);
  if (fontsize1 == 0)
    fontsize1 = fontsize;
  m_fontSize = fontsize1;

  fontsize1 = (int) (((double)fontsize1) * scale + 0.5);
  fontsize1 = MAX(fontsize1, 1);

  m_fontName = parser.GetFontName(m_textStyle);
  m_fontStyle = parser.IsItalic(m_textStyle);
  m_fontWeight = parser.IsBold(m_textStyle);
  m_underlined = parser.IsUnderlined(m_textStyle);
  m_fontEncoding = parser.GetFontEncoding();

  dc.SetFont(wxFont(fontsize1, wxFONTFAMILY_MODERN,
                    m_fontStyle,
                    m_fontWeight,
                    m_underlined,
                    m_fontName,
                    m_fontEncoding));
}

void EditorCell::SetForeground(CellParser& parser)
{
  wxDC& dc = parser.GetDC();
  dc.SetTextForeground(parser.GetColor(m_textStyle));
}

#ifndef WX_USE_UNICODE

int ChangeNumpadToChar(int c)
{
  switch (c) {
    case WXK_NUMPAD0:
      return '0';
      break;
    case WXK_NUMPAD1:
      return '1';
      break;
    case WXK_NUMPAD2:
      return '2';
      break;
    case WXK_NUMPAD3:
      return '3';
      break;
    case WXK_NUMPAD4:
      return '4';
      break;
    case WXK_NUMPAD5:
      return '5';
      break;
    case WXK_NUMPAD6:
      return '6';
      break;
    case WXK_NUMPAD7:
      return '7';
      break;
    case WXK_NUMPAD8:
      return '8';
      break;
    case WXK_NUMPAD9:
      return '9';
      break;
    case WXK_NUMPAD_DECIMAL:
      return '.';
      break;
  }
  return c;
}

#endif

wxString EditorCell::TabExpand(wxString input, size_t posInLine)
{
  wxString retval;
  // Convert the text to our line endings.
  input.Replace(wxT("\r\n"),wxT("\n"));
  input.Replace(wxT("\r"),wxT("\n"));

  size_t index = 0;
  while(index < input.Length())
  {
    wxChar ch = input[index++];

    if(ch == wxT('\n'))
    {
      posInLine = 0;
      retval += ch;
      continue;
    }

    if(ch == wxT('\t'))
    {
      switch(posInLine - (posInLine / 4) * 4)
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
      continue;
    }
    retval += ch;
    posInLine ++;
  }
  // TODO: Implement the actual TAB expansion
  return retval;
}

size_t EditorCell::BeginningOfLine(size_t pos)
{
  if(pos>0)
    pos--;
  
  while(pos > 0)
  {
    if(m_text[pos]==wxT('\n'))
      break;
    pos--;
  }
  if(m_text[pos]==wxT('\n'))
    pos++;
  return pos;
}

void EditorCell::ProcessEvent(wxKeyEvent &event)
{
  if ((event.GetKeyCode() != WXK_DOWN) &&
      (event.GetKeyCode() != WXK_PAGEDOWN) &&
      (event.GetKeyCode() != WXK_PAGEUP) &&
#ifdef WXK_PRIOR
      (event.GetKeyCode() != WXK_PRIOR) &&
#endif
#ifdef WXK_NEXT
      (event.GetKeyCode() != WXK_NEXT) &&
#endif
      (event.GetKeyCode() != WXK_UP) &&
      (event.GetKeyCode() != WXK_PAGEDOWN)
    )
    m_caretColumn = -1; // make caretColumn invalid
  
  switch (event.GetKeyCode())
  {
  case WXK_LEFT:
    SaveValue();
    if (event.ShiftDown())
    {
      if (m_selectionStart == -1)
        SetSelection(m_positionOfCaret,m_positionOfCaret);
    }
    else
      ClearSelection();

    if (event.ControlDown()) {
      int lastpos = m_positionOfCaret;

      while((m_positionOfCaret>0)&&
            (
              wxIsalnum(m_text[m_positionOfCaret - 1]) ||
              m_text[m_positionOfCaret - 1] == wxT('_')
              )
        )
        m_positionOfCaret--;

      while((m_positionOfCaret>0)&&(wxIsspace(m_text[m_positionOfCaret - 1])))
        m_positionOfCaret--;
      
      if((lastpos == m_positionOfCaret)&&(m_positionOfCaret > 0))
        m_positionOfCaret--;
    }
    else if (event.AltDown())
    {
      int count=0;

      while (m_positionOfCaret > 0 && count >= 0)
      {
        m_positionOfCaret--;
        if (m_text[m_positionOfCaret]=='(' || m_text[m_positionOfCaret]=='[')
          count--;
        else if (m_text[m_positionOfCaret]==')' || m_text[m_positionOfCaret]==']')
          count++;
      }
    }
    else if (m_positionOfCaret > 0)
      m_positionOfCaret--;
    
    if (event.ShiftDown())
      SetSelection(m_selectionStart,m_positionOfCaret);
    
    break;
    
  case WXK_RIGHT:
    SaveValue();
    if (event.ShiftDown())
    {
      if (m_selectionStart == -1)
        SetSelection(m_positionOfCaret,m_positionOfCaret);
    }
    else
      ClearSelection();
    
    if (event.ControlDown()) {
      int lastpos = m_positionOfCaret;

      while((m_positionOfCaret<m_text.Length())&&
            (
              wxIsalnum(m_text[m_positionOfCaret]) ||
              m_text[m_positionOfCaret] == wxT('_')
              )
        )
        m_positionOfCaret++;

      while((m_positionOfCaret<m_text.Length())&&(wxIsspace(m_text[m_positionOfCaret])))
        m_positionOfCaret++;
      
      if((m_positionOfCaret<m_text.Length())&&(lastpos == m_positionOfCaret))
        m_positionOfCaret++;
    }
    else if (event.AltDown())
    {
      int count=0;

      while (m_positionOfCaret < (signed)m_text.Length() && count >= 0)
      {
        m_positionOfCaret++;
        if (m_text[m_positionOfCaret-1]=='(' || m_text[m_positionOfCaret-1]=='[')
          count++;
        else if (m_text[m_positionOfCaret-1]==')' || m_text[m_positionOfCaret-1]==']')
          count--;
      }
    }

    else if (m_positionOfCaret < (signed)m_text.Length())
      m_positionOfCaret++;

    if (event.ShiftDown())
      SetSelection(m_selectionStart,m_positionOfCaret);

    break;

  case WXK_PAGEDOWN:
#ifdef WXK_NEXT
  case WXK_NEXT:
#endif
  case WXK_DOWN:
    SaveValue();
    {
      if (event.ShiftDown())
      {
        if (m_selectionStart == -1)
        {
          SetSelection(m_positionOfCaret,m_positionOfCaret);
          m_lastSelectionStart = m_positionOfCaret;
        }
      }
      else
        ClearSelection();
      int column, line;
      PositionToXY(m_positionOfCaret, &column, &line); // get current line
      if (m_caretColumn > -1)
        column = m_caretColumn;
      else
        m_caretColumn = column;

      if (line < m_numberOfLines-1) // can we go down ?
        m_positionOfCaret = XYToPosition(column, line + 1);
      else { // we can't go down. move caret to the end
        m_positionOfCaret = (signed)m_text.Length();
        m_caretColumn = -1; // make caretColumn invalid
      }

      if (event.ShiftDown())
        SetSelection(m_selectionStart,m_positionOfCaret);
    }
    break;

  case WXK_PAGEUP:
#ifdef WXK_PRIOR
  case WXK_PRIOR:
#endif
  case WXK_UP:
    SaveValue();
    {
      if (event.ShiftDown())
      {
        if (m_selectionStart == -1)
        {
          SetSelection(m_positionOfCaret,m_positionOfCaret);
          m_lastSelectionStart = m_positionOfCaret;
        }
      }
      else
        ClearSelection();

      int column, line;
      PositionToXY(m_positionOfCaret, &column, &line); // get current line
      if (m_caretColumn > -1)
        column = m_caretColumn;
      else
        m_caretColumn = column;

      if (line > 0) // can we go up?
        m_positionOfCaret = XYToPosition(column, line - 1);
      else { // we can't move up, move to the beginning
        m_positionOfCaret = 0;
        m_caretColumn = -1; // make caretColumn invalid
      }

      if (event.ShiftDown())
        SetSelection(m_selectionStart,m_positionOfCaret);
    }
    break;

  case WXK_RETURN:
    SaveValue();
    if (m_selectionStart != -1) // we have a selection, delete it, then proceed
    {
      SaveValue();
      long start = MIN(m_selectionEnd, m_selectionStart);
      long end = MAX(m_selectionEnd, m_selectionStart);
      m_text = m_text.SubString(0, start - 1) +
               m_text.SubString(end, m_text.Length());
      m_positionOfCaret = start;
      ClearSelection();
    }

    {
      wxString indentString;
      int indentChars = 0;

      /*
      // Determine how far the last line was indented.
      size_t pos = BeginningOfLine(m_positionOfCaret);
      while((pos < m_text.Length())&&(m_text[pos]==wxT(' ')))
      {
        pos++;
        indentChars++;
      }
      */

      // Determine how many parenthesis this cell opens or closes before the point
      size_t pos = 0;
      while((pos < m_text.Length())&&(pos < m_positionOfCaret))
      {
        wxChar ch = m_text[pos];
        if(ch == wxT('\\'))
        {
          pos++;
          continue;
        }
        
        if(ch == wxT('\"'))
        {
          pos++;
          while(
            (pos < m_text.Length()) &&
            (pos < m_positionOfCaret) &&
            (m_text[pos] != wxT('\"'))
            )
            pos++;
        }
        
        if(
          (ch == wxT('(')) ||
          (ch == wxT('[')) ||
          (ch == wxT('{'))
          )
        {
          indentChars += 4;
        }
        
        if(
          (ch == wxT(')')) ||
          (ch == wxT(']')) ||
          (ch == wxT('}'))
          )  
        {
          indentChars -= 4;
        }
                  pos++;
      }

      if(m_text.Length() > m_positionOfCaret)
      {
        if(
          (m_text[m_positionOfCaret] == wxT(')')) ||
          (m_text[m_positionOfCaret] == wxT(']')) ||
          (m_text[m_positionOfCaret] == wxT('}'))
          )
          indentChars -= 4;
      }

      // The string we indent with.
      if(indentChars > 0)
        for(int i=0;i<indentChars;i++)
          indentString += wxT(" ");
      
      m_text = m_text.SubString(0, m_positionOfCaret - 1) +
        wxT("\n") + indentString +
        m_text.SubString(m_positionOfCaret, m_text.Length());
      m_positionOfCaret++;
      if(indentChars > 0)
        m_positionOfCaret += indentChars;
      m_isDirty = true;
      m_containsChanges = true;
    }
    break;

  case WXK_END:
    SaveValue();
    if (event.ShiftDown())
    {
      if (m_selectionStart == -1)
        SetSelection(m_positionOfCaret,m_positionOfCaret);
    }
    else
      ClearSelection();
    
    if (event.ControlDown())
      m_positionOfCaret = (signed)m_text.Length();
    else
    {
      while (m_positionOfCaret < (signed)m_text.Length() &&
             m_text.GetChar(m_positionOfCaret) != '\n')
        m_positionOfCaret++;
    }

    if (event.ShiftDown())
      SetSelection(m_selectionStart,m_positionOfCaret);
    break;

  case WXK_HOME:
    SaveValue();
    {
      if (event.ShiftDown())
      {
        if (m_selectionStart == -1)
          SetSelection(m_positionOfCaret,m_positionOfCaret);
      }
      else
        ClearSelection();
      
      if (event.ControlDown())
        m_positionOfCaret = 0;
      else
      {
        int col, lin;
        PositionToXY(m_positionOfCaret, &col, &lin);
        m_positionOfCaret = XYToPosition(0, lin);
      }

      if (event.ShiftDown())
        SetSelection(m_selectionStart,m_positionOfCaret);
    }
    break;

  case WXK_DELETE:
    SaveValue();
    if (m_selectionStart == -1)
    {
      if (m_positionOfCaret < (signed)m_text.Length())
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
      long start = MIN(m_selectionEnd, m_selectionStart);
      long end = MAX(m_selectionEnd, m_selectionStart);
      m_text = m_text.SubString(0, start - 1) +
               m_text.SubString(end, m_text.Length());
      m_positionOfCaret = start;
      ClearSelection();
    }
    break;

  case WXK_BACK:
    SaveValue();
    if (SelectionActive()) {
      SaveValue();
      m_saveValue = true;
      m_containsChanges = true;
      m_isDirty = true;
      long start = MIN(m_selectionEnd, m_selectionStart);
      long end = MAX(m_selectionEnd, m_selectionStart);
      m_text = m_text.SubString(0, start - 1) +
               m_text.SubString(end, m_text.Length());
      m_positionOfCaret = start;
      ClearSelection();
      break;
    }
    else
    {
      if(!event.CmdDown())
      {
        // Backspace without Ctrl => Delete one character if there are characters to delete.
        if(m_positionOfCaret > 0)
        {
          m_containsChanges = true;
          m_isDirty = true;
          
          if(m_text.SubString(0, m_positionOfCaret - 1).Right(4) == wxT("    ")) 
          {
            m_text = m_text.SubString(0, m_positionOfCaret - 5) +
              m_text.SubString(m_positionOfCaret, m_text.Length());
            m_positionOfCaret -= 4;
          }
          else
          { 
            /// If deleting ( in () then delete both.
            int right = m_positionOfCaret;
            if (m_positionOfCaret < m_text.Length() &&
                ((m_text.GetChar(m_positionOfCaret-1) == '[' && m_text.GetChar(m_positionOfCaret) == ']') ||
                 (m_text.GetChar(m_positionOfCaret-1) == '(' && m_text.GetChar(m_positionOfCaret) == ')') ||
                 (m_text.GetChar(m_positionOfCaret-1) == '{' && m_text.GetChar(m_positionOfCaret) == '}') ||
                 (m_text.GetChar(m_positionOfCaret-1) == '"' && m_text.GetChar(m_positionOfCaret) == '"')))
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
        while((wxIsalnum(m_text[m_positionOfCaret - 1]))&&(m_positionOfCaret>0))
        {
          m_positionOfCaret--;
          m_text = m_text.SubString(0, m_positionOfCaret - 1) +
                   m_text.SubString(m_positionOfCaret + 1, m_text.Length());
        }            
        // Delete Spaces, Tabs and Newlines until the next printable character
        while((wxIsspace(m_text[m_positionOfCaret - 1]))&&(m_positionOfCaret>0))
        {
          m_positionOfCaret--;
          m_text = m_text.SubString(0, m_positionOfCaret - 1) +
                   m_text.SubString(m_positionOfCaret + 1, m_text.Length());
        }
        
        // If we didn't delete anything till now delete one single character.
        if(lastpos == m_positionOfCaret)
        {
          m_positionOfCaret--;
          m_text = m_text.SubString(0, m_positionOfCaret - 1) +
                   m_text.SubString(m_positionOfCaret + 1, m_text.Length());
        }
      }
    }
    break;

  case WXK_TAB:
    m_isDirty = true;
    if (!FindNextTemplate(event.ShiftDown()))
    {
      m_containsChanges = true;
      {
        if (SelectionActive())
        {
          SaveValue();

          size_t start = MIN(m_selectionStart,m_selectionEnd);
          size_t end   = MAX(m_selectionStart,m_selectionEnd);
          size_t newLineIndex = m_text.find(wxT('\n'),start);

          if(((newLineIndex != wxNOT_FOUND) && (newLineIndex < end)) ||
             (m_text.SubString(newLineIndex,start).Trim() == wxEmptyString)
            )
          {
            start = BeginningOfLine(start);
            size_t pos = start;
            
            if(m_text[end-1]==wxT('\n'))
              end++;

            if(end > m_text.Length())
              end = m_text.Length();
            
            while(pos < end)
            {
              if(event.ShiftDown())
              {
                for(size_t i=0;i<4;i++)
                  if(m_text[pos]==wxT(' '))
                  {
                    m_text =
                      m_text.SubString(0, pos - 1) +
                      m_text.SubString(pos + 1, m_text.Length());
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
              while((pos < end) && (m_text[pos] != wxT('\n')))
                pos ++;
                if(m_text[pos] == wxT('\n'))
              pos ++;
            }
            SetSelection(start,end);
          }
          else
          {
            m_text = m_text.SubString(0, start - 1) +
              m_text.SubString(end, m_text.Length());
            ClearSelection();
          }
          m_positionOfCaret = start;
          break;
        }
        int col, line;
        PositionToXY(m_positionOfCaret, &col, &line);
        wxString ins;
        do {
          col++;
          ins += wxT(" ");
        } while (col%4 != 0);

        m_text = m_text.SubString(0, m_positionOfCaret - 1) +
                 ins +
                 m_text.SubString(m_positionOfCaret, m_text.Length());
        m_positionOfCaret += ins.Length();
      }
    }
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
#if wxUSE_UNICODE
    else
    {
      // TODO: search only a few positions back for an escchar (10? and not over newlines)
      bool insertescchar = false;
      int esccharpos = m_text.Left(m_positionOfCaret).Find(ESC_CHAR, true);
      if (esccharpos > -1) { // we have a match, check for insertion
        wxString greek = InterpretEscapeString(m_text.SubString(esccharpos + 1, m_positionOfCaret - 1));
        if (greek.Length() > 0 ) {
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

      if (insertescchar) {
        m_text = m_text.SubString(0, m_positionOfCaret - 1) + ESC_CHAR +
                 m_text.SubString(m_positionOfCaret, m_text.Length());
        m_isDirty = true;
        m_containsChanges = true;
        m_positionOfCaret++;
      }
    }
#endif
    break;

    /* Ignored keys */
  case WXK_WINDOWS_LEFT:
  case WXK_WINDOWS_RIGHT:
  case WXK_WINDOWS_MENU:
  case WXK_COMMAND:
  case WXK_START:
    break;

  default:
    if (event.ControlDown() && !event.AltDown())
      break;

    m_isDirty = true;
    m_containsChanges = true;
    bool insertLetter = true;

    if (m_saveValue) {
      SaveValue();
      m_saveValue = false;
    }

    
      wxChar keyCode;
#if wxUSE_UNICODE
      keyCode=event.GetUnicodeKey();
#else
      keyCode=event.GetKeyCode();
#endif

      // If we got passed a non-printable character we have to send it back to the
      // hotkey management.
      if(!wxIsprint(keyCode))
      {
        event.Skip();
        break;
      }

    if (m_historyPosition != -1) {
      int len = m_textHistory.GetCount() - m_historyPosition;
      m_textHistory.RemoveAt(m_historyPosition + 1, len - 1);
      m_startHistory.erase(m_startHistory.begin() + m_historyPosition + 1, m_startHistory.end());
      m_endHistory.erase(m_endHistory.begin() + m_historyPosition + 1, m_endHistory.end());
      m_positionHistory.erase(m_positionHistory.begin() + m_historyPosition + 1, m_positionHistory.end());
      m_historyPosition = -1;
    }

    // if we have a selection either put parens around it (and don't write the letter afterwards)
    // or delete selection and write letter (insertLetter = true).
    if (m_selectionStart > -1) {
      SaveValue();
      long start = MIN(m_selectionEnd, m_selectionStart);
      long end = MAX(m_selectionEnd, m_selectionStart);
      
      switch (keyCode)
      {
      case '(':
        m_text = m_text.SubString(0, start - 1) +   wxT("(") +
                 m_text.SubString(start, end - 1) + wxT(")") +
                 m_text.SubString(end, m_text.Length());
        m_positionOfCaret = start;  insertLetter = false;
        break;
      case '\"':
        m_text = m_text.SubString(0, start - 1) +   wxT("\"") +
                 m_text.SubString(start, end - 1) + wxT("\"") +
                 m_text.SubString(end, m_text.Length());
        m_positionOfCaret = start;  insertLetter = false;
        break;
      case '{':
        m_text = m_text.SubString(0, start - 1) +   wxT("{") +
                 m_text.SubString(start, end - 1) + wxT("}") +
                 m_text.SubString(end, m_text.Length());
        m_positionOfCaret = start;  insertLetter = false;
        break;
      case '[':
        m_text = m_text.SubString(0, start - 1) +   wxT("[") +
                 m_text.SubString(start, end - 1) + wxT("]") +
                 m_text.SubString(end, m_text.Length());
        m_positionOfCaret = start;  insertLetter = false;
        break;
      case ')':
        m_text = m_text.SubString(0, start - 1) +   wxT("(") +
                 m_text.SubString(start, end - 1) + wxT(")") +
                 m_text.SubString(end, m_text.Length());
        m_positionOfCaret = end + 2; insertLetter = false;
        break;
      case '}':
        m_text = m_text.SubString(0, start - 1) +   wxT("{") +
                 m_text.SubString(start, end - 1) + wxT("}") +
                 m_text.SubString(end, m_text.Length());
        m_positionOfCaret = end + 2; insertLetter = false;
        break;
      case ']':
        m_text = m_text.SubString(0, start - 1) +   wxT("[") +
                 m_text.SubString(start, end - 1) + wxT("]") +
                 m_text.SubString(end, m_text.Length());
        m_positionOfCaret = end + 2; insertLetter = false;
        break;
      default: // delete selection
        m_text = m_text.SubString(0, start - 1) +
                 m_text.SubString(end, m_text.Length());
        m_positionOfCaret = start;
        break;
      }
      ClearSelection();
    } // end if (m_selectionStart > -1)

// insert letter if we didn't insert brackets around selection
  if (insertLetter) {
      m_text = m_text.SubString(0, m_positionOfCaret - 1) +
#if wxUSE_UNICODE
               event.GetUnicodeKey() +
#else
               wxString::Format(wxT("%c"), ChangeNumpadToChar(event.GetKeyCode())) +
#endif
               m_text.SubString(m_positionOfCaret, m_text.Length());

      m_positionOfCaret++;
      
      if (m_matchParens)
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
          if (m_positionOfCaret < m_text.Length() &&
              m_text.GetChar(m_positionOfCaret) == '"')
            m_text = m_text.SubString(0, m_positionOfCaret - 2)+
                      m_text.SubString(m_positionOfCaret, m_text.Length());
          else
            m_text = m_text.SubString(0, m_positionOfCaret - 1) +
                       wxT("\"") + m_text.SubString(m_positionOfCaret, m_text.Length());
          break;
        case ')': // jump over ')'
          if (m_positionOfCaret < m_text.Length() &&
              m_text.GetChar(m_positionOfCaret) == ')')
            m_text = m_text.SubString(0, m_positionOfCaret - 2) +
                       m_text.SubString(m_positionOfCaret, m_text.Length());
          break;
        case ']': // jump over ']'
          if (m_positionOfCaret < m_text.Length() &&
              m_text.GetChar(m_positionOfCaret) == ']')
            m_text = m_text.SubString(0, m_positionOfCaret - 2) +
                       m_text.SubString(m_positionOfCaret, m_text.Length());
          break;
        case '}': // jump over '}'
          if (m_positionOfCaret < m_text.Length() &&
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
          wxChar key = event.GetKeyCode();
          size_t len = m_text.Length();
          if (m_insertAns && len == 1 && m_positionOfCaret == 1)
          {
            m_text = m_text.SubString(0, m_positionOfCaret - 2) + wxT("%") +
                     m_text.SubString(m_positionOfCaret - 1, m_text.Length());
            m_positionOfCaret += 1;
          }
          break;
        }
      }
    } // end if (insertLetter)
    break;
  } // end switch (event.GetKeyCode())

  if (m_type == MC_TYPE_INPUT)
    FindMatchingParens();

  if (m_isDirty)
  {
    m_width = m_maxDrop = -1;
    StyleText();
  }
  m_displayCaret = true;
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

  if (pos == m_text.Length() ||
      wxString(wxT("\"")).Find(m_text.GetChar(pos)) == -1)
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
         (i >= 1 && m_text.GetChar(i-1) != '\\')))
    {
      ++count;
      if (count&1)
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

  if (m_paren2 == m_text.Length() ||
      wxString(wxT("([{}])")).Find(m_text.GetChar(m_paren2)) == -1)
  {
    m_paren2--;
    if (m_paren2 < 0 ||
        wxString(wxT("([{}])")).Find(m_text.GetChar(m_paren2)) == -1)
    {
      m_paren1 = m_paren2 = -1;
      return ;
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

  while (m_paren1 >= 0 && m_paren1 < (int)m_text.Length())
  {
    if (m_text.GetChar(m_paren1) == second)
      depth--;
    else if (m_text.GetChar(m_paren1) == first)
      depth++;

    if (depth == 0)
      break;
    m_paren1 += dir;
  }

  if (m_paren1 < 0 || m_paren1 >= (int)m_text.Length())
    m_paren1 = m_paren2 = -1;
}

#if wxUSE_UNICODE
wxString EditorCell::InterpretEscapeString(wxString txt)
{
  long int unicodeval = -1;

  if ((txt == wxT("a")) || (txt == wxT("alpha")))
    return L"\x03B1";
  else if ((txt == wxT("b")) || (txt == wxT("beta")))
    return L"\x03B2";
  else if ((txt == wxT("g")) || (txt == wxT("gamma")))
    return L"\x03B3";
  else if ((txt == wxT("d")) || (txt == wxT("delta")))
    return L"\x03B4";
  else if ((txt == wxT("e")) || (txt == wxT("epsilon")))
    return L"\x03B5";
  else if ((txt == wxT("z")) || (txt == wxT("zeta")))
    return L"\x03B6";
  else if ((txt == wxT("h")) || (txt == wxT("eta")))
    return L"\x03B7";
  else if ((txt == wxT("q")) || (txt == wxT("theta")))
    return L"\x03B8";
  else if ((txt == wxT("i")) || (txt == wxT("iota")))
    return L"\x03B9";
  else if ((txt == wxT("k")) || (txt == wxT("kappa")))
    return L"\x03BA";
  else if ((txt == wxT("l")) || (txt == wxT("lambda")))
    return L"\x03BB";
  else if ((txt == wxT("m")) || (txt == wxT("mu")))
    return L"\x03BC";
  else if ((txt == wxT("n")) || (txt == wxT("nu")))
    return L"\x03BD";
  else if ((txt == wxT("x")) || (txt == wxT("xi")))
    return L"\x03BE";
  else if ((txt == wxT("om")) || (txt == wxT("omicron")))
    return L"\x03BF";
  else if ((txt == wxT("p")) || (txt == wxT("pi")))
    return L"\x03C0";
  else if ((txt == wxT("r")) || (txt == wxT("rho")))
    return L"\x03C1";
  else if ((txt == wxT("s")) || (txt == wxT("sigma")))
    return L"\x03C3";
  else if ((txt == wxT("t")) || (txt == wxT("tau")))
    return L"\x03C4";
  else if ((txt == wxT("u")) || (txt == wxT("upsilon")))
    return L"\x03C5";
  else if ((txt == wxT("f")) || (txt == wxT("phi")))
    return L"\x03C6";
  else if ((txt == wxT("c")) || (txt == wxT("chi")))
    return L"\x03C7";
  else if ((txt == wxT("y")) || (txt == wxT("psi")))
    return L"\x03C8";
  else if ((txt == wxT("o")) || (txt == wxT("omega")))
    return L"\x03C9";
  else if ((txt == wxT("A")) || (txt == wxT("Alpha")))
    return L"\x0391";
  else if ((txt == wxT("B")) || (txt == wxT("Beta")))
    return L"\x0392";
  else if ((txt == wxT("G")) || (txt == wxT("Gamma")))
    return L"\x0393";
  else if ((txt == wxT("D")) || (txt == wxT("Delta")))
    return L"\x0394";
  else if ((txt == wxT("E")) || (txt == wxT("Epsilon")))
    return L"\x0395";
  else if ((txt == wxT("Z")) || (txt == wxT("Zeta")))
    return L"\x0396";
  else if ((txt == wxT("H")) || (txt == wxT("Eta")))
    return L"\x0397";
  else if ((txt == wxT("T")) || (txt == wxT("Theta")))
    return L"\x0398";
  else if ((txt == wxT("I")) || (txt == wxT("Iota")))
    return L"\x0399";
  else if ((txt == wxT("K")) || (txt == wxT("Kappa")))
    return L"\x039A";
  else if ((txt == wxT("L")) || (txt == wxT("Lambda")))
    return L"\x039B";
  else if ((txt == wxT("M")) || (txt == wxT("Mu")))
    return L"\x039C";
  else if ((txt == wxT("N")) || (txt == wxT("Nu")))
    return L"\x039D";
  else if ((txt == wxT("X")) || (txt == wxT("Xi")))
    return L"\x039E";
  else if ((txt == wxT("Om")) || (txt == wxT("Omicron")))
    return L"\x039F";
  else if ((txt == wxT("P")) || (txt == wxT("Pi")))
    return L"\x03A0";
  else if ((txt == wxT("R")) || (txt == wxT("Rho")))
    return L"\x03A1";
  else if ((txt == wxT("S")) || (txt == wxT("Sigma")))
    return L"\x03A3";
  else if ((txt == wxT("T")) || (txt == wxT("Tau")))
    return L"\x03A4";
  else if ((txt == wxT("U")) || (txt == wxT("Upsilon")))
    return L"\x03A5";
  else if ((txt == wxT("P")) || (txt == wxT("Phi")))
    return L"\x03A6";
  else if ((txt == wxT("C")) || (txt == wxT("Chi")))
    return L"\x03A7";
  else if ((txt == wxT("Y")) || (txt == wxT("Psi"))) 
    return L"\x03A8";
  else if ((txt == wxT("O")) || (txt == wxT("Omega")))
    return L"\x03A9";
  //////////////////////////
  else if (txt == wxT("2"))
    return L"\x00B2";
  else if (txt == wxT("3"))
    return L"\x00B3";
  else if (txt == wxT("/2"))
    return L"\x00BD";
  else if (txt == wxT("sq"))
    return L"\x221A";
  else if (txt == wxT("ii"))
    return L"\x2148";
  else if (txt == wxT("ee"))
    return L"\x2147";
  else if (txt == wxT("hb"))
    return L"\x210F";
  else if (txt == wxT("in"))
    return L"\x2208";
  else if (txt == wxT("impl"))
    return L"\x21D2";
  else if (txt == wxT("inf"))
    return L"\x221e";
  else if (txt == wxT("empty"))
    return L"\x2205";
  else if (txt == wxT("TB"))
    return L"\x25b6";
  else if (txt == wxT("tb"))
    return L"\x25b8";
  else if (txt == wxT("and"))
    return L"\x22C0";
  else if (txt == wxT("or"))
    return L"\x22C1";
  else if (txt == wxT("xor"))
    return L"\x22BB";
  else if (txt == wxT("nand"))
    return L"\x22BC";
  else if (txt == wxT("nor"))
    return L"\x22BD";
  else if (txt == wxT("implies") || txt == wxT("=>"))
    return L"\x21D2";
  else if (txt == wxT("equiv") || txt == wxT("<=>"))
    return L"\x21D4";
  else if (txt == wxT("not"))
    return L"\x00AC";
  else if (txt == wxT("union"))
    return L"\x22C3";
  else if (txt == wxT("inter"))
    return L"\x22C2";
  else if (txt == wxT("subseteq"))
    return L"\x2286";
  else if (txt == wxT("subset"))
    return L"\x2282";
  else if (txt == wxT("notsubseteq"))
    return L"\x2288";
  else if (txt == wxT("notsubset"))
    return L"\x2284";
  else if (txt == wxT("hbar"))
    return L"\x0127";
  else if (txt == wxT("Hbar"))
    return L"\x0126";
  else if (txt == wxT("partial"))
    return L"\x2202";
  else if (txt == wxT("integral"))
    return L"\x222b";
  else if (txt == wxT("approx"))
    return L"\x2245";
  else if (txt == wxT("prop"))
    return L"\x221d";
  else if (txt == wxT("propto"))
    return L"\x221d";
  else if (txt == wxT("neq"))
    return L"\x2260";
  else if (txt == wxT("qed"))
    return L"\x220E";
  else if (txt == wxT("equiv"))
    return L"\2263";

  /////////////////////////
  else if (txt.ToLong(&unicodeval, 16))
  {
    if(unicodeval>=32)
      return wxString(wxChar(unicodeval));
    else
      return wxT(" ");
        
  }

  /////////////////////////
  else
    return wxEmptyString;
}
#endif

bool EditorCell::ActivateCell()
{
  m_isActive = !m_isActive;
  if (m_isActive)
    SaveValue();
  m_displayCaret = true;
  m_hasFocus = true;

  ClearSelection();
  m_paren1 = m_paren2 = -1;

  // upon activation unhide the parent groupcell
  if (m_isActive) {
    m_firstLineOnly = false;
    ((GroupCell *)GetParent())->Hide(false);
    if (GetType() == MC_TYPE_INPUT)
      FindMatchingParens();
  }

  return true;
}

bool EditorCell::AddEnding()
{
  if (m_text.Left(5) == wxT(":lisp"))
    return false;

  wxString text = m_text.Trim();
  if (text.Right(1) != wxT(";") && text.Right(1) != wxT("$")) {
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
void EditorCell::PositionToXY(int position, int* x, int* y)
{
  int col = 0, lin = 0;
  int pos = 0;

  while (pos < position)
  {
    if (m_text.GetChar(pos) == '\n')
    {
      col = 0,
      lin++;
    }
    else
      col++;
    pos++;
  }

  *x = col;
  *y = lin;
}

int EditorCell::XYToPosition(int x, int y)
{
  int col = 0, lin = 0, pos = 0;

  while (pos < (int)m_text.Length() && lin < y)
  {
    if (m_text.GetChar(pos) == '\n')
      lin++;
    pos++;
  }

  while (pos < (int)m_text.Length() && col < x)
  {
    if (m_text.GetChar(pos) == '\n')
      break;
    pos++;
    col++;
  }

  return pos;
}

wxPoint EditorCell::PositionToPoint(CellParser& parser, int pos)
{
  wxDC& dc = parser.GetDC();
  SetFont(parser, m_fontSize);

  int x = m_currentPoint.x, y = m_currentPoint.y;
  int height, width;
  int cX, cY;
  wxString line = wxEmptyString;

  if (pos == -1)
    pos = m_positionOfCaret;

  if (x == -1 || y == -1)
    return wxPoint(-1, -1);

  PositionToXY(pos, &cX, &cY);

  width = GetLineWidth(dc, cY, cX);
  
  x += width;
  y += m_charHeight * cY;

  return wxPoint(x, y);
}

void EditorCell::SelectPointText(wxDC& dc, wxPoint& point)
{
  wxString s;
  int fontsize1 = m_fontSize;

  dc.SetFont(wxFont(fontsize1, wxFONTFAMILY_MODERN,
                    m_fontStyle,
                    m_fontWeight,
                    m_underlined,
                    m_fontName,
                    m_fontEncoding));

  ClearSelection();
  wxPoint translate(point);

  translate.x -= m_currentPoint.x - 2;
  translate.y -= m_currentPoint.y - 2 - m_center;

  int lin = translate.y / m_charHeight;
  int width, height;
  int lineStart = XYToPosition(0, lin);
  m_positionOfCaret = lineStart;

  while (m_positionOfCaret < (signed)m_text.Length() && m_text.GetChar(m_positionOfCaret) != '\n')
  {
    s = m_text.SubString(lineStart, m_positionOfCaret);
    dc.GetTextExtent(m_text.SubString(lineStart, m_positionOfCaret),
                                      &width, &height);
    if (width > translate.x)
      break;

    m_positionOfCaret++;
  }

  m_positionOfCaret = MIN(m_positionOfCaret, (signed)m_text.Length());

  m_displayCaret = true;
  m_caretColumn = -1;
  if (GetType() == MC_TYPE_INPUT)
    FindMatchingParens();
}

void EditorCell::SelectRectText(wxDC &dc, wxPoint& one, wxPoint& two)
{
  SelectPointText(dc, one);
  long start = m_positionOfCaret;
  SelectPointText(dc, two);
  SetSelection(start,m_positionOfCaret);
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
bool EditorCell::IsPointInSelection(wxDC& dc, wxPoint point)
{
  if ((m_selectionStart == -1) || (m_selectionEnd == -1) || (m_isActive == false))
    return false;

  wxRect rect = GetRect();
  if (!rect.Contains(point))
    return false;

  wxString s;
  int fontsize1 = m_fontSize;

  dc.SetFont(wxFont(fontsize1, wxFONTFAMILY_MODERN,
                    m_fontStyle,
                    m_fontWeight,
                    m_underlined,
                    m_fontName,
                    m_fontEncoding));
  wxPoint translate(point);
  translate.x -= m_currentPoint.x - 2;
  translate.y -= m_currentPoint.y - 2 - m_center;
  int lin = translate.y / m_charHeight;
  int width, height;
  int lineStart = XYToPosition(0, lin);
  int positionOfCaret = lineStart;
  while (m_text.GetChar(positionOfCaret) != '\n' && positionOfCaret < (signed)m_text.Length())
  {
    s = m_text.SubString(lineStart, positionOfCaret);
    dc.GetTextExtent(m_text.SubString(lineStart, positionOfCaret),
                                      &width, &height);
    if (width > translate.x)
      break;
    positionOfCaret++;
  }
  positionOfCaret = MIN(positionOfCaret, (signed)m_text.Length());

  if ((m_selectionStart >= positionOfCaret) || (m_selectionEnd <= positionOfCaret))
    return false;

  return true;
}

// DivideAtCaret
// returns the string from caret to end and
// modifies the m_text so it contains only the string
// from beginning to caret
// Used for 'Divide Cell', called from MathCtrl
wxString EditorCell::DivideAtCaret()
{
  wxString original = m_text;
  m_containsChanges = true;
  SetValue(m_text.SubString(0, m_positionOfCaret - 1));
  ResetSize();
  GetParent()->ResetSize();
  return original.SubString(m_positionOfCaret, original.Length());
}


void EditorCell::SetSelection(int start, int end)
{
  if((start != m_oldSelectionStart)||(end != m_oldSelectionEnd))
  {
    m_oldSelectionStart = start;
    m_oldSelectionEnd   = end;
    m_selectionChanged  = true;
    m_selectionStart    = start;
    m_positionOfCaret   = m_selectionEnd = end;
    if (m_selectionStart == -1 || m_selectionEnd == -1)
      m_selectionString = wxEmptyString;
    else
      m_selectionString = m_text.SubString(
        MIN(m_selectionStart, m_selectionEnd),
        MAX(m_selectionStart, m_selectionEnd) - 1
        );
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
  m_positionOfCaret = MIN(m_selectionEnd + 4, (signed)m_text.Length());
  ClearSelection();
}

/***
 * SelectWordUnderCaret
 * - called from MathCtrl::OnDoubleClick, MathCtrl::Autocomplete and wxMaxima::HelpMenu
 * Selects word under cursor (aA-zZ, 0-9, %, _, count) or
 * the inside of brackets using m_paren1 and m_paren2 if available and selectParens is true.
 * Returns the selected string if selected a word successfully - used for F1 help and
 * MathCtrl::Autocomplete.
 */

wxString EditorCell::SelectWordUnderCaret(bool selectParens, bool toRight)
{
  if (selectParens && (m_paren1 != -1) && (m_paren2 != -1)) {
    SetSelection(MIN(m_paren1,m_paren2) + 1,MAX(m_paren1, m_paren2));
    m_positionOfCaret = m_selectionEnd;
    return wxT("%");
  }

  long left = m_positionOfCaret, right = m_positionOfCaret;
  while (left > 0)
  {
    if (!IsAlphaNum(m_text.GetChar(left-1)))
      break;
    left--;
  }

  if (toRight)
  {
    while (right < (signed)m_text.length() )
    {
      if(!IsAlphaNum(m_text.GetChar(right)))
        break;
      right++;
    }
  }

  SetSelection(left,right);
  m_positionOfCaret = m_selectionEnd;
  if (left != right)
    return m_selectionString;
  else
    return wxString(wxT("%"));
}

bool EditorCell::CopyToClipboard()
{
  if (m_selectionStart == -1)
    return false;
  if (wxTheClipboard->Open())
  {
    long start = MIN(m_selectionStart, m_selectionEnd);
    long end = MAX(m_selectionStart, m_selectionEnd) - 1;
    wxString s = m_text.SubString(start, end);

    wxTheClipboard->SetData(new wxTextDataObject(s));
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

  long start = MIN(m_selectionStart, m_selectionEnd);
  long end = MAX(m_selectionStart, m_selectionEnd);
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
    SetSelection(m_positionOfCaret,m_positionOfCaret);

  text = TabExpand(text,m_positionOfCaret - BeginningOfLine(m_positionOfCaret));

  ReplaceSelection(
    GetSelectionString(),
    text
    );

  if (GetType() == MC_TYPE_INPUT)
    FindMatchingParens();

  m_width = m_height = m_maxDrop = m_center = -1;

}

void EditorCell::PasteFromClipboard(bool primary)
{
  if (primary)
    wxTheClipboard->UsePrimarySelection(true);
  if (wxTheClipboard->Open())
  {
    if (wxTheClipboard->IsSupported(wxDF_TEXT))
    {
      wxTextDataObject obj;
      wxTheClipboard->GetData(obj);
      InsertText(obj.GetText());
    }
    wxTheClipboard->Close();
  }
  if (primary)
    wxTheClipboard->UsePrimarySelection(false);
}

int EditorCell::GetLineWidth(wxDC& dc, int line, int pos)
{
  if (pos == 0)
    return 0;

  int i = 0;
  
  std::list<StyledText> styledText = m_styledText;
  
  while(!styledText.empty() && i<line)
  {
    // Grab a portion of text from the list.
    StyledText textSnippet = styledText.front();
    styledText.pop_front();
    wxString text = textSnippet.GetText();
    if (text.Right(1) == '\n')
      i++;
  }

  if (i<line)
    return 0;

  int width = 0;
  wxString text;
  int textWidth, textHeight;
  pos--;
  while (!styledText.empty() && pos>=0)
  {
    StyledText textSnippet = styledText.front();
    styledText.pop_front();
    text = textSnippet.GetText();
    dc.GetTextExtent(text, &textWidth, &textHeight);
    width += textWidth;
    pos -= text.Length();
  }

  if (pos<0) {
    width -= textWidth;
    dc.GetTextExtent(text.SubString(0, text.Length() + pos), &textWidth, &textHeight);
    width += textWidth;
  }

  return width;
}


bool EditorCell::CanUndo()
{
  return m_textHistory.GetCount()>0 && m_historyPosition != 0;
}

void EditorCell::Undo()
{
  if (m_historyPosition == -1) {
    m_historyPosition = m_textHistory.GetCount()-1;
    m_textHistory.Add(m_text);
    m_startHistory.push_back(m_selectionStart);
    m_endHistory.push_back(m_selectionEnd);
    m_positionHistory.push_back(m_positionOfCaret);
  }
  else
    m_historyPosition--;

  if (m_historyPosition == -1)
    return ;

  // We cannot use SetValue() here, since SetValue() tends to move the cursor.
  m_text = m_textHistory.Item(m_historyPosition);
  StyleText();
  
  m_positionOfCaret = m_positionHistory[m_historyPosition];
  SetSelection(m_startHistory[m_historyPosition],m_endHistory[m_historyPosition]);

  m_paren1 = m_paren2 = -1;
  m_isDirty = true;
  m_width = m_height = m_maxDrop = m_center = -1;
}


bool EditorCell::CanRedo()
{
  return m_textHistory.GetCount()>0 &&
    m_historyPosition >= 0 &&
    m_historyPosition < m_textHistory.GetCount()-1;
}

void EditorCell::Redo()
{
  if (m_historyPosition == -1)
    return;

  m_historyPosition++;

  if (m_historyPosition >= m_textHistory.GetCount())
    return ;

  // We cannot use SetValue() here, since SetValue() tends to move the cursor.
  m_text = m_textHistory.Item(m_historyPosition);
  StyleText();
  
  m_positionOfCaret = m_positionHistory[m_historyPosition];
  SetSelection(m_startHistory[m_historyPosition],m_endHistory[m_historyPosition]);

  m_paren1 = m_paren2 = -1;
  m_isDirty = true;
  m_width = m_height = m_maxDrop = m_center = -1;
}


void EditorCell::SaveValue()
{
  if (m_textHistory.GetCount()>0) {
    if (m_textHistory.Last() == m_text)
      return ;
  }

  if (m_historyPosition != -1) {
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

bool EditorCell::IsAlpha(wxChar ch)
{
  static const wxString alphas = wxT("\\_%");

  if (wxIsalpha(ch))
    return true;

  if (alphas.Find(ch) != wxNOT_FOUND)
    return true;

  return false;
}

bool EditorCell::IsNum(wxChar ch)
{
  if (ch >= '0' && ch <= '9')
    return true;

  return false;
}

bool EditorCell::IsAlphaNum(wxChar ch)
{
  if (IsAlpha(ch) || IsNum(ch))
    return true;

  return false;
}

wxArrayString EditorCell::StringToTokens(wxString string)
{
  size_t size=string.Length();
  size_t pos=0;
  wxArrayString retval;
  wxString token;

  static wxString numSeps = wxT("bBdDeEfF");

  while(pos<size)
  {
    wxChar Ch = string.GetChar(pos);

    // Check for new line
    if(Ch==wxT('\n'))
    {
      if(token != wxEmptyString) {
        retval.Add(token + wxT("d"));
        token = wxEmptyString;
      }
      retval.Add(wxT("\nd"));
      pos++;
    }
    // A minus and a plus are special tokens as they can be both
    // operators or part of a number.
    else if (
      (Ch==wxT('+')) ||
      (Ch==wxT('-'))||
      (Ch==wxT('\x2212')) // An unicode minus sign
      )
    {
      if(token != wxEmptyString)
        retval.Add(token + wxT("d"));
      token=wxString(Ch);
      retval.Add(token + wxT("d"));
      pos++;
      token = wxEmptyString;
    }
    // Check for comment
    else if ((string.Length() > pos+1) &&
        ((Ch == '/' && string.GetChar(pos+1) == '*') ||
         (Ch == '*' && string.GetChar(pos+1) == '/')))
    {
      if(token != wxEmptyString) {
        retval.Add(token + wxT("d"));
        token = wxEmptyString;
      }
      retval.Add(string.SubString(pos, pos+1) + wxT("d"));
      pos = pos+2;
    }
    
    // Find operators that starts at the current position
    else if (operators.Find(Ch) != wxNOT_FOUND)
    {
      if(token != wxEmptyString) {
        retval.Add(token + wxT("d"));
        token = wxEmptyString;
      }
      retval.Add(wxString(string.GetChar(pos++)) + wxT("d"));
    }
    
    // Find a keyword that starts at the current position
    else if ((IsAlpha(Ch)) || (Ch == wxT('\\')))
    {
      if(token != wxEmptyString) {
        retval.Add(token + wxT("d"));
        token=wxEmptyString;
      }
      
      while((pos<size) && IsAlphaNum(string.GetChar(pos)))
      {
        if(string.GetChar(pos) == wxT('\\'))
        {
          token += string.GetChar(pos);
          pos++;
        }
        token += string.GetChar(pos);
        pos++;
      }
      
      retval.Add(token + wxT("d"));
      token = wxEmptyString;
    }
    
    // Find a number that starts at the current positions
    else if (IsNum(Ch))
    {
      if(token != wxEmptyString) {
        retval.Add(token + wxT("d"));
        token=wxEmptyString;
      }
            
      while((pos<size) &&
            (IsNum(string.GetChar(pos)) ||
             numSeps.Find(string.GetChar(pos)) != wxNOT_FOUND))
      {
        token += string.GetChar(pos);
        pos++;
      }
      
      retval.Add(token + wxT("d"));
      token=wxEmptyString;
    }
    else
      token = token + string.GetChar(pos++);
  }
  
  // Add the last token we detected to the token list
  retval.Add(token + wxT("d"));
  
  return retval;
}

void EditorCell::StyleText()
{
  m_styledText.clear();

  if(m_type == MC_TYPE_INPUT)
  {
    wxString textToStyle = m_text;

    if(m_firstLineOnly)
    {
      size_t newlinepos = textToStyle.find(wxT("\n"));
      if(newlinepos != wxNOT_FOUND)
      {
        textToStyle = textToStyle.Left(newlinepos) +
          wxString::Format(wxT(" ... + %i hidden lines"), textToStyle.Freq(wxT('\n')));
      }
    }
    
    wxArrayString tokens = StringToTokens(textToStyle);

    wxString lastTokenWithText;
    for(size_t i=0;i<tokens.GetCount();i++)
    {
      wxString token = tokens[i];
      token = token.Left(token.Length()-1);
      wxChar Ch = token[0];

      // Save the last non-whitespace character in lastChar -
      // or a space if there is no such char.
      wxChar lastChar=wxT(' ');
      if(lastTokenWithText!=wxEmptyString)
            lastChar=lastTokenWithText.Right(1)[0];
      wxString tmp = token;
      tmp=tmp.Trim();
      if(tmp!=wxEmptyString)
        lastTokenWithText = tmp;
      
      // Save the next non-whitespace character in lastChar -
      // or a space if there is no such char.
      wxChar nextChar=wxT(' ');
      size_t o = i+1;
      while(o<tokens.GetCount())
      {
        wxString nextToken=tokens[o];
        nextToken=nextToken.Trim(false);
        if(nextToken!=wxT("d"))
        {
          nextChar=nextToken[0];
          break;
        }
        o++;
      }

      // Handle comments
      if(token == wxT("\""))
      {
        m_styledText.push_back(StyledText(TS_CODE_STRING,token));
        if (i+1<tokens.GetCount()) {
          i++;
          token = tokens[i];
          token = token.Left(token.Length()-1);
          m_styledText.push_back(StyledText(TS_CODE_STRING,token));
          while ((i+1 < tokens.GetCount()) && token != wxT("\"")) {
            i++;
            token = tokens[i];
            token = token.Left(token.Length()-1);
            m_styledText.push_back(StyledText(TS_CODE_STRING,token));
          }
        }
        continue;
      }

      if((Ch==wxT('+')) ||
         (Ch==wxT('-'))||
         (Ch==wxT('\x2212'))
        )
      {
        if(
          (nextChar>=wxT('0')) &&
          (nextChar<=wxT('9'))
          )
        {
          // Our sign precedes a number.
          if(
            (wxIsalnum(lastChar)) ||
            (lastChar==wxT('%'))  ||
            (lastChar==wxT(')'))  ||
            (lastChar==wxT('}'))  ||
            (lastChar==wxT(']'))
            )
          {
            m_styledText.push_back(StyledText(TS_CODE_OPERATOR,token));
          }
          else
          {
            m_styledText.push_back(StyledText(TS_CODE_NUMBER,token));
          }
        }
        else
            m_styledText.push_back(StyledText(TS_CODE_OPERATOR,token));
        continue;
      }

      if(token == wxT("/*"))
      {
        m_styledText.push_back(StyledText(TS_CODE_COMMENT,token));
        while ((i+1 < tokens.GetCount()) && (token != wxT("*/"))) {
          i++;
          token = tokens[i];
          token = token.Left(token.Length()-1);
          m_styledText.push_back(StyledText(TS_CODE_COMMENT,token));
        }
        continue;
      }
      
      if(operators.Find(token) != wxNOT_FOUND)
      {
        if((token==wxT('$'))||(token==wxT(';')))
          m_styledText.push_back(StyledText(TS_CODE_ENDOFLINE,token));
        else
          m_styledText.push_back(StyledText(TS_CODE_OPERATOR,token));
        continue;
      }
      if(isdigit(token[0]))
      {
        m_styledText.push_back(StyledText(TS_CODE_NUMBER,token));
        continue;
      }
      if((IsAlpha(token[0])) || (token[0] == wxT('\\')))
      {
        // Sometimes we can differ between variables and functions by the context.
        // But I assume there cannot be an algorithm that always makes
        // the right decision here:
        //  - Function names can be used without the parenthesis that make out
        //    functions.
        //  - The same name can stand for a function and a variable
        //  - There are indexed functions
        //  - using lambda a user can store a function in a variable
        //  - and is U_C1(t) really meant as a function or does it represent a variable
        //    named U_C1 that depends on t?
        if((tokens.GetCount()>i+1))
        {
          wxString nextToken = tokens[i+1];
          nextToken=nextToken.Trim(false);

          if (token == wxT("for")    ||
              token == wxT("in")     ||
              token == wxT("while")  ||
              token == wxT("do")     ||
              token == wxT("thru")   ||
              token == wxT("next")   ||
              token == wxT("step")   ||
              token == wxT("unless") ||
              token == wxT("from")   ||
              token == wxT("if")     ||
              token == wxT("else")   ||
              token == wxT("elif")   ||
              token == wxT("and")    ||
              token == wxT("or")     ||
              token == wxT("not")    ||
              token == wxT("true")   ||
              token == wxT("false"))
            m_styledText.push_back(token);
          else if(nextChar==wxT('('))
            m_styledText.push_back(StyledText(TS_CODE_FUNCTION,token));
          else
            m_styledText.push_back(StyledText(TS_CODE_VARIABLE,token));
          continue;
        }
        else
          m_styledText.push_back(StyledText(TS_CODE_VARIABLE,token));
      }
      m_styledText.push_back(StyledText(token));
    }
  }
  else {
    wxString token;
    for (size_t i = 0; i<m_text.Length(); i++) {
      if (m_text.GetChar(i) == '\n') {
        m_styledText.push_back(StyledText(token));
        m_styledText.push_back(StyledText(wxT("\n")));
        token = wxEmptyString;
      }
      else
        token += m_text.GetChar(i);
    }
    m_styledText.push_back(StyledText(token));
  }
}


void EditorCell::SetValue(wxString text)
{
  if (m_type == MC_TYPE_INPUT)
  {
    if (m_matchParens)
    {
      if (text == wxT("(")) {
        m_text = wxT("()");
        m_positionOfCaret = 1;
      }
      else if (text == wxT("[")) {
        m_text = wxT("[]");
        m_positionOfCaret = 1;
      }
      else if (text == wxT("{")) {
        m_text = wxT("{}");
        m_positionOfCaret = 1;
      }
      else if (text == wxT("\"")) {
        m_text = wxT("\"\"");
        m_positionOfCaret = 1;
      }
      else {
        m_text = text;
        m_positionOfCaret = m_text.Length();
      }
    }
    else {
      m_text = text;
      m_positionOfCaret = m_text.Length();
    }

    if (m_insertAns)
    {
      if (m_text == wxT("+") ||
          m_text == wxT("*") ||
          m_text == wxT("/") ||
          m_text == wxT("^") ||
          m_text == wxT("=") ||
          m_text == wxT(","))
      {
        m_text = wxT("%") + m_text;
        m_positionOfCaret = m_text.Length();
      }
    }
  }
  else
  {
    m_text = text;
    m_positionOfCaret = m_text.Length();
  }

  FindMatchingParens();
  m_containsChanges = true;

  // Style the text.
  StyleText();
}

bool EditorCell::CheckChanges()
{
  if (m_containsChanges != m_containsChangesCheck) {
    m_containsChangesCheck = m_containsChanges;
    return true;
  }

  return false;
}

int EditorCell::ReplaceAll(wxString oldString, wxString newString,bool IgnoreCase)
{
  SaveValue();
  int count = m_text.Replace(oldString, newString);
  if (count > 0)
  {
    m_containsChanges = true;
    ClearSelection();
  }
  StyleText();

  // If text is selected setting the selection again updates m_selectionString
  if(m_selectionStart >0)
    SetSelection(m_selectionStart,m_selectionEnd);
  
  return count;
}

bool EditorCell::FindNext(wxString str, bool down, bool ignoreCase)
{
  int start = down ? 0 : m_text.Length();
  wxString text(m_text);

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
      start = m_selectionStart - 1;
  }
  else if (m_isActive)
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
    SetSelection(strStart,strStart + str.Length());
    return true;
  }
  return false;
}

bool EditorCell::ReplaceSelection(wxString oldStr, wxString newStr, bool keepSelected, bool IgnoreCase)
{
  long start = MIN(m_selectionStart, m_selectionEnd);
  long end = MAX(m_selectionStart, m_selectionEnd);
  if (m_selectionStart <0)
  {
    if(oldStr == wxEmptyString)
      SetSelection(m_positionOfCaret,m_positionOfCaret);
    else
      return false;
  }
    
  if(IgnoreCase)
    {
      if ( m_text.SubString(start,end - 1).Upper() !=
           wxString(oldStr).Upper()
        )
        return false;
    }
    else
    {
      if ( m_text.SubString(start,end - 1) != oldStr)
        return false;
    }
  
  {
    // We cannot use SetValue() here, since SetValue() tends to move the cursor.
    m_text = m_text.SubString(0, start - 1) +
             newStr +
             m_text.SubString(end, m_text.Length());
    StyleText();
    
    m_containsChanges = true;
    m_positionOfCaret = start + newStr.Length();

    if(keepSelected)
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
  if(m_selectionStart>=0)
    return m_selectionString;
  else
    return wxEmptyString;
}

void EditorCell::ClearSelection()
{
  if(SelectionActive())
  {
    m_selectionChanged = true;
    m_selectionString = wxEmptyString;
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
    varsRegex.Compile(wxT("(<[^> \n]+>)[^>]*$"));
  else
    varsRegex.Compile(wxT("(<[^> \n]+>)"));

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
      SetSelection(start,m_selectionEnd);
    }
    else
      m_positionOfCaret = m_selectionStart = positionOfCaret + start;
    SetSelection(m_selectionStart,m_selectionStart + length);
    return true;
  }

  // Then in the rest of the string
  if (varsRegex.Matches(second))
  {
    varsRegex.GetMatch(&start, &length, 1);
    if (!left)
    {
      m_positionOfCaret = start;
      SetSelection(start,m_selectionEnd);
    }
    else
      m_positionOfCaret = m_selectionStart = positionOfCaret + start;
    SetSelection(m_selectionStart,m_selectionStart + length);
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
