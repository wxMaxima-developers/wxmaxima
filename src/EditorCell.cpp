///
///  Copyright (C) 2006-2011 Andrej Vodopivec <andrej.vodopivec@gmail.com>
///            (C) 2012 Doug Ilijev <doug.ilijev@gmail.com>
///
///  This program is free software; you can redistribute it and/or modify
///  it under the terms of the GNU General Public License as published by
///  the Free Software Foundation; either version 2 of the License, or
///  (at your option) any later version.
///
///  This program is distributed in the hope that it will be useful,
///  but WITHOUT ANY WARRANTY; without even the implied warranty of
///  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
///  GNU General Public License for more details.
///
///
///  You should have received a copy of the GNU General Public License
///  along with this program; if not, write to the Free Software
///  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
///

#include <wx/clipbrd.h>
#include <wx/regex.h>

#include "EditorCell.h"
#include "wxMaxima.h"
#include "wxMaximaFrame.h"

#define ESC_CHAR wxT('\xA6')

EditorCell::EditorCell() : MathCell()
{
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
}

EditorCell::~EditorCell()
{
  if (m_next != NULL)
    delete m_next;
}

MathCell *EditorCell::Copy(bool all)
{
  EditorCell *tmp = new EditorCell();
  tmp->SetValue(m_text);
  tmp->m_containsChanges = m_containsChanges;
  CopyData(this, tmp);
  if (all && m_next != NULL)
    tmp->AppendCell(m_next->Copy(all));
  return tmp;
}

void EditorCell::Destroy()
{
  m_next = NULL;
}

wxString EditorCell::ToString(bool all)
{
  wxString text = m_text;

  if (m_selectionStart > -1)
  {
    long start = MIN(m_selectionStart, m_selectionEnd);
    long end = MAX(m_selectionStart, m_selectionEnd) - 1;
    text = m_text.SubString(start, end);
  }

  return text + MathCell::ToString(all);
}

wxString EditorCell::ToTeX(bool all)
{
  wxString text = m_text;
  return text + MathCell::ToTeX(all);
}

wxString EditorCell::ToXML(bool all)
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
      head += wxT(" type=\"title\"");
      break;
    case MC_TYPE_SECTION:
      head += wxT(" type=\"section\"");
      break;
    case MC_TYPE_SUBSECTION:
      head += wxT(" type=\"subsection\"");
      break;
    case MC_TYPE_INPUT:
    default:
      head += wxT(" type=\"input\"");
      break;
  }
  head += wxT(">\n");

  return head + xmlstring + wxT("</editor>\n") + MathCell::ToXML(all);
}

void EditorCell::RecalculateWidths(CellParser& parser, int fontsize, bool all)
{
  m_isDirty = false;
  if (m_height == -1 || m_width == -1 || fontsize != m_fontSize || parser.ForceUpdate())
  {
    m_fontSize = fontsize;
    wxDC& dc = parser.GetDC();
    double scale = parser.GetScale();
    SetFont(parser, fontsize);

    dc.GetTextExtent(wxT("X"), &m_charWidth, &m_charHeight);

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
      width = m_charWidth;

    m_width = width + 2 * SCALE_PX(2, scale);
    m_height = m_numberOfLines * m_charHeight + 2 * SCALE_PX(2, scale);

    m_center = m_charHeight / 2 + SCALE_PX(2, scale);
  }
  MathCell::RecalculateWidths(parser, fontsize, all);
}

void EditorCell::RecalculateSize(CellParser& parser, int fontsize, bool all)
{
  MathCell::RecalculateSize(parser, fontsize, all);
}

///////////////////////////
// EditorCell::Draw
// Draws the editor cell in the following order:
// 1. draw selection (wxCOPY), TS_SELECTION color
// 2. mark matching parenthesis (wxCOPY), TS_SELECTION color
// 3. draw text (wxCOPY)
// 4. draw caret (wxCOPY), TS_CURSOR color
////////////////////////////
void EditorCell::Draw(CellParser& parser, wxPoint point1, int fontsize, bool all)
{
  double scale = parser.GetScale();
  wxDC& dc = parser.GetDC();
  wxPoint point(point1);

  if (m_width == -1 || m_height == -1)
    RecalculateWidths(parser, fontsize, false);

  if (DrawThisCell(parser, point) && !m_isHidden)
  {
    dc.SetLogicalFunction(wxCOPY); // opaque (for everything except the caret)

    // Need correct m_currentPoint before we call MathCell::Draw!
    m_currentPoint.x = point.x;
    m_currentPoint.y = point.y;

    if (m_isActive) // draw selection or matching parens
    {
      //
      // Mark selection
      //
      if (m_selectionStart > -1)
      {
#if defined(__WXMAC__)
        wxRect rect = GetRect(); // rectangle representing the cell
        dc.SetPen(wxNullPen); // no border on rectangles
#else
        dc.SetPen(*(wxThePenList->FindOrCreatePen(parser.GetColor(TS_SELECTION), 1, 1)) );
// window linux, set a pen
#endif
        dc.SetBrush( *(wxTheBrushList->FindOrCreateBrush(parser.GetColor(TS_SELECTION))) ); //highlight c.

        wxPoint point, point1;
        long start = MIN(m_selectionStart, m_selectionEnd);
        long end = MAX(m_selectionStart, m_selectionEnd);
        long pos1 = start, pos2 = start;

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
      } // if (m_selectionStart > -1)

      //
      // Matching parens - draw only if we dont have selection
      //
      else if (m_paren1 != -1 && m_paren2 != -1)
      {
#if defined(__WXMAC__)
        wxRect rect = GetRect(); // rectangle representing the cell
        dc.SetPen(wxNullPen); // no border on rectangles
#else
        dc.SetPen(*(wxThePenList->FindOrCreatePen(parser.GetColor(TS_SELECTION), 1, 1))); // window linux, set a pen
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
    SetForeground(parser);
    SetPen(parser);
    SetFont(parser, fontsize);

    unsigned int newLinePos = 0, prevNewLinePos = 0, numberOfLines = 0;
#if defined __WXMSW__ || wxUSE_UNICODE
    if (parser.GetChangeAsterisk())  // replace "*" with centerdot for the time of drawing
      m_text.Replace(wxT("*"), wxT("\xB7"));
#endif
    if (!m_firstLineOnly) // draw whole text
      while (newLinePos < m_text.Length())
      {
        while (newLinePos < m_text.Length())
        {
          if (m_text.GetChar(newLinePos) == '\n')
            break;
          newLinePos++;
        }

        dc.DrawText(m_text.SubString(prevNewLinePos, newLinePos - 1),
            point.x + SCALE_PX(2, scale),
            point.y - m_center + SCALE_PX(2, scale) + m_charHeight * numberOfLines);

        newLinePos++;
        prevNewLinePos = newLinePos;
        numberOfLines++;
      }
    else { // draw only first line (+ some info)
      wxString firstline;
      while (newLinePos < m_text.Length())
      {
        while (newLinePos < m_text.Length())
        {
          if (m_text.GetChar(newLinePos) == '\n')
            break;
          newLinePos++;
        }

        if (numberOfLines == 0)
          firstline = m_text.SubString(0, newLinePos - 1);

        newLinePos++;
        numberOfLines++;
      }
      if (numberOfLines < 1)
        numberOfLines = 1;
      firstline << wxT("... (") << numberOfLines - 1 << wxT(" ") << _("lines hidden") << wxT(")");
      dc.DrawText(firstline,
          point.x + SCALE_PX(2, scale),
          point.y - m_center + SCALE_PX(2, scale));
    }
#if defined __WXMSW__ || wxUSE_UNICODE
    if (parser.GetChangeAsterisk()) // replace centerdot with "*"
      m_text.Replace(wxT("\xB7"), wxT("*"));
#endif
    //
    // Draw the caret
    //
    if (m_displayCaret && m_hasFocus && m_isActive)
    {
      int caretInLine = 0;
      int caretInColumn = 0;

      PositionToXY(m_positionOfCaret, &caretInColumn, &caretInLine);

      wxString line = GetLineString(caretInLine, 0, caretInColumn);
      int lineWidth, lineHeight;
      dc.GetTextExtent(line, &lineWidth, &lineHeight);

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
  MathCell::Draw(parser, point1, fontsize, all);
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

void EditorCell::ProcessEvent(wxKeyEvent &event)
{
  static const wxString chars(wxT("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLNOPQRSTUVWXYZ01234567890_%"));
  static const wxString delim(wxT("()[]{},.;?/*:=&$"));

  if ((event.GetKeyCode() != WXK_DOWN) &&
      (event.GetKeyCode() != WXK_PAGEDOWN) &&
      (event.GetKeyCode() != WXK_PAGEUP) &&
      (event.GetKeyCode() != WXK_UP))
      m_caretColumn = -1; // make caretColumn invalid

  switch (event.GetKeyCode())
  {

  case WXK_LEFT:
    SaveValue();
    if (event.ShiftDown())
    {
      if (m_selectionStart == -1)
        m_selectionEnd = m_selectionStart = m_positionOfCaret;
    }
    else
      m_selectionEnd = m_selectionStart = -1;

    if (event.ControlDown()) {
      if (m_positionOfCaret > 1 &&
          (chars.Find(m_text[m_positionOfCaret-1]) == wxNOT_FOUND &&
              chars.Find(m_text[m_positionOfCaret-2]) == wxNOT_FOUND &&
              delim.Find(m_text[m_positionOfCaret-1]) == wxNOT_FOUND &&
	      delim.Find(m_text[m_positionOfCaret-2]) == wxNOT_FOUND)) {
        while (m_positionOfCaret > 0 &&
              chars.Find(m_text[m_positionOfCaret-1]) == wxNOT_FOUND &&
              delim.Find(m_text[m_positionOfCaret-1]) == wxNOT_FOUND)
          m_positionOfCaret--;
      }
      else {
        while (m_positionOfCaret > 0 &&
            chars.Find(m_text[m_positionOfCaret-1]) == wxNOT_FOUND &&
            delim.Find(m_text[m_positionOfCaret-1]) == wxNOT_FOUND)
          m_positionOfCaret--;
        if (chars.Find(m_text[m_positionOfCaret-1]) != wxNOT_FOUND) {
          while (m_positionOfCaret > 0 && chars.Find(m_text[m_positionOfCaret-1]) != wxNOT_FOUND)
            m_positionOfCaret--;
        }
        else if (delim.Find(m_text[m_positionOfCaret-1]) != wxNOT_FOUND) {
          while (m_positionOfCaret > 0 && delim.Find(m_text[m_positionOfCaret-1]) != wxNOT_FOUND)
            m_positionOfCaret--;
        }
        else {
          while (m_positionOfCaret > 0 &&
              chars.Find(m_text[m_positionOfCaret-1]) != wxNOT_FOUND &&
              delim.Find(m_text[m_positionOfCaret-1]) != wxNOT_FOUND)
            m_positionOfCaret--;
        }
      }
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
      m_selectionEnd = m_positionOfCaret;

    break;

  case WXK_RIGHT:
    SaveValue();
    if (event.ShiftDown())
    {
      if (m_selectionStart == -1)
        m_selectionEnd = m_selectionStart = m_positionOfCaret;
    }
    else
      m_selectionEnd = m_selectionStart = -1;

    if (event.ControlDown() && m_positionOfCaret < (signed)m_text.Length()-1) {
      if (m_positionOfCaret < (signed)m_text.Length()-1 &&
          (chars.Find(m_text[m_positionOfCaret]) == wxNOT_FOUND &&
              chars.Find(m_text[m_positionOfCaret+1]) == wxNOT_FOUND &&
              delim.Find(m_text[m_positionOfCaret]) == wxNOT_FOUND &&
	      delim.Find(m_text[m_positionOfCaret+1]) == wxNOT_FOUND)) {
        while (m_positionOfCaret < (signed)m_text.Length() &&
              chars.Find(m_text[m_positionOfCaret]) == wxNOT_FOUND &&
              delim.Find(m_text[m_positionOfCaret]) == wxNOT_FOUND)
          m_positionOfCaret++;
      }
      else {
        while (m_positionOfCaret < (signed)m_text.Length() &&
            chars.Find(m_text[m_positionOfCaret]) == wxNOT_FOUND &&
            delim.Find(m_text[m_positionOfCaret]) == wxNOT_FOUND)
          m_positionOfCaret++;
        if (chars.Find(m_text[m_positionOfCaret]) != wxNOT_FOUND)
        {
          while (m_positionOfCaret < (signed)m_text.Length() &&
              chars.Find(m_text[m_positionOfCaret]) != wxNOT_FOUND)
            m_positionOfCaret++;
        }
        else if (delim.Find(m_text[m_positionOfCaret]) != wxNOT_FOUND)
        {
          while (m_positionOfCaret < (signed)m_text.Length() &&
              delim.Find(m_text[m_positionOfCaret]) != wxNOT_FOUND)
            m_positionOfCaret++;
        }
        else
        {
          while (m_positionOfCaret < (signed)m_text.Length() &&
              chars.Find(m_text[m_positionOfCaret]) != wxNOT_FOUND &&
              delim.Find(m_text[m_positionOfCaret]) != wxNOT_FOUND)
            m_positionOfCaret++;
        }
      }
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
      m_selectionEnd = m_positionOfCaret;

    break;

  case WXK_PAGEDOWN:
  case WXK_DOWN:
    SaveValue();
    {
      if (event.ShiftDown())
      {
        if (m_selectionStart == -1)
          m_selectionEnd = m_selectionStart = m_positionOfCaret;
      }
      else
        m_selectionEnd = m_selectionStart = -1;

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
        m_selectionEnd = m_positionOfCaret;
    }
    break;

  case WXK_PAGEUP:
  case WXK_UP:
    SaveValue();
    {
      if (event.ShiftDown())
      {
        if (m_selectionStart == -1)
          m_selectionEnd = m_selectionStart = m_positionOfCaret;
      }
      else
        m_selectionEnd = m_selectionStart = -1;

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
        m_selectionEnd = m_positionOfCaret;
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
      m_selectionEnd = m_selectionStart = -1;
    }
    m_text = m_text.SubString(0, m_positionOfCaret - 1) +
             wxT("\n") +
             m_text.SubString(m_positionOfCaret, m_text.Length());
    m_positionOfCaret++;
    m_isDirty = true;
    m_containsChanges = true;
    break;

  case WXK_END:
    SaveValue();
    if (event.ShiftDown())
    {
      if (m_selectionStart == -1)
        m_selectionEnd = m_selectionStart = m_positionOfCaret;
    }
    else
      m_selectionEnd = m_selectionStart = -1;

    if (event.ControlDown())
      m_positionOfCaret = (signed)m_text.Length();
    else
    {
      while (m_positionOfCaret < (signed)m_text.Length() &&
             m_text.GetChar(m_positionOfCaret) != '\n')
        m_positionOfCaret++;
    }

    if (event.ShiftDown())
      m_selectionEnd = m_positionOfCaret;
    break;

  case WXK_HOME:
    SaveValue();
    {
      if (event.ShiftDown())
      {
        if (m_selectionStart == -1)
          m_selectionEnd = m_selectionStart = m_positionOfCaret;
      }
      else
        m_selectionEnd = m_selectionStart = -1;

      if (event.ControlDown())
        m_positionOfCaret = 0;
      else
      {
        int col, lin;
        PositionToXY(m_positionOfCaret, &col, &lin);
        m_positionOfCaret = XYToPosition(0, lin);
      }

      if (event.ShiftDown())
        m_selectionEnd = m_positionOfCaret;
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
      m_selectionEnd = m_selectionStart = -1;
    }
    break;

  case WXK_BACK:
    SaveValue();
    if (m_selectionStart > -1) {
      SaveValue();
      m_saveValue = true;
      m_containsChanges = true;
      m_isDirty = true;
      long start = MIN(m_selectionEnd, m_selectionStart);
      long end = MAX(m_selectionEnd, m_selectionStart);
      m_text = m_text.SubString(0, start - 1) +
               m_text.SubString(end, m_text.Length());
      m_positionOfCaret = start;
      m_selectionEnd = m_selectionStart = -1;
      break;
    }
    else if (m_positionOfCaret > 0)
    {
      m_containsChanges = true;
      m_isDirty = true;

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
    break;

  case WXK_TAB:
    m_isDirty = true;
    if (!FindNextTemplate(event.ShiftDown()))
    {
      m_containsChanges = true;
      {
        if (m_selectionStart > -1) {
          SaveValue();
          long start = MIN(m_selectionEnd, m_selectionStart);
          long end = MAX(m_selectionEnd, m_selectionStart);
          m_text = m_text.SubString(0, start - 1) +
                   m_text.SubString(end, m_text.Length());
          m_positionOfCaret = start;
          m_selectionEnd = m_selectionStart = -1;
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
      m_selectionStart = m_selectionEnd = -1;
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
#if wxUSE_UNICODE
      switch (event.GetUnicodeKey())
#else
      switch (event.GetKeyCode())
#endif
      {
      case '(':
        m_text = m_text.SubString(0, start - 1) +   wxT("(") +
                 m_text.SubString(start, end - 1) + wxT(")") +
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
      m_selectionEnd = m_selectionStart = -1; // reset selection
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
#if wxUSE_UNICODE
        switch (event.GetUnicodeKey())
#else
        switch (event.GetKeyCode())
#endif
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
    m_width = m_maxDrop = -1;

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
  else if (txt == wxT("Alpha"))
    return L"\x0391";
  else if (txt == wxT("Beta"))
    return L"\x0392";
  else if (txt == wxT("Gamma"))
    return L"\x0393";
  else if (txt == wxT("Delta"))
    return L"\x0394";
  else if (txt == wxT("Epsilon"))
    return L"\x0395";
  else if (txt == wxT("Zeta"))
    return L"\x0396";
  else if (txt == wxT("Eta"))
    return L"\x0397";
  else if (txt == wxT("Theta"))
    return L"\x0398";
  else if (txt == wxT("Iota"))
    return L"\x0399";
  else if (txt == wxT("Kappa"))
    return L"\x039A";
  else if (txt == wxT("Lambda"))
    return L"\x039B";
  else if (txt == wxT("Mu"))
    return L"\x039C";
  else if (txt == wxT("Nu"))
    return L"\x039D";
  else if (txt == wxT("Xi"))
    return L"\x039E";
  else if (txt == wxT("Omicron"))
    return L"\x039F";
  else if (txt == wxT("Pi"))
    return L"\x03A0";
  else if (txt == wxT("Rho"))
    return L"\x03A1";
  else if (txt == wxT("Sigma"))
    return L"\x03A3";
  else if (txt == wxT("Tau"))
    return L"\x03A4";
  else if (txt == wxT("Upsilon"))
    return L"\x03A5";
  else if (txt == wxT("Phi"))
    return L"\x03A6";
  else if (txt == wxT("Chi"))
    return L"\x03A7";
  else if (txt == wxT("Psi"))
    return L"\x03A8";
  else if (txt == wxT("Omega"))
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

  /////////////////////////
  else if (txt.ToLong(&unicodeval, 16))
    return wxString::Format(wxT("%c"), unicodeval);

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

  m_selectionEnd = m_selectionStart = -1;
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
  if (cX > 0)
    line = GetLineString(cY, 0, cX);

  dc.GetTextExtent(line, &width, &height);

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

  m_selectionEnd = m_selectionStart = -1;
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
  m_selectionEnd = m_positionOfCaret;
  m_selectionStart = start;
  m_paren2 = m_paren1 = -1;
  m_caretColumn = -1;
  if (m_selectionStart == m_selectionEnd)
  {
    m_selectionStart = -1;
    m_selectionEnd = -1;
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
  m_text = m_text.SubString(0, m_positionOfCaret - 1);
  ResetSize();
  GetParent()->ResetSize();
  return original.SubString(m_positionOfCaret, original.Length());
}

void EditorCell::CommentSelection()
{
  if ((m_selectionStart == -1) || (m_selectionEnd == -1))
    return;
  m_containsChanges = true;
  m_isDirty = true;
  m_text = m_text.SubString(0, m_selectionStart - 1) + wxT("/*")
    + m_text.SubString(m_selectionStart, m_selectionEnd - 1) + wxT("*/")
    + m_text.SubString(m_selectionEnd, m_text.Length());
  m_positionOfCaret = MIN(m_selectionEnd + 4, (signed)m_text.Length());
  m_selectionStart = m_selectionEnd = -1;
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
    m_selectionStart = MIN(m_paren1,m_paren2) + 1;
    m_selectionEnd = MAX(m_paren1, m_paren2);
    m_positionOfCaret = m_selectionEnd;
    return wxT("%");
  }
  wxString wordChars = wxT("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890_%");

  long left = m_positionOfCaret, right = m_positionOfCaret;
  while (left > 0)
  {
    if (wordChars.Find(m_text.GetChar(left-1)) == -1)
      break;
    left--;
  }

  if (toRight)
  {
    while (right < (signed)m_text.length() )
    {
      if(wordChars.Find(m_text.GetChar(right)) == -1)
        break;
      right++;
    }
  }

  if (left != right)
  {
    m_selectionStart = left;
    m_selectionEnd = right;
    m_positionOfCaret = m_selectionEnd;
    return m_text.SubString(m_selectionStart, m_selectionEnd - 1);
  }

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
  m_text = m_text.SubString(0, start - 1) +
           m_text.SubString(end, m_text.Length());

  m_selectionEnd = m_selectionStart = -1;
  m_paren1 = m_paren2 = -1;
  m_width = m_height = m_maxDrop = m_center = -1;

  return true;
}

void EditorCell::InsertText(wxString text)
{
  SaveValue();
  m_saveValue = true;
  m_containsChanges = true;

  if (m_selectionStart > -1)
  {
    long start = MIN(m_selectionStart, m_selectionEnd);
    long end = MAX(m_selectionStart, m_selectionEnd);
    m_positionOfCaret = start;
    m_text = m_text.SubString(0, start - 1) +
             m_text.SubString(end, m_text.Length());
  }
  m_text = m_text.SubString(0, m_positionOfCaret - 1) +
           text +
           m_text.SubString(m_positionOfCaret, m_text.Length());
  m_positionOfCaret += text.Length();

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

wxString EditorCell::GetLineString(int line, int start, int end)
{
  if (start >= end)
    return wxEmptyString;

  int posStart = 0, posEnd = 0;

  posStart = XYToPosition(start, line);
  if (end == -1)
  {
    posEnd = XYToPosition(0, line+1);
    posEnd--;
  }
  else
    posEnd = XYToPosition(end, line);

  return m_text.SubString(posStart, posEnd - 1);
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

  m_text = m_textHistory.Item(m_historyPosition);
  m_positionOfCaret = m_positionHistory[m_historyPosition];
  m_selectionStart = m_startHistory[m_historyPosition];
  m_selectionEnd = m_endHistory[m_historyPosition];

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

  m_text = m_textHistory.Item(m_historyPosition);
  m_positionOfCaret = m_positionHistory[m_historyPosition];
  m_selectionStart = m_startHistory[m_historyPosition];
  m_selectionEnd = m_endHistory[m_historyPosition];

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
}

bool EditorCell::CheckChanges()
{
  if (m_containsChanges != m_containsChangesCheck) {
    m_containsChangesCheck = m_containsChanges;
    return true;
  }

  return false;
}

int EditorCell::ReplaceAll(wxString oldString, wxString newString)
{
  SaveValue();
  int count = m_text.Replace(oldString, newString);
  if (count > 0)
  {
    m_containsChanges = true;
    m_selectionStart = m_selectionEnd = -1;
  }
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
    m_selectionStart = strStart;
    m_selectionEnd = strStart + str.Length();
    return true;
  }
  return false;
}

bool EditorCell::ReplaceSelection(wxString oldStr, wxString newStr)
{
  if (m_selectionStart > -1 &&
      m_text.SubString(m_selectionStart, m_selectionEnd - 1) == oldStr)
  {
    m_text = m_text.SubString(0, m_selectionStart - 1) +
             newStr +
             m_text.SubString(m_selectionEnd, m_text.Length());
    m_containsChanges = -1;
    m_positionOfCaret = m_selectionEnd = m_selectionStart + newStr.Length();

    if (GetType() == MC_TYPE_INPUT)
      FindMatchingParens();

    return true;
  }
  return false;
}

wxString EditorCell::GetSelectionString()
{
  if (m_selectionStart == -1 || m_selectionEnd == -1)
    return wxEmptyString;
  return m_text.SubString(m_selectionStart, m_selectionEnd-1);
}

void EditorCell::ClearSelection()
{
  if (m_selectionStart == -1 || m_selectionEnd == -1)
    return;

  m_positionOfCaret = m_selectionEnd;
  m_selectionStart = m_selectionEnd = -1;
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
      m_positionOfCaret = m_selectionStart = start;
    else
      m_positionOfCaret = m_selectionStart = positionOfCaret + start;
    m_selectionEnd = m_selectionStart + length;
    return true;
  }

  // Then in the rest of the string
  if (varsRegex.Matches(second))
  {
    varsRegex.GetMatch(&start, &length, 1);
    if (!left)
      m_positionOfCaret = m_selectionStart = start;
    else
      m_positionOfCaret = m_selectionStart = positionOfCaret + start;
    m_selectionEnd = m_selectionStart + length;
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
