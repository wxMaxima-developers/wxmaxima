///
///  Copyright (C) 2006-2007 Andrej Vodopivec <andrejv@users.sourceforge.net>
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

#include "EditorCell.h"
#include "wxMaxima.h"
#include "wxMaximaFrame.h"

#define INCREASE_SIZE 6

EditorCell::EditorCell() : MathCell()
{
  m_text = wxEmptyString;
  m_fontSize = -1;
  m_positionOfCaret = 0;
  m_selectionStart = -1;
  m_selectionEnd = -1;
  m_isActive = false;
  m_matchParens = true;
  m_paren1 = m_paren2 = -1;
  m_isDirty = false;
  m_hasFocus = false;
  m_underlined = false;
  m_fontWeight = wxFONTWEIGHT_NORMAL;
  m_fontStyle = wxNORMAL;
  m_fontEncoding = wxFONTENCODING_DEFAULT;
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
  return text + MathCell::ToString(all);
}

wxString EditorCell::ToTeX(bool all)
{
  wxString text = m_text;
  return text + MathCell::ToTeX(all);
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

void EditorCell::Draw(CellParser& parser, wxPoint point1, int fontsize, bool all)
{
  double scale = parser.GetScale();
  wxDC& dc = parser.GetDC();
  wxPoint point(point1);

  if (m_width == -1 || m_height == -1)
    RecalculateWidths(parser, fontsize, false);

  if (DrawThisCell(parser, point) && !m_isHidden)
  {
    SetBackground(parser, point1);
    SetForeground(parser);
    SetPen(parser);
    SetFont(parser, fontsize);

    unsigned int newLinePos = 0, prevNewLinePos = 0, numberOfLines = 0;

    //
    // Draw the text
    //
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

    if (m_isActive)
    {
      //
      // Draw the caret
      //
      if (m_displayCaret && m_hasFocus)
      {
        int caretInLine = 0;
        int caretInColumn = 0;

        PositionToXY(m_positionOfCaret, &caretInColumn, &caretInLine);

        wxString line = GetLineString(caretInLine, 0, caretInColumn);
        int lineWidth, lineHeight;
        dc.GetTextExtent(line, &lineWidth, &lineHeight);


        dc.DrawLine(point.x + SCALE_PX(2, scale) + lineWidth,
                    point.y + SCALE_PX(2, scale) - m_center + caretInLine * m_charHeight,
                    point.x + SCALE_PX(2, scale) + lineWidth,
                    point.y + SCALE_PX(2, scale) - m_center + (caretInLine + 1) * m_charHeight);
      }

      //
      // Mark selection
      //
      if (m_selectionStart > -1)
      {
#if defined(__WXMAC__)
	dc.SetLogicalFunction(wxXOR);
	dc.SetPen(*wxBLACK_PEN);
#else
        dc.SetLogicalFunction(wxAND);
        dc.SetBrush(*wxLIGHT_GREY_BRUSH);
        dc.SetPen(*wxLIGHT_GREY_PEN);
#endif
        wxPoint point;
        long start = MIN(m_selectionStart, m_selectionEnd);
        long end = MAX(m_selectionStart, m_selectionEnd);
        long pos1 = start, pos2 = start;
        wxPoint point1;

        while (pos1 < end)
        {
          while (pos1 < end && m_text.GetChar(pos1) != '\n')
            pos1++;
          point = PositionToPoint(parser, pos2);
          point1 = PositionToPoint(parser, pos1);
          dc.DrawRectangle(point.x + SCALE_PX(2, scale),
                           point.y  + SCALE_PX(2, scale) - m_center,
                           point1.x - point.x,
                           m_charHeight);
          pos1++;
          pos2 = pos1;
        }

        dc.SetLogicalFunction(wxCOPY);
      }

      //
      // Matching parens
      //
      else if (m_paren1 != -1 && m_paren2 != -1)
      {
#if defined(__WXMAC__)
	dc.SetLogicalFunction(wxXOR);
#else
        dc.SetLogicalFunction(wxAND);
#endif
        dc.SetBrush(*wxLIGHT_GREY_BRUSH);
        dc.SetPen(*wxLIGHT_GREY_PEN);
        wxPoint point = PositionToPoint(parser, m_paren1);
        int width, height;
        dc.GetTextExtent(m_text.GetChar(m_paren1), &width, &height);
        dc.DrawRectangle(point.x + SCALE_PX(2, scale),
                         point.y  + SCALE_PX(2, scale) - m_center,
                         width, height);
        point = PositionToPoint(parser, m_paren2);
        dc.GetTextExtent(m_text.GetChar(m_paren1), &width, &height);
        dc.DrawRectangle(point.x + SCALE_PX(2, scale),
                         point.y  + SCALE_PX(2, scale) - m_center,
                         width, height);
        dc.SetLogicalFunction(wxCOPY);
      }
    }
    UnsetPen(parser);
  }
  MathCell::Draw(parser, point1, fontsize, all);
}

void EditorCell::SetFont(CellParser& parser, int fontsize)
{
  wxDC& dc = parser.GetDC();
  double scale = parser.GetScale();

  m_fontSize = fontsize;
  int fontsize1 = (int) (((double)fontsize) * scale + 0.5);
  fontsize1 = MAX(fontsize1, 1);

  m_fontName = parser.GetFontName();
  m_fontEncoding = parser.GetFontEncoding();
  m_fontStyle = parser.IsItalic(TS_NORMAL_TEXT);
  m_fontWeight = parser.IsBold(TS_NORMAL_TEXT);

  switch(m_type)
  {
  case MC_TYPE_TITLE:
    fontsize1 += SCALE_PX(INCREASE_SIZE, scale);
    m_fontStyle = wxFONTSTYLE_SLANT;
  case MC_TYPE_SECTION:
    fontsize1 += SCALE_PX(INCREASE_SIZE, scale);
    m_fontWeight = wxFONTWEIGHT_BOLD;
    m_underlined = true;
  case MC_TYPE_COMMENT:
    m_fontEncoding = parser.GetFontEncoding();
    break;
  default:
    m_fontStyle = parser.IsItalic(TS_INPUT);
    m_fontWeight = parser.IsBold(TS_INPUT);
    m_underlined = parser.IsUnderlined(TS_INPUT);
    m_fontEncoding = parser.GetFontEncoding();
    break;
  }

  dc.SetFont(wxFont(fontsize1, wxMODERN,
                    m_fontStyle,
                    m_fontWeight,
                    m_underlined,
                    m_fontName,
                    m_fontEncoding));
}

void EditorCell::SetForeground(CellParser& parser)
{
  wxDC& dc = parser.GetDC();

  switch (m_type)
  {
  case MC_TYPE_COMMENT:
  case MC_TYPE_SECTION:
  case MC_TYPE_TITLE:
    dc.SetTextForeground(wxTheColourDatabase->Find(parser.GetColor(TS_NORMAL_TEXT)));
    break;
  default:
    dc.SetTextForeground(wxTheColourDatabase->Find(parser.GetColor(TS_INPUT)));
    break;
  }
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
  switch (event.GetKeyCode())
  {
  case WXK_ESCAPE:
    {
      wxCommandEvent ev(wxEVT_COMMAND_MENU_SELECTED, deactivate_cell_cancel);
      (wxGetApp().GetTopWindow())->ProcessEvent(ev);
    }
    break;
  case WXK_LEFT:
    if (event.ShiftDown())
    {
      if (m_selectionStart == -1)
        m_selectionEnd = m_selectionStart = m_positionOfCaret;
    }
    else
      m_selectionEnd = m_selectionStart = -1;
    if (m_positionOfCaret > 0)
      m_positionOfCaret--;
    if (event.ShiftDown())
      m_selectionEnd = m_positionOfCaret;
    break;
  case WXK_RIGHT:
    if (event.ShiftDown())
    {
      if (m_selectionStart == -1)
        m_selectionEnd = m_selectionStart = m_positionOfCaret;
    }
    else
      m_selectionEnd = m_selectionStart = -1;
    if (m_positionOfCaret < m_text.Length())
      m_positionOfCaret++;
    if (event.ShiftDown())
      m_selectionEnd = m_positionOfCaret;
    break;
  case WXK_PAGEDOWN:
  case WXK_DOWN:
    {
      if (event.ShiftDown())
      {
        if (m_selectionStart == -1)
          m_selectionEnd = m_selectionStart = m_positionOfCaret;
      }
      else
        m_selectionEnd = m_selectionStart = -1;
      int column, line;
      PositionToXY(m_positionOfCaret, &column, &line);

      line = line < m_numberOfLines-1 ? line + 1 : line;

      m_positionOfCaret = XYToPosition(column, line);

      if (event.ShiftDown())
        m_selectionEnd = m_positionOfCaret;
    }
    break;
  case WXK_PAGEUP:
  case WXK_UP:
    {
      if (event.ShiftDown())
      {
        if (m_selectionStart == -1)
          m_selectionEnd = m_selectionStart = m_positionOfCaret;
      }
      else
        m_selectionEnd = m_selectionStart = -1;
      int column, line;
      PositionToXY(m_positionOfCaret, &column, &line);

      line = line > 0 ? line - 1 : 0;

      m_positionOfCaret = XYToPosition(column, line);
      if (event.ShiftDown())
        m_selectionEnd = m_positionOfCaret;
    }
    break;
  case WXK_RETURN:
    m_text = m_text.SubString(0, m_positionOfCaret - 1) +
             wxT("\n") +
             m_text.SubString(m_positionOfCaret, m_text.Length());
    m_positionOfCaret++;
    m_isDirty = true;
    break;
  case WXK_END:
    if (event.ShiftDown())
    {
      if (m_selectionStart == -1)
        m_selectionEnd = m_selectionStart = m_positionOfCaret;
    }
    else
      m_selectionEnd = m_selectionStart = -1;
    if (event.ControlDown())
      m_positionOfCaret = m_text.Length();
    else
    {
      while (m_positionOfCaret < m_text.Length() &&
             m_text.GetChar(m_positionOfCaret) != '\n')
        m_positionOfCaret++;
    }
    if (event.ShiftDown())
      m_selectionEnd = m_positionOfCaret;
    break;
  case WXK_HOME:
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
    m_isDirty = true;
    if (m_positionOfCaret < m_text.Length())
    {
      if (m_selectionStart == -1)
        m_text = m_text.SubString(0, m_positionOfCaret - 1) +
                 m_text.SubString(m_positionOfCaret + 1, m_text.Length());
      else
      {
        long start = MIN(m_selectionEnd, m_selectionStart);
        long end = MAX(m_selectionEnd, m_selectionStart);
        m_text = m_text.SubString(0, start - 1) +
                 m_text.SubString(end, m_text.Length());
        m_positionOfCaret = start;
        m_selectionEnd = m_selectionStart = -1;
      }
    }
    break;
  case WXK_BACK:
    m_isDirty = true;
    if (m_selectionStart > -1) {
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
      m_text = m_text.SubString(0, m_positionOfCaret - 2) +
               m_text.SubString(m_positionOfCaret, m_text.Length());
      m_positionOfCaret--;
    }
    break;
  case WXK_TAB:
    m_isDirty = true;
    {
      if (m_selectionStart > -1) {
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
    break;
  default:
    if (event.ControlDown())
      break;
    m_isDirty = true;
    if (m_selectionStart > -1) {
      long start = MIN(m_selectionEnd, m_selectionStart);
      long end = MAX(m_selectionEnd, m_selectionStart);
      m_text = m_text.SubString(0, start - 1) +
               m_text.SubString(end, m_text.Length());
      m_positionOfCaret = start;
      m_selectionEnd = m_selectionStart = -1;
    }
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
      }
    }
    break;
  }

  if (m_type == MC_TYPE_INPUT)
    FindMatchingParens();
  if (m_isDirty)
    m_width = m_maxDrop = -1;
  m_displayCaret = true;
}

void EditorCell::FindMatchingParens()
{
  m_paren2 = m_positionOfCaret;
  if (wxString(wxT("([{}])")).Find(m_text.GetChar(m_paren2)) == -1)
  {
    m_paren2--;
    if (wxString(wxT("([{}])")).Find(m_text.GetChar(m_paren2)) == -1)
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

bool EditorCell::ActivateCell()
{
  m_isActive = !m_isActive;
  m_displayCaret = true;
  m_hasFocus = true;

  m_selectionEnd = m_selectionStart = -1;

  return true;
}

bool EditorCell::AddEnding()
{
  wxString text = m_text.Trim();
  if (text.Right(1) != wxT(";") && text.Right(1) != wxT("$")) {
    m_text += wxT(";");
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

  if (m_type == MC_TYPE_TITLE)
    fontsize1 += 2*INCREASE_SIZE;
  else if (m_type == MC_TYPE_SECTION)
    fontsize1 += INCREASE_SIZE;

  dc.SetFont(wxFont(fontsize1, wxMODERN,
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

  while (m_text.GetChar(m_positionOfCaret) != '\n' && m_positionOfCaret < m_text.Length())
  {
    s = m_text.SubString(lineStart, m_positionOfCaret);
    dc.GetTextExtent(m_text.SubString(lineStart, m_positionOfCaret),
                                      &width, &height);
    if (width > translate.x)
      break;

    m_positionOfCaret++;
  }

  m_positionOfCaret = MIN(m_positionOfCaret, m_text.Length());

  m_displayCaret = true;
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
  if (m_selectionStart == m_selectionEnd)
  {
    m_selectionStart = -1;
    m_selectionEnd = -1;
  }
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

  CopyToClipboard();

  long start = MIN(m_selectionStart, m_selectionEnd);
  long end = MAX(m_selectionStart, m_selectionEnd);
  m_positionOfCaret = start;
  m_text = m_text.SubString(0, start - 1) +
           m_text.SubString(end, m_text.Length());

  m_selectionEnd = m_selectionStart = -1;

  m_width = m_height = m_maxDrop = m_center = -1;

  return true;
}

void EditorCell::PasteFromClipboard()
{
  if (wxTheClipboard->Open())
  {
    if (wxTheClipboard->IsSupported(wxDF_TEXT))
    {
      wxTextDataObject obj;
      wxTheClipboard->GetData(obj);
      if (m_selectionStart > -1)
      {
        long start = MIN(m_selectionStart, m_selectionEnd);
        long end = MAX(m_selectionStart, m_selectionEnd);
        m_positionOfCaret = start;
        m_text = m_text.SubString(0, start - 1) +
                 m_text.SubString(end, m_text.Length());
      }
      wxString data = obj.GetText();
      m_text = m_text.SubString(0, m_positionOfCaret - 1) +
               data +
               m_text.SubString(m_positionOfCaret, m_text.Length());
      m_selectionStart = m_positionOfCaret;
      m_positionOfCaret += data.Length();
      m_selectionEnd = m_positionOfCaret;
    }
    wxTheClipboard->Close();
  }

  m_width = m_height = m_maxDrop = m_center = -1;
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

void EditorCell::SetBackground(CellParser& parser, wxPoint& point)
{
  if (GetType() != MC_TYPE_INPUT && !m_isActive) {
    wxDC &dc = parser.GetDC();
    wxRect rect = GetRect(false);
    int y = rect.GetY();

    if (m_height > 0 && m_width > 0 && y>=0) {
       wxBrush br(wxColor(parser.GetColor(TS_TEXT_BACKGROUND)));
       dc.SetBrush(br);
       wxPen pen(wxColor(parser.GetColor(TS_TEXT_BACKGROUND)));
       dc.SetPen(pen);
       int height = rect.GetHeight();
       dc.DrawRectangle(0, y - 1, 10000, height + 2);
    }
  }
}
