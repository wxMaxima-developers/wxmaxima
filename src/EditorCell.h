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

#ifndef _EDITOR_CELL_H
#define _EDITOR_CELL_H

#include "MathCell.h"

class EditorCell : public MathCell
{
public:
  EditorCell();
  ~EditorCell();
  void Destroy();
  MathCell* Copy(bool all);
  void RecalculateSize(CellParser& parser, int fontsize, bool all);
  void RecalculateWidths(CellParser& parser, int fontsize, bool all);
  void Draw(CellParser& parser, wxPoint point, int fontsize, bool all);
  wxString ToString(bool all);
  wxString ToTeX(bool all);
  void SetFont(CellParser& parser, int fontsize);
  void SetForeground(CellParser& parser);
  void SetBackground(CellParser& parser, wxPoint& point);
  void SetValue(wxString& text)
  {
    m_text = text;
  }
  wxString GetValue()
  {
    return m_text;
  }
  void Reset();
  void ProcessEvent(wxKeyEvent& event);
  bool ActivateCell();
  void AddEnding();
  void PositionToXY(int pos, int* line, int* col);
  int XYToPosition(int x, int y);
  wxPoint PositionToPoint(CellParser& parser, int pos = -1);
  void SelectPointText(wxDC &dc, wxPoint& point);
  void SelectRectText(wxDC &dc, wxPoint& one, wxPoint& two);
  bool CopyToClipboard();
  bool CutToClipboard();
  void PasteFromClipboard();
  void SelectAll()
  {
    m_selectionStart = 0;
    m_selectionEnd = m_text.Length();
  }
  bool CanCopy()
  {
    return m_selectionStart != -1;
  }
  void SetMatchParens(bool match)
  {
    m_matchParens = match;
  }
  void FindMatchingParens();
  wxString GetLineString(int line, int start = 0, int end = -1);
  bool IsDirty()
  {
    return m_isDirty;
  }
  void SwitchCaretDisplay()
  {
    m_displayCaret = !m_displayCaret;
  }
  void SetFocus(bool focus)
  {
    m_hasFocus = focus;
  }
private:
  wxString m_text;
  unsigned int m_positionOfCaret;
  long m_selectionStart;
  long m_selectionEnd;
  int m_numberOfLines;
  bool m_isActive;
  int m_fontSize;
  int m_charWidth;
  int m_charHeight;
  bool m_matchParens;
  int m_paren1, m_paren2;
  bool m_isDirty;
  bool m_displayCaret;
  bool m_hasFocus;
  int m_fontStyle;
  wxFontWeight m_fontWeight;
  bool m_underlined;
  wxString m_fontName;
  wxFontEncoding m_fontEncoding;
};

#endif
