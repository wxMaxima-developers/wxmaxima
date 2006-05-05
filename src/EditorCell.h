///
///  Copyright (C) 2006 Andrej Vodopivec <andrejv@users.sourceforge.net>
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
  void SetFont(CellParser& parser, int fontsize);
  void SetForeground(CellParser& parser);
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
  void PositionToXY(int* line, int* col);
  void XYToPosition(int x, int y);
  wxPoint CaretToPoint();
  void SelectPoint(wxPoint& point);
  void SelectRect(wxPoint& one, wxPoint& two);
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
private:
  wxString m_text;
  long m_positionOfCaret;
  long m_selectionStart;
  long m_selectionEnd;
  int m_numberOfLines;
  bool m_isActive;
  int m_fontSize;
  int m_charWidth;
  int m_charHeight;
  bool m_matchParens;
  int m_paren1, m_paren2;
};

#endif
