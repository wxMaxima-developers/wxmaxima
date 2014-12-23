///
///  Copyright (C) 2006-2014 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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

#ifndef EDITORCELL_H
#define EDITORCELL_H

#include "MathCell.h"

#include <vector>
/*! \file

  This file contains the definition of the class EditorCell
 */

/*! This class provides an interface for editing cells

  One of the examples what this class does is to provide an undo buffer.
 */
class EditorCell : public MathCell
{
public:
  //! The constructor
  EditorCell();
  //! The destructor
  ~EditorCell();
  void Destroy();
  MathCell* Copy(bool all);
  void RecalculateSize(CellParser& parser, int fontsize, bool all);
  void RecalculateWidths(CellParser& parser, int fontsize, bool all);
  void Draw(CellParser& parser, wxPoint point, int fontsize, bool all);
  wxString ToString(bool all);
  wxString ToTeX(bool all);
  wxString ToXML(bool all);
  void SetFont(CellParser& parser, int fontsize);
  void SetForeground(CellParser& parser);
  void SetValue(wxString text);
  wxString GetValue()
  {
    return m_text;
  }
  void Reset();
  //! Decide what to do if the user pressed a key when this cell was selected
  void ProcessEvent(wxKeyEvent& event);
  bool ActivateCell();
  bool AddEnding();
  void PositionToXY(int pos, int* line, int* col);
  int XYToPosition(int x, int y);
  wxPoint PositionToPoint(CellParser& parser, int pos = -1);
  void SelectPointText(wxDC &dc, wxPoint& point);
  void SelectRectText(wxDC &dc, wxPoint& one, wxPoint& two);
  wxString SelectWordUnderCaret(bool selectParens = true, bool toRight = true);
  bool IsPointInSelection(wxDC& dc, wxPoint point);
  bool CopyToClipboard();
  bool CutToClipboard();
  void PasteFromClipboard(bool primary = false);
  void SelectAll()
  {
    m_selectionStart = 0;
    m_selectionEnd = m_positionOfCaret = m_text.Length();
  }
  bool CanCopy()
  {
    return m_selectionStart != -1;
  }
  void SetMatchParens(bool match)
  {
    m_matchParens = match;
  }
  void SetInsertAns(bool insertAns)
  {
    m_insertAns = insertAns;
  }
  bool FindMatchingQuotes();
  void FindMatchingParens();
  wxString GetLineString(int line, int start = 0, int end = -1);
  //! true, if this cell's width has to be recalculated.
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
  void SetFirstLineOnly(bool show = true) {
    if (m_firstLineOnly != show) { m_width = m_height = -1; m_firstLineOnly = show; }}
  bool IsActive() { return m_isActive; }
  //! Is the cursor at the start of this cell?
  bool CaretAtStart() { return m_positionOfCaret == 0; }
  //! Move the cursor to the start of this cell
  void CaretToStart();
  //! Is the cursor at the end of this cell?
  bool CaretAtEnd() { return m_positionOfCaret == (signed)m_text.Length(); }
  //! Move the cursor to the end of this cell
  void CaretToEnd();
  //! Move the cursor to a certain position in the cell
  void CaretToPosition(int pos);
  //! True, if there is undo information for this cell
  bool CanUndo();
  //! Issue an undo command
  void Undo();
  //! True, if a redo can be done for this cell.
  bool CanRedo();
  //! Issu a redo command
  void Redo();
  //! Save the current contents of this cell in the undo buffer.
  void SaveValue();
  wxString DivideAtCaret();
  void CommentSelection();
  void ClearUndo();
  //! Query if this cell needs to be re-evaluated by maxima
  bool ContainsChanges() { return m_containsChanges; }
  //! Set the information if this cell needs to be re-evaluated by maxima
  void ContainsChanges(bool changes) { m_containsChanges = m_containsChangesCheck = changes; }
  bool CheckChanges();
  int ReplaceAll(wxString oldString, wxString newString);
  bool FindNext(wxString str, bool down, bool ignoreCase);
  void SetSelection(int start, int end)
  {
    m_selectionStart = start;
    m_positionOfCaret = m_selectionEnd = end;
  }
  void GetSelection(int *start, int *end)
  {
    *start = m_selectionStart; *end = m_selectionEnd;
  }
  /*! Replace the selection with a string

    \todo This function sets the bool m_containsChanges to -1. I assume this should 
    read false instead.
   */
  bool ReplaceSelection(wxString oldStr, wxString newString);
  wxString GetSelectionString();
  void ClearSelection();
  //! Get the cursor's current position inside the cell.
  int GetCaretPosition() { return m_positionOfCaret; }
  bool FindNextTemplate(bool left = false);
  void InsertText(wxString text);
private:
#if wxUSE_UNICODE
  wxString InterpretEscapeString(wxString txt);
#endif
  wxString m_text;
  wxArrayString m_textHistory;
  std::vector<int> m_positionHistory;
  std::vector<int> m_startHistory;
  std::vector<int> m_endHistory;
  ptrdiff_t m_historyPosition;
//  int m_oldPosition;
  int m_positionOfCaret;
  int m_caretColumn;
  long m_selectionStart;
  long m_selectionEnd;
//  long m_oldStart, m_oldEnd;
  int m_numberOfLines;
  bool m_isActive;
  int m_fontSize;
  int m_charWidth;
  int m_charHeight;
  bool m_matchParens;
  int m_paren1, m_paren2;
  bool m_insertAns;
  //! Does this cell's size have to be recalculated?
  bool m_isDirty;
  bool m_displayCaret;
  bool m_hasFocus;
  wxFontStyle m_fontStyle;
  wxFontWeight m_fontWeight;
  bool m_underlined;
  wxString m_fontName;
  wxFontEncoding m_fontEncoding;
  bool m_saveValue;
  //! true, if this function has changed since the last evaluation by maxima
  bool m_containsChanges;
  bool m_containsChangesCheck;
  bool m_firstLineOnly;
};

#endif // EDITORCELL_H
