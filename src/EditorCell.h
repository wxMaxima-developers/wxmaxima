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

#ifndef EDITORCELL_H
#define EDITORCELL_H

#include "MathCell.h"

#include <vector>
#include <list>
#include <wx/tokenzr.h>

/*! \file

  This file contains the definition of the class EditorCell
 */

/*! This class defines what the user sees as input cell

  This class handles input cells including:
   - The per-cell undo buffer
   - The handling of key presses when this cell is active
 */
class EditorCell : public MathCell
{
private:
  //! Draw a box that marks the current selection
  void MarkSelection(size_t start, size_t end,CellParser& parser,double scale, wxDC& dc, TextStyle style);
  /*! The start of the current selection.

     - >0: the position of the cursors in characters from start
     - -1: Currently no selection is active
   */
  long m_selectionStart;
  /*! The end of the current selection.

     - >0: the position of the cursors in characters from start
     - -1: Currently no selection is active
   */
  long m_selectionEnd;
  /*! The currently selected string. 

    Since this string is defined statically it is available in every editor cell
    for highlighting selected strings.
  */
  static wxString m_selectionString;
  long m_oldSelectionStart;
  long m_oldSelectionEnd;

public:
  //! Has the selection changed since the last draw event?
  bool m_selectionChanged;
  //! The constructor
  EditorCell(wxString text = wxEmptyString);
  //! The destructor
  ~EditorCell();

  /*! Expand all tabulators.

    \param input The string the tabulators should be expanded in
    \param posInLine The number of characters that come before the input in the same line
    \todo Implement the actual TAB expansion
  */
  wxString TabExpand(wxString input, size_t posInLine);
  //! Escape all chars that cannot be used in HTML otherwise
  static wxString EscapeHTMLChars(wxString input);
  //! Convert all but the first of a row of multiple spaces to non-breakable
  static wxString PrependNBSP(wxString input);

  void Destroy();
  MathCell* Copy();
  /*! Recalculate the widths of the current cell.

      \todo If we use a centered dot instead of a * and we don't use a fixed-
      size fonts we miscalculate the widths here.
   */
  void RecalculateWidths(CellParser& parser, int fontsize);
  void Draw(CellParser& parser, wxPoint point, int fontsize);
  //! Convert the current cell to a string
  wxString ToString();
  //! Convert the current cell to LaTeX code
  wxString ToTeX();
  //! Convert the current cell to XML code for inclusion in a .wxmx file.
  wxString ToXML();
  //! Convert the current cell to HTML code.
  wxString ToHTML();
  void SetFont(CellParser& parser, int fontsize);
  void SetForeground(CellParser& parser);

  /*! Sets the text that is to be displayed.
    
    Automatically calls StyleText().
   */
  void SetValue(wxString text);
  wxString GetValue()
  {
    return m_text;
  }
  /*! Converts m_text to a list of styled text snippets that will later be used by draw().

    \todo Actually set the needed text styles.
   */
  void StyleText();
  void Reset();
  //! Decide what to do if the user pressed a key when this cell was selected
  void ProcessEvent(wxKeyEvent& event);
  bool ActivateCell();
  //! Return the index of the 1st char of the line containing the letter #pos.
  size_t BeginningOfLine(size_t pos);
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
  void SelectNone()
  {
    m_selectionStart = m_selectionEnd = 0;
  }
  bool SelectionActive()
  {
    return (m_selectionStart != -1)&&(m_selectionEnd != -1);
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
  int GetLineWidth(wxDC& dc, int line, int end);
  //! true, if this cell's width has to be recalculated.
  bool IsDirty()
  {
    return m_isDirty;
  }

  //! Toggles the visibility of the cursor which is used to make it blink.
  void SwitchCaretDisplay()
  {
    m_displayCaret = !m_displayCaret;
  }
  void SetFocus(bool focus)
  {
    m_hasFocus = focus;
  }
  void SetFirstLineOnly(bool show = true) {
    if (m_firstLineOnly != show) { m_width = m_height = -1; m_firstLineOnly = show; }
    // Style the text anew.
    StyleText();
  }
  bool IsActive() { return m_isActive; }
  //! Is the cursor at the start of this cell?
  bool CaretAtStart() { return m_positionOfCaret == 0; }
  //! Move the cursor to the start of this cell
  void CaretToStart();
  //! Is the cursor at the end of this cell?
  bool CaretAtEnd() { return m_positionOfCaret == m_text.Length(); }
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
  /*! Replaces all occurrences of a given string

    TODO: Implement the IgnoreCase case.
   */
  int ReplaceAll(wxString oldString, wxString newString,bool IgnoreCase);
  /*! Finds the next occurrences of a string

    \param str The string to search for
    \param down 
     - true: search downwards
     - false: search upwards
    \param ignoreCase
     - true: Case-insensitive search
     - false: Case-sensitive search
   */
  bool FindNext(wxString str, bool down, bool ignoreCase);
  void SetSelection(int start, int end);
  void GetSelection(int *start, int *end)
  {
    *start = m_selectionStart; *end = m_selectionEnd;
  }
  /*! Replace the current selection with a string

    \param oldStr The old string in the selection. If this string doesn't match
                  the selection this function doesn't replace it.
    \param newString The new string oldStr has to be replaced by
    \param keepSelected 
      - true = we want the new string to be selected afterwards
      - false = the selection is cleared after replacing the string
        and moving the cursor to its end.
    \param ignoreCase
   */
  bool ReplaceSelection(wxString oldStr, wxString newString,bool keepSelected = false,bool ignoreCase = false);
  //! Convert the current selection to a string
  wxString GetSelectionString();
  //! Unselect everything
  void ClearSelection();
  //! Get the cursor's current position inside the cell.
  int GetCaretPosition() { return m_positionOfCaret; }
  bool FindNextTemplate(bool left = false);
  void InsertText(wxString text);
  wxString TextInFrontOfSelection()
    {
      return GetValue().Mid(1,m_selectionStart);
    }
  //! Return to the selection after the cell has been left upwards
  void ReturnToSelectionFromTop()
    {
      SetSelection(m_lastSelectionStart,0);
    }
  //! Return to the selection after the cell has been left downwards
  void ReturnToSelectionFromBot()
    {
      SetSelection(m_lastSelectionStart,m_text.Length());
    }
private:
  /*! Divide a string into tokens

    Used when styling text.
   */
  wxArrayString StringToTokens(wxString string);

  bool IsAlpha(wxChar c);
  bool IsNum(wxChar c);
  bool IsAlphaNum(wxChar c);
  
  /*! A piece of styled text for syntax highlighting

   */
  class StyledText
  {
  private:
    //! The color of this text portion
    TextStyle  m_style;
    //! The text of this text portion
    wxString m_text;
    //! Do we really want to style this text portion different than the default?
    bool m_styleThisText;
  public:    
    //! Defines a piece of styled text
    StyledText(TextStyle style,wxString text)
      {
        m_text = text;
        m_style = style;
        m_styleThisText = true;
      }

    //! Defines a piece of text with the default style
    StyledText(wxString text)
      {
        m_text = text;
        m_styleThisText = false;
      }
    //! Returns the piece of text
    wxString GetText()
      {
        return m_text;
      }
    //! If StyleSet() is true this function returns the color of this text portion
    TextStyle GetStyle()
      {
        return m_style;
      }
    // Has a individual text style been set for this text portion?
    bool StyleSet()
      {
        return m_styleThisText;
      }
  };
  
  std::list<StyledText> m_styledText;

#if wxUSE_UNICODE
  /*! Handle ESC shortcuts for special characters

    These characters can be tought to LaTeX and the html browser if neccessary in
    TextCell::ToTeX and EditorCell::ToTeX. They can also be
    converted to maxima strings in wxMaxima::SendMaxima.
   */
  wxString InterpretEscapeString(wxString txt);
#endif
  wxString m_text;
  wxArrayString m_textHistory;
  std::vector<int> m_positionHistory;
  std::vector<int> m_startHistory;
  std::vector<int> m_endHistory;
  ptrdiff_t m_historyPosition;
  //! Where inside this cell is the cursor?
  int m_positionOfCaret;
  int m_caretColumn;
  long m_lastSelectionStart;
//  long m_oldStart, m_oldEnd;
  int m_numberOfLines;
  bool m_isActive;
  int m_fontSize;
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
