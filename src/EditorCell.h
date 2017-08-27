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
#include "CellPointers.h"

#include <vector>
#include <list>
#include <vector>
#include <wx/tokenzr.h>

/*! \file

  This file contains the definition of the class EditorCell
 */

/*! This class defines what the user sees as input cell
  
  This class handles input cells including:
    - The per-cell undo buffer
    - The handling of key presses when this cell is active
  
  Since Unicode doesn't provide us with a "soft linebreak" letter we 
  use <code>\r</code> as a marker that this line has to be broken here if we
  don't want it to extend beyond the right margin of the screen.

  In a few places we use wxString::iterator instead of accessing individual 
  letters within the string using the [] operator. This might look overly
  complicated. But in UTF-8 all non-standard-ASCII-characters use more than one
  byte making iterating over every single char of the string the only way of
  determining which address char n is at. An iterator is the only way of not
  having to determine the address of every single char indepently. 
 */
class EditorCell : public MathCell
{
private:
  int m_errorIndex;
  CellPointers *m_cellPointers;
  //! The viewport size the linewrap was done for.
  int m_oldViewportWidth;
  //! The zoom factor the linewrap was done for.
  int m_oldZoomFactor;
  //! The scale factor the linewrap was done for.
  int m_oldScaleFactor;
  //! The font size the linewrap was done for.
  int m_oldDefaultFontSize;

  int ChangeNumpadToChar(int c);

  //! A list of all potential autoComplete targets within this cell
  wxArrayString m_wordList;

  //! Draw a box that marks the current selection
  void MarkSelection(long start, long end, double scale, wxDC &dc, TextStyle style, int fontsize);

  /*! The start of the current selection.

     - >0: the position of the cursors in characters from start
     - -1: Currently no selection is active

     If the selection has been done from right to left m_selectionStart>m_selectionEnd.
   */
  long m_selectionStart;
  /*! The end of the current selection.

     - >0: the position of the cursors in characters from start
     - -1: Currently no selection is active

     If the selection has been done from right to left m_selectionStart>m_selectionEnd.
   */
  long m_selectionEnd;
  long m_oldSelectionStart;
  long m_oldSelectionEnd;
public:
  //! The constructor
  EditorCell(MathCell *parent, Configuration **config,
             CellPointers *cellPointers, wxString text = wxEmptyString);

  ~EditorCell();

  //! May this Editor Cell contain the answer to a question?
  void AutoAnswer(bool autoAnswer){m_autoAnswer = autoAnswer;}

  //! Which cell the blinking cursor is in?
  EditorCell *GetActiveCell()
  { return dynamic_cast<EditorCell *>(m_cellPointers->m_activeCell); }

  /*! Tells where the mouse selection has started.

    Needs to be kept in EditorCell so if an EditorCell is deleted it can automatically
    remove this pointer.
   */
  EditorCell *MouseSelectionStart()
  { return dynamic_cast<EditorCell *>(m_cellPointers->m_cellMouseSelectionStartedIn); }

  /*! Tells where the keyboard selection has started.

    Needs to be kept in EditorCell so if an EditorCell is deleted it can automatically
    remove this pointer.
   */
  EditorCell *KeyboardSelectionStart()
  { return dynamic_cast<EditorCell *>(m_cellPointers->m_cellKeyboardSelectionStartedIn); }

  /*! Tells where the search has started.

    Needs to be kept in EditorCell so if an EditorCell is deleted it can automatically
    remove this pointer.
   */
  EditorCell *SearchStart()
  { return dynamic_cast<EditorCell *>(m_cellPointers->m_cellSearchStartedIn); }

  /*! At which character inside its cell has the search started?

    Needs to be kept in EditorCell so if an EditorCell is deleted it can automatically
    remove this pointer.
   */
  int IndexSearchStartedAt()
  { return m_cellPointers->m_indexSearchStartedAt; }

  /*! Remember that this is the cell the search was started in.

    \param index The index of the character the search was started at.
  */
  void SearchStartedHere(int index)
  {
    m_cellPointers->m_cellSearchStartedIn = this;
    m_cellPointers->m_indexSearchStartedAt = index;
  }

  //! Remember that this is the cell the search was started in.
  void SearchStartedHere()
  {
    m_cellPointers->m_cellSearchStartedIn = this;
    m_cellPointers->m_indexSearchStartedAt = m_positionOfCaret;
  }

  //! Remember that this is the cell the mouse selection was started in.
  void MouseSelectionStartedHere()
  { m_cellPointers->m_cellMouseSelectionStartedIn = this; }

  //! Remember that this is the cell the keyboard selection was started in.
  void KeyboardSelectionStartedHere()
  { m_cellPointers->m_cellKeyboardSelectionStartedIn = this; }

  //! Remember that this is the cell the keyboard selection was started in.
  void IndexSearchStartedAt(int index)
  { m_cellPointers->m_indexSearchStartedAt = index; }

  //! Set the string that is to be highlighted as "identical to the curent selection"
  void SetSelectionString(wxString string)
  { m_cellPointers->m_selectionString = string; }

  //! A list of words that might be applicable to the autocomplete function.
  wxArrayString GetWordList()
  { return m_wordList; }

  //! Has the selection changed since the last draw event?
  bool m_selectionChanged;

  /*! Tell this cell to remove it from all gui actions.

    Normally the gui keeps various pointers to a cell: The cell below the cursor,
    the cell the selection was started at, the cell that was the last cell maxima
    appended output to...

    Running this command tells the cell to remove these pointers as the cell is 
    no more displayed currently.
   */
  void MarkAsDeleted();

  /*! Expand all tabulators.

    \param input The string the tabulators should be expanded in
    \param posInLine The number of characters that come before the input in the same line
    \todo Implement the actual TAB expansion
  */
  wxString TabExpand(wxString input, long posInLine);

  //! Escape all chars that cannot be used in HTML otherwise
  static wxString EscapeHTMLChars(wxString input);

  //! Convert all but the first of a row of multiple spaces to non-breakable
  static wxString PrependNBSP(wxString input);

  MathCell *Copy();

  //! Recalculate the widths of the current cell.
  void RecalculateWidths(int fontsize);

  void Draw(wxPoint point, int fontsize);

  wxString ToString();

  /*! Convert the current cell to a string
  
    \param dontLimitToSelection
    - false: If text is selected return only the selected text
    - true:  Always return all text in this text cell
  */
  wxString ToString(bool dontLimitToSelection);

  //! Convert the current cell to LaTeX code
  wxString ToTeX();

  //! Convert the current cell to XML code for inclusion in a .wxmx file.
  wxString ToXML();

  //! Convert the current cell to HTML code.
  wxString ToHTML();

  //! Convert the current cell to RTF code
  wxString ToRTF();

  //! Set the currently used font to the one that matches this cell's formatting
  void SetFont();

  //! Sets the current color to this cell's foreground color
  void SetForeground();

  /*! Sets the text that is to be displayed.
    
    Automatically calls StyleText().
   */
  void SetValue(const wxString &text);

  /*! Returns the text contained in this cell

    Naturally all soft line breaks are converted back to spaces beforehand.
   */
  wxString GetValue()
  {
    return m_text;
  }

  /*! Converts m_text to a list of styled text snippets that will later be displayed by draw().

    This function also generates a wordlist for this EditorCell so Autocompletion can learn
    about variable names contained in lists or cells that still haven't been evaluated.

    For cells containing text instead of code this function adds a <code>\r</code> as a marker
    that this line is to be broken here until the window's width changes.
   */
  void StyleText();
  /*! Is Called by StyleText() for code cells

    \todo This function gets *extremely* slow for long code cells (a cell with 13000 words
    needs minutes in order to run through this function).
  */
  void StyleTextCode();
  void StyleTextTexts();

  void Reset();

  //! Decide what to do if the user pressed a key when this cell was selected
  void ProcessEvent(wxKeyEvent &event);

  /*! Activate the blinking cursor in this cell
    
    Automatically calls DeactivateCursor on an eventual cell the cursor currently is in.
    Normally Mathctrl::SetActiveCell() is used in order to get this function called.
   */
  void ActivateCursor();

  //! Deactivate the blinking cursor in the EditorCell it is in.
  void DeactivateCursor();

  //! Return the index of the 1st char of the line containing the letter #pos.
  size_t BeginningOfLine(long pos);

  //! Return the index of the last char of the line containing the letter #pos,
  size_t EndOfLine(long pos);

  //! Adds a ";" to the end of the last command in this cell in case that it doesn't end in $ or ;
  bool AddEnding();

  //! Determines which line and column the pos'th char is at.
  void PositionToXY(int pos, unsigned int *line, unsigned int *col);

  //! Determines which index the char at the position "x chars left, y chars down" is at.
  int XYToPosition(int x, int y);

  //! The screen coordinates of the cursor
  wxPoint PositionToPoint(int fontsize, int pos = -1);

  //! Sets the cursor to the screen coordinate point
  void SelectPointText(wxDC &dc, wxPoint &point);

  //! Selects the text between the screen coordinates one and two
  void SelectRectText(wxDC &dc, wxPoint &one, wxPoint &two);

  //! Selects the word the cursor is currently at.
  wxString SelectWordUnderCaret(bool selectParens = true, bool toRight = true);

  //! Is the point point inside the currently selected text?
  bool IsPointInSelection(wxDC &dc, wxPoint point);

  bool CopyToClipboard();

  bool CutToClipboard();

  void PasteFromClipboard(bool primary = false);

  //! Get the character position the selection has been started with
  int GetSelectionStart()
  { return m_selectionStart; }

  //! Get the character position the selection has been ended with
  int GetSelectionEnd()
  { return m_selectionEnd; }

  //! Select the whole text contained in this Cell
  void SelectAll()
  {
    m_selectionStart = 0;
    m_selectionEnd = m_positionOfCaret = m_text.Length();
  }

  //! Does the selection currently span the whole cell?
  bool AllSelected()
  {
    return (m_selectionStart == 0) && (m_selectionEnd == (long) m_text.Length());
  }

  //! Unselect everything.
  void SelectNone()
  {
    m_selectionStart = m_selectionEnd = 0;
  }

  //! Is there any text selected right now?
  bool SelectionActive()
  {
    return (m_selectionStart >= 0) && (m_selectionEnd >= 0);
  }

  bool CanCopy()
  {
    return m_selectionStart != -1;
  }

  bool FindMatchingQuotes();

  void FindMatchingParens();

  int GetLineWidth(wxDC &dc, unsigned int line, int end);

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

  void SetFirstLineOnly(bool show = true)
  {
    if (m_firstLineOnly != show)
    {
      m_width = m_height = -1;
      m_firstLineOnly = show;
    }
    // Style the text anew.
    StyleText();
  }

  bool IsActive()
  { return this == m_cellPointers->m_activeCell; }

  //! Is the cursor at the start of this cell?
  bool CaretAtStart()
  { return m_positionOfCaret == 0; }

  //! Move the cursor to the start of this cell
  void CaretToStart();

  //! Is the cursor at the end of this cell?
  bool CaretAtEnd()
  { return m_positionOfCaret == (long) m_text.Length(); }

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

  /*! DivideAtCaret
    Returns the string from caret to end and
    modifies the m_text so it contains only the string
    from beginning to caret
    Used for 'Divide Cell', called from MathCtrl
  */
  wxString DivideAtCaret();

  void CommentSelection();

  void ClearUndo();

  //! Query if this cell needs to be re-evaluated by maxima
  bool ContainsChanges()
  { return m_containsChanges; }

  //! Set the information if this cell needs to be re-evaluated by maxima
  void ContainsChanges(bool changes)
  { m_containsChanges = m_containsChangesCheck = changes; }

  bool CheckChanges();

  /*! Replaces all occurrences of a given string

    TODO: Implement the IgnoreCase case.
   */
  int ReplaceAll(wxString oldString, wxString newString, bool IgnoreCase);

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
    *start = m_selectionStart;
    *end = m_selectionEnd;
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
  bool ReplaceSelection(wxString oldStr, wxString newString, bool keepSelected = false, bool ignoreCase = false);

  //! Convert the current selection to a string
  wxString GetSelectionString();

  //! Unselect everything
  void ClearSelection();

  //! Sets the index the error is at
  void SetErrorIndex(int index){m_errorIndex = index;}

  bool ErrorIndexSet(){return m_errorIndex >= 0;}

  void GotoError(){SetCaretPosition(m_errorIndex);ActivateCursor();}

  //! Get the cursor's current position inside the cell.
  int GetCaretPosition()
  { return m_positionOfCaret; }

  //! Set the cursor's current position inside the cell.
  void SetCaretPosition(int pos)
    { m_positionOfCaret = pos;
      if(m_positionOfCaret < -1)
        m_positionOfCaret = -1;
      if(m_positionOfCaret > m_text.Length())
        m_positionOfCaret = m_text.Length();
    }

  bool FindNextTemplate(bool left = false);

  void InsertText(wxString text);

  wxString TextInFrontOfSelection()
  {
    return GetValue().Mid(1, m_selectionStart);
  }

  //! Return to the selection after the cell has been left upwards
  void ReturnToSelectionFromTop()
  {
    SetSelection(m_lastSelectionStart, 0);
  }

  //! Return to the selection after the cell has been left downwards
  void ReturnToSelectionFromBot()
  {
    SetSelection(m_lastSelectionStart, m_text.Length());
  }

private:
  //! Mark this cell as "Automatically answer questions".
  bool m_autoAnswer;
  /*! Divide a string into tokens

    Used when styling text.
   */
  wxArrayString StringToTokens(wxString string);

#if defined __WXMAC__

  bool HandleCtrlCommand(wxKeyEvent &ev);

#endif

  bool HandleSpecialKey(wxKeyEvent &ev);

  bool HandleOrdinaryKey(wxKeyEvent &ev);

  bool IsAlpha(wxChar c);

  bool IsNum(wxChar c);

  bool IsAlphaNum(wxChar c);
  
  virtual wxString GetToolTip(const wxPoint &point)
    {
      if(ContainsPoint(point))
      {
        m_cellPointers->m_cellUnderPointer = this;
        return m_toolTip;
      }
      else
        return wxEmptyString;
    }
  /*! A piece of styled text for syntax highlighting

    A piece of styled text may be
     - a text line
     - a command, parenthesis, number, line ending
     - '\n'
     - whitespace
     - '\r' indicating a soft line break optionally equipped with indentation
       and a character that marks a continued quote or similar 
   */
  class StyledText
  {
  private:
    //! The color of this text portion
    TextStyle m_style;
    //! The text of this text portion
    wxString m_text;
    //! Do we really want to style this text portion different than the default?
    bool m_styleThisText;
    //! By How many pixels we want to indent this line?
    int m_indentPixels;
    //! Chars that mark continued indentation
    wxString m_indentChar;
  public:
    //! Defines a piece of styled text
    StyledText(TextStyle style, wxString text)
    {
      m_text = text;
      m_style = style;
      m_styleThisText = true;
      m_indentPixels = 0;
      m_indentChar = wxEmptyString;
    }

    /*! Defines a piece of text with the default style that possibly is indented
     */
    StyledText(wxString text, int indentPixels = 0, wxString indentChar = wxEmptyString)
    {
      m_text = text;
      m_style = TS_DEFAULT;
      m_styleThisText = false;
      m_indentPixels = indentPixels;
      m_indentChar = indentChar;
    }
    
    //! Returns the piece of text
    wxString GetText()
    {
      return m_text;
    }

    //! Changes the piece of text kept in this token
    void SetText(wxString text)
    {
      m_text = text;
    }

    //! Changes the indentation level of this token
    void SetIndentation(int indentPixels, wxString indentString = wxEmptyString)
    {
      m_indentPixels = indentPixels;
      m_indentChar = indentString;
    }

    //! By how many pixels do we need to indent this line due to a bullet list or similar?
    int GetIndentPixels()
    {
      return m_indentPixels;
    }

    wxString GetIndentChar()
    {
      return m_indentChar;
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

  std::vector<StyledText> m_styledText;

  /*! Adds soft line breaks to code cells, if needed.

    \todo: We could do an incremental indenation calculation that starts at the last word: 
    The current behavior is O(n^2) (scanning the text needs linear time and for each word 
    the indentation algorithm scans the text again) which is unfortunate.
   */
  void HandleSoftLineBreaks_Code(StyledText *&lastSpace, int &lineWidth, const wxString &token, unsigned int charInCell,
                                 wxString &text, size_t &lastSpacePos, bool spaceIsIndentation, int &indentationPixels);

  /*! How many chars do we need to indent text at the position the caret is currently at?

    \todo We should provide an alternative function that allows to resume the calculation
    for the next word/line - which would provide an additional speedup.
   */
  int GetIndentDepth(wxString text, int positionOfCaret);

#if wxUSE_UNICODE

  /*! Handle ESC shortcuts for special characters

    These characters can be tought to LaTeX and the html browser if necessary in
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
  unsigned int m_numberOfLines;
  int m_fontSize;
  /*! The font size we were called with  the last time

    We need to know this in order to be able to detect we need a full recalculation.
   */
  double m_fontSize_Last;
  int m_charHeight;
  int m_paren1, m_paren2;
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
