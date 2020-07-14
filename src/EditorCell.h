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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

#ifndef EDITORCELL_H
#define EDITORCELL_H

#include "Cell.h"
#include "MaximaTokenizer.h"
#include <vector>
#include <list>

/*! \file

  This file contains the definition of the class EditorCell
 */

/*! This class defines what the user sees as input cell
  
  This class handles input cells including:
    - The per-cell undo buffer
    - The handling of key presses when this cell is active
  
  Since Unicode doesn't provide us with a "soft linebreak" letter we 
  use <code>\\r</code> as a marker that this line has to be broken here if we
  don't want it to extend beyond the right margin of the screen.

  In a few places we use wxString::const_iterator instead of accessing individual 
  letters within the string using the [] operator. This might look overly
  complicated. But in UTF-8 all non-standard-ASCII-characters use more than one
  byte making iterating over every single char of the string the only way of
  determining which address char n is at. An iterator is the only way of not
  having to determine the address of every single char independently.

  \todo Draw only tokens that are in the redraw region.
 */
class EditorCell final : public Cell
{
private:
  #if wxUSE_ACCESSIBILITY
  wxAccStatus GetDescription(int childId, wxString *description) override;
  wxAccStatus GetFocus (int *childId, wxAccessible **child) override;
  wxAccStatus GetDefaultAction(int childId, wxString *actionName) override;
  wxAccStatus GetValue (int childId, wxString *strValue) override;
  wxAccStatus GetRole (int childId, wxAccRole *role) override;
  #endif

public:
  //! The constructor
  EditorCell(GroupCell *parent, Configuration **config, const wxString &text = {});
  EditorCell(const EditorCell &cell);
  Cell *Copy() const override {return new EditorCell(*this);}

  //! Insert the symbol that corresponds to the ESC command txt
  void InsertEscCommand(const wxString &txt) {
    InsertText(InterpretEscapeString(txt));
  }

  //! Get the whole maxima command that is currently under the cursor (including all arguments)
  wxString GetFullCommandUnderCursor();

  //! Add a new parameter to a draw- or similar command including the comma, if needed.
  void AddDrawParameter(wxString param);

  //! May this Editor Cell contain the answer to a question?
  void AutoAnswer(bool autoAnswer){m_autoAnswer = autoAnswer;}

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

  //! A list of words that might be applicable to the autocomplete function.
  const wxArrayString &GetWordList() const { return m_wordList; }

  /*! Expand all tabulators.

    \param input The string the tabulators should be expanded in
    \param posInLine The number of characters that come before the input in the same line
    \todo Implement the actual TAB expansion
  */
  static wxString TabExpand(const wxString &input, long posInLine);

  //! Escape all chars that cannot be used in HTML otherwise
  static wxString EscapeHTMLChars(wxString input);

  //! Convert all but the first of a row of multiple spaces to non-breakable
  static wxString PrependNBSP(wxString input);

  //! Recalculate the widths of the current cell.
  void RecalculateWidths(AFontSize fontsize) override;

  virtual void Draw(wxPoint point) override;

  //! Convert the current cell to HTML code.
  wxString ToHTML() const;
  wxString ToMatlab() const override;
  /*! Convert the current cell to a string
  
    \param dontLimitToSelection
    - false: If text is selected return only the selected text
    - true:  Always return all text in this text cell
  */
  //! Convert the current cell to Matlab code
  wxString ToMatlab(bool dontLimitToSelection) const;
  //! Convert the current cell to RTF code
  wxString ToRTF() const override;
  wxString ToString() const override;
  wxString ToString(bool dontLimitToSelection) const;
  //! Convert the current cell to LaTeX code
  wxString ToTeX() const override;
  //! Convert the current cell to XML code for inclusion in a .wxmx file.
  wxString ToXML() const override;

  //! Set the currently used font to the one that matches this cell's formatting
  void SetFont();

  //! Sets the current color to this cell's foreground color
  void SetForeground();

  /*! Sets the text that is to be displayed.
    
    Automatically calls StyleText().
   */
  void SetValue(const wxString &text) override;

  /*! Returns the text contained in this cell

    Naturally all soft line breaks are converted back to spaces beforehand.
   */
  const wxString &GetValue() const override { return m_text; }

  /*! Converts m_text to a list of styled text snippets that will later be displayed by draw().

    This function also generates a wordlist for this EditorCell so Autocompletion can learn
    about variable names contained in lists or cells that still haven't been evaluated.

    For cells containing text instead of code this function adds a <code>\\r</code> as a marker
    that this line is to be broken here until the window's width changes.
   */
  void StyleText();
  /*! Is Called by StyleText() if this is a code cell */
  void StyleTextCode();
  void StyleTextTexts();

  void Reset();

  //! Decide what to do if the user pressed a key when this cell was selected
  void ProcessEvent(wxKeyEvent &event) override;

  /*! Activate the blinking cursor in this cell
    
    Automatically calls DeactivateCursor on an eventual cell the cursor currently is in.
    Normally Mathctrl::SetActiveCell() is used in order to get this function called.
   */
  void ActivateCursor();

  //! Deactivate the blinking cursor in the EditorCell it is in.
  void DeactivateCursor();

  //! Return the index of the 1st char of the line containing the letter pos.
  size_t BeginningOfLine(long pos) const;

  //! Return the index of the last char of the line containing the letter \#pos,
  size_t EndOfLine(long pos);

  //! Adds a ";" to the end of the last command in this cell in case that it doesn't end in $ or ;
  bool AddEnding() override;

  //! Determines which line and column the pos'th char is at.
  void PositionToXY(int position, unsigned int *x, unsigned int *y);

  //! Determines which index the char at the position "x chars left, y chars down" is at.
  int XYToPosition(int x, int y);

  //! The screen coordinates of the cursor
  wxPoint PositionToPoint(AFontSize fontsize, int pos = -1) override;

  //! Sets the cursor to the screen coordinate point
  void SelectPointText(wxPoint point) override;

  //! Selects the text between the screen coordinates one and two
  void SelectRectText(wxPoint one, wxPoint two) override;

  //! Selects the word the cursor is currently at.
  wxString SelectWordUnderCaret(bool selectParens = true, bool toRight = true,
                                bool includeDoubleQuotes = false);

  //! Is the point point inside the currently selected text?
  bool IsPointInSelection(wxPoint point);

  bool CopyToClipboard() const override;

  bool CutToClipboard() override;

  void PasteFromClipboard(bool primary = false) override;

  //! Get the character position the selection has been started with
  int GetSelectionStart() const
  { return m_selectionStart; }

  //! Get the character position the selection has been ended with
  int GetSelectionEnd() const
  { return m_selectionEnd; }

  //! Select the whole text contained in this Cell
  void SelectAll() override
  {
    m_selectionStart = 0;
    m_selectionEnd = m_positionOfCaret = m_text.Length();
  }

  //! Does the selection currently span the whole cell?
  bool AllSelected() const
  {
    return (m_selectionStart == 0) && (m_selectionEnd == (long) m_text.Length());
  }

  //! Unselect everything.
  void SelectNone()
  {
    m_selectionStart = m_selectionEnd = 0;
  }

  //! Is there any text selected right now?
  bool SelectionActive() const
  {
    return (m_selectionStart >= 0) && (m_selectionEnd >= 0);
  }

  bool CanCopy() const override
  {
    return m_selectionStart != -1;
  }

  bool FindMatchingQuotes();

  void FindMatchingParens();

  int GetLineWidth(unsigned int line, int pos);

  //! true, if this cell's width has to be recalculated.
  bool IsDirty() const override
  {
    return m_isDirty;
  }

  //! Toggles the visibility of the cursor which is used to make it blink.
  void SwitchCaretDisplay() override
  {
    m_displayCaret = !m_displayCaret;
  }

  void SetFocus(bool focus) override
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

  bool IsActive() const override
  { return this == m_cellPointers->m_activeCell; }

  //! Is the cursor at the start of this cell?
  bool CaretAtStart() const
  { return m_positionOfCaret == 0; }

  //! Move the cursor to the start of this cell
  void CaretToStart();

  //! Is the cursor at the end of this cell?
  bool CaretAtEnd() const
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
  bool ContainsChanges() const
  { return m_containsChanges; }

  //! Set the information if this cell needs to be re-evaluated by maxima
  void ContainsChanges(bool changes)
  { m_containsChanges = m_containsChangesCheck = changes; }

  bool CheckChanges();

  /*! Replaces all occurrences of a given string
   */
  int ReplaceAll(wxString oldString, const wxString &newString, bool ignoreCase);

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

  bool IsSelectionChanged() const { return m_selectionChanged; }

  void SetSelection(int start, int end);

  void GetSelection(int *start, int *end) const
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
    \param ignoreCase true = ignore the case of the string to replace
    \param replaceMaximaString true = replace strings including the double quotes.
   */
  bool ReplaceSelection(const wxString &oldStr, const wxString &newString,
                        bool keepSelected = false, bool ignoreCase = false,
                        bool replaceMaximaString = false);

  //! Convert the current selection to a string
  wxString GetSelectionString() const;

  //! The word the cursor currently is at
  wxString GetWordUnderCaret();

  //! Get the command the cursor is in the arguments for.
  wxString GetCurrentCommand();
  
  //! Unselect everything
  void ClearSelection();

  //! Sets the index the error is at
  void SetErrorIndex(int index){m_errorIndex = index;}

  bool ErrorIndexSet() const {return m_errorIndex >= 0;}

  void GotoError(){SetCaretPosition(m_errorIndex);ActivateCursor();}

  //! Start a new line and optionally auto-indent it.
  void ProcessNewline(bool keepCursorAtStartOfLine = true);

  //! Get the cursor's current position inside the cell.
  int GetCaretPosition() const
  { return m_positionOfCaret; }

  //! Convert a number to unicode chars.
  void ConvertNumToUNicodeChar();

  //! Set the cursor's current position inside the cell.
  void SetCaretPosition(int pos)
    { m_positionOfCaret = pos;
      if(m_positionOfCaret < -1)
        m_positionOfCaret = -1;
      if(m_positionOfCaret > (signed long)m_text.Length())
        m_positionOfCaret = m_text.Length();
    }

  bool FindNextTemplate(bool left = false);

  void InsertText(wxString text);

  wxString TextInFrontOfSelection() const
  {
    return GetValue().Mid(1, m_selectionStart);
  }

  //! Return to the selection after the cell has been left upwards
  void ReturnToSelectionFromTop()
  {
    SetSelection(m_lastSelectionStart, 0);
  }

  void SetType(CellType type) override;
  void SetStyle(TextStyle style) override;

  //! Return to the selection after the cell has been left downwards
  void ReturnToSelectionFromBot()
  {
    SetSelection(m_lastSelectionStart, m_text.Length());
  }

  //! Get the list of commands, parenthesis, strings and whitespaces in a code cell
  const MaximaTokenizer::TokenList &GetTokens() const {return m_tokens;}

  void SetNextToDraw(Cell *next) override;

  Cell *GetNextToDraw() const override {return m_nextToDraw;}

private:
  /*! A piece of styled text for syntax highlighting

    A piece of styled text may be
     - a text line
     - a command, parenthesis, number, line ending
     - '\n'
     - whitespace
     - '\\r' indicating a soft line break optionally equipped with indentation
       and a character that marks a continued quote or similar 
   */
  class StyledText
  {
  private:
    //! The text of this text portion
    wxString m_text;
    //! Chars that mark continued indentation
    wxString m_indentChar;
    //! The color of this text portion
    TextStyle m_style = TS_DEFAULT;
    //! The cached width of this piece of text
    int m_width = -1;
    //! By How many pixels we want to indent this line?
    int m_indentPixels = 0;
    //! Do we really want to style this text portion different than the default?
    bool m_styleThisText = false;
  public:
    //! Defines a piece of styled text
    StyledText(TextStyle style, const wxString &text)
        : m_text(text), m_style(style), m_styleThisText(true) {}

    //! Defines a piece of text with the default style that possibly is indented
    explicit StyledText(const wxString &text, int indentPixels = 0,
                        const wxString &indentChar = {})
        : m_text(text), m_indentChar(indentChar), m_indentPixels(indentPixels)  {}

    void SetWidth(int width){m_width = width;}
    void ResetSize(){SetWidth(-1);}
    int GetWidth() const {return m_width;}
    bool SizeKnown() const {return GetWidth() >= 0;}
    //! Returns the piece of text
    const wxString &GetText() const { return m_text; }
    //! Changes the piece of text kept in this token
    void SetText(const wxString &text) { m_text = text; }
    //! Changes the indentation level of this token
    void SetIndentation(int indentPixels, const wxString &indentString = {})
    {
      m_indentPixels = indentPixels;
      m_indentChar = indentString;
    }
    //! By how many pixels do we need to indent this line due to a bullet list or similar?
    int GetIndentPixels() const { return m_indentPixels; }
    const wxString &GetIndentChar() const { return m_indentChar; }
    //! If IsStyleSet() is true this function returns the style of this text
    //! portion
    TextStyle GetStyle() const { return m_style; }
    // Has a individual text style been set for this text portion?
    bool IsStyleSet() const { return m_styleThisText; }
  };

#if defined __WXOSX__
  bool HandleCtrlCommand(wxKeyEvent &ev);
#endif
  bool HandleSpecialKey(wxKeyEvent &event);
  bool HandleOrdinaryKey(wxKeyEvent &event);

  void FontsChanged() override
  {
    ResetSize();
    ResetData();
    m_widths.clear();
  }

  /*! Adds soft line breaks to code cells, if needed.

    \todo: We could do an incremental indentation calculation that starts at the last word: 
    The current behavior is O(n^2) (scanning the text needs linear time and for each word 
    the indentation algorithm scans the text again) which is unfortunate.
   */
  void HandleSoftLineBreaks_Code(StyledText *&lastSpace, int &lineWidth, const wxString &token, unsigned int charInCell,
                                 wxString &text, const size_t &lastSpacePos, int &indentationPixels);

  /*! How many chars do we need to indent text at the position the caret is currently at?

    \todo We should provide an alternative function that allows to resume the calculation
    for the next word/line - which would provide an additional speedup.
   */
  int GetIndentDepth(wxString text, int positionOfCaret);

  /*! Handle ESC shortcuts for special characters

    These characters can be tought to LaTeX and the html browser if necessary in
    TextCell::ToTeX and EditorCell::ToTeX. They can also be
    converted to maxima strings in wxMaxima::SendMaxima.
   */
  wxString InterpretEscapeString(const wxString &txt) const;

  //! Draw a box that marks the current selection
  void MarkSelection(long start, long end, TextStyle style, AFontSize fontsize);

  //! Determines the size of a text snippet
  wxSize GetTextSize(const wxString &text);

//** Large fields
//**
  WX_DECLARE_STRING_HASH_MAP(wxSize, StringHash);
  //! Cached widths of text snippets, one width per style
  StringHash m_widths;

  //! A list of all potential autoComplete targets within this cell
  wxArrayString m_wordList;

  //! The individual commands, parenthesis, strings and whitespaces a code cell consists of
  MaximaTokenizer::TokenList m_tokens;

  wxString m_text;
  std::vector<StyledText> m_styledText;

  std::vector<wxString> m_textHistory;
  std::vector<int> m_positionHistory;
  std::vector<int> m_startHistory;
  std::vector<int> m_endHistory;

//** 8/4 bytes
//**
  AFontName m_fontName;
  CellPtr<Cell> m_nextToDraw;
  //! Where in the undo history are we?
  ptrdiff_t m_historyPosition = -1;

//** 4 bytes
//**
  int m_errorIndex = 1;
  unsigned int m_numberOfLines = 1;

  /*! The start of the current selection.

     - >0: the position of the cursors in characters from start
     - -1: Currently no selection is active

     If the selection has been done from right to left m_selectionStart>m_selectionEnd.
   */
  long m_selectionStart = -1;
  /*! The end of the current selection.

     - >0: the position of the cursors in characters from start
     - -1: Currently no selection is active

     If the selection has been done from right to left m_selectionStart>m_selectionEnd.
   */
  long m_selectionEnd = -1;
  long m_oldSelectionStart = -1;
  long m_oldSelectionEnd = -1;
  long m_lastSelectionStart = -1;

  int m_charHeight = 12;
  int m_paren1 = -1, m_paren2 = -1;

  //! Where inside this cell is the cursor?
  int m_positionOfCaret = 0;
  //! Which column the cursor would be if the current line were long enough?
  //! Used when moving up/down between lines
  int m_caretColumn = -1;

  wxFontStyle m_fontStyle = wxFONTSTYLE_NORMAL;
  wxFontWeight m_fontWeight = wxFONTWEIGHT_NORMAL;

//** 2 bytes
//**
  /*! The font size we were called with  the last time

    We need to know this in order to be able to detect we need a full recalculation.
   */
  AFontSize m_fontSize_Last;

//** 1 byte
//**

  //! Mark this cell as "Automatically answer questions".
  bool m_autoAnswer = false;
  //! true, if this function has changed since the last evaluation by maxima
  bool m_containsChanges = false;
  bool m_containsChangesCheck = false;
  bool m_displayCaret = false;
  bool m_firstLineOnly = false;
  bool m_hasFocus = false;
  bool m_isDirty = false;
  bool m_saveValue = false;
  //! Has the selection changed since the last draw event?
  bool m_selectionChanged = false;
  //! Does this cell's size have to be recalculated?
  bool m_underlined = false;
};

#endif // EDITORCELL_H
