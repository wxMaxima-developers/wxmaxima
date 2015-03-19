// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2012-2013 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2015      Gunter Königsmann <wxMaxima@physikbuch.de>
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

#ifndef MATHCTRL_H
#define MATHCTRL_H

#include <wx/wx.h>
#include <wx/aui/aui.h>
#include <wx/textfile.h>

#include "MathCell.h"
#include "EditorCell.h"
#include "GroupCell.h"
#include "EvaluationQueue.h"
#include "Autocomplete.h"
#include "Structure.h"
#include "ToolBar.h"

/*! The canvas that contains the spreadsheet the whole program is about.

This canvas contains all the math, title, image etc.- cells of the current session.
 */
class MathCtrl: public wxScrolledCanvas
{
public:

  /*! The ids for all popup menu items.

    \attention popid_complete_00 has to stay the last event in this enum
    Since we want to be able to dynamically add events to popups
    (for example for autocompletion) we need to make sure we know what 
    the item with the lowest ID is we can assign dynamically.
  */
  enum PopIds{
    /*! The "copy" popup menu item was clicked

      This item is the first of the enum and is assigned a high enough number
      that it won't collide with the numbers to be found in wxFrame::Event
     */
    popid_copy = wxID_HIGHEST + 500,
    popid_cut,
    popid_paste,
    popid_select_all,
    popid_comment_selection,
    popid_divide_cell,
    popid_copy_image,
    popid_delete,
    popid_simplify,
    popid_expand,
    popid_factor,
    popid_solve,
    popid_solve_num,
    popid_integrate,
    popid_diff,
    popid_subst,
    popid_plot2d,
    popid_plot3d,
    popid_float,
    popid_edit,
    popid_add_comment,
    popid_insert_input,
    popid_copy_tex,
    popid_image,
    popid_animation_save,
    popid_animation_start,
    popid_evaluate,
    popid_merge_cells,
    popid_insert_text,
    popid_insert_title,
    popid_insert_section,
    popid_insert_subsection,
    /*! The first number that is open for dynamic ID assignment

      If we want to add additional elements to a pop-up this is the 
      lowest ID that is guaranteed to be free for this purpose.
      \attention popid_must be the last id in this list
     */
    popid_complete_00
  };
  
  //! The constructor
  MathCtrl(wxWindow* parent, int id, wxPoint pos, wxSize size);
  //! The destructor
  ~MathCtrl();
  wxTimer m_keyboardInactiveTimer;
  //! Reads true if the keyboard was inactive for > 10 seconds
  bool m_keyboardInactive;

  void DestroyTree();
  void DestroyTree(MathCell* tree);
  MathCell* CopyTree();
  GroupCell *InsertGroupCells(GroupCell* tree, GroupCell* where = NULL);

  //! Add a new line to working group or m_last
  void InsertLine(MathCell *newLine, bool forceNewLine = false);
  void Recalculate(bool force = false);
  void RecalculateForce();
  /*! Empties the current document

    Used before opening a new file or when the "new" button is pressed.
  */
  void ClearDocument(); 
  void ResetInputPrompts();
  bool CanCopy(bool fromActive = false) {
    return m_selectionStart != NULL ||
           (fromActive && m_activeCell != NULL && m_activeCell->CanCopy());
  }
  bool CanPaste() { return (m_activeCell != NULL) || (m_hCaretActive);
  }
  bool CanCut() {
    return (m_activeCell != NULL && m_activeCell->CanCopy()) ||
           (m_selectionStart != NULL && m_selectionStart->GetType() == MC_TYPE_GROUP);
  }
  void SelectAll();
  //! Is it possible to delete the currently selected cells?
  bool CanDeleteSelection();
  bool CanAnimate() {
    return m_selectionStart != NULL && m_selectionStart == m_selectionEnd &&
      m_selectionStart->GetType() == MC_TYPE_SLIDE;
  }
  void Animate(bool run);
  //! Delete the currently selected cells
  void DeleteSelection();
  void DivideCell();
  void MergeCells();
  bool CutToClipboard();
  void PasteFromClipboard(bool primary = false);
  /*! Copy the current selection to the clipboard

    \param astext
     - true:  Copy the current selection as text
     - false: Copy the current selection as they would appear in a .wxm file
   */
  bool Copy(bool astext = false);
  //! Copy the selection to the clipboard as it would appear in a .wxm file
  bool CopyCells();
  //! Copy the TeX representation of the current selection to the clipboard
  bool CopyTeX();
  //! Copy a bitmap of the the current selection to the clipboard
  bool CopyBitmap();
  bool CopyToFile(wxString file);
  bool CopyToFile(wxString file, MathCell* start, MathCell* end, bool asData = false);
  void CalculateReorderedCellIndices(MathCell *tree, int &cellIndex, std::vector<int>& cellMap);
  //! Export the file to an html document
  bool ExportToHTML(wxString file);
  //! Export a region of the file to a .wxm or .mac file maxima's load command can read
  void ExportToMAC(wxTextFile& output, MathCell *tree, bool wxm, const std::vector<int>& cellMap, bool fixReorderedIndices);
  //! Export the file to a text file maxima's load command can read
  bool ExportToMAC(wxString file);
  //! export to xml compatible file
  bool ExportToWXMX(wxString file);	
  //! export to a LaTeX file
  bool ExportToTeX(wxString file);
  /*! Convert the current selection to a string

    \param lb
     - true:  Include linebreaks
     - false: Remove linebreaks from the converted string
   */
  wxString GetString(bool lb = false);
  MathCell* GetTree() { return m_tree; }
  /*! Return the first of the currently selected cells.

    NULL means: No cell is selected.
  */
  MathCell* GetSelectionStart() { return m_selectionStart; }
  /*! Return the last of the currently selected cells.

    NULL means: No cell is selected.
  */
  MathCell* GetSelectionEnd() { return m_selectionEnd; }
  //! Select the cell sel
  void SetSelection(MathCell* sel) { m_selectionStart = m_selectionEnd = sel; }
  bool CanEdit();
  void EnableEdit(bool enable = true) { m_editingEnabled = enable; }
  bool ActivatePrevInput();
  bool ActivateNextInput(bool input = false);
  //! Scrolls to a given cell
  void ScrollToCell(MathCell *cell);
  //! Returns the cell the cursor that is drawn as a vertical line is in.
  EditorCell* GetActiveCell() { return m_activeCell; }
  void ShowPoint(wxPoint point);
  void OnSetFocus(wxFocusEvent& event);
  void OnKillFocus(wxFocusEvent& event);
  bool IsSelected(int type);
  bool AnimationRunning() { return m_animate; }
  bool IsActiveInLast() { return m_activeCell != NULL && m_activeCell->GetParent() == m_last; }
  void SetWorkingGroup(GroupCell *group);
  bool IsSelectionInWorking();
  void SetActiveCell(EditorCell *cell, bool callRefresh = true);
  void SetDefaultHCaret();
  void SetHCaret(MathCell *where, bool callRefresh = true); // call with false, when manually refreshing
  //! The cell the horizontal cursor is above. NULL means at the start of the document.
  GroupCell *GetHCaret();
  //! Place the cursor into a new cell where the horizontal cursor is
  void OpenHCaret(wxString txt = wxEmptyString, int type = GC_TYPE_CODE);
  void ShowHCaret();
  /*! Is it possible to issue an undo in the currently selected cell? 

    \return false if no cell is selected or there is no further undo information
   */
  bool CanUndo();
  /*! Is it possible to issue an undo in the currently selected cell?

    \return false if no cell is selected or no redo can be executed.
   */
  bool CanRedo();
  /*! Issue an undo in the currently selected cell

    \todo Currently the deletion of cells cannot be undone: If no cell is selected 
          there is no undo.
   */
  void Undo();
  /*! Issue a redo in the currently selected cell
    
    This command is ignored if no cell is selected.
  */
  void Redo();

  /*! Do we want to follow the evaluation process?

    Maxima can automagically scroll to the cell that is currently evaluated.
    If it does do so can be queried by FollowEvaluation(). Changing the 
    behavior (for example because the user has scrolled away from the
    cell being evaluated and now clearly wants the cursor to stay where
    it is) can be archieved by FollowEvaluation(true) or 
    FollowEvaluation(false).
   */
  void FollowEvaluation(bool FollowEvaluation);
  //! Query if we want to automatically scroll to the cell that is currently evaluated
  bool FollowEvaluation() {return m_followEvaluation;}

  /*! Set or get the "Scrolled away from evaluation" status

    Sets FollowEvaluation() to false and enables the toolbar button to follow the
    evaluation process again.
   */
  void ScrolledAwayFromEvaluation(bool ScrolledAway);
  bool ScrolledAwayFromEvaluation()
  { return m_scrolledAwayFromEvaluation;}

  void SaveValue();
  bool IsSaved() { return m_saved; }
  void SetSaved(bool saved) { m_saved = saved; }
  void RemoveAllOutput();
  void RemoveAllOutput(GroupCell* cell);
  // methods related to evaluation queue

  void AddDocumentToEvaluationQueue();
  
  //! Schedule all cells in the document for evaluation
  void AddEntireDocumentToEvaluationQueue();
  //! Schedule all cells stopping with the one the caret is in for evaluation
  void AddDocumentTillHereToEvaluationQueue();
  //! Schedule all selected cells to be evaluated
  void AddSelectionToEvaluationQueue();
  //! Schedule this cell for evaluation
  void AddCellToEvaluationQueue(GroupCell* gc);
  //! Unschedule all cells marked as to be evaluated
  void ClearEvaluationQueue();
  //! The list of cells that have to be evaluated
  EvaluationQueue* m_evaluationQueue;
  // methods for folding
  GroupCell *UpdateMLast();
  void FoldOccurred();
  //! Fold or unfold a cell
  GroupCell *ToggleFold(GroupCell *which);
  GroupCell *ToggleFoldAll(GroupCell *which);
  void FoldAll();
  void UnfoldAll();
  GroupCell *TearOutTree(GroupCell *start, GroupCell *end);
  // methods for zooming the document in and out
  double GetZoomFactor() { return m_zoomFactor; }
  void SetZoomFactor(double newzoom, bool recalc = true) { m_zoomFactor = newzoom;
    if (recalc) {RecalculateForce(); Refresh();} }
  void CommentSelection();
  void OnMouseWheel(wxMouseEvent &ev);
  //! Called if the user is scrolling through the document.
  void OnScrollChanged(wxScrollEvent &ev);
  /*! Find the next ocourrence of a string

    Used by the find dialog.
   */
  bool FindNext(wxString str, bool down, bool ignoreCase);
  /*! Replace the current ocourrence of a string

    Used by the find dialog.
   */
  void Replace(wxString oldString, wxString newString);
  /*! Replace all ocourrences of a string

    Used by the find dialog.
   */
  int ReplaceAll(wxString oldString, wxString newString);
  wxString GetInputAboveCaret();
  wxString GetOutputAboveCaret();
  bool LoadSymbols(wxString file) { return m_autocomplete.LoadSymbols(file); }
  bool Autocomplete(bool templates = false);
  void AddSymbol(wxString fun, bool templ = false) { m_autocomplete.AddSymbol(fun, templ); }
  void SetActiveCellText(wxString text);
  bool InsertText(wxString text);
  GroupCell *GetWorkingGroup() { return m_workingGroup; }
  void OpenNextOrCreateCell();
  //! The table of contents pane
  Structure*    m_structure;
  //! Called when the "Scroll to currently evaluated" button is pressed.
  void OnFollow();
  //! The toolbar of the main window: We need to access it and therefore have it defined here.
  ToolBar *m_mainToolBar;
  //! Set this cell as the currently selected one
  void SelectGroupCell(GroupCell *cell);
  //! Mark the current question from maxima as "answered"..
  void QuestionAnswered();
  //! true = the last reply from maxima was a question
  bool m_questionPrompt;
  //! Does the GroupCell cell points to contain the question currently asked by maxima?  
  bool GCContainsCurrentQuestion(GroupCell *cell);
  /*! Move the cursor to the end of the question and if needed add a cell for user input
   */
  void OpenQuestionCaret(wxString txt=wxT(""));

 private:
  bool m_scrolledAwayFromEvaluation;

  //! The cell the current question from maxima is being kept in.
  EditorCell *m_answerCell;
  /*! Escape all chars that aren't allowed in html.

    Also converts \n to <BR>
   */
  wxString EscapeHTMLChars(wxString input);
  //! Allow indentation by spaces for html by replacing them by non-breakable spaces
  wxString PrependNBSP(wxString input);
  //! An enum for all classes of items one can click on
  enum ClickType { 
    CLICK_TYPE_NONE,
    CLICK_TYPE_GROUP_SELECTION,
    CLICK_TYPE_INPUT_SELECTION,
    CLICK_TYPE_OUTPUT_SELECTION
  };

  //! An enum of individual IDs for all timers this class handles
  enum TimerIDs
  {
    TIMER_ID,
    CARET_TIMER_ID,
    ANIMATION_TIMER_ID
  };

  //! Add a line to a file.
  void AddLineToFile(wxTextFile& output, wxString s, bool unicode = true);
  MathCell* CopySelection();
  MathCell* CopySelection(MathCell* start, MathCell* end, bool asData = false);

  void GetMaxPoint(int* width, int* height);
  void OnTimer(wxTimerEvent& event);
  bool m_autoSaveIntervalExpired;
  void OnMouseExit(wxMouseEvent& event);
  void OnMouseEnter(wxMouseEvent& event);
  void OnPaint(wxPaintEvent& event);
  void OnSize(wxSizeEvent& event);
  void OnMouseRightDown(wxMouseEvent& event);
  void OnMouseLeftUp(wxMouseEvent& event);
  void OnMouseLeftDown(wxMouseEvent& event);
  void OnMouseLeftInGcCell(wxMouseEvent& event, GroupCell *clickedInGC);
  void OnMouseLeftInGcLeft(wxMouseEvent& event, GroupCell *clickedInGC);
  void OnMouseLeftInGc(wxMouseEvent& event, GroupCell *clickedInGC);
  void OnMouseMotion(wxMouseEvent& event);
  void OnDoubleClick(wxMouseEvent& event);
  /*! A special key has been pressed 

    Printable characters are handled by OnChar instead.
   */
  void OnKeyDown(wxKeyEvent& event);
  //! Key pressed inside a cell
  void OnCharInActive(wxKeyEvent& event);
  //! Key pressed and no cell was active
  void OnCharNoActive(wxKeyEvent& event);
  /*! Key for a printable character pressed.

    Can call OnCharInActive or OnCharNoActive, if appropriate. See OnKeyDown for 
    non-printable characters like "up" or "right".
   */
  void OnChar(wxKeyEvent& event);

  //! Is called when a hCursor is active and we have a WXK_UP/WXK_DOWN event
  void SelectEditable(EditorCell *editor, bool up);

  /*! Handle selecting text using the keyboard
 Is called when the all of the following is true:
  - We have a wxKeyEvent with no active editor, 
  - shift is down and
  - keycode (ccode) is WXK_UP/WXK_DOWN
 */
  void SelectWithChar(int ccode);
  void ClickNDrag(wxPoint down, wxPoint up);
  void AdjustSize();
  void OnEraseBackground(wxEraseEvent& event) { }
  void CheckUnixCopy();
  void OnMouseMiddleUp(wxMouseEvent& event);
  void NumberSections();
  bool IsLesserGCType(int type, int comparedTo);
  void OnComplete(wxCommandEvent &event);
  wxPoint m_down;
  wxPoint m_up;
  wxPoint m_mousePoint;
  /*! Is the active cursor the one represented by a horizontal line? 

    See m_hCaretPosition and m_activeCell for the position of the two
    types of cursors. 
   */
  bool m_hCaretActive;
  /*! The group above the hcaret, NULL for the top of the document
    See m_activeCell for the position if the cursor that is drawn as a
    vertical line.
   */
  GroupCell *m_hCaretPosition;
  //! The start for the selection when selecting group with the horizontally drawn cursor
  GroupCell *m_hCaretPositionStart;
  //! The start for the selection when selecting group with the horizontally drawn cursor
  GroupCell *m_hCaretPositionEnd;
  bool m_leftDown;
  //! Do we want to automatically scroll to a cell as soon as it is being evaluated?
  bool m_followEvaluation;
  bool m_mouseDrag;
  bool m_mouseOutside;
  //! The list of tree that contains the document itself
  GroupCell *m_tree;
  GroupCell *m_last;
  /*! The group cell maxima is currently working on.

    NULL means that maxima isn't currently evaluating a cell.
   */
  GroupCell *m_workingGroup;
  MathCell *m_selectionStart;
  MathCell *m_selectionEnd;
  int m_clickType;
  GroupCell *m_clickInGC;
  /*! The cell the cursor that is drawn as a vertical line is in. 
    The position of the cursor inside that cell is defined by
    EditorCell::m_positionOfCaret.

    See also m_hCaretActive and m_hCaretPosition that handle the
    cursor that is drawn as a horizontal line. 
   */
  EditorCell *m_activeCell;
  CellParser *m_selectionParser;
  bool m_switchDisplayCaret;
  /*! Is editing enabled?

    Editing is disabled while we are waiting for maxima.
   */
  bool m_editingEnabled;
  wxTimer m_timer, m_caretTimer, m_animationTimer;
  bool m_animate;
  wxBitmap *m_memory;
  //! True if no changes have to be saved.
  bool m_saved;
  double m_zoomFactor;
  AutoComplete m_autocomplete;
  wxArrayString m_completions;
  bool m_autocompleteTemplates;

 protected:
  DECLARE_EVENT_TABLE()
};

#endif // MATHCTRL_H
