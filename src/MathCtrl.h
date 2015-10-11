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
#include <list>

#include "MathCell.h"
#include "EditorCell.h"
#include "GroupCell.h"
#include "EvaluationQueue.h"
#include "Autocomplete.h"
#include "AutocompletePopup.h"
#include "Structure.h"
#include "ToolBar.h"

/*! The canvas that contains the spreadsheet the whole program is about.

This canvas contains all the math, title, image etc.- cells of the current session.
 */
class MathCtrl: public wxScrolledCanvas
{
private:
  //! true, if we have the current focus.
  bool m_hasFocus;
  //! The last beginning for the area being drawn
  size_t m_lastTop;
  //! The last ending for the area being drawn
  size_t m_lastBottom;

  /*! \defgroup UndoBufferFill

    These methods and classes contain the undo functionality for tree changes:
     - Cells aren't deleted. They are moved into an undo buffer instead.
     - The undo buffer is also notified when Cells that are added
     - If the cursor enters a cell the contents of this cell is saved
     - And if the cell is left and the contents has changed this information
       is kept in the undo buffer, as well.
     
    The add and delete actions offer to choose which undo buffer to use since
    there are two of them:
     - The normal undo buffer and
     - The buffer that is used instead if a cell change has been issued by the
       undo command so the undo action can be reverted.\n
       This approach allows the undo functionality to use the regular add and
       delete operations keeping the codebase small.
    @{ 
  */ 

  //! The description of one action for the undo (or redo) command
  class TreeUndoAction
    {
    public:
      void Clear()
        {
          m_start=NULL;
          m_oldText=wxEmptyString;
          m_newCellsEnd=NULL;
          m_oldCells=NULL;
        }
      
      TreeUndoAction(){ Clear(); }
      

      /*! The position this action started at.
        
        NULL = At the begin of the document.
      */
      GroupCell *m_start;
      
      /*! The old contents of the cell start
        
        if this field != wxEmptyString this field contains the old contents of the text 
        cell pointed to by the field start.
      */
      wxString  m_oldText;
      
      /*! This action inserted all cells from start to newCellsEnd. 
        
        To undo it these cells have to be deleted again.

        If this field's value is NULL no cells have to be deleted to undo this action.
      */
      GroupCell *m_newCellsEnd;
      
      /*! Cells that were deleted in this action.
        
        This field will have to contain the cells themself, not a copy of them because 
        the latter might break consecutive undos.
        
        If this field's value is NULL no cells have to be added to undo this action.
      */
      GroupCell *m_oldCells;
    };

  //! The list of tree actions that can be undone
  std::list <TreeUndoAction *> treeUndoActions;
  
  //! The list of tree actions that can be redone
  std::list <TreeUndoAction *> treeRedoActions;

  //! The current undo action.
  TreeUndoAction m_currentUndoAction;

  //! Clear the list of actions for which an undo can be undone
  void TreeUndo_ClearRedoActionList();

  //! Remove one action ftom the action list
  void TreeUndo_DiscardAction(std::list <TreeUndoAction *> *actionList);

  /*! The last cell we have entered. 

    This pointer is needed for keeping track of cell contents changes.
   */  
  GroupCell *TreeUndo_ActiveCell;

  //! Drop actions from the back of the undo list until itis within the undo limit.
  void TreeUndo_LimitUndoBuffer();

  /*! Undo an item from a list of undo actions.

    \param actionlist The list to take the undo information from
    \param undoForThisOperation The list to write the information to how on to undo this undo op
  */
  bool TreeUndo(std::list <TreeUndoAction *> *actionlist,std::list <TreeUndoAction *> *undoForThisOperation);

  //! Undo a tree operation.
  bool TreeUndo(){return TreeUndo(&treeUndoActions,&treeRedoActions);}

  //! Redo an undone tree operation.
  bool TreeRedo(){return TreeUndo(&treeRedoActions,&treeUndoActions);}
  
  //! Can we undo a tree operation?
  bool CanTreeUndo();

  //! Can we redo a tree operation?
  bool CanTreeRedo();

  /*! The cursor has entered one cell => save the value to see if it has changed.
  */
  void TreeUndo_CellEntered();

  /*! The cursor is about to leave the current cell => Store the change if the value has changed.
   */
  void TreeUndo_CellLeft();
  
  /*! Remember that these cells were just added so this addition can be undone.

    \param start      The first cell that has been added
    \param end        The last cell that has been added
    \param undoBuffer Where to store the undo information. This normally is
      - treeUedoActions for the normal undo buffer or
      - treeRedoActions for the buffer that allows reverting undos
  */
  void TreeUndo_MarkCellsAsAdded(GroupCell *start, GroupCell *end,std::list <TreeUndoAction *> *undoBuffer);

  
  /*! Remember that these cells were just added so this addition can be undone.

    \param start      The first cell that has been added
    \param end        The last cell that has been added
  */
  void TreeUndo_MarkCellsAsAdded(GroupCell *parentOfStart, GroupCell *end);

  /*! True collectes requests until the end of a whole cell paste action

    A replace or action can consist of a cell delete and many consecutive adds of
    individual cells that at the end form a region.

    As long as mergeRequest is true these actions are merged to a long undo action.
    
    \parameter mergeRequest 
     - true=Start collecting data for one big undo action
     - false=Stop collecting data and write the undo action to the undo buffer.
   \parameter undoList This normally is
      - treeUedoActions for the normal undo buffer or
      - treeRedoActions for the buffer that allows reverting undos
   */
  void TreeUndo_MergeSubsequentEdits(bool mergeRequest,std::list <TreeUndoAction *> *undoList);
  /*! True collectes requests until the end of a whole cell paste action

    A replace or action can consist of a cell delete and many consecutive adds of
    individual cells that at the end form a region.

    As long as mergeRequest is true these actions are merged to a long undo action.
    
    \parameter mergeRequest 
     - true=Start collecting data for one big undo action
     - false=Stop collecting data and write the undo action to the undo buffer.
  */
  void TreeUndo_MergeSubsequentEdits(bool mergeRequest);
  bool m_TreeUndoMergeSubsequentEdits;
  //! true if m_start of m_currentUndoAction already marks the beginning of the action.
  bool m_TreeUndoMergeStartIsSet;
  //!@}
  
  bool m_scrolledAwayFromEvaluation;

  //! The cell the current question from maxima is being kept in.
  EditorCell *m_answerCell;
  /*! Escape all chars that aren't allowed in html.

    Also converts \n to <BR>
   */
  wxString EscapeHTMLChars(wxString input);

  /*! Update the table of contents

    This function actually only schedules the update of the table-of-contents-tab.
    The actual update is done when wxMaxima is idle.
   */
  void UpdateTableOfContents()
    {
      m_scheduleUpdateToc = true;
    }
  
  
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
  //! Copy the currently selected cells
  MathCell* CopySelection();
  /*! Copy the currently given list of cells

    \param start The cell to start copying at
    \param end   The cell the copy has to end with
    \param asdata 
      - true:  The cells are copied in the order they are stored
      - false: The cells are copied in the order they are displayed: Sometimes (for 
               example with fractions that can be displayed in 2d or flat) these
               two orders differ.
   */
  MathCell* CopySelection(MathCell* start, MathCell* end, bool asData = false);
  
  void GetMaxPoint(int* width, int* height);
  //! Is executed if a timer associated with MathCtrl has expired.
  void OnTimer(wxTimerEvent& event);
  /*! Has the autosave interval expired?
  
    True means: A save will be issued after the user stops typing.
   */
  bool m_autoSaveIntervalExpired;
  void OnMouseExit(wxMouseEvent& event);
  void OnMouseEnter(wxMouseEvent& event);
  /*! Is called by wxWidgets when it wants to redraw the console.

    The canonical way to trigger this function is calling the Refresh() function
    of this class.
   */
  void OnPaint(wxPaintEvent& event);
  void OnSize(wxSizeEvent& event);
  void OnMouseRightDown(wxMouseEvent& event);
  void OnMouseLeftUp(wxMouseEvent& event);
  void OnMouseLeftDown(wxMouseEvent& event);
  void OnMouseLeftInGcCell(wxMouseEvent& event, GroupCell *clickedInGC);
  void OnMouseLeftInGcLeft(wxMouseEvent& event, GroupCell *clickedInGC);
  void OnMouseLeftInGc(wxMouseEvent& event, GroupCell *clickedInGC);
  void OnMouseMotion(wxMouseEvent& event);
  void OnMouseWheel(wxMouseEvent& event);
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
  /*!
   * Select the rectangle surrounded by down and up. Called from OnMouseMotion.
   *
   * The method decides what to do, based on the value of m_clickType which
   * was set previously in OnMouseLeftDown. This enables different selection behaviours
   * depending on where we first clicked. If m_clickType equals
   * CLICK_TYPE_NONE - click-dragging does not result in a selection (we clicked in hideRect for instance)
   * CLICK_TYPE_GROUP_SELECTION - we are selecting full groupcells only. Only y-coordinate matters.
   * CLICK_TYPE_INPUT_SELECTION - we clicked in an editor (GroupCell::GetEditable()) and draging
   *   results in selecting text in EditorCell
   * CLICK_TYPE_OUTPUT_SELECTION - we clicked in an output, we want selection to be confined to that
   *   GroupCell's output. GC we first clicked in was stored in OnMouseMotion method
   *   into m_clickInGC pointer.
   */
  void ClickNDrag(wxPoint down, wxPoint up);
  // Select all group cells inside the given rectangle;
  void SelectGroupCells(wxPoint down, wxPoint up);
  void AdjustSize();
  void OnEraseBackground(wxEraseEvent& event) { }
  void CheckUnixCopy();
  void OnMouseMiddleUp(wxMouseEvent& event);
  void NumberSections();
  bool IsLesserGCType(int type, int comparedTo);
  //! Finds the start of the current chapter/section/...
  GroupCell *StartOfSectioningUnit(GroupCell *start);
  //! Finds the end of the current chapter/section/...
  GroupCell *EndOfSectioningUnit(GroupCell *start);
  //! Is called if a action from the autocomplete menu is selected
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
  /*! The start for the selection when selecting group with the horizontally drawn cursor

    This cell does actually define weree the selection was started and therefore does not need 
    to be above m_hCaretPositionEnd in the worksheet.
   */
  GroupCell *m_hCaretPositionStart;
  /*! The end of the selection when selecting group with the horizontally drawn cursor

    This cell does actually define where the selection was ended and therefore does not need 
    to be below m_hCaretPositionEnd in the worksheet.
   */
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
  //! The last group cell maxima was working on.
  GroupCell *m_lastWorkingGroup;
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
  EditorCell *m_cellMouseSelectionStartedIn;
  EditorCell *m_cellKeyboardSelectionStartedIn;
  //! true = blink the cursor
  bool m_switchDisplayCaret;
  //! Is the blinking vertically-drawn cursor currently visible?
  bool m_hCaretBlinkVisible;
  /*! Is editing enabled?

    Editing is disabled while we are waiting for maxima.
   */
  bool m_editingEnabled;
  wxTimer m_timer, m_caretTimer, m_animationTimer;
  //! True only when an animation is running
  bool m_animate;
  wxBitmap *m_memory;
  //! True if no changes have to be saved.
  bool m_saved;
  double m_zoomFactor;
  AutoComplete m_autocomplete;
  wxArrayString m_completions;
  bool m_autocompleteTemplates;


public:
  /*! True = schedule an update of the table of contents
    
    used by UpdateTableOfContents() and the idle task.
  */
  bool m_scheduleUpdateToc;
  //! Is the vertically-drawn cursor active?
  bool HCaretActive(){return m_hCaretActive;}
  /*! Can we merge the selected cells into one?
    
    \todo Does it make sense to make to allow the text of sections and image cells 
    with math cells?
   */
  bool CanMergeSelection();

  bool CanUndo(){return CanTreeUndo()||CanUndoInsideCell();}
  bool CanRedo(){return CanTreeRedo()||CanRedoInsideCell();}
  void Undo();
  void Redo();
  /*! Clear the undo and the redo buffer

    \addtogroup UndoBufferFill
   */
  void TreeUndo_ClearBuffers();

  /*! The ids for all popup menu items.
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
    popid_evaluate_section,
    popid_merge_cells,
    popid_insert_text,
    popid_insert_title,
    popid_insert_section,
    popid_insert_subsection,
    popid_insert_subsubsection,
    menu_zoom_in,
    menu_zoom_out
  };
  
  //! The constructor
  MathCtrl(wxWindow* parent, int id, wxPoint pos, wxSize size);
  //! The destructor
  ~MathCtrl();
  wxTimer m_keyboardInactiveTimer;
  //! Reads true if the keyboard was inactive for > 10 seconds
  bool m_keyboardInactive;

  //! Clear the whole worksheet
  void DestroyTree();
  //! Delete a  part of the worksheet that previously has been unlinked.
  void DestroyTree(MathCell* tree);
  MathCell* CopyTree();
  /*! Insert group cells into the worksheet

    \param cells The list of cells that has to be inserted
    \param where The cell the cells have to be inserted after. NULL means: 
           Insert the cells at the beginning of the worksheet.
    \param undoBuffer The buffer the undo information for this action has 
           to be kept in. Might be
            - treeUndoActions for normal deletes,
            - treeRedoActions for deletions while executing an undo or
            - NULL for: Don't keep any copy of the cells.
   */
  GroupCell *InsertGroupCells(GroupCell* cells,
                              GroupCell* where,
                              std::list <TreeUndoAction *> *undoBuffer
    );
  
  /*! Insert group cells into the worksheet

    \param cells The list of cells that has to be inserted
    \param where The cell the cells have to be inserted after
  */
  GroupCell *InsertGroupCells(GroupCell* cells, GroupCell* where = NULL);

  /*! Add a new line to the output cell of the working group.

    If maxima isn't currently evaluating and therefore there is no working group
    the line is appended to m_last, instead.
  */
  void InsertLine(MathCell *newLine, bool forceNewLine = false);
  void Recalculate(bool force = false);  
  void RecalculateForce() {
    Recalculate(true);
  }
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
  //! Select the whole document
  void SelectAll();
  //! Is at least one entire cell selected?
  bool CellsSelected()
    {
      return((m_selectionStart != NULL) && (m_selectionEnd != NULL));
    }
  /*! Delete a range of cells
  
    \param start The first cell to delete
    \param end The last cell to delete
    \param undoBuffer The buffer the undo information has to be kept in. Might be
      - treeUndoActions for normal deletes,
      - treeRedoActions for deletions while executing an undo or
      - NULL for: Don't keep any copy of the cells.
    \addtogroup UndoBufferFill
  */
  void DeleteRegion(
    GroupCell *start,
    GroupCell *end,
    std::list <TreeUndoAction *> *undoBuffer
    );

  /*! Move a range of cells from the document to the undo buffer
  
    \param start The first cell to delete
    \param end The last cell to delete
    \addtogroup UndoBufferFill
  */
  void DeleteRegion(
    GroupCell *start,
    GroupCell *end
    );

  /*! Delete the currently selected cells
    
    Actually moves them to the undo buffer so this action can be undone.
    \addtogroup UndoBufferFill
  */
  void DeleteSelection();

  //! Is it possible to delete the cells between start and end?
  bool CanDeleteRegion(GroupCell *start, GroupCell *end);

  //! Is it possible to delete the currently selected cells?
  bool CanDeleteSelection();

  /*! Delete the currently active cell - or the cell above this one.

    Used for the "delete current cell" shortcut.
   */
  void DeleteCurrentCell();

  //! Does it make sense to enable the "Play" button and the slider now? 
  bool CanAnimate() {
    return m_selectionStart != NULL && m_selectionStart == m_selectionEnd &&
      m_selectionStart->GetType() == MC_TYPE_SLIDE;
  }
  void Animate(bool run);
  void DivideCell();
  void MergeCells();
  //! Add the currently selected cells to the clipboard and delete them.
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
  wxSize CopyToFile(wxString file);
  wxSize CopyToFile(wxString file, MathCell* start, MathCell* end, bool asData = false,int scale=1);
  void CalculateReorderedCellIndices(MathCell *tree, int &cellIndex, std::vector<int>& cellMap);
  //! Export the file to an html document
  bool ExportToHTML(wxString file);
  //! Export a region of the file to a .wxm or .mac file maxima's load command can read
  void ExportToMAC(wxTextFile& output, MathCell *tree, bool wxm, const std::vector<int>& cellMap, bool fixReorderedIndices);
  //! Export the file to a text file maxima's load command can read
  bool ExportToMAC(wxString file);
  /*! export to xml compatible file
    \param file The file name
    \param markAsSaved false means that this action doesn't clear the 
                             worksheet's "modified" status.
  */
  bool ExportToWXMX(wxString file, bool markAsSaved = true);	
  //! export to a LaTeX file
  bool ExportToTeX(wxString file);
  /*! Convert the current selection to a string 
    \param lb
     - true:  Include linebreaks
     - false: Remove linebreaks from the converted string
   */
  wxString GetString(bool lb = false);
  GroupCell* GetTree() { return m_tree; }
  /*! Return the first of the currently selected cells.

    NULL means: No cell is selected.
  */
  MathCell* GetSelectionStart() { return m_selectionStart; }
  /*! Return the last of the currently selected cells.

    NULL means: No cell is selected.
  */
  MathCell* GetSelectionEnd() { return m_selectionEnd; }
  //! Select the cell sel
  void SetSelection(MathCell* sel) { SetSelection(sel,sel); }
  //! Select the cell range start-end
  void SetSelection(MathCell* start,MathCell* end) { m_selectionStart = start;m_selectionEnd = end; }
  bool CanEdit();
  void EnableEdit(bool enable = true) { m_editingEnabled = enable; }
  bool ActivatePrevInput();
  bool ActivateNextInput(bool input = false);
  //! Scrolls to the cursor
  void ScrollToCaret();
  //! Scrolls to a given cell
  void ScrollToCell(MathCell *cell);
  //! Returns the cell the cursor that is drawn as a vertical line is in.
  EditorCell* GetActiveCell() { return m_activeCell; }
  //! Is the point currently visible on the worksheet?
  bool PointVisibleIs(wxPoint point);
  //! Is the caret (hcaret or vcaret) currently visible on the worksheet?
  bool CaretVisibleIs();

  //! Scrolls to a point on the worksheet
  void ShowPoint(wxPoint point);
  void OnSetFocus(wxFocusEvent& event);
  void OnKillFocus(wxFocusEvent& event);
  bool IsSelected(int type);
  /*! Set the slide of the currently selected slideshow or advance it by one step

    \param pos
      - >=0: The slide the animation has to be set to
      - <0:  Advance the animation by one step.
   */
  void StepAnimation(int change = 1);
  //! Query if an animation is currently running
  bool AnimationRunning() { return m_animate; }
  //! Tell if an animation should run running
  void AnimationRunning(bool state) { m_animate = state; }
  bool IsActiveInLast() { return m_activeCell != NULL && m_activeCell->GetParent() == m_last; }
  void SetWorkingGroup(GroupCell *group);
  bool IsSelectionInWorking();
  void SetActiveCell(EditorCell *cell, bool callRefresh = true);
  void SetDefaultHCaret();
  void SetHCaret(GroupCell *where, bool callRefresh = true); // call with false, when manually refreshing
  //! The cell the horizontal cursor is above. NULL means at the start of the document.
  GroupCell *GetHCaret();
    //! Place the cursor into a new cell where the horizontal cursor is
  void OpenHCaret(wxString txt = wxEmptyString, int type = GC_TYPE_CODE);
  void ShowHCaret();
  /*! Is it possible to issue an undo in the currently selected cell? 

    \return false if no cell is selected or there is no further undo information
   */
  bool CanUndoInsideCell();

  void UndoInsideCell();

  /*! Is it possible to issue an undo in the currently selected cell?

    \return false if no cell is selected or no redo can be executed.
   */
  bool CanRedoInsideCell();

  void RedoInsideCell();

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
  //! Adds a chapter, a section or a subsection to the evaluation queue
  void AddSectionToEvaluationQueue(GroupCell *start);
  //! Schedule all cells in the selection to be evaluated
  void AddSelectionToEvaluationQueue();
  //! Schedule all cells in a region to be evaluated  
  void AddSelectionToEvaluationQueue(GroupCell *start,GroupCell *end);
  //! Schedule this cell for evaluation
  void AddCellToEvaluationQueue(GroupCell* gc);
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
  void SetZoomFactor(double newzoom, bool recalc = true);
  void CommentSelection();
  //! Called if the user is scrolling through the document.
  void OnScrollChanged(wxScrollEvent &ev);
  /*! Find the next ocourrence of a string

    Used by the find dialog.
   */
  bool FindNext(wxString str, bool down, bool ignoreCase);
  /*! Replace the current ocourrence of a string

    Used by the find dialog.
   */
  void Replace(wxString oldString, wxString newString, bool ignoreCase);
  /*! Replace all ocourrences of a string

    Used by the find dialog.
   */
  int ReplaceAll(wxString oldString, wxString newString, bool ignoreCase);
  wxString GetInputAboveCaret();
  wxString GetOutputAboveCaret();
  bool LoadSymbols(wxString file) { return m_autocomplete.LoadSymbols(file); }
  bool Autocomplete(AutoComplete::autoCompletionType type = AutoComplete::command);
  void AddSymbol(wxString fun, AutoComplete::autoCompletionType type = AutoComplete::command) { m_autocomplete.AddSymbol(fun, type); }
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
  /*! Does maxima wait for the answer of a question?

    \retval true = maxima waits for the answer of a question.
  */
  bool QuestionPending(){return m_questionPrompt;}
  //! Converts a wxm description into individual cells
  GroupCell* CreateTreeFromWXMCode(wxArrayString *wxmLines);

    /*! Does maxima wait for the answer of a question?

  */
  void QuestionPending(bool pending){m_questionPrompt = pending;}
//! Does the GroupCell cell points to contain the question currently asked by maxima?  
  bool GCContainsCurrentQuestion(GroupCell *cell);
  /*! Move the cursor to the question maxima currently asks and if needed add a cell for user input

    \todo Currently scrolls to the GroupCell the question is in, not to the actual question.
   */
  void OpenQuestionCaret(wxString txt=wxT(""));

 protected:
  DECLARE_EVENT_TABLE()
};

#endif // MATHCTRL_H
