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
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  This file defines the class Worksheet

  Worksheet represents the worksheet.
 */

#ifndef WORKSHEET_H
#define WORKSHEET_H

#include <wx/wx.h>
#include <wx/aui/aui.h>
#include <wx/textfile.h>
#include <wx/fdrepdlg.h>
#include <wx/dc.h>
#include <list>

#include "VariablesPane.h"
#include "Notification.h"
#include "Cell.h"
#include "EditorCell.h"
#include "GroupCell.h"
#include "EvaluationQueue.h"
#include "FindReplaceDialog.h"
#include "Autocomplete.h"
#include "AutocompletePopup.h"
#include "TableOfContents.h"
#include "ToolBar.h"

/*! The canvas that contains the spreadsheet the whole program is about.

This canvas contains all the math-, title-, image- input- ("editor-")- etc.-
cells of the current session.

Most of the logic that handles that this canvas might be larger than the screen
(and could even be larger than the computer's RAM would it have been handled as
a big bitmap) is provided by wxWidgets:
 - If part of the screen needs to be drawn it calls the OnPaint() method.
 - If the canvas is scrolled wxWidgets the parts that are already decides how much
   of the off-screen part of the canvas is cached in a backing store
 - Also if the canvas is scrolled wxWidgets is intelligent enough to only request
   the parts of the new view that aren't available on the screen or in the backing
   store
 - If we call the Refresh() method wxWidgets requests us to draw the entire visible
   part of the worksheet anew and invalidates all cached areas it might have.
 - and the RefreshRect() method notifies wxWidgets that a rectangular region
   contains changes that need to be redrawn.

The worksheet isn't immediately redrawn on a key press, a mouse klick or on
maxima outputting new data. Instead all such events are processed in order until
wxMaxima has caught up with all the events (which causes wxWidgets to send a Idle
event to the wxMaxima object) or until we reach a timeout. If the user manages
to type faster than wxMaxima redraws the screen this allows us to skip
a few redraws in order to process the keypresses as fast as the user types.
Also this keeps us responsive even if maxima outputs data faster than
wxMaxima can display it.
*/
class Worksheet : public wxScrolled<wxWindow>
{
private:
  //! Which zoom level were we at when we started the zoom gesture?
  double m_zoomAtGestureStart;
  //! If m_cellPointers.m_scrollToCell = true: Do we want to scroll to the top of this cell?
  bool m_scrollToTopOfCell;
  //! Is our window currently active?
  bool m_windowActive;
  //! The configuration storage
  Configuration m_configurationTopInstance;
  //! The rectangle we need to refresh. -1 as "left" coordinate means: No rectangle
  wxRect m_rectToRefresh;
  /*! The size of a scroll step

    Defines the size of a
    scroll step, but besides that also the accuracy wxScrolledCanvas
    calculates some widths in.
  */
  int m_scrollUnit;
  /*! The drawing contect used for calculating sizes.

    Drawing is done from a wxPaintDC in OnPaint() instead.
  */
  wxDC *m_dc;
  //! Where do we need to start the repainting of the worksheet?
  GroupCell *m_redrawStart;
  //! Do we need to redraw the worksheet?
  bool m_redrawRequested;
  //! The clipboard format "mathML"

  //! A class that publishes wxm data to the clipboard
  static wxDataFormat m_wxmFormat;
  //! A class that publishes MathML data to the clipboard
  static wxDataFormat m_mathmlFormat;
  //! A second way to publish MathML data for the cipboard
  static wxDataFormat m_mathmlFormat2;
  //! A class that publishes RTF data to the clipboard
  static wxDataFormat m_rtfFormat;
  //! A second way to publish RTF data on the clipboard
  static wxDataFormat m_rtfFormat2;

  /*! An object that can be filled with MathML data for the clipboard

    \todo Is there a way to make this object share its data with MathMLDataObject2?
   */
  class MathMLDataObject : public wxCustomDataObject
  {
  public:
    MathMLDataObject(wxString data);

    MathMLDataObject();

  private:
    wxCharBuffer m_databuf;
  };

  //! An object that can be filled with wxm data for the clipboard
  class wxmDataObject : public wxCustomDataObject
  {
  public:
    wxmDataObject(wxString data);

    wxmDataObject();

  private:
    wxCharBuffer m_databuf;
  };

  class MathMLDataObject2 : public wxCustomDataObject
  {
  public:
    MathMLDataObject2(wxString data);

    MathMLDataObject2();

  private:
    wxCharBuffer m_databuf;
  };

  /*! An object that can be filled with MathML data for the clipboard

    \todo Is there a way to make this object share its data with RTFDataObject2?
   */
  class RtfDataObject : public wxCustomDataObject
  {
  public:
    RtfDataObject(wxString data);

    RtfDataObject();

  private:
    wxCharBuffer m_databuf;
  };

  class RtfDataObject2 : public wxCustomDataObject
  {
  public:
    RtfDataObject2(wxString data);

    RtfDataObject2();

  private:
    wxCharBuffer m_databuf;
  };

//! true, if we have the current focus.
  bool m_hasFocus;
  //! The last beginning for the area being drawn
  long m_lastTop;
  //! The last ending for the area being drawn
  long m_lastBottom;
  /*! \defgroup UndoBufferFill Undo methods for cell additions/deletions:

    Each EditorCell has its own private undo buffer Additionally wxMaxima
    maintains a undo buffer for worksheet changes. It works the following way:

     - Cells normally aren't deleted. They are moved into an undo buffer instead.
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
      m_start = NULL;
      m_oldText = wxEmptyString;
      m_newCellsEnd = NULL;
      wxDELETE(m_oldCells);
      m_oldCells = NULL;
      m_partOfAtomicAction = false;
    }

    TreeUndoAction()
    {
      m_start = NULL;
      m_oldText = wxEmptyString;
      m_newCellsEnd = NULL;
      m_oldCells = NULL;
      m_partOfAtomicAction = false;
    }

    //! True = This undo action is only part of an atomic undo action.
    bool m_partOfAtomicAction;

    /*! The position this action started at.

      NULL = At the begin of the document.
    */
    GroupCell *m_start;

    /*! The old contents of the cell start

      if this field != wxEmptyString this field contains the old contents of the text
      cell pointed to by the field start.
    */
    wxString m_oldText;

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
  std::list<TreeUndoAction *> treeUndoActions;

  //! The list of tree actions that can be redone
  std::list<TreeUndoAction *> treeRedoActions;

  //! The text the TreeUndo_ActiveCell contained when we entered it
  wxString m_treeUndo_ActiveCellOldText;

  //! Clear the list of actions for which an undo can be undone
  void TreeUndo_ClearRedoActionList();

  //! Clear the list of actions for which undo can undo
  void TreeUndo_ClearUndoActionList();

  //! Remove one action ftom the action list
  void TreeUndo_DiscardAction(std::list<TreeUndoAction *> *actionList);

  //! Add another action to this undo action
  void TreeUndo_AppendAction(std::list<TreeUndoAction *> *actionList)
    {
      if(!actionList->empty())
        actionList->front()->m_partOfAtomicAction = true;
    }

  //! Add another action to this undo action
  void TreeUndo_AppendAction(){TreeUndo_AppendAction(&treeUndoActions);}

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
  bool TreeUndo(std::list<TreeUndoAction *> *actionlist, std::list<TreeUndoAction *> *undoForThisOperation);

  /*! Undo a text change

    Called from TreeUndo().*/
  bool TreeUndoTextChange(std::list<TreeUndoAction *> *actionlist, std::list<TreeUndoAction *> *undoForThisOperation);
  /*! Undo a call deletion

    Called from TreeUndo().*/
  bool TreeUndoCellDeletion(std::list<TreeUndoAction *> *actionlist, std::list<TreeUndoAction *> *undoForThisOperation);
  /*! Undo adding cells

    Called from TreeUndo().*/
  bool TreeUndoCellAddition(std::list<TreeUndoAction *> *actionlist, std::list<TreeUndoAction *> *undoForThisOperation);

  //! Undo a tree operation.
  bool TreeUndo()
  { return TreeUndo(&treeUndoActions, &treeRedoActions); }

  //! Redo an undone tree operation.
  bool TreeRedo()
  { return TreeUndo(&treeRedoActions, &treeUndoActions); }

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
  void TreeUndo_MarkCellsAsAdded(GroupCell *start, GroupCell *end, std::list<TreeUndoAction *> *undoBuffer);


  /*! Remember that these cells were just added so this addition can be undone.

    \param parentOfStart The cell after the first cell that has been added
    \param end        The last cell that has been added
  */
  void TreeUndo_MarkCellsAsAdded(GroupCell *parentOfStart, GroupCell *end);
  //! @}

  bool m_scrolledAwayFromEvaluation;

  //! The cell the current question from maxima is being kept in.
  EditorCell *m_answerCell;

  /*! Escape all chars that aren't allowed in html.

    Also converts \n to <br/>
   */
  wxString EscapeHTMLChars(wxString input);

  //! Allow indentation by spaces for html by replacing them by non-breakable spaces
  wxString PrependNBSP(wxString input);

  //! An enum for all classes of items one can click on
  enum ClickType
  {
    CLICK_TYPE_NONE,
    CLICK_TYPE_GROUP_SELECTION,
    CLICK_TYPE_INPUT_SELECTION,
    CLICK_TYPE_OUTPUT_SELECTION
  };

  //! An enum of individual IDs for all timers this class handles
  enum TimerIDs
  {
    TIMER_ID,
    CARET_TIMER_ID
  };

  //! Add a line to a file.
  void AddLineToFile(wxTextFile &output, wxString s, bool unicode = true);

  //! Copy the currently selected cells
  Cell *CopySelection(bool asData = false);

  /*! Copy the currently given list of cells

    \param start The cell to start copying at
    \param end   The cell the copy has to end with
    \param asData
      - true:  The cells are copied in the order they are stored. m_next and m_previous
               therefore point to the right places. But m_nextToDraw and m_previousToDraw
               will be treated as aliasses of m_next and m_previous.
      - false: If a cell is broken into individual lines m_nextToDraw won't point to the
               next cell that is to be displayed. It will point to the cell containing the
               function name instead that is followed by the cell containing its contents.
               This is accurately copied if asdata=false. But m_next and m_previous are
               treated as mere aliasses of m_nextToDraw and m_previousToDraw in this case.
  */
  Cell *CopySelection(Cell *start, Cell *end, bool asData = false);

  //! Get the cordinates of the bottom right point of the worksheet.
  void GetMaxPoint(int *width, int *height);

  //! Is executed if a timer associated with Worksheet has expired.
  void OnTimer(wxTimerEvent &event);

  /*! Has the autosave interval expired?

    True means: A save will be issued after the user stops typing.
   */
  bool m_autoSaveIntervalExpired;

  #if wxCHECK_VERSION(3,1,1)
  //! Handle pinch-to-zoom-events using the gesture interface
  void OnZoom(wxZoomGestureEvent &event);
  #endif

  void OnMouseExit(wxMouseEvent &event);

  void OnMouseEnter(wxMouseEvent &event);

  /*! Is called by wxWidgets when it wants to redraw the worksheet or a part of it.

    The canonical way to schedule triggering this function is calling the Refresh()
    function of this class.
   */
  void OnPaint(wxPaintEvent &event);

  void OnSize(wxSizeEvent &event);

  void OnMouseRightDown(wxMouseEvent &event);

  void OnMouseLeftUp(wxMouseEvent &event);

  //! Is called if we loose the mouse connection whilst selecting text/cells
  void OnMouseCaptureLost(wxMouseCaptureLostEvent &event);

  void OnMouseLeftDown(wxMouseEvent &event);

  void OnMouseLeftInGcCell(wxMouseEvent &event, GroupCell *clickedInGC);

  void OnMouseLeftInGcLeft(wxMouseEvent &event, GroupCell *clickedInGC);

  void OnMouseLeftInGc(wxMouseEvent &event, GroupCell *clickedInGC);

  void OnMouseMotion(wxMouseEvent &event);

  void OnMouseWheel(wxMouseEvent &event);

  //! Is called on double click on a cell.
  void OnDoubleClick(wxMouseEvent &event);

  //! Key pressed inside a cell
  void OnCharInActive(wxKeyEvent &event);

  //! Key pressed and no cell was active
  void OnCharNoActive(wxKeyEvent &event);

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

  void OnEraseBackground(wxEraseEvent& WXUNUSED(event))
  {}

  void CheckUnixCopy();

  void OnMouseMiddleUp(wxMouseEvent &event);

  bool IsLesserGCType(int type, int comparedTo);

  //! Finds the start of the current chapter/section/...
  GroupCell *StartOfSectioningUnit(GroupCell *start);

  //! Finds the end of the current chapter/section/...
  GroupCell *EndOfSectioningUnit(GroupCell *start);

  //! Is called if a action from the autocomplete menu is selected
  void OnComplete(wxCommandEvent &event);

  //! The position the left mouse key was pressed at.
  wxPoint m_leftDownPosition;
  wxPoint m_down;
  wxPoint m_up;
  wxPoint m_mousePoint;
  /*! Is the active cursor the one represented by a horizontal line?

    See m_hCaretPosition and EditorCell::GetActiveCell() for the position of the two
    types of cursors.
   */
  bool m_hCaretActive;
  /*! The group above the hcaret, NULL for the top of the document
    See EditorCell::GetActiveCell() for the position if the cursor that is drawn as a
    vertical line.
   */
  GroupCell *m_hCaretPosition;
  /*! The start for the selection when selecting group with the horizontally drawn cursor

    This cell does define were the selection was actually started and therefore does not need
    to be above m_hCaretPositionEnd in the worksheet. See also m_cellPointers.m_selectionStart.
   */
  GroupCell *m_hCaretPositionStart;
  /*! The end of the selection when selecting group with the horizontally drawn cursor

    This cell does define where the selection was actually ended and therefore does not need
    to be below m_hCaretPositionEnd in the worksheet. See also m_cellPointers.m_selectionEnd.
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
  int m_clickType;
  GroupCell *m_clickInGC;
  //! true = blink the cursor
  bool m_blinkDisplayCaret;
  //! Is the blinking vertically-drawn cursor currently visible?
  bool m_hCaretBlinkVisible;
  //! Time step for autoscrolll when the mouse is outside the window
  wxTimer m_timer;
  //! The cursor blink rate. Also the timeout for redrawing the worksheet
  wxTimer m_caretTimer;
  //! True if no changes have to be saved.
  bool m_saved;
  AutoComplete *m_autocomplete;
  wxArrayString m_completions;
  bool m_autocompleteTemplates;
  AutocompletePopup *m_autocompletePopup;

public:
  //! Is this worksheet empty?
  bool IsEmpty()
    {
      return ( (m_tree == NULL) ||
               ((m_tree->m_next == NULL) && m_tree->GetEditable()->GetValue().Length()<=1));
    }
  //! Close the autocompletion pop-up if it is currently open.
  void CloseAutoCompletePopup()
    {
      if(m_autocompletePopup != NULL)
        m_autocompletePopup->Destroy();
    }

  /*! Key for a printable character pressed.

    Can call OnCharInActive or OnCharNoActive, if appropriate. See OnKeyDown for
    non-printable characters like "up" or "right".
   */
  void OnChar(wxKeyEvent &event);

  /*! A special key has been pressed

    Printable characters are handled by OnChar instead.
   */
  void OnKeyDown(wxKeyEvent &event);
  //! Change the style of an cell
  void SetCellStyle(GroupCell *group, GroupType style);

  //! Renumber all sections
  void NumberSections();

  //! A error notification message
  Notification *m_notificationMessage;
  //! Is this window active?
  void WindowActive(bool active){m_windowActive = active;}
  //! Clears the notification message from SetNotification
  void ClearNotification();
  /*! Inform the user that something happened in a non-active window

    This command will be ignored if the wxMaxima window is currently active
   */
  void SetNotification(wxString message, int flags = wxICON_INFORMATION);

  //! Is called if this element looses or gets the focus
  void OnActivate(wxActivateEvent &event);
  /*! The pointer to the currently active central settings storage

    Whilst printing or exporting the worksheet a settings storage
    with the print settings might be active.
   */
  Configuration *m_configuration;
  //! Get the currently active EditorCell
  EditorCell *GetActiveCell()
  {
    if (m_cellPointers.m_activeCell != NULL)
      return dynamic_cast<EditorCell *>(m_cellPointers.m_activeCell);
    else
      return NULL;
  }

  //! Tells us which cell the keyboard selection has started in
  EditorCell *KeyboardSelectionStart()
  {
    if (m_cellPointers.m_cellKeyboardSelectionStartedIn != NULL)
      return dynamic_cast<EditorCell *>(m_cellPointers.m_cellKeyboardSelectionStartedIn);
    else
      return NULL;
  }

  EditorCell *MouseSelectionStart()
  {
    if (m_cellPointers.m_cellMouseSelectionStartedIn != NULL)
      return dynamic_cast<EditorCell *>(m_cellPointers.m_cellMouseSelectionStartedIn);
    else
      return NULL;
  }

  EditorCell *SearchStart()
  {
    if (m_cellPointers.m_cellSearchStartedIn != NULL)
      return dynamic_cast<EditorCell *>(m_cellPointers.m_cellSearchStartedIn);
    else
      return NULL;
  }

  int IndexSearchStartedAt()
  {
    return m_cellPointers.m_indexSearchStartedAt;
  }

  //! The pointers to cells that can be deleted by these cells on deletion of the cells.
  Cell::CellPointers m_cellPointers;

  /*! Update the table of contents

    This function actually only schedules the update of the table-of-contents-tab.
    The actual update is done when wxMaxima is idle.
   */
  void UpdateTableOfContents()
  {
    m_scheduleUpdateToc = true;
  }

  /*! Handle redrawing the worksheet or of parts of it

    This functionality is important for scrolling, if we have changed anything
    that needs updating or if part of the screen memory containing a window was
    overwritten by a window that was in front of it and now needs to be redrawn
    for this reason.

    We don't redraw the window immediately if this seems necessary but wait
    for the idle loop instead - which is called when all selected gui events
    have been processed. This allows us to handle events as fast as the user
    can type and (if the user types faster than we can display the text)
    enables us to update the display as often as the typing speed allows, but
    not more often.
    @{
  */
  //! Request the worksheet to be redrawn
  void MarkRefreshAsDone()
  {
    m_redrawStart = NULL;
    m_redrawRequested = false;
  }

  /*! Redraw the worksheet if RequestRedraw() has been called.

    Also handles setting tooltips and redrawing the brackets on mouse movements.
   */
  bool RedrawIfRequested();

  /*! Request the worksheet to be redrawn

    \param start Which cell do we need to start the redraw in? Subsequent calls to
    this function with different cells start the redraw at the upmost of the cells
    that were passed to it.

    The actual redraw is done in the idle loop which means that as many redraw
    actions are merged as is necessary to allow wxMaxima to process things in
    real time.

    \return true, if we did redraw a workscreet portion.
   */
  void RequestRedraw(GroupCell *start = NULL);
  /*! Request a part of the worksheet to be redrawn

    \param rect The rectangle that is to be requested to be redrawn. If this
    function is called multiple times the rectangles are automatically merged.

    The actual redraw is done in the idle loop which means that as many redraw
    actions are merged as is necessary to allow wxMaxima to process things in
    real time.
   */
  void RequestRedraw(wxRect rect);

  //! Redraw the window now and mark any pending redraw request as "handled".
  void ForceRedraw()
  {
    RequestRedraw();
    RedrawIfRequested();
  }

  //! Is a Redraw requested?
  bool RedrawRequested()
    { return (m_redrawRequested || m_mouseMotionWas || (m_rectToRefresh.GetLeft() != -1)); }

  //! To be called after enabling or disabling the visibility of code cells
  void CodeCellVisibilityChanged();

  //! Re-read the configuration
  void UpdateConfig()
  { m_configuration->ReadConfig(); }

  //! The name of the currently-opened file
  wxString m_currentFile;

  /*! Make a few unicode characters interpretable by maxima.

    Does convert the not equal sign to a '#' and similar.
   */
  wxString UnicodeToMaxima(wxString s);

  //! Scroll to the start of the worksheet.
  void ScrollToStart()
  { Scroll(0, 0); }

  //! Unfold the cell that produced the error, if necessary and, if requested, scroll to it
  void ScrollToError();

  //! The find-and-replace-dialog
  FindReplaceDialog *m_findDialog;

  /*! True = schedule an update of the table of contents

    used by UpdateTableOfContents() and the idle task.
  */
  bool m_scheduleUpdateToc;

  //! Is the vertically-drawn cursor active?
  bool HCaretActive()
  { return m_hCaretActive; }

  /*! Can we merge the selected cells into one?

    \todo Does it make sense to make to allow the text of sections and image cells
    with math cells?
   */
  bool CanMergeSelection();

  bool CanUndo()
  { return CanTreeUndo() || CanUndoInsideCell(); }

  bool CanRedo()
  { return CanTreeRedo() || CanRedoInsideCell(); }

  void Undo();

  void Redo();

  /*! Clear the undo and the redo buffer

    \addtogroup UndoBufferFill
   */
  void TreeUndo_ClearBuffers();

  /*! The ids for all popup menu items.
  */
  enum PopIds
  {
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
    popid_copy_animation,
    popid_copy_svg,
    popid_copy_emf,
    popid_copy_rtf,
    popid_add_watch,
    popid_add_watch_label,
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
	popid_copy_matlab,
    popid_copy_tex,
    popid_copy_text,
    popid_copy_mathml,
    popid_image,
    popid_svg,
    popid_emf,
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
    popid_insert_heading5,
    popid_insert_heading6,
    popid_auto_answer,
    popid_popup_gnuplot,
    menu_zoom_in,
    menu_zoom_out,
    popid_fold,
    popid_unfold,
    popid_maxsizechooser
  };

  //! The constructor
  Worksheet(wxWindow *parent, int id, wxPoint pos = wxDefaultPosition, wxSize size = wxDefaultSize);

  //! The destructor
  ~Worksheet();

  //! The timer that tells us when the keyboard is inactive so an autosave isn't disrupting
  wxTimer m_keyboardInactiveTimer;

  //! Clear the whole worksheet
  void DestroyTree();

  //! Copies the worksheet's entire contents
  GroupCell *CopyTree();

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
  GroupCell *InsertGroupCells(GroupCell *cells,
                              GroupCell *where,
                              std::list<TreeUndoAction *> *undoBuffer
  );

  /*! Insert group cells into the worksheet

    \param cells The list of cells that has to be inserted
    \param where The cell the cells have to be inserted after
  */
  GroupCell *InsertGroupCells(GroupCell *cells, GroupCell *where = NULL);

  /*! Add a new line to the output cell of the working group.

    If maxima isn't currently evaluating and therefore there is no working group
    the line is appended to m_last, instead.
  */
  void InsertLine(Cell *newLine, bool forceNewLine = false);

  // Actually recalculate the worksheet.
  bool RecalculateIfNeeded();

  //! Schedule a recalculation of the worksheet starting with the cell start.
  void Recalculate(Cell *start, bool force = false);

  void Recalculate(bool force = false)
  { Recalculate(m_tree, force); }

  //! Schedule a full recalculation of the worksheet
  void RecalculateForce()
  {
    Recalculate(true);
  }

  /*! Empties the current document

    Used before opening a new file or when the "new" button is pressed.
  */
  void ClearDocument();

  void ResetInputPrompts();

  bool CanCopy(bool fromActive = false)
  {
    return (m_cellPointers.m_selectionStart != NULL) ||
           (fromActive && (m_cellPointers.m_activeCell != NULL) &&
            dynamic_cast<EditorCell *>(m_cellPointers.m_activeCell)->CanCopy());
  }

  bool CanPaste()
  {
    return (m_cellPointers.m_activeCell != NULL) || (m_hCaretActive);
  }

  bool CanCut()
  {
    return (m_cellPointers.m_activeCell != NULL &&
            dynamic_cast<EditorCell *>(m_cellPointers.m_activeCell)->CanCopy()) ||
           (m_cellPointers.m_selectionStart != NULL && m_cellPointers.m_selectionStart->GetType() == MC_TYPE_GROUP);
  }

  //! Select the whole document
  void SelectAll();

  //! Is at least one entire cell selected?
  bool CellsSelected()
  {
    return ((m_cellPointers.m_selectionStart != NULL) && (m_cellPointers.m_selectionEnd != NULL));
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
          std::list<TreeUndoAction *> *undoBuffer
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
  bool CanAnimate()
  {
    return m_cellPointers.m_selectionStart != NULL && m_cellPointers.m_selectionStart == m_cellPointers.m_selectionEnd &&
           m_cellPointers.m_selectionStart->GetType() == MC_TYPE_SLIDE;
  }

  /*! Animate the current slide show

    \param run
      - false: Stop the animation
      - true: Run the animation
  */
  void Animate(bool run = true);

  void DivideCell();

  void MergeCells();
  
  void SetLastQuestion(wxString lastQuestion){m_lastQuestion = lastQuestion;}
  wxString GetLastQuestion(){return m_lastQuestion;}

  //! Add the currently selected cells to the clipboard and delete them.
  bool CutToClipboard();

  void PasteFromClipboard();

  /*! Copy the current selection to the clipboard

    \param astext
     - true:  Copy the current selection as text
     - false: Copy the current selection as they would appear in a .wxm file
   */
  bool Copy(bool astext = false);

  //! Copy the selection to the clipboard as it would appear in a .wxm file
  bool CopyCells();

  //! Copy a Matlab representation of the current selection to the clipboard
  bool CopyMatlab();

  //! Copy a textual representation of the current selection to the clipboard
  bool CopyText();

  //! Copy the TeX representation of the current selection to the clipboard
  bool CopyTeX();

  //! Convert the current selection to MathML
  wxString ConvertSelectionToMathML();

  //! Convert the current selection to a bitmap
  wxBitmap ConvertSelectionToBitmap();

  //! Copy the MathML representation of the current selection to the clipboard
  bool CopyMathML();

  //! Copy a bitmap of the current selection to the clipboard
  bool CopyBitmap();

  //! Copy the current animation to the clipboard
  bool CopyAnimation();

    //! Copy a svg of the current selection to the clipboard
  bool CopySVG();

#if wxUSE_ENH_METAFILE
  //! Copy a svg of the current selection to the clipboard
  bool CopyEMF();
#endif

  //! Copy a rtf version of the current selection to the clipboard
  bool CopyRTF();

  wxSize CopyToFile(wxString file);

  wxSize CopyToFile(wxString file, Cell *start, Cell *end, bool asData = false, int scale = 1);

  void CalculateReorderedCellIndices(Cell *tree, int &cellIndex, std::vector<int> &cellMap);

  //! Export the file to an html document
  bool ExportToHTML(wxString file);

  /*! Export a region of the file to a .wxm or .mac file maxima's load command can read

   */
  void
  ExportToMAC(wxTextFile &output, GroupCell *tree, bool wxm, const std::vector<int> &cellMap, bool fixReorderedIndices);

  //! Export the file to a text file maxima's load command can read
  bool ExportToMAC(wxString file);

  /*! export to xml compatible file
    \param file The file name
    \param markAsSaved false means that this action doesn't clear the
                             worksheet's "modified" status.
  */
  bool ExportToWXMX(wxString file, bool markAsSaved = true);

  //! The start of a RTF document
  wxString RTFStart();

  //! The end of a RTF document
  wxString RTFEnd();

  //! export to a LaTeX file
  bool ExportToTeX(wxString file);

  /*! Convert the current selection to a string
    \param lb
     - true:  Include linebreaks
     - false: Remove linebreaks from the converted string
   */
  wxString GetString(bool lb = false);

  GroupCell *GetTree()
  { return m_tree; }

  /*! Return the first of the currently selected cells.

    NULL means: No cell is selected.
  */
  Cell *GetSelectionStart()
  { return m_cellPointers.m_selectionStart; }

  /*! Return the last of the currently selected cells.

    NULL means: No cell is selected.
  */
  Cell *GetSelectionEnd()
  { return m_cellPointers.m_selectionEnd; }

  //! Select the cell sel
  void SetSelection(Cell *sel)
  { SetSelection(sel, sel); }

  //! Select the cell range start-end
  void SetSelection(Cell *start, Cell *end);

  /*! We can edit the input if the we have the whole input in selection!
   */
  bool CanEdit();

  bool ActivatePrevInput();

  bool ActivateNextInput(bool input = false);

  /*! Scrolls to the cursor
  */
  void ScrollToCaret();

  //! Scrolls to the cell given by ScheduleScrollToCell; Is called once we have time to do so.
  void ScrollToCellIfNeeded();
  
  //! Schedules scrolling to a given cell
  void ScheduleScrollToCell(Cell *cell, bool scrollToTop = true)
    {
      m_cellPointers.ScrollToCell(cell);
      m_scrollToTopOfCell = scrollToTop;
      m_cellPointers.m_scrollToCell = true;
    }

  //! Is the point currently visible on the worksheet?
  bool PointVisibleIs(wxPoint point);

  //! Is the caret (hcaret or vcaret) currently visible on the worksheet?
  bool CaretVisibleIs();

  //! The first groupCell that is currently visible.
  GroupCell *FirstVisibleGC();

  /*! Scrolls to a point on the worksheet

    \todo I have deactivated this assert for the release as it scares the users
    in a case we don't seem to have a problem. But we perhaps should try to find
    out why it is triggered.

    Test case:
      - Create a code cell
      - Press the "hide all code cells" button.
      - click between 2 worksheet cells which makes the cursor appear as a horizontal
        line
      - press any letter.
   */
  void ShowPoint(wxPoint point);

  //! What to do if the worksheet is in the input focus
  void OnSetFocus(wxFocusEvent &event);

  //! What to do if the worksheet looses the input focus
  void OnKillFocus(wxFocusEvent &event);

  //! Does the selection start with a cell of the type "type"
  bool IsSelected(CellType type);

  /*! Set the slide of the currently selected slideshow or advance it by one step

    \param change
      - >=0: The slide the animation has to be set to
      - <0:  Advance the animation by one step.
   */
  void StepAnimation(int change = 1);

  //! Is the editor active in the last cell of the worksheet?
  bool IsActiveInLast()
  { return m_cellPointers.m_activeCell != NULL && m_cellPointers.m_activeCell->GetGroup() == m_last; }

  //! Returns the last cell of the worksheet
  GroupCell *GetLastCell()
  {
    return m_last;
  }

  //! Is the selection in the current working group?
  bool IsSelectionInWorkingGroup();

  void SetActiveCell(EditorCell *cell, bool callRefresh = true);

/*!
  Set the HCaret to its default location, at the end of the document.
 */
  void SetDefaultHCaret();

  /*! Set the HCaret at the location of the given Cell.
    
    @param where   The cell to place the cursor before.
  */
  void SetHCaret(GroupCell *where); // call with false, when manually refreshing
  //! The cell the horizontal cursor is above. NULL means at the start of the document.
  GroupCell *GetHCaret();

  //! Place the cursor into a new cell where the horizontal cursor is
  void OpenHCaret(wxString txt = wxEmptyString)
    {
      if(m_mainToolBar == NULL)
        OpenHCaret(txt, GC_TYPE_CODE);
      else
        OpenHCaret(txt, m_mainToolBar->GetCellType());
    }

  //! Place the cursor into a new cell where the horizontal cursor is
  void OpenHCaret(wxString txt, GroupType type);

  //! Activates the horizontal cursor
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
  bool FollowEvaluation()
  { return m_followEvaluation; }

  /*! Set or get the "Scrolled away from evaluation" status

    Sets FollowEvaluation() to false and enables the toolbar button to follow the
    evaluation process again.
   */
  void ScrolledAwayFromEvaluation(bool ScrolledAway);

  bool ScrolledAwayFromEvaluation()
  { return m_scrolledAwayFromEvaluation; }

  void SaveValue();

  bool IsSaved()
  { return m_saved; }

  void SetSaved(bool saved)
  { m_saved = saved; }

  void RemoveAllOutput();

  void RemoveAllOutput(GroupCell *cell);
  // methods related to evaluation queue

  /*! Trigger the evaluation of the current cell(s)

    Internally this function simulates a click on the "Cell/Evaluate Cell(s)"
    button.
   */
  void Evaluate();

  //! Adds a group cell to the evaluation queue marking its contents as "outdated".
  void AddToEvaluationQueue(GroupCell *cell);

  void AddDocumentToEvaluationQueue();

  //! Schedule all cells in the document for evaluation
  void AddEntireDocumentToEvaluationQueue();

  //! Schedule all cells stopping with the one the caret is in for evaluation
  void AddDocumentTillHereToEvaluationQueue();

  //! Add all cells below the cursor to the evaluation queue.
  void AddRestToEvaluationQueue();

  //! Adds a chapter, a section or a subsection to the evaluation queue
  void AddSectionToEvaluationQueue(GroupCell *start);

  //! Schedule all cells in the selection to be evaluated
  void AddSelectionToEvaluationQueue();

  //! Schedule all cells in a region to be evaluated
  void AddSelectionToEvaluationQueue(GroupCell *start, GroupCell *end);

  //! Schedule this cell for evaluation
  void AddCellToEvaluationQueue(GroupCell *gc);

  //! The list of cells that have to be evaluated
  EvaluationQueue m_evaluationQueue;

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
  void SetZoomFactor(double newzoom, bool recalc = true);

  void CommentSelection();

  //! Called if the user is scrolling through the document.
  void OnScrollChanged(wxScrollEvent &ev);

  /*! Do an incremental search from the cursor or the point the last search started at

    Used by the find dialog.
    \todo Keep a list of positions the last few letters were found at?
   */
  bool FindIncremental(wxString str, bool down, bool ignoreCase);

  /*! Find the next ocourrence of a string

    Used by the find dialog.
   */
  bool FindNext(wxString str, bool down, bool ignoreCase, bool warn = true);

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

  bool LoadSymbols()
  { return m_autocomplete->LoadSymbols(); }

  bool Autocomplete(AutoComplete::autoCompletionType type = AutoComplete::command);

  //! Add a symbol to the autocompletion list
  void AddSymbol(wxString fun, AutoComplete::autoCompletionType type = AutoComplete::command)
  { m_autocomplete->AddSymbol(fun, type); }

  //! Add a xml-encoded list of symbols to the autocompletion list
  void AddSymbols(wxString xml)
  { m_autocomplete->AddSymbols(xml); }

  void SetActiveCellText(wxString text);

  bool InsertText(wxString text);

  void OpenNextOrCreateCell();

  //! The table of contents pane
  TableOfContents *m_tableOfContents;

  //! Called when the "Scroll to currently evaluated" button is pressed.
  void OnFollow();

  //! The toolbar of the main window: We need to access it and therefore have it defined here.
  ToolBar *m_mainToolBar;

  //! Set this cell as the currently selected one
  void SelectGroupCell(GroupCell *cell);

  /*! Handling questions from and answers for maxima
  @{
  */
  //! Remember the answer to the LastQuestion().
  void SetAnswer(wxString answer);
  //! Mark the current question from maxima as "answered"..
  void QuestionAnswered();

  //! true = the last reply from maxima was a question
  bool m_questionPrompt;

  /*! Does maxima wait for the answer of a question?

    \retval true = maxima waits for the answer of a question.
  */
  bool QuestionPending()
  { return m_questionPrompt; }
  //!@}
  //! Converts a wxm description into individual cells
  GroupCell *CreateTreeFromWXMCode(wxArrayString *wxmLines);

  /*! Does maxima wait for the answer of a question?

*/
  void QuestionPending(bool pending)
  { m_questionPrompt = pending; }

//! Does the GroupCell cell points to contain the question currently asked by maxima?
  bool GCContainsCurrentQuestion(GroupCell *cell);

  /*! Move the cursor to the question maxima currently asks and if needed add a cell for user input
   */
  void OpenQuestionCaret(wxString txt = wxT(""));

  /*! Returns the cell maxima currently works on. NULL if there isn't such a cell.

    \param resortToLast true = if we already have set the cell maxima works on to NULL
    use the last cell maxima was known to work on.
  */
  GroupCell *GetWorkingGroup(bool resortToLast = false);

  //! The panel the user can display variable contents in
  Variablespane *m_variablesPane;

#if wxUSE_ACCESSIBILITY
  class AccessibilityInfo: public wxAccessible
  {
  public:
    AccessibilityInfo(wxWindow *parent, Worksheet *worksheet);
    wxAccStatus GetChildCount (int *childCount);
    wxAccStatus GetChild (int childId, wxAccessible **child);
    wxAccStatus GetDefaultAction(int childId, wxString *actionName);
    wxAccStatus GetParent (wxAccessible ** parent);
    wxAccStatus GetFocus (int *childId, wxAccessible **child);
    wxAccStatus GetLocation (wxRect &rect, int elementId);
    wxAccStatus HitTest 	(const wxPoint &pt,
	                         int *childId, wxAccessible **childObject);
    wxAccStatus GetDescription(int childId, wxString *description);
   private:
    wxWindow *m_parent;
    Worksheet *m_worksheet;
  };
#endif
protected:
  wxString m_lastQuestion;
  int m_virtualWidth_Last;
  int m_virtualHeight_Last;
  //! A memory we can manually buffer the contents of the area that is to be redrawn in
  wxBitmap m_memory;
  virtual wxSize DoGetBestClientSize() const;
#if wxUSE_ACCESSIBILITY
  AccessibilityInfo *m_accessibilityInfo;
#endif
  void UpdateConfigurationClientSize();
  //! Where to start recalculation. NULL = No recalculation needed.
  GroupCell *m_recalculateStart;
  //! The x position of the mouse pointer
  int m_pointer_x;
  //! The y position of the mouse pointer
  int m_pointer_y;
  //! Was there a mouse motion we didn't react to until now?
  bool m_mouseMotionWas;
DECLARE_EVENT_TABLE()
};

#endif // WORKSHEET_H
