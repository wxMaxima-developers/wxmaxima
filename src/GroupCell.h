// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2008-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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
//  SPDX-License-Identifier: GPL-2.0+

/*! \file

  This file defines the class GroupCell that bundles input and output in the worksheet.
 */


#ifndef GROUPCELL_H
#define GROUPCELL_H

#include "Cell.h"
#include "EditorCell.h"

#define EMPTY_INPUT_LABEL wxT(" -->  ")

//! All types a GroupCell can be of
enum GroupType
{
  GC_TYPE_CODE,
  GC_TYPE_TITLE,
  GC_TYPE_SECTION,
  GC_TYPE_SUBSECTION,
  GC_TYPE_SUBSUBSECTION,
  GC_TYPE_HEADING5,
  GC_TYPE_HEADING6,
  GC_TYPE_TEXT,
  GC_TYPE_IMAGE,
  GC_TYPE_PAGEBREAK
};

/*! A cell grouping input (and, if there is one, also the output) cell to a foldable item

Items where a list of groupcells can be folded include
 - sections
 - chapters
 - The prompt (if maxima outputs one) with the input cell, the output label maxima might
   generate and the output cell (if there is any output)
 - A combination of image and title
 - A combination of image and input cell

 This GroupCell stores the currently hidden cells in the GroupCell m_hiddenTree. This tree
 has the parent m_hiddenTreeParent.
 */
class GroupCell : public Cell
{
public:
  GroupCell(Configuration **config, GroupType groupType, CellPointers *cellPointers, wxString initString = wxEmptyString);

  ~GroupCell();

  wxString GetAnswer(int answer)
    {
      return m_knownAnswers[wxString::Format(wxT("Question #%i"),answer)];
    }
  wxString GetAnswer(wxString question)
    {
      wxString answer = m_knownAnswers[question];
      if(answer.IsEmpty())
        answer = GetAnswer(++m_numberedAnswersCount);
      return answer;
    }
  //! Does this GroupCell save the answer to a question?
  bool AutoAnswer(){return m_autoAnswer;}
  //! Does this GroupCell save the answer to a question?
  void AutoAnswer(bool autoAnswer){
    m_autoAnswer = autoAnswer;
    if(GetEditable() != NULL) GetEditable()->AutoAnswer(autoAnswer);
  }
  // Add a new answer to the cell
  void SetAnswer(wxString question, wxString answer)
    {
      if(answer != wxEmptyString)
        m_knownAnswers[question] = answer;
    }
  /*! Tell this cell to remove it from all gui actions.

    Normally the gui keeps various pointers to a cell: The cell below the cursor,
    the cell the selection was started at, the cell that was the last cell maxima
    appended output to...

    Running this command tells the cell to remove these pointers as the cell is
    no more displayed currently.
   */
  void MarkAsDeleted();
  std::list<Cell *> GetInnerCells();

  /*! Which GroupCell was the last maxima was working on?

    Must be kept in GroupCell as on deletion a GroupCell will unlink itself from
    this pointer.
   */
  GroupCell *GetLastWorkingGroup()
  {
    return dynamic_cast<GroupCell *>(m_cellPointers->m_lastWorkingGroup);
  }

  /*! Marks the cell that is under the mouse pointer.

    Is kept in GroupCell so every GroupCell can decide it is no more under the pointer
    once it has been deleted from the worksheet.
   */
  void CellUnderPointer(GroupCell *cell);

  /*! Returns the tooltip for the element at the position point.

    wxEmptyString means: No toolTip.
   */
  wxString GetToolTip(const wxPoint &point);

  Cell *Copy();

  // general methods
  GroupType GetGroupType()
  { return m_groupType; }

  void SetGroupType(GroupType type)
  { m_groupType = type; }

  void SetCellStyle(int style);

  void SetGroup(Cell *parent); // setting parent for all mathcells in GC

  // selection methods
  void SelectInner(const wxRect &rect, Cell **first, Cell **last);

  void SelectPoint(const wxPoint &rect, Cell **first, Cell **last);

  void SelectOutput(Cell **start, Cell **end);

  void SelectRectInOutput(const wxRect &rect, wxPoint &one, wxPoint &two, Cell **first, Cell **last);

  void SelectRectGroup(const wxRect &rect, wxPoint &one, wxPoint &two, Cell **first, Cell **last);

  // methods for manipulating GroupCell
  bool SetEditableContent(wxString text);

  EditorCell *GetEditable(); // returns pointer to editor (if there is one)
  void AppendOutput(Cell *cell);

  /*! Remove all output cells attached to this one

    If called on an image cell it will not remove the image attached to it (even if the image
    technically is the first output cell attached to an image cell)
    but it will remove eventual error messages attached to the image.
  */
  void RemoveOutput();

  wxString ToTeX(wxString imgDir, wxString filename, int *imgCounter);

  /*! Convert the current cell to its wxm representation.

    \param wxm:
    - true: We mean to export to a .wxm file.
    - false: We generate a.mac file instead that doesn't look nice with a dedicated comment per input line.
   */
  wxString ToWXM(bool wxm = true);

  wxString ToRTF();

  wxString ToTeXCodeCell(wxString imgDir, wxString filename, int *imgCounter);

  wxString ToTeXImage(Cell *tmp, wxString imgDir, wxString filename, int *imgCounter);

  wxString ToTeX();

  //! Add Markdown to the TeX representation of input cells.
  wxString TeXMarkdown(wxString str);

  wxString ToXML();

  //! Return the hide status
  bool IsHidden()
  { return m_hide; }

  void Hide(bool hide);

  void SwitchHide();

  wxRect HideRect();

  // raw manipulation of GC (should be protected)
  void SetInput(Cell *input);

  void SetOutput(Cell *output);

  void AppendInput(Cell *cell);

  wxString TexEscapeOutputCell(wxString Input);

  Cell *GetPrompt()
  { return m_inputLabel; }

  EditorCell *GetInput()
  {
    if (m_inputLabel != NULL)
      return dynamic_cast<EditorCell *>(m_inputLabel->m_next);
    else
      return NULL;
  }

  /*! Returns the list of cells the output consists of, starting with the label.

    See also GetOutput();
  */
  Cell *GetLabel()
  { return m_output; }

  /*! Returns the list of cells the output consists of, starting after the label.

    See also GetLabel()
  */
  Cell *GetOutput()
  { if (m_output == NULL) return NULL; else return m_output->m_next; }

  //! Determine which rectangle is occupied by this GroupCell
  wxRect GetOutputRect()
  { return m_outputRect; }

  /*! Recalculates the height of this GroupCell and all cells inside it if needed.

    This command will also assign the GroupCell an y coordinate it is plotted at.
    The y coordinate of all output cells of this GroupCell is assigned during
    GroupCell::Draw() by providing Cell::Draw() with the cell's coordinates.
   */
  void RecalculateHeight(int fontsize);
  //! Recalculate the height of the input part of the cell
  void RecalculateHeightInput();
  virtual wxRect GetRect(bool all = false);
  /*! Recalculate the height of the output part of the cell

    \attention Needs to be in sync with the height calculation done during Draw() and
    during RecalculateAppended.
   */
  void RecalculateHeightOutput();

  /*! Recalculates the width of this GroupCell and all cells inside it if needed.
   */
  void RecalculateWidths(int fontsize);

  /*! Recalculate the size of this GroupCell.

    Calls RecalculateHeight() and RecalculateWidths()
  */
  void Recalculate();

  /*! Attempt to split math objects that are wider than the screen into multiple lines.
    
    \retval true, if this action has changed the height of cells.
   */
  bool BreakUpCells(Cell *cell);

  //! Undo a BreakUpCells
  void UnBreakUpCells(Cell *cell);

  //! Break this cell into lines
  void BreakLines();

  //! Break this cell into lines
  void BreakLines(Cell *cell);

  /*! Reset the input label of the current cell.

    Won't do nothing if the cell isn't a code cell and therefore isn't equipped
    with an input label.
   */
  void ResetInputLabel();

  //! Call ResetInputLabel() on all cells in the list of cells this GroupCell starts with.
  void ResetInputLabelList();
  //! @{ folding and unfolding

  //! Is this cell foldable?
  bool IsFoldable()
  {
    return ((m_groupType == GC_TYPE_SECTION) ||
            (m_groupType == GC_TYPE_TITLE) ||
            (m_groupType == GC_TYPE_SUBSECTION) ||
            (m_groupType == GC_TYPE_SUBSUBSECTION) || 
            (m_groupType == GC_TYPE_HEADING5) || 
            (m_groupType == GC_TYPE_HEADING6)
    );
  }

  //! Get the tree of cells that got hidden by folding this cell
  GroupCell *GetHiddenTree()
  { return m_hiddenTree; }

  /*! Fold the current cell

    \return
    - false, if the cell already was folded when this function was called
    - true, if the cell was folded by this function call.
  */
  bool HideTree(GroupCell *tree);

  //! Unfold the current cell
  GroupCell *UnhideTree();

  /*! Unfold all that is needed to make the current cell seen

    \return
    - false, if the cell already was visible on calling this function
    - true, if cells were unfolded by this function call
   */
  bool RevealHidden();

  //! Set the parent cell of hidden cells
  void SetHiddenTreeParent(GroupCell *parent);

  /*! Fold this cell

    \return the cell's address if folding was successful, else NULL
  */
  GroupCell *Fold(); // returns pointer to this or NULL if not successful
  /*! Unfold this cell

    \return the last cell that was unfolded.
  */  GroupCell *Unfold();

  /*! Fold all cells

    \return the cell's address if folding was successful, else NULL
  */
  GroupCell *FoldAll();

  /*! Unfold all cells

    \return the last unfolded cell's address if unfolding was successful, else NULL
  */
  GroupCell *UnfoldAll();

  /*! Document structure: Can this cell type be part of the contents of comparedTo?

    For example ordinary text cells can be part of a chapter and sections can be
    part of a chapter, too.
   */
  bool IsLesserGCType(int comparedTo);

  //! @}
  bool IsMainInput(Cell *active);

  //!  Return this cell's section- or image number.
  void Number(int &section, int &subsection, int &subsubsection, int &heading5, int &heading6, int &image);

  /*! Recalculate the cell dimensions after appending new lines.

    Won't work if text has been added to the end of the line instead.
    \attention Needs to be in sync with the height calculation done during Draw() and
    during RecalculateHeightOutput
   */
  void RecalculateAppended();

  /* Draw this GroupCell

     Also assigns all output cells contained in this GroupCell an y coordinate.

    \attention The height the output has needs to be in sync with the height
    calculation done during RecalculateAppended() and during
    RecalculateHeightOutput().
    \attention The y position used here must be in sync with the one calculated
    by RecalculateHeightOutput().

   */
  virtual void Draw(wxPoint point);

  virtual bool AddEnding()
    {
      if(GetEditable() != NULL)
        return GetEditable()->AddEnding();
      else
        return false;
    }

  //! Draw the bracket of this cell
  void DrawBracket();

  //! Is this list of cells empty?
  bool Empty();

  //! Does this tree contain the cell "cell"?
  bool Contains(GroupCell *cell);

  //! A textual representation of this cell
  wxString ToString();

  //! Is this cell part of the evaluation Queue?
  void InEvaluationQueue(bool inQueue)
  { m_inEvaluationQueue = inQueue; }

  //! Is this cell the last cell in the evaluation Queue?
  void LastInEvaluationQueue(bool last)
  { m_lastInEvaluationQueue = last; }

  //! Called on MathCtrl resize
  void OnSize();

  //! Reset the data when the input size changes
  void InputHeightChanged();

  WX_DECLARE_STRING_HASH_MAP(wxString, StringHash);
  //! A list of answers provided by the user
  StringHash m_knownAnswers;
  
#if wxUSE_ACCESSIBILITY
  wxAccStatus GetDescription(int childId, wxString *description);
  wxAccStatus GetLocation (wxRect &rect, int elementId);

  class HCaretCell: public wxAccessible
  {
  public:
    HCaretCell(GroupCell* group) : wxAccessible()
      {
        m_group = group;
      }
    //! Describe the current cell to a Screen Reader
    virtual wxAccStatus GetDescription(int childId, wxString *description)
      {
        if (description != NULL)
        {
          *description = _("A space between GroupCells");
          return wxACC_OK;
        }
        return wxACC_FAIL;
      }
    //! Inform the Screen Reader which cell is the parent of this one
    wxAccStatus GetParent (wxAccessible ** parent)
      {
        if (parent != NULL)
        {
          *parent = m_group;
          return wxACC_OK;
        }
        return wxACC_FAIL;
      }
  //! How many childs of this cell GetChild() can retrieve?
    wxAccStatus GetChildCount (int *childCount)
      {
        if (childCount != NULL)
        {
          *childCount = 0;
          return wxACC_OK;
        }
        return wxACC_FAIL;
      }
    //! Retrieve a child cell. childId=0 is the current cell
    wxAccStatus GetChild (int childId, wxAccessible **child)
      {
        if((childId != 0) || (child == NULL))
          return wxACC_FAIL;
        else
        {
          *child = this;
          return wxACC_OK;
        }
      }
    // //! Does this or a child cell currently own the focus?
    // wxAccStatus GetFocus (int *childId, wxAccessible **child)
    //   {
    //   }
    // //! Where is this cell to be found?
    // wxAccStatus GetLocation (wxRect &rect, int elementId)
    //   {
    //   }
    // //! Is pt inside this cell or a child cell?
    // wxAccStatus HitTest (const wxPoint &pt,
    //                      int *childId, wxAccessible **childObject);
    wxAccStatus GetRole (int childId, wxAccRole *role);

  private:
	GroupCell *m_group;
  };
#endif

  /*! Recalculate the cell's y position using the position and height of the last one.
    
    \return The next GroupCell or NULL if there isn't any.
  */
  GroupCell *UpdateYPosition();
  
protected:
  int m_labelWidth_cached;
  bool NeedsRecalculation();
  int GetInputIndent();
  int GetLineIndent(Cell *cell);
  GroupCell *m_hiddenTree; //!< here hidden (folded) tree of GCs is stored
  GroupCell *m_hiddenTreeParent; //!< store linkage to the parent of the fold
  //! Which type this cell is of?
  GroupType m_groupType;
  //! The input label of this cell. Is followed by the input of the cell.
  Cell *m_inputLabel;
  //! The maxima output this cell contains
  Cell *m_output;
  //! Is this cell folded (which hides its contents)?
  bool m_hide;
  int m_fontSize;
  int m_mathFontSize;
  Cell *m_lastInOutput;
  Cell *m_appendedCells;
private:
  //! Does this GroupCell automatically fill in the answer to questions?
  bool m_autoAnswer;
  wxRect m_outputRect;
  bool m_inEvaluationQueue;
  bool m_lastInEvaluationQueue;
  int m_inputWidth, m_inputHeight, m_outputWidth, m_outputHeight;
  //! The number of cells the current group contains (-1, if no GroupCell)
  int m_cellsInGroup;
  int m_numberedAnswersCount;
  void UpdateCellsInGroup(){
    if(m_output != NULL)
      m_cellsInGroup = 2 + m_output->CellsInListRecursive();
    else
      m_cellsInGroup = 2;
  }
};
#endif /* GROUPCELL_H */
