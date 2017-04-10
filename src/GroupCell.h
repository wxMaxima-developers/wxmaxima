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

/*! \file

  This file defines the class GroupCell that bundles input and output in the worksheet.
 */

#ifndef GROUPCELL_H
#define GROUPCELL_H

#include "MathCell.h"
#include "EditorCell.h"

#define EMPTY_INPUT_LABEL wxT(" -->  ")

enum
{
  GC_TYPE_CODE,
  GC_TYPE_TITLE,
  GC_TYPE_SECTION,
  GC_TYPE_SUBSECTION,
  GC_TYPE_SUBSUBSECTION,
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
class GroupCell : public MathCell
{
public:
  GroupCell(Configuration **config, int groupType, CellPointers *cellPointers, wxString initString = wxEmptyString);

  ~GroupCell();

  /*! Tell this cell to remove it from all gui actions.

    Normally the gui keeps various pointers to a cell: The cell below the cursor,
    the cell the selection was started at, the cell that was the last cell maxima
    appended output to...

    Running this command tells the cell to remove these pointers as the cell is 
    no more displayed currently.
   */
  void MarkAsDeleted();

  /*! Which GroupCell was the last maxima was working on?

    Must be kept in GroupCell as on deletion a GroupCell will unlink itself from 
    this pointer.
   */
  GroupCell *GetLastWorkingGroup()
  {
    return dynamic_cast<GroupCell *>(m_cellPointers->m_lastWorkingGroup);
  }

  //! Mark this cell as being the last cell maxima was working on.
  void IsLastWorkingGroup()
  {
    m_cellPointers->m_lastWorkingGroup = this;
  }

  /*! Marks the cell that is under the mouse pointer.

    Is kept in GroupCell so every GroupCell can decide it is no more under the pointer
    once it has been deleted from the worksheet.
   */
  void CellUnderPointer(GroupCell *cell);

  MathCell *Copy();

  // general methods
  int GetGroupType()
  { return m_groupType; }

  void SetParent(MathCell *parent); // setting parent for all mathcells in GC
  void SetWorking(bool working)
  { m_working = working; }

  // selection methods
  void SelectInner(wxRect &rect, MathCell **first, MathCell **last);

  void SelectPoint(wxPoint &rect, MathCell **first, MathCell **last);

  void SelectOutput(MathCell **start, MathCell **end);

  void SelectRectInOutput(wxRect &rect, wxPoint &one, wxPoint &two, MathCell **first, MathCell **last);

  void SelectRectGroup(wxRect &rect, wxPoint &one, wxPoint &two, MathCell **first, MathCell **last);

  // methods for manipulating GroupCell
  bool SetEditableContent(wxString text);

  EditorCell *GetEditable(); // returns pointer to editor (if there is one)
  void AppendOutput(MathCell *cell);

  /*! Remove all output cells attached to this one

    If called on an image cell it will not remove the image attached to it (even if the image
    technically is the first output cell attached to an image cell)
    but it will remove eventual error messages attached to the image.
  */
  void RemoveOutput();

  wxString ToTeX(wxString imgDir, wxString filename, int *imgCounter);

  //! Convert the current cell to its wxm representation.
  wxString ToWXM();

  wxString ToRTF();

  wxString ToTeXCodeCell(wxString imgDir, wxString filename, int *imgCounter);

  wxString ToTeXImage(MathCell *tmp, wxString imgDir, wxString filename, int *imgCounter);

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
  void SetInput(MathCell *input);

  void SetOutput(MathCell *output);

  void AppendInput(MathCell *cell);

  wxString TexEscapeOutputCell(wxString Input);

  MathCell *GetPrompt()
  { return m_inputLabel; }

  EditorCell *GetInput()
  {
    if (m_inputLabel != NULL)
      return dynamic_cast<EditorCell *>(m_inputLabel->m_next);
    else
      return NULL;
  }

  MathCell *GetLabel()
  { return m_output; }

  MathCell *GetOutput()
  { if (m_output == NULL) return NULL; else return m_output->m_next; }

  //
  wxRect GetOutputRect()
  { return m_outputRect; }

  /*! Recalculates the height of this GroupCell and all cells inside it if needed.
    
    This command will also assign the GroupCell an y coordinate it is plotted at.
    The y coordinate of all output cells of this GroupCell is assigned during 
    GroupCell::Draw() by providing MathCell::Draw() with the cell's coordinates.
   */
  void RecalculateHeight(int fontsize);
  void RecalculateHeightInput(int fontsize);
  void RecalculateHeightOutput(int fontsize);

  /*! Recalculates the width of this GroupCell and all cells inside it if needed.
    
    This command will also assign the GroupCell an x coordinate it is plotted at.
    The x coordinate of all output cells of this GroupCell is assigned during 
    GroupCell::Draw() by providing MathCell::Draw() with the cell's coordinates.
   */
  void RecalculateWidths(int fontsize);

  void Recalculate();

  void BreakUpCells(int fontsize, int clientWidth);

  void BreakUpCells(MathCell *cell, int fontsize, int clientWidth);

  void UnBreakUpCells();

  void BreakLines(int fullWidth);

  void BreakLines(MathCell *cell, int fullWidth);

  /*! Reset the input label of the current cell.

    Won't do nothing if the cell isn't a code cell and therefore isn't equipped
    with an input label.
   */
  void ResetInputLabel();

  void ResetInputLabelList();
  //! @{ folding and unfolding

  //! Is this cell foldable?
  bool IsFoldable()
  {
    return ((m_groupType == GC_TYPE_SECTION) ||
            (m_groupType == GC_TYPE_TITLE) ||
            (m_groupType == GC_TYPE_SUBSECTION) ||
            (m_groupType == GC_TYPE_SUBSUBSECTION)
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
    \todo This function is still recursive and therefore can provoke stack overflows
    -> Convert to a while loop.
  */
  GroupCell *FoldAll();

  /*! Unfold all cells

    \return the last unfolded cell's address if unfolding was successful, else NULL
    \todo This function is still recursive and therefore can provoke stack overflows
    -> Convert to a while loop.
  */
  GroupCell *UnfoldAll();

  /*! Document structure: Can this cell type be part of the contents of comparedTo?

    For example ordinary text cells can be part of a chapter and sections can be
    part of a chapter, too.
   */
  bool IsLesserGCType(int comparedTo);

  //! @}
  bool IsMainInput(MathCell *active);

  //!  Return this cell's section- or image number.
  void Number(int &section, int &subsection, int &subsubsection, int &image);

  /*! Recalculate the cell dimensions after appending new lines.

    Won't work if text has been added to the end of the line instead.
   */
  void RecalculateAppended();

  /* Draw this GroupCell

     Also assigns all output cells contained in this GroupCell an y coordinate.
   */
  void Draw(wxPoint point, int fontsize);

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

  void LastInEvaluationQueue(bool last)
  { m_lastInEvaluationQueue = last; }

  //! Called on MathCtrl resize
  void OnSize();
  
  //! Reset the data when the input size changes
  void InputHeightChanged();
protected:
  GroupCell *m_hiddenTree; // here hidden (folded) tree of GCs is stored
  GroupCell *m_hiddenTreeParent; // store linkage to the parent of the fold
  int m_groupType;
  // The input label of this cell. Is followed by the input of the cell.
  MathCell *m_inputLabel;
  MathCell *m_output;
  bool m_hide;
  bool m_working;
  int m_fontSize;
  int m_fontSize_Old;
  int m_mathFontSize;
  MathCell *m_lastInOutput;
  MathCell *m_appendedCells;
  CellPointers *m_cellPointers;
private:
  wxRect m_outputRect;
  bool m_inEvaluationQueue;
  bool m_lastInEvaluationQueue;
  int m_inputWidth, m_inputHeight, m_outputWidth, m_outputHeight;
};

#endif /* GROUPCELL_H */
