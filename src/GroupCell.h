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

  This file defines the class GroupCell.
 */

#ifndef GROUPCELL_H
#define GROUPCELL_H

#include "MathCell.h"
#include "EditorCell.h"

#define EMPTY_INPUT_LABEL wxT("-->  ")

enum
{
  GC_TYPE_CODE,
  GC_TYPE_TITLE,
  GC_TYPE_SECTION,
  GC_TYPE_SUBSECTION,
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
class GroupCell: public MathCell
{
public:
  GroupCell(int groupType, wxString initString = wxEmptyString);
  ~GroupCell();
  MathCell* Copy();
  void Destroy();
  // general methods
  int GetGroupType() { return m_groupType; }
  void SetParent(MathCell *parent); // setting parent for all mathcells in GC
  void SetWorking(bool working) { m_working = working; }
  // selection methods
  void SelectInner(wxRect& rect, MathCell** first, MathCell** last);
  void SelectPoint(wxPoint& rect, MathCell** first, MathCell** last);
  void SelectOutput(MathCell **start, MathCell **end);
  void SelectRectInOutput(wxRect& rect, wxPoint& one, wxPoint& two, MathCell **first, MathCell **last);
  void SelectRectGroup(wxRect& rect, wxPoint& one, wxPoint& two, MathCell **first, MathCell **last);
  // methods for manipulating GroupCell
  bool SetEditableContent(wxString text);
  EditorCell* GetEditable(); // returns pointer to editor (if there is one)
  void AppendOutput(MathCell *cell);
  void RemoveOutput();
  // exporting
  wxString ToTeX(wxString imgDir, wxString filename, int *imgCounter);
  wxString ToTeX();
  wxString PrepareForTeX(wxString text);
  //! Add Markdown to the TeX representation of input cells.
  wxString TeXMarkdown(wxString str);
  wxString ToXML();
  //! Return the hide status
  bool IsHidden() { return m_hide; }
  void Hide(bool hide);
  void SwitchHide();
  wxRect HideRect();
  // raw manipulation of GC (should be protected)
  void SetInput(MathCell *input);
  void SetOutput(MathCell *output);
  void AppendInput(MathCell *cell);
  wxString TexEscapeOutputCell(wxString Input);
  MathCell* GetPrompt() { return m_input; }
  MathCell* GetInput() { return m_input->m_next; }
  MathCell* GetLabel() { return m_output; }
  MathCell* GetOutput() { if (m_output == NULL) return NULL; else return m_output->m_next; }
  //
  wxRect GetOutputRect() { return m_outputRect; }
  void RecalculateSize(CellParser& parser, int fontsize);
  void RecalculateWidths(CellParser& parser, int fontsize);
  void Recalculate(CellParser& parser, int d_fontsize, int m_fontsize);
  void BreakUpCells(CellParser parser, int fontsize, int clientWidth);
  void BreakUpCells(MathCell *cell, CellParser parser, int fontsize, int clientWidth);
  void UnBreakUpCells();
  void BreakLines(int fullWidth);
  void BreakLines(MathCell *cell, int fullWidth);
  void ResetInputLabel(bool all = false); // if !all only this GC is reset
  //! @{ folding and unfolding

  //! Is this cell foldable?
  bool IsFoldable() { return ((m_groupType == GC_TYPE_SECTION) ||
                              (m_groupType == GC_TYPE_TITLE) ||
                              (m_groupType == GC_TYPE_SUBSECTION)); }
  //! Get the tree of cells that got hidden by folding this cell
  GroupCell *GetHiddenTree() { return m_hiddenTree; }
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
  void SetHiddenTreeParent(GroupCell* parent);
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
  GroupCell *FoldAll(bool all = false);
  /*! Unfold all cells

    \return the last unfolded cell's address if unfolding was successful, else NULL
    \todo This function is still recursive and therefore can provoke stack overflows
    -> Convert to a while loop.
  */
  GroupCell *UnfoldAll(bool all = false);
  /*! Document structure: Can this cell type be part of the contents of comparedTo?

    For example ordinary text cells can be part of a chapter and sections can be
    part of a chapter, too.
   */
  bool IsLesserGCType(int comparedTo);
  //! @}
  bool IsMainInput(MathCell *active);
  void Number(int &section, int &subsection, int &image);
  /*! Recalculate the cell dimensions after appending new lines.

    Won't work if text has been added to the end of the line instead.
   */
  void RecalculateAppended(CellParser& parser);
  void Draw(CellParser& parser, wxPoint point, int fontsize);
  //! Is this list of cells empty?
  bool Empty();
  //! Does this tree contain the cell "cell"?
  bool Contains(GroupCell *cell);

protected:
  wxString ToString();
  GroupCell *m_hiddenTree; // here hidden (folded) tree of GCs is stored
  GroupCell *m_hiddenTreeParent; // store linkage to the parent of the fold
  int m_groupType;
  void DestroyOutput();
  MathCell *m_input, *m_output;
  bool m_hide;
  bool m_working;
  int m_indent;
  int m_fontSize;
  int m_mathFontSize;
  MathCell *m_lastInOutput;
  MathCell *m_appendedCells;
  wxRect m_outputRect;
};

#endif /* GROUPCELL_H */
