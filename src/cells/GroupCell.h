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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file

  This file defines the class GroupCell that bundles input and output in the worksheet.
 */


#ifndef GROUPCELL_H
#define GROUPCELL_H

#include "Cell.h"
#include "EditorCell.h"

//! All types a GroupCell can be of
// This enum's elements must be synchronized with (WXMFormat.h) WXMHeaderId.
enum GroupType : int8_t
{
  GC_TYPE_INVALID = -1,
  GC_TYPE_CODE = 0,
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
class GroupCell final : public Cell
{
public:
  GroupCell(Configuration **config, GroupType groupType, const wxString &initString = {});
  GroupCell(const GroupCell &cell);
  std::unique_ptr<Cell> Copy() const override;
  const CellTypeInfo &GetInfo() override;
  std::unique_ptr<GroupCell> CopyList() const;
  ~GroupCell();

  GroupCell *first() const { return dynamic_cast<GroupCell*>(Cell::first()); }
  GroupCell *last() const { return dynamic_cast<GroupCell*>(Cell::last()); }

  wxString GetAnswer(int answer)
    {
      if((!m_autoAnswer) && (!(*m_configuration)->OfferKnownAnswers()))
        return wxEmptyString;
      
      return m_knownAnswers[wxString::Format(wxT("Question #%i"),answer)];
    }
  //! Does this GroupCell know the answer to any of maxima's questions?
  bool ContainsSavedAnswers(){return m_knownAnswers.size() > 0;}
  wxString GetAnswer(wxString question)
    {
      if((!m_autoAnswer) && (!(*m_configuration)->OfferKnownAnswers()))
        return wxEmptyString;
      
      wxString answer = m_knownAnswers[question];
      if(answer.IsEmpty())
        answer = GetAnswer(++m_numberedAnswersCount);
      return answer;
    }
  //! Does this GroupCell save the answer to a question?
  bool AutoAnswer() const { return m_autoAnswer; }
  //! Does this GroupCell save the answer to a question?
  void AutoAnswer(bool autoAnswer)
  {
    m_autoAnswer = autoAnswer;
    if(GetEditable() != NULL) GetEditable()->AutoAnswer(autoAnswer);
  }
  // Add a new answer to the cell
  void SetAnswer(wxString question, wxString answer)
  {
    if (answer != wxEmptyString)
      m_knownAnswers[question] = answer;
  }

  InnerCellIterator InnerBegin() const override
  {
    if (m_groupType == GC_TYPE_PAGEBREAK) return {};
    return {&m_inputLabel, &m_output};
  }

  /*! Which GroupCell was the last maxima was working on?

    Must be kept in GroupCell as on deletion a GroupCell will unlink itself from
    this pointer.
   */
  GroupCell *GetLastWorkingGroup() const;

  /*! Marks the cell that is under the mouse pointer.

    Is kept in GroupCell so every GroupCell can decide it is no more under the pointer
    once it has been deleted from the worksheet.
   */
  void CellUnderPointer(GroupCell *cell);

  /*! Returns the tooltip for the element at the position point.

    wxEmptyString means: No toolTip.
   */
  const wxString &GetToolTip(wxPoint point) const override;

  // general methods
  GroupType GetGroupType() const { return m_groupType; }

  void SetGroupType(GroupType type) { m_groupType = type; }

  void SetGroup(GroupCell *parent) override; // setting parent for all mathcells in GC

  // selection methods
  void SelectInner(const wxRect &rect, CellPtr<Cell> *first, CellPtr<Cell> *last) override;

  void SelectPoint(wxPoint point, CellPtr<Cell> *first, CellPtr<Cell> *last);

  // cppcheck-suppress functionConst
  void SelectOutput(CellPtr<Cell> *start, CellPtr<Cell> *end);

  // cppcheck-suppress functionConst
  void SelectRectInOutput(const wxRect &rect, wxPoint one, wxPoint two, CellPtr<Cell> *first, CellPtr<Cell> *last);

  void SelectRectGroup(const wxRect &rect, wxPoint one, wxPoint two, CellPtr<Cell> *first, CellPtr<Cell> *last);

  // methods for manipulating GroupCell
  // cppcheck-suppress functionConst
  bool SetEditableContent(wxString text);

  EditorCell *GetEditable() const; // returns pointer to editor (if there is one)

  void AppendOutput(std::unique_ptr<Cell> &&cell);

  /*! Remove all output cells attached to this one

    If called on an image cell it will not remove the image attached to it (even if the image
    technically is the first output cell attached to an image cell)
    but it will remove eventual error messages attached to the image.
  */
  void RemoveOutput();

  //! GroupCells warn if they contain both greek and latin lookalike chars.
  void UpdateConfusableCharWarnings();
  
  wxString ToTeX(wxString imgDir, wxString filename, int *imgCounter) const;

  wxString ToRTF() const override;

  wxString ToTeXCodeCell(wxString imgDir, wxString filename, int *imgCounter) const;

  static wxString ToTeXImage(Cell *tmp, wxString imgDir, wxString filename, int *imgCounter);

  wxString ToTeX() const override;

  //! Add Markdown to the TeX representation of input cells.
  wxString TeXMarkdown(wxString str);

  wxString ToXML() const override;

  void Hide(bool hide) override;

  void SwitchHide();

  wxRect HideRect();

  // raw manipulation of GC (should be protected)
  void SetInput(std::unique_ptr<Cell> &&input);

  void SetOutput(std::unique_ptr<Cell> &&output);

  void AppendInput(std::unique_ptr<Cell> &&cell);

  //! Get the previous GroupCell in the list
  GroupCell *GetPrevious() const { return m_previous.CastAs<GroupCell*>(); }
  //! Get the next GroupCell in the list.
  GroupCell *GetNext() const { return dynamic_cast<GroupCell *>(Cell::GetNext()); }

  static wxString TexEscapeOutputCell(wxString Input);

  Cell *GetPrompt() const { return m_inputLabel.get(); }

  EditorCell *GetInput() const
  { return m_inputLabel ? dynamic_cast<EditorCell *>(m_inputLabel->GetNext()) : nullptr; }

  /*! Returns the list of cells the output consists of, starting with the label.

    See also GetOutput();
  */
  Cell *GetLabel() const { return m_output.get(); }

  /*! Returns the list of cells the output consists of, starting after the label.

    See also GetLabel()
  */
  Cell *GetOutput() const
  { return m_output ? m_output->GetNext() : nullptr; }

  //! Determine which rectangle is occupied by this GroupCell
  wxRect GetOutputRect() const { return m_outputRect; }

  /*! Recalculates the size of this GroupCell and all cells inside it if needed.

    This command will also assign the GroupCell a y coordinate it is plotted at.
    The y coordinate of all output cells of this GroupCell is assigned during
    GroupCell::Draw() by providing Cell::Draw() with the cell's coordinates.
   */
  void Recalculate(AFontSize WXUNUSED(fontsize)) override {Recalculate();}
  void Recalculate();

  //! Recalculate the height of the input part of the cell
  void RecalculateHeightInput();
  wxRect GetRect(bool all = false) const override;
  /*! Recalculate the height of the output part of the cell

    \attention Needs to be in sync with the height calculation done during Draw() and
    during RecalculateAppended.
   */
  void RecalculateHeightOutput();

  /*! Attempt to split math objects that are wider than the screen into multiple lines.
    
    \retval true, if this action has changed the height of cells.
   */
  bool BreakUpCells(Cell *cell);

  //! Undo a BreakUpCells
  bool UnBreakUpCells(Cell *cell);

  //! Break this cell into lines
  void BreakLines();

  /*! Reset the input label of the current cell.

    Won't do nothing if the cell isn't a code cell and therefore isn't equipped
    with an input label.
   */
  void ResetInputLabel();

  //! Call ResetInputLabel() on all cells in the list of cells this GroupCell starts with.
  void ResetInputLabelList();
  //! @{ folding and unfolding

  //! Is this cell foldable?
  bool IsFoldable() const
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
  GroupCell *GetHiddenTree() const { return m_hiddenTree; }

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
  */
  GroupCell *Unfold();

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
  bool IsLesserGCType(int comparedTo) const;

  //! @}
  bool IsMainInput(Cell *active) const;

  //!  Return this cell's section- or image number.
  void Number(int &section, int &subsection, int &subsubsection, int &heading5, int &heading6, int &image) const;

  /*! Draw this GroupCell

     Also assigns all output cells contained in this GroupCell a y coordinate.

    \attention The height the output has needs to be in sync with the height
    calculation done during RecalculateAppended() and during
    RecalculateHeightOutput().
    \attention The y position used here must be in sync with the one calculated
    by RecalculateHeightOutput().

   */
  void Draw(wxPoint point) override;

  bool AddEnding() override
    {
      if(GetEditable() != NULL)
        return GetEditable()->AddEnding();
      else
        return false;
    }

  //! Draw the bracket of this cell
  void DrawBracket();

  //! Is this list of cells empty?
  bool Empty() const;

  //! Does this tree contain the cell "cell"?
  bool Contains(GroupCell *cell) const;

  //! A textual representation of this cell
  wxString ToString() const override;

  //! Is this cell part of the evaluation Queue?
  void InEvaluationQueue(bool inQueue) { m_inEvaluationQueue = inQueue; }

  //! Is this cell the last cell in the evaluation Queue?
  void LastInEvaluationQueue(bool last) { m_lastInEvaluationQueue = last; }

  //! Called on MathCtrl resize
  void OnSize();

  //! Reset the data when the input size changes
  void InputHeightChanged();

  WX_DECLARE_STRING_HASH_MAP(wxString, StringHash);
  //! A list of answers provided by the user
  StringHash m_knownAnswers;

  void SetNextToDraw(Cell *next) override;

  Cell *GetNextToDraw() const override { return m_nextToDraw; }

#if wxUSE_ACCESSIBILITY
  wxAccStatus GetDescription(int childId, wxString *description) const override;
  wxAccStatus GetLocation (wxRect &rect, int elementId) override;
#endif

  /*! Recalculate the cell's y position using the position and height of the last one.
    
    \return The next GroupCell or NULL if there isn't any.
  */
  GroupCell *UpdateYPosition();

  void UpdateYPositionList();

  bool GetSuppressTooltipMarker() const { return m_suppressTooltipMarker; }
  void SetSuppressTooltipMarker(bool suppress)
    {m_suppressTooltipMarker = suppress;}
protected:
  bool NeedsRecalculation(AFontSize fontSize) const override;
  int GetInputIndent();
  int GetLineIndent(Cell *cell);
  void UpdateCellsInGroup();

//** 16-byte objects (16 bytes)
//**
  wxRect m_outputRect{-1, -1, 0, 0};

//** 8/4 byte objects (40 bytes)
//**
  CellPtr<Cell> m_nextToDraw;

  GroupCell *m_hiddenTree = {}; //!< here hidden (folded) tree of GCs is stored
  GroupCell *m_hiddenTreeParent = {}; //!< store linkage to the parent of the fold

  // The pointers below point to inner cells and must be kept contiguous.
  // ** All pointers must be the same: either Cell * or std::unique_ptr<Cell>.
  // ** NO OTHER TYPES are allowed.
  //! The input label of this cell. Is followed by the input of the cell.
  std::unique_ptr<Cell> m_inputLabel;
  //! The maxima output this cell contains
  std::unique_ptr<Cell> m_output;
  // The pointers above point to inner cells and must be kept contiguous.

//** 4-byte objects (12 bytes)
//**
  int m_labelWidth_cached = 0;
  int m_inputWidth, m_inputHeight;

//** 2-byte objects (6 bytes)
//**
  //! The number of cells the current group contains (-1, if no GroupCell)
  int16_t m_cellsInGroup = 1;
  int16_t m_numberedAnswersCount = 0;

  AFontSize m_mathFontSize;

//** 1-byte objects (1 byte)
//**
  //! Which type this cell is of?
  GroupType m_groupType = {};

//** Bitfield objects (1 bytes)
//**
  void InitBitFields()
  { // Keep the initailization order below same as the order
    // of bit fields in this class!
    m_autoAnswer = false;
    m_inEvaluationQueue = false;
    m_lastInEvaluationQueue = false;
    m_updateConfusableCharWarnings = true;
  }

  //! Does this GroupCell automatically fill in the answer to questions?
  bool m_autoAnswer : 1 /* InitBitFields */;
  bool m_inEvaluationQueue : 1 /* InitBitFields */;
  bool m_lastInEvaluationQueue : 1 /* InitBitFields */;
  bool m_updateConfusableCharWarnings : 1 /* InitBitFields */;

  static wxString m_lookalikeChars;
};

#endif /* GROUPCELL_H */
