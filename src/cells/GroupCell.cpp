// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2008-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2008-2009 Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
//            (C) 2012 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class GroupCell

  GroupCell is the Cell type that bundles user input with eventual images or
  output from maxima that belongs to it.
*/

#include <string>
#include <memory>
#include <utility>
#include "GroupCell.h"
#include "AnimationCell.h"
#include "CellImpl.h"
#include "CellList.h"
#include "CellPointers.h"
#include "ImgCell.h"
#include "LabelCell.h"
#include "MarkDown.h"
#include "TextCell.h"
#include "stx/unique_cast.hpp"
#include <wx/clipbrd.h>
#include <wx/log.h>
#include <wx/config.h>

#ifdef __WINDOWS__
constexpr bool TEMPORARY_WINDOWS_PERFORMANCE_HACK = true;
#else
constexpr bool TEMPORARY_WINDOWS_PERFORMANCE_HACK = false;
#endif

#if wxUSE_ACCESSIBILITY
// TODO This class is not used anywhere.
class HCaretCell : public wxAccessible {
public:
  explicit HCaretCell(GroupCell *group) : wxAccessible(), m_group(group) {}

  //! Describe the current cell to a Screen Reader
  wxAccStatus GetDescription(int WXUNUSED(childId),
                             wxString *description) override {
    if (description)
      return (*description = _("A space between GroupCells")), wxACC_OK;

    return wxACC_FAIL;
  }
  //! Inform the Screen Reader which cell is the parent of this one
  wxAccStatus GetParent(wxAccessible **parent) override {
    if (parent)
      return (*parent = m_group->GetAccessible()), wxACC_OK;

    return wxACC_FAIL;
  }
  //! How many childs of this cell GetChild() can retrieve?
  wxAccStatus GetChildCount(int *childCount) override {
    if (childCount)
      return (*childCount = 0), wxACC_OK;

    return wxACC_FAIL;
  }
  //! Retrieve a child cell. childId=0 is the current cell
  wxAccStatus GetChild(int childId, wxAccessible **child) override {
    if (childId == 0 && child)
      return (*child = this), wxACC_OK;

    return wxACC_FAIL;
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
  wxAccStatus GetRole(int childId, wxAccRole *role) override;

private:
  GroupCell *m_group;
};
#endif

#define EMPTY_INPUT_LABEL wxT(" -->  ")

GroupCell::GroupCell(Configuration *config, GroupType groupType,
                     const wxString &initString)
  : Cell(this, config), m_groupType(groupType) {
  InitBitFields();
  m_mathFontSize = m_configuration->GetMathFontSize();
  ForceBreakLine();
  m_type = MC_TYPE_GROUP;

  // set up cell depending on groupType, so we have a working cell
  if (groupType != GC_TYPE_PAGEBREAK) {
    if (groupType == GC_TYPE_CODE)
      m_inputLabel =
	std::make_unique<LabelCell>(this, m_configuration, EMPTY_INPUT_LABEL);
    else
      m_inputLabel = std::make_unique<TextCell>(this, m_configuration, wxT(""));

    m_inputLabel->SetType(MC_TYPE_MAIN_PROMPT);
  }

  std::unique_ptr<EditorCell> editor =
    std::make_unique<EditorCell>(this, m_configuration);

  if (editor && !initString.empty())
    editor->SetValue(initString);
  if (editor)
    AppendInput(std::move(editor));

  SetGroupType(groupType);

  // when creating an image cell, if a string is provided
  // it loads an image (without deleting it)
  if ((groupType == GC_TYPE_IMAGE) && (initString.Length() > 0)) {
    std::unique_ptr<Cell> ic;
    if (wxImage::GetImageCount(initString) < 2) {
      ic = std::make_unique<ImgCell>(
				     this, m_configuration, initString,
				     std::shared_ptr<wxFileSystem>{} /* system fs */, false);

      // Since this is the (only?) place where an ImgCell is constructed when
      // the user manually inserts an image file (not loaded from zip or gnuplot
      // tempfile), set the filename such that reloading the file later is
      // possible.
      static_cast<ImgCell &>(*ic).SetOrigImageFile(initString);
    } else
      ic = std::make_unique<AnimationCell>(this, m_configuration, initString,
                                           false);
    AppendOutput(std::move(ic));
  }
}

GroupCell::GroupCell(GroupCell *WXUNUSED(group), GroupCell const &cell)
  : GroupCell::GroupCell(cell) {}

GroupCell::GroupCell(GroupCell const &cell)
  : GroupCell(cell.m_configuration, cell.m_groupType) {
  CopyCommonData(cell);
  if (cell.m_inputLabel)
    SetInput(cell.m_inputLabel->CopyList(this));
  if (cell.m_output)
    SetOutput(cell.m_output->CopyList(this));
  SetAutoAnswer(cell.m_autoAnswer);
}

DEFINE_CELL(GroupCell)

void GroupCell::SetGroupType(GroupType groupType) {
  CellType type = MC_TYPE_DEFAULT;
  switch (groupType) {
  case GC_TYPE_CODE:
    type = MC_TYPE_INPUT;
    break;
  case GC_TYPE_TEXT:
    type = MC_TYPE_TEXT;
    break;
  case GC_TYPE_TITLE:
    type = MC_TYPE_TITLE;
    break;
  case GC_TYPE_SECTION:
    type = MC_TYPE_SECTION;
    break;
  case GC_TYPE_SUBSECTION:
    type = MC_TYPE_SUBSECTION;
    break;
  case GC_TYPE_SUBSUBSECTION:
    type = MC_TYPE_SUBSUBSECTION;
    break;
  case GC_TYPE_HEADING5:
    type = MC_TYPE_HEADING5;
    break;
  case GC_TYPE_HEADING6:
    type = MC_TYPE_HEADING6;
    break;
  case GC_TYPE_IMAGE:
    type = MC_TYPE_IMAGE;
    break;
  default:
    break;
  }
  if (GetEditable())
    GetEditable()->SetType(type);
  if (GetPrompt()) {
    if (groupType == GC_TYPE_CODE)
      GetPrompt()->SetType(MC_TYPE_MAIN_PROMPT);
    else
      GetPrompt()->SetType(type);
  }
  m_groupType = groupType;
  ResetSize();
}

std::unique_ptr<GroupCell> GroupCell::CopyList() const {
  GroupCell parent(m_configuration, GC_TYPE_CODE);
  auto copy = Cell::CopyList(&parent);
  return stx::static_unique_ptr_cast<GroupCell>(std::move(copy));
}

bool GroupCell::Empty() const {
  return (
	  // No next cell
	  (!GetNext()) &&
	  // This cell at maximum contains a prompt.
	  (ToString().Length() < 6));
}

void GroupCell::ResetInputLabel() {
  if (m_groupType == GC_TYPE_CODE) {
    if (m_inputLabel)
      m_inputLabel->SetValue(EMPTY_INPUT_LABEL);
  }
}

void GroupCell::ResetInputLabelList() {
  for (auto &tmp : OnList(this)) {
    tmp.ResetInputLabel();
    // also reset input labels in the folded cells
    if (tmp.IsFoldable() && (tmp.m_hiddenTree))
      tmp.m_hiddenTree->ResetInputLabelList();
  }
}

bool GroupCell::IsFoldable() const {
  return ((m_groupType == GC_TYPE_SECTION) || (m_groupType == GC_TYPE_TITLE) ||
          (m_groupType == GC_TYPE_SUBSECTION) ||
          (m_groupType == GC_TYPE_SUBSUBSECTION) ||
          (m_groupType == GC_TYPE_HEADING5) ||
          (m_groupType == GC_TYPE_HEADING6));
}

GroupCell::~GroupCell() {}

const wxString &GroupCell::GetAnswer(int answer) const {
  if ((!m_autoAnswer) && (!m_configuration->OfferKnownAnswers()))
    return wxm::emptyString;

  wxString const question = wxString::Format(wxT("Question #%i"), answer);
  auto it = m_knownAnswers.find(question);
  return (it != m_knownAnswers.end()) ? it->second : wxm::emptyString;
}

const wxString &GroupCell::GetAnswer(const wxString &question) const {
  if ((!m_autoAnswer) && (!m_configuration->OfferKnownAnswers()))
    return wxm::emptyString;

  auto it = m_knownAnswers.find(question);
  return (it != m_knownAnswers.end()) ? it->second
    : GetAnswer(++m_numberedAnswersCount);
}

void GroupCell::SetAutoAnswer(bool autoAnswer) {
  m_autoAnswer = autoAnswer;
  if (GetEditable())
    GetEditable()->AutoAnswer(autoAnswer);
}

void GroupCell::SetAnswer(const wxString &question, const wxString &answer) {
  if (!answer.empty())
    m_knownAnswers[question] = answer;
}

GroupCell *GroupCell::GetLastWorkingGroup() const {
  return m_cellPointers->m_lastWorkingGroup;
}

wxString GroupCell::TexEscapeOutputCell(wxString Input) {
  wxString retval(Input);
  Input.Replace(wxT("#"), wxT("\\#"));
  return (Input);
}

void GroupCell::SetInput(std::unique_ptr<Cell> &&input) {
  if (!input)
    return;
  m_inputLabel = std::move(input);
  m_updateConfusableCharWarnings = true;
  InputHeightChanged();
}

void GroupCell::AppendInput(std::unique_ptr<Cell> &&cell) {
  if (!m_inputLabel) {
    m_inputLabel = std::move(cell);
  } else {
    if (m_inputLabel->GetNext() == NULL)
      CellList::AppendCell(m_inputLabel, std::move(cell));
    else if (m_inputLabel->GetNext()->GetValue().Length() == 0) {
      // AppendCell is needed due to its side effect of doing
      // m_group->ResetData. Perhaps we can decide that SetNext alone could do
      // something like that, as long as it wouldn't cause quadratic behavior.
      CellList::SetNext(m_inputLabel.get(), nullptr);
      wxASSERT(!m_inputLabel->GetNextToDraw());
      CellList::AppendCell(m_inputLabel, std::move(cell));
    } else {
      AppendOutput(std::move(cell));
      Cell::Hide(false);
    }
  }
  m_updateConfusableCharWarnings = true;
  InputHeightChanged();
}

void GroupCell::SetOutput(std::unique_ptr<Cell> &&output) {
  if ((m_cellPointers->m_answerCell) &&
      (m_cellPointers->m_answerCell->GetGroup() == this))
    m_cellPointers->m_answerCell = nullptr;

  m_output.reset();
  AppendOutput(std::move(output));
}

void GroupCell::RemoveOutput() {
  if (!m_output)
    return;
  m_numberedAnswersCount = 0;
  if (m_output == NULL)
    return;
  // If there is nothing to do we can skip the rest of this action.

  if ((m_cellPointers->m_answerCell) &&
      (m_cellPointers->m_answerCell->GetGroup() == this))
    m_cellPointers->m_answerCell = nullptr;

  if (GetGroupType() != GC_TYPE_IMAGE)
    m_output.reset();

  m_cellPointers->m_errorList.Remove(this);
  // Calculate the new cell height.

  Cell::Hide(false);

  m_height = m_inputHeight;
  if (m_inputLabel)
    m_width = m_inputLabel->GetFullWidth();
  else
    m_width = 50;
  m_configuration->AdjustWorksheetSize(true);

  // Move all cells that follow the current one up by the amount this cell has
  // shrunk.
  UpdateCellsInGroup();

  m_updateConfusableCharWarnings = true;
  ResetData();
}

void GroupCell::AppendOutput(std::unique_ptr<Cell> &&cell) {
  wxASSERT_MSG(cell, _("Bug: Trying to append NULL to a group cell."));
  if (!cell)
    return;
  if (!m_output) {
    m_output = std::move(cell);

    auto *input = GetEditable();
    if (m_groupType == GC_TYPE_CODE && input)
      input->ContainsChanges(false);
  } else
    CellList::AppendCell(m_output, std::move(cell));

  UpdateCellsInGroup();
  m_updateConfusableCharWarnings = true;
  m_cellsAppended = true;
}

WX_DECLARE_STRING_HASH_MAP(int, CmdsAndVariables);

void GroupCell::UpdateConfusableCharWarnings() {
  ClearToolTip();

  wxString output;
  if (GetOutput())
    output += GetOutput()->VariablesAndFunctionsList();
  // Extract all variable and command names from the cell including input and
  // output
  CmdsAndVariables cmdsAndVariables;

  if (GetEditable())
    for (auto const &tok :
	   MaximaTokenizer(output, m_configuration, GetEditable()->GetTokens())
	   .PopTokens())
      if ((tok.GetStyle() == TS_CODE_VARIABLE) ||
          (tok.GetStyle() == TS_CODE_FUNCTION))
        cmdsAndVariables[tok.GetText()] = 1;

  // Now we step through all the words we found
  while (!cmdsAndVariables.empty()) {
    CmdsAndVariables::iterator cmp = cmdsAndVariables.begin();
    wxString word;
    word = cmp->first;
    cmdsAndVariables.erase(cmp);
    // Now iterate through all remaining words
    cmp = cmdsAndVariables.begin();
    while (cmp != cmdsAndVariables.end()) {
      // iterate through all lookalike chars
      for (wxString::const_iterator it = m_lookalikeChars.begin();
           it != m_lookalikeChars.end(); ++it) {
        wxUniChar ch1 = *it++;
        wxASSERT(it != m_lookalikeChars.end());
        wxUniChar ch2 = *it;
        wxString word_subst = word;
        if (word_subst.Replace(ch1, ch2)) {
          if (cmp->first == word_subst)
            AddToolTip(_("Warning: Lookalike chars: ") + cmp->first.utf8_str() +
                       wxT("\u2260") + word.utf8_str());
        }
        word_subst = word;
        if (word_subst.Replace(ch2, ch1)) {
          if (cmp->first == word_subst)
            AddToolTip(_("Warning: Lookalike chars: ") + cmp->first +
                       wxT(" \u2260 ") + word);
        }
      }
      ++cmp;
    }
  }
  m_updateConfusableCharWarnings = false;
}

void GroupCell::Recalculate() {
  if (NeedsRecalculation(EditorFontSize())) {
    Cell::Recalculate(m_configuration->GetDefaultFontSize());
    m_mathFontSize = m_configuration->GetMathFontSize();
    ClearNeedsToRecalculateWidths();

    // Recalculating pagebreaks is simple
    if (m_groupType == GC_TYPE_PAGEBREAK) {
      m_width = m_configuration->GetCellBracketWidth();
      m_height = 2;
      m_center = 1;
      m_clientWidth_old = m_configuration->GetClientWidth();
      m_cellsAppended = false;
      return;
    }

    if (m_inputLabel != NULL)
      RecalculateInput();

    RecalculateOutput();
    m_height = m_outputRect.GetHeight() + m_inputHeight;
    // Move all cells that follow the current one down by the amount this cell
    // has grown.
    m_configuration->AdjustWorksheetSize(true);

    m_cellsAppended = false;
    m_clientWidth_old = m_configuration->GetClientWidth();
  }
  // The line breaking will have set our "needs recalculation" flag again.
  UpdateYPosition();
  Cell::Recalculate(m_configuration->GetDefaultFontSize());
  m_cellsAppended = false;
  m_clientWidth_old = m_configuration->GetClientWidth();
  wxASSERT(!NeedsRecalculation(m_configuration->GetDefaultFontSize()));
}

void GroupCell::InputHeightChanged() {
  ResetCellListSizes();
  if (m_inputLabel)
    m_inputLabel->ResetCellListSizes();
  EditorCell *editorCell = GetEditable();
  if (editorCell == NULL)
    return;
  RecalculateInput();
  if (m_output != NULL) {
    m_height += m_outputRect.GetHeight();
    m_outputRect.y = m_currentPoint.y + m_center;
    m_width = wxMax(m_width, m_output->GetLineWidth());
  }
  UpdateYPositionList();
}

// Called on resize events
// We need to forget line breaks/breakup cells and
// breakup cells and compute new line breaks
void GroupCell::OnSize() { Recalculate(); }

AFontSize GroupCell::EditorFontSize() const {
  AFontSize fontSize = m_configuration->GetDefaultFontSize();
  if (GetEditable()) {
    fontSize = m_configuration->GetFontSize(GetEditable()->GetStyle());
    if (fontSize.IsNull())
      fontSize = m_configuration->GetDefaultFontSize();
  }
  return fontSize;
}

void GroupCell::RecalculateInput() {
  m_currentPoint.x = m_configuration->GetIndent();

  if (m_inputLabel)
    m_inputLabel->SetCurrentPoint(m_currentPoint);
  if (GetEditable()) {
    wxPoint in = GetCurrentPoint();

    in.x += GetInputIndent();
    GetEditable()->SetCurrentPoint(in);
  }

  // special case
  if (m_groupType == GC_TYPE_PAGEBREAK) {
    m_height = Scale_Px(3);
    m_inputWidth = m_width = m_configuration->GetCellBracketWidth();
    m_inputHeight = m_height = 2;
    m_center = 0;
    return;
  } else {
    SetZeroSize();

    if ((m_configuration->ShowCodeCells()) || (m_groupType != GC_TYPE_CODE)) {
      if (GetEditable())
        GetEditable()->RecalculateList(EditorFontSize());

      if (m_inputLabel) {
        m_inputLabel->Recalculate(EditorFontSize());
        m_inputWidth = m_width = m_inputLabel->GetFullWidth();
        m_center = m_inputLabel->GetCenterList();
        m_inputLabel->ResetCellListSizesList();
        m_inputHeight = m_height = m_inputLabel->GetHeightList();
      }
      m_height = m_inputHeight;
    } else {
      m_inputHeight = 0;
      m_height = 0;
    }
  }
  if (!m_inputLabel)
    m_center = m_height;
  m_width = m_inputWidth;
}

void GroupCell::RecalculateOutput() {
  m_outputRect = wxRect(m_currentPoint.x, m_currentPoint.y + m_center, 0, 0);
  if (IsHidden())
    return;

  if (m_output == NULL)
    return;

  m_mathFontSize = m_configuration->GetMathFontSize();

  // The following line is a hack, kind of: Without it the first
  // (and only) line of an image that was included using the gui, not maxima
  // (and that therefore doesn't start in a label that per definition breaks
  // a line) later will not trigger the
  //  if (tmp.BreakLineHere())
  // that causes its height to be calculated.
  m_output->ForceBreakLine();

  m_mathFontSize = m_configuration->GetMathFontSize(); //-V519

  // Recalculate size of all output cells
  for (Cell &tmp : OnList(m_output.get())) {
    tmp.Recalculate(tmp.IsMath() ? m_configuration->GetMathFontSize()
		    : m_configuration->GetDefaultFontSize());
  }

  // Breakup cells and break lines
  BreakLines();

  // Recalculate size of cells again: Their size might have changed during
  // breaking lines
  for (Cell &tmp : OnList(m_output.get())) {
    tmp.Recalculate(tmp.IsMath() ? m_configuration->GetMathFontSize()
		    : m_configuration->GetDefaultFontSize());
  }

  // Calculate the height of the output
  for (Cell &tmp : OnDrawList(m_output.get())) {
    if (tmp.BreakLineHere()) {
      tmp.ResetCellListSizes();
      int height_Delta = tmp.GetHeightList();
      m_width = wxMax(m_width, tmp.GetLineWidth());
      m_outputRect.width = wxMax(m_outputRect.width, m_width);
      m_outputRect.height += height_Delta;

      if (tmp.GetPrevious() &&
          ((tmp.GetStyle() == TS_LABEL) || (tmp.GetStyle() == TS_USERLABEL)))
        m_outputRect.height += m_configuration->GetInterEquationSkip();

      if (tmp.HasBigSkip())
        m_outputRect.height += MC_LINE_SKIP;
    }
  }
}

bool GroupCell::NeedsRecalculation(AFontSize fontSize) const {
  return Cell::NeedsRecalculation(fontSize) ||
    //    (GetEditable() &&
    //    GetEditable()->NeedsRecalculation(EditorFontSize())) ||
    (m_clientWidth_old != m_configuration->GetClientWidth()) ||
    m_cellsAppended;
}

void GroupCell::UpdateYPositionList() {
  for (auto &tmp : OnList(this))
    tmp.UpdateYPosition();
}

void GroupCell::UpdateYPosition() {
  auto *const previous = GetPrevious();

  wxPoint point(m_configuration->GetIndent(), GetCenter());
  if (!previous) {
    point.y += m_configuration->GetBaseIndent();
    if (m_inputLabel)
      m_inputLabel->SetCurrentPoint(point);
  } else {
    point.y += m_configuration->GetGroupSkip();
    if (previous->GetCurrentPoint().y > 0)
      point.y += previous->GetCurrentPoint().y + previous->GetMaxDrop();
  }
  m_currentPoint = point;

  EditorCell *editor = GetEditable();
  if (editor)
    editor->SetCurrentPoint(CalculateInputPosition());
}

wxPoint GroupCell::CalculateInputPosition() {
  return wxPoint(m_currentPoint.x + GetInputIndent(), m_currentPoint.y);
}

int GroupCell::GetInputIndent() {
  int labelWidth = 0;
  if (m_configuration->IndentMaths())
    labelWidth = Scale_Px(m_configuration->GetLabelWidth()) + MC_TEXT_PADDING;

  if (m_inputLabel != NULL) {
    if (m_inputLabel->GetWidth() >= 0)
      labelWidth =
	wxMax(m_inputLabel->GetWidth() + MC_TEXT_PADDING, labelWidth);
    else
      labelWidth = wxMax(m_labelWidth_cached, labelWidth);
  }

  m_labelWidth_cached = labelWidth;

  return labelWidth;
}

void GroupCell::UpdateOutputPositions() {
  UpdateYPosition();
  if (m_output && !IsHidden()) {
    wxPoint in = GetCurrentPoint();
    if (m_configuration->ShowCodeCells() || (m_groupType != GC_TYPE_CODE))
      in.y += m_inputLabel->GetMaxDrop();

    m_outputRect.SetPosition(in);
    bool first = true;
    int drop = 0;
    for (Cell &tmp : OnDrawList(m_output.get())) {
      if (first || tmp.BreakLineHere()) {
        if (!first && tmp.HasBigSkip())
          in.y += MC_LINE_SKIP;

        in.x = GetCurrentPoint().x + GetLineIndent(&tmp);
        in.y += drop + tmp.GetCenterList();
        drop = tmp.GetMaxDrop();
      }
      tmp.SetCurrentPoint(in);
      in.x += tmp.GetWidth();
      first = false;
    }
  }
}

void GroupCell::Draw(wxPoint const point) {
  Cell::Draw(point);
  if (NeedsRecalculation(m_configuration->GetDefaultFontSize()))
    wxLogMessage(
		 wxString::Format(_("Not recalculated: \"%s\""), ToString().utf8_str()));
  if (m_configuration->ShowBrackets())
    DrawBracket();

  if (!DrawThisCell(point))
    return;

  if (m_updateConfusableCharWarnings)
    UpdateConfusableCharWarnings();

  wxDC *dc = m_configuration->GetDC();
  // draw a thick line for 'page break'
  // and return
  if (m_groupType == GC_TYPE_PAGEBREAK) {
    wxRect rect = GetRect(false);
    int y = rect.GetY();
    wxPen pen(m_configuration->GetColor(TS_CURSOR), 1, wxPENSTYLE_DOT);
    dc->SetPen(pen);
    dc->DrawLine(0, y, m_configuration->GetCanvasSize().GetWidth(), y);
    return;
  }

  wxRect rect = GetRect(false);

  if (m_configuration->GetIndent() < rect.GetRight()) {
    if (rect.GetLeft() <= m_configuration->GetCellBracketWidth())
      rect.SetLeft(m_configuration->GetIndent());

    //
    // Draw input and output
    //
    SetPen();

    if (m_output && !IsHidden()) {
      wxPoint in = point;
      if (m_configuration->ShowCodeCells() || (m_groupType != GC_TYPE_CODE))
        in.y += m_inputLabel->GetMaxDrop();

      m_outputRect.SetPosition(in);

      bool first = true;
      int drop = 0;
      for (Cell &tmp : OnDrawList(m_output.get())) {
        if (first || tmp.BreakLineHere()) {
          if (!first && tmp.HasBigSkip())
            in.y += MC_LINE_SKIP;

          in.x = point.x + GetLineIndent(&tmp);
          in.y += drop + tmp.GetCenterList();
          drop = tmp.GetMaxDrop();
        }

        tmp.Draw(in);
        in.x += tmp.GetWidth();
        first = false;
      }
    }
    if ((m_configuration->ShowCodeCells()) || (m_groupType != GC_TYPE_CODE)) {
      m_configuration->Outdated(false);

      EditorCell *input = GetEditable();
      if (input)
        input->Draw(CalculateInputPosition());

      if (GetPrompt())
        GetPrompt()->Draw(point);

      if (m_groupType == GC_TYPE_CODE && input)
        m_configuration->Outdated(input->ContainsChanges());
    }
  }
  m_configuration->Outdated(false);
}

bool GroupCell::AddEnding() {
  return GetEditable() && GetEditable()->AddEnding();
}

wxRect GroupCell::GetRect(bool WXUNUSED(all)) const {
  return wxRect(m_currentPoint.x, m_currentPoint.y - m_center, m_width,
                m_height);
}

int GroupCell::GetLineIndent(Cell *cell) {
  if (cell && (cell->GetStyle() != TS_LABEL) &&
      (cell->GetStyle() != TS_USERLABEL) &&
      (cell->GetStyle() != TS_MAIN_PROMPT) &&
      (cell->GetStyle() != TS_OTHER_PROMPT) &&
      (cell->GetStyle() != TS_ASCIIMATHS) && m_configuration->IndentMaths())
    return Scale_Px(m_configuration->GetLabelWidth()) + 2 * MC_TEXT_PADDING;
  return 0;
}

void GroupCell::UpdateCellsInGroup() {
  if (m_output != NULL)
    m_cellsInGroup = 2 + m_output->CellsInListRecursive();
  else
    m_cellsInGroup = 2;
}

void GroupCell::CellUnderPointer(GroupCell *cell) {
  m_cellPointers->m_groupCellUnderPointer = cell;
}

void GroupCell::DrawBracket() {
  // If the current cell doesn't know where it is on the screen we don't
  // attempt to draw it's bracket.
  if ((GetRect().GetLeft() < 0) || (GetRect().GetTop() < 0))
    return;

  bool drawBracket = !m_configuration->HideBrackets();

  if (this == m_cellPointers->m_groupCellUnderPointer)
    drawBracket = true;

  wxDC *dc = m_configuration->GetDC();
  wxDC *adc = m_configuration->GetAntialiassingDC();

  int selectionStart_px = -1;
  if (m_cellPointers->m_selectionStart &&
      (m_cellPointers->m_selectionStart->GetType() == MC_TYPE_GROUP))
    selectionStart_px = m_cellPointers->m_selectionStart.CastAs<GroupCell *>()
      ->m_currentPoint.y;

  int selectionEnd_px = -1;
  if (m_cellPointers->m_selectionEnd &&
      (m_cellPointers->m_selectionEnd->GetType() == MC_TYPE_GROUP))
    selectionEnd_px =
      m_cellPointers->m_selectionEnd.CastAs<GroupCell *>()->m_currentPoint.y;

  // Mark this GroupCell as selected if it is selected. Else clear the space we
  // will add brackets in
  if ((m_currentPoint.y >= selectionStart_px) &&
      (m_currentPoint.y <= selectionEnd_px)) {
    dc->SetPen(*(wxThePenList->FindOrCreatePen(
					       m_configuration->GetColor(TS_SELECTION),
					       m_configuration->GetDefaultLineWidth(), wxPENSTYLE_SOLID)));
    // window linux, set a pen
    dc->SetBrush(*(wxTheBrushList->FindOrCreateBrush(
						     m_configuration->GetColor(TS_SELECTION))));
    drawBracket = true;
  } else if (m_cellPointers->m_errorList.Contains(this)) {
    dc->SetPen(*wxRED_PEN);
    dc->SetBrush(*wxRED_BRUSH);
    drawBracket = true;
  } else {
    if ((m_cellPointers->m_answerCell) &&
        (m_cellPointers->m_answerCell->GetGroup() == this)) {
      dc->SetPen(*wxYELLOW_PEN);
      dc->SetBrush(*wxYELLOW_BRUSH);
      drawBracket = true;
    } else {
      dc->SetBrush(m_configuration->GetBackgroundBrush());
      dc->SetPen(*(wxThePenList->FindOrCreatePen(
						 m_configuration->DefaultBackgroundColor(),
						 m_configuration->GetDefaultLineWidth(), wxPENSTYLE_SOLID)));
    }
  }
  wxRect rect = GetRect();
  rect = wxRect(m_configuration->GetIndent() -
		m_configuration->GetCellBracketWidth(),
                rect.GetTop() - 2, m_configuration->GetCellBracketWidth(),
                rect.GetHeight() + 5);
  if (m_configuration->InUpdateRegion(rect))
    dc->DrawRectangle(Cell::CropToUpdateRegion(rect));

  //
  // Mark groupcells currently in queue.
  //
  if (m_inEvaluationQueue) {
    drawBracket = true;
    dc->SetBrush(*wxTRANSPARENT_BRUSH);
    if (m_lastInEvaluationQueue)
      dc->SetPen(*(wxThePenList->FindOrCreatePen(
						 m_configuration->GetColor(TS_CELL_BRACKET),
						 2 * m_configuration->GetDefaultLineWidth(), wxPENSTYLE_SOLID)));
    else
      dc->SetPen(*(wxThePenList->FindOrCreatePen(
						 m_configuration->GetColor(TS_CELL_BRACKET),
						 m_configuration->GetDefaultLineWidth(), wxPENSTYLE_SOLID)));

    wxRect bracketRect = wxRect(
				m_configuration->GetIndent() - m_configuration->GetCellBracketWidth(),
				rect.GetTop() - 2, m_configuration->GetCellBracketWidth(),
				rect.GetHeight() + 5);
    if (m_configuration->InUpdateRegion(bracketRect))
      dc->DrawRectangle(bracketRect);
  }

  Cell *editable = GetEditable();
  if (editable != NULL && editable->IsActive()) {
    drawBracket = true;
    adc->SetPen(*(wxThePenList->FindOrCreatePen(
						m_configuration->GetColor(TS_ACTIVE_CELL_BRACKET),
						2 * m_configuration->GetDefaultLineWidth(),
						wxPENSTYLE_SOLID))); // window linux, set a pen
    dc->SetBrush(*(wxTheBrushList->FindOrCreateBrush(
						     m_configuration->GetColor(TS_ACTIVE_CELL_BRACKET)))); // highlight c.
  } else {
    adc->SetPen(*(wxThePenList->FindOrCreatePen(
						m_configuration->GetColor(TS_CELL_BRACKET),
						m_configuration->GetDefaultLineWidth(),
						wxPENSTYLE_SOLID))); // window linux, set a pen
    dc->SetBrush(*(wxTheBrushList->FindOrCreateBrush(
						     m_configuration->GetColor(TS_CELL_BRACKET)))); // highlight c.
  }

  if ((!IsHidden()) && (!m_hiddenTree)) {
    dc->SetBrush(*wxTRANSPARENT_BRUSH);
  }

  if (drawBracket) {
    adc->SetBrush(dc->GetBrush());
    SetPen(1.5);
    int bracketWidth = m_configuration->GetCellBracketWidth() -
      m_configuration->GetDefaultLineWidth();
    int lineWidth = m_configuration->GetDefaultLineWidth();
    int lineWidth_2 = m_configuration->GetDefaultLineWidth() / 2.0;
    if (IsFoldable()) { // draw the square that allows hiding and unhiding the
                        // cell
      const wxPoint points[4] = {{-bracketWidth, 0},
                                 {-bracketWidth, bracketWidth},
                                 {-lineWidth, bracketWidth},
                                 {-lineWidth, 0}};
      adc->DrawPolygon(4, points, m_currentPoint.x,
                       m_currentPoint.y - m_center);
    } else {
      int n = 0;
      wxPoint points[9];
      // draw the triangle that allows hiding and unhiding the cell
      points[n++] = {-bracketWidth + lineWidth_2, bracketWidth - lineWidth_2};
      points[n++] = {-lineWidth, lineWidth_2};
      points[n++] = {-bracketWidth + lineWidth_2, lineWidth_2};

      // The rest of the bracket
      if (m_configuration->ShowCodeCells() && m_groupType == GC_TYPE_CODE &&
          m_output && !IsHidden()) {
        points[n++] = {-bracketWidth + lineWidth_2,
	  m_inputLabel->GetHeightList()};
        points[n++] = {-bracketWidth / 2 + lineWidth_2,
	  m_inputLabel->GetHeightList()};
        points[n++] = {-bracketWidth + lineWidth_2,
	  m_inputLabel->GetHeightList()};
      }

      // The remaining part of the vertical line at the back
      points[n++] = {-bracketWidth + lineWidth_2, m_height - lineWidth};
      // The horizontal line at the bottom
      points[n++] = {-lineWidth, m_height - lineWidth};
      points[n++] = {-bracketWidth + lineWidth_2, m_height - lineWidth};

      wxASSERT(n <= (std::end(points) - std::begin(points)));
      adc->DrawPolygon(n, points, m_currentPoint.x,
                       m_currentPoint.y - m_center);
    }
  }
}

wxRect GroupCell::HideRect() {
  return wxRect(m_currentPoint.x - m_configuration->GetCellBracketWidth() -
		m_configuration->GetDefaultLineWidth() / 2,
                m_currentPoint.y - m_center -
		m_configuration->GetDefaultLineWidth() / 2,
                m_configuration->GetCellBracketWidth() +
		m_configuration->GetDefaultLineWidth(),
                m_configuration->GetCellBracketWidth() +
		m_configuration->GetDefaultLineWidth());
}

wxString GroupCell::ToString() const {
  // We don't want illegal strings to pop up assert dialogues
  wxLogNull logNull;
  wxString str;

  if (m_inputLabel != NULL) {
    if ((m_configuration->ShowCodeCells()) || (m_groupType != GC_TYPE_CODE)) {
      str = m_inputLabel->ToString();

      if (GetEditable() != NULL)
        str += GetEditable()->ToString();

      str.Replace(wxT("\n"), wxT("\n\t"));
    }
  }

  if (!IsHidden()) {
    bool firstCell = true;
    for (Cell &tmp : OnDrawList(m_output.get())) {
      if (firstCell || (tmp.HasHardLineBreak() && !str.empty()))
        str += wxT("\n");
      str += tmp.ToString();
      firstCell = false;
    }
  }
  return str;
}

wxString GroupCell::ToTeX() const {
  return ToTeX(wxEmptyString, wxEmptyString, NULL);
}

wxString GroupCell::ToRTF() const {
  if (m_groupType == GC_TYPE_PAGEBREAK)
    return (wxT("\\page "));

  wxString retval;
  if (m_groupType == GC_TYPE_CODE) {
    if (m_inputLabel && m_configuration->ShowCodeCells()) {
      if (GetPrevious())
        retval =
	  wxT("\\par}{\\pard\\s22\\li1105\\lin1105\\fi-1105\\f0\\fs24 \n");
      else
        retval += wxT("\\pard\\s22\\li1105\\lin1105\\fi-1105\\f0\\fs24 ");
      retval += RTFescape(m_inputLabel->ToString());
      retval += wxT("\\tab\n");
    } else {
      if (GetPrevious())
        retval = wxT("\\par}\n{\\pard\\s21\\li1105\\lin1105\\f0\\fs24 ");
      else
        retval = wxT("\\pard\\s21\\li1105\\lin1105\\f0\\fs24 ");
    }
  } else
    retval = wxT("\\par}\n{");

  if (GetEditable() != NULL)
    retval += GetEditable()->ToRTF();

  Cell *out = GetLabel();
  if (out != NULL) {
    retval += out->ListToRTF(true);
  }
  return retval;
}

wxString GroupCell::ToTeX(wxString imgDir, wxString filename,
                          int *imgCounter) const {
  int myImgCounter = 0;
  if (imgCounter == NULL)
    imgCounter = &myImgCounter;
  wxString str;
  switch (m_groupType) {
  case GC_TYPE_PAGEBREAK:
    str = wxT("\\pagebreak\n");
    break;

  case GC_TYPE_IMAGE:
    if (imgDir != wxEmptyString) {
      GroupCell parent(m_configuration, GC_TYPE_CODE);
      auto const copy = m_output->Copy(&parent);
      auto *const imgCopy = dynamic_cast<ImgCell *>(copy.get());
      (*imgCounter)++;
      wxString image = filename + wxString::Format(wxT("_%d"), *imgCounter);
      wxString file =
	imgDir + wxT("/") + image + wxT(".") + imgCopy->GetExtension();

      if (!wxDirExists(imgDir))
        wxMkdir(imgDir);

      if (imgCopy->ToImageFile(file).x >= 0) {
        str << wxT("\\begin{figure}[htb]\n") << wxT("  \\centering\n")
            << wxT("    \\includeimage{") << filename << wxT("_img/") << image
            << wxT("}\n") << wxT("  \\caption{")
            << m_inputLabel->GetNext()->ToTeX().Trim() << wxT("}\n")
            << wxT("\\end{figure}\n");
      }
    } else
      str << wxT("\n\\verb|<<GRAPHICS>>|\n");
    break;

  case GC_TYPE_CODE:
    str = ToTeXCodeCell(imgDir, filename, imgCounter);
    str.Replace(wxT("\\[\\displaystyle \\]"), wxT(""));
    break;

  default:
    if (GetEditable() != NULL && !IsHidden()) {
      str = GetEditable()->ListToTeX();
      str.Trim(true);
      switch (GetEditable()->GetStyle()) {
      case TS_TITLE:
        str = wxT("\n\\pagebreak{}\n{\\Huge {\\scshape ") + str + wxT("}}\n");
        str += wxT("\\setcounter{section}{0}\n\\setcounter{subsection}{0}\n");
        str += wxT("\\setcounter{figure}{0}\n");
        break;
      case TS_SECTION:
        // Trim() strings for TeX export to remove newlines in \section{},
        // \subsection{}, ... commands LaTeX creates an error on the following
        // code: \section{Chapter 1
        //
        // }
        str = wxT("\n\\section{") + str.Trim() + wxT("}");
        break;
      case TS_SUBSECTION:
        str = wxT("\n\\subsection{") + str.Trim() + wxT("}");
        break;
      case TS_SUBSUBSECTION:
        str = wxT("\n\\subsubsection{") + str.Trim() + wxT("}");
        break;
      case TS_HEADING5:
        str = wxT("\n\\paragraph{") + str.Trim() + wxT("}");
        break;
      case TS_HEADING6:
        str = wxT("\n\\subparagraph{") + str.Trim() + wxT("}");
        break;
      default:
        if (str.StartsWith(wxT("TeX:"))) {
          str = GetEditable()->ToString();
          str = str.Mid(5, str.Length());
        }
        break;
      }
    }
    break;
  }

  return str;
}

wxString GroupCell::ToTeXCodeCell(wxString imgDir, wxString filename,
                                  int *imgCounter) const {
  wxString str;

  // Input cells
  if (m_configuration->ShowCodeCells()) {
    // For LaTeX export we must use a dot as decimal separator
    // Save LC_NUMERIC, set it to "C", print out the float and then restore it.
    const std::string saved_lc_numeric{
      setlocale(LC_NUMERIC, NULL)}; // get current LC_NUMERIC locale
    setlocale(LC_NUMERIC, "C");
    str += wxString::Format(
			    "\n\n\\noindent\n%%%%%%%%%%%%%%%%\n%%%% "
			    "INPUT:\n\\begin{minipage}[t]{%fem}\\color{red}\\bfseries\n",
			    static_cast<double>(m_configuration->GetLabelWidth() / 14)) +
      m_inputLabel->ToTeX() + wxString("\n\\end{minipage}");
    setlocale(LC_NUMERIC, saved_lc_numeric.c_str());

    if (m_inputLabel->GetNext()) {
      str += wxT("\n\\begin{minipage}[t]{\\textwidth}\\color{blue}\n") +
	m_inputLabel->GetNext()->ToTeX() + "\n\\end{minipage}";
    }
  }

  if (m_output != NULL) {
    str += wxT("\n%%%% OUTPUT:\n");
    // Need to define labelcolor if this is Copy as LaTeX!
    if (imgCounter == NULL)
      str += wxT("\\definecolor{labelcolor}{RGB}{100,0,0}\n");

    bool mathMode = false;

    for (Cell &tmp : OnDrawList(m_output.get())) {
      if (tmp.GetType() == MC_TYPE_IMAGE || tmp.GetType() == MC_TYPE_SLIDE) {
        str << ToTeXImage(&tmp, imgDir, filename, imgCounter);
      } else {
        switch (tmp.GetStyle()) {
        case TS_LABEL:
        case TS_USERLABEL:
          if (mathMode)
            str += wxT("\\mbox{}\\]\n\\[\\displaystyle ");
          else {
            str += wxT("\\[\\displaystyle ");
            mathMode = true;
          }
          str += tmp.ToTeX() + wxT("\n");
          break;

        case TS_STRING:
          if (mathMode) {
            str += wxT("\\mbox{}\n\\]");
            mathMode = false;
          }
          str += TexEscapeOutputCell(tmp.ToTeX()) + wxT("\n");
          break;

        default:
          if (!mathMode) {
            str += wxT("\\[\\displaystyle ");
            mathMode = true;
          }
          str += tmp.ToTeX();
          break;
        }
      }
    }

    if (mathMode) {
      // Some invisible dummy content that keeps TeX happy if there really is
      // no output to display.
      str += wxT("\\mbox{}\n\\]\n%%%%%%%%%%%%%%%%");
    }
  } else
    str += wxT("\n\n\\noindent%\n");

  return str;
}

wxString GroupCell::ToTeXImage(Cell *tmp, wxString imgDir, wxString filename,
                               int *imgCounter) {
  wxASSERT_MSG((imgCounter != NULL), _("Bug: No image counter to write to!"));
  if (imgCounter == NULL)
    return wxEmptyString;

  wxString str;

  if (imgDir != wxEmptyString) {
    // TODO: Is this the right Group?
    auto const copy = tmp->Copy(tmp->GetGroup());
    auto *const imgCopy = dynamic_cast<ImgCell *>(copy.get());
    (*imgCounter)++;
    wxString image = filename + wxString::Format(wxT("_%d"), *imgCounter);
    if (!wxDirExists(imgDir))
      if (!wxMkdir(imgDir))
        return wxEmptyString;

    wxString file =
      imgDir + wxT("/") + image + wxT(".") + imgCopy->GetExtension();
    if (imgCopy->ToImageFile(file).x >= 0)
      str += wxT("\\includegraphics[width=.95\\linewidth,height=."
                 "80\\textheight,keepaspectratio]{") +
	filename + wxT("_img/") + image + wxT("}");
    else
      str << wxT("\n\\verb|<<GRAPHICS>>|\n");
  }

  return str;
}

wxString GroupCell::ToXML() const {
  wxString str;
  str = wxT("\n<cell"); // start opening tag
  // write "type" according to m_groupType
  switch (m_groupType) {
  case GC_TYPE_CODE: {
    str += wxT(" type=\"code\"");
    int i = 0;
    for (StringHash::const_iterator it = m_knownAnswers.begin();
         it != m_knownAnswers.end(); ++it) {
      i++;
      // In theory the attribute should be saved and read verbatim, with the
      // exception of the characters XML wants to be quoted. In reality
      // wxWidget's newline handling seems to be broken => escape newlines.
      wxString question = Cell::XMLescape(it->first);
      question.Replace(wxT("\n"), wxT("&#10;"));
      wxString answer = Cell::XMLescape(it->second);
      answer.Replace(wxT("\n"), wxT("&#10;"));
      str += wxString::Format(wxT(" question%i=\""), i) + question + wxT("\"");
      str += wxString::Format(wxT(" answer%i=\""), i) + answer + wxT("\"");
    }

    if (m_autoAnswer)
      str += wxT(" auto_answer=\"yes\"");
    break;
  }
  case GC_TYPE_IMAGE:
    str += wxT(" type=\"image\"");
    break;
  case GC_TYPE_TEXT:
    str += wxT(" type=\"text\"");
    break;
  case GC_TYPE_TITLE:
    str += wxT(" type=\"title\" sectioning_level=\"1\"");
    break;
  case GC_TYPE_SECTION:
    str += wxT(" type=\"section\" sectioning_level=\"2\"");
    break;
  case GC_TYPE_SUBSECTION:
    str += wxT(" type=\"subsection\" sectioning_level=\"3\"");
    break;
  case GC_TYPE_SUBSUBSECTION:
    // We save subsubsections as subsections with a higher sectioning level:
    // This makes them backwards-compatible in the way that they are displayed
    // as subsections on old wxMaxima installations.
    str += wxT(" type=\"subsection\" sectioning_level=\"4\"");
    break;
  case GC_TYPE_HEADING5:
    str += wxT(" type=\"subsection\" sectioning_level=\"5\"");
    break;
  case GC_TYPE_HEADING6:
    str += wxT(" type=\"subsection\" sectioning_level=\"6\"");
    break;
  case GC_TYPE_PAGEBREAK: {
    str += wxT(" type=\"pagebreak\"/>");
    return str;
  } break;
  default:
    str += wxT(" type=\"unknown\"");
    break;
  }

  if (GetSuppressTooltipMarker())
    str += wxT(" hideToolTip=\"true\"");

  // write hidden status
  if (IsHidden())
    str += wxT(" hide=\"true\"");
  str += wxT(">\n");

  Cell *input = GetEditable();
  Cell *output = GetLabel();
  // write contents
  switch (m_groupType) {
  case GC_TYPE_CODE:
    if (input != NULL) {
      str += wxT("<input>\n");
      str += input->ListToXML();
      str += wxT("</input>");
    }
    if (output != NULL) {
      str += wxT("\n<output>\n");
      str += wxT("<mth>");
      str += output->ListToXML();
      str += wxT("\n</mth></output>");
    }
    break;
  case GC_TYPE_IMAGE:
    if (input != NULL)
      str += input->ListToXML();
    if (output != NULL)
      str += output->ListToXML();
    break;
  case GC_TYPE_TEXT:
    if (input)
      str += input->ListToXML();
    break;
  case GC_TYPE_TITLE:
  case GC_TYPE_SECTION:
  case GC_TYPE_SUBSECTION:
  case GC_TYPE_SUBSUBSECTION:
  case GC_TYPE_HEADING5:
  case GC_TYPE_HEADING6:
    if (input)
      str += input->ListToXML();
    if (m_hiddenTree) {
      str += wxT("<fold>");
      str += m_hiddenTree->ListToXML();
      str += wxT("</fold>");
    }
    break;
  default: {
    for (const Cell &tmp : OnList(output))
      // cppcheck-suppress useStlAlgorithm
      str += tmp.ListToXML();

    break;
  }
  }
  str += wxT("\n</cell>\n");

  return str;
}

Cell::Range GroupCell::GetInnerCellsInRect(const wxRect &rect) const {
  if (m_inputLabel->ContainsRect(rect))
    return m_inputLabel->GetCellsInRect(rect);
  if (m_output && !IsHidden() && m_outputRect.Contains(rect))
    return m_output->GetCellsInRect(rect);
  return {const_cast<GroupCell *>(this), const_cast<GroupCell *>(this)};
}

Cell::Range GroupCell::GetCellsInOutputRect(const wxRect &rect,
                                            const wxPoint one,
                                            const wxPoint two) const {
  if (IsHidden())
    return {};

  wxPoint start = two, end = one;
  if (one.y < two.y || (one.y == two.y && one.x < two.x)) {
    start = one;
    end = two;
  }

  // Lets select a rectangle
  Range r = m_output->GetListCellsInRect(rect);

  if (!r.first)
    return {};

  wxASSERT(r.last);

  // If selection is on multiple lines, we need to correct it
  if (r.first->GetCurrentY() != r.last->GetCurrentY()) {
    Cell *const tmp = r.last;

    // Find the first cell in selection
    for (Cell &curr : OnDrawList(r.first)) {
      r.first = &curr;
      if (&curr == tmp || (curr.GetCurrentX() + curr.GetWidth() >= start.x &&
                           curr.GetCurrentY() + curr.GetDrop() >= start.y))
        break;
    }

    // Find the last cell in selection
    r.last = r.first;
    for (Cell &curr : OnDrawList(r.first->GetNext())) {
      if (curr.GetCurrentX() <= end.x &&
          curr.GetCurrentY() - curr.GetCenterList() <= end.y)
        r.last = &curr;
      if (&curr == tmp)
        break;
    }
  }

  if (r.first == r.last)
    r = r.first->GetInnerCellsInRect(rect);
  return r;
}

const wxString &GroupCell::GetToolTip(const wxPoint point) const {
  // TODO: There's a question of whether we want to return
  // the local tooltip, or empty string (latter would be in line
  // with Cell's behavior.
  if (!ContainsPoint(point))
    return GetLocalToolTip();

  // Default assumption: will be overwritten by the next command,
  // if there is a more accurate solution.
  m_cellPointers->m_cellUnderPointer = const_cast<GroupCell *>(this);

  const wxString *retval = &GetLocalToolTip();

  if (IsHidden())
    return *retval;

  for (auto &tmp : OnList(m_output.get())) {
    // If a cell contains a cell containing a tooltip, the tooltip of the
    // containing cell will be overridden.
    // TODO: Why do we keep iterating? Is there a reason to return the
    // tooltip of the last cell with a tooltip, instead of the first one?

    auto &toolTip = tmp.GetToolTip(point);
    if (!toolTip.empty())
      retval = &toolTip;
  }

  return *retval;
}

// cppcheck-suppress functionConst
bool GroupCell::SetEditableContent(const wxString &text) {
  if (GetEditable()) {
    GetEditable()->SetValue(text);
    return true;
  } else
    return false;
}

void GroupCell::BreakLines() {
  Cell *cell = m_output.get();

  if (cell == NULL)
    return;

  if (NeedsRecalculation(EditorFontSize()))
    m_output->RecalculateList(m_configuration->GetMathFontSize());

  // 1st step: Tell all cells to display as beautiful 2d object, if that is
  // possible.
  if (UnBreakUpCells(cell)) {
    m_output->ResetCellListSizesList();
    m_output->RecalculateList(m_configuration->GetMathFontSize());
  }

  // 2nd step: Convert all objects that are wider than a line to 1D objects that
  // (hopefully) can be broken into lines
  if (BreakUpCells(cell)) {
    m_output->ResetCellListSizesList();
    m_output->RecalculateList(m_configuration->GetMathFontSize());
  }

  // 3rd step: Determine a sane maximum line width
  int fullWidth = m_configuration->GetClientWidth();
  int currentWidth = GetLineIndent(cell);
  if ((cell->GetStyle() != TS_LABEL) && (cell->GetStyle() != TS_USERLABEL))
    fullWidth -= m_configuration->GetIndent();

  // We don't want the text go exactly to the right border.
  fullWidth -= Scale_Px(1);

  // Don't let the layout degenerate for small window widths
  if (fullWidth < Scale_Px(150))
    fullWidth = Scale_Px(150);

  // 4th step: break the output into lines.
  if (!IsHidden()) {
    bool prevBroken = false;
    for (Cell &tmp : OnDrawList(cell)) {
      if (prevBroken) {
        currentWidth += GetLineIndent(&tmp);
        prevBroken = false;
      }
      int const cellWidth = tmp.GetWidth();
      tmp.SoftLineBreak(false);
      if (tmp.BreakLineHere() || (currentWidth + cellWidth >= fullWidth)) {
        tmp.SoftLineBreak(true);
        currentWidth = 0;
        prevBroken = true;
      }
      currentWidth += cellWidth;
    }
  }
  m_output->ResetDataList();
  ResetCellListSizes();
}

Cell::Range GroupCell::GetCellsInOutput() const {
  if (IsHidden())
    return {};

  Range r = {};

  for (Cell &tmp : OnDrawList(m_output.get())) {
    r.first = &tmp;
    if (tmp.GetStyle() == TS_LABEL || tmp.GetStyle() == TS_USERLABEL)
      break;
  }

  for (Cell &tmp : OnDrawList(r.first))
    r.last = &tmp;

  return r;
}

bool GroupCell::BreakUpCells(Cell *cell) {
  if (cell == NULL)
    return false;

  int showLength;
  switch (m_configuration->ShowLength()) {
  case 0:
    showLength = 5000;
    break;
  case 1:
    showLength = 10000;
    break;
  case 2:
    showLength = 25000;
    break;
  case 3:
    showLength = 50000;
    break;
  default:
    showLength = 500;
  }

  int clientWidth =
    .8 * m_configuration->GetClientWidth() - m_configuration->GetIndent();
  if (clientWidth < Scale_Px(50))
    clientWidth = Scale_Px(50);

  // Reduce the number of steps involved in layouting big equations
  if (m_cellsInGroup > showLength) {
    wxLogMessage(
		 _("Resolving to 1D layout for one cell in order to save time"));
    return false;
  } else {
    bool lineHeightsChanged = false;
    if (!IsHidden())
      for (Cell &tmp : OnDrawList(cell)) {
        if (tmp.GetWidth() < 0)
          tmp.Recalculate(m_configuration->GetMathFontSize());
        if (tmp.GetWidth() > clientWidth)
          lineHeightsChanged |= tmp.BreakUp();
      }
    return lineHeightsChanged;
  }
}

bool GroupCell::UnBreakUpCells(Cell *cell) {
  int showLength;
  switch (m_configuration->ShowLength()) {
  case 0:
    showLength = 50;
    break;
  case 1:
    showLength = 500;
    break;
  case 2:
    showLength = 2500;
    break;
  case 3:
    showLength = 5000;
    break;
  default:
    showLength = 500;
  }

  // Reduce the number of steps involved in layouting big equations
  if (m_cellsInGroup > showLength) {
    wxLogMessage(
		 _("Resolving to linear layout for one big cell in order to save time"));
    return true;
  }

  bool retval = false;
  for (Cell &tmp : OnDrawList(cell)) {
    if (tmp.IsBrokenIntoLines()) {
      tmp.Unbreak();
      retval = true;
    }
  }

  return retval;
}

// support for hiding text, code cells

void GroupCell::Hide(bool hide) {
  if (IsFoldable())
    return;

  if (IsHidden() == hide)
    return;

  Cell::Hide(hide);
  if ((m_groupType == GC_TYPE_TEXT) || (m_groupType == GC_TYPE_CODE))
    GetEditable()->SetFirstLineOnly(hide);

  // Don't keep cached versions of scaled images around if they aren't visible
  // at all.
  if (GetLabel())
    GetLabel()->ClearCacheList();

  m_cellsAppended = true;
  ResetSize();
  GetEditable()->ResetSize();
}

void GroupCell::SwitchHide() { Hide(!IsHidden()); }

//
// support for folding/unfolding sections
//
bool GroupCell::HideTree(std::unique_ptr<GroupCell> &&tree) {
  if (m_hiddenTree)
    return false;
  m_hiddenTree = std::move(tree);
  m_hiddenTree->SetHiddenTreeParent(this);

  // Clear cached images from cells that are hidden
  for (auto &tmp : OnList(m_hiddenTree.get())) {
    if (tmp.GetLabel())
      tmp.GetLabel()->ClearCacheList();
  }

  m_cellsAppended = true;
  return true;
}

std::unique_ptr<GroupCell> GroupCell::UnhideTree() {
  m_hiddenTree->SetHiddenTreeParent(m_hiddenTreeParent);
  if (m_hiddenTree)
    m_cellsAppended = true;
  return std::move(m_hiddenTree);
}

/**
 * Unfold a tree from the bottom up, when a hidden cell needs to be seen.
 *
 * @return true if any cells were unfolded.
 */
bool GroupCell::RevealHidden() {
  if (!m_hiddenTreeParent)
    return false;
  m_hiddenTreeParent->RevealHidden();
  m_hiddenTreeParent->Unfold();
  return true;
}

/**
 * For every cell in this GroupCell, set m_hiddenTreeParent to parent.
 * This way, the field can be used to traverse up the tree no matter which
 * child we are on. In other words, every child knows its parent node.
 */
void GroupCell::SetHiddenTreeParent(GroupCell *parent) {
  for (auto &cell : OnList(this))
    cell.m_hiddenTreeParent = parent;
}

GroupCell *GroupCell::Fold() {
  if (!IsFoldable() || m_hiddenTree) // already folded?? shouldn't happen
    return NULL;
  if (GetNext() == NULL)
    return NULL;
  GroupType nextgct = GetNext()->GetGroupType(); // groupType of the next cell
  if ((m_groupType == nextgct) || IsLesserGCType(nextgct))
    return NULL; // if the next gc shouldn't be folded, exit

  // now there is at least one cell to fold (at least m_next)
  GroupCell *end = GetNext();

  while (end != NULL) {
    if (end->GetLabel())
      end->GetLabel()->ClearCacheList();

    GroupCell *tmp = end->GetNext();
    if (tmp == NULL)
      break;
    if ((m_groupType == tmp->GetGroupType()) ||
        IsLesserGCType(tmp->GetGroupType()))
      break; // the next one of the end is not suitable for folding, break
    end = tmp;
  }
  wxASSERT(end);

  auto tornOut = CellList::TearOut(GetNext(), end);
  wxASSERT(tornOut.cellOwner);
  m_hiddenTree =
    static_unique_ptr_cast<GroupCell>(std::move(tornOut.cellOwner));
  m_hiddenTree->SetHiddenTreeParent(this);
  m_cellsAppended = true;
  return this;
}

// unfolds the m_hiddenTree beneath this cell
// be careful to update m_last if this happens in the main tree in MathCtrl
GroupCell *GroupCell::Unfold() {
  if (!IsFoldable() || !m_hiddenTree)
    return NULL;

  m_cellsAppended = true;
  auto splicedIn = CellList::SpliceInAfter(this, std::move(m_hiddenTree));
  GetNext()->SetHiddenTreeParent(m_hiddenTreeParent);
  return dynamic_cast<GroupCell *>(splicedIn.lastSpliced);
}

GroupCell *GroupCell::FoldAll() {
  GroupCell *result = NULL;
  for (auto &tmp : OnList(this)) {
    if (tmp.IsFoldable() && !tmp.m_hiddenTree) {
      tmp.Fold();
      result = &tmp;
    }
    if (tmp.m_hiddenTree != NULL)
      tmp.m_hiddenTree->FoldAll();
  }
  return result;
}

// unfolds recursively its contents
// if (all) then also calls it on it's m_next
GroupCell *GroupCell::UnfoldAll() {
  GroupCell *result = NULL;
  for (auto &tmp : OnList(this)) {
    if (tmp.IsFoldable() && (tmp.m_hiddenTree != NULL)) {
      tmp.Unfold();
      result = &tmp;
    }
    if (tmp.m_hiddenTree != NULL)
      tmp.m_hiddenTree->UnfoldAll();
  }
  return result;
}

bool GroupCell::IsLesserGCType(GroupType comparedTo) const {
  switch (m_groupType) {
  case GC_TYPE_CODE:
  case GC_TYPE_TEXT:
  case GC_TYPE_PAGEBREAK:
  case GC_TYPE_IMAGE:
    return (comparedTo == GC_TYPE_TITLE) || (comparedTo == GC_TYPE_SECTION) ||
      (comparedTo == GC_TYPE_SUBSECTION) ||
      (comparedTo == GC_TYPE_SUBSUBSECTION) ||
      (comparedTo == GC_TYPE_HEADING5) || (comparedTo == GC_TYPE_HEADING6);
  case GC_TYPE_HEADING6:
    return (comparedTo == GC_TYPE_TITLE) || (comparedTo == GC_TYPE_SECTION) ||
      (comparedTo == GC_TYPE_SUBSECTION) ||
      (comparedTo == GC_TYPE_SUBSUBSECTION) ||
      (comparedTo == GC_TYPE_HEADING5);
  case GC_TYPE_HEADING5:
    return (comparedTo == GC_TYPE_TITLE) || (comparedTo == GC_TYPE_SECTION) ||
      (comparedTo == GC_TYPE_SUBSECTION) ||
      (comparedTo == GC_TYPE_SUBSUBSECTION);
  case GC_TYPE_SUBSUBSECTION:
    return (comparedTo == GC_TYPE_TITLE) || (comparedTo == GC_TYPE_SECTION) ||
      (comparedTo == GC_TYPE_SUBSECTION);
  case GC_TYPE_SUBSECTION:
    return (comparedTo == GC_TYPE_TITLE) || (comparedTo == GC_TYPE_SECTION);
  case GC_TYPE_SECTION:
    return comparedTo == GC_TYPE_TITLE;
  case GC_TYPE_TITLE:
    return false;
  default:
    return false;
  }
}

bool GroupCell::IsHeading() const {
  switch (m_groupType) {
  case GC_TYPE_HEADING6:
  case GC_TYPE_HEADING5:
  case GC_TYPE_SUBSUBSECTION:
  case GC_TYPE_SUBSECTION:
  case GC_TYPE_SECTION:
  case GC_TYPE_TITLE:
    return true;
  default:
    return false;
  }
}

void GroupCell::Number(int &section, int &subsection, int &subsubsection,
                       int &heading5, int &heading6, int &image) const {
  for (auto const &tmp : OnList(this)) {
    switch (tmp.m_groupType) {
    case GC_TYPE_TITLE:
      section = subsection = subsubsection = heading5 = heading6 = 0;
      break;
    case GC_TYPE_SECTION:
      section++;
      subsection = subsubsection = heading5 = heading6 = 0;
      {
        wxString num = wxT("  ");
        num << section << wxT(" ");
        tmp.m_inputLabel->SetValue(num);
        tmp.m_inputLabel->SetStyle(TS_SECTION);
      }
      break;
    case GC_TYPE_SUBSECTION:
      subsubsection = heading5 = heading6 = 0;
      subsection++;
      {
        wxString num = wxT("  ");
        num << section << wxT(".") << subsection << wxT(" ");
        tmp.m_inputLabel->SetValue(num);
        tmp.m_inputLabel->SetStyle(TS_SUBSECTION);
      }
      break;
    case GC_TYPE_SUBSUBSECTION:
      heading5 = heading6 = 0;
      subsubsection++;
      {
        wxString num = wxT("  ");
        num << section << wxT(".") << subsection << wxT(".") << subsubsection
            << wxT(" ");
        tmp.m_inputLabel->SetValue(num);
        tmp.m_inputLabel->SetStyle(TS_SUBSUBSECTION);
      }
      break;
    case GC_TYPE_HEADING5:
      heading5++;
      heading6 = 0;
      {
        wxString num = wxT("  ");
        num << section << wxT(".") << subsection << wxT(".") << subsubsection
            << wxT(".") << heading5 << wxT(" ");
        tmp.m_inputLabel->SetValue(num);
        tmp.m_inputLabel->SetStyle(TS_HEADING5);
      }
      break;
    case GC_TYPE_HEADING6:
      heading6++;
      {
        wxString num = wxT("  ");
        num << section << wxT(".") << subsection << wxT(".") << subsubsection
            << wxT(".") << heading5 << wxT(".") << heading6 << wxT(" ");
        tmp.m_inputLabel->SetValue(num);
        tmp.m_inputLabel->SetStyle(TS_HEADING6);
      }
      break;
    case GC_TYPE_IMAGE:
      image++;
      {
        wxString num = wxString::Format(_("Figure %d:"), image);
        tmp.m_inputLabel->SetValue(num);
      }
      break;
    default:
      break;
    }

    if (IsFoldable() && tmp.m_hiddenTree)
      tmp.m_hiddenTree->Number(section, subsection, subsubsection, heading5,
                               heading6, image);
  }
}

bool GroupCell::IsMainInput(Cell *active) const {
  return active && active == m_inputLabel->GetNext();
}

bool GroupCell::Contains(GroupCell *cell) const {
  for (auto const &tmp : OnList(this)) {
    // If this is the cell we search for we can end the search.
    if (&tmp == cell)
      return true;

    // If this cell contains a hidden tree we have to search that at well.
    if ((tmp.IsFoldable()) && (tmp.GetHiddenTree()) != NULL) {
      if (tmp.GetHiddenTree()->Contains(cell))
        return true;
    }
  }

  return false;
}

#if wxUSE_ACCESSIBILITY
wxAccStatus GroupCell::GetDescription(int childId,
                                      wxString *description) const {
  if (description == NULL)
    return wxACC_FAIL;

  if (childId == 0) {
    if (m_groupType == GC_TYPE_PAGEBREAK) {
      *description = _("A page break");
      return wxACC_OK;
    } else {
      *description = _("A GroupCell that bundles input with its output");
      return wxACC_OK;
    }
  } else {
    Cell *childCell = nullptr;
    if (GetChild(childId, &childCell) == wxACC_OK) {
      return childCell->GetDescription(0, description);
    }
  }

  *description = wxEmptyString;
  return wxACC_FAIL;
}

wxAccStatus GroupCell::GetLocation(wxRect &rect, int elementId) {
  if (elementId == 0) {
    rect = wxRect(GetRect().GetTopLeft() +
		  m_configuration->GetVisibleRegion().GetTopLeft(),
                  GetRect().GetBottomRight() +
		  m_configuration->GetVisibleRegion().GetTopLeft());

    // Our GroupCell handles the hcaret below the cell, as well as its contents
    rect.SetBottom(rect.GetBottom() + m_configuration->GetGroupSkip());

    // If we are the 1st groupcell of the worksheet we handle the hcaret above
    // this cell, too.
    if (!GetPrevious())
      rect.SetTop(rect.GetTop() - m_configuration->GetGroupSkip());

    if (rect.GetTop() < 0)
      rect.SetTop(0);
    if (rect.GetLeft() < 0)
      rect.SetLeft(0);
    if (rect.GetBottom() > m_configuration->GetVisibleRegion().GetWidth())
      rect.SetBottom(m_configuration->GetVisibleRegion().GetWidth());
    if (rect.GetRight() > m_configuration->GetVisibleRegion().GetHeight())
      rect.SetRight(m_configuration->GetVisibleRegion().GetHeight());
    rect =
      wxRect(rect.GetTopLeft() + m_configuration->GetWorksheetPosition(),
	     rect.GetBottomRight() + m_configuration->GetWorksheetPosition());
    return wxACC_OK;
  } else {
    Cell *childCell = nullptr;
    if (GetChild(elementId, &childCell) == wxACC_OK)
      return childCell->GetLocation(rect, 0);
  }
  return wxACC_FAIL;
}

#endif

void CellList::Check(const GroupCell *c) {
  if (!c)
    return;
  wxASSERT_MSG(!c->m_next || dynamic_cast<GroupCell *>(c->m_next.get()),
               _("Bug: The successor to a GroupCell is not a GroupCell."));
  wxASSERT_MSG(!c->m_previous || dynamic_cast<GroupCell *>(c->m_previous),
               _("Bug: The predecessor to a GroupCell is not a GroupCell."));
  CellList::Check(static_cast<const Cell *>(c));
}

wxString GroupCell::m_lookalikeChars(
				     wxT("µ") wxT("\u03bc") wxT("\u2126") wxT("\u03a9") wxT("C") wxT(
												     "\u03F2") wxT("C") wxT("\u0421") wxT("\u03F2") wxT("\u0421") wxT("A")
				     wxT("\u0391") wxT("A") wxT("\u0410") wxT("\u0391") wxT("\u0410") wxT(
													  "M") wxT("\u0392") wxT("E") wxT("\u0395") wxT("E") wxT("\u0415")
				     wxT("\u0415") wxT("\u0395") wxT("Z") wxT("\u0396") wxT("H") wxT(
												     "\u0397") wxT("H") wxT("\u041D") wxT("\u0397") wxT("\u041D")
				     wxT("I") wxT("\u0399") wxT("I") wxT("\u0406") wxT("l") wxT(
												"\u0406") wxT("K") wxT("\u039A")
				     wxT("K") wxT("\u041A") wxT("\u039A") wxT("\u041A") wxT("\u212a") wxT(
													  "\u041A") wxT("K") wxT("\u212A") wxT("M") wxT("\u041c") wxT("\u039C")
				     wxT("\u041c") wxT("M") wxT("\u039C") wxT("N") wxT("\u039D") wxT("O") wxT(
													      "\u039F") wxT("O") wxT("\u041E") wxT("\u039F") wxT("\u041E") wxT("\u039F")
				     wxT("\u041E") wxT("P") wxT("\u03A1") wxT("X") wxT("\u0425") wxT("e") wxT(
													      "\u0435") wxT("p") wxT("\u0440") wxT("x") wxT("\u0445") wxT("y")
				     wxT("\u0443") wxT("P") wxT("\u0420") wxT("\u03A1") wxT(
											    "\u0420") wxT("T") wxT("\u03A4") wxT("T") wxT("\u0422") wxT("\u03A4")
				     wxT("\u0422") wxT("Y") wxT("\u03A5") wxT("\u212a") wxT(
											    "\u039A") wxT("l") wxT("I") wxT("B")
				     wxT("\u0392") wxT("S") wxT("\u0405") wxT("\u0392") wxT(
											    "\u0412") wxT("B") wxT("\u0412") wxT("J")
				     wxT("\u0408") wxT("a") wxT("\u0430") wxT(
									      "o") wxT("\u03bf") wxT("\u03a3") wxT("\u2211")
				     wxT("o") wxT("\u043e") wxT("\u03bf") wxT(
									      "\u043e") wxT("c") wxT("\u0441") wxT("s")
				     wxT("\u0455") wxT("t") wxT("\u03c4") wxT(
									      "u") wxT("\u03c5") wxT("x") wxT("\u03c7")
				     wxT("ü") wxT("\u03cb") wxT("\u0460") wxT(
									      "\u03c9") wxT("\u0472") wxT("\u0398")
				     wxT("У") wxT("Y") wxT("У") wxT(
								    "y") wxT("ѡ") wxT("ω")
				     wxT("Ѳ") wxT("Θ") wxT(
							   "θ") wxT("ѳ") wxT("ø")
				     wxT("⌀") wxT("∅") wxT(
							   "⊘") wxT("∅")
				     wxT("⌀") wxT(
						  "ø") wxT("⊘")
				     wxT("ø") wxT(
						  "∅")
				     wxT("⌀") wxT(
						  "⊘"));
