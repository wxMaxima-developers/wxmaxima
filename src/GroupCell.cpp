// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
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

#include <wx/config.h>
#include <wx/clipbrd.h>
#include "MarkDown.h"
#include "GroupCell.h"
#include "SlideShowCell.h"
#include "TextCell.h"
#include "ImgCell.h"
#include "BitmapOut.h"
#include "list"

GroupCell::GroupCell(Configuration **config, GroupType groupType, CellPointers *cellPointers, const wxString &initString) :
    Cell(this, config, cellPointers)
{
  m_nextToDraw = NULL;
  m_numberedAnswersCount = 0;
  m_autoAnswer = false;
  m_cellsInGroup = 1;
  m_inEvaluationQueue = false;
  m_lastInEvaluationQueue = false;
  m_labelWidth_cached = 0;
  m_hiddenTree = NULL;
  m_hiddenTreeParent = NULL;
  m_outputRect.x = -1;
  m_outputRect.y = -1;
  m_outputRect.width = 0;
  m_outputRect.height = 0;
  m_group = this;
  m_fontSize = (*m_configuration)->GetDefaultFontSize();
  m_mathFontSize = (*m_configuration)->GetMathFontSize();
  m_forceBreakLine = true;
  m_breakLine = true;
  m_type = MC_TYPE_GROUP;
  m_isHidden = false;
  m_groupType = groupType;
  m_lastInOutput = NULL;

  // set up cell depending on groupType, so we have a working cell
  if (groupType != GC_TYPE_PAGEBREAK)
  {
    if (groupType == GC_TYPE_CODE)
      m_inputLabel.reset(new TextCell(this, m_configuration, m_cellPointers, EMPTY_INPUT_LABEL));
    else
      m_inputLabel.reset(new TextCell(this, m_configuration, m_cellPointers, wxT("")));

    m_inputLabel->SetType(MC_TYPE_MAIN_PROMPT);
  }

  EditorCell *editor = {};

  switch (groupType)
  {
    case GC_TYPE_CODE:
      editor = new EditorCell(this, m_configuration, m_cellPointers);
      editor->SetType(MC_TYPE_INPUT);
      AppendInput(editor);
      break;
    case GC_TYPE_TEXT:
      m_inputLabel->SetType(MC_TYPE_TEXT);
      editor = new EditorCell(this, m_configuration, m_cellPointers);
      editor->SetType(MC_TYPE_TEXT);
      AppendInput(editor);
      break;
    case GC_TYPE_TITLE:
      m_inputLabel->SetType(MC_TYPE_TITLE);
      editor = new EditorCell(this, m_configuration, m_cellPointers);
      editor->SetType(MC_TYPE_TITLE);
      AppendInput(editor);
      break;
    case GC_TYPE_SECTION:
      m_inputLabel->SetType(MC_TYPE_SECTION);
      editor = new EditorCell(this, m_configuration, m_cellPointers);
      editor->SetType(MC_TYPE_SECTION);
      AppendInput(editor);
      break;
    case GC_TYPE_SUBSECTION:
      m_inputLabel->SetType(MC_TYPE_SUBSECTION);
      editor = new EditorCell(this, m_configuration, m_cellPointers);
      editor->SetType(MC_TYPE_SUBSECTION);
      AppendInput(editor);
      break;
    case GC_TYPE_SUBSUBSECTION:
      m_inputLabel->SetType(MC_TYPE_SUBSUBSECTION);
      editor = new EditorCell(this, m_configuration, m_cellPointers);
      editor->SetType(MC_TYPE_SUBSUBSECTION);
      AppendInput(editor);
      break;
    case GC_TYPE_HEADING5:
      m_inputLabel->SetType(MC_TYPE_HEADING5);
      editor = new EditorCell(this, m_configuration, m_cellPointers);
      editor->SetType(MC_TYPE_HEADING5);
      AppendInput(editor);
      break;
    case GC_TYPE_HEADING6:
      m_inputLabel->SetType(MC_TYPE_HEADING6);
      editor = new EditorCell(this, m_configuration, m_cellPointers);
      editor->SetType(MC_TYPE_HEADING6);
      AppendInput(editor);
      break;
    case GC_TYPE_IMAGE:
      m_inputLabel->SetType(MC_TYPE_TEXT);
      editor = new EditorCell(this, m_configuration, m_cellPointers);
      editor->SetType(MC_TYPE_TEXT);
      AppendInput(editor);
      break;
    default:
      break;
  }

  if (editor && !initString.empty())
    editor->SetValue(initString);

  // when creating an image cell, if a string is provided
  // it loads an image (without deleting it)
  if ((groupType == GC_TYPE_IMAGE) && (initString.Length() > 0))
  {
    std::shared_ptr <wxFileSystem> noFS;
    Cell *ic;
    if (wxImage::GetImageCount(initString) < 2)
      ic = new ImgCell(this, m_configuration, m_cellPointers, initString, noFS, false);
    else
      ic = new SlideShow(this, m_configuration, m_cellPointers, initString, false);
    GroupCell::AppendOutput(ic);
  }

  // The GroupCell this cell belongs to is this GroupCell.
  SetGroup(this);
}

GroupCell::GroupCell(const GroupCell &cell):
    GroupCell(cell.m_configuration, cell.m_groupType, cell.m_cellPointers)
{
  m_nextToDraw = NULL;
  CopyCommonData(cell);
  if (cell.m_inputLabel)
    SetInput(cell.m_inputLabel->CopyList());
  if (cell.m_output)
    SetOutput(cell.m_output->CopyList());
  Hide(cell.m_isHidden);
  AutoAnswer(cell.m_autoAnswer);
}

void GroupCell::SetCellStyle(int style)
{
  if (!GetEditable())
    return;
  
  switch (style)
  {
  case GC_TYPE_CODE:
    m_groupType = GC_TYPE_CODE;
    if(m_inputLabel != NULL)
      m_inputLabel->SetType(MC_TYPE_MAIN_PROMPT);
    if(GetEditable() != NULL)
      GetEditable()->SetType(MC_TYPE_INPUT);
    m_inputLabel -> SetValue(EMPTY_INPUT_LABEL);
    break;

  case GC_TYPE_TEXT:
    m_inputLabel -> SetValue(wxEmptyString);
    m_groupType = GC_TYPE_TEXT;
    if(m_inputLabel != NULL)
      m_inputLabel->SetType(MC_TYPE_TEXT);
    if(GetEditable() != NULL)
      GetEditable()->SetType(MC_TYPE_TEXT);
    RemoveOutput();
   break;
  case GC_TYPE_TITLE:
    m_inputLabel -> SetValue(wxEmptyString);
    m_groupType = GC_TYPE_TITLE;
    if(m_inputLabel != NULL)
      m_inputLabel->SetType(MC_TYPE_TITLE);
    if(GetEditable() != NULL)
      GetEditable()->SetType(MC_TYPE_TITLE);
    RemoveOutput();
    break;
  case GC_TYPE_SECTION:
    m_inputLabel -> SetValue(wxEmptyString);
    m_groupType = GC_TYPE_SECTION;
    if(m_inputLabel != NULL)
      m_inputLabel->SetType(MC_TYPE_SECTION);
    if(GetEditable() != NULL)
      GetEditable()->SetType(MC_TYPE_SECTION);
    RemoveOutput();
    break;
  case GC_TYPE_SUBSECTION:
    m_inputLabel -> SetValue(wxEmptyString);
    m_groupType = GC_TYPE_SUBSECTION;
    if(m_inputLabel != NULL)
      m_inputLabel->SetType(MC_TYPE_SUBSECTION);
    if(GetEditable() != NULL)
      GetEditable()->SetType(MC_TYPE_SUBSECTION);
    RemoveOutput();
    break;
  case GC_TYPE_SUBSUBSECTION:
    m_inputLabel -> SetValue(wxEmptyString);
    m_groupType = GC_TYPE_SUBSUBSECTION;
    if(m_inputLabel != NULL)
      m_inputLabel->SetType(MC_TYPE_SUBSUBSECTION);
    if(GetEditable() != NULL)
      GetEditable()->SetType(MC_TYPE_SUBSUBSECTION);
    RemoveOutput();
    break;
  case GC_TYPE_HEADING5:
    m_inputLabel -> SetValue(wxEmptyString);
    m_groupType = GC_TYPE_HEADING5;
    if(m_inputLabel != NULL)
      m_inputLabel->SetType(MC_TYPE_HEADING5);
    if(GetEditable() != NULL)
      GetEditable()->SetType(MC_TYPE_HEADING5);
    RemoveOutput();
    break;
  case GC_TYPE_HEADING6:
    m_inputLabel -> SetValue(wxEmptyString);
    m_groupType = GC_TYPE_HEADING6;
    if(m_inputLabel != NULL)
      m_inputLabel->SetType(MC_TYPE_HEADING6);
    if(GetEditable() != NULL)
      GetEditable()->SetType(MC_TYPE_HEADING6);
    RemoveOutput();
    break;
  }
  if(GetEditable() != NULL)
    GetEditable()->StyleText();
}

/*! Set the parent of this group cell

*/
void GroupCell::SetGroup(Cell *parent)
{  
  //m_group = parent;
  if (m_inputLabel != NULL)
    m_inputLabel->SetGroupList(parent);

  Cell *tmp = m_output.get();
  if(m_output != NULL)
    tmp->SetGroupList(parent);
}

bool GroupCell::Empty()
{
  return (
          // No next cell
          (m_next == NULL) &&
          // This cell at maximum contains a prompt.
          (ToString().Length() < 6)
  );
}

void GroupCell::ResetInputLabel()
{
  if (m_groupType == GC_TYPE_CODE)
  {
    if (m_inputLabel)
      m_inputLabel->SetValue(EMPTY_INPUT_LABEL);
  }
}

void GroupCell::ResetInputLabelList()
{
  GroupCell *tmp = this;
  while (tmp)
  {
    tmp->ResetInputLabel();
    // also reset input labels in the folded cells
    if (tmp->IsFoldable() && (tmp->m_hiddenTree))
      tmp->m_hiddenTree->ResetInputLabelList();

    tmp = tmp->GetNext();
  }
}

GroupCell::~GroupCell()
{
  GroupCell::MarkAsDeleted();
  wxDELETE(m_hiddenTree);
  m_hiddenTree = NULL;
}

void GroupCell::MarkAsDeleted()
{
  if(this == m_cellPointers->m_selectionStart)
    m_cellPointers->m_selectionStart = NULL;
  if(this == m_cellPointers->m_selectionEnd)
    m_cellPointers->m_selectionEnd = NULL;
  if((m_cellPointers->m_answerCell) &&(m_cellPointers->m_answerCell->GetGroup() == this))
    m_cellPointers->m_answerCell = NULL;
  m_cellPointers->m_errorList.Remove(this);
  if (this == m_cellPointers->m_workingGroup)
    m_cellPointers->m_workingGroup = NULL;
  if (this == m_cellPointers->m_lastWorkingGroup)
    m_cellPointers->m_lastWorkingGroup = NULL;
  if (this == m_cellPointers->m_groupCellUnderPointer)
    m_cellPointers->m_groupCellUnderPointer = NULL;

  Cell::MarkAsDeleted();
}

wxString GroupCell::TexEscapeOutputCell(wxString Input)
{
  wxString retval(Input);
  Input.Replace(wxT("#"), wxT("\\#"));
  return (Input);
}

void GroupCell::SetInput(Cell *input)
{
  if (!input)
    return;
  m_inputLabel.reset(input);
  m_inputLabel->SetGroup(this);
}

void GroupCell::AppendInput(Cell *cell)
{
  if (!m_inputLabel)
  {
    m_inputLabel.reset(cell);
  }
  else
  {
    if (m_inputLabel->m_next == NULL)
      m_inputLabel->AppendCell(cell);
    else if (m_inputLabel->m_next->GetValue().Length() == 0)
    {
      wxDELETE(m_inputLabel->m_next);
      m_inputLabel->m_next = NULL;
      m_inputLabel->SetNextToDraw(NULL);
      m_inputLabel->AppendCell(cell);
    }
    else
    {
      AppendOutput(cell);
      m_isHidden = false;
    }
  }
}


void GroupCell::SetOutput(Cell *output)
{
  if((m_cellPointers->m_answerCell) &&(m_cellPointers->m_answerCell->GetGroup() == this))
    m_cellPointers->m_answerCell = NULL;
  
  m_output.reset(output);

  m_lastInOutput = m_output.get();

  m_outputHeight = -1;
  if(m_output != NULL)
  {
    m_output->SetGroup(this);
    while (m_lastInOutput->m_next != NULL)
      m_lastInOutput = m_lastInOutput->m_next;
    m_output->ResetSizeList();
  }
  UpdateCellsInGroup();
  UpdateConfusableCharWarnings();
  ResetSize();
  ResetData();
  GroupCell::Recalculate();
}

void GroupCell::RemoveOutput()
{
  m_numberedAnswersCount = 0;
  if (m_output == NULL)
    return;
  // If there is nothing to do we can skip the rest of this action.

  if((m_cellPointers->m_answerCell) &&(m_cellPointers->m_answerCell->GetGroup() == this))
    m_cellPointers->m_answerCell = NULL;

  if (GetGroupType() != GC_TYPE_IMAGE)
    m_output.reset();

  m_cellPointers->m_errorList.Remove(this);
  // Calculate the new cell height.

  m_isHidden = false;

  // ResetSize();
  // ResetData();

  m_outputHeight = 0;
  m_height = m_inputHeight;
  if(m_inputLabel)
    m_width = m_inputLabel->GetFullWidth();
  else
    m_width = 50;
  (*m_configuration)->AdjustWorksheetSize(true);
  
  // Move all cells that follow the current one up by the amount this cell has shrinked.
  GroupCell *cell = this;
  while(cell != NULL)
    cell = cell->UpdateYPosition();
  UpdateCellsInGroup();
  UpdateConfusableCharWarnings();
}

void GroupCell::AppendOutput(Cell *cell)
{
  wxASSERT_MSG(cell != NULL, _("Bug: Trying to append NULL to a group cell."));
  if (cell == NULL) return;
  cell->SetGroupList(this);
  if (!m_output)
  {
    m_output.reset(cell);

    if (m_groupType == GC_TYPE_CODE && m_inputLabel->m_next != NULL)
      (dynamic_cast<EditorCell *>(m_inputLabel->m_next))->ContainsChanges(false);

    m_lastInOutput = m_output.get();

    while (m_lastInOutput->m_next != NULL)
      m_lastInOutput = m_lastInOutput->m_next;
  }

  else
  {
    Cell *tmp = m_lastInOutput;
    if (tmp == NULL)
      tmp = m_output.get();

    while (tmp->m_next != NULL)
      tmp = tmp->m_next;

    tmp->AppendCell(cell);

    if(m_lastInOutput != NULL)
      while (m_lastInOutput->m_next != NULL)
        m_lastInOutput = m_lastInOutput->m_next;
  }
  m_output->ResetSize();
  m_output->ResetSize();
  m_outputHeight = -1;
  ResetSize();
  ResetData();
  GroupCell::Recalculate();
  UpdateCellsInGroup();
  UpdateConfusableCharWarnings();
}

void GroupCell::UpdateConfusableCharWarnings()
{
  ClearToolTip();

  wxString code;
  if(GetInput())
    code += GetInput()->ListToString() + " ";
  if(GetOutput())
    code += GetOutput()->VariablesAndFunctionsList();
  // Extract all variable and command names from the cell including input and output
  CmdsAndVariables cmdsAndVariables;

  for (auto const &tok : MaximaTokenizer(code, *m_configuration).PopTokens())
    if((tok.GetStyle() == TS_CODE_VARIABLE) || (tok.GetStyle() == TS_CODE_FUNCTION))
      cmdsAndVariables[tok.GetText()] = 1;
  
  // Now we step through all the words we found
  while(!cmdsAndVariables.empty())
  {
    CmdsAndVariables::iterator cmp = cmdsAndVariables.begin();
    wxString word;
    word = cmp->first;
    cmdsAndVariables.erase(cmp);
    // Now iterate through all remaining words
    cmp = cmdsAndVariables.begin();
    while(cmp != cmdsAndVariables.end())
    {
      // iterate through all lookalike chars
      for (wxString::const_iterator it = m_lookalikeChars.begin(); it < m_lookalikeChars.end(); ++it)
      {
        wxChar ch1 = *it;
        ++it;
        wxASSERT(it < m_lookalikeChars.end());
        wxChar ch2 = *it;
        wxString word_subst = word;
        if(word_subst.Replace(ch1,ch2))
        {
          if(cmp->first == word_subst)
            AddToolTip(_("Warning: Lookalike chars: ") +
                       cmp->first.utf8_str() + wxT("\u2260") +
                       word.utf8_str()
              );
        }
        word_subst = word;
        if(word_subst.Replace(ch2,ch1))
        {
          if(cmp->first == word_subst)
            AddToolTip(_("Warning: Lookalike chars: ") +
                       cmp->first +
                       wxT(" \u2260 ") +
                       word
              );
        }
          
      }
      ++cmp;
    }
  }
}

void GroupCell::Recalculate()
{
  m_fontSize = (*m_configuration)->GetDefaultFontSize();
  m_mathFontSize = (*m_configuration)->GetMathFontSize();
  GroupCell::RecalculateWidths((*m_configuration)->GetDefaultFontSize());
  GroupCell::RecalculateHeight((*m_configuration)->GetDefaultFontSize());
}

void GroupCell::RecalculateWidths(int fontsize)
{
  Configuration *configuration = (*m_configuration);
  
  if (NeedsRecalculation(fontsize))
  {
    // special case of 'line cell'
    if (m_groupType == GC_TYPE_PAGEBREAK)
    {
      m_width = configuration->GetCellBracketWidth();
      m_height = 2;
      ResetData();
      return;
    }
    
    if(m_inputLabel != NULL)
    {
      m_inputLabel->RecalculateWidthsList(fontsize);
      m_inputLabel->SetCurrentPoint(m_currentPoint);
    }

    if (m_output == NULL || m_isHidden)
    {
      if ((configuration->ShowCodeCells()) ||
          (m_groupType != GC_TYPE_CODE))
      {
        m_width = GetInputIndent();
        if(GetInput())
          m_width += GetInput()->GetWidth();
      }  
    }
    else
    {
      if ((configuration->ShowCodeCells()) ||
          (m_groupType != GC_TYPE_CODE))
      {
        m_width = Scale_Px(100);
        if(GetInput() != NULL)
          m_width = GetInput()->GetFullWidth() + GetInputIndent();
        else
        {
            m_width = GetInputIndent();
        }
      }
    }
    BreakLines();
    ResetData();
  }
  Cell::RecalculateWidths(fontsize);
}

void GroupCell::InputHeightChanged()
{
//  ResetData();
  ResetSize();
  EditorCell *editorCell = GetEditable();
  if (editorCell != NULL) {
    editorCell->ResetSize();
    editorCell->RecalculateWidths((*m_configuration)->GetDefaultFontSize());
  }
  if (m_inputLabel != NULL) {
    m_inputLabel->ResetData();
  }
  RecalculateHeightInput();

  if(m_output != NULL)
  {
    m_height += m_outputRect.height;
    m_outputRect.y = m_currentPoint.y + m_center;
  }
}

// Called on resize events
// We need to forget line breaks/breakup cells and
// breakup cells and compute new line breaks
void GroupCell::OnSize()
{
  // Unbreakup cells
  Cell *tmp = m_output.get();
  while (tmp != NULL)
  {
    tmp->Unbreak();
    tmp->SoftLineBreak(false);
    tmp->ResetData();
    tmp = tmp->m_next;
  }
  BreakLines();
  InputHeightChanged();
}

void GroupCell::RecalculateHeightInput()
{
  Configuration *configuration = (*m_configuration);

  if (m_inputLabel)
    m_inputLabel->SetCurrentPoint(m_currentPoint);
  if (GetEditable())
  {
    wxPoint in = GetCurrentPoint();
    
    in.x += GetInputIndent();
    GetEditable()->SetCurrentPoint(GetCurrentPoint());
  }
  
  // special case
  if (m_groupType == GC_TYPE_PAGEBREAK)
  {
    m_width = configuration->GetCellBracketWidth();
    m_height = 2;
    m_center = 0;
    Cell::RecalculateWidthsList((*m_configuration)->GetDefaultFontSize());
    return;
  }
  
  if ((configuration->ShowCodeCells()) ||
      (m_groupType != GC_TYPE_CODE))
  {
    if(m_inputLabel)
    {
      m_inputLabel->RecalculateHeightList((*m_configuration)->GetDefaultFontSize());
      m_center = m_inputLabel->GetCenterList();
      m_height = m_inputLabel->GetHeightList();
    }
  }
  else
  {
    m_center = 0;
    m_height = 0;
  }
  
  if (!m_isHidden)
  {
    Cell *tmp = m_output.get();
    while (tmp != NULL)
    {
      tmp->RecalculateHeight(tmp->IsMath() ?
                             (*m_configuration)->GetMathFontSize() :
                             (*m_configuration)->GetDefaultFontSize());
      tmp = tmp->m_next;
    }
  }
  
  m_currentPoint.x = configuration->GetIndent();
  if (m_previous == NULL)
  {
    m_currentPoint.y = (*m_configuration)->GetBaseIndent() + GetCenterList();
  }
  else
  {
    if(dynamic_cast<GroupCell *>(m_previous)->m_currentPoint.y > 0)
      m_currentPoint.y = dynamic_cast<GroupCell *>(m_previous)->m_currentPoint.y +
        dynamic_cast<GroupCell *>(m_previous)->GetMaxDrop() + GetCenterList() +
        (*m_configuration)->GetGroupSkip();
  }
  
  m_outputRect.x = m_currentPoint.x;
  m_outputRect.y = m_currentPoint.y + m_center;
  if (m_output) m_outputRect.y -= m_output->GetCenterList();
  else
  {
    m_outputRect.width = 0;
    m_outputRect.height = 0;
  }
  if ((configuration->ShowCodeCells()) ||
      (m_groupType != GC_TYPE_CODE))
  {
    m_height = m_inputLabel->GetHeightList();
    m_width = m_inputLabel->GetFullWidth();
  }
  else
  {
    m_height = 0;
    m_width = 0;
  }
  
  m_inputHeight = m_height;
  m_inputWidth = m_width;
}

void GroupCell::RecalculateHeightOutput()
{
  if(m_isHidden)
    return;

  if(m_output == NULL)
    return;
  
  Configuration *configuration = (*m_configuration);
    
  if(m_height < 0)
  {
    m_fontSize = configuration->GetDefaultFontSize();
    m_mathFontSize = (*m_configuration)->GetMathFontSize();
    
    RecalculateWidths(configuration->GetDefaultFontSize());
    RecalculateHeightInput();
  }
  m_output->HardLineBreak();

  Cell *tmp = m_output.get();
  m_mathFontSize = configuration->GetMathFontSize();

  // Recalculate widths of cells
  while (tmp != NULL)
  {
    tmp->RecalculateWidths(tmp->IsMath() ?
                           (*m_configuration)->GetMathFontSize() :
                           (*m_configuration)->GetDefaultFontSize());
    tmp = tmp->m_next;
  }

  // Breakup cells and break lines
  BreakLines(m_output.get());

  // Recalculate size of cells
  tmp = m_output.get();
  while (tmp != NULL)
  {
    tmp->RecalculateHeight(tmp->IsMath() ?
                           (*m_configuration)->GetMathFontSize() :
                           (*m_configuration)->GetDefaultFontSize());
    tmp->ResetData();
    tmp = tmp->m_next;
  }

  // Update heights
  tmp = m_output.get();
  tmp->ForceBreakLine(true);
  while (tmp != NULL)
  {
    if (tmp->BreakLineHere())
    {
      int height_Delta = tmp->GetHeightList();
      m_width = wxMax(m_width, tmp->GetLineWidth());
      m_height            += height_Delta;
      m_outputRect.width = m_width;
      m_outputRect.height += height_Delta;
      
      if (tmp->m_previous != NULL &&
          ((tmp->GetStyle() == TS_LABEL) || (tmp->GetStyle() == TS_USERLABEL)))
      {
        m_height            += configuration->GetInterEquationSkip();
        m_outputRect.height += configuration->GetInterEquationSkip();
      }

      if (tmp->m_bigSkip)
      {
        m_height            += MC_LINE_SKIP;
        m_outputRect.height += MC_LINE_SKIP;
      }
    }
    tmp = tmp->GetNextToDraw();
  }

  ResetData();
  
  // Move all cells that follow the current one down by the amount this cell has grown.
  GroupCell *cell = this;
  while(cell != NULL)
    cell = cell->UpdateYPosition();
  (*m_configuration)->AdjustWorksheetSize(true);
}

bool GroupCell::NeedsRecalculation(int fontSize)
{
  return Cell::NeedsRecalculation(fontSize) ||
    ((GetInput() != NULL) &&
     ((GetInput()->GetWidth() <= 0) || (GetInput()->GetHeight() <= 0) ||
      (GetInput()->GetCurrentPoint().x <= 0) || (GetInput()->GetCurrentPoint().y <= 0)
       ));
}

void GroupCell::RecalculateHeight(int fontsize)
{
  if(NeedsRecalculation(fontsize))
  {
    m_outputRect.SetHeight(0);
    RecalculateHeightInput();   
    RecalculateHeightOutput();
  }

//  if (((m_height <= 0) || (m_next == NULL)) && (m_height < configuration->GetCellBracketWidth()))
//    m_height = configuration->GetCellBracketWidth();
  
  Cell::RecalculateHeight(fontsize);
  UpdateYPosition();
}

GroupCell *GroupCell::UpdateYPosition()
{
  Configuration *configuration = (*m_configuration);
  
  if (m_previous == NULL)
  {
    m_currentPoint.x = configuration->GetIndent();
    if(m_center < 0)
      Recalculate();
    m_currentPoint.y = configuration->GetBaseIndent() + GetCenter();
  }
  else
  {
    m_currentPoint.x = configuration->GetIndent();
    wxASSERT(m_previous->GetCurrentPoint().y > 0);
    if(dynamic_cast<GroupCell *>(m_previous)->m_height > 0)
      m_currentPoint.y = dynamic_cast<GroupCell *>(m_previous)->GetCurrentPoint().y +
        dynamic_cast<GroupCell *>(m_previous)->GetMaxDrop() + GetCenterList() +
        configuration->GetGroupSkip();
    else
      m_currentPoint.y = dynamic_cast<GroupCell *>(m_previous)->m_currentPoint.y;
  }
  return GetNext();
}

int GroupCell::GetInputIndent()
{
  int labelWidth = 0;
  if((*m_configuration)->IndentMaths())
    labelWidth = Scale_Px((*m_configuration)->GetLabelWidth()) + MC_TEXT_PADDING;
  
  if(m_inputLabel != NULL)
  {
    if(m_inputLabel->GetWidth() >= 0)
      labelWidth = wxMax(m_inputLabel->GetWidth() + MC_TEXT_PADDING,labelWidth);
    else
      labelWidth = wxMax(m_labelWidth_cached,labelWidth);
  }
  
  m_labelWidth_cached = labelWidth;
    
  return labelWidth;
}

void GroupCell::Draw(wxPoint point)
{
  if ((m_width < 1 || m_height < 1) ||
      ((GetEditable() != NULL) &&
     (
       ((GetEditable()->GetHeight() < 1) ||
        (GetEditable()->GetWidth() < 1)
         ))))
    Recalculate();
  Cell::Draw(point);

  Configuration *configuration = (*m_configuration);

  if (configuration->ShowBrackets())
    DrawBracket();

  if (DrawThisCell(point))
  {
    wxDC *dc = configuration->GetDC();
    // draw a thick line for 'page break'
    // and return
    if (m_groupType == GC_TYPE_PAGEBREAK)
    {
      wxRect rect = GetRect(false);
      int y = rect.GetY();
      wxPen pen(configuration->GetColor(TS_CURSOR), 1, wxPENSTYLE_DOT);
      dc->SetPen(pen);
      dc->DrawLine(0, y, (*m_configuration)->GetCanvasSize().GetWidth(), y);
      return;
    }
    
    wxRect rect = GetRect(false);
    
    if(configuration->GetIndent() < rect.GetRight())
    {
      if(rect.GetLeft() <= configuration->GetCellBracketWidth())
        rect.SetLeft(configuration->GetIndent());
      
      //
      // Draw input and output
      //
      SetPen();
      wxPoint in(point);

      if ((m_output != NULL) && !m_isHidden)
      {
        Cell *tmp = m_output.get();
        int drop = tmp->GetMaxDrop();
        if ((configuration->ShowCodeCells()) ||
            (m_groupType != GC_TYPE_CODE))
          in.y += m_inputLabel->GetMaxDrop();

        in.y += m_output->GetCenterList();
        m_outputRect.y = in.y - m_output->GetCenterList();
        m_outputRect.x = in.x;

        in.x += GetLineIndent(tmp);
        while (tmp != NULL)
        {         
          tmp->Draw(in);
          if ((tmp->GetNextToDraw() != NULL) && (tmp->GetNextToDraw()->BreakLineHere()))
            {
              if (tmp->GetNextToDraw()->m_bigSkip)
                in.y += MC_LINE_SKIP;
 
              in.x = point.x + GetLineIndent(tmp->GetNextToDraw());              

              in.y += drop + tmp->GetNextToDraw()->GetCenterList();
              drop = tmp->GetNextToDraw()->GetMaxDrop();
            }
          else
            in.x += tmp->GetWidth();

          tmp = tmp->GetNextToDraw();
        }
      }
      if ((configuration->ShowCodeCells()) ||
          (m_groupType != GC_TYPE_CODE))
      {
        configuration->Outdated(false);

        EditorCell *input = GetInput();
        if(input)
        {
          in = point;
          input->Draw(
            wxPoint(
              in.x + GetInputIndent(),
              in.y
              )
            );
        }
        
        if(GetPrompt() != NULL)
          GetPrompt()->Draw(point);
        
        if (m_groupType == GC_TYPE_CODE && m_inputLabel->m_next)
          configuration->Outdated((dynamic_cast<EditorCell *>(m_inputLabel->m_next))->ContainsChanges());
      }
    }
    configuration->Outdated(false); 
    UnsetPen();
  }
}

wxRect GroupCell::GetRect(bool WXUNUSED(all))
{
  return wxRect(m_currentPoint.x, m_currentPoint.y - m_center,
                m_width, m_height);
}

int GroupCell::GetLineIndent(Cell *cell)
{
  int indent = 0;

  if(cell != NULL)
  {
    if(
      (cell->GetStyle() != TS_LABEL) &&
      (cell->GetStyle() != TS_USERLABEL) &&
      (cell->GetStyle() != TS_MAIN_PROMPT) &&
      (cell->GetStyle() != TS_OTHER_PROMPT) &&
      (*m_configuration)->IndentMaths()
      )
      indent += Scale_Px((*m_configuration)->GetLabelWidth()) + 2 * MC_TEXT_PADDING;
  }
  return indent;
}

void GroupCell::CellUnderPointer(GroupCell *cell)
{
  m_cellPointers->m_groupCellUnderPointer = cell;
}

void GroupCell::DrawBracket()
{
  // If the current cell doesn't know where it is on the screen we don't
  // attempt to draw it's bracket.
  if((GetRect().GetLeft() < 0) || (GetRect().GetTop() < 0))
    return;

  Configuration *configuration = (*m_configuration);
  bool drawBracket = !configuration->HideBrackets();

  if (this == m_cellPointers->m_groupCellUnderPointer)
    drawBracket = true;

  wxDC *dc = configuration->GetDC();
  wxDC *adc = configuration->GetAntialiassingDC();

  int selectionStart_px = -1;
  if((m_cellPointers->m_selectionStart != NULL) &&
     (m_cellPointers->m_selectionStart->GetType() == MC_TYPE_GROUP))
    selectionStart_px = dynamic_cast<GroupCell *>(m_cellPointers->m_selectionStart)->m_currentPoint.y;

  int selectionEnd_px = -1;
  if((m_cellPointers->m_selectionEnd != NULL) &&
     (m_cellPointers->m_selectionEnd->GetType() == MC_TYPE_GROUP))
    selectionEnd_px = dynamic_cast<GroupCell *>(m_cellPointers->m_selectionEnd)->m_currentPoint.y;
  
  // Mark this GroupCell as selected if it is selected. Else clear the space we
  // will add brackets in
  if ((m_currentPoint.y >= selectionStart_px) &&
      (m_currentPoint.y <= selectionEnd_px))
  {
    dc->SetPen(*(wxThePenList->FindOrCreatePen(
                  configuration->GetColor(TS_SELECTION),
                  configuration->GetDefaultLineWidth(),
                  wxPENSTYLE_SOLID)
                ));
// window linux, set a pen
    dc->SetBrush(*(wxTheBrushList->FindOrCreateBrush(configuration->GetColor(TS_SELECTION))));
    drawBracket = true;
  }
  else if (m_cellPointers->m_errorList.Contains(this))
  {
    dc->SetPen(*wxRED_PEN);
    dc->SetBrush(*wxRED_BRUSH);
    drawBracket = true;
  }
  else
  {
    if ((m_cellPointers->m_answerCell) && (m_cellPointers->m_answerCell->GetGroup() == this))
    {
      dc->SetPen(*wxYELLOW_PEN);
      dc->SetBrush(*wxYELLOW_BRUSH);
      drawBracket = true;
    }
    else
    {
      dc->SetBrush((*m_configuration)->GetBackgroundBrush());
      dc->SetPen(*(wxThePenList->FindOrCreatePen(
                     configuration->DefaultBackgroundColor(),
                     configuration->GetDefaultLineWidth(),
                     wxPENSTYLE_SOLID)
                   ));
    }
  }
  wxRect rect = GetRect();
  rect = wxRect(
          configuration->GetIndent() - configuration->GetCellBracketWidth(),
          rect.GetTop() - 2,
          configuration->GetCellBracketWidth(),
          rect.GetHeight() + 5
  );
  if (Cell::InUpdateRegion(rect))
    dc->DrawRectangle(Cell::CropToUpdateRegion(rect));

  //
  // Mark groupcells currently in queue.
  //
  if (m_inEvaluationQueue)
  {
    drawBracket = true;
    dc->SetBrush(*wxTRANSPARENT_BRUSH);
    if (m_lastInEvaluationQueue)
      dc->SetPen(*(wxThePenList->FindOrCreatePen(
              configuration->GetColor(TS_CELL_BRACKET),
              2 * configuration->GetDefaultLineWidth(),
              wxPENSTYLE_SOLID)));
    else
      dc->SetPen(*(wxThePenList->FindOrCreatePen(configuration->GetColor(TS_CELL_BRACKET),
                                                configuration->GetDefaultLineWidth(),
                                                wxPENSTYLE_SOLID)));

    wxRect bracketRect = wxRect(
      configuration->GetIndent() - configuration->GetCellBracketWidth(),
      rect.GetTop() - 2,
      configuration->GetCellBracketWidth(),
      rect.GetHeight() + 5);
    if (Cell::InUpdateRegion(bracketRect))
      dc->DrawRectangle(bracketRect);
  }

  Cell *editable = GetEditable();
  if (editable != NULL && editable->IsActive())
  {
    drawBracket = true;
    adc->SetPen(*(wxThePenList->FindOrCreatePen(configuration->GetColor(TS_ACTIVE_CELL_BRACKET),
                                              2 * configuration->GetDefaultLineWidth(),
                                              wxPENSTYLE_SOLID))); // window linux, set a pen
    dc->SetBrush(*(wxTheBrushList->FindOrCreateBrush(configuration->GetColor(TS_ACTIVE_CELL_BRACKET)))); //highlight c.
  }
  else
  {
    adc->SetPen(*(wxThePenList->FindOrCreatePen(configuration->GetColor(TS_CELL_BRACKET),
                                              configuration->GetDefaultLineWidth(),
                                              wxPENSTYLE_SOLID))); // window linux, set a pen
    dc->SetBrush(*(wxTheBrushList->FindOrCreateBrush(configuration->GetColor(TS_CELL_BRACKET)))); //highlight c.
  }

  if ((!m_isHidden) && (!m_hiddenTree))
  {
    dc->SetBrush(*wxTRANSPARENT_BRUSH);
  }

  if (drawBracket)
  {
    adc->SetBrush(dc->GetBrush());
    SetPen(1.5);
    int bracketWidth = configuration->GetCellBracketWidth() - configuration->GetDefaultLineWidth();
    if (IsFoldable())
    { // draw the square that allows hiding and unhiding the cell
      wxPointList points;
      points.DeleteContents(true);
      points.Append(
        new wxPoint(
          m_currentPoint.x - bracketWidth,
          m_currentPoint.y - m_center
          )
        );
      points.Append(
        new wxPoint(
          m_currentPoint.x - bracketWidth,
          m_currentPoint.y - m_center + bracketWidth
          )
        );
      points.Append(
        new wxPoint(
          m_currentPoint.x - configuration->GetDefaultLineWidth(),
          m_currentPoint.y - m_center + bracketWidth
          )
        );
      points.Append(
        new wxPoint(
          m_currentPoint.x - configuration->GetDefaultLineWidth(),
          m_currentPoint.y - m_center
          )
        );
      adc->DrawPolygon(&points);
    }
    else
    { 
      wxPointList points;
      points.DeleteContents(true);
      // draw the triangle that allows hiding and unhiding the cell
      points.Append(
        new wxPoint(
          m_currentPoint.x - bracketWidth + configuration->GetDefaultLineWidth() / 2,
          m_currentPoint.y - m_center + bracketWidth - configuration->GetDefaultLineWidth() / 2
          )
        );
      points.Append(
        new wxPoint(
          m_currentPoint.x - configuration->GetDefaultLineWidth(),
          m_currentPoint.y - m_center + configuration->GetDefaultLineWidth() / 2
          )
        );
      points.Append(
        new wxPoint(
          m_currentPoint.x - bracketWidth + configuration->GetDefaultLineWidth() / 2,
          m_currentPoint.y - m_center + configuration->GetDefaultLineWidth() / 2
          )
        );

      // The rest of the bracket
      if (configuration->ShowCodeCells() && m_groupType == GC_TYPE_CODE && m_output != NULL && !m_isHidden)
      {
        points.Append(
          new wxPoint(
            m_currentPoint.x - bracketWidth + configuration->GetDefaultLineWidth() / 2,
            m_currentPoint.y - m_center + m_inputLabel->GetHeightList()
            )
          );
        points.Append(
          new wxPoint(
            m_currentPoint.x - bracketWidth / 2 + configuration->GetDefaultLineWidth() / 2,
            m_currentPoint.y - m_center + m_inputLabel->GetHeightList()
            )
          );
        points.Append(
          new wxPoint(
            m_currentPoint.x - bracketWidth + configuration->GetDefaultLineWidth() / 2,
            m_currentPoint.y - m_center + m_inputLabel->GetHeightList()
            )
          );
      }
      
      // The remaining part of the vertical line at the back
      points.Append(
        new wxPoint(
          m_currentPoint.x - bracketWidth + configuration->GetDefaultLineWidth() / 2,
          m_currentPoint.y - m_center + m_height - configuration->GetDefaultLineWidth()
          )
        );
      // The horizontal line at the bottom
      points.Append(
        new wxPoint(
          m_currentPoint.x - configuration->GetDefaultLineWidth(),
          m_currentPoint.y - m_center + m_height - configuration->GetDefaultLineWidth()
          )
        );
      points.Append(
        new wxPoint(
          m_currentPoint.x - bracketWidth + configuration->GetDefaultLineWidth() / 2,
          m_currentPoint.y - m_center + m_height - configuration->GetDefaultLineWidth()
          )
        );
        
      adc->DrawPolygon(&points);

    }
  }
}

wxRect GroupCell::HideRect()
{
  Configuration *configuration = (*m_configuration);
  return wxRect(m_currentPoint.x - configuration->GetCellBracketWidth() - configuration->GetDefaultLineWidth() / 2,
                m_currentPoint.y - m_center - configuration->GetDefaultLineWidth() / 2,
                configuration->GetCellBracketWidth() + configuration->GetDefaultLineWidth(),
                configuration->GetCellBracketWidth() + configuration->GetDefaultLineWidth()
  );
}

wxString GroupCell::ToString()
{
  wxString str;
  Configuration *configuration = (*m_configuration);

  if (m_inputLabel != NULL)
  {
    if ((configuration->ShowCodeCells()) ||
        (m_groupType != GC_TYPE_CODE))
    {
      str = m_inputLabel->ToString();

      if (GetEditable() != NULL)
        str += GetEditable()->ToString();

      str.Replace(wxT("\n"), wxT("\n\t"));
    }
  }

  if (m_output != NULL && !m_isHidden)
  {
    Cell *tmp = m_output.get();
    bool firstCell = true;
    while (tmp != NULL)
    {
      if (firstCell || (tmp->HardLineBreak() && str.Length() > 0))
          str += wxT("\n");
      str += tmp->ToString();
      firstCell = false;
      tmp = tmp->GetNextToDraw();
    }
  }
  return str;
}

wxString GroupCell::ToTeX()
{
  return ToTeX(wxEmptyString, wxEmptyString, NULL);
}

wxString GroupCell::ToRTF()
{
  Configuration *configuration = (*m_configuration);
  if (m_groupType == GC_TYPE_PAGEBREAK)
    return (wxT("\\page "));

  wxString retval;
  if (m_groupType == GC_TYPE_CODE)
  {
    if (m_inputLabel != NULL &&
        (
                (configuration->ShowCodeCells()) ||
                (m_groupType != GC_TYPE_CODE)
        )
            )
    {
      if (m_previous != NULL)
        retval = wxT("\\par}{\\pard\\s22\\li1105\\lin1105\\fi-1105\\f0\\fs24 \n");
      else
        retval += wxT("\\pard\\s22\\li1105\\lin1105\\fi-1105\\f0\\fs24 ");
      retval += RTFescape(m_inputLabel->ToString());
      retval += wxT("\\tab\n");
    }
    else
    {
      if (m_previous != NULL)
        retval = wxT("\\par}\n{\\pard\\s21\\li1105\\lin1105\\f0\\fs24 ");
      else
        retval = wxT("\\pard\\s21\\li1105\\lin1105\\f0\\fs24 ");
    }
  }
  else
    retval = wxT("\\par}\n{");

  if (GetEditable() != NULL)
    retval += GetEditable()->ToRTF();

  Cell *out = GetLabel();
  if (out != NULL)
  {
    retval += out->ListToRTF(true);
  }
  return retval;
}

wxString GroupCell::ToTeX(wxString imgDir, wxString filename, int *imgCounter)
{
  wxASSERT_MSG((imgCounter != NULL), _(wxT("Bug: No image counter to write to!")));
  if (imgCounter == NULL) return wxEmptyString;
  wxString str;
  switch (m_groupType)
  {
    case GC_TYPE_PAGEBREAK:
      str = wxT("\\pagebreak\n");
      break;

    case GC_TYPE_IMAGE:
      if (imgDir != wxEmptyString)
      {
        Cell *copy = m_output->Copy();
        (*imgCounter)++;
        wxString image = filename + wxString::Format(wxT("_%d"), *imgCounter);
        wxString file = imgDir + wxT("/") + image + wxT(".") + dynamic_cast<ImgCell *>(copy)->GetExtension();

        if (!wxDirExists(imgDir))
          wxMkdir(imgDir);

        if (dynamic_cast<ImgCell *>(copy)->ToImageFile(file).x >= 0)
        {
          str << wxT("\\begin{figure}[htb]\n")
              << wxT("  \\centering\n")
              << wxT("    \\includeimage{")
              << filename << wxT("_img/") << image << wxT("}\n")
              << wxT("  \\caption{") << m_inputLabel->m_next->ToTeX().Trim() << wxT("}\n")
              << wxT("\\end{figure}\n");
        }
      }
      else
        str << wxT("\n\\verb|<<GRAPHICS>>|\n");
      break;

    case GC_TYPE_CODE:
      str = ToTeXCodeCell(imgDir, filename, imgCounter);
      str.Replace(wxT("\\[\\displaystyle \\]"),wxT(""));
      break;

    default:
      if (GetEditable() != NULL && !m_isHidden)
      {
        str = GetEditable()->ListToTeX();
        str.Trim(true);
        switch (GetEditable()->GetStyle())
        {
          case TS_TITLE:
            str = wxT("\n\\pagebreak{}\n{\\Huge {\\scshape ") + str + wxT("}}\n");
            str += wxT("\\setcounter{section}{0}\n\\setcounter{subsection}{0}\n");
            str += wxT("\\setcounter{figure}{0}\n");
            break;
          case TS_SECTION:
            // Trim() strings for TeX export to remove newlines in \section{}, \subsection{}, ... commands
	    // LaTeX creates an error on the following code:
	    // \section{Chapter 1
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
            if (str.StartsWith(wxT("TeX:")))
            {
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

wxString GroupCell::ToTeXCodeCell(wxString imgDir, wxString filename, int *imgCounter)
{
  wxString str;
  Configuration *configuration = (*m_configuration);

  // Input cells
  if (configuration->ShowCodeCells())
  {
    // For LaTeX export we must use a dot as decimal separator
    // Save LC_NUMERIC, set it to "C", print out the float and then restore it.
    const std::string saved_lc_numeric{setlocale(LC_NUMERIC, NULL)}; // get current LC_NUMERIC locale
    setlocale(LC_NUMERIC, "C");
    str += wxString::Format(
      "\n\n\\noindent\n%%%%%%%%%%%%%%%\n%%% INPUT:\n\\begin{minipage}[t]{%fem}\\color{red}\\bfseries\n",
      (double)configuration->GetLabelWidth()/14
      ) + m_inputLabel->ToTeX() +
      wxString("\n\\end{minipage}");
    setlocale(LC_NUMERIC, saved_lc_numeric.c_str());

    if (m_inputLabel->m_next)
    {

      wxString input = m_inputLabel->m_next->ToTeX();
      str += wxT("\n\\begin{minipage}[t]{\\textwidth}\\color{blue}\n") +
             input + "\n\\end{minipage}";
    }
  }

  if (m_output != NULL)
  {
    str += wxT("\n%%% OUTPUT:\n");
    // Need to define labelcolor if this is Copy as LaTeX!
    if (imgCounter == NULL)
      str += wxT("\\definecolor{labelcolor}{RGB}{100,0,0}\n");

    Cell *tmp = m_output.get();

    bool mathMode = false;

    while (tmp != NULL)
    {

      if (tmp->GetType() == MC_TYPE_IMAGE ||
          tmp->GetType() == MC_TYPE_SLIDE)
      {
        str << ToTeXImage(tmp, imgDir, filename, imgCounter);
      }
      else
      {
        switch (tmp->GetStyle())
        {

          case TS_LABEL:
          case TS_USERLABEL:
            if (mathMode)
              str += wxT("\\mbox{}\\]\n\\[\\displaystyle ");
            else
            {
              str += wxT("\\[\\displaystyle ");
              mathMode = true;
            }
            str += tmp->ToTeX() + wxT("\n");
            break;

          case TS_STRING:
            if (mathMode)
            {
              str += wxT("\\mbox{}\n\\]");
              mathMode = false;
            }
            str += TexEscapeOutputCell(tmp->ToTeX()) + wxT("\n");
            break;

          default:
            if (!mathMode)
            {
              str += wxT("\\[\\displaystyle ");
              mathMode = true;
            }
            str += tmp->ToTeX();
            break;
        }
      }

      tmp = tmp->GetNextToDraw();
    }

    if (mathMode)
    {
      // Some invisible dummy content that keeps TeX happy if there really is
      // no output to display.
      str += wxT("\\mbox{}\n\\]\n%%%%%%%%%%%%%%%");
    }
  }
  else
    str+=wxT("\n\n\\noindent%\n");

  return str;
}

wxString GroupCell::ToTeXImage(Cell *tmp, wxString imgDir, wxString filename, int *imgCounter)
{
  wxASSERT_MSG((imgCounter != NULL), _("Bug: No image counter to write to!"));
  if (imgCounter == NULL) return wxEmptyString;

  wxString str;

  if (imgDir != wxEmptyString)
  {
    Cell *copy = tmp->Copy();
    (*imgCounter)++;
    wxString image = filename + wxString::Format(wxT("_%d"), *imgCounter);
    if (!wxDirExists(imgDir))
      if (!wxMkdir(imgDir))
        return wxEmptyString;

    // Do we want to output LaTeX animations?
    bool AnimateLaTeX = true;
    wxConfig::Get()->Read(wxT("AnimateLaTeX"), &AnimateLaTeX);
    if ((tmp->GetType() == MC_TYPE_SLIDE) && (AnimateLaTeX))
    {
      SlideShow *src = dynamic_cast<SlideShow *>(tmp);
      str << wxT("\\begin{animateinline}{") + wxString::Format(wxT("%i"), src->GetFrameRate()) + wxT("}\n");
      for (int i = 0; i < src->Length(); i++)
      {
        wxString Frame = imgDir + wxT("/") + image + wxString::Format(wxT("_%i"), i);
        if ((src->GetBitmap(i)).SaveFile(Frame + wxT(".png")))
          str << wxT("\\includegraphics[width=.95\\linewidth,height=.80\\textheight,keepaspectratio]{") + Frame +
                 wxT("}\n");
        else
          str << wxT("\n\\verb|<<GRAPHICS>>|\n");
        if (i < src->Length() - 1)
          str << wxT("\\newframe");
      }
      str << wxT("\\end{animateinline}");
    }
    else
    {
      wxString file = imgDir + wxT("/") + image + wxT(".") + dynamic_cast<ImgCell *>(copy)->GetExtension();
      if (dynamic_cast<ImgCell *>(copy)->ToImageFile(file).x >= 0)
        str += wxT("\\includegraphics[width=.95\\linewidth,height=.80\\textheight,keepaspectratio]{") +
               filename + wxT("_img/") + image + wxT("}");
      else
        str << wxT("\n\\verb|<<GRAPHICS>>|\n");
    }
  }

  return str;
}

wxString GroupCell::ToXML()
{
  wxString str;
  str = wxT("\n<cell"); // start opening tag
  // write "type" according to m_groupType
  switch (m_groupType)
  {
    case GC_TYPE_CODE:
    {
      str += wxT(" type=\"code\"");
      int i = 0;
      for(StringHash::const_iterator it = m_knownAnswers.begin();
          it != m_knownAnswers.end();
          ++it)
      {
        i++;
        // In theory the attribute should be saved and read verbatim, with the exception
        // of the characters XML wants to be quoted. In reality wxWidget's newline handling
        // seems to be broken => escape newlines.
        wxString question = Cell::XMLescape(it->first);
        question.Replace(wxT("\n"),wxT("&#10;"));
        wxString answer = Cell::XMLescape(it->second);
        answer.Replace(wxT("\n"),wxT("&#10;"));
        str += wxString::Format(wxT(" question%i=\""),i) + question + wxT("\"");
        str += wxString::Format(wxT(" answer%i=\""),i) + answer + wxT("\"");
      }
      
      if(m_autoAnswer)
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
    case GC_TYPE_PAGEBREAK:
    {
      str += wxT(" type=\"pagebreak\"/>");
      return str;
    }
      break;
    default:
      str += wxT(" type=\"unknown\"");
      break;
  }

  // write hidden status
  if (m_isHidden)
    str += wxT(" hide=\"true\"");
  str += wxT(">\n");

  Cell *input = GetInput();
  Cell *output = GetLabel();
  // write contents
  switch (m_groupType)
  {
    case GC_TYPE_CODE:
      if (input != NULL)
      {
        str += wxT("<input>\n");
        str += input->ListToXML();
        str += wxT("</input>");
      }
      if (output != NULL)
      {
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
      if (m_hiddenTree)
      {
        str += wxT("<fold>");
        str += m_hiddenTree->ListToXML();
        str += wxT("</fold>");
      }
      break;
    default:
    {
      Cell *tmp = output;
      while (tmp != NULL)
      {
        str += tmp->ListToXML();
        tmp = tmp->m_next;
      }
      break;
    }
  }
  str += wxT("\n</cell>\n");

  return str;
}

void GroupCell::SelectRectGroup(const wxRect &rect, const wxPoint &one, const wxPoint &two,
                                Cell **first, Cell **last)
{
  Configuration *configuration = (*m_configuration);

  *first = NULL;
  *last = NULL;


  if ((m_inputLabel) &&
      (
              (configuration->ShowCodeCells()) ||
              (m_groupType != GC_TYPE_CODE)
      ) &&
      (m_inputLabel->ContainsRect(rect))
          )
    m_inputLabel->SelectRect(rect, first, last);
  else if (m_output != NULL && !m_isHidden && m_outputRect.Contains(rect))
    SelectRectInOutput(rect, one, two, first, last);

  if (*first == NULL || *last == NULL)
  {
    *first = this;
    *last = this;
  }
}

void GroupCell::SelectInner(const wxRect &rect, Cell **first, Cell **last)
{
  *first = NULL;
  *last = NULL;

  if (m_inputLabel->ContainsRect(rect))
    m_inputLabel->SelectRect(rect, first, last);
  else if (m_output != NULL && !m_isHidden && m_outputRect.Contains(rect))
    m_output->SelectRect(rect, first, last);

  if (*first == NULL || *last == NULL)
  {
    *first = this;
    *last = this;
  }
}

void GroupCell::SelectPoint(const wxPoint &point, Cell **first, Cell **last)
{
  *first = NULL;
  *last = NULL;

  wxRect rect(point.x, point.y, 1, 1);

  if (m_inputLabel->ContainsRect(rect))
    m_inputLabel->SelectInner(rect, first, last);
}

void GroupCell::SelectRectInOutput(const wxRect &rect, const wxPoint &one, const wxPoint &two,
                                   Cell **first, Cell **last)
{
  if (m_isHidden)
    return;

  Cell *tmp;
  wxPoint start, end;

  if (one.y < two.y || (one.y == two.y && one.x < two.x))
  {
    start = one;
    end = two;
  }
  else
  {
    start = two;
    end = one;
  }

  // Lets select a rectangle
  tmp = m_output.get();
  *first = *last = NULL;

  while (tmp != NULL && !rect.Intersects(tmp->GetRect()))
    tmp = tmp->GetNextToDraw();
  *first = tmp;
  *last = tmp;
  while (tmp != NULL)
  {
    if (rect.Intersects(tmp->GetRect()))
      *last = tmp;
    tmp = tmp->GetNextToDraw();
  }

  if (*first != NULL && *last != NULL)
  {

    // If selection is on multiple lines, we need to correct it
    if ((*first)->GetCurrentY() != (*last)->GetCurrentY())
    {
      tmp = *last;
      Cell *curr;

      // Find the first cell in selection
      while (*first != tmp &&
             ((*first)->GetCurrentX() + (*first)->GetWidth() < start.x
              || (*first)->GetCurrentY() + (*first)->GetDrop() < start.y))
        *first = (*first)->GetNextToDraw();

      // Find the last cell in selection
      curr = *last = *first;
      while (1)
      {
        curr = curr->GetNextToDraw();
        if (curr == NULL)
          break;
        if (curr->GetCurrentX() <= end.x &&
            curr->GetCurrentY() - curr->GetCenterList() <= end.y)
          *last = curr;
        if (curr == tmp)
          break;
      }
    }

    if (*first == *last)
      (*first)->SelectInner(rect, first, last);
  }
}

wxString GroupCell::GetToolTip(const wxPoint &point)
{
  if(ContainsPoint(point))
  {
    // Default assumption: will be overwritten by the next command,
    // if there is a more accurate solution.
    m_cellPointers->m_cellUnderPointer = this;
  }
  
  wxString retval = m_toolTip;

  if (m_isHidden)
    return retval;
  
  Cell *tmp = m_output.get();
  while (tmp != NULL)
  {

    // If a cell contains a cell containing a tooltip the tooltip of the
    // containing cell will be overridden.
    if(tmp->GetToolTip(point) != wxEmptyString)
      retval = tmp->GetToolTip(point);

    tmp = tmp->m_next;
  }

  return retval;
}

// cppcheck-suppress functionConst
bool GroupCell::SetEditableContent(wxString text)
{
  if (GetEditable())
  {
    GetEditable()->SetValue(text);
    return true;
  }
  else
    return false;
}

EditorCell *GroupCell::GetEditable() const
{
  switch (m_groupType)
  {
    case GC_TYPE_PAGEBREAK:
      return NULL;
    case GC_TYPE_CODE:
    case GC_TYPE_IMAGE:
    case GC_TYPE_TEXT:
    case GC_TYPE_TITLE:
    case GC_TYPE_SECTION:
    case GC_TYPE_SUBSECTION:
    case GC_TYPE_SUBSUBSECTION:
    case GC_TYPE_HEADING5:
    case GC_TYPE_HEADING6:
    default:
      return GetInput();
  }
}

void GroupCell::BreakLines()
{
  BreakLines(m_output.get());
}

void GroupCell::BreakLines(Cell *cell)
{
  if(cell == NULL)
    return;

  UnBreakUpCells(cell);
  if(BreakUpCells(cell))
  {
    if(m_output != NULL)
    {
      m_output->ResetSizeList();
      m_output->RecalculateList((*m_configuration)->GetMathFontSize());
    }
    ResetData();
  }

  int fullWidth = (*m_configuration)->GetClientWidth();
  Configuration *configuration = (*m_configuration);
  int currentWidth = GetLineIndent(cell);
  if((cell->GetStyle() != TS_LABEL) && (cell->GetStyle() != TS_USERLABEL))
    fullWidth -= configuration->GetIndent();

  // Don't let the layout degenerate for small window widths
  if (fullWidth < Scale_Px(150)) fullWidth = Scale_Px(150);
  
  while (cell != NULL && !m_isHidden)
  {
    cell->ResetData();
    cell->SoftLineBreak(false);
    if (!cell->m_isBrokenIntoLines)
    {
      if (cell->BreakLineHere() || (currentWidth + cell->GetWidth() >= fullWidth))
      {
        cell->SoftLineBreak(true);
        Cell *nextCell = cell;
        if(cell->GetNextToDraw())
          nextCell = cell->GetNextToDraw();
        currentWidth = GetLineIndent(nextCell) + cell->GetWidth();
      }
      else
        currentWidth += cell->GetWidth();
    }
    cell = cell->GetNextToDraw();
  }
}

void GroupCell::SelectOutput(Cell **start, Cell **end)
{
  if (m_isHidden)
    return;

  *start = m_output.get();

  while (*start != NULL && ((*start)->GetStyle() != TS_LABEL) && ((*start)->GetStyle() != TS_USERLABEL))
    *start = (*start)->GetNextToDraw();


  if (*start != NULL)
    *start = (*start)->GetNextToDraw();

  *end = *start;

  while (*end != NULL &&
         (*end)->GetNextToDraw() != NULL)
    *end = (*end)->GetNextToDraw();

  if (*end == NULL || *start == NULL)
    *end = *start = NULL;
}

bool GroupCell::BreakUpCells(Cell *cell)
{
  bool lineHeightsChanged = false;

  if(cell == NULL)
    return false;

  int showLength;
  switch ((*m_configuration)->ShowLength())
  {
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

  // Reduce the number of steps involved in layouting big equations
  if(m_cellsInGroup > showLength)
  {
    wxLogMessage(_("Resolving to 1D layout for one cell in order to save time"));
    while (cell != NULL && !m_isHidden)
    {
      if (cell->BreakUp())
        lineHeightsChanged = true;
      cell = cell->GetNextToDraw();
    }
    return lineHeightsChanged;
  }
  else
  {
    int clientWidth = (*m_configuration)->GetClientWidth();
    
    while (cell != NULL && !m_isHidden)
    {
      if ((!cell->m_isBrokenIntoLines) &&
          ((cell->GetWidth() +
            (*m_configuration)->GetIndent() +
            Scale_Px((*m_configuration)->GetLabelWidth()) > clientWidth)))
      {
        if (cell->BreakUp())
          lineHeightsChanged = true;
      }
      cell = cell->GetNextToDraw();
    }
    
    return lineHeightsChanged;
  }
}

void GroupCell::UnBreakUpCells(Cell *cell)
{
  int showLength;
  switch ((*m_configuration)->ShowLength())
  {
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
  if(m_cellsInGroup > showLength)
  {
    wxLogMessage(_("Resolving to linear layout for one big cell in order to save time"));
    return;
  }
 
  while (cell != NULL)
  {
    if (cell->m_isBrokenIntoLines)
      cell->Unbreak();
    cell = cell->GetNextToDraw();
  }
}

// support for hiding text, code cells

void GroupCell::Hide(bool hide)
{
  if (IsFoldable())
    return;

  if (m_isHidden == hide)
    return;

  m_isHidden = hide;
  if ((m_groupType == GC_TYPE_TEXT) || (m_groupType == GC_TYPE_CODE))
    GetEditable()->SetFirstLineOnly(m_isHidden);

  // Don't keep cached versions of scaled images around if they aren't visible at all.
  if (GetLabel())
    GetLabel()->ClearCacheList();

  ResetSize();
  GetEditable()->ResetSize();
}

void GroupCell::SwitchHide()
{
  Hide(!m_isHidden);
}

//
// support for folding/unfolding sections
//
bool GroupCell::HideTree(GroupCell *tree)
{
  if (m_hiddenTree)
    return false;
  m_hiddenTree = tree;
  m_hiddenTree->SetHiddenTreeParent(this);

  // Clear cached images from cells that are hidden
  GroupCell *tmp = m_hiddenTree;
  while (tmp)
  {
    if (tmp->GetLabel())
      tmp->GetLabel()->ClearCacheList();
    tmp = tmp->GetNext();
  }

  return true;
}

GroupCell *GroupCell::UnhideTree()
{
  GroupCell *tree = m_hiddenTree;
  m_hiddenTree->SetHiddenTreeParent(m_hiddenTreeParent);
  m_hiddenTree = NULL;
  return tree;
}

/**
 * Unfold a tree from the bottom up, when a hidden cell needs to be seen.
 *
 * @return true if any cells were unfolded.
 */
bool GroupCell::RevealHidden()
{
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
void GroupCell::SetHiddenTreeParent(GroupCell *parent)
{
  GroupCell *cell = this;
  while (cell)
  {
    cell->m_hiddenTreeParent = parent;
    cell = cell->GetNext();
  }
}

GroupCell *GroupCell::Fold()
{
  if (!IsFoldable() || m_hiddenTree) // already folded?? shouldn't happen
    return NULL;
  if (m_next == NULL)
    return NULL;
  int nextgct = dynamic_cast<GroupCell *>(m_next)->GetGroupType(); // groupType of the next cell
  if ((m_groupType == nextgct) || IsLesserGCType(nextgct))
    return NULL; // if the next gc shouldn't be folded, exit

  // now there is at least one cell to fold (at least m_next)
  GroupCell *end = dynamic_cast<GroupCell *>(m_next);
  GroupCell *start = end; // first to fold

  while (end != NULL)
  {
    if (end->GetLabel())
      end->GetLabel()->ClearCacheList();

    GroupCell *tmp = end->GetNext();
    if (tmp == NULL)
      break;
    if ((m_groupType == tmp->GetGroupType()) || IsLesserGCType(tmp->GetGroupType()))
      break; // the next one of the end is not suitable for folding, break
    end = tmp;
  }

  // cell(s) to fold are between start and end (including these two)

  if(end != NULL)
  {
    if(end->m_next != NULL)
    {
      m_next = end->m_next;
      SetNextToDraw(end->m_next);
      end->m_next->m_previous = this;
    }
    else
      m_next = m_nextToDraw = NULL;
    {
      end->m_next = NULL;
      end->SetNextToDraw(NULL);
    }
  }
  
  start->m_previous = NULL;
  m_hiddenTree = start; // save the torn out tree into m_hiddenTree
  m_hiddenTree->SetHiddenTreeParent(this);
  return this;
}

// unfolds the m_hiddenTree beneath this cell
// be careful to update m_last if this happens in the main tree in MathCtrl
GroupCell *GroupCell::Unfold()
{
  if (!IsFoldable() || !m_hiddenTree)
    return NULL;

  Cell *next = m_next;

  // sew together this cell with m_hiddenTree
  m_next = m_nextToDraw = m_hiddenTree;
  m_hiddenTree->m_previous = this;

  Cell *tmp = m_hiddenTree;
  while (tmp->m_next)
    tmp = tmp->m_next;
  // tmp holds the last element of m_hiddenTree
  tmp->m_next = next;
  tmp->SetNextToDraw(next);
  if (next)
    next->m_previous = tmp;

  m_hiddenTree->SetHiddenTreeParent(m_hiddenTreeParent);
  m_hiddenTree = NULL;
  return dynamic_cast<GroupCell *>(tmp);
}

GroupCell *GroupCell::FoldAll()
{
  GroupCell *result = NULL;

  GroupCell *tmp = this;

  while (tmp != NULL)
  {
    if (tmp->IsFoldable() && !tmp->m_hiddenTree)
    {
      tmp->Fold();
      result = tmp;
    }
    if (tmp->m_hiddenTree != NULL)
      tmp->m_hiddenTree->FoldAll();
    tmp = tmp->GetNext();
  }
  return result;
}

// unfolds recursivly its contents
// if (all) then also calls it on it's m_next
GroupCell *GroupCell::UnfoldAll()
{
  GroupCell *result = NULL;

  GroupCell *tmp = this;

  while (tmp != NULL)
  {
    if (tmp->IsFoldable() && (tmp->m_hiddenTree != NULL))
    {
      tmp->Unfold();
      result = tmp;
    }
    if (tmp->m_hiddenTree != NULL)
      m_hiddenTree->UnfoldAll();
    tmp = tmp->GetNext();
  }
  return result;
}

bool GroupCell::IsLesserGCType(int comparedTo) const
{
  switch (m_groupType)
  {
    case GC_TYPE_CODE:
    case GC_TYPE_TEXT:
    case GC_TYPE_PAGEBREAK:
    case GC_TYPE_IMAGE:
      return (comparedTo == GC_TYPE_TITLE) || (comparedTo == GC_TYPE_SECTION) ||
             (comparedTo == GC_TYPE_SUBSECTION) || (comparedTo == GC_TYPE_SUBSUBSECTION) ||
        (comparedTo == GC_TYPE_HEADING5) || (comparedTo == GC_TYPE_HEADING6);
    case GC_TYPE_HEADING6:
      return (comparedTo == GC_TYPE_TITLE) || (comparedTo == GC_TYPE_SECTION)
             || (comparedTo == GC_TYPE_SUBSECTION) || (comparedTo == GC_TYPE_SUBSUBSECTION) ||
        (comparedTo == GC_TYPE_HEADING5);
    case GC_TYPE_HEADING5:
      return (comparedTo == GC_TYPE_TITLE) || (comparedTo == GC_TYPE_SECTION)
             || (comparedTo == GC_TYPE_SUBSECTION)|| (comparedTo == GC_TYPE_SUBSUBSECTION);
    case GC_TYPE_SUBSUBSECTION:
      return (comparedTo == GC_TYPE_TITLE) || (comparedTo == GC_TYPE_SECTION)
             || (comparedTo == GC_TYPE_SUBSECTION);
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

void GroupCell::Number(int &section, int &subsection, int &subsubsection, int &heading5, int &heading6, int &image)
{
  GroupCell *tmp = this;

  while (tmp != NULL)
  {
    switch (tmp->m_groupType)
    {
      case GC_TYPE_TITLE:
        section = subsection = subsubsection = heading5 = heading6 = 0;
        break;
      case GC_TYPE_SECTION:
        section++;
        subsection = subsubsection = heading5 = heading6 = 0;
        {
          wxString num = wxT("  ");
          num << section << wxT(" ");
          tmp->m_inputLabel->SetValue(num);
          tmp->m_inputLabel->SetStyle(TS_SECTION);
        }
        break;
      case GC_TYPE_SUBSECTION:
        subsubsection = heading5 = heading6 = 0;
        subsection++;
        {
          wxString num = wxT("  ");
          num << section << wxT(".") << subsection << wxT(" ");
          tmp->m_inputLabel->SetValue(num);
          tmp->m_inputLabel->SetStyle(TS_SUBSECTION);
        }
        break;
      case GC_TYPE_SUBSUBSECTION:
        heading5 = heading6 = 0;
        subsubsection++;
        {
          wxString num = wxT("  ");
          num << section << wxT(".") << subsection << wxT(".") << subsubsection << wxT(" ");
          tmp->m_inputLabel->SetValue(num);
          tmp->m_inputLabel->SetStyle(TS_SUBSUBSECTION);
        }
        break;
      case GC_TYPE_HEADING5:
        heading5++;
        heading6 = 0;
        {
          wxString num = wxT("  ");
          num << section << wxT(".") << subsection << wxT(".") << subsubsection << wxT(".")
              << heading5 << wxT(" ");
          tmp->m_inputLabel->SetValue(num);
          tmp->m_inputLabel->SetStyle(TS_HEADING5);
        }
        break;
      case GC_TYPE_HEADING6:
        heading6++;
        {
          wxString num = wxT("  ");
          num << section << wxT(".") << subsection << wxT(".") << subsubsection << wxT(".")
              << heading5 << wxT(".") << heading6 << wxT(" ");
          tmp->m_inputLabel->SetValue(num);
          tmp->m_inputLabel->SetStyle(TS_HEADING6);
        }
        break;
      case GC_TYPE_IMAGE:
        image++;
        {
          wxString num = wxString::Format(_("Figure %d:"), image);
          tmp->m_inputLabel->SetValue(num);
        }
        break;
      default:
        break;
    }

    if (IsFoldable() && tmp->m_hiddenTree)
      tmp->m_hiddenTree->Number(section, subsection, subsubsection, heading5, heading6, image);

    tmp = tmp->GetNext();
  }
}

bool GroupCell::IsMainInput(Cell *active) const
{
  return m_inputLabel->m_next != NULL && active == m_inputLabel->m_next;
}

bool GroupCell::Contains(GroupCell *cell)
{
  GroupCell *tmp = this;

  // Iterate through all cells
  while (tmp != NULL)
  {
    // If this is the cell we search for we can end the search.
    if (tmp == cell)
      return true;

    // If this cell contains a hidden tree we have to search that at well.
    if ((tmp->IsFoldable()) && (tmp->GetHiddenTree()) != NULL)
    {
      if (tmp->GetHiddenTree()->Contains(cell))
        return true;
    }

    // Step to the next cell.
    tmp = tmp->GetNext();
  }

  return false;
}

#if wxUSE_ACCESSIBILITY
wxAccStatus GroupCell::GetDescription(int childId, wxString *description)
{
  if (description == NULL)
    return wxACC_FAIL;
  
  if (childId == 0)
  {
    if (m_groupType == GC_TYPE_PAGEBREAK)
    {
      *description = _("A page break");
      return wxACC_OK;
    }
    else
    {
      *description = _("A GroupCell that bundles input with its output");
      return wxACC_OK;
    }
  }
  else
  {
    wxAccessible *cell = NULL;
    if (GetChild(childId, &cell) == wxACC_OK)
    {
      return cell->GetDescription(0, description);
    }
  }
  
  *description = wxEmptyString;
  return wxACC_FAIL;
}


wxAccStatus GroupCell::GetLocation(wxRect &rect, int elementId)
{
  if(elementId == 0)
  {
    rect = wxRect(GetRect().GetTopLeft()     + (*m_configuration)->GetVisibleRegion().GetTopLeft(),
                  GetRect().GetBottomRight() + (*m_configuration)->GetVisibleRegion().GetTopLeft());
    
    // Our GroupCell handles the hcaret below the cell, as well as its contents
    rect.SetBottom(rect.GetBottom()+(*m_configuration)->GetGroupSkip());
    
    // If we are the 1st groupcell of the worksheet we handle the hcaret above this
    // cell, too.
    if(m_previous == NULL)
      rect.SetTop(rect.GetTop()-(*m_configuration)->GetGroupSkip());
    
    if(rect.GetTop() < 0)
      rect.SetTop(0);
    if(rect.GetLeft() < 0)
      rect.SetLeft(0);
    if(rect.GetBottom() > (*m_configuration)->GetVisibleRegion().GetWidth())
      rect.SetBottom((*m_configuration)->GetVisibleRegion().GetWidth());
    if(rect.GetRight() > (*m_configuration)->GetVisibleRegion().GetHeight())
      rect.SetRight((*m_configuration)->GetVisibleRegion().GetHeight());
    rect = wxRect(rect.GetTopLeft()+(*m_configuration)->GetWorksheetPosition(),rect.GetBottomRight()+(*m_configuration)->GetWorksheetPosition());
    return wxACC_OK;
  }
  else
  {
    wxAccessible *child = NULL;
	if (GetChild(elementId, &child) == wxACC_OK)
		return child->GetLocation(rect, 0);
  }
  return wxACC_FAIL;
}

#endif

void GroupCell::SetNextToDraw(Cell *next)
{
  m_nextToDraw = next;
}

wxString GroupCell:: m_lookalikeChars(
    wxT("µ")		wxT("\u03bc")
    wxT("\u2126")	wxT("\u03a9")
    wxT("C")		wxT("\u03F2")
    wxT("C")		wxT("\u0421")
    wxT("\u03F2")	wxT("\u0421")
    wxT("A")		wxT("\u0391")
    wxT("A")		wxT("\u0410")
    wxT("\u0391")	wxT("\u0410")
    wxT("M")		wxT("\u0392")
    wxT("E")		wxT("\u0395")
    wxT("E")		wxT("\u0415")
    wxT("\u0415")	wxT("\u0395")
    wxT("Z")		wxT("\u0396")
    wxT("H")		wxT("\u0397")
    wxT("H")		wxT("\u041D")
    wxT("\u0397")	wxT("\u041D")
    wxT("I")		wxT("\u0399")
    wxT("I")		wxT("\u0406")
    wxT("l")		wxT("\u0406")
    wxT("K")		wxT("\u039A")
    wxT("K")		wxT("\u041A")
    wxT("\u039A")	wxT("\u041A")
    wxT("\u212a")	wxT("\u041A")
    wxT("K")		wxT("\u212A")
    wxT("M")		wxT("\u041c")
    wxT("\u039C")	wxT("\u041c")
    wxT("M")		wxT("\u039C")
    wxT("N")		wxT("\u039D")
    wxT("O")		wxT("\u039F")
    wxT("O")		wxT("\u041E")
    wxT("\u039F")	wxT("\u041E")
    wxT("\u039F")	wxT("\u041E")
    wxT("P")		wxT("\u03A1")
    wxT("X")		wxT("\u0425")
    wxT("e")		wxT("\u0435")
    wxT("p")		wxT("\u0440")
    wxT("x")		wxT("\u0445")
    wxT("y")		wxT("\u0443")
    wxT("P")		wxT("\u0420")
    wxT("\u03A1")	wxT("\u0420")
    wxT("T")		wxT("\u03A4")
    wxT("T")		wxT("\u0422")
    wxT("\u03A4")	wxT("\u0422")
    wxT("Y")		wxT("\u03A5")
    wxT("\u212a")	wxT("\u039A")
    wxT("l")		wxT("I")
    wxT("B")		wxT("\u0392")
    wxT("S")		wxT("\u0405")
    wxT("\u0392")	wxT("\u0412")
    wxT("B")		wxT("\u0412")
    wxT("J")		wxT("\u0408")
    wxT("a")		wxT("\u0430")
    wxT("o")		wxT("\u03bf")
    wxT("\u03a3")      wxT("\u2211")
    wxT("o")		wxT("\u043e")
    wxT("\u03bf")	wxT("\u043e")
    wxT("c")		wxT("\u0441")
    wxT("s")		wxT("\u0455")
    wxT("t")		wxT("\u03c4")
    wxT("u")		wxT("\u03c5")
    wxT("x")		wxT("\u03c7")
    wxT("ü")		wxT("\u03cb")
    wxT("\u0460")	wxT("\u03c9")
    wxT("\u0472")	wxT("\u0398"));
