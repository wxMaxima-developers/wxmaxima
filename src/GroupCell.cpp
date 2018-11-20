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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  This file defines the class GroupCell

  GroupCell is the MathCell type that bundles user input with eventual images or 
  output from maxima that belongs to it.
*/

#include <wx/config.h>
#include <wx/clipbrd.h>
#include "MarkDown.h"
#include "GroupCell.h"
#include "SlideShowCell.h"
#include "TextCell.h"
#include "EditorCell.h"
#include "ImgCell.h"
#include "Bitmap.h"
#include "list"

GroupCell::GroupCell(Configuration **config, int groupType, CellPointers *cellPointers, wxString initString) : MathCell(
        this, config)
{
  m_next = m_previous = m_nextToDraw = m_previousToDraw = NULL;
  m_autoAnswer = false;
  m_cellPointers = cellPointers;
  m_inEvaluationQueue = false;
  m_lastInEvaluationQueue = false;
  m_inputLabel = NULL;
  m_output = NULL;
  m_hiddenTree = NULL;
  m_hiddenTreeParent = NULL;
  m_outputRect.x = -1;
  m_outputRect.y = -1;
  m_outputRect.width = 0;
  m_outputRect.height = 0;
  m_group = this;
  m_fontSize = 10;
  m_mathFontSize = 10;
  m_forceBreakLine = true;
  m_breakLine = true;
  m_type = MC_TYPE_GROUP;
  m_hide = false;
  m_groupType = groupType;
  m_lastInOutput = NULL;
  m_appendedCells = NULL;

  // set up cell depending on groupType, so we have a working cell
  if (groupType != GC_TYPE_PAGEBREAK)
  {
    if (groupType == GC_TYPE_CODE)
      m_inputLabel = new TextCell(this, m_configuration, m_cellPointers, EMPTY_INPUT_LABEL);
    else
      m_inputLabel = new TextCell(this, m_configuration, m_cellPointers, wxT(""));

    m_inputLabel->SetType(MC_TYPE_MAIN_PROMPT);
  }

  EditorCell *editor = NULL;

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
      editor = NULL;
      break;
  }

  if ((editor != NULL) && (initString != wxEmptyString))
    editor->SetValue(initString);

  // when creating an image cell, if a string is provided
  // it loads an image (without deleting it)
  if ((groupType == GC_TYPE_IMAGE) && (initString.Length() > 0))
  {
    ImgCell *ic = new ImgCell(this, m_configuration, m_cellPointers, initString, false);
    AppendOutput(ic);
  }

  // The GroupCell this cell belongs to is this GroupCell.
  SetGroup(this);
}

void GroupCell::SetCellStyle(int style)
{
  if(GetEditable() == NULL)
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
void GroupCell::SetGroup(MathCell *parent)
{
  //m_group = parent;
  if (m_inputLabel != NULL)
    m_inputLabel->SetGroupList(parent);

  MathCell *tmp = m_output;
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

    tmp = dynamic_cast<GroupCell *>(tmp->m_next);
  }

}

MathCell *GroupCell::Copy()
{
  GroupCell *tmp = new GroupCell(m_configuration, m_groupType, m_cellPointers);
  tmp->Hide(m_hide);
  CopyData(this, tmp);
  tmp->AutoAnswer(m_autoAnswer);
  if (m_inputLabel)
    tmp->SetInput(m_inputLabel->CopyList());
  if (m_output != NULL)
    tmp->SetOutput(m_output->CopyList());

  return tmp;
}

wxString GroupCell::ToWXM(bool wxm)
{
  wxString retval;
  bool trailingNewline = true;
  if (IsHidden())
    retval += wxT("/* [wxMaxima: hide output   ] */\n");

  switch (GetGroupType())
  {
    case GC_TYPE_CODE:
      if(wxm)
        retval += wxT("/* [wxMaxima: input   start ] */\n");
      retval += GetEditable()->ToString() + wxT("\n");
      if(wxm)
        retval += wxT("/* [wxMaxima: input   end   ] */\n");
      else
        trailingNewline = false;

      // Export the list of known answers
      if(wxm)
      {
        for(std::list<wxString>::iterator it = m_knownAnswers.begin(); it != m_knownAnswers.end();++it)
        {
          retval += wxT("/* [wxMaxima: answer  start ] */\n");
          retval += *it + wxT("\n");
          retval += wxT("/* [wxMaxima: answer  end   ] */\n");
        }
        if (m_autoAnswer)
          retval += wxT("/* [wxMaxima: autoanswer    ] */\n");
      }
      break;
    case GC_TYPE_TEXT:
      if(wxm)
      {
        retval += wxT("/* [wxMaxima: comment start ]\n");
        retval += GetEditable()->ToString() + wxT("\n");
        retval += wxT("   [wxMaxima: comment end   ] */\n");
      }
      else
      {
        retval += wxT("/* ") + GetEditable()->ToString() + wxT(" */\n");
        trailingNewline = false;
      }
      break;
    case GC_TYPE_SECTION:
      retval += wxT("/* [wxMaxima: section start ]\n");
      retval += GetEditable()->ToString() + wxT("\n");
      retval += wxT("   [wxMaxima: section end   ] */\n");
      break;
    case GC_TYPE_SUBSECTION:
      retval += wxT("/* [wxMaxima: subsect start ]\n");
      retval += GetEditable()->ToString() + wxT("\n");
      retval += wxT("   [wxMaxima: subsect end   ] */\n");
      break;
    case GC_TYPE_SUBSUBSECTION:
      retval += wxT("/* [wxMaxima: subsubsect start ]\n");
      retval += GetEditable()->ToString() + wxT("\n");
      retval += wxT("   [wxMaxima: subsubsect end   ] */\n");
      break;
    case GC_TYPE_HEADING5:
      retval += wxT("/* [wxMaxima: heading5 start ]\n");
      retval += GetEditable()->ToString() + wxT("\n");
      retval += wxT("   [wxMaxima: heading5 end   ] */\n");
      break;
    case GC_TYPE_HEADING6:
      retval += wxT("/* [wxMaxima: heading6 start ]\n");
      retval += GetEditable()->ToString() + wxT("\n");
      retval += wxT("   [wxMaxima: heading6 end   ] */\n");
      break;
    case GC_TYPE_TITLE:
      retval += wxT("/* [wxMaxima: title   start ]\n");
      retval += GetEditable()->ToString() + wxT("\n");
      retval += wxT("   [wxMaxima: title   end   ] */\n");
      break;
    case GC_TYPE_IMAGE:
      retval += wxT("/* [wxMaxima: caption start ]\n");
      retval += GetEditable()->ToString() + wxT("\n");
      retval += wxT("   [wxMaxima: caption end   ] */\n");
      if ((GetLabel() != NULL) && (GetLabel()->GetType() == MC_TYPE_IMAGE))
      {
        ImgCell *image = dynamic_cast<ImgCell *>(GetLabel());
        retval += wxT("/* [wxMaxima: image   start ]\n");
        retval += image->GetExtension() + wxT("\n");
        retval += wxBase64Encode(image->GetCompressedImage()) + wxT("\n");
        retval += wxT("   [wxMaxima: image   end   ] */\n");
      }
      break;
    case GC_TYPE_PAGEBREAK:
      retval += wxT("/* [wxMaxima: page break    ] */\n");
      break;
  }

  // Export eventual hidden trees.
  GroupCell *tmp = GetHiddenTree();
  if (tmp != NULL)
  {
    if(wxm)
      retval += wxT("/* [wxMaxima: fold    start ] */\n");
    while (tmp != NULL)
    {
      retval += tmp->ToWXM(wxm);
      tmp = dynamic_cast<GroupCell *>(tmp->m_next);
    }
    if(wxm)
      retval += wxT("\n/* [wxMaxima: fold    end   ] */\n");
  }
  if(trailingNewline)
    retval += wxT("\n");
  
  return retval;
}


GroupCell::~GroupCell()
{
  MarkAsDeleted();
  wxDELETE(m_inputLabel);
  wxDELETE(m_output);
  wxDELETE(m_hiddenTree);
  m_inputLabel = m_output = m_hiddenTree = NULL;
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

  MathCell::MarkAsDeleted();
}

std::list<MathCell *> GroupCell::GetInnerCells()
{
  std::list<MathCell *> innerCells;
  if (m_groupType != GC_TYPE_PAGEBREAK)
  {
    if(GetInput())
      innerCells.push_back(GetInput());
    if(GetOutput())
      innerCells.push_back(GetOutput());
  }
  return innerCells;
}

wxString GroupCell::TexEscapeOutputCell(wxString Input)
{
  wxString retval(Input);
  Input.Replace(wxT("#"), wxT("\\#"));
  return (Input);
}

void GroupCell::SetInput(MathCell *input)
{
  if (input == NULL)
    return;
  wxDELETE(m_inputLabel);
  m_inputLabel = input;
  m_inputLabel->SetGroup(this);
}

void GroupCell::AppendInput(MathCell *cell)
{
  if (m_inputLabel == NULL)
  {
    m_inputLabel = cell;
  }
  else
  {
    if (m_inputLabel->m_next == NULL)
      m_inputLabel->AppendCell(cell);
    else if (m_inputLabel->m_next->GetValue().Length() == 0)
    {
      wxDELETE(m_inputLabel->m_next);
      m_inputLabel->m_next = m_inputLabel->m_nextToDraw = NULL;
      m_inputLabel->AppendCell(cell);
    }
    else
    {
      AppendOutput(cell);
      m_hide = false;
    }
  }
}


void GroupCell::SetOutput(MathCell *output)
{
  if (output == NULL)
    return;
  
  if((m_cellPointers->m_answerCell) &&(m_cellPointers->m_answerCell->GetGroup() == this))
    m_cellPointers->m_answerCell = NULL;
  
  wxDELETE(m_output);

  m_output = output;
  m_output->SetGroup(this);

  m_lastInOutput = m_output;

  while (m_lastInOutput->m_next != NULL)
    m_lastInOutput = m_lastInOutput->m_next;

  // ResetSize();
  //m_appendedCells = output;
}

void GroupCell::RemoveOutput()
{
  // If there is nothing to do we can skip the rest of this action.
  if (m_output == NULL)
    return;

  if((m_cellPointers->m_answerCell) &&(m_cellPointers->m_answerCell->GetGroup() == this))
    m_cellPointers->m_answerCell = NULL;

  if (!(GetGroupType() == GC_TYPE_IMAGE))
  {
    wxDELETE(m_output);
    m_output = NULL;
  }

  m_cellPointers->m_errorList.Remove(this);
  // Calculate the new cell height.

  ResetSize();
  RecalculateHeight((*m_configuration)->GetDefaultFontSize());
  m_hide = false;
}

void GroupCell::AppendOutput(MathCell *cell)
{
  wxASSERT_MSG(cell != NULL, _("Bug: Trying to append NULL to a group cell."));
  if (cell == NULL) return;
  cell->SetGroupList(this);
  if (m_output == NULL)
  {
    m_output = cell;

    if (m_groupType == GC_TYPE_CODE && m_inputLabel->m_next != NULL)
      (dynamic_cast<EditorCell *>(m_inputLabel->m_next))->ContainsChanges(false);

    m_lastInOutput = m_output;

    while (m_lastInOutput->m_next != NULL)
      m_lastInOutput = m_lastInOutput->m_next;
  }

  else
  {
    MathCell *tmp = m_lastInOutput;
    if (tmp == NULL)
      tmp = m_output;

    while (tmp->m_next != NULL)
      tmp = tmp->m_next;

    tmp->AppendCell(cell);

    if(m_lastInOutput != NULL)
      while (m_lastInOutput->m_next != NULL)
        m_lastInOutput = m_lastInOutput->m_next;
  }

  if (m_appendedCells == NULL)
    m_appendedCells = cell;
  // ResetSize();
}

void GroupCell::Recalculate()
{
  int fontsize = (*m_configuration)->GetDefaultFontSize();
  
  m_fontSize = fontsize;
  m_mathFontSize = (*m_configuration)->GetMathFontSize();

  RecalculateWidths(fontsize);
  RecalculateHeight(fontsize);
}

void GroupCell::RecalculateWidths(int fontsize)
{
  MathCell::RecalculateWidths(fontsize);
  Configuration *configuration = (*m_configuration);
  if (m_width == -1 || m_height == -1 || configuration->ForceUpdate())
  {
    // special case of 'line cell'
    if (m_groupType == GC_TYPE_PAGEBREAK)
    {
      m_width = configuration->GetCellBracketWidth();
      m_height = 2;
      ResetData();
      return;
    }

    UnBreakUpCells();

    if(m_inputLabel != NULL)
    {
      m_inputLabel->RecalculateWidthsList(fontsize);
      
      // recalculate the position of input in ReEvaluateSelection!
      // m_inputLabel->m_currentPoint.x = m_currentPoint.x;
    }
    
    if (m_output == NULL || m_hide)
    {
      if ((configuration->ShowCodeCells()) ||
          (m_groupType != GC_TYPE_CODE))
      {
        m_width = m_inputLabel->GetFullWidth();
      }
      else
        m_width = 0;
    }
    else    {
      MathCell *tmp = m_output;
      while (tmp != NULL)
      {
        tmp->RecalculateWidths(tmp->IsMath() ? m_mathFontSize : m_fontSize);
        tmp = tmp->m_next;
      }
      // This is not correct, m_width will be computed correctly in RecalculateHeight!
      if ((configuration->ShowCodeCells()) ||
          (m_groupType != GC_TYPE_CODE))
      {
        if(m_inputLabel != NULL)
          m_width = m_inputLabel->GetFullWidth();
        else
          m_width = 100;
      }
    }

    BreakUpCells(m_fontSize, configuration->GetClientWidth());
    BreakLines(configuration->GetClientWidth());
  }
  ResetData();
}

void GroupCell::InputHeightChanged()
{
  ResetData();
  ResetSize();
  EditorCell *editorCell = GetEditable();
  if (editorCell != NULL) {
    editorCell->ResetSize();
    editorCell->RecalculateWidths(m_fontSize);
  }
  if (m_inputLabel != NULL) {
    m_inputLabel->ResetData();
  }
  RecalculateHeightInput(m_fontSize);
  if(m_output)
    m_height += m_output->GetMaxHeight();
}

// Called on resize events
// We need to forget line breaks/breakup cells and
// breakup cells and compute new line breaks
void GroupCell::OnSize()
{
  // Unbreakup cells
  MathCell *tmp = m_output;
  while (tmp != NULL)
  {
    tmp->Unbreak();
    tmp->BreakLine(false);
    tmp->ResetData();
    tmp = tmp->m_next;
  }
  int clientWidth = (*m_configuration)->GetClientWidth();
  BreakUpCells(m_fontSize, clientWidth);
  BreakLines(clientWidth);
  InputHeightChanged();
}

void GroupCell::RecalculateHeightInput(int fontsize)
{
  Configuration *configuration = (*m_configuration);
  
  // special case
  if (m_groupType == GC_TYPE_PAGEBREAK)
  {
    m_width = configuration->GetCellBracketWidth();
    m_height = 2;
    m_center = 0;
    MathCell::RecalculateWidthsList(fontsize);
    return;
  }
  
  if ((configuration->ShowCodeCells()) ||
      (m_groupType != GC_TYPE_CODE))
  {
    if(m_inputLabel)
    {
      m_inputLabel->RecalculateHeightList(fontsize);
      m_center = m_inputLabel->GetMaxCenter();
      m_height = m_inputLabel->GetMaxHeight();
    }
  }
  else
  {
    m_center = 0;
    m_height = 0;
  }
  
  if (!m_hide)
  {
    MathCell *tmp = m_output;
    while (tmp != NULL)
    {
      tmp->RecalculateHeight(tmp->IsMath() ? m_mathFontSize : m_fontSize);
      tmp = tmp->m_next;
    }
  }
  
  m_currentPoint.x = configuration->GetIndent();
  if (m_previous == NULL)
  {
    m_currentPoint.y = (*m_configuration)->GetBaseIndent() + GetMaxCenter();
  }
  else
    m_currentPoint.y = dynamic_cast<GroupCell *>(m_previous)->m_currentPoint.y +
      dynamic_cast<GroupCell *>(m_previous)->GetMaxDrop() + GetMaxCenter() +
      (*m_configuration)->GetGroupSkip();
  
  m_outputRect.x = m_currentPoint.x;
  m_outputRect.y = m_currentPoint.y;
  if (m_output) m_outputRect.y -= m_output->GetMaxCenter();
  else
  {
    m_outputRect.width = 0;
    m_outputRect.height = 0;
  }
  if ((configuration->ShowCodeCells()) ||
      (m_groupType != GC_TYPE_CODE))
  {
    m_height = m_inputLabel->GetMaxHeight();
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

void GroupCell::RecalculateHeightOutput(int WXUNUSED(fontsize))
{
  if(!m_hide)
  {
    m_appendedCells = m_output;
    if(m_output != NULL)
      RecalculateAppended();
  }
}

void GroupCell::RecalculateHeight(int fontsize)
{
  MathCell::RecalculateHeight(fontsize);
  Configuration *configuration = (*m_configuration);

  if (m_width < 0 || m_height < 0 || m_currentPoint.x < 0 || m_currentPoint.y < 0 ||
      configuration->ForceUpdate() || fontsize != m_fontSize_Old)
  {
    m_fontSize_Old = fontsize;

    RecalculateHeightInput(fontsize);
    
    RecalculateHeightOutput(fontsize);
  }

  if (((m_height != 0) || (m_next == NULL)) && (m_height < configuration->GetCellBracketWidth()))
    m_height = configuration->GetCellBracketWidth();
  
  configuration= (*m_configuration);
  if (m_previous == NULL)
  {
    m_currentPoint.x = configuration->GetIndent();
    m_currentPoint.y = (*m_configuration)->GetBaseIndent() + GetMaxCenter();
  }
  else
  {
    m_currentPoint.x = configuration->GetIndent();
    if(dynamic_cast<GroupCell *>(m_previous)->m_height > 0)
      m_currentPoint.y = dynamic_cast<GroupCell *>(m_previous)->m_currentPoint.y +
        dynamic_cast<GroupCell *>(m_previous)->GetMaxDrop() + GetMaxCenter() +
        (*m_configuration)->GetGroupSkip();
    else
      m_currentPoint.y = dynamic_cast<GroupCell *>(m_previous)->m_currentPoint.y;
  }

  // If code is hidden and there is no output a cell can have the height
  // 0. If it is higher than that we make our cell high enough to fit the 
  // bracket in.  m_appendedCells = NULL;

  if (m_inputLabel)
    m_inputLabel->SetCurrentPoint(m_currentPoint);
  if (GetEditable())
  {
    if(m_inputLabel == NULL)
      GetEditable()->SetCurrentPoint(m_currentPoint);
    else
      GetEditable()->SetCurrentPoint(wxPoint(m_currentPoint.x + m_inputLabel->GetWidth(), m_currentPoint.y));
  }
}

// We assume that appended cells will be in a new line!
void GroupCell::RecalculateAppended()
{
  if(m_hide)
    return;
  Configuration *configuration = (*m_configuration);
  if (m_appendedCells == NULL)
    return;

  MathCell *tmp = m_appendedCells;
  m_fontSize = configuration->GetFontSize(TS_TEXT);
  m_mathFontSize = configuration->GetMathFontSize();

  // Recalculate widths of cells
  while (tmp != NULL)
  {
    tmp->RecalculateWidths(tmp->IsMath() ? m_mathFontSize : m_fontSize);
    tmp = tmp->m_next;
  }

  // Breakup cells and break lines
  BreakUpCells(m_appendedCells, m_fontSize, configuration->GetClientWidth());
  BreakLines(m_appendedCells, configuration->GetClientWidth());

  // Recalculate size of cells
  tmp = m_appendedCells;
  while (tmp != NULL)
  {
    tmp->RecalculateHeight(tmp->IsMath() ? m_mathFontSize : m_fontSize);
    tmp = tmp->m_next;
  }

  // Update widths
  tmp = m_appendedCells;
  while (tmp != NULL)
  {
    if ((tmp->BreakLineHere()) ||
        ((tmp->m_previousToDraw == NULL) && (tmp->m_nextToDraw == NULL)) ||
        (tmp->GetStyle() == TS_LABEL) || (tmp->GetStyle() == TS_USERLABEL))
    {
      m_width = MAX(m_width, tmp->GetLineWidth());
      m_outputRect.width = MAX(m_outputRect.width, tmp->GetLineWidth());
      m_height            += tmp->GetMaxHeight();
      m_outputRect.height += tmp->GetMaxHeight();
      
      if (tmp->m_previousToDraw != NULL &&
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
    tmp = tmp->m_nextToDraw;
  }

  m_appendedCells = NULL;
}

void GroupCell::Draw(wxPoint point)
{
  MathCell::Draw(point);
  if (DrawThisCell(point))
  {
    if (m_width == -1 || m_height == -1)
      return;
    
    Configuration *configuration = (*m_configuration);
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

    if (configuration->ShowBrackets())
      DrawBracket();
    
    wxRect rect = GetRect(false);

    if(configuration->GetIndent() < rect.GetRight())
    {
       
      if(rect.GetLeft() <= configuration->GetCellBracketWidth())
        rect.SetLeft(configuration->GetIndent());
      
      //
      // Paint background if we have a text cell
      //
      if (m_groupType == GC_TYPE_TEXT )
      {
        int y = rect.GetY();

        if (m_height > 0 && m_width > 0 && y >= 0)
        {
          wxBrush br(configuration->GetColor(TS_TEXT_BACKGROUND));
          dc->SetBrush(br);
          wxPen pen(configuration->GetColor(TS_TEXT_BACKGROUND));
          dc->SetPen(pen);
          rect.SetWidth((*m_configuration)->GetCanvasSize().GetWidth());
          if (InUpdateRegion(rect))
            dc->DrawRectangle(CropToUpdateRegion(rect));
        }
      }
      //
      // Draw input and output
      //
      SetPen();
      wxPoint in(point);

      if ((configuration->ShowCodeCells()) ||
          (m_groupType != GC_TYPE_CODE))
      {
        configuration->Outdated(false);
        m_inputLabel->DrawList(in);
        if (m_groupType == GC_TYPE_CODE && m_inputLabel->m_next)
          configuration->Outdated((dynamic_cast<EditorCell *>(m_inputLabel->m_next))->ContainsChanges());
      }

      if (m_output != NULL && !m_hide)
      {
        MathCell *tmp = m_output;
        int drop = tmp->GetMaxDrop();
        if ((configuration->ShowCodeCells()) ||
            (m_groupType != GC_TYPE_CODE))
          in.y += m_inputLabel->GetMaxDrop();

        in.y += m_output->GetMaxCenter();
        m_outputRect.y = in.y - m_output->GetMaxCenter();
        m_outputRect.x = in.x;

        while (tmp != NULL)
        {         
          tmp->SetCurrentPoint(in);

          tmp->Draw(in);
          if ((tmp->m_nextToDraw != NULL) && (tmp->m_nextToDraw->BreakLineHere()))
            {
              if (tmp->m_nextToDraw->m_bigSkip)
                in.y += MC_LINE_SKIP;
 
              in.x = point.x;
              if(
                (tmp->m_nextToDraw->GetStyle() != TS_LABEL) &&
                (tmp->m_nextToDraw->GetStyle() != TS_USERLABEL) &&
                (tmp->m_nextToDraw->GetStyle() != TS_MAIN_PROMPT) &&
                (tmp->m_nextToDraw->GetStyle() != TS_OTHER_PROMPT) &&
                configuration->IndentMaths()
                )
                in.x += Scale_Px(configuration->GetLabelWidth()) + MC_TEXT_PADDING;
              
              in.y += drop + tmp->m_nextToDraw->GetMaxCenter();
              drop = tmp->m_nextToDraw->GetMaxDrop();
            }
          else
            in.x += tmp->GetWidth();

          tmp = tmp->m_nextToDraw;
        }
      }
    }
    configuration->Outdated(false); 
    UnsetPen();
  }
}

void GroupCell::CellUnderPointer(GroupCell *cell)
{
  if (m_cellPointers->m_groupCellUnderPointer != cell)
  {
    m_cellPointers->m_groupCellUnderPointer = cell;
  }
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
#if defined(__WXMAC__)
    dc->SetPen(wxNullPen); // wxmac doesn't like a border with wxXOR
#else
    dc->SetPen(*(wxThePenList->FindOrCreatePen(
                  configuration->GetColor(TS_SELECTION),
                  configuration->GetDefaultLineWidth(),
                  wxPENSTYLE_SOLID)
                ));
// window linux, set a pen
#endif
    dc->SetBrush(*(wxTheBrushList->FindOrCreateBrush(configuration->GetColor(TS_SELECTION))));
    drawBracket = true;
  }
  else if (m_cellPointers->m_errorList.Contains(this))
  {
#if defined(__WXMAC__)
    dc->SetPen(wxNullPen); // wxmac doesn't like a border with wxXOR
#else
    dc->SetPen(*wxRED_PEN);
// window linux, set a pen
#endif
    dc->SetBrush(*wxRED_BRUSH);
    drawBracket = true;
  }
  else if ((m_cellPointers->m_answerCell) && (m_cellPointers->m_answerCell->GetGroup() == this))
  {
#if defined(__WXMAC__)
    dc->SetPen(wxNullPen); // wxmac doesn't like a border with wxXOR
#else
    dc->SetPen(*wxYELLOW_PEN);
// window linux, set a pen
#endif
    dc->SetBrush(*wxYELLOW_BRUSH);
    drawBracket = true;
  }
  else
  {
    dc->SetBrush(*wxWHITE_BRUSH);
    dc->SetPen(*(wxThePenList->FindOrCreatePen(
            *wxWHITE,
            configuration->GetDefaultLineWidth(),
            wxPENSTYLE_SOLID)
    ));
  }
  wxRect rect = GetRect();
  rect = wxRect(
          configuration->GetIndent() - configuration->GetCellBracketWidth(),
          rect.GetTop() - 2,
          configuration->GetCellBracketWidth(),
          rect.GetHeight() + 5
  );
  if (MathCell::InUpdateRegion(rect))
    dc->DrawRectangle(MathCell::CropToUpdateRegion(rect));

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

    wxRect rect = GetRect();
    rect = wxRect(
            configuration->GetIndent() - configuration->GetCellBracketWidth(),
            rect.GetTop() - 2,
            configuration->GetCellBracketWidth(),
            rect.GetHeight() + 5);
    if (MathCell::InUpdateRegion(rect))
      dc->DrawRectangle(rect);
  }

  MathCell *editable = GetEditable();
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

  if ((!m_hide) && (!m_hiddenTree))
  {
    dc->SetBrush(*wxTRANSPARENT_BRUSH);
  }

  if (drawBracket)
  {
    adc->SetBrush(dc->GetBrush());
    int bracketWidth = configuration->GetCellBracketWidth() - configuration->GetDefaultLineWidth();
    if (IsFoldable())
    { // draw the square that allows hiding and unhiding the cell
      wxPoint *points = new wxPoint[4];
      points[0].x = m_currentPoint.x - bracketWidth;
      points[0].y = m_currentPoint.y - m_center;
      points[1].x = m_currentPoint.x - bracketWidth;
      points[1].y = m_currentPoint.y - m_center + bracketWidth;
      points[2].x = m_currentPoint.x - configuration->GetDefaultLineWidth();
      points[2].y = m_currentPoint.y - m_center + bracketWidth;
      points[3].x = m_currentPoint.x - configuration->GetDefaultLineWidth();
      points[3].y = m_currentPoint.y - m_center;
      adc->DrawPolygon(4, points);
      delete[] points;
    }
    else
    { // draw a the triangle that allows hiding and unhiding the cell
      wxPoint *points = new wxPoint[3];
      points[0].x = m_currentPoint.x - bracketWidth;
      points[0].y = m_currentPoint.y - m_center + configuration->GetDefaultLineWidth() / 2;
      points[1].x = m_currentPoint.x - bracketWidth;
      points[1].y = m_currentPoint.y - m_center + bracketWidth - configuration->GetDefaultLineWidth() / 2;
      points[2].x = m_currentPoint.x - configuration->GetDefaultLineWidth();
      points[2].y = m_currentPoint.y - m_center + configuration->GetDefaultLineWidth() / 2;
      adc->DrawPolygon(3, points);
      delete[] points;

      // The vertical line at the back of the bracket
      adc->DrawLine(m_currentPoint.x - bracketWidth,
                  m_currentPoint.y - m_center + configuration->GetDefaultLineWidth() / 2,
                  m_currentPoint.x - bracketWidth,
                  m_currentPoint.y - m_center + m_height - configuration->GetDefaultLineWidth());
      // bottom horizontal line
      adc->DrawLine(m_currentPoint.x - bracketWidth,
                  m_currentPoint.y - m_center + m_height - configuration->GetDefaultLineWidth(),
                  m_currentPoint.x - configuration->GetDefaultLineWidth(),
                  m_currentPoint.y - m_center + m_height - configuration->GetDefaultLineWidth());
      // middle horizontal
      if (configuration->ShowCodeCells() && m_groupType == GC_TYPE_CODE && m_output != NULL && !m_hide)
      {
        adc->DrawLine(m_currentPoint.x - bracketWidth / 2,
                    m_currentPoint.y - m_center + m_inputLabel->GetMaxHeight() +
                    configuration->GetDefaultLineWidth() / 2,
                    m_currentPoint.x - bracketWidth,
                    m_currentPoint.y - m_center + m_inputLabel->GetMaxHeight() +
                    configuration->GetDefaultLineWidth() / 2);
      }
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

  if (m_output != NULL && !m_hide)
  {
    MathCell *tmp = m_output;
    bool firstCell = true;
    while (tmp != NULL)
    {
      if (firstCell || (tmp->ForceBreakLineHere() && str.Length() > 0))
      {
        if (firstCell)
          str += wxT("\n");
        else
        {
          str += wxT("\n");
        }
      }
      str += tmp->ToString();
      firstCell = false;
      tmp = tmp->m_nextToDraw;
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

  MathCell *out = GetLabel();
  if (out != NULL)
  {
    retval += out->ListToRTF(true);
  }
  return retval;
}

wxString GroupCell::ToTeX(wxString imgDir, wxString filename, int *imgCounter)
{
  wxASSERT_MSG((imgCounter != NULL), _("Bug: No image counter to write to!"));
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
        MathCell *copy = m_output->Copy();
        (*imgCounter)++;
        wxString image = filename + wxString::Format(wxT("_%d"), *imgCounter);
        wxString file = imgDir + wxT("/") + image + wxT(".") + dynamic_cast<ImgCell *>(copy)->GetExtension();

        if (!wxDirExists(imgDir))
          wxMkdir(imgDir);

        if (dynamic_cast<ImgCell *>(copy)->ToImageFile(file).x >= 0)
        {
          str << wxT("\\begin{figure}[htb]\n")
              << wxT("  \\begin{center}\n")
              << wxT("    \\includeimage{")
              << filename << wxT("_img/") << image << wxT("}\n")
              << wxT("  \\caption{") << m_inputLabel->m_next->ToTeX() << wxT("}\n")
              << wxT("  \\end{center}\n")
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
      if (GetEditable() != NULL && !m_hide)
      {
        str = GetEditable()->ListToTeX();
        switch (GetEditable()->GetStyle())
        {
          case TS_TITLE:
            str = wxT("\n\\pagebreak{}\n{\\Huge {\\sc ") + str + wxT("}}\n");
            str += wxT("\\setcounter{section}{0}\n\\setcounter{subsection}{0}\n");
            str += wxT("\\setcounter{figure}{0}\n");
            break;
          case TS_SECTION:
            str = wxT("\n\\section{") + str + wxT("}\n");
            break;
          case TS_SUBSECTION:
            str = wxT("\n\\subsection{") + str + wxT("}\n");
            break;
          case TS_SUBSUBSECTION:
            str = wxT("\n\\subsubsection{") + str + wxT("}\n");
            break;
          case TS_HEADING5:
            str = wxT("\n\\paragraph{") + str + wxT("}\n");
            break;
          case TS_HEADING6:
            str = wxT("\n\\subparagraph{") + str + wxT("}\n");
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
    str = wxT("\n\n\\noindent\n%%%%%%%%%%%%%%%\n")
      wxT("%%% INPUT:\n") +
      wxString::Format(
        wxT("\\begin{minipage}[t]{%iem}\\color{red}\\bf\n"),
        configuration->GetLabelWidth()/14
        ) +
      m_inputLabel->ToTeX() +
      wxT("\n\\end{minipage}");
    if (m_inputLabel->m_next != NULL)
    {

      wxString input = m_inputLabel->m_next->ToTeX();
      str += wxT("\n\\begin{minipage}[t]{\\textwidth}\\color{blue}\n") +
             input +
             wxT("\n\\end{minipage}");
    }
  }

  if (m_output != NULL)
  {
    str += wxT("\n%%% OUTPUT:\n");
    // Need to define labelcolor if this is Copy as LaTeX!
    if (imgCounter == NULL)
      str += wxT("\\definecolor{labelcolor}{RGB}{100,0,0}\n");

    MathCell *tmp = m_output;

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

      tmp = tmp->m_nextToDraw;
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

wxString GroupCell::ToTeXImage(MathCell *tmp, wxString imgDir, wxString filename, int *imgCounter)
{
  wxASSERT_MSG((imgCounter != NULL), _("Bug: No image counter to write to!"));
  if (imgCounter == NULL) return wxEmptyString;

  wxString str;

  if (imgDir != wxEmptyString)
  {
    MathCell *copy = tmp->Copy();
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
      for(std::list<wxString>::iterator it = m_knownAnswers.begin(); it != m_knownAnswers.end();++it)
      {
        i++;
        // In theory the attribute should be saved and read verbatim, with the exception
        // of the characters XML wants to be quoted. In reality wxWidget's newline handling
        // seems to be broken => escape newlines.
        wxString answer = MathCell::XMLescape(*it);
        answer.Replace(wxT("\n"),wxT("&#10;"));
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
  if (m_hide)
    str += wxT(" hide=\"true\"");
  str += wxT(">\n");

  MathCell *input = GetInput();
  MathCell *output = GetLabel();
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
      MathCell *tmp = output;
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

void GroupCell::SelectRectGroup(wxRect &rect, wxPoint &one, wxPoint &two,
                                MathCell **first, MathCell **last)
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
  else if (m_output != NULL && !m_hide && m_outputRect.Contains(rect))
    SelectRectInOutput(rect, one, two, first, last);

  if (*first == NULL || *last == NULL)
  {
    *first = this;
    *last = this;
  }
}

void GroupCell::SelectInner(wxRect &rect, MathCell **first, MathCell **last)
{
  *first = NULL;
  *last = NULL;

  if (m_inputLabel->ContainsRect(rect))
    m_inputLabel->SelectRect(rect, first, last);
  else if (m_output != NULL && !m_hide && m_outputRect.Contains(rect))
    m_output->SelectRect(rect, first, last);

  if (*first == NULL || *last == NULL)
  {
    *first = this;
    *last = this;
  }
}

void GroupCell::SelectPoint(wxPoint &point, MathCell **first, MathCell **last)
{
  *first = NULL;
  *last = NULL;

  wxRect rect(point.x, point.y, 1, 1);

  if (m_inputLabel->ContainsRect(rect))
    m_inputLabel->SelectInner(rect, first, last);
}

void GroupCell::SelectRectInOutput(wxRect &rect, wxPoint &one, wxPoint &two,
                                   MathCell **first, MathCell **last)
{
  if (m_hide)
    return;

  MathCell *tmp;
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
  tmp = m_output;
  *first = *last = NULL;

  while (tmp != NULL && !rect.Intersects(tmp->GetRect()))
    tmp = tmp->m_nextToDraw;
  *first = tmp;
  *last = tmp;
  while (tmp != NULL)
  {
    if (rect.Intersects(tmp->GetRect()))
      *last = tmp;
    tmp = tmp->m_nextToDraw;
  }

  if (*first != NULL && *last != NULL)
  {

    // If selection is on multiple lines, we need to correct it
    if ((*first)->GetCurrentY() != (*last)->GetCurrentY())
    {
      tmp = *last;
      MathCell *curr;

      // Find the first cell in selection
      while (*first != tmp &&
             ((*first)->GetCurrentX() + (*first)->GetWidth() < start.x
              || (*first)->GetCurrentY() + (*first)->GetDrop() < start.y))
        *first = (*first)->m_nextToDraw;

      // Find the last cell in selection
      curr = *last = *first;
      while (1)
      {
        curr = curr->m_nextToDraw;
        if (curr == NULL)
          break;
        if (curr->GetCurrentX() <= end.x &&
            curr->GetCurrentY() - curr->GetMaxCenter() <= end.y)
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
  
  wxString retval;

  if (m_hide)
    return retval;
  
  MathCell *tmp = m_output;
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

EditorCell *GroupCell::GetEditable()
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

void GroupCell::BreakLines(int fullWidth)
{
  BreakLines(m_output, fullWidth);
}

void GroupCell::BreakLines(MathCell *cell, int fullWidth)
{
  Configuration *configuration = (*m_configuration);
  int currentWidth = configuration->GetIndent();

  MathCell *tmp = cell;

  while (tmp != NULL && !m_hide)
  {
    tmp->ResetData();
    tmp->BreakLine(false);
    if (!tmp->m_isBrokenIntoLines)
    {
      if (tmp->BreakLineHere() || (currentWidth + tmp->GetWidth() >= fullWidth))
      {
        currentWidth = configuration->GetIndent() + tmp->GetWidth();
        tmp->BreakLine(true);
      }
      else
        currentWidth += tmp->GetWidth();
    }
    tmp = tmp->m_nextToDraw;
  }
}

void GroupCell::SelectOutput(MathCell **start, MathCell **end)
{
  if (m_hide)
    return;

  *start = m_output;

  while (*start != NULL && ((*start)->GetStyle() != TS_LABEL) && ((*start)->GetStyle() != TS_USERLABEL))
    *start = (*start)->m_nextToDraw;


  if (*start != NULL)
    *start = (*start)->m_nextToDraw;

  *end = *start;

  while (*end != NULL &&
         (*end)->m_nextToDraw != NULL)
    *end = (*end)->m_nextToDraw;

  if (*end == NULL || *start == NULL)
    *end = *start = NULL;
}

void GroupCell::BreakUpCells(int fontsize, int clientWidth)
{
  BreakUpCells(m_output, fontsize, clientWidth);
}

void GroupCell::BreakUpCells(MathCell *cell, int WXUNUSED(fontsize), int clientWidth)
{
  if(cell == NULL)
    return;

  MathCell *tmp = cell;

  bool lineHeightsChanged = false;
    
  while (tmp != NULL && !m_hide)
  {
    if (tmp->GetWidth() +
        (*m_configuration)->GetIndent() +
        Scale_Px((*m_configuration)->GetLabelWidth()) > clientWidth)
    {
      if (tmp->BreakUp())
        lineHeightsChanged = true;
    }
    tmp = tmp->m_nextToDraw;
  }

  if(lineHeightsChanged)
  {
    if(m_output != NULL)
    {
      m_output->ResetSizeList();
      m_output->RecalculateList(m_mathFontSize);
    }
    ResetData();
  }
}

void GroupCell::UnBreakUpCells()
{
  MathCell *tmp = m_output;
  while (tmp != NULL)
  {
    if (tmp->m_isBrokenIntoLines)
    {
      tmp->Unbreak();
    }
    tmp = tmp->m_nextToDraw;
  }
}

// support for hiding text, code cells

void GroupCell::Hide(bool hide)
{
  if (IsFoldable())
    return;

  if (m_hide == hide)
    return;

  m_hide = hide;
  if ((m_groupType == GC_TYPE_TEXT) || (m_groupType == GC_TYPE_CODE))
    GetEditable()->SetFirstLineOnly(m_hide);

  // Don't keep cached versions of scaled images around if they aren't visible at all.
  if (GetLabel())
    GetLabel()->ClearCacheList();

  ResetSize();
  GetEditable()->ResetSize();
}

void GroupCell::SwitchHide()
{
  Hide(!m_hide);
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
    tmp = dynamic_cast<GroupCell *>(tmp->m_next);
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
    cell = dynamic_cast<GroupCell *>(cell->m_next);
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

    GroupCell *tmp = dynamic_cast<GroupCell *>(end->m_next);
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
      m_next = m_nextToDraw = end->m_next;
      end->m_next->m_previous = end->m_next->m_previousToDraw = this;
    }
    else
      m_next = m_nextToDraw = NULL;
    end->m_next = end->m_nextToDraw = NULL;
  }
  
  start->m_previous = start->m_previousToDraw = NULL;
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

  MathCell *next = m_next;

  // sew together this cell with m_hiddenTree
  m_next = m_nextToDraw = m_hiddenTree;
  m_hiddenTree->m_previous = m_hiddenTree->m_previousToDraw = this;

  MathCell *tmp = m_hiddenTree;
  while (tmp->m_next)
    tmp = tmp->m_next;
  // tmp holds the last element of m_hiddenTree
  tmp->m_next = tmp->m_nextToDraw = next;
  if (next)
    next->m_previous = next->m_previousToDraw = tmp;

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
    tmp = dynamic_cast<GroupCell *>(tmp->m_next);
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
    tmp = dynamic_cast<GroupCell *>(tmp->m_next);
  }
  return result;
}

bool GroupCell::IsLesserGCType(int comparedTo)
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

    tmp = dynamic_cast<GroupCell *>(tmp->m_next);
  }
}

bool GroupCell::IsMainInput(MathCell *active)
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
    tmp = dynamic_cast<GroupCell *>(tmp->m_next);
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
    MathCell *cell = NULL;
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
    rect = wxRect(GetRect().GetTopLeft()     + m_visibleRegion.GetTopLeft(),
                  GetRect().GetBottomRight() + m_visibleRegion.GetTopLeft());
    
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
    if(rect.GetBottom() > m_visibleRegion.GetWidth())
      rect.SetBottom(m_visibleRegion.GetWidth());
    if(rect.GetRight() > m_visibleRegion.GetHeight())
      rect.SetRight(m_visibleRegion.GetHeight());
    rect = wxRect(rect.GetTopLeft()+m_worksheetPosition,rect.GetBottomRight()+m_worksheetPosition);
    return wxACC_OK;
  }
  else
  {
    MathCell *child = NULL;
	if (GetChild(elementId, &child) == wxACC_OK)
		return child->GetLocation(rect, 0);
  }
  return wxACC_FAIL;
}

#endif
