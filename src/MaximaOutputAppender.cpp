// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2026 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  Implements the worksheet output appenders extracted from the wxMaxima god
  class. See MaximaOutputAppender.h.
*/

#include "MaximaOutputAppender.h"
#include "wxMaxima.h"
#include "worksheet/Worksheet.h"
#include "MathParser.h"
#include "MaximaEvaluator.h"
#include "cells/GroupCell.h"
#include "cells/TextCell.h"
#include "cells/LabelCell.h"
#include "cells/CellList.h"
#include <wx/tokenzr.h>
#include <wx/utils.h>

void MaximaOutputAppender::ConsoleAppend(wxXmlDocument xml, CellType type,
                                         const wxString &userLabel) {
  if(!m_wxMaxima.GetWorksheet())
    return;

  // If we want to append an error message to the worksheet and there is no cell
  // that can contain it we need to create such a cell.
  if (m_wxMaxima.GetWorksheet()->GetTree() == NULL)
    m_wxMaxima.GetWorksheet()->InsertGroupCells(
                                  std::make_unique<GroupCell>(&m_wxMaxima.m_configuration, GC_TYPE_CODE));
  m_wxMaxima.m_dispReadOut = false;
  GroupCell *tmp = m_wxMaxima.GetWorksheet()->GetWorkingGroup(true);

  if (tmp == NULL) {
    if (m_wxMaxima.GetWorksheet()->GetActiveCell())
      tmp = m_wxMaxima.GetWorksheet()->GetActiveCell()->GetGroup();
  }
  if(tmp != NULL)
    {
      m_wxMaxima.m_parser.SetUserLabel(userLabel);
      m_wxMaxima.m_parser.SetGroup(m_wxMaxima.GetWorksheet()->GetInsertGroup());
      std::unique_ptr<Cell> cell(m_wxMaxima.m_parser.ParseLine(xml, type));
      m_wxMaxima.m_parser.SetGroup(nullptr);
      if (!cell)
        {
          // Same contract as DoConsoleAppend: a parse failure must be visible,
          // not silently dropped.
          DoRawConsoleAppend(_("There was an error in the XML maxima has generated.\n"
                               "Please report this as a bug to the wxMaxima project."),
                             MC_TYPE_ERROR);
          m_wxMaxima.m_evaluator.AbortOnError();
          return;
        }
      // The former second argument `(AppendOpt::DefaultOpt & AppendOpt::NewLine)
      // || cell->BreakLineHere()` was constant true (DefaultOpt contains
      // NewLine) - and only that short-circuit kept the read of `cell` next to
      // std::move(cell) from being evaluation-order UB.
      m_wxMaxima.GetWorksheet()->InsertLine(std::move(cell), true);
    }
}

/*! ConsoleAppend adds a new line s of type to the console window.
 *
 * Dispatches on the cell type: plain-text types (default output, errors,
 * warnings, text, ASCII maths) go to DoRawConsoleAppend verbatim; prompts and
 * everything else are wrapped in <span>...</span> and run through the XML
 * parser via DoConsoleAppend. Also enforces the max-output-cells-per-command
 * limit and strips a trailing </PROMPT> tag.
 */
TextCell *MaximaOutputAppender::ConsoleAppend(wxString s, CellType type) {
  if(!m_wxMaxima.GetWorksheet())
    return NULL;

  TextCell *lastLine = NULL;
  // If we want to append an error message to the worksheet and there is no cell
  // that can contain it we need to create such a cell.
  if (m_wxMaxima.GetWorksheet()->GetTree() == NULL)
    m_wxMaxima.GetWorksheet()->InsertGroupCells(
                                  std::make_unique<GroupCell>(&m_wxMaxima.m_configuration, GC_TYPE_CODE));

  m_wxMaxima.m_dispReadOut = false;
  s.Replace(m_wxMaxima.m_promptSuffix, wxEmptyString);

  // If the string we have to append only contains whitespace we return
  // immediately.
  // TODO: Is a printf(false,"~%")$ a real use-case?
  wxString t(s);
  t.Trim();
  t.Trim(false);
  if (t.IsEmpty())
    return NULL;

  if (m_wxMaxima.m_maxOutputCellsPerCommand > 0) {
    // If we already have output more lines than we are allowed, we inform the
    // user about this and return.
    if (m_wxMaxima.m_outputCellsFromCurrentCommand == m_wxMaxima.m_maxOutputCellsPerCommand) {
      {
        DoRawConsoleAppend(_("... [suppressed additional lines as the output "
                             "is longer than allowed in the wxMaxima configuration] "),
                           MC_TYPE_ERROR);
        m_wxMaxima.m_outputCellsFromCurrentCommand++;
      }
      return NULL;
    } else {
      m_wxMaxima.m_outputCellsFromCurrentCommand++;
    }

    // If we already have output more lines than we are allowed to and we
    // already have informed the user about this we return immediately
    if (m_wxMaxima.m_outputCellsFromCurrentCommand > m_wxMaxima.m_maxOutputCellsPerCommand)
      return NULL;
  }

  if ((type != MC_TYPE_ERROR) && (type != MC_TYPE_WARNING))
    m_wxMaxima.StatusMaximaBusy(StatusBar::MaximaStatus::parsing);

  if (type == MC_TYPE_DEFAULT) {
    // Show a busy cursor whilst interpreting and layouting potentially long
    // data from maxima.
    wxBusyCursor crs;

    lastLine = DoRawConsoleAppend(s, type);
  } else if (type == MC_TYPE_PROMPT) {
    m_wxMaxima.m_lastPrompt = s;

    if (s.StartsWith(wxS("MAXIMA> "))) {
      s = s.Right(8);
    } else
      s = s + wxS(" ");

    DoConsoleAppend(wxS("<span>") + s + wxS("</span>"), type,
                    AppendOpt(AppendOpt::NewLine | AppendOpt::BigSkip));
  } else if (type == MC_TYPE_ERROR) {
    lastLine = DoRawConsoleAppend(s, MC_TYPE_ERROR);
    GroupCell *tmp = m_wxMaxima.GetWorksheet()->GetWorkingGroup(true);

    if (tmp == NULL) {
      if (m_wxMaxima.GetWorksheet()->GetActiveCell())
        tmp = m_wxMaxima.GetWorksheet()->GetActiveCell()->GetGroup();
    }

    if (tmp != NULL) {
      m_wxMaxima.GetWorksheet()->GetErrorList().Add(tmp);
      tmp->GetEditable()->SetErrorIndex(m_wxMaxima.m_commandIndex - 1);
    }
  } else if (type == MC_TYPE_WARNING) {
    lastLine = DoRawConsoleAppend(s, MC_TYPE_WARNING);
  } else if (type == MC_TYPE_TEXT) {
    lastLine = DoRawConsoleAppend(s, MC_TYPE_TEXT);
  } else if (type == MC_TYPE_ASCIIMATHS) {
    lastLine = DoRawConsoleAppend(s, MC_TYPE_ASCIIMATHS);
  } else
    DoConsoleAppend(wxS("<span>") + s + wxS("</span>"), type,
                    AppendOpt::BigSkip);

  return lastLine;
}

void MaximaOutputAppender::DoConsoleAppend(wxString s, CellType type, AppendOpt opts,
                                           const wxString &userLabel) {
  if (s.IsEmpty())
    return;

  if(!m_wxMaxima.GetWorksheet())
    return;

  s.Replace(wxS("\n"), wxS(" "), true);

  m_wxMaxima.m_parser.SetUserLabel(userLabel);
  m_wxMaxima.m_parser.SetGroup(m_wxMaxima.GetWorksheet()->GetInsertGroup());
  std::unique_ptr<Cell> cell(m_wxMaxima.m_parser.ParseLine(s, type));
  m_wxMaxima.m_parser.SetGroup(nullptr);

  if (!cell)
    {
      DoRawConsoleAppend(_("There was an error in the XML maxima has generated.\n"
                           "Please report this as a bug to the wxMaxima project."),
                         MC_TYPE_ERROR);
      m_wxMaxima.m_evaluator.AbortOnError();
      return;
    }

  cell->SetBigSkip(opts & AppendOpt::BigSkip);
  auto *textCell = dynamic_cast<TextCell *>(cell.get());
  if (textCell)
    {
      textCell->SetPromptTooltip(opts & AppendOpt::PromptToolTip);
      bool breakLine = cell->BreakLineHere();
      m_wxMaxima.GetWorksheet()->InsertLine(std::move(cell),
                                 (opts & AppendOpt::NewLine) || breakLine);
    }
}

TextCell *MaximaOutputAppender::DoRawConsoleAppend(wxString s, CellType type,
                                                   AppendOpt opts) {
  if(!m_wxMaxima.GetWorksheet())
    return NULL;

  if (s.IsEmpty())
    return NULL;

  if (type == MC_TYPE_ERROR)
    wxLogMessage(wxS("Maxima error: %s"), s);
  else if (type == MC_TYPE_WARNING)
    wxLogMessage(wxS("Maxima warning: %s"), s);

  TextCell *cell = nullptr;
  // If we want to append an error message to the worksheet and there is no cell
  // that can contain it we need to create such a cell.
  if (m_wxMaxima.GetWorksheet()->GetTree() == NULL)
    m_wxMaxima.GetWorksheet()->InsertGroupCells(
                                  std::make_unique<GroupCell>(&m_wxMaxima.m_configuration, GC_TYPE_CODE));

  bool scrollToCaret =
    (!m_wxMaxima.GetWorksheet()->FollowEvaluation() && m_wxMaxima.GetWorksheet()->CaretVisibleIs());

  if (type == MC_TYPE_MAIN_PROMPT) {
    auto owned = std::make_unique<LabelCell>(
                                             m_wxMaxima.GetWorksheet()->GetTree(), &m_wxMaxima.m_configuration, s,
                                             TS_MAIN_PROMPT);
    owned->SetType(type);
    owned->SetPromptTooltip(opts & AppendOpt::PromptToolTip);
    cell = owned.get();
    m_wxMaxima.GetWorksheet()->InsertLine(std::move(owned), true);
  } else if (type == MC_TYPE_PROMPT) {
    auto owned = std::make_unique<TextCell>(
                                             m_wxMaxima.GetWorksheet()->GetTree(), &m_wxMaxima.m_configuration, s,
                                             TS_OTHER_PROMPT);
    owned->SetType(type);
    owned->SetPromptTooltip(opts & AppendOpt::PromptToolTip);
    cell = owned.get();
    m_wxMaxima.GetWorksheet()->InsertLine(std::move(owned), true);
  } else {
    // Prompts never reach this branch - the else-if chain above already
    // handled MC_TYPE_MAIN_PROMPT and MC_TYPE_PROMPT. (A dead inner
    // "if (type == MC_TYPE_PROMPT) new LabelCell" that would have leaked its
    // cell, plus an InsertLine of a never-assigned unique_ptr, used to live
    // here.) If maxima's last line didn't end in a newline its text cell is
    // still incomplete; the first line of the new data completes it.
    TextCell *incompleteTextCell = m_wxMaxima.GetWorksheet()->GetCurrentTextCell();

    if (incompleteTextCell) {
      auto pos = s.Find("\n");
      wxString newVal = incompleteTextCell->GetValue();
      if (pos != wxNOT_FOUND) {
        newVal += s.Left(pos);
        s = s.Right(s.Length() - pos - 1);
      } else {
        newVal += s;
        s.Clear();
      }

      incompleteTextCell->SetValue(newVal);
      if (s.IsEmpty()) {
        return incompleteTextCell;
      }
    }

    wxStringTokenizer tokens(s, wxS("\n"));
    CellListBuilder<Cell> tree;
    while (tokens.HasMoreTokens()) {
      wxString token = tokens.GetNextToken();
      // Move endless streams of compilation messages to the status bar...
      if (m_wxMaxima.m_sbclCompilationRegEx.Matches(token)) {
        wxString fileName = token;
        m_wxMaxima.m_sbclCompilationRegEx.Replace(&fileName, wxS("\\1"));
        m_wxMaxima.StatusText(
                   wxString::Format(_("Compiling %s"), fileName));
      } else {
        auto owned = std::make_unique<TextCell>(m_wxMaxima.GetWorksheet()->GetTree(),
                                                &m_wxMaxima.m_configuration, token);
        owned->SetType(type);
        owned->SetPromptTooltip(opts & AppendOpt::PromptToolTip);
        cell = owned.get();

        if (tokens.HasMoreTokens())
          cell->SetBigSkip(false);

        auto breakLine = static_cast<bool>(tree);
        tree.Append(std::move(owned));
        if (breakLine)
          tree.GetLastAppended()->ForceBreakLine(true);
      }
    }
    m_wxMaxima.GetWorksheet()->InsertLine(std::move(tree), true);
  }

  if (cell) {
    m_wxMaxima.GetWorksheet()->RequestRecalculation(cell->GetGroup());
    if (scrollToCaret)
      m_wxMaxima.GetWorksheet()->ScrollToCaret();
    m_wxMaxima.GetWorksheet()->RequestRedraw();
  }
  return cell;
}
