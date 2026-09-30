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
  The viewer a double-click on a partially shown matrix opens (GH #2344).
*/

#include "MatrixViewer.h"
#include "Configuration.h"
#include "MathParser.h"
#include "cells/CellList.h"
#include "cells/GroupCell.h"
#include "cells/MatrCell.h"
#include "dialogs/FindReplaceDialog.h"
#include "dialogs/LoggingMessageDialog.h"
#include "worksheet/OutputNavigation.h"
#include "worksheet/Worksheet.h"
#include "worksheet/WorksheetSearch.h"
#include <wx/display.h>
#include <algorithm>

MatrixViewer::MatrixViewer(wxWindow *parent, const MatrCell &matrix,
                           Configuration *config)
  : wxFrame(parent, wxID_ANY,
            wxString::Format(_("Matrix (%lu rows, %lu columns)"),
                             static_cast<unsigned long>(matrix.GetMatrixRows()),
                             static_cast<unsigned long>(matrix.GetMatrixColumns()))),
    m_configuration(std::make_unique<Configuration>(*config)) {
  // The worksheet's constructor points the configuration at it, so it has to
  // exist before the copy of the matrix is parsed: cells find their worksheet
  // through their configuration.
  m_worksheet = new Worksheet(this, wxID_ANY, m_configuration.get(),
                              wxDefaultPosition, wxDefaultSize,
                              /*reactToEvents=*/false);
  // Only now: the worksheet's constructor reads the config file into the
  // configuration, which would undo anything set before.
  ConfigureForViewer(*m_configuration);
  auto *sizer = new wxBoxSizer(wxVERTICAL);
  sizer->Add(m_worksheet, 1, wxEXPAND);
  SetSizer(sizer);

  // No undo buffer: nothing in the viewer can be undone.
  m_worksheet->InsertGroupCells(CopyForViewer(matrix, m_configuration.get()),
                                nullptr, nullptr);
  // Nothing can be inserted, so there is no place for the horizontal cursor
  // between cells to mark.
  m_worksheet->DeactivateHCaret();
  FitToMatrix();

  // Start from where the worksheet's own search left off.
  m_findData.LoadFromConfig();

  Bind(wxEVT_IDLE, &MatrixViewer::OnIdle, this);
  Bind(wxEVT_CHAR_HOOK, &MatrixViewer::OnCharHook, this);
  Bind(wxEVT_FIND_NEXT, &MatrixViewer::OnFind, this);
}

MatrixViewer::~MatrixViewer() {
  // The search dialog is a child window, too, and its pane uses m_findData,
  // a member, while it is destroyed. Its destructor resets m_findDialog.
  delete m_findDialog;
  // Child windows are only destroyed by wxWindow's destructor, i.e. after
  // m_configuration, which the worksheet and its cells still use while they
  // are destroyed. Destroy the worksheet first, as DiffFrame does.
  if (m_worksheet)
    m_worksheet->Destroy();
  m_worksheet = nullptr;
}

void MatrixViewer::ConfigureForViewer(Configuration &config) {
  // Never write these settings back to the config file: they are the
  // viewer's, not the user's.
  config.MakeTemporary();
  config.SetOversizedMatrices(Configuration::OversizedMatrices::showInFull,
                              /*perMatrixOverridable=*/false);
  config.ShowCodeCells(false);
  config.SetLabelChoice(Configuration::labels_none);
  // The matrix is all there is, so it starts at the left margin: no room for
  // the (hidden) labels in front of output, and only a little air where the
  // worksheet keeps a column for the cell brackets. The same margin as at
  // the top, which is GetBaseIndent(). Nothing can be selected or evaluated
  // here, so there is nothing a cell bracket could show, either.
  config.IndentMaths(false);
  config.SetIndent(config.GetBaseIndent());
  config.HideBrackets(true);
}

std::unique_ptr<GroupCell> MatrixViewer::CopyForViewer(const MatrCell &matrix,
                                                       Configuration *config) {
  auto group = std::make_unique<GroupCell>(config, GC_TYPE_CODE);
  MathParser parser(config);
  parser.SetGroup(group.get());
  // A group's first output cell is its label (see GroupCell::AppendOutput()),
  // so the matrix needs one in front of it, or it would become the label
  // itself. An empty one is enough: the viewer doesn't show labels anyway.
  auto output = parser.ParseLine(wxS("<mth><lbl> </lbl>") + matrix.ToXML() +
                                 wxS("</mth>"));
  // The first cell after the label is the matrix. It gets no brackets --
  // the viewer's title already says it is a matrix, and at this size they
  // only take room -- and alternating bands, which make a row or column
  // easier to follow across a matrix this large, even though the viewer
  // shows it in full. Only the outermost matrix: a nested one keeps its
  // brackets, which are what separates it from its neighbours.
  if (output && output->GetNext())
    if (auto *copied = dynamic_cast<MatrCell *>(output->GetNext())) {
      copied->NoParens();
      copied->AlwaysBanded(true);
    }
  if (output)
    group->AppendOutput(std::move(output));
  return group;
}

MatrCell *MatrixViewer::PartiallyShownMatrixAt(const GroupCell *group,
                                               wxPoint point) {
  // Hidden output isn't drawn, so its cells' positions are stale.
  if (!group || group->IsHidden() || !group->ContainsPointOrOutput(point))
    return nullptr;
  // From the label slot on, not from GetOutput(): output that has no label,
  // like a disp()layed matrix read back from a file, keeps its first cell in
  // that slot, and it is drawn just the same.
  return PartiallyShownMatrixIn(group->GetLabel(), point);
}

MatrCell *MatrixViewer::PartiallyShownMatrixIn(Cell *list, wxPoint point) {
  for (Cell &cell : OnList(list)) {
    if (auto *matrix = dynamic_cast<MatrCell *>(&cell)) {
      // A matrix's entries lie inside it, except the ones it leaves out,
      // which keep whatever position they had before: don't look for a
      // (nested) matrix there.
      if (!matrix->ContainsPoint(point))
        continue;
      if (matrix->IsShownPartially())
        return matrix;
    }
    for (Cell &inner : OnInner(&cell))
      if (auto *found = PartiallyShownMatrixIn(&inner, point))
        return found;
  }
  return nullptr;
}

void MatrixViewer::FitToMatrix() {
  m_worksheet->RecalculateIfNeeded();
  int width = 0, height = 0;
  m_worksheet->GetMaxPoint(&width, &height);
  // Room for the worksheet's own scrollbars, which it may still need, and a
  // little air around the matrix.
  const int scrollbar = wxSystemSettings::GetMetric(wxSYS_VSCROLL_X, this);
  const int margin = m_configuration->GetBaseIndent();
  wxSize wanted(width + scrollbar + margin, height + scrollbar + margin);

  // Don't grow beyond most of the screen -- the worksheet scrolls -- and
  // don't shrink to something too small to grab, either.
  int display = wxDisplay::GetFromWindow(GetParent() ? GetParent() : this);
  if (display == wxNOT_FOUND)
    display = 0;
  const wxRect screen = wxDisplay(static_cast<unsigned int>(display)).GetClientArea();
  wanted.x = std::clamp(wanted.x, 300, std::max(300, screen.width * 9 / 10));
  wanted.y = std::clamp(wanted.y, 200, std::max(200, screen.height * 9 / 10));
  SetClientSize(wanted);
}

void MatrixViewer::OnIdle(wxIdleEvent &event) {
  event.Skip();
  if (!m_worksheet)
    return;
  // The same steps wxMaxima's own idle handler takes for the main worksheet,
  // which nothing else does for this one.
  m_worksheet->AdjustSize();
  if (m_worksheet->RecalculateIfNeeded(true)) {
    event.RequestMore();
    return;
  }
  IncrementalSearch();
  if (m_worksheet->RedrawIfRequested())
    event.RequestMore();
}

void MatrixViewer::OnCharHook(wxKeyEvent &event) {
  if (event.GetKeyCode() == WXK_ESCAPE)
    Close();
  // wxMOD_CONTROL is Cmd on macOS, as for the main window's Ctrl+F.
  else if ((event.GetModifiers() == wxMOD_CONTROL) &&
           (event.GetKeyCode() == 'F'))
    OpenFindDialog();
  else
    event.Skip();
}

MatrCell *MatrixViewer::GetMatrix() const {
  if (!m_worksheet || !m_worksheet->GetTree())
    return nullptr;
  // GetOutput() skips the (empty) label CopyForViewer() puts in front.
  return dynamic_cast<MatrCell *>(m_worksheet->GetTree()->GetOutput());
}

std::optional<MatrixEntry> MatrixViewer::SelectedEntry() const {
  if (!m_worksheet)
    return std::nullopt;
  const auto item = m_worksheet->SelectedOutputItem();
  if (!item || !item->IsMatrixEntry() || (item->matrix != GetMatrix()))
    return std::nullopt;
  return item->entry;
}

void MatrixViewer::OpenFindDialog() {
  if (!m_findDialog) {
    // A child of the viewer, so it stays in front of it and closes with it.
    // Search only: nothing in the viewer can be replaced, and there is no
    // input to search in.
    new FindReplaceDialog(this, &m_findData, _("Find in Matrix"),
                          &m_findDialog,
                          wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER,
                          /*searchOnly=*/true);
    // The dialog's outermost parent is wxMaxima's main window, which would
    // search the main worksheet instead.
    m_findDialog->GetPane()->SetEventTarget(this);
  }
  m_searchOrigin = SelectedEntry();
  // Don't search for what is already in the search box until it changes.
  m_oldFindString = m_findData.GetFindString();
  m_oldFindFlags = m_findData.GetFlags();
  m_oldRegexSearch = m_findData.GetRegexSearch();
  m_findDialog->Show();
  m_findDialog->Raise();
  m_findDialog->SetFocus();
}

void MatrixViewer::OnFind(wxFindDialogEvent &event) {
  FindNext(event.GetFindString(), !!(event.GetFlags() & wxFR_DOWN),
           !(event.GetFlags() & wxFR_MATCHCASE), m_findData.GetRegexSearch());
  // The next incremental search refines this match.
  m_searchOrigin = SelectedEntry();
  if (m_findDialog)
    CallAfter([this] {
      if (m_findDialog)
        m_findDialog->SetFocus();
    });
}

void MatrixViewer::IncrementalSearch() {
  if (!m_findDialog || !m_findDialog->IsShown() ||
      !m_configuration->IncrementalSearch())
    return;
  if ((m_oldFindString == m_findData.GetFindString()) &&
      (m_oldFindFlags == m_findData.GetFlags()) &&
      (m_oldRegexSearch == m_findData.GetRegexSearch()))
    return;
  m_oldFindString = m_findData.GetFindString();
  m_oldFindFlags = m_findData.GetFlags();
  m_oldRegexSearch = m_findData.GetRegexSearch();

  MatrCell *matrix = GetMatrix();
  if (!matrix || m_oldFindString.IsEmpty())
    return;
  // Starting at the origin, which may match itself: typing another letter
  // keeps the match as long as it still matches.
  const bool down = !!(m_oldFindFlags & wxFR_DOWN);
  std::optional<MatrixEntry> found;
  if (m_oldRegexSearch) {
    wxLogNull suppressor; // An incomplete regex is no error while typing
    const WorksheetSearch::RegexMatcher matcher(m_oldFindString);
    if (matcher.IsValid())
      found = FindEntry(*matrix, m_searchOrigin, down, true, matcher);
  } else {
    const WorksheetSearch::StringMatcher matcher(
      m_oldFindString, !(m_oldFindFlags & wxFR_MATCHCASE));
    found = FindEntry(*matrix, m_searchOrigin, down, true, matcher);
  }
  if (found)
    SelectEntry(*found);
}

void MatrixViewer::SelectEntry(const MatrixEntry &entry) {
  MatrCell *matrix = GetMatrix();
  if (!matrix)
    return;
  m_worksheet->SelectOutputItem(OutputNavigation::Entry(matrix, entry));

  // SelectOutputItem() only schedules a scroll to the cell, which here is the
  // whole matrix -- and, the matrix being the whole worksheet, doesn't bring
  // the entry into view. Scroll to the entry itself instead, just as far as
  // needed, with a little air around it.
  m_worksheet->RecalculateIfNeeded();
  m_worksheet->GetTree()->UpdateOutputPositions();
  wxRect rect = matrix->BlockRect(MatrixBlock::Spanning(entry, entry));
  if (rect.IsEmpty())
    return;
  const int margin = m_configuration->GetBaseIndent();
  rect.Inflate(margin, margin);
  int unitX = 1, unitY = 1;
  m_worksheet->GetScrollPixelsPerUnit(&unitX, &unitY);
  unitX = std::max(unitX, 1);
  unitY = std::max(unitY, 1);
  int viewX = 0, viewY = 0;
  m_worksheet->GetViewStart(&viewX, &viewY);
  viewX *= unitX;
  viewY *= unitY;
  const wxSize client = m_worksheet->GetClientSize();
  // Moves the view start just far enough that [low, high) is in the view,
  // or, if it is larger than the view, that its start is.
  const auto follow = [](int view, int size, int low, int high) {
    if ((high > view + size) && (high - low <= size))
      view = high - size;
    if ((low < view) || (high - low > size))
      view = low;
    return std::max(view, 0);
  };
  const int newX = follow(viewX, client.x, rect.GetLeft(), rect.GetRight() + 1);
  const int newY = follow(viewY, client.y, rect.GetTop(), rect.GetBottom() + 1);
  // Scrolling happens in whole scroll units: round towards the direction
  // the view moves in, or the far edge of the entry stays just out of view.
  const auto units = [](int from, int to, int unit) {
    return (to > from) ? (to + unit - 1) / unit : to / unit;
  };
  if ((newX != viewX) || (newY != viewY))
    m_worksheet->Scroll(units(viewX, newX, unitX), units(viewY, newY, unitY));
}

bool MatrixViewer::FindNext(const wxString &str, bool down, bool ignoreCase,
                            bool regex, bool warn) {
  MatrCell *matrix = GetMatrix();
  if (!matrix || str.IsEmpty())
    return false;
  std::optional<MatrixEntry> found;
  bool wrapped = false;
  if (regex) {
    const WorksheetSearch::RegexMatcher matcher(str);
    if (matcher.IsValid())
      found = FindEntry(*matrix, SelectedEntry(), down, false, matcher, &wrapped);
  } else {
    const WorksheetSearch::StringMatcher matcher(str, ignoreCase);
    found = FindEntry(*matrix, SelectedEntry(), down, false, matcher, &wrapped);
  }
  if (!found) {
    if (warn)
      LoggingMessageBox(_("No matches found!"), wxMessageBoxCaptionStr,
                        wxOK | wxCENTRE,
                        m_findDialog ? static_cast<wxWindow *>(m_findDialog) : this);
    return false;
  }
  SelectEntry(*found);
  if (wrapped && warn) {
    LoggingMessageDialog dialog(m_findDialog ? static_cast<wxWindow *>(m_findDialog) : this,
                                _("Wrapped search"), wxEmptyString,
                                wxCENTER | wxOK);
    dialog.ShowModal();
  }
  return true;
}

std::optional<MatrixEntry>
MatrixViewer::FindEntry(const MatrCell &matrix, std::optional<MatrixEntry> start,
                        bool down, bool inclusive,
                        const WorksheetSearch::Matcher &matcher, bool *wrapped) {
  if (wrapped)
    *wrapped = false;
  const std::size_t cols = matrix.GetMatrixColumns();
  const std::size_t count = matrix.GetMatrixRows() * cols;
  if ((count == 0) || (matrix.GetInnerCellCount() < count))
    return std::nullopt;

  // The entries in reading order, row by row, are the matrix's inner cells
  // in order. Walk them as a ring: `step` entries away from the start, the
  // start itself last (a full round) or, if inclusive, first.
  std::size_t origin;
  std::size_t firstStep = inclusive ? 0 : 1;
  std::size_t lastStep = inclusive ? count - 1 : count;
  if (start && (start->row * cols + start->col < count))
    origin = start->row * cols + start->col;
  else {
    // Before the first entry or after the last one: step 1 is the first
    // (or the last) entry, and a search from there never wraps.
    origin = down ? count - 1 : 0;
    firstStep = 1;
    lastStep = count;
    start.reset();
  }
  for (std::size_t step = firstStep; step <= lastStep; ++step) {
    const std::size_t index = down ? (origin + step) % count
                                   : (origin + count - step % count) % count;
    const Cell *entry = matrix.GetInnerCell(index);
    if (!entry || !matcher.Matches(entry->ListToString()))
      continue;
    if (wrapped && start)
      *wrapped = down ? (index <= origin) : (index >= origin);
    if (wrapped && start && inclusive && (index == origin))
      *wrapped = false;
    return MatrixEntry{index / cols, index % cols};
  }
  return std::nullopt;
}
