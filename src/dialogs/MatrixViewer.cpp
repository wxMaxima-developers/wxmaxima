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
#include "worksheet/Worksheet.h"
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

  Bind(wxEVT_IDLE, &MatrixViewer::OnIdle, this);
  Bind(wxEVT_CHAR_HOOK, &MatrixViewer::OnCharHook, this);
}

MatrixViewer::~MatrixViewer() {
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
  if (output)
    group->AppendOutput(std::move(output));
  return group;
}

MatrCell *MatrixViewer::PartiallyShownMatrixAt(const GroupCell *group,
                                               wxPoint point) {
  // Hidden output isn't drawn, so its cells' positions are stale.
  if (!group || group->IsHidden() || !group->GetRect().Contains(point))
    return nullptr;
  return PartiallyShownMatrixIn(group->GetOutput(), point);
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
  if (m_worksheet->RedrawIfRequested())
    event.RequestMore();
}

void MatrixViewer::OnCharHook(wxKeyEvent &event) {
  if (event.GetKeyCode() == WXK_ESCAPE)
    Close();
  else
    event.Skip();
}
