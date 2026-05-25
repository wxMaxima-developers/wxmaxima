// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//  Copyright (C) 2026 Gemini CLI
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

#include "DiffFrame.h"
#include "WXMformat.h"
#include "MathParser.h"
#include <wx/file.h>
#include <wx/wfstream.h>
#include <wx/zipstrm.h>
#include <wx/txtstrm.h>
#include <map>
#include <algorithm>

class SpacerGroupCell : public GroupCell {
public:
  SpacerGroupCell(Configuration *config, wxCoord height)
      : GroupCell(config, GC_TYPE_INVALID), m_forcedHeight(height) {}
  void Recalculate(AFontSize fontsize) const override {
    Cell::Recalculate(fontsize);
    m_height = m_forcedHeight;
    m_width = 0;
    m_center = 0;
  }
  std::unique_ptr<Cell> Copy(GroupCell *parent) const override {
    (void)parent;
    return std::make_unique<SpacerGroupCell>(m_configuration, m_forcedHeight);
  }

private:
  wxCoord m_forcedHeight;
};

DiffFrame::DiffFrame(wxWindow *parent, const wxArrayString &files, Configuration *config)
    : wxFrame(parent, wxID_ANY, _("wxMaxima Diff Viewer"), wxDefaultPosition, wxSize(1000, 800)),
      m_configuration(config) {
  wxBoxSizer *mainSizer = new wxBoxSizer(wxHORIZONTAL);

  for (size_t i = 0; i < files.size(); ++i) {
    Worksheet *ws = new Worksheet(this, wxID_ANY, m_configuration);
    m_worksheets.push_back(ws);
    ws->m_currentFile = files[i];
    mainSizer->Add(ws, 1, wxEXPAND);
    
    ws->Bind(wxEVT_SCROLLWIN_THUMBTRACK, &DiffFrame::OnScroll, this);
    ws->Bind(wxEVT_SCROLLWIN_THUMBRELEASE, &DiffFrame::OnScroll, this);
    ws->Bind(wxEVT_SCROLLWIN_LINEUP, &DiffFrame::OnScroll, this);
    ws->Bind(wxEVT_SCROLLWIN_LINEDOWN, &DiffFrame::OnScroll, this);
    ws->Bind(wxEVT_SCROLLWIN_PAGEUP, &DiffFrame::OnScroll, this);
    ws->Bind(wxEVT_SCROLLWIN_PAGEDOWN, &DiffFrame::OnScroll, this);
  }

  SetSizer(mainSizer);
  AlignCells();
  
  for (auto ws : m_worksheets) {
    ws->Recalculate();
  }
}

DiffFrame::~DiffFrame() {}

void DiffFrame::OnScroll(wxScrollWinEvent &event) {
  if (m_syncing) return;
  m_syncing = true;
  
  int orientation = event.GetOrientation();
  int position = event.GetPosition();
  
  for (auto ws : m_worksheets) {
    if (ws != event.GetEventObject()) {
      ws->SetScrollPos(orientation, position);
      ws->Refresh();
    }
  }
  
  m_syncing = false;
  event.Skip();
}

static std::unique_ptr<GroupCell> LoadTree(const wxString &file, Configuration *config) {
    if (file.Lower().EndsWith(wxS(".wxm"))) {
      wxTextFile text(file);
      if (text.Open()) {
        return Format::ParseWXMFile(text, config);
      }
    } else if (file.Lower().EndsWith(wxS(".wxmx"))) {
        wxXmlDocument xmldoc;
        wxFileInputStream wxmxFile(file);
        if (!wxmxFile.IsOk()) return nullptr;
        wxZipInputStream wxmxContents(wxmxFile);
        while (wxZipEntry *entry = wxmxContents.GetNextEntry()) {
            if (entry->GetName() == wxS("content.xml")) {
                xmldoc.Load(wxmxContents);
                delete entry;
                break;
            }
            delete entry;
        }
        if (xmldoc.IsOk()) {
            MathParser parser(config, file);
            return parser.CreateTreeFromXMLNode(xmldoc.GetRoot());
        }
    }
    return nullptr;
}

void DiffFrame::LoadFiles(const wxArrayString &WXUNUSED(files)) {
}

static std::vector<std::pair<int, int>> Align2(const std::vector<wxString>& s1, const std::vector<wxString>& s2) {
    int n = s1.size();
    int m = s2.size();
    std::vector<std::vector<int>> dp(n + 1, std::vector<int>(m + 1, 0));
    for (int i = 1; i <= n; ++i) {
        for (int j = 1; j <= m; ++j) {
            if (!s1[i - 1].IsEmpty() && s1[i - 1] == s2[j - 1])
                dp[i][j] = dp[i - 1][j - 1] + 1;
            else
                dp[i][j] = std::max(dp[i - 1][j], dp[i][j - 1]);
        }
    }

    std::vector<std::pair<int, int>> alignment;
    int i = n, j = m;
    while (i > 0 || j > 0) {
        if (i > 0 && j > 0 && !s1[i - 1].IsEmpty() && s1[i - 1] == s2[j - 1]) {
            alignment.push_back({i - 1, j - 1});
            i--; j--;
        } else if (j > 0 && (i == 0 || dp[i][j - 1] >= dp[i - 1][j])) {
            alignment.push_back({-1, j - 1});
            j--;
        } else {
            alignment.push_back({i - 1, -1});
            i--;
        }
    }
    std::reverse(alignment.begin(), alignment.end());
    return alignment;
}

void DiffFrame::AlignCells() {
  size_t numFiles = m_worksheets.size();
  if (numFiles < 2) return;

  std::vector<std::unique_ptr<GroupCell>> sourceTrees;
  std::vector<std::vector<GroupCell*>> cellLists;
  std::vector<std::vector<wxString>> uuidLists;

  for (size_t i = 0; i < numFiles; ++i) {
      sourceTrees.push_back(LoadTree(m_worksheets[i]->m_currentFile, m_configuration));
      std::vector<GroupCell*> cells;
      std::vector<wxString> uuids;
      if (sourceTrees.back()) {
          for (auto &c : OnList(sourceTrees.back().get())) {
              cells.push_back(&c);
              uuids.push_back(c.GetUUID());
          }
      }
      cellLists.push_back(cells);
      uuidLists.push_back(uuids);
  }

  std::vector<std::vector<int>> finalAlignment; // rows of indices into each tree

  if (numFiles == 2) {
      auto a = Align2(uuidLists[0], uuidLists[1]);
      for (auto const &p : a) {
          finalAlignment.push_back({p.first, p.second});
      }
  } else if (numFiles == 3) {
      // Align 1 and 2
      auto a12 = Align2(uuidLists[0], uuidLists[1]);
      // Construct a "merged" UUID sequence for 1&2
      std::vector<wxString> uuid12;
      for (auto const &p : a12) {
          if (p.first != -1) uuid12.push_back(uuidLists[0][p.first]);
          else uuid12.push_back(uuidLists[1][p.second]);
      }
      // Align (1&2) and 3
      auto a123 = Align2(uuid12, uuidLists[2]);
      
      int idx12 = 0;
      for (auto const &p : a123) {
          if (p.first != -1) {
              finalAlignment.push_back({a12[idx12].first, a12[idx12].second, p.second});
              idx12++;
          } else {
              finalAlignment.push_back({-1, -1, p.second});
          }
      }
  }

  for (auto ws : m_worksheets) ws->DestroyTree();

  for (auto const &row : finalAlignment) {
      std::vector<wxCoord> heights;
      for (size_t i = 0; i < numFiles; ++i) {
          if (row[i] != -1) {
              cellLists[i][row[i]]->Recalculate(m_configuration->GetDefaultFontSize());
              heights.push_back(cellLists[i][row[i]]->GetHeight());
          }
      }
      wxCoord maxHeight = heights.empty() ? 0 : *std::max_element(heights.begin(), heights.end());

      for (size_t i = 0; i < numFiles; ++i) {
          if (row[i] != -1) {
              auto copy = std::unique_ptr<GroupCell>(dynamic_cast<GroupCell*>(cellLists[i][row[i]]->Copy(nullptr).release()));
              bool modified = false;
              for (size_t j = 0; j < numFiles; ++j) {
                  if (i != j && row[j] != -1) {
                      if (cellLists[i][row[i]]->GetEditable()->GetValue() != cellLists[j][row[j]]->GetEditable()->GetValue())
                          modified = true;
                  }
              }
              if (modified) copy->SetStyle(TS_HIGHLIGHT);
              m_worksheets[i]->InsertGroupCells(std::move(copy), m_worksheets[i]->GetLastCell());
          } else {
              m_worksheets[i]->InsertGroupCells(std::make_unique<SpacerGroupCell>(m_configuration, maxHeight), m_worksheets[i]->GetLastCell());
          }
      }
  }
}


