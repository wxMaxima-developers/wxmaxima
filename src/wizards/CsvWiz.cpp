// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2020      Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file contains all the wizards the draw sidepane needs.
*/

#include "CsvWiz.h"
#include <wx/arrstr.h>
#include <wx/config.h>
#include <wx/persist/toplevel.h>

CsvImportWiz::CsvImportWiz(wxWindow *parent, Configuration *config)
    : wxDialog(parent, -1, _("CSV import"), wxDefaultPosition, wxDefaultSize,
               wxMINIMIZE_BOX | wxMAXIMIZE_BOX | wxRESIZE_BORDER | wxCAPTION |
               wxCLOSE_BOX | wxCLIP_CHILDREN) {
    wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);
    vbox->Add(new wxStaticText(this, -1, _("Field Separator")), wxSizerFlags());
    wxArrayString choices;
    choices.Add(_("Semicolon"));
    choices.Add(_("Comma"));
    choices.Add(_("Tab"));
    m_separator =
        new wxChoice(this, -1, wxDefaultPosition, wxDefaultSize, choices);
    long choice = 0;
    wxConfig::Get()->Read(wxS("csvSeparator"), choice);
    if (choice < 0)
        choice = 0;
    if (choice > 2)
        choice = 2;
    m_separator->SetSelection(choice);
    vbox->Add(m_separator, wxSizerFlags().Expand());
    wxBoxSizer *hbox = new wxBoxSizer(wxHORIZONTAL);
    vbox->Add(new wxStaticText(this, -1, _("Filename")), wxSizerFlags());
    m_filename = new BTextCtrl(this, -1, config, wxEmptyString);
    hbox->Add(m_filename, wxSizerFlags(20));
    m_browseButton = new wxButton(this, -1, _("Browse"));
    m_browseButton->Connect(
        wxEVT_BUTTON, wxCommandEventHandler(CsvImportWiz::OnBrowse), NULL, this);
    hbox->Add(m_browseButton, wxSizerFlags());
    vbox->Add(hbox, wxSizerFlags(20).Expand());

    wxBoxSizer *buttonSizer = new wxBoxSizer(wxHORIZONTAL);

    wxButton *okButton = new wxButton(this, wxID_OK, _("OK"));
    wxButton *cancelButton = new wxButton(this, wxID_CANCEL, _("Cancel"));
#if defined __WXMSW__
    buttonSizer->Add(okButton);
    buttonSizer->Add(cancelButton);
#else
    buttonSizer->Add(cancelButton);
    buttonSizer->Add(okButton);
#endif
    okButton->SetDefault();
    vbox->Add(buttonSizer, wxSizerFlags().Right());
    wxPersistenceManager::Get().RegisterAndRestore(this);
    SetSizerAndFit(vbox);
}

wxString CsvImportWiz::GetSeparator() {
    switch (m_separator->GetSelection()) {
    case 0:
        return "'semicolon";
    case 1:
        return "'comma";
    default:
        return "'tab";
    }
}

void CsvImportWiz::OnBrowse(wxCommandEvent &WXUNUSED(event)) {
    auto file = wxFileSelector(
        _("Select csv file to read"), {}, {}, {},
        _("Csv files (*.csv)|*.csv|Text files (*.txt)|*.txt|All|*"), wxFD_OPEN);

    if (!file.empty())
        m_filename->SetValue(file);
}

CsvImportWiz::~CsvImportWiz() {
    wxConfig::Get()->Write(wxS("csvSeparator"), m_separator->GetSelection());
}

CsvExportWiz::CsvExportWiz(wxWindow *parent, Configuration *config,
                           wxString objectType)
    : wxDialog(parent, -1, _("CSV export"), wxDefaultPosition, wxDefaultSize,
               wxMINIMIZE_BOX | wxMAXIMIZE_BOX | wxRESIZE_BORDER | wxCAPTION |
               wxCLOSE_BOX | wxCLIP_CHILDREN) {
    wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);
    vbox->Add(new wxStaticText(this, -1, _("Field Separator")), wxSizerFlags());
    wxArrayString choices;
    choices.Add(_("Semicolon"));
    choices.Add(_("Comma"));
    choices.Add(_("Tab"));
    m_separator =
        new wxChoice(this, -1, wxDefaultPosition, wxDefaultSize, choices);
    long choice = 0;
    wxConfig::Get()->Read(wxS("csvSeparator"), choice);
    if (choice < 0)
        choice = 0;
    if (choice > 2)
        choice = 2;
    m_separator->SetSelection(choice);
    vbox->Add(m_separator, wxSizerFlags().Expand());
    vbox->Add(new wxStaticText(this, -1, objectType), wxSizerFlags());
    m_matrix = new BTextCtrl(this, -1, config, wxEmptyString);
    vbox->Add(m_matrix, wxSizerFlags().Expand());
    wxBoxSizer *hbox = new wxBoxSizer(wxHORIZONTAL);
    vbox->Add(new wxStaticText(this, -1, _("Filename")), wxSizerFlags());
    m_filename = new BTextCtrl(this, -1, config, wxEmptyString);
    hbox->Add(m_filename, wxSizerFlags(20));
    m_browseButton = new wxButton(this, -1, _("Browse"));
    m_browseButton->Connect(
        wxEVT_BUTTON, wxCommandEventHandler(CsvExportWiz::OnBrowse), NULL, this);
    hbox->Add(m_browseButton, wxSizerFlags());
    vbox->Add(hbox, wxSizerFlags(20).Expand());

    wxBoxSizer *buttonSizer = new wxBoxSizer(wxHORIZONTAL);

    wxButton *okButton = new wxButton(this, wxID_OK, _("OK"));
    wxButton *cancelButton = new wxButton(this, wxID_CANCEL, _("Cancel"));
#if defined __WXMSW__
    buttonSizer->Add(okButton);
    buttonSizer->Add(cancelButton);
#else
    buttonSizer->Add(cancelButton);
    buttonSizer->Add(okButton);
#endif
    okButton->SetDefault();
    vbox->Add(buttonSizer, wxSizerFlags().Right());
    wxPersistenceManager::Get().RegisterAndRestore(this);
    SetSizerAndFit(vbox);
}

wxString CsvExportWiz::GetSeparator() {
    switch (m_separator->GetSelection()) {
    case 0:
        return "'semicolon";
    case 1:
        return "'comma";
    default:
        return "'tab";
    }
}

void CsvExportWiz::OnBrowse(wxCommandEvent &WXUNUSED(event)) {
    auto file = wxFileSelector(
        _("Select csv file to read"), {}, {}, {},
        _("Csv files (*.csv)|*.csv|Text files (*.txt)|*.txt|All|*"), wxFD_SAVE);

    if (!file.empty())
        m_filename->SetValue(file);
}

CsvExportWiz::~CsvExportWiz() {
    wxConfig::Get()->Write(wxS("csvSeparator"), m_separator->GetSelection());
}
