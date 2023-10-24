// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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

#include "RecentDocuments.h"
#include <wx/filename.h>

RecentDocuments::RecentDocuments(wxString documentType)
    : m_documentType(documentType) {
    Load();
}

void RecentDocuments::Load() {
    wxConfigBase *config = wxConfig::Get();
    wxString fileName;

    for (int i = 0; i < 30; i++) {
        wxString name = wxString::Format(wxS("RecentDocuments/%s_%d"),
                                         m_documentType.utf8_str(), i);
        wxString filename;
        if (config->Read(name, &filename)) {
            if (filename != wxEmptyString)
                m_listOfFiles.push_back(filename);
        }
    }
    m_listOfFiles.unique();

    Save();
}

wxString RecentDocuments::Get(int num) const {
    std::list<wxString> listOfFiles = m_listOfFiles;
    for (int i = 0; i < num; i++) {
        if (listOfFiles.empty())
            return wxEmptyString;
        else
            listOfFiles.pop_front();
    }
    if (listOfFiles.empty())
        return wxEmptyString;
    else
        return listOfFiles.front();
}

void RecentDocuments::Save() {
    wxConfigBase *config = wxConfig::Get();
    int i = 0;
    for (std::list<wxString>::const_iterator it = m_listOfFiles.begin();
         it != m_listOfFiles.end(); ++it) {
        wxString name = wxString::Format(wxS("RecentDocuments/%s_%d"),
                                         m_documentType.utf8_str(), i);
        i++;
        config->Write(name, *it);
    }
    for (; i < 30; i++) {
        wxString name = wxString::Format(wxS("RecentDocuments/%s_%d"),
                                         m_documentType.utf8_str(), i);
        config->DeleteEntry(name);
    }
}

void RecentDocuments::AddDocument(wxString name) {
    wxFileName fle(name);
    fle.MakeAbsolute();
    if ((fle.GetFullPath() != wxEmptyString) && wxFileExists(fle.GetFullPath()))
        name = fle.GetFullPath();

    std::list<wxString>::const_iterator it = m_listOfFiles.begin();
    while (it != m_listOfFiles.end()) {
        if (*it == name)
            it = m_listOfFiles.erase(it);
        else
            ++it;
    }

    m_listOfFiles.push_front(name);
    m_listOfFiles.unique();
    long recentItems = 10;
    wxConfig::Get()->Read(wxS("recentItems"), &recentItems);
    if (recentItems < 5)
        recentItems = 5;
    if (recentItems > 30)
        recentItems = 30;
    while (m_listOfFiles.size() > (unsigned long)recentItems)
        m_listOfFiles.pop_back();
    Save();
}
