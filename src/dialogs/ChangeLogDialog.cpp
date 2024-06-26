// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2017-2022 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  A dialog that shows the program's changelog.
*/

#include "ChangeLogDialog.h"
#include "news_md.h"
#include <wx/mstream.h>
#include <wx/string.h>
#include <wx/txtstrm.h>
#include <wx/regex.h>
#include <wx/utils.h>

ChangeLogDialog::ChangeLogDialog(wxWindow *parent)
  : wxDialog(parent, -1, _("ChangeLog"), wxDefaultPosition, wxDefaultSize,
             wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER | wxCLOSE_BOX | wxMAXIMIZE_BOX |
             wxMINIMIZE_BOX) {
  wxBusyCursor bsy;
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);
  wxMemoryInputStream istream(NEWS_MD, NEWS_MD_SIZE);
  wxTextInputStream textIn(istream);
  wxString line;
  Connect(wxEVT_TEXT_URL, wxTextUrlEventHandler(ChangeLogDialog::OnTextURLEvent),
          NULL, this);

  m_changelog = new wxRichTextCtrl(
                             this, -1, wxEmptyString, wxDefaultPosition, wxDefaultSize,
                             wxRE_MULTILINE | wxRE_READONLY);
  m_changelog->BeginSuppressUndo();
  wxFont fnt = m_changelog->GetFont();
  wxClientDC dc(this);
  dc.SetFont(fnt);
  wxRegEx issueLink("#[0-9][0-9]*");
  wxRegEx bullet("^ -");

  bool inBulletList = false;
  bool isCaption = false;
  bool firstLine = true;
  while (!istream.Eof()) {
    line = textIn.ReadLine();
    {
      if(line.StartsWith(wxS("- ")))
        {
          if(!inBulletList)
            {
              m_changelog->Newline();
              inBulletList = true;
            }
          else
            {
              m_changelog->Newline();
              m_changelog->EndSymbolBullet();
            }
          m_changelog->BeginSymbolBullet(wxS("\u2022"), 50, 0);
          // drop the "- " at the end of the line.
          line = line.Right(line.Length() - 2);
        }
      else
        {
          if((inBulletList) && (!line.StartsWith(wxS(" "))))
            {
              inBulletList = false;
              m_changelog->Newline();
              m_changelog->EndSymbolBullet();
            }
          if(line.StartsWith(wxS("# ")))
            {
              isCaption = true;
              // drop the "# " at the end of the line.
              line = line.Right(line.Length() - 2);
            }
        }
      line.Trim(true);
      line.Trim(false);
      line += " ";
      if(isCaption)
        {
          if(!firstLine)
            m_changelog->Newline();
          m_changelog->BeginBold();
        }
      while(issueLink.Matches(line))
        {
          size_t start;
          size_t length;
          issueLink.GetMatch(&start, &length);
          wxString match = line.substr(start, length);
          wxString rest  = line.Right(line.Length() - start - length);
          wxString url   = wxS("https://github.com/wxMaxima-developers/wxmaxima/issues/")
            + match.Right(match.Length() - 1);
          m_changelog->WriteText(line.Left(start));
          m_changelog->BeginURL(url);
          m_changelog->WriteText(wxS("↗") + match);
          m_changelog->EndURL();
          line = rest;
        }
      m_changelog->WriteText(line);
      if(isCaption)
        m_changelog->EndBold();
      isCaption = false;
    }
    firstLine = false;
  }

  m_changelog->SetMinSize(wxSize(350 * GetContentScaleFactor(),
                               400 * GetContentScaleFactor()));
  vbox->Add(m_changelog, wxSizerFlags(10).Expand().Border(wxALL, 5));
  wxBoxSizer *buttonSizer = new wxBoxSizer(wxHORIZONTAL);

  wxButton *okButton = new wxButton(this, wxID_OK, _("OK"));
  buttonSizer->Add(okButton, wxSizerFlags().Border(wxALL, 5));
  okButton->SetDefault();
  vbox->Add(buttonSizer, wxSizerFlags(0).Right());

  SetName("ChangeLog");
  wxPersistenceManager::Get().RegisterAndRestore(this);
  SetSizerAndFit(vbox);
}

void ChangeLogDialog::OnTextURLEvent(wxTextUrlEvent &event) {
  if (event.GetMouseEvent().LeftUp()) {
    wxLaunchDefaultBrowser(event.GetString());
  }
}

