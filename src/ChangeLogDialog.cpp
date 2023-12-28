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
  A dialog that shows the program's license.
*/

#include "ChangeLogDialog.h"
#include "news_md.h"
#include <wx/mstream.h>
#include <wx/string.h>
#include <wx/txtstrm.h>
#include <wx/regex.h>

ChangeLogDialog::ChangeLogDialog(wxWindow *parent)
  : wxDialog(parent, -1, _("ChangeLog"), wxDefaultPosition, wxDefaultSize,
             wxRESIZE_BORDER | wxCLOSE_BOX | wxMAXIMIZE_BOX |
             wxMINIMIZE_BOX) {
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);
  wxMemoryInputStream istream(NEWS_MD, NEWS_MD_SIZE);
  wxTextInputStream textIn(istream);
  m_movedToStart = false;
  wxString line;
  wxString licenseText;
  Connect(wxEVT_TEXT_URL, wxTextUrlEventHandler(ChangeLogDialog::OnTextURLEvent),
          NULL, this);

  m_license = new wxRichTextCtrl(
                             this, -1, wxEmptyString, wxDefaultPosition, wxDefaultSize,
                             wxRE_MULTILINE | wxRE_READONLY);

  wxFont fnt = m_license->GetFont();
  wxClientDC dc(this);
  dc.SetFont(fnt);
  wxCoord textWidth = 0;
  wxRegEx issueLink("#([0-9][0-9]*)");
  wxRegEx bullet("^ -");

  bool inBulletList = false;
  bool isCaption = false;
  while (!istream.Eof()) {
    line = textIn.ReadLine();
    {
      if(line.StartsWith(wxS("- ")))
        {
          if(!inBulletList)
            {
              m_license->Newline();
              inBulletList = true;
            }
          else
            {
              m_license->Newline();
              m_license->EndSymbolBullet();
            }
          m_license->BeginSymbolBullet(wxS("\u2022"), 50, 0, 0);
          // drop the "- " at the end of the line.
          line = line.Right(line.Length() - 2);
        }
      else
        {
          if((inBulletList) && (!line.StartsWith(wxS(" "))))
            {
              inBulletList = false;
              m_license->Newline();
              m_license->EndSymbolBullet();
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
        m_license->BeginBold();
      while(issueLink.Matches(line))
        {
          size_t start;
          size_t length;
          issueLink.GetMatch(&start, &length);
          wxString match = line.SubString(start, length);
          wxString rest  = line.Right(line.Length() - start - length);
          wxString url   = wxS("https://github.com/wxMaxima-developers/wxmaxima/issues/")
            + match.Right(match.Length() - 1);
          url = url.Left(url.Length() - 1);
          m_license->WriteText(line.Left(start));
          m_license->BeginURL(url);
          m_license->WriteText(wxS("↗") + match);
          m_license->EndURL();
          line = rest;
        }
      m_license->WriteText(line);
      if(isCaption)
        m_license->EndBold();
      isCaption = false;
    }
  }

  m_license->SetMinSize(wxSize(350 * GetContentScaleFactor(),
                               400 * GetContentScaleFactor()));
  vbox->Add(m_license, wxSizerFlags(10).Expand().Border(wxALL, 5));
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

