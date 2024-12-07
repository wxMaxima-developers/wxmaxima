// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2017-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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

#include "AboutDialog.h"
#include "../wxMaximaIcon.h"
#include <wx/wx.h>
#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/aboutdlg.h>
#include <wx/persist/toplevel.h>

AboutDialog::AboutDialog(wxWindow *WXUNUSED(parent), Configuration *config) {
      wxAboutDialogInfo info;
      wxString description;
      description = _("wxMaxima is a cross-platform graphical user interface for the "
                      "computer algebra system Maxima based on wxWidgets.\n");

#if defined(WXMAXIMA_GIT_SHORT_HASH)
      //cppcheck-suppress syntaxError
      description += wxString::Format(_("Build from Git version: %s\n"), WXMAXIMA_GIT_SHORT_HASH);
#endif
      description += wxString::Format(_("Using: %s\n\n"), wxVERSION_STRING);

      if (config->GetMaximaVersion() != wxEmptyString)
        {
          description += _("Maxima version: ") + config->GetMaximaVersion() +
            " (" + config->GetMaximaArch() + ")\n";
          if (config->GetLispVersion() != wxEmptyString)
            description += _("Maxima was compiled using: ") +
              config->GetLispType() + " " + config->GetLispVersion();
        }
      else
        description += _("\nNot connected to Maxima.");

      info.SetIcon(wxMaximaIcon());
      info.SetDescription(description);
      info.SetName(_("wxMaxima"));
      info.SetVersion(wxS(WXMAXIMA_VERSION));
      info.SetCopyright(wxS("(C) 2004-2024 The wxMaxima Team"));
      info.SetWebSite(wxS("https://wxMaxima-developers.github.io/wxmaxima/"));
#include "contributors.h"
      wxAboutBox(info);
}

