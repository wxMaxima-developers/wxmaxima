// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
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
  This file declares all the wizards the draw sidepane needs.
*/

#ifndef CSVWIZ_H
#define CSVWIZ_H

#include "precomp.h"
#include <wx/wx.h>
#include <wx/button.h>
#include <wx/radiobut.h>
#include <wx/persist.h>
#include <wx/persist/toplevel.h>

#include "BTextCtrl.h"
#include <wx/filedlg.h>
#include <wx/choice.h>
#include "Configuration.h"

//! A wizard for explicit plots using draw
class CsvImportWiz : public wxDialog
{
public:

  CsvImportWiz(wxWindow *parent, Configuration *config);
  ~CsvImportWiz();
  wxString GetFilename(){return m_filename->GetValue();}
  wxString GetSeparator();
protected:
  void OnBrowse(wxCommandEvent &event);
private:
  wxButton *m_browseButton;
  BTextCtrl *m_filename;
  wxChoice *m_separator;
};

//! A wizard for explicit plots using draw
class CsvExportWiz : public wxDialog
{
public:

  CsvExportWiz(wxWindow *parent, Configuration *config, wxString objectType);
  ~CsvExportWiz();
  wxString GetFilename(){return m_filename->GetValue();}
  wxString GetMatrix(){return m_matrix->GetValue();}
  wxString GetSeparator();
protected:
  void OnBrowse(wxCommandEvent &event);
private:
  wxButton *m_browseButton;
  BTextCtrl *m_matrix;
  BTextCtrl *m_filename;
  wxChoice *m_separator;
};

#endif // CSVWIZ_H
