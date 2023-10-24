// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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

#ifndef ACTUALVALUESSTORAGEWIZ_H
#define ACTUALVALUESSTORAGEWIZ_H

#include "precomp.h"
#include <wx/wx.h>
#include <wx/grid.h>
#include <wx/statline.h>

#include "BTextCtrl.h"

class ActualValuesStorageWiz : public wxDialog
{
public:
    ActualValuesStorageWiz(
        Configuration *cfg,
        wxWindow *parent, int id, const wxString &title,
        bool eq = false,
        const wxPoint &pos = wxDefaultPosition,
        const wxSize &size = wxDefaultSize,
        long style = wxDEFAULT_DIALOG_STYLE);

    wxString GetValue();

private:
    void set_properties();

protected:
    wxGrid *m_grid;
    wxButton *button_1;
    wxButton *button_2;
    void OnValueChange(wxGridEvent &event);
};

#endif // ACTUALVALUESSTORAGEWIZ_H
