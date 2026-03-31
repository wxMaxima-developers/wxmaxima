// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2026 Wolfgang Dautermann
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
  This file declares the class wxMaximaArtprovider - our own wxArtProvider with additional images.
*/


// only use that for recent wxWidgets versions
#include <wx/version.h>
#if wxCHECK_VERSION(3, 1, 6)

#include <wx/artprov.h>

// from art:
#define wxmaximaART_MEDIA_PLAYBACK_START    wxART_MAKE_ART_ID(wxmaximaART_MEDIA_PLAYBACK_START)
#define wxmaximaART_MEDIA_PLAYBACK_REVERSE  wxART_MAKE_ART_ID(wxmaximaART_MEDIA_PLAYBACK_REVERSE)

// from art/toolbar:
#define wxmaximaART_ARROW_UP_SQUARE         wxART_MAKE_ART_ID(wxmaximaART_ARROW_UP_SQUARE)
#define wxmaximaART_DIALOG_INFORMATION      wxART_MAKE_ART_ID(wxmaximaART_DIALOG_INFORMATION)
#define wxmaximaART_EYE_SLASH               wxART_MAKE_ART_ID(wxmaximaART_EYE_SLASH)
#define wxmaximaART_GO_BOTTOM               wxART_MAKE_ART_ID(wxmaximaART_GO_BOTTOM)
#define wxmaximaART_GO_JUMP                 wxART_MAKE_ART_ID(wxmaximaART_GO_JUMP)
#define wxmaximaART_GO_LAST                 wxART_MAKE_ART_ID(wxmaximaART_GO_LAST)
#define wxmaximaART_GO_NEXT                 wxART_MAKE_ART_ID(wxmaximaART_GO_NEXT)
#define wxmaximaART_GTK_PREFERENCES         wxART_MAKE_ART_ID(wxmaximaART_GTK_PREFERENCES)
#define wxmaximaART_GTK_SELECT_ALL          wxART_MAKE_ART_ID(wxmaximaART_GTK_SELECT_ALL)
#define wxmaximaART_GTK_STOP                wxART_MAKE_ART_ID(wxmaximaART_GTK_STOP)
#define wxmaximaART_INPUT                   wxART_MAKE_ART_ID(wxmaximaART_INPUT)
#define wxmaximaART_MEDIA_PLAYBACK_STOP     wxART_MAKE_ART_ID(wxmaximaART_MEDIA_PLAYBACK_STOP)
#define wxmaximaART_SOFTWARE_UPDATE_URGENT  wxART_MAKE_ART_ID(wxmaximaART_SOFTWARE_UPDATE_URGENT)
#define wxmaximaART_TEXT                    wxART_MAKE_ART_ID(wxmaximaART_TEXT)
#define wxmaximaART_VIEW_REFRESH1           wxART_MAKE_ART_ID(wxmaximaART_VIEW_REFRESH1)



class wxMaximaArtProvider : public wxArtProvider
{
protected:
  // Override this method to return a bundle containing the required
  // bitmap in all available sizes.
  wxBitmapBundle CreateBitmapBundle(const wxArtID& id,
                                    const wxArtClient& client,
                                    const wxSize& size) override;

private:
  // gunzip the (gzip compressed) SVG in Memory
  wxString gunzip(unsigned char * svg_gz, size_t svg_gz_size);
};
#endif
