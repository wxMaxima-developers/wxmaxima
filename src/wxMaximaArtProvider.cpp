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
  This file defines the class wxMaximaArtprovider - our own wxArtProvider with additional images.
*/

// only use that for recent wxWidgets versions
#include <wx/version.h>
#if wxCHECK_VERSION(3, 2, 0)

#include <wx/bmpbndl.h>

#include "wxMaximaArtProvider.h"

#include "./art/toolbar/eye-slash.svg.h"

wxBitmapBundle wxMaximaArtProvider::CreateBitmapBundle(const wxArtID& id,
                                    const wxArtClient& client,
                                    const wxSize& size)
{
  if (id == wxmaximaART_EYE_SLASH) {
    return wxBitmapBundle::FromSVG(reinterpret_cast<char *>(EYE_SLASH_SVG), GetSizeHint(client));
  }
  return wxNullBitmap;
}
#endif
