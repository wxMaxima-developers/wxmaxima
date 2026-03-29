// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2026 Jerome Benoit <jgmbenoit@rezozer.net>
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
  This file defines the function wxMaximaOperatingSystemDescription.
*/

#include "wxMaximaOSDescription.h"
#include <wx/utils.h>

wxString wxMaximaOperatingSystemDescription() {
  static wxString OSDescription;
  if (OSDescription.IsEmpty()) {
#if __LINUX__
    wxLinuxDistributionInfo ldi = wxGetLinuxDistributionInfo();
    OSDescription = !ldi.Id.IsEmpty()?
          wxString::Format("%s %s",ldi.Id,
             !ldi.Release.IsEmpty()?ldi.Release:
             !ldi.CodeName.IsEmpty()?ldi.CodeName.Capitalize():"no-codename-given")
          :wxGetOsDescription();
#else
    OSDescription = wxGetOsDescription();
#endif
    }
  return OSDescription;
}

wxString wxMaximaOperatingSystemLongDescription() {
  static wxString OSLongDescription;
  if (OSLongDescription.IsEmpty()) {
#if __LINUX__
    wxLinuxDistributionInfo ldi = wxGetLinuxDistributionInfo();
    OSLongDescription = !ldi.Description.IsEmpty()?
          ldi.Description
          :wxGetOsDescription();
#else
    OSLongDescription = wxGetOsDescription();
#endif
    }
  return OSLongDescription;
}
