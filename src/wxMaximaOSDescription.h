// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
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
  This file declares a function that returns a relevant description of the
  Operating System
*/

#ifndef WXMAXIMAOSDESCRIPTION_H
#define WXMAXIMAOSDESCRIPTION_H

#include <wx/string.h>

//! The wxMaxima way to describe the Operating System.
wxString wxMaximaOperatingSystemDescription();
wxString wxMaximaOperatingSystemLongDescription();

#endif // WXMAXIMAOSDESCRIPTION_H
