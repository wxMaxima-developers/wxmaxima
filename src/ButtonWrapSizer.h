// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2009-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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

#ifndef BUTTONWRAPSIZER_H
#define BUTTONWRAPSIZER_H

#include <wx/stattext.h>

/*! \file

  This file contains the definition of the class Buttonwrapsizer that allows to 
  select arbitrary unicode symbols.
*/
#include "precomp.h"
#include <wx/wrapsizer.h>

/*! This class generates a pane containing the last commands that were issued.

 */
class Buttonwrapsizer : public wxWrapSizer
{
public:
  explicit Buttonwrapsizer(int orient = wxHORIZONTAL);
protected:
  virtual void RecalcSizes();
};

#endif // BUTTONWRAPSIZER_H
