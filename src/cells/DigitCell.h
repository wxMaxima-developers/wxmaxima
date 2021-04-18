// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
//            (C) 2020      Kuba Ober <kuba@bertec.com>
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

#ifndef DIGITCELL_H
#define DIGITCELL_H

#include <wx/regex.h>
#include "TextCell.h"

/*! A Text cell that display a digit of a more-than-one-line-wide number
 */
class DigitCell : public TextCell
{
public:
  DigitCell(GroupCell *parent, Configuration **config, const wxString &text = {}, TextStyle style = TS_NUMBER):
    TextCell(parent,config,text,style)
    {}
  DigitCell(const TextCell &cell);
  ~DigitCell(){}
  
  void Recalculate(AFontSize fontsize) override;
  void Draw(wxPoint point) override;
};

#endif // DIGITCELL_H
