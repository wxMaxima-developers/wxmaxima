// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C)      1998 Vadim Zeitlin <zeitlin@dptmaths.ens-cachan.fr>
//  Copyright (C)      2018 Gunter Königsmann <wxMaxima@physikbuch.de>
//  Copyright (C)      2020 Kuba Ober <kuba@bertec.com>
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

  This file contains the class StackToStdErr that redirects wx Errors to a
  dialogue

  It is a customized copy of a portion of wxWidget's log.cpp.
*/

#include "StackToStdErr.h"
#include <iostream>

#if wxUSE_ON_FATAL_EXCEPTION && wxUSE_CRASHREPORT
void StackToStdErr::OnStackFrame(const wxStackFrame &frame)
{
  std::cerr<<frame.GetAddress();
  std::cerr<<": ";
  std::cerr<<frame.GetFileName();
      std::cerr<<" ";
  if(frame.GetLine())
    {
      std::cerr<<"(";
      std::cerr<<frame.GetLine();
      std::cerr<<") ";
    }
  std::cerr<<frame.GetName();
  std::cerr<<"(";
  std::cerr<<frame.GetOffset();
  std::cerr<<")";
  if(frame.GetParamCount() != 0)
    {
      std::cerr<<"Params:\n";
      for(auto i = 0; i < frame.GetParamCount(); i++)
        {
          wxString name, type, value;
          frame.GetParam(i, &type, &name, &value);
          std::cerr<<"   " << i << ": (" << type << ")" << name << "=" <<value<<"\n";
        }
    }
}
#endif
