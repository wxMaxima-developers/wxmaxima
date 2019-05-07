// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  This file defines the class Cell

  Cell is the base class for all cell- or list-type elements.
*/

#include "TextStyle.h"

void TextStyle::Read(wxConfigBase *config, wxString where)
{
  wxString tmp;
  wxColor col;
  if (config->Read(where + wxT("color"), &tmp))
    {
        col = wxColor(tmp);
        if(col.IsOk())
          m_color = col;
    }
  config->Read(where + wxT("bold"), &m_bold);
  config->Read(where + wxT("italic"), &m_italic);
  config->Read(where + wxT("underlined"), &m_underlined);
  config->Read(where + wxT("Style/Text/fontsize"),
	       &m_fontSize);
  config->Read(where + wxT("Style/Text/fontname"),
	       &m_fontName);
#ifdef __WXOSX_MAC__
  if(m_fontName == wxEmptyString) m_fontName = "Monaco";
#endif
  wxFont font;
  font.SetFamily(wxFONTFAMILY_MODERN);
  font.SetFaceName(m_fontName);
  if (!font.IsOk())
    {
      font = wxFontInfo(10);
      m_fontName = font.GetFaceName();
    }
}

void TextStyle::Write(wxConfigBase *config, wxString where)
{
  config->Write(where + wxT("color"), Color().GetAsString());
  config->Write(where + wxT("bold"), m_bold);
  config->Write(where + wxT("italic"), m_italic);
  config->Write(where + wxT("underlined"), m_underlined);
  config->Write(where + wxT("Style/Text/fontsize"),
		m_fontSize);
  config->Write(where + wxT("Style/Text/fontname"),
		m_fontName);
}
