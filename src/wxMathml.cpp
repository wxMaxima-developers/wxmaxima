// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2008-2009 Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
//            (C) 2011-2011 cw.ahbong <cw.ahbong@gmail.com>
//            (C) 2012-2013 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2014-2022 Gunter Königsmann <wxMaxima@physikbuch.de>
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

#include "wxMathml.h"
#include "wxMathML_lisp.h"
#include <iostream>
#include <wx/mstream.h>
#include <wx/string.h>
#include <wx/txtstrm.h>
#include <wx/wfstream.h>
#include <wx/wx.h>

wxMathML::wxMathML(Configuration *config) : m_configuration(config) {
  m_wxMathML_UseFile = m_configuration->WxMathML_UseFile();
}

wxString wxMathML::GetCmd() {
  // If we read wxMathML from a file we should read it anew, just in case
  // the file has changed.
  // If we have transitioned from using a file to using the internal
  // data we read the info anew, instead, as it might differ from the file.
  if (m_configuration->WxMathML_UseFile() || m_wxMathML_UseFile)
    m_maximaCMD = wxEmptyString;
  m_wxMathML_UseFile = m_configuration->WxMathML_UseFile();

  if ((m_maximaCMD.IsEmpty() || (m_configuration->WxMathML_UseFile()))) {
    if (!m_configuration->WxMathML_UseFile()) {
      wxLogMessage(_(
		     "Reading the Lisp part of wxMaxima from the included header file."));
      wxMemoryInputStream istream(WXMATHML_LISP, WXMATHML_LISP_SIZE);
      wxTextInputStream textIn(istream);
      wxString line;

      while (!istream.Eof()) {
        line = textIn.ReadLine();
        m_wxMathML += line + wxT("\n");
      }
      wxASSERT_MSG(m_wxMathML.Length() > 64000,
                   _("Compiler-Bug? wxMathml.lisp is shorter than expected!"));
    } else {
      wxLogMessage(wxString::Format(
				    _("Reading the Lisp part of wxMaxima from the file %s"),
				    m_configuration->WxMathML_Filename().ToUTF8().data()));
      wxFileInputStream input(m_configuration->WxMathML_Filename());
      wxTextInputStream textIn(input);
      wxString line;

      while (!input.Eof()) {
        line = textIn.ReadLine();
        m_wxMathML += line + wxT("\n");
      }
    }
  }
  wxStringTokenizer lines(m_wxMathML, wxT("\n"));
  while (lines.HasMoreTokens()) {
    wxString line = lines.GetNextToken();
    wxString lineWithoutComments;

    bool stringIs = false;
    wxChar lastChar = wxT('\n');
    wxString::const_iterator ch = line.begin();
    while (ch < line.end()) {
      // Remove formatting spaces
      if (((lastChar == '\n') && ((*ch == ' ') || (*ch == '\t'))))
        ++ch;
      else {
        // Handle backslashes that might escape double quotes
        if (*ch == wxT('\\')) {
          lineWithoutComments += *ch;
          ++ch;
        } else {
          // Handle strings
          if (*ch == wxT('\"'))
            stringIs = !stringIs;

          // Handle comments
          if ((*ch == wxT(';')) && (!stringIs))
            break;
        }
        lineWithoutComments += *ch;
        lastChar = *ch;
        ++ch;
      }
    }
    m_maximaCMD += lineWithoutComments + " ";
  }
  if (!m_configuration->WxMathML_UseFile())
    wxASSERT_MSG(m_maximaCMD.Length() > 54000,
                 _("Bug: After removing the whitespace wxMathml.lisp is "
                   "shorter than expected!"));
  m_maximaCMD = wxT(":lisp-quiet ") + m_maximaCMD + "\n";

  return m_maximaCMD;
}
wxString wxMathML::m_maximaCMD;
