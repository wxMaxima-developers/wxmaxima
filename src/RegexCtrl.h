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

#ifndef REGEXCTRL_H
#define REGEXCTRL_H

#include "precomp.h"
#include <wx/wx.h>
#include <wx/regex.h>
#include <wx/textctrl.h>
#include "Configuration.h"
#include "BTextCtrl.h"

/*! A BTextCtrl that allows to input a regex

*/
class RegexCtrl : public BTextCtrl
{
  class wxLogBuffer_noStdErrFlush : public wxLogBuffer
  {
  public:
    wxLogBuffer_noStdErrFlush(): wxLogBuffer() {};
    virtual void Flush() override {}
    ~wxLogBuffer_noStdErrFlush() override{};
  };
public:
  RegexCtrl(wxWindow *parent,
            wxWindowID id, Configuration *cfg);
  bool Matches(wxString text);

protected:
  void OnTextChange(wxCommandEvent &ev);

  ~RegexCtrl();

private:
  wxString m_oldRegex;
  wxRegEx m_regex;
  enum class RegexInputState : int8_t { empty, invalid, valid };
  //! The state of the regex in the regex entry control
  RegexInputState m_regexInputState = RegexInputState::empty;
  RegexInputState GetNewRegexInputState() const;
  //! The tooltip that is displayed if the regex cannot be interpreted
  static wxString RegexTooltip_error;
  //! The tooltip that is displayed if the regex is empty or can be interpreted
  static wxString RegexTooltip_norm;
};

wxDECLARE_EVENT(REGEX_EVENT, wxCommandEvent);

#endif // REGEXCTRL_H
