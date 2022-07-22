// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2013 Doug Ilijev <doug.ilijev@gmail.com>
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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*!\file
  This file declares the class wxMaxima that contains most of the program's logic.

  The worksheet is defined in the class MathCtrl instead and
  everything surrounding it in wxMaximaFrame.
 */


#ifndef MAIN_H
#define MAIN_H

#include "wxMaxima.h"
#include "wxMaximaFrame.h"
#include "MathParser.h"
#include "MaximaIPC.h"
#include "Dirstructure.h"
#include <wx/app.h>
#include <wx/socket.h>
#include <wx/config.h>
#include <wx/process.h>
#include <wx/regex.h>
#include <wx/dnd.h>
#include <wx/wfstream.h>
#include <wx/zstream.h>
#include <wx/txtstrm.h>
#include <wx/sckstrm.h>
#include <wx/buffer.h>
#include <wx/wx.h>
#include <wx/power.h>
#include <memory>

class Maxima; // The Maxima process interface
class MaximaEvent;



// cppcheck-suppress noConstructor
class MyApp : public wxApp
{
public:
  virtual bool OnInit();
  virtual int OnRun();
  virtual int OnExit();

  /*! Create a new window

    The mac platform insists in making all windows of an application
    share the same process. On the other platforms we create a separate
    process for every wxMaxima session instead which means that each
    process uses the NewWindow() function only once.

    \param file The file name
    \param evalOnStartup Do we want to execute the file automatically, but halt on error?
    \param exitAfterEval Do we want to close the window after the file has been evaluated?
    \param wxmData A .wxm file containing the initial worksheet contents
    \param wxmLen  The length of wxmData
   */
  void NewWindow(const wxString &file = {}, bool evalOnStartup = false, bool exitAfterEval = false, unsigned char *wxmData = NULL, size_t wxmLen = 0);

  void OnFileMenu(wxCommandEvent &ev);

  virtual void MacNewFile();
  void BecomeLogTarget();

  virtual void MacOpenFile(const wxString &file);

private:
  wxLocale m_locale;
  //! The name of the config file. Empty = Use the default one.
  wxString m_configFileName;
  Dirstructure m_dirstruct;
  #if defined __WXOSX__
  bool m_allWindowsInOneProcess = true;
  #else
  bool m_allWindowsInOneProcess = false;
  #endif
};



// cppcheck-suppress unknownMacro
DECLARE_APP(MyApp)
wxDECLARE_APP (MyApp);

#endif // MAIN_H
