// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2019      Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file declares a message dialog that sends its contents to the log message sink.
 */

#ifndef LOGGINGMESSAGEDIALOG_H
#define LOGGINGMESSAGEDIALOG_H


#include "precomp.h"
#include <wx/msgdlg.h>
#include <wx/log.h>


/*! A message dialog that sends its contents to the log message sink.
 */
class LoggingMessageDialog : public wxMessageDialog
{
 public:
 LoggingMessageDialog(wxWindow *parent, const wxString &message, const wxString &caption=wxMessageBoxCaptionStr, long style=wxOK|wxCENTRE, const wxPoint &pos=wxDefaultPosition):
  wxMessageDialog (parent, message, caption, style, pos)
    {
      wxLogMessage(message);
    }
};

extern int LoggingMessageBox( 	const wxString &  	message,
				const wxString &  	caption = wxMessageBoxCaptionStr,
				int  	style = wxOK|wxCENTRE,
				wxWindow *  	parent = NULL,
				int  	x = wxDefaultCoord,
				int  	y = wxDefaultCoord 
				);

#endif // LOGGINGMESSAGEDIALOG_H
