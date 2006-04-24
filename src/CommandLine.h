///
///  Copyright (C) 2004-2006 Andrej Vodopivec <andrejv@users.sourceforge.net>
///
///  This program is free software; you can redistribute it and/or modify
///  it under the terms of the GNU General Public License as published by
///  the Free Software Foundation; either version 2 of the License, or
///  (at your option) any later version.
///
///  This program is distributed in the hope that it will be useful,
///  but WITHOUT ANY WARRANTY; without even the implied warranty of
///  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
///  GNU General Public License for more details.
///
///
///  You should have received a copy of the GNU General Public License
///  along with this program; if not, write to the Free Software
///  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
///

#ifndef _WXCOMMANDLINE_H_
#define _WXCOMMANDLINE_H_

#include <wx/wx.h>

#include "BTextCtrl.h"

class CommandLine : public BTextCtrl
{
public:
  CommandLine(wxWindow *parent,
              wxWindowID id,
              const wxString& value,
              const wxPoint& pos,
              const wxSize& size,
              long style);
  ~CommandLine();
  int AddToHistory(wxString s);
  wxString Previous();
  wxString Next();
  wxString Complete(wxString s);
#if defined __WXMSW__
  void SetValue(wxString s);
  void WriteText(wxString s);
#endif
protected:
  wxArrayString m_history;
  int m_historySize;
  int m_historyIndex;
  wxString m_currentValue;
  wxString m_currentHistoryValue;
  wxString m_currentDisplayedValue;
  void FilterLine(wxKeyEvent& event);
#if defined __WXMSW__
  void DoHighlight();
#endif

  DECLARE_EVENT_TABLE()
};


#endif  //_WXCOMMANDLINE_H_
