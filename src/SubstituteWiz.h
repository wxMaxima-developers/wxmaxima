///
///  Copyright (C) 2004-2007 Andrej Vodopivec <andrejv@users.sourceforge.net>
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

#ifndef _SUBSTITUTEWIZ_H_
#define _SUBSTITUTEWIZ_H_

#include <wx/wx.h>
#include <wx/statline.h>

#include "BTextCtrl.h"

class SubstituteWiz: public wxDialog
{
public:
  SubstituteWiz(wxWindow* parent, int id, const wxString& title,
                const wxPoint& pos = wxDefaultPosition,
                const wxSize& size = wxDefaultSize,
                long style = wxDEFAULT_DIALOG_STYLE);
  wxString GetValue();
  void SetValue(wxString s)
  {
    text_ctrl_3->SetValue(s);
  }
private:
  void set_properties();
  void do_layout();
protected:
  wxStaticText* label_1;
  wxStaticText* label_2;
  BTextCtrl* text_ctrl_1;
  wxStaticText* label_3;
  BTextCtrl* text_ctrl_2;
  wxStaticText* label_4;
  BTextCtrl* text_ctrl_3;
  wxStaticLine* static_line_1;
  wxButton* button_1;
  wxButton* button_2;
  wxCheckBox* checkbox_1;
};

#endif // SUBSTITUTEWIT_H
