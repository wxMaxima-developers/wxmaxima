/*
 *  Copyright (C) 2004-2005 Andrej Vodopivec <andrejv@users.sourceforge.net>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */


#include <wx/wx.h>
#include <wx/tipdlg.h>
#include <wx/config.h>
#include <wx/intl.h>

#include "wxMaxima.h"

class MyApp : public wxApp
{
public:
  virtual bool OnInit();
private:
  wxLocale *m_locale;
};

IMPLEMENT_APP(MyApp)

bool MyApp::OnInit()
{
  wxConfig *config = new wxConfig(wxT("wxMaxima"));
  wxConfig::Set(config);
  int x, y, h, w, m, rs=0, lang = wxLANGUAGE_UNKNOWN;
  bool have_pos;
  wxString enc;
  have_pos = config->Read(wxT("pos-x"), &x);
  config->Read(wxT("pos-y"), &y);
  config->Read(wxT("pos-h"), &h);
  config->Read(wxT("pos-w"), &w);
  config->Read(wxT("pos-max"), &m);
  config->Read(wxT("pos-restore"), &rs);
  config->Read(wxT("language"), &lang);
  config->Read(wxT("encoding"), &enc);
  if (rs==0)
    have_pos = false;
  if (!have_pos || m==1) {
    x = 40;
    y = 40;
    h = 650;
    w = 950;
  }
  
  m_locale = new wxLocale;
  if (lang==wxLANGUAGE_UNKNOWN)
    lang = wxLocale::GetSystemLanguage();
  m_locale->Init(lang);
  m_locale->AddCatalog(wxT("wxMaxima"));
  m_locale->AddCatalog(wxT("wxMaxima-wxstd"));

  wxMaxima *frame = new wxMaxima((wxFrame *)NULL, -1, _("wxMaxima"),
                                 wxPoint(x, y), wxSize(w, h));

  frame->Move(wxPoint(x, y));
  frame->SetSize(wxSize(w, h));
  if (m==1)
    frame->Maximize(true);

  frame->Show(TRUE);
  frame->initSession();
  frame->showTip(false);
  
  return TRUE;
}
