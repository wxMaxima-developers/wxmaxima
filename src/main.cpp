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

#include <wx/wx.h>
#include <wx/tipdlg.h>
#include <wx/config.h>
#include <wx/intl.h>
#include <wx/fs_zip.h>
#include <wx/image.h>

#include "wxMaxima.h"

// On wxGTK2 we support printing only if wxWidgets is compiled with gnome_print.
// We have to force gnome_print support to be linked in static builds of wxMaxima.

#if defined wxUSE_LIBGNOMEPRINT
 #if wxUSE_LIBGNOMEPRINT
  #include "wx/html/forcelnk.h"
  FORCE_LINK(gnome_print)
 #endif
#endif

IMPLEMENT_APP(MyApp)

bool MyApp::OnInit()
{
  wxConfig *config = new wxConfig(wxT("wxMaxima"));
  wxConfig::Set(config);

  wxImage::AddHandler(new wxPNGHandler);
  wxImage::AddHandler(new wxXPMHandler);
  wxImage::AddHandler(new wxJPEGHandler);

  wxFileSystem::AddHandler(new wxZipFSHandler);

  int x = 40, y = 40, h = 650, w = 950, m = 0;
  int rs = 0, lang = wxLANGUAGE_UNKNOWN;
  int display_width = 1024, display_height = 768;
  bool have_pos;

  wxDisplaySize(&display_width, &display_height);

  have_pos = config->Read(wxT("pos-x"), &x);
  config->Read(wxT("pos-y"), &y);
  config->Read(wxT("pos-h"), &h);
  config->Read(wxT("pos-w"), &w);
  config->Read(wxT("pos-max"), &m);
  config->Read(wxT("pos-restore"), &rs);
  config->Read(wxT("language"), &lang);

  if (rs == 0)
    have_pos = false;
  if (!have_pos || m == 1 || x > display_width || y > display_height || x < 0 || y < 0)
  {
    x = 40;
    y = 40;
    h = 650;
    w = 950;
  }

  if (lang == wxLANGUAGE_UNKNOWN)
    lang = wxLocale::GetSystemLanguage();

  {
    wxLogNull disableErrors;
    m_locale.Init(lang);
  }

#if defined (__WXMSW__)
  wxSetEnv(wxT("LANG"), m_locale.GetName());
  wxSetWorkingDirectory(wxPathOnly(wxString(argv[0])));
  m_locale.AddCatalogLookupPathPrefix(wxGetCwd() + wxT("/locale"));
#elif defined (__WXMAC__)
  m_locale.AddCatalogLookupPathPrefix(wxGetCwd() + wxT("/wxMaxima.app/Contents/Resources/locale"));
#endif

  m_locale.AddCatalog(wxT("wxMaxima"));
  m_locale.AddCatalog(wxT("wxMaxima-wxstd"));

  wxMaxima *frame = new wxMaxima((wxFrame *)NULL, -1, _("wxMaxima"),
                                 wxPoint(x, y), wxSize(w, h));

  frame->Move(wxPoint(x, y));
  frame->SetSize(wxSize(w, h));
  if (m == 1)
    frame->Maximize(true);

  if (argc == 2) {
    wxString file(argv[1]);
    if (file.Right(4) == wxT(".wxm"))
    {
      if (!frame->ReadBatchFile(file))
        frame->SetOpenFile(file);
    }
    else
      frame->SetOpenFile(file);
  }

  frame->Show(true);
  frame->InitSession();
  if (!frame->IsIconized())
    frame->ShowTip(false);

  return true;
}
