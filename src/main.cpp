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
#include <wx/cmdline.h>
#include <wx/fileconf.h>

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
  int lang = wxLANGUAGE_UNKNOWN;

  wxCmdLineParser cmdLineParser(argc, argv);
  cmdLineParser.AddOption(wxT("f"), wxT("ini"), wxT("use ini file"),wxCMD_LINE_VAL_STRING);
  cmdLineParser.AddOption(wxT("o"), wxT("open"), wxT("open file"), wxCMD_LINE_VAL_STRING);
  cmdLineParser.Parse();
  wxString ini, file;
  if (cmdLineParser.Found(wxT("f"),&ini))
    wxConfig::Set(new wxFileConfig(ini));
  else
    wxConfig::Set(new wxConfig(wxT("wxMaxima")));

  wxConfigBase *config = wxConfig::Get();
  config->Read(wxT("language"), &lang);

  wxImage::AddHandler(new wxPNGHandler);
  wxImage::AddHandler(new wxXPMHandler);
  wxImage::AddHandler(new wxJPEGHandler);

  wxFileSystem::AddHandler(new wxZipFSHandler);

  if (lang == wxLANGUAGE_UNKNOWN)
    lang = wxLocale::GetSystemLanguage();

  {
    wxLogNull disableErrors;
    m_locale.Init(lang);
  }

#if defined (__WXMSW__)
  wxSetEnv(wxT("LANG"), m_locale.GetName());
  if (!wxGetEnv(wxT("BUILD_DIR"), NULL))
    wxSetWorkingDirectory(wxPathOnly(wxString(argv[0])));
  m_locale.AddCatalogLookupPathPrefix(wxGetCwd() + wxT("/locale"));
#elif defined (__WXMAC__)
  m_locale.AddCatalogLookupPathPrefix(wxGetCwd() + wxT("/wxMaxima.app/Contents/Resources/locale"));
#endif

  m_locale.AddCatalog(wxT("wxMaxima"));
  m_locale.AddCatalog(wxT("wxMaxima-wxstd"));


#if defined (__WXMAC__)
  wxApp::SetExitOnFrameDelete(false);
  wxMenuBar *menuBar = new wxMenuBar;
  wxMenu *fileMenu = new wxMenu;
  fileMenu->Append(mac_newId, _("&New\tCtrl-N"));
  fileMenu->Append(mac_openId, _("&Open\tCtrl-O"));
  menuBar->Append(fileMenu, _("File"));
  wxMenuBar::MacSetCommonMenuBar(menuBar);

  Connect(mac_newId, wxEVT_COMMAND_MENU_SELECTED, wxCommandEventHandler(MyApp::OnFileMenu));
  Connect(mac_openId, wxEVT_COMMAND_MENU_SELECTED, wxCommandEventHandler(MyApp::OnFileMenu));
  Connect(wxID_EXIT, wxEVT_COMMAND_MENU_SELECTED, wxCommandEventHandler(MyApp::OnFileMenu));
#endif

  if (cmdLineParser.Found(wxT("o"), &file))
    NewWindow(wxString(file));
  else
    NewWindow();

  return true;
}

void MyApp::NewWindow(wxString file)
{
  int x = 40, y = 40, h = 650, w = 950, m = 0;
  int rs = 0;
  int display_width = 1024, display_height = 768;
  bool have_pos;

  wxConfig *config = (wxConfig *)wxConfig::Get();

  wxDisplaySize(&display_width, &display_height);

  have_pos = config->Read(wxT("pos-x"), &x);
  config->Read(wxT("pos-y"), &y);
  config->Read(wxT("pos-h"), &h);
  config->Read(wxT("pos-w"), &w);
  config->Read(wxT("pos-max"), &m);
  config->Read(wxT("pos-restore"), &rs);

  if (rs == 0)
    have_pos = false;
  if (!have_pos || m == 1 || x > display_width || y > display_height || x < 0 || y < 0)
  {
    x = 40;
    y = 40;
    h = 650;
    w = 950;
  }

#if defined __WXMAC__
  x += topLevelWindows.GetCount()*20;
  y += topLevelWindows.GetCount()*20;
#endif

  wxMaxima *frame = new wxMaxima((wxFrame *)NULL, -1, _("wxMaxima"),
                                 wxPoint(x, y), wxSize(w, h));

  frame->Move(wxPoint(x, y));
  frame->SetSize(wxSize(w, h));
  if (m == 1)
    frame->Maximize(true);

  if (file.Length() > 0 && wxFileExists(file)) {
    frame->SetOpenFile(file);
  }

#if defined __WXMAC__
  topLevelWindows.Append(frame);
#endif

  SetTopWindow(frame);
  frame->Show(true);
  frame->InitSession();
  frame->ShowTip(false);
}

#if defined (__WXMAC__)

void MyApp::OnFileMenu(wxCommandEvent &ev)
{
  switch(ev.GetId())
  {
    case mac_newId:
      NewWindow();
      break;
    case mac_openId:
      {
        wxString file = wxFileSelector(_("Open"), wxEmptyString,
                                      wxEmptyString, wxEmptyString,
                                      _("wxMaxima document (*.wxm, *.wxmx)|*.wxm;*.wxmx"),
                                      wxFD_OPEN);
        if (file.Length() > 0)
          NewWindow(file);
      }
      break;
    case wxID_EXIT:
      {
#if defined __WXMAC__
        bool quit = true;
        wxWindowList::compatibility_iterator node = topLevelWindows.GetFirst();
        while (node) {
          wxWindow *frame = node->GetData();
          node = node->GetNext();
          frame->Raise();
          if (!frame->Close()) {
            quit = false;
            break;
          }
        }
        if (quit)
          wxExit();
#else
        wxWindow *frame = GetTopWindow();
        if (frame == NULL)
          wxExit();
        else if (frame->Close())
          wxExit();
#endif
      }
      break;
  }
}

void MyApp::MacNewFile()
{
  wxWindow *frame = GetTopWindow();
  if (frame == NULL)
    NewWindow();
}

void MyApp::MacOpenFile(const wxString &file)
{
  NewWindow(file);
}

#endif
