// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2015-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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

/*!\file
  The file that defines the starting point of the program
 */

#include <wx/wx.h>
#include <wx/tipdlg.h>
#include <wx/config.h>
#include <wx/intl.h>
#include <wx/fs_zip.h>
#include <wx/image.h>

#include <wx/cmdline.h>
#include <wx/fileconf.h>
#include "Dirstructure.h"
#include <iostream>

#include "wxMaxima.h"
#include "Version.h"

// On wxGTK2 we support printing only if wxWidgets is compiled with gnome_print.
// We have to force gnome_print support to be linked in static builds of wxMaxima.

#if defined wxUSE_LIBGNOMEPRINT
#if wxUSE_LIBGNOMEPRINT
#include "wx/html/forcelnk.h"
FORCE_LINK(gnome_print)
#endif
#endif


IMPLEMENT_APP(MyApp)

void MyApp::Cleanup_Static()
{
  if (m_frame)
    m_frame->CleanUp();
}

bool MyApp::OnInit()
{

  // Migrate an eventual old config file to the location XDG wants it to be.
  #ifndef __WXMSW__
  #if wxCHECK_VERSION(3, 1, 1)
  wxStandardPaths::Get().SetFileLayout(wxStandardPaths::FileLayout_Classic);
  wxString configFileOld = wxStandardPaths::Get().GetUserConfigDir() + wxT("/") +
    wxStandardPaths::Get().MakeConfigFileName(
      wxString(wxT("wxMaxima")),
      wxStandardPaths::ConfigFileConv_Dot);
  wxStandardPaths::Get().SetFileLayout(wxStandardPaths::FileLayout_XDG);
  wxString configFileXDG = wxStandardPaths::Get().GetUserConfigDir() + wxT("/") +
    wxStandardPaths::Get().MakeConfigFileName(
      wxString(wxT("wxMaxima")),
      wxStandardPaths::ConfigFileConv_Ext);

  if(!wxFileExists(configFileXDG))
  {
    wxFileName xdgDir(configFileXDG);
    wxString dirName(xdgDir.GetPath());
    if(!wxDirExists(dirName))
      wxMkDir(dirName,0x700);
    if(wxFileExists(configFileOld))
      wxCopyFile(configFileOld,configFileXDG);
  }
  #endif
  #endif
  
  m_frame = NULL;
//  atexit(Cleanup_Static);
  int lang = wxLANGUAGE_UNKNOWN;

  bool exitAfterEval = false;
  bool evalOnStartup = false;

  wxCmdLineParser cmdLineParser(argc, argv);

  static const wxCmdLineEntryDesc cmdLineDesc[] =
          {
            {wxCMD_LINE_SWITCH, "v", "version", "Output the version info", wxCMD_LINE_VAL_NONE , 0},
                  /* Usually wxCMD_LINE_OPTION_HELP is used with the following option, but that displays a message
                   * using a own window and we want the message on the command line. If a user enters a command
                   * line option, he expects probably a answer just on the command line... */
                  {wxCMD_LINE_SWITCH, "h", "help", "show this help message", wxCMD_LINE_VAL_NONE, wxCMD_LINE_OPTION_HELP},
                  {wxCMD_LINE_OPTION, "o", "open", "open a file", wxCMD_LINE_VAL_STRING , 0},
                  {wxCMD_LINE_SWITCH, "e", "eval",
                   "evaluate the file after opening it.", wxCMD_LINE_VAL_NONE , 0},
                  {wxCMD_LINE_SWITCH, "b", "batch",
                   "run the file and exit afterwards. Halts on questions and stops on errors.",  wxCMD_LINE_VAL_NONE, 0},
                  { wxCMD_LINE_OPTION, "f", "ini", "allows to specify a file to store the configuration in", wxCMD_LINE_VAL_STRING , 0},
                  {wxCMD_LINE_PARAM, NULL, NULL, "input file", wxCMD_LINE_VAL_STRING, wxCMD_LINE_PARAM_OPTIONAL | wxCMD_LINE_PARAM_MULTIPLE},
            {wxCMD_LINE_NONE, "", "", "", wxCMD_LINE_VAL_NONE, 0}
          };

  cmdLineParser.SetDesc(cmdLineDesc);
  cmdLineParser.Parse();
  wxString ini, file;
  // Attention: The config file is changed by wxMaximaFrame::wxMaximaFrame::ReReadConfig
  if (cmdLineParser.Found(wxT("f"),&ini))
  {
    wxConfig::Set(new wxFileConfig(wxT("wxMaxima"), wxEmptyString, ini));
    m_configFileName = ini;
  }
  else
    wxConfig::Set(new wxConfig(wxT("wxMaxima")));
  
  wxImage::AddHandler(new wxPNGHandler);
  wxImage::AddHandler(new wxXPMHandler);
  wxImage::AddHandler(new wxJPEGHandler);

  wxFileSystem::AddHandler(new wxZipFSHandler);

  wxConfigBase *config = wxConfig::Get();
  lang = wxLocale::GetSystemLanguage();
  config->Read(wxT("language"), &lang);

  if(wxLocale::IsAvailable(lang))
    m_locale.Init(lang);
  else
    m_locale.Init(wxLANGUAGE_ENGLISH);

  Dirstructure dirstruct;

#if defined (__WXMSW__)
  wxSetEnv(wxT("LANG"), m_locale.GetName());
  if (!wxGetEnv(wxT("BUILD_DIR"), NULL))
  {
    wxString dir = wxPathOnly(wxStandardPaths::Get().GetExecutablePath());
    if(dir != wxEmptyString)
      wxSetWorkingDirectory(wxPathOnly(wxStandardPaths::Get().GetExecutablePath()));
  }

  wxString fontPrefix = dirstruct.FontDir() + wxT("/");
  
  /* Add private jsMath fonts, if they exist */ 
  if (wxFileExists(fontPrefix + wxT(CMEX10) + wxT(".ttf"))) AddFontResource(fontPrefix + wxT(CMEX10) + wxT(".ttf"));
  if (wxFileExists(fontPrefix + wxT(CMSY10) + wxT(".ttf"))) AddFontResource(fontPrefix + wxT(CMSY10) + wxT(".ttf"));
  if (wxFileExists(fontPrefix + wxT(CMR10) + wxT(".ttf")))  AddFontResource(fontPrefix + wxT(CMR10) + wxT(".ttf"));
  if (wxFileExists(fontPrefix + wxT(CMMI10) + wxT(".ttf"))) AddFontResource(fontPrefix + wxT(CMMI10) + wxT(".ttf"));
  if (wxFileExists(fontPrefix + wxT(CMTI10) + wxT(".ttf"))) AddFontResource(fontPrefix + wxT(CMTI10) + wxT(".ttf"));

  /* Add private Libertine fonts, if they exist */
  if (wxFileExists(fontPrefix + wxT(LIBERTINE1))) 
	  AddFontResource(fontPrefix + wxT(LIBERTINE1));
  if (wxFileExists(fontPrefix + wxT(LIBERTINE2))) AddFontResource(fontPrefix + wxT(LIBERTINE2));
  if (wxFileExists(fontPrefix + wxT(LIBERTINE3))) AddFontResource(fontPrefix + wxT(LIBERTINE3));
  if (wxFileExists(fontPrefix + wxT(LIBERTINE4))) AddFontResource(fontPrefix + wxT(LIBERTINE4));
  if (wxFileExists(fontPrefix + wxT(LIBERTINE5))) AddFontResource(fontPrefix + wxT(LIBERTINE5));
  if (wxFileExists(fontPrefix + wxT(LIBERTINE6))) AddFontResource(fontPrefix + wxT(LIBERTINE6));
  if (wxFileExists(fontPrefix + wxT(LIBERTINE7))) AddFontResource(fontPrefix + wxT(LIBERTINE7));
  if (wxFileExists(fontPrefix + wxT(LIBERTINE8))) AddFontResource(fontPrefix + wxT(LIBERTINE8));
  if (wxFileExists(fontPrefix + wxT(LIBERTINE9))) AddFontResource(fontPrefix + wxT(LIBERTINE9));
#endif

  m_locale.AddCatalogLookupPathPrefix(dirstruct.LocaleDir());
  m_locale.AddCatalogLookupPathPrefix(dirstruct.LocaleDir()+wxT("/wxwin"));
  m_locale.AddCatalogLookupPathPrefix(wxT("/usr/share/locale"));
  m_locale.AddCatalogLookupPathPrefix(wxT("/usr/local/share/locale"));
  m_locale.AddCatalog(wxT("wxMaxima"));
  m_locale.AddCatalog(wxT("wxMaxima-wxstd"));

#if defined __WXMAC__
  wxString path;
  wxGetEnv(wxT("PATH"), &path);
  wxSetEnv(wxT("PATH"), path << wxT(":/usr/local/bin"));

  wxApp::SetExitOnFrameDelete(false);
  wxMenuBar *menuBar = new wxMenuBar;
  wxMenu *fileMenu = new wxMenu;
  fileMenu->Append(wxMaxima::mac_newId, _("&New\tCtrl+N"));
  fileMenu->Append(wxMaxima::mac_openId, _("&Open\tCtrl+O"));
  menuBar->Append(fileMenu, _("File"));
  wxMenuBar::MacSetCommonMenuBar(menuBar);

  Connect(wxMaxima::mac_newId, wxEVT_COMMAND_MENU_SELECTED, wxCommandEventHandler(MyApp::OnFileMenu));
  Connect(wxMaxima::mac_openId, wxEVT_COMMAND_MENU_SELECTED, wxCommandEventHandler(MyApp::OnFileMenu));
  Connect(wxID_EXIT, wxEVT_COMMAND_MENU_SELECTED, wxCommandEventHandler(MyApp::OnFileMenu));
#endif

  if (cmdLineParser.Found(wxT("v")))
  {
    std::cout << "wxMaxima ";
    std::cout << GITVERSION;
#if defined(WXMAXIMA_GIT_VERSION)
    std::cout << " (Git version: " << WXMAXIMA_GIT_VERSION << ")";
#endif
    std::cout << "\n";
    wxExit();
  }
  if (cmdLineParser.Found(wxT("h")))
  {
    std::cout << "A feature-rich graphical user interface for the computer algebra system maxima\n";
    std::cout << cmdLineParser.GetUsageString();
    wxExit();
  }

  if (cmdLineParser.Found(wxT("b")))
  {
    evalOnStartup = true;
    exitAfterEval = true;
  }
  
  if (cmdLineParser.Found(wxT("e")))
    evalOnStartup = true;

  if (cmdLineParser.Found(wxT("o"), &file))
  {
    wxFileName FileName = file;
    FileName.MakeAbsolute();
    wxString CanonicalFilename = FileName.GetFullPath();
    NewWindow(wxString(CanonicalFilename), evalOnStartup, exitAfterEval);
    return true;
  }

  if(cmdLineParser.GetParamCount() > 0)
  {
    for (unsigned int i=0; i < cmdLineParser.GetParamCount(); i++)
    {
      wxFileName FileName = cmdLineParser.GetParam(i);
      FileName.MakeAbsolute();
      
      wxString CanonicalFilename = FileName.GetFullPath();
      NewWindow(CanonicalFilename, evalOnStartup, exitAfterEval);
    }
  }
  else
    NewWindow();

  return true;
}

#if defined (__WXMSW__)
int MyApp::OnExit()
{
  Dirstructure dirstruct;
  wxString fontPrefix = dirstruct.FontDir() + wxT("/");
  if (wxFileExists(fontPrefix + CMEX10)) RemoveFontResource(fontPrefix + wxT(CMEX10));
  if (wxFileExists(fontPrefix + CMSY10)) RemoveFontResource(fontPrefix + wxT(CMSY10));
  if (wxFileExists(fontPrefix + CMR10))  RemoveFontResource(fontPrefix + wxT(CMR10));
  if (wxFileExists(fontPrefix + CMMI10)) RemoveFontResource(fontPrefix + wxT(CMMI10));
  if (wxFileExists(fontPrefix + CMTI10)) RemoveFontResource(fontPrefix + wxT(CMTI10));

  if (wxFileExists(fontPrefix + LIBERTINE1)) RemoveFontResource(fontPrefix + wxT(LIBERTINE1));
  if (wxFileExists(fontPrefix + LIBERTINE2)) RemoveFontResource(fontPrefix + wxT(LIBERTINE2));
  if (wxFileExists(fontPrefix + LIBERTINE3)) RemoveFontResource(fontPrefix + wxT(LIBERTINE3));
  if (wxFileExists(fontPrefix + LIBERTINE4)) RemoveFontResource(fontPrefix + wxT(LIBERTINE4));
  if (wxFileExists(fontPrefix + LIBERTINE5)) RemoveFontResource(fontPrefix + wxT(LIBERTINE5));
  if (wxFileExists(fontPrefix + LIBERTINE6)) RemoveFontResource(fontPrefix + wxT(LIBERTINE6));
  if (wxFileExists(fontPrefix + LIBERTINE7)) RemoveFontResource(fontPrefix + wxT(LIBERTINE7));
  if (wxFileExists(fontPrefix + LIBERTINE8)) RemoveFontResource(fontPrefix + wxT(LIBERTINE8));
  if (wxFileExists(fontPrefix + LIBERTINE9)) RemoveFontResource(fontPrefix + wxT(LIBERTINE9));

  return true;
}
#endif

int window_counter = 0;

void MyApp::NewWindow(wxString file, bool evalOnStartup, bool exitAfterEval)
{
  int x = 40, y = 40, h = 650, w = 950, m = 0;
  int rs = 0;
  int display_width = 1024, display_height = 768;
  bool have_pos;

  wxConfig *config = (wxConfig *) wxConfig::Get();

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

  x += topLevelWindows.GetCount() * 20;
  y += topLevelWindows.GetCount() * 20;

  m_frame = new wxMaxima((wxFrame *) NULL, -1, _("wxMaxima"), m_configFileName,
                         wxPoint(x, y), wxSize(w, h));

  if (m == 1)
    m_frame->Maximize(true);

  if (file.Length() > 0)
  {
    m_frame->SetOpenFile(file);
  }

  m_frame->ExitAfterEval(exitAfterEval);
  m_frame->EvalOnStartup(evalOnStartup);
  topLevelWindows.Append(m_frame);
  if (topLevelWindows.GetCount() > 1)
    m_frame->SetTitle(wxString::Format(_("untitled %d"), ++window_counter));

  SetTopWindow(m_frame);
  m_frame->Show(true);
  m_frame->InitSession();
  m_frame->ShowTip(false);
}

void MyApp::OnFileMenu(wxCommandEvent &ev)
{
  switch (ev.GetId())
  {
    case wxMaximaFrame::menu_new_id:
    case ToolBar::tb_new:
    case wxMaxima::mac_newId:

      // Mac computers insist that all instances of a new application need to share
      // the same process. On all other OSes we create a separate process for each
      // window: This way if one instance of wxMaxima crashes all the other instances
      // are still alive.
#if defined __WXMAC__
      NewWindow();
#else
//      NewWindow();
      wxExecute(wxT("\"")+wxStandardPaths::Get().GetExecutablePath()+wxT("\""));
#endif
      break;
    case wxMaxima::mac_openId:
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
      bool quit = true;
      wxWindowList::compatibility_iterator node = topLevelWindows.GetFirst();
      while (node)
      {
        wxWindow *frame = node->GetData();
        node = node->GetNext();
        frame->Raise();
        if (!frame->Close())
        {
          quit = false;
          break;
        }
      }
      if (quit)
        wxExit();
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

BEGIN_EVENT_TABLE(MyApp, wxApp)
  EVT_MENU(wxMaximaFrame::menu_new_id, MyApp::OnFileMenu)
  EVT_TOOL(ToolBar::tb_new, MyApp::OnFileMenu)
END_EVENT_TABLE()
