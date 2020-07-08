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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
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
#include <wx/utils.h> 
#include <wx/cmdline.h>
#include <wx/fileconf.h>
#include <wx/sysopt.h>
#include "Dirstructure.h"
#include <iostream>

#include "../examples/examples.h"
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


IMPLEMENT_APP_NO_MAIN(MyApp);
IMPLEMENT_WX_THEME_SUPPORT;

int CommonMain()
{
  wxTheApp->CallOnInit();
  #pragma omp parallel
  #pragma omp master
  wxTheApp->OnRun();
  wxConfigBase *config = wxConfig::Get();
  config->Flush();
  delete config;
  if(CellPtrBase::GetLiveInstanceCount() != 0)
    wxLogDebug("CellPtr: %zu live instances leaked", CellPtrBase::GetLiveInstanceCount());
  if(Observed::GetLiveInstanceCount() != 0)
    wxLogDebug("Cell:    %zu live instances leaked", Observed::GetLiveInstanceCount());
  return 0;
}

#ifndef __WXMSW__
int main(int argc, char *argv[])
{
  wxEntryStart( argc, argv );
  return CommonMain();
}
#else
int WINAPI WinMain( HINSTANCE hI, HINSTANCE hPrevI, LPSTR lpCmdLine, int nCmdShow )
{
  wxEntryStart(hI, hPrevI, lpCmdLine, nCmdShow);
  return CommonMain();
}
#endif

std::vector<wxMaxima *> MyApp::m_topLevelWindows;


bool MyApp::OnInit()
{
  // On the Mac if any of these commands outputs text to stderr maxima fails to
  // connect to wxMaxima. We therefore delay all output to the log until there
  // is a window that can display it on the GUI instead.
  wxLogBuffer noStdErr;
  {
    
    Connect(wxID_NEW, wxEVT_MENU, wxCommandEventHandler(MyApp::OnFileMenu), NULL, this);
    Connect(wxMaximaFrame::menu_help_tutorials_start, wxMaximaFrame::menu_help_tutorials_end,
            wxEVT_MENU, wxCommandEventHandler(MyApp::OnFileMenu), NULL, this);
    
    // If supported: Generate symbolic backtraces on crashes.
    #if wxUSE_ON_FATAL_EXCEPTION
    wxHandleFatalExceptions(true);
    #endif
    int major;
    int minor;
    wxGetOsVersion(&major, &minor);
    
    // Directdraw should be faster, but crashes on closing on Win7.
    if((major > 6) || ((major == 6) && (minor >1)))
      wxSystemOptions::SetOption("msw.display.directdraw","1");
    else
      wxSystemOptions::SetOption("msw.display.directdraw","0");
    // No spell checking in our dialog's input portions on the mac.
    wxSystemOptions::SetOption("mac.textcontrol-use-spell-checker","0");
    
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
    
    m_locale.AddCatalogLookupPathPrefix(m_dirstruct.LocaleDir());
    m_locale.AddCatalogLookupPathPrefix(m_dirstruct.LocaleDir() + wxT("/wxwin"));
    m_locale.AddCatalogLookupPathPrefix(wxT("/usr/share/locale"));
    m_locale.AddCatalogLookupPathPrefix(wxT("/usr/local/share/locale"));
    long lang = wxLocale::GetSystemLanguage();
    wxConfig(wxT("wxMaxima"), wxEmptyString, m_configFileName).Read(wxT("language"), &lang);
    if(lang == wxLANGUAGE_UNKNOWN)
      lang = wxLANGUAGE_DEFAULT;
    m_locale.Init(lang);
    
    // Do we reckong we improve something if we set maxima's language, as well?
    if((wxLocale::IsAvailable(lang)) && (lang != wxLANGUAGE_DEFAULT))
    {
      // Set maxima's language, as well.
      wxString localeName = m_locale.GetCanonicalName();
      if(lang != wxLocale::GetSystemLanguage())
      {
        if(m_locale.GetSystemEncoding() == wxFONTENCODING_UTF16)
          localeName+=wxT(".UTF-16");
        else
        {
          if(m_locale.GetSystemEncoding() == wxFONTENCODING_UTF32)
            localeName+=wxT(".UTF-32");
          else
          {
            localeName+=wxT(".UTF-8");
          }            
        }
        wxSetEnv(wxT("LANG"), localeName);
      }
    }
    m_locale.AddCatalog(wxT("wxMaxima"));
    m_locale.AddCatalog(wxT("wxMaxima-wxstd"));
  }
  
  bool exitAfterEval = false;
  bool evalOnStartup = false;
  wxCmdLineParser cmdLineParser(argc, argv);
  
  static const wxCmdLineEntryDesc cmdLineDesc[] =
    {
      {wxCMD_LINE_SWITCH, "v", "version", "Output the version info", wxCMD_LINE_VAL_NONE , 0},
      /* Usually wxCMD_LINE_OPTION_HELP is used with the following option, but that displays a message
       * using a own window and we want the message on the command line. If a user enters a command
       * line option, he expects probably a answer just on the command line... */
      {wxCMD_LINE_SWITCH, "h", "help", "show this help message", wxCMD_LINE_VAL_NONE, 0},
      {wxCMD_LINE_OPTION, "o", "open", "open a file", wxCMD_LINE_VAL_STRING , 0},
      {wxCMD_LINE_SWITCH, "e", "eval",
       "evaluate the file after opening it.", wxCMD_LINE_VAL_NONE , 0},
      {wxCMD_LINE_SWITCH, "b", "batch",
       "run the file and exit afterwards. Halts on questions and stops on errors.",  wxCMD_LINE_VAL_NONE, 0},
                  {wxCMD_LINE_SWITCH, "", "logtostdout",
                   "Log all \"debug messages\" sidebar messages to stderr, too.",  wxCMD_LINE_VAL_NONE, 0},
                  {wxCMD_LINE_SWITCH, "", "pipe",
                   "Pipe messages from Maxima to stdout.",  wxCMD_LINE_VAL_NONE, 0},
                  {wxCMD_LINE_SWITCH, "", "exit-on-error",
                   "Close the program on any Maxima error.",  wxCMD_LINE_VAL_NONE, 0},
                  {wxCMD_LINE_OPTION, "f", "ini", "allows to specify a file to store the configuration in", wxCMD_LINE_VAL_STRING , 0},
                  {wxCMD_LINE_OPTION, "u", "use-version",
                   "Use Maxima version <str>.",  wxCMD_LINE_VAL_STRING, 0},
                  {wxCMD_LINE_OPTION, "l", "lisp",
                   "Use a Maxima compiled with lisp compiler <str>.",  wxCMD_LINE_VAL_STRING, 0},
                  {wxCMD_LINE_OPTION, "X", "extra-args",
                   "Allows to specify extra Maxima arguments",  wxCMD_LINE_VAL_STRING, 0},
                  { wxCMD_LINE_OPTION, "m", "maxima", "allows to specify the location of the Maxima binary", wxCMD_LINE_VAL_STRING , 0},
                  {wxCMD_LINE_PARAM, NULL, NULL, "input file", wxCMD_LINE_VAL_STRING, wxCMD_LINE_PARAM_OPTIONAL | wxCMD_LINE_PARAM_MULTIPLE},
            {wxCMD_LINE_NONE, "", "", "", wxCMD_LINE_VAL_NONE, 0}
          };
       
  cmdLineParser.SetDesc(cmdLineDesc);
  int cmdLineError = cmdLineParser.Parse();

  if (cmdLineParser.Found(wxT("h")))
  {
    std::cout << "A feature-rich graphical user interface for the computer algebra system Maxima\n";
    std::cout << cmdLineParser.GetUsageString();
    exit(0);
  }

  if(cmdLineError != 0)
    exit(-1);
  
  wxString ini, file;
  // Attention: The config file is changed by wxMaximaFrame::wxMaximaFrame::ReReadConfig
  if (cmdLineParser.Found(wxT("f"),&ini))
  {
    wxFileName configFile(ini);
    configFile.MakeAbsolute();
    Configuration::m_configfileLocation_override = configFile.GetFullPath();
    wxConfig::Set(new wxConfig(wxT("wxMaxima"), wxEmptyString,
                               Configuration::m_configfileLocation_override));
  }
  else
    wxConfig::Set(new wxConfig(wxT("wxMaxima")));
  
  if (cmdLineParser.Found(wxT("logtostdout")))
    ErrorRedirector::LogToStdErr();

  if (cmdLineParser.Found(wxT("pipe")))
    wxMaxima::PipeToStdout();

  if (cmdLineParser.Found(wxT("exit-on-error")))
    wxMaxima::ExitOnError();

  wxString extraMaximaArgs;
  wxString arg;
  if (cmdLineParser.Found(wxT("l"), &arg))
    extraMaximaArgs += " -l " +  arg;
  
  if (cmdLineParser.Found(wxT("X"), &arg))
    extraMaximaArgs += " -X " +  arg;

  if (cmdLineParser.Found(wxT("u"), &arg))
    extraMaximaArgs += " -u " +  arg;

  wxMaxima::ExtraMaximaArgs(extraMaximaArgs);
  
  wxImage::AddHandler(new wxPNGHandler);
  wxImage::AddHandler(new wxXPMHandler);
  wxImage::AddHandler(new wxJPEGHandler);
  wxImage::AddHandler(new wxGIFHandler);

  wxFileSystem::AddHandler(new wxZipFSHandler);

#ifdef __WXMSW__
  wxString oldWorkingDir = wxGetCwd();
  if (!wxGetEnv(wxT("BUILD_DIR"), NULL))
  {
    wxString dir = wxPathOnly(wxStandardPaths::Get().GetExecutablePath());
    if(dir != wxEmptyString)
      wxSetWorkingDirectory(wxPathOnly(wxStandardPaths::Get().GetExecutablePath()));
  }
#if wxCHECK_VERSION(3, 1, 1)
  wxString fontPrefix = m_dirstruct.FontDir() + wxT("/");  

  /* Add private Libertine fonts, if they exist */
  if (wxFileExists(fontPrefix + wxT(LIBERTINE1))) 
	  wxFont::AddPrivateFont(fontPrefix + wxT(LIBERTINE1));
  if (wxFileExists(fontPrefix + wxT(LIBERTINE2))) wxFont::AddPrivateFont(fontPrefix + wxT(LIBERTINE2));
  if (wxFileExists(fontPrefix + wxT(LIBERTINE3))) wxFont::AddPrivateFont(fontPrefix + wxT(LIBERTINE3));
  if (wxFileExists(fontPrefix + wxT(LIBERTINE4))) wxFont::AddPrivateFont(fontPrefix + wxT(LIBERTINE4));
  if (wxFileExists(fontPrefix + wxT(LIBERTINE5))) wxFont::AddPrivateFont(fontPrefix + wxT(LIBERTINE5));
  if (wxFileExists(fontPrefix + wxT(LIBERTINE6))) wxFont::AddPrivateFont(fontPrefix + wxT(LIBERTINE6));
  if (wxFileExists(fontPrefix + wxT(LIBERTINE7))) wxFont::AddPrivateFont(fontPrefix + wxT(LIBERTINE7));
  if (wxFileExists(fontPrefix + wxT(LIBERTINE8))) wxFont::AddPrivateFont(fontPrefix + wxT(LIBERTINE8));
  if (wxFileExists(fontPrefix + wxT(LIBERTINE9))) wxFont::AddPrivateFont(fontPrefix + wxT(LIBERTINE9));
#endif
  wxSetWorkingDirectory(oldWorkingDir);

#endif

#if defined __WXOSX__
  wxString path;
  wxGetEnv(wxT("PATH"), &path);
  wxSetEnv(wxT("PATH"), path << wxT(":/usr/local/bin"));

  wxApp::SetExitOnFrameDelete(false);
  wxMenuBar::SetAutoWindowMenu(true);
  wxMenuBar *menuBar = new wxMenuBar;
  // Enables the window list on MacOs.
  menuBar->SetAutoWindowMenu(true);
  wxMenu *fileMenu = new wxMenu;
  fileMenu->Append(wxID_NEW, _("&New\tCtrl+N"));
  fileMenu->Append(wxID_OPEN, _("&Open\tCtrl+O"));
  menuBar->Append(fileMenu, _("File"));
  wxMenuBar::MacSetCommonMenuBar(menuBar);
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
    exit(0);
  }

  if (cmdLineParser.Found(wxT("b")))
  {
    evalOnStartup = true;
    exitAfterEval = true;
  }
  
  if (cmdLineParser.Found(wxT("e")))
    evalOnStartup = true;

  bool windowOpened = false;
  
  if (cmdLineParser.Found(wxT("o"), &file))
  {
    wxFileName FileName = file;
    FileName.MakeAbsolute();
    wxString canonicalFilename = FileName.GetFullPath();
    NewWindow(canonicalFilename, evalOnStartup, exitAfterEval);
    windowOpened = true;
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
    windowOpened = true;
  }

  if(!windowOpened)
    NewWindow();

  // Now we can finally send our debug output to a window without making
  // std::cerr confusing the mac.
  wxString logMessagesSoFar = noStdErr.GetBuffer();
  if(!logMessagesSoFar.IsEmpty())
    wxLogMessage("Log messages during early startup: " + logMessagesSoFar);
  return true;
}

int MyApp::OnExit()
{
  return 0;
}

int MyApp::OnRun()
{
  wxApp::OnRun();
  return 0;
}

void MyApp::NewWindow(const wxString &file, bool evalOnStartup, bool exitAfterEval, unsigned char *wxmData, int wxmLen)
{
  int numberOfWindows = m_topLevelWindows.size();

  wxString title = _("wxMaxima");
  if (file.Length() > 0)
    title = file;
  
  if (numberOfWindows > 1)
    title = wxString::Format(_("wxMaxima %d"), numberOfWindows);

  wxMaxima *frame = new wxMaxima(NULL, -1, &m_locale, title, file);
  if (wxmData)
  {
    // Unzip the .wxm file
    wxMemoryInputStream istream(wxmData, wxmLen);
    wxZlibInputStream zstream(istream);
    wxTextInputStream textIn(zstream, "\t", wxConvAuto(wxFONTENCODING_UTF8));
    wxString initialContents;
    wxString line;
    wxString block;
    while(!zstream.Eof())
    {
      line = textIn.ReadLine();
      if((line.StartsWith("/*")) || (line.EndsWith("*/")))
      {
        initialContents += _(block);
        initialContents += line + "\n";
        block = wxEmptyString;
      }
      else
        block += line + "\n";
    }
    initialContents += _(block);
    frame->SetWXMdata(initialContents);
  }
  
  frame->ExitAfterEval(exitAfterEval);
  frame->EvalOnStartup(evalOnStartup);
  m_topLevelWindows.push_back(frame);

  SetTopWindow(frame);
  frame->Show(true);
  frame->ShowTip(false);
}

void MyApp::OnFileMenu(wxCommandEvent &ev)
{
  switch (ev.GetId())
  {
  case wxMaxima::menu_help_numberformats:
    NewWindow(wxEmptyString, false, false,
              numberFormats_wxm_gz, numberFormats_wxm_gz_len);
    break;
  case wxMaxima::menu_help_3d:
    NewWindow(wxEmptyString, false, false,
              displaying3DCurves_wxm_gz, displaying3DCurves_wxm_gz_len);
    break;

  case wxMaxima::menu_help_varnames:
    NewWindow(wxEmptyString, false, false,
              variableNames_wxm_gz, variableNames_wxm_gz_len);
    break;

  case wxMaxima::menu_help_listaccess:
    NewWindow(wxEmptyString, false, false,
              fastListAccess_wxm_gz, fastListAccess_wxm_gz_len);
    break;

  case wxMaxima::menu_help_fittingData:
    NewWindow(wxEmptyString, false, false,
              fittingEquations_wxm_gz, fittingEquations_wxm_gz_len);
    break;
  case wxMaxima::menu_help_solving:
    NewWindow(wxEmptyString, false, false,
              solvingEquations_wxm_gz, solvingEquations_wxm_gz_len);
    break;
  case wxMaxima::menu_help_diffequations:
    NewWindow(wxEmptyString, false, false,
              diffEquations_wxm_gz, diffEquations_wxm_gz_len);
    break;
  case wxMaxima::menu_help_tolerances:
    NewWindow(wxEmptyString, false, false,
              toleranceCalculations_wxm_gz, toleranceCalculations_wxm_gz_len);
    break;
    case wxID_NEW:
    {
      // Mac computers insist that all instances of a new application need to share
      // the same process. On all other OSes we create a separate process for each
      // window: This way if one instance of wxMaxima crashes all the other instances
      // are still alive.
#if defined __WXOSX__
      NewWindow();
#else
//      NewWindow();
      wxString args;
      if(Configuration::m_configfileLocation_override != wxEmptyString)
        args += " -f \"" + Configuration::m_configfileLocation_override + "\"";
      if(Configuration::m_maximaLocation_override != wxEmptyString)
        args += " -m \"" + Configuration::m_maximaLocation_override + "\"";

      wxExecute(wxT("\"") + wxStandardPaths::Get().GetExecutablePath() + wxT("\"") + args);
#endif
      break;
    }
    case wxID_EXIT:
    {
      for (wxMaxima *win : m_topLevelWindows)
      {
        wxASSERT(win);
        wxCloseEvent *event = new wxCloseEvent(wxEVT_CLOSE_WINDOW);
        event->SetCanVeto(true);
        event->SetLoggingOff(false);
        win->GetEventHandler()->QueueEvent(event);
      }
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
