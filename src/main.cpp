// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
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

#include "main.h"
#include "Dirstructure.h"
#include "wxMathml.h"
#include <iostream>
#include <wx/cmdline.h>
#include <wx/config.h>
#include <wx/fileconf.h>
#include <wx/fs_zip.h>
#include <wx/image.h>
#include <wx/intl.h>
#include <wx/translation.h>
#if wxCHECK_VERSION(3, 1, 6)
#include <wx/uilocale.h>
#endif
#include <wx/sysopt.h>
#include <wx/tipdlg.h>
#include <wx/utils.h>
#include <wx/wx.h>
#ifdef __WXMSW__
#include <windows.h>
#endif

#include "ConfigDialogue.h"
#include "Version.h"
#include "examples/CASvsProgrammingLanguage.h"
#include "examples/diffEquations.h"
#include "examples/displaying3DCurves.h"
#include "examples/fastListAccess.h"
#include "examples/fittingEquations.h"
#include "examples/memoizing.h"
#include "examples/numberFormats.h"
#include "examples/solvingEquations.h"
#include "examples/toleranceCalculations.h"
#include "examples/variableNames.h"

#include "wxMaxima.h"

// On wxGTK2 we support printing only if wxWidgets is compiled with gnome_print.
// We have to force gnome_print support to be linked in static builds of
// wxMaxima.

#if defined wxUSE_LIBGNOMEPRINT
#if wxUSE_LIBGNOMEPRINT
#include <wx/html/forcelnk.h>
FORCE_LINK(gnome_print)
#endif
#endif

IMPLEMENT_APP_NO_MAIN(MyApp);
IMPLEMENT_WX_THEME_SUPPORT;

wxDECLARE_APP(MyApp);

int CommonMain() {
  wxTheApp->CallOnInit();
  wxTheApp->OnRun();
  wxConfigBase *config = wxConfig::Get();
  config->Flush();
  delete config;
  if (CellPtrBase::GetLiveInstanceCount() != 0)
    wxLogDebug("CellPtr: %zu live instances leaked",
               CellPtrBase::GetLiveInstanceCount());
  if (Observed::GetLiveInstanceCount() != 0)
    wxLogDebug("Cell:    %zu live instances leaked",
               Observed::GetLiveInstanceCount());
  if (Observed::GetLiveControlBlockInstanceCount() != 0)
    wxLogDebug("ControlBlock: %zu live instances leaked",
               Observed::GetLiveControlBlockInstanceCount());
  return 0;
}
wxCmdLineParser cmdLineParser;
static const wxCmdLineEntryDesc cmdLineDesc[] = {
    {wxCMD_LINE_SWITCH, "v", "version", "Output the version info.",
     wxCMD_LINE_VAL_NONE, 0},
    /* Usually wxCMD_LINE_OPTION_HELP is used with the following option, but
     * that displays a message using its own window and we want the message on
     * the command line.  If a user enters a command line option, he expects
     * probably an answer just on the command line... */
    {wxCMD_LINE_SWITCH, "h", "help", "Show this help message.",
     wxCMD_LINE_VAL_NONE, 0},
    {wxCMD_LINE_OPTION, "o", "open", "Open a file.", wxCMD_LINE_VAL_STRING, 0},
    {wxCMD_LINE_SWITCH, "e", "eval", "Evaluate the file after opening it.",
     wxCMD_LINE_VAL_NONE, 0},
    {wxCMD_LINE_SWITCH, "", "single_process",
     "Open all files from within the same process.", wxCMD_LINE_VAL_NONE, 0},
    {wxCMD_LINE_SWITCH, "b", "batch",
     "Run the file and exit afterwards. Halts on questions and stops on "
     "errors.",
     wxCMD_LINE_VAL_NONE, 0},
    {wxCMD_LINE_SWITCH, "", "logtostderr",
     "Log all \"debug messages\" sidebar messages to stderr, too.",
     wxCMD_LINE_VAL_NONE, 0},
    {wxCMD_LINE_SWITCH, "", "debug",
     "Enable costly debug checks.",
     wxCMD_LINE_VAL_NONE, 0},
    {wxCMD_LINE_SWITCH, "", "pipe", "Pipe messages from Maxima to stderr.",
     wxCMD_LINE_VAL_NONE, 0},
    {wxCMD_LINE_SWITCH, "", "exit-on-error",
     "Close the program on any Maxima error.", wxCMD_LINE_VAL_NONE, 0},
    {wxCMD_LINE_OPTION, "f", "ini",
     "Allows to specify a file to store the configuration in.",
     wxCMD_LINE_VAL_STRING, 0},
    {wxCMD_LINE_OPTION, "u", "use-version", "Use Maxima version <str>.",
     wxCMD_LINE_VAL_STRING, 0},
    {wxCMD_LINE_OPTION, "l", "lisp",
     "Use a Maxima compiled with lisp compiler <str>.", wxCMD_LINE_VAL_STRING,
     0},
    {wxCMD_LINE_OPTION, "X", "extra-args",
     "Allows to specify extra Maxima arguments.", wxCMD_LINE_VAL_STRING, 0},
    {wxCMD_LINE_OPTION, "m", "maxima",
     "Allows to specify the location of the Maxima binary.",
     wxCMD_LINE_VAL_STRING, 0},
    {wxCMD_LINE_SWITCH, "", "enableipc",
     "Lets Maxima control wxMaxima via interprocess communications. Use this "
     "option with care.",
     wxCMD_LINE_VAL_NONE, 0},
    {wxCMD_LINE_OPTION, "", "wxmathml-lisp",
     "Location of wxMathML.lisp (if not the built-in should be used, mainly for developers).",
     wxCMD_LINE_VAL_STRING, 0},
    {wxCMD_LINE_PARAM, NULL, NULL, "input file", wxCMD_LINE_VAL_STRING,
     wxCMD_LINE_PARAM_OPTIONAL | wxCMD_LINE_PARAM_MULTIPLE},
     wxCMD_LINE_DESC_END};


#ifndef __WXMSW__
int main(int argc, char *argv[]) {
  wxEntryStart(argc, argv);
  cmdLineParser.SetCmdLine(argc, argv);
  cmdLineParser.SetDesc(cmdLineDesc);
  return CommonMain();
}
#else
int WINAPI WinMain(HINSTANCE hI, HINSTANCE hPrevI, LPSTR lpCmdLine,
                   int nCmdShow) {
  // // Only needed if we don't manage to ship the right manifest
  // #ifdef DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE
  // SetProcessDpiAwarenessContext(DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE);
  // #else
  //   #ifdef DPI_AWARENESS_CONTEXT_SYSTEM_AWARE
  //   SetProcessDpiAwarenessContext(DPI_AWARENESS_CONTEXT_SYSTEM_AWARE);
  //   #endif
  // #endif
  wxEntryStart(hI, hPrevI, lpCmdLine, nCmdShow);
  cmdLineParser.SetCmdLine(lpCmdLine);
  cmdLineParser.SetDesc(cmdLineDesc);
  return CommonMain();
}
#endif

bool MyApp::OnInit() {
  wxLogStderr noErrorDialogs;
  m_translations = std::unique_ptr<wxTranslations>(new wxTranslations());
  wxTranslations::Set(m_translations.get());
  // connect to wxMaxima. We therefore delay all output to the log until there
  // is a window that can display it on the GUI instead.
  wxLogBuffer noStdErr;
  {
    // If supported: Generate symbolic backtraces on crashes.
#if wxUSE_ON_FATAL_EXCEPTION
    wxHandleFatalExceptions(true);
#endif
    int major;
    int minor;
    wxGetOsVersion(&major, &minor);

    // Directdraw should be faster, but crashes on closing on Win7.
    if ((major > 6) || ((major == 6) && (minor > 1)))
      wxSystemOptions::SetOption("msw.display.directdraw", "1");
    else
      wxSystemOptions::SetOption("msw.display.directdraw", "0");
    // No spell checking in our dialog's input portions on the mac.
    wxSystemOptions::SetOption("mac.textcontrol-use-spell-checker", "0");

    // Migrate an eventual old config file to the location XDG wants it to be.
#ifndef __WXMSW__
#if wxCHECK_VERSION(3, 1, 1)
    wxStandardPaths::Get().SetFileLayout(wxStandardPaths::FileLayout_Classic);
    wxString configFileOld =
      wxStandardPaths::Get().GetUserConfigDir() + wxS("/") +
      wxStandardPaths::Get().MakeConfigFileName(
						wxString(wxS("wxMaxima")), wxStandardPaths::ConfigFileConv_Dot);
    wxStandardPaths::Get().SetFileLayout(wxStandardPaths::FileLayout_XDG);
    wxString configFileXDG =
      wxStandardPaths::Get().GetUserConfigDir() + wxS("/") +
      wxStandardPaths::Get().MakeConfigFileName(
						wxString(wxS("wxMaxima")), wxStandardPaths::ConfigFileConv_Ext);

    if (!wxFileExists(configFileXDG)) {
      wxFileName xdgDir(configFileXDG);
      wxString dirName(xdgDir.GetPath());
      if (!wxDirExists(dirName))
        wxMkdir(dirName, 0x700);
      if (wxFileExists(configFileOld))
        wxCopyFile(configFileOld, configFileXDG);
    }
#endif
#endif

    wxLanguage lang;
    {
      long lng = wxLocale::GetSystemLanguage();
      wxConfig(wxS("wxMaxima"), wxEmptyString, m_configFileName)
	.Read(wxS("language"), &lng);
      if (lng == wxLANGUAGE_UNKNOWN)
	lng = wxLANGUAGE_DEFAULT;
      lang = static_cast<wxLanguage>(lng);
    }

    {
#if wxCHECK_VERSION(3, 1, 6)
      wxLogNull suppressErrorMessages;
      wxUILocale::UseDefault();
#else
      m_locale = std::unique_ptr<wxLocale>(new wxLocale);
      m_locale->Init(lang);
#endif
    }

    // Create the temporary directory if it doesn't exist
    // On some platforms, the temporary directory has an application-specific
    // path element prepended doesn't exist by default in the temporary
    // directory. We must create it, otherwise all uses  of the temporary
    // directory will fail. This happens e.g. on Windows 10.
    auto const tempDir = wxStandardPaths::Get().GetTempDir();
    if (!wxDirExists(tempDir))
      wxMkdir(tempDir, 0x700);

    wxFileTranslationsLoader::AddCatalogLookupPathPrefix(m_dirstruct.LocaleDir());
    wxFileTranslationsLoader::AddCatalogLookupPathPrefix(m_dirstruct.LocaleDir() +
							 wxS("/wxwin"));
    wxFileTranslationsLoader::AddCatalogLookupPathPrefix(wxS("/usr/share/locale"));
    wxFileTranslationsLoader::AddCatalogLookupPathPrefix(wxS("/usr/local/share/locale"));

    if (wxLocale::IsAvailable(lang))
      wxTranslations::Get()->SetLanguage(lang);

    // Do we reckon we improve something if we set maxima's language, as well?
    if ((wxLocale::IsAvailable(lang)) && (lang != wxLANGUAGE_DEFAULT)) {
      // Set maxima's language, as well.
      wxString localeName = wxLocale().GetCanonicalName();
      if (lang != wxLocale::GetSystemLanguage()) {
        if (wxLocale().GetSystemEncoding() == wxFONTENCODING_UTF16)
          localeName += wxS(".UTF-16");
        else {
          if (wxLocale().GetSystemEncoding() == wxFONTENCODING_UTF32)
            localeName += wxS(".UTF-32");
          else {
            localeName += wxS(".UTF-8");
          }
        }
        wxSetEnv(wxS("LANG"), localeName);
      }
    }
    wxTranslations::Get()->AddCatalog(wxS("wxMaxima"));
    /* wxWidgets introduced version suffixes to gettext catalogs, see:
     * https://github.com/wxWidgets/wxWidgets/commit/ded4da5 */
    /* so try to load a catalog with this suffix */
    wxTranslations::Get()->AddCatalog("wxstd-"
				      wxSTRINGIZE(wxMAJOR_VERSION) "."
				      wxSTRINGIZE(wxMINOR_VERSION));
    wxTranslations::Get()->AddCatalog(wxS("wxstd"));
  }

  bool exitAfterEval = false;
  bool evalOnStartup = false;

  int cmdLineError = cmdLineParser.Parse();

  if (cmdLineParser.Found(wxS("single_process")))
    m_allWindowsInOneProcess = true;

  if (cmdLineParser.Found(wxS("h"))) {
    std::cout << "A feature-rich graphical user interface for the computer "
      "algebra system Maxima\n";
    std::cout << cmdLineParser.GetUsageString();
    exit(0);
  }

  if (cmdLineError != 0)
    exit(-1);

  wxString ini, file;
  // Attention: The config file is changed by
  // wxMaximaFrame::wxMaximaFrame::ReReadConfig
  if (cmdLineParser.Found(wxS("f"), &ini)) {
    wxFileName configFile(ini);
    configFile.MakeAbsolute();
    Configuration::m_configfileLocation_override = configFile.GetFullPath();
    wxConfig::Set(
		  new wxFileConfig(wxS("wxMaxima"), wxEmptyString,
				   Configuration::m_configfileLocation_override));
  } else
    wxConfig::Set(new wxConfig(wxS("wxMaxima")));

  if (cmdLineParser.Found(wxS("logtostderr")))
    ErrorRedirector::LogToStdErr();

  if (cmdLineParser.Found(wxS("debug")))
    Configuration::SetDebugmode();

  if (cmdLineParser.Found(wxS("pipe")))
    wxMaxima::PipeToStdout();

  if (cmdLineParser.Found(wxS("exit-on-error")))
    wxMaxima::ExitOnError();

  if (cmdLineParser.Found(wxS("enableipc")))
    wxMaxima::EnableIPC();

  wxString extraMaximaArgs;
  wxString arg;
  if (cmdLineParser.Found(wxS("l"), &arg))
    extraMaximaArgs += " -l " + arg;

  if (cmdLineParser.Found(wxS("X"), &arg))
    extraMaximaArgs += " -X " + arg;

  if (cmdLineParser.Found(wxS("u"), &arg))
    extraMaximaArgs += " -u " + arg;

  wxMaxima::ExtraMaximaArgs(extraMaximaArgs);

  if (cmdLineParser.Found(wxS("wxmathml-lisp"), &arg)) {
    wxMathML::Set_MathML_Filename(arg);
  }

  if (cmdLineParser.Found(wxS("m"), &arg)) {
    wxMaxima::Set_Maxima_Commandline_Filename(arg);
  }

  wxImage::AddHandler(new wxPNGHandler);
  wxImage::AddHandler(new wxXPMHandler);
  wxImage::AddHandler(new wxJPEGHandler);
  wxImage::AddHandler(new wxGIFHandler);

  wxFileSystem::AddHandler(new wxZipFSHandler);

#ifdef __WXMSW__
  wxString oldWorkingDir = wxGetCwd();
  if (!wxGetEnv(wxS("BUILD_DIR"), NULL)) {
    wxString dir = wxPathOnly(wxStandardPaths::Get().GetExecutablePath());
    if (dir != wxEmptyString)
      wxSetWorkingDirectory(
			    wxPathOnly(wxStandardPaths::Get().GetExecutablePath()));
  }
#if wxCHECK_VERSION(3, 1, 1)
  wxString fontPrefix = m_dirstruct.FontDir() + wxS("/");

  wxSetWorkingDirectory(oldWorkingDir);
#endif
#endif

  Connect(wxEVT_MENU, wxCommandEventHandler(MyApp::OnFileMenu), NULL, this);

#if defined __WXOSX__
  wxString path;
  wxGetEnv(wxS("PATH"), &path);
  wxSetEnv(wxS("PATH"), path << wxS(":/usr/local/bin"));

  //! The macintosh global menu
  wxMenuBar *menubar = new wxMenuBar;
  wxMenu *menu = new wxMenu;
  menu->Append(wxID_NEW, _("&New\tCtrl+N"));
  menu->Append(wxID_OPEN, _("&Open\tCtrl+O"));
  menu->Append(wxID_PREFERENCES, _("Preferences"));
  menu->Append(wxID_EXIT, _("Exit"));
  menubar->Append(menu, _("File"));
  // add open, new, etc options to your menubar.
  wxMenuBar::MacSetCommonMenuBar(menubar);

  menubar->Connect(wxEVT_COMMAND_MENU_SELECTED,
                   wxCommandEventHandler(MyApp::OnFileMenu), NULL, this);
  Connect(wxEVT_COMMAND_MENU_SELECTED, wxCommandEventHandler(MyApp::OnFileMenu),
          NULL, this);
  wxApp::SetExitOnFrameDelete(false);
#endif

  if (cmdLineParser.Found(wxS("v"))) {
    std::cout << "wxMaxima ";
    std::cout << GITVERSION;
#if defined(WXMAXIMA_GIT_VERSION)
    std::cout << " (Git version: " << WXMAXIMA_GIT_VERSION << ")";
#endif
    std::cout << "\n";
    exit(0);
  }

  if (cmdLineParser.Found(wxS("b"))) {
    evalOnStartup = true;
    exitAfterEval = true;
  }

  if (cmdLineParser.Found(wxS("e")))
    evalOnStartup = true;

  bool windowOpened = false;

  if (cmdLineParser.Found(wxS("o"), &file)) {
    wxFileName FileName = file;
    FileName.MakeAbsolute();
    wxString canonicalFilename = FileName.GetFullPath();
    NewWindow(canonicalFilename, evalOnStartup, exitAfterEval);
    windowOpened = true;
  }

  if (cmdLineParser.GetParamCount() > 0) {
    for (unsigned int i = 0; i < cmdLineParser.GetParamCount(); i++) {
      wxFileName FileName = cmdLineParser.GetParam(i);
      FileName.MakeAbsolute();

      wxString CanonicalFilename = FileName.GetFullPath();
      NewWindow(CanonicalFilename, evalOnStartup, exitAfterEval);
    }
    windowOpened = true;
  }

  if (!windowOpened)
    NewWindow();

  // Now we can finally send our debug output to a window without making
  // std::cerr confusing the mac.
  wxString logMessagesSoFar = noStdErr.GetBuffer();
  if (!logMessagesSoFar.IsEmpty())
    wxLogMessage("Log messages during early startup: %s", logMessagesSoFar.mb_str());
  return true;
}

int MyApp::OnExit() { return 0; }

int MyApp::OnRun() {
  wxLogStderr noErrorDialogs;
  wxApp::OnRun();
  return 0;
}

void MyApp::NewWindow(const wxString &file, bool evalOnStartup,
                      bool exitAfterEval, unsigned char *wxmData,
                      size_t wxmLen) {
  int numberOfWindows = wxMaximaFrame::m_topLevelWindows.size();

  wxString title = _("wxMaxima");
  if (file.Length() > 0)
    title = file;

  if (numberOfWindows > 1)
    title = wxString::Format(_("wxMaxima %d"), numberOfWindows);

  wxMaxima *frame = new wxMaxima(NULL, -1, title, file);
  if (wxmData) {
    // Unzip the .wxm file
    wxMemoryInputStream istream(wxmData, wxmLen);
    wxTextInputStream textIn(istream, "\t", wxConvAuto(wxFONTENCODING_UTF8));
    wxString initialContents;
    wxString line;
    wxString block;
    while (!istream.Eof()) {
      line = textIn.ReadLine();
      if ((line.StartsWith("/*")) || (line.EndsWith("*/"))) {
        initialContents += _(block);
        initialContents += line + "\n";
        block = wxEmptyString;
      } else
        block += line + "\n";
    }
    initialContents += _(block);
    frame->SetWXMdata(initialContents);
  }

  frame->EvalOnStartup(evalOnStartup);
  wxMaximaFrame::m_topLevelWindows.push_back(frame);
  frame->ExitAfterEval(exitAfterEval);

  SetTopWindow(frame);
  frame->Show(true);
  frame->ShowTip(false);
}

void MyApp::OnFileMenu(wxCommandEvent &ev) {
  if(ev.GetId() == EventIDs::menu_help_numberformats)
    {
      NewWindow(wxEmptyString, false, false, NUMBERFORMATS_WXM,
		NUMBERFORMATS_WXM_SIZE);
    }
  else if(ev.GetId() == EventIDs::menu_help_3d)
    {
      NewWindow(wxEmptyString, false, false, DISPLAYING3DCURVES_WXM,
		DISPLAYING3DCURVES_WXM_SIZE);
    }
  else if(ev.GetId() == EventIDs::menu_help_varnames) {
    NewWindow(wxEmptyString, false, false, VARIABLENAMES_WXM,
              VARIABLENAMES_WXM_SIZE);
  }
  else if(ev.GetId() == EventIDs::menu_help_listaccess) {
    NewWindow(wxEmptyString, false, false, FASTLISTACCESS_WXM,
              FASTLISTACCESS_WXM_SIZE);
  }
  else if(ev.GetId() == EventIDs::menu_help_fittingData) {
    NewWindow(wxEmptyString, false, false, FITTINGEQUATIONS_WXM,
              FITTINGEQUATIONS_WXM_SIZE);
  }
  else if(ev.GetId() == EventIDs::menu_help_solving) {
    NewWindow(wxEmptyString, false, false, SOLVINGEQUATIONS_WXM,
              SOLVINGEQUATIONS_WXM_SIZE);
  }
  else if(ev.GetId() == EventIDs::menu_help_diffequations) {
    NewWindow(wxEmptyString, false, false, DIFFEQUATIONS_WXM,
              DIFFEQUATIONS_WXM_SIZE);
    }
  else if(ev.GetId() == EventIDs::menu_help_tolerances) {
    NewWindow(wxEmptyString, false, false, TOLERANCECALCULATIONS_WXM,
              TOLERANCECALCULATIONS_WXM_SIZE);
  }
  else if(ev.GetId() == EventIDs::menu_help_memoizing) {
    NewWindow(wxEmptyString, false, false, MEMOIZING_WXM, MEMOIZING_WXM_SIZE);
  }
  else if(ev.GetId() == EventIDs::menu_help_casvsprogramming) {
    NewWindow(wxEmptyString, false, false, CASVSPROGRAMMINGLANGUAGE_WXM, CASVSPROGRAMMINGLANGUAGE_WXM_SIZE);
  }
  else if(ev.GetId() == wxID_OPEN) {
    wxString lastPath;
    wxConfig::Get()->Read(wxS("lastPath"), &lastPath);
    wxString file =
      wxFileSelector(_("Open"), wxEmptyString, wxEmptyString, wxEmptyString,
		     _("All openable types (*.wxm, *.wxmx, *.mac, *.out, "
		       "*.xml)|*.wxm;*.wxmx;*.mac;*.out;*.xml|"
		       "wxMaxima document (*.wxm, *.wxmx)|*.wxm;*.wxmx|"
		       "Maxima session (*.mac)|*.mac|"
		       "Xmaxima session (*.out)|*.out|"
		       "xml from broken .wxmx (*.xml)|*.xml"),
		     wxFD_OPEN);
    if (!file.empty()) {
      // On the mac the "File/New" menu item by default opens a new window
      // instead of reusing the old one.
      NewWindow(file);
    }
  }
  else if(ev.GetId() == wxID_NEW) {
      // Mac computers insist that all instances of a new application need to
      // share the same process. On all other OSes we create a separate process
      // for each window: This way if one instance of wxMaxima crashes all the
      // other instances are still alive.
    if (m_allWindowsInOneProcess)
      NewWindow();
    else {
      // Compile a list of arguments we want to pass to the new process
      std::vector<wxString> args;
      if (Configuration::m_configfileLocation_override != wxEmptyString)
	{
	  args.push_back("-f");
	  args.push_back(Configuration::m_configfileLocation_override);
	}
      if (wxMaxima::Get_Maxima_Commandline_Filename() != wxEmptyString)
	{
	  args.push_back("-m");
	  args.push_back(wxMaxima::Get_Maxima_Commandline_Filename());
	}
      if (ErrorRedirector::LoggingToStdErr())
	  args.push_back("--logtostderr");
      if (Configuration::GetDebugmode())
	  args.push_back("--debug");
      if (wxMaxima::GetPipeToStdout())
	  args.push_back("--pipe");
      if (wxMaxima::GetExitOnError())
	  args.push_back("--exit-on-error");
      if (wxMaxima::GetEnableIPC())
	  args.push_back("--enableipc");
      if (!wxMaxima::ExtraMaximaArgs().IsEmpty())
	{
	  args.push_back("-u");
	  args.push_back(wxMaxima::ExtraMaximaArgs());
	}

      // wxExecute wants the arguments as C strings, not as wxStrings =>
      // generate those C strings
      std::vector<wxCharBuffer> args_c_strings;
      for(const auto &i : args)
	args_c_strings.push_back(wxCharBuffer(i.mb_str()));

      // Additionally wxExecute expects these C strings in a C array.
      // Let's generate an unique pointer to that one so C++ automatically destroys it
      // once it is no more needed.
      std::unique_ptr<char *> args_array(new char*[args.size() + 2]);
      wxCharBuffer executableName(wxStandardPaths::Get().GetExecutablePath().mb_str());
      args_array.get()[0] = executableName.data();;
      args_array.get()[args.size() + 1] = NULL;
      for(size_t i = 0; i< args.size(); i++)
	args_array.get()[i + 1] = args_c_strings[i].data();
      wxExecute(args_array.get());

      wxString command;
      for(size_t i = 0; i< args.size() + 1; i++)
	command += wxString::FromUTF8(args_array.get()[i]) + "\n";
      command.Trim();
      wxLogMessage(_("Starting a new wxMaxima process as: %s"), command.mb_str());
    }
  }
  else if(ev.GetId() == wxID_PREFERENCES) {
    Configuration config;
    ConfigDialogue *configW = new ConfigDialogue(NULL, &config);
    configW->Centre(wxBOTH);
    if (configW->ShowModal() == wxID_OK)
      configW->WriteSettings();

    configW->Destroy();
    wxConfig::Get()->Flush();
  }
  else if(ev.GetId() ==  wxID_EXIT) {
    for (wxMaximaFrame *win : wxMaximaFrame::m_topLevelWindows) {
      wxASSERT(win);
      wxCloseEvent *event = new wxCloseEvent(wxEVT_CLOSE_WINDOW);
      event->SetCanVeto(true);
      event->SetLoggingOff(false);
      if(win)
	win->GetEventHandler()->QueueEvent(event);
    }
    if (wxMaximaFrame::m_topLevelWindows.empty())
      wxExit();
  }
}

void MyApp::MacNewFile() {
  wxWindow *frame = GetTopWindow();
  if (frame == NULL)
    NewWindow();
}

void MyApp::MacOpenFile(const wxString &file) { NewWindow(file); }
