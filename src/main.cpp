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
#include "Maxima.h"
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
#include <wx/debugrpt.h>
#if wxCHECK_VERSION(3, 1, 6)
#include <wx/uilocale.h>
#endif
#include <wx/sysopt.h>
#include <wx/tipdlg.h>
#include <wx/utils.h>
#include <wx/wx.h>
#include <vector>
#ifdef __WXMSW__
#include <windows.h>
#endif

#include "dialogs/ConfigDialogue.h"
#include "Version.h"
#include "examples/CASvsProgrammingLanguage.h"
#include "examples/diffEquations.h"
#include "examples/draw2d.h"
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


// We require Unicode support now.
// It is always on by default and builds without unicode support are strongly
// discouraged by the wxWidgets team
#if wxUSE_UNICODE == 0
#error "wxWidgets builds without Unicode support are strongly discouraged and not supported"
#endif

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
  // wxConfigBase *config = wxConfig::Get();
  // config->Flush();
  // delete config;
  if(Configuration::GetDebugmode())
    {
     if (CellPtrBase::GetLiveInstanceCount() != 0)
       wxLogDebug("CellPtr: %zu live instances leaked",
                  CellPtrBase::GetLiveInstanceCount());
     if (Observed::GetLiveInstanceCount() != 0)
       wxLogDebug("Cell:    %zu live instances leaked",
                  Observed::GetLiveInstanceCount());
     if (Observed::GetLiveControlBlockInstanceCount() != 0)
       wxLogDebug("ControlBlock: %zu live instances leaked",
                  Observed::GetLiveControlBlockInstanceCount());
     }
  return wxMaxima::GetExitCode();
}
wxCmdLineParser cmdLineParser;
static const wxCmdLineEntryDesc cmdLineDesc[] = {
  {wxCMD_LINE_SWITCH, "v", "version", "Output the version info.",
   wxCMD_LINE_VAL_NONE, 0},
  {wxCMD_LINE_SWITCH, "h", "help", "Show this help message.",
   wxCMD_LINE_VAL_NONE, wxCMD_LINE_OPTION_HELP},
  {wxCMD_LINE_OPTION, "o", "open", "Open a file.", wxCMD_LINE_VAL_STRING, 0},
  {wxCMD_LINE_SWITCH, "e", "eval", "Evaluate the file after opening it.",
   wxCMD_LINE_VAL_NONE, 0},
  {wxCMD_LINE_SWITCH, "", "single_process",
   "Open all worksheets from within the same process.", wxCMD_LINE_VAL_NONE, 0},
  {wxCMD_LINE_SWITCH, "", "single_thread",
   "Run all background tasks from within the main thread.", wxCMD_LINE_VAL_NONE, 0},
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
int WINAPI WinMain(_In_ HINSTANCE hI, _In_opt_ HINSTANCE hPrevI, _In_ LPSTR lpCmdLine,
                   _In_ int nCmdShow) {
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


// TODO: "C++ify" the code, don't use global variables.
// In wxMaxima.cpp they are used as 'extern' variables.
wxLogWindow * wxm_logwindow; // The wxWidgets log window, we use.
int windowcount = 0; // How many wxMaxima windows are open (in the current process)?
bool MyApp::OnInit() {
  // if DEBUG=1 show the logwindow at start, else hide it.
#if (DEBUG==1)
  wxm_logwindow = new wxLogWindow( NULL, wxS("wxMaxima log window"), true, false);
#else
  wxm_logwindow = new wxLogWindow( NULL, wxS("wxMaxima log window"), false, false);
#endif
  // Needed for making wxSocket work for multiple threads. We currently don't
  // use this feature. But it doesn't harm to be prepared
  wxSocketBase::Initialize();

  m_translations = std::unique_ptr<wxTranslations>(new wxTranslations());
  wxTranslations::Set(m_translations.get());
  {
    // If supported: Generate symbolic backtraces on crashes.
#if wxUSE_ON_FATAL_EXCEPTION && wxUSE_CRASHREPORT
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

    /* Maxima depending on the lisp it was compiled by and the lisp's version
       might only support Unicode if LANG tells it so.

       Do we reckon we improve something if we set maxima's language, as well?
    */
    if ((wxLocale::IsAvailable(lang)) && (lang != wxLANGUAGE_DEFAULT)) {
      // Set maxima's language, as well.
      wxString localeName = wxLocale().GetCanonicalName();
      if(localeName.IsEmpty())
        localeName = wxS("C");
      if ((!localeName.Upper().EndsWith(wxS("UTF-8"))) &&
          (!localeName.Upper().EndsWith(wxS("UTF8"))) &&
          (!localeName.Upper().EndsWith(wxS("UTF-16"))) &&
          (!localeName.Upper().EndsWith(wxS("UTF16"))) &&
          (!localeName.Upper().EndsWith(wxS("UTF-32"))) &&
          (!localeName.Upper().EndsWith(wxS("UTF32"))))
        {
        if (wxLocale().GetSystemEncoding() == wxFONTENCODING_UTF16)
          localeName += wxS(".UTF-16");
        else {
          if (wxLocale().GetSystemEncoding() == wxFONTENCODING_UTF32)
            localeName += wxS(".UTF-32");
          else {
            localeName += wxS(".UTF-8");
          }
        }
        Configuration::SetMaximaLang(localeName);
        wxSetEnv(wxS("LANG"), localeName);
      }
    }
    wxTranslations::Get()->AddCatalog(wxS("wxMaxima"));
    /* Add standard wxWidgets catalogs ("wxstd" and possible port-specific catalogs). */
    /* returns false, if no suitable catalog was found, in this case log that. */
    if (wxTranslations::Get()->AddStdCatalog() == false) {
      wxLogMessage("Translations: no standard wxWidgets catalogs were found");
    }
  }

  bool exitAfterEval = false;
  bool evalOnStartup = false;

  int cmdLineError = cmdLineParser.Parse();

  if (cmdLineParser.Found(wxS("single_process")))
    m_allWindowsInOneProcess = true;

  if (cmdLineParser.Found(wxS("single_thread")))
    Configuration::UseThreads(false);

  if (cmdLineError != 0)
    exit(1);

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

  if (cmdLineParser.Found(wxS("logtostderr"))) {
#ifdef __WXMSW__
    // Windows *GUI applications* do not have stderr (and stdout/stdin) assigned.
    // Assign them, as we want to output something there. (log messages on STDERR
    // when using the option --logtostderr).
    // A seperate "text window" will be opened, where the messages will be shown.
    FreeConsole(); // it does not seem to work without the FreeConsole() / AllocConsole() calls.
    // create a separate new console window
    AllocConsole();
    // attach the new console to this application's process
    AttachConsole(GetCurrentProcessId());
    // reopen the std I/O streams to redirect I/O to the new console
    if (!freopen("CON", "w", stdout)) wxLogMessage(_("Re-opening STDOUT failed."));
    if (!freopen("CON", "w", stderr)) wxLogMessage(_("Re-opening STDERR failed."));
    if (!freopen("CON", "r", stdin)) wxLogMessage(_("Re-opening STDIN failed."));
#endif
    m_logChain = std::unique_ptr<wxLogChain>(new wxLogChain(new wxLogStderr));
  }

  if (cmdLineParser.Found(wxS("debug")))
    Configuration::SetDebugmode();

  if (cmdLineParser.Found(wxS("pipe")))
    Maxima::SetPipeToStdErr(true);

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
#if defined(WXMAXIMA_GIT_SHORT_HASH)
    wxMessageOutput::Get()->Printf("wxMaxima %s (Git version: %s)\n", WXMAXIMA_VERSION, WXMAXIMA_GIT_SHORT_HASH);
#else
    wxMessageOutput::Get()->Printf("wxMaxima %s\n", WXMAXIMA_VERSION);
#endif
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

  return true;
}

int MyApp::OnExit() {
  for(auto i:m_wxMaximaProcesses)
    i->Detach();
  return wxMaxima::GetExitCode();
}

int MyApp::OnRun() {
  wxApp::OnRun();
  return 0;
}

void MyApp::NewWindow(const wxString &file, bool evalOnStartup,
                      bool exitAfterEval, unsigned char *wxmData,
                      std::size_t wxmLen) {

  wxString title = _("wxMaxima");
  if (file.Length() > 0)
    title = file;

  size_t numberOfWindows = wxMaximaFrame::CountWindows();

  if (numberOfWindows > 1)
    title = wxString::Format(_("wxMaxima %ld"), static_cast<long>(numberOfWindows));

  wxString initialContents;
  if (wxmData) {
    // Unzip the .wxm file
    wxMemoryInputStream istream(wxmData, wxmLen);
    wxTextInputStream textIn(istream, "\t", wxConvAuto(wxFONTENCODING_UTF8));
    wxString line;
    wxString block;
    while (!istream.Eof()) {
      line = textIn.ReadLine();
      if ((line.StartsWith("/*")) || (line.EndsWith("*/"))) {
        initialContents += _(block);
        initialContents += line + "\n";
        block.Clear();
      } else
        block += line + "\n";
    }
    initialContents += _(block);
  }
  wxMaxima *frame = new wxMaxima(NULL, wxID_ANY, title, file, initialContents);
  frame->EvalOnStartup(evalOnStartup);
  frame->ExitAfterEval(exitAfterEval);
  frame->Show(true);
  frame->ShowTip(false);
}

void MyApp::OnFileMenu(wxCommandEvent &ev) {
  if(ev.GetId() == EventIDs::menu_help_numberformats)
    {
      NewWindow(wxEmptyString, false, false, NUMBERFORMATS_WXM,
                NUMBERFORMATS_WXM_SIZE);
    }
  else if(ev.GetId() == EventIDs::menu_help_2d)
    {
      NewWindow(wxEmptyString, false, false, DRAW2D_WXM,
                DRAW2D_WXM_SIZE);
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
      args.push_back(wxStandardPaths::Get().GetExecutablePath().mb_str());

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
      if (Configuration::GetDebugmode())
        args.push_back("--debug");
      if (Maxima::GetPipeToStdErr())
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
        {
          wxCharBuffer buf = i.utf8_str();
          args_c_strings.push_back(buf);
        }

      // Additionally wxExecute expects these C strings in a C array.
      std::vector<char *> argslist;
      for(auto &i : args_c_strings)
        argslist.push_back(static_cast<char *>(i.data()));
      // Add an "end of arguments list" marker to the list of arguments
      argslist.push_back(NULL);
      wxProcess *prcss = new wxProcess;
      // Let's generate an unique pointer to that one so C++ automatically destroys it
      // once it is no more needed.
      wxExecute(argslist.data(),
                wxEXEC_HIDE_CONSOLE | wxEXEC_ASYNC | wxEXEC_MAKE_GROUP_LEADER,
                prcss);
      m_wxMaximaProcesses.push_back(prcss);
      // Tell wxWidgets that we don't want any signals from this process
      prcss->Detach();
      wxLogMessage(_("Starting a new wxMaxima process for a new window"));
    }
  }
  else if(ev.GetId() == wxID_PREFERENCES) {
    Configuration config;
    ConfigDialogue *configW = new ConfigDialogue(NULL);
    configW->Centre(wxBOTH);
    if (configW->ShowModal() == wxID_OK)
      configW->WriteSettings();

    configW->Destroy();
    wxConfig::Get()->Flush();
  }
}


void MyApp::MacNewFile() {
  const wxWindow *frame = GetTopWindow();
  if (frame == NULL)
    NewWindow();
}

void MyApp::MacOpenFile(const wxString &file) { NewWindow(file); }

#if wxUSE_ON_FATAL_EXCEPTION && wxUSE_CRASHREPORT
void MyApp::OnFatalException()
{
    GenerateDebugReport(wxDebugReport::Context_Exception);
}

// Adapted from wxWidget's report sample
void MyApp::GenerateDebugReport(wxDebugReport::Context ctx)
{
    {
      wxDebugReportCompress *report = new wxDebugReportCompress;

      // add all standard files: currently this means just a minidump and an
      // XML file with system info and stack trace
      report->AddAll(ctx);

      // you can also call report->AddFile(...) with your own log files, files
      // created using wxRegKey::Export() and so on, here we just add a test
      // file containing the date of the crash
      wxFileName fn(report->GetDirectory(), "timestamp.my");
      wxFFile file(fn.GetFullPath(), "w");
      if ( file.IsOpened() )
        {
          wxDateTime dt = wxDateTime::Now();
          file.Write(dt.FormatISODate() + ' ' + dt.FormatISOTime());
          file.Close();
        }

      // No translation after a crash, as the internationalization stuff might have been
      // broken by the crash.
      report->AddText("README.txt",
                      "On crashes, if the compiler has provided enough information\n"
                      "and wxWidgets knows how to interpret it we might get a stack\n"
                      "backtrace, that contains the exact line of program code the\n"
                      "crash happened in, what parameters the function that crashed\n"
                      "was called with and, if that does still not explain the crash\n"
                      "how we ended up in that situation.\n"
                      "With all that information fixing a crash usually only takes\n"
                      "minutes. The rest of the crash report\n"
                      "might be there, if needed, but usually doesn't help too much.\n\n"
                      "be aware, though, that the stack backtrace contains the\n"
                      "parameters of the chain of function calls that ended up in\n"
                      "a crash, and therefore in rare cases might contain personal\n"
                      "information.",
                      "What is this report all about?");
      report->AddFile(fn.GetFullName(), "timestamp of this report");

      // can also add an existing file directly, it will be copied
      // automatically
#ifdef __WXMSW__
      wxString windir;
      if ( !wxGetEnv("WINDIR", &windir) )
        windir = "C:\\Windows";
      fn.AssignDir(windir);
      fn.AppendDir("system32");
      fn.AppendDir("drivers");
      fn.AppendDir("etc");
#else // !__WXMSW__
      fn.AssignDir("/etc");
#endif // __WXMSW__/!__WXMSW__
      fn.SetFullName("hosts");

      if ( fn.FileExists() )
        report->AddFile(fn.GetFullPath(), "Local hosts file");

      // calling Show() is not mandatory, but is more polite
      if ( wxDebugReportPreviewStd().Show(*report) )
        {
          if ( report->Process() )
            {
              wxLogMessage("Report generated in \"%s\".",
                           report->GetCompressedFileName());
              report->Reset();
            }
        }
      //else: user cancelled the report

      delete report;
    }
}
#endif

#if defined __WXOSX__
  bool MyApp::m_allWindowsInOneProcess = true;
#else
  bool MyApp::m_allWindowsInOneProcess = false;
#endif
std::vector<wxProcess *> MyApp::m_wxMaximaProcesses;
