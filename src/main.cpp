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
#include "BuildConfig.h"
#include "Maxima.h"
#include "Dirstructure.h"
#include "dialogs/DiffFrame.h"
#include "wxMathml.h"
#include <iostream>
#include <wx/cmdline.h>
#include <wx/config.h>
#include <wx/fileconf.h>
#include <wx/fs_zip.h>
#include <wx/image.h>
#include <wx/intl.h>
#include <wx/msgout.h>
#include <wx/translation.h>
#ifdef USE_QA
#include <wx/debugrpt.h>
#include "dialogs/LoggingMessageDialog.h"
#ifndef __WXMSW__
#include <unistd.h>   // write(), STDERR_FILENO (async-signal-safe output in crash handler)
#endif
#endif
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
#include <io.h>      // _open_osfhandle()
#include <fcntl.h>   // _O_TEXT
#include <cstdio>    // freopen(), _fdopen(), stdout/stderr/stdin
#include <cstdint>   // intptr_t
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
#if wxCHECK_VERSION(3, 2, 0)
#include "wxMaximaArtProvider.h"
#endif

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
  // Only enter the main event loop if OnInit() actually succeeded. Otherwise a
  // startup that bails out (e.g. "wxmaxima --diff" with the wrong number of
  // files) would open no window yet keep running forever, because we disable
  // wxWidgets' exit-on-last-frame and rely on our own window bookkeeping.
  if (wxTheApp->CallOnInit())
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
  Configuration::g_stats.Report();
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
  {wxCMD_LINE_SWITCH, "d", "diff", "Compare 2 or 3 files side-by-side using the Diff viewer.",
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
   "Allows specifying a file to store the configuration in.",
   wxCMD_LINE_VAL_STRING, 0},
  {wxCMD_LINE_OPTION, "u", "use-version", "Use Maxima version <str>.",
   wxCMD_LINE_VAL_STRING, 0},
  {wxCMD_LINE_OPTION, "l", "lisp",
   "Use a Maxima compiled with lisp compiler <str>.", wxCMD_LINE_VAL_STRING,
   0},
  {wxCMD_LINE_OPTION, "X", "extra-args",
   "Allows specifying extra Maxima arguments.", wxCMD_LINE_VAL_STRING, 0},
  {wxCMD_LINE_OPTION, "m", "maxima",
   "Allows specifying the location of the Maxima binary.",
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
#ifdef __WXGTK__
#if !wxCHECK_VERSION(3, 3, 0)
  // Workaround for https://github.com/wxWidgets/wxWidgets/issues/18815
  // and general KDE Plasma Global Menu issues where the menu disappears
  // when clicked or when the layout is updated.
  // This is only needed for wxWidgets <= 3.2.x.
  bool isAffected = false;
  wxString desktop;
  if (wxGetEnv(wxS("XDG_CURRENT_DESKTOP"), &desktop)) {
    desktop.MakeLower();
    if (desktop.Contains(wxS("kde")) || desktop.Contains(wxS("unity")) ||
        desktop.Contains(wxS("ubuntu"))) {
      isAffected = true;
    }
  }
  wxString modules;
  if (!isAffected && wxGetEnv(wxS("GTK_MODULES"), &modules)) {
    if (modules.Contains(wxS("appmenu-gtk-module"))) {
      isAffected = true;
    }
  }
  if (isAffected) {
    wxSetEnv(wxS("UBUNTU_MENUPROXY"), wxS("0"));
  }
#endif
#endif
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


#ifdef __WXMSW__
// wxMaxima is built as a GUI-subsystem binary (add_executable(wxmaxima WIN32 ...)),
// so Windows does not wire up stdin/stdout/stderr for it. Anything that would
// normally go to a console -- the --version / --help text, and the
// wxMessageOutput fallback used for early diagnostics -- therefore ends up in a
// *modal* wxMessageBox. That hangs any non-interactive run: e.g. `wxmaxima
// --version` launched from a pipe (as ctest does) waits forever for a click that
// never comes.
//
// Bind the CRT standard streams to whatever the parent actually gave us: the
// inherited handle when stdout/stderr were redirected to a pipe or file, or the
// launching terminal's console (via AttachConsole) when we were started from
// one. When wxMaxima is started from the GUI (double-click) there is neither, so
// every binding below is a no-op and behaviour is unchanged.
//
// This is Windows-only by construction; macOS and Linux never compile it.
static void BindStdStreamToParent(DWORD stdHandleId, FILE *stream,
                                  const char *mode, bool &consoleAttached) {
  HANDLE handle = GetStdHandle(stdHandleId);
  if ((handle == nullptr) || (handle == INVALID_HANDLE_VALUE)) {
    // No inherited handle -- hook up to the launching console, at most once.
    if (!consoleAttached)
      consoleAttached = (AttachConsole(ATTACH_PARENT_PROCESS) != 0);
    handle = GetStdHandle(stdHandleId);
  }
  if ((handle == nullptr) || (handle == INVALID_HANDLE_VALUE))
    return;
  int fd = _open_osfhandle(reinterpret_cast<intptr_t>(handle), _O_TEXT);
  if (fd == -1)
    return;
  FILE *opened = _fdopen(fd, mode);
  if (opened == nullptr)
    return;
  *stream = *opened;  // rebind the standard stream onto the parent's handle
  setvbuf(stream, nullptr, _IONBF, 0);
}

static void RedirectStdioToParent() {
  bool consoleAttached = false;
  BindStdStreamToParent(STD_OUTPUT_HANDLE, stdout, "w", consoleAttached);
  BindStdStreamToParent(STD_ERROR_HANDLE, stderr, "w", consoleAttached);
  BindStdStreamToParent(STD_INPUT_HANDLE, stdin, "r", consoleAttached);
}

// True once we have a usable stderr handle (inherited pipe/file or an attached
// console), i.e. once RedirectStdioToParent() found something to bind to.
static bool HaveStdErrHandle() {
  HANDLE handle = GetStdHandle(STD_ERROR_HANDLE);
  return (handle != nullptr) && (handle != INVALID_HANDLE_VALUE);
}

// Old SDKs may lack these; the flag values are fixed by the Console API.
#ifndef ENABLE_QUICK_EDIT_MODE
#define ENABLE_QUICK_EDIT_MODE 0x0040
#endif
#ifndef ENABLE_EXTENDED_FLAGS
#define ENABLE_EXTENDED_FLAGS 0x0080
#endif

void MyApp::DisableConsoleQuickEdit() {
  // CONIN$ addresses the console we are attached to even if our own stdin was
  // redirected to a pipe; if there is no console at all, opening it fails and
  // there is nothing whose QuickEdit mode could block us.
  HANDLE conIn = CreateFileW(L"CONIN$", GENERIC_READ | GENERIC_WRITE,
                             FILE_SHARE_READ | FILE_SHARE_WRITE, nullptr,
                             OPEN_EXISTING, 0, nullptr);
  if (conIn == INVALID_HANDLE_VALUE)
    return;
  DWORD mode = 0;
  if (GetConsoleMode(conIn, &mode) && (mode & ENABLE_QUICK_EDIT_MODE)) {
    // ENABLE_EXTENDED_FLAGS must be set for the QuickEdit bit to be honored.
    if (SetConsoleMode(conIn,
                       (mode | ENABLE_EXTENDED_FLAGS) & ~ENABLE_QUICK_EDIT_MODE)) {
      m_consoleModeToRestore = mode;
      m_consoleModeChanged = true;
      wxLogMessage(_("Disabled the console's QuickEdit mode for this batch run "
                     "so a click into the console cannot suspend wxMaxima."));
    }
  }
  CloseHandle(conIn);
}

void MyApp::RestoreConsoleMode() {
  if (!m_consoleModeChanged)
    return;
  m_consoleModeChanged = false;
  HANDLE conIn = CreateFileW(L"CONIN$", GENERIC_READ | GENERIC_WRITE,
                             FILE_SHARE_READ | FILE_SHARE_WRITE, nullptr,
                             OPEN_EXISTING, 0, nullptr);
  if (conIn == INVALID_HANDLE_VALUE)
    return;
  SetConsoleMode(conIn, m_consoleModeToRestore);
  CloseHandle(conIn);
}

#endif

bool MyApp::OnInit() {
#ifdef __WXMSW__
  // Suppress the OS hard-error / "this application could not be started" message
  // boxes for us AND for the child processes we spawn (Maxima, gnuplot). Child
  // processes inherit this error mode because wxExecute() does not pass
  // CREATE_DEFAULT_ERROR_MODE. This matters on a head-less --batch run: when we
  // exit we kill Maxima/gnuplot (see KillMaxima / KillAndDetachProcess), and a
  // fast exit (e.g. an empty worksheet) can land that kill while the child is
  // still starting up. Windows then pops a modal "cannot start" box for it; on a
  // CI runner nobody dismisses it, so the killed child blocks on the box instead
  // of terminating and keeps our inherited stdout/stderr pipe open, hanging the
  // ctest that is waiting for EOF. Suppressing the box lets the kill terminate
  // the child cleanly.
  SetErrorMode(SEM_FAILCRITICALERRORS | SEM_NOGPFAULTERRORBOX |
               SEM_NOOPENFILEERRORBOX);
  // Must run before any console output or wxMessageOutput use (see above).
  RedirectStdioToParent();
  // Belt and suspenders for the same head-less-hang: mark our own stdout/stderr
  // (the ctest/parent pipe RedirectStdioToParent() just bound to) NON-inheritable
  // so no child - and no WerFault.exe spawned for a dying child - can ever hold
  // that pipe open past our exit. We still write to the streams ourselves; the
  // flag only controls what children inherit. Neither Maxima (talks over a TCP
  // socket) nor the gnuplot terminal probe (uses its own Redirect() pipes) needs
  // these handles.
  {
    HANDLE outHandle = GetStdHandle(STD_OUTPUT_HANDLE);
    HANDLE errHandle = GetStdHandle(STD_ERROR_HANDLE);
    if ((outHandle != nullptr) && (outHandle != INVALID_HANDLE_VALUE))
      SetHandleInformation(outHandle, HANDLE_FLAG_INHERIT, 0);
    if ((errHandle != nullptr) && (errHandle != INVALID_HANDLE_VALUE))
      SetHandleInformation(errHandle, HANDLE_FLAG_INHERIT, 0);
  }
  // For a GUI-subsystem binary the default wxMessageOutput is a *modal* message
  // box. That is what hangs every head-less wxmaxima launch (--version, --help /
  // wxCmdLineParser::Usage(), and early diagnostics all go through it). Now that
  // stdio is bound to the parent (pipe/console), route those to stdout instead,
  // so they are printed and the process can exit. Windows-only by construction:
  // macOS/Linux never compile this, so the macOS message-output behaviour that
  // is sensitive around menu setup is left exactly as it was.
  wxMessageOutput::Set(new wxMessageOutputStderr(stdout));
#endif
  // if DEBUG=1 show the logwindow at start, else hide it.
  // in wxMaxima.cpp we later read a configuration variable (LogWindow) and show/hide it, according to the previous state (issue #2033).
#if (DEBUG==1)
  m_logWindow = new wxLogWindow(NULL, wxS("wxMaxima log window"), true, false);
#else
  m_logWindow = new wxLogWindow(NULL, wxS("wxMaxima log window"), false, false);
#endif
  // Needed for making wxSocket work for multiple threads. We currently don't
  // use this feature. But it doesn't harm to be prepared
  wxSocketBase::Initialize();

#if wxCHECK_VERSION(3, 2, 0)
  wxArtProvider::Push(new wxMaximaArtProvider);
#endif
  m_translations = std::unique_ptr<wxTranslations>(new wxTranslations());
  wxTranslations::Set(m_translations.get());
  {
    // If supported: Generate symbolic backtraces on crashes.
#if wxUSE_ON_FATAL_EXCEPTION && wxUSE_DEBUGREPORT
    wxHandleFatalExceptions(true);
#endif
    // Make sure child Maxima processes are killed if we are terminated by a
    // signal instead of shutting down cleanly (no-op on Windows).
    MaximaProcessManager::SetupTerminationHandlers();
#ifdef __WXMSW__
    // After an update the .wxmx/.wxm/.mac association often still points at the
    // previous install path; re-point it at the wxMaxima that is running now.
    wxMaxima::RepairFileAssociations();
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

  wxString programName = wxFileName(argv[0]).GetName();
  if (programName.IsSameAs(wxS("wxmxdiff"), false)) {
    bool hasDiff = false;
    for (int i = 1; i < argc; ++i) {
      if (argv[i] == wxS("-d") || argv[i] == wxS("--diff")) {
        hasDiff = true;
        break;
      }
    }
    if (!hasDiff) {
      wxString cmdLine;
      for (int i = 0; i < argc; ++i) {
        wxString arg = argv[i];
        if (arg.Contains(wxS(" ")) || arg.Contains(wxS("\t"))) {
          cmdLine += wxS("\"") + arg + wxS("\" ");
        } else {
          cmdLine += arg + wxS(" ");
        }
        if (i == 0) {
          cmdLine += wxS("-d ");
        }
      }
      cmdLineParser.SetCmdLine(cmdLine);
    }
  }


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
    // If RedirectStdioToParent() already bound them to the parent's console or to
    // a redirected pipe/file (e.g. when run from a terminal or under ctest), keep
    // those -- creating a separate console here would detach us from the pipe the
    // caller is capturing. Only when we have no usable stderr (interactive
    // double-click launch) do we open a separate text window for the log.
    if (!HaveStdErrHandle()) {
      FreeConsole(); // it does not seem to work without the FreeConsole() / AllocConsole() calls.
      // create a separate new console window
      AllocConsole();
      // attach the new console to this application's process
      AttachConsole(GetCurrentProcessId());
      // reopen the std I/O streams to redirect I/O to the new console
      if (!freopen("CON", "w", stdout)) wxLogMessage(_("Re-opening STDOUT failed."));
      if (!freopen("CON", "w", stderr)) wxLogMessage(_("Re-opening STDERR failed."));
      if (!freopen("CON", "r", stdin)) wxLogMessage(_("Re-opening STDIN failed."));
    }
#endif
    m_logChain = std::unique_ptr<wxLogChain>(new wxLogChain(new wxLogStderr));
  }

  if (cmdLineParser.Found(wxS("debug")))
    Configuration::SetDebugmode();

  if (cmdLineParser.Found(wxS("pipe")))
    Maxima::SetPipeToStdErr(true);

  if (cmdLineParser.Found(wxS("exit-on-error"))) {
    wxMaxima::ExitOnError();
    LoggingMessageDialog::SetNonInteractive();
  }

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

  wxInitAllImageHandlers();

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

  Bind(wxEVT_MENU, &MyApp::OnFileMenu, this);

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

  menubar->Bind(wxEVT_COMMAND_MENU_SELECTED, &MyApp::OnFileMenu, this);
  Bind(wxEVT_COMMAND_MENU_SELECTED, &MyApp::OnFileMenu, this);
  wxApp::SetExitOnFrameDelete(false);
#endif

  if (cmdLineParser.Found(wxS("v"))) {
    if (WxMaximaGitShortHash())
      wxMessageOutput::Get()->Printf("wxMaxima %s (Git version: %s)\n", WXMAXIMA_VERSION, WxMaximaGitShortHash());
    else
      wxMessageOutput::Get()->Printf("wxMaxima %s\n", WXMAXIMA_VERSION);
    exit(0);
  }

  if (cmdLineParser.Found(wxS("b"))) {
    evalOnStartup = true;
    exitAfterEval = true;
    LoggingMessageDialog::SetNonInteractive();
#ifdef __WXMSW__
    DisableConsoleQuickEdit();
#endif
  }

  m_configuration = std::make_unique<Configuration>();

  if (cmdLineParser.Found(wxS("e")))
    evalOnStartup = true;

  bool windowOpened = false;

  if (cmdLineParser.Found(wxS("d"))) {
    wxArrayString diffFiles;
    for (unsigned int i = 0; i < cmdLineParser.GetParamCount(); i++) {
      diffFiles.push_back(cmdLineParser.GetParam(i));
    }
    if (diffFiles.size() >= 2 && diffFiles.size() <= 3) {
      DiffFrame *diffFrame = new DiffFrame(nullptr, diffFiles, this->m_configuration.get());
      diffFrame->Show();
      windowOpened = true;
    } else {
      // A modal box (rather than wxLogError) so it is shown reliably even
      // though we are about to return false and never start the event loop.
      wxMessageBox(_("Comparing worksheets (the --diff option or the wxmxdiff "
                     "command) needs 2 or 3 files to compare."),
                   _("Error"), wxOK | wxICON_ERROR);
      return false;
    }
  }

  if (cmdLineParser.Found(wxS("o"), &file)) {
    wxString realFile = file;
    wxString uuid;
    if (file.Contains(wxS("#"))) {
      realFile = file.BeforeFirst(wxS('#'));
      uuid = file.AfterFirst(wxS('#'));
    }
    wxFileName FileName = realFile;
    FileName.MakeAbsolute();
    wxString canonicalFilename = FileName.GetFullPath();
    if (!uuid.IsEmpty())
      canonicalFilename += wxS("#") + uuid;
    NewWindow(canonicalFilename, evalOnStartup, exitAfterEval);
    windowOpened = true;
  }

  if (cmdLineParser.GetParamCount() > 0 && !cmdLineParser.Found(wxS("d"))) {
    for (unsigned int i = 0; i < cmdLineParser.GetParamCount(); i++) {
      wxString fileParam = cmdLineParser.GetParam(i);
      wxString realFile = fileParam;
      wxString uuid;
      if (fileParam.Contains(wxS("#"))) {
        realFile = fileParam.BeforeFirst(wxS('#'));
        uuid = fileParam.AfterFirst(wxS('#'));
      }
      wxFileName FileName = realFile;
      FileName.MakeAbsolute();

      wxString CanonicalFilename = FileName.GetFullPath();
      if (!uuid.IsEmpty())
        CanonicalFilename += wxS("#") + uuid;
      NewWindow(CanonicalFilename, evalOnStartup, exitAfterEval);
    }
    windowOpened = true;
  }

  if (!windowOpened)
    NewWindow();

  return true;
}

int MyApp::OnExit() {
  Configuration::g_stats.Report();
  for(auto i:m_wxMaximaProcesses)
    i->Detach();
#ifdef __WXMSW__
  RestoreConsoleMode();
#endif
  return wxMaxima::GetExitCode();
}

int MyApp::OnRun() {
  wxApp::OnRun();
  return 0;
}

void MyApp::NewWindow(const wxString &file, bool evalOnStartup,
                      bool exitAfterEval, unsigned char *wxmData,
                      std::size_t wxmLen) {

  wxString realFile = file;
  if (file.Contains(wxS("#"))) {
    realFile = file.BeforeFirst(wxS('#'));
  }

  wxString title = _("wxMaxima");
  if (realFile.Length() > 0)
    title = realFile;

  // CountWindows() counts the wxMaxima windows that already exist; +1 for the
  // one we are about to create, which is the number we put into its title.
  size_t numberOfWindows = wxMaximaFrame::CountWindows() + 1;

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
        initialContents += block;
        initialContents += line + "\n";
        block.Clear();
      } else
        block += line + "\n";
    }
    initialContents += block;
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

#ifdef __WXMAC__
void MyApp::MacNewFile() {
  const wxWindow *frame = GetTopWindow();
  if (frame == NULL)
    NewWindow();
}

void MyApp::MacOpenFile(const wxString &file) { NewWindow(file); }
#endif // __WXMAC__

void MyApp::OnAssertFailure(const wxChar *file, int line, const wxChar *func,
                            const wxChar *cond, const wxChar *msg) {
  if (!LoggingMessageDialog::IsNonInteractive()) {
    // Interactive use: keep wxWidgets' default behaviour (the assert dialog).
    wxApp::OnAssertFailure(file, line, func, cond, msg);
    return;
  }

  // Non-interactive (batch/test) mode: the default modal assert dialog would
  // pop up on a headless CI runner with nobody to dismiss it and wedge the
  // process until the job's wall-clock limit. Print the assertion so the CI
  // log shows exactly which one fired, then abort - turning an undiagnosable
  // hang into a fast test failure with a usable backtrace/core.
  wxString report =
    wxString::Format(wxS("wxMaxima assertion failed: %s:%d"),
                     (file && *file) ? file : wxS("?"), line);
  if (func && *func)
    report += wxString::Format(wxS(" in %s"), func);
  if (cond && *cond)
    report += wxString::Format(wxS(": \"%s\""), cond);
  if (msg && *msg)
    report += wxString::Format(wxS(": %s"), msg);
  report += wxS("\n");

  fputs(report.utf8_str().data(), stderr);
  fflush(stderr);
  abort();
}

#ifdef USE_QA
#if wxUSE_ON_FATAL_EXCEPTION && wxUSE_DEBUGREPORT
void MyApp::OnFatalException()
{
    // Kill child Maxima processes first (async-signal-safe), so a crash can't
    // leave a busy Maxima orphaned while we generate the debug report.
    MaximaProcessManager::KillAllChildMaximas();
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

      // In non-interactive (batch/test) mode, skip the modal dialog so that
      // ctest can kill the process normally after a timeout. Just compress and
      // save the report so developers still get the backtrace on stderr.
      if (LoggingMessageDialog::IsNonInteractive()) {
        if (report->Process()) {
#ifndef __WXMSW__
          // write() is async-signal-safe; the whole crash handler isn't, but
          // this is at least safer than printf for stderr output from a handler.
          const std::string msg =
            std::string("wxMaxima crash report saved to: ") +
            report->GetCompressedFileName().ToStdString() + "\n";
          write(STDERR_FILENO, msg.c_str(), msg.size());
#else
          fputs(("wxMaxima crash report saved to: " +
                 report->GetCompressedFileName().ToStdString() + "\n").c_str(),
                stderr);
#endif
          report->Reset();
        }
      } else {
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
      }

      delete report;
    }
}
#endif
#endif

#if defined __WXOSX__
  bool MyApp::m_allWindowsInOneProcess = true;
#else
  bool MyApp::m_allWindowsInOneProcess = false;
#endif
std::vector<wxProcess *> MyApp::m_wxMaximaProcesses;
