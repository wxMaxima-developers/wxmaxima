// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2013 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file declares the class wxMaxima that contains most of the program's logic.

  The worksheet is defined in the class MathCtrl instead and
  everything surrounding it in wxMaximaFrame.
*/


#ifndef WXMAXIMA_H
#define WXMAXIMA_H

#include "BuildConfig.h"
#include <vector>
#include "wxMaximaFrame.h"
#include "WXMXformat.h"
#include "Configuration.h"
#include "MathParser.h"
#include "MaximaIPC.h"
#include "MaximaCommandMenus.h"
#include "MaximaResponseReader.h"
#include "MaximaProcessManager.h"
#include "Dirstructure.h"
#include <wx/socket.h>
#include <wx/config.h>
#include <wx/process.h>
#include <wx/regex.h>
#include <wx/dnd.h>
#include <wx/txtstrm.h>
#include <wx/sckstrm.h>
#include <wx/buffer.h>
#include <wx/power.h>
#ifdef USE_QA
#include <wx/debugrpt.h>
#endif
#include <memory>
#ifdef __WXMSW__
#include <windows.h>
#endif
#include <unordered_map>

//! How many milliseconds should we wait between polling for stdout+cpu power?
#define MAXIMAPOLLMSECS 2000

class Maxima; // The Maxima process interface

/* The top-level window and the main application logic

 */
class wxMaxima : public wxMaximaFrame
{
public:
  wxMaxima(wxWindow *parent, int id, const wxString &title,
           const wxString &filename = wxEmptyString,
           const wxString &initialWorksheetContents = wxEmptyString,
           const wxPoint pos = wxDefaultPosition, const wxSize size = wxDefaultSize);

  virtual ~wxMaxima();
  wxString EscapeFilenameForShell(wxString name);
  //! Exit if we encounter an error
  static void ExitOnError(){m_exitOnError = true;}
  //! Do we exit if we encounter an error?
  static bool GetExitOnError(){return m_exitOnError;}
  /*! Is THIS worksheet still set to exit on a Maxima error?

    m_exitOnError records the process-wide --exit-on-error command line switch
    and must never be cleared, or one window clearing it would disable
    exit-on-error for every other window of a --single_process run (the
    multithreadtest CI hang: the first worksheet to finish its evaluation queue
    reset the shared flag, so a later error in a second worksheet dropped it
    into an interactive session that never exits). Whether exit-on-error is
    still armed once a worksheet has drained its own queue is therefore tracked
    per worksheet, here. */
  bool ExitOnErrorArmed() const { return m_exitOnError && m_exitOnErrorArmed; }

#ifdef __WXMSW__
  /*! Re-point the .wxmx/.wxm/.mac file association at the running wxMaxima.

    After an update the association often still references the previous install
    path (the ProgID's shell\open\command is stale), so Windows "forgets" which
    program opens .wxmx files. Called once at startup; writes per-user
    (HKCU\Software\Classes, no admin) and only when actually stale, and never
    hijacks an extension the user deliberately gave to another program.
  */
  static void RepairFileAssociations();
#endif
  /*! Allow maxima to click buttons in wxMaxima

    Disabled by default for security reasons
  */
  static void EnableIPC(){ MaximaIPC::EnableIPC(); }
  //! Do we allow maxima to click buttons in wxMaxima?
  static bool GetEnableIPC(){ return MaximaIPC::GetEnableIPC(); }
  //! Additional maxima arguments from the command line
  static void ExtraMaximaArgs(const wxString &args){m_extraMaximaArgs = args;}
  //! Which additional maxima arguments did we get from the command line?
  static wxString ExtraMaximaArgs(){return m_extraMaximaArgs;}

  //! An enum of individual IDs for all timers this class handles
  enum TimerIDs
  {
    //! The keyboard was inactive long enough that we can attempt an auto-save.
    KEYBOARD_INACTIVITY_TIMER_ID,
    //! The time between two auto-saves has elapsed.
    AUTO_SAVE_TIMER_ID,
    //! We look if we got new data from maxima's stdout.
    MAXIMA_STDOUT_POLL_ID
  };

#ifdef wxHAS_POWER_EVENTS
  void OnPowerEvent(wxPowerEvent &event);
#endif

  //! Is triggered when a timer this class is responsible for requires
  void OnTimerEvent(wxTimerEvent &event);

  //! A timer that polls for output from the maxima process.
  wxTimer m_maximaStdoutPollTimer;

  void ShowTip(bool force);
  //! Do we want to evaluate the document on startup?
  void EvalOnStartup(bool eval)
    {
      m_evalOnStartup = eval;
    }

  //! Do we want to exit the program after evaluation?
  void ExitAfterEval(bool exitaftereval);
  /*! Is this an interactive session (as opposed to a batch run that will exit)?

    Returns false while we are batch-processing a file and will exit afterwards,
    true once we are (or have become) interactive. Use this to gate interactive-
    only work -- help-anchor compilation, autocomplete symbol loading, the recent-
    documents list -- so it is skipped in batch runs. AbortOnError() keeps
    m_exitAfterEval accurate: a Maxima error only turns a batch session
    interactive when we are not going to --exit-on-error.
  */
  bool IsInteractive() const { return !m_exitAfterEval; }

  void StripLispComments(wxString &s);

  //! Launches the help browser on the uri passed as an argument.
  void LaunchHelpBrowser(wxString uri);

  void SendMaxima(wxString s, bool addToHistory = false, bool background = false);

  //! Open a file
  bool OpenFile(const wxString &file, const wxString &command ={});

  //! Does this document need saving?
  bool DocumentSaved()
    { return m_fileSaved; }

  //! Load an image from a file into the worksheet.
  void LoadImage(const wxString &file)
    { GetWorksheet()->OpenHCaret(file, GC_TYPE_IMAGE); }

  //! Query the value of a new maxima variable
  bool QueryVariableValue();

  //! A version number that can be compared using "<" and ">"
  class VersionNumber
  {
  public:
    explicit VersionNumber(const wxString &version);
    long Major() const {return m_major;}
    long Minor() const {return m_minor;}
    long Patchlevel() const {return m_patchlevel;}
    auto operator<=>(const VersionNumber&) const = default;
  private:
    long m_major = -1;
    long m_minor = -1;
    long m_patchlevel = -1;
  };

  //! Use this maxima command (from a command line option) instead of the configured path
  static void Set_Maxima_Commandline_Filename(const wxString &filename)
    {maxima_command_line_filename = filename;}
  //! The maxima command, if we use the --maxima=<str> command. If empty we use the configured path, not a command line option --maxima=<str>.
  static const wxString Get_Maxima_Commandline_Filename() {return maxima_command_line_filename;}

  static int GetExitCode(){return m_exitCode;}

private:
  //! If we use the command line option --maxima=<str>, this variable is not-empty and contains its name
  static wxString maxima_command_line_filename;
  //! True if maxima has sent us the correct Auth string
  bool m_maximaAuthenticated = false;
  //! True if maxima has failed to authenticate and we therefore distrust its data.
  bool m_discardAllData = false;
  //! The string Maxima needs to know in order to prove to be the correct process
  wxString m_maximaAuthString;
  //! The object that allows maxima to send us GUI events for testing purposes
  MaximaIPC m_ipc{this};
  //! True if we want to exit if we encounter an error (process-wide command
  //! line switch; never cleared once set -- see ExitOnErrorArmed()).
  static bool m_exitOnError;
  //! Per-worksheet: still honour --exit-on-error? Cleared once this worksheet
  //! has drained its own evaluation queue, so a clean batch run can exit
  //! normally without a later stray error turning into an exit-on-error.
  bool m_exitOnErrorArmed = true;
  //! Extra arguments wxMaxima's command line told us to pass to maxima
  static wxString m_extraMaximaArgs;
  //! The variable names to query for the variables pane and for internal reasons
  std::vector<wxString> m_varNamesToQuery;

  //! Is true if opening the file from the command line failed before updating the statusbar.
  bool m_openInitialFileError = false;
  //! Escape strings into a format lisp accepts
  wxString EscapeForLisp(wxString str);
  //! The number of Jiffies Maxima had used the last time we asked
  long long m_maximaJiffies_old = 0;
  //! The number of Jiffies the CPU had made the last time
  long long m_cpuTotalJiffies_old = 0;
  //! Do we need to update the menus + toolbars?
  //! All configuration commands we still have to send to maxima
  wxString m_configCommands;
  //! A RegEx that matches gnuplot errors.
  static wxRegEx m_gnuplotErrorRegex;
  //! Clear the evaluation queue and return true if "Abort on Error" is set.
  bool AbortOnError();
  //! This string allows us to detect when the string we search for has changed.
  wxString m_oldFindString;
  //! This string allows us to detect when the string we search for has changed.
  int m_oldFindFlags = 0;
  //! On opening a new file we only need a new maxima process if the old one ever evaluated cells.
  bool m_hasEvaluatedCells = false;
  //! The number of output cells the current command has produced so far.
  long m_outputCellsFromCurrentCommand = 0;
  //! The maximum number of lines per command we will display
  long m_maxOutputCellsPerCommand = 0;
  /*! Double the number of consecutive unsuccessful attempts to connect to the maxima server

    Each prompt is deemed as but one hint for a working maxima while each crash counts twice
    which hinders us from endlessly restarting in case maxima crashes, outputs something
    seemingly sensible and crashes again.
  */
  int m_unsuccessfulConnectionAttempts = 11;
  //! The current working directory maxima's file I/O is relative to.
  wxString m_CWD;
  //! Do we want to evaluate the file after startup?
  bool m_evalOnStartup = false;
  //! Do we want to exit the program after the evaluation was successful?
  bool m_exitAfterEval = false;
  //! Can we display the "ready" prompt right now?
  bool m_ready = false;

  /*! A human-readable presentation of eventual unmatched-parenthesis type errors

    If text doesn't contain any error this function returns wxEmptyString
  */
  wxString GetUnmatchedParenthesisState(wxString text, std::size_t &index);
  //! The buffer all text from maxima is stored in before converting it to a wxString.
  wxMemoryBuffer m_uncompletedChars;

protected:
  //! Invoke our standard wizard that generates maxima commands
  void CommandWiz(const wxString &title,
                  const wxString &description, const wxString &description_tooltip,
                  const wxString &commandRule,
                  wxString label1, wxString defaultval1, wxString tooltip1 = {},
                  wxString label2 = {}, wxString defaultval2 = {}, wxString tooltip2 = {},
                  wxString label3 = {}, wxString defaultval3 = {}, wxString tooltip3 = {},
                  wxString label4 = {}, wxString defaultval4 = {}, wxString tooltip4 = {},
                  wxString label5 = {}, wxString defaultval5 = {}, wxString tooltip5 = {},
                  wxString label6 = {}, wxString defaultval6 = {}, wxString tooltip6 = {},
                  wxString label7 = {}, wxString defaultval7 = {}, wxString tooltip7 = {},
                  wxString label8 = {}, wxString defaultval8 = {}, wxString tooltip8 = {},
                  wxString label9 = {}, wxString defaultval9 = {}, wxString tooltip9 = {},
                  wxString label10 = {}, wxString defaultval10 = {}, wxString tooltip10 = {}
    );
  //! Reads a potentially unclosed XML tag and closes it
  wxString ReadPotentiallyUnclosedTag(wxStringTokenizer &lines, wxString firstLine);

  //! Finds the name of an opening tag
  static wxRegEx m_xmlOpeningTagName;
  //! Looks if this opening tag is actually complete.
  static wxRegEx m_xmlOpeningTag;
  //! The gnuplot process info
  wxProcess *m_gnuplotProcess = NULL;
  //! Info about the gnuplot process we start for querying the terminals it supports
  wxProcess *m_gnuplotTerminalQueryProcess = NULL;
  //! Is this window active?
  bool m_isActive = true;
  //! Called when this window is focussed or defocussed.
  void OnFocus(wxFocusEvent &event);

  //! Forwards the keyboard focus to a text control that might need it
  void PassKeyboardFocus();
  //! Called when this window is minimized.
  void OnMinimize(wxIconizeEvent &event);
  //! Is called on start and whenever the configuration changes
  void ConfigChanged();
  //! Called when the "Scroll to last error" button is pressed.
  void OnJumpToError(wxCommandEvent &event);
  //! Sends a new char to the symbols sidebar
  void OnSymbolAdd(wxCommandEvent &event);
  //! Called when the "Scroll to currently evaluated" button is pressed.
  void OnFollow(wxCommandEvent &event);
  void OnWizardAbort(wxCommandEvent &event);
  void OnWizardOK(wxCommandEvent &event);
  void OnWizardInsert(wxCommandEvent &event);
  void OnWizardHelpButton(wxCommandEvent &event);

  //! Show the help for Maxima
  void ShowMaximaHelp(wxString = {});

  //! Show the help for Maxima (without handling of anchors).
  void ShowMaximaHelpWithoutAnchor();

  //! Show the help for wxMaxima
  void ShowWxMaximaHelp();

  //! Try to determine if help is needed for maxima or wxMaxima and show this help
  void ShowHelp(const wxString &keyword);

  void CheckForUpdates(bool reportUpToDate = false);

  void OnRecentDocument(wxCommandEvent &event);
  void OnRecentPackage (wxCommandEvent &event);
  void OnUnsavedDocument(wxCommandEvent &event);

  void OnChar(wxKeyEvent &event);
  void OnKeyDown(wxKeyEvent &event);

  /*! The idle task that refreshes the gui (worksheet, menus, title line,...)

    In GUIs normally all events (mouse events, key presses,...) are
    put into a queue and then are executed one by one. This makes
    sure that they will be processed in order - but makes a gui a
    more or less single-processor-task.

    This function now is called once every time the event queue is
    empty (which means: wxMaxima has caught up with all events
    from the GUI). An event.RequestMore() inside that function tells
    the OS to call OnIdle immediately again when OnIdl finishes while
    the event queue is still empty.

    Moving things to the OnIdle() function makes them nearly feel like
    they were in a low-priority background thread without the
    disadvantage that background threads cannot access the GUI.

    Moving the screen update to this function, as well, makes sure that
    if we get events faster than we can update the screen that doesn't
    cause lag, but causes screen updates for old events to be dropped
    if we already have new ones.
    This normally makes programs feel way more responsive.

    The worksheet is additionally refreshed by a timer task in case
    that th computer is too busy to ever reach the idle task at all.
  */
  void OnIdle(wxIdleEvent &event);
  bool m_dataFromMaximaIs = false;

  void MenuCommand(const wxString &cmd);           //!< Inserts command cmd into the worksheet
#ifdef __WXMSW__
  //! Register wxMaxima's own path as the .wxmx diff tool for TortoiseSVN/Git.
  void RegisterWxmxDiffTool();
#endif
  void ReplaceSuggestion(wxCommandEvent &event);   //!< Processes clicks on suggestions
  //! Make the menu item, toolbars and panes visible that should be visible right now.
  void UpdateMenus();        //!< Enables and disables the Right menu buttons
  void UpdateToolBar();      //!< Enables and disables the Right toolbar buttons
  void UpdateSlider();       //!< Updates the slider to show the right frame
  /*! Toggle the visibility of a pane
    \param event The event that triggered calling this function.
  */
  void ShowPane(wxCommandEvent &event);            //<! Makes a sidebar visible
  void OnGnuplotClose(wxProcessEvent &event);      //
  void OnGnuplotQueryTerminals(wxProcessEvent &event);      //

  //! Is triggered when the textstyle drop-down box's value is changed.
  void ChangeCellStyle(wxCommandEvent &event);

  //! Is triggered when the "Find" button in the search dialog is pressed
  void OnFind(wxFindDialogEvent &event);

  //! Is triggered when the "Replace" button in the search dialog is pressed
  void OnReplace(wxFindDialogEvent &event);

  //! Is triggered when the "Replace All" button in the search dialog is pressed
  void OnReplaceAll(wxFindDialogEvent &event);

  /*! Add a parameter to a draw command

    \param cmd The parameter to  add to the draw command
    \param dimensionsOfNewDrawCommand The number of dimensions the new draw command needs to
    have if we need to create one..
  */
  void AddDrawParameter(wxString cmd, int dimensionsOfNewDrawCommand = 2);

  /* Append something to the console. Might be Text or XML maths.

     \return A pointer to the last line of Unicode text that was appended or
     NULL, if there is no such line (for example if the appended object is
     maths instead).
  */
  TextCell *ConsoleAppend(wxString s, CellType type);        //!< append maxima output to console
  void ConsoleAppend(wxXmlDocument xml, CellType type, const wxString &userLabel = {});        //!< append maxima output to console

  enum AppendOpt { NewLine = 1, BigSkip = 2, PromptToolTip = 4, DefaultOpt = NewLine|BigSkip };
  void DoConsoleAppend(wxString s, CellType type, AppendOpt opts = AppendOpt::DefaultOpt,
                       const wxString &userLabel = {});

  /*!Append one or more lines of ordinary unicode text to the console

    \return A pointer to the last line that was appended or NULL, if there is no such line
  */
  TextCell *DoRawConsoleAppend(wxString s, CellType  type, AppendOpt opts = {});

  /*! Spawn the "configure" menu.

    \todo Inform maxima about the new default plot window size.
  */
  //! Trigger reading all variables from Maxima that are shown in the Variables sidebar
  void VarReadEvent(wxCommandEvent &event);
  //! Trigger adding all variables to the variables sidebar
  void VarAddAllEvent(wxCommandEvent &event);
  void EvaluateEvent(wxCommandEvent &event);       //

  void SliderEvent(wxScrollEvent &event);

  //! Issued on double click on the network status
  void NetworkDClick(wxMouseEvent &ev);
  //! Issued on double click on the Maxima status icon
  void MaximaDClick(wxMouseEvent &ev);
  //! Issued on double click on the status message in the status bar
  void StatusMsgDClick(wxMouseEvent &ev);

  //! Issued on double click on a history item
  void HistoryDClick(wxCommandEvent &event);

  //! Issued on double click on a table of contents item
  void TableOfContentsSelection(wxListEvent &event);

  void OnInspectorEvent(wxCommandEvent &ev);

  void DumpProcessOutput();

  //! Try to evaluate the next command for maxima that is in the evaluation queue
  void TriggerEvaluation();

  void TryUpdateInspector();

  bool UpdateDrawPane();

  wxString ExtractFirstExpression(const wxString &entry);

  wxString GetDefaultEntry();

  void OnClose(wxCloseEvent &event);               //!< close wxMaxima window
  wxString GetCommand(bool params = true);         //!< returns the command to start maxima
  //    (uses guessConfiguration)

  void ReadMaximaIPC(const wxString &data){m_ipc.ReadInputData(data);}

  wxString m_maximaVariable_display2d;
  wxString m_maximaVariable_altdisplay2d;
  wxString m_maximaVariable_engineeringFormat;
  bool m_readMaximaVariables = false;
  bool m_updateAutocompletion = true;
  /*! How much CPU time has been used by the system until now? Used by GetMaximaCPUPercentage.

    \return The CPU time elapsed in the same unit as GetMaximaCpuTime(); -1 means: Unable to determine this value.
  */
  long long GetTotalCpuTime();

  /*! How much CPU time has maxima used till now? Used by GetMaximaCPUPercentage.

    \return The CPU time maxima has used in the same unit as GetTotalCpuTime(); -1 means: Unable to determine this value.

  */
  long long GetMaximaCpuTime();

  /*! How much CPU horsepower is maxima using currently?

    \return The percentage of the CPU horsepower maxima is using or -1, if this value is unknown.
  */
  double GetMaximaCPUPercentage();

  //! Does this file contain anything worth saving?
  bool SaveNecessary();

  /*! Setup maxima's variables

    This method is called once when maxima starts. It loads wxmathml.lisp
    and sets some option variables.

    \todo Set pngcairo to be the default terminal as soon as the mac platform
    supports it.
  */
  void SetupVariables();

  /*! Update the title

    Updates the "saved" status, as well, but does only do anything if saved has
    changed or force is true.
    \param saved The new "saved" status
    \param force Force update if the "saved" status hasn't changed.
  */
  void ResetTitle(bool saved, bool force = false);

  void FirstOutput();

  /*! Opens a content.xml file that has been extracted from a broken .wxmx file
   */
  bool OpenXML(const wxString &file, Worksheet *document);

  //! Complains if the version string from the XML file indicates too low a maxima version
  bool CheckWXMXVersion(const wxString &docversion);

  //! Opens a .mac file or a .out file from Xmaxima
  bool OpenMACFile(const wxString &file, Worksheet *document, bool clearDocument = true);

  //! Opens a wxm file
  bool OpenWXMFile(const wxString &file, Worksheet *document, bool clearDocument = true);

  //! Opens a wxmx file
  bool OpenWXMXFile(const wxString &file, Worksheet *document, bool clearDocument = true);

  //! Loads a wxmx description
  std::unique_ptr<GroupCell> CreateTreeFromXMLNode(wxXmlNode *xmlcells, const wxString &wxmxfilename = {});

  /*! Saves the current file

    \param forceSave true means: Always ask for a file name before saving.
    \return true, if the file was saved - or didn't need to
  */
  bool SaveFile(bool forceSave = false);

  //! Try to save the file before closing it - or return false
  bool SaveOnClose();
  /*! Save the project in a temp file.

    Returns false if a save was necessary, but not possible.
  */
  bool AutoSave();

  /*! Tries or offers to save the document

    If auto-save is on the document is automatically saved.

    \return
    * wxID_NO: No saving is necessary, currently
    */
  int SaveDocumentP();

  //! Set the current working directory file I/O from maxima is relative to.
  void SetCWD(wxString file);

  //! Get the current working directory file I/O from maxima is relative to.
  wxString GetCWD()
    {
      return m_CWD;
    }

  std::unique_ptr<Maxima> m_client;
  /*! The Right Way to delete a wxSocketServer

    The destructor might delete the server before all pending server events have been
    processed which leads to a crash.
  */
  struct ServerDeleter {
    void operator()(wxSocketServer* server) const {
      server->Destroy(); // Destroy() calls Close() automatically.
    }
  };
  /*! The server maxima connects to as client

    The destructor of the server causes
    crashes if there are still pending events.
    Instead we need to call destroy.
  */
  std::unique_ptr<wxSocketServer,  ServerDeleter> m_server;

  wxProcess *m_maximaProcess = NULL;
  //! The stdout of the maxima process
  wxInputStream *m_maximaStdout = NULL;
  //! The stderr of the maxima process
  wxInputStream *m_maximaStderr = NULL;
  //! The port the actual maxima process (not its wrapper script) runs at
  int m_port = -1;
  //! A marker for the start of maths
  static wxString m_mathPrefix1;
  //! A marker for the start of maths
  static wxString m_mathPrefix2;
  //! The marker for the start of a input prompt
  static wxString m_promptPrefix;
public:
  //! The marker for the end of a input prompt
  const static wxString m_promptSuffix;
protected:
  /*! True = schedule an update of the table of contents

    used by UpdateTableOfContents() and the idle task.
  */
  bool m_scheduleUpdateToc = false;
  void QuestionAnswered(){if(GetWorksheet()) GetWorksheet()->QuestionAnswered();}
    //! Is called when we get a new list of demo files
  //! Is called when we get a new list of demo files
  void OnNewDemoFiles(wxCommandEvent &event);
  //! Is called when something requests an update of the table of contents
  void OnUpdateTOCEvent(wxCommandEvent &event);

  //! Sets gnuplot's command name and tries to determine gnuplot's path
  void GnuplotCommandName(wxString gnuplot);
  //! The first prompt maxima will output
  static wxString m_firstPrompt;
  bool m_dispReadOut = false;               //!< what is displayed in statusbar
  wxWindowIDRef m_gnuplot_process_id;
  wxWindowIDRef m_maxima_process_id;
  wxString m_lastPrompt;
  wxString m_firstPromptBuffer;
  wxString m_lastPath;
  std::unique_ptr<wxPrintData> m_printData;
  /*! Did we tell maxima to close?

    If we didn't we respan an unexpectedly-closing maxima.
  */
  bool m_closing = false;
  //! The directory with maxima's temp files
  wxString m_maximaTempDir;
  wxString m_maximaHtmlDir;
  bool m_fileSaved = true;
  static int m_exitCode;
  //! Maxima's idea about gnuplot's location
  wxString m_gnuplotcommand;
#ifdef __WINDOWS__
  // For Windows, store "gnuplot.exe", even if wgnuplot.exe is preferred.
  // We query the terminals using: gnuplot -e "print GPVAL_TERMINALS"
  // and that does only work with gnuplot, not wgnuplot.
  //! The Char the current command starts at in the current WorkingGroup
  wxString m_gnuplotcommand_commandline;
#endif
  long m_commandIndex = -1;
  FindReplacePane::FindReplaceData m_findData;
  static wxRegEx m_funRegEx;
  static wxRegEx m_varRegEx;
  static wxRegEx m_blankStatementRegEx;
  static wxRegEx m_sbclCompilationRegEx;
  MathParser m_parser;
  bool m_maximaBusy = true;
  bool m_maximaError = false;
private:
  wxString m_fileToOpen;
  bool m_fourierLoaded = false;

#if wxUSE_DRAG_AND_DROP

  friend class MyDropTarget;

#endif
  friend class MaximaIPC;
  //! The extracted menu-command handlers reach wxMaxima's services (MenuCommand,
  //! the wizards, m_configuration) through this friendship.
  friend class MaximaCommandMenus;
  //! The extracted Maxima-response (Read*) handlers reach wxMaxima's services
  //! (the worksheet, the sidebars, the status bar, the configuration) through
  //! this friendship.
  friend class MaximaResponseReader;
  //! The extracted Maxima process/socket lifecycle reaches wxMaxima's services
  //! (the status bar, worksheet, configuration, process/socket members)
  //! through this friendship.
  friend class MaximaProcessManager;

  /*! A timer that determines when to do the next autosave;

    The actual autosave is triggered if both this timer is expired and the keyboard
    has been inactive for >10s so the autosave won't cause the application to shortly
    stop responding due to saving the file while the user is typing a word.

    This timer is used in one-shot mode so in the unlikely case that saving needs more
    time than this timer to expire the user still got a chance to do something against
    it between two expirys.
  */
  wxTimer m_autoSaveTimer;

  /* A timer that delays redraws while maxima evaluates

     If we always start a redraw when maxima has nearly finished a command that slows
     down evaluating many simple commands in a row.
  */
  wxTimer m_fastResponseTimer;

  //! Starts a single-shot of m_autoSaveTimer.
  void StartAutoSaveTimer();
  //! The menu-command handlers peeled off this god class; bound directly to the
  //! relevant menu/button events. Holds a reference back to this frame
  //! (initialised in the constructor, where *this is unambiguously complete).
  MaximaCommandMenus m_menuCommands;
  //! The Maxima-response (Read*) handlers peeled off this god class;
  //! wxMaxima::MaximaEvent dispatches each incoming chunk to them. Holds a
  //! reference back to this frame (initialised in the constructor, where *this
  //! is unambiguously complete).
  MaximaResponseReader m_responseReader;
  //! The Maxima process/socket lifecycle peeled off this god class. Holds a
  //! reference back to this frame (initialised in the constructor, where *this
  //! is unambiguously complete).
  MaximaProcessManager m_processManager;
};

#if wxUSE_DRAG_AND_DROP

// cppcheck-suppress noConstructor
class MyApp : public wxApp
{
public:
  virtual bool OnInit() override;
  virtual int OnRun() override;
  virtual int OnExit() override;
#if wxUSE_ON_FATAL_EXCEPTION && wxUSE_DEBUGREPORT
  void	OnFatalException () override;
#endif
  /*! Handle a failed wxASSERT.

    In interactive use this defers to wxWidgets' default behaviour (a modal
    "assertion failed" dialog). In non-interactive mode (--batch /
    --exit-on-error, i.e. the test suite) that dialog would pop up on a
    headless CI runner with nobody to dismiss it, wedging the process until
    the CI job's wall-clock limit. Instead we print the assertion's location
    and message to stderr - so the CI log shows exactly which assert fired -
    and abort, turning the hang into a fast, diagnosable test failure.
  */
  void OnAssertFailure(const wxChar *file, int line, const wxChar *func,
                       const wxChar *cond, const wxChar *msg) override;
  /*! Create a new window

    The mac platform insists in making all windows of an application
    share the same process. On the other platforms we create a separate
    process for every wxMaxima session instead which means that each
    process uses the NewWindow() function only once.

    \param file The file name
    \param evalOnStartup Do we want to execute the file automatically, but halt on error?
    \param exitAfterEval Do we want to close the window after the file has been evaluated?
    \param wxmData A .wxm file containing the initial worksheet contents
    \param wxmLen  The length of wxmData
  */
  static void NewWindow(const wxString &file = {}, bool evalOnStartup = false, bool exitAfterEval = false, unsigned char *wxmData = NULL, std::size_t wxmLen = 0);

  void OnFileMenu(wxCommandEvent &ev);

#ifdef __WXMSW__
  /*! Clear the attached console's QuickEdit mode for an unattended batch run.

    A click into a QuickEdit console (the Windows default) puts it into
    selection mode, and every write to that console then blocks the writer -
    at zero CPU, before the main window even exists - until the selection is
    dismissed. Users watching a batch run click into its console all the
    time, so \--batch runs appeared to hang for seconds to minutes on
    startup. Launches without an attached console are unaffected (the
    console handle cannot be opened, and this method does nothing).

    The previous mode is restored in OnExit().
  */
  void DisableConsoleQuickEdit();
  //! Undo DisableConsoleQuickEdit(), if it changed anything.
  void RestoreConsoleMode();
#endif // __WXMSW__

#ifdef __WXMAC__
  void MacNewFile() override;
  void MacOpenFile(const wxString &file) override;
#endif // __WXMAC__

public:
  static wxLogWindow *m_logWindow; // The wxWidgets log window, we use.

  std::unique_ptr<wxLogChain> m_logChain;
  static std::vector<wxProcess *> m_wxMaximaProcesses;
#ifdef USE_QA
#if wxUSE_ON_FATAL_EXCEPTION && wxUSE_DEBUGREPORT
  void GenerateDebugReport(wxDebugReport::Context ctx);
#endif
#endif
  std::unique_ptr<wxLocale> m_locale;
  std::unique_ptr<wxTranslations> m_translations;
#ifdef __WXMSW__
  //! The console input mode DisableConsoleQuickEdit() found, for RestoreConsoleMode().
  unsigned long m_consoleModeToRestore = 0;
  //! True if DisableConsoleQuickEdit() actually changed the console mode.
  bool m_consoleModeChanged = false;
#endif
  //! The name of the config file. Empty = Use the default one.
  wxString m_configFileName;
  Dirstructure m_dirstruct;
  static bool m_allWindowsInOneProcess;
  std::unique_ptr<Configuration> m_configuration;
};

class MyDropTarget : public wxFileDropTarget
{
public:
  explicit MyDropTarget(wxMaxima *wxmax)
    { m_wxmax = wxmax; }

  bool OnDropFiles(wxCoord x, wxCoord y, const wxArrayString &files);

private:
  wxMaxima *m_wxmax = NULL;
};

#endif


// cppcheck-suppress noConstructor

#endif // WXMAXIMA_H
