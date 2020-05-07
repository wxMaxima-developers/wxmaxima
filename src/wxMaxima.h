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

#include "wxMaximaFrame.h"
#include "MathParser.h"
#include "Dirstructure.h"

#include <wx/socket.h>
#include <wx/config.h>
#include <wx/process.h>
#include <wx/regex.h>
#include <wx/html/htmlwin.h>
#include <wx/dnd.h>
#include <wx/wfstream.h>
#include <wx/zstream.h>
#include <wx/txtstrm.h>
#include <wx/sckstrm.h>
#include <wx/buffer.h>
#include <memory>
#ifdef __WXMSW__
#include <windows.h>
#endif

#if defined (__WXMSW__)
#include <wx/msw/helpchm.h>
#endif

#include <wx/html/helpctrl.h>

#define DOCUMENT_VERSION_MAJOR 1
/*! The part of the .wxmx format version number that appears after the dot.
  
  - Updated to version 1.1 after user selectable animation-speeds were introduced:
    Old wxMaxima versions play them back in the default speed instead but still
    open the file.
  - Bumped to version 1.2 after saving highlighting was introduced: Older versions
    of wxMaxima will ignore the highlighting on opening .wxmx files.
  - Bumped to version 1.3 after sub-subsections were introduced:
    Old wxMaxima versions interpret them as subsections but still open the file.
  - Bumped to version 1.4 when we started allowing to embed .jpg images in a .wxmx
    file. wxMaxima versions between 13.04 and 15.08 replace these images by a 
    "image cannot be loaded" marker but will correctly display the rest of the file.
  - Bumped to version 1.5 when GroupCells were added an attribute that allows them to
    be used as an answer to questions.
 */
#define DOCUMENT_VERSION_MINOR 5

//! How many miliseconds should we wait between polling for stdout+cpu power?
#define MAXIMAPOLLMSECS 2000

#ifndef __WXGTK__

class MyAboutDialog : public wxDialog
{
public:
  MyAboutDialog(wxWindow *parent, int id, const wxString title, wxString description);

  ~MyAboutDialog()
  {};

  void OnLinkClicked(wxHtmlLinkEvent &event);

};

#endif

/* The top-level window and the main application logic

 */
class wxMaxima : public wxMaximaFrame
{
public:

  wxMaxima(wxWindow *parent, int id, wxLocale *locale, const wxString title,
           const wxString &filename = wxEmptyString,
           const wxPoint pos = wxDefaultPosition, const wxSize size = wxDefaultSize);
  
  virtual ~wxMaxima();

  //! Pipe maxima's output to stdout
  static void PipeToStdout(){m_pipeToStdout = true;}
  static void ExitOnError(){m_exitOnError = true;}
  static void ExtraMaximaArgs(wxString args){m_extraMaximaArgs = args;}

  //! An enum of individual IDs for all timers this class handles
  enum TimerIDs
  {
    //! The keyboard was inactive long enough that we can attempt an auto-save.
            KEYBOARD_INACTIVITY_TIMER_ID,
    //! The time between two auto-saves has elapsed.
            AUTO_SAVE_TIMER_ID,
    //! We look if we got new data from maxima's stdout.
            MAXIMA_STDOUT_POLL_ID,
            //! We have finished waiting if the current string ends in a newline
            WAITFORSTRING_ID,
            /*! We have given Maxima enough time to do the important 

              now it is time to compile the list of helpfile anchors */
            COMPILEHELPANCHORS_ID
  };

  /*! A timer that determines when to do the next autosave;

    The actual autosave is triggered if both this timer is expired and the keyboard
    has been inactive for >10s so the autosave won't cause the application to shortly
    stop responding due to saving the file while the user is typing a word.

    This timer is used in one-shot mode so in the unlikely case that saving needs more
    time than this timer to expire the user still got a chance to do something against
    it between two expirys. 
   */
  wxTimer m_autoSaveTimer;

  //! A timer that tells us to wait until maxima ends its data.
  wxTimer m_waitForStringEndTimer;

  //! A timer that ells us that we now can do the low-prio compilation of help anchors
  wxTimer m_compileHelpAnchorsTimer;
  
  //! Is triggered when a timer this class is responsible for requires
  void OnTimerEvent(wxTimerEvent &event);

  //! A timer that polls for output from the maxima process.
  wxTimer m_maximaStdoutPollTimer;

  void ShowTip(bool force);

  void SetWXMdata(wxString data){m_initialWorkSheetContents = data;}
  //! Do we want to evaluate the document on statup?
  void EvalOnStartup(bool eval)
    {
      m_evalOnStartup = eval;
    }
  
  //! Do we want to exit the program after evaluation?
  void ExitAfterEval(bool exitaftereval)
    {
      m_exitAfterEval = exitaftereval;
      if(exitaftereval)
      {
        m_logPane->SetBatchMode();
      }
    }
  
  void StripLispComments(wxString &s);

  void SendMaxima(wxString s, bool addToHistory = false);

  //! Open a file
  bool OpenFile(wxString file,
                wxString command = wxEmptyString);

  //! Does this document need saving?
  bool DocumentSaved()
  { return m_fileSaved; }

  //! Load an image from a file into the worksheet.
  void LoadImage(wxString file)
  { m_worksheet->OpenHCaret(file, GC_TYPE_IMAGE); }

  //! Query the value of a new maxima variable
  bool QueryVariableValue();

  //! A version number that can be compared using "<" and ">"
  class VersionNumber
  {
  public:
    explicit VersionNumber(wxString version);
    long Major() const {return m_major;}
    long Minor() const {return m_minor;}
    long Patchlevel() const {return m_patchlevel;}
    friend bool operator<(const VersionNumber& v1, const VersionNumber& v2);
    friend bool operator>(const VersionNumber& v1, const VersionNumber& v2);
  private:
    long m_major;
    long m_minor;
    long m_patchlevel;
  };

private:
  #ifdef HAVE_OMP_HEADER
  omp_lock_t m_helpFileAnchorsLock;
  #endif
  //! wxm data the worksheet is populated from 
  wxString m_initialWorkSheetContents;
  static bool m_pipeToStdout;
  static bool m_exitOnError;
  static wxString m_extraMaximaArgs;
  //! Search for the wxMaxima help file
  wxString SearchwxMaximaHelp();
  wxLocale *m_locale;
  //! The variable names to query for the variables pane
  wxArrayString m_varNamesToQuery;
  bool m_isLogTarget;
  //! Is true if opening the file from the command line failed before updating the statusbar.
  bool m_openInitialFileError;
  //! Escape strings into a format lisp accepts
  wxString EscapeForLisp(wxString str);
  //! The number of Jiffies Maxima had used the last time we asked
  long long m_maximaJiffies_old;
  //! The number of Jiffies the CPU had made the last time
  long long m_cpuTotalJiffies_old;
  //! Do we need to update the menus + toolbars?
  bool m_updateControls;
  //! All configuration commands we still have to send to maxima
  wxString m_configCommands;
  //! A RegEx that matches gnuplot errors.
  wxRegEx m_gnuplotErrorRegex;
  //! Clear the evaluation queue and return true if "Abort on Error" is set. 
  bool AbortOnError();
  //! This string allows us to detect when the string we search for has changed.
  wxString m_oldFindString;
  //! This string allows us to detect when the string we search for has changed.
  int m_oldFindFlags;
  //! On opening a new file we only need a new maxima process if the old one ever evaluated cells.
  bool m_hasEvaluatedCells;
  //! Searches for maxima's output prompts
  wxRegEx m_outputPromptRegEx;
  //! The number of output cells the current command has produced so far.
  int m_outputCellsFromCurrentCommand;
  //! The maximum number of lines per command we will display 
  int m_maxOutputCellsPerCommand;
  /*! Double the number of consecutive unsuccessful attempts to connect to the maxima server

    Each prompt is deemed as but one hint for a working maxima while each crash counts twice
    which hinders us from endlessly restarting in case maxima crashes, outputs something 
    seemingly sensible and crashes again.
   */
  int m_unsuccessfulConnectionAttempts;
  //! The current working directory maxima's file I/O is relative to.
  wxString m_CWD;
  //! Do we want to evaluate the file after startup?
  bool m_evalOnStartup;
  //! Do we want to exit the program after the evaluation was successful?
  bool m_exitAfterEval;
  //! Can we display the "ready" prompt right now?
  bool m_ready;

  /*! A human-readable presentation of eventual unmatched-parenthesis type errors

    If text doesn't contain any error this function returns wxEmptyString
  */
  wxString GetUnmatchedParenthesisState(wxString text,int &index);
  //! The buffer all text from maxima is stored in before converting it to a wxString.
  wxMemoryBuffer m_uncompletedChars;

protected:
  void CompileHelpFileAnchors();
  //! The gnuplot process info
  wxProcess *m_gnuplotProcess;
  //! Is this window active?
  bool m_isActive;
  //! Called when this window is activated or deactivated.
  void OnActivate(wxActivateEvent &event);
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

  /*! Get the name of the help file
   */
  wxString GetMaximaHelpFile();
  //! A helper function for GetHelpFile()
  wxString GetMaximaHelpFile2();

  //! Show the help for maxima
  void ShowMaximaHelp(wxString keyword = wxEmptyString);

  //! Show the help for wxMaxima
  void ShowWxMaximaHelp();
  
  //! Show the maxima help in MSW's proprietary help format
  void ShowCHMHelp(wxString helpfile, wxString keyword);
  
  //! Try to determine if help is needed for maxima or wxMaxima and show this help
  void ShowHelp(wxString keyword);

  /*! Launches the HTML help browser

    \param helpfile The name of the file the help browser has to be launched with
    \param keyword The keyword to show help for
  */
  void ShowHTMLHelp(wxString helpfile, wxString keyword = wxEmptyString);

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

    If the queue is empty (which means that the computer has caught 
    up with the incoming events) this function is called once.

    Moving the screen update to this function makes sure that we
    don't do many subsequent updates slowing down the computer any 
    further if there are still a handful of key presses to process
    lowering the average response times of the program.

    The worksheet is refreshed by a timer task in case the computer 
    is too busy to execute the idle task at all.
  */
  void OnIdle(wxIdleEvent &event);
  /*! Read and interpret the new data from maxima

    We don't interpret this data directly in the idle event since if we
    block somewhere in the idle event we block gnome.
    \return
      - true, if there was new data
      - false, if there wasn't any new data.
   */
  bool InterpretDataFromMaxima();
  bool m_dataFromMaximaIs;
  
  void MenuCommand(wxString cmd);                  //!< Inserts command cmd into the worksheet
  void FileMenu(wxCommandEvent &event);            //!< Processes "file menu" clicks
  void MaximaMenu(wxCommandEvent &event);          //!< Processes "maxima menu" clicks
  void AlgebraMenu(wxCommandEvent &event);         //!< Processes "algebra menu" clicks
  void EquationsMenu(wxCommandEvent &event);       //!< Processes "equations menu" clicks
  void CalculusMenu(wxCommandEvent &event);        //!< event handling for menus
  void SimplifyMenu(wxCommandEvent &event);        //!< Processes "Simplify menu" clicks
  void PlotMenu(wxCommandEvent &event);            //!< Processes "Plot menu" cloicks
  void ListMenu(wxCommandEvent &event);            //!< Processes "list menu" clicks
  void DrawMenu(wxCommandEvent &event);            //!< Processes "draw menu" clicks
  void NumericalMenu(wxCommandEvent &event);       //!< Processes "Numerical menu" clicks
  void HelpMenu(wxCommandEvent &event);            //!< Processes "Help menu" clicks
  void EditMenu(wxCommandEvent &event);            //!< Processes "Edit menu" clicks
  void ReplaceSuggestion(wxCommandEvent &event);   //!< Processes clicks on suggestions
  void Interrupt(wxCommandEvent &event);           //!< Interrupt button and hotkey presses
  //! Make the menu item, toolbars and panes visible that should be visible right now.
  void UpdateMenus(wxUpdateUIEvent &event);     //!< Enables and disables the Right menu buttons
  void UpdateToolBar(wxUpdateUIEvent &event);      //!< Enables and disables the Right toolbar buttons
  void UpdateSlider(wxUpdateUIEvent &event);       //!< Updates the slider to show the right frame
  /*! Toggle the visibility of a pane
    \param event The event that triggered calling this function.
   */
  void ShowPane(wxCommandEvent &event);            //<! Makes a sidebar visible
  void OnProcessEvent(wxProcessEvent &event);      //
  void OnGnuplotClose(wxProcessEvent &event);      //
  void PopupMenu(wxCommandEvent &event);           //
  void StatsMenu(wxCommandEvent &event);           //

  //! Is triggered when the textstyle drop-down box's value is changed.
  void ChangeCellStyle(wxCommandEvent &event);
  
  //! Is triggered when the "Find" button in the search dialog is pressed
  void OnFind(wxFindDialogEvent &event);

  //! Is triggered when the "Close" button in the search dialog is pressed
  void OnFindClose(wxFindDialogEvent &event);

  //! Is triggered when the "Replace" button in the search dialog is pressed
  void OnReplace(wxFindDialogEvent &event);

  //! Is triggered when the "Replace All" button in the search dialog is pressed
  void OnReplaceAll(wxFindDialogEvent &event);

  //! Is called if maxima connects to wxMaxima.
  void OnMaximaConnect();
  
  //! server event: Maxima sends or receives data, connects or disconnects
  void ServerEvent(wxSocketEvent &event);

  /* Tries to read the new data from maxima

     Is called by ClientEvent() if wxWidgets reckons there is data. But as this sometimes
     doesn't happen even if there is data (only on MSW) it is called from the idle loop,
     as well.
  */
  void TryToReadDataFromMaxima();
    
  //! Triggered when we get new chars from maxima.
  void OnNewChars();

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
  TextCell *ConsoleAppend(wxString s, CellType type, wxString userLabel = wxEmptyString);        //!< append maxima output to console
  void DoConsoleAppend(wxString s, CellType  type, 
                       bool newLine = true, bool bigSkip = true, wxString userLabel = wxEmptyString);

  /*!Append one or more lines of ordinary unicode text to the console

    \return A pointer to the last line that was appended or NULL, if there is no such line
   */
  TextCell *DoRawConsoleAppend(wxString s, CellType  type); 

  /*! Spawn the "configure" menu.

    \todo Inform maxima about the new default plot window size.
  */
  void EditInputMenu(wxCommandEvent &event);
  //! Trigger reading all variables from Maxima that are shown in the Variables sidebar
  void VarReadEvent(wxCommandEvent &event);
  //! Trigger adding all variables to the variables sidebar
  void VarAddAllEvent(wxCommandEvent &event);
  void EvaluateEvent(wxCommandEvent &event);       //
  void InsertMenu(wxCommandEvent &event);          //
  void PrintMenu(wxCommandEvent &event);

  void SliderEvent(wxScrollEvent &event);

  //! Issued on double click on the network status
  void NetworkDClick(wxCommandEvent &ev);

  //! Issued on double click on a history item
  void HistoryDClick(wxCommandEvent &event);

  //! Issued on double click on a table of contents item
  void TableOfContentsSelection(wxListEvent &event);

  void OnInspectorEvent(wxCommandEvent &ev);

  void DumpProcessOutput();

  //! Try to evaluate the next command for maxima that is in the evaluation queue
  void TriggerEvaluation();

  void TryUpdateInspector();

  void UpdateDrawPane();
  
  wxString ExtractFirstExpression(wxString entry);

  wxString GetDefaultEntry();
  
  //! starts the server
  bool StartServer();
  /*! starts maxima (uses getCommand) or restarts it if needed

    Normally a restart is only needed if
      - maxima isn't currently in the process of starting up or
      - maxima is running and has never evaluated any program 
        so a restart won't change anything
    \param force true means to restart maxima unconditionally.
   */
  bool StartMaxima(bool force = false);

  void OnClose(wxCloseEvent &event);               //!< close wxMaxima window
  wxString GetCommand(bool params = true);         //!< returns the command to start maxima
  //    (uses guessConfiguration)

  //! Polls the stderr and stdout of maxima for input.
  void ReadStdErr();

  /*! Determines the process id of maxima from its initial output

    This function does several things:
     - it sets m_pid to the process id of maxima
     - it discards all data until this point
     - and it prepares the worksheet for editing.

     \param data The string ReadFirstPrompt() does read its data from. 
                  After leaving this function data is empty again.
   */
  void ReadFirstPrompt(wxString &data);

  /*! Determine where the text for ReadMiscText ends

    Every error message or other line maxima outputs should end in a newline character. 
    But sometimes it doesn't and a <code>\<mth\></code> tag comes first \f$ =>\f$ This 
    function determines where the miscellaneous text ends.
   */
  int GetMiscTextEnd(const wxString &data);

  //! Find the end of a tag in wxMaxima's output.
  int FindTagEnd(wxString &data, const wxString &tag);

  /*! Reads text that isn't enclosed between xml tags.

     Some commands provide status messages before the math output or the command has finished.
     This function makes wxMaxima output them directly as they arrive.

     After processing the lines not enclosed in xml tags they are removed from data.
   */
  void ReadMiscText(wxString &data);

  /*! Reads the input prompt from Maxima.

     After processing the input prompt it is removed from data.
   */
  void ReadPrompt(wxString &data);

  /*! Reads the output of wxstatusbar() commands

    wxstatusbar allows the user to give and update visual feedback from long-running 
    commands and makes sure this feedback is deleted once the command is finished.
   */
  void ReadStatusBar(wxString &data);

  /*! Reads the math cell's contents from Maxima.
     
     Math cells are enclosed between the tags \<mth\> and \</mth\>. 
     This function removes the from data after appending them
     to the console.

     After processing the status bar marker is removed from data.
   */
  void ReadMath(wxString &data);

  /*! Reads autocompletion templates we get on definition of a function or variable

    After processing the templates they are removed from data.
   */
  void ReadLoadSymbols(wxString &data);

  //! Read (and discard) suppressed output
  void ReadSuppressedOutput(wxString &data);

  /*! Reads the variable values maxima advertises to us
   */
  void ReadVariables(wxString &data);
  
  /*! Reads the "add variable to watch list" tag maxima can send us
   */
  void ReadAddVariables(wxString &data);

#ifndef __WXMSW__

  //! reads the output the maxima command sends to stdout
  void ReadProcessOutput();

#endif

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

  void KillMaxima(bool logMessage = true);                 //!< kills the maxima process
  /*! Update the title

    Updates the "saved" status, as well, but does only do anything if saved has
    changed or force is true.
    \param saved The new "saved" status
    \param force Force update if the "saved" status hasn't changed.
   */
  void ResetTitle(bool saved, bool force = false);

  /*! Makes this window the debug log target of all windows from this maxima process

    Only necessary on the mac where the same process creates loads of windows.
   */
  void BecomeLogTarget();
  
  void FirstOutput();

  /*! Opens a content.xml file that has been extracted from a broken .wxmx file
   */
  bool OpenXML(wxString file, Worksheet *document);

  //! Complains if the version string from the XML file indicates too low a maxima version
  bool CheckWXMXVersion(wxString docversion);

  //! Reads the contents of a .mac or a .out file. Used by OpenMacFile
  wxString ReadMacContents(wxString file);

  //! Opens a .mac file or a .out file from Xmaxima
  bool OpenMACFile(wxString file, Worksheet *document, bool clearDocument = true);

  //! Opens a wxm file
  bool OpenWXMFile(wxString file, Worksheet *document, bool clearDocument = true);

  //! Opens a wxmx file
  bool OpenWXMXFile(wxString file, Worksheet *document, bool clearDocument = true);

  //! Loads a wxmx description
  GroupCell *CreateTreeFromXMLNode(wxXmlNode *xmlcells, wxString wxmxfilename = wxEmptyString);

  /*! Saves the current file

    \param forceSave true means: Always ask for a file name before saving.
   */
  bool SaveFile(bool forceSave = false);

  //! Try to save the file before closing it - or return false 
  bool SaveOnClose();
  /*! Save the project in a temp file.

    Returns false if a save was necessary, but not possible.
   */
  bool AutoSave();
  
  int SaveDocumentP();

  //! Set the current working directory file I/O from maxima is relative to.
  void SetCWD(wxString file);

  //! Get the current working directory file I/O from maxima is relative to.
  wxString GetCWD()
  {
    return m_CWD;
  }

  std::shared_ptr<wxSocketBase> m_client;
  std::shared_ptr<wxSocketInputStream> m_clientStream;
  std::unique_ptr<wxTextInputStream> m_clientTextStream;
  wxSocketServer *m_server;
  wxProcess *m_process;
  //! The stdout of the maxima process
  wxInputStream *m_maximaStdout;
  //! The stderr of the maxima process
  wxInputStream *m_maximaStderr;
  int m_port;
  //! All chars from maxima that still aren't part of m_currentOutput
  wxString m_newCharsFromMaxima;
  /*! The end of maxima's current uninterpreted output, see m_currentOutput.
   
    If we just want to look if maxima's current output contains an ending tag
    this is the place we can search in fast.

    wxEmptyString means that the current output isn't long enought to make 
    creating this string worthwile.
   */
  wxString m_currentOutputEnd;
  //! All from maxima's current output we still haven't interpreted
  wxString m_currentOutput;
  //! The marker for the start of a input prompt
  wxString m_promptPrefix;
  //! The marker for the end of a input prompt
  wxString m_promptSuffix;
  //! The marker for the start of a variables section
  wxString m_variablesPrefix;
  //! The marker for the end of a variables section
  wxString m_variablesSuffix;
  //! The marker for the start of a "add variables" section
  wxString m_addVariablesPrefix;
  //! The marker for the end of a "add variables" section
  wxString m_addVariablesSuffix;
  //! The marker for the start of a list of autocompletion templates
  wxString m_symbolsPrefix;
  //! The marker for the end of a list of autocompletion templates
  wxString m_symbolsSuffix;
  //! The marker that tells to start suppressing any output from maxima
  wxString m_suppressOutputPrefix;
  //! The marker that tells to stop to suppress any output from maxima
  wxString m_suppressOutputSuffix;
  wxString m_firstPrompt;
  bool m_dispReadOut;               //!< what is displayed in statusbar
  wxString m_lastPrompt;
  wxString m_lastPath;
  std::unique_ptr<wxPrintData> m_printData;
  /*! Did we tell maxima to close?

    If we didn't we respan an unexpectedly-closing maxima.
   */
  bool m_closing;
  wxString m_openFile;
  //! The directory with maxima's temp files
  wxString m_maximaTempDir;
  //! The directory with maxima's documentation
  wxString m_maximaDocDir;
  bool m_fileSaved;
  wxString m_chmhelpFile;
  wxString m_maximaVersion;
  wxString m_maximaArch;
  wxString m_lispVersion;
  wxString m_lispType;
  static int m_exitCode;
  //! Maxima's idea about gnuplot's location
  wxString m_gnuplotcommand;
  //! The Char the current command starts at in the current WorkingGroup
  int m_commandIndex;
#if defined (__WXMSW__)
  wxCHMHelpController m_chmhelpCtrl;
#endif
  wxHtmlHelpController m_htmlhelpCtrl;
  wxFindReplaceData m_findData;
  wxRegEx m_funRegEx;
  wxRegEx m_varRegEx;
  wxRegEx m_blankStatementRegEx;
  wxRegEx m_sbclCompilationRegEx;
  MathParser m_parser;
  bool m_maximaBusy;
  wxMemoryBuffer m_rawDataToSend;
  unsigned long int m_rawBytesSent;
#if wxUSE_DRAG_AND_DROP

  friend class MyDropTarget;

#endif
};

#if wxUSE_DRAG_AND_DROP

class MyDropTarget : public wxFileDropTarget
{
public:
  explicit MyDropTarget(wxMaxima *wxmax)
  { m_wxmax = wxmax; }

  bool OnDropFiles(wxCoord x, wxCoord y, const wxArrayString &files);

private:
  wxMaxima *m_wxmax;
};

#endif


// cppcheck-suppress noConstructor
class MyApp : public wxApp
{
public:
  virtual bool OnInit();
  virtual int OnRun();
  virtual int OnExit();
  wxLocale m_locale;

  /*! Create a new window

    The mac platform insists in making all windows of an application
    share the same process. On the other platforms we create a separate
    process for every wxMaxima session instead which means that each
    process uses the NewWindow() function only once.

    \param file The file name
    \param evalOnStartup Do we want to execute the file automatically, but halt on error?
    \param exitAfterEval Do we want to close the window after the file has been evaluated?
   */
  void NewWindow(wxString file = wxEmptyString, bool evalOnStartup = false, bool exitAfterEval = false, unsigned char *wxmData = NULL, int wxmLen = 0);

  void NewTutorialWindow(wxString contents);

  static std::vector<wxMaxima *> m_topLevelWindows;
  static void DelistTopLevelWindow(wxMaxima *);

  void OnFileMenu(wxCommandEvent &ev);

  virtual void MacNewFile();
  void BecomeLogTarget();

  virtual void MacOpenFile(const wxString &file);

private:
  //! The name of the config file. Empty = Use the default one.
  wxString m_configFileName;
  Dirstructure m_dirstruct;
};

// cppcheck-suppress unknownMacro
DECLARE_APP(MyApp)

#endif // WXMAXIMA_H
