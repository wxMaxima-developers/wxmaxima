// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2013 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2014-2017 Gunter Königsmann <wxMaxima@physikbuch.de>
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

/*!\file
  This file declares the class wxMaxima that contains most of the program's logic. 

  The worksheet is defined in the class MathCtrl instead and 
  everything surrounding it in wxMaximaFrame.
 */


#ifndef WXMAXIMA_H
#define WXMAXIMA_H

#include "wxMaximaFrame.h"
#include "MathParser.h"

#include <wx/socket.h>
#include <wx/config.h>
#include <wx/process.h>
#include <wx/regex.h>
#include <wx/html/htmlwin.h>
#include <wx/dnd.h>

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


#ifndef __WXGTK__

class MyAboutDialog : public wxDialog
{
public:
  MyAboutDialog(wxWindow *parent, int id, const wxString title, wxString description);

  ~MyAboutDialog()
  {};

  void OnLinkClicked(wxHtmlLinkEvent &event);

DECLARE_EVENT_TABLE()
};

#endif

/* The top-level window and the main application logic

 */
class wxMaxima : public wxMaximaFrame
{
public:

  wxMaxima(wxWindow *parent, int id, const wxString title, const wxString configFile,
           const wxPoint pos, const wxSize size = wxDefaultSize);
  
  ~wxMaxima();

  void CleanUp();                                  //!< shuts down server and client on exit
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

  /*! A timer that determines when to do the next autosave;

    The actual autosave is triggered if both this timer is expired and the keyboard
    has been inactive for >10s so the autosave won't cause the application to shortly
    stop responding due to saving the file while the user is typing a word.

    This timer is used in one-shot mode so in the unikely case that saving needs more
    time than this timer to expire the user still got a chance to do something against
    it between two expirys. 
   */
  wxTimer m_autoSaveTimer;

  //! Is triggered when a timer this class is responsible for requires
  void OnTimerEvent(wxTimerEvent &event);

  //! A timer that polls for output from the maxima process.
  wxTimer m_maximaStdoutPollTimer;

  /*! The interval between auto-saves (in milliseconds). 

    Values <10000 mean: Auto-save is off.
  */
  long int m_autoSaveInterval;

  void ShowTip(bool force);

  /*! Get the name of the help file

    \todo We probably should use the help files from the newest maxima version
    installed instead of the ones from the alphabetically first installation we find.
   */
  wxString GetHelpFile();

  void ShowMaximaHelp(wxString keyword = wxEmptyString);

  void ShowWxMaximaHelp();

  void InitSession();

  void SetOpenFile(wxString file)
  {
    m_openFile = file;
  }

  void SetBatchMode(bool batch = false)
  {
    m_batchmode = batch;
  }

  void StripComments(wxString &s);

  void SendMaxima(wxString s, bool history = false);

  void OpenFile(wxString file,
                wxString command = wxEmptyString); //!< Open a file
  bool DocumentSaved()
  { return m_fileSaved; }

  void LoadImage(wxString file)
  { m_console->OpenHCaret(file, GC_TYPE_IMAGE); }

private:
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
  //! The number of consecutive unsuccessful attempts to connect to the maxima server
  int m_unsuccessfullConnectionAttempts;
  //! The current working directory maxima's file I/O is relative to.
  wxString m_CWD;
  //! Are we in batch mode?
  bool m_batchmode;
  //! Can we display the "ready" prompt right now?
  bool m_ready;

  /*! A human-readable presentation of eventual unmatched-parenthesis type errors

    If text doesn't contain any error this function returns wxEmptyString

    \todo: Use iterators for traversing the string as they are *way* faster than
    the current method.
   */
  wxString GetUnmatchedParenthesisState(wxString text,int &index);

protected:
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

  //! Called when the "Scroll to currently evaluated" button is pressed.
  void OnFollow(wxCommandEvent &event);

  void ShowCHMHelp(wxString helpfile, wxString keyword);

  /*! Launches the HTML help browser

    \param helpfile The name of the file the help browser has to be launched with
    \param otherhelpfile We offer help for maxima and wxMaxima in separate manuals.
                         This parameter contains the filename of the manual we aren't
                         using currently so the help browser can open a tab containing
                         this file.
  */
  void ShowHTMLHelp(wxString helpfile, wxString otherhelpfile, wxString keyword);

  void CheckForUpdates(bool reportUpToDate = false);

  void OnRecentDocument(wxCommandEvent &event);
  void OnUnsavedDocument(wxCommandEvent &event);

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

  void MenuCommand(wxString cmd);                  //!< Inserts command cmd into the worksheet
  void FileMenu(wxCommandEvent &event);            //!< Processes "file menu" clicks
  void MaximaMenu(wxCommandEvent &event);          //!< Processes "maxima menu" clicks
  void AlgebraMenu(wxCommandEvent &event);         //!< Processes "algebra menu" clicks
  void EquationsMenu(wxCommandEvent &event);       //!< Processes "equations menu" clicks
  void CalculusMenu(wxCommandEvent &event);        //!< event handling for menus
  void SimplifyMenu(wxCommandEvent &event);        //!< Processes "Simplify menu" clicks
  void PlotMenu(wxCommandEvent &event);            //!< Processes "Plot menu" cloicks
  void NumericalMenu(wxCommandEvent &event);       //!< Processes "Numerical menu" clicks
  void HelpMenu(wxCommandEvent &event);            //!< Processes "Help menu" clicks
  void EditMenu(wxCommandEvent &event);            //!< Processes "Edit menu" clicks
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
  void PopupMenu(wxCommandEvent &event);           //
  void StatsMenu(wxCommandEvent &event);           //

  //! Is triggered when the "Find" button in the search dialog is pressed
  void OnFind(wxFindDialogEvent &event);

  //! Is triggered when the "Close" button in the search dialog is pressed
  void OnFindClose(wxFindDialogEvent &event);

  //! Is triggered when the "Replace" button in the search dialog is pressed
  void OnReplace(wxFindDialogEvent &event);

  //! Is triggered when the "Replace All" button in the search dialog is pressed
  void OnReplaceAll(wxFindDialogEvent &event);

  void SanitizeSocketBuffer(char *buffer, int length);  //!< fix early nulls
  void ServerEvent(wxSocketEvent &event);          //!< server event: maxima connection
  /*! Is triggered on Input or disconnect from maxima

    The data we get from maxima is split into small packets we append to m_currentOutput 
    until we got a full line we can display.
   */
  void ClientEvent(wxSocketEvent &event);

  void ConsoleAppend(wxString s, int type, wxString userLabel = wxEmptyString);        //!< append maxima output to console
  void DoConsoleAppend(wxString s, int type,       //
                       bool newLine = true, bool bigSkip = true, wxString userLabel = wxEmptyString);

  void DoRawConsoleAppend(wxString s, int type);   //

  /*! Spawn the "configure" menu.

    \todo Inform maxima about the new default plot window size.
  */
  void EditInputMenu(wxCommandEvent &event);

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
  void TryEvaluateNextInQueue();

  void TryUpdateInspector();

  wxString ExtractFirstExpression(wxString entry);

  wxString GetDefaultEntry();

  bool StartServer();                              //!< starts the server
  /*!< starts maxima (uses getCommand) or restarts it if needed

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
    But sometimes it doesn't and a <code><mth></code> tag comes first \f$ =>\f$ This 
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

#ifndef __WXMSW__

  //!< reads the output the maxima command sends to stdout
  void ReadProcessOutput();

#endif

  //!< Does this file contain anything worth saving?
  bool SaveNecessary();

  /*!
    This method is called once when maxima starts. It loads wxmathml.lisp
    and sets some option variables.

    \todo Set pngcairo to be the default terminal as soon as the mac platform 
    supports it.
 */
  void SetupVariables();

  void KillMaxima();                 //!< kills the maxima process
  /*! Update the title

    Updates the "saved" status, as well, but does only do anything if saved has
    changed or force is true.
    \param saved The new "saved" status
    \param force Force update if the "saved" status hasn't changed.
   */
  void ResetTitle(bool saved, bool force = false);

  void FirstOutput(wxString s);

  /*! Opens a content.xml file that has been extracted from a broken .wxmx file
   */
  bool OpenXML(wxString file, MathCtrl *document, bool clearDocument = true);

  //! Complains if the version string from the XML file indicates too low a maxima version
  bool CheckWXMXVersion(wxString docversion);

  //! Reads the contents of a .mac or a .out file. Used by OpenMacFile
  wxString ReadMacContents(wxString file);

  //! Opens a .mac file or a .out file from Xmaxima
  bool OpenMACFile(wxString file, MathCtrl *document, bool clearDocument = true);

  //! Opens a wxm file
  bool OpenWXMFile(wxString file, MathCtrl *document, bool clearDocument = true);

  //! Opens a wxmx file
  bool OpenWXMXFile(wxString file, MathCtrl *document, bool clearDocument = true);

  //! Loads a wxmx description
  GroupCell *CreateTreeFromXMLNode(wxXmlNode *xmlcells, wxString wxmxfilename = wxEmptyString);

  /*! Saves the current file

    \param forceSave true means: Always ask for a file name before saving.
   */
  bool SaveFile(bool forceSave = false);

  int SaveDocumentP();

  //! Set the current working directory file I/O from maxima is relative to.
  void SetCWD(wxString file);

  //! Get the current working directory file I/O from maxima is relative to.
  wxString GetCWD()
  {
    return m_CWD;
  }

  wxSocketBase *m_client;
  wxSocketServer *m_server;
  //! Is the network connection to maxima working?
  bool m_isConnected;
  //! Is maxima running?
  bool m_isRunning;
  wxProcess *m_process;
  //! The stdout of the maxima process
  wxInputStream *m_maximaStdout;
  //! The stderr of the maxima process
  wxInputStream *m_maximaStderr;
  int m_port;
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
  //! The marker for the start of a list of autocompletion templates
  wxString m_symbolsPrefix;
  //! The marker for the end of a list of autocompletion templates
  wxString m_symbolsSuffix;
  wxString m_firstPrompt;
  bool m_dispReadOut;               //!< what is displayed in statusbar
  bool m_inLispMode;                //!< don't add ; in lisp mode
  wxString m_lastPrompt;
  wxString m_lastPath;
  wxPrintData *m_printData;
  bool m_closing;
  char *m_inputBuffer;
  wxString m_openFile;
  bool m_fileSaved;
  bool m_variablesOK;
  wxString m_chmhelpFile;
  bool m_htmlHelpInitialized;
  wxString m_maximaVersion;
  wxString m_lispVersion;
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
#if wxUSE_DRAG_AND_DROP

  friend class MyDropTarget;

#endif
DECLARE_EVENT_TABLE()
};

#if wxUSE_DRAG_AND_DROP

class MyDropTarget : public wxFileDropTarget
{
public:
  MyDropTarget(wxMaxima *wxmax)
  { m_wxmax = wxmax; }

  bool OnDropFiles(wxCoord x, wxCoord y, const wxArrayString &files);

private:
  wxMaxima *m_wxmax;
};

#endif

class MyApp : public wxApp
{
public:
  virtual bool OnInit();

#if defined (__WXMSW__)
  virtual int OnExit();
#endif
  wxLocale m_locale;

  /*! Create a new window

    The mac platform insists in making all windows of an application
    share the same process. On the other platforms we create a separate
    process for every wxMaxima session instead which means that each
    process uses the NewWindow() function only once.

    \param file The file name
    \param batchmode Do we want to execute the file and save it, but halt on error?
   */
  void NewWindow(wxString file = wxEmptyString, bool batchmode = false);

  //! Is called by atExit and tries to close down the maxima process if wxMaxima has crashed.
  static void Cleanup_Static();

  //! A pointer to the currently running wxMaxima instance
  static wxMaxima *m_frame;

  wxWindowList topLevelWindows;

  void OnFileMenu(wxCommandEvent &ev);

  virtual void MacNewFile();

  virtual void MacOpenFile(const wxString &file);
private:
  //! The name of the config file. Empty = Use the default one.
  wxString m_configFileName;
  DECLARE_EVENT_TABLE()
};

DECLARE_APP(MyApp)

#endif // WXMAXIMA_H
