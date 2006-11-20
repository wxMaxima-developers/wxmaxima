///
///  Copyright (C) 2004-2006 Andrej Vodopivec <andrejv@users.sourceforge.net>
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

#ifndef _WXMAXIMA_H_
#define _WXMAXIMA_H_

#include "wxMaximaFrame.h"
#include "MathParser.h"

#include <wx/socket.h>
#include <wx/config.h>
#include <wx/dnd.h>
#include <wx/process.h>

#if defined (__WXMSW__)
 #include <wx/msw/helpchm.h>
#else
 #include <wx/html/helpctrl.h>
#endif

#define SOCKET_SIZE 1024

class MyApp : public wxApp
{
public:
  virtual bool OnInit();
  wxLocale m_locale;
};

DECLARE_APP(MyApp)

class wxMaxima : public wxMaximaFrame
{
public:
  wxMaxima(wxWindow *parent, int id, const wxString title,
           const wxPoint pos, const wxSize size = wxDefaultSize);
  ~wxMaxima();
  void ShowTip(bool force);
  wxString GetHelpFile();
  void ShowHelp(wxString keyword = wxEmptyString);
  void InitSession();
  void SetOpenFile(wxString file)
  {
    m_openFile = file;
  }
  void SendMaxima(wxString s, bool clear = true, bool out = true,
                  bool silent = true,   // sends input to maxima, also add to command
                  bool split = true);   // history...
  bool ReadBatchFile(wxString file);
protected:
  void EnterCommand(wxCommandEvent& event);        // enter in the command line

  void FileMenu(wxCommandEvent& event);            //
  void MaximaMenu(wxCommandEvent& event);          //
  void AlgebraMenu(wxCommandEvent& event);         //
  void EquationsMenu(wxCommandEvent& event);       //
  void CalculusMenu(wxCommandEvent& event);        // event handling for menus
  void SimplifyMenu(wxCommandEvent& event);        //
  void PlotMenu(wxCommandEvent& event);            //
  void NumericalMenu(wxCommandEvent& event);       //
  void HelpMenu(wxCommandEvent& event);            //
  void EditMenu(wxCommandEvent& event);            //
  void Interrupt(wxCommandEvent& event);           //
  void OnMonitorFile(wxCommandEvent& event);       //
  void OnActivate(wxActivateEvent& event);         //
  void OnSetFocus(wxFocusEvent& event);            //
  void UpdateMenus(wxUpdateUIEvent& event);        //
  void UpdateToolBar(wxUpdateUIEvent& event);      //
  void OnProcessEvent(wxProcessEvent& event);      //
  void PopupMenu(wxCommandEvent& event);           //

  void ServerEvent(wxSocketEvent& event);          // server event: maxima connection
  void ClientEvent(wxSocketEvent& event);          // client event: maxima input/output

  void ConsoleAppend(wxString s, int type);        // append maxima output to console
  void DoConsoleAppend(wxString s, int type,       //
                       bool newLine = true, bool bigSkip = true);
  void DoRawConsoleAppend(wxString s, int type,    //
                          bool newLine = true);    //

  void EditInputMenu(wxCommandEvent& event);       //
  void ReEvaluate(wxCommandEvent& event);          //
  void PrependCell(wxCommandEvent& event);         //
  void HandleCellEvent(wxCommandEvent& event);
  void DumpProcessOutput();

  void HandleMainPrompt(wxString prompt);

  wxString SplitInput(wxString input);

#if WXM_PRINT
  void CheckForPrintingSupport();
  void PrintMenu(wxCommandEvent& event);
#endif

  wxString GetDefaultEntry();
  bool StartServer();                              // starts the server
  bool StartMaxima();                              // starts maxima (uses getCommand)
  void CleanUp();                                  // shuts down server and client on exit
  void OnClose(wxCloseEvent& event);               // close wxMaxima window
  wxString GetCommand(bool params = true);         // returns the command to start maxima
                                                   //    (uses guessConfiguration)

  void ReadFirstPrompt();            // reads everything before first prompt
  // setsup m_pid
  void ReadPrompt();                 // reads prompts
  void ReadMath();                   // reads output other than prompts
  void ReadLispError();              // lisp errors (no prompt prefix/suffix)
  void OpenFile(wxString file,
                wxString command = wxEmptyString);      // Open a file
#ifndef __WXMSW__
  void ReadProcessOutput();          // reads output of maxima command
#endif

  void SetupVariables();             // sets some maxima variables
  void KillMaxima();                 // kills the maxima process
  void ResetTitle(bool saved);
  void FirstOutput(wxString s);
  wxString RemoveTabs(wxString s);
  wxSocketBase *m_client;
  wxSocketServer *m_server;
  bool m_isConnected;
  bool m_isRunning;
  bool m_first;
  long m_pid;
  wxProcess *m_process;
  wxInputStream *m_input;
  int m_port;
  wxString m_currentOutput;
  wxString m_promptSuffix;
  wxString m_promptPrefix;
  wxString m_firstPrompt;
  wxString m_commentPrefix;
  wxString m_newInput;
  bool m_readingPrompt;
  bool m_dispReadOut;               // what is displayed in statusbar
  bool m_inLispMode;                // don't add ; in lisp mode
  bool m_inPrompt;
  bool m_inInsertMode;
  wxString m_lastPrompt;
  wxString m_monitorFile;
  time_t m_monitorTime;
  wxString m_lastPath;
  MathParser m_MParser;
  wxPrintData* m_printData;
#if WXM_PRINT
  bool m_supportPrinting;
#endif
  bool m_closing;
  wxString m_openFile;
  wxString m_currentFile;
  wxArrayString m_batchFileLines;
  unsigned int m_batchFilePosition;
  bool m_fileSaved;
  bool m_variablesOK;
  wxString m_helpFile;
#if defined (__WXMSW__)
  wxCHMHelpController m_helpCtrl;
#else
  wxHtmlHelpController m_helpCtrl;
#endif
  DECLARE_EVENT_TABLE()
};

#endif //_WXMAXIM_H_
