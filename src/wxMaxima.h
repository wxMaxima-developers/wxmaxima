/*
 *  Copyright (C) 2004-2005 Andrej Vodopivec <andrejv@users.sourceforge.net>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */


#ifndef _WXMAXIMA_H_
#define _WXMAXIMA_H_

#include "wxMaximaFrame.h"
#include "MathParser.h"

#include <wx/socket.h>
#include <wx/config.h>
#include <wx/dnd.h>
#include <wx/process.h>

#include <vector>

using namespace std;

#define SOCKET_SIZE 1024

enum {
  TEXTT,
  INPUTT,
  PROMPTT,
  MPROMPTT,
  ERRORT,
  COMMENTT
};

class wxMaxima : public wxMaximaFrame
{
 public:
  wxMaxima(wxWindow *parent, int id, const wxString title,
           const wxPoint pos, const wxSize size = wxDefaultSize);
  ~wxMaxima();
  void ShowTip(bool force);
  void InitSession();
  void SendMaxima(wxString s, bool clear=true, bool out=true,
                  bool silent=true);               // sends input to maxima
 protected:
  void EnterCommand(wxCommandEvent& event);        // enter in the command line

  void FileMenu(wxCommandEvent& event);            //
  void PrintMenu(wxCommandEvent& event);           //
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

  void ServerEvent(wxSocketEvent& event);          // server event: maxima connection
  void ClientEvent(wxSocketEvent& event);          // client event: maxima input/output
  
  void ConsoleAppend(wxString s, int type = TEXTT);// append maxima output to console
  void DoConsoleAppend(wxString s, int type,       //
          bool newLine, bool bigSkip = true);      //
  void DoRawConsoleAppend(wxString s, int type,    //
          bool newLine = true);                    //
  
  void CheckForPrintingSupport();
  wxString GetDefaultEntry();
  bool StartServer();                              // starts the server
  bool StartMaxima();                              // starts maxima (uses getCommand)
  void CleanUp();                                  // shuts down server and client on exit
  void OnClose(wxCloseEvent& event);               // close wxMaxima window
  bool GuessConfiguration();                       // tries to guess the configuration values
  wxString GetCommand();                           // returns the command to start maxima
                                                   //    (uses guessConfiguration)

  void ReadFirstPrompt();            // reads everything before first prompt
                                     // setsup m_pid
  void ReadPrompt();                 // reads prompts
  void ReadMath();                   // reads output other than prompts
  void ReadLispError();              // lisp errors (no prompt prefix/suffix)
#ifndef __WXMSW__
  void ReadProcessOutput();          // reads output of maxima command
#endif
  void SetupVariables();             // sets some maxima variables
  void KillMaxima();                 // kills the maxima process
  void FirstOutput(wxString s);
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
  bool m_readingPrompt;
  bool m_dispReadOut;               // what is displayed in statusbar
  bool m_inLispMode;                // don't add ; in lisp mode
  bool m_inPrompt;
  wxString m_lastPrompt;
  wxString m_monitorFile;
  time_t m_monitorTime;
  wxString m_lastPath;
  MathParser m_MParser;
  wxPrintData* m_printData;
  bool m_supportPrinting;
  bool m_closing;
  DECLARE_EVENT_TABLE()
};

#endif	//_WXMAXIM_H_
