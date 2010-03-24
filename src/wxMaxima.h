///
///  Copyright (C) 2004-2009 Andrej Vodopivec <andrejv@users.sourceforge.net>
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
#include <wx/process.h>
#include <wx/fdrepdlg.h>
#include <wx/regex.h>
#include <wx/html/htmlwin.h>
#include <wx/dnd.h>

#if defined (__WXMSW__)
 #include <wx/msw/helpchm.h>
#else
 #include <wx/html/helpctrl.h>
#endif

#define SOCKET_SIZE 1024
#define DOCUMENT_VERSION_MAJOR 1
#define DOCUMENT_VERSION_MINOR 1

class MyApp : public wxApp
{
public:
  virtual bool OnInit();
  wxLocale m_locale;
  void NewWindow(wxString file = wxEmptyString);
#if defined (__WXMAC__)
  wxWindowList topLevelWindows;
  void OnFileMenu(wxCommandEvent &ev);
  virtual void MacNewFile();
  virtual void MacOpenFile(const wxString& file);
#endif
};

DECLARE_APP(MyApp)

#ifndef __WXGTK__
class MyAboutDialog : public wxDialog
{
public:
  MyAboutDialog(wxWindow *parent, int id, const wxString title, wxString description);
  ~MyAboutDialog() {};
  void OnLinkClicked(wxHtmlLinkEvent& event);
  DECLARE_EVENT_TABLE()
};
#endif

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
  void SendMaxima(wxString s, bool history = false);
  void OpenFile(wxString file,
                wxString command = wxEmptyString); // Open a file
  bool DocumentSaved() { return m_fileSaved; }
  void LoadImage(wxString file) { m_console->OpenHCaret(file, GC_TYPE_IMAGE); }
protected:
  void CheckForUpdates(bool reportUpToDate = false);
  void OnRecentDocument(wxCommandEvent& event);
  void OnIdle(wxIdleEvent& event);
  void MenuCommand(wxString cmd);                  //
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
  void UpdateMenus(wxUpdateUIEvent& event);        //
  void UpdateToolBar(wxUpdateUIEvent& event);      //
  void UpdateSlider(wxUpdateUIEvent& event);       //
  void ShowPane(wxCommandEvent& event);            //
  void OnProcessEvent(wxProcessEvent& event);      //
  void PopupMenu(wxCommandEvent& event);           //
  void StatsMenu(wxCommandEvent& event);           //

  void OnFind(wxFindDialogEvent& event);
  void OnFindClose(wxFindDialogEvent& event);
  void OnReplace(wxFindDialogEvent& event);
  void OnReplaceAll(wxFindDialogEvent& event);

  void ServerEvent(wxSocketEvent& event);          // server event: maxima connection
  void ClientEvent(wxSocketEvent& event);          // client event: maxima input/output

  void ConsoleAppend(wxString s, int type);        // append maxima output to console
  void DoConsoleAppend(wxString s, int type,       //
                       bool newLine = true, bool bigSkip = true);
  void DoRawConsoleAppend(wxString s, int type);   //

  void EditInputMenu(wxCommandEvent& event);       //
  void EvaluateEvent(wxCommandEvent& event);       //
  void InsertMenu(wxCommandEvent& event);          //
  void SliderEvent(wxScrollEvent& event);
  void HistoryDClick(wxCommandEvent& event);
  void OnInspectorEvent(wxCommandEvent& ev);
  void DumpProcessOutput();
  void TryEvaluateNextInQueue();
  void TryUpdateInspector();

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
  void ReadLoadSymbols();            // functions after load command
#ifndef __WXMSW__
  void ReadProcessOutput();          // reads output of maxima command
#endif

  void SetupVariables();             // sets some maxima variables
  void KillMaxima();                 // kills the maxima process
  void ResetTitle(bool saved);
  void FirstOutput(wxString s);

  // loading functions
  bool OpenWXMFile(wxString file, MathCtrl *document, bool clearDocument = true);
  bool OpenWXMXFile(wxString file, MathCtrl *document, bool clearDocument = true);
  GroupCell* CreateTreeFromXMLNode(wxXmlNode *xmlcells, wxString wxmxfilename = wxEmptyString);
  GroupCell* CreateTreeFromWXMCode(wxArrayString *wxmLines);

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
  wxString m_lastPrompt;
  wxString m_lastPath;
  MathParser m_MParser;
  wxPrintData* m_printData;
#if WXM_PRINT
  bool m_supportPrinting;
#endif
  bool m_closing;
  wxString m_openFile;
  wxString m_currentFile;
  bool m_fileSaved;
  bool m_variablesOK;
  wxString m_helpFile;
  wxString m_maximaVersion;
  wxString m_lispVersion;
#if defined (__WXMSW__)
  wxCHMHelpController m_helpCtrl;
#else
  wxHtmlHelpController m_helpCtrl;
#endif
  wxFindReplaceDialog *m_findDialog;
  wxFindReplaceData m_findData;
  wxRegEx m_funRegEx;
  wxRegEx m_varRegEx;
  DECLARE_EVENT_TABLE()
};

#if wxUSE_DRAG_AND_DROP

class MyDropTarget : public wxFileDropTarget
{
public:
  MyDropTarget(wxMaxima * wxmax) { m_wxmax = wxmax; }
  bool OnDropFiles(wxCoord x, wxCoord y, const wxArrayString& files);
private:
  wxMaxima *m_wxmax;
};

#endif

#endif //_WXMAXIM_H_
