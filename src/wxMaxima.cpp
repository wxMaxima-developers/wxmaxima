///
///  Copyright (C) 2004-2008 Andrej Vodopivec <andrejv@users.sourceforge.net>
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

#include "wxMaxima.h"
#include "Config.h"
#include "SubstituteWiz.h"
#include "IntegrateWiz.h"
#include "LimitWiz.h"
#include "Plot2dWiz.h"
#include "SeriesWiz.h"
#include "SumWiz.h"
#include "Plot3dWiz.h"
#include "Gen1Wiz.h"
#include "Gen2Wiz.h"
#include "Gen3Wiz.h"
#include "Gen4Wiz.h"
#include "BC2Wiz.h"
#include "MatWiz.h"
#include "SystemWiz.h"
#include "MathPrintout.h"
#include "MyTipProvider.h"
#include "EditorCell.h"
#include "SlideShowCell.h"

#include <wx/clipbrd.h>
#include <wx/filedlg.h>
#include <wx/utils.h>
#include <wx/msgdlg.h>
#include <wx/textfile.h>
#include <wx/tokenzr.h>
#include <wx/mimetype.h>
#include <wx/dynlib.h>
#include <wx/dir.h>
#include <wx/filename.h>
#include <wx/artprov.h>
#include <wx/aboutdlg.h>

#include <wx/zipstrm.h>
#include <wx/wfstream.h>
#include <wx/txtstrm.h>

enum {
  maxima_process_id
};

#if defined (__WXMAC__)
  #define MACPREFIX "wxMaxima.app/Contents/Resources/"
#endif

wxMaxima::wxMaxima(wxWindow *parent, int id, const wxString title,
                   const wxPoint pos, const wxSize size) :
    wxMaximaFrame(parent, id, title, pos, size)
{
  m_port = 4010;
  m_pid = -1;
  m_inLispMode = false;
  m_first = true;
  m_isRunning = false;
  m_promptSuffix = wxT("<PROMPT-S/>");
  m_promptPrefix = wxT("<PROMPT-P/>");
  GetMenuBar()->Enable(menu_interrupt_id, false);

  m_firstPrompt = wxT("(%i1) ");

  m_client = NULL;
  m_server = NULL;

  wxConfig::Get()->Read(wxT("lastPath"), &m_lastPath);
  m_lastPrompt = wxEmptyString;

#if WXM_PRINT
  CheckForPrintingSupport();
  m_printData = new wxPrintData;
  m_printData->SetQuality(wxPRINT_QUALITY_HIGH);
#endif

  m_closing = false;
  m_openFile = wxEmptyString;
  m_currentFile = wxEmptyString;
  m_fileSaved = true;

  m_variablesOK = false;

  m_batchFilePosition = 0;

  m_helpFile = wxEmptyString;

  m_isConnected = false;
  m_isRunning = false;


  LoadRecentDocuments();
  UpdateRecentDocuments();

  m_console->SetFocus();
}

wxMaxima::~wxMaxima()
{
  if (m_client != NULL)
    m_client->Destroy();

#if WXM_PRINT
  delete m_printData;
#endif
}

///--------------------------------------------------------------------------------
///  Startup
///--------------------------------------------------------------------------------

#if WXM_PRINT

void wxMaxima::CheckForPrintingSupport()
{
#if defined __WXMSW__
  m_supportPrinting = true;
#elif defined __WXMAC__
  m_supportPrinting = true;
#elif defined __WXGTK__
 #if wxCHECK_VERSION(2, 9, 0)
  m_supportPrinting = true;
 #elif defined wxUSE_LIBGNOMEPRINT
  #if wxUSE_LIBGNOMEPRINT
  wxLogNull log;
  wxDynamicLibrary* m_gnomep = new wxDynamicLibrary(wxT("libgnomeprint-2-2.so"));
  wxDynamicLibrary* m_gnomepui = new wxDynamicLibrary(wxT("libgnomeprintui-2-2.so"));
  if (m_gnomep->IsLoaded() && m_gnomepui->IsLoaded())
    m_supportPrinting = true;
  else
    m_supportPrinting = false;
  delete m_gnomep;
  delete m_gnomepui;
  #else
  m_supportPrinting = false;
  #endif
 #else
  m_supportPrinting = false;
 #endif
#endif
  m_supportPrinting = true;
}

#endif

void wxMaxima::InitSession()
{
  bool server = false;
  int defaultPort = 4010;

  wxConfig::Get()->Read(wxT("defaultPort"), &defaultPort);
  m_port = defaultPort;

  while (!(server = StartServer()))
  {
    m_port++;
    if (m_port > defaultPort + 50)
    {
      wxMessageBox(_("wxMaxima could not start the server.\n\n"
                     "Please check you have network support\n"
                     "enabled and try again!"),
                   _("Fatal error"),
                   wxOK | wxICON_ERROR);
      break;
    }
  }

  if (!server)
    SetStatusText(_("Starting server failed"));
  else if (!StartMaxima())
    SetStatusText(_("Starting maxima process failed"), 1);
}

void wxMaxima::FirstOutput(wxString s)
{
  bool showHeader = false;
  wxConfig::Get()->Read(wxT("showHeader"), &showHeader);

  int start = s.Find(m_firstPrompt);
  //m_console->ClearWindow(); // we don't want to clear document when restarting maxima

  if (showHeader) {
    DoRawConsoleAppend(wxEmptyString, MC_TYPE_MAIN_PROMPT);
    DoRawConsoleAppend(s.SubString(0, start - 2), MC_TYPE_HEADER);
  }

  if (m_batchFileLines.GetCount()>0) {
    PrintFile();
  }

  m_lastPrompt = wxT("(%i1) ");
  //m_console->SetSaved(true);
}

/***
 * This reads the file as a wxMaxima batch file. If the file is OK we
 * setup inputs and comments and return true. If not we return false - wxMaxima
 * then uses batch to load the file!
 */
bool wxMaxima::ReadBatchFile(wxString file)
{
  wxTextFile inputFile(file);
  m_batchFileLines.Clear();

  if (!inputFile.Open()) {
    wxMessageBox(_("Error opening file"), _("Error"));
    return false;
  }

  if (inputFile.GetFirstLine() !=
      wxT("/* [wxMaxima batch file version 1] [ DO NOT EDIT BY HAND! ]*/"))
  {
    inputFile.Close();
    return false;
  }

  wxString line;
  for (line = inputFile.GetFirstLine();
       !inputFile.Eof();
       line = inputFile.GetNextLine()) {
    m_batchFileLines.Add(line);
  }
  m_batchFileLines.Add(line);

  inputFile.Close();
  m_batchFilePosition = 0;
  m_currentFile = file;

  AddRecentDocument(file);
  return true;
}

///--------------------------------------------------------------------------------
///  Appending stuff to output
///--------------------------------------------------------------------------------

/***
 * ConsoleAppend adds a new line s of type to the console window. It will call
 * DoConsoleAppend if s is in xml and DoRawCosoleAppend if s is not in xml.
 */
void wxMaxima::ConsoleAppend(wxString s, int type)
{
  m_dispReadOut = false;
  s.Replace(m_promptSuffix, wxEmptyString);

  wxString t(s);
  t.Trim();
  t.Trim(false);
  if (!t.Length())
    return ;

  if (type != MC_TYPE_ERROR)
    SetStatusText(_("Parsing output"), 1);

  if (type == MC_TYPE_DEFAULT)
  {
    while (s.Length() > 0)
    {
      int start = s.Find(wxT("<mth"));
      if (start == -1) {
        t = s;
        t.Trim();
        t.Trim(false);
        if (t.Length())
          DoRawConsoleAppend(s, MC_TYPE_DEFAULT);
        s = wxEmptyString;
      }

      else {
        wxString pre = s.SubString(0, start - 1);
        wxString pre1(pre);
        pre1.Trim();
        pre1.Trim(false);
        int end = s.Find(wxT("</mth>"));
        if (end == -1)
          end = s.Length();
        else
          end += 5;
        wxString rest = s.SubString(start, end);

        if (pre1.Length()) {
          DoRawConsoleAppend(pre, MC_TYPE_DEFAULT);
          DoConsoleAppend(wxT("<span>") + rest +
                          wxT("</span>"), type, false);
        }
        else {
          DoConsoleAppend(wxT("<span>") + rest +
                          wxT("</span>"), type, false);
        }
        s = s.SubString(end + 1, s.Length());
      }
    }
  }

  else if (type == MC_TYPE_PROMPT) {
    SetStatusText(_("Ready for user input"), 1);
    m_lastPrompt = s;

    if (s.StartsWith(wxT("MAXIMA> "))) {
      s = s.Right(8);
    }
    else
      s = s + wxT(" ");

    DoConsoleAppend(wxT("<span>") + s + wxT("</span>"), type, true);
  }

  else if (type == MC_TYPE_ERROR)
    DoRawConsoleAppend(s, MC_TYPE_ERROR);

  else
    DoConsoleAppend(wxT("<span>") + s + wxT("</span>"), type, false);
}

void wxMaxima::DoConsoleAppend(wxString s, int type, bool newLine,
                               bool bigSkip)
{
  MathCell* cell;

  s.Replace(wxT("\n"), wxT(""), true);

  cell = m_MParser.ParseLine(s, type);

  if (cell == NULL)
  {
    wxMessageBox(_("There was an error in generated XML!\n\n"
                   "Please report this as a bug."), _("Error"),
                 wxOK | wxICON_EXCLAMATION);
    return ;
  }

  cell->SetSkip(bigSkip);
  m_console->InsertLine(cell, newLine || cell->BreakLineHere());
}

void wxMaxima::DoRawConsoleAppend(wxString s, int type, bool newLine, bool hide)
{
  if (type == MC_TYPE_INPUT || type == MC_TYPE_TEXT ||
      type == MC_TYPE_SECTION || type == MC_TYPE_TITLE ||
      type == MC_TYPE_HEADER)
  {
    EditorCell* cell = new EditorCell();

    cell->SetValue(s);
    cell->SetType(type);

    m_console->InsertLine(cell, newLine);
  }

  else if (type == MC_TYPE_MAIN_PROMPT)
  {
    TextCell* cell = new TextCell(s);
    cell->SetType(type);
    m_console->InsertLine(cell, newLine, hide);
  }

  else
  {
    wxStringTokenizer tokens(s, wxT("\n"));
    int count = 0;
    while (tokens.HasMoreTokens())
    {
      TextCell* cell = new TextCell(tokens.GetNextToken());

      cell->SetType(type);

      if (tokens.HasMoreTokens())
        cell->SetSkip(false);

      if (count == 0)
        m_console->InsertLine(cell, newLine);
      else
        m_console->InsertLine(cell, true);
      count++;
    }
  }
}

void wxMaxima::SendMaxima(wxString s)
{
  if (!m_isConnected) {
    ConsoleAppend(wxT("\nNot connected to maxima!\n"), MC_TYPE_ERROR);
    return ;
  }

  if (!m_variablesOK) {
    m_variablesOK = true;
    SetupVariables();
  }

#if wxUSE_UNICODE
  s.Replace(wxT("\x00B2"), wxT("^2"));
  s.Replace(wxT("\x00B3"), wxT("^3"));
#endif

  SetStatusText(_("Maxima is calculating"), 1);
  m_dispReadOut = false;

  s.Replace(wxT("\n"), wxEmptyString);
  s.Append(wxT("\n"));
  m_console->EnableEdit(false);

#if wxUSE_UNICODE
  m_client->Write(s.utf8_str(), strlen(s.utf8_str()));
#else
  m_client->Write(s.c_str(), s.Length());
#endif
}

///--------------------------------------------------------------------------------
///  Socket stuff
///--------------------------------------------------------------------------------

/***
 * Client event is triggered when there is something we can read from
 * the socket.
 */
void wxMaxima::ClientEvent(wxSocketEvent& event)
{
  char buffer[SOCKET_SIZE + 1];
  int read;
  switch (event.GetSocketEvent())
  {

  case wxSOCKET_INPUT:
    m_client->Read(buffer, SOCKET_SIZE);
    if (!m_client->Error())
    {
      read = m_client->LastCount();
      buffer[read] = 0;

#if wxUSE_UNICODE
      m_currentOutput += wxString(buffer, wxConvUTF8);
#else
      m_currentOutput += wxString(buffer, *wxConvCurrent);
#endif

      if (!m_dispReadOut && m_currentOutput != wxT("\n")) {
        SetStatusText(_("Reading maxima output"), 1);
        m_dispReadOut = true;
      }

      if (m_first && m_currentOutput.Find(m_firstPrompt) > -1)
        ReadFirstPrompt();

      ReadMath();

      ReadPrompt();

      ReadLispError();
    }
    break;

  case wxSOCKET_LOST:
    if (!m_closing)
      ConsoleAppend(wxT("\nCLIENT: Lost socket connection ...\n"
                        "Restart maxima with 'Maxima->Restart maxima'.\n"),
                    MC_TYPE_ERROR);
    m_console->SetWorkingGroup(NULL);
    m_console->SetSelection(NULL);
    m_console->SetActiveCell(NULL);
    m_pid = -1;
    GetMenuBar()->Enable(menu_interrupt_id, false);
    m_client->Destroy();
    m_client = NULL;
    m_isConnected = false;
    break;

  default:
    break;
  }
}

/***
 * ServerEvent is triggered when maxima connects to the socket server.
 */
void wxMaxima::ServerEvent(wxSocketEvent& event)
{
  switch (event.GetSocketEvent())
  {

  case wxSOCKET_CONNECTION :
    {
      if (m_isConnected) {
        wxSocketBase *tmp = m_server->Accept(false);
        tmp->Close();
        return;
      }
      m_isConnected = true;
      m_client = m_server->Accept(false);
      m_client->SetEventHandler(*this, socket_client_id);
      m_client->SetNotify(wxSOCKET_INPUT_FLAG | wxSOCKET_LOST_FLAG);
      m_client->Notify(true);
#ifndef __WXMSW__
      ReadProcessOutput();
#endif
    }
    break;

  case wxSOCKET_LOST:
    if (!m_closing)
      ConsoleAppend(wxT("\nSERVER: Lost socket connection ...\n"
                        "Restart maxima with 'Maxima->Restart maxima'.\n"),
                    MC_TYPE_ERROR);
    m_pid = -1;
    GetMenuBar()->Enable(menu_interrupt_id, false);
    m_isConnected = false;

  default:
    break;
  }
}

bool wxMaxima::StartServer()
{
  SetStatusText(wxString::Format(_("Starting server on port %d"), m_port), 1);

  wxIPV4address addr;

#ifndef __WXMAC__
  addr.LocalHost();
#else
  addr.AnyAddress();
#endif

  addr.Service(m_port);

  m_server = new wxSocketServer(addr);
  if (!m_server->Ok())
  {
    delete m_server;
    m_server = NULL;
    m_isRunning = false;
    m_isConnected = false;
    SetStatusText(_("Starting server failed"), 1);
    return false;
  }
  SetStatusText(_("Server started"), 1);
  m_server->SetEventHandler(*this, socket_server_id);
  m_server->SetNotify(wxSOCKET_CONNECTION_FLAG);
  m_server->Notify(true);

  m_isConnected = false;
  m_isRunning = true;
  return m_isRunning;
}

///--------------------------------------------------------------------------------
///  Maxima process stuff
///--------------------------------------------------------------------------------

bool wxMaxima::StartMaxima()
{
  if (m_isConnected)
  {
    KillMaxima();
    //    m_client->Close();
    m_isConnected = false;
  }

  m_variablesOK = false;
  wxString command = GetCommand();

  if (command.Length() > 0)
  {
#if defined(__WXMSW__)
    if (wxGetOsVersion() == wxWIN95)
    {
      wxString maximaPrefix = command.SubString(1, command.Length() - 3);
      wxString sysPath;

      wxGetEnv(wxT("path"), &sysPath);
      maximaPrefix.Replace(wxT("\\bin\\maxima.bat"), wxEmptyString);

      wxSetEnv(wxT("maxima_prefix"), maximaPrefix);
      wxSetEnv(wxT("path"), maximaPrefix + wxT("\\bin;") + sysPath);

      command = maximaPrefix + wxT("\\lib\\maxima");
      if (!wxDirExists(command))
        return false;

      wxArrayString files;
      wxDir::GetAllFiles(command, &files, wxT("maxima.exe"));
      if (files.Count() == 0)
        return false;
      else
      {
        command = files[0];
        command.Append(wxString::Format(
                         wxT(" -eval \"(maxima::start-client %d)\" -eval \"(run)\" -f"),
                         m_port
                       ));
      }
    }
    else
      command.Append(wxString::Format(wxT(" -s %d"), m_port));
    wxSetEnv(wxT("home"), wxGetHomeDir());
#else
    command.Append(wxString::Format(wxT(" -r \":lisp (setup-client %d)\""),
                                    m_port));
#endif

#if defined __WXMAC__
    wxSetEnv(wxT("DISPLAY"), wxT(":0.0"));
#endif

    m_process = new wxProcess(this, maxima_process_id);
    m_process->Redirect();
    m_first = true;
    GetMenuBar()->Enable(menu_interrupt_id, false);
    m_pid = -1;
    SetStatusText(_("Starting maxima..."), 1);
    wxExecute(command, wxEXEC_ASYNC, m_process);
    m_input = m_process->GetInputStream();
    SetStatusText(_("Maxima started. Waiting for connection..."), 1);
  }
  else
    return false;
  return true;
}


void wxMaxima::Interrupt(wxCommandEvent& event)
{
  if (m_pid < 0)
  {
    GetMenuBar()->Enable(menu_interrupt_id, false);
    return ;
  }
#if defined (__WXMSW__)
  wxString path, maxima = GetCommand(false);
  wxArrayString out;
  maxima = maxima.SubString(2, maxima.Length() - 3);
  wxFileName::SplitPath(maxima, &path, NULL, NULL);
  wxString command = wxT("\"") + path + wxT("\\winkill.exe\"");
  command += wxString::Format(wxT(" -INT %ld"), m_pid);
  wxExecute(command, out);
#else
  wxProcess::Kill(m_pid, wxSIGINT);
#endif
}

void wxMaxima::KillMaxima()
{
  m_process->Detach();
  if (m_pid < 0)
  {
    if (m_inLispMode)
      SendMaxima(wxT("($quit)"));
    else
      SendMaxima(wxT("quit();"));
    return ;
  }
  wxProcess::Kill(m_pid, wxSIGKILL);
}

void wxMaxima::OnProcessEvent(wxProcessEvent& event)
{
  if (!m_closing)
    SetStatusText(_("Maxima process terminated."), 1);

//  delete m_process;
//  m_process = NULL;
}

void wxMaxima::CleanUp()
{
  if (m_isConnected)
    KillMaxima();
  if (m_isRunning)
    m_server->Destroy();
}

///--------------------------------------------------------------------------------
///  Dealing with stuff read from the socket
///--------------------------------------------------------------------------------

void wxMaxima::ReadFirstPrompt()
{
#if defined(__WXMSW__)
  int start = m_currentOutput.Find(wxT("Maxima"));
  if (start == -1)
    start = 0;
  FirstOutput(wxT("wxMaxima ")
              wxT(VERSION)
              wxT(" http://wxmaxima.sourceforge.net\n") +
              m_currentOutput.SubString(start, m_currentOutput.Length() - 1));
#endif // __WXMSW__

  int s = m_currentOutput.Find(wxT("pid=")) + 4;
  int t = s + m_currentOutput.SubString(s, m_currentOutput.Length()).Find(wxT("\n"));

  if (s < t)
    m_currentOutput.SubString(s, t).ToLong(&m_pid);
  if (m_pid > 0)
    GetMenuBar()->Enable(menu_interrupt_id, true);

  m_first = false;
  m_inLispMode = false;
  SetStatusText(_("Ready for user input"), 1);

  if (m_openFile.Length())
  {
    OpenFile(m_openFile);
    m_openFile = wxEmptyString;
  }

  m_closing = false; // when restarting maxima this is temporarily true
  m_currentOutput = wxEmptyString;
  m_console->ShowHCaret();
  m_console->EnableEdit(true);
  m_console->Refresh();
}

/***
 * Checks if maxima displayed a new chunk of math
 */
void wxMaxima::ReadMath()
{
  int end = m_currentOutput.Find(m_promptPrefix);

  while (end > -1)
  {
    m_readingPrompt = true;
    wxString o = m_currentOutput.Left(end);
    ConsoleAppend(o, MC_TYPE_DEFAULT);
    m_currentOutput = m_currentOutput.SubString(end + m_promptPrefix.Length(),
                      m_currentOutput.Length());
    end = m_currentOutput.Find(m_promptPrefix);
  }

  if (m_readingPrompt)
    return ;

  wxString mth = wxT("</mth>");
  end = m_currentOutput.Find(mth);
  while (end > -1)
  {
    wxString o = m_currentOutput.Left(end);
    ConsoleAppend(o + mth, MC_TYPE_DEFAULT);
    m_currentOutput = m_currentOutput.SubString(end + mth.Length(),
                      m_currentOutput.Length());
    end = m_currentOutput.Find(mth);
  }
}

/***
 * Checks if maxima displayed a new prompt.
 */
void wxMaxima::ReadPrompt()
{
  bool ready = true;
  int end = m_currentOutput.Find(m_promptSuffix);
  if (end > -1)
  {
    m_readingPrompt = false;
    wxString o = m_currentOutput.Left(end);
    if (o != wxT("\n") && o.Length())
    {
      // Maxima displayed a new main prompt
      if (o.StartsWith(wxT("(%i")))
      {
        m_lastPrompt = o;
        m_console->m_evaluationQueue->RemoveFirst(); // remove it from queue

        if (m_console->m_evaluationQueue->GetFirst() == NULL) { // queue empty?
          m_console->ShowHCaret();
          m_console->SetWorkingGroup(NULL);
          m_console->Refresh();
          SetStatusText(_("Ready for user input"), 1);
        }
        else { // we don't have an empty queue
          m_console->Refresh();
          m_console->EnableEdit();
          ready = false;
          TryEvaluateNextInQueue();
        }

        m_console->EnableEdit();
      }

      // We have a question
      else {
        if (o.Find(wxT("<mth>")) > -1)
          DoConsoleAppend(o, MC_TYPE_PROMPT);
        else
          DoRawConsoleAppend(o, MC_TYPE_PROMPT);
      }

      if (o.StartsWith(wxT("\nMAXIMA>")))
        m_inLispMode = true;
      else
        m_inLispMode = false;
    }

    if (ready)
      SetStatusText(_("Ready for user input"), 1);
    m_currentOutput = m_currentOutput.SubString(end + m_promptSuffix.Length(),
                      m_currentOutput.Length());
  }
}

/***
 * This function displays a session file in the console without evaluating inputs.
 */
void wxMaxima::PrintFile()
{
  m_console->Freeze();
  m_console->ClearWindow();
  bool hide = false;
  for (int i=0; i<m_batchFileLines.GetCount(); i++)
  {
    if (m_batchFileLines[i] == wxT("/* [wxMaxima: hide output   ] */"))
      hide = true;

    // Print title
    else if (m_batchFileLines[i] == wxT("/* [wxMaxima: title   start ]"))
    {
      DoRawConsoleAppend(wxEmptyString, MC_TYPE_MAIN_PROMPT, true, hide);
      ++i;
      wxString line;
      while (m_batchFileLines[i] != wxT("   [wxMaxima: title   end   ] */"))
      {
        if (line.Length() == 0)
          line = m_batchFileLines[i];
        else
          line += wxT("\n") + m_batchFileLines[i];
        ++i;
      }
      DoRawConsoleAppend(line, MC_TYPE_TITLE);
      hide = false;
    }

    // Print section
    else if (m_batchFileLines[i] == wxT("/* [wxMaxima: section start ]"))
    {
      DoRawConsoleAppend(wxEmptyString, MC_TYPE_MAIN_PROMPT, true, hide);
      ++i;
      wxString line;
      while (m_batchFileLines[i] != wxT("   [wxMaxima: section end   ] */"))
      {
        if (line.Length() == 0)
          line = m_batchFileLines[i];
        else
          line += wxT("\n") + m_batchFileLines[i];
        ++i;
      }
      DoRawConsoleAppend(line, MC_TYPE_SECTION);
      hide = false;
    }

    // Print comment
    else if (m_batchFileLines[i] == wxT("/* [wxMaxima: comment start ]"))
    {
      DoRawConsoleAppend(wxEmptyString, MC_TYPE_MAIN_PROMPT, true, hide);
      ++i;
      wxString line;
      while (m_batchFileLines[i] != wxT("   [wxMaxima: comment end   ] */"))
      {
        if (line.Length() == 0)
          line = m_batchFileLines[i];
        else
          line += wxT("\n") + m_batchFileLines[i];
        ++i;
      }
      DoRawConsoleAppend(line, MC_TYPE_TEXT);
      hide = false;
    }

    // Print input
    else if (m_batchFileLines[i] == wxT("/* [wxMaxima: input   start ] */"))
    {
      wxString line;
      DoRawConsoleAppend(wxT(">> "), MC_TYPE_MAIN_PROMPT);
      ++i;
      while (m_batchFileLines[i] != wxT("/* [wxMaxima: input   end   ] */"))
      {
        if (line.Length() == 0)
          line = m_batchFileLines[i];
        else
          line += wxT("\n") + m_batchFileLines[i];
        ++i;
      }
      DoRawConsoleAppend(line, MC_TYPE_INPUT, false);
      hide = false;
    }
  }
  m_batchFileLines.Clear();
  m_console->Scroll(0,0);
  m_console->SetSaved(true);

  m_console->Thaw();
}

/*
* This function reads a .wxmx file
*/
void wxMaxima::ReadXmlFile(wxString file)
{
	m_console->Freeze();
	wxBeginBusyCursor();

	wxArrayString xml;
	wxZipEntry* entry;
  wxFFileInputStream in(file);
  wxZipInputStream zip(in);

	for (int i = 0; i < zip.GetTotalEntries(); i++)
	{
    entry = zip.GetNextEntry();
    wxString name = entry->GetName();

    if (name != _T("out")) {
      wxImage img;
      if (img.LoadFile(zip, wxBITMAP_TYPE_PNG))
      {
        wxString basename;
        wxFileName::SplitPath(name, NULL, NULL, &basename, NULL);
        img.SaveFile(wxFileName::GetTempDir() + basename, wxBITMAP_TYPE_PNG);
      }
    }

    else{
      wxTextInputStream in(zip);
      in.SetStringSeparators(wxT("\n"));
      while(!zip.Eof())
        xml.Add(in.ReadWord());
    }

    delete entry;
	}

  for(int i = 2; i < xml.GetCount(); i++ )
  {

    if (xml[i] == _T("<title>")) {
      wxString content = wxEmptyString;
      while (xml[++i] != _T("</title>"))
      {
        if (content.Length() > 0)
          content += wxT("\n");
        content += xml[i];
      }
      DoRawConsoleAppend(wxEmptyString, MC_TYPE_MAIN_PROMPT);
      DoRawConsoleAppend(content, MC_TYPE_TITLE);
    }

    else if (xml[i] == _T("<section>")) {
      wxString content = wxEmptyString;
      while (xml[++i] != _T("</section>"))
      {
        if (content.Length() > 0)
          content += wxT("\n");
        content += xml[i];
      }
      DoRawConsoleAppend(wxEmptyString, MC_TYPE_MAIN_PROMPT);
      DoRawConsoleAppend(content, MC_TYPE_SECTION);
    }

    else if (xml[i] == _T("<comment>")) {
      wxString content = wxEmptyString;
      while (xml[++i] != _T("</comment>"))
      {
        if (content.Length() > 0)
          content += wxT("\n");
        content += xml[i];
      }
      DoRawConsoleAppend(wxEmptyString, MC_TYPE_MAIN_PROMPT);
      DoRawConsoleAppend(content, MC_TYPE_TEXT);
    }

    else if (xml[i] == _T("<input>")) {
      wxString content = wxEmptyString;
      while (xml[++i] != _T("</input>"))
      {
        if (content.Length() > 0)
          content += wxT("\n");
        content += xml[i];
      }
      DoRawConsoleAppend(wxT(">>"), MC_TYPE_MAIN_PROMPT );
      DoRawConsoleAppend(content, MC_TYPE_INPUT, false );
    }

    else if (xml[i] == _T("<image>")) {
      wxString content = wxEmptyString;
      while (xml[++i] != _T("</image>"))
        content += xml[i];
      DoRawConsoleAppend(wxEmptyString, MC_TYPE_MAIN_PROMPT);
      ConsoleAppend(_T("<mth>") + content + _T("</mth>"), MC_TYPE_DEFAULT);
    }

    else if (xml[i] == _T("<mth>")) {
      wxString content = _T("<mth>");
      while (xml[++i] != _T("</mth>"))
        content += xml[i] + _T("\n");
      ConsoleAppend(content + _T("</mth>"), MC_TYPE_DEFAULT );
    }
  }

  m_currentFile = file;

	wxEndBusyCursor();
	m_console->Thaw();

	m_fileSaved = false;
	ResetTitle(true);
}

/***
 * This works only for gcl by default - other lisps have different prompts.
 */
void wxMaxima::ReadLispError()
{
  static const wxString lispError = wxT("dbl:MAXIMA>>"); // gcl
  int end = m_currentOutput.Find(lispError);
  if (end > -1)
  {
    m_readingPrompt = false;
    m_inLispMode = true;
    wxString o = m_currentOutput.Left(end);
    ConsoleAppend(o, MC_TYPE_DEFAULT);
    ConsoleAppend(lispError, MC_TYPE_PROMPT);
    SetStatusText(_("Ready for user input"), 1);
    m_currentOutput = wxEmptyString;
  }
}

#ifndef __WXMSW__
void wxMaxima::ReadProcessOutput()
{
  wxString o;
  while (m_process->IsInputAvailable())
    o += m_input->GetC();

  int st = o.Find(wxT("Maxima"));
  if (st == -1)
    st = 0;

  FirstOutput(wxT("wxMaxima ")
              wxT(VERSION)
              wxT(" http://wxmaxima.sourceforge.net\n") +
              o.SubString(st, o.Length() - 1));

  SetStatusText(_("Ready for user input"), 1);
}
#endif

/***
 * This method is called once when maxima starts. It loads wxmathml.lisp
 * and sets some option variables.
 */
void wxMaxima::SetupVariables()
{
  SendMaxima(wxT(":lisp-quiet (setf *prompt-suffix* \"") +
             m_promptSuffix +
             wxT("\")"));
  SendMaxima(wxT(":lisp-quiet (setf *prompt-prefix* \"") +
             m_promptPrefix +
             wxT("\")"));
  SendMaxima(wxT(":lisp-quiet (setf $IN_NETMATH nil)"));
  SendMaxima(wxT(":lisp-quiet (setf $SHOW_OPENPLOT t)"));
#if defined (__WXMSW__)
  wxString cwd = wxGetCwd();
  cwd.Replace(wxT("\\"), wxT("/"));
  SendMaxima(wxT(":lisp-quiet ($load \"") + cwd + wxT("/data/wxmathml\")"));
#elif defined (__WXMAC__)
  wxString cwd = wxGetCwd();
  cwd = cwd + wxT("/") + wxT(MACPREFIX);
  SendMaxima(wxT(":lisp-quiet ($load \"") + cwd + wxT("wxmathml\")"));
#else
  wxString prefix = wxT(PREFIX);
  SendMaxima(wxT(":lisp-quiet ($load \"") + prefix +
             wxT("/share/wxMaxima/wxmathml\")"));
#endif
}

///--------------------------------------------------------------------------------
///  Getting confuguration
///--------------------------------------------------------------------------------

wxString wxMaxima::GetCommand(bool params)
{
#if defined (__WXMSW__)
  wxConfig *config = (wxConfig *)wxConfig::Get();
  wxString maxima = wxGetCwd();
  wxString parameters;

  maxima.Replace(wxT("wxMaxima"), wxT("bin\\maxima.bat"));

  if (!wxFileExists(maxima))
  {
    config->Read(wxT("maxima"), &maxima);
    if (!wxFileExists(maxima))
    {
      wxMessageBox(_("wxMaxima could not find maxima!\n\n"
                     "Please configure wxMaxima with 'Edit->Configure'.\n"
                     "Then start maxima with 'Maxima->Restart maxima'."), _("Warning"),
                   wxOK | wxICON_EXCLAMATION);
      SetStatusText(_("Please configure wxMaxima with 'Edit->Configure'."));
      return wxEmptyString;
    }
  }

  config->Read(wxT("parameters"), &parameters);
  if (params)
    return wxT("\"") + maxima + wxT("\" ") + parameters;
  return maxima;
#else
  wxConfig *config = (wxConfig *)wxConfig::Get();
  wxString command, parameters;
  bool have_config = config->Read(wxT("maxima"), &command);

  //Fix wrong" maxima=1" paraneter in ~/.wxMaxima if upgrading from 0.7.0a
  if (!have_config || (have_config && command.IsSameAs (wxT("1"))))
  {
#if defined (__WXMAC__)
    command = wxT("/usr/local/bin/maxima");
#else
    command = wxT("maxima");
#endif
    config->Write(wxT("maxima"), command);
  }

  config->Read(wxT("parameters"), &parameters);
  command = wxT("\"") + command + wxT("\" ") + parameters;
  return command;
#endif
}

///--------------------------------------------------------------------------------
///  Tips and help
///--------------------------------------------------------------------------------
/*
void wxMaxima::ShowTip(bool force)
{
  bool ShowTips = true;
  int tipNum = 0;
  wxConfig *config = (wxConfig *)wxConfig::Get();
  config->Read(wxT("ShowTips"), &ShowTips);
  config->Read(wxT("tipNum"), &tipNum);
  if (!ShowTips && !force)
    return ;
  wxString tips = wxT("tips.txt");
#if defined (__WXMSW__)
  wxString prefix = wxGetCwd() + wxT("\\data\\");
#elif defined (__WXMAC__)
  wxString prefix = wxT(MACPREFIX);
  prefix += wxT("/");
#else
  wxString prefix = wxT(PREFIX);
  prefix += wxT("/share/wxMaxima/");
#endif

  tips = prefix + tips;
  if (wxFileExists(tips))
  {
    MyTipProvider *t = new MyTipProvider(tips, tipNum);
    ShowTips = wxShowTip(this, t, ShowTips);
    config->Write(wxT("ShowTips"), ShowTips);
    tipNum = t->GetCurrentTip();
    config->Write(wxT("tipNum"), tipNum);
    config->Flush();
    delete t;
  }
  else
  {
    wxMessageBox(_("wxMaxima could not find tip files."
                   "\n\nPlease check your installation."),
                 _("Error"), wxICON_ERROR | wxOK);
  }
}
*/

wxString wxMaxima::GetHelpFile()
{
#if defined __WXMSW__
  wxString command;
  wxString chm;

  command = GetCommand(false);

  if (command.empty())
    return wxEmptyString;

  command.Replace(wxT("bin\\maxima.bat"), wxT("share\\maxima"));

  chm = wxFindFirstFile(command + wxT("\\*"), wxDIR);

  if (chm.empty())
    return wxEmptyString;

  chm = chm + wxT("\\doc\\chm\\");

  wxString locale = wxGetApp().m_locale.GetCanonicalName().Left(2);

  if (wxFileExists(chm + locale + wxT("\\maxima.chm")))
    return chm + locale + wxT("\\maxima.chm");

  if (wxFileExists(chm + wxT("maxima.chm")))
    return chm + wxT("maxima.chm");

  return wxEmptyString;
#else
  wxString headerFile;
  wxConfig::Get()->Read(wxT("helpFile"), &headerFile);

  if (headerFile.Length() && wxFileExists(headerFile))
    return headerFile;
  else
    headerFile = wxEmptyString;

  wxProcess *process = new wxProcess(this, -1);
  process->Redirect();
  wxString command = GetCommand();
  command += wxT(" -d");
  wxArrayString output;
  wxExecute(command, output, wxEXEC_ASYNC);

  wxString line;
  wxString docdir;
  wxString langsubdir;

  for (unsigned int i=0; i<output.GetCount(); i++)
  {
    line = output[i];
    if (line.StartsWith(wxT("maxima-htmldir")))
      docdir = line.Mid(15);
    else if (line.StartsWith(wxT("maxima-lang-subdir")))
    {
      langsubdir = line.Mid(19);
      if (langsubdir == wxT("NIL"))
        langsubdir = wxEmptyString;
    }
  }

  if (docdir.Length() == 0)
    return wxEmptyString;

  headerFile = docdir + wxT("/");
  if (langsubdir.Length())
    headerFile += langsubdir + wxT("/");

  headerFile += wxT("header.hhp");

  if (!wxFileExists(headerFile))
    headerFile = docdir + wxT("/header.hhp");

  if (wxFileExists(headerFile))
    wxConfig::Get()->Write(wxT("helpFile"), headerFile);

  return headerFile;
#endif
}

void wxMaxima::ShowHelp(wxString keyword)
{
  wxLogNull disableWarnings;

  if (m_helpFile.Length() == 0)
  {
    m_helpFile = GetHelpFile();
    if (m_helpFile.Length() == 0)
    {
      wxMessageBox(_("wxMaxima could not find help files."
                     "\n\nPlease check your installation."),
                   _("Error"), wxICON_ERROR | wxOK);
      return ;
    }

    m_helpCtrl.Initialize(m_helpFile);
  }

  if (keyword == wxT("%"))
    m_helpCtrl.DisplayContents();
  else
    m_helpCtrl.KeywordSearch(keyword, wxHELP_SEARCH_INDEX);
}

///--------------------------------------------------------------------------------
///  Idle event
///--------------------------------------------------------------------------------

/***
 * On idle event we check if the document is saved.
 */
void wxMaxima::OnIdle(wxIdleEvent& event)
{
  ResetTitle(m_console->IsSaved());
  event.Skip();
}

///--------------------------------------------------------------------------------
///  Menu and button events
///--------------------------------------------------------------------------------

void wxMaxima::MenuCommand(wxString cmd)
{
  m_console->SetFocus();
  m_console->OpenHCaret(cmd);
  m_console->AddCellToEvaluationQueue((GroupCell*)m_console->GetActiveCell()->GetParent());
  TryEvaluateNextInQueue();
}

void wxMaxima::DumpProcessOutput()
{
  wxString o;
  while (m_process->IsInputAvailable())
  {
    o += m_input->GetC();
  }

  wxMessageBox(o, wxT("Process output (stdout)"));

  o = wxEmptyString;
  wxInputStream *error = m_process->GetErrorStream();
  while (m_process->IsErrorAvailable())
  {
    o += error->GetC();
  }

  wxMessageBox(o, wxT("Process output (stderr)"));

}

///--------------------------------------------------------------------------------
///  Menu and button events
///--------------------------------------------------------------------------------

#if WXM_PRINT

void wxMaxima::PrintMenu(wxCommandEvent& event)
{
  if (!m_supportPrinting)
    return ;
  switch (event.GetId())
  {
  case wxID_PRINT:
#if defined (__WXMSW__) || defined (__WXGTK20__) || defined (__WXMAC__)
  case tb_print:
#endif
    {
      wxPrintDialogData printDialogData(*m_printData);
      wxPrinter printer(&printDialogData);
      wxString title(_("wxMaxima document"));
      if (m_currentFile.Length())
        wxFileName::SplitPath(m_currentFile, NULL, NULL, &title, NULL);
      MathPrintout printout(title);
      MathCell* copy = m_console->CopyTree();
      printout.SetData(copy);
      /*
            if (!printer.Print(this, &printout, true)) {
              if (wxPrinter::GetLastError() == wxPRINTER_ERROR)
                wxMessageBox(_("There was a problem printing.\n"
                               "Perhaps your current printer is not set correctly?"),
                             _("Printing"), wxOK);
            }
            else {
              (*m_printData) = printer.GetPrintDialogData().GetPrintData();
            }
      */
      if (printer.Print(this, &printout, true))
        (*m_printData) = printer.GetPrintDialogData().GetPrintData();
      break;
    }
  }
}

#endif

void wxMaxima::UpdateMenus(wxUpdateUIEvent& event)
{
  wxMenuBar* menubar = GetMenuBar();
  menubar->Enable(menu_copy_from_console, m_console->CanCopy(true));
  menubar->Enable(menu_cut, m_console->CanCut());
  menubar->Enable(menu_copy_tex_from_console, m_console->CanCopy());
  menubar->Enable(menu_copy_input_from_console, m_console->CanCopy());
  menubar->Enable(menu_cut_input_from_console, m_console->CanCopy());
  menubar->Enable(menu_copy_as_bitmap, m_console->CanCopy());
  menubar->Enable(menu_copy_to_file, m_console->CanCopy());
  menubar->Enable(menu_select_all, m_console->GetTree() != NULL);
  menubar->Enable(menu_undo, m_console->CanUndo());
  menubar->Enable(menu_delete_selection, m_console->CanDeleteSelection());
  if (m_console->GetSelectionStart() != NULL)
    menubar->Enable(menu_evaluate, m_console->GetSelectionStart()->GetType() == MC_TYPE_GROUP);
  else
    menubar->Enable(menu_evaluate, m_console->GetActiveCell() != NULL);

  menubar->Enable(menu_evaluate_all, m_console->GetTree() != NULL);
  menubar->Enable(menu_save_id, !m_fileSaved);
#if WXM_PRINT
  if (m_console->GetTree() != NULL && m_supportPrinting)
    menubar->Enable(wxID_PRINT, true);
  else
    menubar->Enable(wxID_PRINT, false);
#endif
  int fontSize = 12;
  wxConfig::Get()->Read(wxT("fontSize"), &fontSize);
  if (fontSize < 20)
    menubar->Enable(menu_inc_fontsize, true);
  else
    menubar->Enable(menu_inc_fontsize, false);
  if (fontSize > 8)
    menubar->Enable(menu_dec_fontsize, true);
  else
    menubar->Enable(menu_dec_fontsize, false);
}

#if defined (__WXMSW__) || defined (__WXGTK20__) || defined(__WXMAC__)

void wxMaxima::UpdateToolBar(wxUpdateUIEvent& event)
{
  wxToolBar * toolbar = GetToolBar();
  toolbar->EnableTool(tb_copy,  m_console->CanCopy(true));
  toolbar->EnableTool(tb_delete, m_console->CanDeleteSelection());
  toolbar->EnableTool(tb_save, !m_fileSaved);
  if (m_pid > 0)
    toolbar->EnableTool(tb_interrupt, true);
  else
    toolbar->EnableTool(tb_interrupt, false);
#if WXM_PRINT
  if (m_console->GetTree() != NULL && m_supportPrinting)
    toolbar->EnableTool(tb_print, true);
  else
    toolbar->EnableTool(tb_print, false);
#endif
  if (m_console->CanAnimate() && !m_console->AnimationRunning())
    toolbar->EnableTool(tb_animation_start, true);
  else
    toolbar->EnableTool(tb_animation_start, false);
  if (m_console->CanAnimate() && m_console->AnimationRunning())
    toolbar->EnableTool(tb_animation_stop, true);
  else
    toolbar->EnableTool(tb_animation_stop, false);
}

#endif

wxString wxMaxima::GetDefaultEntry()
{
  if (m_console->CanCopy(true))
    return m_console->GetString();
  return wxT("%");
}

void wxMaxima::OpenFile(wxString file, wxString cmd)
{
  if (file.Length())
  {
    AddRecentDocument(file);

    m_lastPath = wxPathOnly(file);
    wxString unixFilename(file);
#if defined __WXMSW__
    unixFilename.Replace(wxT("\\"), wxT("/"));
#endif

    if (cmd.Length() > 0)
    {
      MenuCommand(cmd + wxT("(\"") + unixFilename + wxT("\")$"));
    }

    else if (file.Right(4) == wxT(".wxm"))
    {
      if (!ReadBatchFile(file))
        MenuCommand(wxT("batch(\"") + unixFilename + wxT("\")$"));
      else
        StartMaxima();
    }

    else if (file.Right(5) == wxT(".wxmx"))
    {
      m_console->ClearWindow();
      ReadXmlFile(file);
      m_console->SetSaved(true);
    }

    else if (file.Right(4) == wxT(".dem"))
      MenuCommand(wxT("demo(\"") + unixFilename + wxT("\")$"));

    else
      MenuCommand(wxT("load(\"") + unixFilename + wxT("\")$"));
  }
}

void wxMaxima::FileMenu(wxCommandEvent& event)
{
  wxString expr = GetDefaultEntry();
  wxString cmd;
#if defined __WXMSW__
  wxString b = wxT("\\");
  wxString f = wxT("/");
#endif

  switch (event.GetId())
  {
#if defined (__WXMSW__) || defined (__WXGTK20__) || defined (__WXMAC__)
  case tb_open:
#endif
  case menu_open_id:
    {
      if (!m_fileSaved) {
        int close = wxMessageBox(_("Document not saved!\n\nClose current document and lose all changes?"),
                                 _("Close document?"),
                                 wxOK|wxCANCEL);
        if (close != wxOK)
          return;
      }

      wxString file = wxFileSelector(_("Open"), m_lastPath,
                                     wxEmptyString, wxEmptyString,
                                     _("wxMaxima document (*.wxm, *.wxmx)|*.wxm;*.wxmx"),
                                     wxFD_OPEN);
      OpenFile(file);
    }
    break;

  case menu_save_as_id:
    {
      wxString file(_("untitled"));
      if (m_currentFile.Length() >0)
        wxFileName::SplitPath(m_currentFile, NULL, NULL, &file, NULL);
      file = wxFileSelector(_("Save As"), m_lastPath,
                            file + wxT(".wxm"), wxT("wxm"),
                            _("wxMaxima document (*.wxm)|*.wxm|"
                              "wxMaxima xml document (*.wxmx)|*.wxmx|"
                              "Maxima batch file (*.mac)|*.mac|"
                              "All|*"),
                            wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
      if (file.Length())
      {
        if (file.Right(4) != wxT(".wxm") && file.Right(4) != wxT(".mac") &&
            file.Right(5) != wxT(".wxmx"))
          file = file + wxT(".wxm");

        m_lastPath = wxPathOnly(file);
        m_currentFile = file;
        if (file.Right(5) == wxT(".wxmx"))
          m_console->ExportToWXMX(file);
        else
          m_console->ExportToMAC(file);

        AddRecentDocument(file);
      }
    }
    break;

#if defined (__WXMSW__) || defined (__WXGTK20__) || defined (__WXMAC__)
  case tb_save:
#endif
  case menu_save_id:
    {
      wxString file = m_currentFile;
      if (m_currentFile.Length() == 0)
      {
        file = wxFileSelector(_("Save As"), m_lastPath,
                              _("untitled.wxm"), wxT("wxm"),
                              _("wxMaxima document (*.wxm)|*.wxm|"
                                "wxMaxima xml document (*.wxmx)|*.wxmx|"
                                "Maxima batch file (*.mac)|*.mac|"
                                "All|*"),
                              wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
      }
      if (file.Length())
      {
        if (file.Right(4) != wxT(".wxm") && file.Right(4) != wxT(".mac")
            && file.Right(5) != wxT(".wxmx"))
          file = file + wxT(".wxm");

        m_currentFile = file;
        m_lastPath = wxPathOnly(file);
        if (file.Right(5) == wxT(".wxmx"))
          m_console->ExportToWXMX(file);
        else
          m_console->ExportToMAC(file);

        AddRecentDocument(file);
      }
    }
    break;

  case menu_export_html:
    {
      wxString file(_("untitled"));
      if (m_currentFile.Length() >0)
        wxFileName::SplitPath(m_currentFile, NULL, NULL, &file, NULL);
      file = wxFileSelector(_("Export"), m_lastPath,
                            file + wxT(".html"), wxT("html"),
                            _("HTML file (*.html)|*.html|"
                              "pdfLaTeX file (*.tex)|*.tex|"
                              "All|*"),
                            wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
      if (file.Length())
      {
        m_lastPath = wxPathOnly(file);
        if (file.Right(5) != wxT(".html") && file.Right(4) != wxT(".tex"))
          file = file + wxT(".html");

        if (file.Right(4) == wxT(".tex")) {
          if (!m_console->ExportToTeX(file))
            wxMessageBox(_("Exporting to TeX failed!"), _("Error!"),
                         wxOK);
        }
        else {
          if (!m_console->ExportToHTML(file))
            wxMessageBox(_("Exporting to HTML failed!"), _("Error!"),
                         wxOK);
        }
      }
    }
    break;

  case menu_load_id:
    {
      wxString file = wxFileSelector(_("Load Package"), m_lastPath,
                                     wxEmptyString, wxEmptyString,
                                     _("Maxima package (*.mac)|*.mac|"
                                       "Lisp package (*.lisp)|*.lisp|All|*"),
                                     wxFD_OPEN);
      OpenFile(file, wxT("load"));
    }
    break;

  case menu_batch_id:
    {
      wxString file = wxFileSelector(_("Batch File"), m_lastPath,
                                     wxEmptyString, wxEmptyString,
                                     _("Maxima package (*.mac)|*.mac"),
                                     wxFD_OPEN);
      OpenFile(file, wxT("batch"));
    }
    break;

  case wxID_EXIT:
    Close();
    break;

#if defined (__WXMSW__) || defined (__WXGTK20__) || defined (__WXMAC__)
  case tb_animation_start:
    if (m_console->CanAnimate() && !m_console->AnimationRunning())
      m_console->Animate(true);
    break;

  case tb_animation_stop:
    if (m_console->CanAnimate() && m_console->AnimationRunning())
      m_console->Animate(false);
    break;
#endif

  default:
    break;
  }
}

void wxMaxima::EditMenu(wxCommandEvent& event)
{
  switch (event.GetId())
  {
  case wxID_PREFERENCES:
#if defined (__WXMSW__) || defined (__WXGTK20__) || defined (__WXMAC__)
  case tb_pref:
#endif
    {
      Config *configW = new Config(this, -1, _("wxMaxima configuration"));
      configW->Centre(wxBOTH);
      if (configW->ShowModal() == wxID_OK)
      {
        bool match = true;
        wxConfig::Get()->Read(wxT("matchParens"), &match);
        m_console->RecalculateForce();
        m_console->Refresh();
      }
      configW->Destroy();
    }
    break;
#if defined (__WXMSW__) || defined (__WXGTK20__) || defined (__WXMAC__)
  case tb_copy:
#endif
  case menu_copy_from_console:
    if (m_console->CanCopy(true))
      m_console->Copy();
    break;
  case menu_copy_input_from_console:
    if (m_console->CanCopy())
      m_console->CopyCells();
    break;
  case menu_cut_input_from_console:
    if (m_console->CanCopy())
      m_console->CopyCells();
    if (m_console->CanDeleteSelection())
      m_console->DeleteSelection();
    break;
  case menu_cut:
    if (m_console->CanCut())
      m_console->CutToClipboard();
    break;
  case menu_select_all:
    m_console->SelectAll();
    break;
  case menu_paste:
    if (m_console->CanPaste())
      m_console->PasteFromClipboard();
    break;
  case menu_undo:
    if (m_console->CanUndo())
      m_console->Undo();
    break;
  case menu_copy_tex_from_console:
    if (m_console->CanCopy())
      m_console->CopyTeX();
    break;
  case menu_copy_as_bitmap:
    if (m_console->CanCopy())
      m_console->CopyBitmap();
    break;
  case menu_copy_to_file:
    {
      wxString file = wxFileSelector(_("Save Selection to Image"), m_lastPath,
                                     wxT("image.png"), wxT("png"),
                                     _("PNG image (*.png)|*.png|"
                                       "JPEG image (*.jpg)|*.jpg|"
                                       "Windows bitmap (*.bmp)|*.bmp|"
                                       "X pixmap (*.xpm)|*.xpm"),
                                     wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
      if (file.Length())
      {
        m_console->CopyToFile(file);
        m_lastPath = wxPathOnly(file);
      }
    }
    break;
#if defined (__WXMSW__) || defined (__WXGTK20__) || defined (__WXMAC__)
  case tb_delete:
#endif
  case menu_delete_selection:
  case popid_delete:
    if (m_console->CanDeleteSelection())
    {
      m_console->DeleteSelection();
      m_console->Recalculate();
      m_console->Refresh();
      return;
    }
    break;
  case menu_inc_fontsize:
    {
      int fontSize = 12;
      wxConfig::Get()->Read(wxT("fontSize"), &fontSize);
      if (fontSize < MC_MAX_SIZE)
      {
        fontSize++;
        wxConfig::Get()->Write(wxT("fontSize"), fontSize);
        m_console->RecalculateForce();
        m_console->Refresh();
      }
    }
    break;
  case menu_dec_fontsize:
    {
      int fontSize = 12;
      wxConfig::Get()->Read(wxT("fontSize"), &fontSize);
      if (fontSize > MC_MIN_SIZE)
      {
        fontSize--;
        wxConfig::Get()->Write(wxT("fontSize"), fontSize);
        m_console->RecalculateForce();
        m_console->Refresh();
      }
    }
    break;
  case menu_fullscreen:
    {
      ShowFullScreen( !IsFullScreen() );
    }
    break;
  case menu_remove_output:
    m_console->RemoveAllOutput();
    break;
  case menu_paste_input:
    {
      if (wxTheClipboard->Open()) {
        if (wxTheClipboard->IsSupported( wxDF_TEXT ))
        {
          wxTextDataObject data;
          wxTheClipboard->GetData(data);
          wxString inputs(data.GetText());
          wxStringTokenizer lines(inputs, wxT("\n"));
          wxString input, line;
          wxArrayString inp;

          // Read the content from clipboard
          while (lines.HasMoreTokens())
          {
            // Go to the begining of the cell
            do {
              line = lines.GetNextToken();
            } while (lines.HasMoreTokens() &&
                     line != wxT("/* [wxMaxima: input   start ] */") &&
                     line != wxT("/* [wxMaxima: comment start ]") &&
                     line != wxT("/* [wxMaxima: section start ]") &&
                     line != wxT("/* [wxMaxima: title   start ]"));

            // Read the cell content
            do {
              line = lines.GetNextToken();
              if (line == wxT("/* [wxMaxima: input   end   ] */"))
              {
                inp.Add(wxT("input"));
                inp.Add(input);
                input = wxEmptyString;
              }
              else if (line == wxT("   [wxMaxima: comment end   ] */"))
              {
                inp.Add(wxT("comment"));
                inp.Add(input);
                input = wxEmptyString;
              }
              else if (line == wxT("   [wxMaxima: section end   ] */"))
              {
                inp.Add(wxT("section"));
                inp.Add(input);
                input = wxEmptyString;
              }
              else if (line == wxT("   [wxMaxima: title   end   ] */"))
              {
                inp.Add(wxT("title"));
                inp.Add(input);
                input = wxEmptyString;
              }
              else
              {
                if (input.Length()>0)
                  input = input + wxT("\n") + line;
                else
                  input = line;
              }
            } while (lines.HasMoreTokens() &&
                     line != wxT("/* [wxMaxima: input   end   ] */") &&
                     line != wxT("   [wxMaxima: comment end   ] */") &&
                     line != wxT("   [wxMaxima: section end   ] */") &&
                     line != wxT("   [wxMaxima: title   end   ] */"));
          }

          // Paste the content into the document
          m_console->Freeze();
          for (int i=0; i<inp.Count(); i = i+2)
          {
            if (inp[i] == wxT("input"))
              m_console->OpenHCaret(inp[i+1]);
            else if (inp[i] == wxT("comment"))
              m_console->OpenHCaret(inp[i+1], GC_TYPE_TEXT);
            else if (inp[i] == wxT("section"))
              m_console->OpenHCaret(inp[i+1], GC_TYPE_SECTION);
            else if (inp[i] == wxT("title"))
              m_console->OpenHCaret(inp[i+1], GC_TYPE_TITLE);
          }
          m_console->Thaw();

        }
        wxTheClipboard->Close();
      }
    }
    return;
    break;
  }
}

void wxMaxima::MaximaMenu(wxCommandEvent& event)
{
  wxString expr = GetDefaultEntry();
  wxString cmd;
  wxString b = wxT("\\");
  wxString f = wxT("/");
  switch (event.GetId())
  {
  case menu_restart_id:
    m_closing = true;
    m_console->ClearEvaluationQueue();
    m_console->ResetInputPrompts();
    StartMaxima();
    break;
  case menu_soft_restart:
    MenuCommand(wxT("kill(all);"));
    break;
  case menu_functions:
    MenuCommand(wxT("functions;"));
    break;
  case menu_variables:
    MenuCommand(wxT("values;"));
    break;
  case menu_display:
    {
      wxString choices[] =
        {
          wxT("xml"), wxT("ascii"), wxT("none")
        };
      wxString choice = wxGetSingleChoice(
                          _("Select math display algorithm"),
                          _("Display algorithm"),
                          3,
                          choices,
                          this
                        );
      if (choice.Length())
      {
        cmd = wxT("set_display('") + choice + wxT(")$");
        MenuCommand(cmd);
      }
    }
    break;
  case menu_texform:
    cmd = wxT("tex(") + expr + wxT(")$");
    MenuCommand(cmd);
    break;
  case menu_time:
    cmd = wxT("if showtime#false then showtime:false else showtime:all$");
    MenuCommand(cmd);
    break;
  case menu_fun_def:
    cmd = GetTextFromUser(_("Show the definition of function:"),
                          _("Function"), wxEmptyString, this);
    if (cmd.Length())
    {
      cmd = wxT("fundef(") + cmd + wxT(");");
      MenuCommand(cmd);
    }
    break;
  case menu_add_path:
    {
      if (m_lastPath.Length() == 0)
        m_lastPath = wxGetHomeDir();
      wxString dir = wxDirSelector(_("Add dir to path:"), m_lastPath);
      if (dir.Length())
      {
        m_lastPath = dir;
#if defined (__WXMSW__)
        dir.Replace(wxT("\\"), wxT("/"));
#endif
        cmd = wxT("file_search_maxima : cons(sconcat(\"") + dir +
              wxT("/###.{lisp,mac,mc}\"), file_search_maxima)$");
        MenuCommand(cmd);
      }
    }
    break;
  case menu_evaluate_all:
    m_console->AddDocumentToEvaluationQueue();
    TryEvaluateNextInQueue();
    break;
  case menu_clear_var:
    cmd = GetTextFromUser(_("Delete variable(s):"), _("Delete"),
                          wxT("all"), this);
    if (cmd.Length())
    {
      cmd = wxT("remvalue(") + cmd + wxT(");");
      MenuCommand(cmd);
    }
    break;
  case menu_clear_fun:
    cmd = GetTextFromUser(_("Delete function(s):"), _("Delete"),
                          wxT("all"), this);
    if (cmd.Length())
    {
      cmd = wxT("remfunction(") + cmd + wxT(");");
      MenuCommand(cmd);
    }
    break;
  case menu_subst:
  case button_subst:
    {
      SubstituteWiz *wiz = new SubstituteWiz(this, -1, _("Substitute"));
      wiz->SetValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wiz->GetValue();
        MenuCommand(val);
      }
      wiz->Destroy();
    }
    break;
  default:
    break;
  }
}

void wxMaxima::EquationsMenu(wxCommandEvent& event)
{
  wxString expr = GetDefaultEntry();
  wxString cmd;
  switch (event.GetId())
  {
  case menu_allroots:
    cmd = wxT("allroots(") + expr + wxT(");");
    MenuCommand(cmd);
    break;
  case menu_realroots:
    cmd = wxT("realroots(") + expr + wxT(");");
    MenuCommand(cmd);
    break;
  case button_solve:
  case menu_solve:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Equation(s):"), _("Variable(s):"),
                                 expr, wxT("x"), this, -1, _("Solve"), true);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("solve([") + wiz->GetValue1() + wxT("], [") +
              wiz->GetValue2() + wxT("]);");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_solve_num:
    {
      if (expr.StartsWith(wxT("%")))
        expr = wxT("''(") + expr + wxT(")");
      Gen4Wiz *wiz = new Gen4Wiz(_("Equation:"), _("Variable:"),
                                 _("Lower bound:"), _("Upper bound:"),
                                 expr, wxT("x"), wxT("-1"), wxT("1"),
                                 this, -1, _("Find root"), true);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("find_root(") + wiz->GetValue1() + wxT(", ") +
              wiz->GetValue2() + wxT(", ") +
              wiz->GetValue3() + wxT(", ") +
              wiz->GetValue4() + wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
    break;
  case button_solve_ode:
  case menu_solve_ode:
    {
      Gen3Wiz *wiz = new Gen3Wiz(_("Equation:"), _("Function:"), _("Variable:"),
                                 expr, wxT("y"), wxT("x"),
                                 this, -1, _("Solve ODE"));
      wiz->SetValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wxT("ode2(") + wiz->GetValue1() + wxT(", ") +
                       wiz->GetValue2() + wxT(", ") + wiz->GetValue3() + wxT(");");
        MenuCommand(val);
      }
      wiz->Destroy();
    }
    break;
  case menu_ivp_1:
    {
      Gen3Wiz *wiz = new Gen3Wiz(_("Solution:"), _("Point:"), _("Value:"),
                                 expr, wxT("x="), wxT("y="),
                                 this, -1, _("IC1"), true);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wxT("ic1(") + wiz->GetValue1() + wxT(", ") +
                       wiz->GetValue2() + wxT(", ") + wiz->GetValue3() + wxT(");");
        MenuCommand(val);
      }
      wiz->Destroy();
    }
    break;
  case menu_ivp_2:
    {
      Gen4Wiz *wiz = new Gen4Wiz(_("Solution:"), _("Point:"),
                                 _("Value:"), _("Derivative:"),
                                 expr, wxT("x="), wxT("y="), wxT("'diff(y,x)="),
                                 this, -1, _("IC2"), true);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wxT("ic2(") + wiz->GetValue1() + wxT(", ") +
                       wiz->GetValue2() + wxT(", ") + wiz->GetValue3() +
                       wxT(", ") + wiz->GetValue4() + wxT(");");
        MenuCommand(val);
      }
      wiz->Destroy();
    }
    break;
  case menu_bvp:
    {
      BC2Wiz *wiz = new BC2Wiz(this, -1, _("BC2"));
      wiz->SetValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wiz->GetValue();
        MenuCommand(val);
      }
      wiz->Destroy();
    }
    break;
  case menu_eliminate:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Equations:"),
                                 _("Variables:"), expr, wxEmptyString,
                                 this, -1, _("Eliminate"), true);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("eliminate([") + wiz->GetValue1() + wxT("],[")
              + wiz->GetValue2() + wxT("]);");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_solve_algsys:
    {
      wxString sz = GetTextFromUser(_("Number of equations:"),
                                    _("Solve algebraic system"),
                                    wxT("3"), this);
      if (sz.Length() == 0)
        return ;
      long isz;
      if (!sz.ToLong(&isz) || isz <= 0)
      {
        wxMessageBox(_("Not a valid number of equations!"), _("Error!"),
                     wxOK | wxICON_ERROR);
        return ;
      }
      SysWiz *wiz = new SysWiz(this, -1, _("Solve algebraic system"), isz);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("algsys") + wiz->GetValue();
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_solve_lin:
    {
      wxString sz = GetTextFromUser(_("Number of equations:"),
                                    _("Solve linear system"),
                                    wxT("3"), this);
      if (sz.Length() == 0)
        return ;
      long isz;
      if (!sz.ToLong(&isz) || isz <= 0)
      {
        wxMessageBox(_("Not a valid number of equations!"), _("Error!"),
                     wxOK | wxICON_ERROR);
        return ;
      }
      SysWiz *wiz = new SysWiz(this, -1, _("Solve linear system"), isz);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("linsolve") + wiz->GetValue();
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_solve_de:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Equation(s):"), _("Function(s):"),
                                 expr, wxT("y(x)"),
                                 this, -1, _("Solve ODE"));
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("desolve([") + wiz->GetValue1() + wxT("],[")
              + wiz->GetValue2() + wxT("]);");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_atvalue:
    {
      Gen3Wiz *wiz = new Gen3Wiz(_("Expression:"), _("Point:"),
                                 _("Value:"), expr, wxT("x=0"), wxT("0"),
                                 this, -1, _("At value"));
      wiz->SetValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wxT("atvalue(") + wiz->GetValue1() + wxT(", ")
                       + wiz->GetValue2() +
                       wxT(", ") + wiz->GetValue3() + wxT(");");
        MenuCommand(val);
      }
      wiz->Destroy();
    }
    break;
  default:
    break;
  }
}

void wxMaxima::AlgebraMenu(wxCommandEvent& event)
{
  wxString expr = GetDefaultEntry();
  wxString cmd;
  switch (event.GetId())
  {
  case menu_invert_mat:
    cmd = wxT("invert(") + expr + wxT(");");
    MenuCommand(cmd);
    break;
  case menu_determinant:
    cmd = wxT("determinant(") + expr + wxT(");");
    MenuCommand(cmd);
    break;
  case menu_eigen:
    cmd = wxT("eigenvalues(") + expr + wxT(");");
    MenuCommand(cmd);
    break;
  case menu_eigvect:
    cmd = wxT("eigenvectors(") + expr + wxT(");");
    MenuCommand(cmd);
    break;
  case menu_adjoint_mat:
    cmd = wxT("adjoint(") + expr + wxT(");");
    MenuCommand(cmd);
    break;
  case menu_transpose:
    cmd = wxT("transpose(") + expr + wxT(");");
    MenuCommand(cmd);
    break;
  case menu_map_mat:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Function:"), _("Matrix:"),
                                 wxEmptyString, expr,
                                 this, -1, _("Matrix map"));
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("matrixmap(") + wiz->GetValue1() + wxT(", ")
              + wiz->GetValue2() + wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_enter_mat:
    {
      MatDim *wiz = new MatDim(this, -1, _("Matrix"));
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        if (wiz->GetValue0() != wxEmptyString)
          cmd = wiz->GetValue0() + wxT(": ");
        long w, h;
        int type = wiz->GetMatrixType();
        if (!(wiz->GetValue2()).ToLong(&w) ||
                !(wiz->GetValue1()).ToLong(&h) ||
                w <= 0 || h <= 0)
        {
          wxMessageBox(_("Not a valid matrix dimension!"), _("Error!"),
                       wxOK | wxICON_ERROR);
          return ;
        }
        if (w != h)
          type = MATRIX_GENERAL;
        MatWiz *mwiz = new MatWiz(this, -1, _("Enter matrix"),
                                  type, w, h);
        mwiz->Centre(wxBOTH);
        if (mwiz->ShowModal() == wxID_OK)
        {
          cmd += mwiz->GetValue();
          MenuCommand(cmd);
        }
        mwiz->Destroy();
      }
      wiz->Destroy();
    }
    break;
  case menu_cpoly:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Matrix:"), _("Variable:"),
                                 expr, wxT("x"),
                                 this, -1, _("Char poly"));
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("charpoly(") + wiz->GetValue1() + wxT(", ")
              + wiz->GetValue2() + wxT("), expand;");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_gen_mat:
    {
      Gen3Wiz *wiz = new Gen3Wiz(_("Array:"), _("Width:"), _("Height:"),
                                 expr, wxT("3"), wxT("3"),
                                 this, -1, _("Generate Matrix"));
      wiz->SetValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wxT("genmatrix(") + wiz->GetValue1() +
                       wxT(", ") + wiz->GetValue2() +
                       wxT(", ") + wiz->GetValue3() + wxT(");");
        MenuCommand(val);
      }
      wiz->Destroy();
    }
    break;
  case button_map:
  case menu_map:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Function:"), _("List:"),
                                 wxEmptyString, expr,
                                 this, -1, _("Map"));
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("map(") + wiz->GetValue1() + wxT(", ") + wiz->GetValue2() +
              wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_make_list:
    {
      Gen4Wiz *wiz = new Gen4Wiz(_("Expression:"), _("Variable:"),
                                 _("From:"), _("To:"),
                                 expr, wxT("k"), wxT("1"), wxT("10"),
                                 this, -1, _("Make list"));
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("makelist(") + wiz->GetValue1() + wxT(", ") +
              wiz->GetValue2() + wxT(", ") +
              wiz->GetValue3() + wxT(", ") +
              wiz->GetValue4() + wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_apply:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Function:"), _("List:"),
                                 wxT("\"+\""), expr,
                                 this, -1, _("Apply"), true);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("apply(") + wiz->GetValue1() + wxT(", ")
              + wiz->GetValue2() + wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
    break;
  default:
    break;
  }
}

void wxMaxima::SimplifyMenu(wxCommandEvent& event)
{
  wxString expr = GetDefaultEntry();
  wxString cmd;
  switch (event.GetId())
  {
  case menu_nouns:
    cmd = wxT("ev(") + expr + wxT(", nouns);");
    MenuCommand(cmd);
    break;
  case button_ratsimp:
  case menu_ratsimp:
    cmd = wxT("ratsimp(") + expr + wxT(");");
    MenuCommand(cmd);
    break;
  case button_radcan:
  case menu_radsimp:
    cmd = wxT("radcan(") + expr + wxT(");");
    MenuCommand(cmd);
    break;
  case menu_to_fact:
    cmd = wxT("makefact(") + expr + wxT(");");
    MenuCommand(cmd);
    break;
  case menu_to_gamma:
    cmd = wxT("makegamma(") + expr + wxT(");");
    MenuCommand(cmd);
    break;
  case menu_factcomb:
    cmd = wxT("factcomb(") + expr + wxT(");");
    MenuCommand(cmd);
    break;
  case menu_factsimp:
    cmd = wxT("minfactorial(") + expr + wxT(");");
    MenuCommand(cmd);
    break;
  case menu_logcontract:
    cmd = wxT("logcontract(") + expr + wxT(");");
    MenuCommand(cmd);
    break;
  case menu_logexpand:
    cmd = expr + wxT(", logexpand=super;");
    MenuCommand(cmd);
    break;
  case button_expand:
  case menu_expand:
    cmd = wxT("expand(") + expr + wxT(");");
    MenuCommand(cmd);
    break;
  case button_factor:
  case menu_factor:
    cmd = wxT("factor(") + expr + wxT(");");
    MenuCommand(cmd);
    break;
  case menu_gfactor:
    cmd = wxT("gfactor(") + expr + wxT(");");
    MenuCommand(cmd);
    break;
  case button_trigreduce:
  case menu_trigreduce:
    cmd = wxT("trigreduce(") + expr + wxT(");");
    MenuCommand(cmd);
    break;
  case button_trigsimp:
  case menu_trigsimp:
    cmd = wxT("trigsimp(") + expr + wxT(");");
    MenuCommand(cmd);
    break;
  case button_trigexpand:
  case menu_trigexpand:
    cmd = wxT("trigexpand(") + expr + wxT(");");
    MenuCommand(cmd);
    break;
  case menu_trigrat:
    cmd = wxT("trigrat(") + expr + wxT(");");
    MenuCommand(cmd);
    break;
  case button_rectform:
  case menu_rectform:
    cmd = wxT("rectform(") + expr + wxT(");");
    MenuCommand(cmd);
    break;
  case menu_polarform:
    cmd = wxT("polarform(") + expr + wxT(");");
    MenuCommand(cmd);
    break;
  case menu_demoivre:
    cmd = wxT("demoivre(") + expr + wxT(");");
    MenuCommand(cmd);
    break;
  case menu_exponentialize:
    cmd = wxT("exponentialize(") + expr + wxT(");");
    MenuCommand(cmd);
    break;
  case menu_realpart:
    cmd = wxT("realpart(") + expr + wxT(");");
    MenuCommand(cmd);
    break;
  case menu_imagpart:
    cmd = wxT("imagpart(") + expr + wxT(");");
    MenuCommand(cmd);
    break;
  case menu_talg:
    cmd = wxT("algebraic : not(algebraic);");
    MenuCommand(cmd);
    break;
  case menu_tellrat:
    cmd = GetTextFromUser(_("Enter an equation for rational simplification:"),
                          _("Tellrat"), wxEmptyString, this);
    if (cmd.Length())
    {
      cmd = wxT("tellrat(") + cmd + wxT(");");
      MenuCommand(cmd);
    }
    break;
  case menu_modulus:
    cmd = GetTextFromUser(_("Calculate modulus:"),
                          _("Modulus"), wxT("false"), this);
    if (cmd.Length())
    {
      cmd = wxT("modulus : ") + cmd + wxT(";");
      MenuCommand(cmd);
    }
    break;
  default:
    break;
  }
}

void wxMaxima::CalculusMenu(wxCommandEvent& event)
{
  wxString expr = GetDefaultEntry();
  wxString cmd;
  switch (event.GetId())
  {
  case menu_change_var:
    {
      Gen4Wiz *wiz = new Gen4Wiz(_("Integral/Sum:"), _("Old variable:"),
                                 _("New variable:"), _("Equation:"),
                                 expr, wxT("x"), wxT("y"), wxT("y=x"),
                                 this, -1, _("Change variable"), true);
      wiz->SetValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wxT("changevar(") + wiz->GetValue1() + wxT(", ") +
                       wiz->GetValue4() + wxT(", ") + wiz->GetValue3() + wxT(", ") +
                       wiz->GetValue2() + wxT(");");
        MenuCommand(val);
      }
      wiz->Destroy();
    }
    break;
  case menu_pade:
    {
      Gen3Wiz *wiz = new Gen3Wiz(_("Taylor series:"), _("Num. deg:"),
                                 _("Denom. deg:"), expr, wxT("4"), wxT("4"),
                                 this, -1, _("Pade approximation"));
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wxT("pade(") + wiz->GetValue1() + wxT(", ") +
                       wiz->GetValue2() + wxT(", ") + wiz->GetValue3() + wxT(");");
        MenuCommand(val);
      }
      wiz->Destroy();
    }
    break;
  case menu_continued_fraction:
    cmd += wxT("cfdisrep(cf(") + expr + wxT("));");
    MenuCommand(cmd);
    break;
  case menu_lcm:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Polynomial 1:"), _("Polynomial 2:"),
                                 wxEmptyString, wxEmptyString,
                                 this, -1, _("LCM"), true);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("lcm(") + wiz->GetValue1() + wxT(", ")
              + wiz->GetValue2() + wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_gcd:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Polynomial 1:"), _("Polynomial 2:"),
                                 wxEmptyString, wxEmptyString,
                                 this, -1, _("GCD"), true);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("gcd(") + wiz->GetValue1() + wxT(", ")
              + wiz->GetValue2() + wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_divide:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Polynomial 1:"), _("Polynomial 2:"),
                                 expr, wxEmptyString,
                                 this, -1, _("Divide"), true);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("divide(") + wiz->GetValue1() + wxT(", ") +
              wiz->GetValue2() + wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_partfrac:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Expression:"), _("Variable:"),
                                 expr, wxT("n"),
                                 this, -1, _("Partial fractions"));
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("partfrac(") + wiz->GetValue1() + wxT(", ")
              + wiz->GetValue2() + wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_risch:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Expression:"), _("Variable:"),
                                 expr, wxT("x"),
                                 this, -1, _("Integrate (risch)"));
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("risch(") + wiz->GetValue1() + wxT(", ")
              + wiz->GetValue2() + wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
    break;
  case button_integrate:
  case menu_integrate:
    {
      IntegrateWiz *wiz = new IntegrateWiz(this, -1, _("Integrate"));
      wiz->SetValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wiz->GetValue();
        MenuCommand(val);
      }
      wiz->Destroy();
    }
    break;
  case menu_laplace:
    {
      Gen3Wiz *wiz = new Gen3Wiz(_("Expression:"), _("Old variable:"),
                                 _("New variable:"), expr, wxT("t"), wxT("s"),
                                 this, -1, _("Laplace"));
      wiz->SetValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wxT("laplace(") + wiz->GetValue1() + wxT(", ")
                       + wiz->GetValue2() +
                       wxT(", ") + wiz->GetValue3() + wxT(");");
        MenuCommand(val);
      }
      wiz->Destroy();
    }
    break;
  case menu_ilt:
    {
      Gen3Wiz *wiz = new Gen3Wiz(_("Expression:"), _("Old variable:"),
                                 _("New variable:"), expr, wxT("s"), wxT("t"),
                                 this, -1, _("Inverse Laplace"));
      wiz->SetValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wxT("ilt(") + wiz->GetValue1() + wxT(", ") +
                       wiz->GetValue2() + wxT(", ") + wiz->GetValue3() + wxT(");");
        MenuCommand(val);
      }
      wiz->Destroy();
    }
    break;
  case button_diff:
  case menu_diff:
    {
      Gen3Wiz *wiz = new Gen3Wiz(_("Expression:"), _("Variable(s):"),
                                 _("Times:"), expr, wxT("x"), wxT("1"),
                                 this, -1, _("Differentiate"));
      wiz->SetValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxStringTokenizer vars(wiz->GetValue2(), wxT(","));
        wxStringTokenizer times(wiz->GetValue3(), wxT(","));

        wxString val = wxT("diff(") + wiz->GetValue1();

        while (vars.HasMoreTokens() && times.HasMoreTokens()) {
          val += wxT(",") + vars.GetNextToken();
          val += wxT(",") + times.GetNextToken();
        }

        val += wxT(");");
        MenuCommand(val);
      }
      wiz->Destroy();
    }
    break;
  case button_taylor:
  case menu_series:
    {
      SeriesWiz *wiz = new SeriesWiz(this, -1, _("Series"));
      wiz->SetValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wiz->GetValue();
        MenuCommand(val);
      }
      wiz->Destroy();
    }
    break;
  case button_limit:
  case menu_limit:
    {
      LimitWiz *wiz = new LimitWiz(this, -1, _("Limit"));
      wiz->SetValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wiz->GetValue();
        MenuCommand(val);
      }
      wiz->Destroy();
    }
    break;
  case button_sum:
  case menu_sum:
    {
      SumWiz *wiz = new SumWiz(this, -1, _("Sum"));
      wiz->SetValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wiz->GetValue();
        MenuCommand(val);
      }
      wiz->Destroy();
    }
    break;
  case button_product:
  case menu_product:
    {
      Gen4Wiz *wiz = new Gen4Wiz(_("Expression:"), _("Variable:"), _("From:"),
                                 _("To:"), expr, wxT("k"), wxT("1"), wxT("n"),
                                 this, -1, _("Product"));
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        cmd = wxT("product(") + wiz->GetValue1() + wxT(", ") +
              wiz->GetValue2() + wxT(", ") +
              wiz->GetValue3() + wxT(", ") +
              wiz->GetValue4() + wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
    break;
  default:
    break;
  }
}

void wxMaxima::PlotMenu(wxCommandEvent& event)
{
  wxString expr = GetDefaultEntry();
  wxString cmd;
  switch (event.GetId())
  {
  case button_plot3:
  case gp_plot3:
    {
      Plot3DWiz *wiz = new Plot3DWiz(this, -1, _("Plot 3D"));
      wiz->SetValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wiz->GetValue();
        MenuCommand(val);
      }
      wiz->Destroy();
    }
    break;
  case button_plot2:
  case gp_plot2:
    {
      Plot2DWiz *wiz = new Plot2DWiz(this, -1, _("Plot 2D"));
      wiz->SetValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wiz->GetValue();
        MenuCommand(val);
      }
      wiz->Destroy();
    }
    break;
  case menu_plot_format:
    {
      wxString format = GetTextFromUser(_("Enter new plot format:"),
                                        _("Plot format"),
                                        wxT("gnuplot"), this);
      if (format.Length())
      {
        MenuCommand(wxT("set_plot_option(['plot_format, '") + format +
                   wxT("])$"));
      }
    }
  default:
    break;
  }
}

void wxMaxima::NumericalMenu(wxCommandEvent& event)
{
  wxString expr = GetDefaultEntry();
  wxString cmd;
  switch (event.GetId())
  {
  case menu_to_float:
    cmd = wxT("float(") + expr + wxT("), numer;");
    MenuCommand(cmd);
    break;
  case menu_to_bfloat:
    cmd = wxT("bfloat(") + expr + wxT(");");
    MenuCommand(cmd);
    break;
  case menu_num_out:
    cmd = wxT("if numer#false then numer:false else numer:true;");
    MenuCommand(cmd);
    break;
  case menu_set_precision:
    cmd = GetTextFromUser(_("Enter new precision:"), _("Precision"),
                          wxT("16"), this);
    if (cmd.Length())
    {
      cmd = wxT("fpprec : ") + cmd + wxT(";");
      MenuCommand(cmd);
    }
    break;
  default:
    break;
  }
}

void wxMaxima::HelpMenu(wxCommandEvent& event)
{
  wxString expr = GetDefaultEntry();
  wxString cmd;
  wxString helpSearchString = wxT("%");
  if (m_console->CanCopy(true))
    helpSearchString = m_console->GetString();
  else if (m_console->GetActiveCell() != NULL) {
    helpSearchString = ((EditorCell *)m_console->GetActiveCell())->SelectWordUnderCaret();
  }
  if (helpSearchString == wxT(""))
    helpSearchString = wxT("%");

  switch (event.GetId())
  {
  case wxID_ABOUT:
  {
    wxAboutDialogInfo info;
#if defined __WXMSW__ || defined __WXMAC__
    info.SetIcon(GetIcon());
    info.SetDescription(_("wxMaxima is a graphical user interface for the\ncomputer algebra system Maxima based on wxWidgets."));
#else
    info.SetDescription(_("wxMaxima is a graphical user interface for the computer algebra system Maxima based on wxWidgets."));
#endif
    info.SetName(_("wxMaxima"));
    info.SetVersion(wxT(VERSION));
    info.SetCopyright(wxT("(C) 2004-2008 Andrej Vodopivec"));
    info.SetWebSite(wxT("http://wxmaxima.sourceforge.net/"));
#if !defined __WXMSW__ && !defined __WXMAC__
    info.AddDeveloper(wxT("Andrej Vodopivec <andrej.vodopivec@gmail.com>"));
    info.AddDeveloper(wxT("Ziga Lenarcic <ziga.lenarcic@gmail.com>"));
    info.AddTranslator(wxT("Alexey Beshenov (ru)"));
    info.AddTranslator(wxT("Istvan Blahota (hu)"));
    info.AddTranslator(wxT("Marco Ciampa (it)"));
    info.AddTranslator(wxT("Eric Delevaux (fr)"));
    info.AddTranslator(wxT("Harald Geyer (de)"));
    info.AddTranslator(wxT("Eduardo M. Kalinowski (pt_br)"));
    info.AddTranslator(wxT("Sergey Semerikov (uk)"));
    info.AddTranslator(wxT("Rafal Topolnicki (pl)"));
    info.AddTranslator(wxT("Antonio Ullan (es)"));
    info.AddTranslator(wxT("Vadim V. Zhytnikov (ru)"));
    info.AddArtist(wxT("wxMaxima icon: Sven Hodapp"));
#endif
    wxAboutBox(info);
  }
  break;
  case wxID_HELP:
#if defined (__WXMSW__) || defined (__WXGTK20__) || defined (__WXMAC__)
  case tb_help:
#endif
    ShowHelp(helpSearchString);
    break;
  case menu_example:
    if (expr == wxT("%"))
      cmd = GetTextFromUser(_("Show an example for the command:"), _("Example"),
                            wxEmptyString, this);
    else
      cmd = expr;
    if (cmd.Length())
    {
      cmd = wxT("example(") + cmd + wxT(");");
      MenuCommand(cmd);
    }
    break;
  case menu_apropos:
    if (expr == wxT("%"))
      cmd = GetTextFromUser(_("Show all commands similar to:"), _("Apropos"),
                            wxEmptyString, this);
    else
      cmd = expr;
    if (cmd.Length())
    {
      cmd = wxT("apropos(\"") + cmd + wxT("\");");
      MenuCommand(cmd);
    }
    break;
//  case menu_show_tip:
//    ShowTip(true);
//    break;
  case menu_build_info:
    MenuCommand(wxT("build_info()$"));
    break;
  case menu_bug_report:
    MenuCommand(wxT("bug_report()$"));
    break;
  default:
    break;
  }
}

void wxMaxima::OnClose(wxCloseEvent& event)
{
  if (!m_fileSaved && event.CanVeto()) {
    int close = wxMessageBox(_("Document not saved!\n\nQuit wxMaxima and lose all changes?"),
                             _("Quit?"),
                             wxOK|wxCANCEL);
    if (close != wxOK) {
      event.Veto();
      return;
    }
  }
  wxConfig *config = (wxConfig *)wxConfig::Get();
  wxSize size = GetSize();
  wxPoint pos = GetPosition();
  bool maximized = IsMaximized();
  config->Write(wxT("pos-x"), pos.x);
  config->Write(wxT("pos-y"), pos.y);
  config->Write(wxT("pos-w"), size.GetWidth());
  config->Write(wxT("pos-h"), size.GetHeight());
  if (maximized)
    config->Write(wxT("pos-max"), 1);
  else
    config->Write(wxT("pos-max"), 0);
  if (m_lastPath.Length() > 0)
    config->Write(wxT("lastPath"), m_lastPath);
  m_closing = true;
#if defined __WXMAC__
  wxGetApp().topLevelWindows.Erase(wxGetApp().topLevelWindows.Find(this));
#endif
  wxTheClipboard->Flush();
  CleanUp();
  Destroy();
}

void wxMaxima::PopupMenu(wxCommandEvent& event)
{
  wxString selection = m_console->GetString();
  switch (event.GetId())
  {
  case popid_copy:
    if (m_console->CanCopy(true))
      m_console->Copy();
    break;
  case popid_copy_tex:
    if (m_console->CanCopy(true))
      m_console->CopyTeX();
    break;
  case popid_cut:
    if (m_console->CanCopy(true))
      m_console->CutToClipboard();
    break;
  case popid_paste:
    m_console->PasteFromClipboard();
    break;
  case popid_select_all:
    m_console->SelectAll();
    break;
  case popid_copy_image:
    if (m_console->CanCopy())
      m_console->CopyBitmap();
    break;
  case popid_simplify:
    MenuCommand(wxT("ratsimp(") + selection + wxT(");"));
    break;
  case popid_expand:
    MenuCommand(wxT("expand(") + selection + wxT(");"));
    break;
  case popid_factor:
    MenuCommand(wxT("factor(") + selection + wxT(");"));
    break;
  case popid_solve:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Equation(s):"), _("Variable(s):"),
                                 selection, wxT("x"), this, -1, _("Solve"), true);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString cmd = wxT("solve([") + wiz->GetValue1() + wxT("], [") +
                       wiz->GetValue2() + wxT("]);");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
    break;
  case popid_solve_num:
    {
      Gen4Wiz *wiz = new Gen4Wiz(_("Equation:"), _("Variable:"),
                                 _("Lower bound:"), _("Upper bound:"),
                                 selection, wxT("x"), wxT("-1"), wxT("1"),
                                 this, -1, _("Find root"), true);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString cmd = wxT("find_root(") + wiz->GetValue1() + wxT(", ") +
                       wiz->GetValue2() + wxT(", ") +
                       wiz->GetValue3() + wxT(", ") +
                       wiz->GetValue4() + wxT(");");
        MenuCommand(cmd);
      }
      wiz->Destroy();
    }
    break;
  case popid_integrate:
    {
      IntegrateWiz *wiz = new IntegrateWiz(this, -1, _("Integrate"));
      wiz->SetValue(selection);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wiz->GetValue();
        MenuCommand(val);
      }
      wiz->Destroy();
    }
    break;
  case popid_diff:
    {
      Gen3Wiz *wiz = new Gen3Wiz(_("Expression:"), _("Variable(s):"),
                                 _("Times:"), selection, wxT("x"), wxT("1"),
                                 this, -1, _("Differentiate"));
      wiz->SetValue(selection);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxStringTokenizer vars(wiz->GetValue2(), wxT(","));
        wxStringTokenizer times(wiz->GetValue3(), wxT(","));

        wxString val = wxT("diff(") + wiz->GetValue1();

        while (vars.HasMoreTokens() && times.HasMoreTokens()) {
          val += wxT(",") + vars.GetNextToken();
          val += wxT(",") + times.GetNextToken();
        }

        val += wxT(");");
        MenuCommand(val);
      }
      wiz->Destroy();
    }
    break;
  case popid_subst:
    {
      SubstituteWiz *wiz = new SubstituteWiz(this, -1, _("Substitute"));
      wiz->SetValue(selection);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wiz->GetValue();
        MenuCommand(val);
      }
      wiz->Destroy();
    }
    break;
  case popid_plot2d:
    {
      Plot2DWiz *wiz = new Plot2DWiz(this, -1, _("Plot 2D"));
      wiz->SetValue(selection);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wiz->GetValue();
        MenuCommand(val);
      }
      wiz->Destroy();
    }
    break;
  case popid_plot3d:
    {
      Plot3DWiz *wiz = new Plot3DWiz(this, -1, _("Plot 3D"));
      wiz->SetValue(selection);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK)
      {
        wxString val = wiz->GetValue();
        MenuCommand(val);
      }
      wiz->Destroy();
    }
    break;
  case popid_float:
    MenuCommand(wxT("float(") + selection + wxT("), numer;"));
    break;
  case popid_image:
    {
      wxString file = wxFileSelector(_("Save selection to file"), m_lastPath,
                                     wxT("image.png"), wxT("png"),
                                     _("PNG image (*.png)|*.png|"
                                       "JPEG image (*.jpg)|*.jpg|"
                                       "Windows bitmap (*.bmp)|*.bmp|"
                                       "X pixmap (*.xpm)|*.xpm"),
                                     wxFD_SAVE | wxFD_OVERWRITE_PROMPT);
      if (file.Length())
      {
        m_console->CopyToFile(file);
        m_lastPath = wxPathOnly(file);
      }
    }
    break;
#if defined __WXMSW__
  case popid_image_copy:
    m_console->CopyBitmap();
    break;
#endif
  case popid_evaluate:
    m_console->AddSelectionToEvaluationQueue();
    TryEvaluateNextInQueue();
    break;
  }
}

void wxMaxima::OnRecentDocument(wxCommandEvent& event)
{
  if (!m_fileSaved) {
    int close = wxMessageBox(_("Document not saved!\n\nClose current document and lose all changes?"),
                             _("Close document?"),
                             wxOK|wxCANCEL);
    if (close != wxOK)
      return;
  }

  wxString file = GetRecentDocument(event.GetId() - menu_recent_document_0);
  if (wxFileExists(file))
    OpenFile(file);
  else {
    wxMessageBox(_("File you tried to open does not exist."), _("File not found"), wxOK);
    RemoveRecentDocument(file);
  }
}

void wxMaxima::EditInputMenu(wxCommandEvent& event)
{
  if (!m_console->CanEdit())
    return ;

  MathCell* tmp = m_console->GetSelectionStart();

  if (tmp == NULL)
    return ;

  m_console->SetActiveCell(tmp);
}

// EvaluateEvent
// User tried to evaluate, find out what is the case
// Normally just add the respective groupcells to evaluationqueue
// If there is a special case - eg sending from output section
// of the working group, handle it carefully.
void wxMaxima::EvaluateEvent(wxCommandEvent& event)
{
  MathCell* tmp = m_console->GetActiveCell();
  if (tmp != NULL) // we have an active cell
  {
    if (tmp->GetType() == MC_TYPE_INPUT && !m_inLispMode)
      tmp->AddEnding();
    // if active cell is part of a working group, we have a special
    // case - answering a question. Manually send answer to Maxima.
    if ((GroupCell *)tmp->GetParent() == m_console->m_evaluationQueue->GetFirst()) {
      SendMaxima(tmp->ToString(false));
    }
    else { // normally just add to queue
      m_console->AddCellToEvaluationQueue((GroupCell *)tmp->GetParent());
      TryEvaluateNextInQueue();
    }
  }
  else { // no evaluate has been called on no active cell?
    m_console->AddSelectionToEvaluationQueue();
    TryEvaluateNextInQueue();
  }
}
// TryEvaluateNextInQueue
// Tries to evaluate next group cell in queue
// Calling this function should not do anything dangerous
void wxMaxima::TryEvaluateNextInQueue()
{
  GroupCell * group = m_console->m_evaluationQueue->GetFirst();
  if (group == NULL)
    return; //empty queue

  if (group->GetInput()->GetValue() != wxEmptyString)
  {
    group->GetInput()->AddEnding();
    wxString text = group->GetInput()->ToString(false);

    // override evaluation when input equals wxmaxima_debug_dump_output
    if (text.IsSameAs(wxT("wxmaxima_debug_dump_output;"))) {
      m_console->m_evaluationQueue->RemoveFirst();
      DumpProcessOutput();
      return;
    }

    group->RemoveOutput();

    m_console->SetWorkingGroup(group);
    group->GetPrompt()->SetValue(m_lastPrompt);
    m_console->Recalculate();
    m_console->ScrollToCell(group);

    SendMaxima(text);
  }
  else
  {
    m_console->m_evaluationQueue->RemoveFirst();
    TryEvaluateNextInQueue();
  }
}

void wxMaxima::InsertMenu(wxCommandEvent& event)
{
  int type = 0;
  switch (event.GetId())
  {
  case popid_insert_input:
  case menu_insert_input:
#if defined (__WXMSW__) || defined (__WXGTK20__) || defined (__WXMAC__)
  case tb_insert_input:
#endif
    type = GC_TYPE_CODE;
    break;
  case menu_add_comment:
  case popid_add_comment:
#if defined (__WXMSW__) || defined (__WXGTK20__) || defined (__WXMAC__)
  case tb_insert_text:
#endif
    type = GC_TYPE_TEXT;
    break;
  case menu_add_title:
    type = GC_TYPE_TITLE;
    break;
  case menu_add_section:
    type = GC_TYPE_SECTION;
    break;
  case menu_insert_image:
    {
      wxString file = wxFileSelector(_("Insert Image"), m_lastPath,
                                     wxEmptyString, wxEmptyString,
                                     _("Image files (*.png, *.jpg, *.bmp, *.xpm)|*.png;*.jpg;*.bmp;*.xpm"),
                                     wxFD_OPEN);
      if (file != wxEmptyString)
        m_console->OpenHCaret(file, GC_TYPE_IMAGE);
      return ;
    }
    break;
  }

  m_console->OpenHCaret(wxEmptyString, type);
}

void wxMaxima::ResetTitle(bool saved)
{
  if (saved != m_fileSaved)
  {
    m_fileSaved = saved;
    if (m_currentFile.Length() == 0) {
      if (saved)
        SetTitle(wxString::Format(_("wxMaxima %s "), wxT(VERSION)) + _("[ unsaved ]"));
      else
        SetTitle(wxString::Format(_("wxMaxima %s "), wxT(VERSION)) + _("[ unsaved* ]"));
    }
    else
    {
      wxString name, ext;
      wxFileName::SplitPath(m_currentFile, NULL, NULL, &name, &ext);
      if (m_fileSaved)
        SetTitle(wxString::Format(_("wxMaxima %s "), wxT(VERSION)) +
                 wxT(" [ ") + name + wxT(".") + ext + wxT(" ]"));
      else
        SetTitle(wxString::Format(_("wxMaxima %s "), wxT(VERSION)) +
                 wxT(" [ ") + name + wxT(".") + ext + wxT("* ]"));
    }
  }
}

///--------------------------------------------------------------------------------
///  Plot Slider
///--------------------------------------------------------------------------------

void wxMaxima::UpdateSlider(wxUpdateUIEvent &ev)
{

  if (m_console->IsSelected(MC_TYPE_SLIDE))
  {
    if (!m_console->AnimationRunning())
    {
      SlideShow *cell = (SlideShow *)m_console->GetSelectionStart();

      m_plotSlider->SetRange(0, cell->Length() - 1);
      m_plotSlider->SetValue(cell->GetDisplayedIndex());
      m_plotSlider->Enable(true);
    }
    else
      m_plotSlider->Enable(false);
  }
  else
    m_plotSlider->Enable(false);
}

void wxMaxima::SliderEvent(wxScrollEvent &ev)
{
  SlideShow *cell = (SlideShow *)m_console->GetSelectionStart();
  if (cell != NULL)
  {
    cell->SetDisplayedIndex(ev.GetPosition());
    m_console->Refresh();
  }
}

BEGIN_EVENT_TABLE(wxMaxima, wxFrame)
  EVT_COMMAND_SCROLL(plot_slider_id, wxMaxima::SliderEvent)
  EVT_MENU(popid_copy, wxMaxima::PopupMenu)
  EVT_MENU(popid_copy_image, wxMaxima::PopupMenu)
  EVT_MENU(popid_delete, wxMaxima::EditMenu)
  EVT_MENU(popid_simplify, wxMaxima::PopupMenu)
  EVT_MENU(popid_factor, wxMaxima::PopupMenu)
  EVT_MENU(popid_expand, wxMaxima::PopupMenu)
  EVT_MENU(popid_solve, wxMaxima::PopupMenu)
  EVT_MENU(popid_solve_num, wxMaxima::PopupMenu)
  EVT_MENU(popid_subst, wxMaxima::PopupMenu)
  EVT_MENU(popid_plot2d, wxMaxima::PopupMenu)
  EVT_MENU(popid_plot3d, wxMaxima::PopupMenu)
  EVT_MENU(popid_diff, wxMaxima::PopupMenu)
  EVT_MENU(popid_integrate, wxMaxima::PopupMenu)
  EVT_MENU(popid_float, wxMaxima::PopupMenu)
  EVT_MENU(popid_copy_tex, wxMaxima::PopupMenu)
  EVT_MENU(popid_image, wxMaxima::PopupMenu)
#if defined __WXMSW__
  EVT_MENU(popid_image_copy, wxMaxima::PopupMenu)
#endif
  EVT_BUTTON(button_integrate, wxMaxima::CalculusMenu)
  EVT_BUTTON(button_diff, wxMaxima::CalculusMenu)
  EVT_BUTTON(button_solve, wxMaxima::EquationsMenu)
  EVT_BUTTON(button_solve_ode, wxMaxima::EquationsMenu)
  EVT_BUTTON(button_sum, wxMaxima::CalculusMenu)
  EVT_BUTTON(button_expand, wxMaxima::SimplifyMenu)
  EVT_BUTTON(button_factor, wxMaxima::SimplifyMenu)
  EVT_BUTTON(button_taylor, wxMaxima::CalculusMenu)
  EVT_BUTTON(button_limit, wxMaxima::CalculusMenu)
  EVT_BUTTON(button_ratsimp, wxMaxima::SimplifyMenu)
  EVT_BUTTON(button_trigexpand, wxMaxima::SimplifyMenu)
  EVT_BUTTON(button_trigreduce, wxMaxima::SimplifyMenu)
  EVT_BUTTON(button_trigsimp, wxMaxima::SimplifyMenu)
  EVT_BUTTON(button_product, wxMaxima::CalculusMenu)
  EVT_BUTTON(button_radcan, wxMaxima::SimplifyMenu)
  EVT_BUTTON(button_subst, wxMaxima::MaximaMenu)
  EVT_BUTTON(button_plot2, wxMaxima::PlotMenu)
  EVT_BUTTON(button_plot3, wxMaxima::PlotMenu)
  EVT_BUTTON(button_map, wxMaxima::AlgebraMenu)
  EVT_BUTTON(button_rectform, wxMaxima::SimplifyMenu)
  EVT_MENU(menu_polarform, wxMaxima::SimplifyMenu)
  EVT_MENU(menu_restart_id, wxMaxima::MaximaMenu)
#ifndef __WXMAC__
  EVT_MENU(wxID_EXIT, wxMaxima::FileMenu)
#endif
  EVT_MENU(wxID_ABOUT, wxMaxima::HelpMenu)
  EVT_MENU(menu_save_id, wxMaxima::FileMenu)
  EVT_MENU(menu_save_as_id, wxMaxima::FileMenu)
  EVT_MENU(menu_load_id, wxMaxima::FileMenu)
  EVT_MENU(menu_functions, wxMaxima::MaximaMenu)
  EVT_MENU(menu_variables, wxMaxima::MaximaMenu)
  EVT_MENU(wxID_PREFERENCES, wxMaxima::EditMenu)
  EVT_MENU(menu_sconsole_id, wxMaxima::FileMenu)
  EVT_MENU(menu_export_html, wxMaxima::FileMenu)
  EVT_MENU(wxID_HELP, wxMaxima::HelpMenu)
  EVT_MENU(menu_bug_report, wxMaxima::HelpMenu)
  EVT_MENU(menu_build_info, wxMaxima::HelpMenu)
  EVT_MENU(menu_interrupt_id, wxMaxima::Interrupt)
  EVT_MENU(menu_open_id, wxMaxima::FileMenu)
  EVT_MENU(menu_batch_id, wxMaxima::FileMenu)
  EVT_MENU(menu_ratsimp, wxMaxima::SimplifyMenu)
  EVT_MENU(menu_radsimp, wxMaxima::SimplifyMenu)
  EVT_MENU(menu_expand, wxMaxima::SimplifyMenu)
  EVT_MENU(menu_factor, wxMaxima::SimplifyMenu)
  EVT_MENU(menu_gfactor, wxMaxima::SimplifyMenu)
  EVT_MENU(menu_trigsimp, wxMaxima::SimplifyMenu)
  EVT_MENU(menu_trigexpand, wxMaxima::SimplifyMenu)
  EVT_MENU(menu_trigreduce, wxMaxima::SimplifyMenu)
  EVT_MENU(menu_rectform, wxMaxima::SimplifyMenu)
  EVT_MENU(menu_demoivre, wxMaxima::SimplifyMenu)
  EVT_MENU(menu_num_out, wxMaxima::NumericalMenu)
  EVT_MENU(menu_to_float, wxMaxima::NumericalMenu)
  EVT_MENU(menu_to_bfloat, wxMaxima::NumericalMenu)
  EVT_MENU(menu_exponentialize, wxMaxima::SimplifyMenu)
  EVT_MENU(menu_invert_mat, wxMaxima::AlgebraMenu)
  EVT_MENU(menu_determinant, wxMaxima::AlgebraMenu)
  EVT_MENU(menu_eigen, wxMaxima::AlgebraMenu)
  EVT_MENU(menu_eigvect, wxMaxima::AlgebraMenu)
  EVT_MENU(menu_adjoint_mat, wxMaxima::AlgebraMenu)
  EVT_MENU(menu_transpose, wxMaxima::AlgebraMenu)
  EVT_MENU(menu_set_precision, wxMaxima::NumericalMenu)
  EVT_MENU(menu_talg, wxMaxima::SimplifyMenu)
  EVT_MENU(menu_tellrat, wxMaxima::SimplifyMenu)
  EVT_MENU(menu_modulus, wxMaxima::SimplifyMenu)
  EVT_MENU(menu_allroots, wxMaxima::EquationsMenu)
  EVT_MENU(menu_realroots, wxMaxima::EquationsMenu)
  EVT_MENU(menu_solve, wxMaxima::EquationsMenu)
  EVT_MENU(menu_solve_num, wxMaxima::EquationsMenu)
  EVT_MENU(menu_solve_ode, wxMaxima::EquationsMenu)
  EVT_MENU(menu_map_mat, wxMaxima::AlgebraMenu)
  EVT_MENU(menu_enter_mat, wxMaxima::AlgebraMenu)
  EVT_MENU(menu_cpoly, wxMaxima::AlgebraMenu)
  EVT_MENU(menu_solve_lin, wxMaxima::EquationsMenu)
  EVT_MENU(menu_solve_algsys, wxMaxima::EquationsMenu)
  EVT_MENU(menu_eliminate, wxMaxima::EquationsMenu)
  EVT_MENU(menu_clear_var, wxMaxima::MaximaMenu)
  EVT_MENU(menu_clear_fun, wxMaxima::MaximaMenu)
  EVT_MENU(menu_ivp_1, wxMaxima::EquationsMenu)
  EVT_MENU(menu_ivp_2, wxMaxima::EquationsMenu)
  EVT_MENU(menu_bvp, wxMaxima::EquationsMenu)
  EVT_MENU(menu_bvp, wxMaxima::EquationsMenu)
  EVT_MENU(menu_fun_def, wxMaxima::MaximaMenu)
  EVT_MENU(menu_divide, wxMaxima::CalculusMenu)
  EVT_MENU(menu_gcd, wxMaxima::CalculusMenu)
  EVT_MENU(menu_lcm, wxMaxima::CalculusMenu)
  EVT_MENU(menu_continued_fraction, wxMaxima::CalculusMenu)
  EVT_MENU(menu_partfrac, wxMaxima::CalculusMenu)
  EVT_MENU(menu_risch, wxMaxima::CalculusMenu)
  EVT_MENU(menu_integrate, wxMaxima::CalculusMenu)
  EVT_MENU(menu_laplace, wxMaxima::CalculusMenu)
  EVT_MENU(menu_ilt, wxMaxima::CalculusMenu)
  EVT_MENU(menu_diff, wxMaxima::CalculusMenu)
  EVT_MENU(menu_series, wxMaxima::CalculusMenu)
  EVT_MENU(menu_limit, wxMaxima::CalculusMenu)
  EVT_MENU(menu_gen_mat, wxMaxima::AlgebraMenu)
  EVT_MENU(menu_map, wxMaxima::AlgebraMenu)
  EVT_MENU(menu_sum, wxMaxima::CalculusMenu)
  EVT_MENU(menu_example, wxMaxima::HelpMenu)
  EVT_MENU(menu_apropos, wxMaxima::HelpMenu)
//  EVT_MENU(menu_show_tip, wxMaxima::HelpMenu)
  EVT_MENU(menu_trigrat, wxMaxima::SimplifyMenu)
  EVT_MENU(menu_solve_de, wxMaxima::EquationsMenu)
  EVT_MENU(menu_atvalue, wxMaxima::EquationsMenu)
  EVT_MENU(menu_sum, wxMaxima::CalculusMenu)
  EVT_MENU(menu_product, wxMaxima::CalculusMenu)
  EVT_MENU(menu_change_var, wxMaxima::CalculusMenu)
  EVT_MENU(menu_make_list, wxMaxima::AlgebraMenu)
  EVT_MENU(menu_apply, wxMaxima::AlgebraMenu)
  EVT_MENU(menu_time, wxMaxima::MaximaMenu)
  EVT_MENU(menu_factsimp, wxMaxima::SimplifyMenu)
  EVT_MENU(menu_factcomb, wxMaxima::SimplifyMenu)
  EVT_MENU(menu_realpart, wxMaxima::SimplifyMenu)
  EVT_MENU(menu_imagpart, wxMaxima::SimplifyMenu)
  EVT_MENU(menu_nouns, wxMaxima::SimplifyMenu)
  EVT_MENU(menu_logcontract, wxMaxima::SimplifyMenu)
  EVT_MENU(menu_logexpand, wxMaxima::SimplifyMenu)
  EVT_MENU(gp_plot2, wxMaxima::PlotMenu)
  EVT_MENU(gp_plot3, wxMaxima::PlotMenu)
  EVT_MENU(menu_plot_format, wxMaxima::PlotMenu)
  EVT_MENU(menu_soft_restart, wxMaxima::MaximaMenu)
  EVT_MENU(menu_display, wxMaxima::MaximaMenu)
  EVT_MENU(menu_pade, wxMaxima::CalculusMenu)
  EVT_MENU(menu_add_path, wxMaxima::MaximaMenu)
  EVT_MENU(menu_copy_from_console, wxMaxima::EditMenu)
  EVT_MENU(menu_undo, wxMaxima::EditMenu)
  EVT_MENU(menu_copy_tex_from_console, wxMaxima::EditMenu)
  EVT_MENU(menu_delete_selection, wxMaxima::EditMenu)
  EVT_MENU(menu_texform, wxMaxima::MaximaMenu)
  EVT_MENU(menu_to_fact, wxMaxima::SimplifyMenu)
  EVT_MENU(menu_to_gamma, wxMaxima::SimplifyMenu)
#if WXM_PRINT
  EVT_MENU(wxID_PRINT, wxMaxima::PrintMenu)
 #if defined (__WXMSW__) || (__WXGTK20__) || defined (__WXMAC__)
  EVT_TOOL(tb_print, wxMaxima::PrintMenu)
 #endif
#endif
  EVT_MENU(menu_inc_fontsize, wxMaxima::EditMenu)
  EVT_MENU(menu_dec_fontsize, wxMaxima::EditMenu)
  EVT_MENU(menu_fullscreen, wxMaxima::EditMenu)
  EVT_MENU(menu_copy_as_bitmap, wxMaxima::EditMenu)
  EVT_MENU(menu_copy_to_file, wxMaxima::EditMenu)
  EVT_MENU(menu_copy_input_from_console, wxMaxima::EditMenu)
  EVT_MENU(menu_cut_input_from_console, wxMaxima::EditMenu)
  EVT_MENU(menu_select_all, wxMaxima::EditMenu)
  EVT_MENU(menu_subst, wxMaxima::MaximaMenu)
#if defined (__WXMSW__) || defined (__WXGTK20__) || defined (__WXMAC__)
  EVT_TOOL(tb_open, wxMaxima::FileMenu)
  EVT_TOOL(tb_save, wxMaxima::FileMenu)
  EVT_TOOL(tb_copy, wxMaxima::EditMenu)
  EVT_TOOL(tb_delete, wxMaxima::EditMenu)
  EVT_TOOL(tb_pref, wxMaxima::EditMenu)
  EVT_TOOL(tb_insert_input, wxMaxima::InsertMenu)
  EVT_TOOL(tb_insert_text, wxMaxima::InsertMenu)
  EVT_TOOL(tb_interrupt, wxMaxima::Interrupt)
  EVT_TOOL(tb_help, wxMaxima::HelpMenu)
  EVT_TOOL(tb_animation_start, wxMaxima::FileMenu)
  EVT_TOOL(tb_animation_stop, wxMaxima::FileMenu)
#endif
  EVT_SOCKET(socket_server_id, wxMaxima::ServerEvent)
  EVT_SOCKET(socket_client_id, wxMaxima::ClientEvent)
  EVT_UPDATE_UI(plot_slider_id, wxMaxima::UpdateSlider)
  EVT_UPDATE_UI(menu_copy_from_console, wxMaxima::UpdateMenus)
  EVT_UPDATE_UI(menu_copy_tex_from_console, wxMaxima::UpdateMenus)
  EVT_UPDATE_UI(menu_inc_fontsize, wxMaxima::UpdateMenus)
  EVT_UPDATE_UI(menu_dec_fontsize, wxMaxima::UpdateMenus)
  EVT_UPDATE_UI(wxID_PRINT, wxMaxima::UpdateMenus)
  EVT_UPDATE_UI(menu_copy_as_bitmap, wxMaxima::UpdateMenus)
  EVT_UPDATE_UI(menu_copy_to_file, wxMaxima::UpdateMenus)
  EVT_UPDATE_UI(menu_copy_input_from_console, wxMaxima::UpdateMenus)
  EVT_UPDATE_UI(menu_cut_input_from_console, wxMaxima::UpdateMenus)
  EVT_UPDATE_UI(menu_evaluate, wxMaxima::UpdateMenus)
  EVT_UPDATE_UI(menu_evaluate_all, wxMaxima::UpdateMenus)
  EVT_UPDATE_UI(menu_select_all, wxMaxima::UpdateMenus)
  EVT_UPDATE_UI(menu_undo, wxMaxima::UpdateMenus)
#if defined (__WXMSW__) || defined (__WXGTK20__) || defined (__WXMAC__)
  EVT_UPDATE_UI(tb_print, wxMaxima::UpdateToolBar)
  EVT_UPDATE_UI(tb_copy, wxMaxima::UpdateToolBar)
  EVT_UPDATE_UI(tb_delete, wxMaxima::UpdateToolBar)
  EVT_UPDATE_UI(tb_interrupt, wxMaxima::UpdateToolBar)
  EVT_UPDATE_UI(tb_save, wxMaxima::UpdateToolBar)
  EVT_UPDATE_UI(tb_animation_start, wxMaxima::UpdateToolBar)
  EVT_UPDATE_UI(tb_animation_stop, wxMaxima::UpdateToolBar)
#endif
  EVT_UPDATE_UI(menu_save_id, wxMaxima::UpdateMenus)
  EVT_CLOSE(wxMaxima::OnClose)
  EVT_END_PROCESS(maxima_process_id, wxMaxima::OnProcessEvent)
  EVT_MENU(popid_edit, wxMaxima::EditInputMenu)
  EVT_MENU(menu_evaluate, wxMaxima::EvaluateEvent)
  EVT_MENU(menu_add_comment, wxMaxima::InsertMenu)
  EVT_MENU(menu_add_section, wxMaxima::InsertMenu)
  EVT_MENU(menu_add_title, wxMaxima::InsertMenu)
  EVT_MENU(popid_add_comment, wxMaxima::InsertMenu)
  EVT_MENU(menu_insert_input, wxMaxima::InsertMenu)
  EVT_MENU(popid_insert_input, wxMaxima::InsertMenu)
  EVT_MENU(menu_cut, wxMaxima::EditMenu)
  EVT_MENU(menu_paste, wxMaxima::EditMenu)
  EVT_MENU(menu_paste_input, wxMaxima::EditMenu)
  EVT_MENU(popid_cut, wxMaxima::PopupMenu)
  EVT_MENU(popid_paste, wxMaxima::PopupMenu)
  EVT_MENU(popid_select_all, wxMaxima::PopupMenu)
  EVT_MENU(popid_evaluate, wxMaxima::PopupMenu)
  EVT_MENU(menu_evaluate_all, wxMaxima::MaximaMenu)
  EVT_IDLE(wxMaxima::OnIdle)
  EVT_MENU(menu_remove_output, wxMaxima::EditMenu)
  EVT_MENU_RANGE(menu_recent_document_0, menu_recent_document_9, wxMaxima::OnRecentDocument)
  EVT_MENU(menu_insert_image, wxMaxima::InsertMenu)
END_EVENT_TABLE()
