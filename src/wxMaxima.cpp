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

#include "wxMaxima.h"
#include "Version.h"
#include "Config.h"
#include "TextInput.h"
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
#include "DragNDrop.h"
#include "SystemWiz.h"
#include "MathPrintout.h"

#include <wx/filedlg.h>
#include <wx/utils.h>
#include <wx/msgdlg.h>
#include <wx/textfile.h>
#include <wx/tipdlg.h>
#include <wx/html/helpctrl.h>
#include <wx/tokenzr.h>
#include <wx/mimetype.h>
#include <wx/dynlib.h>
#include <wx/dir.h>

enum {
  maxima_process_id
};

wxMaxima::wxMaxima(wxWindow *parent, int id, const wxString title,
                   const wxPoint pos, const wxSize size) :
  wxMaximaFrame(parent,id,title,pos,size)
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

#if wxUSE_DRAG_AND_DROP
  m_console->SetDropTarget(new FileDrop(this, m_inputLine, DND_LOAD));
  m_inputLine->SetDropTarget(new FileDrop(this, m_inputLine, DND_WRITE));
#endif

  CheckForPrintingSupport();

  m_printData = new wxPrintData;
  m_printData->SetQuality(wxPRINT_QUALITY_HIGH);
  m_inputLine->SetFocus();
  m_closing = false;
}


wxMaxima::~wxMaxima()
{
  if (m_client != NULL)
    m_client->Destroy();
  delete m_printData;
}

void wxMaxima::CheckForPrintingSupport()
{
#if defined __WXMSW__
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
}

void wxMaxima::InitSession()
{
  while (!StartServer()) {
    m_port++;
    if (m_port > 5000) {
      wxMessageBox(_("wxMaxima could not start the server.\n\n"
                     "Please check you have network support\n"
                     "enabled and try again!"),
                   _("Fatal error"),
                   wxOK|wxICON_ERROR);
      exit(0);
    }
  }
  if (!StartMaxima())
    SetStatusText(_("Starting maxima process failed"), 1);
}

wxString wxMaxima::ClearWhitespaces(wxString s)
{
  wxString t(s);
  t.Replace(wxT("\n"), wxT(""));
  while (t.Find(wxT(" <")) > -1)
    t.Replace(wxT(" <"), wxT("<"));
  while (t.Find(wxT("> ")) > -1)
    t.Replace(wxT("> "), wxT(">"));
  return t;
}

void wxMaxima::FirstOutput(wxString s)
{
  wxConfigBase* config = wxConfig::Get();
  bool showHeader = true;
  config->Read(wxT("showHeader"), &showHeader);

  int start = s.Find(m_firstPrompt);
  m_console->ClearWindow();

  if (showHeader)
    ConsoleAppend(s.SubString(0, start-1), TEXTT);
  ConsoleAppend(m_firstPrompt, PROMPTT);
}

///////////////////////////
//
// MAXIMA INTERACTION
//
///////////////////////////

void wxMaxima::ConsoleAppend(wxString s, int type)
{
  m_inPrompt = false;
  m_dispReadOut = false;
  s.Replace(m_promptSuffix, wxT(""));

  wxString t(s);
  t.Trim();
  t.Trim(false);
  if (!t.Length())
    return;

  if (type != ERRORT)
    SetStatusText(_("Parsing output"), 1);
  if (type == TEXTT) {
    while (s.Length()>0) {
      int start = s.Find(wxT("<mth"));
      if (start == -1) {
        t = s;
        t.Trim();
        t.Trim(false);
        if (t.Length())
          DoRawConsoleAppend(s, TEXTT);
        s = wxT("");
      }
      else {
        wxString pre = s.SubString(0, start-1);
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
          DoRawConsoleAppend(pre, TEXTT);
          DoConsoleAppend(wxT("<span>") + ClearWhitespaces(rest) +
                          wxT("</span>"), type, false);
        }
        else {
          DoConsoleAppend(wxT("<span>") + ClearWhitespaces(rest) +
                          wxT("</span>"), type, false);
        }
        s = s.SubString(end+1, s.Length());
      }
    }
  }
  else if (type == INPUTT) {
    // Break long lines
    unsigned int i=0;
    int j=0;
    wxString breaks = wxT(" *+-/()[]:=,");
    while (i<s.Length()) {
      if (j>70 && breaks.Find(s[i])>-1) {
        s = s.SubString(0, i) + wxT("\n") + s.SubString(i+1, s.Length());
        j = 0;
      }
      else if (s[i] == '\n')
        j = 0;
      else
        j++;
      i++;
    }
    // Append the input
    DoRawConsoleAppend(s, type, false);
  }
  else if (type == PROMPTT) {
    SetStatusText(_("Ready for user input"), 1);
    m_lastPrompt = s;
    s = ClearWhitespaces(s);
    if (s.StartsWith(wxT("(%i")) || s.StartsWith(wxT("MAXIMA>"))) {
      m_inPrompt = true;
      type = MPROMPTT;
    }
    DoConsoleAppend(wxT("<span>") + ClearWhitespaces(s) + wxT("</span>"),
                    type, true);
  }
  else if (type == ERRORT) {
    DoRawConsoleAppend(s, ERRORT);
  }
  else {
    DoConsoleAppend(wxT("<span>") + ClearWhitespaces(s) + wxT("</span>"),
                    type, false);
  }
}

void wxMaxima::DoConsoleAppend(wxString s, int type, bool newLine,
                               bool bigSkip)
{
  MathCell* cell;

  if (type == PROMPTT)
    cell = m_MParser.ParseLine(s, TC_PROMPT);
  else if (type == MPROMPTT)
    cell = m_MParser.ParseLine(s, TC_MAIN_PROMPT);
  else if (type == INPUTT)
    cell = m_MParser.ParseLine(s, TC_INPUT);
  else if (type == ERRORT)
    cell = m_MParser.ParseLine(s, TC_ERROR);
  else
    cell = m_MParser.ParseLine(s);

  if (cell == NULL) {
    wxMessageBox(_("There was an error in generated XML!\n\n"
                   "Please report this as a bug."), _("Error"),
                   wxOK|wxICON_EXCLAMATION);
    return;
  }

  cell->SetSkip(bigSkip);
  m_console->AddLine(cell, newLine || cell->BreakLineHere());
}

void wxMaxima::DoRawConsoleAppend(wxString s, int type, bool newLine)
{
  wxStringTokenizer tokens(s, wxT("\n"));
  int count = 0;
  while(tokens.HasMoreTokens()) {
    TextCell* cell = new TextCell(tokens.GetNextToken());
    if (type == PROMPTT)
      cell->SetStyle(TC_PROMPT);
    else if (type == MPROMPTT)
      cell->SetStyle(TC_MAIN_PROMPT);
    else if (type == INPUTT)
      cell->SetStyle(TC_INPUT);
    else if (type == ERRORT)
      cell->SetStyle(TC_ERROR);
    else
      cell->SetStyle(TC_VARIABLE);
    
    if (tokens.HasMoreTokens())
      cell->SetSkip(false);
    
    if (count == 0)
      m_console->AddLine(cell, newLine);
    else
      m_console->AddLine(cell, true);
    count++;
  }
}

void wxMaxima::SendMaxima(wxString s, bool clear, bool out, bool silent)
{
  if (!m_isConnected) {
    ConsoleAppend(wxT("\nNot connected to maxima!\n"), ERRORT);
    return;
  }
  if (s.StartsWith(wxT("<ml>"))) {
    s = s.SubString(4, s.Length());
    s.Replace(wxT("<nl>"), wxT("\n"));
  }
  if (clear)
    m_inputLine->SetValue(wxT(""));
  if (out)
    ConsoleAppend(s, INPUTT);
  if (silent) {
    m_inputLine->AddToHistory(s);
    SetStatusText(_("Maxima is calculating"), 1);
    m_dispReadOut = false;
  }
  s.Append(wxT("\n"));
#if wxUSE_UNICODE
  char *buf;
  wxWX2MBbuf tmp = wxConvertWX2MB(s.wx_str());
  buf = strdup(tmp);
  m_client->Write(buf, strlen(buf));
  free(buf);
#else
  m_client->Write(s.c_str(), s.Length());
#endif
}

void wxMaxima::EnterCommand(wxCommandEvent& event)
{
  wxString input = m_inputLine->GetValue();
  input.Trim();
  input.Trim(false);
  if (!m_inLispMode && (input.Length() == 0  ||
                        (input.Last() != ';' && input.Last()!='$')))
    input.Append(';');
  SendMaxima(input);
  m_inputLine->Clear();
  m_inputLine->SetFocus();
}

void wxMaxima::ClientEvent(wxSocketEvent& event)
{
  char buffer[SOCKET_SIZE+1];
  int read;
  switch (event.GetSocketEvent()) {
  case wxSOCKET_INPUT:
#if wxCHECK_VERSION(2, 5, 3)
    wxMilliSleep(1);  // Let's wait for more data
#else
    wxUsleep(1);
#endif
    m_client->Read(buffer, SOCKET_SIZE);
    if (!m_client->Error()) {
      read = m_client->LastCount();
      buffer[read] = 0;
#if wxUSE_UNICODE
      m_currentOutput += wxConvertMB2WX(buffer);
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
    }
    break;
  case wxSOCKET_LOST:
    if (!m_closing)
      ConsoleAppend(wxT("\nCLIENT: Lost socket connection ...\n"
                        "Restart maxima with 'Maxima->Restart maxima'.\n"),
                    ERRORT);
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

void wxMaxima::ReadFirstPrompt()
{
#if defined(__WXMSW__)
  int start = m_currentOutput.Find(wxT("Maxima 5.9"));
  if (start==-1)
    start = 0;
  FirstOutput(wxT("wxMaxima ")
              wxT(WXMAXIMA_VERSION)
              wxT(" http://wxmaxima.sourceforge.net\n") +
              m_currentOutput.SubString(start, m_currentOutput.Length()-1));
#endif // __WXMSW__
  int s = m_currentOutput.Find(wxT("pid=")) + 4;
  int t = s + m_currentOutput.SubString(s, m_currentOutput.Length()).Find(wxT("\n"));
  if (s < t)
    m_currentOutput.SubString(s,t).ToLong(&m_pid);
  if (m_pid > 0)
    GetMenuBar()->Enable(menu_interrupt_id, true);
  m_first = false;
  m_inLispMode = false;
  SetStatusText(_("Ready for user input"), 1);
  m_currentOutput = wxT("");
}

void wxMaxima::ReadMath()
{
  int end = m_currentOutput.Find(m_promptPrefix);
  while (end > -1) {
    m_readingPrompt = true;
    wxString o = m_currentOutput.Left(end);
    ConsoleAppend(o, TEXTT);
    m_currentOutput = m_currentOutput.SubString(end+m_promptPrefix.Length(),
                                                m_currentOutput.Length());
    end = m_currentOutput.Find(m_promptPrefix);
  }
  if (m_readingPrompt) return;
  wxString mth = wxT("</mth>");
  end = m_currentOutput.Find(mth);
  while (end > -1) {
    wxString o = m_currentOutput.Left(end);
    ConsoleAppend(o + mth, TEXTT);
    m_currentOutput = m_currentOutput.SubString(end+mth.Length(),
                                                m_currentOutput.Length());
    end = m_currentOutput.Find(mth);
  }
}

void wxMaxima::ReadPrompt()
{
  int end = m_currentOutput.Find(m_promptSuffix);
  if (end > -1) {
    m_readingPrompt = false;
    wxString o = m_currentOutput.Left(end);
    if (o != wxT("\n") && o.Length()) {
      ConsoleAppend(o, PROMPTT);
      if (o.StartsWith(wxT("\nMAXIMA>")))
        m_inLispMode = true;
      else
        m_inLispMode = false;
    }
    else
      SetStatusText(_("Ready for user input"), 1);
    m_currentOutput = m_currentOutput.SubString(end + m_promptSuffix.Length(),
                                                m_currentOutput.Length());
  }
}

void wxMaxima::ServerEvent(wxSocketEvent& event)
{
  switch (event.GetSocketEvent()) {
  case wxSOCKET_CONNECTION :
    {
      m_client = m_server->Accept(false);
      m_client->SetEventHandler(*this, socket_client_id);
      m_client->SetNotify(wxSOCKET_INPUT_FLAG|wxSOCKET_LOST_FLAG);
      m_client->Notify(true);
      m_isConnected = true;
#ifndef __WXMSW__
      ReadProcessOutput();
#endif
      SetupVariables();
    }
    break;
  case wxSOCKET_LOST:
    if (!m_closing)
      ConsoleAppend(wxT("\nSERVER: Lost socket connection ...\n"
                        "Restart maxima with 'Maxima->Restart maxima'.\n"),
                    ERRORT);
    m_pid = -1;
    GetMenuBar()->Enable(menu_interrupt_id, false);
    m_isConnected = false;
  default:
    break;
  }
}

#ifndef __WXMSW__
void wxMaxima::ReadProcessOutput()
{
  wxString o;
  while (m_process->IsInputAvailable()) {
    o += m_input->GetC();
  }
  int st = o.Find(wxT("Maxima 5.9"));
  if (st==-1)
    st = 0;
  FirstOutput(wxT("wxMaxima ")
              wxT(WXMAXIMA_VERSION)
              wxT(" http://wxmaxima.sourceforge.net\n") +
              o.SubString(st, o.Length()-1));
  m_inPrompt = true;
  SetStatusText(_("Ready for user input"), 1);
}
#endif

void wxMaxima::SetupVariables()
{
  SendMaxima(wxT(":lisp-quiet (setf *prompt-suffix* \"") +
             m_promptSuffix +
             wxT("\")"), false, false, false);
  SendMaxima(wxT(":lisp-quiet (setf *prompt-prefix* \"") +
             m_promptPrefix +
             wxT("\")"), false, false, false);
  SendMaxima(wxT(":lisp-quiet (setf $IN_NETMATH nil)"), false, false, false);
  SendMaxima(wxT(":lisp-quiet (setf $SHOW_OPENPLOT t)"), false, false, false);
#if defined (__WXMSW__)
  SendMaxima(wxT(":lisp-quiet ($load \"wxmathml.lisp\")"), false, false, false);
#else
  wxString prefix = wxT(PREFIX);
  SendMaxima(wxT(":lisp-quiet ($load \"") + prefix +
             wxT("/share/wxMaxima/wxmathml.lisp\")"), false, false, false);
#endif
}

bool wxMaxima::StartServer()
{
  SetStatusText(wxString::Format(_("Starting server on port %d"), m_port), 1);

  wxIPV4address addr;

  addr.AnyAddress();
  addr.Service(m_port);

  m_server = new wxSocketServer(addr);
  if (!m_server->Ok()) {
    delete m_server;
    m_isRunning = false;
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

bool wxMaxima::GuessConfiguration()
{
#if defined (__WXMSW__)
  wxString maxima = wxGetCwd();
  maxima.Replace(wxT("wxMaxima"), wxT("bin\\maxima.bat"));
  if (!wxFileExists(maxima))
    return false;
#else
  wxString maxima = wxT("maxima");
#endif

  wxConfig *config = (wxConfig *)wxConfig::Get();
  config->Write(wxT("maxima"), maxima);
  config->Write(wxT("parameters"), wxT(""));

  config->Flush();
  return true;
}

wxString wxMaxima::GetCommand()
{
  wxConfig *config = (wxConfig *)wxConfig::Get();
  wxString command, r;
  bool have_config = config->Read(wxT("maxima"), &command);

  // Test if we have correct configuration on Windows
#if defined (__WXMSW__)
  if (have_config && !wxFileExists(command))
    have_config = false;
#endif

  if (!have_config) {
    GuessConfiguration();
    have_config = config->Read(wxT("maxima"), &command);
  }

  if (!have_config) {
    wxMessageBox(_("wxMaxima could not find maxima!\n\n"
                   "Please configure wxMaxima with 'Edit->Configure'.\n"
                   "Then start maxima with 'Maxima->Restart maxima'."), _("Warning"),
                   wxOK|wxICON_EXCLAMATION);
    SetStatusText(_("Please configure wxMaxima with 'Edit->Configure'."));
    return wxT("");
  }

  config->Read(wxT("parameters"), &r);
  command = wxT("\"") + command + wxT("\" ") + r;
  return command;
}

bool wxMaxima::StartMaxima()
{
  if (m_isConnected) {
    KillMaxima();
//    m_client->Close();
    m_isConnected = false;
  }

  wxString command = GetCommand();

  if (command.Length()>0) {
#if defined(__WXMSW__)
    if (wxGetOsVersion() == wxWIN95) {
      wxString maximaPrefix = command.SubString(1, command.Length()-3);
      wxString sysPath;

      wxGetEnv(wxT("path"), &sysPath);
      maximaPrefix.Replace(wxT("\\bin\\maxima.bat"), wxT(""));

      wxSetEnv(wxT("maxima_prefix"), maximaPrefix);
      wxSetEnv(wxT("path"), maximaPrefix + wxT("\\bin;") + sysPath);

      command = maximaPrefix + wxT("\\lib\\maxima");
      if (!wxDirExists(command))
        return false;
      
      wxArrayString files;
      wxDir::GetAllFiles(command, &files, wxT("maxima.exe"));
      if (files.Count() == 0)
        return false;
      else {
        command = files[0];
        command.Append(wxString::Format(
           wxT(" -eval \"(maxima::start-server %d)\" -eval \"(run)\" -f"),
           m_port
        ));
      }
    }
    else
      command.Append(wxString::Format(wxT(" -s %d"), m_port));
    wxSetEnv(wxT("home"), wxGetHomeDir());
#else
    command.Append(wxString::Format(wxT(" -r \":lisp (setup-server %d)\""),
                   m_port));
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

void wxMaxima::OnProcessEvent(wxProcessEvent& event)
{
  if (!m_closing)
    SetStatusText(_("Maxima process terminated."), 1);
  delete m_process;
  m_process = NULL;
}

void wxMaxima::CleanUp()
{
  if (m_isConnected) {
    KillMaxima();
  }
  if (m_isRunning) {
    m_server->Destroy();
  }
}

void wxMaxima::ShowTip(bool force)
{
  bool ShowTips = true;
  int tipNum = 0;
  wxConfig *config = (wxConfig *)wxConfig::Get();
  config->Read(wxT("ShowTips"), &ShowTips);
  config->Read(wxT("tipNum"), &tipNum);
  if (!ShowTips && !force)
    return;
#if defined (__WXMSW__)
  wxString tips = wxGetCwd() + wxT("\\tips.txt");
#else
  wxString prefix = wxT(PREFIX);
  wxString tips = prefix + wxT("/share/wxMaxima/tips.txt");
#endif
  if (wxFileExists(tips)) {
    wxTipProvider *t = wxCreateFileTipProvider(tips, tipNum);
    ShowTips = wxShowTip(this, t, ShowTips);
    config->Write(wxT("ShowTips"), ShowTips);
    tipNum = t->GetCurrentTip();
    config->Write(wxT("tipNum"), tipNum);
    config->Flush();
    delete t;
  }
}

///////////////////////////
//
// EVENT HANDLING
//
///////////////////////////

void wxMaxima::PrintMenu(wxCommandEvent& event)
{
  switch(event.GetId()) {
    case menu_print:
    case tb_print:
    {
      wxPrintDialogData printDialogData(*m_printData);
      wxPrinter printer(&printDialogData);
      MathPrintout printout(_("Maxima session"));
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

void wxMaxima::UpdateMenus(wxUpdateUIEvent& event)
{
  wxMenuBar* menubar = GetMenuBar();
  menubar->Enable(menu_copy_from_console, m_console->CanCopy());
  menubar->Enable(menu_copy_lb_from_console, m_console->CanCopy());
  menubar->Enable(menu_copy_as_bitmap, m_console->CanCopy());
  menubar->Enable(menu_copy_to_file, m_console->CanCopy());
  menubar->Enable(menu_delete_selection, m_console->CanDeleteSelection());
  if (m_console->GetTree()!=NULL && m_supportPrinting)
    menubar->Enable(menu_print, true);
  else
    menubar->Enable(menu_print, false);
  int fontSize = 12;
  wxConfig::Get()->Read(wxT("fontSize"), &fontSize);
  if (fontSize<20)
    menubar->Enable(menu_inc_fontsize, true);
  else
    menubar->Enable(menu_inc_fontsize, false);
  if (fontSize>8)
    menubar->Enable(menu_dec_fontsize, true);
  else
    menubar->Enable(menu_dec_fontsize, false);
}

void wxMaxima::UpdateToolBar(wxUpdateUIEvent& event)
{
  wxToolBar * toolbar = GetToolBar();
  toolbar->EnableTool(tb_copy, m_console->CanCopy());
  toolbar->EnableTool(tb_delete, m_console->CanDeleteSelection());
  if (m_pid > 0)
    toolbar->EnableTool(tb_interrupt, true);
  else
    toolbar->EnableTool(tb_interrupt, false);
  if (m_console->GetTree()!=NULL && m_supportPrinting)
    toolbar->EnableTool(tb_print, true);
  else
    toolbar->EnableTool(tb_print, false);
}

wxString wxMaxima::GetDefaultEntry()
{
  wxString s = m_inputLine->GetValue();
  if (s.Length() == 0) {
    if (m_console->CanCopy())
      s = m_console->GetString();
    else
      s = wxT("%");
  }
  return s;
}

void wxMaxima::OnMonitorFile(wxCommandEvent& event)
{
  wxString file = wxFileSelector(_("Select package to load"), m_lastPath,
                                 wxT(""), wxT(""),
                                 _("Maxima package (*.mac)|*.mac|"
                                    "Lisp package (*.lisp)|*.lisp|All|*"),
                                 wxOPEN);
  if (file.Length()!=0) {
    m_monitorFile = file;
#if defined __WXMSW__
    m_monitorFile.Replace(wxT("\\"), wxT("/"));
#endif
    m_monitorTime = wxFileModificationTime(file);
    SetStatusText(wxT("Monitoring file ") + m_monitorFile);
  }
}

void wxMaxima::OnActivate(wxActivateEvent& event)
{
  if (m_monitorFile.Length() != 0) {
    if (m_monitorTime != wxFileModificationTime(m_monitorFile)) {
      if (m_inPrompt) {
        SendMaxima(wxT("load(\"") + m_monitorFile + wxT("\");"));
        SetStatusText(wxT("Reloaded file ") + m_monitorFile);
        m_monitorTime = wxFileModificationTime(m_monitorFile);
      }
    }
  }
  event.Skip();
}

void wxMaxima::OnSetFocus(wxFocusEvent& event)
{
  if (m_monitorFile.Length() != 0) {
    if (m_monitorTime != wxFileModificationTime(m_monitorFile)) {
      if (m_inPrompt) {
        SendMaxima(wxT("load(\"") + m_monitorFile + wxT("\");"));
        SetStatusText(wxT("Reloaded file ") + m_monitorFile);
        m_monitorTime = wxFileModificationTime(m_monitorFile);
      }
    }
  }
  event.Skip();
}

void wxMaxima::Interrupt(wxCommandEvent& event)
{
  if (m_pid < 0) {
    GetMenuBar()->Enable(menu_interrupt_id, false);
    return;
  }
#if defined (__WXMSW__)
  wxString path, maxima;
  wxArrayString out;
  wxConfig::Get()->Read(wxT("maxima"), &maxima);
  wxFileName::SplitPath(maxima, &path, NULL, NULL);
  wxString command = wxT("\"") + path;
  command += wxString::Format(wxT("\\winkill\" -INT %ld"), m_pid);
  wxExecute(command, out);
#else
  wxProcess::Kill(m_pid, wxSIGINT);
#endif
}

void wxMaxima::KillMaxima()
{
  m_process->Detach();
  if (m_pid < 0) {
    if (m_inLispMode)
      SendMaxima(wxT("($quit)"), false, false);
    else
      SendMaxima(wxT("quit();"), false, false);
    return;
  }
  wxProcess::Kill(m_pid, wxSIGKILL);
}

void wxMaxima::FileMenu(wxCommandEvent& event)
{
  wxString expr = GetDefaultEntry();
  wxString cmd;
#if defined __WXMSW__
  wxString b = wxT("\\");
  wxString f = wxT("/");
#endif
  switch (event.GetId()) {
  case tb_open:
    {
      wxString file = wxFileSelector(_("Select file to open"), m_lastPath,
									                   wxT(""), wxT(""),
                                     _("Maxima session (*.sav)|*.sav|"
                                       "Maxima package (*.mac)|*.mac|"
                                       "Lisp package (*.lisp)|*.lisp|"
                                       "Demo file (*.dem)|*.dem|"
                                       "All|*"),
                                     wxOPEN);
      if (file.Length()) {
        m_lastPath = wxPathOnly(file);
#if defined __WXMSW__
        file.Replace(b,f);
#endif
        if (file.Right(4) == wxT(".sav"))
          SendMaxima(wxT("loadfile(\"") + file + wxT("\")$"));
        else if (file.Right(4) == wxT(".dem"))
          SendMaxima(wxT("demo(\"") + file + wxT("\")$"));
        else
          SendMaxima(wxT("load(\"") + file + wxT("\")$"));
      }
    }
    break;
  case menu_open_id:
    {
      wxString file = wxFileSelector(_("Select file to open"), m_lastPath,
									                   wxT(""), wxT(""),
                                     _("Maxima session (*.sav)|*.sav|"
                                       "All|*"),
                                     wxOPEN);
      if (file.Length()) {
        m_lastPath = wxPathOnly(file);
#if defined __WXMSW__
        file.Replace(b,f);
#endif
        SendMaxima(wxT("loadfile(\"") + file + wxT("\")$"));
      }
    }
    break;
  case tb_save:
  case menu_save_id:
    {
      wxString file = wxFileSelector(_("Save to file"), m_lastPath,
                                     wxT("wxMaxima.sav"), wxT("sav"),
                                     _("Maxima session (*.sav)|*.sav|"
                                       "All|*"),
                                     wxSAVE|wxOVERWRITE_PROMPT);
      if (file.Length()) {
        m_lastPath = wxPathOnly(file);
        if (wxFileExists(file))
          wxRemoveFile(file);
#if defined __WXMSW__
        file.Replace(b,f);
#endif
        wxString cmd(wxT("save(\""));
        cmd.append(file);
        cmd.append(wxT("\", ALL);"));
        SendMaxima(cmd);
      }
    }
    break;
  case menu_export_html:
    {
      wxString file = wxFileSelector(_("Export to HTML file"), m_lastPath,
                                     wxT("index.html"), wxT("html"),
                                     _("HTML file (*.html)|*.html|"
                                       "All|*"),
                                     wxSAVE|wxOVERWRITE_PROMPT);
      if (file.Length()) {
        m_lastPath = wxPathOnly(file);
        if (file.Right(5) != wxT(".html"))
          file = file + wxT(".html");
        if (!m_console->ExportToHTML(file))
          wxMessageBox(_("Exporting to HTML failed!"), _("Error!"),
                       wxOK);
      }
    }
    break;
  case menu_load_id:
    {
      wxString file = wxFileSelector(_("Select package to load"), m_lastPath,
                                     wxT(""), wxT(""),
                                     _("Maxima package (*.mac)|*.mac|"
                                       "Lisp package (*.lisp)|*.lisp|All|*"),
                                     wxOPEN);
      if (file.Length()) {
        m_lastPath = wxPathOnly(file);
#if defined __WXMSW__
        file.Replace(b,f);
#endif
        wxString cmd(wxT("load(\""));
        cmd.append(file);
        cmd.append(wxT("\");"));
        SendMaxima(cmd);
      }
    }
    break;
  case menu_batch_id:
  case tb_batch:
    {
      wxString file = wxFileSelector(_("Select package to batch"), m_lastPath,
                                     wxT(""), wxT(""),
                                     _("Maxima file (*.mac)|*.mac|"
                                       "Demo file (*.dem)|*.dem|All|*"),
                                     wxOPEN);
      if (file.Length()) {
        m_lastPath = wxPathOnly(file);
#if defined __WXMSW__
        file.Replace(b,f);
#endif
        wxString cmd;
        if (file.Right(4) == wxT(".dem") || file.Right(5) == wxT(".demo"))
          cmd.append(wxT("demo(\""));
        else
          cmd.append(wxT("batch(\""));
        cmd.append(file);
        cmd.append(wxT("\");"));
        SendMaxima(cmd);
      }
    }
    break;
  case menu_select_file:
    {
      wxString file = wxFileSelector(_("Select a file"), m_lastPath,
                                     wxT(""), wxT(""),
                                     _("All|*|"
                                       "Maxima package (*.mac)|*.mac|"
                                       "Demo file (*.dem)|*.dem|"
                                       "Lisp file (*.lisp)|*.lisp"),
                                     wxOPEN);
      if (file.Length()) {
        m_lastPath = wxPathOnly(file);
#if defined __WXMSW__
        file.Replace(b,f);
#endif
        m_inputLine->WriteText(file);
        return;
      }
    }
    break;
  case menu_exit_id:
    Close();
    break;
  default:
    break;
  }
}

void wxMaxima::EditMenu(wxCommandEvent& event)
{
  switch (event.GetId()) {
  case menu_options_id:
  case tb_pref:
    {
      Config *configW = new Config(this,-1, _("Maxima configuration"));
      configW->Centre(wxBOTH);
      if (configW->ShowModal() == wxID_OK) {
        bool match = true;
        wxConfig::Get()->Read(wxT("matchParens"), &match);
        m_inputLine->SetMatchParens(match);
        m_console->Recalculate(false);
        m_console->Refresh();
      }
      configW->Destroy();
    }
    break;
  case menu_clear_screen:
    m_console->ClearWindow();
    ConsoleAppend(m_lastPrompt, PROMPTT);
    break;
  case tb_copy:
  case menu_copy_from_console:
    if (m_console->CanCopy())
      m_console->Copy();
    else if (m_inputLine->CanCopy())
      m_inputLine->Copy();
    break;
  case menu_copy_lb_from_console:
    if (m_console->CanCopy())
      m_console->Copy(true);
    break;
  case menu_copy_as_bitmap:
    if (m_console->CanCopy())
      m_console->CopyBitmap();
    break;
  case menu_selection_to_input:
    if (m_console->CanCopy())
      m_inputLine->WriteText((m_console->GetString()).Trim(false));
    return;
    break;
  case menu_copy_to_file:
  {
    wxString file = wxFileSelector(_("Save selection to file"), m_lastPath,
                                   wxT("image.png"), wxT("png"),
                                   _("PNG image (*.png)|*.png|"
                                     "JPEG image (*.jpg)|*.jpg|"
                                     "Windows bitmap (*.bmp)|*.bmp|"
                                     "X pixmap (*.xpm)|*.xpm"),
                                   wxSAVE|wxOVERWRITE_PROMPT);
    if (file.Length()) {
      m_console->CopyToFile(file);
      m_lastPath = wxPathOnly(file);
    }
  }
  case tb_delete:
  case menu_delete_selection:
    if (m_console->CanDeleteSelection())
      m_console->DeleteSelection();
    break;
  case menu_inc_fontsize:
    {
      int fontSize = 12;
      wxConfig::Get()->Read(wxT("fontSize"), &fontSize);
      if (fontSize<20) {
        fontSize++;
        wxConfig::Get()->Write(wxT("fontSize"), fontSize);
        m_console->Recalculate(false);
        m_console->Refresh();
      }
    }
    break;
  case menu_dec_fontsize:
    {
      int fontSize = 12;
      wxConfig::Get()->Read(wxT("fontSize"), &fontSize);
      if (fontSize>8) {
        fontSize--;
        wxConfig::Get()->Write(wxT("fontSize"), fontSize);
        m_console->Recalculate(false);
        m_console->Refresh();
      }
    }
    break;
  }
  m_inputLine->SetFocus();
}

void wxMaxima::MaximaMenu(wxCommandEvent& event)
{
  wxString expr = GetDefaultEntry();
  wxString cmd;
  wxString b = wxT("\\");
  wxString f = wxT("/");
  switch (event.GetId()) {
  case menu_restart_id:
    StartMaxima();
    break;
  case menu_soft_restart:
    SendMaxima(wxT("kill(all);"));
    break;
  case menu_functions:
    SendMaxima(wxT("functions;"));
    break;
  case menu_variables:
    SendMaxima(wxT("values;"));
    break;
  case menu_display:
    cmd = wxT("if display2d#false then display2d:false else display2d:true;");
    SendMaxima(cmd, false);
    break;
  case menu_texform:
    cmd = wxT("tex(") + expr + wxT(");");
    SendMaxima(cmd);
    break;
  case menu_time:
    cmd = wxT("if showtime#false then showtime:false else showtime:all;");
    SendMaxima(cmd, false);
    break;
  case menu_fun_def:
    cmd = GetTextFromUser(_("Show the definition of function:"),
                          _("Function"), wxT(""), this);
    if (cmd.Length()) {
      cmd = wxT("fundef(") + cmd + wxT(");");
      SendMaxima(cmd);
    }
    break;
  case menu_add_path:
    {
      if (m_lastPath.Length()==0)
        m_lastPath = wxGetHomeDir();
      wxString dir = wxDirSelector(_("Add dir to path:"), m_lastPath);
      if (dir.Length()) {
        m_lastPath = dir;
 #if defined (__WXMSW__)
        dir.Replace(wxT("\\"), wxT("/"));
 #endif
        cmd = wxT("file_search_maxima : cons(sconcat(\"") + dir +
              wxT("/###.{lisp,mac,mc}\"), file_search_maxima)$");
        SendMaxima(cmd);
      }
    }
    break;
  case menu_clear_var:
    cmd = GetTextFromUser(_("Delete variable(s):"), _("Delete"),
                          wxT("all"), this);
    if (cmd.Length()) {
      cmd = wxT("remvalue(") + cmd + wxT(");");
      SendMaxima(cmd);
    }
    break;
  case menu_clear_fun:
    cmd = GetTextFromUser(_("Delete function(s):"), _("Delete"),
                          wxT("all"), this);
    if (cmd.Length()) {
      cmd = wxT("remfunction(") + cmd + wxT(");");
      SendMaxima(cmd);
    }
    break;
  case button_subst:
    {
      SubstituteWiz *wiz = new SubstituteWiz(this, -1, _("Substitute"));
      wiz->SetValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        wxString val = wiz->GetValue();
        SendMaxima(val);
      }
      wiz->Destroy();
    }
    break;
  case button_long_input:
    {
      TextInput *wiz = new TextInput(this, -1, _("Text input"));
      if (expr.StartsWith(wxT("<ml>"))) {
        expr = expr.SubString(4, expr.Length());
        expr.Replace(wxT("<nl>"), wxT("\n"));
      }
      wiz->SetValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        wxString val = wiz->GetValue();
        val.Trim();
        val.Trim(false);
        if (!m_inLispMode && val.Last()!=';' && val.Last()!='$')
          val += wxT("$");
        SendMaxima(val);
      }
      wiz->Destroy();
    }
    break;
  default:
    break;
  }
  m_inputLine->SetFocus();
}

void wxMaxima::EquationsMenu(wxCommandEvent& event)
{
  wxString expr = GetDefaultEntry();
  wxString cmd;
  switch (event.GetId()) {
  case menu_allroots:
    cmd = wxT("allroots(") + expr + wxT(");");
    SendMaxima(cmd);
    break;
  case menu_realroots:
    cmd = wxT("realroots(") + expr +  wxT(");");
    SendMaxima(cmd);
    break;
  case button_solve:
  case menu_solve:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Solve equation(s):"), _("for variable(s):"),
                                 expr, wxT("x"), this, -1, _("Solve"), true);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        cmd = wxT("solve([") + wiz->GetValue1() + wxT("], [") +
              wiz->GetValue2() + wxT("]);");
        SendMaxima(cmd);
      }
      wiz->Destroy();
    }
    break;
  case button_solve_ode:
  case menu_solve_ode:
    {
      Gen3Wiz *wiz = new Gen3Wiz(_("Equation:"), _("function:"), _("variable:"),
                                 expr, wxT("y"), wxT("x"),
                                 this, -1, _("Solve ODE"));
      wiz->setValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        wxString val = wxT("ode2(") + wiz->GetValue1() + wxT(", ") +
          wiz->GetValue2() + wxT(", ") + wiz->GetValue3() + wxT(");");
        SendMaxima(val);
      }
      wiz->Destroy();
    }
    break;
  case menu_ivp_1:
    {
      Gen3Wiz *wiz = new Gen3Wiz(_("Solution:"), _("At point:"), _("the value is:"),
                                 expr, wxT("x="), wxT("y="),
                                 this, -1, _("IC1"), true);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        wxString val = wxT("ic1(") + wiz->GetValue1() + wxT(", ") +
          wiz->GetValue2() + wxT(", ") + wiz->GetValue3() + wxT(");");
        SendMaxima(val);
      }
      wiz->Destroy();
    }
    break;
  case menu_ivp_2:
    {
      Gen4Wiz *wiz = new Gen4Wiz(_("Solution:"), _("At point:"),
                                 _("the value is:"), _("the derivative is:"),
                                 expr, wxT("x="), wxT("y="), wxT("'diff(y,x)="),
                                 this, -1, _("IC2"), true);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        wxString val = wxT("ic2(") + wiz->GetValue1() + wxT(", ") +
          wiz->GetValue2() + wxT(", ") + wiz->GetValue3() +
          wxT(", ") + wiz->GetValue4() + wxT(");");
        SendMaxima(val);
      }
      wiz->Destroy();
    }
    break;
  case menu_bvp:
    {
      BC2Wiz *wiz = new BC2Wiz(this, -1, _("BC2"));
      wiz->SetValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        wxString val = wiz->GetValue();
        SendMaxima(val);
      }
      wiz->Destroy();
    }
    break;
  case menu_eliminate:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("From equations:"),
                                 _("eliminate variables:"), expr, wxT(""),
                                 this, -1, _("Eliminate"), true);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        cmd = wxT("eliminate([") + wiz->GetValue1() + wxT("],[")
          + wiz->GetValue2() + wxT("]);");
        SendMaxima(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_solve_algsys:
    {
      wxString sz = GetTextFromUser(_("Number of equations:"),
                                    _("Solve algebraic system"),
                                    wxT("3"), this);
      if (sz.Length()==0)
        return;
      long isz;
      if (!sz.ToLong(&isz) || isz<=0) {
        wxMessageBox(_("Not a valid number of equations!"), _("Error!"),
                     wxOK|wxICON_ERROR);
        return;
      }
      SysWiz *wiz = new SysWiz(this, -1, _("Solve algebraic system"), isz);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        cmd = wxT("algsys") + wiz->GetValue();
        SendMaxima(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_solve_lin:
    {
      wxString sz = GetTextFromUser(_("Number of equations:"),
                                    _("Solve linear system"),
                                    wxT("3"), this);
      if (sz.Length()==0)
        return;
      long isz;
      if (!sz.ToLong(&isz) || isz<=0) {
        wxMessageBox(_("Not a valid number of equations!"), _("Error!"),
                     wxOK|wxICON_ERROR);
        return;
      }
      SysWiz *wiz = new SysWiz(this, -1, _("Solve linear system"), isz);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        cmd = wxT("linsolve") + wiz->GetValue();
        SendMaxima(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_solve_de:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Solve equation(s):"), _("for function(s):"),
                                 expr, wxT("y(x)"),
                                 this, -1, _("Solve ODE"));
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        cmd = wxT("desolve([") + wiz->GetValue1() + wxT("],[")
          + wiz->GetValue2() + wxT("]);");
        SendMaxima(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_atvalue:
    {
      Gen3Wiz *wiz = new Gen3Wiz(_("Expression:"), _("at point:"),
                                 _("has value:"), expr, wxT("x=0"), wxT("0"),
                                 this, -1, _("Atvalue"));
      wiz->setValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        wxString val = wxT("atvalue(") + wiz->GetValue1() + wxT(", ")
                       + wiz->GetValue2() +
        wxT(", ") + wiz->GetValue3() + wxT(");");
        SendMaxima(val);
      }
      wiz->Destroy();
    }
    break;
  default:
    break;
  }
  m_inputLine->SetFocus();
}

void wxMaxima::AlgebraMenu(wxCommandEvent& event)
{
  wxString expr = GetDefaultEntry();
  wxString cmd;
  switch (event.GetId()) {
  case menu_invert_mat:
    cmd = wxT("invert(") + expr + wxT(");");
    SendMaxima(cmd);
    break;
  case menu_determinant:
    cmd = wxT("determinant(") + expr + wxT(");");
    SendMaxima(cmd);
    break;
  case menu_eigen:
    cmd = wxT("eigenvalues(") + expr + wxT(");");
    SendMaxima(cmd);
    break;
  case menu_eigvect:
    cmd = wxT("eigenvectors(") + expr + wxT(");");
    SendMaxima(cmd);
    break;
  case menu_adjoint_mat:
    cmd = wxT("adjoint(") + expr + wxT(");");
    SendMaxima(cmd);
    break;
  case menu_transpose:
    cmd = wxT("transpose(") + expr + wxT(");");
    SendMaxima(cmd);
    break;
  case menu_map_mat:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Map function:"), _("to matrix:"),
                                 wxT(""), expr,
                                 this, -1, _("Matrix map"));
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        cmd = wxT("matrixmap(") + wiz->GetValue1() + wxT(", ")
          + wiz->GetValue2() + wxT(");");
        SendMaxima(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_enter_mat:
    {
      MatDim *wiz = new MatDim(this, -1, _("Matrix"));
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        if (expr.Length() && expr != wxT("%"))
          cmd = expr;
        long w, h;
        int type = wiz->GetMatrixType();
        if (!(wiz->GetValue2()).ToLong(&w) ||
            !(wiz->GetValue1()).ToLong(&h) ||
             w <= 0 || h <= 0) {
              wxMessageBox(_("Not a valid matrix dimension!"), _("Error!"),
                           wxOK|wxICON_ERROR);
              return;
        }
        if (w != h)
          type = MATRIX_GENERAL;
        MatWiz *mwiz = new MatWiz(this, -1, _("Enter matrix"),
                                  type, w, h);
        mwiz->Centre(wxBOTH);
        if (mwiz->ShowModal() == wxID_OK) {
          cmd += mwiz->GetValue();
          SendMaxima(cmd);
        }
        mwiz->Destroy();
      }
      wiz->Destroy();
    }
    break;
  case menu_cpoly:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Char poly of:"), _("in variable:"),
                                 expr, wxT("x"),
                                 this, -1, _("Char poly"));
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        cmd = wxT("charpoly(") + wiz->GetValue1() + wxT(", ")
          + wiz->GetValue2() + wxT("), expand;");
        SendMaxima(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_gen_mat:
    {
      Gen3Wiz *wiz = new Gen3Wiz(_("From array:"), _("width:"), _("height:"),
                                 expr, wxT("3"), wxT("3"),
                                 this, -1, _("Generate Matrix"));
      wiz->setValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        wxString val = wxT("genmatrix(") + wiz->GetValue1() +
          wxT(", ") + wiz->GetValue2() +
          wxT(", ") + wiz->GetValue3() + wxT(");");
        SendMaxima(val);
      }
      wiz->Destroy();
    }
    break;
  case button_map:
  case menu_map:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Map function:"), _("to list:"),
                                 wxT(""), expr,
                                 this, -1, _("Map"));
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        cmd = wxT("map(") + wiz->GetValue1() + wxT(", ") + wiz->GetValue2() +
              wxT(");");
        SendMaxima(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_make_list:
    {
      Gen4Wiz *wiz = new Gen4Wiz(_("from expression:"), _("by variable:"),
                                 _("from:"), _("to:"),
                                 expr, wxT("k"), wxT("1"), wxT("10"),
                                 this, -1, _("Make list"));
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        cmd = wxT("makelist(") + wiz->GetValue1() + wxT(", ") +
          wiz->GetValue2() + wxT(", ") +
          wiz->GetValue3() + wxT(", ") +
          wiz->GetValue4() + wxT(");");
        SendMaxima(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_apply:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Apply function:"), _("to list:"),
                                 wxT("\"+\""), expr,
                                 this, -1, _("Apply"), true);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        cmd = wxT("apply(") + wiz->GetValue1() + wxT(", ")
          + wiz->GetValue2() + wxT(");");
        SendMaxima(cmd);
      }
      wiz->Destroy();
    }
    break;
  default:
    break;
  }
  m_inputLine->SetFocus();
}

void wxMaxima::SimplifyMenu(wxCommandEvent& event)
{
  wxString expr = GetDefaultEntry();
  wxString cmd;
  switch (event.GetId()) {
  case menu_nouns:
    cmd = wxT("ev(") + expr + wxT(", nouns);");
    SendMaxima(cmd);
    break;
  case button_ratsimp:
  case menu_ratsimp:
    cmd = wxT("ratsimp(") + expr + wxT(");");
    SendMaxima(cmd);
    break;
  case button_radcan:
  case menu_radsimp:
    cmd = wxT("radcan(") + expr + wxT(");");
    SendMaxima(cmd);
    break;
  case menu_to_fact:
    cmd = wxT("makefact(") + expr + wxT(");");
    SendMaxima(cmd);
    break;
  case menu_to_gamma:
    cmd = wxT("makegamma(") + expr + wxT(");");
    SendMaxima(cmd);
    break;
  case menu_factcomb:
    cmd = wxT("factcomb(") + expr + wxT(");");
    SendMaxima(cmd);
    break;
  case menu_factsimp:
    cmd = wxT("minfactorial(") + expr + wxT(");");
    SendMaxima(cmd);
    break;
  case menu_logcontract:
    cmd = wxT("logcontract(") + expr + wxT(");");
    SendMaxima(cmd);
    break;
  case menu_logexpand:
    cmd = expr + wxT(", logexpand=super;");
    SendMaxima(cmd);
    break;
  case button_expand:
  case menu_expand:
    cmd = wxT("expand(") + expr + wxT(");");
    SendMaxima(cmd);
    break;
  case button_factor:
  case menu_factor:
    cmd = wxT("factor(") + expr + wxT(");");
    SendMaxima(cmd);
    break;
  case menu_gfactor:
    cmd = wxT("gfactor(") + expr + wxT(");");
    SendMaxima(cmd);
    break;
  case button_trigreduce:
  case menu_trigreduce:
    cmd = wxT("trigreduce(") + expr + wxT(");");
    SendMaxima(cmd);
    break;
  case button_trigsimp:
  case menu_trigsimp:
    cmd = wxT("trigsimp(") + expr + wxT(");");
    SendMaxima(cmd);
    break;
  case button_trigexpand:
  case menu_trigexpand:
    cmd = wxT("trigexpand(") + expr + wxT(");");
    SendMaxima(cmd);
    break;
  case menu_trigrat:
    cmd = wxT("trigrat(") + expr + wxT(");");
    SendMaxima(cmd);
    break;
  case button_rectform:
  case menu_rectform:
    cmd = wxT("rectform(") + expr + wxT(");");
    SendMaxima(cmd);
    break;
  case menu_polarform:
    cmd = wxT("polarform(") + expr + wxT(");");
    SendMaxima(cmd);
    break;
  case menu_demoivre:
    cmd = wxT("demoivre(") + expr + wxT(");");
    SendMaxima(cmd);
    break;
  case menu_exponentialize:
    cmd = wxT("exponentialize(") + expr + wxT(");");
    SendMaxima(cmd);
    break;
  case menu_realpart:
    cmd = wxT("realpart(") + expr + wxT(");");
    SendMaxima(cmd);
    break;
  case menu_imagpart:
    cmd = wxT("imagpart(") + expr + wxT(");");
    SendMaxima(cmd);
    break;
  case menu_talg:
    cmd = wxT("algebraic : not(algebraic);");
    SendMaxima(cmd);
    break;
  case menu_tellrat:
    cmd = GetTextFromUser(_("Enter an equation for rational simplification:"),
                          _("Ratsimp"), wxT(""), this);
    if (cmd.Length()) {
      cmd = wxT("tellrat(") + cmd + wxT(");");
      SendMaxima(cmd);
    }
    break;
  case menu_modulus:
    cmd = GetTextFromUser(_("Calculate modulus:"),
                          _("Modulus"), wxT("false"), this);
    if (cmd.Length()) {
      cmd = wxT("modulus : ") + cmd + wxT(";");
      SendMaxima(cmd);
    }
    break;
  default:
    break;
  }
  m_inputLine->SetFocus();
}

void wxMaxima::CalculusMenu(wxCommandEvent& event)
{
  wxString expr = GetDefaultEntry();
  wxString cmd;
  switch (event.GetId()) {
  case menu_change_var:
    {
      Gen4Wiz *wiz = new Gen4Wiz(_("Integral/sum:"), _("old variable:"),
                                 _("new variable:"), _("equation:"),
                                 expr, wxT("x"), wxT("y"), wxT("y=x"),
                                 this, -1, _("Change variable"), true);
      wiz->setValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        wxString val = wxT("changevar(") + wiz->GetValue1() + wxT(", ") +
          wiz->GetValue4() + wxT(", ") + wiz->GetValue3() + wxT(", ") +
          wiz->GetValue2() + wxT(");");
        SendMaxima(val);
      }
      wiz->Destroy();
    }
    break;
  case menu_pade:
    {
      Gen3Wiz *wiz = new Gen3Wiz(_("Taylor series:"), _("num deg:"),
                                 _("denom deg:"), expr, wxT("4"), wxT("4"),
                                 this, -1, _("Pade approximation"));
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        wxString val = wxT("pade(") + wiz->GetValue1() + wxT(", ") +
          wiz->GetValue2() + wxT(", ") + wiz->GetValue3() + wxT(");");
        SendMaxima(val);
      }
      wiz->Destroy();
    }
    break;
  case menu_continued_fraction:
    cmd += wxT("cfdisrep(cf(") + expr + wxT("));");
    SendMaxima(cmd);
    break;
  case menu_lcm:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Polynomial 1:"), _("Polynomial 2:"),
                                 wxT(""), wxT(""),
                                 this, -1, _("LCM"), true);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        cmd = wxT("lcm(") + wiz->GetValue1() + wxT(", ")
          + wiz->GetValue2() + wxT(");");
        SendMaxima(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_gcd:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Polynomial 1:"), _("Polynomial 2:"),
                                 wxT(""), wxT(""),
                                 this, -1, _("GCD"), true);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        cmd = wxT("gcd(") + wiz->GetValue1() + wxT(", ")
          + wiz->GetValue2() + wxT(");");
        SendMaxima(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_divide:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Polynomial 1:"), _("Polynomial 2:"),
                                 expr, wxT(""),
                                 this, -1, _("Divide"), true);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        cmd = wxT("divide(") + wiz->GetValue1() + wxT(", ") +
          wiz->GetValue2() + wxT(");");
        SendMaxima(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_partfrac:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Expression:"), _("in variable:"),
                                 expr, wxT("n"),
                                 this, -1, _("Partial fractions"));
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        cmd = wxT("partfrac(") + wiz->GetValue1() + wxT(", ")
          + wiz->GetValue2() + wxT(");");
        SendMaxima(cmd);
      }
      wiz->Destroy();
    }
    break;
  case menu_risch:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Expression:"), _("by variable:"),
                                 expr, wxT("x"),
                                 this, -1, _("Integrate (risch)"));
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        cmd = wxT("risch(") + wiz->GetValue1() + wxT(", ")
          + wiz->GetValue2() + wxT(");");
        SendMaxima(cmd);
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
      if (wiz->ShowModal() == wxID_OK) {
        wxString val = wiz->GetValue();
        SendMaxima(val);
      }
      wiz->Destroy();
    }
    break;
  case menu_laplace:
    {
      Gen3Wiz *wiz = new Gen3Wiz(_("Expression:"), _("change var:"),
                                 _("to:"), expr, wxT("t"), wxT("s"),
                                 this, -1, _("Laplace"));
      wiz->setValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        wxString val = wxT("laplace(") + wiz->GetValue1() + wxT(", ")
          + wiz->GetValue2() +
          wxT(", ") + wiz->GetValue3() + wxT(");");
        SendMaxima(val);
      }
      wiz->Destroy();
    }
    break;
  case menu_ilt:
    {
      Gen3Wiz *wiz = new Gen3Wiz(_("Expression:"), _("change var:"),
                                 _("to:"), expr, wxT("s"), wxT("t"),
                                 this, -1, _("Inverse Laplace"));
      wiz->setValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        wxString val = wxT("ilt(") + wiz->GetValue1() + wxT(", ") +
          wiz->GetValue2() + wxT(", ") + wiz->GetValue3() + wxT(");");
        SendMaxima(val);
      }
      wiz->Destroy();
    }
    break;
  case button_diff:
  case menu_diff:
    {
      Gen3Wiz *wiz = new Gen3Wiz(_("Expression:"), _("in variable:"),
                                 _("times:"), expr, wxT("x"), wxT("1"),
                                 this, -1, _("Differentiate"));
      wiz->setValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        wxString val = wxT("diff(") + wiz->GetValue1() + wxT(", ") +
          wiz->GetValue2();
        if (wiz->GetValue3()!=wxT("1"))
          val += wxT(", ") + wiz->GetValue3();
        val += wxT(");");
        SendMaxima(val);
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
      if (wiz->ShowModal() == wxID_OK) {
        wxString val = wiz->GetValue();
        SendMaxima(val);
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
      if (wiz->ShowModal() == wxID_OK) {
        wxString val = wiz->GetValue();
        SendMaxima(val);
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
      if (wiz->ShowModal() == wxID_OK) {
        wxString val = wiz->GetValue();
        SendMaxima(val);
      }
      wiz->Destroy();
    }
    break;
  case menu_unsum:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Unsum expression:"), _("by variable:"),
                                 expr, wxT("n"),
                                 this, -1, _("Unsum"));
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        cmd = wxT("unsum(") + wiz->GetValue1() + wxT(", ")
          + wiz->GetValue2() + wxT(");");
        SendMaxima(cmd);
      }
      wiz->Destroy();
    }
    break;
  case button_product:
  case menu_product:
    {
      Gen4Wiz *wiz = new Gen4Wiz(_("Product:"), _("by variable:"), _("from:"),
                                 _("to:"), expr, wxT("k"), wxT("1"), wxT("n"),
                                 this, -1, _("Product"));
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        cmd = wxT("product(") + wiz->GetValue1() + wxT(", ") +
          wiz->GetValue2() + wxT(", ") +
          wiz->GetValue3() + wxT(", ") +
          wiz->GetValue4() + wxT(");");
        SendMaxima(cmd);
      }
      wiz->Destroy();
    }
    break;
  default:
    break;
  }
  m_inputLine->SetFocus();
}

void wxMaxima::PlotMenu(wxCommandEvent& event)
{
  wxString expr = GetDefaultEntry();
  wxString cmd;
  switch (event.GetId()) {
  case button_plot3:
  case gp_plot3:
    {
      Plot3DWiz *wiz = new Plot3DWiz(this, -1, _("Plot 3D"));
      wiz->SetValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        wxString val = wiz->GetValue();
        SendMaxima(val);
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
      if (wiz->ShowModal() == wxID_OK) {
        wxString val = wiz->GetValue();
        SendMaxima(val);
      }
      wiz->Destroy();
    }
    break;
  case menu_plot_format:
    {
      wxString format = GetTextFromUser(_("Enter new plot format:"),
                                        _("Plot format"),
                                        wxT("gnuplot"), this);
      if (format.Length()) {
        SendMaxima(wxT("set_plot_option(['plot_format, '") + format +
                   wxT("])$"));
      }
    }
  default:
    break;
  }
  m_inputLine->SetFocus();
}

void wxMaxima::NumericalMenu(wxCommandEvent& event)
{
  wxString expr = GetDefaultEntry();
  wxString cmd;
  switch (event.GetId()) {
  case menu_to_float:
    cmd = wxT("float(") + expr + wxT(");");
    SendMaxima(cmd);
    break;
  case menu_to_bfloat:
    cmd = wxT("bfloat(") + expr + wxT(");");
    SendMaxima(cmd);
    break;
  case menu_num_out:
    cmd = wxT("if numer#false then numer:false else numer:true;");
    SendMaxima(cmd, false);
    break;
  case menu_set_precision:
    cmd = GetTextFromUser(_("Enter new precision:"), _("Precision"),
                          wxT("16"), this);
    if (cmd.Length()) {
      cmd = wxT("fpprec : ") + cmd + wxT(";");
      SendMaxima(cmd);
    }
    break;
  default:
    break;
  }
  m_inputLine->SetFocus();
}

void wxMaxima::HelpMenu(wxCommandEvent& event)
{
  wxString expr = GetDefaultEntry();
  wxString cmd;
  switch (event.GetId()) {
  case menu_about_id:
    wxMessageBox(wxString::Format(
                 _("wxMaxima is a wxWidgets interface for the\n"
                   "computer algebra system MAXIMA.\n"
                   "\nVersion: %s."
                   "\nReleased: %s."
                   "\nLicense: GPL.\n"
                   "\n%s\n%s"),
                 wxT(WXMAXIMA_VERSION), wxT(WXMAXIMA_DATE),
                 wxT("http://wxmaxima.sourceforge.net/"),
                 wxT("http://maxima.sourceforge.net/")),
                 _("About wxMaxima"), wxOK|wxICON_INFORMATION);
    break;
  case menu_help_id:
  case tb_help:
    {
      wxString filename;
#ifndef __WXMSW__
      filename = wxT(PREFIX);
      filename += wxT("/share/wxMaxima/");
#else
      filename = wxGetCwd() + wxT("\\");
#endif
      if (!wxFileExists(filename + wxT("intro.zip")) ||
          !wxFileExists(filename + wxT("docs.zip")))
        wxMessageBox(_("wxMaxima could not find help files."
                       "\n\nPlease check your installation."),
                     _("Error"), wxICON_ERROR|wxOK);
      else {
        wxHtmlHelpController *help = new wxHtmlHelpController;
        help->AddBook(filename + wxT("docs.zip"));
        help->AddBook(filename + wxT("intro.zip"));
        if (expr == wxT("%"))
          help->DisplayContents();
        else
          help->KeywordSearch(expr);
      }
    }
    return;
  case menu_describe:
    if (expr == wxT("%"))
      cmd = GetTextFromUser(_("Show the description of command/variable:"),
                            _("Describe"), wxT(""), this);
    else {
      cmd = expr;
      m_inputLine->SetValue(wxT(""));
    }
    if (cmd.Length()) {
      cmd = wxT("? ") + cmd + wxT(";");
      SendMaxima(cmd, false);
    }
    break;
  case menu_example:
    if (expr == wxT("%"))
      cmd = GetTextFromUser(_("Show an example for the command:"), _("Example"),
                            wxT(""), this);
    else {
      cmd = expr;
      m_inputLine->SetValue(wxT(""));
    }
    if (cmd.Length()) {
      cmd = wxT("example(") + cmd + wxT(");");
      SendMaxima(cmd, false);
    }
    break;
  case menu_apropos:
    if (expr == wxT("%"))
      cmd = GetTextFromUser(_("Show all commands similar to:"), _("Apropos"),
                            wxT(""), this);
    else {
      cmd = expr;
      m_inputLine->SetValue(wxT(""));
    }
    if (cmd.Length()) {
      cmd = wxT("apropos(\"") + cmd + wxT("\");");
      SendMaxima(cmd, false);
    }
    break;
  case menu_show_tip:
    ShowTip(true);
    break;
  case menu_build_info:
    SendMaxima(wxT("build_info()$"), false);
    break;
  case menu_bug_report:
    SendMaxima(wxT("bug_report()$"), false);
    break;
  default:
    break;
  }
}

void wxMaxima::OnClose(wxCloseEvent& event)
{
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
  if (m_lastPath.Length()>0)
    config->Write(wxT("lastPath"), m_lastPath);
  m_closing = true;
  CleanUp();
  Destroy();
}

BEGIN_EVENT_TABLE(wxMaxima, wxFrame)
  EVT_TEXT_ENTER(input_line_id, wxMaxima::EnterCommand)
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
  EVT_BUTTON(button_long_input, wxMaxima::MaximaMenu)
  EVT_BUTTON(button_radcan, wxMaxima::SimplifyMenu)
  EVT_BUTTON(button_subst, wxMaxima::MaximaMenu)
  EVT_BUTTON(button_plot2, wxMaxima::PlotMenu)
  EVT_BUTTON(button_plot3, wxMaxima::PlotMenu)
  EVT_BUTTON(button_map, wxMaxima::AlgebraMenu)
  EVT_BUTTON(button_rectform, wxMaxima::SimplifyMenu)
  EVT_BUTTON(button_enter, wxMaxima::EnterCommand)
  EVT_MENU(menu_polarform, wxMaxima::SimplifyMenu)
  EVT_MENU(menu_restart_id, wxMaxima::MaximaMenu)
  EVT_MENU(menu_exit_id, wxMaxima::FileMenu)
  EVT_MENU(menu_about_id, wxMaxima::HelpMenu)
  EVT_MENU(menu_save_id, wxMaxima::FileMenu)
  EVT_MENU(menu_load_id, wxMaxima::FileMenu)
  EVT_MENU(menu_monitor_file, wxMaxima::OnMonitorFile)
  EVT_MENU(menu_batch_id, wxMaxima::FileMenu)
  EVT_MENU(menu_select_file, wxMaxima::FileMenu)
  EVT_MENU(menu_functions, wxMaxima::MaximaMenu)
  EVT_MENU(menu_variables, wxMaxima::MaximaMenu)
  EVT_MENU(menu_options_id, wxMaxima::EditMenu)
  EVT_MENU(menu_sconsole_id, wxMaxima::FileMenu)
  EVT_MENU(menu_export_html, wxMaxima::FileMenu)
  EVT_MENU(menu_help_id, wxMaxima::HelpMenu)
  EVT_MENU(menu_bug_report, wxMaxima::HelpMenu)
  EVT_MENU(menu_build_info, wxMaxima::HelpMenu)
  EVT_MENU(menu_interrupt_id, wxMaxima::Interrupt)
  EVT_MENU(menu_open_id, wxMaxima::FileMenu)
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
  EVT_MENU(menu_fun_def, wxMaxima::MaximaMenu)
  EVT_MENU(menu_describe, wxMaxima::HelpMenu)
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
  EVT_MENU(menu_show_tip, wxMaxima::HelpMenu)
  EVT_MENU(menu_trigrat, wxMaxima::SimplifyMenu)
  EVT_MENU(menu_solve_de, wxMaxima::EquationsMenu)
  EVT_MENU(menu_atvalue, wxMaxima::EquationsMenu)
  EVT_MENU(menu_sum, wxMaxima::CalculusMenu)
  EVT_MENU(menu_unsum, wxMaxima::CalculusMenu)
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
  EVT_MENU(menu_clear_screen, wxMaxima::EditMenu)
  EVT_MENU(menu_copy_from_console, wxMaxima::EditMenu)
  EVT_MENU(menu_copy_lb_from_console, wxMaxima::EditMenu)
  EVT_MENU(menu_delete_selection, wxMaxima::EditMenu)
  EVT_MENU(menu_goto_input, wxMaxima::EditMenu)
  EVT_MENU(menu_texform, wxMaxima::MaximaMenu)
  EVT_MENU(menu_to_fact, wxMaxima::SimplifyMenu)
  EVT_MENU(menu_to_gamma, wxMaxima::SimplifyMenu)
  EVT_MENU(menu_goto_input, wxMaxima::MaximaMenu)
  EVT_MENU(menu_print, wxMaxima::PrintMenu)
  EVT_MENU(menu_inc_fontsize, wxMaxima::EditMenu)
  EVT_MENU(menu_dec_fontsize, wxMaxima::EditMenu)
  EVT_MENU(menu_copy_as_bitmap, wxMaxima::EditMenu)
  EVT_MENU(menu_copy_to_file, wxMaxima::EditMenu)
  EVT_MENU(menu_selection_to_input, wxMaxima::EditMenu)
  EVT_TOOL(tb_open, wxMaxima::FileMenu)
  EVT_TOOL(tb_batch, wxMaxima::FileMenu)
  EVT_TOOL(tb_save, wxMaxima::FileMenu)
  EVT_TOOL(tb_copy, wxMaxima::EditMenu)
  EVT_TOOL(tb_delete, wxMaxima::EditMenu)
  EVT_TOOL(tb_print, wxMaxima::PrintMenu)
  EVT_TOOL(tb_pref, wxMaxima::EditMenu)
  EVT_TOOL(tb_interrupt, wxMaxima::Interrupt)
  EVT_TOOL(tb_help, wxMaxima::HelpMenu)
  EVT_SOCKET(socket_server_id, wxMaxima::ServerEvent)
  EVT_SOCKET(socket_client_id, wxMaxima::ClientEvent)
  EVT_UPDATE_UI(menu_copy_from_console, wxMaxima::UpdateMenus)
  EVT_UPDATE_UI(menu_copy_lb_from_console, wxMaxima::UpdateMenus)
  EVT_UPDATE_UI(menu_inc_fontsize, wxMaxima::UpdateMenus)
  EVT_UPDATE_UI(menu_dec_fontsize, wxMaxima::UpdateMenus)
  EVT_UPDATE_UI(menu_print, wxMaxima::UpdateMenus)
  EVT_UPDATE_UI(menu_copy_as_bitmap, wxMaxima::UpdateMenus)
  EVT_UPDATE_UI(menu_copy_to_file, wxMaxima::UpdateMenus)
  EVT_UPDATE_UI(tb_print, wxMaxima::UpdateToolBar)
  EVT_UPDATE_UI(tb_copy, wxMaxima::UpdateToolBar)
  EVT_UPDATE_UI(tb_delete, wxMaxima::UpdateToolBar)
  EVT_UPDATE_UI(tb_interrupt, wxMaxima::UpdateToolBar)
//  EVT_UPDATE_UI(menu_selection_to_input, wxMaxima::UpdateMenus)
  EVT_CLOSE(wxMaxima::OnClose)
  EVT_ACTIVATE(wxMaxima::OnActivate)
  EVT_SET_FOCUS(wxMaxima::OnSetFocus)
  EVT_END_PROCESS(maxima_process_id, wxMaxima::OnProcessEvent)
END_EVENT_TABLE()
