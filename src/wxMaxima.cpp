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
#include "IC1Wiz.h"
#include "IC2Wiz.h"
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
#include <wx/fs_zip.h>
#include <wx/html/helpctrl.h>
#include <wx/tokenzr.h>
#include <wx/mimetype.h>
#include <wx/dynlib.h>

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

  wxFileSystem::AddHandler(new wxZipFSHandler);

  wxConfigBase* config = wxConfig::Get();
  config->Read(wxT("lastPath"), &m_lastPath);

  m_lastPrompt = wxEmptyString;

  m_console->SetDropTarget(new FileDrop(this, m_inputLine, DND_LOAD));
  m_inputLine->SetDropTarget(new FileDrop(this, m_inputLine, DND_WRITE));

  checkForPrintingSupport();

  m_printData = new wxPrintData;
  m_printData->SetQuality(wxPRINT_QUALITY_HIGH);
  m_inputLine->SetFocus();
  m_closing = false;
  wxInitAllImageHandlers();
}


wxMaxima::~wxMaxima()
{
  if (m_client != NULL)
    m_client->Destroy();
  delete m_printData;
}

void wxMaxima::checkForPrintingSupport()
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

void wxMaxima::initSession()
{
  while (!startServer()) {
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
  startMaxima();
}

wxString wxMaxima::clearWhitespaces(wxString s)
{
  wxString t(s);
  t.Replace(wxT("\n"), wxT(""));
  while (t.Find(wxT(" <")) > -1)
    t.Replace(wxT(" <"), wxT("<"));
  while (t.Find(wxT("> ")) > -1)
    t.Replace(wxT("> "), wxT(">"));
  return t;
}

void wxMaxima::firstOutput(wxString s)
{
  wxConfigBase* config = wxConfig::Get();
  bool showHeader = true;
  config->Read(wxT("showHeader"), &showHeader);

  int start = s.Find(m_firstPrompt);
  m_console->ClearWindow();

  if (showHeader)
    consoleAppend(s.SubString(0, start-1), TEXTT);
  consoleAppend(m_firstPrompt, PROMPTT);
}

///////////////////////////
//
// MAXIMA INTERACTION
//
///////////////////////////

void wxMaxima::consoleAppend(wxString s, int type)
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
    SetStatusText(_("Parsing output"));
  if (type == TEXTT) {
    while (s.Length()>0) {
      int start = s.Find(wxT("<mth"));
      if (start == -1) {
        s.Replace(wxT("&"), wxT("&amp;"));   // This is not in xml formated text
        s.Replace(wxT(">"), wxT("&gt;"));    // so it is safe to do some
        s.Replace(wxT("<"), wxT("&lt;"));    // substitutions
        t = s;
        t.Trim();
        t.Trim(false);
        if (t.Length())
          doRawConsoleAppend(s, TEXTT);
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
        pre.Replace(wxT("&"), wxT("&amp;"));
        pre.Replace(wxT(">"), wxT("&gt;"));
        pre.Replace(wxT("<"), wxT("&lt;"));
        if (pre1.Length()) {
          doRawConsoleAppend(pre, TEXTT);
          doConsoleAppend(wxT("<span>") + clearWhitespaces(rest) +
                          wxT("</span>"), type, false);
        }
        else {
          doConsoleAppend(wxT("<span>") + clearWhitespaces(rest) +
                          wxT("</span>"), type, false);
        }
        s = s.SubString(end+1, s.Length());
      }
    }
  }
  else if (type == INPUTT) {
    // This can be replaced - so we dont confuse libxml
    s.Replace(wxT("&"), wxT("&amp;"));
    s.Replace(wxT("<"), wxT("&lt;"));
    s.Replace(wxT(">"), wxT("&gt;"));
    // Break long lines
    unsigned int i=0;
    int j=0;
    wxString breaks = wxT(" *+-/()[]:=,");
    while (i<s.Length()) {
      if (j>70 && breaks.Find(s[i])>-1) {
        s = s.SubString(0, i) + wxT("\n") + s.SubString(i+1, s.Length());
        j = 0;
        i = i;
      }
      else if (s[i] == '\n')
        j = 0;
      else
        j++;
      i++;
    }
    // Append the input
    doRawConsoleAppend(s, type, false);
  }
  else if (type == PROMPTT) {
    SetStatusText(_("Ready for user input"));
    m_lastPrompt = s;
    s = clearWhitespaces(s);
    if (s.StartsWith(wxT("(%i")) || s.StartsWith(wxT("MAXIMA>"))) {
      m_inPrompt = true;
      type = MPROMPTT;
    }
    doConsoleAppend(wxT("<span>") + clearWhitespaces(s) + wxT("</span>"),
                    type, true);
  }
  else if (type == ERRORT) {
    doRawConsoleAppend(s, ERRORT);
  }
  else {
    doConsoleAppend(wxT("<span>") + clearWhitespaces(s) + wxT("</span>"),
                    type, false);
  }
}

void wxMaxima::doConsoleAppend(wxString s, int type, bool newLine,
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

void wxMaxima::doRawConsoleAppend(wxString s, int type, bool newLine)
{
  wxStringTokenizer tokens(s, wxT("\n"));
  int count = 0;
  while(tokens.HasMoreTokens()) {
    wxString line = wxT("<span>") + tokens.GetNextToken() + wxT("</span>");
    if (tokens.HasMoreTokens()) {
      if (count == 0)
        doConsoleAppend(line, type, newLine, false);
      else
        doConsoleAppend(line, type, true, false);
    }
    else {
      if (count == 0)
        doConsoleAppend(line, type, newLine, true);
      else
        doConsoleAppend(line, type, true, true);
    }
    count++;
  }
}

void wxMaxima::sendMaxima(wxString s, bool clear, bool out, bool silent)
{
  if (!m_isConnected) {
    consoleAppend(wxT("\nNot connected to maxima!\n"), ERRORT);
    return;
  }
  if (s.StartsWith(wxT("<ml>"))) {
    s = s.SubString(4, s.Length());
    s.Replace(wxT("<nl>"), wxT("\n"));
  }
  if (clear)
    m_inputLine->SetValue(wxT(""));
  if (out)
    consoleAppend(s, INPUTT);
  if (silent) {
    m_inputLine->addToHistory(s);
    SetStatusText(_("Maxima is calculating"));
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
  m_client->Write(s.mb_str(*wxConvCurrent), s.Length());
#endif
}

void wxMaxima::enterCommand(wxCommandEvent& event)
{
  wxString input = m_inputLine->GetValue();
  input.Trim();
  input.Trim(false);
  if (!m_inLispMode && input.Last() != ';' && input.Last()!='$')
    input.Append(';');
  sendMaxima(input);
  m_inputLine->Clear();
  m_inputLine->SetFocus();
}

void wxMaxima::clientEvent(wxSocketEvent& event)
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
        SetStatusText(_("Reading maxima output"));
        m_dispReadOut = true;
      }

      if (m_first && m_currentOutput.Find(m_firstPrompt) > -1)
        readFirstPrompt();

      readMath();

      readPrompt();
    }
    break;
  case wxSOCKET_LOST:
    if (!m_closing)
      consoleAppend(wxT("\nCLIENT: Lost socket connection ...\n"
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

void wxMaxima::readFirstPrompt()
{
#if defined(__WXMSW__)
  int start = m_currentOutput.Find(wxT("Maxima 5.9"));
  if (start==-1)
    start = 0;
  firstOutput(wxT("wxMaxima ")
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
  SetStatusText(_("Ready for user input"));
  m_currentOutput = wxT("");
}

void wxMaxima::readMath()
{
  int end = m_currentOutput.Find(m_promptPrefix);
  while (end > -1) {
    m_readingPrompt = true;
    wxString o = m_currentOutput.Left(end);
    consoleAppend(o, TEXTT);
    m_currentOutput = m_currentOutput.SubString(end+m_promptPrefix.Length(),
                                                m_currentOutput.Length());
    end = m_currentOutput.Find(m_promptPrefix);
  }
  if (m_readingPrompt) return;
  wxString mth = wxT("</mth>");
  end = m_currentOutput.Find(mth);
  while (end > -1) {
    wxString o = m_currentOutput.Left(end);
    consoleAppend(o + wxT("</mth>"), TEXTT);
    m_currentOutput = m_currentOutput.SubString(end+mth.Length(),
                                                m_currentOutput.Length());
    end = m_currentOutput.Find(mth);
  }
}

void wxMaxima::readPrompt()
{
  int end = m_currentOutput.Find(m_promptSuffix);
  if (end > -1) {
    m_readingPrompt = false;
    wxString o = m_currentOutput.Left(end);
    if (o != wxT("\n") && o.Length()) {
      consoleAppend(o, PROMPTT);
      if (o.StartsWith(wxT("\nMAXIMA>")))
        m_inLispMode = true;
      else
        m_inLispMode = false;
    }
    else
      SetStatusText(_("Ready for user input"));
    m_currentOutput = m_currentOutput.SubString(end + m_promptSuffix.Length(),
                                                m_currentOutput.Length());
  }
}

void wxMaxima::serverEvent(wxSocketEvent& event)
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
      readProcessOutput();
#endif
      setupVariables();
    }
    break;
  case wxSOCKET_LOST:
    if (!m_closing)
      consoleAppend(wxT("\nSERVER: Lost socket connection ...\n"
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
void wxMaxima::readProcessOutput()
{
  wxString o;
  while (m_process->IsInputAvailable()) {
    o += m_input->GetC();
  }
  int st = o.Find(wxT("Maxima 5.9"));
  if (st==-1)
    st = 0;
  firstOutput(wxT("wxMaxima ")
              wxT(WXMAXIMA_VERSION)
              wxT(" http://wxmaxima.sourceforge.net\n") +
              o.SubString(st, o.Length()-1));
  m_inPrompt = true;
  SetStatusText(_("Ready for user input"));
}
#endif

void wxMaxima::setupVariables()
{
  sendMaxima(wxT(":lisp-quiet (setf *prompt-suffix* \"") +
             m_promptSuffix +
             wxT("\")"), false, false, false);
  sendMaxima(wxT(":lisp-quiet (setf *prompt-prefix* \"") +
             m_promptPrefix +
             wxT("\")"), false, false, false);
  sendMaxima(wxT(":lisp-quiet (setf $IN_NETMATH nil)"), false, false, false);
  sendMaxima(wxT(":lisp-quiet (setf $SHOW_OPENPLOT t)"), false, false, false);
#if defined (__WXMSW__)
  sendMaxima(wxT(":lisp-quiet ($load \"wxmathml.lisp\")"), false, false, false);
#else
  wxString prefix = wxT(PREFIX);
  sendMaxima(wxT(":lisp-quiet ($load \"") + prefix +
             wxT("/share/wxMaxima/wxmathml.lisp\")"), false, false, false);
#endif
}

bool wxMaxima::startServer()
{
  SetStatusText(wxString::Format(wxT("Starting server on port %d"), m_port));

  wxIPV4address addr;

  addr.AnyAddress();
  addr.Service(m_port);

  m_server = new wxSocketServer(addr);
  if (!m_server->Ok()) {
    delete m_server;
    m_isRunning = false;
    return false;
  }
  m_server->SetEventHandler(*this, socket_server_id);
  m_server->SetNotify(wxSOCKET_CONNECTION_FLAG);
  m_server->Notify(true);

  m_isConnected = false;
  m_isRunning = true;
  return m_isRunning;
}

bool wxMaxima::guessConfiguration()
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

wxString wxMaxima::getCommand()
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
    guessConfiguration();
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

bool wxMaxima::startMaxima()
{
  if (m_isConnected) {
    killMaxima();
//    m_client->Close();
    m_isConnected = false;
  }

  wxString command = getCommand();

  if (command.Length()>0) {
#if defined(__WXMSW__)
    if (wxGetOsVersion() == wxWIN95) {
      wxString maximaPrefix = command.SubString(1, command.Length()-3);
      wxString sysPath;

      wxGetEnv(wxT("path"), &sysPath);
      maximaPrefix.Replace(wxT("\\bin\\maxima.bat"), wxT(""));

      wxSetEnv(wxT("maxima_prefix"), maximaPrefix);
      wxSetEnv(wxT("path"), maximaPrefix + wxT("\\bin;") + sysPath);

      command.Replace(wxT("bin\\maxima.bat"),
                      wxT("lib\\maxima\\5.9.1\\binary-gcl\\maxima.exe"));
      command.Append(wxString::Format(
        wxT(" -eval \"(maxima::start-server %d)\" -eval \"(run)\" -f"), m_port));
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
    SetStatusText(_("Starting maxima..."));
    wxExecute(command, wxEXEC_ASYNC, m_process);
    m_input = m_process->GetInputStream();
    SetStatusText(_("Maxima started. Waiting for connection..."));
  }
  else
    return false;
  return true;
}

void wxMaxima::onProcessEvent(wxProcessEvent& event)
{
  if (!m_closing)
    SetStatusText(_("Maxima process terminated."));
}

void wxMaxima::cleanUp()
{
  if (m_isConnected) {
    killMaxima();
  }
  if (m_isRunning) {
    m_server->Destroy();
  }
}

void wxMaxima::showTip(bool force)
{
  bool showTips = true;
  int tipNum = 0;
  wxConfig *config = (wxConfig *)wxConfig::Get();
  config->Read(wxT("showTips"), &showTips);
  config->Read(wxT("tipNum"), &tipNum);
  if (!showTips && !force)
    return;
#if defined (__WXMSW__)
  wxString tips = wxGetCwd() + wxT("\\tips.txt");
#else
  wxString prefix = wxT(PREFIX);
  wxString tips = prefix + wxT("/share/wxMaxima/tips.txt");
#endif
  if (wxFileExists(tips)) {
    wxTipProvider *t = wxCreateFileTipProvider(tips, tipNum);
    showTips = wxShowTip(this, t, showTips);
    config->Write(wxT("showTips"), showTips);
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

void wxMaxima::printMenu(wxCommandEvent& event)
{
  switch(event.GetId()) {
    case menu_print:
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
    case menu_print_setup:
    {
      wxPrintDialogData printDialogData(*m_printData);
      wxPrintDialog printerDialog(this, &printDialogData);
      printerDialog.GetPrintDialogData().SetSetupDialog(true);
      printerDialog.ShowModal();
      (*m_printData) = printerDialog.GetPrintDialogData().GetPrintData();
      break;
    }
  }
}

void wxMaxima::updateMenus(wxUpdateUIEvent& event)
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

wxString wxMaxima::getDefaultEntry()
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

void wxMaxima::onMonitorFile(wxCommandEvent& event)
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

void wxMaxima::onActivate(wxActivateEvent& event)
{
  if (m_monitorFile.Length() != 0) {
    if (m_monitorTime != wxFileModificationTime(m_monitorFile)) {
      if (m_inPrompt) {
        sendMaxima(wxT("load(\"") + m_monitorFile + wxT("\");"));
        SetStatusText(wxT("Reloaded file ") + m_monitorFile);
        m_monitorTime = wxFileModificationTime(m_monitorFile);
      }
    }
  }
}

void wxMaxima::onSetFocus(wxFocusEvent& event)
{
  if (m_monitorFile.Length() != 0) {
    if (m_monitorTime != wxFileModificationTime(m_monitorFile)) {
      if (m_inPrompt) {
        sendMaxima(wxT("load(\"") + m_monitorFile + wxT("\");"));
        SetStatusText(wxT("Reloaded file ") + m_monitorFile);
        m_monitorTime = wxFileModificationTime(m_monitorFile);
      }
    }
  }
  event.Skip();
}

void wxMaxima::interrupt(wxCommandEvent& event)
{
  if (m_pid < 0) {
    GetMenuBar()->Enable(menu_interrupt_id, false);
    return;
  }
#if defined (__WXMSW__)
  wxConfig *config = (wxConfig *)wxConfig::Get();
  wxString path, maxima;
  wxArrayString out;
  config->Read(wxT("maxima"), &maxima);
  wxFileName::SplitPath(maxima, &path, NULL, NULL);
  wxString command = wxT("\"") + path;
  command += wxString::Format(wxT("\\winkill\" -INT %ld"), m_pid);
  wxExecute(command, out);
#else
  wxProcess::Kill(m_pid, wxSIGINT);
#endif
}

void wxMaxima::killMaxima()
{
  if (m_pid < 0) {
    if (m_inLispMode)
      sendMaxima(wxT("($quit)"), false, false);
    else
      sendMaxima(wxT("quit();"), false, false);
    return;
  }
  wxProcess::Kill(m_pid, wxSIGKILL);
}

void wxMaxima::fileMenu(wxCommandEvent& event)
{
  wxString expr = getDefaultEntry();
  wxString cmd;
  wxString b = wxT("\\");
  wxString f = wxT("/");
  switch (event.GetId()) {
  case menu_open_id:
    {
      wxString file = wxFileSelector(_("Select file to open"), m_lastPath,
									                   wxT(""), wxT(""),
                                     _("Maxima session (*.sav)|*.sav|"
                                       "Maxima package (*.mac)|*.mac|"
                                       "All|*"),
                                     wxOPEN);
      if (file.Length()) {
        m_lastPath = wxPathOnly(file);
        file.Replace(b,f);
        sendMaxima(wxT("loadfile(\"") + file + wxT("\")$"));
      }
    }
    break;
  case menu_save_id:
    {
      wxString file = wxFileSelector(_("Save to file"), m_lastPath,
                                     wxT(""), wxT(""),
                                     _("Maxima session (*.sav)|*.sav|"
                                       "All|*"),
                                     wxSAVE|wxOVERWRITE_PROMPT);
      if (file.Length()) {
        m_lastPath = wxPathOnly(file);
        if (wxFileExists(file))
          wxRemoveFile(file);
        file.Replace(b,f);
        wxString cmd(wxT("save(\""));
        cmd.append(file);
        cmd.append(wxT("\", ALL);"));
        sendMaxima(cmd);
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
        file.Replace(b,f);
        wxString cmd(wxT("load(\""));
        cmd.append(file);
        cmd.append(wxT("\");"));
        sendMaxima(cmd);
      }
    }
    break;
  case menu_batch_id:
    {
      wxString file = wxFileSelector(_("Select package to batch"), m_lastPath,
                                     wxT(""), wxT(""),
                                     _("Maxima file (*.mac)|*.mac|"
                                       "Demo file (*.dem)|*.dem|All|*"),
                                     wxOPEN);
      if (file.Length()) {
        m_lastPath = wxPathOnly(file);
        file.Replace(b,f);
        wxString cmd;
        if (file.Right(4) == wxT(".dem") || file.Right(5) == wxT(".demo"))
          cmd.append(wxT("demo(\""));
        else
          cmd.append(wxT("batch(\""));
        cmd.append(file);
        cmd.append(wxT("\");"));
        sendMaxima(cmd);
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
        file.Replace(b,f);
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

void wxMaxima::editMenu(wxCommandEvent& event)
{
  switch (event.GetId()) {
  case menu_options_id:
    {
      Config *configW = new Config(this,-1, _("Maxima configuration"));
      configW->Centre(wxBOTH);
      if (configW->ShowModal() == wxID_OK) {
        wxConfigBase* config = wxConfig::Get();
        bool match = true;
        config->Read(wxT("matchParens"), &match);
        m_inputLine->setMatchParens(match);
        m_console->Recalculate(false);
        m_console->Refresh();
      }
      configW->Destroy();
    }
    break;
  case menu_clear_screen:
    m_console->ClearWindow();
    consoleAppend(m_lastPrompt, PROMPTT);
    break;
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
  case menu_copy_to_file:
  {
    wxString file = wxFileSelector(_("Save selection to file"), m_lastPath,
                                   wxT(""), wxT(""),
                                   _("PNG image (*.png)|*.png|"
                                     "JPEG image (*.jpg)|*.jpg|"
                                     "Windows bitmap (*.bmp)|*.bmp|"
                                     "X pixmap (*.xpm)|*.xpm"),
                                   wxSAVE);
    if (file.Length()) {
      m_console->CopyToFile(file);
      m_lastPath = wxPathOnly(file);
    }
  }
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

void wxMaxima::maximaMenu(wxCommandEvent& event)
{
  wxString expr = getDefaultEntry();
  wxString cmd;
  wxString b = wxT("\\");
  wxString f = wxT("/");
  switch (event.GetId()) {
  case menu_restart_id:
    startMaxima();
    break;
  case menu_soft_restart:
    sendMaxima(wxT("kill(all);"));
    break;
  case menu_functions:
    sendMaxima(wxT("functions;"));
    break;
  case menu_variables:
    sendMaxima(wxT("values;"));
    break;
  case menu_display:
    cmd = wxT("if display2d#false then display2d:false else display2d:true;");
    sendMaxima(cmd, false);
    break;
  case menu_texform:
    cmd = wxT("tex(") + expr + wxT(");");
    sendMaxima(cmd);
    break;
  case menu_time:
    cmd = wxT("if showtime#false then showtime:false else showtime:all;");
    sendMaxima(cmd, false);
    break;
  case menu_fun_def:
    cmd = GetTextFromUser(_("Show the definition of function:"),
                          _("Function"), wxT(""), this);
    if (cmd.Length()) {
      cmd = wxT("fundef(") + cmd + wxT(");");
      sendMaxima(cmd);
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
        cmd = wxT("FILE_SEARCH_MAXIMA : cons(\"") + dir +
              wxT("/###.{lisp,mac,mc}\", FILE_SEARCH_MAXIMA)$");
        sendMaxima(cmd);
      }
    }
    break;
  case menu_clear_var:
    cmd = GetTextFromUser(_("Delete variable(s):"), _("Delete"),
                          wxT("all"), this);
    if (cmd.Length()) {
      cmd = wxT("remvalue(") + cmd + wxT(");");
      sendMaxima(cmd);
    }
    break;
  case menu_clear_fun:
    cmd = GetTextFromUser(_("Delete function(s):"), _("Delete"),
                          wxT("all"), this);
    if (cmd.Length()) {
      cmd = wxT("remfunction(") + cmd + wxT(");");
      sendMaxima(cmd);
    }
    break;
  case button_subst:
    {
      SubstituteWiz *wiz = new SubstituteWiz(this, -1, _("Substitute"));
      wiz->setValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        wxString val = wiz->getValue();
        sendMaxima(val);
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
      wiz->setValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        wxString val = wiz->getValue();
        val.Trim();
        val.Trim(false);
        if (val.Last()!=';' && val.Last()!='$')
          val += wxT("$");
        sendMaxima(val);
      }
      wiz->Destroy();
    }
    break;
  default:
    break;
  }
  m_inputLine->SetFocus();
}

void wxMaxima::equationsMenu(wxCommandEvent& event)
{
  wxString expr = getDefaultEntry();
  wxString cmd;
  switch (event.GetId()) {
  case menu_allroots:
    cmd = wxT("allroots(") + expr + wxT(");");
    sendMaxima(cmd);
    break;
  case menu_realroots:
    cmd = wxT("realroots(") + expr +  wxT(");");
    sendMaxima(cmd);
    break;
  case button_solve:
  case menu_solve:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Solve equation(s):"), _("for variable(s):"),
                                 expr, wxT("x"), this, -1, _("Solve"), true);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        cmd = wxT("solve([") + wiz->getValue_1() + wxT("], [") +
              wiz->getValue_2() + wxT("]);");
        sendMaxima(cmd);
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
        wxString val = wxT("ode2(") + wiz->getValue_1() + wxT(", ") +
          wiz->getValue_2() + wxT(", ") + wiz->getValue_3() + wxT(");");
        sendMaxima(val);
      }
      wiz->Destroy();
    }
    break;
  case menu_ivp_1:
    {
      IC1Wiz *wiz = new IC1Wiz(this, -1, _("IC1"));
      wiz->setValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        wxString val = wiz->getValue();
        sendMaxima(val);
      }
      wiz->Destroy();
    }
    break;
  case menu_ivp_2:
    {
      IC2Wiz *wiz = new IC2Wiz(this, -1, _("IC2"));
      wiz->setValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        wxString val = wiz->getValue();
        sendMaxima(val);
      }
      wiz->Destroy();
    }
    break;
  case menu_bvp:
    {
      BC2Wiz *wiz = new BC2Wiz(this, -1, _("BC2"));
      wiz->setValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        wxString val = wiz->getValue();
        sendMaxima(val);
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
        cmd = wxT("eliminate([") + wiz->getValue_1() + wxT("],[")
          + wiz->getValue_2() + wxT("]);");
        sendMaxima(cmd);
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
        cmd = wxT("algsys") + wiz->getValue();
        sendMaxima(cmd);
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
        cmd = wxT("linsolve") + wiz->getValue();
        sendMaxima(cmd);
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
        cmd = wxT("desolve([") + wiz->getValue_1() + wxT("],[")
          + wiz->getValue_2() + wxT("]);");
        sendMaxima(cmd);
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
        wxString val = wxT("atvalue(") + wiz->getValue_1() + wxT(", ")
                       + wiz->getValue_2() +
        wxT(", ") + wiz->getValue_3() + wxT(");");
        sendMaxima(val);
      }
      wiz->Destroy();
    }
    break;
  default:
    break;
  }
  m_inputLine->SetFocus();
}

void wxMaxima::algebraMenu(wxCommandEvent& event)
{
  wxString expr = getDefaultEntry();
  wxString cmd;
  switch (event.GetId()) {
  case menu_invert_mat:
    cmd = wxT("invert(") + expr + wxT(");");
    sendMaxima(cmd);
    break;
  case menu_determinant:
    cmd = wxT("determinant(") + expr + wxT(");");
    sendMaxima(cmd);
    break;
  case menu_eigen:
    cmd = wxT("eigenvalues(") + expr + wxT(");");
    sendMaxima(cmd);
    break;
  case menu_eigvect:
    cmd = wxT("eigenvectors(") + expr + wxT(");");
    sendMaxima(cmd);
    break;
  case menu_adjoint_mat:
    cmd = wxT("adjoint(") + expr + wxT(");");
    sendMaxima(cmd);
    break;
  case menu_transpose:
    cmd = wxT("transpose(") + expr + wxT(");");
    sendMaxima(cmd);
    break;
  case menu_map_mat:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Map function:"), _("to matrix:"),
                                 wxT(""), expr,
                                 this, -1, _("Matrix map"));
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        cmd = wxT("matrixmap(") + wiz->getValue_1() + wxT(", ")
          + wiz->getValue_2() + wxT(");");
        sendMaxima(cmd);
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
        int type = wiz->getMatrixType();
        if (!(wiz->getValue_2()).ToLong(&w) ||
            !(wiz->getValue_1()).ToLong(&h) ||
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
          cmd += mwiz->getValue();
          sendMaxima(cmd);
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
        cmd = wxT("charpoly(") + wiz->getValue_1() + wxT(", ")
          + wiz->getValue_2() + wxT("), expand;");
        sendMaxima(cmd);
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
        wxString val = wxT("genmatrix(") + wiz->getValue_1() +
          wxT(", ") + wiz->getValue_2() +
          wxT(", ") + wiz->getValue_3() + wxT(");");
        sendMaxima(val);
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
        cmd = wxT("map(") + wiz->getValue_1() + wxT(", ") + wiz->getValue_2() +
              wxT(");");
        sendMaxima(cmd);
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
        cmd = wxT("makelist(") + wiz->getValue_1() + wxT(", ") +
          wiz->getValue_2() + wxT(", ") +
          wiz->getValue_3() + wxT(", ") +
          wiz->getValue_4() + wxT(");");
        sendMaxima(cmd);
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
        cmd = wxT("apply(") + wiz->getValue_1() + wxT(", ")
          + wiz->getValue_2() + wxT(");");
        sendMaxima(cmd);
      }
      wiz->Destroy();
    }
    break;
  default:
    break;
  }
  m_inputLine->SetFocus();
}

void wxMaxima::simplifyMenu(wxCommandEvent& event)
{
  wxString expr = getDefaultEntry();
  wxString cmd;
  switch (event.GetId()) {
  case button_ratsimp:
  case menu_ratsimp:
    cmd = wxT("ratsimp(") + expr + wxT(");");
    sendMaxima(cmd);
    break;
  case button_radcan:
  case menu_radsimp:
    cmd = wxT("radcan(") + expr + wxT(");");
    sendMaxima(cmd);
    break;
  case menu_to_fact:
    cmd = wxT("makefact(") + expr + wxT(");");
    sendMaxima(cmd);
    break;
  case menu_to_gamma:
    cmd = wxT("makegamma(") + expr + wxT(");");
    sendMaxima(cmd);
    break;
  case menu_factcomb:
    cmd = wxT("factcomb(") + expr + wxT(");");
    sendMaxima(cmd);
    break;
  case menu_factsimp:
    cmd = wxT("minfactorial(") + expr + wxT(");");
    sendMaxima(cmd);
    break;
  case menu_logcontract:
    cmd = wxT("logcontract(") + expr + wxT(");");
    sendMaxima(cmd);
    break;
  case menu_logexpand:
    cmd = expr + wxT(", logexpand=super;");
    sendMaxima(cmd);
    break;
  case button_expand:
  case menu_expand:
    cmd = wxT("expand(") + expr + wxT(");");
    sendMaxima(cmd);
    break;
  case button_factor:
  case menu_factor:
    cmd = wxT("factor(") + expr + wxT(");");
    sendMaxima(cmd);
    break;
  case menu_gfactor:
    cmd = wxT("gfactor(") + expr + wxT(");");
    sendMaxima(cmd);
    break;
  case button_trigreduce:
  case menu_trigreduce:
    cmd = wxT("trigreduce(") + expr + wxT(");");
    sendMaxima(cmd);
    break;
  case button_trigsimp:
  case menu_trigsimp:
    cmd = wxT("trigsimp(") + expr + wxT(");");
    sendMaxima(cmd);
    break;
  case button_trigexpand:
  case menu_trigexpand:
    cmd = wxT("trigexpand(") + expr + wxT(");");
    sendMaxima(cmd);
    break;
  case menu_trigrat:
    cmd = wxT("trigrat(") + expr + wxT(");");
    sendMaxima(cmd);
    break;
  case button_rectform:
  case menu_rectform:
    cmd = wxT("rectform(") + expr + wxT(");");
    sendMaxima(cmd);
    break;
  case menu_polarform:
    cmd = wxT("polarform(") + expr + wxT(");");
    sendMaxima(cmd);
    break;
  case menu_demoivre:
    cmd = wxT("demoivre(") + expr + wxT(");");
    sendMaxima(cmd);
    break;
  case menu_exponentialize:
    cmd = wxT("exponentialize(") + expr + wxT(");");
    sendMaxima(cmd);
    break;
  case menu_realpart:
    cmd = wxT("realpart(") + expr + wxT(");");
    sendMaxima(cmd);
    break;
  case menu_imagpart:
    cmd = wxT("imagpart(") + expr + wxT(");");
    sendMaxima(cmd);
    break;
  case menu_talg:
    cmd = wxT("algebraic : not(algebraic);");
    sendMaxima(cmd);
    break;
  case menu_tellrat:
    cmd = GetTextFromUser(_("Enter an equation for rational simplification:"),
                          _("Ratsimp"), wxT(""), this);
    if (cmd.Length()) {
      cmd = wxT("tellrat(") + cmd + wxT(");");
      sendMaxima(cmd);
    }
    break;
  case menu_modulus:
    cmd = GetTextFromUser(_("Calculate modulus:"),
                          _("Modulus"), wxT("false"), this);
    if (cmd.Length()) {
      cmd = wxT("modulus : ") + cmd + wxT(";");
      sendMaxima(cmd);
    }
    break;
  default:
    break;
  }
  m_inputLine->SetFocus();
}

void wxMaxima::calculusMenu(wxCommandEvent& event)
{
  wxString expr = getDefaultEntry();
  wxString cmd;
  switch (event.GetId()) {
  case menu_pade:
    {
      Gen3Wiz *wiz = new Gen3Wiz(_("Taylor series:"), _("num deg:"),
                                 _("denom deg:"), expr, wxT("4"), wxT("4"),
                                 this, -1, _("Pade approximation"));
      wiz->setValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        wxString val = wxT("pade(") + wiz->getValue_1() + wxT(", ") +
          wiz->getValue_2() + wxT(", ") + wiz->getValue_3() + wxT(");");
        sendMaxima(val);
      }
      wiz->Destroy();
    }
    break;
  case menu_continued_fraction:
    cmd += wxT("cfdisrep(cf(") + expr + wxT("));");
    sendMaxima(cmd);
    break;
  case menu_lcm:
    {
      Gen2Wiz *wiz = new Gen2Wiz(_("Polynomial 1:"), _("Polynomial 2:"),
                                 wxT(""), wxT(""),
                                 this, -1, _("LCM"), true);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        cmd = wxT("lcm(") + wiz->getValue_1() + wxT(", ")
          + wiz->getValue_2() + wxT(");");
        sendMaxima(cmd);
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
        cmd = wxT("gcd(") + wiz->getValue_1() + wxT(", ")
          + wiz->getValue_2() + wxT(");");
        sendMaxima(cmd);
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
        cmd = wxT("divide(") + wiz->getValue_1() + wxT(", ") +
          wiz->getValue_2() + wxT(");");
        sendMaxima(cmd);
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
        cmd = wxT("partfrac(") + wiz->getValue_1() + wxT(", ")
          + wiz->getValue_2() + wxT(");");
        sendMaxima(cmd);
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
        cmd = wxT("risch(") + wiz->getValue_1() + wxT(", ")
          + wiz->getValue_2() + wxT(");");
        sendMaxima(cmd);
      }
      wiz->Destroy();
    }
    break;
  case button_integrate:
  case menu_integrate:
    {
      IntegrateWiz *wiz = new IntegrateWiz(this, -1, _("Integrate"));
      wiz->setValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        wxString val = wiz->getValue();
        sendMaxima(val);
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
        wxString val = wxT("laplace(") + wiz->getValue_1() + wxT(", ")
          + wiz->getValue_2() +
          wxT(", ") + wiz->getValue_3() + wxT(");");
        sendMaxima(val);
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
        wxString val = wxT("ilt(") + wiz->getValue_1() + wxT(", ") +
          wiz->getValue_2() + wxT(", ") + wiz->getValue_3() + wxT(");");
        sendMaxima(val);
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
        wxString val = wxT("diff(") + wiz->getValue_1() + wxT(", ") +
          wiz->getValue_2();
        if (wiz->getValue_3()!=wxT("1"))
          val += wxT(", ") + wiz->getValue_3();
        val += wxT(");");
        sendMaxima(val);
      }
      wiz->Destroy();
    }
    break;
  case button_taylor:
  case menu_series:
    {
      SeriesWiz *wiz = new SeriesWiz(this, -1, _("Series"));
      wiz->setValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        wxString val = wiz->getValue();
        sendMaxima(val);
      }
      wiz->Destroy();
    }
    break;
  case button_limit:
  case menu_limit:
    {
      LimitWiz *wiz = new LimitWiz(this, -1, _("Limit"));
      wiz->setValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        wxString val = wiz->getValue();
        sendMaxima(val);
      }
      wiz->Destroy();
    }
    break;
  case button_sum:
  case menu_sum:
    {
      SumWiz *wiz = new SumWiz(this, -1, _("Sum"));
      wiz->setValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        wxString val = wiz->getValue();
        sendMaxima(val);
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
        cmd = wxT("unsum(") + wiz->getValue_1() + wxT(", ")
          + wiz->getValue_2() + wxT(");");
        sendMaxima(cmd);
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
        cmd = wxT("product(") + wiz->getValue_1() + wxT(", ") +
          wiz->getValue_2() + wxT(", ") +
          wiz->getValue_3() + wxT(", ") +
          wiz->getValue_4() + wxT(");");
        sendMaxima(cmd);
      }
      wiz->Destroy();
    }
    break;
  default:
    break;
  }
  m_inputLine->SetFocus();
}

void wxMaxima::plottingMenu(wxCommandEvent& event)
{
  wxString expr = getDefaultEntry();
  wxString cmd;
  switch (event.GetId()) {
  case button_plot3:
  case gp_plot3:
    {
      Plot3DWiz *wiz = new Plot3DWiz(this, -1, _("Plot 3D"));
      wiz->setValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        wxString val = wiz->getValue();
        sendMaxima(val);
      }
      wiz->Destroy();
    }
    break;
  case button_plot2:
  case gp_plot2:
    {
      Plot2DWiz *wiz = new Plot2DWiz(this, -1, _("Plot 2D"));
      wiz->setValue(expr);
      wiz->Centre(wxBOTH);
      if (wiz->ShowModal() == wxID_OK) {
        wxString val = wiz->getValue();
        sendMaxima(val);
      }
      wiz->Destroy();
    }
    break;
  case menu_plot_format:
    {
      wxString format = GetTextFromUser(_("Enter new plot format:"),
                        _("Plot format"), wxT("gnuplot"), this);
      if (format.Length()) {
        sendMaxima(wxT("set_plot_option(['plot_format, '") + format + wxT("])$"));
      }
    }
  default:
    break;
  }
  m_inputLine->SetFocus();
}

void wxMaxima::numericalMenu(wxCommandEvent& event)
{
  wxString expr = getDefaultEntry();
  wxString cmd;
  switch (event.GetId()) {
  case menu_to_float:
    cmd = wxT("float(") + expr + wxT(");");
    sendMaxima(cmd);
    break;
  case menu_to_bfloat:
    cmd = wxT("bfloat(") + expr + wxT(");");
    sendMaxima(cmd);
    break;
  case menu_num_out:
    cmd = wxT("if numer#false then numer:false else numer:true;");
    sendMaxima(cmd, false);
    break;
  case menu_set_precision:
    cmd = GetTextFromUser(_("Enter new precision:"), _("Precision"),
                          wxT("16"), this);
    if (cmd.Length()) {
      cmd = wxT("fpprec : ") + cmd + wxT(";");
      sendMaxima(cmd);
    }
    break;
  default:
    break;
  }
  m_inputLine->SetFocus();
}

void wxMaxima::aboutMenu(wxCommandEvent& event)
{
  wxString expr = getDefaultEntry();
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
      sendMaxima(cmd, false);
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
      sendMaxima(cmd, false);
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
      sendMaxima(cmd, false);
    }
    break;
  case menu_show_tip:
    showTip(true);
    break;
  case menu_build_info:
    sendMaxima(wxT("build_info()$"), false);
    break;
  case menu_bug_report:
    sendMaxima(wxT("bug_report()$"), false);
    break;
  default:
    break;
  }
}

void wxMaxima::onClose(wxCloseEvent& event)
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
  m_process->Detach();
  m_closing = true;
  cleanUp();
  Destroy();
}

BEGIN_EVENT_TABLE(wxMaxima, wxFrame)
  EVT_TEXT_ENTER(input_line_id, wxMaxima::enterCommand)
  EVT_BUTTON(button_integrate, wxMaxima::calculusMenu)
  EVT_BUTTON(button_diff, wxMaxima::calculusMenu)
  EVT_BUTTON(button_solve, wxMaxima::equationsMenu)
  EVT_BUTTON(button_solve_ode, wxMaxima::equationsMenu)
  EVT_BUTTON(button_sum, wxMaxima::calculusMenu)
  EVT_BUTTON(button_expand, wxMaxima::simplifyMenu)
  EVT_BUTTON(button_factor, wxMaxima::simplifyMenu)
  EVT_BUTTON(button_taylor, wxMaxima::calculusMenu)
  EVT_BUTTON(button_limit, wxMaxima::calculusMenu)
  EVT_BUTTON(button_ratsimp, wxMaxima::simplifyMenu)
  EVT_BUTTON(button_trigexpand, wxMaxima::simplifyMenu)
  EVT_BUTTON(button_trigreduce, wxMaxima::simplifyMenu)
  EVT_BUTTON(button_trigsimp, wxMaxima::simplifyMenu)
  EVT_BUTTON(button_product, wxMaxima::calculusMenu)
  EVT_BUTTON(button_long_input, wxMaxima::maximaMenu)
  EVT_BUTTON(button_radcan, wxMaxima::simplifyMenu)
  EVT_BUTTON(button_subst, wxMaxima::maximaMenu)
  EVT_BUTTON(button_plot2, wxMaxima::plottingMenu)
  EVT_BUTTON(button_plot3, wxMaxima::plottingMenu)
  EVT_BUTTON(button_map, wxMaxima::algebraMenu)
  EVT_BUTTON(button_rectform, wxMaxima::simplifyMenu)
  EVT_BUTTON(button_enter, wxMaxima::enterCommand)
  EVT_MENU(menu_polarform, wxMaxima::simplifyMenu)
  EVT_MENU(menu_restart_id, wxMaxima::maximaMenu)
  EVT_MENU(menu_exit_id, wxMaxima::fileMenu)
  EVT_MENU(menu_about_id, wxMaxima::aboutMenu)
  EVT_MENU(menu_save_id, wxMaxima::fileMenu)
  EVT_MENU(menu_load_id, wxMaxima::fileMenu)
  EVT_MENU(menu_monitor_file, wxMaxima::onMonitorFile)
  EVT_MENU(menu_batch_id, wxMaxima::fileMenu)
  EVT_MENU(menu_select_file, wxMaxima::fileMenu)
  EVT_MENU(menu_functions, wxMaxima::maximaMenu)
  EVT_MENU(menu_variables, wxMaxima::maximaMenu)
  EVT_MENU(menu_options_id, wxMaxima::editMenu)
  EVT_MENU(menu_sconsole_id, wxMaxima::fileMenu)
  EVT_MENU(menu_help_id, wxMaxima::aboutMenu)
  EVT_MENU(menu_bug_report, wxMaxima::aboutMenu)
  EVT_MENU(menu_build_info, wxMaxima::aboutMenu)
  EVT_MENU(menu_interrupt_id, wxMaxima::interrupt)
  EVT_MENU(menu_open_id, wxMaxima::fileMenu)
  EVT_MENU(menu_ratsimp, wxMaxima::simplifyMenu)
  EVT_MENU(menu_radsimp, wxMaxima::simplifyMenu)
  EVT_MENU(menu_expand, wxMaxima::simplifyMenu)
  EVT_MENU(menu_factor, wxMaxima::simplifyMenu)
  EVT_MENU(menu_gfactor, wxMaxima::simplifyMenu)
  EVT_MENU(menu_trigsimp, wxMaxima::simplifyMenu)
  EVT_MENU(menu_trigexpand, wxMaxima::simplifyMenu)
  EVT_MENU(menu_trigreduce, wxMaxima::simplifyMenu)
  EVT_MENU(menu_rectform, wxMaxima::simplifyMenu)
  EVT_MENU(menu_demoivre, wxMaxima::simplifyMenu)
  EVT_MENU(menu_num_out, wxMaxima::numericalMenu)
  EVT_MENU(menu_to_float, wxMaxima::numericalMenu)
  EVT_MENU(menu_to_bfloat, wxMaxima::numericalMenu)
  EVT_MENU(menu_exponentialize, wxMaxima::simplifyMenu)
  EVT_MENU(menu_invert_mat, wxMaxima::algebraMenu)
  EVT_MENU(menu_determinant, wxMaxima::algebraMenu)
  EVT_MENU(menu_eigen, wxMaxima::algebraMenu)
  EVT_MENU(menu_eigvect, wxMaxima::algebraMenu)
  EVT_MENU(menu_adjoint_mat, wxMaxima::algebraMenu)
  EVT_MENU(menu_transpose, wxMaxima::algebraMenu)
  EVT_MENU(menu_set_precision, wxMaxima::numericalMenu)
  EVT_MENU(menu_talg, wxMaxima::simplifyMenu)
  EVT_MENU(menu_tellrat, wxMaxima::simplifyMenu)
  EVT_MENU(menu_modulus, wxMaxima::simplifyMenu)
  EVT_MENU(menu_allroots, wxMaxima::equationsMenu)
  EVT_MENU(menu_realroots, wxMaxima::equationsMenu)
  EVT_MENU(menu_solve, wxMaxima::equationsMenu)
  EVT_MENU(menu_solve_ode, wxMaxima::equationsMenu)
  EVT_MENU(menu_map_mat, wxMaxima::algebraMenu)
  EVT_MENU(menu_enter_mat, wxMaxima::algebraMenu)
  EVT_MENU(menu_cpoly, wxMaxima::algebraMenu)
  EVT_MENU(menu_solve_lin, wxMaxima::equationsMenu)
  EVT_MENU(menu_solve_algsys, wxMaxima::equationsMenu)
  EVT_MENU(menu_eliminate, wxMaxima::equationsMenu)
  EVT_MENU(menu_clear_var, wxMaxima::maximaMenu)
  EVT_MENU(menu_clear_fun, wxMaxima::maximaMenu)
  EVT_MENU(menu_ivp_1, wxMaxima::equationsMenu)
  EVT_MENU(menu_ivp_2, wxMaxima::equationsMenu)
  EVT_MENU(menu_bvp, wxMaxima::equationsMenu)
  EVT_MENU(menu_fun_def, wxMaxima::maximaMenu)
  EVT_MENU(menu_describe, wxMaxima::aboutMenu)
  EVT_MENU(menu_divide, wxMaxima::calculusMenu)
  EVT_MENU(menu_gcd, wxMaxima::calculusMenu)
  EVT_MENU(menu_lcm, wxMaxima::calculusMenu)
  EVT_MENU(menu_continued_fraction, wxMaxima::calculusMenu)
  EVT_MENU(menu_partfrac, wxMaxima::calculusMenu)
  EVT_MENU(menu_risch, wxMaxima::calculusMenu)
  EVT_MENU(menu_integrate, wxMaxima::calculusMenu)
  EVT_MENU(menu_laplace, wxMaxima::calculusMenu)
  EVT_MENU(menu_ilt, wxMaxima::calculusMenu)
  EVT_MENU(menu_diff, wxMaxima::calculusMenu)
  EVT_MENU(menu_series, wxMaxima::calculusMenu)
  EVT_MENU(menu_limit, wxMaxima::calculusMenu)
  EVT_MENU(menu_gen_mat, wxMaxima::algebraMenu)
  EVT_MENU(menu_map, wxMaxima::algebraMenu)
  EVT_MENU(menu_sum, wxMaxima::calculusMenu)
  EVT_MENU(menu_example, wxMaxima::aboutMenu)
  EVT_MENU(menu_apropos, wxMaxima::aboutMenu)
  EVT_MENU(menu_show_tip, wxMaxima::aboutMenu)
  EVT_MENU(menu_trigrat, wxMaxima::simplifyMenu)
  EVT_MENU(menu_solve_de, wxMaxima::equationsMenu)
  EVT_MENU(menu_atvalue, wxMaxima::equationsMenu)
  EVT_MENU(menu_sum, wxMaxima::calculusMenu)
  EVT_MENU(menu_unsum, wxMaxima::calculusMenu)
  EVT_MENU(menu_product, wxMaxima::calculusMenu)
  EVT_MENU(menu_make_list, wxMaxima::algebraMenu)
  EVT_MENU(menu_apply, wxMaxima::algebraMenu)
  EVT_MENU(menu_time, wxMaxima::maximaMenu)
  EVT_MENU(menu_factsimp, wxMaxima::simplifyMenu)
  EVT_MENU(menu_factcomb, wxMaxima::simplifyMenu)
  EVT_MENU(menu_realpart, wxMaxima::simplifyMenu)
  EVT_MENU(menu_imagpart, wxMaxima::simplifyMenu)
  EVT_MENU(menu_logcontract, wxMaxima::simplifyMenu)
  EVT_MENU(menu_logexpand, wxMaxima::simplifyMenu)
  EVT_MENU(gp_plot2, wxMaxima::plottingMenu)
  EVT_MENU(gp_plot3, wxMaxima::plottingMenu)
  EVT_MENU(menu_plot_format, wxMaxima::plottingMenu)
  EVT_MENU(menu_soft_restart, wxMaxima::maximaMenu)
  EVT_MENU(menu_display, wxMaxima::maximaMenu)
  EVT_MENU(menu_pade, wxMaxima::calculusMenu)
  EVT_MENU(menu_add_path, wxMaxima::maximaMenu)
  EVT_MENU(menu_clear_screen, wxMaxima::editMenu)
  EVT_MENU(menu_copy_from_console, wxMaxima::editMenu)
  EVT_MENU(menu_copy_lb_from_console, wxMaxima::editMenu)
  EVT_MENU(menu_delete_selection, wxMaxima::editMenu)
  EVT_MENU(menu_goto_input, wxMaxima::editMenu)
  EVT_MENU(menu_texform, wxMaxima::maximaMenu)
  EVT_MENU(menu_to_fact, wxMaxima::simplifyMenu)
  EVT_MENU(menu_to_gamma, wxMaxima::simplifyMenu)
  EVT_MENU(menu_goto_input, wxMaxima::maximaMenu)
  EVT_MENU(menu_print, wxMaxima::printMenu)
  EVT_MENU(menu_print_setup, wxMaxima::printMenu)
  EVT_MENU(menu_inc_fontsize, wxMaxima::editMenu)
  EVT_MENU(menu_dec_fontsize, wxMaxima::editMenu)
  EVT_MENU(menu_copy_as_bitmap, wxMaxima::editMenu)
  EVT_MENU(menu_copy_to_file, wxMaxima::editMenu)
  EVT_SOCKET(socket_server_id, wxMaxima::serverEvent)
  EVT_SOCKET(socket_client_id, wxMaxima::clientEvent)
  EVT_UPDATE_UI(menu_copy_from_console, wxMaxima::updateMenus)
  EVT_UPDATE_UI(menu_copy_lb_from_console, wxMaxima::updateMenus)
  EVT_UPDATE_UI(menu_inc_fontsize, wxMaxima::updateMenus)
  EVT_UPDATE_UI(menu_dec_fontsize, wxMaxima::updateMenus)
  EVT_UPDATE_UI(menu_print, wxMaxima::updateMenus)
  EVT_UPDATE_UI(menu_copy_as_bitmap, wxMaxima::updateMenus)
  EVT_UPDATE_UI(menu_copy_to_file, wxMaxima::updateMenus)
  EVT_CLOSE(wxMaxima::onClose)
  EVT_ACTIVATE(wxMaxima::onActivate)
  EVT_SET_FOCUS(wxMaxima::onSetFocus)
  EVT_END_PROCESS(maxima_process_id, wxMaxima::onProcessEvent)
END_EVENT_TABLE()
