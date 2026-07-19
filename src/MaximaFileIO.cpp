// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2026 Gunter Königsmann <wxMaxima@physikbuch.de>
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

/*! \file
  Implements the worksheet file loaders/savers extracted from the wxMaxima god
  class.
*/

#include "MaximaFileIO.h"
#include "wxMaxima.h"
#include "dialogs/LoggingMessageDialog.h"
#include "worksheet/Worksheet.h"
#include "WXMformat.h"
#include "WXMXformat.h"
#include "cells/GroupCell.h"
#include <wx/busyinfo.h>
#include <wx/textfile.h>
#include <wx/tokenzr.h>
#include <wx/wfstream.h>
#include <wx/zipstrm.h>
#include <wx/mstream.h>
#include <wx/sstream.h>
#include <wx/txtstrm.h>
#include <wx/xml/xml.h>
#include <wx/filedlg.h>
#include <wx/config.h>
#include <wx/filename.h>
#include "cells/Cell.h"
#include <memory>

bool MaximaFileIO::OpenMACFile(const wxString &file, Worksheet *document,
                           bool clearDocument) {
  // Show a busy cursor while we open the file.
  wxBusyCursor crs;

  m_wxMaxima.StatusText(_("Opening file"));
  //  wxWindowUpdateLocker noUpdates(document);

  bool xMaximaFile = file.Lower().EndsWith(wxS(".out"));

  // open mac file
  wxTextFile inputFile(file);

  if (!inputFile.Open()) {
    LoggingMessageBox(_("wxMaxima encountered an error loading ") + file,
                      _("Error"), wxOK | wxICON_EXCLAMATION);
    m_wxMaxima.StatusMaximaBusy(StatusBar::MaximaStatus::waiting);
    m_wxMaxima.StatusText(_("File could not be opened"));
    return false;
  }

  if (clearDocument)
    document->ClearDocument();

  auto tree = Format::ParseMACFile(inputFile, xMaximaFile, &m_wxMaxima.m_configuration);

  document->InsertGroupCells(std::move(tree), nullptr);

  if (clearDocument) {
    if(m_wxMaxima.GetWorksheet())
      m_wxMaxima.GetWorksheet()->SetCurrentFile(file);
    m_wxMaxima.m_processManager.StartMaxima();
    m_wxMaxima.ResetTitle(true, true);
    document->SetSaved(true);
  } else {
    m_wxMaxima.ResetTitle(false);
    if(m_wxMaxima.GetWorksheet())
      m_wxMaxima.GetWorksheet()->UpdateTableOfContents();
  }

  document->RequestRedraw();

  if(m_wxMaxima.GetWorksheet())
    {
      m_wxMaxima.GetWorksheet()->SetDefaultHCaret();
      m_wxMaxima.CallAfter([this]{m_wxMaxima.GetWorksheet()->SetFocus();});
    }
  m_wxMaxima.SetCWD(file);

  m_wxMaxima.StatusMaximaBusy(StatusBar::MaximaStatus::waiting);

  if(m_wxMaxima.GetWorksheet())
    {
      m_wxMaxima.GetWorksheet()->SetHCaret(NULL);
      m_wxMaxima.GetWorksheet()->ScrollToCaret();
    }
  return true;
}

bool MaximaFileIO::OpenWXMFile(const wxString &file, Worksheet *document,
                           bool clearDocument) {
  // Show a busy cursor while we open the file.
  wxBusyCursor crs;

  m_wxMaxima.StatusText(_("Opening file"));
  //  wxWindowUpdateLocker noUpdates(document);

  // open wxm file
  wxTextFile inputFile(file);

  if (!inputFile.Open()) {
    LoggingMessageBox(_("wxMaxima encountered an error loading ") + file + " " + _("(Maybe a permission problem?)"),
                      _("Error"), wxOK | wxICON_EXCLAMATION);
    m_wxMaxima.StatusMaximaBusy(StatusBar::MaximaStatus::waiting);
    m_wxMaxima.StatusText(_("File could not be opened"));
    return false;
  }

  if (inputFile.GetFirstLine() != Format::WXMFirstLine) {
    inputFile.Close();
    LoggingMessageBox(_("wxMaxima encountered an error loading ") + file + " " + _(" (File format (WXM version 1, first line of the file) not recognized)"), // FIXME: The error message could be improved...
                      _("Error"), wxOK | wxICON_EXCLAMATION);
    return false;
  }

  auto tree = Format::ParseWXMFile(inputFile, &m_wxMaxima.m_configuration);
  inputFile.Close();

  // from here on code is identical for wxm and wxmx
  if (clearDocument) {
    document->ClearDocument();
    if(m_wxMaxima.GetWorksheet())
      m_wxMaxima.GetWorksheet()->SetCurrentFile(file);
    m_wxMaxima.m_processManager.StartMaxima();
  }

  document->InsertGroupCells(
                             std::move(tree)); // this also requests a recalculate

  if (clearDocument) {
    if(m_wxMaxima.GetWorksheet())
      m_wxMaxima.GetWorksheet()->SetCurrentFile(file);
    m_wxMaxima.ResetTitle(true, true);
    document->SetSaved(true);
  } else
    m_wxMaxima.ResetTitle(false);

  document->RequestRedraw();

  if(m_wxMaxima.GetWorksheet())
    {
      m_wxMaxima.GetWorksheet()->SetDefaultHCaret();
      m_wxMaxima.CallAfter([this]{m_wxMaxima.GetWorksheet()->SetFocus();});
    }
  m_wxMaxima.SetCWD(file);

  m_wxMaxima.StatusMaximaBusy(StatusBar::MaximaStatus::waiting);
  if(m_wxMaxima.GetWorksheet())
    {
      m_wxMaxima.GetWorksheet()->SetHCaret(NULL);
      m_wxMaxima.GetWorksheet()->ScrollToCaret();
    }
  return true;
}

wxString MaximaFileIO::ReadPotentiallyUnclosedTag(wxStringTokenizer &lines,
                                              wxString firstLine, int depth) {
  wxString result = firstLine + wxS("\n");
  wxString closingTag = firstLine;
  m_wxMaxima.m_xmlOpeningTagName.Replace(&closingTag, wxS("</\\1>"));

  while (lines.HasMoreTokens()) {
    wxString line = lines.GetNextToken();
    if ((line.Contains(wxS("<line"))) || (line.Contains(wxS("</line"))) ||
        (!line.Contains(wxS("<")))) {
      // TODO: Handle broken line tags and line tags that are split into lines.
      result += line + wxS("\n");
    } else {
      if (line.Contains(wxS("</")))
        break;
      else {
        // Each nested unclosed tag recurses; bound the depth so a crafted
        // .wxmx with pathologically deep nesting can't overflow the stack.
        // Too deep: keep the line as flat text instead of recursing.
        if (line.Contains(wxS("<")) && depth < 250)
          result += ReadPotentiallyUnclosedTag(lines, line, depth + 1);
        else
          result += line + wxS("\n");
      }
    }
  }
  if (!m_wxMaxima.m_xmlOpeningTag.Matches(firstLine))
    return wxEmptyString;
  else {
    result += closingTag + wxS("\n");
    return result;
  }
}

namespace {
//! Parse a string of XML into xmldoc. No-op on an empty string: a
//! wxMemoryInputStream built from an empty wxMemoryOutputStream trips a
//! wxWidgets assertion ("must have buffer to CopyTo"), and empty content can
//! never be valid XML anyway. Used by the .wxmx recovery ladder, whose earlier
//! stages can whittle the recovered text down to nothing.
void LoadXmlFromString(wxXmlDocument &xmldoc, const wxString &contents) {
  if (contents.IsEmpty())
    return;
  wxMemoryOutputStream ostream;
  wxTextOutputStream txtstrm(ostream);
  txtstrm.WriteString(contents);
  wxMemoryInputStream istream(ostream);
#if wxCHECK_VERSION(3, 3, 0)
  xmldoc.Load(istream, wxXMLDOC_KEEP_WHITESPACE_NODES);
#else
  xmldoc.Load(istream, wxS("UTF-8"), wxXMLDOC_KEEP_WHITESPACE_NODES);
#endif
}
} // namespace

bool MaximaFileIO::OpenWXMXFile(const wxString &file, Worksheet *document,
                            bool clearDocument) {
  wxLogMessage(_("Opening a wxmx file"));
  // Show a busy cursor while we open a file.
  wxBusyCursor crs;

  m_wxMaxima.StatusText(_("Opening file"));

  //  wxWindowUpdateLocker noUpdates(document);

  // If the file is empty we don't want to generate an error, but just
  // open an empty file.
  //
  // This makes the following thing work on windows without the need of an
  // empty template file:
  //
  // - Create a registry key named
  // HKEY_LOKAL_MACHINE\SOFTWARE\CLASSES\.wxmx\ShellNew
  // - Create a string named "NullFile" within this key
  //
  // => After the next reboot the right-click context menu's "new" submenu
  // contains
  //    an entry that creates valid empty .wxmx files.
  {
    // The IsOpened() check keeps an unopenable file (deleted, no permission)
    // from tripping wxFile::Eof()'s is-opened assert; such a file falls
    // through to the zip reader whose error path reports it.
    wxFile emptyCheck(file, wxFile::read);
    if (emptyCheck.IsOpened() && emptyCheck.Eof()) {
      document->ClearDocument();
      if(m_wxMaxima.GetWorksheet())
        m_wxMaxima.GetWorksheet()->SetCurrentFile(file);
      m_wxMaxima.m_processManager.StartMaxima();

      m_wxMaxima.ResetTitle(true, true);
      document->SetSaved(true);
      return true;
    }
  }

  // open wxmx file
  wxXmlDocument xmldoc;

  wxFileInputStream wxmxFile(file);
  wxZipInputStream wxmxContents(wxmxFile);
  // GetNextEntry hands the caller ownership of each wxZipEntry; the
  // unique_ptr frees the skipped entries and the matching one alike.
  std::unique_ptr<wxZipEntry> contentsEntry;
  while(!wxmxContents.Eof())
    {
      contentsEntry.reset(wxmxContents.GetNextEntry());
      if((!contentsEntry) || (contentsEntry->GetName() == wxS("content.xml")))
        break;
    }

  // Open the file
#if wxCHECK_VERSION(3, 3, 0)
  xmldoc.Load(wxmxContents, wxXMLDOC_KEEP_WHITESPACE_NODES);
#else
  xmldoc.Load(wxmxContents, wxS("UTF-8"), wxXMLDOC_KEEP_WHITESPACE_NODES);
#endif

  if (!xmldoc.IsOk()) {
    // If we cannot read the file a typical error in old wxMaxima versions was
    // to include a letter of ascii code 27 in content.xml. Let's filter this
    // char out.

    // Upper bound on the content.xml we are willing to read into memory. A
    // real worksheet's XML is at most a few tens of MB; a zip entry that
    // declares (or, decompressed, produces) far more than this is either
    // corrupt or a decompression bomb, and reading it all would exhaust
    // memory / appear to hang. 256 MB is comfortably above any genuine file.
    constexpr size_t MAX_CONTENT_XML_BYTES = 256u * 1024 * 1024;
    wxString contents;
    if (contentsEntry) {
      // Re-open the file.
      wxmxContents.OpenEntry(*contentsEntry);
      // Read the file into a string
      wxTextInputStream istream1(wxmxContents, wxS('\t'),
                                 wxConvAuto(wxFONTENCODING_UTF8));
      // wxWidgets 3.3 no longer flips Eof() to true on an empty zip entry
      // (IsOk() goes false instead), so guard with IsOk() as well to avoid an
      // infinite ReadLine() loop on an empty content.xml.
      while (wxmxContents.IsOk() && !wxmxContents.Eof() &&
             (contents.length() < MAX_CONTENT_XML_BYTES))
        contents += istream1.ReadLine() + wxS("\n");
      // No assert on the size: an oversized entry is untrusted input we handle,
      // not a broken invariant.
      if (contents.length() >= MAX_CONTENT_XML_BYTES)
        wxLogMessage(_("content.xml exceeds the sanity limit of %zu bytes; "
                       "the file may be corrupt or a decompression bomb."),
                     MAX_CONTENT_XML_BYTES);
    } else {
      wxLogMessage(_("Trying to recover a broken .wxmx file."));
      // Let's try to recover the uncompressed text from a truncated .zip file
      wxFileInputStream input(file);
      if (input.IsOk()) {
        wxLogMessage(_("Trying to extract content.xml out of a broken .zip file."));
        wxTextInputStream text(input, wxS('\t'),
                               wxConvAuto(wxFONTENCODING_UTF8));
        while (input.IsOk() && !input.Eof()) {
          contents = text.ReadLine();
          if (contents.StartsWith(wxS("<wxMaximaDocument"))) {
            contents += wxS("\n");
            break;
          }
        }
        while (input.IsOk() && !input.Eof() &&
               (contents.length() < MAX_CONTENT_XML_BYTES)) {
          wxString line = text.ReadLine();
          if ((!input.Eof()) || (line != wxEmptyString)) {
            if (line.StartsWith(wxS("</wxMaximaDocument>"))) {
              contents += wxS("</wxMaximaDocument>\n");
              break;
            } else
              contents += line + wxS("\n");
          }
        }
      }
    }
    // Remove the illegal character
    contents.Replace(wxS('\u001b'), wxS("\u238B"));

    LoadXmlFromString(xmldoc, contents);

    // If the xml document still cannot be loaded let's extract only the input
    // cells.
    if (!xmldoc.IsOk()) {
      wxLogMessage(_("Trying to discard all output."));
      contents.Replace(wxS("><"), wxS(">\n<"));
      wxStringTokenizer lines(contents, wxS("\n"), wxTOKEN_RET_EMPTY_ALL);
      wxString contents_inputOnly;
      while (lines.HasMoreTokens()) {
        wxString line = lines.GetNextToken();
        if (line.Contains(wxS("<output"))) {
          while (lines.HasMoreTokens() && (!line.Contains(wxS("</output>"))))
            line = lines.GetNextToken();
        } else {
          contents_inputOnly += line + wxS("\n");
        }
      }
      contents = contents_inputOnly;

      LoadXmlFromString(xmldoc, contents);
    }

    // If even that cannot be loaded let's try to reconstruct the closing
    // markers.
    if (!xmldoc.IsOk()) {
      wxLogMessage(_("Trying to reconstruct the xml closing markers."));
      wxStringTokenizer lines(contents, wxS("\n"), wxTOKEN_RET_EMPTY_ALL);
      wxString contents_reconstructed;
      wxString line;
      while (lines.HasMoreTokens() &&
             (!line.Contains(wxS("<wxMaximaDocument")))) {
        line = lines.GetNextToken();
        contents_reconstructed += line;
      }
      while (lines.HasMoreTokens()) {
        line = lines.GetNextToken();
        if (line.Contains(wxS("<cell")))
          contents_reconstructed += ReadPotentiallyUnclosedTag(lines, line);
      }
      contents_reconstructed += wxS("</wxMaximaDocument>\n");
      contents = contents_reconstructed;
      LoadXmlFromString(xmldoc, contents);
    }
  }
  if (!xmldoc.IsOk()) {
    LoggingMessageBox(_("wxMaxima cannot read the xml contents of ") + file,
                      _("Error"), wxOK | wxICON_EXCLAMATION);
    m_wxMaxima.StatusMaximaBusy(StatusBar::MaximaStatus::waiting);
    m_wxMaxima.StatusText(_("File could not be opened"));
    return false;
  }

  // start processing the XML file
  if (xmldoc.GetRoot()->GetName() != wxS("wxMaximaDocument")) {
    LoggingMessageBox(
                      _("xml contained in the file claims not to be a wxMaxima worksheet. ") +
                      file,
                      _("Error"), wxOK | wxICON_EXCLAMATION);
    m_wxMaxima.StatusMaximaBusy(StatusBar::MaximaStatus::waiting);
    m_wxMaxima.StatusText(_("File could not be opened"));
    return false;
  }

  // read document version and complain
  wxString docversion =
    xmldoc.GetRoot()->GetAttribute(wxS("version"), wxS("1.0"));
  if (!CheckWXMXVersion(docversion)) {
    m_wxMaxima.StatusMaximaBusy(StatusBar::MaximaStatus::waiting);
    return false;
  }

  // Determine where the cursor was before saving
  wxString ActiveCellNumber_String =
    xmldoc.GetRoot()->GetAttribute(wxS("activecell"), wxS("-1"));
  long ActiveCellNumber;
  if (!ActiveCellNumber_String.ToLong(&ActiveCellNumber))
    ActiveCellNumber = -1;

  wxString VariablesNumberString =
    xmldoc.GetRoot()->GetAttribute(wxS("variables_num"), wxS("0"));
  long VariablesNumber;
  if (!VariablesNumberString.ToLong(&VariablesNumber))
    VariablesNumber = 0;
  // A crafted file can claim billions of watch variables; the loop below would
  // then call AddWatch() that many times and appear to hang. No real worksheet
  // has anywhere near this many, so cap it (the attributes past the real count
  // don't exist and would only add empty watches anyway).
  // No assert here: variables_num is untrusted file content, so an out-of-range
  // value is bad input to be handled, not a broken program invariant.
  constexpr long MAX_WATCH_VARIABLES = 100000;
  if (VariablesNumber > MAX_WATCH_VARIABLES) {
    wxLogMessage(_("The file claims %ld watch variables; capping at %ld."),
                 VariablesNumber, MAX_WATCH_VARIABLES);
    VariablesNumber = MAX_WATCH_VARIABLES;
  }
  if (VariablesNumber > 0) {
    if(m_wxMaxima.GetWorksheet() && (m_wxMaxima.m_variablesPane))
      m_wxMaxima.m_variablesPane->Clear();

    for (long i = 0; i < VariablesNumber; i++) {
      wxString variable =
        xmldoc.GetRoot()->GetAttribute(wxString::Format("variables_%li", i));
      if(m_wxMaxima.GetWorksheet() && (m_wxMaxima.m_variablesPane))
        m_wxMaxima.m_variablesPane->AddWatch(variable);
    }
  }

  // read the zoom factor
  wxString doczoom = xmldoc.GetRoot()->GetAttribute(wxS("zoom"), wxS("100"));

  // Read the worksheet's contents.
  wxXmlNode *xmlcells = xmldoc.GetRoot();
  auto tree = m_wxMaxima.CreateTreeFromXMLNode(xmlcells, file);

  // from here on code is identical for wxm and wxmx
  if (clearDocument) {
    document->ClearDocument();
    if(m_wxMaxima.GetWorksheet())
      m_wxMaxima.GetWorksheet()->SetCurrentFile(file);
    m_wxMaxima.m_processManager.StartMaxima();
    long int zoom = 100;
    if (!(doczoom.ToLong(&zoom)))
      zoom = 100;
    document->SetZoomFactor(static_cast<double>(zoom) / 100.0);
  }

  document->InsertGroupCells(
                             std::move(tree)); // this also requests a recalculate
  if (clearDocument) {
    if(m_wxMaxima.GetWorksheet())
      m_wxMaxima.GetWorksheet()->SetCurrentFile(file);
    m_wxMaxima.ResetTitle(true, true);
    document->SetSaved(true);
  } else
    m_wxMaxima.ResetTitle(false);

  if(m_wxMaxima.GetWorksheet())
    {
      m_wxMaxima.GetWorksheet()->SetDefaultHCaret();
      m_wxMaxima.CallAfter([this]{m_wxMaxima.GetWorksheet()->SetFocus();});
    }
  m_wxMaxima.SetCWD(file);

  // We can set the cursor to the last known position.
  if (m_wxMaxima.GetWorksheet() && (ActiveCellNumber == 0))
    m_wxMaxima.GetWorksheet()->SetHCaret(NULL);
  if (m_wxMaxima.GetWorksheet() && (ActiveCellNumber > 0)) {
    GroupCell *pos = m_wxMaxima.GetWorksheet()->GetTree();

    // Stop once we run off the end of the document instead of spinning through
    // the remaining (possibly billions of) iterations a crafted activecell
    // attribute can request.
    for (long i = 1; (i < ActiveCellNumber) && pos; i++)
      pos = pos->GetNext();

    if (m_wxMaxima.GetWorksheet() && pos)
      m_wxMaxima.GetWorksheet()->SetHCaret(pos);
  }
  m_wxMaxima.StatusMaximaBusy(StatusBar::MaximaStatus::waiting);

  return true;
}

bool MaximaFileIO::CheckWXMXVersion(const wxString &docversion) {
  double version = 1.0;
  // ToCDouble: the version attribute is always written with a '.' decimal
  // separator. Locale-aware ToDouble failed to parse it under locales with a
  // ',' separator, silently skipping the version check entirely.
  if (docversion.ToCDouble(&version)) {
    int version_major = static_cast<int>(version);
    int version_minor = static_cast<int>(10 * (version - static_cast<double>(version_major)));

    if (version_major > DOCUMENT_VERSION_MAJOR) {
      LoggingMessageBox(_("Document was saved using a newer version of "
                          "wxMaxima. Please update your wxMaxima."),
                        _("Error"), wxOK | wxICON_EXCLAMATION);
      m_wxMaxima.StatusText(_("File could not be opened"));
      return false;
    }
    if (version_minor > DOCUMENT_VERSION_MINOR)
      LoggingMessageBox(
                        _("Document was saved using a newer version of wxMaxima so it may "
                          "not load correctly. Please update your wxMaxima."),
                        _("Warning"), wxOK | wxICON_EXCLAMATION);
  }
  return true;
}

bool MaximaFileIO::OpenXML(const wxString &file, Worksheet *document) {
  // Show a busy cursor as long as we open a file.
  wxBusyCursor crs;

  m_wxMaxima.StatusText(_("Opening file"));

  //  wxWindowUpdateLocker noUpdates(document);

  wxXmlDocument xmldoc;

  // Let's see if we can load the XML contained in this file.
  xmldoc.Load(file);

  if (!xmldoc.IsOk()) {
    LoggingMessageBox(_("The .xml file doesn't seem to be valid xml or isn't a "
                        "content.xml extracted from a .wxmx zip archive"),
                      _("Error"), wxOK | wxICON_EXCLAMATION);
    m_wxMaxima.StatusMaximaBusy(StatusBar::MaximaStatus::waiting);
    m_wxMaxima.StatusText(_("File could not be opened"));
    return false;
  }

  // Process the XML document
  if (xmldoc.GetRoot()->GetName() != wxS("wxMaximaDocument")) {
    LoggingMessageBox(
                      _("xml contained in the file claims not to be a wxMaxima worksheet. ") +
                      file,
                      _("Error"), wxOK | wxICON_EXCLAMATION);
    m_wxMaxima.StatusMaximaBusy(StatusBar::MaximaStatus::waiting);
    m_wxMaxima.StatusText(_("File could not be opened"));
    return false;
  }

  // read document version and complain
  wxString docversion =
    xmldoc.GetRoot()->GetAttribute(wxS("version"), wxS("1.0"));
  if (!CheckWXMXVersion(docversion)) {
    m_wxMaxima.StatusMaximaBusy(StatusBar::MaximaStatus::waiting);
    return false;
  }

  // Read the worksheet's contents.
  wxXmlNode *xmlcells = xmldoc.GetRoot();
  auto tree = m_wxMaxima.CreateTreeFromXMLNode(xmlcells, file);

  document->ClearDocument();
  if(m_wxMaxima.GetWorksheet())
    m_wxMaxima.GetWorksheet()->SetCurrentFile(file);
  m_wxMaxima.m_processManager.StartMaxima();
  document->InsertGroupCells(
                             std::move(tree)); // this also requests a recalculate
  m_wxMaxima.ResetTitle(true, true);
  document->RequestRedraw();
  if(m_wxMaxima.GetWorksheet())
    {
      m_wxMaxima.GetWorksheet()->SetDefaultHCaret();
      m_wxMaxima.CallAfter([this]{m_wxMaxima.GetWorksheet()->SetFocus();});
    }
  m_wxMaxima.SetCWD(file);

  m_wxMaxima.StatusMaximaBusy(StatusBar::MaximaStatus::waiting);
  return true;
}


bool MaximaFileIO::OpenFile(const wxString &file, const wxString &command) {
  wxBusyCursor crs;
  bool retval = true;
  if (file.IsEmpty()) {
    wxLogError(_("Trying to open a file with an empty name!"));
    return false;
  }

  wxString realFile = file;
  wxString uuid;
  if (file.Contains(wxS("#"))) {
    realFile = file.BeforeFirst(wxS('#'));
    uuid = file.AfterFirst(wxS('#'));
  }

  if (!(wxFileExists(realFile))) {
    wxLogError(_("Trying to open the non-existing file %s"), realFile);
    return false;
  }

  m_wxMaxima.m_lastPath = wxPathOnly(realFile);
  wxString unixFilename(realFile);
#if defined __WXMSW__
  unixFilename.Replace(wxS("\\"), wxS("/"));
#endif

  //  wxWindowUpdateLocker dontUpdateTheWorksheet(m_wxMaxima.GetWorksheet());

  if (command.Length() > 0) {
    m_wxMaxima.MenuCommand(command + wxS("(\"") + unixFilename + wxS("\")$"));
    if (command == wxS("load")) {
      m_wxMaxima.ReReadConfig();
      m_wxMaxima.m_recentPackages.AddDocument(unixFilename);
      m_wxMaxima.UpdateRecentDocuments();
      m_wxMaxima.ReReadConfig();
    }
  } else if (realFile.Lower().EndsWith(wxS(".wxm"))) {
    retval = OpenWXMFile(realFile, m_wxMaxima.GetWorksheet());
    if (retval) {
      m_wxMaxima.ReReadConfig();
      if (m_wxMaxima.IsInteractive())
        m_wxMaxima.m_recentDocuments.AddDocument(realFile);
      m_wxMaxima.ReReadConfig();
    }
  }

  else if (realFile.Lower().EndsWith(wxS(".mac"))) {
    retval = OpenMACFile(realFile, m_wxMaxima.GetWorksheet());
    if (retval) {
      m_wxMaxima.ReReadConfig();
      if (m_wxMaxima.IsInteractive())
        m_wxMaxima.m_recentDocuments.AddDocument(realFile);
      m_wxMaxima.ReReadConfig();
    }
  } else if (realFile.Lower().EndsWith(wxS(".out"))) {
    retval = OpenMACFile(realFile, m_wxMaxima.GetWorksheet());
    if (retval) {
      m_wxMaxima.ReReadConfig();
      if (m_wxMaxima.IsInteractive())
        m_wxMaxima.m_recentDocuments.AddDocument(realFile);
      m_wxMaxima.ReReadConfig();
    }
  }

  else if (realFile.EndsWith(wxS(".wxmx")) || realFile.EndsWith(wxS(".wxmx~")) ) {
    retval = OpenWXMXFile(realFile, m_wxMaxima.GetWorksheet());
    if (retval) {
      m_wxMaxima.ReReadConfig();
      if (m_wxMaxima.IsInteractive())
        m_wxMaxima.m_recentDocuments.AddDocument(realFile);
      m_wxMaxima.ReReadConfig();
    }
  }

  else if (realFile.Right(4).Lower() == wxS(".zip")) {
    retval = OpenWXMXFile(realFile, m_wxMaxima.GetWorksheet());
    if (retval) {
      m_wxMaxima.ReReadConfig();
      if (m_wxMaxima.IsInteractive())
        m_wxMaxima.m_recentDocuments.AddDocument(realFile);
      m_wxMaxima.ReReadConfig();
    }
  }

  else if (realFile.Right(4).Lower() == wxS(".dem")) {
    m_wxMaxima.MenuCommand(wxS("demo(\"") + unixFilename + wxS("\")$"));
    m_wxMaxima.ReReadConfig();
    m_wxMaxima.m_recentPackages.AddDocument(realFile);
    m_wxMaxima.UpdateRecentDocuments();
    m_wxMaxima.ReReadConfig();
  }

  else if (realFile.Right(4).Lower() == wxS(".xml"))
    retval = OpenXML(realFile, m_wxMaxima.GetWorksheet()); // clearDocument = true

  else {
    m_wxMaxima.MenuCommand(wxS("load(\"") + unixFilename + wxS("\")$"));
    m_wxMaxima.ReReadConfig();
    m_wxMaxima.m_recentPackages.AddDocument(realFile);
    m_wxMaxima.UpdateRecentDocuments();
    m_wxMaxima.ReReadConfig();
  }

  m_wxMaxima.UpdateRecentDocuments();
  m_wxMaxima.RemoveTempAutosavefile();
  m_wxMaxima.StartAutoSaveTimer();

  m_wxMaxima.GetWorksheet()->TreeUndo_ClearBuffers();
  if (m_wxMaxima.GetWorksheet()->GetCurrentFile() != wxEmptyString) {
    wxString filename(m_wxMaxima.GetWorksheet()->GetCurrentFile());
    m_wxMaxima.SetCWD(std::move(filename));
  }
  if (m_wxMaxima.m_tableOfContents != NULL) {
    m_wxMaxima.m_scheduleUpdateToc = false;
    m_wxMaxima.m_tableOfContents->UpdateTableOfContents(
                                             m_wxMaxima.GetWorksheet()->GetHCaret());
  }

  if (!retval)
    m_wxMaxima.StatusText(wxString::Format("Errors trying to open the file %s.",
                                file));

  if (retval) {
    m_wxMaxima.GetWorksheet()->RequestRedraw();
    m_wxMaxima.StatusText(_("File opened"));
  } else
    m_wxMaxima.StatusText(_("File could not be opened"));

  m_wxMaxima.m_configuration.RecalculateForce();
  m_wxMaxima.GetWorksheet()->UpdateConfigurationClientSize();
  m_wxMaxima.GetWorksheet()->RequestRecalculation();
  m_wxMaxima.UpdateMenus();

  if (retval && !uuid.IsEmpty()) {
    Cell *cell = m_wxMaxima.GetWorksheet()->FindCellByUUID(uuid);
    if (cell) {
      m_wxMaxima.GetWorksheet()->ScrolledAwayFromEvaluation(true);
      m_wxMaxima.GetWorksheet()->ScheduleScrollToCell(cell);
    }
  }

  return retval;
}

bool MaximaFileIO::SaveFile(bool forceSave) {
  // Show a busy cursor as long as we export a file.
  wxBusyCursor crs;

  wxString file = m_wxMaxima.GetWorksheet()->GetCurrentFile();
  wxString fileExt = wxS("wxmx");
  int ext = 0;

  wxConfigBase *config = wxConfig::Get();

  if (file.Length() == 0 || forceSave) {
    // Never pop an interactive Save-As dialog in non-interactive mode (a
    // --batch / --exit-on-error run). Its native dialog does not honor
    // LoggingMessageDialog's non-interactive flag, so under a headless run it
    // was re-shown from every idle cycle when a batch run tried to auto-save a
    // session that had no filename - e.g. after its input file failed to load,
    // which wedged the process (it never reached its own Close()).
    if (LoggingMessageDialog::IsNonInteractive()) {
      wxLogMessage(_("Not saving: running non-interactively and no file name is set."));
      return false;
    }

    if (file.Length() == 0) {
      config->Read(wxS("defaultExt"), &fileExt);
      file = _("untitled") + wxS(".") + fileExt;
    } else
      wxFileName::SplitPath(file, NULL, NULL, &file, &fileExt);

    wxFileDialog fileDialog(
                            &m_wxMaxima, _("Save As"), m_wxMaxima.m_lastPath, file,
                            _("Whole document (*.wxmx)|*.wxmx|"
                              "The input, readable by load() (maxima > 5.38) (*.wxm)|*.wxm|"
                              "All Files (*.*)|*"),
                            wxFD_SAVE | wxFD_OVERWRITE_PROMPT);

    if (fileExt == wxS("wxmx"))
      fileDialog.SetFilterIndex(0);
    else if (fileExt == wxS("wxm"))
      fileDialog.SetFilterIndex(1);
    else {
      fileDialog.SetFilterIndex(0);
      fileExt = wxS("wxmx");
    }
    if (fileDialog.ShowModal() == wxID_OK) {
      file = fileDialog.GetPath();
      ext = fileDialog.GetFilterIndex();
    } else {
      m_wxMaxima.StartAutoSaveTimer();
      return false;
    }
  }

  if (file.Length()) {
    if (!file.Lower().EndsWith(wxS(".wxm")) &&
        (!file.Lower().EndsWith(wxS(".wxmx")))) {
      switch (ext) {
      case 0:
        file += wxS(".wxmx");
        break;
      case 1:
        file += wxS(".wxm");
        break;
      default:
        file += wxS(".wxmx");
      }
    }

    m_wxMaxima.StatusSaveStart();
    config->Write(wxS("defaultExt"), wxS("wxmx"));

    m_wxMaxima.m_lastPath = wxPathOnly(file);
    if (file.Lower().EndsWith(wxS(".wxm"))) {
      config->Write(wxS("defaultExt"), wxS("wxm"));
      if (!m_wxMaxima.GetWorksheet()->ExportToMAC(file)) {
        ReportSaveFailed();
        m_wxMaxima.StartAutoSaveTimer();
        if (m_wxMaxima.ExitOnErrorArmed()) {
          wxMaxima::m_exitCode = 1;
          m_wxMaxima.Close();
        }
        return false;
      } else {
        m_wxMaxima.RemoveTempAutosavefile();
        if (file != m_wxMaxima.m_tempfileName)
          m_wxMaxima.GetWorksheet()->SetCurrentFile(file);
      }
    } else {
      if (!Format::ExportToWXMX(m_wxMaxima.GetWorksheet()->GetTree(), file, &m_wxMaxima.m_configuration,
                                &m_wxMaxima.GetWorksheet()->GetViewCellPointers(),
                                m_wxMaxima.m_variablesPane->GetVarnames(),
                                m_wxMaxima.GetWorksheet()->GetHCaret())) {
        ReportSaveFailed();
        m_wxMaxima.StartAutoSaveTimer();
        if (m_wxMaxima.ExitOnErrorArmed()) {
          wxMaxima::m_exitCode = 1;
          m_wxMaxima.Close();
        }
        return false;
      } else {
        m_wxMaxima.GetWorksheet()->SetSaved(true);
        m_wxMaxima.RemoveTempAutosavefile();
        if (file != m_wxMaxima.m_tempfileName)
          m_wxMaxima.GetWorksheet()->SetCurrentFile(file);
      }
    }

    if (m_wxMaxima.IsInteractive())
      m_wxMaxima.m_recentDocuments.AddDocument(file);
    m_wxMaxima.SetCWD(file);
    m_wxMaxima.StatusSaveFinished();
    m_wxMaxima.UpdateRecentDocuments();
  }

  m_wxMaxima.StartAutoSaveTimer();

  return true;
}

void MaximaFileIO::ReportSaveFailed() {
  m_wxMaxima.StatusSaveFailed();
  if (!m_saveFailedBoxPending) {
    m_saveFailedBoxPending = true;
    m_wxMaxima.CallAfter([this]{
      LoggingMessageBox(_("Saving failed!"), _("Error!"), wxOK);
      // Cleared only after the box closes so failures of further autosave
      // attempts while it is open don't stack a second box on top of it.
      m_saveFailedBoxPending = false;
    });
  }
}

bool MaximaFileIO::AutoSave() {
  if (!m_wxMaxima.SaveNecessary())
    return true;

  bool savedWas = m_wxMaxima.GetWorksheet()->IsSaved();
  wxString oldTempFile = m_wxMaxima.m_tempfileName;
  wxString oldFilename = m_wxMaxima.GetWorksheet()->GetCurrentFile();
  m_wxMaxima.m_tempfileName = wxFileName::CreateTempFileName("untitled_");
  wxRenameFile(m_wxMaxima.m_tempfileName,  m_wxMaxima.m_tempfileName + ".wxmx");
  m_wxMaxima.m_tempfileName = m_wxMaxima.m_tempfileName + ".wxmx";

  /* if the current filename is empty - the file was not saved under a given name - save it using a temporary file name */
  if (m_wxMaxima.m_configuration.AutoSaveAsTempFile() || m_wxMaxima.GetWorksheet()->GetCurrentFile().IsEmpty()) {
    bool saved = Format::ExportToWXMX(m_wxMaxima.GetWorksheet()->GetTree(), m_wxMaxima.m_tempfileName,
                                      &m_wxMaxima.m_configuration,
                                      &m_wxMaxima.GetWorksheet()->GetViewCellPointers(),
                                      m_wxMaxima.m_variablesPane->GetVarnames(),
                                      m_wxMaxima.GetWorksheet()->GetHCaret());
    wxFileName m_tempfileName_permissions(m_wxMaxima.m_tempfileName);
    m_tempfileName_permissions.SetPermissions(wxPOSIX_USER_READ | wxPOSIX_USER_WRITE);

    wxLogMessage(_("Autosaving as temp file %s"), m_wxMaxima.m_tempfileName);
    if ((m_wxMaxima.m_tempfileName != oldTempFile) && saved) {
      m_wxMaxima.GetWorksheet()->SetSaved(true);
      if (!oldTempFile.IsEmpty()) {
        if (wxFileExists(oldTempFile)) {
          wxLogMessage(_("Trying to remove the old temp file %s"), oldTempFile);
          wxRemoveFile(oldTempFile);
        }
      }
    }
    m_wxMaxima.RegisterAutoSaveFile();
  } else {
    wxLogMessage(_("Autosaving the .wxmx file as %s"), m_wxMaxima.GetWorksheet()->GetCurrentFile());
    savedWas = SaveFile(false);
  }

  m_wxMaxima.GetWorksheet()->SetSaved(savedWas);
  m_wxMaxima.ResetTitle(savedWas, true);

  oldTempFile = m_wxMaxima.m_tempfileName;
  m_wxMaxima.GetWorksheet()->SetCurrentFile(oldFilename);
  return savedWas;
}

