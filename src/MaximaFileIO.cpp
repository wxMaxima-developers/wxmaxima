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
#include "Worksheet.h"
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
                                              wxString firstLine) {
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
        if (line.Contains(wxS("<")))
          result += ReadPotentiallyUnclosedTag(lines, line);
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
  if (wxFile(file, wxFile::read).Eof()) {
    document->ClearDocument();
    if(m_wxMaxima.GetWorksheet())
      m_wxMaxima.GetWorksheet()->SetCurrentFile(file);
    m_wxMaxima.m_processManager.StartMaxima();

    m_wxMaxima.ResetTitle(true, true);
    document->SetSaved(true);
    return true;
  }

  // open wxmx file
  wxXmlDocument xmldoc;

  wxFileInputStream wxmxFile(file);
  wxZipInputStream wxmxContents(wxmxFile);
  wxZipEntry *contentsEntry = NULL;
  while(!wxmxContents.Eof())
    {
      contentsEntry = wxmxContents.GetNextEntry();
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
      while (wxmxContents.IsOk() && !wxmxContents.Eof())
        contents += istream1.ReadLine() + wxS("\n");
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
        while (input.IsOk() && !input.Eof()) {
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

    {
      // Write the string into a memory buffer
      wxMemoryOutputStream ostream;
      wxTextOutputStream txtstrm(ostream);
      txtstrm.WriteString(contents);
      wxMemoryInputStream istream(ostream);

      // Try to load the file from the memory buffer.
#if wxCHECK_VERSION(3, 3, 0)
      xmldoc.Load(istream, wxXMLDOC_KEEP_WHITESPACE_NODES);
#else
      xmldoc.Load(istream, wxS("UTF-8"), wxXMLDOC_KEEP_WHITESPACE_NODES);
#endif
    }

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

      {
        // Write the string into a memory buffer
        wxMemoryOutputStream ostream;
        wxTextOutputStream txtstrm(ostream);
        txtstrm.WriteString(contents);
        wxMemoryInputStream istream(ostream);

        // Try to load the file from the memory buffer.
#if wxCHECK_VERSION(3, 3, 0)
        xmldoc.Load(istream, wxXMLDOC_KEEP_WHITESPACE_NODES);
#else
        xmldoc.Load(istream, wxS("UTF-8"), wxXMLDOC_KEEP_WHITESPACE_NODES);
#endif
      }
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
      {
        // Write the string into a memory buffer
        wxMemoryOutputStream ostream;
        wxTextOutputStream txtstrm(ostream);
        txtstrm.WriteString(contents);
        wxMemoryInputStream istream(ostream);

        // Try to load the file from the memory buffer.
#if wxCHECK_VERSION(3, 3, 0)
        xmldoc.Load(istream, wxXMLDOC_KEEP_WHITESPACE_NODES);
#else
        xmldoc.Load(istream, wxS("UTF-8"), wxXMLDOC_KEEP_WHITESPACE_NODES);
#endif
      }
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

    for (long i = 1; i < ActiveCellNumber; i++)
      if (pos)
        pos = pos->GetNext();

    if (m_wxMaxima.GetWorksheet() && pos)
      m_wxMaxima.GetWorksheet()->SetHCaret(pos);
  }
  m_wxMaxima.StatusMaximaBusy(StatusBar::MaximaStatus::waiting);

  return true;
}

bool MaximaFileIO::CheckWXMXVersion(const wxString &docversion) {
  double version = 1.0;
  if (docversion.ToDouble(&version)) {
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

