// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2009-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2015-2019 Gunter Königsmann     <wxMaxima@physikbuch.de>
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
  This file defines the class MaximaManual.

  MaximaManual creates the list of maxima's manual anchors.
*/

#include "MaximaManual.h"
#include "Dirstructure.h"
#include "main.h"
#include "wxm_manual_anchors_xml.h"
#include <wx/busyinfo.h>
#include <wx/log.h>
#include <wx/mstream.h>
#include <wx/tokenzr.h>
#include <wx/txtstrm.h>
#include <wx/uri.h>
#include <wx/utils.h>
#include <wx/wfstream.h>

wxDECLARE_APP(MyApp);

MaximaManual::MaximaManual(Configuration *configuration):
  m_abortBackgroundTask(false),
  m_configuration(configuration)
{
}

MaximaManual::HelpFileAnchors MaximaManual::GetHelpfileAnchors() {
  const std::lock_guard<std::mutex> lock(m_helpFileAnchorsLock);
  return m_helpFileAnchors;
}

wxString MaximaManual::GetHelpfileAnchorName(wxString keyword) {
  const std::lock_guard<std::mutex> lock(m_helpFileAnchorsLock);

  if(m_helpFileAnchors.size() < 100)
    LoadBuiltInManualAnchors();

  auto anchor = m_helpFileAnchors.find(keyword);
  if (anchor == m_helpFileAnchors.end())
    return wxEmptyString;
  else
    return anchor->second;
}

wxString MaximaManual::GetHelpfileUrl_Singlepage(const wxString &keyword) {
  const std::lock_guard<std::mutex> lock(m_helpFileAnchorsLock);
  auto anchor = m_helpFileURLs_singlePage.find(keyword);
  if (anchor == m_helpFileURLs_singlePage.end())
    return wxEmptyString;
  else
    return anchor->second;
}
wxString MaximaManual::GetHelpfileUrl_FilePerChapter(const wxString &keyword) {
  const std::lock_guard<std::mutex> lock(m_helpFileAnchorsLock);

  auto anchor = m_helpFileURLs_filePerChapter.find(keyword);
  if (anchor == m_helpFileURLs_filePerChapter.end())
    return wxEmptyString;
  else
    return anchor->second;
}

bool MaximaManual::LoadBuiltInManualAnchors() {
  wxLogMessage(_("Using the built-in list of manual anchors."));
  wxMemoryInputStream istream(MANUAL_ANCHORS_XML, MANUAL_ANCHORS_XML_SIZE);
  wxXmlDocument xmlDoc;
  if (!xmlDoc.Load(istream))
    return false;
  if (!LoadManualAnchorsFromXML(xmlDoc, false))
    return false;
  return true;
}

bool MaximaManual::LoadManualAnchorsFromCache() {
  wxString anchorsFile = Dirstructure::Get()->AnchorsCacheFile();
  if (!wxFileExists(anchorsFile)) {
    wxLogMessage(_("No file with the subjects the manual contained in the last "
                   "wxMaxima run."));
    return false;
  }
  wxXmlDocument xmlDocument(anchorsFile);
  if (!xmlDocument.IsOk()) {
    wxLogMessage(_("The cache for the subjects the manual contains cannot be read."));
    wxRemoveFile(anchorsFile);
    return false;
  }

  if (LoadManualAnchorsFromXML(xmlDocument)) {
    wxLogMessage(_("Read the entries the maxima manual offers from %s"),
                 Dirstructure::Get()->AnchorsCacheFile().utf8_str());
    return true;
  }
  const std::lock_guard<std::mutex> lock(m_helpFileAnchorsLock);
  return !m_helpFileURLs_singlePage.empty();
}

void MaximaManual::AnchorAliasses(HelpFileAnchors &anchors) {
  HelpFileAnchors aliasses;
  aliasses["%solve"] = "to_poly_solve";
  aliasses["find_root_error"] = "find_root";
  aliasses["wxbarsplot"] = "barsplot";
  aliasses["wxboxplot"] = "boxplot";
  aliasses["wxhistogram"] = "histogram";
  aliasses["wxpiechart"] = "piechart";
  aliasses["wxscatterplot"] = "scatterplot";
  aliasses["wxstarplot"] = "starplot";
  aliasses["wxdrawdf"] = "drawdf";
  aliasses["wxdraw"] = "draw";
  aliasses["wxdraw2d"] = "draw2d";
  aliasses["wxdraw3d"] = "draw3d";
  aliasses["with_slider_draw"] = "draw";
  aliasses["with_slider_draw2d"] = "draw2d";
  aliasses["with_slider_draw3d"] = "draw3d";

  for (auto it = aliasses.begin(); it != aliasses.end(); ++it) {
    if ((anchors.find(it->first) == anchors.end()) &&
        (anchors.find(it->second) != anchors.end()))
      anchors[it->first] = anchors[it->second];
  }
}

void MaximaManual::CompileHelpFileAnchors(const wxString &maximaHtmlDir,
                                          const wxString &maximaVersion,
                                          const wxString &saveName) {

  std::size_t foundAnchorsTotal = 0;
  if (!(m_maximaHtmlDir.IsEmpty())) {
    std::vector<wxString> helpFiles;
    {
      GetHTMLFiles htmlFilesTraverser(helpFiles, m_maximaHtmlDir);
      wxDir dir(m_maximaHtmlDir);
      dir.Traverse(htmlFilesTraverser);
    }
    {
      GetHTMLFiles_Recursive htmlFilesTraverser(
                                                helpFiles, m_configuration->MaximaShareDir());
      wxDir dir(m_configuration->MaximaShareDir());
      dir.Traverse(htmlFilesTraverser);
    }

    for (const auto &file : helpFiles) {
      if(m_abortBackgroundTask)
            return;
      bool is_Singlepage = file.Contains("_singlepage.");
      wxString fileURI = wxURI(wxS("file://") + file).BuildURI();
      // wxWidgets cannot automatically replace a # as it doesn't know if it is
      // a anchor separator
      fileURI.Replace("#", "%23");
#ifdef __WINDOWS__
      fileURI.Replace("\\", "/");
#endif
#ifdef __WXMSW__
      // Fixes a missing "///" after the "file:". This works because we always
      // get absolute file names.
      wxRegEx uriCorector1("^file:([a-zA-Z]):");
      wxRegEx uriCorector2("^file:([a-zA-Z][a-zA-Z]):");

      uriCorector1.ReplaceFirst(&fileURI, wxS("file:///\\1:"));
      uriCorector2.ReplaceFirst(&fileURI, wxS("file:///\\1:"));
#endif

      wxRegEx idExtractor(".*<span id=\\\"([a-zAZ0-9_-]*)\\\"");
      wxRegEx idExtractor2("<dt id=\\\"(index-[a-zAZ0-9_-]*)\\\"");
      wxRegEx idExtractor3("<dt [^>]* id=\\\"(index-[a-zAZ0-9_-]*)\\\"");
      wxRegEx idExtractor_oldManual(".*<a name=\\\"([a-zAZ0-9_-]*)\\\"");
      wxString escapeChars = "`\"^()<=>[]`%?;\\$%&+-*/.!\'@#:^_";
      wxFileInputStream input(file);
      if (input.IsOk()) {
        wxTextInputStream text(input, wxS('\t'),
                               wxConvAuto(wxFONTENCODING_UTF8));
        while (input.IsOk() && !input.Eof()) {
          if(m_abortBackgroundTask)
            return;
          wxString line = text.ReadLine();
          wxStringTokenizer tokens(line, wxS(">"));
          while (tokens.HasMoreTokens()) {
            wxString token = tokens.GetNextToken();
            wxString oldToken(token);
            wxString id;
            if (idExtractor.Replace(&token, "\\1") > 0)
              id = token;
            else {
              if (idExtractor2.Replace(&token, "\\1") > 0)
                id = token;
              else
                {
                  if (idExtractor3.Replace(&token, "\\1") > 0)
                    id = token;
                  else {
                    if (idExtractor_oldManual.Replace(&token, "\\1") > 0)
                      id = token;
                  }
                }
            }
            if (!id.IsEmpty()) {
              // anchorless tokens begin with "index-"
              token.Replace("index-", "");
              // In anchors a space is represented by a hyphen
              token.Replace("-", " ");
              // Some other chars including the minus are represented by "_00xx"
              // where xx is being the ascii code of the char.
              for (wxString::const_iterator it = escapeChars.begin();
                   it != escapeChars.end(); ++it)
                token.Replace(wxString::Format("_00%02x", static_cast<char>(*it)), *it);
              // What the g_t means I don't know. But we don't need it
              if (token.StartsWith(wxS("g_t")))
                token = token.Right(token.Length() - 3);
              if(!token.Contains(wxS(" ")))
                {
                  {
                    const std::lock_guard<std::mutex> lock(m_helpFileAnchorsLock);
                    if (is_Singlepage)
                      m_helpFileURLs_singlePage[token] = fileURI + "#" + id;
                    else
                      m_helpFileURLs_filePerChapter[token] = fileURI + "#" + id;
                    m_helpFileAnchors[token] = id;
                  }
                  foundAnchorsTotal++;
                }
            }
          }
        }
      }
      {
        const std::lock_guard<std::mutex> lock(m_helpFileAnchorsLock);
        AnchorAliasses(m_helpFileAnchors);
        AnchorAliasses(m_helpFileURLs_filePerChapter);
        AnchorAliasses(m_helpFileURLs_singlePage);
      }
    }

    if(foundAnchorsTotal < 100)
      {
        wxLogMessage(_("Have only %li keyword anchors at the end of parsing the maxima manual => "
                       "Not caching the result of using the built-in keyword list"),
                     static_cast<long>(foundAnchorsTotal));
        LoadBuiltInManualAnchors();
      }
    else
      {
        SaveManualAnchorsToCache(maximaHtmlDir, maximaVersion, saveName);
      }
  }
}


wxDirTraverseResult
MaximaManual::GetHTMLFiles::OnFile(const wxString &filename) {
  wxFileName newItemName(filename);
  wxString newItem =
    m_prefix + wxFileName::GetPathSeparator() + newItemName.GetFullName();
  newItem.Replace(wxFileName::GetPathSeparator(), "/");
  if (newItem.EndsWith(".html") &&
      (std::find(m_files.begin(), m_files.end(), newItem) == m_files.end()))
    m_files.push_back(newItem);
  return wxDIR_CONTINUE;
}

wxDirTraverseResult
MaximaManual::GetHTMLFiles::OnDir(const wxString &WXUNUSED(dirname)) {
  return wxDIR_IGNORE;
}

wxDirTraverseResult
MaximaManual::GetHTMLFiles_Recursive::OnFile(const wxString &filename) {
  wxFileName newItemName(filename);
  newItemName.MakeAbsolute();
  wxString newItem = newItemName.GetFullPath();
  newItem.Replace(wxFileName::GetPathSeparator(), "/");
  if (newItem.EndsWith(".html") &&
      (std::find(m_files.begin(), m_files.end(), newItem) == m_files.end()))
    m_files.push_back(newItem);
  return wxDIR_CONTINUE;
}

wxDirTraverseResult
MaximaManual::GetHTMLFiles_Recursive::OnDir(const wxString &WXUNUSED(dirname)) {
  return wxDIR_CONTINUE;
}

void MaximaManual::SaveManualAnchorsToCache(const wxString &maximaHtmlDir,
                                            const wxString &maximaVersion,
                                            const wxString &saveName) {
  const std::lock_guard<std::mutex> lock(m_helpFileAnchorsLock);
  auto num = m_helpFileURLs_singlePage.size();
  if (num <= 50) {
    wxLogMessage(_("Found only %li keywords in maxima's "
                   "manual. Not caching them to disc."),
                 static_cast<long>(num));
    return;
  }
  wxXmlAttribute *htmlDir =
    new wxXmlAttribute(wxS("html_dir"), maximaHtmlDir);

  wxXmlAttribute *maximaVersionNode =
    new wxXmlAttribute(wxS("maxima_version"), maximaVersion, htmlDir);

  wxXmlNode *topNode =
    new wxXmlNode(NULL, wxXML_DOCUMENT_NODE, wxEmptyString, wxEmptyString);
  wxXmlNode *headNode =
    new wxXmlNode(topNode, wxXML_ELEMENT_NODE, wxS("maxima_toc"),
                  wxEmptyString, maximaVersionNode);

  MaximaManual::HelpFileAnchors::const_iterator it;
  for (it = m_helpFileAnchors.begin(); it != m_helpFileAnchors.end(); ++it) {
    wxXmlNode *manualEntry =
      new wxXmlNode(headNode, wxXML_ELEMENT_NODE, "entry");
    {
      wxXmlNode *keyNode =
        new wxXmlNode(manualEntry, wxXML_ELEMENT_NODE, "key");
      new wxXmlNode(keyNode, wxXML_TEXT_NODE, wxEmptyString, it->first);
    }
    {
      wxXmlNode *anchorNode =
        new wxXmlNode(manualEntry, wxXML_ELEMENT_NODE, "anchor");
      new wxXmlNode(anchorNode, wxXML_TEXT_NODE, wxEmptyString, it->second);
    }
    if (m_helpFileURLs_singlePage.find(it->first) !=
        m_helpFileURLs_singlePage.end()) {
      wxXmlNode *keyNode =
        new wxXmlNode(manualEntry, wxXML_ELEMENT_NODE, "url_singlepage");
      new wxXmlNode(keyNode, wxXML_TEXT_NODE, wxEmptyString,
                    m_helpFileURLs_singlePage[it->first]);
    }
    if (m_helpFileURLs_filePerChapter.find(it->first) !=
        m_helpFileURLs_filePerChapter.end()) {
      wxXmlNode *keyNode =
        new wxXmlNode(manualEntry, wxXML_ELEMENT_NODE, "url_fileperchapter");
      new wxXmlNode(keyNode, wxXML_TEXT_NODE, wxEmptyString,
                    m_helpFileURLs_filePerChapter[it->first]);
    }
  }
  wxXmlDocument xmlDoc;
  xmlDoc.SetDocumentNode(topNode);
  wxXmlNode *commentNode = new wxXmlNode(
                                         NULL, wxXML_COMMENT_NODE, wxEmptyString,
                                         _("This file is generated by wxMaxima\n"
                                           "It caches the list of subjects maxima's manual offers and is "
                                           "automatically\n"
                                           "overwritten if maxima's version changes or the file cannot be read"));

  xmlDoc.AppendToProlog(commentNode);
  wxLogMessage(_("Trying to cache the list of subjects the "
                 "manual contains in the file %s."), saveName.utf8_str());
  xmlDoc.Save(saveName);
}

bool MaximaManual::LoadManualAnchorsFromXML(const wxXmlDocument &xmlDocument,
                                            bool checkManualVersion) {
  wxXmlNode *headNode = xmlDocument.GetDocumentNode();
  if (!headNode) {
    wxLogMessage(_("The cache for the subjects the manual contains has no head node."));
    return false;
  }
  headNode = headNode->GetChildren();
  while ((headNode) && (headNode->GetName() != wxS("maxima_toc")))
    headNode = headNode->GetNext();
  if (!headNode) {
    wxLogMessage(_("Anchors file has no top node."));
    return false;
  }
  wxString htmlDir = headNode->GetAttribute(wxS("html_dir"));
  wxString cacheMaximaVersion = headNode->GetAttribute(wxS("maxima_version"));
  wxLogMessage(_("Maxima version: %s, Anchors cache version"),
               m_maximaVersion.utf8_str(),
               cacheMaximaVersion.utf8_str());
  if (checkManualVersion && ((cacheMaximaVersion != m_maximaVersion) ||
                             (htmlDir != m_maximaHtmlDir))) {
    if (cacheMaximaVersion != m_maximaVersion)
      wxLogMessage(_("The cache for the subjects the manual contains is from a "
                     "different Maxima version."));
    if (htmlDir != m_maximaHtmlDir)
      wxLogMessage(_("The help dir from the cache differs from the current one."));
    return false;
  }
  if(!checkManualVersion)
    wxLogMessage(_("Ignoring the cache version for the manual anchors."));
  wxXmlNode *entry = headNode->GetChildren();
  if (entry == NULL) {
    wxLogMessage(_("No entries in the caches for the subjects the manual contains."));
    return false;
  }
  std::size_t anchors = 0;
  std::size_t urls_FilePerChapter = 0;
  std::size_t urls_SinglePage = 0;
  while (entry) {
    if (entry->GetName() == wxS("entry")) {
      wxString key;
      wxString url_singlepage;
      wxString url_filePerChapter;
      wxString anchor;
      wxXmlNode *node = entry->GetChildren();
      while (node) {
        if ((node->GetName() == wxS("anchor")) && (node->GetChildren()))
          anchor = node->GetChildren()->GetContent();
        if ((node->GetName() == wxS("key")) && (node->GetChildren()))
          key = node->GetChildren()->GetContent();
        if ((node->GetName() == wxS("url_singlepage")) && (node->GetChildren()))
          url_singlepage = node->GetChildren()->GetContent();
        if ((node->GetName() == wxS("url_fileperchapter")) &&
            (node->GetChildren()))
          url_filePerChapter = node->GetChildren()->GetContent();
        node = node->GetNext();
      }
      if ((!key.IsEmpty()) && (!anchor.IsEmpty()))
        {
          anchors++;
          const std::lock_guard<std::mutex> lock(m_helpFileAnchorsLock);
          m_helpFileAnchors[key] = anchor;
        }
      if ((!key.IsEmpty()) && (!url_filePerChapter.IsEmpty()))
        {
          urls_FilePerChapter++;
          const std::lock_guard<std::mutex> lock(m_helpFileAnchorsLock);
          m_helpFileURLs_filePerChapter[key] = url_filePerChapter;
        }
      if ((!key.IsEmpty()) && (!url_singlepage.IsEmpty()))
        {
          urls_SinglePage++;
          const std::lock_guard<std::mutex> lock(m_helpFileAnchorsLock);
          m_helpFileURLs_singlePage[key] = url_singlepage;
        }
    }
    entry = entry->GetNext();
  }
  wxLogMessage(_("Found %li anchors, URLs (individual files): %li, URLs (singlepage): %li"),
               static_cast<long>(anchors),
               static_cast<long>(urls_FilePerChapter),
               static_cast<long>(urls_SinglePage));
  const std::lock_guard<std::mutex> lock(m_helpFileAnchorsLock);
  return !m_helpFileURLs_singlePage.empty();
}

wxString MaximaManual::GetHelpfileURL(const wxString &keyword) {
  wxString anchor;
  if (m_configuration->SinglePageManual()) {
    anchor = GetHelpfileUrl_Singlepage(keyword);
    if(anchor.IsEmpty())
      anchor = GetHelpfileUrl_FilePerChapter(keyword);
  }
  else {
    anchor = GetHelpfileUrl_FilePerChapter(keyword);
    if(anchor.IsEmpty())
      anchor = GetHelpfileUrl_Singlepage(keyword);
  }
  return anchor;
}

void MaximaManual::FindMaximaHtmlDir(const wxString &docDir) {
  // One may set the help file location in the wxMaxima configuration (on Unix:
  // ~/.wxMaxima), e.g.
  // helpFile=/usr/local/share/maxima/5.44.0/doc/html/maxima_singlepage.html
  // Use that file, if the configuration option is used.
  wxString headerFile;
  wxConfig::Get()->Read(wxS("helpFile"), &headerFile);
  if (headerFile.Length() && wxFileExists(headerFile)) {
    wxLogMessage(_("Using Maxima help file from wxMaxima configuration file "
                   "(helpFile=...))"));
    return;
  }
#ifdef __CYGWIN__
  // Cygwin uses /c/something instead of c:/something and passes this path to
  // the web browser - which doesn't support cygwin paths => convert the path to
  // a native windows pathname if needed.
  if (headerFile.Length() > 1 && headerFile.at(1) == wxS('/')) {
    headerFile.at(1) = headerFile.at(2);
    headerFile.at(2) = wxS(':');
  }
#endif // __CYGWIN__
  wxPathList helpfilepaths;
  helpfilepaths.Add(docDir);
  helpfilepaths.Add(docDir + "/info");
  helpfilepaths.Add(docDir + "/info/html");
  helpfilepaths.Add(docDir + "/html");
  helpfilepaths.Add(docDir + "/../html");
  helpfilepaths.Add(m_configuration->MaximaShareDir() + "/../doc/html");
  helpfilepaths.Add(m_configuration->MaximaShareDir() + "/doc/html");
  wxString helpfile_location =
    helpfilepaths.FindAbsoluteValidPath("maxima_singlepage.html");
  wxFileName helpfile_cleanup(helpfile_location);
  helpfile_cleanup.Normalize(wxPATH_NORM_ENV_VARS | wxPATH_NORM_DOTS |
                             wxPATH_NORM_TILDE | wxPATH_NORM_ABSOLUTE |
                             wxPATH_NORM_LONG | wxPATH_NORM_SHORTCUT);

  if (helpfile_cleanup.IsFileReadable()) {
    m_maximaHtmlDir = helpfile_cleanup.GetPath();
    wxLogMessage(_("Found the maxima HTML manual in the folder %s."),
                 m_maximaHtmlDir.ToUTF8().data());
  } else {
    m_maximaHtmlDir.clear();
    wxLogMessage(_("Didn't find the maxima HTML manual."));
  }
}

void MaximaManual::LoadHelpFileAnchors(const wxString &docdir,
                                       const wxString &maximaVersion) {
  FindMaximaHtmlDir(docdir);
  m_maximaVersion = maximaVersion;
  {
    const std::lock_guard<std::mutex> lock(m_helpFileAnchorsLock);
    if(!m_helpFileURLs_singlePage.empty())
      return;
  }
  if (!LoadManualAnchorsFromCache()) {
    if (!m_maximaHtmlDir.IsEmpty()) {
      if (m_helpfileanchorsThread.joinable()) {
        wxLogMessage(_("Waiting for the Manual anchors background task."));
        m_helpfileanchorsThread.join();
      }
      wxLogMessage(_("Background task that compiles the Manual anchors scheduled."));
      m_abortBackgroundTask = false;
      if(m_configuration->UseThreads())
        m_helpfileanchorsThread = jthread(&MaximaManual::CompileHelpFileAnchors,
                                          this,
                                          m_maximaHtmlDir,
                                          m_maximaVersion,
                                          Dirstructure::AnchorsCacheFile()
                                          );
      else
        CompileHelpFileAnchors(
                               m_maximaHtmlDir,
                               m_maximaVersion,
                               Dirstructure::AnchorsCacheFile()
                               );
    } else {
      wxLogMessage(_("Maxima help file not found!"));
      LoadBuiltInManualAnchors();
    }
  }
}

MaximaManual::~MaximaManual() {
  if(m_helpfileanchorsThread.joinable())
    {
      m_abortBackgroundTask = true;
      wxLogMessage(_("Waiting for the thread that parses the maxima manual to finish"));
      m_helpfileanchorsThread.join();
    }
}
