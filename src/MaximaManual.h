// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2009-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2015 Gunter Königsmann     <wxMaxima@physikbuch.de>
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
  This file declares the class MaximaManual.

  MaximaManual creates the list of autocompletions for a string and allows
  dynamically appending maxima commands to this list as soon as they are defined.
*/

#ifndef MAXIMAMANUAL_H
#define MAXIMAMANUAL_H

#include <atomic>
#include <thread>
#include <mutex>
#include <memory>
#include <vector>
#include <wx/dir.h>
#include <wx/wx.h>
#include <wx/arrstr.h>
#include <wx/regex.h>
#include <wx/xml/xml.h>
#include <wx/filename.h>
#include "precomp.h"
#include "Configuration.h"
#include <unordered_map>

/* The autocompletion logic

   The wordlists for autocompletion for keywords come from several sources:

   - wxMaxima::ReadLoadSymbols receive the contents of maxima's variables
   "values" and "functions" after a package is loaded.
   - all words that appear in the worksheet
   - and a list of maxima's builtin commands.
*/
class MaximaManual
{
public:
  explicit MaximaManual(Configuration *configuration);
#if wxCHECK_VERSION(3, 3, 0) || wxUSE_STL
  typedef std::unordered_map <wxString, wxString> HelpFileAnchors;
#else
  WX_DECLARE_STRING_HASH_MAP(wxString, HelpFileAnchors);
#endif
  HelpFileAnchors GetHelpfileAnchors();
  void FindMaximaHtmlDir(wxString docDir);
  wxString GetHelpfileAnchorName(wxString keyword);
  wxString GetHelpfileUrl_Singlepage(wxString keyword);
  wxString GetHelpfileUrl_FilePerChapter(wxString keyword);
  wxString GetHelpfileURL(wxString keyword);
  //! Search maxima's help file for command and variable names
  void LoadHelpFileAnchors(wxString docdir, wxString maximaVersion);
  //! Collect all keyword anchors in the help file
  void CompileHelpFileAnchors(wxString maximaHtmlDir,
                              wxString maximaVersion,
                              wxString saveName);
  //! Load the result from the last CompileHelpFileAnchors from the disk cache
  bool LoadManualAnchorsFromCache();
  //! Load the help file anchors from an wxXmlDocument
  bool LoadManualAnchorsFromXML(const wxXmlDocument &xmlDocument, bool checkManualVersion = true);
  //! Load the help file anchors from the built-in list
  bool LoadBuiltInManualAnchors();
  //! Save the list of help file anchors to the cache.
  void SaveManualAnchorsToCache(wxString maximaHtmlDir,
                                wxString maximaVersion,
                                wxString saveName);
  ~MaximaManual();
private:
  std::atomic_bool m_abortBackgroundTask;
  //! Add our aliases to a list of anchors
  static void AnchorAliasses(HelpFileAnchors &anchors);
  //! Scans the maxima directory for a list of loadable files
  class GetHTMLFiles : public wxDirTraverser
  {
  public:
    explicit GetHTMLFiles(std::vector<wxString>& files, wxString prefix = wxEmptyString) :
      m_files(files), m_prefix(prefix) { }
    virtual wxDirTraverseResult OnFile(const wxString& filename) override;
    virtual wxDirTraverseResult OnDir(const wxString& dirname) override;
    std::vector<wxString>& GetResult() const {return m_files;}
  protected:
    std::vector<wxString>& m_files;
    wxString m_prefix;
  };
  class GetHTMLFiles_Recursive : public wxDirTraverser
  {
  public:
    explicit GetHTMLFiles_Recursive(std::vector<wxString>& files, wxString prefix = wxEmptyString) :
      m_files(files), m_prefix(prefix) { }
    virtual wxDirTraverseResult OnFile(const wxString& filename) override;
    virtual wxDirTraverseResult OnDir(const wxString& dirname) override;
    std::vector<wxString>& GetResult() const {return m_files;}
  protected:
    std::vector<wxString>& m_files;
    wxString m_prefix;
  };

  //    m_configuration.MaximaShareDir(dir);

  //! The thread the help file anchors are compiled in
  std::unique_ptr<std::thread> m_helpfileanchorsThread;
  std::mutex m_helpFileAnchorsLock;
  //! The configuration storage
  Configuration *m_configuration;
  //! All anchors for keywords maxima's helpfile contains (singlepage version)

  HelpFileAnchors m_helpFileURLs_singlePage;
  //! All anchors for keywords maxima's helpfile contains (file-per-chapter version)
  HelpFileAnchors m_helpFileURLs_filePerChapter;
  //! All anchors for keywords maxima's helpfile contains (without the file name)
  HelpFileAnchors m_helpFileAnchors;
  wxString m_maximaHtmlDir;
  wxString m_maximaVersion;
};

#endif // MAXIMAMANUAL_H
