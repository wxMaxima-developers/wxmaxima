// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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

/*!\file
  
  The definition of the class RecentDocuments that provides a recent files 
  mechanism that is extensible to multiple file types.
*/

#ifndef RECENTDOCUMENTS_H
#define RECENTDOCUMENTS_H

#include <list>
#include "precomp.h"
#include <wx/wx.h>
#include <wx/config.h>
#include <wx/tokenzr.h>
#include <wx/string.h>


//! A class that maintains a list of recent documents.
class RecentDocuments
{
public:
  /*! The constructor.
    
    \param documentType The name of the list of recent documents to handle with this instance
    of the class.

    We cannot use wxWidget's recent documents class as we handle several types of recent
    documents:
    * Documents, 
    * temporary save files for unsaved files and 
    * maxima packages
    */
  explicit RecentDocuments(wxString documentType);
  //! Add a new recent document.
  void AddDocument(wxString name);
  //! Load the recent documents list.
  void Load();
  //! Save the recent documents list.
  void Save();
  //! Get the list of recent documents
  std::list<wxString> Get() const {return m_listOfFiles;}
  /*! Get the nth item of the list of recent documents

    \note This function traverses the list of recent documents. Therefore it is
    slower than an iterator if one needs to access each element in turn.
  */
  wxString Get(int num) const;
  //! Make sure that we save the list of recent documents on closing the program
  ~RecentDocuments(){Save();}
private:
  //! The list of recent documents.
  std::list<wxString> m_listOfFiles;
  //! The name of the type of document this instance of this class deals with
  wxString m_documentType;
};
#endif
