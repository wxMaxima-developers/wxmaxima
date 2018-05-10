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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*!\file
  
  The definition of the recent files mechanism
 */

#ifndef RECENTDOCUMENTS_H
#define RECENTDOCUMENTS_H

#include <list>
#include <wx/wx.h>
#include <wx/config.h>
#include <wx/tokenzr.h>
#include <wx/string.h>
#include <wx/config.h>


//! A class that maintains a list of recent documents.
class RecentDocuments
{
public:
  /*! The constructor.
    
    \param documentType The name of the list of recent documents to handle with this instance
    of the class.
   */
  RecentDocuments(wxString documentType);
  //! Add a new recent document.
  void AddDocument(wxString name);
  //! Load the recent documents list.
  void Load();
  //! Save the recent documents list.
  void Save();
  std::list<wxString> Get(){return m_listOfFiles;}
 private:
  std::list<wxString> m_listOfFiles;
  wxString m_documentType;
};
#endif
