// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2009-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2015 Gunter Königsmann <wxMaxima@physikbuch.de>
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

/*! \file

  This file contains the definition of the class Structure that handles the 
  table of contents pane.
 */
#include <wx/wx.h>
#include <vector>
#include "GroupCell.h"

#ifndef STRUCTURE_H
#define STRUCTURE_H

enum {
  structure_ctrl_id = 4,
  structure_regex_id
};

/*! This class generates a pane containing the table of contents.

 */
class Structure : public wxPanel
{
public:
  Structure(wxWindow* parent, int id);
  /* The destructor
   */
  ~Structure();
  //! Add a file to the recently opened files list.
  void AddToStructure(wxString cmd);
  //! What happens if someone changes the search box contents
  void OnRegExEvent(wxCommandEvent &ev);
  /*! Update the structure information from the tree 

    Since this function traverses the tree and we don't want it 
    to impact the performance too much
      - we call it only on creation of a cell and on leaving it again
      - and we only traverse the tree if the pane is actually shown.
   */
  void Update(MathCell* tree,GroupCell *pos);
  //! Get the nth Cell in the table of contents.
  MathCell *GetCell(int index){return m_structure[index];}
private:
  //! Update the displayed contents.
  void UpdateDisplay();
  wxListBox *m_displayedItems;
  wxTextCtrl *m_regex;
  
  std::vector <MathCell *> m_structure;
  int m_current;
  DECLARE_EVENT_TABLE()
};

#endif // STRUCTURE_H
