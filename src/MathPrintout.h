// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2016 Gunter Königsmann <wxMaxima@physikbuch.de>
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

/*! \file
  This file declares the class MathPrintOut

  MathPrintOut is the class that handles printing.
*/

#ifndef MATHPRINTOUT_H
#define MATHPRINTOUT_H

#include <wx/wx.h>
#include <wx/print.h>

#include <vector>

#include "MathCell.h"
#include "GroupCell.h"

using namespace std;

class MathPrintout : public wxPrintout
{
public:
  MathPrintout(wxString title, Configuration **configuration);

  ~MathPrintout();

  void DestroyTree();

  void DestroyTree(GroupCell *tree);

  void SetData(GroupCell *tree);

  void SetupData();

  void BreakPages();

  void Recalculate();

  bool OnPrintPage(int num);

  bool HasPage(int num);

  void GetPageInfo(int *minPage, int *maxPage, int *pageFrom, int *pageTo);

  bool OnBeginDocument(int startPage, int endPage);

  void OnPreparePrinting();

  void GetPageMargins(int *horizontal, int *vertical);

  int GetHeaderHeight();

  void PrintHeader(int pageNum, wxDC *dc);

private:
  Configuration **m_configuration, *m_oldconfig;
  int m_numberOfPages;
  bool m_printConfigCreated;
  wxString m_title;
  GroupCell *m_tree;
  vector<GroupCell *> m_pages;
};

#endif // MATHPRINTOUT_H
