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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  This file declares the class MathPrintOut

  MathPrintOut is the class that handles printing.
*/

#ifndef MATHPRINTOUT_H
#define MATHPRINTOUT_H

#include "precomp.h"
#include <wx/wx.h>
#include <wx/print.h>
#include <wx/dcmemory.h>

#include <vector>

#include "GroupCell.h"

class Printout : public wxPrintout
{
public:
  Printout(wxString title, GroupCell *tree, double scaleFactor);

  ~Printout();

  void DestroyTree();

  void DestroyTree(GroupCell *tree);

  void SetupData();

  void BreakPages();

  void Recalculate();

  bool OnPrintPage(int num);

  bool HasPage(unsigned int num);

  void GetPageInfo(int *minPage, int *maxPage, int *fromPage, int *toPage);

  bool OnBeginDocument(int startPage, int endPage);

  void OnPreparePrinting();

  void GetPageMargins(int *horizontal, int *vertical);

  int GetHeaderHeight();

  void PrintHeader(int pageNum, wxDC *dc);

private:
  wxString m_title;
  std::unique_ptr<GroupCell> m_tree;
  std::vector<Cell *> m_pages;
  Configuration m_configuration;
  Configuration *m_configPointer;
  double m_scaleFactor;
};

#endif // MATHPRINTOUT_H
