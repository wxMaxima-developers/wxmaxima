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

  /* Determine which cells are the Right places to start a page
   */
  void BreakPages();

  /*! Determine the sizes of all worksheet objects
   */
  void Recalculate();

  /*! Is called by wxWidgets when it wants us to print a specific page */
  virtual bool OnPrintPage(int num) override;

  virtual bool HasPage(int num) override;

  virtual void GetPageInfo(int *minPage, int *maxPage, int *fromPage, int *toPage) override;

  virtual bool OnBeginDocument(int startPage, int endPage) override;

  virtual void OnPreparePrinting() override;

private:
  //! The copy of the worksheet we print
  std::unique_ptr<GroupCell> m_tree;
  //! The cells we determined to be the right page starts
  std::vector<const Cell *> m_pages;
  //! The config that is active during printing
  Configuration m_configuration;
  //! A pointer to m_configuration we can point to
  Configuration *const m_configPointer;
  const double m_scaleFactor;
  // Sets Configuration::Printing() to true while we print
  Printing m_printing;
};

#endif // MATHPRINTOUT_H
