// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2015      Gunter Königsmann <wxMaxima@physikbuch.de>
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

#ifndef SVGOUT_H
#define SVGOUT_H

#include "Cell.h"

#include <wx/dcsvg.h>
/* Renders portions of the work sheet (including 2D maths) as svg.

   This is used for exporting HTML with embedded maths as a scalable vector
   graphics and for them on the clipboard
 */
class Svgout
{
public:
  /*! The constructor.
  */
  explicit Svgout(Configuration **configuration, wxString filename = wxEmptyString, double scale = 1.0);

  ~Svgout();
  
  /*! Renders tree as svg
    
    \param tree The list of cells that is to be rendered
    \return true, if the svgout could be created.
   */
  wxSize SetData(Cell *tree);
  
  //! Copies the svg representation of the list of cells that was passed to SetData()
  bool ToClipboard();

protected:
  void DestroyTree();

  // cppcheck-suppress functionConst
  void RecalculateWidths();

  // cppcheck-suppress functionConst
  void BreakLines();

  // cppcheck-suppress functionConst
  void RecalculateHeight();

  void GetMaxPoint(int *width, int *height);

  // cppcheck-suppress functionConst
  void BreakUpCells();

  bool Layout();

  void Draw();

  Cell *m_tree;

  double GetRealHeight() const;

  double GetRealWidth() const;

  
  /*! An object that can be filled with SVG data for the clipboard
   */
  class SVGDataObject : public wxCustomDataObject
  {
  public:
    explicit SVGDataObject(wxMemoryBuffer data);

    SVGDataObject();

  private:
    //! A class that publishes MathML data to the clipboard
    wxCharBuffer m_databuf;
  };

private:
  //! This class doesn't have a copy constructor
  Svgout(const Svgout&) = delete;
  //! This class doesn't have a = operator
  Svgout& operator=(const Svgout&) = delete;

  int Scale_Px(double px){ return (*m_configuration)->Scale_Px(px);}
  //! The name of a temp file we create while calculating the svg size.
  wxString m_tempFileName;
  //! The draw context we draw to during recalculation.
  wxSVGFileDC *m_recalculationDc;
  //! The draw context we draw to.
  wxSVGFileDC *m_dc;
  static wxDataFormat m_svgFormat;
  wxString m_filename;
  Configuration **m_configuration, *m_oldconfig;
  //! How many times the natural resolution do we want this svgout to be?
  double m_scale;
  //! The width of the current svgout;
  int m_width;
  //! The height of the current svgout;
  int m_height;
  //! The resolution of the svgout.
  wxSize m_ppi;

  /*! The current working directory we were in when we started creating a svg file

    wxWidgets tends to place bitmaps it links to svg files in its current working
    directory, not in the dir of the .svg file so we temporarily switch the working
    directory.
   */
  wxString m_CWD;
public:
  //! Returns the svg representation in a format that can be placed on the clipBoard.
  SVGDataObject *GetDataObject();
};

#endif // SVGOUT_H
