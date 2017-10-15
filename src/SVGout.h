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

#ifndef SVGOUT_H
#define SVGOUT_H

#include "MathCell.h"

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
  Svgout(Configuration **configuration, wxString filename = wxEmptyString, int scale = 10);

  ~Svgout();
  
  /*! Renders tree as svg
    
    \param tree The list of cells that is to be rendered
    \param maxSize maxSize tells the maximum size [in square pixels] that will be rendered. 
           -1 means: No limit.

    \return true, if the svgout could be created.
   */
  wxSize SetData(MathCell *tree, long int maxSize = -1);
  
  //! Copies the svg representation of the list of cells that was passed to SetData()
  bool ToClipboard();

protected:
  void DestroyTree();

  void RecalculateWidths();

  void BreakLines();

  void RecalculateHeight();

  void GetMaxPoint(int *width, int *height);

  void BreakUpCells();

  bool Layout(long int maxSize = -1);

  void Draw();

  MathCell *m_tree;

  double GetRealHeight();

  double GetRealWidth();

  
  /*! An object that can be filled with SVG data for the clipboard
   */
  class SVGDataObject : public wxCustomDataObject
  {
  public:
    SVGDataObject(wxMemoryBuffer data);

    SVGDataObject();

  private:
    //! A class that publishes MathML data to the clipboard
    wxCharBuffer m_databuf;
  };

private:
  wxSVGFileDC *m_dc;
  static wxDataFormat m_svgFormat;
  wxString m_filename;
  Configuration **m_configuration, *m_oldconfig;
  //! How many times the natural resolution do we want this svgout to be?
  int m_scale;
  //! The width of the current svgout;
  int m_width;
  //! The height of the current svgout;
  int m_height;
  //! The resolution of the svgout.
  wxSize m_ppi;

public:
  //! Returns the svg representation in a format that can be placed on the clipBoard.
  SVGDataObject *GetDataObject();
};

#endif // SVGOUT_H
