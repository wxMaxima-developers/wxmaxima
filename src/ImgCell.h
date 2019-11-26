// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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
//  SPDX-License-Identifier: GPL-2.0+

#ifndef IMGCELL_H
#define IMGCELL_H

#include "Cell.h"
#include <wx/image.h>
#include "Image.h"

#include <wx/filesys.h>
#include <wx/fs_arc.h>

class ImgCell : public Cell
{
public:
  ImgCell(Cell *parent, Configuration **config, CellPointers *cellpointers);
  ImgCell(Cell *parent, Configuration **config, CellPointers *cellPointers, wxMemoryBuffer image, wxString type);
  ImgCell(Cell *parent, Configuration **config, CellPointers *cellPointers, wxString image, bool remove = true,
          wxFileSystem *filesystem = NULL);

  ImgCell(Cell *parent, Configuration **config, CellPointers *cellPointers, const wxBitmap &bitmap);
  ImgCell(const ImgCell &cell);
  Cell *Copy() override {return new ImgCell(*this);}
  ImgCell& operator=(const ImgCell &other);
  ~ImgCell();

  //! Tell the image which gnuplot files it was made from
  void GnuplotSource(wxString sourcefile, wxString datafile, wxFileSystem *filesystem = NULL)
    {
      if(m_image != NULL)
        m_image->GnuplotSource(sourcefile,datafile, filesystem);
    }
  //! The name of the file with gnuplot commands that created this file
  wxString GnuplotSource() const
    {
      if(m_image == NULL)
        return wxEmptyString;
      else
        return m_image->GnuplotSource();
    }
  //! The name of the file with gnuplot data needed for creating this file
  wxString GnuplotData() const
    {
      if(m_image == NULL)
        return wxEmptyString;
      else
        return m_image->GnuplotData();
    }

  std::list<Cell *> GetInnerCells() override;
  void MarkAsDeleted() override;

  void LoadImage(wxString image, bool remove = true);

  friend class SlideShow;

  /*! Writes the image to a file

    The image file that is written is either a bit-per-bit copy of the original
    file loaded into the ImgCell - or in the case that there is no original file
    a losslessly compressed png version of the bitmap.

    See also GetExtension().
   */
  wxSize ToImageFile(wxString filename);

  /*! Removes the cached scaled image from memory

    The scaled version of the image will be recreated automatically once it is 
    needed.
   */
  virtual void ClearCache() override
  { if (m_image)m_image->ClearCache(); }

  virtual wxString GetToolTip(const wxPoint &point) override;
  
  //! Sets the bitmap that is shown
  void SetBitmap(const wxBitmap &bitmap);

  //! Copies the cell to the system's clipboard
  bool CopyToClipboard() override;

  void DrawRectangle(bool draw)
  { m_drawRectangle = draw; }

  //! Returns the file name extension that matches the image type
  wxString GetExtension() const
  { if (m_image)return m_image->GetExtension(); else return wxEmptyString; }

  //! Returns the original compressed version of the image
  wxMemoryBuffer GetCompressedImage() const
  { return m_image->m_compressedImage; }

  double GetMaxWidth() const {if(m_image != NULL) return m_image->GetMaxWidth(); else return -1;}
  double GetMaxHeight() const {if(m_image != NULL) return m_image->GetMaxHeight();else return -1;}
  void SetMaxWidth(double width) const {if(m_image != NULL) return m_image->SetMaxWidth(width);}
  void SetMaxHeight(double height) const {if(m_image != NULL) return m_image->SetMaxHeight(height);}

  void RecalculateHeight(int fontsize) override;

  void RecalculateWidths(int fontsize) override;

  virtual void Draw(wxPoint point) override;

  wxString ToString() override;

  wxString ToMatlab() override;

  wxString ToRTF() override;

  wxString ToTeX() override;

  wxString ToXML() override;

protected:
  Image *m_image;
  
  static int s_counter;
  bool m_drawRectangle;

  virtual void DrawBoundingBox(wxDC &WXUNUSED(dc), bool WXUNUSED(all) = false) override
  {
    m_drawBoundingBox = true;
  }

private:
  bool m_drawBoundingBox;
};

#endif // IMGCELL_H
