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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

#ifndef IMGCELL_H
#define IMGCELL_H

#include "Cell.h"
#include <wx/image.h>
#include "Image.h"
#include "ImgCellBase.h"
#include <memory>

#include <wx/fs_arc.h>

class ImgCell final : public ImgCellBase
{
public:
  ImgCell(GroupCell *group, Configuration *config);
  ImgCell(GroupCell *group, Configuration *config, const wxMemoryBuffer &image, const wxString &type);
  ImgCell(GroupCell *group, Configuration *config, const wxString &image, const wxString &wxmFile, bool remove = true);

  ImgCell(GroupCell *group, Configuration *config, const wxBitmap &bitmap);
  ImgCell(GroupCell *group, const ImgCell &cell);
  std::unique_ptr<Cell> Copy(GroupCell *group) const override;
  const CellTypeInfo &GetInfo() override;
  ~ImgCell() override;

  //! This class can be derived from wxAccessible which has no copy constructor
  ImgCell &operator=(const ImgCell&) = delete;

  //! Tell the image which gnuplot files it was made from
  void GnuplotSource(wxString sourcefile, wxString datafile, const wxString &wxmFile)
    { if (m_image) m_image->GnuplotSource(std::move(sourcefile),
                                          std::move(datafile),
                                          wxmFile); }
  void CompressedGnuplotSource(wxString sourcefile, wxString datafile,
                               const wxString &wxmFile)
    { if (m_image) m_image->CompressedGnuplotSource(std::move(sourcefile),
                                                    std::move(datafile),
                                                    wxmFile); }

  //! The name of the file with gnuplot commands that created this file
  wxString GnuplotSource() const override
    { return m_image ? m_image->GnuplotSource() : wxString(); }

  void LoadImage(wxString image, bool remove = true);

  //! Set the image's resolution
  void SetPPI(int ppi) override {m_image->SetPPI(ppi);}
  int GetPPI() const override {return m_image->GetPPI();}
  size_t GetOriginalWidth() const override {return m_image->GetOriginalWidth();}
  size_t GetOriginalHeight() const override {return m_image->GetOriginalHeight();}

  void ReloadImage(const wxString &image, const wxString &wxmFile);

  //! Can this image be exported in SVG format?
  bool CanExportSVG() const override {return (m_image != NULL) && m_image->CanExportSVG();}

  friend class AnimationCell;

  /*! Writes the image to a file

    The image file that is written is either a bit-per-bit copy of the original
    file loaded into the ImgCell - or in the case that there is no original file
    a losslessly compressed png version of the bitmap.

    See also GetExtension().
  */
  wxSize ToImageFile(wxString filename) override;

  /*! Removes the cached scaled image from memory

    The scaled version of the image will be recreated automatically once it is
    needed.
  */
  void ClearCache() override { if (m_image) m_image->ClearCache(); }

  const wxString &GetToolTip(wxPoint point) const override;

  //! Sets the bitmap that is shown
  void SetBitmap(const wxBitmap &bitmap);

  //! Copies the cell to the system's clipboard
  bool CopyToClipboard() const override;

  void DrawRectangle(bool draw) { m_drawRectangle = draw; }

  //! Returns the file name extension that matches the image type
  wxString GetExtension() const override
    { if (m_image)return m_image->GetExtension(); else return wxEmptyString; }

  //! Returns the name of the file the image was originally created from
  wxString GetOrigImageFile() const
    { return m_origImageFile; }

  //! Sets the name of the file the image was originally created from
  void SetOrigImageFile(wxString file)
    { m_origImageFile = file; }

  //! Returns the original compressed version of the image
  wxMemoryBuffer GetCompressedImage() const { return m_image->m_compressedImage; }

  wxCoord GetMaxWidth() const override { return m_image ? m_image->GetMaxWidth() : -1; }
  wxCoord GetHeightList() const override { return m_image ? m_image->GetHeightList() : -1; }
  void SetMaxWidth(wxCoord width) override { if (m_image) m_image->SetMaxWidth(width); }
  void SetMaxHeight(wxCoord height) override { if (m_image) m_image->SetMaxHeight(height); }

  void Recalculate(AFontSize fontsize) override;

  void Draw(wxPoint point, wxDC *dc, wxDC *antialiassingDC) override;

  wxString ToMatlab() const override;
  wxString ToRTF() const override;
  wxString ToString() const override;
  wxString ToTeX() const override;
  wxString ToXML() const override;

  bool CanPopOut() const override { return m_image && m_image->HasGnuplotSource(); }

private:
  void SetConfiguration(Configuration *config) override;
  void DrawBoundingBox(wxDC &WXUNUSED(dc), bool WXUNUSED(all) = false) override;
  int GetImageBorderWidth() const override { return m_imageBorderWidth; }

  std::shared_ptr<Image> m_image;

  CellPointers *const m_cellPointers = GetCellPointers();

  int m_imageBorderWidth = 0;

//** Bitfield objects (1 bytes)
//**
  void InitBitFields_ImgCell()
    { // Keep the initialization order below same as the order
      // of bit fields in this class!
      m_drawRectangle = true;
      m_drawBoundingBox = false;
    }
  bool m_drawRectangle : 1 /* InitBitFields_ImgCell */;
  bool m_drawBoundingBox : 1 /* InitBitFields_ImgCell */;

  static int s_counter;

  wxString m_origImageFile;
};

#endif // IMGCELL_H
