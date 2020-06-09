// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2015      Gunter Königsmann <wxMaxima@physikbuch.de>
//  Copyright (C) 2020      Kuba Ober <kuba@bertec.com>
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

#ifndef OUTCOMMON_H
#define OUTCOMMON_H

#include "Configuration.h"
#include <wx/dataobj.h>
#include <memory>

/*! \file
 * This is the header for common code used by various file output modules that render
 * cells to files.
 */

class Cell;
class wxDC;

//! A collection of common code used in rendering the cells to a non-default output,
//! e.g. a graphics file.
class OutCommon
{
public:
  //! An object that can be filled with output data in given format for the clipboard
  class DataObject final : public wxCustomDataObject
  {
    wxMemoryBuffer m_databuf;

  public:
    explicit DataObject(const wxDataFormat &format, const wxMemoryBuffer &data);
    bool GetDataHere(void *buf) const override;
    size_t GetDataSize() const override;
    void Free() override;
  };

  //! Returns the representation that can be placed on the clipboard. Removes the
  //! file the data was saved in.
  std::unique_ptr<DataObject> GetDataObject(const wxDataFormat &format);

  /*! Constructs the instance of the helper, temporarily overriding the
   * configuration.
   *
   * \param fullWidth multiplied by scale is only used as maximum width during line
   *        breaking.
   * \param configuration The configuration to build upon
   * \param filename The name of the output file
   * \param scale The scaling factor of the output document
   */
  explicit OutCommon(Configuration **configuration, const wxString &filename,
                     int fullWidth, double scale);
  explicit OutCommon(Configuration **configuration, int fullWidth, double scale);
  ~OutCommon();

  OutCommon(const OutCommon&) = delete;
  void operator=(const OutCommon&) = delete;

  int Scale_Px(double px) const { return m_thisconfig.Scale_Px(px); }
  double GetScale() const { return m_scale; }
  const wxString &GetFilename() const { return m_filename; }
  const wxString &GetTempFilename() const { return m_tempFilename; }
  Configuration &GetConfiguration() { return m_thisconfig; }
  wxSize getPPI() const { return m_ppi; }

  //! Sets the context for the configuration used in recalculating the cell dimensions.
  void SetRecalculationContext(wxDC &context) { m_recalculationDc = &context; }

  //! Prepares to render the tree to the output DC, and computes the size of the output.
  bool PrepareLayout(Cell *tree);

  //! Returns the size of the prepared output layout
  wxSize GetSize() const { return m_size; }
  //! Sets the "default" size of the prepared layout
  void SetSize(wxSize size) { m_size = size; }

  //! Returns the size of the prepared output layout multiplied by the scale factor
  wxSize GetScaledSize() const;
  //! Returns the size of the prepared output layout divided by the scale factor
  wxSize GetInvScaledSize() const;

  //! Copies the representation of the list of cells that was passed to SetData()
  //! to the clipboard.
  bool ToClipboard(const wxDataFormat &format);

  //! Recursively draws the cells.
  void Draw(Cell *tree);

private:
  void GetMaxPoint(Cell *tree, int *width, int *height) const;
  void RecalculateWidths(Cell *tree) const;
  void RecalculateHeight(Cell *tree) const;

  void BreakLines(Cell *tree) const;
  void BreakUpCells(Cell *tree);

  //! The name of a temp file we create while calculating the output size.
  wxString m_tempFilename;
  //! The draw context we draw to during recalculation.
  wxDC *m_recalculationDc = {};

  wxString m_filename;
  Configuration **m_configuration;
  Configuration *m_oldconfig = *m_configuration;
  Configuration m_thisconfig;
  //! How many times the natural resolution do we want this output to be?
  double m_scale = 1.0;
  //! The size of the current output
  wxSize m_size = wxDefaultSize;
  //! The maximum width of the rendered output
  int m_fullWidth;
  //! The resolution of the output (useful for bitmaps only)
  wxSize m_ppi;
};

#endif  // OUTCOMMON_H
