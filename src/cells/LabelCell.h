// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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

#ifndef LABELCELL_H
#define LABELCELL_H

#include "TextCell.h"

/*! A label cell

  Labels are TextCells that scale down automatically if they need more space 
  than we got.
 */
class LabelCell final : public TextCell
{
public:
  //! The constructor for cell that, if displayed, means that something is amiss
  LabelCell(GroupCell *group,
            Configuration **config, wxString automaticLabel, TextStyle style = TS_MAIN_PROMPT);
  LabelCell(GroupCell *group, const LabelCell &cell);
  std::unique_ptr<Cell> Copy(GroupCell *group) const override;
  const CellTypeInfo &GetInfo() override;

  void Recalculate(AFontSize fontsize) override;
  void Draw(wxPoint point) override;
  bool NeedsRecalculation(AFontSize fontSize) const override;
  void SetStyle(TextStyle style) override;
  wxString ToString() const override;
  //! Set the automatic label maxima has assigned the current equation
  void SetUserDefinedLabel(const wxString &userDefinedLabel);
  //! Returns the XML flags this cell needs in wxMathML
  wxString GetXMLFlags() const override;
  void UpdateDisplayedText() override;
  const wxString &GetAltCopyText() const override;
  void SetAltCopyText(const wxString &WXUNUSED(text)) override;
  wxString ToXML() const override;

private:
//** Large objects (48 bytes)
//**
  //! The user-defined label for this label cell.
  wxString m_userDefinedLabel;

//** 2-byte objects (2 bytes)
//**
  AFontSize m_fontSize_scaledToFit = {};

//** 1-byte objects (1 byte)
//**
  Configuration::showLabels m_labelChoice_Last = {};

//** Bitfield objects (0 bytes)
//**
  void InitBitFields()
  { // Keep the initialization order below same as the order
    // of bit fields in this class!
  }

  TextIndex GetLabelIndex() const;
};

#endif // LABELCELL_H
