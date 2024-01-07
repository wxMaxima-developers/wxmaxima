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

/*! \file
  This file declares the class SumCell

  SumCell is the Cell type that represents maxima's <code>sum()</code>,
  <code>lsum</code> and <code>product()</code>
  commands.
*/
#ifndef PRODUCTCELL_H
#define PRODUCTCELL_H

#include "SumCell.h"

//cppcheck-suppress ctuOneDefinitionRuleViolation
class ProductCell final : public SumCell
{
public:
  ProductCell(GroupCell *group, Configuration *config,
          std::unique_ptr<Cell> &&under, std::unique_ptr<Cell> &&over,
          std::unique_ptr<Cell> &&base);
  ProductCell(GroupCell *group, const ProductCell &cell);
  std::unique_ptr<Cell> Copy(GroupCell *group) const override;
  const CellTypeInfo &GetInfo() override;

protected:
  //! What maxima command name corresponds to this cell?
  virtual const wxString GetMaximaCommandName() const override;
  //! What matlab command name corresponds to this cell?
  virtual const wxString GetMatlabCommandName() const override;
  //! What LaTeX command name corresponds to this cell?
  virtual const wxString GetLaTeXCommandName() const override;
  //! What unicode symbol name corresponds to this cell?
  virtual const wxString GetUnicodeSymbol() const override;
  //! Returns the data that creates our SVG symbol
  virtual const wxString GetSvgSymbolData() const override;
  //! Returns the type our cell has when saving it to .wxmx
  virtual const wxString GetXMLType() const override;
  //! How big do we want our svg symbol to be?
  virtual const wxSize GetSymbolSize() const override;
private:
  const static wxString m_svgProdSign;
};

#endif // PRODUCTCELL_H
