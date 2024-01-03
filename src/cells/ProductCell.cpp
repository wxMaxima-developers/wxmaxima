// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
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
  This file defines the class ProductCell

  ProductCell is the Cell type that represents maxima's <code>prod()</code>
  and <code>lprod</code> commands.
*/

#include "ProductCell.h"
#include "CellImpl.h"
#include "prodSign_svg.h"

ProductCell::ProductCell(GroupCell *group, Configuration *config,
                 std::unique_ptr<Cell> &&under, std::unique_ptr<Cell> &&over,
                 std::unique_ptr<Cell> &&base)
  : SumCell(group, config, std::move(under), std::move(over), std::move(base))
{
}

ProductCell::ProductCell(GroupCell *group, const ProductCell &cell)
  : ProductCell(group, cell.m_configuration,
                CopyList(group, cell.Under()),
                CopyList(group, cell.Over()),
                CopyList(group, cell.Base())) {
  CopyCommonData(cell);
  SetAltCopyText(cell.GetAltCopyText());
}

DEFINE_CELL(ProductCell)

const wxString ProductCell::GetMaximaCommandName() const {
  wxString s;
  s = wxS("product(");
  if (Over()->ListToString() == wxEmptyString) 
    s = wxS("lprod(");
  return s;
}

const wxString ProductCell::GetSvgSymbolData() const
{
    return(m_svgProdSign);
}

//! What maxima command name corresponds to this cell?
const wxString ProductCell::GetMatlabCommandName() const
{
  return wxS("product(");
}

const wxString ProductCell::GetLaTeXCommandName() const
{
  return wxS("\\prod");
}

const wxString ProductCell::GetUnicodeSymbol() const
{
  return wxS("\u220F");
}

const wxString ProductCell::GetXMLType() const
{
  wxString type = wxS("prod");
  if (Over()->ListToString() == wxEmptyString)
    type = wxS("lprod");
  return type;
}

const wxSize ProductCell::GetSymbolSize() const
{
  wxSize signSize;
  // A sane height for the product sign
  signSize.y = Scale_Px(40.0);
  // The width of the product sign is defined by its height and aspect ratio
  signSize.x = signSize.y;
  return signSize;
}

const wxString ProductCell::m_svgProdSign(reinterpret_cast<const char*>(PRODSIGN));
