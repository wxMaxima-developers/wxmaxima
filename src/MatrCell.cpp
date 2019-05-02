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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  This file defines the class MatrCell

  MatrCell is the Cell type that represents matrices and matrix-like 
  elements like the table_form command.
*/

#include "MatrCell.h"

MatrCell::MatrCell(Cell *parent, Configuration **config, CellPointers *cellPointers) : Cell(parent, config)
{
  m_cellPointers = cellPointers;
  m_matWidth = 0;
  m_matHeight = 0;
  m_specialMatrix = false;
  m_roundedParens = false;
  m_inferenceMatrix = false;
  m_rowNames = m_colNames = false;
}

Cell *MatrCell::Copy()
{
  MatrCell *tmp = new MatrCell(m_group, m_configuration, m_cellPointers);
  CopyData(this, tmp);
  tmp->m_specialMatrix = m_specialMatrix;
  tmp->m_inferenceMatrix = m_inferenceMatrix;
  tmp->m_roundedParens = m_roundedParens;
  tmp->m_rowNames = m_rowNames;
  tmp->m_colNames = m_colNames;
  tmp->m_matWidth = m_matWidth;
  tmp->m_matHeight = m_matHeight;
  for (unsigned int i = 0; i < m_matWidth * m_matHeight; i++)
    if(i < m_cells.size())
      (tmp->m_cells).push_back(m_cells[i]->CopyList());
  
  return tmp;
}

MatrCell::~MatrCell()
{
  for (unsigned int i = 0; i < m_cells.size(); i++)
  {
    wxDELETE(m_cells[i]);
    m_cells[i] = NULL;
  }
  MarkAsDeleted();
}

std::list<Cell *> MatrCell::GetInnerCells()
{
  std::list<Cell *> innerCells;
  for (unsigned int i = 0; i < m_cells.size(); i++)
    if(m_cells[i])
      innerCells.push_back(m_cells[i]);
  return innerCells;
}



void MatrCell::RecalculateWidths(int fontsize)
{
  Cell::RecalculateWidths(fontsize);
  for (unsigned int i = 0; i < m_cells.size(); i++)
  {
    m_cells[i]->RecalculateWidthsList(MAX(MC_MIN_SIZE, fontsize - 2));
  }
  m_widths.clear();
  for (unsigned int i = 0; i < m_matWidth; i++)
  {
    m_widths.push_back(0);
    for (unsigned int j = 0; j < m_matHeight; j++)
    {
      if((m_matWidth * j + i)<m_cells.size())
        m_widths[i] = MAX(m_widths[i], m_cells[m_matWidth * j + i]->GetFullWidth());
    }
  }
  m_width = 0;
  for (unsigned int i = 0; i < m_matWidth; i++)
  {
    m_width += (m_widths[i] + Scale_Px(10));
  }
  if (m_width < Scale_Px(14))
    m_width = Scale_Px(14);
  ResetData();
}

void MatrCell::RecalculateHeight(int fontsize)
{
  Cell::RecalculateHeight(fontsize);
  for (unsigned int i = 0; i < m_cells.size(); i++)
  {
    m_cells[i]->RecalculateHeightList(MAX(MC_MIN_SIZE, fontsize - 2));
  }
  m_centers.clear();
  m_drops.clear();
  for (unsigned int i = 0; i < m_matHeight; i++)
  {
    m_centers.push_back(0);
    m_drops.push_back(0);
    for (unsigned int j = 0; j < m_matWidth; j++)
      if(m_matWidth * i + j < m_cells.size())
      {
        m_centers[i] = MAX(m_centers[i], m_cells[m_matWidth * i + j]->GetMaxCenter());
        m_drops[i] = MAX(m_drops[i], m_cells[m_matWidth * i + j]->GetMaxDrop());
      }
  }
  m_height = 0;
  for (unsigned int i = 0; i < m_matHeight; i++)
  {
    m_height += (m_centers[i] + m_drops[i] + Scale_Px(10));
  }
  if (m_height == 0)
    m_height = fontsize + Scale_Px(10);
  m_center = m_height / 2;
}

void MatrCell::Draw(wxPoint point)
{
  Cell::Draw(point);
  if (DrawThisCell(point) && InUpdateRegion())
  {
    Configuration *configuration = (*m_configuration);
    wxDC *dc = configuration->GetDC();
    wxPoint mp;
    mp.x = point.x + Scale_Px(5);
    mp.y = point.y - m_center;
    for (unsigned int i = 0; i < m_matWidth; i++)
    {
      mp.y = point.y - m_center + Scale_Px(5);
      for (unsigned int j = 0; j < m_matHeight; j++)
      {
        if((j * m_matWidth + i) < m_cells.size())
          {
            mp.y += m_centers[j];
            wxPoint mp1(mp);
            mp1.x = mp.x + (m_widths[i] - m_cells[j * m_matWidth + i]->GetFullWidth()) / 2;
            m_cells[j * m_matWidth + i]->DrawList(mp1);
            mp.y += (m_drops[j] + Scale_Px(10));
          }
      }
      mp.x += (m_widths[i] + Scale_Px(10));
    }
    SetPen(1.5);
    if (m_specialMatrix)
    {
      if (m_inferenceMatrix)
        dc->DrawLine(point.x + Scale_Px(1),
                    point.y - m_center + Scale_Px(2),
                    point.x + Scale_Px(1),
                    point.y + m_center - Scale_Px(2));
      else
      {
        if (m_rowNames)
          dc->DrawLine(point.x + m_widths[0] + 2 * Scale_Px(5),
                      point.y - m_center + Scale_Px(2),
                      point.x + m_widths[0] + 2 * Scale_Px(5),
                      point.y + m_center - Scale_Px(2));
        if (m_colNames)
          dc->DrawLine(point.x + Scale_Px(1),
                      point.y - m_center + m_centers[0] + m_drops[0] + 2 * Scale_Px(5),
                      point.x + Scale_Px(1) + m_width,
                      point.y - m_center + m_centers[0] + m_drops[0] + 2 * Scale_Px(5));
      }
    }
    else
    {

      if(m_roundedParens)
      {
        SetPen(1);
        int signWidth = Scale_Px(4);
        if (m_height <= signWidth / 3)
          signWidth = m_height / 3;
        
        wxPoint pointList[5];
        // Left bracket
        pointList[0] = wxPoint(point.x + Scale_Px(1) + signWidth,
                               point.y - m_center);
        pointList[1] = wxPoint(point.x + Scale_Px(1) + signWidth / 2,
                               point.y - m_center + signWidth / 2);
        pointList[2] = wxPoint(point.x + Scale_Px(1),
                               point.y);
        pointList[3] = wxPoint(point.x + Scale_Px(1) + signWidth / 2,
                               point.y + m_center - signWidth / 2);
        pointList[4] = wxPoint(point.x + Scale_Px(1) + signWidth,
                               point.y + m_center);
        configuration->GetAntialiassingDC()->DrawSpline(5,pointList);
        pointList[2] = wxPoint(point.x + Scale_Px(1.5),
                               point.y);
        configuration->GetAntialiassingDC()->DrawSpline(5,pointList);

        // Right bracket
        pointList[0] = wxPoint(point.x + m_width - Scale_Px(1) - signWidth,
                               point.y - m_center);
        pointList[1] = wxPoint(point.x + m_width - Scale_Px(1) - signWidth / 2,
                               point.y - m_center + signWidth / 2);
        pointList[2] = wxPoint(point.x + m_width - Scale_Px(1.5),
                               point.y);
        pointList[3] = wxPoint(point.x + m_width - Scale_Px(1) - signWidth / 2,
                               point.y + m_center - signWidth / 2);
        pointList[4] = wxPoint(point.x + m_width - Scale_Px(1) - signWidth,
                               point.y + m_center);
        configuration->GetAntialiassingDC()->DrawSpline(5,pointList);
        pointList[2] = wxPoint(point.x + m_width - Scale_Px(1),
                               point.y);
        configuration->GetAntialiassingDC()->DrawSpline(5,pointList);
      }
      else
      {
        // left bracket
        wxDC *adc = configuration->GetAntialiassingDC();
        adc->DrawLine(point.x + Scale_Px(5),
                      point.y - m_center + Scale_Px(2),
                      point.x + Scale_Px(1),
                      point.y - m_center + Scale_Px(2));
        adc->DrawLine(point.x + Scale_Px(1),
                      point.y - m_center + Scale_Px(2),
                      point.x + Scale_Px(1),
                      point.y + m_center - Scale_Px(2));
        adc->DrawLine(point.x + Scale_Px(1),
                      point.y + m_center - Scale_Px(2),
                      point.x + Scale_Px(5),
                      point.y + m_center - Scale_Px(2));
        // right bracket
        adc->DrawLine(point.x + m_width - Scale_Px(5) - 1,
                      point.y - m_center + Scale_Px(2),
                      point.x + m_width - Scale_Px(1) - 1,
                      point.y - m_center + Scale_Px(2));
        adc->DrawLine(point.x + m_width - Scale_Px(1) - 1,
                      point.y - m_center + Scale_Px(2),
                      point.x + m_width - Scale_Px(1) - 1,
                      point.y + m_center - Scale_Px(2));
        adc->DrawLine(point.x + m_width - Scale_Px(1) - 1,
                      point.y + m_center - Scale_Px(2),
                      point.x + m_width - Scale_Px(5) - 1,
                      point.y + m_center - Scale_Px(2));
      }
    }
    UnsetPen();
  }
}

wxString MatrCell::ToString()
{
  wxString s = wxT("matrix(\n");
  for (unsigned int i = 0; i < m_matHeight; i++)
  {
    s += wxT("\t\t[");
    for (unsigned int j = 0; j < m_matWidth; j++)
    {
	  s += m_cells[i * m_matWidth + j]->ListToString();
      if (j < m_matWidth - 1)
        s += wxT(",\t");
    }
    s += wxT("]");
    if (i < m_matHeight - 1)
      s += wxT(",");
    s += wxT("\n");
  }
  s += wxT("\t)");
  return s;
}

wxString MatrCell::ToMatlab()
{
	//ToDo: We ignore colNames and rowNames here. Are they currently in use?
	wxString s;

	s = wxT("[");
	for (unsigned int i = 0; i < m_matHeight; i++)
	{
	  for (unsigned int j = 0; j < m_matWidth; j++)
	  {
		s += m_cells[i * m_matWidth + j]->ListToMatlab();
		if (j < m_matWidth - 1)
		  s += wxT(", ");
	  }
	  if (i < m_matHeight - 1)
		s += wxT(";\n");
	}

	s += wxT("];");

	return s;
}

wxString MatrCell::ToTeX()
{
  //ToDo: We ignore colNames and rowNames here. Are they currently in use?
  wxString s;

  if (!m_specialMatrix)
  {
    if(m_roundedParens)
      s = wxT("\\begin{pmatrix}");
    else
      s = wxT("\\begin{bmatrix}");
  }
  else
  {
    s = wxT("\\begin{array}{");
    for (unsigned int j = 0; j < m_matWidth; j++)
      s += wxT("c");
    s += wxT("}");
  }
  for (unsigned int i = 0; i < m_matHeight; i++)
  {
    for (unsigned int j = 0; j < m_matWidth; j++)
    {
      s += m_cells[i * m_matWidth + j]->ListToTeX();
      if (j < m_matWidth - 1)
        s += wxT(" & ");
    }
    if (i < m_matHeight - 1)
      s += wxT("\\\\\n");
  }
  if (!m_specialMatrix)
  {
    if(m_roundedParens)
      s += wxT("\\end{pmatrix}");
    else
      s += wxT("\\end{bmatrix}");
  }
  else
    s += wxT("\\end{array}");
  return s;
}

wxString MatrCell::ToMathML()
{
  wxString retval;
  if (!m_specialMatrix)
    retval = wxT("<mrow><mo>(</mo><mrow>");
  retval += wxT("<mtable>");

  for (unsigned int i = 0; i < m_matHeight; i++)
  {
    retval += wxT("<mtr>");
    for (unsigned int j = 0; j < m_matWidth; j++)
      retval += wxT("<mtd>") + m_cells[i * m_matWidth + j]->ListToMathML() + wxT("</mtd>");
    retval += wxT("</mtr>");
  }
  retval += wxT("</mtable>\n");
  if (!m_specialMatrix)
    retval += wxT("</mrow><mo>)</mo></mrow>\n");
  return retval;
}

wxString MatrCell::ToOMML()
{
  wxString retval;

  retval = wxT("<m:d>");
  if (!m_specialMatrix)
  {
    if(m_roundedParens)
      retval += wxT("<m:dPr><m:begChr>(</m:begChr><m:endChr>)</m:endChr> <m:grow>\"1\"</m:grow></m:dPr>");
    else
      retval += wxT("<m:dPr><m:begChr>[</m:begChr><m:endChr>]</m:endChr> <m:grow>\"1\"</m:grow></m:dPr>");
  }
  
  retval += wxT("<m:e><m:m>");

  for (unsigned int i = 0; i < m_matHeight; i++)
  {
    retval += wxT("<m:mr>");
    for (unsigned int j = 0; j < m_matWidth; j++)
      retval += wxT("<m:e>") + m_cells[i * m_matWidth + j]->ListToOMML() + wxT("</m:e>");
    retval += wxT("</m:mr>");
  }

  retval += wxT("</m:m></m:e></m:d>");
  return retval;
}

wxString MatrCell::ToXML()
{
  wxString flags;
  if (m_forceBreakLine)
    flags += wxT(" breakline=\"true\"");
  if (m_roundedParens)
    flags += wxT(" roundedParens=\"true\"");

  wxString s = wxEmptyString;
  if (m_specialMatrix)
    s = wxString::Format(
      wxT("<tb") + flags + wxT(" special=\"true\" inference=\"%s\" rownames=\"%s\" colnames=\"%s\">"),
            m_inferenceMatrix ? wxT("true") : wxT("false"),
            m_rowNames ? wxT("true") : wxT("false"),
            m_colNames ? wxT("true") : wxT("false"));
  else
    s = wxT("<tb") +flags +wxT(">");

  for (unsigned int i = 0; i < m_matHeight; i++)
  {
    s += wxT("<mtr>");
    for (unsigned int j = 0; j < m_matWidth; j++)
      s += wxT("<mtd>") + m_cells[i * m_matWidth + j]->ListToXML() + wxT("</mtd>");
    s += wxT("</mtr>");
  }
  s += wxT("</tb>");

  return s;
}

void MatrCell::SetDimension()
{
  if (m_matHeight != 0)
    m_matWidth = m_matWidth / m_matHeight;
}
