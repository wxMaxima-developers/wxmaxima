///
///  Copyright (C) 2004-2011 Andrej Vodopivec <andrej.vodopivec@gmail.com>
///
///  This program is free software; you can redistribute it and/or modify
///  it under the terms of the GNU General Public License as published by
///  the Free Software Foundation; either version 2 of the License, or
///  (at your option) any later version.
///
///  This program is distributed in the hope that it will be useful,
///  but WITHOUT ANY WARRANTY; without even the implied warranty of
///  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
///  GNU General Public License for more details.
///
///
///  You should have received a copy of the GNU General Public License
///  along with this program; if not, write to the Free Software
///  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
///

#include "MatrCell.h"

MatrCell::MatrCell() : MathCell()
{
  m_matWidth = 0;
  m_matHeight = 0;
  m_specialMatrix = false;
  m_inferenceMatrix = false;
  m_rowNames = m_colNames = false;
}

MatrCell::~MatrCell()
{
  for (unsigned int i = 0; i < m_cells.size(); i++)
  {
    if (m_cells[i] != NULL)
      delete m_cells[i];
  }
  if (m_next != NULL)
    delete m_next;
}

void MatrCell::SetParent(MathCell *parent, bool all)
{
  for (unsigned int i = 0; i < m_cells.size(); i++)
  {
    if (m_cells[i] != NULL)
      m_cells[i]->SetParent(parent, true);
  }

  MathCell::SetParent(parent, all);
}

MathCell* MatrCell::Copy(bool all)
{
  MatrCell *tmp = new MatrCell;
  CopyData(this, tmp);
  tmp->m_specialMatrix = m_specialMatrix;
  tmp->m_inferenceMatrix = m_inferenceMatrix;
  tmp->m_rowNames = m_rowNames;
  tmp->m_colNames = m_colNames;
  tmp->m_matWidth = m_matWidth;
  tmp->m_matHeight = m_matHeight;
  for (int i = 0; i < m_matWidth*m_matHeight; i++)
    (tmp->m_cells).push_back(m_cells[i]->Copy(true));
  if (all && m_next != NULL)
    tmp->AppendCell(m_next->Copy(all));
  return tmp;
}

void MatrCell::Destroy()
{
  for (unsigned int i = 0; i < m_cells.size(); i++)
  {
    if (m_cells[i] != NULL)
      delete m_cells[i];
    m_cells[i] = NULL;
  }
  m_next = NULL;
}

void MatrCell::RecalculateWidths(CellParser& parser, int fontsize, bool all)
{
  double scale = parser.GetScale();
  for (int i = 0; i < m_matWidth*m_matHeight; i++)
  {
    m_cells[i]->RecalculateWidths(parser, MAX(MC_MIN_SIZE, fontsize - 2), true);
  }
  m_widths.clear();
  for (int i = 0; i < m_matWidth; i++)
  {
    m_widths.push_back(0);
    for (int j = 0; j < m_matHeight; j++)
    {
      m_widths[i] = MAX(m_widths[i], m_cells[m_matWidth * j + i]->GetFullWidth(scale));
    }
  }
  m_width = 0;
  for (int i = 0; i < m_matWidth; i++)
  {
    m_width += (m_widths[i] + SCALE_PX(10, scale));
  }
  if (m_width < SCALE_PX(14, scale))
    m_width = SCALE_PX(14, scale);
  MathCell::RecalculateWidths(parser, fontsize, all);
}

void MatrCell::RecalculateSize(CellParser& parser, int fontsize, bool all)
{
  double scale = parser.GetScale();

  for (int i = 0; i < m_matWidth*m_matHeight; i++)
  {
    m_cells[i]->RecalculateSize(parser, MAX(MC_MIN_SIZE, fontsize - 2), true);
  }
  m_centers.clear();
  m_drops.clear();
  for (int i = 0; i < m_matHeight; i++)
  {
    m_centers.push_back(0);
    m_drops.push_back(0);
    for (int j = 0; j < m_matWidth; j++)
    {
      m_centers[i] = MAX(m_centers[i], m_cells[m_matWidth * i + j]->GetMaxCenter());
      m_drops[i] = MAX(m_drops[i], m_cells[m_matWidth * i + j]->GetMaxDrop());
    }
  }
  m_height = 0;
  for (int i = 0; i < m_matHeight; i++)
  {
    m_height += (m_centers[i] + m_drops[i] + SCALE_PX(10, scale));
  }
  if (m_height == 0)
    m_height = fontsize + SCALE_PX(10, scale);
  m_center = m_height / 2;
  MathCell::RecalculateSize(parser, fontsize, all);
}

void MatrCell::Draw(CellParser& parser, wxPoint point, int fontsize, bool all)
{
  if (DrawThisCell(parser, point))
  {
    wxDC& dc = parser.GetDC();
    double scale = parser.GetScale();
    wxPoint mp;
    mp.x = point.x + SCALE_PX(5, scale);
    mp.y = point.y - m_center;
    for (int i = 0; i < m_matWidth; i++)
    {
      mp.y = point.y - m_center + SCALE_PX(5, scale);
      for (int j = 0; j < m_matHeight; j++)
      {
        mp.y += m_centers[j];
        wxPoint mp1(mp);
        mp1.x = mp.x + (m_widths[i] - m_cells[j * m_matWidth + i]->GetFullWidth(scale)) / 2;
        m_cells[j*m_matWidth + i]->Draw(parser, mp1, MAX(MC_MIN_SIZE, fontsize - 2), true);
        mp.y += (m_drops[j] + SCALE_PX(10, scale));
      }
      mp.x += (m_widths[i] + SCALE_PX(10, scale));
    }
    SetPen(parser);
    if (m_specialMatrix)
    {
      if (m_inferenceMatrix)
        dc.DrawLine(point.x + SCALE_PX(1, scale),
                    point.y - m_center + SCALE_PX(2, scale),
                    point.x + SCALE_PX(1, scale),
                    point.y + m_center - SCALE_PX(2, scale));
      else {
        if (m_rowNames)
          dc.DrawLine(point.x + m_widths[0] + 2*SCALE_PX(5, scale),
                      point.y - m_center + SCALE_PX(2, scale),
                      point.x + m_widths[0] + 2*SCALE_PX(5, scale),
                      point.y + m_center - SCALE_PX(2, scale));
        if (m_colNames)
          dc.DrawLine(point.x + SCALE_PX(1, scale),
                      point.y - m_center + m_centers[0] + m_drops[0] + 2*SCALE_PX(5, scale),
                      point.x + SCALE_PX(1, scale) + m_width,
                      point.y - m_center + m_centers[0] + m_drops[0] + 2*SCALE_PX(5, scale));
      }
    }
    else
    {
      // left bracket
      dc.DrawLine(point.x + SCALE_PX(5, scale),
                  point.y - m_center + SCALE_PX(2, scale),
                  point.x + SCALE_PX(1, scale),
                  point.y - m_center + SCALE_PX(2, scale));
      dc.DrawLine(point.x + SCALE_PX(1, scale),
                  point.y - m_center + SCALE_PX(2, scale),
                  point.x + SCALE_PX(1, scale),
                  point.y + m_center - SCALE_PX(2, scale));
      dc.DrawLine(point.x + SCALE_PX(1, scale),
                  point.y + m_center - SCALE_PX(2, scale),
                  point.x + SCALE_PX(5, scale),
                  point.y + m_center - SCALE_PX(2, scale));
      // right bracket
      dc.DrawLine(point.x + m_width - SCALE_PX(5, scale) - 1,
                  point.y - m_center + SCALE_PX(2, scale),
                  point.x + m_width - SCALE_PX(1, scale) - 1,
                  point.y - m_center + SCALE_PX(2, scale));
      dc.DrawLine(point.x + m_width - SCALE_PX(1, scale) - 1,
                  point.y - m_center + SCALE_PX(2, scale),
                  point.x + m_width - SCALE_PX(1, scale) - 1,
                  point.y + m_center - SCALE_PX(2, scale));
      dc.DrawLine(point.x + m_width - SCALE_PX(1, scale) - 1,
                  point.y + m_center - SCALE_PX(2, scale),
                  point.x + m_width - SCALE_PX(5, scale) - 1,
                  point.y + m_center - SCALE_PX(2, scale));
    }
    UnsetPen(parser);
  }
  MathCell::Draw(parser, point, fontsize, all);
}

wxString MatrCell::ToString(bool all)
{
  wxString s = wxT("matrix(");
  for (int i = 0; i < m_matHeight; i++)
  {
    s += wxT("[");
    for (int j = 0; j < m_matWidth; j++)
    {
      s += m_cells[i * m_matWidth + j]->ToString(true);
      if (j < m_matWidth - 1)
        s += wxT(",");
    }
    s += wxT("]");
    if (i < m_matHeight - 1)
      s += wxT(",");
  }
  s += wxT(")");
  s += MathCell::ToString(all);
  return s;
}

wxString MatrCell::ToTeX(bool all)
{
  wxString s = wxT("\\begin{pmatrix}");
  for (int i = 0; i < m_matHeight; i++)
  {
    for (int j = 0; j < m_matWidth; j++)
    {
      s += m_cells[i * m_matWidth + j]->ToTeX(true);
      if (j < m_matWidth - 1)
        s += wxT(" & ");
    }
    if (i < m_matHeight - 1)
      s += wxT("\\cr ");
  }
  s += wxT("\\end{pmatrix}");
  s += MathCell::ToTeX(all);
  return s;
}

wxString MatrCell::ToXML(bool all)
{
  wxString s = wxEmptyString;
  if (m_specialMatrix)
    s = wxString::Format(
      wxT("<tb special=\"true\" inference=\"%s\" rownames=\"%s\" colnames=\"%s\">"),
      m_inferenceMatrix ? wxT("true") : wxT("false"),
      m_rowNames ? wxT("true") : wxT("false"),
      m_colNames ? wxT("true") : wxT("false"));
  else
    s = wxT("<tb>");
  
for (int i = 0; i < m_matHeight; i++)
  {
    s += wxT("<mtr>");
    for (int j = 0; j < m_matWidth; j++)
      s += wxT("<mtd>") + m_cells[i * m_matWidth + j]->ToXML(true) + wxT("</mtd>");
    s += wxT("</mtr>");
  }
  s += wxT("</tb>");

  return s + MathCell::ToXML(all);
}

void MatrCell::SetDimension()
{
  if (m_matHeight != 0)
    m_matWidth = m_matWidth / m_matHeight;
}

void MatrCell::SelectInner(wxRect& rect, MathCell** first, MathCell** last)
{
  *first = NULL;
  *last = NULL;
  for (int i = 0; i < m_matHeight; i++)
  {
    for (int j = 0; j < m_matWidth; j++)
    {
      if (m_cells[i*m_matWidth + j]->ContainsRect(rect))
        m_cells[i*m_matWidth + j]->SelectRect(rect, first, last);
    }
  }
  if (*first == NULL || *last == NULL)
  {
    *first = this;
    *last = this;
  }
}
