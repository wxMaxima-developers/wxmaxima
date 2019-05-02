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
  This file defines the class Cell

  Cell is the base class for all cell- or list-type elements.
*/

#include "Cell.h"
#include <wx/regex.h>
#include <wx/sstream.h>

wxString Cell::GetToolTip(const wxPoint &point)
{
  if(!ContainsPoint(point))
    return wxEmptyString;

  wxString toolTip;
  std::list<Cell *> innerCells = GetInnerCells();
  for(std::list<Cell *>::iterator it = innerCells.begin(); it != innerCells.end(); ++it)
  {
    if(*it != NULL)
    {
      if((toolTip = (*it)->GetToolTip(point)) != wxEmptyString)
        return toolTip;
    }
  }
  return m_toolTip;
}

Cell::Cell(Cell *group, Configuration **config)
#if wxUSE_ACCESSIBILITY
  :wxAccessible()
#endif
{
  m_group = group;
  m_textStyle = TS_DEFAULT;
  m_toolTip = wxEmptyString;
  m_toolTip = wxEmptyString;
  m_cellPointers = NULL;
  m_group = group;
  m_parent = group;
  m_configuration = config;
  m_next = NULL;
  m_previous = NULL;
  m_nextToDraw = NULL;
  m_previousToDraw = NULL;
  m_fullWidth = -1;
  m_lineWidth = -1;
  m_maxCenter = -1;
  m_maxDrop = -1;
  m_width = -1;
  m_height = -1;
  m_center = -1;
  SoftLineBreak(false);
  m_breakPage = false;
  m_forceBreakLine = false;
  m_bigSkip = true;
  m_isHidden = false;
  m_isBrokenIntoLines = false;
  m_highlight = false;
  m_type = MC_TYPE_DEFAULT;
  m_textStyle = TS_VARIABLE;
  m_SuppressMultiplicationDot = false;
  m_imageBorderWidth = 0;
  SetCurrentPoint(wxPoint(-1, -1));
  m_toolTip = (*m_configuration)->GetDefaultCellToolTip();
  m_fontSize = (*m_configuration)->GetMathFontSize();
}

Cell::~Cell()
{
  // Find the last cell in this list of cells
  Cell *last = this;
  while (last->m_next != NULL)
    last = last->m_next;

  // Delete all cells beginning with the last one
  while ((last != NULL) && (last != this))
  {
    Cell *tmp = last;
    last = last->m_previous;
    wxDELETE(tmp);
    last->m_next = NULL;
  }
}

void Cell::SetType(CellType type)
{
  m_type = type;

  switch (m_type)
  {
    case MC_TYPE_MAIN_PROMPT:
      m_textStyle = TS_MAIN_PROMPT;
      break;
    case MC_TYPE_PROMPT:
      m_textStyle = TS_OTHER_PROMPT;
      break;
    case MC_TYPE_LABEL:
      m_textStyle = TS_LABEL;
      HardLineBreak();
      break;
    case MC_TYPE_INPUT:
      m_textStyle = TS_INPUT;
      break;
    case MC_TYPE_ERROR:
      m_textStyle = TS_ERROR;
      break;
    case MC_TYPE_WARNING:
      m_textStyle = TS_WARNING;
      break;
    case MC_TYPE_TEXT:
      m_textStyle = TS_TEXT;
      break;
    case MC_TYPE_SUBSUBSECTION:
      m_textStyle = TS_SUBSUBSECTION;
      break;
    case MC_TYPE_HEADING5:
      m_textStyle = TS_HEADING5;
      break;
    case MC_TYPE_HEADING6:
      m_textStyle = TS_HEADING6;
      break;
    case MC_TYPE_SUBSECTION:
      m_textStyle = TS_SUBSECTION;
      break;
    case MC_TYPE_SECTION:
      m_textStyle = TS_SECTION;
      break;
    case MC_TYPE_TITLE:
      m_textStyle = TS_TITLE;
      break;
    default:
      m_textStyle = TS_DEFAULT;
      break;
  }
  ResetSize();
  if(m_group != NULL)
    GetGroup()->ResetSize();
}

Cell *Cell::CopyList()
{
  Cell *dest = Copy();
  Cell *ret = dest;
  Cell *src = m_next;

  while (src != NULL)
  {
    dest->AppendCell(src->Copy());
    src = src->m_next;
    dest = dest->m_next;
  }

  return ret;
}

void Cell::ClearCacheList()
{
  Cell *tmp = this;

  while (tmp != NULL)
  {
    tmp->ClearCache();
    tmp = tmp->m_next;
  }
}

void Cell::SetGroupList(Cell *parent)
{
  Cell *tmp = this;
  while (tmp != NULL)
  {
    tmp->SetGroup(parent);
    tmp->SetParent(this);
    tmp = tmp->m_next;
  }
}

int Cell::CellsInListRecursive()
{
  
  //! The number of cells the current group contains (-1, if no GroupCell)
  int cells = 0;

  Cell *tmp = this;

  while(tmp != NULL)
  {
    cells ++;
    std::list<Cell*> cellList = tmp->GetInnerCells();
    for (std::list<Cell *>::iterator it = cellList.begin(); it != cellList.end(); ++it)
    {
      if(*it != NULL)
        cells += (*it)->CellsInListRecursive();
    }
    tmp = tmp->m_next;
  }
  return cells;
}

void Cell::SetGroup(Cell *group)
{
  m_group = group;
  if(group != NULL)
  {
    wxASSERT (group->GetType() == MC_TYPE_GROUP);
  }
  
  std::list<Cell*> cellList = GetInnerCells();
  for (std::list<Cell *>::iterator it = cellList.begin(); it != cellList.end(); ++it)
  {
    if(*it != NULL)
      (*it)->SetGroupList(group);
  }
}


/***
 * Append new cell to the end of this list.
 */
void Cell::AppendCell(Cell *p_next)
{
  if (p_next == NULL)
    return;
  m_maxDrop = -1;
  m_maxCenter = -1;

  // Search the last cell in the list
  Cell *LastInList = this;
  while (LastInList->m_next != NULL)
    LastInList = LastInList->m_next;

  // Append this p_next to the list
  LastInList->m_next = p_next;
  LastInList->m_next->m_previous = LastInList;

  // Search the last cell in the list that is sorted by the drawing order
  Cell *LastToDraw = LastInList;
  while (LastToDraw->m_nextToDraw != NULL)
    LastToDraw = LastToDraw->m_nextToDraw;

  // Append p_next to this list.
  LastToDraw->m_nextToDraw = p_next;
  p_next->m_previousToDraw = LastToDraw;
}

Cell *Cell::GetGroup()
{
  wxASSERT_MSG(m_group != NULL, _("Bug: Math Cell that claims to have no group Cell it belongs to"));
  return m_group;
}

/***
 * Get the maximum drop of the center.
 */
int Cell::GetMaxCenter()
{
  if ((m_maxCenter < 0) || ((*m_configuration)->RecalculationForce()))
  {
    Cell *tmp = this;
    m_maxCenter  = 0;
    while (tmp != NULL)
    {
      if ((tmp != this) && (tmp->m_breakLine))
        break;
      if(!tmp->m_isBrokenIntoLines)
        m_maxCenter = MAX(m_maxCenter, tmp->m_center);
      tmp = tmp->m_nextToDraw;
    }
  }
  return m_maxCenter;
}

/***
 * Get the maximum drop of cell.
 */
int Cell::GetMaxDrop()
{
  if ((m_maxDrop < 0) || ((*m_configuration)->RecalculationForce()))
  {
    m_maxDrop = 0;
    Cell *tmp = this;
    while (tmp != NULL)
    {
      if ((tmp != this) && (tmp->m_breakLine))
        break;
      if(!tmp->m_isBrokenIntoLines)
        m_maxDrop = MAX(m_maxDrop, tmp->m_height - tmp->m_center);
      tmp = tmp->m_nextToDraw;
    }
  }
  return m_maxDrop;
}

//!  Get the maximum hight of cells in line.
int Cell::GetMaxHeight()
{
  return GetMaxCenter() + GetMaxDrop();
}

/*! Get full width of this group.
 */
int Cell::GetFullWidth()
{
  // Recalculate the with of this list of cells only if this has been marked as necessary.
  if ((m_fullWidth < 0) || ((*m_configuration)->RecalculationForce()))
  {
    Cell *tmp = this;

    // We begin this calculation with a negative offset since the full width of only a single
    // cell doesn't contain the space that separates two cells - that is automatically added
    // to every cell in the next step.
    m_fullWidth = 0;
    while (tmp != NULL)
    {
      m_fullWidth += tmp->m_width;
      tmp = tmp->m_next;
    }
  }
  return m_fullWidth;
}

/*! Get the width of this line.
 */
int Cell::GetLineWidth()
{
  if (m_lineWidth < 0)
  {
    m_lineWidth = 0;
    int width = m_width;

    Cell *tmp = this;
    while(tmp != NULL)
    {
      width += tmp->m_width;

      if (width > m_lineWidth)
        m_lineWidth = width;

      tmp = tmp->m_nextToDraw;
      if(tmp != NULL)
      {
        if(tmp->m_isBrokenIntoLines || tmp->m_breakLine || (tmp->m_type == MC_TYPE_MAIN_PROMPT))
          break;
      }
    }
  }
  return m_lineWidth;
}

/*! Draw this cell to dc

 To make this work each derived class must draw the content of the cell
 and then call MathCall::Draw(...).
 */
void Cell::Draw(wxPoint point)
{
  if((m_height > 0) && (point.y > 0))
    SetCurrentPoint(point);

  // Tell the screen reader that this cell's contents might have changed.

#if wxUSE_ACCESSIBILITY
  if((*m_configuration)->GetWorkSheet() != NULL)
    NotifyEvent(0, (*m_configuration)->GetWorkSheet(), wxOBJID_CLIENT, wxOBJID_CLIENT);
#endif
}

void Cell::DrawList(wxPoint point)
{
  Cell *tmp = this;
  while (tmp != NULL)
  {
    tmp->Draw(point);
    point.x += tmp->m_width;
    wxASSERT(tmp != tmp->m_nextToDraw);
    tmp = tmp->m_nextToDraw;
  }
}

void Cell::RecalculateList(int fontsize)
{
  Cell *tmp = this;

  while (tmp != NULL)
  {
    tmp->RecalculateWidths(fontsize);
    tmp->RecalculateHeight(fontsize);
    tmp = tmp->m_nextToDraw;
  }
}

void Cell::ResetSizeList()
{
  Cell *tmp = this;

  while (tmp != NULL)
  {
    tmp->ResetSize();
    tmp = tmp->m_next;
  }
}


void Cell::RecalculateHeightList(int fontsize)
{
  Cell *tmp = this;

  while (tmp != NULL)
  {
    tmp->RecalculateHeight(fontsize);
    tmp = tmp->m_next;
  }
}

/*! Recalculate widths of cells.

  (Used for changing font size since in this case all size information has to be
  recalculated).

  Should set: set m_width.
*/
void Cell::RecalculateWidthsList(const int &fontsize)
{
  Cell *tmp = this;

  while (tmp != NULL)
  {
    tmp->RecalculateWidths(fontsize);
    tmp = tmp->m_next;
  }
}

void Cell::RecalculateWidths(int fontsize)
{
  ResetData();
  m_fontSize = fontsize;
}

/*! Is this cell currently visible in the window?.
 */
bool Cell::DrawThisCell(wxPoint point)
{
  // If the cell isn't on the worksheet we don't draw it.
  if((point.x < 0) || (point.y < 0))
    return false;

  SetCurrentPoint(point);

  // If a cell is broken into lines the cells it contains are displayed but
  // not the cell itself (example: Denominator and Numerator are displayed
  // but not the horizontal line with denominator above and numerator below.
  if(m_isBrokenIntoLines)
    return false;
  
  if(!(*m_configuration)->ClipToDrawRegion())
    return true;
  
  return(InUpdateRegion());
}

/*! Get the rectangle around this cell

  \param all
   - true  return the rectangle around the whole line.
   - false return the rectangle around this cell.
 */
wxRect Cell::GetRect(bool all)
{
  if (all)
    return wxRect(m_currentPoint.x, m_currentPoint.y - GetMaxCenter(),
                  GetLineWidth(), GetMaxHeight());
  else
    return wxRect(m_currentPoint.x, m_currentPoint.y - m_center,
                  m_width, m_height);
}

bool Cell::InUpdateRegion(const wxRect &rect)
{
  if (!(*m_configuration)->ClipToDrawRegion())
    return true;

  if((m_currentPoint.x < 0) || (m_currentPoint.y < 0))
    return false;
  
  wxRect updateRegion = (*m_configuration)->GetUpdateRegion();

  // If we have deferred the recalculation of the cell height but now
  // got a draw request due to moving the mouse wheel we need to guess
  // the cell size 
  if(m_height < 0)
  {
    int height = 0;
    if(m_next)
      height = m_next->m_currentPoint.y - m_currentPoint.y;

    if ((updateRegion.GetBottom() >= m_currentPoint.y) &&
        (updateRegion.GetTop() <= m_currentPoint.y+height))
    return true;
  }

  if(updateRegion.Contains(m_currentPoint))
    return true;

  return updateRegion.Intersects(rect) ||
    updateRegion.Contains(rect) ||
    (updateRegion == rect) || rect.Contains(updateRegion);
}

void Cell::DrawBoundingBox(wxDC &dc, bool all)
{
  wxRect rect = GetRect(all);
  if (InUpdateRegion())
  {
    dc.DrawRectangle(CropToUpdateRegion(rect));
  }
}

/***
 * Do we have an operator in this line - draw () in frac...
 */
bool Cell::IsCompound()
{
  if (IsOperator())
    return true;
  if (m_next == NULL)
    return false;
  return m_next->IsCompound();
}

/***
 * Is operator - draw () in frac...
 */
bool Cell::IsOperator()
{
  return false;
}

/***
 * Return the string representation of cell.
 */
wxString Cell::ToString()
{
  return wxEmptyString;
}

wxString Cell::ListToString()
{
  wxString retval;
  Cell *tmp = this;
  bool firstline = true;

  while (tmp != NULL)
  {
    if ((!firstline) && (tmp->m_forceBreakLine))
    {
      if(!retval.EndsWith(wxT('\n')))
        retval += wxT("\n");
      // if(
      //    (tmp->GetStyle() != TS_LABEL) &&
      //    (tmp->GetStyle() != TS_USERLABEL) &&
      //    (tmp->GetStyle() != TS_MAIN_PROMPT) &&
      //    (tmp->GetStyle() != TS_OTHER_PROMPT))
      //   retval += wxT("\t");
    }
    // if(firstline)
    // {
    //   if((tmp->GetStyle() != TS_LABEL) &&
    //      (tmp->GetStyle() != TS_USERLABEL) &&
    //      (tmp->GetStyle() != TS_MAIN_PROMPT) &&
    //      (tmp->GetStyle() != TS_OTHER_PROMPT))
    //     retval += wxT("\t");
    // }
    retval += tmp->ToString();

    firstline = false;
    tmp = tmp->m_nextToDraw;
  }

  return retval;
}

wxString Cell::ToMatlab()
{
  return wxEmptyString;
}

wxString Cell::ListToMatlab()
{
	wxString retval;
	Cell *tmp = this;
	bool firstline = true;

	while (tmp != NULL)
	{
	  if ((!firstline) && (tmp->m_forceBreakLine))
	  {
		if(!retval.EndsWith(wxT('\n')))
		  retval += wxT("\n");
		// if(
		//    (tmp->GetStyle() != TS_LABEL) &&
		//    (tmp->GetStyle() != TS_USERLABEL) &&
		//    (tmp->GetStyle() != TS_MAIN_PROMPT) &&
		//    (tmp->GetStyle() != TS_OTHER_PROMPT))
		//   retval += wxT("\t");
	  }
	  // if(firstline)
	  // {
	  //   if((tmp->GetStyle() != TS_LABEL) &&
	  //      (tmp->GetStyle() != TS_USERLABEL) &&
	  //      (tmp->GetStyle() != TS_MAIN_PROMPT) &&
	  //      (tmp->GetStyle() != TS_OTHER_PROMPT))
	  //     retval += wxT("\t");
	  // }
	  retval += tmp->ToMatlab();

	  firstline = false;
	  tmp = tmp->m_nextToDraw;
	}

	return retval;
}

wxString Cell::ToTeX()
{
  return wxEmptyString;
}

wxString Cell::ListToTeX()
{
  wxString retval;
  Cell *tmp = this;

  while (tmp != NULL)
  {
    if ((tmp->m_textStyle == TS_LABEL && retval != wxEmptyString) ||
        (tmp->m_breakLine && retval != wxEmptyString))
      retval += wxT("\\]\\[");
    retval += tmp->ToTeX();
    tmp = tmp->m_next;
  }

  // TODO: Things like {a}_{b} make the LaTeX code harder to read. But things like
  // \sqrt{a} need us to use braces from time to time.
  //
  // How far I got was:
  //
  //  wxRegEx removeUnneededBraces1(wxT("{([a-zA-Z0-9])}([{}_a-zA-Z0-9 \\\\^_])"));
  //  removeUnneededBraces1.Replace(&retval,wxT(" \\1\\2"),true);
  return retval;
}

wxString Cell::ToXML()
{
  return wxEmptyString;
}

wxString Cell::ToMathML()
{
  return wxEmptyString;
}

wxString Cell::ListToMathML(bool startofline)
{
  bool highlight = false;

  wxString retval;

  // If the region to export contains linebreaks or labels we put it into a table.
  bool needsTable = false;
  Cell *temp = this;
  while (temp)
  {
    if (temp->HardLineBreak())
      needsTable = true;

    if (temp->GetType() == MC_TYPE_LABEL)
      needsTable = true;

    temp = temp->m_next;
  }

  temp = this;
  // If the list contains multiple cells we wrap them in a <mrow> in order to
  // group them into a single object.
  bool multiCell = (temp->m_next != NULL);

  // Export all cells
  while (temp != NULL)
  {
    // Do we need to end a highlighting region?
    if ((!temp->m_highlight) && (highlight))
      retval += wxT("</mrow>");

    // Handle linebreaks
    if ((temp != this) && (temp->HardLineBreak()))
      retval += wxT("</mtd></mlabeledtr>\n<mlabeledtr columnalign=\"left\"><mtd>");

    // If a linebreak isn't followed by a label we need to introduce an empty one.
    if ((((temp->HardLineBreak()) || (startofline && (this == temp))) &&
         ((temp->GetStyle() != TS_LABEL) && (temp->GetStyle() != TS_USERLABEL))) && (needsTable))
      retval += wxT("<mtext></mtext></mtd><mtd>");

    // Do we need to start a highlighting region?
    if ((temp->m_highlight) && (!highlight))
      retval += wxT("<mrow mathcolor=\"red\">");
    highlight = temp->m_highlight;


    retval += temp->ToMathML();
    temp = temp->m_next;
  }

  // If the region we converted to MathML ended within a highlighted region
  // we need to close this region now.
  if (highlight)
    retval += wxT("</mrow>");

  // If we grouped multiple cells as a single object we need to cose this group now
  if ((multiCell) && (!needsTable))
    retval = wxT("<mrow>") + retval + wxT("</mrow>\n");

  // If we put the region we exported into a table we need to end this table now
  if (needsTable)
    retval = wxT("<mtable>\n<mlabeledtr columnalign=\"left\"><mtd>") + retval + wxT("</mtd></mlabeledtr>\n</mtable>");
  return retval;
}

wxString Cell::OMML2RTF(wxXmlNode *node)
{
  wxString result;

  while (node != NULL)
  {
    if (node->GetType() == wxXML_ELEMENT_NODE)
    {
      wxString ommlname = node->GetName();
      result += wxT("{\\m") + ommlname.Right(ommlname.Length() - 2);

      // Convert the attributes
      wxXmlAttribute *attributes = node->GetAttributes();
      while (attributes != NULL)
      {
        wxString ommlatt = attributes->GetName();
        result += wxT("{\\m") + ommlatt.Right(ommlatt.Length() - 2) +
                  wxT(" ") + attributes->GetValue() + wxT("}");
        attributes = attributes->GetNext();
      }

      // Convert all child nodes
      if (node->GetChildren() != NULL)
      {
        result += OMML2RTF(node->GetChildren());
      }
      result += wxT("}");
    }
    else
      result += wxT(" ") + RTFescape(node->GetContent());

    node = node->GetNext();
  }
  return result;
}

wxString Cell::OMML2RTF(wxString ommltext)
{
  if (ommltext == wxEmptyString)
    return wxEmptyString;

  wxString result;
  wxXmlDocument ommldoc;
  ommltext = wxT("<m:r>") + ommltext + wxT("</m:r>");

  wxStringInputStream ommlStream(ommltext);

  ommldoc.Load(ommlStream, wxT("UTF-8"));

  wxXmlNode *node = ommldoc.GetRoot();
  result += OMML2RTF(node);

  if ((result != wxEmptyString) && (result != wxT("\\mr")))
  {
    result = wxT("{\\mmath {\\*\\moMath") + result + wxT("}}");
  }
  return result;
}

wxString Cell::XMLescape(wxString input)
{
  input.Replace(wxT("&"), wxT("&amp;"));
  input.Replace(wxT("<"), wxT("&lt;"));
  input.Replace(wxT(">"), wxT("&gt;"));
  input.Replace(wxT("'"), wxT("&apos;"));
  input.Replace(wxT("\""), wxT("&quot;"));
  return input;
}

wxString Cell::RTFescape(wxString input, bool MarkDown)
{
  // Characters with a special meaning in RTF
  input.Replace("\\", "\\\\");
  input.Replace("{", "\\{");
  input.Replace("}", "\\}");
  input.Replace(wxT("\r"), "\n");

  // The Character we will use as a soft line break
  input.Replace("\r", wxEmptyString);

  // Encode unicode characters in a rather mind-boggling way
  wxString output;
  for (size_t i = 0; i < input.Length(); i++)
  {
    wxChar ch = input[i];
    if (ch == wxT('\n'))
    {
      if (((i > 0) && (input[i - 1] == wxT('\n'))) || !MarkDown)
        output += wxT("\\par}\n{\\pard ");
      else
        output += wxT("\n");
    }
    else
    {
      if ((ch < 128) && (ch > 0))
      {
        output += ch;
      }
      else
      {
        if (ch < 32768)
        {
          output += wxString::Format("\\u%i?", int(ch));
        }
        else
        {
          output += wxString::Format("\\u%i?", int(ch) - 65536);
        }
      }
    }
  }
  return (output);
}

wxString Cell::ListToOMML(bool WXUNUSED(startofline))
{
  bool multiCell = (m_next != NULL);

  wxString retval;

  // If the region to export contains linebreaks or labels we put it into a table.
  // Export all cells

  Cell *tmp = this;
  while (tmp != NULL)
  {
    wxString token = tmp->ToOMML();

    // End exporting the equation if we reached the end of the equation.
    if (token == wxEmptyString)
      break;

    retval += token;

    // Hard linebreaks aren't supported by OMML and therefore need a new equation object
    if (tmp->HardLineBreak())
      break;

    tmp = tmp->m_next;
  }

  if ((multiCell) && (retval != wxEmptyString))
    return wxT("<m:r>") + retval + wxT("</m:r>");
  else
    return retval;
}

wxString Cell::ListToRTF(bool startofline)
{
  wxString retval;
  Cell *tmp = this;

  while (tmp != NULL)
  {
    wxString rtf = tmp->ToRTF();
    if (rtf != wxEmptyString)
    {
      if ((GetStyle() == TS_LABEL) || ((GetStyle() == TS_USERLABEL)))
      {
        retval += wxT("\\par}\n{\\pard\\s22\\li1105\\lin1105\\fi-1105\\f0\\fs24 ") + rtf + wxT("\\tab");
        startofline = false;
      }
      else
      {
        if (startofline)
          retval += wxT("\\par}\n{\\pard\\s21\\li1105\\lin1105\\f0\\fs24 ") + rtf + wxT("\\n");
        startofline = true;
      }
      tmp = tmp->m_next;
    }
    else
    {
      if (tmp->ListToOMML() != wxEmptyString)
      {
        // Math!

        // set the style for this line.
        if (startofline)
          retval += wxT("\\pard\\s21\\li1105\\lin1105\\f0\\fs24 ");

        retval += OMML2RTF(tmp->ListToOMML());

        startofline = true;

        // Skip the rest of this equation
        while (tmp != NULL)
        {
          // A non-equation item starts a new rtf item
          if (tmp->ToOMML() == wxEmptyString)
            break;

          // A newline starts a new equation
          if (tmp->HardLineBreak())
          {
            tmp = tmp->m_next;
            break;
          }

          tmp = tmp->m_next;
        }
      }
      else
      {
        tmp = tmp->m_next;
      }
    }
  }
  return retval;
}

wxString Cell::ListToXML()
{
  bool highlight = false;

  wxString retval;
  Cell *tmp = this;

  while (tmp != NULL)
  {
    if ((tmp->GetHighlight()) && (!highlight))
    {
      retval += wxT("<hl>\n");
      highlight = true;
    }

    if ((!tmp->GetHighlight()) && (highlight))
    {
      retval += wxT("</hl>\n");
      highlight = false;
    }

    retval += tmp->ToXML();
    tmp = tmp->m_next;
  }

  if (highlight)
  {
    retval += wxT("</hl>\n");
  }

  return retval;
}

/***
 * Get the part for diff tag support - only ExpTag overvrides this.
 */
wxString Cell::GetDiffPart()
{
  return wxEmptyString;
}

/***
 * Find the first and last cell in rectangle rect in this line.
 */
void Cell::SelectRect(wxRect &rect, Cell **first, Cell **last)
{
  SelectFirst(rect, first);
  if (*first != NULL)
  {
    *last = *first;
    (*first)->SelectLast(rect, last);
    if (*last == *first)
      (*first)->SelectInner(rect, first, last);
  }
  else
    *last = NULL;
}

/***
 * Find the first cell in rectangle rect in this line.
 */
void Cell::SelectFirst(wxRect &rect, Cell **first)
{
  if (rect.Intersects(GetRect(false)))
    *first = this;
  else if (m_nextToDraw != NULL)
    m_nextToDraw->SelectFirst(rect, first);
  else
    *first = NULL;
}

/***
 * Find the last cell in rectangle rect in this line.
 */
void Cell::SelectLast(wxRect &rect, Cell **last)
{
  if (rect.Intersects(GetRect(false)))
    *last = this;
  if (m_nextToDraw != NULL)
    m_nextToDraw->SelectLast(rect, last);
}

/***
 * Select rectangle in deeper cell - derived classes should override this
 */
void Cell::SelectInner(wxRect &rect, Cell **first, Cell **last)
{
  *first = NULL;
  *last = NULL;

  std::list<Cell*> cellList = GetInnerCells();
  for (std::list<Cell *>::iterator it = cellList.begin(); it != cellList.end(); ++it)
    if(*it != NULL)
    {
      if ((*it)->ContainsRect(rect))
        (*it)->SelectRect(rect, first, last);
    }

  if (*first == NULL || *last == NULL)
  {
    *first = this;
    *last = this;
  }
}

bool Cell::BreakLineHere()
{
  return (((!m_isBrokenIntoLines) && m_breakLine) || m_forceBreakLine);
}

bool Cell::ContainsRect(const wxRect &sm, bool all)
{
  wxRect big = GetRect(all);
  if (big.x <= sm.x &&
      big.y <= sm.y &&
      big.x + big.width >= sm.x + sm.width &&
      big.y + big.height >= sm.y + sm.height)
    return true;
  return false;
}

/*!
 Resets remembered data.

 Resets cached data like width and the height of the current cell
 as well as the vertical position of the center. Temporarily unbreaks all
 lines until the widths are recalculated if there aren't any hard line
 breaks.
 */
void Cell::ResetData()
{
  m_fullWidth = -1;
  m_lineWidth = -1;
  m_maxCenter = -1;
  m_maxDrop   = -1;
}

Cell *Cell::first()
{
  Cell *tmp = this;
  while (tmp->m_previous)
    tmp = tmp->m_previous;

  return tmp;
}

Cell *Cell::last()
{
  Cell *tmp = this;
  while (tmp->m_next)
    tmp = tmp->m_next;

  return tmp;
}

void Cell::Unbreak()
{
  ResetData();
  m_isBrokenIntoLines = false;
  m_nextToDraw = m_next;
  if (m_nextToDraw != NULL)
    m_nextToDraw->m_previousToDraw = this;
}

void Cell::UnbreakList()
{
  Cell *tmp = this;
  while (tmp != NULL)
  {
    tmp->Unbreak();
    tmp = tmp->m_next;
  }
}

/*!
  Set the pen in device context according to the style of the cell.
*/
void Cell::SetPen(double lineWidth)
{
  Configuration *configuration = (*m_configuration);
  wxDC *dc = configuration->GetDC();

  wxPen pen;

  if (m_highlight)
    pen = *(wxThePenList->FindOrCreatePen(configuration->GetColor(TS_HIGHLIGHT),
                                          lineWidth * configuration->GetDefaultLineWidth(), wxPENSTYLE_SOLID));
  else if (m_type == MC_TYPE_PROMPT)
    pen = *(wxThePenList->FindOrCreatePen(configuration->GetColor(TS_OTHER_PROMPT),
                                              lineWidth * configuration->GetDefaultLineWidth(), wxPENSTYLE_SOLID));
  else if (m_type == MC_TYPE_INPUT)
    pen = *(wxThePenList->FindOrCreatePen(configuration->GetColor(TS_INPUT),
                                          lineWidth * configuration->GetDefaultLineWidth(), wxPENSTYLE_SOLID));
  else
    pen = *(wxThePenList->FindOrCreatePen(configuration->GetColor(TS_DEFAULT),
                                          lineWidth * configuration->GetDefaultLineWidth(), wxPENSTYLE_SOLID));

  dc->SetPen(pen);
  if(configuration->GetAntialiassingDC() != dc)
    configuration->GetAntialiassingDC()->SetPen(pen);
}

/***
 * Reset the pen in the device context.
 */
void Cell::UnsetPen()
{
  Configuration *configuration = (*m_configuration);
  wxDC *dc = configuration->GetDC();
  if (m_type == MC_TYPE_PROMPT || m_type == MC_TYPE_INPUT || m_highlight)
    dc->SetPen(*(wxThePenList->FindOrCreatePen(configuration->GetColor(TS_DEFAULT),
                                              1, wxPENSTYLE_SOLID)));
}

/***
 * Copy all important data from s to t
 */
void Cell::CopyData(Cell *s, Cell *t)
{
  t->m_altCopyText = s->m_altCopyText;
  t->m_toolTip = s->m_toolTip;
  t->m_forceBreakLine = s->m_forceBreakLine;
  t->m_type = s->m_type;
  t->m_textStyle = s->m_textStyle;
}

void Cell::SetForeground()
{
  Configuration *configuration = (*m_configuration);
  wxColour color;
  wxDC *dc = configuration->GetDC();
  if (m_highlight)
  {
    color = configuration->GetColor(TS_HIGHLIGHT);
  }
  else
  {
    switch (m_type)
    {
      case MC_TYPE_PROMPT:
        color = configuration->GetColor(TS_OTHER_PROMPT);
        break;
      case MC_TYPE_MAIN_PROMPT:
        color = configuration->GetColor(TS_MAIN_PROMPT);
        break;
      case MC_TYPE_ERROR:
        color = wxColour(wxT("red"));
        break;
      case MC_TYPE_WARNING:
        color = configuration->GetColor(TS_WARNING);
        break;
      case MC_TYPE_LABEL:
        color = configuration->GetColor(TS_LABEL);
        break;
      default:
        color = configuration->GetColor(m_textStyle);
        break;
    }
  }

  dc->SetTextForeground(color);
}

bool Cell::IsMath()
{
  return !(m_textStyle == TS_LABEL ||
           m_textStyle == TS_USERLABEL ||
           m_textStyle == TS_INPUT);
}

#if wxUSE_ACCESSIBILITY
wxAccStatus Cell::GetDescription(int childId, wxString *description)
{
  if(description == NULL)
    return wxACC_FAIL;

  if (childId == 0)
  {
	  *description = _("Math output");
	  return wxACC_OK;
  }
  else
  {
    Cell *cell = NULL;
    if(GetChild(childId,&cell) == wxACC_OK)
    {
      if(cell != NULL)
        return cell->GetDescription(0, description);
    }
  }

  *description = wxEmptyString;
  return wxACC_FAIL;
}

wxAccStatus Cell::GetParent (wxAccessible **parent)
{
  if(parent == NULL)
    return wxACC_FAIL;

  if(*parent != this)
    *parent = m_parent;
  else
  {
    if((*m_configuration)->GetWorkSheet() != NULL)
      *parent = (*m_configuration)->GetWorkSheet()->GetAccessible();
  }
  return  wxACC_OK;
}

wxAccStatus Cell::GetValue (int childId, wxString *strValue)
{
  if(strValue == NULL)
    return wxACC_FAIL;

  Cell *cell;
  if(GetChild(childId,&cell) == wxACC_OK)
  {
    *strValue = cell->ToString();
    return wxACC_OK;
  }
  else
  {
    *strValue = wxEmptyString;
    return wxACC_FAIL;
  }
}

wxAccStatus Cell::GetChildCount (int *childCount)
{
  *childCount = GetInnerCells().size();
  return wxACC_OK;
}

wxAccStatus Cell::HitTest(const wxPoint &pt,
	int *childId, Cell  **childObject)
{
  wxRect rect;
  GetLocation(rect, 0);
  // If this cell doesn't contain the point none of the sub-cells does.
  if (!rect.Contains(pt))
  {
    if (childObject != NULL)
      *childObject = NULL;
    if (childId != NULL)
      *childId = 0;
    return wxACC_FAIL;
  }
  else
  {
    int childCount;
    GetChildCount(&childCount);
    for (int i = 0; i < childCount; i++)
    {
      Cell *child;
      GetChild(i, &child);
      child->GetLocation(rect, 0);
      if (rect.Contains(pt))
      {
        if (childObject != NULL)
          *childObject = child;
        if (childId != NULL)
          *childId = i;
        return wxACC_OK;
      }
    }
    if (childObject != NULL)
      *childObject = this;
    if (childId != NULL)
      *childId = 0;
    return wxACC_OK;
  }
}

wxAccStatus Cell::GetChild(int childId, Cell  **child)
{
  if(child == NULL)
    return wxACC_FAIL;

  if (childId == 0)
  {
    *child = this;
    return wxACC_OK;
  }
  else
  {
    if (childId > 0)
    {
      std::list<Cell*> cellList = m_parent->GetInnerCells();
      int cnt = 1;
      for (std::list<Cell *>::iterator it = cellList.begin(); it != cellList.end(); ++it)
        if (cnt++ == childId)
        {
          *child = *it;
          return wxACC_OK;
        }
    }
    return wxACC_FAIL;
  }
}

wxAccStatus Cell::GetFocus (int *childId, Cell  **child)
{
  int childCount;
  GetChildCount(&childCount);

  for(int i = 0; i < childCount;i++)
  {
    int dummy1;
    Cell *cell = NULL;
    GetChild(i + 1, &cell);
    if (cell != NULL)
      if(cell->GetFocus(&dummy1, child) == wxACC_OK)
      {
        if(childId != NULL)
          *childId = i+1;
        if(child != NULL)
          *child = cell;
        return wxACC_OK;
      }
  }

  if(childId != NULL)
    *childId = 0;
  if(child != NULL)
    *child = NULL;
  return wxACC_FAIL;
}

wxAccStatus Cell::GetLocation(wxRect &rect, int elementId)
{
  if(elementId == 0)
  {
    rect = wxRect(GetRect().GetTopLeft()     + (*m_configuration)->GetVisibleRegion().GetTopLeft(),
                  GetRect().GetBottomRight() + (*m_configuration)->GetVisibleRegion().GetTopLeft());
    if(rect.GetTop() < 0)
      rect.SetTop(0);
    if(rect.GetLeft() < 0)
      rect.SetLeft(0);
    if(rect.GetBottom() > (*m_configuration)->GetVisibleRegion().GetWidth())
      rect.SetBottom((*m_configuration)->GetVisibleRegion().GetWidth());
    if(rect.GetRight() > (*m_configuration)->GetVisibleRegion().GetHeight())
      rect.SetRight((*m_configuration)->GetVisibleRegion().GetHeight());
    rect = wxRect(rect.GetTopLeft()+(*m_configuration)->GetWorksheetPosition(),rect.GetBottomRight()+(*m_configuration)->GetWorksheetPosition());
    return wxACC_OK;
  }
  else
  {
    Cell *cell = NULL;
	if (GetChild(elementId, &cell) == wxACC_OK)
		return cell->GetLocation(rect, 0);
  }
  return wxACC_FAIL;
}

wxAccStatus Cell::GetRole (int WXUNUSED(childId), wxAccRole *role)
{
  if(role != NULL)
  {
    *role =   wxROLE_SYSTEM_STATICTEXT;
    return wxACC_OK;
  }
  else return wxACC_FAIL;
}

#endif

Cell::CellPointers::CellPointers(wxScrolledCanvas *mathCtrl)
{
  m_scrollToCell = false;
  m_cellToScrollTo = NULL;
  m_wxmxImgCounter = 0;
  m_mathCtrl = mathCtrl;
  m_cellMouseSelectionStartedIn = NULL;
  m_cellKeyboardSelectionStartedIn = NULL;
  m_cellUnderPointer = NULL;
  m_cellSearchStartedIn = NULL;
  m_answerCell = NULL;
  m_indexSearchStartedAt = -1;
  m_activeCell = NULL;
  m_groupCellUnderPointer = NULL;
  m_lastWorkingGroup = NULL;
  m_workingGroup = NULL;
  m_selectionString = wxEmptyString;
  m_selectionStart = NULL;
  m_selectionEnd = NULL;
  m_currentTextCell = NULL;
}

wxString Cell::CellPointers::WXMXGetNewFileName()
{
  wxString file(wxT("image"));
  file << (++m_wxmxImgCounter) << wxT(".");
  return file;
}

bool Cell::CellPointers::ErrorList::Contains(Cell *cell)
{
  for(std::list<Cell *>::iterator it = m_errorList.begin(); it != m_errorList.end();++it)
  {
    if((*it)==cell)
      return true;
  }
  return false;
}

void Cell::MarkAsDeleted()
{
  // Delete all pointers to this cell
  if(this == m_cellPointers->CellToScrollTo())
  {
      m_cellPointers->m_scrollToCell = false;
  }
  if(this == m_cellPointers->m_workingGroup)
    m_cellPointers->m_workingGroup = NULL;
  if(this == m_cellPointers->m_lastWorkingGroup)
    m_cellPointers->m_lastWorkingGroup = NULL;
  if(this == m_cellPointers->m_activeCell)
    m_cellPointers->m_activeCell = NULL;
  if(this == m_cellPointers->m_currentTextCell)
    m_cellPointers->m_currentTextCell = NULL;

  if((this == m_cellPointers->m_selectionStart) || (this == m_cellPointers->m_selectionEnd))
    m_cellPointers->m_selectionStart = m_cellPointers->m_selectionEnd = NULL;
  if(this == m_cellPointers->m_cellUnderPointer)
    m_cellPointers->m_cellUnderPointer = NULL;

  // Delete all pointers to the cells this cell contains
  std::list<Cell *> innerCells = GetInnerCells();
  for(std::list<Cell *>::iterator it = innerCells.begin(); it != innerCells.end(); ++it)
  {
    if(*it != NULL)
      (*it)->MarkAsDeleted();
  }
}
