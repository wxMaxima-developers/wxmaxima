///
///  Copyright (C) 2008-2011 Andrej Vodopivec <andrej.vodopivec@gmail.com>
///            (C) 2008-2009 Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
///            (C) 2012 Doug Ilijev <doug.ilijev@gmail.com>
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

#include <wx/config.h>
#include <wx/clipbrd.h>

#include "GroupCell.h"
#include "TextCell.h"
#include "EditorCell.h"
#include "ImgCell.h"
#include "Bitmap.h"

GroupCell::GroupCell(int groupType, wxString initString) : MathCell()
{
  m_input = NULL;
  m_output = NULL;
  m_hiddenTree = NULL;
  m_hiddenTreeParent = NULL;
  m_outputRect.x = -1;
  m_outputRect.y = -1;
  m_outputRect.width = 0;
  m_outputRect.height = 0;
  m_group = this;
  m_forceBreakLine = true;
  m_breakLine = true;
  m_type = MC_TYPE_GROUP;
  m_indent = MC_GROUP_LEFT_INDENT;
  m_hide = false;
  m_working = false;
  m_groupType = groupType;
  m_lastInOutput = NULL;
  m_appendedCells = NULL;

  // set up cell depending on groupType, so we have a working cell
  if (groupType != GC_TYPE_PAGEBREAK) {
    if (groupType == GC_TYPE_CODE)
      m_input = new TextCell(EMPTY_INPUT_LABEL);
    else
      m_input = new TextCell(wxT(" "));

    m_input->SetType(MC_TYPE_MAIN_PROMPT);
  }

  bool match = true;
  bool insertAns = true;
  wxConfig::Get()->Read(wxT("matchParens"), &match);
  wxConfig::Get()->Read(wxT("insertAns"), &insertAns);
  EditorCell *editor = new EditorCell();
  editor->SetMatchParens(match);
  editor->SetInsertAns(insertAns);

  switch (groupType) {
    case GC_TYPE_CODE:
      editor->SetType(MC_TYPE_INPUT);
      AppendInput(editor);
      break;
    case GC_TYPE_TEXT:
      m_input->SetType(MC_TYPE_TEXT);
      editor->SetType(MC_TYPE_TEXT);
      AppendInput(editor);
      break;
    case GC_TYPE_TITLE:
      m_input->SetType(MC_TYPE_TITLE);
      editor->SetType(MC_TYPE_TITLE);
      AppendInput(editor);
      break;
    case GC_TYPE_SECTION:
      m_input->SetType(MC_TYPE_SECTION);
      editor->SetType(MC_TYPE_SECTION);
      AppendInput(editor);
      break;
    case GC_TYPE_SUBSECTION:
      m_input->SetType(MC_TYPE_SUBSECTION);
      editor->SetType(MC_TYPE_SUBSECTION);
      AppendInput(editor);
      break;
    case GC_TYPE_IMAGE:
      m_input->SetType(MC_TYPE_TEXT);
      editor->SetType(MC_TYPE_TEXT);
      editor->SetValue(wxEmptyString);
      AppendInput(editor);
      break;
    default:
      delete editor;
      editor = NULL;
      break;
  }

  if (editor != NULL)
    editor->SetValue(initString);

  // when creating an image cell, if a string is provided
  // it loads an image (without deleting it)
  if ((groupType == GC_TYPE_IMAGE) && (initString.Length() > 0)) {
    ImgCell *ic = new ImgCell(initString, false);
    AppendOutput(ic);
  }

  SetParent(this, false);
}

GroupCell::~GroupCell()
{
  if (m_input != NULL)
    delete m_input;
  DestroyOutput();
  if (m_hiddenTree)
    delete m_hiddenTree;
}

void GroupCell::SetParent(MathCell *parent, bool all)
{
  if (m_input != NULL)
    m_input->SetParent(parent, true);

  MathCell *tmp = m_output;
  while (tmp != NULL) {
    tmp->SetParent(parent, false);
    tmp = tmp->m_next;
  }
}

void GroupCell::DestroyOutput()
{
  MathCell *tmp = m_output, *tmp1;
  while (tmp != NULL) {
    tmp1 = tmp;
    tmp = tmp->m_next;
    tmp1->Destroy();
    delete tmp1;
  }
  m_output = NULL;
}

// when all=false (default) only reset input label of the current (code) cell
// if all = true, then also reset next cells and folded cells
void GroupCell::ResetInputLabel(bool all)
{
  if (m_groupType == GC_TYPE_CODE) {
    if (m_input)
      m_input->SetValue(EMPTY_INPUT_LABEL);
  }
  // if all, also reset input labels in the folded cells
  else if (all && IsFoldable() && m_hiddenTree)
    m_hiddenTree->ResetInputLabel(true);

  // reset the next cell
  if (all && m_next)
    dynamic_cast<GroupCell*>(m_next)->ResetInputLabel(true);
}

MathCell* GroupCell::Copy(bool all)
{
  GroupCell* tmp = new GroupCell(m_groupType);
  tmp->Hide(m_hide);
  CopyData(this, tmp);
  if (m_input)
    tmp->SetInput(m_input->Copy(true));
  if (m_output != NULL)
    tmp->SetOutput(m_output->Copy(true));
  if (all && m_next != NULL)
    tmp->AppendCell(m_next->Copy(all));
  return tmp;
}

void GroupCell::Destroy()
{
  if (m_input != NULL)
    delete m_input;
  m_input = NULL;
  if (m_output != NULL)
    DestroyOutput();
  m_output = NULL;
  m_next = NULL;
}

void GroupCell::SetInput(MathCell *input)
{
  if (input == NULL)
    return ;
  if (m_input != NULL)
    delete m_input;
  m_input = input;
  m_input->m_group = this;
}

void GroupCell::AppendInput(MathCell *cell)
{
  if (m_input == NULL) {
    m_input = cell;
  }
  else
  {
    if (m_input->m_next == NULL)
      m_input->AppendCell(cell);
    else if (m_input->m_next->GetValue().Length() == 0) {
      delete m_input->m_next;
      m_input->m_next = m_input->m_nextToDraw = NULL;
      m_input->AppendCell(cell);
    }
    else {
      AppendOutput(cell);
      m_hide = false;
    }
  }
}

void GroupCell::SetOutput(MathCell *output)
{
  if (output == NULL)
    return ;
  if (m_output != NULL)
    DestroyOutput();

  m_output = output;
  m_output->m_group = this;

  m_lastInOutput = m_output;

  while (m_lastInOutput->m_next != NULL)
    m_lastInOutput = m_lastInOutput->m_next;

  //m_appendedCells = output;
}

void GroupCell::RemoveOutput()
{
  DestroyOutput();
  ResetSize();
  m_output = NULL;
  m_lastInOutput = NULL;
  m_appendedCells = NULL;
  m_hide = false;
}

void GroupCell::AppendOutput(MathCell *cell)
{
  if (m_output == NULL) {
    m_output = cell;

    if (m_groupType == GC_TYPE_CODE && m_input->m_next != NULL)
      ((EditorCell *)(m_input->m_next))->ContainsChanges(false);

    m_lastInOutput = m_output;

    while (m_lastInOutput->m_next != NULL)
      m_lastInOutput = m_lastInOutput->m_next;

  }

  else {
    MathCell *tmp = m_lastInOutput;
    if (tmp == NULL)
      tmp = m_output;

    while (tmp->m_next != NULL)
      tmp = tmp->m_next;

    tmp->AppendCell(cell);

    while (m_lastInOutput->m_next != NULL)
      m_lastInOutput = m_lastInOutput->m_next;
  }

  if (m_appendedCells == NULL)
    m_appendedCells = cell;
}

void GroupCell::Recalculate(CellParser& parser, int d_fontsize, int m_fontsize)
{
  m_fontSize = d_fontsize;
  m_mathFontSize = m_fontsize;

  RecalculateWidths(parser, d_fontsize, false);
  RecalculateSize(parser, d_fontsize, false);
}

void GroupCell::RecalculateWidths(CellParser& parser, int fontsize, bool all)
{
  if (m_width == -1 || m_height == -1 || parser.ForceUpdate())
  {
    // special case of 'line cell'
    if (m_groupType == GC_TYPE_PAGEBREAK) {
      m_width = 10;
      m_height = 2;
      MathCell::RecalculateWidths(parser, fontsize, all);
      return;
    }

    UnBreakUpCells();

    double scale = parser.GetScale();
    m_input->RecalculateWidths(parser, fontsize, true);

    // recalculate the position of input in ReEvaluateSelection!
    if (m_input->m_next != NULL) {
      m_input->m_next->m_currentPoint.x = m_currentPoint.x + m_input->GetWidth() + MC_CELL_SKIP;
    }

    if (m_output == NULL || m_hide) {
      m_width = m_input->GetFullWidth(scale);
    }

    else {
      MathCell *tmp = m_output;
      while (tmp != NULL) {
        tmp->RecalculateWidths(parser, tmp->IsMath() ? m_mathFontSize : m_fontSize, false);
        tmp = tmp->m_next;
      }
      // This is not correct, m_width will be computed correctly in RecalculateSize!
      m_width = m_input->GetFullWidth(scale);
    }

    BreakUpCells(parser, m_fontSize, parser.GetClientWidth());
    BreakLines(parser.GetClientWidth());
  }
  MathCell::RecalculateWidths(parser, m_fontSize, all);
}

void GroupCell::RecalculateSize(CellParser& parser, int fontsize, bool all)
{
  if (m_width == -1 || m_height == -1 || parser.ForceUpdate())
  {
    // special case
    if (m_groupType == GC_TYPE_PAGEBREAK) {
      m_width = 10;
      m_height = 2;
      m_center = 0;
      m_indent = 0;
      MathCell::RecalculateWidths(parser, fontsize, all);
      return;
    }

    double scale = parser.GetScale();
    m_input->RecalculateSize(parser, fontsize, true);
    m_center = m_input->GetMaxCenter();
    m_height = m_input->GetMaxHeight();
    m_indent = parser.GetIndent();

    if (m_output != NULL && !m_hide) {
      MathCell *tmp = m_output;
      while (tmp != NULL) {
        tmp->RecalculateSize(parser,  tmp->IsMath() ? m_mathFontSize : m_fontSize, false);
        tmp = tmp->m_next;
      }

      m_outputRect.x = m_currentPoint.x;
      m_outputRect.y = m_currentPoint.y - m_output->GetMaxCenter();
      m_outputRect.width = 0;
      m_outputRect.height = 0;
      m_height = m_input->GetMaxHeight();
      m_width = m_input->GetFullWidth(scale);

      tmp = m_output;
      while (tmp != NULL) {
        if (tmp->BreakLineHere() || tmp == m_output) {
          m_width = MAX(m_width, tmp->GetLineWidth(scale));
          m_outputRect.width = MAX(m_outputRect.width, tmp->GetLineWidth(scale));
          m_height += tmp->GetMaxHeight();
          if (tmp->m_bigSkip)
            m_height += MC_LINE_SKIP;
          m_outputRect.height += tmp->GetMaxHeight() + MC_LINE_SKIP;
        }
        tmp = tmp->m_nextToDraw;
      }
    }
  }

  MathCell::RecalculateSize(parser, fontsize, all);
}

// We assume that appended cells will be in a new line!
void GroupCell::RecalculateAppended(CellParser& parser)
{
  if (m_appendedCells == NULL)
    return;

  MathCell *tmp = m_appendedCells;
  int fontsize = m_fontSize;
  double scale = parser.GetScale();

  // Recalculate widths of cells
  while (tmp != NULL) {
    tmp->RecalculateWidths(parser, tmp->IsMath() ? m_mathFontSize : m_fontSize, false);
    tmp = tmp->m_next;
  }

  // Breakup cells and break lines
  BreakUpCells(m_appendedCells, parser, fontsize, parser.GetClientWidth());
  BreakLines(m_appendedCells, parser.GetClientWidth());

  // Recalculate size of cells
  tmp = m_appendedCells;
  while (tmp != NULL) {
    tmp->RecalculateSize(parser,  tmp->IsMath() ? m_mathFontSize : m_fontSize, false);
    tmp = tmp->m_next;
  }

  // Update widths
  tmp = m_appendedCells;
  while (tmp != NULL) {
    if (tmp->BreakLineHere() || tmp == m_appendedCells) {
      m_width = MAX(m_width, tmp->GetLineWidth(scale));
      m_outputRect.width = MAX(m_outputRect.width, tmp->GetLineWidth(scale));
      m_height += tmp->GetMaxHeight();
      if (tmp->m_bigSkip)
        m_height += MC_LINE_SKIP;
      m_outputRect.height += tmp->GetMaxHeight() + MC_LINE_SKIP;
    }
    tmp = tmp->m_nextToDraw;
  }

  m_appendedCells = NULL;
}

void GroupCell::Draw(CellParser& parser, wxPoint point, int fontsize, bool all)
{
  double scale = parser.GetScale();
  wxDC& dc = parser.GetDC();
  if (m_width == -1 || m_height == -1) {
    RecalculateWidths(parser, fontsize, false);
    RecalculateSize(parser, fontsize, false);
  }
  if (DrawThisCell(parser, point))
  {
    // draw a thick line for 'page break'
    // and return
    if (m_groupType == GC_TYPE_PAGEBREAK) {
      wxRect rect = GetRect(false);
      int y = rect.GetY();
      wxPen pen(parser.GetColor(TS_CURSOR), 1, wxPENSTYLE_DOT);
      dc.SetPen(pen);
      dc.DrawLine(0, y , 10000, y);
      MathCell::Draw(parser, point, fontsize, all);
      return;
    }

    //
    // Paint background if we have a text cell
    //
    if (m_groupType == GC_TYPE_TEXT) {
      wxRect rect = GetRect(false);
      int y = rect.GetY();

      if (m_height > 0 && m_width > 0 && y>=0) {
        wxBrush br(parser.GetColor(TS_TEXT_BACKGROUND));
        dc.SetBrush(br);
        wxPen pen(parser.GetColor(TS_TEXT_BACKGROUND));
        dc.SetPen(pen);
        dc.DrawRectangle(0, y , 10000, rect.GetHeight());
      }
    }
    //
    // Draw input and output
    //
    SetPen(parser);
    wxPoint in(point);
    parser.Outdated(false);
    m_input->Draw(parser, in, fontsize, true);
    if (m_groupType == GC_TYPE_CODE && m_input->m_next)
      parser.Outdated(((EditorCell *)(m_input->m_next))->ContainsChanges());

    if (m_output != NULL && !m_hide) {
      MathCell *tmp = m_output;
      int drop = tmp->GetMaxDrop();
      in.y += m_input->GetMaxDrop() + m_output->GetMaxCenter();
      m_outputRect.y = in.y - m_output->GetMaxCenter();
      m_outputRect.x = in.x;

      while (tmp != NULL) {

        if (!tmp->m_isBroken) {
          tmp->m_currentPoint.x = in.x;
          tmp->m_currentPoint.y = in.y;
          if (tmp->DrawThisCell(parser, in))
            tmp->Draw(parser, in, MAX(tmp->IsMath() ? m_mathFontSize : m_fontSize, MC_MIN_SIZE), false);
          if (tmp->m_nextToDraw != NULL) {
            if (tmp->m_nextToDraw->BreakLineHere()) {
              in.x = m_indent;
              in.y += drop + tmp->m_nextToDraw->GetMaxCenter();
              if (tmp->m_bigSkip)
                in.y += MC_LINE_SKIP;
              drop = tmp->m_nextToDraw->GetMaxDrop();
            } else
              in.x += (tmp->GetWidth() + MC_CELL_SKIP);
          }

        } else {
          if (tmp->m_nextToDraw != NULL && tmp->m_nextToDraw->BreakLineHere()) {
            in.x = m_indent;
            in.y += drop + tmp->m_nextToDraw->GetMaxCenter();
            if (tmp->m_bigSkip)
              in.y += MC_LINE_SKIP;
            drop = tmp->m_nextToDraw->GetMaxDrop();
          }
        }

        tmp = tmp->m_nextToDraw;
      }
    }

    parser.Outdated(false);
    MathCell *editable = GetEditable();
    if (editable != NULL && editable->IsActive()) {
      dc.SetPen( *(wxThePenList->FindOrCreatePen(parser.GetColor(TS_ACTIVE_CELL_BRACKET), 2, wxPENSTYLE_SOLID))); // window linux, set a pen
      dc.SetBrush( *(wxTheBrushList->FindOrCreateBrush(parser.GetColor(TS_ACTIVE_CELL_BRACKET)))); //highlight c.
    }
    else {
      dc.SetPen( *(wxThePenList->FindOrCreatePen(parser.GetColor(TS_CELL_BRACKET), 1, wxPENSTYLE_SOLID))); // window linux, set a pen
      dc.SetBrush( *(wxTheBrushList->FindOrCreateBrush(parser.GetColor(TS_CELL_BRACKET)))); //highlight c.
    }

    if ((!m_hide) && (!m_hiddenTree)) {
      dc.SetBrush(*wxTRANSPARENT_BRUSH);
    }

    if (IsFoldable()) { // draw a square
      wxPoint *points = new wxPoint[4];
      points[0].x = point.x - SCALE_PX(10, scale);
      points[0].y = point.y - m_center;
      points[1].x = point.x - SCALE_PX(10, scale);
      points[1].y = point.y - m_center + SCALE_PX(10, scale);
      points[2].x = point.x;
      points[2].y = point.y - m_center + SCALE_PX(10, scale);
      points[3].x = point.x;
      points[3].y = point.y - m_center;
      dc.DrawPolygon(4, points);
      delete [] points;
    }
    else { // draw a triangle and line
      wxPoint *points = new wxPoint[3];
      points[0].x = point.x - SCALE_PX(10, scale);
      points[0].y = point.y - m_center;
      points[1].x = point.x - SCALE_PX(10, scale);
      points[1].y = point.y - m_center + SCALE_PX(10, scale);
      points[2].x = point.x;
      points[2].y = point.y - m_center;
      dc.DrawPolygon(3, points);
      delete [] points;

      // vertical
      dc.DrawLine(point.x - SCALE_PX(10, scale), point.y - m_center,
          point.x - SCALE_PX(10, scale), point.y - m_center + m_height);
      // bottom horizontal
      dc.DrawLine(point.x - SCALE_PX(10, scale), point.y - m_center + m_height,
          point.x - SCALE_PX(2, scale) , point.y - m_center + m_height);
      // middle horizontal
      if (m_groupType == GC_TYPE_CODE && m_output != NULL && !m_hide) {
        dc.DrawLine(point.x - SCALE_PX(6, scale),
            point.y - m_center + m_input->GetMaxHeight(),
            point.x - SCALE_PX(10, scale),
            point.y - m_center + m_input->GetMaxHeight());
      }
    }

    UnsetPen(parser);
  }
  MathCell::Draw(parser, point, fontsize, all);
}

wxRect GroupCell::HideRect()
{
  return wxRect(m_currentPoint.x - 10, m_currentPoint.y - m_center, 10, 10);
}

wxString GroupCell::ToString(bool all)
{
  wxString str;
  if (GetEditable()) {
    str = m_input->ToString(true);
    if (m_output != NULL && !m_hide) {
      MathCell *tmp = m_output;
      while (tmp != NULL) {
        if (tmp->ForceBreakLineHere() && str.Length()>0)
          str += wxT("\n");
        str += tmp->ToString(false);
        tmp = tmp->m_nextToDraw;
      }
    }
  }
  return str + MathCell::ToString(all);
}

wxString GroupCell::PrepareForTeX(wxString str)
{
#if !wxUSE_UNICODE
  wxString str1(str.wc_str(wxConvLocal), wxConvUTF8);
#else
  wxString str1(str);
#endif

  str1.Replace(wxT("\\"), wxT("\\verb|\\|"));
  str1.Replace(wxT("_"), wxT("\\_"));
  str1.Replace(wxT("%"), wxT("\\%"));
  str1.Replace(wxT("$"), wxT("\\$"));
  str1.Replace(wxT("{"), wxT("\\{"));
  str1.Replace(wxT("}"), wxT("\\}"));
  str1.Replace(wxT("^"), wxT("\\verb|^|"));
  str1.Replace(wxT(">"), wxT("\\verb|>|"));
  str1.Replace(wxT("<"), wxT("\\verb|<|"));

#if !wxUSE_UNICODE
  wxString str2(str1.wc_str(wxConvUTF8), wxConvLocal);
#else
  wxString str2(str1);
#endif

  return str2;
}

wxString GroupCell::ToTeX(bool all)
{
  return ToTeX(all, wxEmptyString, wxEmptyString, NULL);
}

wxString GroupCell::ToTeX(bool all, wxString imgDir, wxString filename, int *imgCounter)
{
  wxString str;

  // pagebreak
  if (m_groupType == GC_TYPE_PAGEBREAK) {
    str = wxT("\\pagebreak\n");
  }

  // IMAGE CELLS
  else if (m_groupType == GC_TYPE_IMAGE && imgDir != wxEmptyString) {
    MathCell *copy = m_output->Copy(false);
    (*imgCounter)++;
    wxString image = filename + wxString::Format(wxT("_%d.png"), *imgCounter);
    wxString file = imgDir + wxT("/") + image;

    Bitmap bmp;
    bmp.SetData(copy);

    if (!wxDirExists(imgDir))
      wxMkdir(imgDir);

    if (bmp.ToFile(file))
    {
      str << wxT("\\begin{figure}[htb]\n")
          << wxT("  \\begin{center}\n")
          << wxT("    \\includegraphics{")
          << filename << wxT("_img/") << image << wxT("}\n")
          << wxT("  \\caption{") << PrepareForTeX(m_input->m_next->GetValue()) << wxT("}\n")
          << wxT("  \\end{center}\n")
          << wxT("\\end{figure}");
    }
  }
  else if (m_groupType == GC_TYPE_IMAGE)
    str << wxT("\n\\vert|<<GRAPHICS>>|\n");

  // CODE CELLS
  else if (m_groupType == GC_TYPE_CODE) {
    // Input cells
    str = wxT("\n\\noindent\n%%%%%%%%%%%%%%%\n")
          wxT("%%% INPUT:\n")
          wxT("\\begin{minipage}[t]{8ex}{\\color{red}\\bf\n")
          wxT("\\begin{verbatim}\n") +
          m_input->ToString(false) +
          wxT("\n\\end{verbatim}}\n\\end{minipage}");

    if (m_input->m_next!=NULL)
    {
      str += wxT("\n\\begin{minipage}[t]{\\textwidth}{\\color{blue}\n\\begin{verbatim}\n") +
             m_input->m_next->ToString(true) +
             wxT("\n\\end{verbatim}}\n\\end{minipage}");
    }

    str += wxT("\n");

    if (m_output != NULL) {
      str += wxT("%%% OUTPUT:\n");
      // Need to define labelcolor if this is Copy as LaTeX!
      if (imgCounter == NULL)
        str += wxT("\\definecolor{labelcolor}{RGB}{100,0,0}\n");
      str += wxT("\\begin{math}\\displaystyle\n");
      MathCell *tmp = m_output;

      while (tmp != NULL) {

        if (tmp->GetType() == MC_TYPE_IMAGE || tmp->GetType() == MC_TYPE_SLIDE)
        {
          if (imgDir != wxEmptyString)
          {
            MathCell *copy = tmp->Copy(false);
            (*imgCounter)++;
            wxString image = filename + wxString::Format(wxT("_%d.png"), *imgCounter);
            wxString file = imgDir + wxT("/") + image;

            Bitmap bmp;
            bmp.SetData(copy);

            if (!wxDirExists(imgDir))
              if (!wxMkdir(imgDir))
                continue;

            if (bmp.ToFile(file))
              str += wxT("\\includegraphics[width=9cm]{") +
                  filename + wxT("_img/") + image + wxT("}");
          }
          else
            str << wxT("\n\\verb|<<GRAPHICS>>|\n");
        }

        else if (tmp->GetStyle() == TS_LABEL)
        {
          if (str.Right(13) != wxT("displaystyle\n"))
            str += wxT("\n\\end{math}\n\n\\begin{math}\\displaystyle\n");
          str += wxT("\\parbox{8ex}{\\color{labelcolor}") + tmp->ToTeX(false) + wxT("}\n");
        }

        else
          str += tmp->ToTeX(false);

        tmp = tmp->m_nextToDraw;
      }
      str += wxT("\n\\end{math}\n%%%%%%%%%%%%%%%\n");
    }
  }

  // TITLES, SECTIONS, SUBSECTIONS, TEXT
  else if (GetEditable() != NULL && !m_hide) {
    str = GetEditable()->ToTeX(true);
    switch (GetEditable()->GetStyle()) {
      case TS_TITLE:
        str = wxT("\n\\pagebreak{}\n{\\Huge {\\sc ") + PrepareForTeX(str) + wxT("}}\n");
        str += wxT("\\setcounter{section}{0}\n\\setcounter{subsection}{0}\n");
        str += wxT("\\setcounter{figure}{0}\n\n");
        break;
      case TS_SECTION:
        str = wxT("\n\\section{") + PrepareForTeX(str) + wxT("}\n\n");
        break;
      case TS_SUBSECTION:
        str = wxT("\n\\subsection{") + PrepareForTeX(str) + wxT("}\n\n");
        break;
      default:
        if (str.StartsWith(wxT("TeX:")))
          str = str.Mid(5, str.Length());
        else {
          str = PrepareForTeX(str);
        }
        break;
    }
  }

  return str + MathCell::ToTeX(all);
}

// ToXML
// writes a groupcell in the form of
// <cell type="code" hide="true">
// --contents--
// </cell>
wxString GroupCell::ToXML(bool all)
{
  wxString str;
  str = wxT("\n<cell"); // start opening tag
  // write "type" according to m_groupType
  switch (m_groupType) {
    case GC_TYPE_CODE:
      str += wxT(" type=\"code\"");
      break;
    case GC_TYPE_IMAGE:
      str += wxT(" type=\"image\"");
      break;
    case GC_TYPE_TEXT:
      str += wxT(" type=\"text\"");
      break;
    case GC_TYPE_TITLE:
      str += wxT(" type=\"title\"");
      break;
    case GC_TYPE_SECTION:
      str += wxT(" type=\"section\"");
      break;
    case GC_TYPE_SUBSECTION:
      str += wxT(" type=\"subsection\"");
      break;
    case GC_TYPE_PAGEBREAK:
      {
        str += wxT(" type=\"pagebreak\"/>");
        return str + MathCell::ToXML(all);
      }
      break;
    default:
      str += wxT(" type=\"unknown\"");
      break;
  }

  // write hidden status
  if (m_hide)
    str += wxT(" hide=\"true\"");
  str += wxT(">\n");

  MathCell *input = GetInput();
  MathCell *output = GetLabel();
  // write contents
  switch (m_groupType) {
    case GC_TYPE_CODE:
      if (input != NULL) {
        str += wxT("<input>\n");
        str += input->ToXML(false);
        str += wxT("</input>");
      }
      if (output != NULL) {
        str += wxT("\n<output>\n");
        str += wxT("<mth>") + output->ToXML(true) + wxT("</mth>");
        str += wxT("\n</output>");
      }
      break;
    case GC_TYPE_IMAGE:
      if (input != NULL)
        str += input->ToXML(false);
      if (output != NULL)
        str += output->ToXML(true);
      break;
    case GC_TYPE_TEXT:
      if (input)
        str += input->ToXML(false);
      break;
    case GC_TYPE_TITLE:
    case GC_TYPE_SECTION:
    case GC_TYPE_SUBSECTION:
      if (input)
        str += input->ToXML(false);
      if (m_hiddenTree) {
        str += wxT("<fold>");
        GroupCell *tmp = m_hiddenTree;
        while (tmp) {
          str += tmp->ToXML(false);
          tmp = dynamic_cast<GroupCell*>(tmp->m_next);
        }
        str += wxT("</fold>");
      }
      break;
    default:
      if (output != NULL)
        str += output->ToXML(false);
      break;
  }
  str += wxT("\n</cell>\n");

  return str + MathCell::ToXML(all);
}

void GroupCell::SelectRectGroup(wxRect& rect, wxPoint& one, wxPoint& two,
    MathCell **first, MathCell **last)
{
  *first = NULL;
  *last = NULL;

  if ((m_input) && (m_input->ContainsRect(rect)))
    m_input->SelectRect(rect, first, last);
  else if (m_output != NULL && !m_hide && m_outputRect.Contains(rect))
    SelectRectInOutput(rect, one, two, first, last);

  if (*first == NULL || *last == NULL)
  {
    *first = this;
    *last = this;
  }
}

void GroupCell::SelectInner(wxRect& rect, MathCell **first, MathCell **last)
{
  *first = NULL;
  *last = NULL;

  if (m_input->ContainsRect(rect))
    m_input->SelectRect(rect, first, last);
  else if (m_output != NULL  && !m_hide && m_outputRect.Contains(rect))
    m_output->SelectRect(rect, first, last);

  if (*first == NULL || *last == NULL)
  {
    *first = this;
    *last = this;
  }
}

void GroupCell::SelectPoint(wxPoint& point, MathCell **first, MathCell **last)
{
  *first = NULL;
  *last = NULL;

  wxRect rect(point.x, point.y, 1, 1);

  if (m_input->ContainsRect(rect))
    m_input->SelectInner(rect, first, last);
}

void GroupCell::SelectRectInOutput(wxRect& rect, wxPoint& one, wxPoint& two,
    MathCell **first, MathCell **last)
{
  if (m_hide)
    return;

  MathCell* tmp;
  wxPoint start, end;

  if (one.y < two.y || (one.y == two.y && one.x < two.x)) {
    start = one;
    end = two;
  } else {
    start = two;
    end = one;
  }

  // Lets select a rectangle
  tmp = m_output;
  *first = *last = NULL;

  while (tmp != NULL && !rect.Intersects(tmp->GetRect()))
    tmp = tmp->m_nextToDraw;
  *first = tmp;
  *last = tmp;
  while (tmp != NULL) {
    if (rect.Intersects(tmp->GetRect()))
      *last = tmp;
    tmp = tmp->m_nextToDraw;
  }

  if (*first != NULL && *last != NULL) {

    // If selection is on multiple lines, we need to correct it
    if ((*first)->GetCurrentY() != (*last)->GetCurrentY()) {
      tmp = *last;
      MathCell *curr;

      // Find the first cell in selection
      while (*first != tmp &&
             ((*first)->GetCurrentX() + (*first)->GetWidth() < start.x
              || (*first)->GetCurrentY() + (*first)->GetDrop() < start.y))
        *first = (*first)->m_nextToDraw;

      // Find the last cell in selection
      curr = *last = *first;
       while (1) {
        curr = curr->m_nextToDraw;
        if (curr == NULL)
          break;
        if (curr->GetCurrentX() <= end.x &&
            curr->GetCurrentY() - curr->GetMaxCenter() <= end.y)
          *last = curr;
        if (curr == tmp)
          break;
      }
    }

    if (*first == *last)
      (*first)->SelectInner(rect, first, last);
  }
}

bool GroupCell::SetEditableContent(wxString text)
{
  if (GetEditable()) {
    GetEditable()->SetValue(text);
    return true;
  }
  else
    return false;
}

EditorCell *GroupCell::GetEditable()
{
  switch (m_groupType) {
    case GC_TYPE_CODE:
    case GC_TYPE_IMAGE:
    case GC_TYPE_TEXT:
    case GC_TYPE_TITLE:
    case GC_TYPE_SECTION:
    case GC_TYPE_SUBSECTION:
      return dynamic_cast<EditorCell*>(GetInput());
    case GC_TYPE_PAGEBREAK:
    default:
      return NULL;
  }
}

void GroupCell::BreakLines(int fullWidth)
{
  BreakLines(m_output, fullWidth);
}

void GroupCell::BreakLines(MathCell *cell, int fullWidth)
{
  int currentWidth = m_indent;

  MathCell *tmp = cell;

  while (tmp != NULL && !m_hide) {
    tmp->ResetData();
    tmp->BreakLine(false);
    if (!tmp->m_isBroken) {
      if (tmp->BreakLineHere() || (currentWidth + tmp->GetWidth() >= fullWidth)) {
        currentWidth = m_indent + tmp->GetWidth();
        tmp->BreakLine(true);
      } else
        currentWidth += (tmp->GetWidth() + MC_CELL_SKIP);
    }
    tmp = tmp->m_nextToDraw;
  }
}

void GroupCell::SelectOutput(MathCell **start, MathCell **end)
{
  if (m_hide)
    return;

  *start = m_output;

  while (*start != NULL && (*start)->GetStyle() != TS_LABEL)
    *start = (*start)->m_nextToDraw;


  if (*start != NULL)
    *start = (*start)->m_nextToDraw;

  *end = *start;

  while (*end != NULL &&
         (*end)->m_nextToDraw != NULL)
    *end = (*end)->m_nextToDraw;

  if (*end == NULL || *start == NULL)
    *end = *start = NULL;
}

void GroupCell::BreakUpCells(CellParser parser, int fontsize, int clientWidth)
{
  BreakUpCells(m_output, parser, fontsize, clientWidth);
}

void GroupCell::BreakUpCells(MathCell *cell, CellParser parser, int fontsize, int clientWidth)
{
  MathCell *tmp = cell;

  while (tmp != NULL && !m_hide) {
    if (tmp->GetWidth() > clientWidth) {
      if (tmp->BreakUp()) {
        tmp->RecalculateWidths(parser,  tmp->IsMath() ? m_mathFontSize : m_fontSize, false);
        tmp->RecalculateSize(parser,  tmp->IsMath() ? m_mathFontSize : m_fontSize, false);
      }
    }
    tmp = tmp->m_nextToDraw;
  }
}

void GroupCell::UnBreakUpCells()
{
  MathCell *tmp = m_output;
  while (tmp != NULL) {
    if (tmp->m_isBroken) {
      tmp->Unbreak(false);
    }
    tmp = tmp->m_next;
  }
}

// support for hiding text, code cells

void GroupCell::Hide(bool hide) {
  if (IsFoldable())
    return;

  if (m_hide == hide)
    return;
  else
  {
    if (m_hide == false) {
      if ((m_groupType == GC_TYPE_TEXT) || (m_groupType == GC_TYPE_CODE))
        GetEditable()->SetFirstLineOnly(true);
      m_hide = true;
    }
    else {
      if ((m_groupType == GC_TYPE_TEXT) || (m_groupType == GC_TYPE_CODE))
        GetEditable()->SetFirstLineOnly(false);
      m_hide = false;
    }

    ResetSize();
    GetEditable()->ResetSize();
  }
}

void GroupCell::SwitchHide() {
  Hide(!m_hide);
}

//
// support for folding/unfolding sections
//
bool GroupCell::HideTree(GroupCell *tree)
{
  if (m_hiddenTree)
    return false;
  m_hiddenTree = tree;
  m_hiddenTree->SetHiddenTreeParent(this);
  return true;
}

GroupCell *GroupCell::UnhideTree()
{
  GroupCell *tree = m_hiddenTree;
  m_hiddenTree->SetHiddenTreeParent(m_hiddenTreeParent);
  m_hiddenTree = NULL;
  return tree;
}

/**
 * Unfold a tree from the bottom up, when a hidden cell needs to be seen.
 *
 * @return true if any cells were unfolded.
 */
bool GroupCell::RevealHidden()
{
  if (!m_hiddenTreeParent)
    return false;
  m_hiddenTreeParent->RevealHidden();
  m_hiddenTreeParent->Unfold();
  return true;
}

/**
 * For every cell in this GroupCell, set m_hiddenTreeParent to parent.
 * This way, the field can be used to traverse up the tree no matter which
 * child we are on. In other words, every child knows its parent node.
 */
void GroupCell::SetHiddenTreeParent(GroupCell* parent) {
  GroupCell* cell = this;
  while (cell) {
    cell->m_hiddenTreeParent = parent;
    cell = dynamic_cast<GroupCell*>(cell->m_next);
  }
}

GroupCell *GroupCell::Fold() {
  if (!IsFoldable() || m_hiddenTree) // already folded?? shouldn't happen
    return NULL;
  if (m_next == NULL)
    return NULL;
  int nextgct = dynamic_cast<GroupCell*>(m_next)->GetGroupType(); // groupType of the next cell
  if ((m_groupType == nextgct) || IsLesserGCType(nextgct))
    return NULL; // if the next gc shouldn't be folded, exit

  // now there is at least one cell to fold (at least m_next)
  GroupCell *end = dynamic_cast<GroupCell*>(m_next);
  GroupCell *start = end; // first to fold

  while (end) {
    GroupCell *tmp = dynamic_cast<GroupCell*>(end->m_next);
    if (tmp == NULL)
      break;
    if ((m_groupType == tmp->GetGroupType()) || IsLesserGCType(tmp->GetGroupType()))
      break; // the next one of the end is not suitable for folding, break
    end = tmp;
  }

  // cell(s) to fold are between start and end (including these two)

  MathCell *next = end->m_next;
  m_next = m_nextToDraw = next; // may be NULL, it's ok
  if (next)
    next->m_previous = next->m_previousToDraw = this;

  start->m_previous = start->m_previousToDraw = NULL;
  end->m_next = end->m_nextToDraw = NULL;
  m_hiddenTree = start; // save the torn out tree into m_hiddenTree
  m_hiddenTree->SetHiddenTreeParent(this);
  return this;
}

// unfolds the m_hiddenTree beneath this cell
// be careful to update m_last if this happens in the main tree in MathCtrl
GroupCell *GroupCell::Unfold() {
  if (!IsFoldable() || !m_hiddenTree)
    return NULL;

  MathCell *next = m_next;

  // sew together this cell with m_hiddenTree
  m_next = m_nextToDraw = m_hiddenTree;
  m_hiddenTree->m_previous = m_hiddenTree->m_previousToDraw = this;

  MathCell *tmp = m_hiddenTree;
  while (tmp->m_next)
    tmp = tmp->m_next;
  // tmp holds the last element of m_hiddenTree
  tmp->m_next = tmp->m_nextToDraw = next;
  if (next)
    next->m_previous = next->m_previousToDraw = tmp;

  m_hiddenTree->SetHiddenTreeParent(m_hiddenTreeParent);
  m_hiddenTree = NULL;
  return dynamic_cast<GroupCell*>(tmp);
}

GroupCell *GroupCell::FoldAll(bool all) {
  GroupCell *result = this;
  if (IsFoldable() && !m_hiddenTree) {
    Fold();
    if (m_hiddenTree)
      m_hiddenTree->FoldAll(true);
  }
  else
    result = NULL;

  if (all && m_next)
    return dynamic_cast<GroupCell*>(m_next)->FoldAll(true);
  else
    return result;
}

// unfolds recursivly its contents
// if (all) then also calls it on it's m_next
GroupCell *GroupCell::UnfoldAll(bool all) {
  if (all && m_next)
    dynamic_cast<GroupCell*>(m_next)->UnfoldAll(true);

  if (!IsFoldable() || !m_hiddenTree)
    return NULL;

  m_hiddenTree->UnfoldAll(true);

  return Unfold();
}

bool GroupCell::IsLesserGCType(int comparedTo) {
  switch (m_groupType) {
    case GC_TYPE_CODE:
    case GC_TYPE_TEXT:
    case GC_TYPE_PAGEBREAK:
    case GC_TYPE_IMAGE:
      if ((comparedTo == GC_TYPE_TITLE) || (comparedTo == GC_TYPE_SECTION) ||
          (comparedTo == GC_TYPE_SUBSECTION))
        return true;
      else
        return false;
    case GC_TYPE_SUBSECTION:
      if ((comparedTo == GC_TYPE_TITLE) || (comparedTo == GC_TYPE_SECTION))
        return true;
      else
        return false;
    case GC_TYPE_SECTION:
      if (comparedTo == GC_TYPE_TITLE)
        return true;
      else
        return false;
    case GC_TYPE_TITLE:
      return false;
    default:
      return false;
  }
}

void GroupCell::Number(int &section, int &subsection, int &image) {
  switch (m_groupType) {
    case GC_TYPE_TITLE:
      section = subsection = 0;
      break;
    case GC_TYPE_SECTION:
      section++;
      subsection = 0;
      {
        wxString num = wxT(" ");
        num << section << wxT(" ");
        ((TextCell*)m_input)->SetValue(num);
      }
      break;
    case GC_TYPE_SUBSECTION:
      subsection++;
      {
        wxString num = wxT("  ");
        num << section << wxT(".") << subsection << wxT(" ");
        ((TextCell*)m_input)->SetValue(num);
      }
      break;
    case GC_TYPE_IMAGE:
      image++;
      {
        wxString num = wxString::Format(_("Figure %d:"), image);
        ((TextCell*)m_input)->SetValue(num);
      }
      break;
    default:
      break;
  }

  if (IsFoldable() && m_hiddenTree)
    m_hiddenTree->Number(section, subsection, image);

  if (m_next)
    dynamic_cast<GroupCell*>(m_next)->Number(section, subsection, image);
}

bool GroupCell::IsMainInput(MathCell *active)
{
  if (m_input->m_next == NULL)
    return false;

  return (active == m_input->m_next);
}
