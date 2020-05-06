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
  This file defines the class MathParser that reads wxmx data and math from Maxima.
*/

#include <wx/config.h>
#include <wx/tokenzr.h>
#include <wx/sstream.h>
#include <wx/regex.h>
#include <wx/intl.h>

#include "MathParser.h"

#include "Version.h"
#include "ExptCell.h"
#include "SubCell.h"
#include "SqrtCell.h"
#include "LimitCell.h"
#include "MatrCell.h"
#include "ParenCell.h"
#include "AbsCell.h"
#include "ConjugateCell.h"
#include "AtCell.h"
#include "DiffCell.h"
#include "SumCell.h"
#include "IntCell.h"
#include "FunCell.h"
#include "ImgCell.h"
#include "SubSupCell.h"
#include "SlideShowCell.h"
#include "StringUtils.h"
#include "GroupCell.h"

wxXmlNode *MathParser::SkipWhitespaceNode(wxXmlNode *node)
{
  if (node)
  {
    // If this is a text node there is a chance that this is a whitespace we want to skip
    if (node->GetType() == wxXML_TEXT_NODE)
    {
      // This is a text node => Let's see if it is whitespace-only and skip it if it is.
      wxString contents = node->GetContent();
      contents.Trim();
      if (contents.Length() <= 1)
        node = node->GetNext();
    }
  }
  return node;
}

wxXmlNode *MathParser::GetNextTag(wxXmlNode *node)
{
  if (node)
    node = node->GetNext();
  return SkipWhitespaceNode(node);
}

MathParser::MathParser(Configuration **cfg, Cell::CellPointers *cellPointers, const wxString &zipfile)
{
  bool ok = m_graphRegex.Compile(wxT("[[:cntrl:]]"));
  wxASSERT(ok);
  m_configuration = cfg;
  m_cellPointers = cellPointers;
  m_ParserStyle = MC_TYPE_DEFAULT;
  m_FracStyle = FracCell::FC_NORMAL;
  m_highlight = false;
  if (!zipfile.IsEmpty())
  {
    m_fileSystem.reset(new wxFileSystem());
    m_fileSystem->ChangePathTo(zipfile + wxT("#zip:/"), true);
  }
}

MathParser::~MathParser()
{
}

// ParseCellTag
// This function is responsible for creating
// a tree of groupcells when loading XML document.
// Any changes in GroupCell structure or methods
// has to be reflected here in order to ensure proper
// loading of WXMX files.
Cell *MathParser::ParseCellTag(wxXmlNode *node)
{
  GroupCell *group = NULL;

  // read hide status
  bool hide = (node->GetAttribute(stR("hide"), stR("false")) == wxT("true")) ? true : false;
  // read (group)cell type
  wxString type = node->GetAttribute(stR("type"), stR("text"));
  wxString sectioning_level = node->GetAttribute(stR("sectioning_level"), stR("0"));

  if (type == wxT("code"))
  {
    group = new GroupCell(m_configuration, GC_TYPE_CODE, m_cellPointers);
    wxString isAutoAnswer = node->GetAttribute(stR("auto_answer"), stR("no"));
    if(isAutoAnswer == wxT("yes"))
      group->AutoAnswer(true);
    int i = 1;
    wxString answer;
    wxString question;
    while (node->GetAttribute(wxString::Format(stR("answer%i"),i),&answer))
    {
      if(node->GetAttribute(wxString::Format(stR("question%i"),i),&question))
        group->SetAnswer(question,answer);
      else
        group->SetAnswer(wxString::Format(stR("Question #%i"),i),answer);
      i++;
    }
    wxXmlNode *children = node->GetChildren();
    children = SkipWhitespaceNode(children);
    while (children)
    {
      if (children->GetName() == wxT("input"))
      {
        std::unique_ptr<Cell> editor(ParseTag(children->GetChildren()));
        if (editor == NULL)
          editor = std::unique_ptr<Cell>(
            new EditorCell(group, m_configuration, m_cellPointers, _("Bug: Missing contents")));
        if(editor)
          group->SetEditableContent(editor->GetValue());
      }
      if (children->GetName() == wxT("output"))
      {
        group->AppendOutput(HandleNullPointer(ParseTag(children->GetChildren())));
      }
      children = GetNextTag(children);
    }
  }
  else if (type == wxT("image"))
  {
    group = new GroupCell(m_configuration, GC_TYPE_IMAGE, m_cellPointers);
    wxXmlNode *children = node->GetChildren();
    children = SkipWhitespaceNode(children);
    while (children)
    {
      if (children->GetName() == wxT("editor"))
      {
        std::unique_ptr<Cell> ed( ParseEditorTag(children));
        if(ed)
          group->SetEditableContent(ed->GetValue());
      }
      else
        group->AppendOutput(ParseTag(children));
      children = GetNextTag(children);
    }
  }
  else if (type == wxT("pagebreak"))
  {
    group = new GroupCell(m_configuration, GC_TYPE_PAGEBREAK, m_cellPointers);
  }
  else if (type == wxT("text"))
  {
    group = new GroupCell(m_configuration, GC_TYPE_TEXT, m_cellPointers);
    std::unique_ptr<Cell> editor(ParseTag(node->GetChildren()));
    if (editor == NULL)
      editor = std::unique_ptr<Cell>(
        new EditorCell(group, m_configuration, m_cellPointers, _("Bug: Missing contents")));
    if(editor)
      group->SetEditableContent(editor->GetValue());
  }
  else
  {
    // text types
    if (type == wxT("title"))
      group = new GroupCell(m_configuration, GC_TYPE_TITLE, m_cellPointers);
    else if (type == wxT("section"))
      group = new GroupCell(m_configuration, GC_TYPE_SECTION, m_cellPointers);
    else if (type == wxT("subsection"))
    {
      group = NULL;
      // We save subsubsections as subsections with a higher sectioning level:
      // This makes them backwards-compatible in the way that they are displayed
      // as subsections on old wxMaxima installations.
      // A sectioning level of the value 0 means that the file is too old to
      // provide a sectioning level.
      if ((sectioning_level == wxT("0")) || (sectioning_level == wxT("3")))
        group = new GroupCell(m_configuration, GC_TYPE_SUBSECTION, m_cellPointers);
      if (sectioning_level == wxT("4"))
        group = new GroupCell(m_configuration, GC_TYPE_SUBSUBSECTION, m_cellPointers);
      if (sectioning_level == wxT("5"))
        group = new GroupCell(m_configuration, GC_TYPE_HEADING5, m_cellPointers);
      if (group == NULL)
        group = new GroupCell(m_configuration, GC_TYPE_HEADING6, m_cellPointers);
    }
    else if (type == wxT("subsubsection"))
    {
      group = new GroupCell(m_configuration, GC_TYPE_SUBSUBSECTION, m_cellPointers);
    }
    else if (type == wxT("heading5"))
    {
      group = new GroupCell(m_configuration, GC_TYPE_HEADING5, m_cellPointers);
    }
    else if (type == wxT("heading6"))
    {
      group = new GroupCell(m_configuration, GC_TYPE_HEADING6, m_cellPointers);
    }
    else
      return NULL;

    wxXmlNode *children = node->GetChildren();
    children = SkipWhitespaceNode(children);
    while (children)
    {
      if (children->GetName() == wxT("editor"))
      {
        std::unique_ptr<Cell> ed(ParseEditorTag(children));
        if(ed)
          group->SetEditableContent(ed->GetValue());
      }
      else if (children->GetName() == wxT("fold"))
      { // we have folded groupcells
        wxXmlNode *xmlcells = children->GetChildren();
        xmlcells = SkipWhitespaceNode(xmlcells);
        Cell *tree = NULL;
        Cell *last = NULL;
        while (xmlcells)
        {
          Cell *cell = ParseTag(xmlcells, false);

          if (cell == NULL)
            continue;

          if (tree == NULL) tree = cell;

          if (last == NULL) last = cell;
          else
          {
            last->m_next = cell;
            last->SetNextToDraw(cell);
            last->m_next->m_previous = last;

            last = last->m_next;
          }
          xmlcells = GetNextTag(xmlcells);
        }
        if (tree)
          group->HideTree(dynamic_cast<GroupCell *>(tree));
      }
      children = GetNextTag(children);
    }
  }

  group->SetGroup(group);
  group->Hide(hide);
  return group;
}

Cell *MathParser::HandleNullPointer(Cell *cell)
{
  if (cell == NULL)
  {
    cell = new TextCell(NULL, m_configuration, m_cellPointers, _("Bug: Missing contents"));
    cell->SetToolTip(_("The xml data from maxima or from the .wxmx file was missing data here.\n"
                       "If you find a way how to reproduce this problem please file a bug "
                       "report against wxMaxima."));
    cell->SetStyle(TS_ERROR);
  }
  return (cell);
}

Cell *MathParser::ParseEditorTag(wxXmlNode *node)
{
  EditorCell *editor = new EditorCell(NULL, m_configuration, m_cellPointers);
  wxString type = node->GetAttribute(stR("type"), stR("input"));
  if (type == wxT("input"))
    editor->SetType(MC_TYPE_INPUT);
  else if (type == wxT("text"))
    editor->SetType(MC_TYPE_TEXT);
  else if (type == wxT("title"))
    editor->SetType(MC_TYPE_TITLE);
  else if (type == wxT("section"))
    editor->SetType(MC_TYPE_SECTION);
  else if (type == wxT("subsection"))
    editor->SetType(MC_TYPE_SUBSECTION);
  else if (type == wxT("subsubsection"))
    editor->SetType(MC_TYPE_SUBSUBSECTION);
  else if (type == wxT("heading5"))
    editor->SetType(MC_TYPE_HEADING5);
  else if (type == wxT("heading6"))
    editor->SetType(MC_TYPE_HEADING6);

  wxString text = wxEmptyString;
  wxXmlNode *line = node->GetChildren();
  while (line)
  {
    if (line->GetName() == wxT("line"))
    {
      if (!text.IsEmpty())
        text += wxT('\n');
      text += line->GetNodeContent();
    }
    line = line->GetNext();
  } // end while
  editor->SetValue(text);
  return editor;
}

Cell *MathParser::ParseFracTag(wxXmlNode *node)
{
  FracCell *frac = new FracCell(NULL, m_configuration, m_cellPointers);
  frac->SetFracStyle(m_FracStyle);
  frac->SetHighlight(m_highlight);
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);
  frac->SetNum(HandleNullPointer(ParseTag(child, false)));
  child = GetNextTag(child);
  frac->SetDenom(HandleNullPointer(ParseTag(child, false)));
  
  if (node->GetAttribute(stR("line")) == wxT("no"))
    frac->SetFracStyle(FracCell::FC_CHOOSE);
  if (node->GetAttribute(stR("diffstyle")) == wxT("yes"))
    frac->SetFracStyle(FracCell::FC_DIFF);
  frac->SetType(m_ParserStyle);
  frac->SetStyle(TS_VARIABLE);
  frac->SetupBreakUps();
  ParseCommonAttrs(node, frac);
  return frac;
}

Cell *MathParser::ParseDiffTag(wxXmlNode *node)
{
  DiffCell *diff = new DiffCell(NULL, m_configuration, m_cellPointers);
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);
  if (child)
  {
    int fc = m_FracStyle;
    m_FracStyle = FracCell::FC_DIFF;

    diff->SetDiff(HandleNullPointer(ParseTag(child, false)));
    m_FracStyle = fc;
    child = GetNextTag(child);

    diff->SetBase(HandleNullPointer(ParseTag(child, true)));
    diff->SetType(m_ParserStyle);
    diff->SetStyle(TS_VARIABLE);
  }
  ParseCommonAttrs(node, diff);
  return diff;
}

Cell *MathParser::ParseSupTag(wxXmlNode *node)
{
  ExptCell *expt = new ExptCell(NULL, m_configuration, m_cellPointers);
  if (node->GetAttributes() != NULL)
    expt->IsMatrix(true);
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);

  Cell *baseCell;
    expt->SetBase(baseCell = HandleNullPointer(ParseTag(child, false)));
  child = GetNextTag(child);

  Cell *power = HandleNullPointer(ParseTag(child, false));
  power->SetExponentFlag();
  expt->SetPower(power);
  expt->SetType(m_ParserStyle);
  expt->SetStyle(TS_VARIABLE);

  ParseCommonAttrs(node, expt);
  if(node->GetAttribute(stR("mat"), stR("false")) == wxT("true"))
    expt->SetAltCopyText(baseCell->ToString()+stR("^^")+power->ToString());

  return expt;
}

Cell *MathParser::ParseSubSupTag(wxXmlNode *node)
{
  SubSupCell *subsup = new SubSupCell(NULL, m_configuration, m_cellPointers);
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);
  subsup->SetBase(HandleNullPointer(ParseTag(child, false)));
  child = GetNextTag(child);
  wxString pos;
  if((child != NULL) && (child->GetAttribute("pos", wxEmptyString) != wxEmptyString))
  {
    while(child != NULL)
    {
      Cell *cell = HandleNullPointer(ParseTag(child, false));
      pos = child->GetAttribute("pos", wxEmptyString);
      if(pos == "presub")
        subsup->SetPreSub(cell);
      if(pos == "presup")
        subsup->SetPreSup(cell);
      if(pos == "postsup")
        subsup->SetPostSup(cell);
      if(pos == "postsub")
        subsup->SetPostSub(cell);
      child = SkipWhitespaceNode(child);
      child = GetNextTag(child);
    }
  }
  else
  {
    Cell *index = HandleNullPointer(ParseTag(child, false));
    index->SetExponentFlag();
    subsup->SetIndex(index);
    child = GetNextTag(child);
    Cell *power = HandleNullPointer(ParseTag(child, false));
    power->SetExponentFlag();
    subsup->SetExponent(power);
    subsup->SetType(m_ParserStyle);
    subsup->SetStyle(TS_VARIABLE);
    ParseCommonAttrs(node, subsup);
  }
  return subsup;
}

Cell *MathParser::ParseMmultiscriptsTag(wxXmlNode *node)
{
  bool pre = false;
  bool subscript = true;
  SubSupCell *subsup = new SubSupCell(NULL, m_configuration, m_cellPointers);
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);
  subsup->SetBase(HandleNullPointer(ParseTag(child, false)));
  child = GetNextTag(child);
  while(child != NULL)
  {
    if(child->GetName() == "mprescripts")
    {
      pre = true;
      subscript = true;
      child = GetNextTag(child);
      continue;
    }
    
    if(child->GetName() != "none")
    {
      if(pre && subscript)
        subsup->SetPreSub(ParseTag(child, false));
      if(pre && (!subscript))
        subsup->SetPreSup(ParseTag(child, false));
      if((!pre) && subscript)
        subsup->SetPostSub(ParseTag(child, false));
      if((!pre) && (!subscript))
        subsup->SetPostSup(ParseTag(child, false));
    }
    subscript = !subscript;
    child = SkipWhitespaceNode(child);
    child = GetNextTag(child);
  }
  return subsup;
}

Cell *MathParser::ParseSubTag(wxXmlNode *node)
{
  SubCell *sub = new SubCell(NULL, m_configuration, m_cellPointers);
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);
  sub->SetBase(HandleNullPointer(ParseTag(child, false)));
  child = GetNextTag(child);
  Cell *index = HandleNullPointer(ParseTag(child, false));
  sub->SetIndex(index);
  index->SetExponentFlag();
  sub->SetType(m_ParserStyle);
  sub->SetStyle(TS_VARIABLE);
  ParseCommonAttrs(node, sub);
  return sub;
}

Cell *MathParser::ParseAtTag(wxXmlNode *node)
{
  AtCell *at = new AtCell(NULL, m_configuration, m_cellPointers);
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);

  at->SetBase(HandleNullPointer(ParseTag(child, false)));
  at->SetHighlight(m_highlight);
  child = GetNextTag(child);
  at->SetIndex(HandleNullPointer(ParseTag(child, false)));
  at->SetType(m_ParserStyle);
  at->SetStyle(TS_VARIABLE);
  ParseCommonAttrs(node, at);
  return at;
}

Cell *MathParser::ParseFunTag(wxXmlNode *node)
{
  FunCell *fun = new FunCell(NULL, m_configuration, m_cellPointers);
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);

  fun->SetName(HandleNullPointer(ParseTag(child, false)));
  child = GetNextTag(child);
  fun->SetType(m_ParserStyle);
  fun->SetStyle(TS_FUNCTION);
  fun->SetArg(HandleNullPointer(ParseTag(child, false)));
  ParseCommonAttrs(node, fun);
  if((fun != NULL) && (fun->ToString().Contains(")(")))
    fun->SetToolTip(_("If this isn't a function returning a lambda() expression a multiplication sign (*) between closing and opening parenthesis is missing here."));
  return fun;
}

Cell *MathParser::ParseText(wxXmlNode *node, TextStyle style)
{
  wxString str;
  TextCell *retval = NULL;
  if ((node != NULL) && ((str = node->GetContent()) != wxEmptyString))
  {
    str.Replace(stR("-"), stR("\u2212")); // unicode minus sign

    wxStringTokenizer lines(str, stR('\n'));
    while (lines.HasMoreTokens())
    {
      TextCell *cell = new TextCell(NULL, m_configuration, m_cellPointers);
      switch(style)
      {
      case TS_ERROR:
        cell->SetType(MC_TYPE_ERROR);
        break;

      case TS_WARNING:
        cell->SetType(MC_TYPE_WARNING);
        break;

      case TS_LABEL:
        cell->SetType(MC_TYPE_LABEL);
        break;

      case TS_USERLABEL:
        cell->SetType(MC_TYPE_LABEL);
        break;

      default:
        cell->SetType(m_ParserStyle);
      }
      cell->SetStyle(style);
      
      cell->SetHighlight(m_highlight);
      cell->SetValue(lines.GetNextToken());
      if (retval == NULL)
        retval = cell;
      else
      {
        cell->ForceBreakLine(true);
        retval->AppendCell(cell);
      };
    }
  }

  if (retval == NULL)
    retval = new TextCell(NULL, m_configuration, m_cellPointers);

  ParseCommonAttrs(node, retval);
  return retval;
}

void MathParser::ParseCommonAttrs(wxXmlNode *node, Cell *cell)
{
  if(cell == NULL)
    return;
  if(node == NULL)
    return;

  if(node->GetAttribute(stR("breakline"), stR("false")) == wxT("true"))
    cell->ForceBreakLine(true);

  wxString val;
  
  if(node->GetAttribute(stR("tooltip"), &val))
    cell->SetToolTip(val);
  if(node->GetAttribute(stR("altCopy"), &val))
    cell->SetAltCopyText(val);
}

Cell *MathParser::ParseCharCode(wxXmlNode *node, TextStyle style)
{
  TextCell *cell = new TextCell(NULL, m_configuration, m_cellPointers);
  wxString str;
  if ((node != NULL) && ((str = node->GetContent()) != wxEmptyString))
  {
    long code;
    if (str.ToLong(&code))
      str = wxString::Format(stR("%c"), code);
    cell->SetValue(str);
    cell->SetType(m_ParserStyle);
    cell->SetStyle(style);
    cell->SetHighlight(m_highlight);
  }
  ParseCommonAttrs(node, cell);
  return cell;
}

Cell *MathParser::ParseSqrtTag(wxXmlNode *node)
{
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);

  SqrtCell *cell = new SqrtCell(NULL, m_configuration, m_cellPointers);

  cell->SetInner(HandleNullPointer(ParseTag(child, true)));
  cell->SetType(m_ParserStyle);
  cell->SetStyle(TS_VARIABLE);
  cell->SetHighlight(m_highlight);
  ParseCommonAttrs(node, cell);
  return cell;
}

Cell *MathParser::ParseAbsTag(wxXmlNode *node)
{
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);
  AbsCell *cell = new AbsCell(NULL, m_configuration, m_cellPointers);
  cell->SetInner(HandleNullPointer(ParseTag(child, true)));
  cell->SetType(m_ParserStyle);
  cell->SetStyle(TS_VARIABLE);
  cell->SetHighlight(m_highlight);
  ParseCommonAttrs(node, cell);
  return cell;
}

Cell *MathParser::ParseConjugateTag(wxXmlNode *node)
{
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);
  ConjugateCell *cell = new ConjugateCell(NULL, m_configuration, m_cellPointers);
  cell->SetInner(HandleNullPointer(ParseTag(child, true)));
  cell->SetType(m_ParserStyle);
  cell->SetStyle(TS_VARIABLE);
  cell->SetHighlight(m_highlight);
  ParseCommonAttrs(node, cell);
  return cell;
}

Cell *MathParser::ParseParenTag(wxXmlNode *node)
{
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);
  ParenCell *cell = new ParenCell(NULL, m_configuration, m_cellPointers);
  // No special Handling for NULL args here: They are completely legal in this case.
  cell->SetInner(ParseTag(child, true), m_ParserStyle);
  cell->SetHighlight(m_highlight);
  cell->SetStyle(TS_VARIABLE);
  if (node->GetAttributes() != NULL)
    cell->SetPrint(false);
  ParseCommonAttrs(node, cell);
  return cell;
}

Cell *MathParser::ParseLimitTag(wxXmlNode *node)
{
  LimitCell *limit = new LimitCell(NULL, m_configuration, m_cellPointers);
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);
  limit->SetName(HandleNullPointer(ParseTag(child, false)));
  child = GetNextTag(child);
  limit->SetUnder(HandleNullPointer(ParseTag(child, false)));
  child = GetNextTag(child);
  limit->SetBase(HandleNullPointer(ParseTag(child, false)));
  limit->SetType(m_ParserStyle);
  limit->SetStyle(TS_VARIABLE);
  ParseCommonAttrs(node, limit);
  return limit;
}

Cell *MathParser::ParseSumTag(wxXmlNode *node)
{
  SumCell *sum = new SumCell(NULL, m_configuration, m_cellPointers);
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);
  wxString type = node->GetAttribute(stR("type"), stR("sum"));

  if (type == wxT("prod"))
    sum->SetSumStyle(SM_PROD);
  sum->SetHighlight(m_highlight);
  sum->SetUnder(HandleNullPointer(ParseTag(child, false)));
  child = GetNextTag(child);
  if (type != wxT("lsum"))
    sum->SetOver(HandleNullPointer(ParseTag(child, false)));
  child = GetNextTag(child);
  sum->SetBase(HandleNullPointer(ParseTag(child, false)));
  sum->SetType(m_ParserStyle);
  sum->SetStyle(TS_VARIABLE);
  ParseCommonAttrs(node, sum);
  return sum;
}

Cell *MathParser::ParseIntTag(wxXmlNode *node)
{
  IntCell *in = new IntCell(NULL, m_configuration, m_cellPointers);
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);
  in->SetHighlight(m_highlight);
  wxString definiteAtt = node->GetAttribute(stR("def"), stR("true"));
  if (definiteAtt != wxT("true"))
  {
    in->SetBase(HandleNullPointer(ParseTag(child, false)));
    child = GetNextTag(child);
    in->SetVar(HandleNullPointer(ParseTag(child, true)));
    in->SetType(m_ParserStyle);
    in->SetStyle(TS_VARIABLE);
  }
  else
  {
    // A Definite integral
    in->SetIntStyle(IntCell::INT_DEF);
    in->SetUnder(HandleNullPointer(ParseTag(child, false)));
    child = GetNextTag(child);
    in->SetOver(HandleNullPointer(ParseTag(child, false)));
    child = GetNextTag(child);
    in->SetBase(HandleNullPointer(ParseTag(child, false)));
    child = GetNextTag(child);
    in->SetVar(HandleNullPointer(ParseTag(child, true)));
    in->SetType(m_ParserStyle);
    in->SetStyle(TS_VARIABLE);
  }
  ParseCommonAttrs(node, in);
  return in;
}

Cell *MathParser::ParseTableTag(wxXmlNode *node)
{
  MatrCell *matrix = new MatrCell(NULL, m_configuration, m_cellPointers);
  matrix->SetHighlight(m_highlight);

  if (node->GetAttribute(stR("special"), stR("false")) == wxT("true"))
    matrix->SetSpecialFlag(true);
  if (node->GetAttribute(stR("inference"), stR("false")) == wxT("true"))
  {
    matrix->SetInferenceFlag(true);
    matrix->SetSpecialFlag(true);
  }
  if (node->GetAttribute(stR("colnames"), stR("false")) == wxT("true"))
    matrix->ColNames(true);
  if (node->GetAttribute(stR("rownames"), stR("false")) == wxT("true"))
    matrix->RowNames(true);
  if (node->GetAttribute(stR("roundedParens"), stR("false")) == wxT("true"))
    matrix->RoundedParens(true);

  wxXmlNode *rows = SkipWhitespaceNode(node->GetChildren());
  while (rows)
  {
    matrix->NewRow();
    wxXmlNode *cells = SkipWhitespaceNode(rows->GetChildren());
    while (cells)
    {
      matrix->NewColumn();
      matrix->AddNewCell(HandleNullPointer(ParseTag(cells, false)));
      cells = GetNextTag(cells);
    }
    rows = GetNextTag(rows);
  }
  matrix->SetType(m_ParserStyle);
  matrix->SetStyle(TS_VARIABLE);
  matrix->SetDimension();
  ParseCommonAttrs(node, matrix);
  return matrix;
}

#define WITH_TAG_DEFINITIONS \
  FOR_TAG_DEF(v) FOR_TAG_DEF(mi) FOR_TAG_DEF(mo) FOR_TAG_DEF(t) \
  FOR_TAG_DEF(n) FOR_TAG_DEF(mn) FOR_TAG_DEF(h) \
  FOR_TAG_DEF(p) FOR_TAG_DEF(f) FOR_TAG_DEF(mfrac) \
  FOR_TAG_DEF(e) FOR_TAG_DEF(msup) FOR_TAG_DEF(i) FOR_TAG_DEF(munder) \
  FOR_TAG_DEF(fn) FOR_TAG_DEF(g) FOR_TAG_DEF(s) FOR_TAG_DEF(fnm) \
  FOR_TAG_DEF(q) FOR_TAG_DEF(d) FOR_TAG_DEF(sm) FOR_TAG_DEF(in) \
  FOR_TAG_DEF(mspace) FOR_TAG_DEF(at) FOR_TAG_DEF(a) FOR_TAG_DEF(cj) \
  FOR_TAG_DEF(ie) FOR_TAG_DEF(mmultiscripts) FOR_TAG_DEF(lm) \
  FOR_TAG_DEF(r) FOR_TAG_DEF(mrow) FOR_TAG_DEF(tb) \
  FOR_TAG_DEF(mth) FOR_TAG_DEF(line) FOR_TAG_DEF(lbl) \
  FOR_TAG_DEF(st) FOR_TAG_DEF(hl) FOR_TAG_DEF(img) \
  FOR_TAG_DEF(slide) FOR_TAG_DEF(editor) FOR_TAG_DEF(cell) \
  FOR_TAG_DEF(ascii) \

#define FOR_TAG_DEF(name) name,
enum class Tag
{
  _unknown = -1,
  WITH_TAG_DEFINITIONS
};
#undef FOR_TAG_DEF

struct TagNameDef {
  Tag tag;
  wxString name;
};

#define FOR_TAG_DEF(name) { Tag::name, wxT(#name) },
static const TagNameDef TagDefs[]{
  WITH_TAG_DEFINITIONS
};
#undef FOR_TAG_DEF

#undef WITH_TAG_DEFINITIONS

Cell *MathParser::ParseTag(wxXmlNode *node, bool all)
{
  Cell *retval = NULL;
  Cell *cell = NULL;
  bool warning = all;
  wxString altCopy;

  node = SkipWhitespaceNode(node);

  while (node)
  {
    if (node->GetType() == wxXML_ELEMENT_NODE)
    {
      // Parse XML tags. The only other type of element we recognize are text
      // nodes.
      Cell *tmp = nullptr;
      const wxString &tagName(node->GetName());
      Tag tag = Tag::_unknown;
      for (auto &def : TagDefs)
        if (def.name == tagName)
        {
          tag = def.tag;
          break;
        }

      switch (tag)
      {
      case Tag::v:
      case Tag::mi: // Variables (atoms)
        tmp = ParseText(node->GetChildren(), TS_VARIABLE);
        break;
      case Tag::mo: // operator
        tmp = ParseText(node->GetChildren(), TS_FUNCTION);
        break;
      case Tag::t: // Other text
      {
        TextStyle style = TS_DEFAULT;
        if (node->GetAttribute(stR("type")) == wxT("error"))
          style = TS_ERROR;
        if (node->GetAttribute(stR("type")) == wxT("warning"))
          style = TS_WARNING;
        tmp = ParseText(node->GetChildren(), style);
        break;
      }
      case Tag::n:
      case Tag::mn: // Numbers
        tmp = ParseText(node->GetChildren(), TS_NUMBER);
        break;
      case Tag::h: // Hidden cells (*)
        tmp = ParseText(node->GetChildren());
        tmp->m_isHidableMultSign = true;
        break;
      case Tag::p: // Parenthesis
        tmp = ParseParenTag(node);
        break;
      case Tag::f:
      case Tag::mfrac: // Fractions
        tmp = ParseFracTag(node);
        break;
      case Tag::e:
      case Tag::msup: // Exponentials
        tmp = ParseSupTag(node);
        break;
      case Tag::i:
      case Tag::munder: // Subscripts
        tmp = ParseSubTag(node);
        break;
      case Tag::fn: // Functions
        tmp = ParseFunTag(node);
        break;
      case Tag::g: // Greek constants
        tmp = ParseText(node->GetChildren(), TS_GREEK_CONSTANT);
        break;
      case Tag::s: // Special constants %e,...
        tmp = ParseText(node->GetChildren(), TS_SPECIAL_CONSTANT);
        break;
      case Tag::fnm: // Function names
        tmp = ParseText(node->GetChildren(), TS_FUNCTION);
        break;
      case Tag::q: // Square roots
        tmp = ParseSqrtTag(node);
        break;
      case Tag::d: // Differentials
        tmp = ParseDiffTag(node);
        break;
      case Tag::sm: // Sums
        tmp = ParseSumTag(node);
        break;
      case Tag::in: // integrals
        tmp = ParseIntTag(node);
        break;
      case Tag::mspace:
        tmp = new TextCell(NULL, m_configuration, m_cellPointers, stR(" "));
        break;
      case Tag::at:
        tmp = ParseAtTag(node);
        break;
      case Tag::a:
        tmp = ParseAbsTag(node);
        break;
      case Tag::cj:
        tmp = ParseConjugateTag(node);
        break;
      case Tag::ie:
        tmp = ParseSubSupTag(node);
        break;
      case Tag::mmultiscripts:
        tmp = ParseMmultiscriptsTag(node);
        break;
      case Tag::lm: // A limit tag
        tmp = ParseLimitTag(node);
        break;
      case Tag::r:
      case Tag::mrow: // A group of tags
        tmp = ParseTag(node->GetChildren());
        break;
      case Tag::tb:
        tmp = ParseTableTag(node);
        break;
      case Tag::editor:
        tmp = ParseEditorTag(node);
        break;
      case Tag::cell:
        tmp = ParseCellTag(node);
        break;
      case Tag::ascii:
        tmp = ParseCharCode(node->GetChildren());
        break;
      case Tag::st:
        tmp = ParseText(node->GetChildren(), TS_STRING);
        break;
      case Tag::hl:
      {
        bool highlight = m_highlight;
        m_highlight = true;
        tmp = ParseTag(node->GetChildren());
        m_highlight = highlight;
        break;
      }
      case Tag::mth:
      case Tag::line:
        tmp = ParseTag(node->GetChildren());
        if (tmp != NULL)
          tmp->ForceBreakLine(true);
        else
          tmp = new TextCell(NULL, m_configuration, m_cellPointers, stR(" "));
        break;
      case Tag::lbl:
      {
        wxString user_lbl = node->GetAttribute(stR("userdefinedlabel"), m_userDefinedLabel);
        wxString userdefined = node->GetAttribute(stR("userdefined"), stR("no"));

        if ( userdefined != wxT("yes"))
        {
          tmp = ParseText(node->GetChildren(), TS_LABEL);
        }
        else
        {
          tmp = ParseText(node->GetChildren(), TS_USERLABEL);

          // Backwards compatibility to 17.04/17.12:
          // If we cannot find the user-defined label's text but still know that there
          // is one it's value has been saved as "automatic label" instead.
          if(user_lbl == wxEmptyString)
          {
            user_lbl = dynamic_cast<TextCell *>(tmp)->GetValue();
            user_lbl = user_lbl.substr(1,user_lbl.Length() - 2);
          }
        }

        dynamic_cast<TextCell *>(tmp)->SetUserDefinedLabel(user_lbl);
        tmp->ForceBreakLine(true);
        break;
      }
      case Tag::img:
      {
        Cell *imageCell;
        wxString filename(node->GetChildren()->GetContent());

        if (m_fileSystem) // loading from zip
          imageCell = new ImgCell(NULL, m_configuration, m_cellPointers, filename, m_fileSystem, false);
        else
        {
          if (node->GetAttribute(stR("del"), stR("yes")) != wxT("no"))
          {
            std::shared_ptr <wxFileSystem> noFS;
            if(wxImage::GetImageCount(filename) < 2)
              imageCell = new ImgCell(NULL, m_configuration, m_cellPointers, filename, noFS, true);
            else
              imageCell = new SlideShow(NULL, m_configuration, m_cellPointers, filename, true);
          }
          else
          {
            // This is the only case show_image() produces ergo this is the only
            // case we might get a local path
            if (
              (!wxFileExists(filename)) &&
              (wxFileExists((*m_configuration)->GetWorkingDirectory() + wxT('/') + filename))
              )
              filename = (*m_configuration)->GetWorkingDirectory() + wxT('/') + filename;
            if(wxImage::GetImageCount(filename) < 2)
              imageCell = new ImgCell(NULL, m_configuration, m_cellPointers, filename, {/*noFS*/}, false);
            else
              imageCell = new SlideShow(NULL, m_configuration, m_cellPointers, filename, false);
          }
        }
        wxString gnuplotSource = node->GetAttribute(stR("gnuplotsource"), wxEmptyString);
        wxString gnuplotData = node->GetAttribute(stR("gnuplotdata"), wxEmptyString);
        if(imageCell->GetType() == MC_TYPE_IMAGE)
        {
          if((imageCell != NULL) && (gnuplotSource != wxEmptyString))
          {
            dynamic_cast<ImgCell *>(imageCell)->GnuplotSource(gnuplotSource, gnuplotData, m_fileSystem);
          }
          if (node->GetAttribute(stR("rect"), stR("true")) == wxT("false"))
            dynamic_cast<ImgCell *>(imageCell)->DrawRectangle(false);
          wxString sizeString;
          if ((sizeString = node->GetAttribute(stR("maxWidth"), stR("-1"))) != wxT("-1"))
          {
            double width;
            if(sizeString.ToDouble(&width))
              dynamic_cast<ImgCell *>(imageCell)->SetMaxWidth(width);
          }
          if ((sizeString = node->GetAttribute(stR("maxHeight"), stR("-1"))) != wxT("-1"))
          {
            double height;
            if(sizeString.ToDouble(&height))
              dynamic_cast<ImgCell *>(imageCell)->SetMaxHeight(height);
          }
        }
        tmp = imageCell;
        break;
      }
      case Tag::slide:
      {
        wxString gnuplotSources;
        wxString gnuplotData;
        bool del = node->GetAttribute(stR("del"), stR("false")) == wxT("true");
        node->GetAttribute(stR("gnuplotSources"), &gnuplotSources);
        node->GetAttribute(stR("gnuplotData"), &gnuplotData);
        SlideShow *slideShow = new SlideShow(NULL, m_configuration, m_cellPointers, m_fileSystem);
        wxString str(node->GetChildren()->GetContent());
        wxArrayString images;
        wxString framerate;
        if (node->GetAttribute(stR("fr"), &framerate))
        {
          long fr;
          if (framerate.ToLong(&fr))
            slideShow->SetFrameRate(fr);
        }
        if (node->GetAttribute(stR("frame"), &framerate))
        {
          long frame;
          if (framerate.ToLong(&frame))
            slideShow->SetDisplayedIndex(frame);
        }
        if (node->GetAttribute(stR("running"), stR("true")) == wxT("false"))
          slideShow->AnimationRunning(false);
        wxStringTokenizer imageFiles(str, stR(";"));
        int numImgs = 0;
        while (imageFiles.HasMoreTokens())
        {
          wxString imageFile = imageFiles.GetNextToken();
          if (imageFile.Length())
          {
            images.Add(imageFile);
            numImgs++;
          }
        }
        if (slideShow)
        {
          slideShow->LoadImages(images, del);
          wxStringTokenizer dataFiles(gnuplotData, stR(";"));
          wxStringTokenizer gnuplotFiles(gnuplotSources, stR(";"));
          for(int i=0; i<numImgs; i++)
          {
            if((dataFiles.HasMoreTokens()) && (gnuplotFiles.HasMoreTokens()))
            {
              slideShow->GnuplotSource(
                i,
                gnuplotFiles.GetNextToken(),
                dataFiles.GetNextToken(),
                m_fileSystem
                );
            }
          }
        }
        tmp = slideShow;
        break;
      }
      case Tag::_unknown:
        if (node->GetChildren())
          tmp = ParseTag(node->GetChildren());
        break;
      }

      // Append the cell we found (tmp) to the list of cells we parsed so far (cell).
      if (tmp)
      {
        ParseCommonAttrs(node, tmp);
        if (cell == NULL)
          cell = tmp;
        else
          cell->AppendCell(tmp);
      }
    }
    else
    {
      // We didn't get a tag but got a text cell => Parse the text.
      if (!cell)
        cell = ParseText(node);
      else
        cell->AppendCell(ParseText(node));
    }

    if (cell)
    {
      // Append the new cell to the return value
      if (!retval)
        retval = cell;
      else
        cell = cell->m_next;
    }
    else if ((warning) && (!all))
    {
      // Tell the user we ran into problems.
      wxString name;
      if (cell) name = cell->ToString();
      name.Trim(true);
      name.Trim(false);
      if (!name.IsEmpty())
      {
        LoggingMessageBox(_("Parts of the document will not be loaded correctly:\nFound unknown XML Tag name " + name),
                     _("Warning"),
                     wxOK | wxICON_WARNING);
        warning = false;
      }
    }

    node = GetNextTag(node);
    
    if (!all)
      break;
  }

  return retval;
}

/***
 * Parse the string s, which is (correct) xml fragment.
 * Put the result in line.
 */
Cell *MathParser::ParseLine(wxString s, CellType style)
{
  m_ParserStyle = style;
  m_FracStyle = FracCell::FC_NORMAL;
  m_highlight = false;
  Cell *cell = NULL;

  int showLength;

  switch ((*m_configuration)->ShowLength())
  {
    case 0:
      showLength = 6000;
      break;
    case 1:
      showLength = 20000;
      break;
    case 2:
      showLength = 250000;
      break;
    case 3:
      showLength = 0;
      break;
  default:
      showLength = 50000;    
  }

  m_graphRegex.Replace(&s, stR("\uFFFD"));

  if (((long) s.Length() < showLength) || (showLength == 0))
  {

    wxXmlDocument xml;

    wxStringInputStream xmlStream(s);

    xml.Load(xmlStream, stR("UTF-8"), wxXMLDOC_KEEP_WHITESPACE_NODES);

    wxXmlNode *doc = xml.GetRoot();

    if (doc != NULL)
      cell = ParseTag(doc->GetChildren());
  }
  else
  {
    cell = new TextCell(NULL, m_configuration, m_cellPointers,
                        _("(Expression longer than allowed by the configuration setting)"),
      TS_WARNING);
    cell->SetToolTip(_("The maximum size of the expressions wxMaxima is allowed to display "
                       "can be changed in the configuration dialogue."
                       ));
    cell->ForceBreakLine(true);
  }
  return cell;
}
