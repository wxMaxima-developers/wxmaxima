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

#include <wx_inc.h>
#include <wx/config.h>
#include <wx/tokenzr.h>
#include <wx/sstream.h>
#include <wx/regex.h>

#include "MathParser.h"

#include "FracCell.h"
#include "ExptCell.h"
#include "TextCell.h"
#include "SubCell.h"
#include "SqrtCell.h"
#include "LimitCell.h"
#include "MatrCell.h"
#include "ParenCell.h"
#include "AbsCell.h"
#include "AtCell.h"
#include "DiffCell.h"
#include "SumCell.h"
#include "IntCell.h"
#include "FunCell.h"
#include "EditorCell.h"
#include "ImgCell.h"
#include "SubSupCell.h"
#include "SlideShowCell.h"
#include "GroupCell.h"

#define MAXLENGTH 50000

MathParser::MathParser(wxString zipfile)
{
  m_ParserStyle = MC_TYPE_DEFAULT;
  m_FracStyle = FC_NORMAL;
  m_highlight = false;
  if (zipfile.Length() > 0) {
    m_fileSystem = new wxFileSystem();
    m_fileSystem->ChangePathTo(wxT("file:") + zipfile + wxT("#zip:/"), true);
  }
  else
    m_fileSystem = NULL;
}

MathParser::~MathParser()
{
  if (m_fileSystem)
    delete m_fileSystem;
}

// ParseCellTag
// This function is responsible for creating
// a tree of groupcells when loading XML document.
// Any changes in GroupCell structure or methods
// has to be reflected here in order to ensure proper
// loading of WXMX files.
MathCell* MathParser::ParseCellTag(wxXmlNode* node)
{
  GroupCell *group = NULL;

  // read hide status
#if wxCHECK_VERSION(2,9,0)
  bool hide = (node->GetAttribute(wxT("hide"), wxT("false")) == wxT("true")) ? true : false;
#else
  bool hide = (node->GetPropVal(wxT("hide"), wxT("false")) == wxT("true")) ? true : false;
#endif
  // read (group)cell type
#if wxCHECK_VERSION(2,9,0)
  wxString type = node->GetAttribute(wxT("type"), wxT("text"));
#else
  wxString type = node->GetPropVal(wxT("type"), wxT("text"));
#endif

  if (type == wxT("code")) {
    group = new GroupCell(GC_TYPE_CODE);
    wxXmlNode *children = node->GetChildren();
    while (children) {
      if (children->GetName() == wxT("input")) {
        MathCell *editor = ParseTag(children->GetChildren());
        group->SetEditableContent(editor->GetValue());
        delete editor;
      }
      if (children->GetName() == wxT("output"))
        group->AppendOutput(ParseTag(children->GetChildren()));
      children = children->GetNext();
    }
  }
  else if (type == wxT("image")) {
    group = new GroupCell(GC_TYPE_IMAGE);
    wxXmlNode *children = node->GetChildren();
    while (children) {
      if (children->GetName() == wxT("editor")) {
        MathCell *ed = ParseEditorTag(children);
        group->SetEditableContent(ed->GetValue());
        delete ed;
      }
      else
        group->AppendOutput(ParseTag(children));
      children = children->GetNext();
    }
  }
  else if (type == wxT("pagebreak")) {
    group = new GroupCell(GC_TYPE_PAGEBREAK);
  }
  else if (type == wxT("text")) {
    group = new GroupCell(GC_TYPE_TEXT);
    MathCell *editor = ParseTag(node->GetChildren());
    group->SetEditableContent(editor->GetValue());
    delete editor;
  }
  else {
    // text types
    if (type == wxT("title"))
      group = new GroupCell(GC_TYPE_TITLE);
    else if (type == wxT("section"))
      group = new GroupCell(GC_TYPE_SECTION);
    else if (type == wxT("subsection"))
      group = new GroupCell(GC_TYPE_SUBSECTION);
    else
      return NULL;

    wxXmlNode *children = node->GetChildren();
    while (children) {
      if (children->GetName() == wxT("editor")) {
        MathCell *ed = ParseEditorTag(children);
        group->SetEditableContent(ed->GetValue());
        delete ed;
      }
      else if (children->GetName() == wxT("fold")) { // we have folded groupcells
        wxXmlNode *xmlcells = children->GetChildren();
        MathCell *tree = NULL;
        MathCell *last = NULL;
        if (xmlcells) {
          last = tree = ParseTag(xmlcells, false); // first cell
          while (xmlcells->GetNext()) {
            xmlcells = xmlcells->GetNext();
            MathCell *cell = ParseTag(xmlcells, false);

            last->m_next = last->m_nextToDraw = cell;
            last->m_next->m_previous = last->m_next->m_previousToDraw = last;

            last = last->m_next;
          }
          if (tree)
            group->HideTree((GroupCell *)tree);
        }
      }
      children = children->GetNext();
    }
  }

  group->SetParent(group, false);
  group->Hide(hide);
  return group;
}

MathCell* MathParser::ParseEditorTag(wxXmlNode* node)
{
  EditorCell *editor = new EditorCell();
#if wxCHECK_VERSION(2,9,0)
  wxString type = node->GetAttribute(wxT("type"), wxT("input"));
#else
  wxString type = node->GetPropVal(wxT("type"), wxT("input"));
#endif
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

  wxString text = wxEmptyString;
  wxXmlNode *line = node->GetChildren();
  while (line) {
    if (line->GetName() == wxT("line")) {
      if (!text.IsEmpty())
        text += wxT("\n");
#if wxUSE_UNICODE
      text += line->GetNodeContent();
#else
      wxString str = line->GetNodeContent();
      wxString str1(str.wc_str(wxConvUTF8), *wxConvCurrent);
      text += str1;
#endif
    }
    line = line->GetNext();
  } // end while
  editor->SetValue(text);
  return editor;
}

MathCell* MathParser::ParseFracTag(wxXmlNode* node)
{
  FracCell *frac = new FracCell;
  frac->SetFracStyle(m_FracStyle);
  frac->SetHighlight(m_highlight);
  wxXmlNode* child = node->GetChildren();
  if (child)
  {
    frac->SetNum(ParseTag(child, false));
    child = child->GetNext();
    if (child)
    {
      frac->SetDenom(ParseTag(child, false));
#if wxCHECK_VERSION(2,9,0)
      if (node->GetAttributes() != NULL)
#else
      if (node->GetProperties() != NULL)
#endif
      {
        frac->SetFracStyle(FC_CHOOSE);
      }
      frac->SetType(m_ParserStyle);
      frac->SetStyle(TS_VARIABLE);
      frac->SetupBreakUps();
      return frac;
    }
  }
  delete frac;
  return NULL;
}

MathCell* MathParser::ParseDiffTag(wxXmlNode* node)
{
  DiffCell *diff = new DiffCell;
  wxXmlNode* child = node->GetChildren();
  if (child)
  {
    int fc = m_FracStyle;
    m_FracStyle = FC_DIFF;
    diff->SetDiff(ParseTag(child, false));
    m_FracStyle = fc;
    child = child->GetNext();
    if (child)
    {
      diff->SetBase(ParseTag(child, true));
      diff->SetType(m_ParserStyle);
      diff->SetStyle(TS_VARIABLE);
      return diff;
    }
  }
  delete diff;
  return NULL;
}

MathCell* MathParser::ParseSupTag(wxXmlNode* node)
{
  ExptCell *expt = new ExptCell;
#if wxCHECK_VERSION(2,9,0)
  if (node->GetAttributes() != NULL)
#else
  if (node->GetProperties() != NULL)
#endif
    expt->IsMatrix(true);
  wxXmlNode* child = node->GetChildren();
  if (child)
  {
    expt->SetBase(ParseTag(child, false));
    child = child->GetNext();
    if (child)
    {
      MathCell* power = ParseTag(child, false);
      power->SetExponentFlag();
      expt->SetPower(power);
      expt->SetType(m_ParserStyle);
      expt->SetStyle(TS_VARIABLE);
      return expt;
    }
  }
  delete expt;
  return NULL;
}

MathCell* MathParser::ParseSubSupTag(wxXmlNode* node)
{
  SubSupCell *subsup = new SubSupCell;
  wxXmlNode* child = node->GetChildren();
  if (child)
  {
    subsup->SetBase(ParseTag(child, false));
    child = child->GetNext();
    if (child)
    {
      MathCell* index = ParseTag(child, false);
      index->SetExponentFlag();
      subsup->SetIndex(index);
      child = child->GetNext();
      if (child)
      {
        MathCell* power = ParseTag(child, false);
        power->SetExponentFlag();
        subsup->SetExponent(power);
        subsup->SetType(m_ParserStyle);
        subsup->SetStyle(TS_VARIABLE);
        return subsup;
      }
    }
  }
  delete subsup;
  return NULL;
}

MathCell* MathParser::ParseSubTag(wxXmlNode* node)
{
  SubCell *sub = new SubCell;
  wxXmlNode* child = node->GetChildren();
  if (child)
  {
    sub->SetBase(ParseTag(child, false));
    child = child->GetNext();
    if (child)
    {
      MathCell* index = ParseTag(child, false);
      index->SetExponentFlag();
      sub->SetIndex(index);
      sub->SetType(m_ParserStyle);
      sub->SetStyle(TS_VARIABLE);
      return sub;
    }
  }
  delete sub;
  return NULL;
}

MathCell* MathParser::ParseAtTag(wxXmlNode* node)
{
  AtCell *at = new AtCell;
  wxXmlNode* child = node->GetChildren();
  if (child)
  {
    at->SetBase(ParseTag(child, false));
    at->SetHighlight(m_highlight);
    child = child->GetNext();
    if (child)
    {
      at->SetIndex(ParseTag(child, false));
      at->SetType(m_ParserStyle);
      at->SetStyle(TS_VARIABLE);
      return at;
    }
  }
  delete at;
  return NULL;
}

MathCell* MathParser::ParseFunTag(wxXmlNode* node)
{
  FunCell *fun = new FunCell;
  wxXmlNode* child = node->GetChildren();
  if (child)
  {
    fun->SetName(ParseTag(child, false));
    child = child->GetNext();
    if (child)
    {
      fun->SetType(m_ParserStyle);
      fun->SetStyle(TS_VARIABLE);
      fun->SetArg(ParseTag(child, false));
      return fun;
    }
  }
  delete fun;
  return NULL;
}

MathCell* MathParser::ParseText(wxXmlNode* node, int style)
{
  TextCell* cell = new TextCell;
  wxString str;
  if (node != NULL && (str = node->GetContent()) != wxEmptyString)
  {
#if !wxUSE_UNICODE
    wxString str1(str.wc_str(wxConvUTF8), *wxConvCurrent);
    str = str1;
#endif
#if wxUSE_UNICODE
    str.Replace(wxT("-"), wxT("\x2212")); // unicode minus sign
#endif
    if (style == TS_NUMBER)
    {
      if (str.Length() > 100) // This could be made configurable.
        str = str.Left(30) + wxString::Format(wxT("[%d digits]"), str.Length() - 60) + str.Right(30);
    }
    cell->SetType(m_ParserStyle);
    cell->SetStyle(style);
    cell->SetHighlight(m_highlight);
    cell->SetValue(str);
  }
  return cell;
}

MathCell* MathParser::ParseCharCode(wxXmlNode* node, int style)
{
  TextCell* cell = new TextCell;
  wxString str;
  if (node != NULL && (str = node->GetContent()) != wxEmptyString)
  {
    long code;
    if (str.ToLong(&code))
      str = wxString::Format(wxT("%c"), code);
#if !wxUSE_UNICODE
    wxString str1(str.wc_str(wxConvUTF8), *wxConvCurrent);
    str = str1;
#endif
    cell->SetValue(str);
    cell->SetType(m_ParserStyle);
    cell->SetStyle(style);
    cell->SetHighlight(m_highlight);
  }
  return cell;
}

MathCell* MathParser::ParseSqrtTag(wxXmlNode* node)
{
  wxXmlNode* child = node->GetChildren();
  SqrtCell* cell = new SqrtCell;
  cell->SetInner(ParseTag(child, true));
  cell->SetType(m_ParserStyle);
  cell->SetStyle(TS_VARIABLE);
  cell->SetHighlight(m_highlight);
  return cell;
}

MathCell* MathParser::ParseAbsTag(wxXmlNode* node)
{
  wxXmlNode* child = node->GetChildren();
  AbsCell* cell = new AbsCell;
  cell->SetInner(ParseTag(child, true));
  cell->SetType(m_ParserStyle);
  cell->SetStyle(TS_VARIABLE);
  cell->SetHighlight(m_highlight);
  return cell;
}

MathCell* MathParser::ParseParenTag(wxXmlNode* node)
{
  wxXmlNode* child = node->GetChildren();
  ParenCell* cell = new ParenCell;
  cell->SetInner(ParseTag(child, true), m_ParserStyle);
  cell->SetHighlight(m_highlight);
  cell->SetStyle(TS_VARIABLE);
#if wxCHECK_VERSION(2,9,0)
  if (node->GetAttributes() != NULL)
    cell->SetPrint(false);
#else
  if (node->GetProperties() != NULL)
    cell->SetPrint(false);
#endif
  return cell;
}

MathCell* MathParser::ParseLimitTag(wxXmlNode* node)
{
  LimitCell *limit = new LimitCell;
  wxXmlNode* child = node->GetChildren();
  if (child)
  {
    limit->SetName(ParseTag(child, false));
    child = child->GetNext();
    if (child)
    {
      limit->SetUnder(ParseTag(child, false));
      child = child->GetNext();
      if (child)
      {
        limit->SetBase(ParseTag(child, false));
        limit->SetType(m_ParserStyle);
        limit->SetStyle(TS_VARIABLE);
        return limit;
      }
    }
  }
  delete limit;
  return NULL;
}

MathCell* MathParser::ParseSumTag(wxXmlNode* node)
{
  SumCell *sum = new SumCell;
  wxXmlNode* child = node->GetChildren();
#if wxCHECK_VERSION(2,9,0)
  wxString type = node->GetAttribute(wxT("type"), wxT("sum"));
#else
  wxString type = node->GetPropVal(wxT("type"), wxT("sum"));
#endif
  if (type == wxT("prod"))
    sum->SetSumStyle(SM_PROD);
  sum->SetHighlight(m_highlight);
  if (child)
  {
    sum->SetUnder(ParseTag(child, false));
    child = child->GetNext();
    if (child)
    {
      if (type != wxT("lsum"))
        sum->SetOver(ParseTag(child, false));
      child = child->GetNext();
      if (child)
      {
        sum->SetBase(ParseTag(child, false));
        sum->SetType(m_ParserStyle);
        sum->SetStyle(TS_VARIABLE);
        return sum;
      }
    }
  }
  delete sum;
  return NULL;
}

MathCell* MathParser::ParseIntTag(wxXmlNode* node)
{
  IntCell *in = new IntCell;
  wxXmlNode* child = node->GetChildren();
  in->SetHighlight(m_highlight);
#if wxCHECK_VERSION(2,9,0)
  if (node->GetAttributes() == NULL)
#else
  if (node->GetProperties() == NULL)
#endif
  {
    in->SetIntStyle(INT_DEF);
    if (child)
    {
      in->SetUnder(ParseTag(child, false));
      child = child->GetNext();
      if (child)
      {
        in->SetOver(ParseTag(child, false));
        child = child->GetNext();
        if (child)
        {
          in->SetBase(ParseTag(child, false));
          child = child->GetNext();
          if (child)
          {
            in->SetVar(ParseTag(child, true));
            in->SetType(m_ParserStyle);
            in->SetStyle(TS_VARIABLE);
            return in;
          }
        }
      }
    }
  }
  else
  {
    if (child)
    {
      in->SetBase(ParseTag(child, false));
      child = child->GetNext();
      if (child)
      {
        in->SetVar(ParseTag(child, true));
        in->SetType(m_ParserStyle);
        in->SetStyle(TS_VARIABLE);
        return in;
      }
    }
  }
  delete in;
  return NULL;
}

MathCell* MathParser::ParseTableTag(wxXmlNode* node)
{
  MatrCell *matrix = new MatrCell;
  matrix->SetHighlight(m_highlight);

#if wxCHECK_VERSION(2,9,0)
  if (node->GetAttribute(wxT("special"), wxT("false")) == wxT("true"))
    matrix->SetSpecialFlag(true);
  if (node->GetAttribute(wxT("inference"), wxT("false")) == wxT("true"))
  {
    matrix->SetInferenceFlag(true);
    matrix->SetSpecialFlag(true);
  }
  if (node->GetAttribute(wxT("colnames"), wxT("false")) == wxT("true"))
    matrix->ColNames(true);
  if (node->GetAttribute(wxT("rownames"), wxT("false")) == wxT("true"))
    matrix->RowNames(true);
#else
  if (node->GetPropVal(wxT("special"), wxT("false")) == wxT("true"))
    matrix->SetSpecialFlag(true);
  if (node->GetPropVal(wxT("inference"), wxT("false")) == wxT("true"))
  {
    matrix->SetInferenceFlag(true);
    matrix->SetSpecialFlag(true);
  }
  if (node->GetPropVal(wxT("colnames"), wxT("false")) == wxT("true"))
    matrix->ColNames(true);
  if (node->GetPropVal(wxT("rownames"), wxT("false")) == wxT("true"))
    matrix->RowNames(true);
#endif

  wxXmlNode* rows = node->GetChildren();
  while (rows)
  {
    matrix->NewRow();
    wxXmlNode* cells = rows->GetChildren();
    while (cells)
    {
      matrix->NewColumn();
      matrix->AddNewCell(ParseTag(cells, false));
      cells = cells->GetNext();
    }
    rows = rows->GetNext();
  }
  matrix->SetType(m_ParserStyle);
  matrix->SetStyle(TS_VARIABLE);
  matrix->SetDimension();
  return matrix;
}

MathCell* MathParser::ParseTag(wxXmlNode* node, bool all)
{
  //  wxYield();
  MathCell* tmp = NULL;
  MathCell* cell = NULL;
  bool warning = all;
  wxString altCopy;

  while (node)
  {
    // Parse tags
    if (node->GetType() == wxXML_ELEMENT_NODE)
    {
      wxString tagName(node->GetName());

      if (tagName == wxT("v"))
      {               // Variables (atoms)
        if (cell == NULL)
          cell = ParseText(node->GetChildren(), TS_VARIABLE);
        else
          cell->AppendCell(ParseText(node->GetChildren(), TS_VARIABLE));
      }
      else if (tagName == wxT("t"))
      {          // Other text
        if (cell == NULL)
          cell = ParseText(node->GetChildren(), TS_DEFAULT);
        else
          cell->AppendCell(ParseText(node->GetChildren(), TS_DEFAULT));
      }
      else if (tagName == wxT("n"))
      {          // Numbers
        if (cell == NULL)
          cell = ParseText(node->GetChildren(), TS_NUMBER);
        else
          cell->AppendCell(ParseText(node->GetChildren(), TS_NUMBER));
      }
      else if (tagName == wxT("h"))
      {          // Hidden cells (*)
        MathCell* tmp = ParseText(node->GetChildren());
        tmp->m_isHidden = true;
        if (cell == NULL)
          cell = tmp;
        else
          cell->AppendCell(tmp);
      }
      else if (tagName == wxT("p"))
      {          // Parenthesis
        if (cell == NULL)
          cell = ParseParenTag(node);
        else
          cell->AppendCell(ParseParenTag(node));
      }
      else if (tagName == wxT("f"))
      {               // Fractions
        if (cell == NULL)
          cell = ParseFracTag(node);
        else
          cell->AppendCell(ParseFracTag(node));
      }
      else if (tagName == wxT("e"))
      {          // Exponentials
        if (cell == NULL)
          cell = ParseSupTag(node);
        else
          cell->AppendCell(ParseSupTag(node));
      }
      else if (tagName == wxT("i"))
      {          // Subscripts
        if (cell == NULL)
          cell = ParseSubTag(node);
        else
          cell->AppendCell(ParseSubTag(node));
      }
      else if (tagName == wxT("fn"))
      {         // Functions
        if (cell == NULL)
          cell = ParseFunTag(node);
        else
          cell->AppendCell(ParseFunTag(node));
      }
      else if (tagName == wxT("g"))
      {          // Greek constants
        MathCell* tmp = ParseText(node->GetChildren(), TS_GREEK_CONSTANT);
        if (cell == NULL)
          cell = tmp;
        else
          cell->AppendCell(tmp);
      }
      else if (tagName == wxT("s"))
      {          // Special constants %e,...
        MathCell* tmp = ParseText(node->GetChildren(), TS_SPECIAL_CONSTANT);
        if (cell == NULL)
          cell = tmp;
        else
          cell->AppendCell(tmp);
      }
      else if (tagName == wxT("fnm"))
      {         // Function names
        MathCell* tmp = ParseText(node->GetChildren(), TS_FUNCTION);
        if (cell == NULL)
          cell = tmp;
        else
          cell->AppendCell(tmp);
      }
      else if (tagName == wxT("q"))
      {          // Square roots
        if (cell == NULL)
          cell = ParseSqrtTag(node);
        else
          cell->AppendCell(ParseSqrtTag(node));
      }
      else if (tagName == wxT("d"))
      {          // Differentials
        if (cell == NULL)
          cell = ParseDiffTag(node);
        else
          cell->AppendCell(ParseDiffTag(node));
      }
      else if (tagName == wxT("sm"))
      {         // Sums
        if (cell == NULL)
          cell = ParseSumTag(node);
        else
          cell->AppendCell(ParseSumTag(node));
      }
      else if (tagName == wxT("in"))
      {         // integrals
        if (cell == NULL)
          cell = ParseIntTag(node);
        else
          cell->AppendCell(ParseIntTag(node));
      }
      else if (tagName == wxT("mspace"))
      {
        if (cell == NULL)
          cell = new TextCell(wxT(" "));
        else
          cell->AppendCell(new TextCell(wxT(" ")));
      }
      else if (tagName == wxT("at"))
      {
        if (cell == NULL)
          cell = ParseAtTag(node);
        else
          cell->AppendCell(ParseAtTag(node));
      }
      else if (tagName == wxT("a"))
      {
        if (cell == NULL)
          cell = ParseAbsTag(node);
        else
          cell->AppendCell(ParseAbsTag(node));
      }
      else if (tagName == wxT("ie"))
      {
        if (cell == NULL)
          cell = ParseSubSupTag(node);
        else
          cell->AppendCell(ParseSubSupTag(node));
      }
      else if (tagName == wxT("lm"))
      {
        if (cell == NULL)
          cell = ParseLimitTag(node);
        else
          cell->AppendCell(ParseLimitTag(node));
      }
      else if (tagName == wxT("r"))
      {
        if (cell == NULL)
          cell = ParseTag(node->GetChildren());
        else
          cell->AppendCell(ParseTag(node->GetChildren()));
      }
      else if (tagName == wxT("tb"))
      {
        if (cell == NULL)
          cell = ParseTableTag(node);
        else
          cell->AppendCell(ParseTableTag(node));
      }
      else if ((tagName == wxT("mth")) || (tagName == wxT("line")))
      {
        MathCell *tmp = ParseTag(node->GetChildren());
        if (tmp != NULL)
          tmp->ForceBreakLine(true);
        else
          tmp = new TextCell(wxT(" "));
        if (cell == NULL)
          cell = tmp;
        else
          cell->AppendCell(tmp);
      }
      else if (tagName == wxT("lbl"))
      {
        MathCell* tmp = ParseText(node->GetChildren(), TS_LABEL);
        tmp->ForceBreakLine(true);
        if (cell == NULL)
          cell = tmp;
        else
          cell->AppendCell(tmp);
      }
      else if (tagName == wxT("st"))
      {
        MathCell* tmp = ParseText(node->GetChildren(), TS_STRING);
        if (cell == NULL)
          cell = tmp;
        else
          cell->AppendCell(tmp);
      }
      else if (tagName == wxT("hl"))
      {
        bool highlight = m_highlight;
        m_highlight = true;
        MathCell* tmp = ParseTag(node->GetChildren());
        m_highlight = highlight;
        if (cell == NULL)
          cell = tmp;
        else
          cell->AppendCell(tmp);
      }
      else if (tagName == wxT("img"))
      {
        wxString filename(node->GetChildren()->GetContent());
#if !wxUSE_UNICODE
        wxString filename1(filename.wc_str(wxConvUTF8), *wxConvCurrent);
        filename = filename1;
#endif

        ImgCell *tmp;

        if (m_fileSystem) // loading from zip
          tmp = new ImgCell(filename, false, m_fileSystem);
#if wxCHECK_VERSION(2,9,0)
        else if (node->GetAttribute(wxT("del"), wxT("yes")) != wxT("no"))
#else
        else if (node->GetPropVal(wxT("del"), wxT("yes")) != wxT("no"))
#endif
          tmp = new ImgCell(filename, true, NULL);
        else
          tmp = new ImgCell(filename, false, NULL);

#if wxCHECK_VERSION(2,9,0)
        if (node->GetAttribute(wxT("rect"), wxT("true")) == wxT("false"))
          tmp->DrawRectangle(false);
#else
        if (node->GetPropVal(wxT("rect"), wxT("true")) == wxT("false"))
          tmp->DrawRectangle(false);
#endif

        if (cell == NULL)
          cell = tmp;
        else
          cell->AppendCell(tmp);
      }
      else if (tagName == wxT("slide"))
      {
        SlideShow *tmp = new SlideShow(m_fileSystem);
        wxString str(node->GetChildren()->GetContent());
        wxArrayString images;
        wxStringTokenizer tokens(str, wxT(";"));
        while (tokens.HasMoreTokens()) {
          wxString token = tokens.GetNextToken();
          if (token.Length())
          {
#if !wxUSE_UNICODE
            wxString token1(token.wc_str(wxConvUTF8), *wxConvCurrent);
            token = token1;
#endif
            images.Add(token);
          }
        }
        tmp->LoadImages(images);
        if (cell == NULL)
          cell = tmp;
        else
          cell->AppendCell(tmp);
      }
      else if (tagName == wxT("editor"))
      {
        if (cell == NULL)
          cell = ParseEditorTag(node);
        else
          cell->AppendCell(ParseEditorTag(node));
      }
      else if (tagName == wxT("cell"))
      {
        if (cell == NULL)
          cell = ParseCellTag(node);
        else
          cell->AppendCell(ParseCellTag(node));
      }
      else if (tagName == wxT("ascii"))
      {
        if (cell == NULL)
          cell = ParseCharCode(node->GetChildren());
        else
          cell->AppendCell(ParseCharCode(node->GetChildren()));
      }
      else if (node->GetChildren())
      {
        if (cell == NULL)
          cell = ParseTag(node->GetChildren());
        else
          cell->AppendCell(ParseTag(node->GetChildren()));
      }
    }
    // Parse text
    else
    {
      if (cell == NULL)
        cell = ParseText(node);
      else
        cell->AppendCell(ParseText(node));
    }
    if (!all)
      break;

    if (cell != NULL)
    {
      if (tmp == NULL)
        tmp = cell;
      else
        cell = cell->m_next;
    }
    else if (warning)
    {
      wxMessageBox(_("Parts of the document will not be loaded correctly!"), _("Warning"),
        wxOK | wxICON_WARNING);
      warning = false;
    }

#if wxCHECK_VERSION(2,9,0)
    if (node->GetAttribute(wxT("altCopy"), &altCopy))
      cell->SetAltCopyText(altCopy);
#else
    if (node->GetPropVal(wxT("altCopy"), &altCopy))
      cell->SetAltCopyText(altCopy);
#endif

    node = node->GetNext();
  }

  if (tmp != NULL)
    return tmp;
  return cell;
}

/***
 * Parse the string s, which is (correct) xml fragment.
 * Put the result in line.
 */
MathCell* MathParser::ParseLine(wxString s, int style)
{
  m_ParserStyle = style;
  m_FracStyle = FC_NORMAL;
  m_highlight = false;
  MathCell* cell = NULL;

  wxConfigBase* config = wxConfig::Get();
  bool showLong = false;
  config->Read(wxT("showLong"), &showLong);

  wxRegEx graph(wxT("[[:cntrl:]]"));

#if wxUSE_UNICODE
  graph.Replace(&s, wxT("\xFFFD"));
#else
  graph.Replace(&s, wxT("?"));
#endif

  if (s.Length() < MAXLENGTH || showLong)
  {

    wxXmlDocument xml;

#if wxUSE_UNICODE
    wxStringInputStream xmlStream(s);
#else
    wxString su(s.wc_str(*wxConvCurrent), wxConvUTF8);
    wxStringInputStream xmlStream(su);
#endif

    xml.Load(xmlStream);

    wxXmlNode *doc = xml.GetRoot();

    if (doc != NULL)
      cell = ParseTag(doc->GetChildren());
  }
  else
  {
    cell = new TextCell(_(" << Expression too long to display! >>"));
    cell->ForceBreakLine(true);
  }
  return cell;
}
