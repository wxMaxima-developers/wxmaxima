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
#include <wx/intl.h>

#include "MathParser.h"

#include "Version.h"
#include "ExptCell.h"
#include "SubCell.h"
#include "SqrtCell.h"
#include "LimitCell.h"
#include "ListCell.h"
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
#include "VisiblyInvalidCell.h"
#include "SlideShowCell.h"

/*! Calls a member function from a function pointer

  \todo Replace this by a C++17 construct when we switch to C++17
 */
#define CALL_MEMBER_FN(object, ptrToMember)  ((object).*(ptrToMember))

template <typename Cell>
void ParseAltCopyAttr(const wxXmlNode *node, Cell *cell)
{
  if (!node || !cell)
    return;
  wxString val;
  if(node->GetAttribute(wxT("altCopy"), &val))
    cell->SetAltCopyText(val);
}

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

MathParser::MathParser(Configuration **cfg, const wxString &zipfile)
{
  // We cannot do this at the startup of the program as we first need to wait
  // for the language selection to take place
  if(m_unknownXMLTagToolTip.IsEmpty())
    m_unknownXMLTagToolTip = _("Encountered an unknown XML tag: <%s>");
  m_configuration = cfg;
  m_ParserStyle = MC_TYPE_DEFAULT;
  m_FracStyle = FracCell::FC_NORMAL;
  if(m_innerTags.empty())
  {
    m_innerTags[wxT("v")] = &MathParser::ParseVariableNameTag;
    m_innerTags[wxT("mi")] = &MathParser::ParseVariableNameTag;
    m_innerTags[wxT("mo")] = &MathParser::ParseOperatorNameTag;
    m_innerTags[wxT("t")] = &MathParser::ParseMiscTextTag;
    m_innerTags[wxT("n")] = &MathParser::ParseNumberTag;
    m_innerTags[wxT("mn")] = &MathParser::ParseNumberTag;
    m_innerTags[wxT("p")] = &MathParser::ParseParenTag;
    m_innerTags[wxT("f")] = &MathParser::ParseFracTag;
    m_innerTags[wxT("mfrac")] = &MathParser::ParseFracTag;
    m_innerTags[wxT("e")] = &MathParser::ParseSupTag;
    m_innerTags[wxT("msup")] = &MathParser::ParseSupTag;
    m_innerTags[wxT("i")] = &MathParser::ParseSubTag;
    m_innerTags[wxT("munder")] = &MathParser::ParseSubTag;
    m_innerTags[wxT("fn")] = &MathParser::ParseFunTag;
    m_innerTags[wxT("g")] = &MathParser::ParseGreekTag;
    m_innerTags[wxT("s")] = &MathParser::ParseSpecialConstantTag;
    m_innerTags[wxT("fnm")] = &MathParser::ParseFunctionNameTag;
    m_innerTags[wxT("q")] = &MathParser::ParseSqrtTag;
    m_innerTags[wxT("d")] = &MathParser::ParseDiffTag;
    m_innerTags[wxT("sm")] = &MathParser::ParseSumTag;
    m_innerTags[wxT("in")] = &MathParser::ParseIntTag;
    m_innerTags[wxT("mspace")] = &MathParser::ParseSpaceTag;
    m_innerTags[wxT("at")] = &MathParser::ParseAtTag;
    m_innerTags[wxT("a")] = &MathParser::ParseAbsTag;
    m_innerTags[wxT("cj")] = &MathParser::ParseConjugateTag;
    m_innerTags[wxT("ie")] = &MathParser::ParseSubSupTag;
    m_innerTags[wxT("mmultiscripts")] = &MathParser::ParseMmultiscriptsTag;
    m_innerTags[wxT("lm")] = &MathParser::ParseLimitTag;
    m_innerTags[wxT("r")] = &MathParser::ParseRowTag;
    m_innerTags[wxT("mrow")] = &MathParser::ParseRowTag;
    m_innerTags[wxT("tb")] = &MathParser::ParseTableTag;
    m_innerTags[wxT("mth")] = &MathParser::ParseMthTag;
    m_innerTags[wxT("line")] = &MathParser::ParseMthTag;
    m_innerTags[wxT("lbl")] = &MathParser::ParseOutputLabelTag;
    m_innerTags[wxT("st")] = &MathParser::ParseStringTag;
    m_innerTags[wxT("hl")] = &MathParser::ParseHighlightTag;
    m_innerTags[wxT("h")] = &MathParser::ParseHiddenOperatorTag;
    m_innerTags[wxT("img")] = &MathParser::ParseImageTag;
    m_innerTags[wxT("slide")] = &MathParser::ParseSlideshowTag;
    m_innerTags[wxT("editor")] = &MathParser::ParseEditorTag;
    m_innerTags[wxT("cell")] = &MathParser::ParseCellTag;
    m_innerTags[wxT("ascii")] = &MathParser::ParseCharCode;
    m_innerTags[wxT("output")] = &MathParser::ParseOutputTag;
    m_innerTags[wxT("mtd")] = &MathParser::ParseMtdTag;
    m_innerTags[wxT("math")] = &MathParser::ParseMthTag;
  }
  if(m_groupTags.empty())
  {
    m_groupTags[wxT("code")] = &MathParser::GroupCellFromCodeTag;
    m_groupTags[wxT("image")] = &MathParser::GroupCellFromImageTag;
    m_groupTags[wxT("pagebreak")] = &MathParser::GroupCellFromPagebreakTag;
    m_groupTags[wxT("text")] = &MathParser::GroupCellFromTextTag;
    m_groupTags[wxT("title")] = &MathParser::GroupCellFromTitleTag;
    m_groupTags[wxT("section")] = &MathParser::GroupCellFromSectionTag;
    m_groupTags[wxT("subsection")] = &MathParser::GroupCellFromSubsectionTag;
    m_groupTags[wxT("subsubsection")] = &MathParser::GroupCellFromSubsubsectionTag;
    m_groupTags[wxT("heading5")] = &MathParser::GroupCellHeading5Tag;
    m_groupTags[wxT("heading6")] = &MathParser::GroupCellHeading6Tag;
  }
  m_highlight = false;
  if (zipfile.Length() > 0)
  {
    m_fileSystem = std::unique_ptr<wxFileSystem>(new wxFileSystem());
    m_fileSystem->ChangePathTo(zipfile + wxT("#zip:/"), true);
  }
}

MathParser::~MathParser()
{}

Cell *MathParser::ParseHiddenOperatorTag(wxXmlNode *node)
{
  Cell *retval = ParseText(node->GetChildren());
  retval->SetHidableMultSign(true);
  return retval;
}

Cell *MathParser::ParseOutputTag(wxXmlNode *node)
{
  Cell *retval = NULL;
  wxXmlNode *children = node->GetChildren();
  if(children)
    retval = ParseTag(children);
  return retval;
}

Cell *MathParser::ParseMtdTag(wxXmlNode *node)
{
  Cell *retval = NULL;
  wxXmlNode *children = node->GetChildren();
  if(children)
    retval = ParseTag(children);
  return retval;
}

Cell *MathParser::ParseRowTag(wxXmlNode *node)
{
  if (node->GetAttribute(wxT("list")) == wxT("true"))
  {
    wxXmlNode *child = node->GetChildren();
    child = SkipWhitespaceNode(child);
    ListCell *cell = new ListCell(NULL, m_configuration);
    // No special Handling for NULL args here: They are completely legal in this case.
    cell->SetInner(ParseTag(child, true), m_ParserStyle);
    cell->SetHighlight(m_highlight);
    cell->SetStyle(TS_VARIABLE);
    ParseCommonAttrs(node, cell);
    return cell;
  }
  else
  {
    Cell *tmp = NULL;
    if((node != NULL) && (node->GetChildren() != NULL))
      tmp = ParseTag(node->GetChildren(), true);
    return tmp;
  }
}

Cell *MathParser::ParseHighlightTag(wxXmlNode *node)
{
  Cell *tmp;
  bool highlight = m_highlight;
  m_highlight = true;
  tmp = ParseTag(node->GetChildren());
  m_highlight = highlight;
  return tmp;
}

Cell *MathParser::ParseMiscTextTag(wxXmlNode *node)
{
  if (node->GetAttribute(wxT("listdelim")) == wxT("true"))
    return NULL;
  else
  {
    TextStyle style = TS_DEFAULT;
    if (node->GetAttribute(wxT("type")) == wxT("error"))
      style = TS_ERROR;
    if (node->GetAttribute(wxT("type")) == wxT("warning"))
      style = TS_WARNING;
    return ParseText(node->GetChildren(), style);
  }
}

Cell *MathParser::ParseSlideshowTag(wxXmlNode *node)
{
  wxString gnuplotSources;
  wxString gnuplotData;
  bool del = node->GetAttribute(wxT("del"), wxT("false")) == wxT("true");
  node->GetAttribute(wxT("gnuplotSources"), &gnuplotSources);
  node->GetAttribute(wxT("gnuplotData"), &gnuplotData);
  SlideShow *slideShow = new SlideShow(NULL, m_configuration, m_fileSystem);
  wxString str(node->GetChildren()->GetContent());
  wxArrayString images;
  wxString framerate;
  if (node->GetAttribute(wxT("fr"), &framerate))
  {
    long fr;
    if (framerate.ToLong(&fr))
      slideShow->SetFrameRate(fr);
  }
  if (node->GetAttribute(wxT("frame"), &framerate))
  {
    long frame;
    if (framerate.ToLong(&frame))
      slideShow->SetDisplayedIndex(frame);
  }
  if (node->GetAttribute(wxT("running"), wxT("true")) == wxT("false"))
    slideShow->AnimationRunning(false);
  wxStringTokenizer imageFiles(str, wxT(";"));
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

  slideShow->LoadImages(images, del);
  wxStringTokenizer dataFiles(gnuplotData, wxT(";"));
  wxStringTokenizer gnuplotFiles(gnuplotSources, wxT(";"));
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

  return slideShow;
}

Cell *MathParser::ParseImageTag(wxXmlNode *node)
{
  ImgCell *imageCell = {};
  wxString filename(node->GetChildren()->GetContent());

  if (m_fileSystem) // loading from zip
    imageCell = new ImgCell(NULL, m_configuration, filename, m_fileSystem, false);
  else
  {
    if (node->GetAttribute(wxT("del"), wxT("yes")) != wxT("no"))
    {
      if (wxImage::GetImageCount(filename) < 2)
        imageCell = new ImgCell(NULL, m_configuration, filename, {}/*system fs*/, true);
      else
        return new SlideShow(NULL, m_configuration, filename, true);
    }
    else
    {
      // This is the only case show_image() produces ergo this is the only
      // case we might get a local path
      if (
        (!wxFileExists(filename)) &&
        (wxFileExists((*m_configuration)->GetWorkingDirectory() + wxT("/") + filename))
        )
        filename = (*m_configuration)->GetWorkingDirectory() + wxT("/") + filename;
      if (wxImage::GetImageCount(filename) < 2)
        imageCell = new ImgCell(NULL, m_configuration, filename, {}/* system fs */, false);
      else
        return new SlideShow(NULL, m_configuration, filename, false);
    }
  }
  wxString gnuplotSource = node->GetAttribute(wxT("gnuplotsource"), wxEmptyString);
  wxString gnuplotData = node->GetAttribute(wxT("gnuplotdata"), wxEmptyString);

  if (!gnuplotSource.empty())
    imageCell->GnuplotSource(gnuplotSource, gnuplotData, m_fileSystem);

  if (node->GetAttribute(wxT("rect"), wxT("true")) == wxT("false"))
    imageCell->DrawRectangle(false);
  wxString sizeString;
  if ((sizeString = node->GetAttribute(wxT("maxWidth"), wxT("-1"))) != wxT("-1"))
  {
    double width;
    if (sizeString.ToDouble(&width))
      imageCell->SetMaxWidth(width);
  }
  if ((sizeString = node->GetAttribute(wxT("maxHeight"), wxT("-1"))) != wxT("-1"))
  {
    double height;
    if (sizeString.ToDouble(&height))
      imageCell->SetMaxHeight(height);
  }
  return imageCell;
}

Cell *MathParser::ParseOutputLabelTag(wxXmlNode *node)
{
  Cell *tmp = {};
  wxString user_lbl = node->GetAttribute(wxT("userdefinedlabel"), m_userDefinedLabel);
  wxString userdefined = node->GetAttribute(wxT("userdefined"), wxT("no"));
  
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
  return tmp;
}


Cell *MathParser::ParseMthTag(wxXmlNode *node)
{
  Cell *retval;
  retval = ParseTag(node->GetChildren());
  if (retval != NULL)
    retval->ForceBreakLine(true);
  else
    retval = new TextCell(NULL, m_configuration, wxT(" "));
  return retval;
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
  bool hide = (node->GetAttribute(wxT("hide"), wxT("false")) == wxT("true")) ? true : false;
  // read (group)cell type
  wxString type = node->GetAttribute(wxT("type"), wxT("text"));

  GroupCell * (MathParser::* function)(wxXmlNode *node) = m_groupTags[type];
  if (function != NULL)
    group =  CALL_MEMBER_FN(*this,function)(node);
  else  
    return group;
  
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
    { // This GroupCell contains folded groupcells
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
    else if (children->GetName() == wxT("input"))
    {
      std::unique_ptr<Cell> editor(ParseTag(children->GetChildren()));
      if (editor == NULL)
        editor = std::unique_ptr<Cell>(
          new EditorCell(group, m_configuration, _("Bug: Missing contents")));
      if(editor)
        group->SetEditableContent(editor->GetValue());
    }
    else
    {
      group->AppendOutput(HandleNullPointer(ParseTag(children)));
    }

    children = GetNextTag(children);
  }

  group->SetGroup(group); //-V678
  group->Hide(hide);
  return group;
}

GroupCell *MathParser::GroupCellFromSubsectionTag(wxXmlNode *node)
{
  wxString sectioning_level = node->GetAttribute(wxT("sectioning_level"), wxT("0"));
  GroupCell *group = NULL;
  // We save subsubsections as subsections with a higher sectioning level:
  // This makes them backwards-compatible in the way that they are displayed
  // as subsections on old wxMaxima installations.
  // A sectioning level of the value 0 means that the file is too old to
  // provide a sectioning level.
  if ((sectioning_level == wxT("0")) || (sectioning_level == wxT("3")))
    group = new GroupCell(m_configuration, GC_TYPE_SUBSECTION);
  if (sectioning_level == wxT("4"))
    group = new GroupCell(m_configuration, GC_TYPE_SUBSUBSECTION); //-V773
  if (sectioning_level == wxT("5"))
    group = new GroupCell(m_configuration, GC_TYPE_HEADING5); //-V773
  if (group == NULL)
    group = new GroupCell(m_configuration, GC_TYPE_HEADING6);
  ParseCommonGroupCellAttrs(node, group);
  return group;
}

GroupCell *MathParser::GroupCellFromImageTag(wxXmlNode *node)
{
  GroupCell *group = new GroupCell(m_configuration, GC_TYPE_IMAGE);
  ParseCommonGroupCellAttrs(node, group);
  return group;
}

GroupCell *MathParser::GroupCellFromCodeTag(wxXmlNode *node)
{
  GroupCell *group = new GroupCell(m_configuration, GC_TYPE_CODE);
  wxString isAutoAnswer = node->GetAttribute(wxT("auto_answer"), wxT("no"));
  if(isAutoAnswer == wxT("yes"))
    group->AutoAnswer(true);
  int i = 1;
  wxString answer;
  wxString question;
  while (node->GetAttribute(wxString::Format(wxT("answer%i"),i),&answer))
  {
    if(node->GetAttribute(wxString::Format(wxT("question%i"),i),&question))
      group->SetAnswer(question,answer);
    else
      group->SetAnswer(wxString::Format(wxT("Question #%i"),i),answer);
    i++;
  }
  ParseCommonGroupCellAttrs(node, group);
  return group;
}


Cell *MathParser::HandleNullPointer(Cell *cell)
{
  if (cell == NULL)
  {
    cell = new VisiblyInvalidCell(NULL, m_configuration);
    cell->SetToolTip(_("The xml data from maxima or from the .wxmx file was missing data here.\n"
                       "If you find a way how to reproduce this problem please file a bug "
                       "report against wxMaxima."));
    cell->SetStyle(TS_ERROR);
  }
  return (cell);
}

Cell *MathParser::ParseEditorTag(wxXmlNode *node)
{
  EditorCell *editor = new EditorCell(NULL, m_configuration);
  wxString type = node->GetAttribute(wxT("type"), wxT("input"));
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
        text += wxT("\n");
      text += line->GetNodeContent();
    }
    line = line->GetNext();
  } // end while
  editor->SetValue(text);
  return editor;
}

Cell *MathParser::ParseFracTag(wxXmlNode *node)
{
  FracCell *frac = new FracCell(NULL, m_configuration);
  frac->SetFracStyle(m_FracStyle);
  frac->SetHighlight(m_highlight);
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);
  frac->SetNum(HandleNullPointer(ParseTag(child, false)));
  child = GetNextTag(child);
  frac->SetDenom(HandleNullPointer(ParseTag(child, false)));
  
  if (node->GetAttribute(wxT("line")) == wxT("no"))
    frac->SetFracStyle(FracCell::FC_CHOOSE);
  if (node->GetAttribute(wxT("diffstyle")) == wxT("yes"))
    frac->SetFracStyle(FracCell::FC_DIFF);
  frac->SetType(m_ParserStyle);
  frac->SetStyle(TS_VARIABLE);
  frac->SetupBreakUps();
  ParseCommonAttrs(node, frac);
  return frac;
}

Cell *MathParser::ParseDiffTag(wxXmlNode *node)
{
  DiffCell *diff = new DiffCell(NULL, m_configuration);
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
  ExptCell *expt = new ExptCell(NULL, m_configuration);
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
  ParseAltCopyAttr(node, expt);
  if(node->GetAttribute(wxT("mat"), wxT("false")) == wxT("true"))
    expt->SetAltCopyText(baseCell->ToString()+wxT("^^")+power->ToString());

  return expt;
}

Cell *MathParser::ParseSubSupTag(wxXmlNode *node)
{
  SubSupCell *subsup = new SubSupCell(NULL, m_configuration);
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
    ParseAltCopyAttr(node, subsup);
  }
  return subsup;
}

Cell *MathParser::ParseMmultiscriptsTag(wxXmlNode *node)
{
  bool pre = false;
  bool subscript = true;
  SubSupCell *subsup = new SubSupCell(NULL, m_configuration);
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
  SubCell *sub = new SubCell(NULL, m_configuration);
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
  ParseAltCopyAttr(node, sub);
  return sub;
}

Cell *MathParser::ParseAtTag(wxXmlNode *node)
{
  AtCell *at = new AtCell(NULL, m_configuration);
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
  FunCell *fun = new FunCell(NULL, m_configuration);
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);

  fun->SetName(HandleNullPointer(ParseTag(child, false)));
  child = GetNextTag(child);
  fun->SetType(m_ParserStyle);
  fun->SetStyle(TS_FUNCTION);
  fun->SetArg(HandleNullPointer(ParseTag(child, false)));
  ParseCommonAttrs(node, fun);
  ParseAltCopyAttr(node, fun);
  if (fun->ToString().Contains(")("))
    fun->SetToolTip(_("If this isn't a function returning a lambda() expression a multiplication sign (*) between closing and opening parenthesis is missing here."));
  return fun;
}

Cell *MathParser::ParseText(wxXmlNode *node, TextStyle style)
{
  wxString str;
  TextCell *retval = NULL;
  if ((node != NULL) && ((str = node->GetContent()) != wxEmptyString))
  {
    str.Replace(wxT("-"), wxT("\u2212")); // unicode minus sign

    wxStringTokenizer lines(str, wxT('\n'));
    while (lines.HasMoreTokens())
    {
      TextCell *cell = new TextCell(NULL, m_configuration);
      switch(style)
      {
      case TS_ERROR:
        cell->SetType(MC_TYPE_ERROR);
        break;

      case TS_WARNING:
        cell->SetType(MC_TYPE_WARNING);
        break;

      case TS_LABEL:
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
    retval = new TextCell(NULL, m_configuration);

  ParseCommonAttrs(node, retval);
  ParseAltCopyAttr(node, retval);
  return retval;
}

void MathParser::ParseCommonAttrs(wxXmlNode *node, Cell *cell)
{
  if(cell == NULL)
    return;
  if(node == NULL)
    return;

  if(node->GetAttribute(wxT("breakline"), wxT("false")) == wxT("true"))
    cell->ForceBreakLine(true);

  wxString val;
  
  if(node->GetAttribute(wxT("tooltip"), &val))
    cell->SetToolTip(val);
}

void MathParser::ParseCommonGroupCellAttrs(wxXmlNode *node, GroupCell *group)
{
  if(group == NULL)
    return;
  if(node == NULL)
    return;

  if(node->GetAttribute(wxT("hideToolTip")) == wxT("true"))
    group->SetSuppressTooltipMarker(true);
}

Cell *MathParser::ParseCharCode(wxXmlNode *node)
{
  TextCell *cell = new TextCell(NULL, m_configuration);
  wxString str;
  if ((node != NULL) && ((str = node->GetContent()) != wxEmptyString))
  {
    long code;
    if (str.ToLong(&code))
      str = wxString::Format(wxT("%c"), code);
    cell->SetValue(str);
    cell->SetType(m_ParserStyle);
    cell->SetStyle(TS_DEFAULT);
    cell->SetHighlight(m_highlight);
  }
  ParseCommonAttrs(node, cell);
  return cell;
}

Cell *MathParser::ParseSqrtTag(wxXmlNode *node)
{
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);

  SqrtCell *cell = new SqrtCell(NULL, m_configuration);

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
  AbsCell *cell = new AbsCell(NULL, m_configuration);
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
  ConjugateCell *cell = new ConjugateCell(NULL, m_configuration);
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
  ParenCell *cell = new ParenCell(NULL, m_configuration);
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
  LimitCell *limit = new LimitCell(NULL, m_configuration);
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
  SumCell *sum = new SumCell(NULL, m_configuration);
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);
  wxString type = node->GetAttribute(wxT("type"), wxT("sum"));

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
  ParseAltCopyAttr(node, sum);
  return sum;
}

Cell *MathParser::ParseIntTag(wxXmlNode *node)
{
  IntCell *in = new IntCell(NULL, m_configuration);
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);
  in->SetHighlight(m_highlight);
  wxString definiteAtt = node->GetAttribute(wxT("def"), wxT("true"));
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
  MatrCell *matrix = new MatrCell(NULL, m_configuration);
  matrix->SetHighlight(m_highlight);

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
  if (node->GetAttribute(wxT("roundedParens"), wxT("false")) == wxT("true"))
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

Cell *MathParser::ParseTag(wxXmlNode *node, bool all)
{
  Cell *retval = NULL;
  Cell *cell = NULL;
  bool warning = all;

  node = SkipWhitespaceNode(node);

  while (node)
  {
    if (node->GetType() == wxXML_ELEMENT_NODE)
    {
      // Parse XML tags. The only other type of element we recognize are text
      // nodes.
      wxString tagName(node->GetName());

      Cell *tmp = NULL;

      Cell * (MathParser::* function)(wxXmlNode *node) = m_innerTags[tagName];
      if (function != NULL)
        tmp =  CALL_MEMBER_FN(*this, function)(node);
//      if ((tmp == NULL) && (node->GetChildren()))
//        tmp = ParseTag(node->GetChildren());

      if((tmp == NULL) && ((node->GetAttribute(wxT("listdelim")) != wxT("true"))))
        tmp = new VisiblyInvalidCell(NULL, m_configuration,
                                     wxString::Format(m_unknownXMLTagToolTip, tagName.utf8_str()));

      if(tmp != NULL)
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
      if (cell == NULL)
        cell = ParseText(node);
      else
        cell->AppendCell(ParseText(node));
    }

    if (cell != NULL)
    {
      // Append the new cell to the return value
      if (retval == NULL)
        retval = cell;
      else
        cell = cell->m_next;
    }
    else if ((warning) && (!all))
    {
      // Tell the user we ran into problems.
      wxString name;
      name.Trim(true);
      name.Trim(false);
      if (cell != NULL) name = cell->ToString();
      if (name.Length() != 0)
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

  m_graphRegex.Replace(&s, wxT("\uFFFD"));

  if (((long) s.Length() < showLength) || (showLength == 0))
  {

    wxXmlDocument xml;

    wxStringInputStream xmlStream(s);

    xml.Load(xmlStream, wxT("UTF-8"), wxXMLDOC_KEEP_WHITESPACE_NODES);

    wxXmlNode *doc = xml.GetRoot();

    if (doc != NULL)
      cell = ParseTag(doc->GetChildren());
  }
  else
  {
    cell = new TextCell(NULL, m_configuration,
                        _("(Expression longer than allowed by the configuration setting)"),
      TS_WARNING);
    cell->SetToolTip(_("The maximum size of the expressions wxMaxima is allowed to display "
                       "can be changed in the configuration dialogue."
                       ));
    cell->ForceBreakLine(true);
  }
  return cell;
}

wxRegEx MathParser::m_graphRegex(wxT("[[:cntrl:]]"));
MathParser::MathCellFunctionHash MathParser::m_innerTags;
MathParser::GroupCellFunctionHash MathParser::m_groupTags;
wxString MathParser::m_unknownXMLTagToolTip;
