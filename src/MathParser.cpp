// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
//            (C) 2020      Kuba Ober <kuba@bertec.com>
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
#include "CellList.h"
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
#include "LabelCell.h"
#include "LongNumberCell.h"
#include "SubSupCell.h"
#include "StringUtils.h"
#include "VisiblyInvalidCell.h"
#include "SlideShowCell.h"

/*! Calls a member function from a function pointer

  \todo Replace this by a C++17 construct when we switch to C++17
 */
#define CALL_MEMBER_FN(object, ptrToMember)  ((object).*(ptrToMember))

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

std::unique_ptr<Cell> MathParser::ParseHiddenOperatorTag(wxXmlNode *node)
{
  auto retval = ParseText(node->GetChildren());
  retval->SetHidableMultSign(true);
  return retval;
}

std::unique_ptr<Cell> MathParser::ParseOutputTag(wxXmlNode *node)
{
  wxXmlNode *children = node->GetChildren();
  return children ? ParseTag(children) : nullptr;
}

std::unique_ptr<Cell> MathParser::ParseMtdTag(wxXmlNode *node)
{
  wxXmlNode *children = node->GetChildren();
  return children ? ParseTag(children) : nullptr;
}

std::unique_ptr<Cell> MathParser::ParseRowTag(wxXmlNode *node)
{
  if (node->GetAttribute(wxT("list")) == wxT("true"))
  {
    wxXmlNode *child = node->GetChildren();
    child = SkipWhitespaceNode(child);
    // No special Handling for NULL args here: They are completely legal in this case.
    auto inner = ParseTag(child, true);
    auto cell = std::make_unique<ListCell>(nullptr, m_configuration, std::move(inner));
    cell->SetType(m_ParserStyle);
    cell->SetHighlight(m_highlight);
    ParseCommonAttrs(node, cell);
    return cell;
  }
  else
  {
    if (node && node->GetChildren())
      return ParseTag(node->GetChildren(), true);
    return nullptr;
  }
}

std::unique_ptr<Cell> MathParser::ParseHighlightTag(wxXmlNode *node)
{
  bool highlight = m_highlight;
  m_highlight = true;
  auto tmp = ParseTag(node->GetChildren());
  m_highlight = highlight;
  return tmp;
}

std::unique_ptr<Cell> MathParser::ParseMiscTextTag(wxXmlNode *node)
{
  if (node->GetAttribute(wxT("listdelim")) == wxT("true"))
    return {};
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

std::unique_ptr<Cell> MathParser::ParseSlideshowTag(wxXmlNode *node)
{
  wxString gnuplotSources;
  wxString gnuplotData;
  bool del = node->GetAttribute(wxT("del"), wxT("false")) == wxT("true");
  node->GetAttribute(wxT("gnuplotSources"), &gnuplotSources);
  node->GetAttribute(wxT("gnuplotData"), &gnuplotData);
  auto slideShow = std::make_unique<SlideShow>(nullptr, m_configuration, m_fileSystem);
  auto const &str = node->GetChildren()->GetContent();
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

std::unique_ptr<Cell> MathParser::ParseImageTag(wxXmlNode *node)
{
  std::unique_ptr<ImgCell> imageCell;
  wxString filename(node->GetChildren()->GetContent());

  if (m_fileSystem) // loading from zip
    imageCell = std::make_unique<ImgCell>(nullptr, m_configuration, filename, m_fileSystem, false);
  else
  {
    std::shared_ptr<wxFileSystem> system_fs = {};
    if (node->GetAttribute(wxT("del"), wxT("yes")) != wxT("no"))
    {
      if (wxImage::GetImageCount(filename) < 2)
        imageCell = std::make_unique<ImgCell>(nullptr, m_configuration, filename, system_fs, true);
      else
        return std::make_unique<SlideShow>(nullptr, m_configuration, filename, true);
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
        imageCell = std::make_unique<ImgCell>(nullptr, m_configuration, filename, system_fs, false);
      else
        return std::make_unique<SlideShow>(nullptr, m_configuration, filename, false);
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

std::unique_ptr<Cell> MathParser::ParseOutputLabelTag(wxXmlNode *node)
{
  std::unique_ptr<Cell> tmp;
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
      user_lbl = tmp->GetValue();
      user_lbl = user_lbl.substr(1,user_lbl.Length() - 2);
    }
  }
  
  dynamic_cast<LabelCell *>(tmp.get())->SetUserDefinedLabel(user_lbl);
  tmp->ForceBreakLine(true);
  return tmp;
}

std::unique_ptr<Cell> MathParser::ParseMthTag(wxXmlNode *node)
{
  auto retval = ParseTag(node->GetChildren());
  if (retval)
    retval->ForceBreakLine(true);
  else
    retval = std::make_unique<TextCell>(nullptr, m_configuration, S_(" "));
  return retval;
}

std::unique_ptr<Cell> MathParser::ParseStringTag(wxXmlNode *node)
{
  return ParseText(node->GetChildren(), TS_STRING);
}

// ParseCellTag
// This function is responsible for creating
// a tree of groupcells when loading XML document.
// Any changes in GroupCell structure or methods
// has to be reflected here in order to ensure proper
// loading of WXMX files.
std::unique_ptr<Cell> MathParser::ParseCellTag(wxXmlNode *node)
{
  std::unique_ptr<GroupCell> group;

  // read hide status
  bool hide = (node->GetAttribute(wxT("hide"), wxT("false")) == wxT("true")) ? true : false;
  // read (group)cell type
  wxString type = node->GetAttribute(wxT("type"), wxT("text"));

  auto function = m_groupTags[type];
  if (function)
    group = std::unique_ptr<GroupCell>(CALL_MEMBER_FN(*this, function)(node));
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
      CellListBuilder<GroupCell> tree;
      wxXmlNode *xmlcells = children->GetChildren();
      xmlcells = SkipWhitespaceNode(xmlcells);
      for (; xmlcells; xmlcells = GetNextTag(xmlcells))
        tree.DynamicAppend(ParseTag(xmlcells, false));

      if (tree)
        group->HideTree(std::move(tree));
    }
    else if (children->GetName() == wxT("input"))
    {
      auto editor = ParseTag(children->GetChildren());
      if (!editor)
        editor = std::make_unique<EditorCell>(group.get(), m_configuration, _("Bug: Missing contents"));
      if (editor)
        group->SetEditableContent(editor->GetValue());
    }
    else
    {
      group->AppendOutput(HandleNullPointer(ParseTag(children)));
    }

    children = GetNextTag(children);
  }

  group->SetGroup(group.get()); //-V678
  group->Hide(hide);
  return group;
}

std::unique_ptr<GroupCell> MathParser::GroupCellFromSubsectionTag(wxXmlNode *node)
{
  wxString sectioning_level = node->GetAttribute(wxT("sectioning_level"), wxT("0"));
  std::unique_ptr<GroupCell> group;
  // We save subsubsections as subsections with a higher sectioning level:
  // This makes them backwards-compatible in the way that they are displayed
  // as subsections on old wxMaxima installations.
  // A sectioning level of the value 0 means that the file is too old to
  // provide a sectioning level.
  if ((sectioning_level == wxT("0")) || (sectioning_level == wxT("3")))
    group = std::make_unique<GroupCell>(m_configuration, GC_TYPE_SUBSECTION);
  if (sectioning_level == wxT("4"))
    group = std::make_unique<GroupCell>(m_configuration, GC_TYPE_SUBSUBSECTION); //-V773
  if (sectioning_level == wxT("5"))
    group = std::make_unique<GroupCell>(m_configuration, GC_TYPE_HEADING5); //-V773
  if (group == NULL)
    group = std::make_unique<GroupCell>(m_configuration, GC_TYPE_HEADING6);
  ParseCommonGroupCellAttrs(node, group);
  return group;
}

std::unique_ptr<GroupCell> MathParser::GroupCellFromTextTag(wxXmlNode *WXUNUSED(node))
{
  return std::make_unique<GroupCell>(m_configuration, GC_TYPE_TEXT);
}

std::unique_ptr<GroupCell> MathParser::GroupCellHeading6Tag(wxXmlNode *WXUNUSED(node))
{
  return std::make_unique<GroupCell>(m_configuration, GC_TYPE_HEADING6);
}

std::unique_ptr<GroupCell> MathParser::GroupCellHeading5Tag(wxXmlNode *WXUNUSED(node))
{
  return std::make_unique<GroupCell>(m_configuration, GC_TYPE_HEADING5);
}

std::unique_ptr<GroupCell> MathParser::GroupCellFromSubsubsectionTag(wxXmlNode *WXUNUSED(node))
{
  return std::make_unique<GroupCell>(m_configuration, GC_TYPE_SUBSUBSECTION);
}

std::unique_ptr<GroupCell> MathParser::GroupCellFromImageTag(wxXmlNode *node)
{
  auto group = std::make_unique<GroupCell>(m_configuration, GC_TYPE_IMAGE);
  ParseCommonGroupCellAttrs(node, group);
  return group;
}

std::unique_ptr<GroupCell> MathParser::GroupCellFromPagebreakTag(wxXmlNode *WXUNUSED(node))
{
  return std::make_unique<GroupCell>(m_configuration, GC_TYPE_PAGEBREAK);
}

std::unique_ptr<GroupCell> MathParser::GroupCellFromSectionTag(wxXmlNode *WXUNUSED(node))
{
  return std::make_unique<GroupCell>(m_configuration, GC_TYPE_SECTION);
}

std::unique_ptr<GroupCell> MathParser::GroupCellFromTitleTag(wxXmlNode *WXUNUSED(node))
{
  return std::make_unique<GroupCell>(m_configuration, GC_TYPE_TITLE);
}

std::unique_ptr<GroupCell> MathParser::GroupCellFromCodeTag(wxXmlNode *node)
{
  auto group = std::make_unique<GroupCell>(m_configuration, GC_TYPE_CODE);
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

std::unique_ptr<Cell> MathParser::HandleNullPointer(std::unique_ptr<Cell> &&cell)
{
  auto tmp = std::move(cell);
  if (!tmp)
  {
    tmp = std::make_unique<VisiblyInvalidCell>(nullptr, m_configuration);
    tmp->SetToolTip(&T_("The xml data from maxima or from the .wxmx file was missing data here.\n"
                        "If you find a way how to reproduce this problem please file a bug "
                        "report against wxMaxima."));
    tmp->SetStyle(TS_ERROR);
  }
  return tmp;
}

std::unique_ptr<Cell> MathParser::ParseEditorTag(wxXmlNode *node)
{
  auto editor = std::make_unique<EditorCell>(nullptr, m_configuration);
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

std::unique_ptr<Cell>MathParser::ParseFracTag(wxXmlNode *node)
{
  auto fracStyle = m_FracStyle;
  auto highlight = m_highlight;

  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);
  auto num = HandleNullPointer(ParseTag(child, false));
  child = GetNextTag(child);
  auto denom = HandleNullPointer(ParseTag(child, false));
  
  auto frac = std::make_unique<FracCell>(nullptr, m_configuration, std::move(num), std::move(denom));
  frac->SetFracStyle(fracStyle);
  frac->SetHighlight(highlight);
  if (node->GetAttribute(wxT("line")) == wxT("no"))
    frac->SetFracStyle(FracCell::FC_CHOOSE);
  if (node->GetAttribute(wxT("diffstyle")) == wxT("yes"))
    frac->SetFracStyle(FracCell::FC_DIFF);
  frac->SetType(m_ParserStyle);
  frac->SetupBreakUps();
  ParseCommonAttrs(node, frac);
  return frac;
}

std::unique_ptr<Cell>MathParser::ParseDiffTag(wxXmlNode *node)
{
  std::unique_ptr<DiffCell> diff;

  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);
  if (child)
  {
    auto fc = m_FracStyle;
    m_FracStyle = FracCell::FC_DIFF;
    auto diffInner = HandleNullPointer(ParseTag(child, false));
    m_FracStyle = fc;
    child = GetNextTag(child);
    auto base = HandleNullPointer(ParseTag(child, true));

    diff = std::make_unique<DiffCell>(nullptr, m_configuration, std::move(base), std::move(diffInner));
    diff->SetType(m_ParserStyle);
  }
  else
  {
    diff = std::make_unique<DiffCell>(nullptr, m_configuration,
      Cell::MakeVisiblyInvalidCell(m_configuration),
      Cell::MakeVisiblyInvalidCell(m_configuration));
  }
  ParseCommonAttrs(node, diff);
  return diff;
}

std::unique_ptr<Cell> MathParser::ParseSupTag(wxXmlNode *node)
{
  bool matrix = (node->GetAttributes() != NULL);
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);

  auto base = HandleNullPointer(ParseTag(child, false));
  auto baseText = base->ToString();
  child = GetNextTag(child);

  auto power = HandleNullPointer(ParseTag(child, false));
  power->SetExponentFlag();
  auto powerText = power->ToString();

  auto expt = std::make_unique<ExptCell>(nullptr, m_configuration, std::move(base), std::move(power));
  expt->IsMatrix(matrix);
  expt->SetType(m_ParserStyle);

  ParseCommonAttrs(node, expt);
  if(node->GetAttribute(wxT("mat"), wxT("false")) == wxT("true"))
    expt->SetAltCopyText(baseText + wxT("^^") + powerText);

  return expt;
}

std::unique_ptr<Cell> MathParser::ParseSubSupTag(wxXmlNode *node)
{
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);
  auto base = HandleNullPointer(ParseTag(child, false));
  child = GetNextTag(child);

  auto subsup = std::make_unique<SubSupCell>(nullptr, m_configuration, std::move(base));
  wxString pos;
  if((child != NULL) && (child->GetAttribute("pos", wxEmptyString) != wxEmptyString))
  {
    while(child != NULL)
    {
      auto cell = HandleNullPointer(ParseTag(child, false));
      pos = child->GetAttribute("pos", wxEmptyString);
      if(pos == "presub")
        subsup->SetPreSub(std::move(cell));
      if(pos == "presup")
        subsup->SetPreSup(std::move(cell));
      if(pos == "postsup")
        subsup->SetPostSup(std::move(cell));
      if(pos == "postsub")
        subsup->SetPostSub(std::move(cell));
      child = SkipWhitespaceNode(child);
      child = GetNextTag(child);
    }
  }
  else
  {
    auto index = HandleNullPointer(ParseTag(child, false));
    index->SetExponentFlag();
    subsup->SetIndex(std::move(index));
    child = GetNextTag(child);
    auto power = HandleNullPointer(ParseTag(child, false));
    power->SetExponentFlag();
    subsup->SetExponent(std::move(power));
    subsup->SetType(m_ParserStyle);
    subsup->SetStyle(TS_VARIABLE);
    ParseCommonAttrs(node, subsup);
  }
  return subsup;
}

std::unique_ptr<Cell> MathParser::ParseMmultiscriptsTag(wxXmlNode *node)
{
  bool pre = false;
  bool subscript = true;
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);
  auto base = HandleNullPointer(ParseTag(child, false));
  child = GetNextTag(child);

  auto subsup = std::make_unique<SubSupCell>(nullptr, m_configuration, std::move(base));
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

std::unique_ptr<Cell> MathParser::ParseSubTag(wxXmlNode *node)
{
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);
  auto base = HandleNullPointer(ParseTag(child, false));
  child = GetNextTag(child);
  auto index = HandleNullPointer(ParseTag(child, false));
  index->SetExponentFlag();

  auto sub = std::make_unique<SubCell>(nullptr, m_configuration, std::move(base), std::move(index));
  sub->SetType(m_ParserStyle);
  ParseCommonAttrs(node, sub);
  return sub;
}

std::unique_ptr<Cell> MathParser::ParseAtTag(wxXmlNode *node)
{
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);
  auto base = HandleNullPointer(ParseTag(child, false));
  auto highlight = m_highlight;
  child = GetNextTag(child);
  auto index = HandleNullPointer(ParseTag(child, false));

  auto at = std::make_unique<AtCell>(nullptr, m_configuration, std::move(base), std::move(index));
  at->SetHighlight(highlight);
  at->SetType(m_ParserStyle);
  ParseCommonAttrs(node, at);
  return at;
}

std::unique_ptr<Cell> MathParser::ParseFunTag(wxXmlNode *node)
{
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);

  auto name = HandleNullPointer(ParseTag(child, false));
  child = GetNextTag(child);
  auto arg = HandleNullPointer(ParseTag(child, false));

  auto fun = std::make_unique<FunCell>(nullptr, m_configuration, std::move(name), std::move(arg));
  fun->SetType(m_ParserStyle);

  ParseCommonAttrs(node, fun);
  if (fun->ToString().Contains(")("))
    fun->SetToolTip(&T_("If this isn't a function returning a lambda() "
                        "expression a multiplication sign (*) between closing "
                        "and opening parenthesis is missing here."));
  return fun;
}

std::unique_ptr<Cell> MathParser::ParseText(wxXmlNode *node, TextStyle style)
{
  wxString str;
  CellListBuilder<TextCell> tree;
  if ((node != NULL) && ((str = node->GetContent()) != wxEmptyString))
  {
    str.Replace(wxT("-"), wxT("\u2212")); // unicode minus sign

    wxStringTokenizer lines(str, wxT('\n'));
    while (lines.HasMoreTokens())
    {
      std::unique_ptr<TextCell> cell;
      wxString value = lines.GetNextToken();
      if(style == TS_NUMBER)
      {
        if(value.Length() >= 20)
          cell = std::make_unique<LongNumberCell>(nullptr, m_configuration, value);
        else
          cell = std::make_unique<TextCell>(nullptr, m_configuration, value, style);
      }
      else if((style == TS_LABEL) ||
              (style == TS_USERLABEL) ||
              (style == TS_MAIN_PROMPT) ||
              (style == TS_OTHER_PROMPT))
        cell = std::make_unique<LabelCell>(nullptr, m_configuration, value, style);
      else
        cell = std::make_unique<TextCell>(nullptr, m_configuration, value, style);

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
      if (tree)
        cell->ForceBreakLine(true);
      tree.Append(std::move(cell));
    }
  }

  if (!tree)
    tree.Append(std::make_unique<TextCell>(nullptr, m_configuration));

  std::unique_ptr<TextCell> head = std::move(tree);
  ParseCommonAttrs(node, head);
  return head;
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
  if (node->GetAttribute(wxT("tooltip"), &val))
    if (!val.empty())
      cell->SetToolTip(std::move(val));
  if(node->GetAttribute(wxT("altCopy"), &val))
    cell->SetAltCopyText(val);
}

void MathParser::ParseCommonGroupCellAttrs(wxXmlNode *node, const std::unique_ptr<GroupCell> &group)
{
  if (!group || !node)
    return;

  if (node->GetAttribute(wxT("hideToolTip")) == wxT("true"))
    group->SetSuppressTooltipMarker(true);
}

std::unique_ptr<Cell> MathParser::ParseCharCode(wxXmlNode *node)
{
  auto cell = std::make_unique<TextCell>(nullptr, m_configuration);
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

std::unique_ptr<Cell> MathParser::ParseSqrtTag(wxXmlNode *node)
{
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);

  auto inner = HandleNullPointer(ParseTag(child, true));
  auto cell = std::make_unique<SqrtCell>(nullptr, m_configuration, std::move(inner));
  cell->SetType(m_ParserStyle);
  cell->SetHighlight(m_highlight);
  ParseCommonAttrs(node, cell);
  return cell;
}

std::unique_ptr<Cell> MathParser::ParseAbsTag(wxXmlNode *node)
{
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);
  auto inner = HandleNullPointer(ParseTag(child, true));

  auto cell = std::make_unique<AbsCell>(nullptr, m_configuration, std::move(inner));
  cell->SetType(m_ParserStyle);
  cell->SetHighlight(m_highlight);
  ParseCommonAttrs(node, cell);
  return cell;
}

std::unique_ptr<Cell> MathParser::ParseConjugateTag(wxXmlNode *node)
{
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);
  auto inner = HandleNullPointer(ParseTag(child, true));

  auto cell = std::make_unique<ConjugateCell>(nullptr, m_configuration, std::move(inner));
  cell->SetType(m_ParserStyle);
  cell->SetHighlight(m_highlight);
  ParseCommonAttrs(node, cell);
  return cell;
}

std::unique_ptr<Cell> MathParser::ParseParenTag(wxXmlNode *node)
{
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);
  // No special Handling for NULL args here: They are completely legal in this case.
  auto inner = ParseTag(child, true);
  auto cell = std::make_unique<ParenCell>(nullptr, m_configuration, std::move(inner));
  cell->SetType(m_ParserStyle);
  cell->SetHighlight(m_highlight);
  cell->SetStyle(TS_VARIABLE);
  if (node->GetAttributes() != NULL)
    cell->SetPrint(false);
  ParseCommonAttrs(node, cell);
  return cell;
}

std::unique_ptr<Cell> MathParser::ParseLimitTag(wxXmlNode *node)
{
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);
  auto name = HandleNullPointer(ParseTag(child, false));
  child = GetNextTag(child);
  auto under = HandleNullPointer(ParseTag(child, false));
  child = GetNextTag(child);
  auto base = HandleNullPointer(ParseTag(child, false));

  auto limit = std::make_unique<LimitCell>(nullptr, m_configuration, std::move(base), std::move(under), std::move(name));
  limit->SetType(m_ParserStyle);
  ParseCommonAttrs(node, limit);
  return limit;
}

std::unique_ptr<Cell> MathParser::ParseSumTag(wxXmlNode *node)
{
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);
  wxString type = node->GetAttribute(wxT("type"), wxT("sum"));
  sumStyle style = ((type == wxT("prod")) || (type == wxT("lprod"))) ? SM_PROD : SM_SUM;
  auto highlight = m_highlight;

  auto under = HandleNullPointer(ParseTag(child, false));
  child = GetNextTag(child);
  std::unique_ptr<Cell> over;
  if ((type != wxT("lsum")) && (type != wxT("lprod")))
    over = HandleNullPointer(ParseTag(child, false));
  child = GetNextTag(child);
  auto base = HandleNullPointer(ParseTag(child, false));

  auto sum = std::make_unique<SumCell>(nullptr, m_configuration, style, std::move(under), std::move(over), std::move(base));
  sum->SetHighlight(highlight);
  sum->SetType(m_ParserStyle);
  sum->SetStyle(TS_VARIABLE);
  ParseCommonAttrs(node, sum);
  return sum;
}

std::unique_ptr<Cell> MathParser::ParseIntTag(wxXmlNode *node)
{
  std::unique_ptr<IntCell> in;
  wxXmlNode *child = node->GetChildren();
  child = SkipWhitespaceNode(child);
  auto highlight = m_highlight;

  wxString definiteAtt = node->GetAttribute(wxT("def"), wxT("true"));
  if (definiteAtt != wxT("true"))
  {
    // An Indefinite Integral
    auto base = HandleNullPointer(ParseTag(child, false));
    child = GetNextTag(child);
    auto var = HandleNullPointer(ParseTag(child, true));
    in = std::make_unique<IntCell>(nullptr, m_configuration, std::move(base), std::move(var));
  }
  else
  {
    // A Definite Integral
    auto under = HandleNullPointer(ParseTag(child, false));
    child = GetNextTag(child);
    auto over = HandleNullPointer(ParseTag(child, false));
    child = GetNextTag(child);
    auto base = HandleNullPointer(ParseTag(child, false));
    child = GetNextTag(child);
    auto var = HandleNullPointer(ParseTag(child, true));

    in = std::make_unique<IntCell>(nullptr, m_configuration, std::move(base),
                                   std::move(under), std::move(over),
                                   std::move(var));
    in->SetIntStyle(IntCell::INT_DEF);
  }
  in->SetType(m_ParserStyle);
  in->SetHighlight(highlight);
  ParseCommonAttrs(node, in);
  return in;
}

std::unique_ptr<Cell> MathParser::ParseTableTag(wxXmlNode *node)
{
  auto matrix = std::make_unique<MatrCell>(nullptr, m_configuration);
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

std::unique_ptr<Cell> MathParser::ParseTag(wxXmlNode *node, bool all)
{
  CellListBuilder<> tree;
  bool gotInvalid = false;

  node = SkipWhitespaceNode(node);
  for (; node; node = GetNextTag(node))
  {
    auto &tagName = node->GetName();
    tree.ClearLastAppended();
    if (node->GetType() == wxXML_ELEMENT_NODE)
    {
      // Parse XML tags. The only other type of element we recognize are text
      // nodes.

      auto function = m_innerTags[tagName];
      if (function)
        tree.Append(CALL_MEMBER_FN(*this, function)(node));

      if (false)
        if (!tree.GetLastAppended() && node->GetChildren())
          tree.Append(ParseTag(node->GetChildren()));

      if (!tree.GetLastAppended() && (node->GetAttribute(wxT("listdelim")) != wxT("true")))
      {
        auto tmp = std::make_unique<VisiblyInvalidCell>(
            nullptr, m_configuration,
            wxString::Format(m_unknownXMLTagToolTip, tagName));
        tree.Append(std::move(tmp));
        gotInvalid = true;
      }

      if (tree.GetLastAppended())
        ParseCommonAttrs(node, tree.GetLastAppended());
    }
    else
    {
      // We didn't get a tag but got a text cell => Parse the text.
      tree.Append(ParseText(node));
    }

    if (gotInvalid && !all)
    {
      // Tell the user we ran into problems.
      wxString msg;
      if (gotInvalid)
        msg = tree.GetLastAppended()->ToString();
      else if (!tree.GetLastAppended() && !tagName.empty())
        msg = wxString::Format(m_unknownXMLTagToolTip, tagName);
      if (!msg.empty())
      {
        LoggingMessageBox(msg, _("Warning"), wxOK | wxICON_WARNING);
        gotInvalid = false;
      }
    }
    
    if (!all)
      break;
  }

  return std::move(tree);
}

std::unique_ptr<Cell> MathParser::ParseLine(wxString s, CellType style)
{
  m_ParserStyle = style;
  m_FracStyle = FracCell::FC_NORMAL;
  m_highlight = false;
  std::unique_ptr<Cell> cell;

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
    cell = std::make_unique<TextCell>(nullptr, m_configuration,
                        T_("(Expression longer than allowed by the configuration setting)"), TS_WARNING);
    cell->SetToolTip(&T_("The maximum size of the expressions wxMaxima is allowed to display "
                         "can be changed in the configuration dialogue."));
    cell->ForceBreakLine(true);
  }
  return cell;
}

wxRegEx MathParser::m_graphRegex(wxT("[[:cntrl:]]"));
MathParser::MathCellFunctionHash MathParser::m_innerTags;
MathParser::GroupCellFunctionHash MathParser::m_groupTags;
wxString MathParser::m_unknownXMLTagToolTip;
