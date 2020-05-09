// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2004-2015 Gunter Königsmann <wxMaxima@physikbuch.de>
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

The header file for the xml cell parser
 */

#ifndef MATHPARSER_H
#define MATHPARSER_H

#include <wx/xml/xml.h>

#include <wx/filesys.h>
#include <wx/fs_arc.h>
#include <wx/regex.h>
#include "Cell.h"
#include "TextCell.h"
#include "EditorCell.h"
#include "FracCell.h"

/*! This class handles parsing the xml representation of a cell tree.

The xml representation of a cell tree can be found in the file contents.xml 
inside a wxmx file
 */
class MathParser
{
public:
  MathParser(Configuration **cfg, Cell::CellPointers *cellPointers, wxString zipfile = wxEmptyString);
  //! This class doesn't have a copy constructor
  MathParser(const MathParser&) = delete;
  //! This class doesn't have a = operator
  MathParser& operator=(const MathParser&) = delete;
  ~MathParser();

  void SetUserLabel(wxString label){ m_userDefinedLabel = label; }
  Cell *ParseLine(wxString s, CellType style = MC_TYPE_DEFAULT);

  Cell *ParseTag(wxXmlNode *node, bool all = true);
  Cell *ParseTagContents(wxXmlNode *node);

private:
  //! A storage for a tag and the function to call if one encounters it
  class TagFunction
  {
  public:
    TagFunction(wxString tag, Cell * (MathParser::* function)(wxXmlNode *node)):
      m_tag(tag),
      m_function(function)
      {}
    wxString m_tag;
    Cell * (MathParser::* m_function)(wxXmlNode *node);
  };

  //! Who you gonna call if you encounter any of these tags?
  static std::vector<TagFunction> m_knownTags;
  static void ParseCommonAttrs(wxXmlNode *node, Cell *cell);

  Cell *HandleNullPointer(Cell *cell);

  /*! Get the next xml tag

    wxXmlNode can operate in two modes:
     - One mode skips all whitespace between the beginning of the line and the first
       character if that character was escaped by a & for including it into the XML
       stream. This obviously is a bad idea in our case.
     - And the other mode inserts bogus whitespace text nodes if there is whitespace 
       between XML tags. This one is more helpful - but only if we provide a function
       that skips these whitespace text nodes.
    If we encounter a non-whitespace text node where we shouldn't we raise an 
    assertion that informs the user that we might want a bug report about this.
   */
  wxXmlNode *GetNextTag(wxXmlNode *node);

  /*! Returns node - or (if node is a whitespace-only text node) the next one.

    If we encounter a non-whitespace text node where we shouldn't we raise an 
    assertion that informs the user that we might want a bug report about this.
   */
  wxXmlNode *SkipWhitespaceNode(wxXmlNode *node);

  /*! Convert XML to a group tree

    This function is responsible for creating
    a tree of groupcells when loading XML document.
    \attention Any changes in GroupCell structure or methods
    has to be reflected here in order to ensure proper
    loading of WXMX files.
  */
  Cell *ParseCellTag(wxXmlNode *node);

  Cell *ParseEditorTag(wxXmlNode *node);

  Cell *ParseFracTag(wxXmlNode *node);

  Cell *ParseText(wxXmlNode *node, TextStyle style = TS_DEFAULT);
  Cell *ParseVariableNameTag(wxXmlNode *node){return ParseText(node->GetChildren(), TS_VARIABLE);}
  Cell *ParseOperatorNameTag(wxXmlNode *node){return ParseText(node->GetChildren(), TS_FUNCTION);}
  Cell *ParseMiscTextTag(wxXmlNode *node);
  Cell *ParseNumberTag(wxXmlNode *node){return ParseText(node->GetChildren(), TS_NUMBER);}
  Cell *ParseHiddenOperatorTag(wxXmlNode *node);
  Cell *ParseGreekTag(wxXmlNode *node){return ParseText(node->GetChildren(), TS_GREEK_CONSTANT);}
  Cell *ParseSpecialConstantTag(wxXmlNode *node){return ParseText(node->GetChildren(), TS_SPECIAL_CONSTANT);}
  Cell *ParseFunctionNameTag(wxXmlNode *node){return ParseText(node->GetChildren(), TS_FUNCTION);}
  Cell *ParseSpaceTag(wxXmlNode *node){return new TextCell(NULL, m_configuration, m_cellPointers, wxT(" "));}
  Cell *ParseMthTag(wxXmlNode *node);
  Cell *ParseOutputLabelTag(wxXmlNode *node);
  Cell *ParseStringTag(wxXmlNode *node){return ParseText(node->GetChildren(), TS_STRING);}
  Cell *ParseHighlightTag(wxXmlNode *node);
  Cell *ParseImageTag(wxXmlNode *node);
  Cell *ParseSlideshowTag(wxXmlNode *node);

  Cell *ParseCharCode(wxXmlNode *node);

  Cell *ParseSupTag(wxXmlNode *node);

  Cell *ParseSubTag(wxXmlNode *node);

  Cell *ParseAbsTag(wxXmlNode *node);

  Cell *ParseConjugateTag(wxXmlNode *node);

  Cell *ParseUnderTag(wxXmlNode *node);

  Cell *ParseTableTag(wxXmlNode *node);

  Cell *ParseAtTag(wxXmlNode *node);

  Cell *ParseDiffTag(wxXmlNode *node);

  Cell *ParseSumTag(wxXmlNode *node);

  Cell *ParseIntTag(wxXmlNode *node);

  Cell *ParseFunTag(wxXmlNode *node);

  Cell *ParseSqrtTag(wxXmlNode *node);

  Cell *ParseLimitTag(wxXmlNode *node);

  Cell *ParseParenTag(wxXmlNode *node);

  Cell *ParseSubSupTag(wxXmlNode *node);

  Cell *ParseMmultiscriptsTag(wxXmlNode *node);
  
  wxString m_userDefinedLabel;
  static wxRegEx m_graphRegex;

  CellType m_ParserStyle;
  int m_FracStyle;
  Cell::CellPointers *m_cellPointers;
  Configuration **m_configuration;
  bool m_highlight;
  std::shared_ptr<wxFileSystem> m_fileSystem; // used for loading pictures in <img> and <slide>
};

#endif // MATHPARSER_H
