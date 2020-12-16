// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2004-2015 Gunter Königsmann <wxMaxima@physikbuch.de>
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

The header file for the xml cell parser
 */

#ifndef MATHPARSER_H
#define MATHPARSER_H

#include "precomp.h"
#include <wx/xml/xml.h>

#include <wx/filesys.h>
#include <wx/fs_arc.h>
#include <wx/regex.h>
#include <wx/hashmap.h>
#include "Cell.h"
#include "TextCell.h"
#include "EditorCell.h"
#include "FracCell.h"
#include "GroupCell.h"

/*! This class handles parsing the xml representation of a cell tree.

The xml representation of a cell tree can be found in the file contents.xml 
inside a wxmx file
 */
class MathParser
{
public:
  /*! The constructor

     \todo I guess we could increase the performance further by putting the 
     most-frequently-used tags to the front of the list.
   */
  explicit MathParser(Configuration **cfg, const wxString &zipfile = {});
  //! This class doesn't have a copy constructor
  MathParser(const MathParser&) = delete;
  //! This class doesn't have a = operator
  MathParser& operator=(const MathParser&) = delete;
  ~MathParser();

  void SetUserLabel(wxString label){ m_userDefinedLabel = label; }
  /***
   * Parse the string s, which is (correct) xml fragment.
   * Put the result in line.
   */
  std::unique_ptr<Cell> ParseLine(wxString s, CellType style = MC_TYPE_DEFAULT);
  /***
   * Parse the node and return the corresponding tag.
   */
  
  std::unique_ptr<Cell> ParseTag(wxXmlNode *node, bool all = true);
  std::unique_ptr<Cell> ParseRowTag(wxXmlNode *node);

private:
  //! A pointer to a method that handles an XML tag for a type of Cell
  using MathCellFunc = std::unique_ptr<Cell> (MathParser::*)(wxXmlNode *node);
  WX_DECLARE_STRING_HASH_MAP(MathCellFunc, MathCellFunctionHash);

  //! A pointer to a method that handles an XML tag for a type of GroupCell
  using GroupCellFunc = std::unique_ptr<GroupCell> (MathParser::*)(wxXmlNode *node);
  WX_DECLARE_STRING_HASH_MAP(GroupCellFunc, GroupCellFunctionHash);

  //! Who you gonna call if you encounter any of these math cell tags?
  static MathCellFunctionHash m_innerTags;
  //! A list of functions to call on encountering all types of GroupCell tags
  static GroupCellFunctionHash m_groupTags;

  //! Parses attributes that apply to nearly all types of cells
  static void ParseCommonAttrs(wxXmlNode *node, Cell *cell);
  template <typename T>
  static void ParseCommonAttrs(wxXmlNode *node, const std::unique_ptr<T> &cell)
  { ParseCommonAttrs(node, cell.get()); }

  //! Parses attributes that apply to nearly all types of cells
  static void ParseCommonGroupCellAttrs(wxXmlNode *node, const std::unique_ptr<GroupCell> &group);

  //! Returns cell or, if cell==NULL, an empty text cell as a fallback.
  std::unique_ptr<Cell> HandleNullPointer(std::unique_ptr<Cell> &&cell);

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

  /*! \defgroup GroupCellParsing Methods that generate GroupCells from XML
    @{
  */

  /*! Convert XML to a tree of group cells

    This function is responsible for creating a tree of groupcells when loading XML document.

    \attention Any changes in GroupCell structure or methods
    has to be reflected here in order to ensure proper loading of WXMX files.
  */
  std::unique_ptr<Cell> ParseCellTag(wxXmlNode *node);
  //! Convert a code cell XML tag to a GroupCell
  std::unique_ptr<GroupCell> GroupCellFromCodeTag(wxXmlNode *node);
  //! Convert a image XML tag to a GroupCell
  std::unique_ptr<GroupCell> GroupCellFromImageTag(wxXmlNode *node);
  //! Convert a title XML tag to a GroupCell
  std::unique_ptr<GroupCell> GroupCellFromTitleTag(wxXmlNode *WXUNUSED(node));
  //! Convert a title XML tag to a GroupCell
  std::unique_ptr<GroupCell> GroupCellFromSectionTag(wxXmlNode *WXUNUSED(node));
  //! Convert a pagebreak XML tag to a GroupCell
  std::unique_ptr<GroupCell> GroupCellFromPagebreakTag(wxXmlNode *WXUNUSED(node));
  //! Convert a subsection XML tag to a GroupCell
  std::unique_ptr<GroupCell> GroupCellFromSubsectionTag(wxXmlNode *node);
  //! Convert a subsubsection XML tag to a GroupCell
  std::unique_ptr<GroupCell> GroupCellFromSubsubsectionTag(wxXmlNode *WXUNUSED(node));
  //! Convert a heading5 XML tag to a GroupCell
  std::unique_ptr<GroupCell> GroupCellHeading5Tag(wxXmlNode *WXUNUSED(node));
  //! Convert a heading6 XML tag to a GroupCell
  std::unique_ptr<GroupCell> GroupCellHeading6Tag(wxXmlNode *WXUNUSED(node));
  //! Convert a text cell XML tag to a GroupCell
  std::unique_ptr<GroupCell> GroupCellFromTextTag(wxXmlNode *WXUNUSED(node));
  /* @} */
  
  /*! \defgroup MathCellParsing Methods that generate Cell objects from XML
    @{
  */
  //! Parse an editor XML tag to a Cell.
  std::unique_ptr<Cell> ParseEditorTag(wxXmlNode *node);
  //! Parse an frac XML tag to a Cell. 
  std::unique_ptr<Cell> ParseFracTag(wxXmlNode *node);
  //! Parse a text XML tag to a Cell.
  std::unique_ptr<Cell> ParseText(wxXmlNode *node, TextStyle style = TS_DEFAULT);
  //! Parse a Variable name tag t a Cell.
  std::unique_ptr<Cell> ParseVariableNameTag(wxXmlNode *node){return ParseText(node->GetChildren(), TS_VARIABLE);}
  //! Parse an Operator name tag to a Cell. 
  std::unique_ptr<Cell> ParseOperatorNameTag(wxXmlNode *node){return ParseText(node->GetChildren(), TS_FUNCTION);}
  //! Parse a miscellaneous text tag to a Cell. 
  std::unique_ptr<Cell> ParseMiscTextTag(wxXmlNode *node);
  //! Parse a number tag to a Cell. 
  std::unique_ptr<Cell> ParseNumberTag(wxXmlNode *node){return ParseText(node->GetChildren(), TS_NUMBER);}
  //! Parse a hidden operator tag to a Cell. 
  std::unique_ptr<Cell> ParseHiddenOperatorTag(wxXmlNode *node);
  //! Parse an hidden operator tag to a Cell. 
  std::unique_ptr<Cell> ParseGreekTag(wxXmlNode *node){return ParseText(node->GetChildren(), TS_GREEK_CONSTANT);}
  //! Parse a special constant tag to a Cell. 
  std::unique_ptr<Cell> ParseSpecialConstantTag(wxXmlNode *node){return ParseText(node->GetChildren(), TS_SPECIAL_CONSTANT);}
  //! Parse a function name tag to a Cell. 
  std::unique_ptr<Cell> ParseFunctionNameTag(wxXmlNode *node){return ParseText(node->GetChildren(), TS_FUNCTION);}
  //! Parse a space tag to a Cell.
  std::unique_ptr<Cell> ParseSpaceTag(wxXmlNode *WXUNUSED(node)){return std::make_unique<TextCell>(m_group, m_configuration, wxT(" "));}
  /*! Parse a math-in-maths tag to a Cell. 

    \todo Does such a thing actually exist?
   */
  std::unique_ptr<Cell> ParseMthTag(wxXmlNode *node);
  //! Parse an output label tag to a Cell. 
  std::unique_ptr<Cell> ParseOutputLabelTag(wxXmlNode *node);
  //! Parse a string tag to a Cell. 
  std::unique_ptr<Cell> ParseStringTag(wxXmlNode *node);
  //! Parse a highlight tag to a Cell. 
  std::unique_ptr<Cell> ParseHighlightTag(wxXmlNode *node);
  //! Parse a image tag to a Cell. 
  std::unique_ptr<Cell> ParseImageTag(wxXmlNode *node);
  //! Parse a animation tag to a Cell. 
  std::unique_ptr<Cell> ParseSlideshowTag(wxXmlNode *node);
  //! Parse a charcode tag to a Cell. 
  std::unique_ptr<Cell> ParseCharCode(wxXmlNode *node);
  //! Parse a superscript tag to a Cell. 
  std::unique_ptr<Cell> ParseSupTag(wxXmlNode *node);
  //! Parse a subscript tag to a Cell. 
  std::unique_ptr<Cell> ParseSubTag(wxXmlNode *node);
  //! Parse a abs tag to a Cell. 
  std::unique_ptr<Cell> ParseAbsTag(wxXmlNode *node);
  //! Parse a conjugate cell tag to a Cell. 
  std::unique_ptr<Cell> ParseConjugateTag(wxXmlNode *node);
#if 0
  //! Parse an index tag to a Cell. FIXME this is unused, without implementation.
  std::unique_ptr<Cell> ParseUnderTag(wxXmlNode *node);
#endif
  //! Parse an table tag to a Cell. 
  std::unique_ptr<Cell> ParseTableTag(wxXmlNode *node);
  //! Parse an atcell tag to a Cell. 
  std::unique_ptr<Cell> ParseAtTag(wxXmlNode *node);
  //! Parse a diff tag to a Cell. 
  std::unique_ptr<Cell> ParseDiffTag(wxXmlNode *node);
  //! Parse a sum tag to a Cell. 
  std::unique_ptr<Cell> ParseSumTag(wxXmlNode *node);
  //! Parse a integral tag to a Cell. 
  std::unique_ptr<Cell> ParseIntTag(wxXmlNode *node);
  //! Parse a function tag to a Cell. 
  std::unique_ptr<Cell> ParseFunTag(wxXmlNode *node);
  //! Parse a square root tag to a Cell. 
  std::unique_ptr<Cell> ParseSqrtTag(wxXmlNode *node);
  //! Parse a lim() tag to a Cell. 
  std::unique_ptr<Cell> ParseLimitTag(wxXmlNode *node);
  //! Parse a parenthesis() tag to a Cell. 
  std::unique_ptr<Cell> ParseParenTag(wxXmlNode *node);
  //! Parse a super-and-subscript cell tag to a Cell. 
  std::unique_ptr<Cell> ParseSubSupTag(wxXmlNode *node);
  //! Parse a pre-and-post-super-and-subscript cell tag to a Cell. 
  std::unique_ptr<Cell> ParseMmultiscriptsTag(wxXmlNode *node);
  //! Parse an Output tag telling that the math is from maxima. 
  std::unique_ptr<Cell> ParseOutputTag(wxXmlNode *node);
  //! Parse an Matrix cell tag. 
  std::unique_ptr<Cell> ParseMtdTag(wxXmlNode *node);
  // @}
  //! The last user defined label
  wxString m_userDefinedLabel;
  //! A RegEx that catches the last graphics placeholder
  static wxRegEx m_graphRegex;

  CellType m_ParserStyle;
  FracCell::FracType m_FracStyle;
  CellPtr<GroupCell> m_group;
  Configuration **m_configuration;
  bool m_highlight;
  std::shared_ptr<wxFileSystem> m_fileSystem; // used for loading pictures in <img> and <slide>
  static wxString m_unknownXMLTagToolTip;
};

#endif // MATHPARSER_H
