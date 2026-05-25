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

#include <memory>
#include "precomp.h"
#include <wx/xml/xml.h>

#include <wx/filesys.h>
#include <wx/fs_arc.h>
#include <wx/regex.h>
#include <wx/hashmap.h>
#include "cells/Cell.h"
#include "cells/TextCell.h"
#include "cells/EditorCell.h"
#include "cells/FracCell.h"
#include "cells/GroupCell.h"
#include <unordered_map>
#include <unordered_set>

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
  explicit MathParser(Configuration *cfg, const wxString &zipfile = {});
  //! This class doesn't have a copy constructor
  MathParser(const MathParser&) = delete;
  //! This class doesn't have a = operator
  MathParser& operator=(const MathParser&) = delete;
  virtual ~MathParser();

  /*! Tells the parser what user label to assign to the next label cell

    Maxima doesn't tell us what label the user tries to assign to the current command's
    output, but the code that sends the current command to maxima can try to detemine
    that and tell us so wxMaxima can display a speaking label to this cell.
   */
  void SetUserLabel(const wxString &label){ m_userDefinedLabel = label; }
  /***
   * Parse the string s, which is (correct) xml fragment.
   * Put the result in line.
   */
  std::unique_ptr<Cell> ParseLine(wxString s, CellType style = MC_TYPE_DEFAULT);
  std::unique_ptr<Cell> ParseLine(const wxXmlDocument &xml, CellType style = MC_TYPE_DEFAULT);
  /***
   * Parse the node and return the corresponding tag.
   */

  std::unique_ptr<Cell> ParseTag(wxXmlNode *node, bool all = true, int depth = 0);
  std::unique_ptr<Cell> ParseRowTag(wxXmlNode *node, int depth = 0);

  std::unique_ptr<GroupCell> CreateTreeFromXMLNode(wxXmlNode *xmlcells);

  //! Sets the group the newly parsed cells are provided with
  void SetGroup(GroupCell *group) { m_group = group; }

private:
  //! A pointer to a method that handles an XML tag for a type of Cell
  using MathCellFunc = std::unique_ptr<Cell> (MathParser::*)(wxXmlNode *node, int depth);

  //! A pointer to a method that handles an XML tag for a type of GroupCell
  using GroupCellFunc = std::unique_ptr<GroupCell> (MathParser::*)(wxXmlNode *node);

  typedef std::unordered_map <wxString, MathCellFunc, wxStringHash> MathCellFunctionHash;
  typedef std::unordered_map <wxString, GroupCellFunc, wxStringHash> GroupCellFunctionHash;

  //! Who you gonna call if you encounter any of these math cell tags?
  static MathCellFunctionHash m_innerTags;
  //! A list of functions to call on encountering all types of GroupCell tags
  static GroupCellFunctionHash m_groupTags;
  //! Attributes we know about and that shouldn't end up in m_extraAttributes
  static std::unordered_set<wxString, wxStringHash> m_knownAttributes;

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
  static wxXmlNode *GetNextTag(wxXmlNode *node);

  /*! Counts the number of non-whitespace children of a node */
  static int CountChildren(wxXmlNode *node);

  /*! Returns node - or (if node is a whitespace-only text node) the next one.

    If we encounter a non-whitespace text node where we shouldn't we raise an
    assertion that informs the user that we might want a bug report about this.
  */
  static wxXmlNode *SkipWhitespaceNode(wxXmlNode *node);

  /*! \defgroup GroupCellParsing Methods that generate GroupCells from XML
    @{
  */

  /*! Convert XML to a tree of group cells

    This function is responsible for creating a tree of groupcells when loading XML document.

    \attention Any changes in GroupCell structure or methods
    has to be reflected here in order to ensure proper loading of WXMX files.
  */
  std::unique_ptr<Cell> ParseCellTag(wxXmlNode *node, int depth = 0);
  //! Convert a code cell XML tag to a GroupCell
  std::unique_ptr<GroupCell> GroupCellFromCodeTag(wxXmlNode *node);
  //! Convert an image XML tag to a GroupCell
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
  std::unique_ptr<Cell> ParseEditorTag(wxXmlNode *node, int depth = 0);
  //! Parse an frac XML tag to a Cell.
  std::unique_ptr<Cell> ParseFracTag(wxXmlNode *node, int depth = 0);
  //! Parse a text XML tag to a Cell.
  std::unique_ptr<Cell> ParseText(wxXmlNode *node, TextStyle style = TS_MATH, int depth = 0);
  /*! Parse a Variable name / operator tag to a Cell.

    Operators identify themself as variable.
  */
  std::unique_ptr<Cell> ParseVariableNameTag(wxXmlNode *node, int depth = 0);
  //! Parse an Operator name tag to a Cell.
  std::unique_ptr<Cell> ParseOperatorNameTag(wxXmlNode *node, int depth = 0){return ParseText(node->GetChildren(), TS_FUNCTION, depth);}
  //! Parse a miscellaneous text tag to a Cell.
  std::unique_ptr<Cell> ParseMiscTextTag(wxXmlNode *node, int depth = 0);
  //! Parse a number tag to a Cell.
  std::unique_ptr<Cell> ParseNumberTag(wxXmlNode *node, int depth = 0){return ParseText(node->GetChildren(), TS_NUMBER, depth);}
  //! Parse a hidden operator tag to a Cell.
  std::unique_ptr<Cell> ParseHiddenOperatorTag(wxXmlNode *node, int depth = 0);
  //! Parse an hidden operator tag to a Cell.
  std::unique_ptr<Cell> ParseGreekTag(wxXmlNode *node, int depth = 0){return ParseText(node->GetChildren(), TS_GREEK_CONSTANT, depth);}
  //! Parse a special constant tag to a Cell.
  std::unique_ptr<Cell> ParseSpecialConstantTag(wxXmlNode *node, int depth = 0){return ParseText(node->GetChildren(), TS_SPECIAL_CONSTANT, depth);}
  //! Parse a function name tag to a Cell.
  std::unique_ptr<Cell> ParseFunctionNameTag(wxXmlNode *node, int depth = 0){return ParseText(node->GetChildren(), TS_FUNCTION, depth);}
  //! Parse a space tag to a Cell.
  std::unique_ptr<Cell> ParseSpaceTag(wxXmlNode *WXUNUSED(node), int WXUNUSED(depth) = 0){return std::make_unique<TextCell>(m_group, m_configuration, wxS(" "));}
  /*! Parse a math-in-maths tag to a Cell.

    \todo Does such a thing actually exist?
  */
  std::unique_ptr<Cell> ParseMthTag(wxXmlNode *node, int depth = 0);
  //! Parse an output label tag to a Cell.
  std::unique_ptr<Cell> ParseOutputLabelTag(wxXmlNode *node, int depth = 0);
  //! Parse a string tag to a Cell.
  std::unique_ptr<Cell> ParseStringTag(wxXmlNode *node, int depth = 0);
  //! Parse a highlight tag to a Cell.
  std::unique_ptr<Cell> ParseHighlightTag(wxXmlNode *node, int depth = 0);
  //! Parse an image tag to a Cell.
  std::unique_ptr<Cell> ParseImageTag(wxXmlNode *node, int depth = 0);
  //! Parse an animation tag to a Cell.
  std::unique_ptr<Cell> ParseAnimationTag(wxXmlNode *node, int depth = 0);
  //! Parse a charcode tag to a Cell.
  std::unique_ptr<Cell> ParseCharCode(wxXmlNode *node, int depth = 0);
  //! Parse a superscript tag to a Cell.
  std::unique_ptr<Cell> ParseSupTag(wxXmlNode *node, int depth = 0);
  //! Parse a subscript tag to a Cell.
  std::unique_ptr<Cell> ParseSubTag(wxXmlNode *node, int depth = 0);
  //! Parse an abs tag to a Cell.
  std::unique_ptr<Cell> ParseAbsTag(wxXmlNode *node, int depth = 0);
  //! Parse a conjugate cell tag to a Cell.
  std::unique_ptr<Cell> ParseConjugateTag(wxXmlNode *node, int depth = 0);
#if 0
  //! Parse an index tag to a Cell. FIXME this is unused, without implementation.
  std::unique_ptr<Cell> ParseUnderTag(wxXmlNode *node, int depth = 0);
#endif
  //! Parse an table tag to a Cell.
  std::unique_ptr<Cell> ParseTableTag(wxXmlNode *node, int depth = 0);
  //! Parse an atcell tag to a Cell.
  std::unique_ptr<Cell> ParseAtTag(wxXmlNode *node, int depth = 0);
  //! Parse a diff tag to a Cell.
  std::unique_ptr<Cell> ParseDiffTag(wxXmlNode *node, int depth = 0);
  //! Parse a sum tag to a Cell.
  std::unique_ptr<Cell> ParseSumTag(wxXmlNode *node, int depth = 0);
  //! Parse an integral tag to a Cell.
  std::unique_ptr<Cell> ParseIntTag(wxXmlNode *node, int depth = 0);
  //! Parse a function tag to a Cell.
  std::unique_ptr<Cell> ParseFunTag(wxXmlNode *node, int depth = 0);
  //! Parse a square root tag to a Cell.
  std::unique_ptr<Cell> ParseSqrtTag(wxXmlNode *node, int depth = 0);
  //! Parse a lim() tag to a Cell.
  std::unique_ptr<Cell> ParseLimitTag(wxXmlNode *node, int depth = 0);
  //! Parse a parenthesis() tag to a Cell.
  std::unique_ptr<Cell> ParseParenTag(wxXmlNode *node, int depth = 0);
  //! Parse a super-and-subscript cell tag to a Cell.
  std::unique_ptr<Cell> ParseSubSupTag(wxXmlNode *node, int depth = 0);
  //! Parse a pre-and-post-super-and-subscript cell tag to a Cell.
  std::unique_ptr<Cell> ParseMmultiscriptsTag(wxXmlNode *node, int depth = 0);
  //! Parse an Output tag telling that the math is from maxima.
  std::unique_ptr<Cell> ParseOutputTag(wxXmlNode *node, int depth = 0);
  //! Parse an Matrix cell tag.
  std::unique_ptr<Cell> ParseMtdTag(wxXmlNode *node, int depth = 0);
  // @}
  //! The last user defined label
  wxString m_userDefinedLabel;
  //! A RegEx that catches the last graphics placeholder
  static wxRegEx m_graphRegex;

  CellType m_ParserStyle = MC_TYPE_DEFAULT;
  FracCell::FracType m_FracStyle;
  CellPtr<GroupCell> m_group;
  Configuration *m_configuration = NULL;
  bool m_highlight = false;
  wxString m_wxmxFile; // if not wxEmptyString: The wxmx file to load images from
  static wxString m_unknownXMLTagToolTip;
};

#endif // MATHPARSER_H
