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

#ifndef _MATHPARSER_H_
#define _MATHPARSER_H_

#include <wx/xml/xml.h>

#include <wx/filesys.h>
#include <wx/fs_arc.h>

#include "MathCell.h"
#include "TextCell.h"

class MathParser
{
public:
  MathParser(wxString zipfile = wxEmptyString);
  ~MathParser();
  MathCell* ParseLine(wxString s, int style = MC_TYPE_DEFAULT);
  MathCell* ParseTag(wxXmlNode* node, bool all = true);
private:
  MathCell* ParseCellTag(wxXmlNode* node);
  MathCell* ParseEditorTag(wxXmlNode* node);
  MathCell* ParseFracTag(wxXmlNode* node);
  MathCell* ParseText(wxXmlNode* node, int style = TS_DEFAULT);
  MathCell* ParseCharCode(wxXmlNode* node, int style = TS_DEFAULT);
  MathCell* ParseSupTag(wxXmlNode* node);
  MathCell* ParseSubTag(wxXmlNode* node);
  MathCell* ParseAbsTag(wxXmlNode* node);
  MathCell* ParseUnderTag(wxXmlNode* node);
  MathCell* ParseTableTag(wxXmlNode* node);
  MathCell* ParseAtTag(wxXmlNode* node);
  MathCell* ParseDiffTag(wxXmlNode* node);
  MathCell* ParseSumTag(wxXmlNode* node);
  MathCell* ParseIntTag(wxXmlNode* node);
  MathCell* ParseFunTag(wxXmlNode* node);
  MathCell* ParseSqrtTag(wxXmlNode* node);
  MathCell* ParseLimitTag(wxXmlNode* node);
  MathCell* ParseParenTag(wxXmlNode* node);
  MathCell* ParseSubSupTag(wxXmlNode* node);
  int m_ParserStyle;
  int m_FracStyle;
  bool m_highlight;
  wxFileSystem *m_fileSystem; // used for loading pictures in <img> and <slide>
};

#endif //_MATHPARSER_H_
