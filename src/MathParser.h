/*
 *  Copyright (C) 2004-2006 Andrej Vodopivec <andrejv@users.sourceforge.net>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#ifndef _MATHPARSER_H_
#define _MATHPARSER_H_

#include <libxml/parser.h>

#include "MathCell.h"
#include "TextCell.h"

class MathParser {
  public:
    MathParser();
    ~MathParser();
    MathCell* ParseLine(wxString s, int style = MC_TYPE_TEXT);
  private:
    MathCell* ParseTag(xmlNodePtr node, bool all = true);
    MathCell* ParseFracTag(xmlNodePtr node);
    MathCell* ParseText(xmlNodePtr node, int style = TS_NORMAL_TEXT);
    MathCell* ParseSupTag(xmlNodePtr node);
    MathCell* ParseSubTag(xmlNodePtr node);
    MathCell* ParseAbsTag(xmlNodePtr node);
    MathCell* ParseUnderTag(xmlNodePtr node);
    MathCell* ParseTableTag(xmlNodePtr node);
    MathCell* ParseAtTag(xmlNodePtr node);
    MathCell* ParseDiffTag(xmlNodePtr node);
    MathCell* ParseSumTag(xmlNodePtr node);
    MathCell* ParseIntTag(xmlNodePtr node);
    MathCell* ParseFunTag(xmlNodePtr node);
    MathCell* ParseSqrtTag(xmlNodePtr node);
    MathCell* ParseLimitTag(xmlNodePtr node);
    MathCell* ParseParenTag(xmlNodePtr node);
    wxString ToUnicode(wxString s);
    wxString ToLocal(wxString s);
    int m_ParserStyle;
    int m_FracStyle;
};

#endif	//_MATHPARSER_H_
