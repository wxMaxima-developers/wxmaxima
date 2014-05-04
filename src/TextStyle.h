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

#ifndef TEXTSTYLE_H
#define TEXTSTYLE_H

struct style
{
  style() :
    bold(false), italic(false), underlined(false)
    {  };
  wxColour color;
  wxString font;
  int fontSize;
  bool bold;
  bool italic;
  bool underlined;
};

enum
{
  TS_DEFAULT = 0,
  TS_VARIABLE,
  TS_FUNCTION,
  TS_NUMBER,
  TS_GREEK_CONSTANT,
  TS_SPECIAL_CONSTANT,
  TS_STRING,
  TS_MAIN_PROMPT,
  TS_OTHER_PROMPT,
  TS_LABEL,
  TS_INPUT,
  TS_HIGHLIGHT,
  TS_TEXT_BACKGROUND,
  TS_TEXT,
  TS_SUBSECTION,
  TS_SECTION,
  TS_TITLE,
  TS_ERROR,
  TS_CELL_BRACKET,
  TS_ACTIVE_CELL_BRACKET,
  TS_CURSOR,
  TS_SELECTION,
  TS_OUTDATED
};

#define STYLE_NUM 23

#endif // TEXTSTYLE_H
