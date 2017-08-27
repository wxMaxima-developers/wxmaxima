// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2017 Guglielmo Saggiorato <astyonax@gmail.com>
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
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//

/*!\file

  This file belongs to the wxMaxima.
  This file defines the function string BetterTex(string) used to enhance
  readibility of the exported LaTeX code
 */

#include "BetterTeX.h"
#include <wx/regex.h>

wxString BetterTeX(wxString str){
  // Using WxWidgets Regular Expressions
  // http://docs.wxwidgets.org/trunk/overview_resyntax.html
  // missing features: lookbehind/ahead
  // I document here few notes:
  // The implementation of wxRE_ADVANCED (or ARE in the docs) is from Henry Spencer,
  // that originally wrote `regex`. This is also the `tcl`  implementation.
  // It's not exaclty ECMAScript compatible, so when wx will remove support
  // for their/this regex.h, there will be some adjustments to use the C++11
  // regex implementation.

  wxLogMessage("Hi, here is BetterTeX for you.");
  wxString retval = str;

  // PART 0 --USEFUL regular expressions
  wxRegEx removeWhiteSpaceAroundBraces(wxT("\\s*([}{^])\\s*"),wxRE_ADVANCED);
  removeWhiteSpaceAroundBraces.Replace(&retval,"\\1");

  // PART A --IMHO useless keywords
  wxRegEx mathit(wxT("{\\\\mathit{(\\w+?)}}"),wxRE_ADVANCED);
  mathit.Replace(&retval,"{\\1}");

  wxRegEx ensuremath(wxT("{\\\\ensuremath{(\\w+?)}}"),wxRE_ADVANCED);
  ensuremath.Replace(&retval,"{\\1}");
  // END PART A

  // PART B --HANDLES CASES Powers {{str}^{str}}

  // intercept braces around single char
  // --
  wxRegEx bracesAroundSingleLetter(wxT("([^}ct]){([a-zA-Z0-9])}"));
  bracesAroundSingleLetter.Replace(&retval,"\\1 \\2");
  removeWhiteSpaceAroundBraces.Replace(&retval,"\\1");

  // {str}^{str}->str^{str}
  wxLogMessage(retval);
  wxRegEx bracesAroundBasePower("([^}ct]{0,1}){([^}{]+?)}\\^",wxRE_ADVANCED);
  bracesAroundBasePower.Replace(&retval,"\\1 \\2^");
  removeWhiteSpaceAroundBraces.Replace(&retval,"\\1");

  // intercepts {a^b} , but not \sqrt{a^b}, or \frac{a^b}{c}
  wxRegEx bracesAroundCharPower("([^}ct]{0,1}){(\\w\\^\\w)}",wxRE_ADVANCED);
  bracesAroundCharPower.Replace(&retval,"\\1 \\2");
  removeWhiteSpaceAroundBraces.Replace(&retval,"\\1");

  // intercepts {a^{str}} , but not \sqrt{a^b}, or \frac{a^b}{c}
  wxRegEx bracesAroundStrPower("([^}ct]{0,1}){([^}{]+?\\^{[^}{]+?})}",wxRE_ADVANCED);
  bracesAroundStrPower.Replace(&retval,"\\1 \\2");
  removeWhiteSpaceAroundBraces.Replace(&retval,"\\1");
  // END PART B

  // PART C --HANDLES Parenteresis as in \left ..\right..
  // intercepts {\left(..\right)} -- protected if begins with c{..} or }{} as would be in \frac{..}{..}
  // wxRegEx bracesAroundParenthesis(wxT("[^}c]{\\(\\left\\(.+?\\right\\)\\)\s*}"));
  // bracesAroundParenthesis.Replace(&retval,"\\1");

  // intercepts {optional_command or words} -- protected if begins with c{..} or }{} as would be in \frac{..}{..}
  // wxRegEx bracesAroundText(wxT("[^}c]{\\(\\\\*\s*\w+?\s*\\)}"));
  // bracesAroundText.Replace(&retval,"\\1");
  wxLogMessage(retval);
  return retval;
}
