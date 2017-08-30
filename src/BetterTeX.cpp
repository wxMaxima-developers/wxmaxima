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

wxString ProtectedReplace(wxString str, wxString expr, wxString replacement, int filter_on=1)
{
    // This implements a sort of look behind
    // to protect some keyword or context from being destroyed

    wxString protection = "(\\\\[a-zA-Z0-9]+?|\\})";
    wxString keywords = "}\\frac\\sqrt";

    wxRegEx re(protection+expr,wxRE_ADVANCED);
    wxRegEx re_go(expr,wxRE_ADVANCED);

    (re.IsValid() && re.Matches(str));

    wxString kw  = re.GetMatch(str,filter_on);
    wxLogMessage(":"+kw+":");

    bool doit = not bool(kw.length());
    doit = doit || (keywords.find(kw) == wxNOT_FOUND);

    if (doit)
    {
        wxLogMessage("inside");
        re_go.Replace(&str,replacement);
        wxRegEx spaces("\\s+([\\}\\{\\)\\(])",wxRE_ADVANCED);
        spaces.Replace(&str,"\\1");
        wxRegEx spaces2("([\\}\\{\\)\\(])\\s+",wxRE_ADVANCED);
        spaces2.Replace(&str,"\\1");
    }
    return str;
}

wxString BetterTeX(wxString str)
{
  // Using WxWidgets Regular Expressions
  // http://docs.wxwidgets.org/trunk/overview_resyntax.html
  // missing features: lookbehind/ahead
  // I document here few notes:
  // The implementation of wxRE_ADVANCED (or ARE in the docs) is from Henry Spencer,
  // that originally wrote `regex`. This is also the `tcl`  implementation.
  // It's not exaclty ECMAScript compatible, so when wx will remove support
  // for their/this regex.h, there will be some adjustments to use the C++11
  // regex implementation.
  // wxLogMessage(str);
  wxString retval = str;
  wxLogMessage("0 " + retval);
  // PART 0 --USEFUL regular expressions
  // wxRegEx removeWhiteSpaceAroundBraces(wxT("\\s*[}{)(}]\\s*"),wxRE_ADVANCED);
  // removeWhiteSpaceAroundBraces.Replace(&retval,"");

  // PART A --IMHO useless keywords
  wxRegEx mathit(wxT("\\\\mathit{(\\w+?)}"),wxRE_ADVANCED);
  mathit.Replace(&retval,"\\1");

  wxRegEx ensuremath(wxT("\\\\ensuremath{([\\w\\\\]+?)}"),wxRE_ADVANCED);
  ensuremath.Replace(&retval,"\\1");

  // removeWhiteSpaceAroundBraces.Replace(&retval,"\\1");
  wxLogMessage("A " + retval);
  // END PART A
  // wxLogMessage(retval);
  // PART B --HANDLES CASES super  {{str}^{str}} and sub {}_{} scripts

  // intercept braces around single char
  // --
  // wxRegEx bracesAroundSingleLetter(wxT("([^}ct]){([a-zA-Z0-9])}"));
  // bracesAroundSingleLetter.Replace(&retval,"\\1 \\2");
  retval = ProtectedReplace(retval,"{([a-zA-Z0-9])}","\\1");
  // removeWhiteSpaceAroundBraces.Replace(&retval,"\\1");
  wxLogMessage("B " + retval);
  // {str}^{str}->str^{str}
  // wxRegEx bracesAroundBasePower("([^}ct]{0,1}){([^}{]+?)}[\\^_]",wxRE_ADVANCED);
  // bracesAroundBasePower.Replace(&retval,"\\1 \\2^");
  retval = ProtectedReplace(retval,wxT("{([^}{\\(\\))]+?)}[\\^_]"),"\\1^");
  // removeWhiteSpaceAroundBraces.Replace(&retval,"\\1");
  wxLogMessage("C " + retval);
  // intercepts {a^b}
  // wxRegEx bracesAroundCharPower("([^}ct]{0,1}){(\\w[\\^_]\\w)}",wxRE_ADVANCED);
  // bracesAroundCharPower.Replace(&retval,"\\1 \\2");
  retval = ProtectedReplace(retval,wxT("{(\\w[\\^_]\\w)}"),"\\1");
  // removeWhiteSpaceAroundBraces.Replace(&retval,"\\1");
  wxLogMessage("D " + retval);
  // intercepts {a^{str}} , but not \sqrt{a^b}, or \frac{a^b}{c}
  // wxRegEx bracesAroundStrPower("([^}ct]{0,1}){([^}{]+?[\\^_]{[^}{]+?})}",wxRE_ADVANCED);
  // bracesAroundStrPower.Replace(&retval,"\\1 \\2");
  retval = ProtectedReplace(retval,wxT("{([^}{]+?[\\^_]{[^}{]+?})}"),"\\1");
  // removeWhiteSpaceAroundBraces.Replace(&retval,"\\1");
  // END PART B
  // wxLogMessage(retval);
  // PART C --HANDLES Parenteresis as in \left ..\right..
  // intercepts {\left(..\right)} -- protected if begins with c{..} or }{} as would be in \frac{..}{..}
  wxLogMessage("E " + retval);
  wxString tmp;
  do{
    tmp=retval;
    retval = ProtectedReplace(retval,"{(\\\\left.+?\\\\right.)}","\\1");
    // retval = ProtectedReplace(retval,wxT("{(\\\\left\\|.+?\\\\right\\|)}"),"\\1");
    // removeWhiteSpaceAroundBraces.Replace(&retval,"\\1");

    // intercepts {\left(..\right)^{str}} -- protected if begins with c{..} or }{} as would be in \frac{..}{..}
    // wxRegEx bracesAroundParenthesisPower(wxT("([^}c]{0,1}){(\\\\left\\(.+?\\\\right\\)[\\^_]{[^}{]+?})}"),wxRE_ADVANCED);
    // bracesAroundParenthesisPower.Replace(&retval,"\\1 \\2");
    retval = ProtectedReplace(retval,wxT("{(\\\\left\\(.+?\\\\right\\)[\\^_]{[^}{]+?})}"),"\\1");
    // removeWhiteSpaceAroundBraces.Replace(&retval,"\\1");
  }
  while (retval!=tmp);
  wxLogMessage("--> "+retval);
  // TODO: handle \left[ \right], or || etc
  return retval;
}
