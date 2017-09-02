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

// #define RXDBG

wxString ProtectedReplace(wxString str, wxString expr, wxString replacement,wxString watchout="")
{
    // This implements a sort of look behind
    // to protect some keyword or context from being destroyed

    wxString protection = "([\\\\frac|\\\\sqrt|}"+watchout+"])";
    wxRegEx re(protection+expr,wxRE_ADVANCED);
    wxRegEx re_go(expr,wxRE_ADVANCED);

    re.IsValid() && re.Matches(str);
    bool doit = true;
    wxString kw  = re.GetMatch(str,1);
    #ifdef RXDBG
        wxLogMessage(":"+kw+":");
    #endif

    doit = (kw.length()==0) && doit;
    // doit = doit || (keywords.find(kw) == wxNOT_FOUND);

    if (doit)
    {
        #ifdef RXDBG
            wxLogMessage("inside");
        #endif
        re_go.Replace(&str,replacement);
        wxRegEx spaces("\\s+([\\}\\{\\)\\(_^])",wxRE_ADVANCED);
        spaces.Replace(&str,"\\1");
        wxRegEx spaces2("([\\}\\{\\)\\(_^])\\s+",wxRE_ADVANCED);
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
  // It's not exaclty ECMAScript compatible, so when wxwidgets will remove support
  // for their/this regex.h, there will be some adjustments to use the C++11
  // regex implementation.
  wxString retval = str;
  #ifdef RXDBG
    wxLogMessage("0 " + retval);
  #endif
  // PART A --IMHO useless keywords
  wxRegEx mathit(wxT("\\\\mathit{(\\w+?)}"),wxRE_ADVANCED);
  mathit.Replace(&retval,"\\1");
  wxRegEx ensuremath(wxT("\\\\ensuremath{([\\w\\\\]+?)}"),wxRE_ADVANCED);
  ensuremath.Replace(&retval,"\\1");
  #ifdef RXDBG
    wxLogMessage("A " + retval);
  #endif
  // PART B --HANDLES CASES super  {{str}^{str}} and sub {}_{} scripts
  // intercept braces around single char
  retval = ProtectedReplace(retval,"{([a-zA-Z0-9])}"," \\1 ");
  #ifdef RXDBG
    wxLogMessage("B " + retval);
  #endif
  // {str}^{str}->str^{str}
  // {([^}{\\(\\)]+?)}[\\^_]
  retval = ProtectedReplace(retval,wxT("{([^}{\\(\\)]+?)}[\\^]"),"\\1^","_");
  retval = ProtectedReplace(retval,wxT("{([^}{\\(\\)]+?)}[\\_]"),"\\1_","^");
  // match single char again (is there some sort of unpredictable behavior??)
  retval = ProtectedReplace(retval,wxT("{([^}{\\(\\)])}[\\^]"),"\\1^");
  retval = ProtectedReplace(retval,wxT("{([^}{\\(\\)])}[\\_]"),"\\1_");
  #ifdef RXDBG
    wxLogMessage("C " + retval);
  #endif
  // intercepts {a^b}
  retval = ProtectedReplace(retval,wxT("{(\\w[\\^_]\\w)}"),"\\1");
  #ifdef RXDBG
    wxLogMessage("D " + retval);
  #endif
  // intercepts {a^{str}} , but not \sqrt{a^b}, or \frac{a^b}{c}
  retval = ProtectedReplace(retval,wxT("{([^}{]+?[\\^_]{[^}{]+?})}"),"\\1");
  // END PART B

  // PART C --HANDLES Parenteresis as in \left ..\right..
  #ifdef RXDBG
   wxLogMessage("E " + retval);
  #endif
  wxString tmp;
  int countloop=0;
  do{
    tmp=retval;
    // intercepts {\left(..\right)} -- protected if begins with c{..} or }{} as would be in \frac{..}{..}
    retval = ProtectedReplace(retval,"{(\\\\left.+?\\\\right.)}","\\1");
    retval = ProtectedReplace(retval,"{(\\\\\\w+?\\\\left.+?\\\\right.)}","\\1");
    // intercepts {\left(..\right)^{str}} -- protected if begins with c{..} or }{} as would be in \frac{..}{..}
    retval = ProtectedReplace(retval,wxT("{(\\\\left\\(.+?\\\\right\\)[\\^_]{[^}{]+?})}"),"\\1");
    countloop+=1;
  }
  while ((retval!=tmp) && countloop < 2);
  #ifdef RXDBG
    wxLogMessage("--> "+retval);
  #endif
  return retval;
}
