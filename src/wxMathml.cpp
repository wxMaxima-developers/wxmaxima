#include "wxMathml.h"
#include "../data/wxMathML.h"
#include <iostream>
#include <wx/wx.h>
#include <wx/string.h>

wxMathML::wxMathML()
{ 
  m_wxMathML = wxString(wxMathML_lisp, wxMathML_lisp_len);
}

wxString wxMathML::GetCmd()
{
  wxString cmd;
  
  wxStringTokenizer lines(m_wxMathML,wxT("\n"));
  while(lines.HasMoreTokens())
  {
    wxString line = lines.GetNextToken();
    wxString lineWithoutComments;

    bool stringIs = false;
    wxChar lastChar = wxT('\n');
    wxString::iterator ch = line.begin();
    while (ch < line.end())
    {
      // Remove formatting spaces
      if(((lastChar == '\n') && ((*ch == ' ') || (*ch == '\t'))))
	  ch++;
      else
	{
	  // Handle backslashes that might escape double quotes
	  if (*ch == wxT('\\'))
	    {
	      lineWithoutComments += *ch;
	      lastChar = *ch;
	      ch++;
	    }
	  else
	    {
	      // Handle strings
	      if (*ch == wxT('\"'))
		stringIs = !stringIs;

	      // Handle comments
	      if ((*ch == wxT(';')) && (!stringIs))
		break;
	    }
	  lineWithoutComments += *ch;
	  lastChar = *ch;
	  ch++;
	}
    }
    cmd += lineWithoutComments + " ";
  }
  std::cerr<<cmd.Length();
  wxASSERT_MSG(cmd.Length()>58000,_("Compiler-Bug? wxMathml.lisp is shorter than expected!"));
  return wxT(":lisp-quiet ") + cmd + "\n";
}
