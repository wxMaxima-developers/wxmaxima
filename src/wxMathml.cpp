#include "wxMathml.h"
#include "../data/wxMathML.h"
#include <iostream>

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
    wxChar lastChar=wxT(' ');
    wxString::iterator ch = line.begin();
    while (ch < line.end())
    {
      // Remove formatting spaces
      if(((lastChar == ' ') && (*ch == ' ')) && (!stringIs))
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
  return wxT(":lisp-quiet ") + cmd + "\n";
}
