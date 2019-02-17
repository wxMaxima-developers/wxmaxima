#include "wxMathml.h"
#include "../data/wxMathML.h"
#include <iostream>
#include <wx/wx.h>
#include <wx/mstream.h>
#include <wx/zstream.h>
#include <wx/txtstrm.h>
#include <wx/string.h>

wxMathML::wxMathML()
{
  // Unzip wxMathml.lisp: We need to store it in a .zip format
  // in order to avoid a bug in the ArchLinux C compiler that
  // seems to replace long strings by a "\0".
  wxMemoryInputStream istream(wxMathML_lisp_gz, wxMathML_lisp_gz_len);

  wxZlibInputStream zstream(istream);
  wxTextInputStream textIn(zstream);
  wxString line;
  m_wxMathML = wxEmptyString;
  
  while(!istream.Eof())
  {
    line = textIn.ReadLine();
    m_wxMathML += line + wxT("\n");
  }
  wxASSERT_MSG(m_wxMathML.Length()>64000,_("Compiler-Bug? wxMathml.lisp is shorter than expected!"));
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
  wxASSERT_MSG(cmd.Length()>58000,_("Bug: After removing the whitespace wxMathml.lisp is shorter than expected!"));
  return wxT(":lisp-quiet ") + cmd + "\n";
}
