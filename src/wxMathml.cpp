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
  if(m_maximaCMD.IsEmpty())
    {
      // Unzip wxMathml.lisp: We need to store it in a .zip format
      // in order to avoid a bug in the ArchLinux C compiler that
      // seems to replace long strings by a "\0".
      wxMemoryInputStream istream(wxMathML_lisp_gz, wxMathML_lisp_gz_len);
      
      wxZlibInputStream zstream(istream);
      wxTextInputStream textIn(zstream);
      wxString line;
      
      while(!istream.Eof())
	{
	  line = textIn.ReadLine();
	  m_wxMathML += line + wxT("\n");
	}
      wxASSERT_MSG(m_wxMathML.Length()>64000,_("Compiler-Bug? wxMathml.lisp is shorter than expected!"));
    }
}

wxString wxMathML::GetCmd()
{
  if(m_maximaCMD.IsEmpty())
    {
      wxStringTokenizer lines(m_wxMathML,wxT("\n"));
      while(lines.HasMoreTokens())
	{
	  wxString line = lines.GetNextToken();
	  wxString lineWithoutComments;

	  bool stringIs = false;
	  wxChar lastChar = wxT('\n');
	  wxString::const_iterator ch = line.begin();
	  while (ch < line.end())
	    {
	      // Remove formatting spaces
	      if(((lastChar == '\n') && ((*ch == ' ') || (*ch == '\t'))))
		++ch;
	      else
		{
		  // Handle backslashes that might escape double quotes
		  if (*ch == wxT('\\'))
		    {
		      lineWithoutComments += *ch;
		      ++ch;
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
		  ++ch;
		}
	    }
	  m_maximaCMD += lineWithoutComments + " ";
	}
      wxASSERT_MSG(m_maximaCMD.Length()>54000,_("Bug: After removing the whitespace wxMathml.lisp is shorter than expected!"));
      m_maximaCMD = wxT(":lisp-quiet ") + m_maximaCMD + "\n";
    }
  return m_maximaCMD;
}

wxString wxMathML::m_maximaCMD;
