#include "wxMathml.h"
#include <iostream>
#include <wx/wx.h>
#include <wx/mstream.h>
#include <wx/txtstrm.h>
#include <wx/wfstream.h>
#include <wx/string.h>

wxMathML::wxMathML(Configuration *config):
  m_configuration(config)
{
  m_wxMathML_UseFile = m_configuration->WxMathML_UseFile();

}

wxString wxMathML::GetCmd()
{
  // If we read wxMathML from a file we should read it anew, just in case
  // the file has changed.
  // If we have transitioned from using a file to using the internal
  // data we read the info anew, instead, as it might differ from the file.
  if(m_configuration->WxMathML_UseFile() || m_wxMathML_UseFile)
    m_maximaCMD = wxEmptyString;
  m_wxMathML_UseFile = m_configuration->WxMathML_UseFile();

  if(m_maximaCMD.IsEmpty())
    {
      if(!m_configuration->WxMathML_UseFile())
	{
	  wxLogMessage(_("Reading the Lisp part of wxMaxima from the included header file."));
	  wxMemoryInputStream istream(WXMATHML_LISP, WXMATHML_LISP_SIZE);
	  wxTextInputStream textIn(istream);
	  wxString line;

	  while(!istream.Eof())
	    {
	      line = textIn.ReadLine();
	      m_wxMathML += line + wxT("\n");
	    }
	  wxASSERT_MSG(m_wxMathML.Length()>64000,_("Compiler-Bug? wxMathml.lisp is shorter than expected!"));
	}
      else
	{
	  wxLogMessage(wxString::Format(_("Reading the Lisp part of wxMaxima from the file %s"),
					m_configuration->WxMathML_Filename().c_str()));
	  wxFileInputStream input(m_configuration->WxMathML_Filename());
	  wxTextInputStream textIn(input);
	  wxString line;

	  while(!input.Eof())
	    {
	      line = textIn.ReadLine();
	      m_wxMathML += line + wxT("\n");
	    }
	}

    }
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
  if(!m_configuration->WxMathML_UseFile())
    wxASSERT_MSG(m_maximaCMD.Length()>54000,_("Bug: After removing the whitespace wxMathml.lisp is shorter than expected!"));
  m_maximaCMD = wxT(":lisp-quiet ") + m_maximaCMD + "\n";

  return m_maximaCMD;
}
wxString wxMathML::m_maximaCMD;
