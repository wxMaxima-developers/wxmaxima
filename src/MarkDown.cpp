// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2015      Gunter Königsmann <wxMaxima@physikbuch.de>
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

#include "MarkDown.h"

MarkDownParser::~MarkDownParser()
{
  while(!regexReplaceList.empty())
  {
    delete regexReplaceList.front();
    regexReplaceList.pop_front();
  }
}

MarkDownParser :: MarkDownParser()
{
  m_flowedTextRequested = true;
  wxConfig::Get()->Read(wxT("flowedTextRequested"), &m_flowedTextRequested);
}
  
wxString MarkDownParser::MarkDown(wxString str)
{
  wxString result=wxEmptyString;
  std::list <int> indentationLevels;
  bool addNewline = false;
  wxString line;

  while(str != wxEmptyString)
    {

      // Extract a line from the string.
      int newLinePos = str.find(NewLine());
      if(newLinePos == wxNOT_FOUND)
	{
	  line = str;
	  str  = wxEmptyString;
	}
      else
	{
	  line = str.Left(newLinePos);
	  str  = str.Right(str.Length() - newLinePos - NewLine().Length());
	}

      for(replaceList::iterator it=regexReplaceList.begin();
          it!=regexReplaceList.end();
          ++it)
        (*it)->DoReplace(&line);
  
      int index=0;
      while((index<line.Length()) && (line[index] == wxT(' ')))
	index++;

      // Does the line contain anything other than spaces?
      if(index<line.Length())
	{
          // The line contains anything other than spaces.
          
	  // If the line begins with a star followed by a space it is part
	  // of a bullet list
	  if(line[index] == wxT('*') && (line[index + 1] == wxT(' ')) )
	    {
	      // We are part of a bullet list. Let's see if this is the first
	      // item and we need to start a new list.
	      if(indentationLevels.empty())
		{
		  result += itemizeBegin()+itemizeItem();
		  indentationLevels.push_back(index );
		}
	      else
		{
		  // We are inside a bullet list. Are we at a new indentation
		  // level?
		  if(indentationLevels.back()<index)
		    {
		      result += itemizeEndItem() + itemizeBegin();
		      indentationLevels.push_back(index);
		    }

		  // End lists if we are at a old indentation level.
		  while(indentationLevels.back() > index)
		    {
		      result += itemizeEnd();
		      indentationLevels.pop_back();
		    }

		  // Add a new item.
		  result += itemizeEndItem() + itemizeItem();
		}
	      result += line.Right(line.Length() - index - 1);
              addNewline = false;
	    }
	  else
	    {
	      // Ordinary text.
	      //
	      // If we are at a old indentation level we need to end some lists
	      // and add a new item if we still are inside a list.
              if(!indentationLevels.empty())
              {
                if(indentationLevels.back() > index)
		{
		  if(NewLineBreaksLine() && !m_flowedTextRequested)
		    addNewline = false;
		  result += itemizeEndItem();
		  while((!indentationLevels.empty())&&
			(indentationLevels.back()>index))
                  {
                    result += itemizeEnd();
                    indentationLevels.pop_back();
                  }
		  if(!indentationLevels.empty()) result += itemizeItem();
		}
                line = line.Right(line.Length() - index);
              }
              
	      // Add the text to the output.
	      if (addNewline)
                result += NewLine();
              else
                result += " ";
              
	      result += line;
            }
	}
      else
	{
	  if(addNewline) result += NewLine();
	  result += line;
          addNewline = true;
	}
    }

  // Close all item lists
  while(!indentationLevels.empty())
    {
      result += itemizeEnd();
      indentationLevels.pop_back();
    }
  
  return result;
}
  
MarkDownTeX::MarkDownTeX() : MarkDownParser()
{
  regexReplaceList.push_back(
    new RegexReplacer(wxT("\\\\verb\\|<\\|=\\\\verb\\|>\\|"),wxT("\\\\ensuremath{\\\\Longleftrightarrow}")));
  regexReplaceList.push_back(
    new RegexReplacer(wxT("=\\\\verb\\|>\\|"),wxT("\\\\ensuremath{\\\\Longrightarrow}")));
  regexReplaceList.push_back(
    new RegexReplacer(wxT("\\\\verb\\|<\\|-\\\\verb\\|>\\|"),wxT("\\\\ensuremath{\\\\longleftrightarrow}")));
  regexReplaceList.push_back(
    new RegexReplacer(wxT("-\\\\verb\\|>\\|"),wxT("\\\\ensuremath{\\\\longrightarrow}")));
  regexReplaceList.push_back(
    new RegexReplacer(wxT("\\\\verb\\|<\\|-"),wxT("\\\\ensuremath{\\\\longleftarrow}")));
  regexReplaceList.push_back(
    new RegexReplacer(wxT("\\\\verb\\|<\\|="),wxT("\\\\ensuremath{\\\\leq}")));
  regexReplaceList.push_back(
    new RegexReplacer(wxT("\\\\verb\\|>\\|="),wxT("\\\\ensuremath{\\\\geq}")));
  regexReplaceList.push_back(
    new RegexReplacer(wxT("\\+/-"),wxT("\\\\ensuremath{\\\\pm}")));
}

MarkDownHTML::MarkDownHTML() : MarkDownParser()
{
  regexReplaceList.push_back(
    new RegexReplacer(wxT("\\&lt;=\\&gt;"),wxT("\\&hArr;")));
  regexReplaceList.push_back(
    new RegexReplacer(wxT("=\\&gt;"),wxT("\\&rArr;")));
  regexReplaceList.push_back(
    new RegexReplacer(wxT("&lt;-\\&gt;"),wxT("\\&harr;")));
  regexReplaceList.push_back(
    new RegexReplacer(wxT("-\\&gt;"),wxT("\\&rarr;")));
  regexReplaceList.push_back(
    new RegexReplacer(wxT("\\&lt;-"),wxT("\\&larr;")));
  regexReplaceList.push_back(
    new RegexReplacer(wxT("\\&lt;="),wxT("\\&le;")));
  regexReplaceList.push_back(
    new RegexReplacer(wxT("\\&gt;="),wxT("\\&ge;")));;
  regexReplaceList.push_back(
    new RegexReplacer(wxT("\\+/-"),wxT("\\&plusmn;")));;
}
