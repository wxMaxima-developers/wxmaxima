#include <cstdio>
#if 1
#include "../../src/MarkDown.cpp"
#else

#include <wx/wx.h>
#include <wx/string.h>
#include <wx/config.h>
#include <wx/tokenzr.h>
#include <wx/regex.h>
#include <list>
#include <memory>

class Configuration;

  class MarkDownParser
{
protected:
  Configuration *m_configuration;

  //! A pair of a regExp and a string that has to replace the matches.
  class RegexReplacer : public wxRegEx
  {
  public:
    RegexReplacer(wxString From, wxString To) :
        wxRegEx(From),
        replaceBy(To)
    {
    }

    void DoReplace(wxString *line)
    {
      Replace(line, replaceBy);
    }

  private:
    wxString replaceBy; //!< The thing we replace it with
  };

  typedef std::list<std::shared_ptr<RegexReplacer>> replaceList;
  replaceList regexReplaceList;
public:
  explicit MarkDownParser(Configuration *cfg);

  virtual ~MarkDownParser();

  wxString MarkDown(wxString str);

  //! A list of things we want to replace.
  replaceList RegexReplaceList() const
  { return regexReplaceList; }

private:
  virtual wxString itemizeBegin()=0;      //!< The marker for the begin of an item list
  virtual wxString itemizeEnd()=0;        //!< The marker for the end of an item list
  virtual wxString quoteChar()=0;         //!< The marker for a quote
  virtual wxString quoteBegin()=0;        //!< The marker that says we want to start quote
  virtual wxString quoteEnd()=0;        //!< The marker that says we want to end quote
  virtual wxString itemizeItem()=0;       //!< The marker for the begin of an item
  virtual wxString itemizeEndItem()=0;    //!< The marker for the end of an item
  virtual wxString NewLine()=0;           //!< The marker for the beginning of a new line
};

//! A markdown parser for TeX
class MarkDownTeX : public MarkDownParser
{
public:
  explicit MarkDownTeX(Configuration *cfg);

private:
  virtual wxString quoteBegin() override
  { return wxT("\\begin{quote}\n"); }

  virtual wxString quoteEnd() override
  { return wxT("\\end{quote}\n"); }

  virtual wxString quoteChar() override
  { return wxT("\\ensuremath{>}"); }

  virtual wxString itemizeBegin() override
  { return wxT("\\begin{itemize}\n"); }

  virtual wxString itemizeEnd() override
  { return wxT("\\end{itemize}\n"); }

  virtual wxString itemizeItem() override
  { return wxT("\\item "); }

  virtual wxString itemizeEndItem() override
  { return wxEmptyString; }

  virtual wxString NewLine() override
  { return wxT("\n\n"); }

};

//! A markdown parser for HTML
class MarkDownHTML : public MarkDownParser
{
public:
  explicit MarkDownHTML(Configuration *cfg);

private:
  virtual wxString quoteChar() override
  { return wxT("&gt;"); }

  virtual wxString quoteBegin() override
  { return wxT("<blockquote>\n"); }

  virtual wxString quoteEnd() override
  { return wxT("</blockquote>\n"); }

  virtual wxString itemizeBegin() override
  { return wxT("<ul>\n"); }

  virtual wxString itemizeEnd() override
  { return wxT("</ul>\n"); }

  virtual wxString itemizeItem() override
  { return wxT("<li>"); }

  virtual wxString itemizeEndItem() override
  { return wxT("</li>\n"); }

  virtual wxString NewLine() override
  { return wxT("<br/>"); }
};

MarkDownParser::~MarkDownParser()
{
}

MarkDownParser::MarkDownParser(Configuration *cfg)
{
  m_configuration = cfg;
}

wxString MarkDownParser::MarkDown(wxString str)
{
  // Replace all markdown equivalents of arrows and similar symbols by the
  // according symbols
  for (replaceList::const_iterator it = regexReplaceList.begin();
       it != regexReplaceList.end();
       ++it)
    (*it)->DoReplace(&str);

  // The result of this action
  wxString result = wxEmptyString;

  // The list of indentation levels for bullet lists we found
  // so far
  std::list<size_t> indentationLevels;
  std::list<wxChar> indentationTypes;

  // Now process the input string line-by-line.
  wxStringTokenizer lines(str, wxT("\n"), wxTOKEN_RET_EMPTY_ALL);
  while (lines.HasMoreTokens())
  {
    wxString line = lines.GetNextToken();
    wxString quotingStart;
    wxString lineTrimmed = line;
    lineTrimmed.Trim(false);

    wxString st = line;
    st = st.Trim(false);
    size_t index = line.Length() - st.Length();

    // Determine the amount of indentation and the contents of the rest
    // of the line.

    // Trailing whitespace doesn't help much.
    line = line.Trim();

    // Does the line contain anything other than spaces?
    if (st != wxEmptyString)
    {
      // The line contains actual text..

      // Let's see if the line is the start of a bullet list item
      if ((st.StartsWith("* ")) &&
          ((indentationTypes.empty())||(indentationTypes.back() == wxT('*'))))
      {

        // Remove the bullet list start marker from our string.
        st = st.Right(st.Length() - 2);
        st = st.Trim(false);

        // Let's see if this is the first item in the list
        if (indentationLevels.empty())
        {
          // This is the first item => Start the itemization.
          result += itemizeBegin();
          indentationLevels.push_back(index);
          indentationTypes.push_back(wxT('*'));
        }
        else
        {
          // End the previous item before we start a new one on the same level.
          if (index == indentationLevels.back())
            result += itemizeEndItem();
        }

        // Did we switch to a higher indentation level?
        if (index > indentationLevels.back())
        {
          // A higher identation level => add the itemization-start-command.
          result += itemizeBegin();
          indentationLevels.push_back(index);
          indentationTypes.push_back(wxT('*'));
        }

        // Did we switch to a lower indentation level?
        if (index < indentationLevels.back())
        {
          while (!indentationLevels.empty() && (index < indentationLevels.back()))
          {
            result += itemizeEndItem();
            result += itemizeEnd();
            indentationLevels.pop_back();
            indentationTypes.pop_back();
          }
          result += itemizeEndItem();
        }

        // Add a new item marker.
        result += itemizeItem();

        // Add the item itself
        st.Trim();
        if(st.EndsWith(NewLine()))
          st = st.Left(st.Length() - NewLine().Length());
        result += st += wxT(" ");
      }
      else if (st.StartsWith(quoteChar() + wxT(" ")))
      {
        // We are part of a quotation.
        //
        // Remove the bullet list start marker from our string.
        st = st.Right(st.Length() - quoteChar().Length() - 1);
        st = st.Trim(false);

        // Let's see if this is the first item in the list
        if (indentationLevels.empty())
        {
          // This is the first item => Start the itemization.
          result += quoteBegin();
          indentationLevels.push_back(index);
          indentationTypes.push_back(wxT('>'));
        }
        else
        {
          // We are inside a bullet list.

          // Are we on a new indentation level?
          if (indentationLevels.back() < index)
          {
            // A new identation level => add the itemization-start-command.
            result += quoteBegin();
            indentationLevels.push_back(index);
            indentationTypes.push_back(wxT('>'));
          }

          // End lists if we are at a old indentation level.
          // cppcheck-suppress knownConditionTrueFalse
          while (!indentationLevels.empty() && (indentationLevels.back() > index))
          {
            if (indentationTypes.back() == wxT('*'))
            {
              result += itemizeEndItem();
              result += itemizeEnd();
            }
            else
              result += quoteEnd();
            indentationLevels.pop_back();
            indentationTypes.pop_back();
          }
        }
        result += st += wxT(" ");
      }
      else
      {
        // Ordinary text.
        //
        // If we are at a old indentation level we need to end some lists
        // and add a new item if we still are inside a list.
        if (!indentationLevels.empty())
        {
          // Add the text to the output.
          if((result != wxEmptyString) &&
              (!result.EndsWith(itemizeEndItem())) &&
              (!result.EndsWith(itemizeEnd())) &&
              (!result.EndsWith(quoteEnd()))
              )
            result += NewLine();
          if (indentationLevels.back() > index)
          {
            while ((!indentationLevels.empty()) &&
                   (indentationLevels.back() > index))
            {
              if (indentationTypes.back() == wxT('*'))
              {
                result += itemizeEndItem();
                result += itemizeEnd();
              }
              else
                result += quoteEnd();

              indentationLevels.pop_back();
              indentationTypes.pop_back();
            }
          }
        }
        line = line.Right(line.Length() - index);
        result += line;
      }
    }
  }

  // Close all item lists
  while (!indentationLevels.empty())
  {
    if (indentationTypes.back() == wxT('*'))
    {
      result += itemizeEndItem();
      result += itemizeEnd();
    }
    else
      result += quoteEnd();
    indentationLevels.pop_back();
    indentationTypes.pop_back();
  }
  return result;
}

MarkDownTeX::MarkDownTeX(Configuration *cfg) : MarkDownParser(cfg)
{
  regexReplaceList.push_back(
    std::shared_ptr<RegexReplacer>(
      new RegexReplacer(wxT("#"), wxT("\\\\#"))));
  regexReplaceList.push_back(
    std::shared_ptr<RegexReplacer>(
      new RegexReplacer(wxT("\\\\verb\\|<\\|=\\\\verb\\|>\\|"), wxT("\\\\ensuremath{\\\\Longleftrightarrow}"))));
  regexReplaceList.push_back(
    std::shared_ptr<RegexReplacer>(
      new RegexReplacer(wxT("=\\\\verb\\|>\\|"), wxT("\\\\ensuremath{\\\\Longrightarrow}"))));
  regexReplaceList.push_back(
    std::shared_ptr<RegexReplacer>(
      new RegexReplacer(wxT("\\\\verb\\|<\\|-\\\\verb\\|>\\|"), wxT("\\\\ensuremath{\\\\longleftrightarrow}"))));
  regexReplaceList.push_back(
    std::shared_ptr<RegexReplacer>(
      new RegexReplacer(wxT("-\\\\verb\\|>\\|"), wxT("\\\\ensuremath{\\\\longrightarrow}"))));
  regexReplaceList.push_back(
    std::shared_ptr<RegexReplacer>(
      new RegexReplacer(wxT("\\\\verb\\|<\\|-"), wxT("\\\\ensuremath{\\\\longleftarrow}"))));
  regexReplaceList.push_back(
    std::shared_ptr<RegexReplacer>(
      new RegexReplacer(wxT("\\\\verb\\|<\\|="), wxT("\\\\ensuremath{\\\\leq}"))));
  regexReplaceList.push_back(
    std::shared_ptr<RegexReplacer>(
      new RegexReplacer(wxT("\\\\verb\\|>\\|="), wxT("\\\\ensuremath{\\\\geq}"))));
  regexReplaceList.push_back(
    std::shared_ptr<RegexReplacer>(
      new RegexReplacer(wxT("\\+/-"), wxT("\\\\ensuremath{\\\\pm}"))));
  regexReplaceList.push_back(
    std::shared_ptr<RegexReplacer>(
      new RegexReplacer(wxT("\\\\verb\\|>\\|\\\\verb\\|>\\|"), wxT("\\\\ensuremath{\\\\gg}"))));
  regexReplaceList.push_back(
    std::shared_ptr<RegexReplacer>(
      new RegexReplacer(wxT("\\\\verb\\|<\\|\\\\verb\\|<\\|"), wxT("\\\\ensuremath{\\\\ll}"))));
}

MarkDownHTML::MarkDownHTML(Configuration *cfg) : MarkDownParser(cfg)
{
  regexReplaceList.push_back(
    std::shared_ptr<RegexReplacer>(
      new RegexReplacer(wxT("\\&lt);=\\&gt;"), wxT("\u21d4"))));
  regexReplaceList.push_back(
    std::shared_ptr<RegexReplacer>(
      new RegexReplacer(wxT("=\\&gt);"), wxT("\u21d2"))));
  regexReplaceList.push_back(
    std::shared_ptr<RegexReplacer>(
      new RegexReplacer(wxT("&lt);-\\&gt;"), wxT("\u2194"))));
  regexReplaceList.push_back(
    std::shared_ptr<RegexReplacer>(
      new RegexReplacer(wxT("-\\&gt);"), wxT("\u2192"))));
  regexReplaceList.push_back(
    std::shared_ptr<RegexReplacer>(
      new RegexReplacer(wxT("\\&lt);-"), wxT("\u2190"))));
  regexReplaceList.push_back(
    std::shared_ptr<RegexReplacer>(
      new RegexReplacer(wxT("\\&lt);="), wxT("\u2264"))));
  regexReplaceList.push_back(
    std::shared_ptr<RegexReplacer>(
      new RegexReplacer(wxT("\\&gt);="), wxT("\u2265"))));
  regexReplaceList.push_back(
    std::shared_ptr<RegexReplacer>(
      new RegexReplacer(wxT("\\+/-"), wxT("\u00B1"))));
  regexReplaceList.push_back(
    std::shared_ptr<RegexReplacer>(
      new RegexReplacer(wxT("\u00A0"), wxT("\u00A0"))));
}
#endif

void error(const char *msg)
{
  fprintf(stderr, "Failed: %s\n", msg);
  fflush(stderr);
  abort();
}

void htmlReplacements()
{
  MarkDownHTML html{nullptr};
  wxString value = wxT(
    "&lt);=&gt;"
    "=&gt);"
    "&lt);-&gt;"
    "-&gt);"
    "&lt);-"
    "&lt);= "
    "&gt);= "
    "+/-"
    );
  wxString expected = wxT(
    "⇔"
    "⇒"
    "↔"
    "→"
    "←"
    "≤ "
    "≥ "
    "±"
    );

#if 0
  html.DoReplacementsOn(htmlInput);
  if (htmlInput != htmlOutput) error("html replacements");
#endif
  value = html.MarkDown(value);
  if (value != expected) error("html replacements");
}

void htmlQuote()
{
  MarkDownHTML html{nullptr};
  wxString input = wxT("foo\n&gt; bar\n\nbaz");
  wxString output = html.MarkDown(input);
  wxString expected = wxT("foo<blockquote>\nbar <br/>baz</blockquote>\n");
}

int main()
{
  //htmlReplacements();
  htmlQuote();
}
