// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2012-2013 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2015-2026 Gunter Königsmann <wxMaxima@physikbuch.de>
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
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
//
//  SPDX-License-Identifier: GPL-2.0+

/*! \file
  The document serializers declared in WorksheetExport.h.

  The code was moved verbatim out of Worksheet.cpp; see the header for why it
  lives here. test_WorksheetExport pins the byte-exact output.
*/

#include "WorksheetExport.h"
#include "Configuration.h"
#include "MarkDown.h"
#include "Version.h"
#include "WXMformat.h"
#include "WXMXformat.h"
#include "cells/AnimationCell.h"
#include "cells/CellList.h"
#include "cells/EditorCell.h"
#include "cells/GroupCell.h"
#include "cells/ImgCellBase.h"
#include "graphical_io/BitmapOut.h"
#include "graphical_io/SVGout.h"
#include <wx/config.h>
#include <wx/filefn.h>
#include <wx/filename.h>
#include <wx/log.h>
#include <wx/mstream.h>
#include <wx/tokenzr.h>
#include <wx/txtstrm.h>
#include <wx/uri.h>
#include <wx/wfstream.h>
#include <wx/xml/xml.h>

namespace {

//! Returns the index in (%i...) or (%o...)
int GetCellIndex(Cell *cell) {
  if (!cell)
    return -1;
  wxString strindex = cell->ToString().Trim(); //(%i...)
  long temp;
  if (!strindex.Mid(3, strindex.Len() - 4).ToLong(&temp))
    return -1;
  return temp;
}

//! Simple iterator over a Maxima input string, skipping comments and strings
class SimpleMathConfigurationIterator {
public:
  explicit SimpleMathConfigurationIterator(const wxString &ainput)
    : pos(0), input(ainput) {
    if (isValid() && (input.at(0) == '"' || (input.at(0) == '/' && input.length() > 1 &&
                                             input.at(1) == '*'))) {
      // skip strings or comments at string start
      pos--;
      ++(*this);
    }
  }

  void operator++() {
    unsigned int oldpos = pos;
    pos++;
    while (pos < input.length() && oldpos != pos) {
      oldpos = pos;
      if (input.at(pos) == '"') { // skip strings
        pos++;                 // skip leading "
        while (pos < input.length() && input.at(pos) != '"')
          pos++;
        pos++; // skip trailing "
      }
      if (pos + 1 < input.length() && input.at(pos) == '/' &&
          input.at(pos + 1) == '*') { // skip comments
        pos += 2;                  // skip /*
        while (pos < input.length() &&
               (input.at(pos) != '*' || input.at(pos + 1) != '/'))
          pos++;
        pos += 2; // skip */
      }
    }
  }

  bool isValid() const
    { return pos < input.length(); }

  inline wxChar operator*() const
    { return input.at(pos); }

  std::size_t pos;

  //! reference to the input string
  const wxString &input;
};

} // namespace

void WorksheetExport::AddLineToFile(wxTextFile &output, const wxString &s) {
  if (s == wxS("\n") || s == wxEmptyString)
    output.AddLine(wxEmptyString);
  else {
    wxStringTokenizer lines(s, wxS("\n"), wxTOKEN_RET_EMPTY_ALL);
    wxString line;

    while (lines.HasMoreTokens()) {
      line = lines.GetNextToken();
      if (lines.HasMoreTokens() || !line.IsEmpty())
        output.AddLine(line);
    }
  }
}

void WorksheetExport::CalculateReorderedCellIndices(GroupCell *tree, int &cellIndex,
                                                    std::vector<int> &cellMap) {
  for (auto &tmp : OnList(tree)) {
    if (!tmp.IsHidden() && tmp.GetGroupType() == GC_TYPE_CODE) {
      wxString input;
      Cell *prompt = tmp.GetPrompt();
      const Cell *cell = tmp.GetEditable();

      if (cell)
        input = cell->ToString();

      if (prompt && cell && input.Len() > 0) {
        int outputExpressions = 0;
        int initialHiddenExpressions = 0;
        SimpleMathConfigurationIterator it(input);
        for (; it.isValid(); ++it) {
          switch (*it) {
          case '$':
            if (initialHiddenExpressions == outputExpressions)
              initialHiddenExpressions++; // fallthrough
          case ';':
            outputExpressions++;
          }
        }

        long promptIndex = GetCellIndex(prompt);
        long outputIndex =
          GetCellIndex(tmp.GetLabel()) - initialHiddenExpressions;
        if (promptIndex >= 0)
          {
            std::size_t index = promptIndex;
            if (outputIndex < 0 && initialHiddenExpressions < outputExpressions) {
              // input index, but no output index means the expression was
              // evaluated, but produced no result
              //  => it is invalid and should be ignored
              outputExpressions = 0;
            } else if (index + outputExpressions > cellMap.size())
              cellMap.resize(index + outputExpressions);
            for (int i = 0; i < outputExpressions; i++)
              cellMap.at(index + i) = cellIndex + i;
          }

        cellIndex += outputExpressions; // new cell index
      }
    }

    if (tmp.GetHiddenTree())
      CalculateReorderedCellIndices(tmp.GetHiddenTree(), cellIndex, cellMap);
  }
}

void WorksheetExport::ExportToMAC(wxTextFile &output, GroupCell *tree, bool wxm,
                                  const std::vector<int> &cellMap,
                                  bool fixReorderedIndices) {
  //
  // Write contents
  //
  for (auto &tmp : OnList(tree)) {
    AddLineToFile(output, Format::TreeToWXM(&tmp, wxm));

    if (wxm && tmp.GetGroupType() == GC_TYPE_CODE) {
      const EditorCell *txt = tmp.GetEditable();
      if (txt && fixReorderedIndices) {
        wxString input = txt->ToString(true);

        for (SimpleMathConfigurationIterator it =
               SimpleMathConfigurationIterator(input);
             it.pos + 1 < it.input.length(); ++it)
          if (*it == '%' &&
              (input.at(it.pos + 1) == 'i' || input.at(it.pos + 1) == 'o') &&
              (it.pos == 0 || input.at(it.pos - 1) != '%')) {
            it.pos += 2;
            unsigned int startPos = it.pos;
            unsigned int temp = 0;
            for (; it.pos < input.Length() && (*it >= '0' && *it <= '9');
                 ++it.pos)
              temp = temp * 10 + (*it - '0');
            if (temp >= cellMap.size() || cellMap.at(temp) < 1)
              continue;
            wxString tempstr;
            tempstr << cellMap.at(temp);
            input.replace(startPos, it.pos - startPos, tempstr);
            it.pos = startPos + tempstr.length();
          }
      }
    }
  }
}

bool WorksheetExport::ExportToTeX(GroupCell *tree, Configuration *configuration,
                                  const wxString &file) {
  wxString imgDir;
  wxString path, filename, ext;

  wxFileName::SplitPath(file, &path, &filename, &ext);
  imgDir = path + wxS("/") + filename + wxS("_img");
  std::size_t imgCounter = 0;

  wxFileOutputStream outfile(file);
  if (!outfile.IsOk())
    return false;

  wxTextOutputStream output(outfile);

  if (configuration->DocumentclassOptions().IsEmpty())
    output << "\\documentclass{" + configuration->Documentclass() + "}\n\n";
  else
    output << "\\documentclass[" + configuration->DocumentclassOptions() +
      "]{" + configuration->Documentclass() + "}\n\n";
  // cppcheck-suppress unknownMacro
  output << wxS("%% Created with wxMaxima " WXMAXIMA_VERSION "\n\n");
  output << wxS("\\setlength{\\parskip}{\\medskipamount}\n");
  output << wxS("\\setlength{\\parindent}{0pt}\n");
  output << wxS("\\usepackage{iftex}\n");
  output << wxS("\\ifPDFTeX\n");
  output << wxS("  % PDFLaTeX or LaTeX \n");
  output << wxS("  \\usepackage[utf8]{inputenc}\n");
  output << wxS("  \\usepackage[T1]{fontenc}\n");
  output << wxS("  \\DeclareUnicodeCharacter{00B5}{\\ensuremath{\\mu}}\n");
  output << wxS("\\else\n");
  output << wxS("  %  XeLaTeX or LuaLaTeX\n");
  output << wxS("  \\usepackage{fontspec}\n");
  output << wxS("\\fi\n");

  // The following line loads all code needed in order to include graphics.
  output << wxS("\\usepackage{graphicx}\n");
  // We want to color the labels and text cells. The following line adds the
  // necessary logic for this to TeX.
  output << wxS("\\usepackage{color}\n");
  output << wxS("\\usepackage[leqno]{amsmath}\n");

  // We want to shrink pictures the user has included if they are
  // higher or wider than the page.
  output << wxS("\\usepackage{ifthen}\n");
  output << wxS("\\newsavebox{\\picturebox}\n");
  output << wxS("\\newlength{\\pictureboxwidth}\n");
  output << wxS("\\newlength{\\pictureboxheight}\n");
  output << wxS("\\newcommand{\\includeimage}[1]{\n");
  output << wxS("    \\savebox{\\picturebox}{\\includegraphics{#1}}\n");
  output << wxS(
                "    \\settoheight{\\pictureboxheight}{\\usebox{\\picturebox}}\n");
  output << wxS(
                "    \\settowidth{\\pictureboxwidth}{\\usebox{\\picturebox}}\n");
  output << wxS(
                "    \\ifthenelse{\\lengthtest{\\pictureboxwidth > .95\\linewidth}}\n");
  output << wxS("    {\n");
  output << wxS("        "
                "\\includegraphics[width=.95\\linewidth,height=.80\\textheight,"
                "keepaspectratio]{#1}\n");
  output << wxS("    }\n");
  output << wxS("    {\n");
  output << wxS(
                "        "
                "\\ifthenelse{\\lengthtest{\\pictureboxheight>.80\\textheight}}\n");
  output << wxS("        {\n");
  output << wxS("            "
                "\\includegraphics[width=.95\\linewidth,height=.80\\textheight,"
                "keepaspectratio]{#1}\n");
  output << wxS("            \n");
  output << wxS("        }\n");
  output << wxS("        {\n");
  output << wxS("            \\includegraphics{#1}\n");
  output << wxS("        }\n");
  output << wxS("    }\n");
  output << wxS("}\n");
  output << wxS("\\newlength{\\thislabelwidth}\n");

  // Define an "abs" operator for abs commands that are long enough to be broken
  // into lines.
  output << wxS("\\DeclareMathOperator{\\abs}{abs}\n");

  output << wxS("\n");
  output << wxS("\\definecolor{labelcolor}{RGB}{100,0,0}\n");
  output << wxS("\n");

  // Add an eventual preamble requested by the user.
  wxString texPreamble = configuration->TexPreamble();
  if (!texPreamble.IsEmpty())
    output << texPreamble << wxS("\n\n");

  output << wxS("\\begin{document}\n");

  //
  // Write contents
  //
  for (auto &tmp : OnList(tree)) {
    wxString s = tmp.ToTeX(imgDir, filename, &imgCounter);
    output << s << wxS("\n");
  }

  //
  // Close document
  //
  output << wxS("\\end{document}\n");

  bool done = !outfile.GetFile()->Error();
  outfile.Close();

  return done;
}

wxString WorksheetExport::RTFStart(Configuration *configuration) {
  // The beginning of the RTF document
  wxString document = wxS("{\\rtf1\\ansi\\deff0\n\n");

  // The font table
  document += wxS("{\\fonttbl{\\f0\\froman Times;}}\n\n");

  // Define all colors we want to use
  document += wxS("{\\colortbl;\n");
  for (int i = 1; i < NUMBEROFSTYLES; i++) {
    wxColor color = wxColor(configuration->GetColor((TextStyle)i));
    if (color.IsOk())
      document += wxString::Format(wxS("\\red%li\\green%li\\blue%li;\n"),
                                   static_cast<long>(color.Red()),
                                   static_cast<long>(color.Green()),
                                   static_cast<long>(color.Blue()));
    else
      document += wxString::Format(wxS("\\red%i\\green%i\\blue%i;\n"), 0, 0, 0);
  }
  document += wxS("}\n\n");

  /* Our style sheet:
     Style  Meaning
     0    Ordinary text
     1    Chapter Cell
     2    Section Cell
     3    Subsection Cell
     16   Title Cell
     21   Math Cell
     22   Math Cell with Label
  */
  document += wxS("{\\stylesheet\n");
  document += wxS("{\\s0\\snext0\\widctlpar\\hyphpar0\\kerning1\\li0\\ri0\\lin0"
                  "\\rin0\\fi0\\f0\\fs24 Normal;}\n");
  document += wxS("{\\s1\\outlinelevel0\\keepn\\b\\f0\\fs40\\sbasedon16\\snext0"
                  " Section Cell;}\n");
  document += wxS("{\\s2\\outlinelevel1\\keepn\\b\\f0\\fs36\\sbasedon1\\snext0 "
                  "Subsection Cell;}\n");
  document += wxS("{\\s3\\outlinelevel2\\keepn\\b\\f0\\fs32\\sbasedon2\\snext0 "
                  "SubSubsection Cell;}\n");
  document += wxS("{\\s4\\outlinelevel3\\keepn\\b\\f0\\fs30\\sbasedon2\\snext0 "
                  "Heading5 Cell;}\n");
  document += wxS("{\\s5\\outlinelevel4\\keepn\\b\\f0\\fs28\\sbasedon2\\snext0 "
                  "Heading6 Cell;}\n");
  document += wxS("{\\s16\\keepn\\b\\f0\\fs56\\snext0 Title Cell;}\n");
  document += wxS("{\\s21\\li1105\\lin1105\\f0\\fs24\\sbasedon0 Math;}\n");
  document += wxS("{\\s22\\li1105\\lin1105\\fi-"
                  "1105\\f0\\fs24\\sbasedon0\\snext21 Math+Label;}\n");
  document += wxS("}\n\n{\n");
  return document;
}

wxString WorksheetExport::RTFEnd() {
  // Close the document
  return wxS("}\n}");
}

std::unique_ptr<Cell> WorksheetExport::CopySelection(Cell *start, Cell *end,
                                                     bool asData) {
  CellListBuilder<> copy;

  if (asData)
    for (const Cell &tmp : OnList(start)) {
      copy.Append(tmp.Copy(tmp.GetGroup()));
      if (&tmp == end)
        break;
    }
  else
    for (const Cell &tmp : OnDrawList(start)) {
      copy.Append(tmp.Copy(tmp.GetGroup()));
      if (&tmp == end)
        break;
    }

  /* The warning from gcc is correct. But an old MacOs compiler errors out
     on correct code, here. */
#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wredundant-move"
#endif
    return std::move(copy);
#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif
}

wxSize WorksheetExport::CopyToFile(const wxString &file, Cell *start, Cell *end,
                                   bool asData, double scale,
                                   const Configuration *const *configuration) {
  auto tmp = CopySelection(start, end, asData);
  BitmapOut output(configuration, std::move(tmp), scale);
  wxSize retval = output.ToFile(file);
  return retval;
}

bool WorksheetExport::ExportToHTML(GroupCell *tree, Configuration *configuration,
                                   const wxString &file,
                                   ViewCellPointers *cellPointers, GroupCell *hCaret) {
  // Don't update the worksheet whilst exporting
  //  wxWindowUpdateLocker noUpdates(this);

  // The path to the image directory as seen from the html directory
  wxString imgDir_rel;
  // The absolute path to the image directory
  wxString imgDir;
  // What happens if we split the filename into several parts.
  wxString path, filename, ext;
  wxConfigBase *config = wxConfig::Get();

  int count = 0;
  MarkDownHTML MarkDown(configuration);

  wxFileName::SplitPath(file, &path, &filename, &ext);
  imgDir_rel = filename + wxS("_htmlimg");
  imgDir = path + wxS("/") + imgDir_rel;

  if (!wxDirExists(imgDir)) {
    if (!wxMkdir(imgDir))
      return false;
  }

  wxString cssfileName_rel = imgDir_rel + wxS("/") + filename + wxS(".css");
  wxString cssfileName = path + wxS("/") + cssfileName_rel;
  wxFileOutputStream cssfile(cssfileName);
  if (!cssfile.IsOk())
    return false;

  wxURI filename_uri(filename);
  wxString filename_encoded =
    filename_uri.BuildURI(); /* handle HTML entities like " " => "%20" */

  wxTextOutputStream css(cssfile);

  wxString output;

  configuration->ClipToDrawRegion(false);
  output << wxS("<!DOCTYPE html>\n");
  output << wxS("<html>\n"); // We do not know the language of the
  // exported document.
  output << wxS(" <head>\n");
  output << wxS("  <title>") + filename + wxS("</title>\n");
  output << wxS("  <meta name=\"generator\" content=\"wxMaxima\">\n");
  output << wxS("  <meta http-equiv=\"Content-Type\" content=\"text/html; "
                "charset=utf-8\">\n");

  //////////////////////////////////////////////
  // Write styles
  //////////////////////////////////////////////

  if ((configuration->HTMLequationFormat() ==
       Configuration::mathML_mathJaX) ||
      (configuration->HTMLequationFormat() == Configuration::mathJaX_TeX)) {
    output << wxS("<script type=\"text/x-mathjax-config\">\n");
    output << wxS("  MathJax.Hub.Config({\n");
    output << wxS("    displayAlign: \"left\",\n");
    output << wxS("    context: \"MathJax\",\n");
    output << wxS("    TeX: {TagSide: \"left\"}\n");
    output << wxS("  })\n");
    output << wxS("</script>\n");
    output << wxS("<script id=\"MathJax-script\" async src=\"") +
      configuration->MathJaXURL() + wxS("\">\n");
    // prevent optimizing <script src="..."><script> to <script src=..."/>
    output << wxS("  // A comment that hinders wxWidgets from optimizing this "
                  "tag too much.\n");
    output << wxS("</script>\n");
  }

  wxString font, fontTitle, fontSection, fontSubsection, fontSubsubsection,
    fontHeading5, fontHeading6, fontText;
  wxString colorInput(wxS("blue"));
  wxString colorPrompt(wxS("red"));
  wxString colorText(wxS("black")), colorTitle(wxS("black")),
    colorSection(wxS("black")), colorSubSec(wxS("black")),
    colorSubsubSec(wxS("black")), colorHeading5(wxS("black")),
    colorHeading6(wxS("black"));
  wxString colorCodeVariable = wxS("rgb(0,128,0)");
  wxString colorCodeFunction = wxS("rgb(128,0,0)");
  wxString colorCodeComment = wxS("rgb(64,64,64)");
  wxString colorCodeNumber = wxS("rgb(128,64,0)");
  wxString colorCodeString = wxS("rgb(0,0,128)");
  wxString colorCodeOperator = wxS("rgb(0,0,128)");
  wxString colorCodeLisp = wxS("rgb(255,0,128)");
  wxString colorCodeEndOfLine = wxS("rgb(192,192,192)");

  wxString colorTextBg(wxS("white"));
  wxString colorBg(wxS("white"));

  // bold and italic
  bool boldInput = false;
  bool italicInput = false;
  bool boldPrompt = false;
  bool italicPrompt = false;
  bool boldString = false;
  bool italicString = false;

  bool boldTitle = false;
  bool italicTitle = false;
  bool underTitle = false;
  bool boldSection = false;
  bool italicSection = false;
  bool underSection = false;
  bool boldSubsection = false;
  bool boldSubsubsection = false;
  bool boldHeading5 = false;
  bool boldHeading6 = false;
  bool italicSubsection = false;
  bool italicSubsubsection = false;
  bool italicHeading5 = false;
  bool italicHeading6 = false;
  bool underSubsection = false;
  bool underSubsubsection = false;
  bool underHeading5 = false;
  bool underHeading6 = false;

  int fontSize = 12;
  // main fontsize
  config->Read(wxS("fontSize"), &fontSize);

  // read fonts
  config->Read(wxS("Style/fontname"), &font);
  config->Read(wxS("Style/Title/fontname"), &fontTitle);
  config->Read(wxS("Style/Section/fontname"), &fontSection);
  config->Read(wxS("Style/Subsection/fontname"), &fontSubsection);
  config->Read(wxS("Style/Subsubsection/fontname"), &fontSubsubsection);
  config->Read(wxS("Style/Heading5/fontname"), &fontHeading5);
  config->Read(wxS("Style/Heading6/fontname"), &fontHeading6);
  config->Read(wxS("Style/Text/fontname"), &fontText);

  // read colors
  config->Read(wxS("Style/Input/color"), &colorInput);
  config->Read(wxS("Style/MainPrompt/color"), &colorPrompt);
  config->Read(wxS("Style/Text/color"), &colorText);
  config->Read(wxS("Style/Section/color"), &colorSection);
  config->Read(wxS("Style/Subsection/color"), &colorSubSec);
  config->Read(wxS("Style/Subsubsection/color"), &colorSubsubSec);
  config->Read(wxS("Style/Heading5/color"), &colorHeading5);
  config->Read(wxS("Style/Heading6/color"), &colorHeading6);
  config->Read(wxS("Style/Title/color"), &colorTitle);
  config->Read(wxS("Style/TextBackground/color"), &colorBg);
  config->Read(wxS("Style/Background/color"), &colorTextBg);

  config->Read(wxS("Style/CodeHighlighting/Variable/color"),
               &colorCodeVariable);
  config->Read(wxS("Style/CodeHighlighting/Function/color"),
               &colorCodeFunction);
  config->Read(wxS("Style/CodeHighlighting/Comment/color"), &colorCodeComment);
  config->Read(wxS("Style/CodeHighlighting/Number/color"), &colorCodeNumber);
  config->Read(wxS("Style/CodeHighlighting/String/color"), &colorCodeString);
  config->Read(wxS("Style/CodeHighlighting/Operator/color"),
               &colorCodeOperator);
  config->Read(wxS("Style/CodeHighlighting/Lisp/color"), &colorCodeLisp);

  // read bold and italic
  config->Read(wxS("Style/Input/bold"), &boldInput);
  config->Read(wxS("Style/String/bold"), &boldString);
  config->Read(wxS("Style/Input/italic"), &italicInput);
  config->Read(wxS("Style/String/italic"), &italicString);
  config->Read(wxS("Style/MainPrompt/bold"), &boldPrompt);
  config->Read(wxS("Style/MainPrompt/italic"), &italicPrompt);

  config->Read(wxS("Style/Title/bold"), &boldTitle);
  config->Read(wxS("Style/Title/italic"), &italicTitle);
  config->Read(wxS("Style/Title/underlined"), &underTitle);
  config->Read(wxS("Style/Section/bold"), &boldSection);
  config->Read(wxS("Style/Section/italic"), &italicSection);
  config->Read(wxS("Style/Section/underlined"), &underSection);
  config->Read(wxS("Style/Subsection/bold"), &boldSubsection);
  config->Read(wxS("Style/Subsection/italic"), &italicSubsection);
  config->Read(wxS("Style/Subsection/underlined"), &underSubsection);
  config->Read(wxS("Style/Subsubsection/bold"), &boldSubsubsection);
  config->Read(wxS("Style/Subsubsection/italic"), &italicSubsubsection);
  config->Read(wxS("Style/Subsubsection/underlined"), &underSubsubsection);
  config->Read(wxS("Style/Heading5/bold"), &boldHeading5);
  config->Read(wxS("Style/Heading5/italic"), &italicHeading5);
  config->Read(wxS("Style/Heading5/underlined"), &underHeading5);
  config->Read(wxS("Style/Heading6/bold"), &boldHeading6);
  config->Read(wxS("Style/Heading6/italic"), &italicHeading6);
  config->Read(wxS("Style/Heading6/underlined"), &underHeading6);

  wxURI css_url(cssfileName_rel);
  wxString encoded_css_url =
    css_url.BuildURI(); /* handle HTML entities like " " => "%20" */
  output << wxS("  <link rel=\"stylesheet\" type=\"text/css\" href=\"") +
    encoded_css_url + wxS("\">\n");

  wxString versionString = wxS("Created with wxMaxima version " WXMAXIMA_VERSION);
  wxString versionPad;
  for (unsigned int i = 0; i < versionString.Length(); i++)
    versionPad += "*";

  css << wxS("\n");
  css << wxS("/* *********") + versionPad + wxS("******** \n");
  css << wxS("   *        ") + versionString + wxS("       * \n");
  css << wxS("   *********") + versionPad + wxS("******** */\n");

  // BODY STYLE
  css << wxS("body {\n");
  if (font.Length()) {
    css << wxS("  font-family: ") + font + wxS(";\n");
  }
  if (colorBg.Length()) {
    wxColour color(colorBg);
    css << wxS("  background-color: ") +
      color.GetAsString(wxC2S_CSS_SYNTAX) +
      wxS(";\n");
  }
  css << wxS("}\n");

  // INPUT STYLE
  css << wxS(".input {\n");
  if (colorInput.Length()) {
    wxColour color(colorInput);
    css << wxS("  color: \n") +
      color.GetAsString(wxC2S_CSS_SYNTAX) +
      wxS(";\n");
  }
  if (boldInput)
    css << wxS("  font-weight: bold;\n");
  if (italicInput)
    css << wxS("  font-style: italic;\n");
  css << wxS("}\n");

  // COMMENT STYLE
  css << wxS(".comment {\n");
  if (fontText.Length()) {
    css << wxS("  font-family: ") + fontText + wxS(";\n");
  }

  if (colorText.Length()) {
    wxColour color(colorText);
    css << wxS("  color: ") +
      color.GetAsString(wxC2S_CSS_SYNTAX) +
      wxS(";\n");
  }
  if (colorTextBg.Length()) {
    wxColour color(colorTextBg);
    css << wxS("  background-color: ") +
      color.GetAsString(wxC2S_CSS_SYNTAX) +
      wxS(";\n");
  }
  css << wxS("  padding: 2mm;\n");
  css << wxS("}\n");

  // Colors for code highlighting
  if (colorCodeVariable.Length()) {
    wxColour color(colorCodeVariable);
    css << wxS(".code_variable {\n");
    css << wxS("  color: \n") +
      color.GetAsString(wxC2S_CSS_SYNTAX) +
      wxS(";\n");
    css << wxS("}\n");
  }

  css << wxS("p {\n");
  css << wxS("  margin-top: 0em;\n");
  css << wxS("  margin-bottom: 0em;\n");
  css << wxS("}\n");

  if (colorCodeFunction.Length()) {
    wxColour color(colorCodeFunction);
    css << wxS(".code_function {\n\n");
    css << wxS("  color: ") +
      color.GetAsString(wxC2S_CSS_SYNTAX) +
      wxS(";\n");
    css << wxS("}\n");
  }

  if (colorCodeComment.Length()) {
    wxColour color(colorCodeComment);
    css << wxS(".code_comment {\n");
    css << wxS("  color: ") +
      color.GetAsString(wxC2S_CSS_SYNTAX) +
      wxS(";\n");
    css << wxS("}\n");
  }

  if (colorCodeNumber.Length()) {
    wxColour color(colorCodeNumber);
    css << wxS(".code_number {\n");
    css << wxS("  color: ") +
      color.GetAsString(wxC2S_CSS_SYNTAX) +
      wxS(";\n");
    css << wxS("}\n");
  }

  if (colorCodeString.Length()) {
    wxColour color(colorCodeString);
    css << wxS(".code_string {\n");
    css << wxS("  color: ") +
      color.GetAsString(wxC2S_CSS_SYNTAX) +
      wxS(";\n");
    css << wxS("}\n");
  }

  if (colorCodeOperator.Length()) {
    wxColour color(colorCodeOperator);
    css << wxS(".code_operator {\n");
    css << wxS("  color: ") +
      color.GetAsString(wxC2S_CSS_SYNTAX) +
      wxS(";\n");
    css << wxS("}\n");
  }

  if (colorCodeLisp.Length()) {
    wxColour color(colorCodeLisp);
    css << wxS(".code_lisp {\n");
    css << wxS("  color: ") +
      color.GetAsString(wxC2S_CSS_SYNTAX) +
      wxS(";\n");
    css << wxS("}\n");
  }

  if (colorCodeEndOfLine.Length()) {
    wxColour color(colorCodeEndOfLine);
    css << wxS(".code_endofline {\n");
    css << wxS("  color: ") +
      color.GetAsString(wxC2S_CSS_SYNTAX) +
      wxS(";");
    css << wxS("}\n");
  }

  // SMOOTHER IMAGE SCALING FOR THE IE
  css << "img {\n";
  css << wxS("  -ms-interpolation-mode: bicubic;\n");
  css << wxS("}\n");

  // IMAGE STYLE
  css << wxS(".image {\n");
  if (fontText.Length()) {
    css << wxS("  font-family: ") + fontText + wxS(";\n");
  }
  if (colorText.Length()) {
    wxColour color(colorText);
    css << wxS("  color: ") +
      color.GetAsString(wxC2S_CSS_SYNTAX) +
      wxS(";\n");
  }
  css << wxS("  padding: 2mm;\n");
  css << wxS("}\n");

  // SECTION STYLE
  css << wxS(".section {\n");
  if (fontSection.Length()) {
    css << wxS("  font-family: ") + fontSection + wxS(";\\");
  }
  if (colorSection.Length()) {
    wxColour color(colorSection);
    css << wxS("  color: ") +
      color.GetAsString(wxC2S_CSS_SYNTAX) +
      wxS(";\n");
  }
  if (boldSection)
    css << wxS("  font-weight: bold;\n");
  if (underSection)
    css << wxS("  text-decoration: underline;\n");
  if (italicSection)
    css << wxS("  font-style: italic;\n");
  css << wxS("  font-size: 1.5em;\n");
  css << wxS("  padding: 2mm;\n");
  css << wxS("}\n");

  // SUBSECTION STYLE
  css << wxS(".subsect {\n");
  if (fontSubsection.Length()) {
    css << wxS("  font-family: ") + fontSubsection + wxS(";\n");
  }
  if (colorSubSec.Length()) {
    wxColour color(colorSubSec);
    css << wxS("  color: ") +
      color.GetAsString(wxC2S_CSS_SYNTAX) +
      wxS(";\n");
  }
  if (boldSubsection)
    css << wxS("  font-weight: bold;\n");
  if (underSubsection)
    css << wxS("  text-decoration: underline;\n");
  if (italicSubsection)
    css << wxS("  font-style: italic;\n");
  css << wxS("  font-size: 1.2em;\n");
  css << wxS("  padding: 2mm;\n");
  css << wxS("}\n");

  // SUBSUBSECTION STYLE
  css << wxS(".subsubsect {\n");
  if (fontSubsubsection.Length()) {
    css << wxS("  font-family: ") + fontSubsubsection + wxS(";\n");
  }
  if (colorSubsubSec.Length()) {
    wxColour color(colorSubsubSec);
    css << wxS("  color: ") +
      color.GetAsString(wxC2S_CSS_SYNTAX) +
      wxS(";\n");
  }
  if (boldSubsubsection)
    css << wxS("  font-weight: bold;\n");
  if (underSubsubsection)
    css << wxS("  text-decoration: underline;\n");
  if (italicSubsubsection)
    css << wxS("  font-style: italic;\n");
  css << wxS("  font-size: 1.2em;\n");
  css << wxS("  padding: 2mm;\n");
  css << wxS("}\n");

  // HEADING5 STYLE
  css << wxS(".heading5 {\n");
  if (fontHeading5.Length()) {
    css << wxS("  font-family: ") + fontHeading5 + wxS(";\n");
  }
  if (colorHeading5.Length()) {
    wxColour color(colorHeading5);
    css << wxS("  color: ") +
      color.GetAsString(wxC2S_CSS_SYNTAX) +
      wxS(";\n");
  }
  if (boldHeading5)
    css << wxS("  font-weight: bold;\n");
  if (underHeading5)
    css << wxS("  text-decoration: underline;\n");
  if (italicHeading5)
    css << wxS("  font-style: italic;\n");
  css << wxS("  font-size: 1.2em;\n");
  css << wxS("  padding: 2mm;\n");
  css << wxS("}\n");

  // HEADING6 STYLE
  css << wxS(".heading6 {\n");
  if (fontHeading6.Length()) {
    css << wxS("  font-family: ") + fontHeading6 + wxS(";\n");
  }
  if (colorHeading6.Length()) {
    wxColour color(colorHeading6);
    css << wxS("  color: ") +
      color.GetAsString(wxC2S_CSS_SYNTAX) +
      wxS(";\n");
  }
  if (boldHeading6)
    css << wxS("  font-weight: bold;\n");
  if (underHeading6)
    css << wxS("  text-decoration: underline;\n");
  if (italicHeading6)
    css << wxS("  font-style: italic;\n");
  css << wxS("  font-size: 1.2em;\n");
  css << wxS("  padding: 2mm;\n");
  css << wxS("}\n");

  // TITLE STYLE
  css << wxS(".title {\n");
  if (fontTitle.Length()) {
    css << wxS("  font-family: ") + fontTitle + wxS(";\n");
  }
  if (colorTitle.Length()) {
    wxColour color(colorTitle);
    css << wxS("  color: ") +
      color.GetAsString(wxC2S_CSS_SYNTAX) +
      wxS(";\n");
  }
  if (boldTitle)
    css << wxS("  font-weight: bold;\n");
  if (underTitle)
    css << wxS("  text-decoration: underline;\n");
  if (italicTitle)
    css << wxS("  font-style: italic;\n");
  css << wxS("  font-size: 2em;\n");
  css << wxS("  padding: 2mm;\n");
  css << wxS("}\n");

  // PROMPT STYLE
  css << wxS(".prompt {\n");
  if (colorPrompt.Length()) {
    wxColour color(colorPrompt);
    css << wxS("  color: ") +
      color.GetAsString(wxC2S_CSS_SYNTAX) +
      wxS(";\n");
  }
  if (boldPrompt)
    css << wxS("  font-weight: bold;\n");
  if (italicPrompt)
    css << wxS("  font-style: italic;\n");
  css << wxS("}\n");

  // TABLES
  css << wxS("table {\n");
  css << wxS("  border: 0px;\n");
  css << wxS("}\n");
  css << wxS("td {\n");
  css << wxS("  vertical-align: top;\n");
  css << wxS("  padding: 1mm;\n");
  css << wxS("}\n");

  output << wxS(" </head>\n");
  output << wxS(" <body>\n");

  output << wxS("\n");
  output << wxS("<!-- *********") + versionPad + wxS("******** -->\n");
  output << wxS("<!-- *        ") + versionString + wxS("       * -->\n");
  output << wxS("<!-- *********") + versionPad + wxS("******** -->\n");

  if ((configuration->HTMLequationFormat() != Configuration::bitmap) &&
      (configuration->HTMLequationFormat() != Configuration::svg)) {
    // Tell users that have disabled JavaScript why they don't get 2d maths.
    output << wxS("<noscript>");
    output << wxS("<div class=\"error message\">");
    output << wxS("<p>Please enable JavaScript in order to get a 2d display of "
                  "the equations embedded in this web page.</p>");
    output << wxS("</div>");
    output << wxS("</noscript>");

    // Tell mathJax about the \abs{} operator we define for LaTeX.
    output << wxS("<p hidden = \"hidden\">\\(");
    output << wxS("      \\DeclareMathOperator{\\abs}{abs}\n");
    output << wxS("      \\newcommand{\\ensuremath}[1]{\\mbox{$#1$}}\n");
    output << wxS("\\)</p>");
  }

  //////////////////////////////////////////////
  // Write the actual contents
  //////////////////////////////////////////////

  for (auto &tmp : OnList(tree)) {
    // Handle a code cell
    if (tmp.GetGroupType() == GC_TYPE_CODE) {
      // Handle the label
      const Cell *out = tmp.GetLabel();

      if (out || (configuration->ShowCodeCells()))
        output << wxS("\n\n<!-- Code cell -->\n\n\n");

      // Handle the input
      if (configuration->ShowCodeCells()) {
        const Cell *prompt = tmp.GetPrompt();
        output << wxS("<table><tr><td>\n");
        output << wxS("  <span class=\"prompt\">\n");
        output << prompt->ToString();
        output << wxS("\n  </span></td>\n");

        const EditorCell *input = tmp.GetEditable();
        if (input) {
          output << wxS("  <td><span class=\"input\">\n");
          output << input->ToHTML();
          output << wxS("  </span></td>\n");
        }
        output << wxS("</tr></table>\n");
      }

      // Handle the output - if output exists.
      if (out == NULL) {
        // No output to export.x
        output << wxS("\n");
      } else {
        // We got output.
        // Output is a list that can consist of equations, images and
        // animations. We need to handle each of these item types separately =>
        // break down the list into chunks of one type.
        Cell *chunkStart = tmp.GetLabel();
        while (chunkStart != NULL) {
          Cell *chunkEnd = chunkStart;

          if ((chunkEnd->GetType() != MC_TYPE_SLIDE) &&
              (chunkEnd->GetType() != MC_TYPE_IMAGE))
            while (chunkEnd->GetNext() != NULL) {
              auto *chunkNext = chunkEnd->GetNext();
              if ((chunkNext->GetType() == MC_TYPE_SLIDE) ||
                  (chunkNext->GetType() == MC_TYPE_IMAGE) ||
                  (chunkNext->GetTextStyle() == TS_LABEL) ||
                  (chunkNext->GetTextStyle() == TS_USERLABEL))
                break;
              chunkEnd = chunkNext;
            }

          // Create a list containing only our chunk.
          auto chunk = CopySelection(chunkStart, chunkEnd);

          // Export the chunk.

          if (dynamic_cast<AnimationCell *>(&(*chunk)) != NULL) {
            dynamic_cast<AnimationCell *>(&(*chunk))->ToGif(
                                                            imgDir + wxS("/") + filename +
                                                            wxString::Format(wxS("_%d.gif"), count));
            output
              << wxS("  <img src=\"") + filename_encoded + wxS("_htmlimg/") +
              filename_encoded +
              wxString::Format(
                               _("_%d.gif\"  alt=\"Animated Diagram\" "
                                 "loading=\"lazy\" style=\"max-width:90%%;\">\n"),
                               count);
          } else if (dynamic_cast<ImgCellBase *>(&(*chunk)) == NULL) {
            switch (configuration->HTMLequationFormat()) {
            case Configuration::mathJaX_TeX: {
              wxString line = chunk->ListToTeX();

              line.Replace(wxS("<"), wxS("&lt;"));
              line.Replace(wxS("&"), wxS("&amp;"));
              line.Replace(wxS(">"), wxS("&gt;"));
              // Work around a known limitation in MathJaX: According to
              // https://github.com/mathjax/MathJax/issues/569 Non-Math Text
              // will still be interpreted as Text, not as TeX for a long while.
              //
              // So instead of  "\%o1" print "%o1" - that works fine now.
              // Since we are using a *fixed* Mathjax version for an export,
              // nothing will happen, if Mathjax changes that behaviour and
              // would interpret the % as TeX comment. When we would upgrade to
              // the new MathJax version we would need to escape the % with \%,
              // but now that is not necessary.
              line.Replace(wxS("\\tag{\\% "), wxS("\\tag{%"));

              output << wxS("<p>\n\\[") << line << wxS("\\]\n</p>\n");
              break;
            }

            case Configuration::svg: {
              auto const alttext =
                EditorCell::EscapeHTMLChars(chunk->ListToString());
              auto const filepath = wxString::Format(wxS("%s/%s_%d.svg"),
                                                     imgDir, filename, count);
              Svgout svgout(&configuration, std::move(chunk), filepath);

              wxString line =
                wxS("  <img src=\"") + filename_encoded + wxS("_htmlimg/") +
                filename_encoded +
                wxString::Format(
                                 wxS("_%d.svg\" width=\"%li\" style=\"max-width:90%%;\" "
                                     "loading=\"lazy\" alt=\""),
                                 count, static_cast<long>(svgout.GetSize().x)) +
                alttext + wxS("\"><br>\n");

              output << line + "\n";
              break;
            }

            case Configuration::bitmap: {
              wxSize size;
              size = CopyToFile(imgDir + wxS("/") + filename +
                                wxString::Format(wxS("_%d.png"), count),
                                &(*chunk), NULL, true,
                                configuration->BitmapScale(), &configuration);
              int borderwidth = 0;
              wxString alttext =
                EditorCell::EscapeHTMLChars(chunk->ListToString());
              borderwidth = chunk->GetImageBorderWidth();

              wxString line =
                wxS("  <img src=\"") + filename_encoded + wxS("_htmlimg/") +
                filename_encoded +
                wxString::Format(
                                 wxS("_%d.png\" width=\"%li\" style=\"max-width:90%%;\" "
                                     "loading=\"lazy\" alt=\" "),
                                 count,
                                 static_cast<long>(size.x) / configuration->BitmapScale() -
                                 2 * borderwidth) +
                alttext + wxS("\"><br>\n");

              output << line + "\n";
              break;
            }

            default: {
              wxString line = chunk->ListToMathML();
              output
                << wxS("<math xmlns=\"http://www.w3.org/1998/Math/MathML\" "
                       "display=\"block\">")
                << line << wxS("</math>\n");
            }
            }
          } else {
            wxSize size;
            ext = wxS(".") +
              dynamic_cast<ImgCellBase *>(&(*chunk))->GetExtension();
            size = dynamic_cast<ImgCellBase *>(&(*chunk))->ToImageFile(
                                                                       imgDir + wxS("/") + filename +
                                                                       wxString::Format(wxS("_%d"), count) + ext);
            int borderwidth = 0;
            wxString alttext =
              EditorCell::EscapeHTMLChars(chunk->ListToString());
            borderwidth = chunk->GetImageBorderWidth();

            wxString line =
              wxS("  <img src=\"") + filename_encoded + wxS("_htmlimg/") +
              filename_encoded +
              wxString::Format(
                               wxS("_%li%s\" width=\"%li\" style=\"max-width:90%%;\" "
                                   "loading=\"lazy\" alt=\""),
                               static_cast<long>(count),
                               ext.utf8_str(), static_cast<long>(size.x) - 2 * borderwidth) +
              alttext + wxS("\"><br>\n");

            output << line + "\n";
          }
          count++;

          chunkStart = chunkEnd->GetNext();
        }
      }
    } else // No code cell
      {
        switch (tmp.GetGroupType()) {
        case GC_TYPE_TEXT:
          output << wxS("\n\n<!-- Text cell -->\n\n\n");
          output << wxS("<div class=\"comment\">\n");
          // A text cell can include block-level HTML elements, e.g. <ul> ...
          // </ul> (converted from Markdown) Therefore do not output <p> ... </p>
          // elements, that would result in invalid HTML.
          output << MarkDown.MarkDown(EditorCell::EscapeHTMLChars(
                                                                  tmp.GetEditable()->ToString())) +
            "\n";
          output << wxS("</div>\n");
          break;
        case GC_TYPE_SECTION:
          output << wxS("\n\n<!-- Section cell -->\n\n\n");
          output << wxS("<div class=\"section\">\n");
          output << wxS("<p>\n");
          output << EditorCell::EscapeHTMLChars(tmp.GetPrompt()->ToString() +
                                                tmp.GetEditable()->ToString()) +
            "\n";
          output << wxS("</p>\n");
          output << wxS("</div>\n");
          break;
        case GC_TYPE_SUBSECTION:
          output << wxS("\n\n<!-- Subsection cell -->\n\n\n");
          output << wxS("<div class=\"subsect\">\n");
          output << wxS("<p>\n");
          output << EditorCell::EscapeHTMLChars(tmp.GetPrompt()->ToString() +
                                                tmp.GetEditable()->ToString()) +
            "\n";
          output << wxS("</p>\n");
          output << wxS("</div>\n");
          break;
        case GC_TYPE_SUBSUBSECTION:
          output << wxS("\n\n<!-- Subsubsection cell -->\n\n\n");
          output << wxS("<div class=\"subsubsect\">\n");
          output << wxS("<p>\n");
          output << EditorCell::EscapeHTMLChars(tmp.GetPrompt()->ToString() +
                                                tmp.GetEditable()->ToString()) +
            "\n";
          output << wxS("</p>\n");
          output << wxS("</div>\n");
          break;
        case GC_TYPE_HEADING5:
          output << wxS("\n\n<!-- Heading5 cell -->\n\n\n");
          output << wxS("<div class=\"heading5\">\n");
          output << wxS("<p>\n");
          output << EditorCell::EscapeHTMLChars(tmp.GetPrompt()->ToString() +
                                                tmp.GetEditable()->ToString()) +
            "\n";
          output << wxS("</p>\n");
          output << wxS("</div>\n");
          break;
        case GC_TYPE_HEADING6:
          output << wxS("\n\n<!-- Heading6 cell -->\n\n\n");
          output << wxS("<div class=\"heading6\">\n");
          output << wxS("<p>\n");
          output << EditorCell::EscapeHTMLChars(tmp.GetPrompt()->ToString() +
                                                tmp.GetEditable()->ToString()) +
            "\n";
          output << wxS("</p>\n");
          output << wxS("</div>\n");
          break;
        case GC_TYPE_TITLE:
          output << wxS("\n\n<!-- Title cell -->\n\n\n");
          output << wxS("<div class=\"title\">\n");
          output << wxS("<p>\n");
          output << EditorCell::EscapeHTMLChars(tmp.GetEditable()->ToString()) +
            "\n";
          output << wxS("</p>\n");
          output << wxS("</div>\n");
          break;
        case GC_TYPE_PAGEBREAK:
          output << wxS("\n\n<!-- Page break cell -->\n\n\n");
          output << wxS("<div class=\"comment\">\n");
          output << wxS("<hr>\n");
          output << wxS("</div>\n");
          break;
        case GC_TYPE_IMAGE: {
          Cell *out = tmp.GetLabel();
          if(out)
            {
              output << wxS("\n\n<!-- Image cell -->\n\n\n");
              output << wxS("<div class=\"image\">\n");
              output << EditorCell::EscapeHTMLChars(tmp.GetPrompt()->ToString() +
                                                    tmp.GetEditable()->ToString())
                     << wxS("\n");
              output << wxS("<br>\n");
              if (dynamic_cast<AnimationCell *>(tmp.GetOutput()) != NULL) {
                dynamic_cast<AnimationCell *>(tmp.GetOutput())
                  ->ToGif(imgDir + wxS("/") + filename +
                          wxString::Format(wxS("_%d.gif"), count));
                output << wxS("  <img src=\"") + filename_encoded + wxS("_htmlimg/") +
                  filename_encoded +
                  wxString::Format(
                                   _("_%d.gif\" alt=\"Animated Diagram\" "
                                     "style=\"max-width:90%%;\" loading=\"lazy\">"),
                                   count)
                       << wxS("\n");
              } else {
                ImgCellBase *imgCell = dynamic_cast<ImgCellBase *>(out);
                wxASSERT(imgCell);
                if(imgCell)
                  {
                    imgCell->ToImageFile(imgDir + wxS("/") + filename +
                                         wxString::Format(wxS("_%d."), count) +
                                         imgCell->GetExtension());
                    output
                      << wxS("  <img src=\"") + filename_encoded + wxS("_htmlimg/") +
                      filename_encoded +
                      wxString::Format(
                                       wxS("_%d.%s\" alt=\"Diagram\" "
                                           "style=\"max-width:90%%;\" loading=\"lazy\">"),
                                       count, imgCell->GetExtension().utf8_str());
                  }
              }
              output << wxS("</div>\n");
              count++;
            }
          else
            wxLogMessage(_("ImageCell without image."));
        } break;
        case GC_TYPE_CODE:
        case GC_TYPE_INVALID:
          break;
        }
      }
  }

  //////////////////////////////////////////////
  // Footer
  //////////////////////////////////////////////

  output << wxS("\n");
  output << wxS(" <hr>\n");
  output << wxS(" <p><small> Created with "
                "<a href=\"https://wxMaxima-developers.github.io/wxmaxima/\">"
                "wxMaxima</a>.</small></p>\n");
  output << wxEmptyString;

  if (configuration->ExportContainsWXMX()) {
    wxString wxmxfileName_rel = imgDir_rel + wxS("/") + filename + wxS(".wxmx");
    wxString wxmxfileName = path + wxS("/") + wxmxfileName_rel;
    std::vector<wxString> dummy;
    Format::ExportToWXMX(tree, wxmxfileName, configuration,
                         cellPointers, dummy, hCaret);
    wxURI wxmxfileName_uri(wxmxfileName_rel);
    wxString wxmxfileName_encoded = wxmxfileName_uri.BuildURI(); /* handle HTML entities like " " => "%20" */

    output
      << wxS(" <small> The source of this Maxima session can be downloaded "
             "<a href=\"") +
      wxmxfileName_encoded + wxS("\">here</a>.</small>\n");
  }

  //
  // Close the document
  //
  output << wxS(" </body>\n");
  output << wxS("</html>\n");

  configuration->ClipToDrawRegion(true);

  // Indent the document and test it for validity.
  wxXmlDocument doc;
  {
    wxLogNull suppressErrorMessages;
    wxMemoryOutputStream ostream;
    wxTextOutputStream txtstrm(ostream);
    txtstrm.WriteString(output);
    wxMemoryInputStream istream(ostream);
    doc.Load(istream);
  }

  // Replace the raw document by the indented one. If that step worked, that is
  // it.
  if (doc.IsOk()) {
    wxMemoryOutputStream ostream;
    doc.Save(ostream);
    output = wxString::FromUTF8(
                                reinterpret_cast<char *>(ostream.GetOutputStreamBuffer()->GetBufferStart()),
                                ostream.GetOutputStreamBuffer()->GetBufferSize());

    // Now the string has a header we want to drop again.

    auto newlinepos = output.Find("\n");
    if(newlinepos >= 0)
      output = output.SubString(static_cast<std::size_t>(newlinepos) + 1, output.Length());
  } else
    wxLogMessage(_("Bug: HTML output is no valid XML"));

  wxFileOutputStream outfile(file);
  if (!outfile.IsOk()) {
    return false;
  }

  wxTextOutputStream outstream(outfile);
  outstream << output;

  bool outfileOK = !outfile.GetFile()->Error();
  bool cssOK = !cssfile.GetFile()->Error();
  outfile.Close();
  cssfile.Close();

  configuration->ClipToDrawRegion(true);
  return outfileOK && cssOK;
}

