// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2026 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  LaTeX/MathML to Maxima: the two front ends and the shared interpretation.
  See MathImport.h for the overall design.
*/

#include "MathImport.h"

#include <wx/intl.h>
#include <wx/log.h>
#include <wx/sstream.h>
#include <wx/xml/xml.h>

#include <array>
#include <optional>
#include <string_view>
#include <utility>
#include <vector>

namespace wxm {
namespace {

//! Thrown when the input has no reading we trust. Carries the user's message.
struct ImportError {
  wxString message;
};

[[noreturn]] void Fail(const wxString &message) { throw ImportError{message}; }

// ---------------------------------------------------------------------------
// The layout tree both front ends produce
// ---------------------------------------------------------------------------

/*! One node of a formula's layout -- what it looks like, not what it means

  The children depend on the kind:
   - Row: the items, left to right
   - Frac, Binom: numerator, denominator
   - Sqrt: the radicand. Root: radicand, index
   - Scripts: base, subscript, superscript (the scripts are Rows, empty if
     absent; hasSub/hasSup say which are there)
   - Fenced: one Row, the contents between the delimiters
   - Table: one Row per table row, whose children are the cells (Rows)
*/
struct MathNode {
  enum class Kind { Ident, Number, Op, Text, Row, Frac, Binom, Sqrt, Root,
                    Scripts, Fenced, Table };
  Kind kind = Kind::Row;
  //! Ident, Number, Op, Text: the text. Fenced: the opening delimiter.
  wxString text;
  //! Fenced: the closing delimiter
  wxString close;
  //! Ident: a function's name (\sin, \operatorname{...}, <mi>sin</mi>)
  bool function = false;
  //! Ident: the text already is Maxima (%pi, %e, %i, inf)
  bool literal = false;
  //! Ident: set upright (\mathrm{d}, mathvariant="normal"), i.e. a constant
  bool upright = false;
  //! Scripts: which of the scripts exist
  bool hasSub = false;
  bool hasSup = false;
  //! Table: lines of an equation array, not a matrix
  bool aligned = false;
  std::vector<MathNode> children;

  MathNode() = default;
  MathNode(Kind k, wxString t = {}) : kind(k), text(std::move(t)) {}

  bool IsOp(const wxString &op) const { return kind == Kind::Op && text == op; }
  bool IsEmptyRow() const { return kind == Kind::Row && children.empty(); }
};

MathNode Ident(const wxString &name, bool function = false) {
  MathNode n(MathNode::Kind::Ident, name);
  n.function = function;
  return n;
}

MathNode Literal(const wxString &maxima) {
  MathNode n(MathNode::Kind::Ident, maxima);
  n.literal = true;
  return n;
}

MathNode Op(const wxString &op) { return {MathNode::Kind::Op, op}; }

MathNode Row(std::vector<MathNode> items = {}) {
  MathNode n(MathNode::Kind::Row);
  n.children = std::move(items);
  return n;
}

MathNode Two(MathNode::Kind kind, MathNode &&a, MathNode &&b) {
  MathNode n(kind);
  n.children.push_back(std::move(a));
  n.children.push_back(std::move(b));
  return n;
}

MathNode Fenced(const wxString &open, const wxString &close, MathNode &&contents) {
  MathNode n(MathNode::Kind::Fenced, open);
  n.close = close;
  n.children.push_back(std::move(contents));
  return n;
}

//! Gives a node a sub- or superscript, creating the Scripts node if needed
void AttachScript(MathNode &node, bool sup, MathNode &&script) {
  if ((node.kind != MathNode::Kind::Scripts) ||
      (sup ? node.hasSup : node.hasSub)) {
    MathNode scripts(MathNode::Kind::Scripts);
    scripts.children.push_back(std::move(node));
    scripts.children.push_back(Row());
    scripts.children.push_back(Row());
    node = std::move(scripts);
  }
  if (sup) {
    node.hasSup = true;
    node.children[2] = std::move(script);
  } else {
    node.hasSub = true;
    node.children[1] = std::move(script);
  }
}

//! Greek letters by their LaTeX name, which is also how Maxima spells them
struct GreekLetter {
  const wchar_t *name;
  wchar_t letter;
};
constexpr std::array<GreekLetter, 40> greekLetters{{
    {L"alpha", L'α'}, {L"beta", L'β'}, {L"gamma", L'γ'},
    {L"delta", L'δ'}, {L"epsilon", L'ε'}, {L"zeta", L'ζ'},
    {L"eta", L'η'}, {L"theta", L'θ'}, {L"iota", L'ι'},
    {L"kappa", L'κ'}, {L"lambda", L'λ'}, {L"mu", L'μ'},
    {L"nu", L'ν'}, {L"xi", L'ξ'}, {L"omicron", L'ο'},
    {L"rho", L'ρ'}, {L"sigma", L'σ'}, {L"tau", L'τ'},
    {L"upsilon", L'υ'}, {L"phi", L'φ'}, {L"chi", L'χ'},
    {L"psi", L'ψ'}, {L"omega", L'ω'},
    {L"Gamma", L'Γ'}, {L"Delta", L'Δ'}, {L"Theta", L'Θ'},
    {L"Lambda", L'Λ'}, {L"Xi", L'Ξ'}, {L"Pi", L'Π'},
    {L"Sigma", L'Σ'}, {L"Upsilon", L'Υ'}, {L"Phi", L'Φ'},
    {L"Psi", L'Ψ'}, {L"Omega", L'Ω'},
    // The variant shapes are the same letters
    {L"varepsilon", L'ε'}, {L"vartheta", L'ϑ'},
    {L"varphi", L'ϕ'}, {L"varrho", L'ϱ'}, {L"varsigma", L'ς'},
    {L"varpi", L'ϖ'}}};

//! The Maxima name of a Greek letter given as a character, if it is one
std::optional<wxString> GreekName(wxUniChar ch) {
  for (const auto &g : greekLetters)
    if (ch == g.letter) {
      wxString name(g.name);
      if (name.StartsWith(wxS("var")))
        name = name.Mid(3);
      return name;
    }
  return std::nullopt;
}

//! Function names as LaTeX or MathML spell them, and Maxima's name for them
struct FunctionName {
  const wchar_t *written;
  const wchar_t *maxima;
};
constexpr std::array<FunctionName, 44> functionNames{{
    {L"sin", L"sin"}, {L"cos", L"cos"}, {L"tan", L"tan"}, {L"cot", L"cot"},
    {L"sec", L"sec"}, {L"csc", L"csc"},
    {L"sinh", L"sinh"}, {L"cosh", L"cosh"}, {L"tanh", L"tanh"},
    {L"coth", L"coth"}, {L"sech", L"sech"}, {L"csch", L"csch"},
    {L"arcsin", L"asin"}, {L"arccos", L"acos"}, {L"arctan", L"atan"},
    {L"arccot", L"acot"}, {L"arcsec", L"asec"}, {L"arccsc", L"acsc"},
    {L"asin", L"asin"}, {L"acos", L"acos"}, {L"atan", L"atan"},
    {L"arsinh", L"asinh"}, {L"arcosh", L"acosh"}, {L"artanh", L"atanh"},
    {L"arcoth", L"acoth"}, {L"arcsinh", L"asinh"}, {L"arccosh", L"acosh"},
    {L"arctanh", L"atanh"}, {L"asinh", L"asinh"}, {L"acosh", L"acosh"},
    {L"atanh", L"atanh"},
    {L"exp", L"exp"}, {L"ln", L"log"}, {L"log", L"log"}, {L"lg", L"lg"},
    {L"det", L"determinant"}, {L"gcd", L"gcd"}, {L"max", L"max"},
    {L"min", L"min"}, {L"sgn", L"signum"}, {L"Re", L"realpart"},
    {L"Im", L"imagpart"}, {L"arg", L"carg"}, {L"erf", L"erf"}}};

std::optional<wxString> MaximaFunctionName(const wxString &written) {
  for (const auto &f : functionNames)
    if (written == f.written)
      return wxString(f.maxima);
  return std::nullopt;
}

//! Functions whose ^{-1} means the inverse function, not a reciprocal
bool HasInverseNotation(const wxString &maxima) {
  static const wxString trig[] = {wxS("sin"),  wxS("cos"),  wxS("tan"),
                                  wxS("cot"),  wxS("sec"),  wxS("csc"),
                                  wxS("sinh"), wxS("cosh"), wxS("tanh"),
                                  wxS("coth"), wxS("sech"), wxS("csch")};
  for (const auto &t : trig)
    if (maxima == t)
      return true;
  return false;
}

/*! A symbol both front ends may meet as a single Unicode character

  LaTeX input may contain pasted Unicode, and MathML always does. Returns the
  layout node for it, or nothing if the character isn't special.
*/
std::optional<MathNode> UnicodeSymbol(wxUniChar ch) {
  switch (static_cast<wxUint32>(ch.GetValue())) {
  case 0x2212: return Op(wxS("-"));          // minus sign
  case 0x22C5:                               // dot operator
  case 0x00B7:                               // middle dot
  case 0x00D7:                               // multiplication sign
  case 0x2217:                               // asterisk operator
  case 0x2062: return Op(wxS("*"));          // invisible times
  case 0x00F7:                               // division sign
  case 0x2215: return Op(wxS("/"));          // division slash
  case 0x2264:
  case 0x2A7D: return Op(wxS("<="));
  case 0x2265:
  case 0x2A7E: return Op(wxS(">="));
  case 0x2260: return Op(wxS("#"));
  case 0x2192:
  case 0x27F6: return Op(wxS("->"));
  case 0x00B1:
  case 0x2213: return Op(wxS("±"));     // plus-minus
  case 0x2032: return Op(wxS("'"));          // prime
  case 0x2061: return Op(wxS("⁡"));     // function application
  case 0x2063: return Op(wxS(","));          // invisible separator
  case 0x2026:
  case 0x22EF: return Op(wxS("…"));     // ellipsis
  case 0x2016: return Op(wxS("‖"));     // double vertical bar
  case 0x27E8:
  case 0x2329: return Op(wxS("⟨"));
  case 0x27E9:
  case 0x232A: return Op(wxS("⟩"));
  case 0x2211: return Ident(wxS("sum"), true);
  case 0x220F: return Ident(wxS("prod"), true);
  case 0x222B: return Ident(wxS("int"), true);
  case 0x2202: return Ident(wxS("∂"));  // partial
  case 0x2146: return Ident(wxS("ⅆ"));  // differential d
  case 0x2147: return Literal(wxS("%e"));    // exponential e
  case 0x2148: return Literal(wxS("%i"));    // imaginary i
  case 0x03C0: return Literal(wxS("%pi"));
  case 0x221E: return Literal(wxS("inf"));
  default: break;
  }
  if (auto greek = GreekName(ch))
    return Ident(*greek);
  return std::nullopt;
}

// ---------------------------------------------------------------------------
// The LaTeX front end
// ---------------------------------------------------------------------------

struct LaTeXToken {
  enum class Kind { Command, Char, End };
  Kind kind = Kind::End;
  //! A command's name without the backslash, or the character
  wxString text;
  //! There was white space in front of it (only matters inside \text)
  bool spaceBefore = false;
};

std::vector<LaTeXToken> LexLaTeX(const wxString &src) {
  std::vector<LaTeXToken> tokens;
  bool space = false;
  for (auto it = src.begin(); it != src.end(); ++it) {
    const wxUniChar ch = *it;
    if (wxIsspace(ch)) {
      space = true;
      continue;
    }
    if (ch == wxS('%')) {
      // A comment, to the end of the line
      while ((it != src.end()) && (*it != wxS('\n')))
        ++it;
      if (it == src.end())
        break;
      space = true;
      continue;
    }
    LaTeXToken token;
    token.spaceBefore = space;
    space = false;
    if (ch == wxS('\\')) {
      token.kind = LaTeXToken::Kind::Command;
      auto next = it;
      ++next;
      if (next == src.end())
        Fail(_("The LaTeX code ends with a lone backslash."));
      if (((*next >= wxS('a')) && (*next <= wxS('z'))) ||
          ((*next >= wxS('A')) && (*next <= wxS('Z')))) {
        while ((next != src.end()) &&
               (((*next >= wxS('a')) && (*next <= wxS('z'))) ||
                ((*next >= wxS('A')) && (*next <= wxS('Z'))))) {
          token.text += *next;
          ++next;
        }
        --next;
      } else
        token.text = *next;
      it = next;
    } else {
      token.kind = LaTeXToken::Kind::Char;
      token.text = ch;
    }
    tokens.push_back(std::move(token));
  }
  return tokens;
}

/*! Reads LaTeX tokens into the layout tree

  Recursive descent along TeX's own structure: a row of atoms, each of which
  may get scripts; groups in braces; commands with their arguments.
*/
class LaTeXParser {
public:
  explicit LaTeXParser(std::vector<LaTeXToken> tokens)
    : m_tokens(std::move(tokens)) {}

  MathNode ParseAll() {
    MathNode row = ParseRow();
    if (!AtEnd()) {
      const LaTeXToken &t = Peek();
      if (t.kind == LaTeXToken::Kind::Command)
        Fail(wxString::Format(_("Unexpected \\%s in the LaTeX code."), t.text));
      Fail(wxString::Format(_("Unexpected \"%s\" in the LaTeX code."), t.text));
    }
    return row;
  }

private:
  std::vector<LaTeXToken> m_tokens;
  std::size_t m_pos = 0;

  bool AtEnd() const { return m_pos >= m_tokens.size(); }
  const LaTeXToken &Peek() const {
    static const LaTeXToken end;
    return AtEnd() ? end : m_tokens[m_pos];
  }
  bool PeekChar(const wxString &ch) const {
    return (Peek().kind == LaTeXToken::Kind::Char) && (Peek().text == ch);
  }
  bool PeekCommand(const wxString &name) const {
    return (Peek().kind == LaTeXToken::Kind::Command) && (Peek().text == name);
  }
  void Expect(const wxString &ch) {
    if (!PeekChar(ch))
      Fail(wxString::Format(_("A \"%s\" is missing in the LaTeX code."), ch));
    ++m_pos;
  }

  //! Where a row ends: a closing brace, a cell or line separator, \right, \end
  bool AtRowEnd(const wxString &stopChar) const {
    if (AtEnd())
      return true;
    if (PeekChar(wxS("}")) || PeekChar(wxS("&")) ||
        (!stopChar.IsEmpty() && PeekChar(stopChar)))
      return true;
    return PeekCommand(wxS("right")) || PeekCommand(wxS("\\")) ||
      PeekCommand(wxS("end")) || PeekCommand(wxS("cr"));
  }

  MathNode ParseRow(const wxString &stopChar = {}) {
    MathNode row = Row();
    while (!AtRowEnd(stopChar)) {
      if (PeekChar(wxS("^")) || PeekChar(wxS("_"))) {
        const bool sup = PeekChar(wxS("^"));
        ++m_pos;
        if (row.children.empty())
          Fail(sup ? _("A superscript has nothing to attach to.")
                   : _("A subscript has nothing to attach to."));
        AttachScript(row.children.back(), sup, ParseArgument());
        continue;
      }
      if (auto atom = ParseAtom())
        row.children.push_back(std::move(*atom));
    }
    return row;
  }

  //! A command's or a script's argument: a group, or a single token
  MathNode ParseArgument() {
    while (!AtEnd()) {
      if (PeekChar(wxS("{"))) {
        ++m_pos;
        MathNode group = ParseRow();
        Expect(wxS("}"));
        return group;
      }
      // x^23 is x squared times 3: an argument without braces is one token.
      if ((Peek().kind == LaTeXToken::Kind::Char) && wxIsdigit(Peek().text[0])) {
        MathNode digit(MathNode::Kind::Number, Peek().text);
        ++m_pos;
        return digit;
      }
      if (auto atom = ParseAtom())
        return std::move(*atom);
    }
    Fail(_("A LaTeX command lacks its argument."));
  }

  //! The raw text of an argument in braces (for \text and environment names)
  wxString ParseRawArgument() {
    if (!PeekChar(wxS("{")))
      Fail(_("A LaTeX command lacks its argument."));
    ++m_pos;
    wxString text;
    int depth = 0;
    while (!AtEnd()) {
      const LaTeXToken &t = Peek();
      if (t.kind == LaTeXToken::Kind::Char && t.text == wxS("}")) {
        if (depth == 0)
          break;
        --depth;
      }
      if (t.kind == LaTeXToken::Kind::Char && t.text == wxS("{"))
        ++depth;
      if (t.spaceBefore && !text.IsEmpty())
        text += wxS(" ");
      if (t.kind == LaTeXToken::Kind::Command)
        text += wxS("\\");
      text += t.text;
      ++m_pos;
    }
    Expect(wxS("}"));
    return text;
  }

  //! The delimiter after \left, \right, \bigl and friends
  wxString ParseDelimiter() {
    const LaTeXToken t = Peek();
    if (AtEnd())
      Fail(_("A \\left or \\right lacks its delimiter."));
    ++m_pos;
    if (t.kind == LaTeXToken::Kind::Char) {
      if (t.text == wxS("."))
        return {};
      return t.text;
    }
    if ((t.text == wxS("{")) || (t.text == wxS("lbrace")))
      return wxS("{");
    if ((t.text == wxS("}")) || (t.text == wxS("rbrace")))
      return wxS("}");
    if ((t.text == wxS("lvert")) || (t.text == wxS("rvert")) ||
        (t.text == wxS("vert")))
      return wxS("|");
    if ((t.text == wxS("|")) || (t.text == wxS("lVert")) ||
        (t.text == wxS("rVert")) || (t.text == wxS("Vert")))
      return wxS("‖");
    if (t.text == wxS("langle"))
      return wxS("⟨");
    if (t.text == wxS("rangle"))
      return wxS("⟩");
    if (t.text == wxS("lbrack"))
      return wxS("[");
    if (t.text == wxS("rbrack"))
      return wxS("]");
    Fail(wxString::Format(_("The LaTeX delimiter \\%s isn't supported."), t.text));
  }

  /*! The letters of a font command's argument, read as one name

    \mathrm{d} is an upright d, \mathit{speed} a variable called speed.
  */
  static MathNode FontArgument(MathNode &&arg, bool upright) {
    if (arg.kind != MathNode::Kind::Row) {
      if (arg.kind == MathNode::Kind::Ident && upright)
        arg.upright = true;
      return std::move(arg);
    }
    wxString name;
    for (const MathNode &item : arg.children) {
      if ((item.kind != MathNode::Kind::Ident && item.kind != MathNode::Kind::Number) ||
          item.function || item.literal) {
        name.Clear();
        break;
      }
      name += item.text;
    }
    if (!name.IsEmpty() && !wxIsdigit(name[0])) {
      MathNode ident = Ident(name);
      ident.upright = upright;
      return ident;
    }
    if (arg.children.size() == 1)
      return FontArgument(std::move(arg.children[0]), upright);
    return std::move(arg);
  }

  //! A table: the rows of a matrix or of an equation array, up to \end{env}
  MathNode ParseTable(const wxString &env, bool aligned) {
    MathNode table(MathNode::Kind::Table);
    table.aligned = aligned;
    MathNode row = Row();
    for (;;) {
      row.children.push_back(ParseRow());
      if (PeekChar(wxS("&"))) {
        ++m_pos;
        continue;
      }
      if (PeekCommand(wxS("\\")) || PeekCommand(wxS("cr"))) {
        ++m_pos;
        // \\[2pt] adds vertical space, which means nothing here
        if (PeekChar(wxS("[")))
          while (!AtEnd() && !PeekChar(wxS("]")))
            ++m_pos;
        if (PeekChar(wxS("]")))
          ++m_pos;
        table.children.push_back(std::move(row));
        row = Row();
        continue;
      }
      if (PeekCommand(wxS("end"))) {
        ++m_pos;
        if (ParseRawArgument() != env)
          Fail(wxString::Format(_("\\begin{%s} is closed by a different \\end."), env));
        break;
      }
      Fail(wxString::Format(_("\\begin{%s} has no \\end."), env));
    }
    // The last line ends with \\ more often than not
    bool lastRowEmpty = true;
    for (const MathNode &cell : row.children)
      if (!cell.IsEmptyRow())
        lastRowEmpty = false;
    if (!lastRowEmpty)
      table.children.push_back(std::move(row));
    if (aligned) {
      // x &= 1 is just x = 1: the & only aligns
      for (MathNode &line : table.children) {
        MathNode merged = Row();
        for (MathNode &cell : line.children)
          merged.children.push_back(std::move(cell));
        line.children.clear();
        line.children.push_back(std::move(merged));
      }
    }
    return table;
  }

  MathNode ParseEnvironment() {
    const wxString env = ParseRawArgument();
    wxString name = env;
    const bool starred = name.EndsWith(wxS("*"));
    if (starred)
      name.RemoveLast();
    if ((name == wxS("equation")) || (name == wxS("displaymath")) ||
        (name == wxS("math"))) {
      MathNode contents = ParseRow();
      if (!PeekCommand(wxS("end")))
        Fail(wxString::Format(_("\\begin{%s} has no \\end."), env));
      ++m_pos;
      ParseRawArgument();
      return contents;
    }
    if ((name == wxS("align")) || (name == wxS("aligned")) ||
        (name == wxS("gather")) || (name == wxS("gathered")) ||
        (name == wxS("eqnarray")) || (name == wxS("split")) ||
        (name == wxS("multline")) || (name == wxS("alignat")) ||
        (name == wxS("alignedat"))) {
      if ((name == wxS("alignat")) || (name == wxS("alignedat")))
        ParseRawArgument(); // the number of columns
      return ParseTable(env, true);
    }
    if (name == wxS("array"))
      ParseRawArgument(); // the column specification
    if ((name == wxS("matrix")) || (name == wxS("smallmatrix")) ||
        (name == wxS("array")))
      return ParseTable(env, false);
    if (name == wxS("pmatrix"))
      return Fenced(wxS("("), wxS(")"), Row({ParseTable(env, false)}));
    if (name == wxS("bmatrix"))
      return Fenced(wxS("["), wxS("]"), Row({ParseTable(env, false)}));
    if (name == wxS("vmatrix"))
      return Fenced(wxS("|"), wxS("|"), Row({ParseTable(env, false)}));
    Fail(wxString::Format(_("The LaTeX environment \"%s\" isn't supported."), env));
  }

  //! One atom, or nothing for a command that only changes spacing or style
  std::optional<MathNode> ParseAtom() {
    const LaTeXToken t = Peek();
    ++m_pos;
    if (t.kind == LaTeXToken::Kind::Char)
      return ParseCharAtom(t.text);
    return ParseCommand(t.text);
  }

  std::optional<MathNode> ParseCharAtom(const wxString &ch) {
    const wxUniChar c = ch[0];
    if (wxIsdigit(c) || ((c == wxS('.')) && (m_pos < m_tokens.size()) &&
                         (Peek().kind == LaTeXToken::Kind::Char) &&
                         wxIsdigit(Peek().text[0]))) {
      wxString number = ch;
      bool point = (c == wxS('.'));
      while (Peek().kind == LaTeXToken::Kind::Char) {
        const wxUniChar n = Peek().text[0];
        if (wxIsdigit(n)) {
          number += n;
          ++m_pos;
        } else if ((n == wxS('.')) && !point && (m_pos + 1 < m_tokens.size()) &&
                   (m_tokens[m_pos + 1].kind == LaTeXToken::Kind::Char) &&
                   wxIsdigit(m_tokens[m_pos + 1].text[0])) {
          number += n;
          point = true;
          ++m_pos;
        } else
          break;
      }
      return MathNode(MathNode::Kind::Number, std::move(number));
    }
    if (((c >= wxS('a')) && (c <= wxS('z'))) || ((c >= wxS('A')) && (c <= wxS('Z'))))
      return Ident(ch);
    if (ch == wxS("{")) {
      MathNode group = ParseRow();
      Expect(wxS("}"));
      return group;
    }
    if (ch == wxS("~"))
      return std::nullopt; // a non-breaking space
    if (ch == wxS(":") && PeekChar(wxS("="))) {
      ++m_pos;
      return Op(wxS(":="));
    }
    static const wxString ops = wxS("()[]+-=<>/*!,;:|'.");
    if (ops.Find(c) != wxNOT_FOUND)
      return Op(ch);
    if (auto symbol = UnicodeSymbol(c))
      return symbol;
    if (wxIsalpha(c))
      return Ident(ch);
    Fail(wxString::Format(_("Unexpected \"%s\" in the LaTeX code."), ch));
  }

  std::optional<MathNode> ParseCommand(const wxString &name) {
    // Spacing and style that mean nothing for the maths
    static const wxString ignored[] = {
      wxS(","), wxS(";"), wxS(":"), wxS("!"), wxS(" "), wxS(">"),
      wxS("quad"), wxS("qquad"), wxS("thinspace"), wxS("medspace"),
      wxS("thickspace"), wxS("negthinspace"), wxS("displaystyle"),
      wxS("textstyle"), wxS("scriptstyle"), wxS("scriptscriptstyle"),
      wxS("limits"), wxS("nolimits"), wxS("big"), wxS("Big"), wxS("bigg"),
      wxS("Bigg"), wxS("bigl"), wxS("bigr"), wxS("Bigl"), wxS("Bigr"),
      wxS("biggl"), wxS("biggr"), wxS("Biggl"), wxS("Biggr"), wxS("bigm"),
      wxS("Bigm"), wxS("nonumber"), wxS("notag"), wxS("left.")};
    for (const auto &i : ignored)
      if (name == i)
        return std::nullopt;

    if (name == wxS("left")) {
      const wxString open = ParseDelimiter();
      MathNode contents = ParseRow();
      if (!PeekCommand(wxS("right")))
        Fail(_("A \\left has no matching \\right."));
      ++m_pos;
      return Fenced(open, ParseDelimiter(), std::move(contents));
    }
    if (name == wxS("right"))
      Fail(_("A \\right has no matching \\left."));
    if ((name == wxS("frac")) || (name == wxS("dfrac")) ||
        (name == wxS("tfrac")) || (name == wxS("cfrac"))) {
      MathNode num = ParseArgument();
      return Two(MathNode::Kind::Frac, std::move(num), ParseArgument());
    }
    if ((name == wxS("binom")) || (name == wxS("dbinom")) || (name == wxS("tbinom"))) {
      MathNode n = ParseArgument();
      return Two(MathNode::Kind::Binom, std::move(n), ParseArgument());
    }
    if (name == wxS("sqrt")) {
      if (PeekChar(wxS("["))) {
        ++m_pos;
        MathNode index = ParseRow(wxS("]"));
        Expect(wxS("]"));
        MathNode radicand = ParseArgument();
        return Two(MathNode::Kind::Root, std::move(radicand), std::move(index));
      }
      MathNode root(MathNode::Kind::Sqrt);
      root.children.push_back(ParseArgument());
      return root;
    }
    if ((name == wxS("mathrm")) || (name == wxS("mathup")) ||
        (name == wxS("textrm")) || (name == wxS("rm")))
      return FontArgument(ParseArgument(), true);
    if ((name == wxS("mathit")) || (name == wxS("mathbf")) ||
        (name == wxS("mathsf")) || (name == wxS("mathtt")) ||
        (name == wxS("mathnormal")) || (name == wxS("boldsymbol")) ||
        (name == wxS("bm")) || (name == wxS("mathbb")) ||
        (name == wxS("mathcal")) || (name == wxS("mathfrak")) ||
        (name == wxS("textit")) || (name == wxS("textbf")))
      return FontArgument(ParseArgument(), false);
    if ((name == wxS("text")) || (name == wxS("mbox")) || (name == wxS("hbox"))) {
      const wxString text = ParseRawArgument();
      // \text{d}x and \text{speed} are names; anything else is prose
      bool word = !text.IsEmpty();
      for (const auto ch : text)
        if (!wxIsalpha(ch))
          word = false;
      if (word) {
        MathNode ident = Ident(text);
        ident.upright = true;
        return ident;
      }
      return MathNode(MathNode::Kind::Text, text);
    }
    if (name == wxS("operatorname")) {
      if (PeekChar(wxS("*")))
        ++m_pos;
      MathNode fn = FontArgument(ParseArgument(), true);
      if (fn.kind != MathNode::Kind::Ident)
        Fail(_("\\operatorname needs a name."));
      if (auto maxima = MaximaFunctionName(fn.text))
        fn.text = *maxima;
      fn.function = true;
      return fn;
    }
    if (name == wxS("begin"))
      return ParseEnvironment();
    if (name == wxS("end"))
      Fail(_("An \\end has no matching \\begin."));

    if (name == wxS("pi"))
      return Literal(wxS("%pi"));
    if (name == wxS("infty"))
      return Literal(wxS("inf"));
    if (name == wxS("imath"))
      return Literal(wxS("%i"));
    if (name == wxS("partial"))
      return Ident(wxS("∂"));
    for (const auto &g : greekLetters)
      if (name == g.name) {
        wxString greek(g.name);
        if (greek.StartsWith(wxS("var")))
          greek = greek.Mid(3);
        if (greek == wxS("pi"))
          return Literal(wxS("%pi"));
        return Ident(greek);
      }
    if (auto fn = MaximaFunctionName(name)) {
      MathNode ident = Ident(*fn, true);
      // \log and \lg differ only in their base: keep which one it was
      if (name == wxS("lg"))
        ident.text = wxS("lg");
      return ident;
    }
    if (name == wxS("sum"))
      return Ident(wxS("sum"), true);
    if (name == wxS("prod"))
      return Ident(wxS("prod"), true);
    if (name == wxS("int"))
      return Ident(wxS("int"), true);
    if (name == wxS("lim"))
      return Ident(wxS("lim"), true);

    struct Operator {
      const wchar_t *command;
      const wchar_t *op;
    };
    static constexpr std::array<Operator, 39> operators{{
        {L"cdot", L"*"}, {L"times", L"*"}, {L"ast", L"*"}, {L"div", L"/"},
        {L"le", L"<="}, {L"leq", L"<="}, {L"leqslant", L"<="},
        {L"ge", L">="}, {L"geq", L">="}, {L"geqslant", L">="},
        {L"ne", L"#"}, {L"neq", L"#"}, {L"lt", L"<"}, {L"gt", L">"},
        {L"to", L"->"}, {L"rightarrow", L"->"}, {L"longrightarrow", L"->"},
        {L"pm", L"±"}, {L"mp", L"±"}, {L"colon", L":"},
        {L"coloneqq", L":="}, {L"prime", L"'"},
        {L"lvert", L"|"}, {L"rvert", L"|"}, {L"vert", L"|"}, {L"mid", L"|"},
        {L"lVert", L"‖"}, {L"rVert", L"‖"}, {L"Vert", L"‖"},
        {L"|", L"‖"}, {L"{", L"{"}, {L"}", L"}"}, {L"lbrace", L"{"},
        {L"rbrace", L"}"}, {L"langle", L"⟨"}, {L"rangle", L"⟩"},
        {L"ldots", L"…"}, {L"cdots", L"…"}, {L"dots", L"…"}}};
    for (const auto &o : operators)
      if (name == o.command)
        return Op(o.op);
    if (name == wxS("\\"))
      Fail(_("A line break (\\\\) is only understood inside an equation array or a matrix."));
    Fail(wxString::Format(_("The LaTeX command \\%s isn't supported."), name));
  }
};

//! Removes the $...$, $$...$$, \(...\) or \[...\] around a formula
wxString StripMathDelimiters(wxString latex) {
  latex.Trim(true).Trim(false);
  const std::pair<const wxChar *, const wxChar *> delimiters[] = {
    {wxS("$$"), wxS("$$")}, {wxS("\\["), wxS("\\]")},
    {wxS("\\("), wxS("\\)")}, {wxS("$"), wxS("$")}};
  for (const auto &[open, close] : delimiters) {
    const wxString o(open), c(close);
    if ((latex.Length() >= o.Length() + c.Length()) && latex.StartsWith(o) &&
        latex.EndsWith(c)) {
      latex = latex.Mid(o.Length(), latex.Length() - o.Length() - c.Length());
      break;
    }
  }
  return latex;
}

// ---------------------------------------------------------------------------
// The MathML front end
// ---------------------------------------------------------------------------

//! Named entities MathML generators like to use; XML itself knows only five
wxString ReplaceNamedEntities(wxString mathml) {
  static const std::pair<const wxChar *, const wxChar *> entities[] = {
    {wxS("&InvisibleTimes;"), wxS("&#x2062;")}, {wxS("&it;"), wxS("&#x2062;")},
    {wxS("&ApplyFunction;"), wxS("&#x2061;")}, {wxS("&af;"), wxS("&#x2061;")},
    {wxS("&InvisibleComma;"), wxS("&#x2063;")}, {wxS("&ic;"), wxS("&#x2063;")},
    {wxS("&minus;"), wxS("&#x2212;")}, {wxS("&times;"), wxS("&#xD7;")},
    {wxS("&sdot;"), wxS("&#x22C5;")}, {wxS("&middot;"), wxS("&#xB7;")},
    {wxS("&div;"), wxS("&#xF7;")}, {wxS("&PlusMinus;"), wxS("&#xB1;")},
    {wxS("&pm;"), wxS("&#xB1;")}, {wxS("&pi;"), wxS("&#x3C0;")},
    {wxS("&infin;"), wxS("&#x221E;")}, {wxS("&int;"), wxS("&#x222B;")},
    {wxS("&sum;"), wxS("&#x2211;")}, {wxS("&prod;"), wxS("&#x220F;")},
    {wxS("&le;"), wxS("&#x2264;")}, {wxS("&ge;"), wxS("&#x2265;")},
    {wxS("&ne;"), wxS("&#x2260;")}, {wxS("&rarr;"), wxS("&#x2192;")},
    {wxS("&RightArrow;"), wxS("&#x2192;")}, {wxS("&prime;"), wxS("&#x2032;")},
    {wxS("&nbsp;"), wxS(" ")}, {wxS("&ExponentialE;"), wxS("&#x2147;")},
    {wxS("&ee;"), wxS("&#x2147;")}, {wxS("&ImaginaryI;"), wxS("&#x2148;")},
    {wxS("&ii;"), wxS("&#x2148;")}, {wxS("&DifferentialD;"), wxS("&#x2146;")},
    {wxS("&dd;"), wxS("&#x2146;")}, {wxS("&PartialD;"), wxS("&#x2202;")},
    {wxS("&part;"), wxS("&#x2202;")}, {wxS("&infty;"), wxS("&#x221E;")}};
  for (const auto &[name, ref] : entities)
    mathml.Replace(name, ref);
  return mathml;
}

//! An element's name without a namespace prefix (m:mi -> mi)
wxString LocalName(const wxXmlNode *node) {
  wxString name = node->GetName();
  const int colon = name.Find(wxS(':'));
  if (colon != wxNOT_FOUND)
    name = name.Mid(static_cast<std::size_t>(colon) + 1);
  return name;
}

std::vector<const wxXmlNode *> ChildElements(const wxXmlNode *node) {
  std::vector<const wxXmlNode *> children;
  for (const wxXmlNode *c = node->GetChildren(); c; c = c->GetNext())
    if (c->GetType() == wxXML_ELEMENT_NODE)
      children.push_back(c);
  return children;
}

wxString TextOf(const wxXmlNode *node) {
  wxString text = node->GetNodeContent();
  text.Trim(true).Trim(false);
  return text;
}

//! The Maxima command an annotation of the formula spells out, if any
std::optional<wxString> MaximaAnnotation(const wxXmlNode *node) {
  if (LocalName(node) == wxS("annotation")) {
    const wxString encoding = node->GetAttribute(wxS("encoding")).Lower();
    if ((encoding == wxS("application/x-maxima")) || (encoding == wxS("maxima")))
      return TextOf(node);
  }
  for (const wxXmlNode *c : ChildElements(node))
    if (auto annotation = MaximaAnnotation(c))
      return annotation;
  return std::nullopt;
}

class MathMLReader {
public:
  static MathNode Read(const wxXmlNode *node) {
    const wxString name = LocalName(node);
    const auto children = ChildElements(node);

    if ((name == wxS("math")) || (name == wxS("mrow")) ||
        (name == wxS("mstyle")) || (name == wxS("mpadded")) ||
        (name == wxS("menclose")) || (name == wxS("mtd")))
      return RowOf(children);
    if ((name == wxS("semantics")) || (name == wxS("maction"))) {
      // The first child is the presentation; the rest are annotations
      if (children.empty())
        return Row();
      return Read(children.front());
    }
    if ((name == wxS("mphantom")) || (name == wxS("mspace")) ||
        (name == wxS("annotation")) || (name == wxS("annotation-xml")) ||
        (name == wxS("none")) || (name == wxS("mprescripts")))
      return Row();
    if (name == wxS("mi"))
      return ReadIdentifier(node);
    if (name == wxS("mn"))
      return {MathNode::Kind::Number, TextOf(node)};
    if (name == wxS("mo"))
      return ReadOperator(TextOf(node));
    if ((name == wxS("mtext")) || (name == wxS("ms")))
      return {MathNode::Kind::Text, TextOf(node)};
    if (name == wxS("mfrac")) {
      Arity(name, children, 2);
      const bool binom = (node->GetAttribute(wxS("linethickness")) == wxS("0"));
      return Two(binom ? MathNode::Kind::Binom : MathNode::Kind::Frac,
                 Read(children[0]), Read(children[1]));
    }
    if (name == wxS("msqrt")) {
      MathNode sqrt(MathNode::Kind::Sqrt);
      sqrt.children.push_back(RowOf(children));
      return sqrt;
    }
    if (name == wxS("mroot")) {
      Arity(name, children, 2);
      return Two(MathNode::Kind::Root, Read(children[0]), Read(children[1]));
    }
    if ((name == wxS("msup")) || (name == wxS("msub")) ||
        (name == wxS("mover")) || (name == wxS("munder"))) {
      Arity(name, children, 2);
      const bool sup = (name == wxS("msup")) || (name == wxS("mover"));
      MathNode script = Read(children[1]);
      if ((name == wxS("mover")) || (name == wxS("munder")))
        RejectAccent(node, script);
      MathNode base = Read(children[0]);
      AttachScript(base, sup, std::move(script));
      return base;
    }
    if ((name == wxS("msubsup")) || (name == wxS("munderover"))) {
      Arity(name, children, 3);
      MathNode base = Read(children[0]);
      AttachScript(base, false, Read(children[1]));
      AttachScript(base, true, Read(children[2]));
      return base;
    }
    if (name == wxS("mfenced")) {
      wxString open = wxS("("), close = wxS(")");
      node->GetAttribute(wxS("open"), &open);
      node->GetAttribute(wxS("close"), &close);
      MathNode contents = Row();
      for (const wxXmlNode *c : children) {
        if (!contents.children.empty())
          contents.children.push_back(Op(wxS(",")));
        contents.children.push_back(Read(c));
      }
      return Fenced(open.Trim(true).Trim(false), close.Trim(true).Trim(false),
                    std::move(contents));
    }
    if (name == wxS("mtable")) {
      MathNode table(MathNode::Kind::Table);
      for (const wxXmlNode *tr : children) {
        auto cells = ChildElements(tr);
        // A labeled row's first child is its label
        if ((LocalName(tr) == wxS("mlabeledtr")) && !cells.empty())
          cells.erase(cells.begin());
        MathNode row = Row();
        for (const wxXmlNode *td : cells)
          row.children.push_back(Read(td));
        table.children.push_back(std::move(row));
      }
      return table;
    }
    if (name == wxS("apply"))
      Fail(_("Content MathML isn't supported, only presentation MathML."));
    Fail(wxString::Format(_("The MathML element <%s> isn't supported."), name));
  }

private:
  static MathNode RowOf(const std::vector<const wxXmlNode *> &children) {
    MathNode row = Row();
    for (const wxXmlNode *c : children)
      row.children.push_back(Read(c));
    return row;
  }

  static void Arity(const wxString &name, const std::vector<const wxXmlNode *> &children,
                    std::size_t n) {
    if (children.size() != n)
      Fail(wxString::Format(_("A MathML <%s> element has the wrong number of children."),
                            name));
  }

  //! A bar, hat, dot or arrow over a letter has no Maxima equivalent
  static void RejectAccent(const wxXmlNode *node, const MathNode &script) {
    const bool accent = (node->GetAttribute(wxS("accent")) == wxS("true")) ||
      (node->GetAttribute(wxS("accentunder")) == wxS("true"));
    if (accent || (script.kind == MathNode::Kind::Op && script.text != wxS("->")))
      Fail(_("Accents (bars, hats, dots or arrows over a symbol) have no Maxima meaning."));
  }

  static MathNode ReadIdentifier(const wxXmlNode *node) {
    const wxString text = TextOf(node);
    if (text.IsEmpty())
      return Row();
    if (text.Length() == 1) {
      if (auto symbol = UnicodeSymbol(text[0]))
        return *symbol;
      MathNode ident = Ident(text);
      ident.upright = (node->GetAttribute(wxS("mathvariant")) == wxS("normal"));
      return ident;
    }
    if (text == wxS("lg"))
      return Ident(text, true);
    if (auto fn = MaximaFunctionName(text))
      return Ident(*fn, true);
    if ((text == wxS("lim")) || (text == wxS("sum")) || (text == wxS("prod")))
      return Ident(text, true);
    MathNode ident = Ident(text);
    ident.upright = true;
    return ident;
  }

  static MathNode ReadOperator(const wxString &text) {
    if (text.IsEmpty())
      return Row();
    if (text.Length() == 1)
      if (auto symbol = UnicodeSymbol(text[0]))
        return *symbol;
    if (text == wxS("lim") || text == wxS("sum") || text == wxS("prod"))
      return Ident(text, true);
    if (auto fn = MaximaFunctionName(text))
      return Ident(*fn, true);
    if (text == wxS("&lt;") || text == wxS("<"))
      return Op(wxS("<"));
    if (text == wxS("&gt;") || text == wxS(">"))
      return Op(wxS(">"));
    if (text == wxS("<=") || text == wxS(">=") || text == wxS(":="))
      return Op(text);
    return Op(text);
  }
};

// ---------------------------------------------------------------------------
// The interpretation: from layout to a Maxima expression
// ---------------------------------------------------------------------------

//! How tightly an expression binds, for deciding where parentheses go
enum Prec : int {
  P_DEF = 0,     //!< f(x) := ...
  P_REL = 1,     //!< = < > ...
  P_ADD = 2,     //!< + - and a leading minus
  P_MUL = 3,     //!< * /
  P_POW = 5,     //!< ^
  P_POSTFIX = 6, //!< !
  P_ATOM = 7     //!< names, numbers, function calls, anything in brackets
};

//! A piece of Maxima code and how tightly its outermost operator binds
struct Ex {
  wxString s;
  int prec = P_ATOM;
};

//! The expression's code, in parentheses if it binds less than minPrec
wxString Wrap(const Ex &e, int minPrec) {
  return (e.prec >= minPrec) ? e.s : (wxS("(") + e.s + wxS(")"));
}

Ex Call(const wxString &name, const std::vector<Ex> &args) {
  wxString s = name + wxS("(");
  for (std::size_t i = 0; i < args.size(); ++i) {
    if (i > 0)
      s += wxS(",");
    s += args[i].s;
  }
  return {s + wxS(")"), P_ATOM};
}

Ex Power(const Ex &base, const Ex &exponent) {
  return {Wrap(base, P_POSTFIX) + wxS("^") + Wrap(exponent, P_POW), P_POW};
}

bool IsLabel(const wxString &text) {
  // (%i1), (%o12), (%t3) as wxMaxima puts them in front of its output
  if (!text.StartsWith(wxS("(%")) || !text.EndsWith(wxS(")")) || text.Length() < 5)
    return false;
  for (std::size_t i = 3; i + 1 < text.Length(); ++i)
    if (!wxIsdigit(text[i]))
      return false;
  return true;
}

bool IsOpener(const wxString &op) {
  return op == wxS("(") || op == wxS("[") || op == wxS("{") || op == wxS("⟨");
}

bool IsCloser(const wxString &op) {
  return op == wxS(")") || op == wxS("]") || op == wxS("}") || op == wxS("⟩");
}

//! Row contents, with nested rows dissolved (braces and <mrow> are invisible)
void Flatten(const MathNode &node, std::vector<MathNode> &out) {
  if (node.kind == MathNode::Kind::Row) {
    for (const MathNode &c : node.children)
      Flatten(c, out);
    return;
  }
  if (node.kind == MathNode::Kind::Text) {
    wxString text = node.text;
    if (text.Trim(true).Trim(false).IsEmpty() || IsLabel(text))
      return;
  }
  out.push_back(node);
}

/*! Turns opening and closing operators into Fenced nodes

  LaTeX and MathML both mostly write parentheses as plain operators, so the
  pairs are matched here. A | opens unless a | is the innermost open
  delimiter, in which case it closes it.
*/
std::vector<MathNode> GroupFences(std::vector<MathNode> items) {
  struct Frame {
    wxString open;
    std::vector<MathNode> items;
  };
  std::vector<Frame> stack(1);

  auto closeFrame = [&stack](const wxString &close) -> MathNode {
    if (stack.size() < 2)
      Fail(wxString::Format(_("A \"%s\" has no matching opening bracket."), close));
    Frame frame = std::move(stack.back());
    stack.pop_back();
    return Fenced(frame.open, close, Row(std::move(frame.items)));
  };
  auto closes = [&stack](const wxString &op) {
    if (IsCloser(op))
      return true;
    return (op == wxS("|") || op == wxS("‖")) && (stack.size() > 1) &&
      (stack.back().open == op) && !stack.back().items.empty();
  };

  for (MathNode &item : items) {
    if (item.kind == MathNode::Kind::Op && closes(item.text)) {
      MathNode fenced = closeFrame(item.text);
      stack.back().items.push_back(std::move(fenced));
      continue;
    }
    // In LaTeX "(x)^2" the 2 is attached to the closing parenthesis
    if (item.kind == MathNode::Kind::Scripts &&
        item.children[0].kind == MathNode::Kind::Op &&
        closes(item.children[0].text)) {
      item.children[0] = closeFrame(item.children[0].text);
      stack.back().items.push_back(std::move(item));
      continue;
    }
    if (item.kind == MathNode::Kind::Op &&
        (IsOpener(item.text) || item.text == wxS("|") || item.text == wxS("‖"))) {
      stack.push_back(Frame{item.text, {}});
      continue;
    }
    stack.back().items.push_back(std::move(item));
  }
  if (stack.size() > 1)
    Fail(wxString::Format(_("A \"%s\" is never closed."), stack.back().open));
  return std::move(stack.front().items);
}

std::vector<MathNode> Prepare(const MathNode &row) {
  std::vector<MathNode> flat;
  Flatten(row, flat);
  return GroupFences(std::move(flat));
}

//! How an unexpected node is named in an error message
wxString Describe(const MathNode &node) {
  switch (node.kind) {
  case MathNode::Kind::Op:
  case MathNode::Kind::Ident:
  case MathNode::Kind::Number:
  case MathNode::Kind::Text:
    return node.text;
  default:
    return _("an expression");
  }
}

//! A valid Maxima name for an identifier
wxString MaximaName(const MathNode &ident) {
  if (ident.literal)
    return ident.text;
  // e is Euler's number, written upright or not; i only if written upright,
  // as it is far more often an index than the imaginary unit.
  if (ident.text == wxS("e"))
    return wxS("%e");
  if (ident.text == wxS("i") && ident.upright)
    return wxS("%i");
  for (const auto ch : ident.text)
    if (!wxIsalnum(ch) && ch != wxS('_'))
      Fail(wxString::Format(_("\"%s\" can't be used as a Maxima name."), ident.text));
  if (wxIsdigit(ident.text[0]))
    Fail(wxString::Format(_("\"%s\" can't be used as a Maxima name."), ident.text));
  return ident.text;
}

bool IsDifferentialD(const MathNode &node) {
  return node.kind == MathNode::Kind::Ident && !node.literal && !node.function &&
    (node.text == wxS("d") || node.text == wxS("ⅆ"));
}

bool IsPartial(const MathNode &node) {
  return node.kind == MathNode::Kind::Ident && node.text == wxS("∂");
}

bool IsPlainVariable(const MathNode &node) {
  return node.kind == MathNode::Kind::Ident && !node.literal && !node.function &&
    !IsPartial(node) && node.text != wxS("ⅆ");
}

class RowParser;

//! The meaning of a whole row, which has to be one expression
Ex ExprOf(const MathNode &row);
//! The comma-separated expressions in a row
std::vector<Ex> ListOf(const MathNode &row);

/*! Reads a meaning into a row of layout nodes

  A precedence-climbing parser: definitions, relations, sums, products
  (explicit or by juxtaposition), unary minus, postfix factorials, and the
  primaries, where function calls, derivatives, integrals and the like are
  recognised.
*/
class RowParser {
public:
  explicit RowParser(std::vector<MathNode> items) : m_items(std::move(items)) {}

  bool AtEnd() const { return m_pos >= m_items.size(); }

  void ExpectEnd() const {
    if (!AtEnd())
      Fail(wxString::Format(_("Couldn't make sense of \"%s\" here."),
                            Describe(m_items[m_pos])));
  }

  std::vector<Ex> ParseList() {
    std::vector<Ex> list;
    if (AtEnd())
      return list;
    list.push_back(ParseDefinition());
    while (PeekOp(wxS(","))) {
      ++m_pos;
      list.push_back(ParseDefinition());
    }
    return list;
  }

  Ex ParseDefinition() {
    Ex lhs = ParseRelation();
    if (PeekOp(wxS(":="))) {
      ++m_pos;
      Ex rhs = ParseRelation();
      return {Wrap(lhs, P_REL) + wxS(":=") + Wrap(rhs, P_REL), P_DEF};
    }
    return lhs;
  }

private:
  std::vector<MathNode> m_items;
  std::size_t m_pos = 0;
  //! Inside an integral: stop before a "d x"
  bool m_stopAtDifferential = false;

  const MathNode *Peek(std::size_t offset = 0) const {
    return (m_pos + offset < m_items.size()) ? &m_items[m_pos + offset] : nullptr;
  }
  bool PeekOp(const wxString &op) const { return Peek() && Peek()->IsOp(op); }

  Ex ParseRelation() {
    Ex lhs = ParseAdditive();
    static const wxString relations[] = {wxS("="), wxS("<"), wxS(">"),
                                         wxS("<="), wxS(">="), wxS("#")};
    for (;;) {
      bool found = false;
      for (const auto &rel : relations)
        if (PeekOp(rel)) {
          ++m_pos;
          const Ex rhs = ParseAdditive();
          lhs = {Wrap(lhs, P_ADD) + rel + Wrap(rhs, P_ADD), P_REL};
          found = true;
          break;
        }
      if (!found)
        return lhs;
    }
  }

  Ex ParseAdditive() {
    Ex lhs;
    if (PeekOp(wxS("-"))) {
      ++m_pos;
      lhs = {wxS("-") + Wrap(ParseTerm(), P_MUL), P_ADD};
    } else {
      if (PeekOp(wxS("+")))
        ++m_pos;
      lhs = ParseTerm();
    }
    for (;;) {
      if (PeekOp(wxS("±")))
        Fail(_("A plus-minus sign has no single Maxima meaning."));
      if (!PeekOp(wxS("+")) && !PeekOp(wxS("-")))
        return lhs;
      const wxString op = Peek()->text;
      ++m_pos;
      const Ex rhs = ParseTerm();
      lhs = {lhs.s + op + Wrap(rhs, P_MUL), P_ADD};
    }
  }

  //! Could this node begin a factor of an implicit product ("2x", "a b")?
  bool CanStartFactor(const MathNode *node) const {
    if (!node)
      return false;
    switch (node->kind) {
    case MathNode::Kind::Op:
    case MathNode::Kind::Text:
    case MathNode::Kind::Row:
      return false;
    default:
      break;
    }
    if (m_stopAtDifferential && AtDifferential())
      return false;
    return true;
  }

  //! Is a "d x" (or ⅆx, or MathML's <mi>dx</mi>) next?
  bool AtDifferential() const {
    const MathNode *node = Peek();
    if (!node || node->kind != MathNode::Kind::Ident)
      return false;
    if (IsDifferentialD(*node)) {
      const MathNode *var = Peek(1);
      return var && IsPlainVariable(*var);
    }
    return node->upright && !node->function && node->text.Length() == 2 &&
      node->text[0] == wxS('d') && wxIsalpha(node->text[1]);
  }

  //! Consumes a differential, returning its variable
  wxString TakeDifferential() {
    const MathNode *node = Peek();
    if (IsDifferentialD(*node)) {
      m_pos += 2;
      return MaximaName(m_items[m_pos - 1]);
    }
    ++m_pos;
    return node->text.Mid(1);
  }

  Ex ParseTerm() {
    Ex lhs = ParseFactor();
    for (;;) {
      if (PeekOp(wxS("*"))) {
        ++m_pos;
        lhs = {Wrap(lhs, P_MUL) + wxS("*") + Wrap(ParseFactor(), P_MUL), P_MUL};
      } else if (PeekOp(wxS("/"))) {
        ++m_pos;
        lhs = {Wrap(lhs, P_MUL) + wxS("/") + Wrap(ParseFactor(), P_POW), P_MUL};
      } else if (CanStartFactor(Peek())) {
        lhs = {Wrap(lhs, P_MUL) + wxS("*") + Wrap(ParseFactor(), P_MUL), P_MUL};
      } else
        return lhs;
    }
  }

  /*! The argument of a function written without parentheses: \sin 2x

    The factors that follow, up to an operator or the next function, so that
    \sin x \cos x is sin(x)*cos(x).
  */
  Ex ParseBareArgument(const wxString &function) {
    if (!CanStartFactor(Peek()))
      Fail(wxString::Format(_("The function %s lacks its argument."), function));
    Ex arg = ParseFactor();
    while (CanStartFactor(Peek()) && !StartsFunction(*Peek()))
      arg = {Wrap(arg, P_MUL) + wxS("*") + Wrap(ParseFactor(), P_MUL), P_MUL};
    return arg;
  }

  static bool StartsFunction(const MathNode &node) {
    if (node.kind == MathNode::Kind::Scripts)
      return StartsFunction(node.children[0]);
    return node.kind == MathNode::Kind::Ident && node.function;
  }

  Ex ParseFactor() {
    if (PeekOp(wxS("-"))) {
      ++m_pos;
      return {wxS("-") + Wrap(ParseFactor(), P_MUL), P_ADD};
    }
    if (PeekOp(wxS("+")))
      ++m_pos;
    Ex f = ParsePrimary();
    while (PeekOp(wxS("!"))) {
      ++m_pos;
      if (PeekOp(wxS("!"))) {
        ++m_pos;
        f = {Wrap(f, P_POSTFIX) + wxS("!!"), P_POSTFIX};
      } else
        f = {Wrap(f, P_POSTFIX) + wxS("!"), P_POSTFIX};
    }
    return f;
  }

  Ex ParsePrimary() {
    const MathNode *node = Peek();
    if (!node)
      Fail(_("The formula ends where an expression was expected."));
    switch (node->kind) {
    case MathNode::Kind::Number: {
      ++m_pos;
      int points = 0;
      for (const auto ch : node->text) {
        if (ch == wxS('.'))
          ++points;
        else if (!wxIsdigit(ch))
          Fail(wxString::Format(_("\"%s\" isn't a number Maxima understands."), node->text));
      }
      if (points > 1 || node->text.IsEmpty())
        Fail(wxString::Format(_("\"%s\" isn't a number Maxima understands."), node->text));
      return {node->text, P_ATOM};
    }
    case MathNode::Kind::Ident: {
      const MathNode &ident = *node;
      ++m_pos;
      return IdentifierUse(ident, nullptr);
    }
    case MathNode::Kind::Scripts: {
      const MathNode &scripts = *node;
      ++m_pos;
      std::vector<MathNode> base;
      Flatten(scripts.children[0], base);
      if (base.size() == 1 && base[0].kind == MathNode::Kind::Ident)
        return IdentifierUse(base[0], &scripts);
      if (scripts.hasSub)
        Fail(_("Only names can have a subscript."));
      if (IsPrimes(scripts.children[2]))
        Fail(_("A prime needs a function and its argument, as in f'(x)."));
      Ex b;
      if (base.size() == 1 && base[0].kind == MathNode::Kind::Fenced) {
        RowParser sub(std::move(base));
        b = sub.ParsePrimary();
      } else
        b = ExprOf(scripts.children[0]);
      return Power(b, Exponent(scripts.children[2]));
    }
    case MathNode::Kind::Fenced: {
      const MathNode fenced = *node;
      ++m_pos;
      return FencedExpr(fenced);
    }
    case MathNode::Kind::Frac: {
      const MathNode frac = *node;
      ++m_pos;
      return Fraction(frac);
    }
    case MathNode::Kind::Sqrt:
      ++m_pos;
      return Call(wxS("sqrt"), {ExprOf(node->children[0])});
    case MathNode::Kind::Root: {
      ++m_pos;
      const Ex index = ExprOf(node->children[1]);
      return Power(ExprOf(node->children[0]),
                   Ex{wxS("1/") + Wrap(index, P_POW), P_MUL});
    }
    case MathNode::Kind::Binom:
      ++m_pos;
      return Call(wxS("binomial"),
                  {ExprOf(node->children[0]), ExprOf(node->children[1])});
    case MathNode::Kind::Table: {
      const MathNode table = *node;
      ++m_pos;
      return Matrix(table);
    }
    case MathNode::Kind::Op:
      if (node->text == wxS("±"))
        Fail(_("A plus-minus sign has no single Maxima meaning."));
      if (node->text == wxS("…"))
        Fail(_("An ellipsis (...) has no single Maxima meaning."));
      if (node->text == wxS("->"))
        Fail(_("An arrow is only understood below a limit."));
      if (node->text == wxS("'"))
        Fail(_("A prime needs a function and its argument, as in f'(x)."));
      Fail(wxString::Format(_("Couldn't make sense of \"%s\" here."), node->text));
    case MathNode::Kind::Text:
      Fail(wxString::Format(_("Text (\"%s\") can't be converted to Maxima."), node->text));
    case MathNode::Kind::Row:
      break;
    }
    Fail(_("The formula ends where an expression was expected."));
  }

  static bool IsPrimes(const MathNode &row) {
    std::vector<MathNode> items;
    Flatten(row, items);
    if (items.empty())
      return false;
    for (const MathNode &i : items)
      if (!i.IsOp(wxS("'")))
        return false;
    return true;
  }

  static int CountPrimes(const MathNode &row) {
    std::vector<MathNode> items;
    Flatten(row, items);
    return static_cast<int>(items.size());
  }

  //! A superscript's meaning as an exponent
  static Ex Exponent(const MathNode &sup) {
    std::vector<MathNode> items;
    Flatten(sup, items);
    if (items.size() == 1 && items[0].kind == MathNode::Kind::Op)
      Fail(wxString::Format(_("A superscript \"%s\" has no single Maxima meaning."),
                            items[0].text));
    // A^T could be a transpose as well as a power: it is taken as written.
    return ExprOf(sup);
  }

  //! The next item, if it is a pair of parentheses holding arguments
  std::optional<std::vector<Ex>> TakeParenArguments(std::optional<MathNode> &supAfter) {
    const MathNode *next = Peek();
    if (!next)
      return std::nullopt;
    const MathNode *fenced = next;
    if (next->kind == MathNode::Kind::Scripts && !next->hasSub &&
        next->children[0].kind == MathNode::Kind::Fenced)
      fenced = &next->children[0];
    if (fenced->kind != MathNode::Kind::Fenced || fenced->text != wxS("(") ||
        fenced->close != wxS(")"))
      return std::nullopt;
    if (fenced != next)
      supAfter = next->children[2];
    auto args = ListOf(fenced->children[0]);
    ++m_pos;
    return args;
  }

  //! A name, maybe with scripts: a variable, a function, a big operator
  Ex IdentifierUse(const MathNode &ident, const MathNode *scripts) {
    // MathML's invisible "function application" makes any name a function
    bool function = ident.function;
    if (PeekOp(wxS("⁡"))) {
      ++m_pos;
      function = true;
    }
    if (function) {
      if (ident.text == wxS("sum") || ident.text == wxS("prod"))
        return BigOperator(ident.text, scripts);
      if (ident.text == wxS("int"))
        return Integral(scripts);
      if (ident.text == wxS("lim"))
        return Limit(scripts);
      return FunctionCall(ident, scripts);
    }
    if (IsPartial(ident))
      Fail(_("A ∂ is only understood in a derivative."));

    wxString name = MaximaName(ident);
    Ex result{name, P_ATOM};
    if (scripts && scripts->hasSub)
      result = Subscripted(name, scripts->children[1], ident.literal);

    // f'(x), f''(x), f^{\prime}(x)
    int primes = 0;
    if (scripts && scripts->hasSup && IsPrimes(scripts->children[2]))
      primes = CountPrimes(scripts->children[2]);
    while (PeekOp(wxS("'"))) {
      ++m_pos;
      ++primes;
    }
    if (primes > 0)
      return PrimedFunction(result, primes);

    if (CallsWithParentheses(ident)) {
      std::optional<MathNode> supAfter;
      if (auto args = TakeParenArguments(supAfter)) {
        result = Call(result.s, *args);
        if (supAfter)
          result = Power(result, Exponent(*supAfter));
      }
    }
    if (scripts && scripts->hasSup)
      result = Power(result, Exponent(scripts->children[2]));
    return result;
  }

  /*! Is "name(...)" a function call rather than a product?

    x(y+1) is a product, but f(x) and speed(t) are calls: single letters
    count as functions only if they are the usual function letters.
  */
  static bool CallsWithParentheses(const MathNode &ident) {
    if (ident.literal)
      return false;
    if (ident.text.Length() > 1)
      return true;
    static const wxString functionLetters = wxS("fghFGH");
    return functionLetters.Find(ident.text[0]) != wxNOT_FOUND;
  }

  static Ex Subscripted(const wxString &name, const MathNode &sub, bool literal) {
    if (literal)
      Fail(wxString::Format(_("%s can't have a subscript."), name));
    std::vector<MathNode> items;
    Flatten(sub, items);
    // x_1, a_n, x_{ij}: one name in Maxima, displayed with a subscript
    bool simple = !items.empty();
    wxString suffix;
    for (const MathNode &i : items) {
      if ((i.kind == MathNode::Kind::Number && !i.text.Contains(wxS("."))) ||
          (i.kind == MathNode::Kind::Ident && !i.literal && !i.function &&
           !IsPartial(i) && i.text.Length() == 1 && wxIsalnum(i.text[0])))
        suffix += i.text;
      else
        simple = false;
    }
    if (simple)
      return {name + wxS("_") + suffix, P_ATOM};
    // x_{n+1}: an array index
    auto indices = ListOf(sub);
    wxString s = name + wxS("[");
    for (std::size_t i = 0; i < indices.size(); ++i) {
      if (i > 0)
        s += wxS(",");
      s += indices[i].s;
    }
    return {s + wxS("]"), P_ATOM};
  }

  //! f'(x) = diff(f(x),x): the argument has to be a plain variable
  Ex PrimedFunction(const Ex &function, int primes) {
    std::optional<MathNode> supAfter;
    auto args = TakeParenArguments(supAfter);
    if (!args || args->size() != 1)
      Fail(_("A prime needs a function and its argument, as in f'(x)."));
    const wxString var = (*args)[0].s;
    for (const auto ch : var)
      if (!wxIsalnum(ch) && ch != wxS('_') && ch != wxS('%'))
        Fail(_("A prime needs a function and its argument, as in f'(x)."));
    std::vector<Ex> diffArgs{Call(function.s, *args), Ex{var, P_ATOM}};
    if (primes > 1)
      diffArgs.push_back(Ex{wxString::Format(wxS("%d"), primes), P_ATOM});
    Ex result = Call(wxS("diff"), diffArgs);
    if (supAfter)
      result = Power(result, Exponent(*supAfter));
    return result;
  }

  Ex FunctionCall(const MathNode &ident, const MathNode *scripts) {
    wxString name = ident.text;
    std::optional<Ex> logBase;
    if (name == wxS("lg")) {
      name = wxS("log");
      logBase = Ex{wxS("10"), P_ATOM};
    }
    std::optional<MathNode> sup;
    if (scripts && scripts->hasSub) {
      if (name != wxS("log"))
        Fail(wxString::Format(_("The function %s can't have a subscript."), name));
      logBase = ExprOf(scripts->children[1]);
    }
    if (scripts && scripts->hasSup) {
      std::vector<MathNode> items;
      Flatten(scripts->children[2], items);
      // sin^{-1} x is asin(x)
      if (items.size() == 2 && items[0].IsOp(wxS("-")) &&
          items[1].kind == MathNode::Kind::Number && items[1].text == wxS("1") &&
          HasInverseNotation(name))
        name = wxS("a") + name;
      else
        sup = scripts->children[2];
    }

    std::optional<MathNode> supAfter;
    std::vector<Ex> args;
    if (auto parenArgs = TakeParenArguments(supAfter))
      args = *parenArgs;
    else
      args.push_back(ParseBareArgument(name));
    if (args.empty())
      Fail(wxString::Format(_("The function %s lacks its argument."), name));

    Ex result = Call(name, args);
    if (logBase) {
      if (args.size() != 1)
        Fail(_("A logarithm takes one argument."));
      result = {result.s + wxS("/") + Call(wxS("log"), {*logBase}).s, P_MUL};
    }
    if (supAfter)
      result = Power(result, Exponent(*supAfter));
    if (sup)
      result = Power(result, Exponent(*sup));
    return result;
  }

  //! Splits a "var = lower" subscript of a sum, product or limit
  static std::pair<wxString, std::vector<MathNode>>
  VariableAndRest(const MathNode &sub, const wxString &op, const wxString &what) {
    std::vector<MathNode> items = Prepare(sub);
    if (items.size() < 3 || !IsPlainVariable(items[0]) || !items[1].IsOp(op))
      Fail(what);
    const wxString var = MaximaName(items[0]);
    items.erase(items.begin(), items.begin() + 2);
    return {var, std::move(items)};
  }

  //! sum_{i=1}^{n} or prod_{i=1}^{n}, and the term after it
  Ex BigOperator(const wxString &which, const MathNode *scripts) {
    const wxString maxima = (which == wxS("sum")) ? wxS("sum") : wxS("product");
    const wxString message = wxString::Format(
      _("A %s needs its index and both limits, as in %s_{i=1}^{n}."), maxima,
      (which == wxS("sum")) ? wxS("\\sum") : wxS("\\prod"));
    if (!scripts || !scripts->hasSub || !scripts->hasSup)
      Fail(message);
    auto [var, lower] = VariableAndRest(scripts->children[1], wxS("="), message);
    RowParser lowerParser(std::move(lower));
    const Ex lo = lowerParser.ParseRelation();
    lowerParser.ExpectEnd();
    const Ex hi = ExprOf(scripts->children[2]);
    if (AtEnd())
      Fail(wxString::Format(_("The %s has nothing to sum up."), maxima));
    const Ex body = ParseTerm();
    return Call(maxima, {body, Ex{std::move(var), P_ATOM}, lo, hi});
  }

  //! int_a^b f dx
  Ex Integral(const MathNode *scripts) {
    std::optional<std::pair<Ex, Ex>> limits;
    if (scripts) {
      if (scripts->hasSub != scripts->hasSup)
        Fail(_("An integral needs either both limits or none."));
      if (scripts->hasSub)
        limits = std::make_pair(ExprOf(scripts->children[1]),
                                ExprOf(scripts->children[2]));
    }
    const bool stopWas = m_stopAtDifferential;
    m_stopAtDifferential = true;
    Ex integrand{wxS("1"), P_ATOM};
    if (!AtDifferential())
      integrand = ParseAdditive();
    m_stopAtDifferential = stopWas;
    if (!AtDifferential())
      Fail(_("Couldn't find the integration variable (the \"dx\") of an integral."));
    wxString var = TakeDifferential();
    std::vector<Ex> args;
    args.reserve(4);
    args.push_back(std::move(integrand));
    args.push_back(Ex{std::move(var), P_ATOM});
    if (limits) {
      args.push_back(std::move(limits->first));
      args.push_back(std::move(limits->second));
    }
    return Call(wxS("integrate"), args);
  }

  //! lim_{x -> a} f, also with a^+ or a^- for a one-sided limit
  Ex Limit(const MathNode *scripts) {
    const wxString message =
      _("A limit needs its variable and value, as in \\lim_{x\\to 0}.");
    if (!scripts || !scripts->hasSub || scripts->hasSup)
      Fail(message);
    auto [var, point] = VariableAndRest(scripts->children[1], wxS("->"), message);
    wxString direction;
    MathNode &last = point.back();
    if (last.kind == MathNode::Kind::Scripts && last.hasSup && !last.hasSub) {
      std::vector<MathNode> sup;
      Flatten(last.children[2], sup);
      if (sup.size() == 1 && (sup[0].IsOp(wxS("+")) || sup[0].IsOp(wxS("-")))) {
        direction = sup[0].IsOp(wxS("+")) ? wxS("plus") : wxS("minus");
        MathNode base = last.children[0];
        last = std::move(base);
      }
    }
    RowParser pointParser(std::move(point));
    Ex at = pointParser.ParseRelation();
    pointParser.ExpectEnd();
    if (AtEnd())
      Fail(_("The limit has no expression to take the limit of."));
    std::vector<Ex> args;
    args.reserve(4);
    args.push_back(ParseTerm());
    args.push_back(Ex{std::move(var), P_ATOM});
    args.push_back(std::move(at));
    if (!direction.IsEmpty())
      args.push_back(Ex{std::move(direction), P_ATOM});
    return Call(wxS("limit"), args);
  }

  /*! The order of a "d" or "∂", possibly with an exponent (d^2)

    Returns nothing if the node isn't one.
  */
  static std::optional<wxString> DifferentialOrder(const MathNode &node) {
    if (IsDifferentialD(node) || IsPartial(node))
      return wxString(wxS("1"));
    if (node.kind == MathNode::Kind::Scripts && node.hasSup && !node.hasSub &&
        (IsDifferentialD(node.children[0]) || IsPartial(node.children[0])))
      return ExprOf(node.children[2]).s;
    return std::nullopt;
  }

  /*! The variables a derivative's denominator differentiates by

    dx dy, dx^2, ∂x ∂y, ... Returns nothing if the denominator isn't one.
  */
  static std::optional<std::vector<std::pair<wxString, wxString>>>
  DerivativeVariables(const std::vector<MathNode> &den) {
    std::vector<std::pair<wxString, wxString>> vars;
    std::size_t i = 0;
    while (i < den.size()) {
      const MathNode &d = den[i];
      // MathML often has <mi>dx</mi>
      if (d.kind == MathNode::Kind::Ident && d.upright && d.text.Length() == 2 &&
          d.text[0] == wxS('d')) {
        vars.emplace_back(d.text.Mid(1), wxS("1"));
        ++i;
        continue;
      }
      if (!(IsDifferentialD(d) || IsPartial(d)) || i + 1 >= den.size())
        return std::nullopt;
      const MathNode &v = den[i + 1];
      if (IsPlainVariable(v))
        vars.emplace_back(MaximaName(v), wxS("1"));
      else if (v.kind == MathNode::Kind::Scripts && v.hasSup && !v.hasSub &&
               IsPlainVariable(v.children[0]))
        vars.emplace_back(MaximaName(v.children[0]), ExprOf(v.children[2]).s);
      else
        return std::nullopt;
      i += 2;
    }
    if (vars.empty())
      return std::nullopt;
    return vars;
  }

  //! \frac{a}{b}, or a derivative written as one
  Ex Fraction(const MathNode &frac) {
    std::vector<MathNode> num = Prepare(frac.children[0]);
    const std::vector<MathNode> den = Prepare(frac.children[1]);
    if (!num.empty()) {
      if (DifferentialOrder(num[0])) {
        if (auto vars = DerivativeVariables(den)) {
          num.erase(num.begin());
          Ex body;
          if (num.empty()) {
            // d/dx applied to what follows
            if (!CanStartFactor(Peek()))
              Fail(_("A derivative d/dx has nothing to differentiate."));
            body = ParseTerm();
          } else {
            RowParser bodyParser(std::move(num));
            body = bodyParser.ParseRelation();
            bodyParser.ExpectEnd();
          }
          std::vector<Ex> args;
          args.push_back(std::move(body));
          const bool single = (vars->size() == 1);
          for (const auto &[var, order] : *vars) {
            args.push_back(Ex{var, P_ATOM});
            if (!single || order != wxS("1"))
              args.push_back(Ex{order, P_ATOM});
          }
          return Call(wxS("diff"), args);
        }
      }
    }
    const Ex n = ExprOf(frac.children[0]);
    const Ex d = ExprOf(frac.children[1]);
    return {Wrap(n, P_MUL) + wxS("/") + Wrap(d, P_POW), P_MUL};
  }

  static Ex Matrix(const MathNode &table) {
    std::vector<Ex> rows;
    for (const MathNode &row : table.children) {
      wxString s = wxS("[");
      for (std::size_t i = 0; i < row.children.size(); ++i) {
        if (i > 0)
          s += wxS(",");
        s += ExprOf(row.children[i]).s;
      }
      rows.push_back(Ex{s + wxS("]"), P_ATOM});
    }
    return Call(wxS("matrix"), rows);
  }

  Ex FencedExpr(const MathNode &fenced) {
    const wxString &open = fenced.text;
    const wxString &close = fenced.close;
    std::vector<MathNode> contents = Prepare(fenced.children[0]);
    const bool table = (contents.size() == 1) &&
      (contents[0].kind == MathNode::Kind::Table);

    if ((open == wxS("(") && close == wxS(")")) ||
        (open.IsEmpty() && close.IsEmpty())) {
      if (table)
        return Matrix(contents[0]);
      auto list = ListOf(fenced.children[0]);
      if (list.size() != 1)
        Fail(_("A list in parentheses, like (a, b), has no single Maxima meaning."));
      // The parentheses come back wherever they are needed
      return list[0];
    }
    if (open == wxS("[") && close == wxS("]")) {
      if (table)
        return Matrix(contents[0]);
      auto list = ListOf(fenced.children[0]);
      // [a+b] is a bracket; [a, b] a list
      if (list.size() == 1)
        return list[0];
      wxString s = wxS("[");
      for (std::size_t i = 0; i < list.size(); ++i)
        s += ((i > 0) ? wxS(",") : wxS("")) + list[i].s;
      return {s + wxS("]"), P_ATOM};
    }
    if (open == wxS("{") && close == wxS("}")) {
      auto list = ListOf(fenced.children[0]);
      wxString s = wxS("{");
      for (std::size_t i = 0; i < list.size(); ++i)
        s += ((i > 0) ? wxS(",") : wxS("")) + list[i].s;
      return {s + wxS("}"), P_ATOM};
    }
    if (open == wxS("|") && close == wxS("|")) {
      if (table)
        return Call(wxS("determinant"), {Matrix(contents[0])});
      return Call(wxS("abs"), {ExprOf(fenced.children[0])});
    }
    Fail(wxString::Format(_("The brackets \"%s ... %s\" have no single Maxima meaning."),
                          open, close));
  }
};

Ex ExprOf(const MathNode &row) {
  RowParser parser(Prepare(row));
  if (parser.AtEnd())
    Fail(_("Something in the formula is empty."));
  Ex e = parser.ParseDefinition();
  parser.ExpectEnd();
  return e;
}

std::vector<Ex> ListOf(const MathNode &row) {
  RowParser parser(Prepare(row));
  auto list = parser.ParseList();
  parser.ExpectEnd();
  return list;
}

//! The whole formula: one expression, a list, or the lines of an equation array
wxString Interpret(const MathNode &root) {
  std::vector<MathNode> items = Prepare(root);
  // A period or comma at the end belongs to the surrounding sentence
  while (!items.empty() && (items.back().IsOp(wxS(".")) || items.back().IsOp(wxS(",")) ||
                            items.back().IsOp(wxS(";"))))
    items.pop_back();
  if (items.empty())
    Fail(_("There is no formula to convert."));

  // An equation array, or wxMaxima's own output with its labels in a column
  // of their own: one statement per line.
  if (items.size() == 1 && items[0].kind == MathNode::Kind::Table) {
    const MathNode &table = items[0];
    // Every line holds at most one formula once labels are gone, and some
    // line had a label next to it: wxMaxima's own output. A bare matrix
    // environment has no labels, and stays a matrix.
    bool oneFormulaPerLine = true;
    bool hadLabels = false;
    std::vector<const MathNode *> statements;
    for (const MathNode &row : table.children) {
      const MathNode *statement = nullptr;
      for (const MathNode &cell : row.children) {
        if (Prepare(cell).empty()) {
          hadLabels = true;
          continue;
        }
        if (statement)
          oneFormulaPerLine = false;
        statement = &cell;
      }
      if (statement)
        statements.push_back(statement);
    }
    if (!statements.empty() && (table.aligned || (oneFormulaPerLine && hadLabels))) {
      wxString result;
      for (const MathNode *statement : statements) {
        if (!result.IsEmpty())
          result += wxS(";\n");
        result += ExprOf(*statement).s;
      }
      return result;
    }
  }

  RowParser parser(std::move(items));
  auto list = parser.ParseList();
  parser.ExpectEnd();
  if (list.size() == 1)
    return list[0].s;
  wxString s = wxS("[");
  for (std::size_t i = 0; i < list.size(); ++i)
    s += ((i > 0) ? wxS(",") : wxS("")) + list[i].s;
  return s + wxS("]");
}

} // namespace

MathImportResult LaTeXToMaxima(const wxString &latex) {
  MathImportResult result;
  try {
    LaTeXParser parser(LexLaTeX(StripMathDelimiters(latex)));
    result.maxima = Interpret(parser.ParseAll());
  } catch (const ImportError &e) {
    result.maxima.Clear();
    result.error = e.message;
  }
  return result;
}

bool LooksLikeMathML(const wxString &text) {
  wxString t = text;
  t.Trim(false);
  if (t.StartsWith(wxS("<?xml"))) {
    const int end = t.Find(wxS("?>"));
    if (end == wxNOT_FOUND)
      return false;
    t = t.Mid(static_cast<std::size_t>(end) + 2);
    t.Trim(false);
  }
  return t.StartsWith(wxS("<math")) || t.StartsWith(wxS("<m:math")) ||
    t.StartsWith(wxS("<mml:math"));
}

MathImportResult MathMLToMaxima(const wxString &mathml) {
  MathImportResult result;
  try {
    wxStringInputStream stream(ReplaceNamedEntities(mathml));
    wxXmlDocument doc;
    {
      // A broken document is reported below, not in a popup
      wxLogNull noPopups;
      if (!doc.Load(stream) || !doc.GetRoot())
        Fail(_("The MathML can't be read: it isn't well-formed XML."));
    }
    const wxXmlNode *root = doc.GetRoot();
    if (LocalName(root) != wxS("math"))
      Fail(_("The MathML can't be read: it has no <math> element."));
    if (auto annotation = MaximaAnnotation(root)) {
      if (annotation->IsEmpty())
        Fail(_("There is no formula to convert."));
      result.maxima = *annotation;
      return result;
    }
    result.maxima = Interpret(MathMLReader::Read(root));
  } catch (const ImportError &e) {
    result.maxima.Clear();
    result.error = e.message;
  }
  return result;
}

MathImportResult MathToMaxima(const wxString &text) {
  if (LooksLikeMathML(text))
    return MathMLToMaxima(text);
  return LaTeXToMaxima(text);
}

} // namespace wxm
