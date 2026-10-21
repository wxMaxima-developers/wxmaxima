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
  Defines the sample worksheet the configuration dialog's "Worksheet" tab
  shows to preview what its display settings do.
*/

#include "WorksheetPreview.h"
#include "MathParser.h"
#include "cells/CellList.h"
#include "cells/GroupCell.h"
#include <vector>

namespace wxm {

wxString WorksheetPreview::MatrixXml() {
  wxString xml = wxS("<tb roundedParens=\"true\">");
  for (int row = 1; row <= MatrixSize; row++) {
    xml += wxS("<mtr>");
    for (int col = 1; col <= MatrixSize; col++)
      xml += wxString::Format(wxS("<mtd><mn>%i</mn></mtd>"), 100 * row + col);
    xml += wxS("</mtr>");
  }
  return xml + wxS("</tb>");
}

wxString WorksheetPreview::ExpandedBinomialXml() {
  // Maxima orders the terms by falling power: x^n+n*x^(n-1)+...+n*x+1.
  wxString xml;
  long coefficient = 1;
  for (int power = WideFractionPower; power >= 0; power--) {
    if (power < WideFractionPower)
      xml += wxS("<mo>+</mo>");
    if ((coefficient != 1) || (power == 0)) {
      xml += wxString::Format(wxS("<n>%li</n>"), coefficient);
      if (power > 0)
        xml += wxS("<h>*</h>");
    }
    if (power == 1)
      xml += wxS("<v>x</v>");
    else if (power > 1)
      xml += wxString::Format(wxS("<e><v>x</v><n>%i</n></e>"), power);
    // The next binomial coefficient, n over (n-power+1)
    coefficient = coefficient * power / (WideFractionPower - power + 1);
  }
  return xml;
}

wxString WorksheetPreview::Factorial200() {
  // Schoolbook multiplication on decimal digits, least significant first:
  // a few hundred digits don't warrant anything cleverer.
  std::vector<int> digits{1};
  for (int factor = 2; factor <= 200; factor++) {
    int carry = 0;
    for (int &digit : digits) {
      const int product = digit * factor + carry;
      digit = product % 10;
      carry = product / 10;
    }
    for (; carry > 0; carry /= 10)
      digits.push_back(carry % 10);
  }
  wxString result;
  for (auto digit = digits.rbegin(); digit != digits.rend(); ++digit)
    result += static_cast<wxChar>(wxS('0') + *digit);
  return result;
}

WorksheetPreview WorksheetPreview::Make(Configuration *config) {
  WorksheetPreview preview;
  CellListBuilder<GroupCell> tree;
  // Each output is parsed by a parser of its own: a parser remembers the
  // user-defined label it was last given. And it has to know the group:
  // cells made later from the parsed ones (a long number broken into lines)
  // take their group from them.
  auto addCode = [config, &tree](const wxString &input, const wxString &output) {
    auto group = std::make_unique<GroupCell>(config, GC_TYPE_CODE, input);
    MathParser parser(config);
    parser.SetGroup(group.get());
    group->AppendOutput(parser.ParseLine(wxS("<mth>") + output + wxS("</mth>")));
    return tree.Append(group.release());
  };
  // Not translated: the sample worksheet in the Style tab isn't either.
  auto addHeading = [config, &tree, &preview](Topic topic, const wxString &text) {
    preview.headings[static_cast<size_t>(topic)] =
      tree.Append(new GroupCell(config, GC_TYPE_TEXT, text));
  };

  addHeading(Topic::labels, wxS("Labels and multiplication signs:"));
  // A user-defined label, as Maxima sends it for "name: value": with an
  // automatic label as well, which "Show labels" chooses between.
  addCode(wxS("kinetic_energy: m*v^2/2;"),
          wxS("<lbl altCopy=\"%o1\" userdefinedlabel=\"kinetic_energy\">(%o1) </lbl>"
              "<f><r><v>m</v><h>*</h><e><v>v</v><n>2</n></e></r><n>2</n></f>"));
  addCode(wxS("2*a*b;"),
          wxS("<lbl altCopy=\"%o2\">(%o2) </lbl>"
              "<n>2</n><h>*</h><v>a</v><h>*</h><v>b</v>"));

  addHeading(Topic::specialSymbols, wxS("Special symbols and Greek letters:"));
  addCode(wxS("alpha+%e^(%i*%pi*x);"),
          wxS("<lbl altCopy=\"%o3\">(%o3) </lbl>"
              "<g>alpha</g><mo>+</mo>"
              "<e><s>%e</s><r><s>%i</s><h>*</h><s>%pi</s><h>*</h><v>x</v></r></e>"));

  addHeading(Topic::longNumbers, wxS("A long number:"));
  addCode(wxS("200!;"),
          wxS("<lbl altCopy=\"%o4\">(%o4) </lbl><n>") + Factorial200() + wxS("</n>"));

  addHeading(Topic::layoutStrategy, wxS("A narrow and a wide fraction:"));
  addCode(wxS("sqrt(x)/(x^2+1);"),
          wxS("<lbl altCopy=\"%o5\">(%o5) </lbl>"
              "<f><r><q><v>x</v></q></r>"
              "<r><e><v>x</v><n>2</n></e><mo>+</mo><n>1</n></r></f>"));
  addCode(wxString::Format(wxS("expand((x+1)^%i)/(x-1);"), WideFractionPower),
          wxS("<lbl altCopy=\"%o6\">(%o6) </lbl><f><r>") + ExpandedBinomialXml() +
          wxS("</r><r><v>x</v><mo>-</mo><n>1</n></r></f>"));

  addHeading(Topic::oversizedMatrix, wxS("A matrix too large for the window:"));
  preview.matrixGroup =
    addCode(wxString::Format(wxS("genmatrix(lambda([i,j],100*i+j),%i,%i);"),
                             MatrixSize, MatrixSize),
            wxS("<lbl altCopy=\"%o7\">(%o7) </lbl>") + MatrixXml());

  preview.tree = tree.TakeHead();
  return preview;
}

} // namespace wxm
