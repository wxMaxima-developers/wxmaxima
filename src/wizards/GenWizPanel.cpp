// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2017-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
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

#include "GenWizPanel.h"
#include "WizardHelp.h"
#include "WizardHelpKeywords.h"
#include "WxMaximaManualAnchors.h"
#include <wx/dcclient.h>
#include <algorithm>
#include <wx/msgdlg.h>
#include <wx/notebook.h>

GenWizPanel::GenWizPanel(
                         wxWindow *parent, Configuration *cfg, MaximaManual *manual,
                         const wxString &description, const wxString &description_tooltip,
                         const wxString &commandRule, const wxString &label1, const wxString &defaultval1,
                         const wxString &tooltip1, const wxString &label2, const wxString &defaultval2, const wxString &tooltip2,
                         const wxString &label3, const wxString &defaultval3, const wxString &tooltip3, const wxString &label4,
                         const wxString &defaultval4, const wxString &tooltip4, const wxString &label5,
                         const wxString &defaultval5, const wxString &tooltip5, const wxString &label6,
                         const wxString &defaultval6, const wxString &tooltip6, const wxString &label7,
                         const wxString &defaultval7, const wxString &tooltip7, const wxString &label8,
                         const wxString &defaultval8, const wxString &tooltip8, const wxString &label9,
                         const wxString &defaultval9, const wxString &tooltip9, const wxString &label10,
                         const wxString &defaultval10, const wxString &tooltip10)
: GenWizPanel(parent, cfg, manual,
              description, description_tooltip,
              commandRule, false, label1,
              defaultval1, tooltip1, label2,
              defaultval2, tooltip2, label3,
              defaultval3, tooltip3, label4,
              defaultval4, tooltip4, label5,
              defaultval5, tooltip5, label6,
              defaultval6, tooltip6, label7,
              defaultval7, tooltip7, label8,
              defaultval8, tooltip8, label9,
              defaultval9, tooltip9, label10,
              defaultval10, tooltip10) {}

GenWizPanel::GenWizPanel(
                         wxWindow *parent, Configuration *cfg, MaximaManual *manual,
                         const wxString &description, const wxString &description_tooltip,
                         const wxString &commandRule, bool dockable, const wxString &label1,
                         const wxString &defaultval1, const wxString &tooltip1, const wxString &label2,
                         const wxString &defaultval2, const wxString &tooltip2, const wxString &label3,
                         const wxString &defaultval3, const wxString &tooltip3, const wxString &label4,
                         const wxString &defaultval4, const wxString &tooltip4, const wxString &label5,
                         const wxString &defaultval5, const wxString &tooltip5, const wxString &label6,
                         const wxString &defaultval6, const wxString &tooltip6, const wxString &label7,
                         const wxString &defaultval7, const wxString &tooltip7, const wxString &label8,
                         const wxString &defaultval8, const wxString &tooltip8, const wxString &label9,
                         const wxString &defaultval9, const wxString &tooltip9, const wxString &label10,
                         const wxString &defaultval10, const wxString &tooltip10)
: wxPanel(parent, wxID_ANY), m_commandRule(commandRule),
  m_description(description), m_descriptionToolTip(description_tooltip),
  m_configuration(cfg), m_maximaManual(manual) {
  wxBoxSizer *vbox = new wxBoxSizer(wxVERTICAL);
  for (int i = 0; i < 10; i++) {
    m_label.push_back(new wxStaticText(this, -1, wxEmptyString));
    m_textctrl.push_back(new BTextCtrl(
                                       this, -1, cfg, wxEmptyString, wxDefaultPosition,
                                       wxSize(50 * GetContentScaleFactor(), -1), wxTE_PROCESS_ENTER));
  }

  m_textctrl.at(0)->SetFocus();
  wxFlexGridSizer *grid_sizer = new wxFlexGridSizer(
                                                    2, wxSize(5 * GetContentScaleFactor(), 5 * GetContentScaleFactor()));
  grid_sizer->AddGrowableCol(1);
  for (int i = 0; i < 10; i++) {
    grid_sizer->Add(m_label.at(i), 0, wxALIGN_CENTER_VERTICAL, 0);
    m_textctrl.at(i)->Bind(wxEVT_TEXT, &GenWizPanel::OnParamChange, this);
    m_textctrl.at(i)->Bind(wxEVT_TEXT_ENTER, &GenWizPanel::OnTextEnter, this);
    grid_sizer->Add(m_textctrl.at(i), wxSizerFlags(0).Expand());
  }

  vbox->Add(grid_sizer, wxSizerFlags(0).Expand().Border(
                                                        wxALL, 5 * GetContentScaleFactor()));

  //  if(m_warning != nullptr)
  //    grid_sizer->Add(m_warning, 0, wxALL, 5);
  m_notebook = new wxNotebook(this, wxID_ANY);
  // Not stretched: the tabs are as high as their text needs (see
  // FitTextHeight()), and whatever room is left goes below the buttons
  // rather than into an empty tab that pushes the buttons out of sight.
  vbox->Add(
            m_notebook,
            wxSizerFlags(0).Border(wxALL, 5 * GetContentScaleFactor()).Expand());

  wxBoxSizer *buttonSizer = new wxBoxSizer(wxHORIZONTAL);
  m_insertButton = new wxButton(this, wxID_ANY, _("Insert"));

#if defined __WXMSW__
  button_1 = new wxButton(this, wxID_OK, _("OK"));
  if (dockable)
    m_insertButton->SetDefault();
  else
    button_1->SetDefault();
  button_2 = new wxButton(this, wxID_CANCEL, _("Cancel"));
#else
  button_1 = new wxButton(this, wxID_CANCEL, _("Cancel"));
  button_2 = new wxButton(this, wxID_OK, _("OK"));
  if (dockable)
    m_insertButton->SetDefault();
  else
    button_2->SetDefault();
#endif
  buttonSizer->Add(button_1,
                   wxSizerFlags(0).Border(wxALL, 5 * GetContentScaleFactor()));
  m_insertButton->Show(dockable);
  buttonSizer->Add(m_insertButton,
                   wxSizerFlags(0).Border(wxALL, 5 * GetContentScaleFactor()));
  m_insertButton->Show(dockable);
  buttonSizer->Add(button_2,
                   wxSizerFlags(0).Border(wxALL, 5 * GetContentScaleFactor()));

  vbox->Add(buttonSizer,
            wxSizerFlags(0).Border(wxALL, 5 * GetContentScaleFactor()));
  NewWizard(description, description_tooltip, commandRule, label1, defaultval1,
            tooltip1, label2, defaultval2, tooltip2, label3, defaultval3,
            tooltip3, label4, defaultval4, tooltip4, label5, defaultval5,
            tooltip5, label6, defaultval6, tooltip6, label7, defaultval7,
            tooltip7, label8, defaultval8, tooltip8, label9, defaultval9,
            tooltip9, label10, defaultval10, tooltip10);
  Bind(wxEVT_SIZE, &GenWizPanel::OnSize, this);
  m_notebook->Bind(wxEVT_NOTEBOOK_PAGE_CHANGED, &GenWizPanel::OnNotebookPageChange, this);
  SetSizer(vbox);
  FitInside();
}

void GenWizPanel::OnSize(wxSizeEvent &event) { event.Skip(); }

GenWizPanel::~GenWizPanel() {}

void GenWizPanel::NewWizard(
                            const wxString& description, const wxString &description_tooltip,
                            const wxString &commandRule, const wxString &label1, const wxString &defaultval1,
                            const wxString &tooltip1, const wxString &label2, const wxString &defaultval2, const wxString &tooltip2,
                            const wxString &label3, const wxString &defaultval3, const wxString &tooltip3, const wxString &label4,
                            const wxString &defaultval4, const wxString &tooltip4, const wxString &label5,
                            const wxString &defaultval5, const wxString &tooltip5, const wxString &label6,
                            const wxString &defaultval6, const wxString &tooltip6, const wxString &label7,
                            const wxString &defaultval7, const wxString &tooltip7, const wxString &label8,
                            const wxString &defaultval8, const wxString &tooltip8, const wxString &label9,
                            const wxString &defaultval9, const wxString &tooltip9, const wxString &label10,
                            const wxString &defaultval10, const wxString &tooltip10) {
  // Only offer help on what the manual actually documents: a button that
  // opens the help browser on nothing is worse than none.
  m_manualKeywords.clear();
  for (const auto &word : wxm::WizardHelpKeywords(commandRule))
    if ((!m_maximaManual->GetHelpfileAnchorName(word).IsEmpty()) ||
        (!WxMaximaManualAnchors::AnchorFor(word).IsEmpty()))
      m_manualKeywords.push_back(word);
  m_ignorePageChange = true;
  long page = m_configuration->WizardTab();
  if (page < 0)
    page = 0;
  if (page > 1)
    page = 1;

  m_commandRule = commandRule;
  if ((!commandRule.IsEmpty()) || (!description.IsEmpty()) ||
      (!m_manualKeywords.empty())) {
    m_notebook->DeleteAllPages();
    m_output = nullptr;
    m_notebook->Show();
  } else {
    m_notebook->Hide();
  }

  if (!commandRule.IsEmpty()) {
    wxSizer *pageSizer = new wxBoxSizer(wxVERTICAL);
    wxPanel *outputpane = new wxPanel(m_notebook, wxID_ANY);
    m_output = new wxTextCtrl(
                              outputpane, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize,
                              wxTE_READONLY | wxTE_MULTILINE | wxTE_CHARWRAP | wxTE_NO_VSCROLL);
    m_output->Bind(wxEVT_SIZE, [this](wxSizeEvent &event) {
      FitTextHeight(m_output, false);
      event.Skip();
    });
    pageSizer->Add(
                   m_output,
                   wxSizerFlags(1).Border(wxALL, 0 * GetContentScaleFactor()).Expand());
    outputpane->SetSizer(pageSizer);
    outputpane->FitInside();
    m_notebook->AddPage(outputpane, _("Command"));
  }

  UpdateOutput();
  m_description = description;
  if ((!description.IsEmpty()) || (!m_manualKeywords.empty())) {
    wxSizer *pageSizer = new wxBoxSizer(wxVERTICAL);
    wxPanel *descriptionpane = new wxPanel(m_notebook, wxID_ANY);
    if (!description.IsEmpty()) {
      wxTextCtrl *desc = new wxTextCtrl(descriptionpane, wxID_ANY, description,
                                        wxDefaultPosition, wxDefaultSize,
                                        wxTE_READONLY | wxTE_MULTILINE |
                                        wxTE_BESTWRAP | wxTE_NO_VSCROLL);
      desc->Bind(wxEVT_SIZE, [this, desc](wxSizeEvent &event) {
        FitTextHeight(desc, true);
        event.Skip();
      });
      FitTextHeight(desc, true);
      pageSizer->Add(
                     desc,
                     wxSizerFlags(1).Border(wxALL, 0 * GetContentScaleFactor()).Expand());
    }

    m_topicButtonIDs.clear();
    m_topicButtonID0 = wxWindow::NewControlId(m_manualKeywords.size());
    int id = m_topicButtonID0;
    for (const auto &i : m_manualKeywords) {
      wxButton *button = new wxButton(
                                      descriptionpane, id, wxString::Format(_("Help on %s"), i));
      pageSizer->Add(button, wxSizerFlags(0).Expand());
      m_topicButtonIDs.push_back(id);
      id++;
    }
    descriptionpane->SetSizer(pageSizer);
    descriptionpane->FitInside();
    m_notebook->AddPage(descriptionpane, _("Description"));
  }
  if ((!commandRule.IsEmpty()) && (!description.IsEmpty()))
    m_notebook->ChangeSelection(page);

  m_descriptionToolTip = description_tooltip;
  m_textctrl.at(0)->SetValue(defaultval1);
  m_label.at(0)->SetLabel(label1);
  m_textctrl.at(0)->SetToolTip(tooltip1);
  m_textctrl.at(0)->SetFocus();

  m_textctrl.at(1)->SetValue(defaultval2);
  m_label.at(1)->SetLabel(label2);
  m_label.at(1)->Show(!label2.IsEmpty());
  m_textctrl.at(1)->Show(!label2.IsEmpty());
  m_textctrl.at(1)->SetToolTip(tooltip2);

  m_textctrl[2]->SetValue(defaultval3);
  m_label[2]->SetLabel(label3);
  m_label[2]->Show(!label3.IsEmpty());
  m_textctrl[2]->Show(!label3.IsEmpty());
  m_textctrl[2]->SetToolTip(tooltip3);

  m_textctrl[3]->SetValue(defaultval4);
  m_label[3]->SetLabel(label4);
  m_label[3]->Show(!label4.IsEmpty());
  m_textctrl[3]->Show(!label4.IsEmpty());
  m_textctrl[3]->SetToolTip(tooltip4);

  m_label[4]->SetLabel(label5);
  m_textctrl[4]->SetValue(defaultval5);
  m_label[4]->Show(!label5.IsEmpty());
  m_textctrl[4]->Show(!label5.IsEmpty());
  m_textctrl[4]->SetToolTip(tooltip5);

  m_label[5]->SetLabel(label6);
  m_textctrl[5]->SetValue(defaultval6);
  m_label[5]->Show(!label6.IsEmpty());
  m_textctrl[5]->Show(!label6.IsEmpty());
  m_textctrl[5]->SetToolTip(tooltip6);

  m_label[6]->SetLabel(label7);
  m_textctrl[6]->SetValue(defaultval7);
  m_label[6]->Show(!label7.IsEmpty());
  m_textctrl[6]->Show(!label7.IsEmpty());
  m_textctrl[6]->SetToolTip(tooltip7);

  m_label[7]->SetLabel(label8);
  m_textctrl[7]->SetValue(defaultval8);
  m_label[7]->Show(!label8.IsEmpty());
  m_textctrl[7]->Show(!label8.IsEmpty());
  m_textctrl[7]->SetToolTip(tooltip8);

  m_label[8]->SetLabel(label9);
  m_textctrl[8]->SetValue(defaultval9);
  m_label[8]->Show(!label9.IsEmpty());
  m_textctrl[8]->Show(!label9.IsEmpty());
  m_textctrl[8]->SetToolTip(tooltip9);

  m_label[9]->SetLabel(label10);
  m_textctrl[9]->SetValue(defaultval10);
  m_label[9]->Show(!label10.IsEmpty());
  m_textctrl[9]->Show(!label10.IsEmpty());
  m_textctrl[9]->SetToolTip(tooltip10);

  FitNotebookHeight();
  Layout();
  m_ignorePageChange = false;
}

void GenWizPanel::FitNotebookHeight() {
  // wxGTK underestimates the height of the row of tabs above the pages by
  // about a line of text (measured: it assumes 22 pixels where GTK draws
  // 37), which cut off the bottom of the highest page, i.e. the last "Help
  // on" button. Ask for that line on top.
  m_notebook->SetMinSize(wxDefaultSize);
  m_notebook->InvalidateBestSize();
  m_notebook->SetMinSize(
    wxSize(-1, m_notebook->GetBestSize().GetHeight() + m_notebook->GetCharHeight()));
}

wxString GenWizPanel::GetHelpKeyword(wxWindowID ID) {
  if (ID < m_topicButtonID0)
    return wxEmptyString;
  ID -= m_topicButtonID0;
  if (static_cast<unsigned>(ID) >= m_manualKeywords.size())
    return wxEmptyString;
  return m_manualKeywords.at(ID);
}

void GenWizPanel::OnTextEnter(wxCommandEvent &event) {
  if (m_insertButton) {
    wxCommandEvent *bttnprss = new wxCommandEvent(wxEVT_BUTTON, wxID_OK);
    m_insertButton->GetEventHandler()->QueueEvent(bttnprss);
  }
  event.Skip();
}

void GenWizPanel::UpdateOutput() {
  if (m_commandRule.IsEmpty())
    return;

  wxString output(m_commandRule);
  for (unsigned int i = 0; i < m_textctrl.size(); i++)
    output.Replace(wxString::Format("#%i#", i + 1), m_textctrl.at(i)->GetValue());
  if (m_output) {
    m_output->SetValue(output);
    FitTextHeight(m_output, false);
  }
}

namespace {
/*! How many lines \p text needs when wrapped to \p width pixels

  Mirrors what a wrapping text control does: \p wordWrap breaks between
  words where it can (a word too long for a line of its own is broken
  anywhere), otherwise the text is broken after whichever character reaches
  the edge. A newline always starts a new line.
*/
int WrappedLineCount(const wxWindow *measurer, const wxString &text, int width,
                     bool wordWrap) {
  wxClientDC dc(const_cast<wxWindow *>(measurer));
  dc.SetFont(measurer->GetFont());
  auto textWidth = [&dc](const wxString &str) {
    return dc.GetTextExtent(str).GetWidth();
  };

  int lines = 0;
  wxString paragraph;
  wxString rest = text;
  do {
    paragraph = rest.BeforeFirst('\n', &rest);
    lines++;
    int x = 0;
    // The pieces a line may be broken between: words with the blanks that
    // follow them, or single characters.
    wxString piece;
    for (auto it = paragraph.begin(); it != paragraph.end();) {
      piece.Clear();
      if (wordWrap) {
        while ((it != paragraph.end()) && (*it != ' '))
          piece += *it++;
        while ((it != paragraph.end()) && (*it == ' '))
          piece += *it++;
      } else
        piece = *it++;
      // Trailing blanks may hang over the edge: only the word has to fit.
      wxString visible = piece;
      visible.Trim();
      const int visibleWidth = textWidth(visible);
      if ((x > 0) && (x + visibleWidth > width)) {
        lines++;
        x = 0;
      }
      if (visibleWidth > width) {
        // Too long for any line: broken after whichever character reaches
        // the edge.
        for (const auto &ch : piece) {
          const int chWidth = textWidth(wxString(ch));
          if ((x > 0) && (x + chWidth > width)) {
            lines++;
            x = 0;
          }
          x += chWidth;
        }
      } else
        x += textWidth(piece);
    }
  } while (!rest.IsEmpty());
  return lines;
}
} // namespace

void GenWizPanel::FitTextHeight(wxTextCtrl *ctrl, bool wordWrap) {
  if (ctrl == nullptr)
    return;
  // Before the control has been laid out it has no width of its own yet;
  // assume the width the rest of the panel will give it.
  int width = ctrl->GetClientSize().GetWidth();
  if (width < 10 * ctrl->GetCharWidth())
    width = GetBestSize().GetWidth();
  // Leaves room for the text control's own margins, whose size the toolkit
  // doesn't tell. Erring on the narrow side costs at worst an empty line,
  // erring on the wide side cuts off the last one.
  width -= 2 * ctrl->GetCharWidth();
  if (width < ctrl->GetCharWidth())
    width = ctrl->GetCharWidth();

  const int lines = WrappedLineCount(ctrl, ctrl->GetValue(), width, wordWrap);
  const int frame = ctrl->GetSize().GetHeight() - ctrl->GetClientSize().GetHeight();
  const int height = (lines + 1) * ctrl->GetCharHeight() + std::max(frame, 0);
  if (ctrl->GetMinSize().GetHeight() == height)
    return;
  ctrl->SetMinSize(wxSize(-1, height));
  // SetMinSize() alone leaves the best sizes the notebook page, the notebook
  // and this panel have cached unchanged.
  ctrl->InvalidateBestSize();
  // The new height has to reach the notebook, this panel and whatever this
  // panel is docked in -- but not from inside the size event that brought
  // us here, which is still in the middle of laying all of them out.
  if (!m_relayoutPending) {
    m_relayoutPending = true;
    CallAfter([this] {
      m_relayoutPending = false;
      // A page whose text changed height has to rearrange its contents
      // even if the notebook as a whole keeps its size.
      for (size_t i = 0; i < m_notebook->GetPageCount(); i++)
        m_notebook->GetPage(i)->Layout();
      FitNotebookHeight();
      Layout();
      if (m_onBestSizeChange)
        m_onBestSizeChange();
    });
  }
}

void GenWizPanel::OnNotebookPageChange(wxBookCtrlEvent &event) {
  event.Skip();
  if (m_ignorePageChange)
    return;
  m_configuration->WizardTab(event.GetSelection());
}

void GenWizPanel::OnParamChange(wxCommandEvent &WXUNUSED(event)) {
  UpdateOutput();
}
