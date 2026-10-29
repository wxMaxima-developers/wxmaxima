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
  Implements TroubleshootingDialog.
*/

#include "TroubleshootingDialog.h"
#include "Bidi.h"
#include "BuildConfig.h"
#include "Dirstructure.h"
#include "HelpBrowserDiagnosis.h"
#include "SvgBitmap.h"
#include "wxMaximaArtProvider.h"
#ifdef WXM_USE_AI_TOOLS
#include "ai/AiProvider.h"
#endif
#include <wx/artprov.h>
#include <wx/button.h>
#include <wx/clipbrd.h>
#include <wx/dataobj.h>
#include <wx/filefn.h>
#include <wx/filename.h>
#include <wx/intl.h>
#if defined __WXOSX__
#include <wx/choicebk.h>
#else
#include <wx/listbook.h>
#endif
#include <wx/mimetype.h>
#include <wx/settings.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/uilocale.h>
#include <algorithm>

using wxm::TroubleshootingCheck;
using wxm::TroubleshootingResult;
using wxm::TroubleshootingStatus;

namespace {
TroubleshootingResult Result(TroubleshootingCheck check, TroubleshootingStatus status,
                             const wxString &details, const wxString &hint = wxEmptyString) {
  TroubleshootingResult result;
  result.check = check;
  result.status = status;
  result.details = details;
  result.hint = hint;
  return result;
}

//! The file a file:// URL points to, without its #anchor; empty for any other URL
wxString FileOfUrl(const wxString &url) {
  if (!url.StartsWith(wxS("file:")))
    return wxEmptyString;
  wxString file = wxFileName::URLToFileName(url).GetFullPath();
  const int anchor = file.Find(wxS('#'));
  if (anchor != wxNOT_FOUND)
    file = file.Left(anchor);
  return file;
}

TroubleshootingResult CheckExternalBrowser() {
  const TroubleshootingCheck check = TroubleshootingCheck::externalBrowser;
#ifdef __WXOSX__
  return Result(check, TroubleshootingStatus::ok, _("macOS opens it in the default browser."));
#else
#if defined(__UNIX__)
  // wxLaunchDefaultBrowser() tries xdg-open first.
  wxPathList path;
  path.AddEnvList(wxS("PATH"));
  const wxString xdgOpen = path.FindAbsoluteValidPath(wxS("xdg-open"));
  if (!xdgOpen.IsEmpty())
    return Result(check, TroubleshootingStatus::ok, xdgOpen);
#endif
  std::unique_ptr<wxFileType> type(wxTheMimeTypesManager->GetFileTypeFromExtension(wxS("html")));
  wxString command;
  if (type && type->GetOpenCommand(&command, wxFileType::MessageParameters(wxS("x.html"))) &&
      !command.IsEmpty())
    return Result(check, TroubleshootingStatus::ok, command);
  return Result(check, TroubleshootingStatus::warning,
                _("No program for opening HTML files was found."),
                _("Help that is shown in a browser, and links in the worksheet, may "
                  "not open. Make a web browser the default program for HTML files."));
#endif
}

TroubleshootingResult CheckTranslations(long language) {
  const TroubleshootingCheck check = TroubleshootingCheck::translations;
  wxTranslations *translations = wxTranslations::Get();
  if (!translations)
    return Result(check, TroubleshootingStatus::failed, _("No translations were set up."));
  wxString wanted;
  if (language != wxLANGUAGE_DEFAULT)
    wanted = wxLocale::GetLanguageCanonicalName(static_cast<int>(language));
  if (wanted.IsEmpty())
    wanted = wxUILocale::GetCurrent().GetName();
  if (translations->IsLoaded(wxS("wxMaxima")))
    return Result(check, TroubleshootingStatus::ok,
                  wxString::Format(_("Using the translation for %s."),
                                   translations->GetBestTranslation(wxS("wxMaxima"))));
  if (wanted.IsEmpty() || wanted.StartsWith(wxS("en")) || wanted.StartsWith(wxS("C")) ||
      wanted.StartsWith(wxS("POSIX")))
    return Result(check, TroubleshootingStatus::info,
                  wxString::Format(_("The language is %s, which needs no translation."),
                                   wanted.IsEmpty() ? wxString(wxS("English")) : wanted));
  wxString details = wxString::Format(_("No translation for %s was found in %s."),
                                      wanted, Dirstructure::LocaleDir());
  const wxArrayString available = translations->GetAvailableTranslations(wxS("wxMaxima"));
  if (!available.IsEmpty())
    details += wxS("\n") + _("Translations found:") + wxS(" ") +
      wxJoin(available, wxS(','), 0);
  return Result(check, TroubleshootingStatus::warning, details,
                _("The wxMaxima.mo files may be missing from the installation, or "
                  "there may be no translation for this language yet. "
                  "Edit > Configure > Worksheet chooses the language."));
}

//! The colour a status' symbol is drawn in; the text colour for those without one
wxColour StatusColour(TroubleshootingStatus status) {
  // Mid-tones that are readable both on a light and on a dark background
  switch (status) {
  case TroubleshootingStatus::ok:
    return wxColour(0x2e, 0xa0, 0x43);
  case TroubleshootingStatus::failed:
    return wxColour(0xe0, 0x30, 0x30);
  case TroubleshootingStatus::warning:
    return wxColour(0xe0, 0x90, 0x00);
  case TroubleshootingStatus::info:
    return wxColour(0x30, 0x80, 0xe0);
  default:
    return wxSystemSettings::GetColour(wxSYS_COLOUR_GRAYTEXT);
  }
}

/*! Draws a status as its coloured symbol followed by its name

  wxDataViewListCtrl's own text renderer can't colour part of a cell, at
  least not on every platform: markup is only understood by some of them.
  The value is the status, as a long.
*/
class StatusRenderer : public wxDataViewCustomRenderer
{
public:
  StatusRenderer()
    : wxDataViewCustomRenderer(wxS("long"), wxDATAVIEW_CELL_INERT, wxALIGN_LEFT) {}
  bool SetValue(const wxVariant &value) override {
    m_status = static_cast<TroubleshootingStatus>(value.GetLong());
    return true;
  }
  bool GetValue(wxVariant &value) const override {
    value = static_cast<long>(m_status);
    return true;
  }
#if wxUSE_ACCESSIBILITY
  wxString GetAccessibleDescription() const override {
    return wxm::TroubleshootingStatusName(m_status);
  }
#endif
  wxSize GetSize() const override {
    const wxSize symbol = GetTextExtent(wxm::TroubleshootingStatusSymbol(m_status) + wxS(" "));
    const wxSize name = GetTextExtent(wxm::TroubleshootingStatusName(m_status));
    return wxSize(symbol.x + name.x, std::max(symbol.y, name.y));
  }
  bool Render(wxRect rect, wxDC *dc, int state) override {
    const wxString symbol = wxm::TroubleshootingStatusSymbol(m_status) + wxS(" ");
    const wxSize extent = dc->GetTextExtent(symbol);
    const wxColour oldColour = dc->GetTextForeground();
    dc->SetTextForeground(StatusColour(m_status));
    dc->DrawText(symbol, rect.x, rect.y + (rect.height - extent.y) / 2);
    dc->SetTextForeground(oldColour);
    RenderText(wxm::TroubleshootingStatusName(m_status), extent.x, rect, dc, state);
    return true;
  }

private:
  TroubleshootingStatus m_status = TroubleshootingStatus::pending;
};
} // namespace

std::vector<TroubleshootingResult> TroubleshootingDialog::LocalChecks(const Inputs &inputs) {
  std::vector<TroubleshootingResult> results;

  if (wxFileExists(inputs.wxMaximaManual))
    results.push_back(Result(TroubleshootingCheck::wxMaximaHtmlManual,
                             TroubleshootingStatus::ok, inputs.wxMaximaManual));
  else
    results.push_back(Result(TroubleshootingCheck::wxMaximaHtmlManual,
                             TroubleshootingStatus::warning,
                             wxString::Format(_("%s doesn't exist."), inputs.wxMaximaManual),
                             _("wxMaxima's manual may not have been installed with it.")));

  {
    const TroubleshootingCheck check = TroubleshootingCheck::helpKnowsManual;
    const wxString url = inputs.helpUrlForIntegrate;
    const wxString file = FileOfUrl(url);
    if (url.IsEmpty())
      results.push_back(Result(check, TroubleshootingStatus::warning,
                               _("The help knows no entry for integrate in Maxima's manual."),
                               _("wxMaxima reads Maxima's manual once Maxima has started; "
                                 "if Maxima never started or its manual is missing, F1 "
                                 "shows Maxima's manual from the internet instead.")));
    else if (!file.IsEmpty() && !wxFileExists(file))
      results.push_back(Result(check, TroubleshootingStatus::warning,
                               wxString::Format(_("The help would show %s, which doesn't exist."),
                                                url),
                               _("Maxima's manual may have been moved or removed since "
                                 "wxMaxima last read it.")));
    else
      results.push_back(Result(check, TroubleshootingStatus::ok, url));
  }

  // The diagnosis lists what the integrated browser needs and what was found.
  if (inputs.offerHelpSidebar)
    results.push_back(Result(TroubleshootingCheck::helpSidebar, TroubleshootingStatus::ok,
                             wxm::HelpBrowserDiagnosis()));
  else
    results.push_back(Result(TroubleshootingCheck::helpSidebar, TroubleshootingStatus::warning,
                             wxm::HelpBrowserDiagnosis(),
                             _("Help is then shown in a web browser instead.")));

  results.push_back(CheckExternalBrowser());
  results.push_back(CheckTranslations(inputs.language));
  // Decided at build time; not having it is no fault, so it is only a note.
  results.push_back(Result(TroubleshootingCheck::bidiText,
                           Bidi::IsAvailable() ? TroubleshootingStatus::ok
                                               : TroubleshootingStatus::info,
                           Bidi::Diagnosis()));

  const bool rendersText = SvgBitmap::RendersText();
  results.push_back(Result(TroubleshootingCheck::svgText,
                           rendersText ? TroubleshootingStatus::ok : TroubleshootingStatus::info,
                           SvgBitmap::TextRenderingDiagnosis(rendersText)));

#ifdef WXM_USE_AI_TOOLS
  if (AiProvider::SecretStoreAvailable())
    results.push_back(Result(TroubleshootingCheck::aiSidebar, TroubleshootingStatus::ok,
                             _("The system's secret store, which keeps the API keys, "
                               "is reachable.")));
  else
    results.push_back(Result(TroubleshootingCheck::aiSidebar, TroubleshootingStatus::warning,
                             _("The system's secret store, which would keep the API keys, "
                               "isn't reachable."),
                             _("wxMaxima never writes API keys into its configuration "
                               "file, so without a secret store it offers no AI sidebar. "
                               "On Linux that store is a keyring such as GNOME Keyring "
                               "or KWallet, which has to be running and unlocked.")));
#else
  results.push_back(Result(TroubleshootingCheck::aiSidebar, TroubleshootingStatus::info,
                           _("This wxMaxima was built without the AI tools (they need "
                             "wxWidgets 3.3.4 or newer).")));
#endif
  return results;
}

TroubleshootingDialog::TroubleshootingDialog(wxWindow *parent, const Inputs &inputs)
  : wxDialog(parent, wxID_ANY, _("Troubleshooting"), wxDefaultPosition, wxDefaultSize,
             wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER),
    m_inputs(inputs)
{
  wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
  wxStaticText *intro =
    new wxStaticText(this, wxID_ANY,
                     _("wxMaxima checks the things that most often keep it from working. "
                       "For that it starts a Maxima of its own: the worksheet's Maxima "
                       "isn't touched. Select a line to see what was found."));
  intro->Wrap(FromDIP(680));
  sizer->Add(intro, wxSizerFlags().Expand().Border(wxALL, FromDIP(8)));

  // The same kind of book the configuration dialog uses
#if defined __WXOSX__
  m_book = new wxChoicebook(this, wxID_ANY);
#else
  m_book = new wxListbook(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxLB_LEFT);
  const wxSize iconSize = FromDIP(wxSize(32, 32));
  wxVector<wxBitmapBundle> images;
  images.push_back(wxArtProvider::GetBitmapBundle(wxmaximaART_CONFIG_MAXIMA, wxART_OTHER, iconSize));
  images.push_back(wxArtProvider::GetBitmapBundle(wxmaximaART_CONFIG_DOCUMENT_EXPORT, wxART_OTHER,
                                                  iconSize));
  images.push_back(wxArtProvider::GetBitmapBundle(wxART_HELP, wxART_OTHER, iconSize));
  images.push_back(wxArtProvider::GetBitmapBundle(wxmaximaART_CONFIG_STYLES, wxART_OTHER, iconSize));
  m_book->SetImages(images);
#endif

  m_rows.resize(static_cast<size_t>(wxm::TroubleshootingCheck::count));
  for (const auto section : wxm::AllTroubleshootingSections()) {
    wxPanel *page = new wxPanel(m_book);
    wxBoxSizer *pageSizer = new wxBoxSizer(wxVERTICAL);
    wxDataViewListCtrl *list =
      new wxDataViewListCtrl(page, wxID_ANY, wxDefaultPosition, FromDIP(wxSize(560, 330)),
                             wxDV_SINGLE | wxDV_ROW_LINES);
    list->AppendColumn(new wxDataViewColumn(_("Result"), new StatusRenderer(), 0, FromDIP(130),
                                            wxALIGN_LEFT),
                       wxS("long"));
    list->AppendTextColumn(_("Check"), wxDATAVIEW_CELL_INERT, FromDIP(420));
    pageSizer->Add(list, wxSizerFlags(1).Expand());
    page->SetSizer(pageSizer);
    m_lists.push_back(list);
    list->Bind(wxEVT_DATAVIEW_SELECTION_CHANGED, &TroubleshootingDialog::OnSelectionChanged, this);
#if defined __WXOSX__
    m_book->AddPage(page, wxm::TroubleshootingSectionTitle(section));
#else
    m_book->AddPage(page, wxm::TroubleshootingSectionTitle(section), false,
                    static_cast<int>(section));
#endif
  }
  sizer->Add(m_book, wxSizerFlags(3).Expand().Border(wxLEFT | wxRIGHT, FromDIP(8)));

  m_details = new wxTextCtrl(this, wxID_ANY, wxEmptyString, wxDefaultPosition,
                             FromDIP(wxSize(680, 150)),
                             wxTE_MULTILINE | wxTE_READONLY | wxTE_WORDWRAP);
  sizer->Add(m_details, wxSizerFlags(2).Expand().Border(wxALL, FromDIP(8)));

  wxBoxSizer *buttons = new wxBoxSizer(wxHORIZONTAL);
  m_runAgain = new wxButton(this, wxID_ANY, _("&Run again"));
  wxButton *copy = new wxButton(this, wxID_ANY, _("&Copy report"));
  wxButton *close = new wxButton(this, wxID_CLOSE);
  buttons->Add(m_runAgain, wxSizerFlags().Border(wxRIGHT, FromDIP(8)));
  buttons->Add(copy);
  buttons->AddStretchSpacer();
  buttons->Add(close);
  sizer->Add(buttons, wxSizerFlags().Expand().Border(wxLEFT | wxRIGHT | wxBOTTOM, FromDIP(8)));
  SetSizerAndFit(sizer);
  SetEscapeId(wxID_CLOSE);

  for (const auto check : wxm::AllTroubleshootingChecks()) {
    TroubleshootingResult result;
    result.check = check;
    m_results.push_back(result);
    wxDataViewListCtrl *list = m_lists.at(static_cast<size_t>(wxm::TroubleshootingSectionOf(check)));
    m_rows.at(static_cast<size_t>(check)) = list->GetItemCount();
    wxVector<wxVariant> row;
    row.push_back(wxVariant(static_cast<long>(result.status)));
    row.push_back(wxVariant(wxm::TroubleshootingCheckTitle(check)));
    list->AppendItem(row);
  }

  // wxEVT_BOOKCTRL_PAGE_CHANGED is the notebook's event: each book has its own.
#if defined __WXOSX__
  m_book->Bind(wxEVT_CHOICEBOOK_PAGE_CHANGED, &TroubleshootingDialog::OnPageChanged, this);
#else
  m_book->Bind(wxEVT_LISTBOOK_PAGE_CHANGED, &TroubleshootingDialog::OnPageChanged, this);
#endif
  m_runAgain->Bind(wxEVT_BUTTON, &TroubleshootingDialog::OnRunAgain, this);
  copy->Bind(wxEVT_BUTTON, &TroubleshootingDialog::OnCopyReport, this);
  close->Bind(wxEVT_BUTTON, [this](wxCommandEvent &) { EndModal(wxID_CLOSE); });

  RunChecks();
}

void TroubleshootingDialog::RunChecks() {
  m_runner.reset();
  for (auto &result : m_results) {
    const bool byRunner = TroubleshootingRunner::Checks().count(result.check) > 0;
    result.status = byRunner ? TroubleshootingStatus::running : TroubleshootingStatus::pending;
    result.details.Clear();
    result.hint.Clear();
    ShowResult(result);
  }
  for (const auto &result : LocalChecks(m_inputs))
    ShowResult(result);

  m_runAgain->Disable();
  m_runner = std::make_unique<TroubleshootingRunner>(
    m_inputs.runner,
    [this](const TroubleshootingResult &result) { ShowResult(result); },
    [this]() { CallAfter([this] { OnFinished(); }); });
  m_runner->Start();
  if (SelectedCheck() == wxm::TroubleshootingCheck::count)
    SelectCheck(wxm::AllTroubleshootingChecks().front());
  ShowDetails();
}

void TroubleshootingDialog::ShowResult(const TroubleshootingResult &result) {
  const auto index = static_cast<size_t>(result.check);
  m_results.at(index) = result;
  const wxm::TroubleshootingSection section = wxm::TroubleshootingSectionOf(result.check);
  m_lists.at(static_cast<size_t>(section))
    ->SetValue(wxVariant(static_cast<long>(result.status)), m_rows.at(index), 0);
  UpdatePageText(section);
  if (result.check == SelectedCheck())
    ShowDetails();
}

void TroubleshootingDialog::UpdatePageText(wxm::TroubleshootingSection section) {
  std::vector<TroubleshootingStatus> statuses;
  for (const auto &result : m_results)
    if (wxm::TroubleshootingSectionOf(result.check) == section)
      statuses.push_back(result.status);
  const wxString text =
    wxm::TroubleshootingStatusSymbol(wxm::TroubleshootingSummaryStatus(statuses)) + wxS(" ") +
    wxm::TroubleshootingSectionTitle(section);
  const auto page = static_cast<size_t>(section);
  if (m_book->GetPageText(page) != text)
    m_book->SetPageText(page, text);
}

wxm::TroubleshootingCheck TroubleshootingDialog::SelectedCheck() const {
  const int page = m_book->GetSelection();
  if (page == wxNOT_FOUND)
    return wxm::TroubleshootingCheck::count;
  const int row = m_lists.at(static_cast<size_t>(page))->GetSelectedRow();
  if (row == wxNOT_FOUND)
    return wxm::TroubleshootingCheck::count;
  for (const auto check : wxm::AllTroubleshootingChecks())
    if ((static_cast<int>(wxm::TroubleshootingSectionOf(check)) == page) &&
        (m_rows.at(static_cast<size_t>(check)) == static_cast<unsigned int>(row)))
      return check;
  return wxm::TroubleshootingCheck::count;
}

void TroubleshootingDialog::SelectCheck(wxm::TroubleshootingCheck check) {
  const auto page = static_cast<size_t>(wxm::TroubleshootingSectionOf(check));
  const unsigned int row = m_rows.at(static_cast<size_t>(check));
  m_book->ChangeSelection(page);
  wxDataViewListCtrl *list = m_lists.at(page);
  list->SelectRow(row);
  list->EnsureVisible(list->RowToItem(static_cast<int>(row)));
  ShowDetails();
}

void TroubleshootingDialog::ShowDetails() {
  const wxm::TroubleshootingCheck check = SelectedCheck();
  if (check == wxm::TroubleshootingCheck::count) {
    m_details->Clear();
    return;
  }
  const TroubleshootingResult &result = m_results.at(static_cast<size_t>(check));
  wxString text = wxm::TroubleshootingStatusSymbol(result.status) + wxS(" ") +
    wxm::TroubleshootingStatusName(result.status) + wxS(": ") +
    wxm::TroubleshootingCheckTitle(result.check);
  if (!result.details.IsEmpty())
    text += wxS("\n\n") + result.details;
  if (!result.hint.IsEmpty())
    text += wxS("\n\n") + result.hint;
  m_details->ChangeValue(text);
}

void TroubleshootingDialog::OnSelectionChanged(wxDataViewEvent &WXUNUSED(event)) {
  ShowDetails();
}

void TroubleshootingDialog::OnPageChanged(wxBookCtrlEvent &event) {
  // A page shows the details of its selected row, or its first row's.
  const int page = event.GetSelection();
  if ((page != wxNOT_FOUND) && (m_lists.at(static_cast<size_t>(page))->GetSelectedRow() == wxNOT_FOUND))
    m_lists.at(static_cast<size_t>(page))->SelectRow(0);
  ShowDetails();
}

void TroubleshootingDialog::OnRunAgain(wxCommandEvent &WXUNUSED(event)) {
  RunChecks();
}

void TroubleshootingDialog::OnCopyReport(wxCommandEvent &WXUNUSED(event)) {
  wxClipboardLocker lock;
  if (!lock)
    return;
  wxTheClipboard->SetData(new wxTextDataObject(wxm::TroubleshootingReport(m_results)));
}

void TroubleshootingDialog::OnFinished() {
  m_runAgain->Enable();
  // Point at the first problem, unless the user has looked elsewhere already.
  if (SelectedCheck() != wxm::AllTroubleshootingChecks().front())
    return;
  for (const auto &result : m_results)
    if ((result.status == TroubleshootingStatus::failed) ||
        (result.status == TroubleshootingStatus::warning)) {
      SelectCheck(result.check);
      return;
    }
}
