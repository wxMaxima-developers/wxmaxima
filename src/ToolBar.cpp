// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode:
// nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2008-2009 Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
//            (C) 2012-2013 Doug Ilijev <doug.ilijev@gmail.com>
//            (C) 2015-2019 Gunter Königsmann <wxMaxima@physikbuch.de>
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
  This file defines the class ToolBar that represents wxMaxima's main tool bar.
*/

#include "ToolBar.h"
#include "Image.h"
#include "SvgBitmap.h"
#include "ArtProvider.h"
#if wxUSE_ACCESSIBILITY
#include <wx/access.h>
#endif

#include "wxMaximaArtProvider.h"
#include <cstdlib>
#include <wx/artprov.h>
#include <wx/display.h>
#include <wx/filename.h>
#include <wx/mstream.h>
#include <wx/txtstrm.h>
#include <wx/wfstream.h>
#include <wx/zstream.h>
#include <algorithm>
#include <optional>

#define TOOLBAR_ICON_SCALE (0.25)

wxSize ToolBar::GetOptimalBitmapSize()
{
  wxSize siz;
  wxDisplay display;
  int display_idx = wxDisplay::GetFromWindow(GetParent());
  if (display_idx < 0)
    m_ppi = wxSize(72, 72);
  else
    m_ppi = wxDisplay(display_idx).GetPPI();
  if ((m_ppi.x <= 10) || (m_ppi.y <= 10))
    m_ppi = wxSize(72, 72);

#if defined __WXOSX__
  int targetSize =
    std::max(m_ppi.x, 75) * TOOLBAR_ICON_SCALE * GetContentScaleFactor();
#else
  int targetSize = std::max(m_ppi.x, 75) * TOOLBAR_ICON_SCALE;
#endif
  int sizeA = 128 << 4;
  while (sizeA * 3 / 2 > targetSize && sizeA >= 32) {
    sizeA >>= 1;
  };

  int sizeB = 192 << 4;
  while (sizeB * 4 / 3 > targetSize && sizeB >= 32) {
    sizeB >>= 1;
  }

  if (std::abs(targetSize - sizeA) < std::abs(targetSize - sizeB))
    targetSize = sizeA;
  else
    targetSize = sizeB;
  siz = wxSize(targetSize, targetSize);
  return siz;
}

ToolBar::~ToolBar() { m_plotSlider = nullptr; }

void ToolBar::UpdateSlider(AnimationCell *cell) {
  if (cell == nullptr)
    return;
  std::size_t animationDisplayedIndex = cell->GetDisplayedIndex();
  std::size_t animationMaxIndex = cell->Length();

  if ((m_animationDisplayedIndex != animationDisplayedIndex) ||
      (m_animationMaxIndex != animationMaxIndex)) {
    m_animationMaxIndex = animationMaxIndex;
    m_animationDisplayedIndex = animationDisplayedIndex;
    if (m_plotSlider != nullptr) {
      m_plotSlider->SetRange(0, cell->Length() - 1);
      m_plotSlider->SetValue(cell->GetDisplayedIndex());
      m_plotSlider->SetToolTip(wxString::Format(
                                                _("Frame %li of %li"),
                                                static_cast<long>(cell->GetDisplayedIndex()) + 1,
                                                static_cast<long>(cell->Length())));
    }
  }
}

#if wxUSE_ACCESSIBILITY
// wxAuiToolBar is owner-drawn, so its tool buttons are not real windows and are
// invisible to screen readers (inspect.exe sees only the toolbar's real child
// controls -- the animation slider and the text field). This accessible exposes
// every tool as an MSAA "simple element" with a name, role, state and screen
// location, while delegating the real control items to their own accessibles so
// those are not lost when we take over the child enumeration.
class ToolBarAccessible : public wxAccessible {
public:
  explicit ToolBarAccessible(ToolBar *toolBar)
    : wxAccessible(toolBar), m_toolBar(toolBar) {}

  wxAccStatus GetChildCount(int *childCount) override {
    if (!childCount)
      return wxACC_FAIL;
    *childCount = static_cast<int>(m_toolBar->GetToolCount());
    return wxACC_OK;
  }

  wxAccStatus GetChild(int childId, wxAccessible **child) override {
    if (!child)
      return wxACC_FAIL;
    *child = nullptr; // nullptr + wxACC_OK => a "simple element" answered by us
    if (childId == 0)
      return wxACC_OK;
    wxAuiToolBarItem *item = ItemFor(childId);
    // A control item (slider / text field) is a real window: hand screen readers
    // its own accessible so its value and role survive.
    if (item && item->GetWindow())
      *child = item->GetWindow()->GetOrCreateAccessible();
    return wxACC_OK;
  }

  wxAccStatus GetName(int childId, wxString *name) override {
    if (!name)
      return wxACC_FAIL;
    if (childId == 0) {
      *name = _("Toolbar");
      return wxACC_OK;
    }
    wxAuiToolBarItem *item = ItemFor(childId);
    if (!item)
      return wxACC_FAIL;
    // The short help (tooltip) is the most speech-friendly label; fall back to
    // the tool's own label text.
    *name = item->GetShortHelp().IsEmpty() ? item->GetLabel()
                                           : item->GetShortHelp();
    return wxACC_OK;
  }

  wxAccStatus GetRole(int childId, wxAccRole *role) override {
    if (!role)
      return wxACC_FAIL;
    if (childId == 0) {
      *role = wxROLE_SYSTEM_TOOLBAR;
      return wxACC_OK;
    }
    wxAuiToolBarItem *item = ItemFor(childId);
    if (!item)
      return wxACC_FAIL;
    if (item->GetWindow()) {
      *role = wxROLE_SYSTEM_PANE; // the real control reports its own detailed role
      return wxACC_OK;
    }
    switch (item->GetKind()) {
    case wxITEM_SEPARATOR:
      *role = wxROLE_SYSTEM_SEPARATOR;
      break;
    case wxITEM_CHECK:
      *role = wxROLE_SYSTEM_CHECKBUTTON;
      break;
    case wxITEM_RADIO:
      *role = wxROLE_SYSTEM_RADIOBUTTON;
      break;
    case wxITEM_NORMAL:
    case wxITEM_DROPDOWN:
      *role = wxROLE_SYSTEM_PUSHBUTTON;
      break;
    default:
      // A spacer (no label) or a text label (has one): wxAuiToolBar's own kinds
      // for these are private, so tell them apart by whether they carry a label.
      *role = item->GetLabel().IsEmpty() ? wxROLE_SYSTEM_WHITESPACE
                                         : wxROLE_SYSTEM_STATICTEXT;
      break;
    }
    return wxACC_OK;
  }

  wxAccStatus GetState(int childId, long *state) override {
    if (!state)
      return wxACC_FAIL;
    if (childId == 0) {
      *state = 0;
      return wxACC_OK;
    }
    wxAuiToolBarItem *item = ItemFor(childId);
    if (!item)
      return wxACC_FAIL;
    long s = wxACC_STATE_SYSTEM_FOCUSABLE;
    if (!m_toolBar->GetToolEnabled(item->GetId()))
      s |= wxACC_STATE_SYSTEM_UNAVAILABLE;
    if ((item->GetKind() == wxITEM_CHECK || item->GetKind() == wxITEM_RADIO) &&
        m_toolBar->GetToolToggled(item->GetId()))
      s |= wxACC_STATE_SYSTEM_CHECKED;
    *state = s;
    return wxACC_OK;
  }

  wxAccStatus GetLocation(wxRect &rect, int elementId) override {
    if (elementId == 0) {
      rect = m_toolBar->GetScreenRect();
      return wxACC_OK;
    }
    wxAuiToolBarItem *item = ItemFor(elementId);
    if (!item)
      return wxACC_FAIL;
    wxRect r = m_toolBar->GetToolRect(item->GetId()); // client coordinates
    rect = wxRect(m_toolBar->ClientToScreen(r.GetPosition()), r.GetSize());
    return wxACC_OK;
  }

  wxAccStatus GetDefaultAction(int childId, wxString *actionName) override {
    if (!actionName)
      return wxACC_FAIL;
    wxAuiToolBarItem *item = ItemFor(childId);
    if (!item || item->GetWindow() || (item->GetKind() == wxITEM_SEPARATOR))
      return wxACC_NOT_IMPLEMENTED;
    *actionName = _("Press");
    return wxACC_OK;
  }

  wxAccStatus DoDefaultAction(int childId) override {
    wxAuiToolBarItem *item = ItemFor(childId);
    if (!item || !m_toolBar->GetToolEnabled(item->GetId()))
      return wxACC_FAIL;
    // wxAuiToolBar fires wxEVT_MENU (== wxEVT_TOOL) with the tool id on a click.
    wxCommandEvent evt(wxEVT_MENU, item->GetId());
    evt.SetEventObject(m_toolBar);
    m_toolBar->GetEventHandler()->ProcessEvent(evt);
    return wxACC_OK;
  }

private:
  // childId is 1-based; tool indices are 0-based.
  wxAuiToolBarItem *ItemFor(int childId) const {
    if (childId < 1)
      return nullptr;
    return m_toolBar->FindToolByIndex(childId - 1);
  }
  ToolBar *m_toolBar;
};
#endif

ToolBar::ToolBar(wxWindow *parent)
  : wxAuiToolBar(parent, -1, wxDefaultPosition, wxDefaultSize,
                 wxAUI_TB_OVERFLOW | wxAUI_TB_PLAIN_BACKGROUND |
                 wxAUI_TB_HORIZONTAL),
    m_defaultCellStyle(GC_TYPE_CODE),
    m_canCopy_old(true),
    m_canCut_old(true),
    m_canSave_old(true),
    m_canPrint_old(true),
    m_canEvalTillHere_old(true),
    m_canEvalThisCell_old(true),
    m_worksheetEmpty_old(false)
{
  m_svgRast.reset(wxm_nsvgCreateRasterizer());
  SetGripperVisible(false);
  SetToolBitmapSize(GetOptimalBitmapSize());
  AddTools();
  // Bound here, not in AddTools(), which runs again every time the user shows
  // or hides a section: binding there made the context menu pop up once more
  // for every time it had run.
  Bind(wxEVT_SIZE, &ToolBar::OnSize, this);
  Bind(wxEVT_RIGHT_DOWN, &ToolBar::OnMouseRightDown, this);
#if wxUSE_ACCESSIBILITY
  // Expose the owner-drawn tools to screen readers (the window takes ownership).
  SetAccessible(new ToolBarAccessible(this));
#endif
}

void ToolBar::AddTools() {
  Clear();
  m_ppi = wxDefaultSize;
  // Every tool added below starts out enabled, so the remembered states
  // CanUndo(), CanCopy(), ... compare against must say so, too. Otherwise, after
  // the user added or removed a group of tools via the context menu, a tool
  // whose remembered state was "disabled" would stay enabled until that state
  // happened to change - e.g. an Undo button that is active with nothing to
  // undo.
  m_canUndo_old = true;
  m_canRedo_old = true;
  m_canCopy_old = true;
  m_canCut_old = true;
  m_canSave_old = true;
  m_canPrint_old = true;
  m_canEvalTillHere_old = true;
  m_canEvalThisCell_old = true;
  m_canFormatText_old = true;
  m_worksheetEmpty_old = false;

  // The cell type drop-down and the animation slider are asked for their
  // state from elsewhere (e.g. GetCellType() decides the type of a new cell),
  // so they are created even if their section is hidden - just not shown.
  wxArrayString textStyle;
  textStyle.Add(_("Maths"));
  textStyle.Add(_("Text"));
  textStyle.Add(_("Title"));
  textStyle.Add(_("Section"));
  textStyle.Add(_("Subsection"));
  textStyle.Add(_("Subsubsection"));
  textStyle.Add(_("Heading 5"));
  textStyle.Add(_("Heading 6"));
  int textStyleSelection = 0;
  if (m_textStyle)
    textStyleSelection = m_textStyle->GetSelection();
  wxDELETE(m_textStyle);
  m_textStyle = new wxChoice(this, tb_changeStyle, wxDefaultPosition,
                             wxDefaultSize, textStyle);
  m_textStyle->SetToolTip(
                          _("For faster creation of cells the following shortcuts exist:\n\n"
                            "   Ctrl+0: Math cell\n"
                            "   Ctrl+1: Text cell\n"
                            "   Ctrl+2: Title cell\n"
                            "   Ctrl+3: Section cell\n"
                            "   Ctrl+4: Subsection cell\n"
                            "   Ctrl+5: Sub-Subsection cell\n"
                            "   Ctrl+6: Heading5 cell\n"
                            "   Ctrl+7: Heading6 cell\n"));
  m_textStyle->SetSelection(textStyleSelection);
  m_textStyle->Show(ShowSection(ToolBarSections::Section::CellStyle));

  m_ppi = GetPPI();
  int sliderWidth = std::max(m_ppi.x, 75) * 200 / 72;
  int width, height;
  wxDisplaySize(&width, &height);
  if (width < 800)
    sliderWidth = std::min(sliderWidth, 100);
  wxDELETE(m_plotSlider);
  m_plotSlider = new wxSlider(this, plot_slider_id, 0, 0, 10, wxDefaultPosition,
                              wxSize(sliderWidth, -1), wxSL_HORIZONTAL);
  m_plotSlider->SetToolTip(
                           _("After clicking on animations created with with_slider_draw() or "
                             "similar, this slider allows changing the current frame."));
  m_plotSlider->Enable(false);
  m_animationMaxIndex = 0;
  m_plotSlider->Show(ShowSection(ToolBarSections::Section::Animation));

  // The last section that was shown
  std::optional<ToolBarSections::Section> previous;
  for (const auto section : SectionOrder()) {
    if (!ShowSection(section))
      continue;
#ifndef __WXOSX__
    if (previous && ToolBarSections::SeparatorBetween(*previous, section))
      AddSeparator();
#endif
    AddSection(section);
    previous = section;
  }
  Realize();
}

void ToolBar::AddSection(ToolBarSections::Section section) {
  using ToolBarSections::Section;
  switch (section) {
  case Section::New:
    AddTool(wxID_NEW, _("New"), wxArtProvider::GetBitmapBundle(wxART_NEW, wxART_TOOLBAR), _("New document"));
    break;
  case Section::OpenSave:
    AddTool(wxID_OPEN, _("Open"), wxArtProvider::GetBitmapBundle(wxART_FILE_OPEN, wxART_TOOLBAR), _("Open document"));
    AddTool(wxID_SAVE, _("Save"), wxArtProvider::GetBitmapBundle(wxART_FILE_SAVE, wxART_TOOLBAR), _("Save document"));
    break;
  case Section::Print:
    AddTool(wxID_PRINT, _("Print"), wxArtProvider::GetBitmapBundle(wxART_PRINT, wxART_TOOLBAR), _("Print document"));
    break;
  case Section::UndoRedo:
    AddTool(wxID_UNDO, _("Undo"), wxArtProvider::GetBitmapBundle(wxART_UNDO, wxART_TOOLBAR));
    AddTool(wxID_REDO, _("Redo"), wxArtProvider::GetBitmapBundle(wxART_REDO, wxART_TOOLBAR));
    break;
  case Section::Options:
    AddTool(wxID_PREFERENCES, _("Options"), wxArtProvider::GetBitmapBundle(wxmaximaART_GTK_PREFERENCES, wxART_TOOLBAR), _("Configure wxMaxima"));
    break;
  case Section::CopyPaste:
    AddTool(wxID_CUT, _("Cut"), wxArtProvider::GetBitmapBundle(wxART_CUT, wxART_TOOLBAR), _("Cut selection"));
    AddTool(wxID_COPY, _("Copy"), wxArtProvider::GetBitmapBundle(wxART_COPY, wxART_TOOLBAR), _("Copy selection"));
    AddTool(wxID_PASTE, _("Paste"), wxArtProvider::GetBitmapBundle(wxART_PASTE, wxART_TOOLBAR), _("Paste from clipboard"));
    break;
  case Section::SelectAll:
    AddTool(wxID_SELECTALL, _("Select all"), wxArtProvider::GetBitmapBundle(wxmaximaART_GTK_SELECT_ALL, wxART_TOOLBAR), _("Select all"));
    break;
  case Section::Search:
    AddTool(wxID_FIND, _("Find"), wxArtProvider::GetBitmapBundle(wxART_FIND_AND_REPLACE, wxART_TOOLBAR), _("Find and replace"));
    break;
  case Section::MaximaControl:
    AddTool(menu_restart_id, _("Restart Maxima"), wxArtProvider::GetBitmapBundle(wxmaximaART_VIEW_REFRESH1, wxART_TOOLBAR),
            _("Completely stop maxima and restart it"));
    AddTool(tb_interrupt, _("Interrupt"), wxArtProvider::GetBitmapBundle(wxmaximaART_GTK_STOP, wxART_TOOLBAR),
            _("Interrupt current computation. To completely restart maxima press "
              "the button left to this one."));
    AddTool(tb_follow, _("Follow"), wxArtProvider::GetBitmapBundle(wxmaximaART_ARROW_UP_SQUARE, wxART_TOOLBAR), _("Return to the cell that is currently being evaluated"));
    EnableTool(tb_follow, false);
    break;
  case Section::Evaluate:
    AddTool(tb_eval, _("Evaluate current cell"), wxArtProvider::GetBitmapBundle(wxmaximaART_GO_NEXT, wxART_TOOLBAR),
            _("Send the current cell to maxima"));
    AddTool(tb_eval_all, _("Evaluate all"), wxArtProvider::GetBitmapBundle(wxmaximaART_GO_NEXT, wxART_TOOLBAR),
            _("Send all cells to maxima"));
    AddTool(tb_evaltillhere, _("Evaluate to point"), wxArtProvider::GetBitmapBundle(wxmaximaART_GO_BOTTOM, wxART_TOOLBAR),
            _("Evaluate the file from its beginning to the cell above the cursor"));
    AddTool(tb_evaluate_rest, _("Evaluate the rest"), wxArtProvider::GetBitmapBundle(wxmaximaART_GO_LAST, wxART_TOOLBAR),
            _("Evaluate the file from the cursor to its end"));
    break;
  case Section::HideCode:
    AddTool(tb_hideCode, _("Hide Code"), wxArtProvider::GetBitmapBundle(wxmaximaART_EYE_SLASH, wxART_TOOLBAR), _("Toggle the visibility of code cells"));
    break;
  case Section::CellStyle:
    AddControl(m_textStyle);
    break;
  case Section::TextFormat:
    // Character formatting for text cells (GH #492). Toolbar-only for now:
    // the obvious shortcuts Ctrl+B/I/U are already taken.
    AddTool(tb_bold, _("Bold"), wxArtProvider::GetBitmapBundle(wxmaximaART_FORMAT_TEXT_BOLD, wxART_TOOLBAR),
            _("Bold text"), wxITEM_CHECK);
    AddTool(tb_italic, _("Italic"), wxArtProvider::GetBitmapBundle(wxmaximaART_FORMAT_TEXT_ITALIC, wxART_TOOLBAR),
            _("Italic text"), wxITEM_CHECK);
    AddTool(tb_underline, _("Underline"), wxArtProvider::GetBitmapBundle(wxmaximaART_FORMAT_TEXT_UNDERLINE, wxART_TOOLBAR),
            _("Underlined text"), wxITEM_CHECK);
    AddTool(tb_strikethrough, _("Strikethrough"),
            wxArtProvider::GetBitmapBundle(wxmaximaART_FORMAT_TEXT_STRIKETHROUGH, wxART_TOOLBAR),
            _("Struck-through text"), wxITEM_CHECK);
    AddTool(tb_superscript, _("Superscript"),
            wxArtProvider::GetBitmapBundle(wxmaximaART_FORMAT_TEXT_SUPERSCRIPT, wxART_TOOLBAR),
            _("Superscript: smaller text, raised above the line"), wxITEM_CHECK);
    AddTool(tb_subscript, _("Subscript"),
            wxArtProvider::GetBitmapBundle(wxmaximaART_FORMAT_TEXT_SUBSCRIPT, wxART_TOOLBAR),
            _("Subscript: smaller text, lowered below the line"), wxITEM_CHECK);
    // Only a text cell can be formatted, and there is none active yet.
    TextFormatState(false, TextFormat::None);
    break;
  case Section::Animation:
    AddTool(tb_animation_startStop, _("Start or Stop animation"), wxArtProvider::GetBitmapBundle(wxmaximaART_MEDIA_PLAYBACK_START, wxART_TOOLBAR),
            _("Start or stop the currently selected animation that has been "
              "created with the with_slider class of commands"));
    EnableTool(tb_animation_startStop, false);
    AddControl(m_plotSlider);
    break;
  case Section::FlexibleSpace:
    AddStretchSpacer(100);
    break;
  case Section::Help:
    AddTool(wxID_HELP, _("Help"), wxArtProvider::GetBitmapBundle(wxART_HELP, wxART_TOOLBAR), _("Show wxMaxima help"));
    break;
  }
}

bool ToolBar::ShowSection(ToolBarSections::Section section) {
  bool show = ToolBarSections::ShownByDefault(section);
  wxConfig::Get()->Read(ToolBarSections::VisibilityConfigKey(section), &show);
  return show;
}

void ToolBar::ShowSection(ToolBarSections::Section section, bool show) {
  wxConfig::Get()->Write(ToolBarSections::VisibilityConfigKey(section), show);
}

std::vector<ToolBarSections::Section> ToolBar::SectionOrder() {
  wxString order;
  wxConfig::Get()->Read(wxS("Toolbar/sectionOrder"), &order);
  return ToolBarSections::ParseOrder(order);
}

void ToolBar::SectionOrder(const std::vector<ToolBarSections::Section> &order) {
  wxConfig::Get()->Write(wxS("Toolbar/sectionOrder"),
                         ToolBarSections::OrderToString(order));
}

wxString ToolBar::SectionName(ToolBarSections::Section section) {
  using ToolBarSections::Section;
  switch (section) {
  case Section::New:
    return _("New button");
  case Section::OpenSave:
    return _("Open and save button");
  case Section::Print:
    return _("Print button");
  case Section::UndoRedo:
    return _("Undo and redo button");
  case Section::Options:
    return _("Preferences button");
  case Section::CopyPaste:
    return _("Copy, Cut and Paste button");
  case Section::SelectAll:
    return _("Select All button");
  case Section::Search:
    return _("Search button");
  case Section::MaximaControl:
    return _("Restart, interrupt and follow buttons");
  case Section::Evaluate:
    return _("Evaluation buttons");
  case Section::HideCode:
    return _("Hide code button");
  case Section::CellStyle:
    return _("Cell type");
  case Section::TextFormat:
    return _("Text formatting buttons");
  case Section::Animation:
    return _("Animation controls");
  case Section::FlexibleSpace:
    return _("Flexible space (pushes what follows to the right)");
  case Section::Help:
    return _("Help button");
  }
  return wxEmptyString;
}

wxSize ToolBar::GetPPI()
{
  wxSize ppi(-1, -1);
  int display_idx = wxDisplay::GetFromWindow(GetParent());
  if (display_idx < 0)
    ppi = wxSize(72, 72);
  else
    ppi = wxDisplay(display_idx).GetPPI();
  if ((ppi.x <= 10) || (ppi.y <= 10))
    ppi = wxSize(72, 72);
  return ppi;
}

void ToolBar::UpdateBitmaps() {
  wxSize bitmapSize = GetOptimalBitmapSize();
  SetToolBitmapSize(bitmapSize);

  wxSize ppi = GetPPI();
  if ((ppi.x == m_ppi.x) && (ppi.y == m_ppi.y))
    return;
  wxLogMessage(_("Display resolution according to wxWidgets: %li x %li ppi"),
               static_cast<long>(ppi.x),
               static_cast<long>(ppi.y));

  m_ppi = ppi;

  SetToolBitmap(tb_eval, wxArtProvider::GetBitmapBundle(wxmaximaART_GO_NEXT, wxART_TOOLBAR));
  SetToolBitmap(tb_eval_all, wxArtProvider::GetBitmapBundle(wxmaximaART_GO_JUMP, wxART_TOOLBAR));
  SetToolBitmap(wxID_PREFERENCES, wxArtProvider::GetBitmapBundle(wxmaximaART_GTK_PREFERENCES, wxART_TOOLBAR));
  SetToolBitmap(wxID_SELECTALL, wxArtProvider::GetBitmapBundle(wxmaximaART_GTK_SELECT_ALL, wxART_TOOLBAR));
  SetToolBitmap(menu_restart_id, wxArtProvider::GetBitmapBundle(wxmaximaART_VIEW_REFRESH1, wxART_TOOLBAR));
  SetToolBitmap(tb_interrupt, wxArtProvider::GetBitmapBundle(wxmaximaART_GTK_STOP, wxART_TOOLBAR));
  SetToolBitmap(tb_follow, m_followIcon);
  SetToolBitmap(tb_evaltillhere, wxArtProvider::GetBitmapBundle(wxmaximaART_GO_BOTTOM, wxART_TOOLBAR));
  SetToolBitmap(tb_evaluate_rest, wxArtProvider::GetBitmapBundle(wxmaximaART_GO_LAST, wxART_TOOLBAR));
  SetToolBitmap(tb_hideCode, wxArtProvider::GetBitmapBundle(wxmaximaART_EYE_SLASH, wxART_TOOLBAR));
  if (FindTool(tb_bold)) {
    SetToolBitmap(tb_bold, wxArtProvider::GetBitmapBundle(wxmaximaART_FORMAT_TEXT_BOLD, wxART_TOOLBAR));
    SetToolBitmap(tb_italic, wxArtProvider::GetBitmapBundle(wxmaximaART_FORMAT_TEXT_ITALIC, wxART_TOOLBAR));
    SetToolBitmap(tb_underline, wxArtProvider::GetBitmapBundle(wxmaximaART_FORMAT_TEXT_UNDERLINE, wxART_TOOLBAR));
    SetToolBitmap(tb_strikethrough,
                  wxArtProvider::GetBitmapBundle(wxmaximaART_FORMAT_TEXT_STRIKETHROUGH, wxART_TOOLBAR));
    SetToolBitmap(tb_superscript,
                  wxArtProvider::GetBitmapBundle(wxmaximaART_FORMAT_TEXT_SUPERSCRIPT, wxART_TOOLBAR));
    SetToolBitmap(tb_subscript,
                  wxArtProvider::GetBitmapBundle(wxmaximaART_FORMAT_TEXT_SUBSCRIPT, wxART_TOOLBAR));
  }

  SetToolBitmap(tb_animation_startStop, wxArtProvider::GetBitmapBundle(wxmaximaART_MEDIA_PLAYBACK_START, wxART_TOOLBAR));
  Realize();
}

void ToolBar::SetDefaultCellStyle() {
  switch (m_textStyle->GetSelection()) {
  case 0:
    m_defaultCellStyle = GC_TYPE_CODE;
    break;
  case 1:
    m_defaultCellStyle = GC_TYPE_TEXT;
    break;
  case 2:
    m_defaultCellStyle = GC_TYPE_TITLE;
    break;
  case 3:
    m_defaultCellStyle = GC_TYPE_SECTION;
    break;
  case 4:
    m_defaultCellStyle = GC_TYPE_SUBSECTION;
    break;
  case 5:
    m_defaultCellStyle = GC_TYPE_SUBSUBSECTION;
    break;
  case 6:
    m_defaultCellStyle = GC_TYPE_HEADING5;
    break;
  case 7:
    m_defaultCellStyle = GC_TYPE_HEADING6;
    break;
  default: {
  }
  }
}

GroupType ToolBar::GetCellType() {
  switch (m_textStyle->GetSelection()) {
  case 1:
    return GC_TYPE_TEXT;
  case 2:
    return GC_TYPE_TITLE;
  case 3:
    return GC_TYPE_SECTION;
  case 4:
    return GC_TYPE_SUBSECTION;
  case 5:
    return GC_TYPE_SUBSUBSECTION;
  case 6:
    return GC_TYPE_HEADING5;
  case 7:
    return GC_TYPE_HEADING6;
  case 8:
    return GC_TYPE_IMAGE;
  case 9:
    return GC_TYPE_PAGEBREAK;
  default:
    return GC_TYPE_CODE;
  }
}

void ToolBar::SetCellStyle(GroupType style) {
  switch (style) {
  case GC_TYPE_CODE:
  case GC_TYPE_TEXT:
  case GC_TYPE_TITLE:
  case GC_TYPE_SECTION:
  case GC_TYPE_SUBSECTION:
  case GC_TYPE_SUBSUBSECTION:
  case GC_TYPE_HEADING5:
  case GC_TYPE_HEADING6:
    break;
  default:
    style = m_defaultCellStyle;
  }

  switch (style) {
  case GC_TYPE_CODE:
    m_textStyle->SetSelection(0);
    break;
  case GC_TYPE_TEXT:
    m_textStyle->SetSelection(1);
    break;
  case GC_TYPE_TITLE:
    m_textStyle->SetSelection(2);
    break;
  case GC_TYPE_SECTION:
    m_textStyle->SetSelection(3);
    break;
  case GC_TYPE_SUBSECTION:
    m_textStyle->SetSelection(4);
    break;
  case GC_TYPE_SUBSUBSECTION:
    m_textStyle->SetSelection(5);
    break;
  case GC_TYPE_HEADING5:
    m_textStyle->SetSelection(6);
    break;
  case GC_TYPE_HEADING6:
    m_textStyle->SetSelection(7);
    break;
  default:
    break;
  }
}

void ToolBar::TextFormatState(bool value, TextFormat::Format active) {
  if (!FindTool(tb_bold))
    return;
  const std::pair<int, TextFormat::Format> buttons[] = {
    {tb_bold, TextFormat::Bold},
    {tb_italic, TextFormat::Italic},
    {tb_underline, TextFormat::Underline},
    {tb_strikethrough, TextFormat::Strikethrough},
    {tb_superscript, TextFormat::Superscript},
    {tb_subscript, TextFormat::Subscript}};
  if (value != m_canFormatText_old) {
    for (const auto &[id, flag] : buttons)
      EnableTool(id, value);
    m_canFormatText_old = value;
  }
  for (const auto &[id, flag] : buttons) {
    // A disabled button doesn't show as pressed: there is no format to show.
    const bool state = value && (active & flag);
    // Like EnableTool(): wxAuiToolBar::ToggleTool() doesn't repaint.
    if (GetToolToggled(id) != state) {
      ToggleTool(id, state);
      Refresh();
    }
  }
}

void ToolBar::AnimationButtonState(AnimationStartStopState state) {
  if (m_AnimationStartStopState != state) {
    switch (state) {
    case Running:
      m_plotSlider->Enable(true);
      if (m_AnimationStartStopState != Running) {
        SetToolBitmap(tb_animation_startStop, m_StopButton);
      }
      EnableTool(tb_animation_startStop, true);
      break;
    case Stopped:
      if (m_AnimationStartStopState == Running) {
        SetToolBitmap(tb_animation_startStop, m_PlayButton);
      }
      EnableTool(tb_animation_startStop, true);
      m_plotSlider->Enable(true);
      break;
    case Inactive:
      EnableTool(tb_animation_startStop, false);
      m_plotSlider->Enable(false);
      m_plotSlider->SetToolTip(
                               _("After clicking on animations created with with_slider_draw() or "
                                 "similar, this slider allows changing the current frame."));
      m_animationMaxIndex = 0;
      m_animationDisplayedIndex = 0;

      if (m_AnimationStartStopState == Running) {
        SetToolBitmap(tb_animation_startStop, m_PlayButton);
      }
      break;
    }
    m_AnimationStartStopState = state;
    Realize();
  }
}

void ToolBar::OnSize(wxSizeEvent &event) {
  //  AddTools();
  event.Skip();
}

void ToolBar::OnMouseRightDown(wxMouseEvent &WXUNUSED(event)) {
  // Lists the sections in the order they are shown in, so the menu matches
  // the toolbar. Reordering them is done in the configuration dialogue.
  wxMenu popupMenu;
  for (const auto section : SectionOrder()) {
    popupMenu.AppendCheckItem(SectionMenuId(section), SectionName(section));
    popupMenu.Check(SectionMenuId(section), ShowSection(section));
  }
  popupMenu.Bind(wxEVT_MENU, &ToolBar::OnMenu, this);
  PopupMenu(&popupMenu);
}

void ToolBar::OnMenu(wxCommandEvent &event) {
  for (const auto section : ToolBarSections::DefaultOrder()) {
    if (event.GetId() == SectionMenuId(section)) {
      ShowSection(section, !ShowSection(section));
      AddTools();
      return;
    }
  }
}
