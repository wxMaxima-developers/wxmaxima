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
  The worksheet's right-click context menu (see WorksheetContextMenu.h).

  The construction code was moved out of Worksheet::OnMouseRightDown; member
  accesses became accessor calls on the worksheet reference, everything else
  is unchanged.
*/

#include "WorksheetContextMenu.h"
#include "ArtProvider.h"
#include "Worksheet.h"
#include "levenshtein/levenshtein.h"
#include <wx/menu.h>
#include <array>
#include <vector>

void PopulateWorksheetContextMenu(Worksheet &worksheet, wxMenu &popupMenu,
                                  int downx, int downy, bool clickInSelection) {
  Configuration *const configuration = worksheet.GetConfig();

  // construct a menu appropriate to what we have
  //
  if (!worksheet.GetActiveCell()) {
    if (worksheet.IsSelected(MC_TYPE_IMAGE) || worksheet.IsSelected(MC_TYPE_SLIDE)) {
      popupMenu.Append(wxID_COPY, _("Copy"), wxEmptyString, wxITEM_NORMAL);
      popupMenu.Append(EventIDs::menu_copy_uuid, _("Copy position (UUID)"));
      popupMenu.Append(EventIDs::popid_image, _("Save Image..."), wxEmptyString,
                       wxITEM_NORMAL);
      if (worksheet.IsSelected(MC_TYPE_SLIDE)) {
#ifdef wxUSE_GIF
  // can only save to animated gif if gif is supported
        popupMenu.Append(EventIDs::popid_animation_save, _("Save Animation..."),
                         wxEmptyString, wxITEM_NORMAL);
#endif
        popupMenu.Append(EventIDs::popid_copy_animation, _("Copy Animation"),
                         wxEmptyString, wxITEM_NORMAL);
        popupMenu.Append(EventIDs::popid_animation_start, _("Start Animation"),
                         wxEmptyString, wxITEM_NORMAL);
      } else {
        const ImgCell * const img = worksheet.GetSelectedImgCell();
        if (img != NULL) {
          popupMenu.AppendSeparator();
          popupMenu.Append(EventIDs::popid_maxsizechooser, _("Restrict Maximum size"),
                           wxEmptyString, wxITEM_NORMAL);
          popupMenu.Append(EventIDs::popid_resolutionchooser, _("Set image resolution"),
                           wxEmptyString, wxITEM_NORMAL);

          wxString imgFile = img->GetOrigImageFile();

          // Disable the reload menu item if the original file name is unknown.
          if (imgFile.Length() == 0) {
            wxMenuItem *reloadItem =
              popupMenu.Append(EventIDs::popid_reloadimage, _("Reload Image"),
                               wxEmptyString, wxITEM_NORMAL);
            reloadItem->Enable(false);
          } else {
            popupMenu.Append(
                             EventIDs::popid_reloadimage,
                             wxString::Format(_("Reload Image \"%s\""), imgFile),
                             wxEmptyString, wxITEM_NORMAL);
          }

          popupMenu.Append(EventIDs::popid_change_image, _("Change Image..."),
                           wxEmptyString, wxITEM_NORMAL);
        }
      }
      if (worksheet.GetCellPointers().m_selectionStart &&
          worksheet.GetCellPointers().m_selectionStart->CanPopOut()) {
        popupMenu.AppendSeparator();
        popupMenu.Append(EventIDs::popid_popup_gnuplot, _("Popout interactively"),
                         wxEmptyString, wxITEM_NORMAL);
      }
    } else if (worksheet.GetCellPointers().m_selectionStart) {
      if (worksheet.IsSelected(MC_TYPE_DEFAULT)) {
        wxString wordUnderCursor = worksheet.GetSelectionStart()->ToString();
        wxString anchor = worksheet.GetMaximaManual()->GetHelpfileAnchorName(wordUnderCursor);
        if (!anchor.IsEmpty()) {
          {
            popupMenu.Append(wxID_HELP, wxString::Format(_("Help on \"%s\""),
                                                         wordUnderCursor));
          }
          popupMenu.Append(EventIDs::menu_copy_uuid, _("Copy position (UUID)"));
          if(worksheet.GetAutocomplete().HasDemofile(wordUnderCursor))
            {
              wxMenuItem *demoItem = new wxMenuItem(&popupMenu,
                                                   EventIDs::menu_help_demo_for_command,
                                                   wxString::Format(_("Demo for \"%s\""),
                                                                    wordUnderCursor));
#if wxCHECK_VERSION(3, 2, 0)
              demoItem->SetBitmap(ArtProvider::GetQuestionmarkBundle());
#endif
              popupMenu.Append(demoItem);
            }
          popupMenu.AppendSeparator();
        }
      }
      if (worksheet.GetCellPointers().m_selectionStart->GetType() == MC_TYPE_GROUP) {
        if (worksheet.CanCopy()) {
          popupMenu.Append(wxID_COPY, _("Copy"), wxEmptyString, wxITEM_NORMAL);
          popupMenu.Append(EventIDs::menu_copy_uuid, _("Copy position (UUID)"));
          popupMenu.Append(EventIDs::popid_copy_matlab, _("Copy for Octave/Matlab"),
                           wxEmptyString, wxITEM_NORMAL);
          popupMenu.Append(EventIDs::popid_copy_tex, _("Copy as LaTeX"), wxEmptyString,
                           wxITEM_NORMAL);
          popupMenu.Append(EventIDs::popid_copy_text, _("Copy as plain text"),
                           wxEmptyString, wxITEM_NORMAL);
          if (worksheet.GetCellPointers().m_selectionStart == worksheet.GetCellPointers().m_selectionEnd)
            popupMenu.Append(EventIDs::popid_copy_mathml,
                             _("Copy as MathML (e.g. to word processor)"),
                             wxEmptyString, wxITEM_NORMAL);
          popupMenu.Append(EventIDs::popid_copy_image, _("Copy as Image"), wxEmptyString,
                           wxITEM_NORMAL);
          if ((worksheet.GetSelectionStart() != NULL) &&
              (worksheet.GetSelectionStart() == worksheet.GetSelectionEnd()) &&
              (worksheet.GetSelectionStart()->GetType() == MC_TYPE_SLIDE))
            popupMenu.Append(EventIDs::popid_copy_animation, _("Copy Animation"),
                             wxEmptyString, wxITEM_NORMAL);
          popupMenu.Append(EventIDs::popid_copy_svg, _("Copy as SVG"), wxEmptyString,
                           wxITEM_NORMAL);
#if wxUSE_ENH_METAFILE
          popupMenu.Append(EventIDs::popid_copy_emf, _("Copy as EMF"), wxEmptyString,
                           wxITEM_NORMAL);
#endif
          popupMenu.Append(EventIDs::popid_copy_rtf, _("Copy as RTF"), wxEmptyString,
                           wxITEM_NORMAL);
          if (worksheet.CanDeleteSelection())
            popupMenu.Append(EventIDs::popid_delete, _("Delete Selection"), wxEmptyString,
                             wxITEM_NORMAL);
        }
        popupMenu.AppendSeparator();
        if (worksheet.GetCellPointers().m_selectionStart == worksheet.GetCellPointers().m_selectionEnd)
          popupMenu.Append(ToolBar::tb_evaltillhere, _("Evaluate Cells Above"),
                           wxEmptyString, wxITEM_NORMAL);

        if (worksheet.GetCellPointers().m_selectionStart == worksheet.GetCellPointers().m_selectionEnd)
          popupMenu.Append(EventIDs::popid_evaluate, _("Evaluate Cell"), wxEmptyString,
                           wxITEM_NORMAL);
        else
          popupMenu.Append(EventIDs::popid_evaluate, _("Evaluate Cell(s)"), wxEmptyString,
                           wxITEM_NORMAL);

        if (worksheet.GetCellPointers().m_selectionStart == worksheet.GetCellPointers().m_selectionEnd)
          popupMenu.Append(ToolBar::tb_evaluate_rest, _("Evaluate Cells Below"),
                           wxEmptyString, wxITEM_NORMAL);

        if (worksheet.CanMergeSelection())
          {
            wxMenuItem *item = new wxMenuItem(&popupMenu,
                                              EventIDs::popid_merge_cells,
                                              _("Merge Cells"));
#if wxCHECK_VERSION(3, 2, 0)
            item->SetBitmap(ArtProvider::GetCellMergeBundle());
#endif
            popupMenu.Append(item);
          }

        // Add a "evaluate this <sectioning unit>" context menu entry.
        GroupCell *group;
        if (worksheet.GetCellPointers().m_selectionEnd)
          group = worksheet.GetCellPointers().m_selectionEnd.CastAs<GroupCell *>();
        else
          group = worksheet.GetCellPointers().m_selectionStart.CastAs<GroupCell *>();
        if (Worksheet::StartOfSectioningUnit(group)->GetGroupType() == GC_TYPE_TITLE) {
          popupMenu.AppendSeparator();
          popupMenu.Append(EventIDs::popid_evaluate_section,
                           _("Evaluate Part\tShift+Ctrl+Enter"), wxEmptyString,
                           wxITEM_NORMAL);
        }
        if (Worksheet::StartOfSectioningUnit(group)->GetGroupType() == GC_TYPE_SECTION) {
          popupMenu.AppendSeparator();
          popupMenu.Append(EventIDs::popid_evaluate_section,
                           _("Evaluate Section\tShift+Ctrl+Enter"),
                           wxEmptyString, wxITEM_NORMAL);
        }
        if (Worksheet::StartOfSectioningUnit(group)->GetGroupType() ==
            GC_TYPE_SUBSECTION) {
          popupMenu.AppendSeparator();
          popupMenu.Append(EventIDs::popid_evaluate_section,
                           _("Evaluate Subsection\tShift+Ctrl+Enter"),
                           wxEmptyString, wxITEM_NORMAL);
        }
        if (Worksheet::StartOfSectioningUnit(group)->GetGroupType() ==
            GC_TYPE_SUBSUBSECTION) {
          popupMenu.AppendSeparator();
          popupMenu.Append(EventIDs::popid_evaluate_section,
                           _("Evaluate Sub-Subsection\tShift+Ctrl+Enter"),
                           wxEmptyString, wxITEM_NORMAL);
        }
        if (Worksheet::StartOfSectioningUnit(group)->GetGroupType() == GC_TYPE_HEADING5) {
          popupMenu.AppendSeparator();
          popupMenu.Append(EventIDs::popid_evaluate_section,
                           _("Evaluate Heading 5\tShift+Ctrl+Enter"),
                           wxEmptyString, wxITEM_NORMAL);
        }
        if (Worksheet::StartOfSectioningUnit(group)->GetGroupType() == GC_TYPE_HEADING6) {
          popupMenu.AppendSeparator();
          popupMenu.Append(EventIDs::popid_evaluate_section,
                           _("Evaluate Heading 6\tShift+Ctrl+Enter"),
                           wxEmptyString, wxITEM_NORMAL);
        }
        if ((group->ContainsSavedAnswers()) ||
            (worksheet.GCContainsCurrentQuestion(group))) {
          popupMenu.AppendSeparator();
          popupMenu.AppendCheckItem(
                                    EventIDs::popid_auto_answer, _("Automatically send known answers"),
                                    _("wxMaxima remembers answers from the last run and is able to "
                                      "automatically send them to maxima, if requested"));
          popupMenu.Check(EventIDs::popid_auto_answer, group->AutoAnswer());
          popupMenu.AppendCheckItem(
                                    EventIDs::popid_never_autoanswer, _("Never offer known answers"),
                                    _("wxMaxima remembers answers from the last run and is able to "
                                      "offer them as the default answer"));
          popupMenu.Check(EventIDs::popid_never_autoanswer,
                          !configuration->OfferKnownAnswers());
        }
        if (group->GetGroupType() == GC_TYPE_IMAGE) {
          popupMenu.AppendSeparator();
          popupMenu.Append(EventIDs::popid_maxsizechooser, _("Restrict Maximum size"),
                           wxEmptyString, wxITEM_NORMAL);
          popupMenu.Append(EventIDs::popid_resolutionchooser, _("Set image resolution"),
                           wxEmptyString, wxITEM_NORMAL);
        }
        if (worksheet.GetCellPointers().m_selectionStart &&
            worksheet.GetCellPointers().m_selectionStart->CanPopOut()) {
          popupMenu.AppendSeparator();
          popupMenu.Append(EventIDs::popid_popup_gnuplot, _("Popout interactively"),
                           wxEmptyString, wxITEM_NORMAL);
        }
        if(downx < configuration->GetIndent() + configuration->GetCellBracketWidth())
          {
            popupMenu.AppendSeparator();
            popupMenu.AppendCheckItem(EventIDs::menu_show_cellbrackets, _("Hide cell brackets"));
            popupMenu.Check(EventIDs::menu_show_cellbrackets, configuration->ShowBrackets());
            popupMenu.AppendCheckItem(EventIDs::menu_print_cellbrackets, _("Print cell brackets"));
            popupMenu.Check(EventIDs::menu_print_cellbrackets, configuration->PrintBrackets());
          }
      } else {
        if (worksheet.CanCopy()) {
          popupMenu.Append(wxID_COPY, _("Copy"), wxEmptyString, wxITEM_NORMAL);
          popupMenu.Append(EventIDs::menu_copy_uuid, _("Copy position (UUID)"));
          popupMenu.Append(EventIDs::popid_copy_matlab, _("Copy for Octave/Matlab"),
                           wxEmptyString, wxITEM_NORMAL);
          popupMenu.Append(EventIDs::popid_copy_tex, _("Copy as LaTeX"), wxEmptyString,
                           wxITEM_NORMAL);
          popupMenu.Append(EventIDs::popid_copy_text, _("Copy as plain text"),
                           wxEmptyString, wxITEM_NORMAL);
          popupMenu.Append(EventIDs::popid_copy_mathml,
                           _("Copy as MathML (e.g. to word processor)"),
                           wxEmptyString, wxITEM_NORMAL);

          popupMenu.Append(EventIDs::popid_copy_image, _("Copy as Image"), wxEmptyString,
                           wxITEM_NORMAL);
          if ((worksheet.GetSelectionStart() != NULL) &&
              (worksheet.GetSelectionStart() == worksheet.GetSelectionEnd()) &&
              (worksheet.GetSelectionStart()->GetType() == MC_TYPE_SLIDE))
            popupMenu.Append(EventIDs::popid_copy_animation, _("Copy Animation"),
                             wxEmptyString, wxITEM_NORMAL);
          popupMenu.Append(EventIDs::popid_copy_svg, _("Copy as SVG"), wxEmptyString,
                           wxITEM_NORMAL);
#if wxUSE_ENH_METAFILE
          popupMenu.Append(EventIDs::popid_copy_emf, _("Copy as EMF"), wxEmptyString,
                           wxITEM_NORMAL);
#endif
          popupMenu.Append(EventIDs::popid_copy_rtf, _("Copy as RTF"), wxEmptyString,
                           wxITEM_NORMAL);
          if (worksheet.CanDeleteSelection())
            popupMenu.Append(EventIDs::popid_delete, _("Delete Selection"), wxEmptyString,
                             wxITEM_NORMAL);
        }
        if (worksheet.IsSelected(MC_TYPE_LABEL)) {
          if (popupMenu.GetMenuItemCount() > 0)
            popupMenu.AppendSeparator();
          popupMenu.Append(EventIDs::popid_add_watch_label, _("Add to watchlist"),
                           wxEmptyString, wxITEM_NORMAL);
        }

        {
          const TextCell * const textCell = worksheet.GetSelectedTextCell();
          if (textCell != NULL)
            {
              if(textCell->GetTextStyle() == TS_SPECIAL_CONSTANT) {
                if (popupMenu.GetMenuItemCount() > 0)
                  popupMenu.AppendSeparator();
                popupMenu.AppendCheckItem(EventIDs::popid_special_constant_percent,
                                          _("Show \"%\" in special constants"),
                                          wxEmptyString);
                popupMenu.Check(EventIDs::popid_special_constant_percent,
                                configuration->CheckKeepPercent());
              }
              if (textCell->IsOperator()) {
                if (popupMenu.GetMenuItemCount() > 0)
                  popupMenu.AppendSeparator();
                popupMenu.AppendCheckItem(EventIDs::popid_hideasterisk,
                                          _("Hide multiplication dots"), wxEmptyString);
                popupMenu.Check(EventIDs::popid_hideasterisk,
                                configuration->HidemultiplicationSign());
                popupMenu.AppendCheckItem(EventIDs::popid_changeasterisk,
                                          _("Show * as multiplication dot"),
                                          wxEmptyString);
                popupMenu.Check(EventIDs::popid_changeasterisk,
                                configuration->GetChangeAsterisk());
              }
              if (textCell->GetTextStyle() == TS_VARIABLE) {
                if (popupMenu.GetMenuItemCount() > 0)
                  popupMenu.AppendSeparator();
                popupMenu.Append(EventIDs::popid_add_watch, _("Add to watchlist"),
                                 wxEmptyString, wxITEM_NORMAL);
              }
            }
        }
        if (worksheet.IsSelected(MC_TYPE_DEFAULT) || worksheet.IsSelected(MC_TYPE_LABEL)) {
          popupMenu.AppendSeparator();
          popupMenu.Append(EventIDs::popid_float, _("To Float"), wxEmptyString,
                           wxITEM_NORMAL);
          popupMenu.AppendSeparator();
          popupMenu.Append(EventIDs::popid_solve, _("Solve..."), wxEmptyString,
                           wxITEM_NORMAL);
          popupMenu.Append(EventIDs::popid_solve_num, _("Find Root..."), wxEmptyString,
                           wxITEM_NORMAL);
          popupMenu.AppendSeparator();
          popupMenu.Append(EventIDs::popid_simplify, _("Simplify Expression"),
                           wxEmptyString, wxITEM_NORMAL);
          popupMenu.Append(EventIDs::popid_factor, _("Factor Expression"), wxEmptyString,
                           wxITEM_NORMAL);
          popupMenu.Append(EventIDs::popid_expand, _("Expand Expression"), wxEmptyString,
                           wxITEM_NORMAL);
          popupMenu.Append(EventIDs::popid_subst, _("Substitute..."), wxEmptyString,
                           wxITEM_NORMAL);
          popupMenu.AppendSeparator();
          popupMenu.Append(EventIDs::popid_integrate, _("Integrate..."), wxEmptyString,
                           wxITEM_NORMAL);
          popupMenu.Append(EventIDs::popid_diff, _("Differentiate..."), wxEmptyString,
                           wxITEM_NORMAL);
          popupMenu.AppendSeparator();
          popupMenu.Append(EventIDs::popid_plot2d, _("Plot 2D..."), wxEmptyString,
                           wxITEM_NORMAL);
          popupMenu.Append(EventIDs::popid_plot3d, _("Plot 3D..."), wxEmptyString,
                           wxITEM_NORMAL);
        }
      }
      if ((worksheet.GetSelectionStart() == worksheet.GetSelectionEnd()) &&
          (worksheet.GetSelectionStart()->GetTextStyle() == TS_NUMBER)) {
        popupMenu.AppendSeparator();
        popupMenu.Append(EventIDs::popid_digits_20, _("Show max. 20 digits"));
        popupMenu.Append(EventIDs::popid_digits_50, _("Show max. 50 digits"));
        popupMenu.Append(EventIDs::popid_digits_100, _("Show max. 100 digits"));
        popupMenu.Append(EventIDs::popid_digits_all, _("Always show all digits"));
        popupMenu.Append(EventIDs::popid_digits_all_linebreak,
                         _("Show all + allow linebreaks in long numbers"));
      }

      if (worksheet.IsSelected(MC_TYPE_LABEL) || worksheet.IsSelected(MC_TYPE_PROMPT) ||
          worksheet.IsSelected(MC_TYPE_MAIN_PROMPT) ||
          worksheet.IsSelected(MC_TYPE_PROMPT)) {
        popupMenu.AppendSeparator();
        popupMenu.AppendCheckItem(EventIDs::popid_inputlabels_hide, _("Show input labels"));
        popupMenu.AppendSeparator();
        popupMenu.AppendRadioItem(EventIDs::popid_labels_user, _("Prefer user labels"));
        popupMenu.AppendRadioItem(EventIDs::popid_labels_autogenerated,
                                  _("Automatic labels"));
        popupMenu.AppendRadioItem(EventIDs::popid_labels_useronly, _("User labels only"));
        popupMenu.AppendRadioItem(EventIDs::popid_labels_disable, _("Don't show labels"));
        popupMenu.Check(EventIDs::popid_inputlabels_hide,
                        configuration->ShowInputLabels());
        popupMenu.Check(EventIDs::popid_labels_autogenerated,
                        configuration->GetLabelChoice() ==
                        Configuration::labels_automatic);
        popupMenu.Check(EventIDs::popid_labels_user,
                        configuration->GetLabelChoice() ==
                        Configuration::labels_prefer_user);
        popupMenu.Check(EventIDs::popid_labels_useronly,
                        configuration->GetLabelChoice() ==
                        Configuration::labels_useronly);
        popupMenu.Check(EventIDs::popid_labels_disable,
                        configuration->GetLabelChoice() ==
                        Configuration::labels_none);
        /***********************
           Label width popup menu commented out.
           (a) does not work at all. Selections here have no influence.
           (b) Source of the issue: https://github.com/wxMaxima-developers/wxmaxima/issues/1964
        wxMenu *labelWidthMenu = new wxMenu();
        for(int i = LABELWIDTH_MIN; i <= LABELWIDTH_MAX; i++)
          {
            labelWidthMenu->AppendRadioItem(EventIDs::popid_labelwidth1 + i,
                                            wxString::Format(wxS("%li em"),
                                                             static_cast<long>(i)));
            if(i == configuration->LabelWidth())
              labelWidthMenu->Check(EventIDs::popid_labelwidth1 + i, true);
          }
        popupMenu.Append(EventIDs::popid_labelwidth, _("Label width"), labelWidthMenu);
        ************************/
      }
    }

    else if (worksheet.HCaretActive()) {
      popupMenu.Append(wxID_PASTE, _("Paste"), wxEmptyString, wxITEM_NORMAL);
      popupMenu.Append(wxID_SELECTALL, _("Select All"), wxEmptyString,
                       wxITEM_NORMAL);
      popupMenu.AppendSeparator();
      popupMenu.Append(EventIDs::popid_insert_text, _("Insert Text Cell"), wxEmptyString,
                       wxITEM_NORMAL);
      popupMenu.Append(EventIDs::popid_insert_title, _("Insert Title Cell"),
                       wxEmptyString, wxITEM_NORMAL);
      popupMenu.Append(EventIDs::popid_insert_section, _("Insert Section Cell"),
                       wxEmptyString, wxITEM_NORMAL);
      popupMenu.Append(EventIDs::popid_insert_subsection, _("Insert Subsection Cell"),
                       wxEmptyString, wxITEM_NORMAL);
      popupMenu.Append(EventIDs::popid_insert_subsubsection,
                       _("Insert Subsubsection Cell"), wxEmptyString,
                       wxITEM_NORMAL);
      popupMenu.Append(EventIDs::popid_insert_heading5, _("Insert Heading5 Cell"),
                       wxEmptyString, wxITEM_NORMAL);
      popupMenu.Append(EventIDs::popid_insert_heading6, _("Insert Heading6 Cell"),
                       wxEmptyString, wxITEM_NORMAL);
      popupMenu.AppendSeparator();
      popupMenu.Append(ToolBar::tb_evaltillhere, _("Evaluate Cells Above"),
                       wxEmptyString, wxITEM_NORMAL);
      popupMenu.Append(ToolBar::tb_evaluate_rest, _("Evaluate Cells Below"),
                       wxEmptyString, wxITEM_NORMAL);
    }
  }

  // popup menu in active cell
  else {
    popupMenu.Append(wxID_CUT, _("Cut"), wxEmptyString, wxITEM_NORMAL);
    popupMenu.Append(wxID_COPY, _("Copy"), wxEmptyString, wxITEM_NORMAL);
    popupMenu.Append(wxID_PASTE, _("Paste"), wxEmptyString, wxITEM_NORMAL);
    popupMenu.AppendSeparator();
    popupMenu.Append(wxID_SELECTALL, _("Select All"), wxEmptyString,
                     wxITEM_NORMAL);

    if (clickInSelection &&
        worksheet.GetActiveCell()->GetGroup()->GetGroupType() == GC_TYPE_CODE)
      popupMenu.Append(EventIDs::popid_comment_selection, _("Comment Selection"),
                       wxEmptyString, wxITEM_NORMAL);
    wxString selectionString1 = worksheet.GetActiveCell()->GetSelectionString();
    if ((selectionString1.IsEmpty()) && (worksheet.GetActiveCell()->ContainsPoint(wxPoint(downx, downy))))
      selectionString1 = worksheet.GetActiveCell()->GetWordUnderCaret();
    const wxString selectionString(selectionString1);
    if (!selectionString.IsEmpty() && !selectionString.Contains("\n") &&
        !selectionString.Contains("\r") && !selectionString.Contains(":") &&
        ((selectionString.at(0) < '0') || (selectionString.at(0) > '9')))
      {
        wxMenuItem *item = new wxMenuItem(&popupMenu,
                                          EventIDs::popid_add_watch,
                                          _("Add to watchlist"));
#if wxCHECK_VERSION(3, 2, 0)
        item->SetBitmap(ArtProvider::GetAddToWatchlistBundle());
#endif
        popupMenu.Append(item);
      }

    if (!clickInSelection)
      {
        wxMenuItem *item = new wxMenuItem(&popupMenu,
                                          EventIDs::popid_divide_cell,
                                          _("Divide Cell"));
#if wxCHECK_VERSION(3, 2, 0)
        item->SetBitmap(ArtProvider::GetDivideCellBundle());
#endif
        popupMenu.Append(item);
      }
    GroupCell *group = NULL;
    if (worksheet.GetActiveCell()) {
      wxASSERT(worksheet.GetActiveCell()->GetGroup());
      group = worksheet.GetActiveCell()->GetGroup();
    }
    if (worksheet.GetCellPointers().m_selectionStart) {
      if (worksheet.GetCellPointers().m_selectionStart->GetType() == MC_TYPE_GROUP) {
        group = worksheet.GetCellPointers().m_selectionStart.CastAs<GroupCell *>();
      }
    }
    if (group) {
      popupMenu.AppendSeparator();
      switch (Worksheet::StartOfSectioningUnit(group)->GetGroupType()) {
      case GC_TYPE_TITLE:
        popupMenu.Append(EventIDs::popid_evaluate_section,
                         _("Evaluate Part\tShift+Ctrl+Enter"), wxEmptyString,
                         wxITEM_NORMAL);
        break;
      case GC_TYPE_SECTION:
        popupMenu.Append(EventIDs::popid_evaluate_section,
                         _("Evaluate Section\tShift+Ctrl+Enter"), wxEmptyString,
                         wxITEM_NORMAL);
        break;
      case GC_TYPE_SUBSECTION:
        popupMenu.Append(EventIDs::popid_evaluate_section,
                         _("Evaluate Subsection\tShift+Ctrl+Enter"),
                         wxEmptyString, wxITEM_NORMAL);
        break;
      case GC_TYPE_SUBSUBSECTION:
        popupMenu.Append(EventIDs::popid_evaluate_section,
                         _("Evaluate Sub-Subsection\tShift+Ctrl+Enter"),
                         wxEmptyString, wxITEM_NORMAL);
        break;
      case GC_TYPE_HEADING5:
        popupMenu.Append(EventIDs::popid_evaluate_section,
                         _("Evaluate Heading 5\tShift+Ctrl+Enter"),
                         wxEmptyString, wxITEM_NORMAL);
        break;
      case GC_TYPE_HEADING6:
        popupMenu.Append(EventIDs::popid_evaluate_section,
                         _("Evaluate Heading 6\tShift+Ctrl+Enter"),
                         wxEmptyString, wxITEM_NORMAL);
        break;
      default:
        break;
      }
      switch (group->GetGroupType()) {
      case GC_TYPE_CODE:
        if ((group->GetEditable() != NULL) &&
            (group->GetEditable()->ContainsPoint(wxPoint(downx, downy)))) {
          wxString wordUnderCursor = group->GetEditable()->GetWordUnderCaret();
          std::array<std::vector<wxString>, 4> dst;
          std::vector<wxString> sameBeginning;
          wxString anchor =
            worksheet.GetMaximaManual()->GetHelpfileAnchorName(wordUnderCursor);
          if (!anchor.IsEmpty())
            {
              popupMenu.Append(wxID_HELP, wxString::Format(_("Help on \"%s\""),
                                                           wordUnderCursor));
            }
          if(worksheet.GetAutocomplete().HasDemofile(wordUnderCursor))
            {
              wxMenuItem *demoItem = new wxMenuItem(&popupMenu,
                                                    EventIDs::menu_help_demo_for_command,
                                                    wxString::Format(_("Demo for \"%s\""),
                                                                     wordUnderCursor));
#if wxCHECK_VERSION(3, 2, 0)
              demoItem->SetBitmap(ArtProvider::GetQuestionmarkBundle());
#endif
              popupMenu.Append(demoItem);
            }
          MaximaManual::HelpFileAnchors helpFileAnchors =
            worksheet.GetMaximaManual()->GetHelpfileAnchors();
          for (const auto &[cmdName, anchorName] : helpFileAnchors) {
            if (cmdName.Contains(" "))
              continue;
            if (cmdName.EndsWith("_"))
              continue;
            if (cmdName.EndsWith("_1"))
              continue;
            if (cmdName.EndsWith("_2"))
              continue;
            if (cmdName.EndsWith("_3"))
              continue;
            if (cmdName.EndsWith("pkg"))
              continue;
            if (cmdName.StartsWith(wordUnderCursor)) {
              if (wordUnderCursor != cmdName)
                sameBeginning.push_back(cmdName);
            } else {
              auto dstnce = LevenshteinDistance(wordUnderCursor, cmdName);
              if ((dstnce <= 4) && (dstnce > 0))
                dst.at(dstnce - 1).push_back(cmdName);
            }
          }
          worksheet.m_replacementsForCurrentWord.clear();
          if (sameBeginning.size() <= 10)
            worksheet.m_replacementsForCurrentWord = std::move(sameBeginning);
          for (int o = 0; o < 4; o++) {
            if (worksheet.m_replacementsForCurrentWord.size() + dst.at(o).size() <=
                10) {
              for (unsigned int i = 0; i < dst.at(o).size(); i++)
                worksheet.m_replacementsForCurrentWord.push_back(dst.at(o).at(i));
            } else
              break;
          }
          for (unsigned int i = 0; i < worksheet.m_replacementsForCurrentWord.size();
               i++)
            popupMenu.Append(EventIDs::popid_suggestion1 + i,
                             _("Suggestion: ") + worksheet.m_replacementsForCurrentWord.at(i));
        }
        popupMenu.AppendSeparator();
        if ((group->ContainsSavedAnswers()) ||
            (worksheet.GCContainsCurrentQuestion(group))) {
          popupMenu.AppendSeparator();
          popupMenu.AppendCheckItem(
                                    EventIDs::popid_auto_answer, _("Automatically send known answers"),
                                    _("wxMaxima remembers answers from the last run and is able to "
                                      "automatically send them to maxima, if requested"));
          popupMenu.Check(EventIDs::popid_auto_answer, group->AutoAnswer());
          popupMenu.AppendCheckItem(
                                    EventIDs::popid_never_autoanswer, _("Never offer known answers"),
                                    _("wxMaxima remembers answers from the last run and is able to "
                                      "offer them as the default answer"));
          popupMenu.Check(EventIDs::popid_never_autoanswer,
                          !configuration->OfferKnownAnswers());
        }
        break;
      case GC_TYPE_TITLE:
        if (group->GetHiddenTree() != NULL)
          popupMenu.Append(EventIDs::popid_unfold, _("Unhide Part"), wxEmptyString,
                           wxITEM_NORMAL);
        else
          popupMenu.Append(EventIDs::popid_fold, _("Hide Part"), wxEmptyString,
                           wxITEM_NORMAL);
        break;
      case GC_TYPE_SECTION:
        if (group->GetHiddenTree() != NULL)
          popupMenu.Append(EventIDs::popid_unfold, _("Unhide Section"), wxEmptyString,
                           wxITEM_NORMAL);
        else
          popupMenu.Append(EventIDs::popid_fold, _("Hide Section"), wxEmptyString,
                           wxITEM_NORMAL);
        break;
      case GC_TYPE_SUBSECTION:
        popupMenu.Append(EventIDs::popid_evaluate_section,
                         _("Evaluate Subsection\tShift+Ctrl+Enter"),
                         wxEmptyString, wxITEM_NORMAL);
        if (group->GetHiddenTree() != NULL)
          popupMenu.Append(EventIDs::popid_unfold, _("Unhide Subsection"), wxEmptyString,
                           wxITEM_NORMAL);
        else
          popupMenu.Append(EventIDs::popid_fold, _("Hide Subsection"), wxEmptyString,
                           wxITEM_NORMAL);
        break;
      case GC_TYPE_SUBSUBSECTION:
        popupMenu.Append(EventIDs::popid_evaluate_section,
                         _("Evaluate Sub-Subsection\tShift+Ctrl+Enter"),
                         wxEmptyString, wxITEM_NORMAL);
        if (group->GetHiddenTree() != NULL)
          popupMenu.Append(EventIDs::popid_unfold, _("Unhide Subsubsection"),
                           wxEmptyString, wxITEM_NORMAL);
        else
          popupMenu.Append(EventIDs::popid_fold, _("Hide Subsubsection"), wxEmptyString,
                           wxITEM_NORMAL);
        break;
      case GC_TYPE_HEADING5:
        popupMenu.Append(EventIDs::popid_evaluate_section,
                         _("Evaluate Heading 5\tShift+Ctrl+Enter"),
                         wxEmptyString, wxITEM_NORMAL);
        if (group->GetHiddenTree() != NULL)
          popupMenu.Append(EventIDs::popid_unfold, _("Unhide Heading 5"), wxEmptyString,
                           wxITEM_NORMAL);
        else
          popupMenu.Append(EventIDs::popid_fold, _("Hide Heading 5"), wxEmptyString,
                           wxITEM_NORMAL);
        break;
      case GC_TYPE_HEADING6:
        popupMenu.Append(EventIDs::popid_evaluate_section,
                         _("Evaluate Heading 6\tShift+Ctrl+Enter"),
                         wxEmptyString, wxITEM_NORMAL);
        if (group->GetHiddenTree() != NULL)
          popupMenu.Append(EventIDs::popid_unfold, _("Unhide Heading 6"), wxEmptyString,
                           wxITEM_NORMAL);
        else
          popupMenu.Append(EventIDs::popid_fold, _("Hide Heading 6"), wxEmptyString,
                           wxITEM_NORMAL);
        break;
      default:
        if (group->GetHiddenTree() != NULL)
          popupMenu.Append(EventIDs::popid_unfold, _("Unhide contents"), wxEmptyString,
                           wxITEM_NORMAL);
        else
          popupMenu.Append(EventIDs::popid_fold, _("Hide contents"), wxEmptyString,
                           wxITEM_NORMAL);
      }
    }

    if (worksheet.GetActiveCell()) {
      wxString toolTip = worksheet.GetActiveCell()->GetLocalToolTip();
      if ((toolTip.IsEmpty()) && (group))
        toolTip = group->GetLocalToolTip();

      if (!(toolTip.IsEmpty())) {
        if (popupMenu.GetMenuItemCount() > 0)
          popupMenu.AppendSeparator();
        popupMenu.AppendCheckItem(
                                  EventIDs::popid_hide_tooltipMarker,
                                  _("Hide yellow tooltip marker for this cell"),
                                  _("Don't mark cells that contain tooltips in yellow"));
        if(group)
          popupMenu.Check(EventIDs::popid_hide_tooltipMarker,
                          group->GetSuppressTooltipMarker());
        popupMenu.AppendCheckItem(
                                  EventIDs::popid_hide_tooltipMarkerForThisMessage,
                                  _("Hide yellow tooltip marker for this message type"),
                                  _("Don't mark this message text in yellow"));
        popupMenu.Check(EventIDs::popid_hide_tooltipMarkerForThisMessage,
                        configuration->HideMarkerForThisMessage(toolTip));
      }
      TextStyle selectionStyle = worksheet.GetActiveCell()->GetSelectionStyle();
      if ((selectionStyle == TS_CODE_VARIABLE) ||
          (selectionStyle == TS_STRING) ||
          (selectionStyle == TS_GREEK_CONSTANT) ||
          (selectionStyle == TS_FUNCTION) || (selectionStyle == TS_USERLABEL)) {
        wxMenu *facts_sub = new wxMenu;
        if ((selectionStyle != TS_FUNCTION) && (selectionStyle != TS_STRING)) {
          facts_sub->Append(EventIDs::popid_property_real, _("Is a real variable"),
                            _("This symbol has no imaginary part"));
          facts_sub->Append(
                            EventIDs::popid_property_complex, _("Is a complex variable"),
                            _("This symbol might have both real and imaginary part"));
          facts_sub->Append(EventIDs::popid_property_imaginary,
                            _("Is an imaginary variable"),
                            _("This symbol has no real part"));
          facts_sub->Append(EventIDs::popid_property_even, _("Even number"));
          facts_sub->Append(EventIDs::popid_property_odd, _("Odd number"));
          facts_sub->Append(EventIDs::popid_property_integer,
                            _("Is an integer variable"));
          facts_sub->Append(EventIDs::popid_property_noninteger,
                            _("Is no integer variable"));
          facts_sub->Append(EventIDs::popid_property_rational, _("A rational variable"));
          facts_sub->Append(EventIDs::popid_property_irrational,
                            _("A irrational variable"));
          facts_sub->Append(EventIDs::popid_property_greaterThan,
                            _("Greater or less than a value"),
                            _("Calls assume() in order to tell maxima about "
                              "the variable's range"));
          facts_sub->AppendSeparator();
          facts_sub->Append(EventIDs::popid_property_constant, _("Symbolic Constant"));
          facts_sub->Append(EventIDs::popid_property_nonarray, _("No Array"));
          facts_sub->Append(EventIDs::popid_property_scalar, _("A Scalar"));
          facts_sub->Append(EventIDs::popid_property_nonscalar, _("No Scalar"));
          facts_sub->Append(EventIDs::popid_property_mainvar,
                            _("Likely to be the main variable"));
          facts_sub->AppendSeparator();
          facts_sub->Append(EventIDs::popid_property_bindtest,
                            _("Don't use if unassigned"),
                            _("Throw an error if this symbol is used before "
                              "assigning it any contents"));
          facts_sub->AppendSeparator();
        }
        if ((selectionStyle == TS_CODE_VARIABLE) ||
            (selectionStyle == TS_FUNCTION)) {
          facts_sub->Append(
                            EventIDs::popid_property_atvalue, _("Value at a specific point"),
                            _("Inform maxima about the value of the function at a specific "
                              "point, e.G. for declaring initial conditions"));
          facts_sub->AppendSeparator();
          facts_sub->Append(EventIDs::popid_property_evenfun, _("Is an even function"),
                            _("This function will return even integers"));
          facts_sub->Append(EventIDs::popid_property_oddfun, _("Is an odd function"),
                            _("This function will return odd integers"));
          facts_sub->Append(EventIDs::popid_property_additive,
                            _("Additive function: f(a+b)=f(a)+f(b)"));
          facts_sub->Append(EventIDs::popid_property_multiplicative,
                            _("Multiplicative function: f(a*b)=f(a)*f(b)"));
          facts_sub->Append(EventIDs::popid_property_nary,
                            _("nary: f(x, f(y,z)) = foo(x,y,z)"));
          facts_sub->Append(EventIDs::popid_property_antisymmetric,
                            _("Antisymmetric function: f(x)=-f(-x)"));
          facts_sub->Append(EventIDs::popid_property_commutative,
                            _("Commutative function"));
          facts_sub->Append(EventIDs::popid_property_symmetric,
                            _("Symmetric function: f(x)=f(-x)"));
          facts_sub->Append(EventIDs::popid_property_increasing,
                            _("Increasing function f(x)>x"));
          facts_sub->Append(EventIDs::popid_property_decreasing,
                            _("Decreasing function f(x)<x"));
          facts_sub->Append(EventIDs::popid_property_integervalued,
                            _("A function returning integers"));
          facts_sub->Append(EventIDs::popid_property_posfun,
                            _("A function returning positive numbers"));
          facts_sub->Append(EventIDs::popid_property_lassociative,
                            _("A left-associative function"));
          facts_sub->Append(EventIDs::popid_property_rassociative,
                            _("A right-associative function"));
          facts_sub->Append(EventIDs::popid_property_linear, _("A linear function"));
          facts_sub->Append(EventIDs::popid_property_outative,
                            _("outative: f(2*x) = 2*f(x)"));
          facts_sub->AppendSeparator();
          facts_sub->Append(
                            EventIDs::popid_property_noun, _("noun: Not evaluated by default"),
                            _("f(x) stays f(x) even if the value of f(x) is computable"));
          facts_sub->Append(EventIDs::popid_property_evfun,
                            _("ev() shall evaluate this automatically"),
                            _("Make an eventual ev() evaluate this"));
          facts_sub->Append(EventIDs::popid_property_evflag,
                            _("Suitable as flag for ev()"),
                            _("Can be specified as flag in ev(exp, flag)"));
        }
        if ((selectionStyle == TS_CODE_VARIABLE) ||
            (selectionStyle == TS_STRING)) {
          if (facts_sub->GetMenuItemCount() > 0)
            facts_sub->AppendSeparator();
          facts_sub->Append(EventIDs::popid_property_alphabetic,
                            _("Declare these chars as ordinary letters"));
        }
        if(!selectionString.IsEmpty())
          {
            popupMenu.Append(
                             wxWindow::NewControlId(),
                             wxString::Format(_("Declare facts about %s"),
                                              selectionString.mb_str()),
                             facts_sub, _("Inform maxima about facts you know for this symbol"));
          }
      }
    }
  }

  if ((worksheet.GetCellPointers().m_selectionStart)) {
    wxString toolTip = worksheet.GetCellPointers().m_selectionStart->GetLocalToolTip();
    const GroupCell *group = worksheet.GetCellPointers().m_selectionStart->GetGroup();
    if (toolTip.IsEmpty())
      toolTip = group->GetLocalToolTip();

    if (!(toolTip.IsEmpty())) {
      if (popupMenu.GetMenuItemCount() > 0)
        popupMenu.AppendSeparator();
      popupMenu.AppendCheckItem(
                                EventIDs::popid_hide_tooltipMarker,
                                _("Hide yellow tooltip marker for this cell"),
                                _("Don't mark cells that contain tooltips in yellow"));
      popupMenu.Check(EventIDs::popid_hide_tooltipMarker,
                      group->GetSuppressTooltipMarker());
      popupMenu.AppendCheckItem(
                                EventIDs::popid_hide_tooltipMarkerForThisMessage,
                                _("Hide yellow tooltip marker for this message type"),
                                _("Don't mark this message text in yellow"));
      popupMenu.Check(EventIDs::popid_hide_tooltipMarkerForThisMessage,
                      configuration->HideMarkerForThisMessage(toolTip));
    }
  }
}
