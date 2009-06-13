///
///  Copyright (C) 2009 Andrej Vodopivec <andrejv@users.sourceforge.net>
///
///  This program is free software; you can redistribute it and/or modify
///  it under the terms of the GNU General Public License as published by
///  the Free Software Foundation; either version 2 of the License, or
///  (at your option) any later version.
///
///  This program is distributed in the hope that it will be useful,
///  but WITHOUT ANY WARRANTY; without even the implied warranty of
///  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
///  GNU General Public License for more details.
///
///
///  You should have received a copy of the GNU General Public License
///  along with this program; if not, write to the Free Software
///  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
///

#ifndef GROUPCELL_H_
#define GROUPCELL_H_

#include "MathCell.h"

#define EMPTY_INPUT_LABEL wxT("-->  ")

enum
{
  GC_TYPE_CODE,
  GC_TYPE_TITLE,
  GC_TYPE_SECTION,
  GC_TYPE_SUBSECTION,
  GC_TYPE_TEXT,
  GC_TYPE_IMAGE,
  GC_TYPE_PAGEBREAK
};

class GroupCell: public MathCell
{
public:
  GroupCell(int groupType, wxString initString = wxEmptyString);
  ~GroupCell();
  MathCell* Copy(bool all);
  void Destroy();
  // general methods
  int GetGroupType() { return m_groupType; }
  void SetParent(MathCell *parent, bool all); // setting parent for all mathcells in GC
  void SetWorking(bool working) { m_working = working; }
  // selection methods
  void SelectInner(wxRect& rect, MathCell** first, MathCell** last);
  void SelectPoint(wxPoint& rect, MathCell** first, MathCell** last);
  void SelectOutput(MathCell **start, MathCell **end);
  void SelectRectInOutput(wxRect& rect, wxPoint& one, wxPoint& two, MathCell **first, MathCell **last);
  void SelectRectGroup(wxRect& rect, wxPoint& one, wxPoint& two, MathCell **first, MathCell **last);
  // methods for manipulating GroupCell
  bool SetEditableContent(wxString text);
  MathCell* GetEditable(); // returns pointer to editor (if there is one)
  void AppendOutput(MathCell *cell);
  void RemoveOutput();
  // exporting
  wxString ToTeX(bool all, wxString imgDir, wxString filename, int *imgCounter);
  wxString ToXML(bool all);
  // hide status
  bool IsHidden() { return m_hide; }
  void Hide(bool hide);
  void SwitchHide();
  wxRect HideRect();
  // raw manipulation of GC (should be protected)
  void SetInput(MathCell *input);
  void SetOutput(MathCell *output);
  void AppendInput(MathCell *cell);
  MathCell* GetPrompt() { return m_input; }
  MathCell* GetInput() { return m_input->m_next; }
  MathCell* GetLabel() { return m_output; }
  MathCell* GetOutput() { if (m_output == NULL) return NULL; else return m_output->m_next; }
  //
  wxRect GetOutputRect() { return m_outputRect; }
  void RecalculateSize(CellParser& parser, int fontsize, bool all);
  void RecalculateWidths(CellParser& parser, int fontsize, bool all);
  void Recalculate(CellParser& parser, int d_fontsize, int m_fontsize);
  void BreakUpCells(CellParser parser, int fontsize, int clientWidth);
  void BreakUpCells(MathCell *cell, CellParser parser, int fontsize, int clientWidth);
  void UnBreakUpCells();
  void BreakLines(int fullWidth);
  void BreakLines(MathCell *cell, int fullWidth);
  void ResetInputLabel(bool all = false); // if !all only this GC is reset
  // folding and unfolding
  bool IsFoldable() { return ((m_groupType == GC_TYPE_SECTION) ||
                              (m_groupType == GC_TYPE_TITLE) ||
                              (m_groupType == GC_TYPE_SUBSECTION)); }
  GroupCell *GetHiddenTree() { return m_hiddenTree; }
  bool HideTree(GroupCell *tree);
  GroupCell *UnhideTree();
  GroupCell *Fold(); // returns pointer to this or NULL if not successful
  GroupCell *Unfold(); // return pointer to last cell that unfolded
  GroupCell *FoldAll(bool all = false);
  GroupCell *UnfoldAll(bool all = false);
  bool IsLesserGCType(int comparedTo);
  void Number(int &section, int &subsection, int &image);
  void RecalculateAppended(CellParser& parser);
  void Draw(CellParser& parser, wxPoint point, int fontsize, bool all);
protected:
  GroupCell *m_hiddenTree; // here hidden (folded) tree of GCs is stored
  int m_groupType;
  void DestroyOutput();
  MathCell *m_input, *m_output;
  bool m_hide;
  bool m_working;
  int m_indent;
  int m_fontSize;
  int m_mathFontSize;
  MathCell *m_lastInOutput;
  MathCell *m_appendedCells;
  wxRect m_outputRect;
  wxString ToString(bool all);
};

#endif /* GROUPCELL_H_ */
