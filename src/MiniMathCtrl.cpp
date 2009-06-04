///
///  Copyright (C) 2009 Ziga Lenarcic <zigalenarcic@users.sourceforge.net>
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


#include "wxMaxima.h"
#include "MiniMathCtrl.h"
#include "Bitmap.h"
#include "Setup.h"

#include <wx/clipbrd.h>
#include <wx/config.h>
#include <wx/settings.h>
#include <wx/filename.h>
#include <wx/textfile.h>

#define SCROLL_UNIT 10

MiniMathCtrl::MiniMathCtrl(wxWindow* parent, int id, wxPoint position, wxSize size) :
  wxScrolledWindow(parent, id, position, size,
  wxVSCROLL | wxHSCROLL
#if defined __WXMSW__
  | wxSUNKEN_BORDER
#endif
  )
{
  m_defaultText = new TextCell(_("<< Nothing to display >>"));
  m_defaultText->SetStyle(TS_OUTDATED);
  m_tree = m_defaultText;
  m_memory = NULL;
  m_selectionStart = NULL;
  m_selectionEnd = NULL;
  m_last = NULL;
  m_leftDown = false;
  m_mouseDrag = false;
  m_mouseOutside = false;
  AdjustSize();
}

MiniMathCtrl::~MiniMathCtrl() {
  if (m_tree != NULL)
    DestroyTree();
  if (m_memory != NULL)
    delete m_memory;
}

/***
 * Redraw the control
 */
void MiniMathCtrl::OnPaint(wxPaintEvent& event) {
  wxPaintDC dc(this);

  wxMemoryDC dcm;

  // Get the font size
  wxConfig *config = (wxConfig *)wxConfig::Get();
  int fontsize = 12;
  config->Read(wxT("fontSize"), &fontsize);

  // Prepare data
  wxRect rect = GetUpdateRegion().GetBox();
  //printf("Updating rect [%d, %d] -> [%d, %d]\n", rect.x, rect.y, rect.width, rect.height);
  wxSize sz = GetSize();
  int tmp, top, bottom, drop;
  CalcUnscrolledPosition(0, rect.GetTop(), &tmp, &top);
  CalcUnscrolledPosition(0, rect.GetBottom(), &tmp, &bottom);

  // Thest if m_memory is NULL (resize event)
  if (m_memory == NULL)
    m_memory = new wxBitmap(sz.x, sz.y);

  // Prepare memory DC
  wxString bgColStr= wxT("white");
  config->Read(wxT("Style/Background/color"), &bgColStr);
  SetBackgroundColour(wxColour(bgColStr));

  dcm.SelectObject(*m_memory);
  dcm.SetBackground(*(wxTheBrushList->FindOrCreateBrush(GetBackgroundColour(), wxSOLID)));
  dcm.Clear();
  PrepareDC(dcm);
  dcm.SetMapMode(wxMM_TEXT);
  dcm.SetBackgroundMode(wxTRANSPARENT);
  dcm.SetLogicalFunction(wxCOPY);

  CellParser parser(dcm);
  parser.SetBouns(top, bottom);

  // Draw content
  if (m_tree != NULL)
  {
    //
    // First draw selection under content with wxCOPY and selection brush/color
    //
    if (m_selectionStart != NULL)
    {
      MathCell* tmp = m_selectionStart;

#if defined(__WXMAC__)
      dcm.SetPen(wxNullPen); // wxmac doesn't like a border with wxXOR
#else
      dcm.SetPen(*(wxThePenList->FindOrCreatePen(parser.GetColor(TS_SELECTION), 1, 1)));
// window linux, set a pen
#endif
      dcm.SetBrush( *(wxTheBrushList->FindOrCreateBrush(parser.GetColor(TS_SELECTION)))); //highlight c.

      while (tmp != NULL) {
        if (!tmp->m_isBroken && !tmp->m_isHidden)
          if ((tmp->GetType() == MC_TYPE_IMAGE) || (tmp->GetType() == MC_TYPE_SLIDE))
            tmp->DrawBoundingBox(dcm, false, 5); // draw 5 pixels of border for img/slide cells
          else
            tmp->DrawBoundingBox(dcm, false);
        if (tmp == m_selectionEnd)
          break;
        tmp = tmp->m_nextToDraw;
      } // end while (1)
    }
    //
    // Draw content over
    //
    wxPoint point;
    point.x = MC_BASE_INDENT;
    point.y = MC_BASE_INDENT + m_tree->GetMaxCenter();
    // Draw tree
    MathCell* tmp = m_tree;
    drop = tmp->GetMaxDrop();

    dcm.SetPen(*(wxThePenList->FindOrCreatePen(parser.GetColor(TS_DEFAULT), 1, wxSOLID)));
    dcm.SetBrush(*(wxTheBrushList->FindOrCreateBrush(parser.GetColor(TS_DEFAULT))));

    bool changeAsterisk = false;
    config->Read(wxT("changeAsterisk"), &changeAsterisk);
    parser.SetChangeAsterisk(changeAsterisk);

    while (tmp != NULL)
    {
      tmp->m_currentPoint.x = point.x;
      tmp->m_currentPoint.y = point.y;
      if (tmp->DrawThisCell(parser, point))
        tmp->Draw(parser, point, MAX(fontsize, MC_MIN_SIZE), false);
      if (tmp->m_next != NULL) {
        point.x = MC_GROUP_LEFT_INDENT;
        point.y += drop + tmp->m_next->GetMaxCenter();
        point.y += MC_GROUP_SKIP;
        drop = tmp->m_next->GetMaxDrop();
      }
      tmp = tmp->m_next;
    }

  }
  
  // Blit the memory image to the window
  dcm.SetDeviceOrigin(0, 0);
  dc.Blit(0, rect.GetTop(), sz.x, rect.GetBottom() - rect.GetTop() + 1, &dcm,
      0, rect.GetTop());
}


/***
 * Recalculate dimensions of cells
 */
void MiniMathCtrl::RecalculateForce() {
  Recalculate(true);
}

void MiniMathCtrl::Recalculate(bool force) {

  MathCell *tmp = m_tree;
  wxConfig *config = (wxConfig *)wxConfig::Get();
  int fontsize = 12;
  config->Read(wxT("fontSize"), &fontsize);

  wxClientDC dc(this);
  CellParser parser(dc);
  parser.SetForceUpdate(force);
  parser.SetClientWidth(GetClientSize().GetWidth() - MC_GROUP_LEFT_INDENT - MC_BASE_INDENT);

  wxPoint point;
  point.x = MC_GROUP_LEFT_INDENT;
  point.y = MC_BASE_INDENT ;

  if (tmp != NULL)
    point.y += m_tree->GetMaxCenter();

  while (tmp != NULL) {
    tmp->m_currentPoint.x = point.x;
    tmp->m_currentPoint.y = point.y;
    tmp->RecalculateWidths(parser, MAX(fontsize, MC_MIN_SIZE), false);
    tmp->RecalculateSize(parser, MAX(fontsize, MC_MIN_SIZE), false);
    point.y += tmp->GetMaxDrop();
    tmp = tmp->m_next;
    if (tmp != NULL)
      point.y += tmp->GetMaxCenter();
    point.y += MC_GROUP_SKIP;
  }

  AdjustSize();
}

/***
 * Resize the controll
 */
void MiniMathCtrl::OnSize(wxSizeEvent& event) {
  wxDELETE(m_memory);

  if (m_tree != NULL) {
    m_selectionStart = NULL;
    m_selectionEnd = NULL;
    RecalculateForce();
  }
  else
    AdjustSize();

  Refresh();
  wxScrolledWindow::OnSize(event);
}

/***
 * Clear the window
 */
void MiniMathCtrl::ClearWindow() {
  if (m_tree != NULL) {
    m_selectionStart = NULL;
    m_selectionEnd = NULL;
    m_last = NULL;
    DestroyTree();
  }
  Recalculate();
  Refresh();
  Scroll(0, 0);
}



/***
 * Left mouse button down - selection handling
 *
 * Sets m_clickType and m_clickInGC according to what we clicked,
 * and selects appropriately.
 * m_clickType is used in ClickNDrag when click-draging to determine what kind of selection
 * behaviour we want.
 *
 * - check in which GroupCell it falls
 * - if it falls between groupCells activate caret and CLICK_TYPE_GROUP_SELECTION
 * - if it falls within a groupcell investigate where did it fall (input or output)
 */
void MiniMathCtrl::OnMouseLeftDown(wxMouseEvent& event) {
  m_leftDown = true;
  CalcUnscrolledPosition(event.GetX(), event.GetY(), &m_down.x, &m_down.y);

  if (m_tree == NULL)
    return ;

  // default when clicking
  m_selectionStart = m_selectionEnd = NULL;

  Refresh();
}

void MiniMathCtrl::OnMouseLeftUp(wxMouseEvent& event) {
  m_leftDown = false;
  m_mouseDrag = false;
  CheckUnixCopy();
  SetFocus();
}

void MiniMathCtrl::OnMouseMotion(wxMouseEvent& event) {
  if (m_tree == NULL || !m_leftDown)
    return;
  m_mouseDrag = true;
  CalcUnscrolledPosition(event.GetX(), event.GetY(), &m_up.x, &m_up.y);
  if (m_mouseOutside) {
    m_mousePoint.x = event.GetX();
    m_mousePoint.y = event.GetY();
  }
  ClickNDrag(m_down, m_up);
}

/***
 * Select the rectangle surounded by down and up. Called from OnMouseMotion.
 *
 * The method decides what to do, based on the value of m_clickType which
 * was set previously in OnMouseLeftDown. This enables different selection behaviours
 * depending on where we first clicked. If m_clickType equals
 * CLICK_TYPE_NONE - click-draging does not result in a selection (we clicked in hideRect for instance)
 * CLICK_TYPE_GROUP_SELECTION - we are selecting full groupcells only. Only y-coordinate matters.
 * CLICK_TYPE_INPUT_SELECTION - we clicked in an editor (GroupCell::GetEditable()) and draging
 *   results in selecting text in EditorCell
 * CLICK_TYPE_OUTPUT_SELECTION - we clicked in an output, we want selection to be confined to that
 *   GroupCell's output. GC we first clicked in was stored in OnMouseMotion method
 *   into m_clickInGC pointer.
 */
void MiniMathCtrl::ClickNDrag(wxPoint down, wxPoint up) {

  MathCell *st = m_selectionStart, *en = m_selectionEnd;
  wxRect rect;


  // Refresh only if the selection has changed
  if (st != m_selectionStart || en != m_selectionEnd)
    Refresh();
}

/***
 * Get the string representation of the selection
 */
wxString MiniMathCtrl::GetString(bool lb) {

  if (m_selectionStart == NULL) {
      return wxEmptyString;
  }

  wxString s;
  MathCell* tmp = m_selectionStart;
  while (tmp != NULL) {
    if (lb && tmp->BreakLineHere() && s.Length() > 0)
      s += wxT("\n");
    s += tmp->ToString(false);
    if (tmp == m_selectionEnd)
      break;
    tmp = tmp->m_nextToDraw;
  }
  return s;
}

/***
 * Copy selection to clipboard.
 */
bool MiniMathCtrl::Copy() {

  if (m_selectionStart == NULL)
    return false;
  wxString s = GetString(true);

  if (wxTheClipboard->Open()) {
    wxTheClipboard->SetData(new wxTextDataObject(s));
    wxTheClipboard->Close();
    return true;
  }
  return false;
}

bool MiniMathCtrl::CopyTeX() {

  if (m_selectionStart == NULL)
    return false;

  wxString s;
  MathCell* tmp = m_selectionStart;

  bool inMath = false;
  bool inVerbatim = false;
  wxString label;

  if (tmp->GetType() != MC_TYPE_GROUP) {
    inMath = true;
    s = wxT("$$");
  }
  while (tmp != NULL) {
    s += tmp->ToTeX(false);
    if (tmp == m_selectionEnd)
      break;
    tmp = tmp->m_next;
  }
  if (inMath == true)
    s += wxT("$$");

  if (wxTheClipboard->Open()) {
    wxTheClipboard->SetData(new wxTextDataObject(s));
    wxTheClipboard->Close();
    return true;
  }

  return false;
}

bool MiniMathCtrl::CopyCells()
{
  if (m_selectionStart == NULL)
      return false;

  wxString s;
  GroupCell *tmp = (GroupCell *)m_selectionStart->GetParent();
  GroupCell *end = (GroupCell *)m_selectionEnd->GetParent();

  while (tmp != NULL) {

    switch (tmp->GetGroupType())
    {
      case GC_TYPE_CODE:
        s += wxT("/* [wxMaxima: input   start ] */\n");
        s += tmp->GetInput()->ToString(false) + wxT("\n");
        s += wxT("/* [wxMaxima: input   end   ] */\n");
        break;
      case GC_TYPE_TEXT:
        s += wxT("/* [wxMaxima: comment start ]\n");
        s += tmp->GetLabel()->ToString(false) + wxT("\n");
        s += wxT("   [wxMaxima: comment end   ] */\n");
        break;
      case GC_TYPE_SECTION:
        s += wxT("/* [wxMaxima: section start ]\n");
        s += tmp->GetLabel()->ToString(false) + wxT("\n");
        s += wxT("   [wxMaxima: section end   ] */\n");
        break;
      case GC_TYPE_TITLE:
        s += wxT("/* [wxMaxima: title   start ]\n");
        s += tmp->GetLabel()->ToString(false) + wxT("\n");
        s += wxT("   [wxMaxima: title   end   ] */\n");
        break;
    }
    if (tmp == end)
      break;
    tmp = (GroupCell *)tmp->m_next;
  }

  if (wxTheClipboard->Open())
  {
    wxTheClipboard->SetData(new wxTextDataObject(s));
    wxTheClipboard->Close();
    return true;
  }

  return false;
}


/***
 * Get maximum x and y in the tree.
 */
void MiniMathCtrl::GetMaxPoint(int* width, int* height) {
  MathCell* tmp = m_tree;
  int currentHeight= MC_BASE_INDENT;
  int currentWidth= MC_BASE_INDENT;
  *width = MC_BASE_INDENT;
  *height = MC_BASE_INDENT;
  bool bigSkip = false;

  while (tmp != NULL) {
    if (!tmp->m_isBroken) {
      if (tmp->BreakLineHere()) {
        currentHeight += tmp->GetMaxHeight();
        if (bigSkip)
          currentHeight += MC_GROUP_SKIP;
        *height = currentHeight;
        currentWidth = MC_BASE_INDENT + tmp->GetWidth();
        *width = MAX(currentWidth + MC_BASE_INDENT, *width);
      }
      else {
        currentWidth += (tmp->GetWidth() + MC_CELL_SKIP);
        *width = MAX(currentWidth - MC_CELL_SKIP, *width);
      }
      bigSkip = tmp->m_bigSkip;
    }
    tmp = tmp->m_next;
  }

}

/***
 * Adjust the virtual size and scrollbars.
 */
void MiniMathCtrl::AdjustSize() {
  int width= MC_BASE_INDENT, height= MC_BASE_INDENT;
  int clientWidth, clientHeight, virtualHeight;

  GetClientSize(&clientWidth, &clientHeight);
  if (m_tree != NULL)
    GetMaxPoint(&width, &height);

  SetVirtualSize(width, height);
  SetScrollRate(SCROLL_UNIT, SCROLL_UNIT);
}

/***
 * Support for selecting cells outside display
 */
void MiniMathCtrl::OnMouseExit(wxMouseEvent& event) {
  m_mouseOutside = true;
  if (m_leftDown) {
    m_mousePoint.x = event.GetX();
    m_mousePoint.y = event.GetY();
  }
}

void MiniMathCtrl::OnMouseEnter(wxMouseEvent& event) {
  m_mouseOutside = false;
}


/***
 * Destroy the tree
 */
void MiniMathCtrl::DestroyTree() {
  DestroyTree(m_tree);
  m_tree = m_last = NULL;
}

void MiniMathCtrl::DestroyTree(MathCell* tmp) {
  MathCell* tmp1;
  while (tmp != NULL) {
    tmp1 = tmp;
    tmp = tmp->m_next;
    tmp1->Destroy();
    delete tmp1;
  }
}

/***
 * Copy tree
 */
MathCell* MiniMathCtrl::CopyTree() {
  if (m_tree == NULL)
    return (MathCell*)NULL;

  MathCell* tmp1 = m_tree;
  MathCell* tmp;
  MathCell* copy;
  tmp = tmp1->Copy(false);
  copy = tmp;

  tmp1 = tmp1->m_next;
  while (tmp1 != NULL) {
    tmp->AppendCell(tmp1->Copy(false));
    tmp = tmp->m_next;
    tmp1 = tmp1->m_next;
  }
  return copy;
}

/***
 * Copy selection as bitmap
 */
bool MiniMathCtrl::CopyBitmap() {
  MathCell* tmp = CopySelection();

  Bitmap bmp;
  bmp.SetData(tmp);

  return bmp.ToClipboard();
}


/***
 * Copy selection
 */
MathCell* MiniMathCtrl::CopySelection() {
  return CopySelection(m_selectionStart, m_selectionEnd);
}

MathCell* MiniMathCtrl::CopySelection(MathCell* start, MathCell* end, bool asData) {
  MathCell *tmp, *tmp1= NULL, *tmp2= NULL;
  tmp = start;

  while (tmp != NULL) {
    if (tmp1 == NULL) {
      tmp1 = tmp->Copy(false);
      tmp2 = tmp1;
    } else {
      tmp2->AppendCell(tmp->Copy(false));
      tmp2 = tmp2->m_next;
    }
    if (tmp == end)
      break;
    if (asData)
      tmp = tmp->m_next;
    else
      tmp = tmp->m_next;
  }

  return tmp1;
}


void MiniMathCtrl::ScrollToSelectionStart() {
  ScrollToCell(m_selectionStart);
}

void MiniMathCtrl::ScrollToCell(MathCell *cell)
{
  if (cell == NULL)
    return;

  MathCell *tmp = cell->GetParent();
  if (tmp == NULL)
    return;

  int cellY = tmp->GetCurrentY();

  if (cellY == -1)
    return;

  int cellDrop = tmp->GetDrop();
  int cellCenter = tmp->GetCenter();

  int view_x, view_y;
  int height, width;

  GetViewStart(&view_x, &view_y);
  GetSize(&width, &height);

  view_y *= SCROLL_UNIT;

  if (cellY + cellDrop + SCROLL_UNIT > view_y + height - height / 10)
    Scroll(-1, MAX((cellY + cellDrop - height + height / 10)/SCROLL_UNIT + 4, 0));
  else if (cellY - cellCenter - SCROLL_UNIT < view_y && cellDrop + cellCenter < height)
    Scroll(-1, MAX(cellY/SCROLL_UNIT - 2, 0));

  Refresh();
}

void MiniMathCtrl::ShowPoint(wxPoint point) {
  if (point.x == -1 || point.y == -1)
    return;

  int view_x, view_y;
  int height, width;
  bool sc = false;

  int scrollToX = -1, scrollToY = -1;

  GetViewStart(&view_x, &view_y);
  GetSize(&width, &height);

  view_x *= SCROLL_UNIT;
  view_y *= SCROLL_UNIT;

  if ((point.y < view_y) || (point.y > view_y + height
      - wxSystemSettings::GetMetric(wxSYS_HTHUMB_X) - 20)) {
    sc = true;
    scrollToY = point.y - height / 2;
  } else
    scrollToY = view_y;

  if ((point.x < view_x) || (point.x > view_x + width
      - wxSystemSettings::GetMetric(wxSYS_HTHUMB_X) - 20)) {
    sc = true;
    scrollToX = point.x - width / 2;
  } else
    scrollToX = view_x;

  if (sc)
    Scroll(scrollToX / SCROLL_UNIT, scrollToY / SCROLL_UNIT);
}

void MiniMathCtrl::SelectAll()
{
  m_selectionStart = m_tree;
  m_selectionEnd = m_last;

  Refresh();
}


void MiniMathCtrl::CheckUnixCopy()
{
  bool copy = false;
  wxConfig::Get()->Read(wxT("unixCopy"), &copy);

  if (copy) {
    if (CanCopy() && wxTheClipboard->Open()) {
      wxTheClipboard->SetData(new wxTextDataObject(GetString()));
      wxTheClipboard->Close();
    }
  }
}

bool MiniMathCtrl::IsSelected(int type) {
  if (m_selectionStart == NULL)
    return false;

  else if (type == MC_TYPE_IMAGE || type == MC_TYPE_SLIDE) {
    if (m_selectionStart != m_selectionEnd || m_selectionStart->GetType() != type)
      return false;
    return true;
  }

  else if (m_selectionStart->GetType() != type)
    return false;

  return true;
}


BEGIN_EVENT_TABLE(MiniMathCtrl, wxScrolledWindow)
  EVT_SIZE(MiniMathCtrl::OnSize)
  EVT_PAINT(MiniMathCtrl::OnPaint)
  EVT_LEFT_UP(MiniMathCtrl::OnMouseLeftUp)
  EVT_LEFT_DOWN(MiniMathCtrl::OnMouseLeftDown)
  EVT_MOTION(MiniMathCtrl::OnMouseMotion)
  EVT_ENTER_WINDOW(MiniMathCtrl::OnMouseEnter)
  EVT_LEAVE_WINDOW(MiniMathCtrl::OnMouseExit)
  EVT_ERASE_BACKGROUND(MiniMathCtrl::OnEraseBackground)
END_EVENT_TABLE()
