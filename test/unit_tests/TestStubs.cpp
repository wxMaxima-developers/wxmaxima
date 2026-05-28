#include "Cell.cpp"
#include "CellImpl.h"
#include "FontVariantCache.h"
#include "CellIterators.h"
#include "CellList.cpp"
#include "CellPtr.cpp"
#include "TextStyle.cpp"
#include "FontVariantCache.cpp"

#if !DISABLE_CELLPOINTER_STUBS
#include "CellPointers.cpp"

CellPointers pointers(nullptr);
CellPointers *Cell::GetCellPointers() const { return &pointers; }
#endif

#if DISABLE_CELLPOINTER_STUBS
CellPointers *Cell::GetCellPointers() const { return {}; }
#endif

bool Configuration::m_debugMode = false;
bool Configuration::m_use_threads = false;
wxColor Configuration::DefaultBackgroundColor() { return *wxWHITE; }

Configuration::Configuration(wxDC *dc, InitOpt) : m_dc(dc) {}
Configuration::~Configuration() {}
bool Configuration::InUpdateRegion(wxRect) const { return true; }
wxCoord Configuration::Scale_Px(double) const { return 1; }
wxSize Configuration::GetPPI() const { return wxSize(72, 72); }
AFontSize Configuration::Scale_Px(AFontSize) const { return AFontSize(10.0); }
wxFontStyle Configuration::IsItalic(long) const { return {}; }
wxColour Configuration::GetColor(TextStyle) { return {}; }
bool Configuration::HideMarkerForThisMessage(wxString) { return false; }
void Configuration::NotifyOfCellRedraw(const Cell *) {}
long Configuration::GetLineWidth() const { return 1; }

// ... (rest of commented out code)

GroupCell::GroupCell(Configuration *config, GroupType groupType, const wxString &) :
  Cell(this, config), m_groupType(groupType)
{
  m_type = MC_TYPE_GROUP;
}
GroupCell::GroupCell(const GroupCell &cell) : GroupCell(cell.m_configuration, cell.m_groupType) {}
GroupCell::GroupCell(GroupCell *WXUNUSED(cell1), const GroupCell &cell) : GroupCell(cell.m_configuration, cell.m_groupType) {}
GroupCell::~GroupCell() {}
wxString GroupCell::ToString() const { return {}; }
bool GroupCell::NeedsRecalculation(AFontSize) const { return {}; }
void GroupCell::Draw(wxDC *, wxDC *) {}
wxRect GroupCell::GetRect(bool) const { return {}; }
bool GroupCell::Recalculate() const { return false; }
void GroupCell::Hide(bool) {}
wxString GroupCell::ToXML() const { return {}; }
wxString GroupCell::ToTeX() const { return {}; }
wxString GroupCell::ToRTF() const { return {}; }
Cell::Range GroupCell::GetInnerCellsInRect(const wxRect &) const { return {}; }
const wxString GroupCell::GetToolTip(wxPoint) const { return {}; }
bool GroupCell::AddEnding() { return {}; }
bool GroupCell::FirstLineOnlyEditor() const { return false; }
void GroupCell::SetCurrentPoint(wxPoint) {}
#if wxUSE_ACCESSIBILITY
wxAccStatus GroupCell::GetDescription(int, wxString *) const { return {}; }
wxAccStatus GroupCell::GetLocation(wxRect &, int) { return {}; }
#endif
DEFINE_CELL(GroupCell)
