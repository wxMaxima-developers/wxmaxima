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

Configuration::Configuration(wxDC *dc, InitOpt) : m_dc(dc) {}
Configuration::~Configuration() {}
bool Configuration::InUpdateRegion(wxRect) const { return true; }
long Configuration::Scale_Px(double) const { return 1; }
wxSize Configuration::GetPPI() const { return wxSize(72, 72); }
AFontSize Configuration::Scale_Px(AFontSize) const { return AFontSize(10.0); }
wxFontStyle Configuration::IsItalic(long) const { return {}; }
wxColour Configuration::GetColor(TextStyle) { return {}; }
bool Configuration::HideMarkerForThisMessage(wxString) { return false; }
void Configuration::NotifyOfCellRedraw(const Cell *) {}

//Style::Style(){}
//Style::Style(Style const &){}
//bool Style::IsBold() const {return true;}
//wxFontWeight Style::GetWeight() const {return wxFONTWEIGHT_BOLD;}
//bool Style::IsItalic() const {return true;}
//wxFontStyle  Style::GetFontStyle() const {return wxFONTSTYLE_ITALIC;}

//Style::did_change Style::SetFontName(wxString name){return true;}
// // std::shared_ptr<wxFont> FontVariantCache::GetFont (double size,
// // 						   bool isItalic,
// // 						   bool isBold,
// // 						   bool isUnderlined){
// //   return std::shared_ptr<wxFont>(new wxFont(*wxNORMAL_FONT));
// // }

// //std::shared_ptr<FontVariantCache> FontVariantCache(wxString fontName){
//   //  return std::shared_ptr<FontVariantCache>(new FontVariantCache(wxT("")));
// //}

// // Style Configuration::GetStyle(TextStyle, AFontSize) const
// // {
// //   Style style;  
// //   return style;  
// }
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
void GroupCell::Draw(wxPoint) {}
wxRect GroupCell::GetRect(bool) const { return {}; }
void GroupCell::Recalculate() {}
void GroupCell::Hide(bool) {}
wxString GroupCell::ToXML() const { return {}; }
wxString GroupCell::ToTeX() const { return {}; }
wxString GroupCell::ToRTF() const { return {}; }
Cell::Range GroupCell::GetInnerCellsInRect(const wxRect &) const { return {}; }
const wxString &GroupCell::GetToolTip(wxPoint) const { return wxm::emptyString; }
bool GroupCell::AddEnding() { return {}; }
#if wxUSE_ACCESSIBILITY
wxAccStatus GroupCell::GetDescription(int, wxString *) const { return {}; }
wxAccStatus GroupCell::GetLocation(wxRect &, int) { return {}; }
#endif
DEFINE_CELL(GroupCell)
