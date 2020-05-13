// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//            (C) 2014-2018 Gunter Königsmann <wxMaxima@physikbuch.de>
//            (C) 2020 Kuba Ober <kuba@mareimbrium.org>
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

/*!
 * \file This file implements the hierarchical text style system.
 */

#include "TextStyle.h"
#include "FontCache.h"
#include <wx/colour.h>
#include <wx/hashmap.h>
#include <wx/thread.h>
#include <wx/translation.h>
#include <array>

wxColor Style::UNSET_Color = wxNullColour;

/*! \brief Mixes two hashes together.
 *
 * Used to obtain hashes of composite data types, such as structures.
*/
template <typename T>
static size_t MixHash(size_t seed, const T &value)
{
  std::hash<T> const hasher;
  seed ^= hasher(value) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
  return seed;
}

/*! \brief An implementation of string interning arena.
 */
template <typename T>
class Interner
{
  struct Slice {
    size_t nextIndex = 0;
    std::array<T, 128> data;
    T* GetNext() {
      if (nextIndex >= data.size()) return nullptr;
      return &data[nextIndex++];
    }
  };

  std::vector<std::unique_ptr<Slice>> m_storage;
  std::vector<const T*> m_index;
public:
  bool IsInterned(const T *value) const
  {
    for (auto const &slice : m_storage)
    {
      auto const data = slice->data;
      auto const nextIndex = slice->nextIndex;
      if (nextIndex)
      {
        auto *const first = &(data[0]);
        auto *const last = &(data[nextIndex-1]);
        if (value >= first && value <= last)
          return true;
      }
    }
    return false;
  }
  const T* Intern(const T *value) {
    if (!value) return nullptr;
    return &Intern(*value);
  }
  const T* Intern(const T &value)
  {
    // pointer equality: is the value within one of the slices?
    if (IsInterned(&value))
      return &value;
    // value equality: is the value equal to one already interned?
    auto indexPos = std::lower_bound(m_index.begin(), m_index.end(), value,
                                     [](auto *a, auto const &b){ return *a < b; });
    if (indexPos != m_index.end() && **indexPos == value)
      return *indexPos;
    // none of the above: we must intern
    if (m_storage.empty()) m_storage.emplace_back(new Slice());
    T *loc = m_storage.back()->GetNext();
    if (!loc)
    {
      std::unique_ptr<Slice> slice{new Slice()};
      loc = slice->GetNext();
      m_storage.emplace_back(std::move(slice));
    }
    *loc = value;
    indexPos = m_index.insert(indexPos, loc);
    wxASSERT(*indexPos);
    return *indexPos;
  }
};

/*! \brief Syncrhonizes access to the font face string interner.
 *
 * This is only active on Windows. The class is empty on non-windows systems, where
 * access to the font objects is only allowed from the main thread.
 */
struct InternerLock
{
#ifdef __WINDOWS__
  // Windows allows font access from multiple threads, as long as each font
  // is built separately. We must thus synchronize the access to the interner.
  static wxMutex mutex;
  InternerLock() { mutex.Lock(); }
  ~InternerLock() { mutex.Unlock(); }
  InternerLock(const InternerLock &) = delete;
  void operator=(const InternerLock &) = delete;
#endif
};

#ifdef __WINDOWS__
//! The mutex is only for Windows, where fonts can be accessed from multiple threads.
//! On other systems, this is not allowed.
wxMutex InternerLock::mutex;
#endif

static Interner<wxString>& GetFaceInterner()
{
  static Interner<wxString> interner;
  return interner;
}

const wxString *FontName::Intern(const wxString &str)
{
  // cppcheck-suppress unusedVariable
  InternerLock lock;
  return GetFaceInterner().Intern(str);
}

const wxString *FontName::GetInternedEmpty()
{
  static const wxString *internedEmpty = FontName::Intern(wxEmptyString);
  return internedEmpty;
}

/*
 * TreeNode
 */

/*! \brief Checks the internal consistency of a given TreeNode.
 *
 * Checks are done in the debug mode only. Does nothing in release mode.
 */
template <typename T>
static void CheckOne(const TreeNode<T> *n)
{
  if (!n) return;
  wxASSERT(n->up || (!n->left && !n->right));
  wxASSERT(!n->up || (!n->left == (n->up->down == n)));
  wxASSERT(!n->left || (n->left->right == n && n->left->up == n->up));
  wxASSERT(!n->right || (n->right->left == n && n->right->up == n->up));
}

/*! \brief Checks the internal consistency of a given TreeNode and its neighbors.
 *
 * Checks are done in the debug mode only. Does nothing in release mode.
 */
template <typename T>
static void Check(const TreeNode<T> *n)
{
  if (!n) return;
  CheckOne(n);
  CheckOne(n->up);
  CheckOne(n->left);
  CheckOne(n->right);
}

/*! \brief Links this node to a given immediate base TreeNode.
 *
 * The node is unlinked first if necessary. \arg u can be null.
 */
template <typename T>
void TreeNode<T>::LinkUp(const TreeNode *u) const
{
  Unlink();
  if (!u) return;
  up = u;
  right = up->down;
  if (right) right->left = this;
  up->down = this;
  Check(this);
}

//! Unlinks the node from all other nodes.
template <typename T>
void TreeNode<T>::Unlink() const
{
  Check(this);
  if (!up) return;
  if (left) left->right = right;
  if (right) right->left = left;
  if (up->down == this) up->down = right;
  Check(up);
  Check(left);
  Check(right);
  up = left = right = nullptr;
  Check(this);
}

//! Re-bases downstream nodes onto the base node of this node (if any), then unlinks the node.
template <typename T>
TreeNode<T>::~TreeNode()
{
  Check(this);
  auto *down = this->down;
  while (down) {
    auto *next = down->right;
    down->LinkUp(up);
    down = next;
  }
  Unlink();
}

/*
 * Style
 */

Style::Style(const Style &o) : m(o.m), m_node(o.m_node) {}

Style::~Style() {}

void Style::DetachFromHierarchy()
{
  SetFamily(GetFamily());
  SetEncoding(GetEncoding());
  SetWeight(GetWeight());
  SetFontStyle(GetFontStyle());
  SetFontName(GetFontName());
  SetFontSize(GetFontSize());
  SetColor(GetColor());
  m_node.Unlink();
}

Style &Style::operator=(const Style &o)
{
  if (&o != this)
  {
    m = o.m;
    m_node = o.m_node;
  }
  return *this;
}

/*! Traverse the hierarchy toward base styles and return the first tyle that
 * satisfies a given predicate, or null if none found.
 */
const Style *Style::ResolveStyle(bool (Style::*isSet)() const) const
{
  const Style *style = this;
  while (style->m_node.up && !(style->*isSet)())
    style = style->m_node.up->object;
  bool const set = (style->*isSet)();
  return set ? style : nullptr;
}

/*! Traverse the tree DFS-order toward derived styles, resetting the attribute and
 * font hashes, terminating each descent traversal once the predicate becomes true.
 */
void Style::PropagateAttributeChange(bool (Style::*isSet)() const) const
{
  m.attributeHash = m.fontHash = 0;
  const TreeNode *left = m_node.down;
  while (left)
  {
    auto *style = left->object;
    if (!(style->*isSet)()) style->PropagateAttributeChange(isSet);
    left = left->right;
  }
}

/*! Traverse the tree DFS-order toward derived styles, resetting the font hash,
 * terminating each descent traversal on a style that has its font size set (overriden).
 */
void Style::PropagateFontSizeChange() const
{
  m.fontHash = 0;
  const TreeNode *left = m_node.down;
  while (left)
  {
    auto *style = left->object;
    if (!style->IsFontSizeSet()) style->PropagateFontSizeChange();
    left = left->right;
  }
}

bool Style::HasSomeBaseStyle(const Style &baseStyle) const
{
  auto *style = this;
  while (style)
  {
    if (style == &baseStyle) return true;
    style = style->m_node.up->object;
  }
  return false;
}

wxFontFamily Style::GetFamily() const
{
  auto *style = ResolveStyle(&Style::IsFamilySet);
  return style ? style->m.family : Default_Family;
}

wxFontEncoding Style::GetEncoding() const
{
  auto *style = ResolveStyle(&Style::IsEncodingSet);
  return style ? style->m.encoding : Default_Encoding;
}

wxFontWeight Style::GetWeight() const
{
  auto *style = ResolveStyle(&Style::IsWeightSet);
  return style ? wxFontWeight(style->m.weight) : Default_Weight;
}

wxFontStyle Style::GetFontStyle() const
{
  auto *style = ResolveStyle(&Style::IsFontStyleSet);
  return style ? style->m.fontStyle : Default_FontStyle;
}

bool Style::IsUnderlined() const
{
  auto *style = ResolveStyle(&Style::IsUnderlinedSet);
  return style ? style->m.underlined : Default_Underlined;
}

bool Style::IsStrikethrough() const
{
  auto *style = ResolveStyle(&Style::IsStrikethroughSet);
  return style ? style->m.strikethrough : Default_Strikethrough;
}

FontName Style::GetFontName() const
{
  auto *style = ResolveStyle(&Style::IsFontNameSet);
  return style ? style->m.fontName : Default_FontName();
}

double Style::GetFontSize() const
{
  auto *style = ResolveStyle(&Style::IsFontSizeSet);
  auto fontSize = style ? style->m.fontSize : Default_FontSize;
  wxASSERT(fontSize > 0);
  return fontSize;
}

const wxColor &Style::GetColor() const
{
  auto *style = ResolveStyle(&Style::IsColorSet);
  return style ? style->m.color : Default_Color();
}

const wxString &Style::GetBaseName() const
{
  static wxString empty;
  if (!m_node.up) return empty;
  auto *baseStyle = m_node.up->object;
  return baseStyle->m.styleName;
}

double Style::GetBaseFontSize() const
{
  auto *style = this;
  if (m_node.up)
    style = m_node.up->object->ResolveStyle(&Style::IsFontSizeSet);
  auto fontSize = style ? style->m.fontSize : Default_FontSize;
  wxASSERT(fontSize > 0);
  return fontSize;
}

using did_change = Style::did_change;

did_change Style::SetFamily(wxFontFamily family)
{
  if (m.family == family) return false;
  m.family = family;
  PropagateAttributeChange(&Style::IsFamilySet);
  return true;
}

did_change Style::SetEncoding(wxFontEncoding encoding)
{
  if (m.encoding == encoding) return false;
  m.encoding = encoding;
  PropagateAttributeChange(&Style::IsEncodingSet);
  return true;
}

did_change Style::SetFontStyle(wxFontStyle fontStyle)
{
  if (m.fontStyle == fontStyle) return false;
  m.fontStyle = fontStyle;
  PropagateAttributeChange(&Style::IsFontStyleSet);
  return true;
}

did_change Style::SetWeight(int weight)
{
  if (m.weight == weight) return false;
  m.weight = weight;
  PropagateAttributeChange(&Style::IsWeightSet);
  return true;
}

did_change Style::SetBold(bool bold)
{
  return SetWeight(bold ? wxFONTWEIGHT_BOLD : wxFONTWEIGHT_NORMAL);
}

did_change Style::SetLight(bool light)
{
  return SetWeight(light ? wxFONTWEIGHT_LIGHT : wxFONTWEIGHT_NORMAL);
}

did_change Style::SetItalic(bool italic)
{
  return SetFontStyle(italic ? wxFONTSTYLE_ITALIC : wxFONTSTYLE_NORMAL);
}

did_change Style::SetSlant(bool slant)
{
  return SetFontStyle(slant ? wxFONTSTYLE_SLANT : wxFONTSTYLE_NORMAL);
}

did_change Style::SetUnderlined(bool underlined)
{
  if (m.setUnderlined && m.underlined == underlined) return false;
  m.underlined = underlined;
  m.setUnderlined = true;
  PropagateAttributeChange(&Style::IsUnderlinedSet);
  return true;
}

did_change Style::SetStrikethrough(bool strikethrough)
{
  if (m.setStrikethrough && m.strikethrough == strikethrough) return false;
  m.strikethrough = strikethrough;
  m.setStrikethrough = true;
  PropagateAttributeChange(&Style::IsStrikethroughSet);
  return true;
}

did_change Style::SetFontName(::FontName faceName)
{
  if (m.fontName == faceName) return false;
  m.fontName = faceName;
  PropagateAttributeChange(&Style::IsFontNameSet);
  return true;
}

// cppcheck-suppress unusedFunction
did_change Style::SetFontNameFromFont()
{
  return SetFontName(::FontName(GetFont().GetFaceName()));
}

did_change Style::SetFontSize(double size)
{
  if (m.fontSize == size) return false;
  m.fontSize = size;
  PropagateFontSizeChange();
  return true;
}

did_change Style::SetColor(const wxColor &color)
{
  if (color == m.color) return false;
  m.color = color;
  return true;
}

did_change Style::SetColor(wxSystemColour sysColour)
{
  return SetColor(wxSystemSettings::GetColour(sysColour));
}

did_change Style::SetName(const wxString &styleName)
{
  if (styleName == m.styleName) return false;
  m.styleName = styleName;
  return true;
}

size_t Style::GetAttributeHash() const
{
  size_t hash_ = m.attributeHash;
  if (!hash_)
  {
    hash_ = MixHash(hash_, m.family);
    hash_ = MixHash(hash_, m.encoding);
    hash_ = MixHash(hash_, m.weight);
    hash_ = MixHash(hash_, m.setUnderlined << 0 | m.underlined << 1 | m.setStrikethrough << 2 | m.strikethrough << 3);
    hash_ = MixHash(hash_, m.fontName);
    if (!hash_) hash_++;
    m.attributeHash = hash_;
  }
  return hash_;
}

size_t Style::GetSizeHash() const
{
  size_t hash_ = 0;
  hash_ = MixHash(hash_, m.fontSize);
  if (!hash_) hash_++;
  return hash_;
}

size_t Style::GetFontHash() const
{
  size_t hash_ = m.fontHash;
  if (!hash_)
  {
    hash_ = MixHash(GetAttributeHash(), GetSizeHash());
    if (!hash_) hash_++;
    m.fontHash = hash_;
  }
  return hash_;
}

const wxFont& Style::LookupFont() const
{
  GetFontHash();
  m.font = &FontCache::GetAFont(*this);
  wxASSERT(m.font);
  wxASSERT(!GetFontName().empty());
  return *m.font;
}

const wxFont& Style::GetFontAt(double fontSize) const
{
  if (fontSize == GetFontSize()) return GetFont();
  auto prevFontHash = m.fontHash;
  double prevFontSize = m.fontSize;
  m.fontHash = 0;
  m.fontSize = fontSize;
  auto &font = LookupFont();
  m.fontHash = prevFontHash;
  m.fontSize = prevFontSize;
  return font;
}

/*! \brief Unlinks the style from the hierarchy and sets it from the given font.
 *
 * The non-font attributes get set locally to their current values (when inherited).
 */
void Style::SetFromFont(const wxFont& font)
{
  if (font.IsOk())
    FontCache::AddAFont(font);
  SetFromFontNoCache(font);
}

void Style::SetFromFontNoCache(const wxFont &font)
{
  m.color = GetColor();
  m_node.Unlink();
  m.attributeHash = m.fontHash = 0;
  m.encoding = font.GetEncoding();
  m.family = font.GetFamily();
  m.fontStyle = font.GetStyle();
  m.setUnderlined = true;
  m.underlined = font.GetUnderlined();
  m.setStrikethrough = true;
  m.strikethrough = font.GetStrikethrough();
  m.weight = font.GetNumericWeight();
  m.fontName = ::FontName(font.GetFaceName());
  m.fontSize = GetFontSize(font);
}

bool Style::IsFractionalFontSizeSupported()
{
  return wxCHECK_VERSION(3,1,2);
}

double Style::GetFontSize(const wxFont &font)
{
#if wxCHECK_VERSION(3,1,2)
  return font.GetFractionalPointSize();
#endif
  return font.GetPointSize();
}

void Style::SetFontSize(wxFont &font, double fontSize)
{
#if wxCHECK_VERSION(3,1,2)
  return font.SetFractionalPointSize(fontSize);
#endif
  return font.SetPointSize(fontSize);
}

wxFontInfo Style::GetAsFontInfo() const
{
  wxFontInfo result;

  result
    .Family(GetFamily())
    .FaceName(GetFontName())
    .Underlined(IsUnderlined())
    .Strikethrough(IsStrikethrough())
    .Encoding(GetEncoding())
    ;

  // This pattern is used to ensure that the legacy variant
  // still compiles (doesn't bitrot).
#if wxCHECK_VERSION(3,1,2)
  return result
    .Style(GetFontStyle())
    .Weight(GetWeight())
    ;
#endif
  return result
    .Slant(IsSlant())
    .Italic(IsItalic())
    .Bold(IsBold())
    .Light(IsLight())
    ;
}

FontName Style::Default_FontName()
{
#if defined(__WXOSX_MAC__)
  static auto fontName = ::FontName(wxT("Monaco"));
#elif defined(__WINDOWS__)
  static auto fontName = ::FontName(wxT("Linux Libertine O"));
#else
  static auto fontName = ::FontName(wxT("Arial"));
#endif
  return fontName;
}

static const wxString k_color = wxT("%s/color");
static const wxString k_bold = wxT("%s/bold");
static const wxString k_light = wxT("%s/light");
static const wxString k_italic = wxT("%s/italic");
static const wxString k_slant = wxT("%s/slant");
static const wxString k_underlined = wxT("%s/underlined");
static const wxString k_strikethrough = wxT("%s/strikethrough");
static const wxString k_fontsize = wxT("%s/Style/Text/fontsize");
static const wxString k_fontname = wxT("%s/Style/Text/fontname");

void Style::Read(wxConfigBase *config, const wxString &where)
{
  wxString tmpStr;
  bool tmpBool;
  long tmpLong;

  // Unset all fields, but do not change the style dependency hierarchy
  // nor the name of the style.
  auto styleName = std::move(m.styleName);
  m = {};
  m.styleName = std::move(styleName);

  if (config->Read(wxString::Format(k_color, where), &tmpStr))
  {
    wxColor color = wxColor(tmpStr);
    if (color.IsOk()) SetColor(color);
  }
  if (config->Read(wxString::Format(k_bold, where), &tmpBool)) SetBold(tmpBool);
  if (config->Read(wxString::Format(k_light, where), &tmpBool)) SetLight(tmpBool);
  if (config->Read(wxString::Format(k_italic, where), &tmpBool)) SetItalic(tmpBool);
  if (config->Read(wxString::Format(k_slant, where), &tmpBool)) SetSlant(tmpBool);
  if (config->Read(wxString::Format(k_underlined, where), &tmpBool)) SetUnderlined(tmpBool);
  if (config->Read(wxString::Format(k_strikethrough, where), &tmpBool)) SetStrikethrough(tmpBool);
  if (config->Read(wxString::Format(k_fontsize, where), &tmpLong))
    SetFontSize(tmpLong);
  if (config->Read(wxString::Format(k_fontname, where), &tmpStr))
    SetFontName(::FontName(tmpStr));

  // We can't perform further validation of the style here, since styles form
  // dependency hierarchies. Any validation is deferred to the point of first use, etc.
}

void Style::Write(wxConfigBase *config, const wxString &where) const
{
  config->Write(wxString::Format(k_color, where), GetColor().GetAsString());
  config->Write(wxString::Format(k_bold, where), IsBold());
  config->Write(wxString::Format(k_light, where), IsLight());
  config->Write(wxString::Format(k_italic, where), IsItalic());
  config->Write(wxString::Format(k_slant, where), IsSlant());
  config->Write(wxString::Format(k_underlined, where), IsUnderlined());
  config->Write(wxString::Format(k_strikethrough, where), IsStrikethrough());
  config->Write(wxString::Format(k_fontsize, where), long(GetFontSize()));
  config->Write(wxString::Format(k_fontname, where), GetFontName().GetAsString());
}
