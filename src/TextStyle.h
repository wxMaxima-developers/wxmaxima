// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2004-2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
//  Copyright (C) 2020 Kuba Ober <kuba@mareimbrium.org>
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

#ifndef TEXTSTYLE_H
#define TEXTSTYLE_H

/*! \file
 * This file declares everything needed for the hierarchical text style system used
 * to style all the elements on the work sheet.
 */

#include <wx/colour.h>
#include <wx/config.h>
#include <wx/font.h>
#include <wx/settings.h>
#include <cstdint>
#include <functional>

/*! An interned font face name, very quick to compare and hash.
 *
 * Any given face name is only stored in memory once, and further comparisons and
 * hashing are based on the pointer to the underlying unique string.
 * There is no reference counting: the interned strings persist until the application
 * exits. This is acceptable, since there is a limited number of fonts available in
 * any system, and the size of the interned face name table will not grow without bounds.
 * The interning mechanism, however, does not have a fixed upper limit to the number of
 * interned strings.
 */
class FontName final
{
public:
  FontName() = default;
  /*! Constructs a font name by interning the name string
   * The constructor is explicit because this requires a string lookup in the intern
   * table. The font names should be constructed once for any given name, and then used
   * instead of the string.
   */
  explicit FontName(const wxString &fontName) : m_fontName(Intern(fontName)) {}
  FontName(const FontName &o) : m_fontName(o.m_fontName) {}
  operator const wxString &() const { return GetAsString(); }
  bool operator==(const FontName &o) const { return m_fontName == o.m_fontName; }
  bool operator<(const FontName &o) const { return m_fontName < o.m_fontName; }
  bool empty() const { return !m_fontName || m_fontName->empty(); }
  FontName &operator=(const FontName &o)
  {
    m_fontName = o.m_fontName;
    return *this;
  }
  FontName &Set(const wxString &str)
  {
    m_fontName = Intern(str);
    return *this;
  }
  const wxString& GetAsString() const { return m_fontName ? *m_fontName : *GetInternedEmpty(); }

private:
  friend struct std::hash<FontName>;
  const wxString *m_fontName = {};

  static const wxString *Intern(const wxString &str);
  static const wxString *GetInternedEmpty();
};

template <> struct std::hash<FontName> final
{
  size_t operator()(FontName name) const
  {
    return std::hash<const void*>()(name.m_fontName);
  }
};

/*! An intrusive tree node, used to manage text Style hierarchy.
 */
template <typename Object>
struct TreeNode
{
  mutable Object *object;
  mutable const TreeNode *up = {}, *down = {}, *left = {}, *right = {};
  void LinkUp(const TreeNode *) const;
  void Unlink() const;
  explicit TreeNode(Object *object) : object(object) {}
  TreeNode(const TreeNode &o) { LinkUp(o.up); }
  TreeNode& operator=(const TreeNode &o) { LinkUp(o.up); return *this; }
  ~TreeNode();
};

/*! A class that carries text styling information.
 *
 * It covers the characteristics of the font as well as other aspects of the style,
 * e.g. its descriptive name and color of the text.
 *
 * The text styles are hierarchical. The default values for the fields are unset,
 * and they forward to the style up in the hierarchy (set by BaseOn).
 * The styles track their dependencies and existence, so that when a style is destructed,
 * the styles that are based on it disconnect, so no dangling pointer access is possible.
 * However, the style hierarchy does not define object ownership, and no memory
 * management is done by the styles themselves - only dangling references are prevented.
 *
 * Each field may be either set to a value, overriding any inherited values, or unset.
 * This is handled by the Set<Field>, IsSet<Field>, and Unset<Field> family of methods.
 * The Get<Field> methods return the first set value encountered as the hierarchy is
 * traversed upwards (to the most base style). If no value is set in the entire hierarchy,
 * a default value is returned. Those defaults are defined within each Get<Field> method.
 *
 * The text styles are also used as keys into the FontCache. They are designed to be quick
 * to compare for equality and order (less-than). When a style is used as a key,
 * it is required to remain constant, and thus the cache Detach()-es it from the style
 * hierarchy, copying all inherited values into the style itself and making it independent
 * of any other styles.
 */
class Style final
{
  using TreeNode = TreeNode<Style>;
public:
  Style() = default;
  Style(double fontSize) { m.fontSize = fontSize; }
  Style(const Style &);
  explicit Style(const wxString &fontName) { m.fontName = ::FontName(fontName); }
  ~Style();

  void DetachFromHierarchy();
  Style &operator=(const Style &);
  bool operator==(const Style &o) const = delete;

  //! Read this style from a config source
  void Read(wxConfigBase *config, const wxString &where);
  //! Write this style to a config source
  void Write(wxConfigBase *config, const wxString &where) const;

  constexpr static wxFontFamily Default_Family = wxFONTFAMILY_MODERN;
  constexpr static wxFontEncoding Default_Encoding = wxFONTENCODING_DEFAULT;
  constexpr static wxFontWeight Default_Weight = wxFONTWEIGHT_NORMAL;
  constexpr static wxFontStyle Default_FontStyle = wxFONTSTYLE_NORMAL;
  constexpr static bool Default_Underlined = false;
  constexpr static bool Default_Strikethrough = false;
  static FontName Default_FontName();
  constexpr static double Default_FontSize = 10.0;
  static inline const wxColor &Default_Color() { return *wxBLACK; }

  bool HasSomeBaseStyle(const Style &) const;
  wxFontFamily GetFamily() const;
  wxFontEncoding GetEncoding() const;
  wxFontWeight GetWeight() const;
  bool IsBold() const { return GetWeight() == wxFONTWEIGHT_BOLD; }
  bool IsLight() const { return GetWeight() == wxFONTWEIGHT_LIGHT; }
  wxFontStyle GetFontStyle() const;
  bool IsItalic() const { return GetFontStyle() == wxFONTSTYLE_ITALIC; }
  bool IsSlant() const { return GetFontStyle() == wxFONTSTYLE_SLANT; }
  bool IsUnderlined() const;
  bool IsStrikethrough() const;
  FontName GetFontName() const;
  double GetFontSize() const;
  const wxColor &GetColor() const;
  const wxString &GetName() const { return m.styleName; }
  const wxString &GetBaseName() const;

  //! Gets the font size of the nearest style up the style hierarchy
  double GetBaseFontSize() const;

  constexpr static wxFontFamily UNSET_Family = wxFONTFAMILY_UNKNOWN;
  constexpr static wxFontEncoding UNSET_Encoding = wxFONTENCODING_DEFAULT;
  constexpr static wxFontWeight UNSET_Weight = wxFONTWEIGHT_INVALID;
  constexpr static wxFontStyle UNSET_FontStyle = wxFontStyle(-1);
  constexpr static FontName UNSET_FontName = {};
  constexpr static double UNSET_FontSize = 0.0;
  static wxColor UNSET_Color;

  bool IsFamilySet() const { return m.family != UNSET_Family; }
  bool IsEncodingSet() const { return m.encoding != UNSET_Encoding; }
  bool IsWeightSet() const { return m.weight != UNSET_Weight; }
  bool IsFontStyleSet() const { return m.fontStyle != UNSET_FontStyle; }
  bool IsUnderlinedSet() const { return m.setUnderlined; }
  bool IsStrikethroughSet() const { return m.setStrikethrough; }
  bool IsFontNameSet() const { return !m.fontName.empty(); }
  bool IsFontSizeSet() const { return m.fontSize > UNSET_FontSize; }
  bool IsFontSelected() const { return IsFontNameSet() || IsFontSizeSet(); }
  bool IsColorSet() const { return m.color.IsOk(); }

  void UnsetFamily() { SetFamily(UNSET_Family); }
  void UnsetEncoding() { SetEncoding(UNSET_Encoding); }
  void UnsetWeight() { SetWeight(UNSET_Weight); }
  void UnsetStyle() { SetFontStyle(UNSET_FontStyle); }
  void UnsetUnderlined() { m.setUnderlined = false; }
  void UnsetStrikethrough() { m.setStrikethrough = false; }
  void UnsetFontName() { SetFontName(UNSET_FontName); }
  void UnsetFontSize() { SetFontSize(UNSET_FontSize); }
  void UnsetFontSelection() { UnsetFontName(); UnsetFontSize(); }
  void UnsetColor() { SetColor(UNSET_Color); }

  using did_change = bool;
  did_change SetFamily(wxFontFamily family);
  did_change SetEncoding(wxFontEncoding encoding);
  did_change SetWeight(int weight);
  did_change SetBold(bool bold = true);
  did_change SetLight(bool light = true);
  did_change SetFontStyle(wxFontStyle style);
  did_change SetItalic(bool italic = true);
  did_change SetSlant(bool slant = true);
  did_change SetUnderlined(bool underlined = true);
  did_change SetStrikethrough(bool strikethrough = true);
  did_change SetFontName(FontName fontName);
  did_change SetFontNameFromFont();
  did_change SetFontSize(double size);
  did_change SetColor(const wxColor &color);
  did_change SetColor(wxSystemColour sysColour);
  did_change SetName(const wxString &styleName);

  Style& BaseOn(const Style &baseStyle) { return LinkBaseStyle(&baseStyle), *this; }
  Style& Family(wxFontFamily family) { return SetFamily(family), *this; }
  Style& Encoding(wxFontEncoding encoding) { return SetEncoding(encoding), *this; }
  Style& Weight(int weight) { return SetWeight(weight), *this; }
  Style& FontStyle(wxFontStyle style) { return SetFontStyle(style), *this; }
  Style& Bold() { return SetBold(), *this; }
  Style& Light() { return SetLight(), *this; }
  Style& Italic() { return SetItalic(), *this; }
  Style& Slant() { return SetSlant(), *this; }
  Style& Underlined() { return SetUnderlined(), *this; }
  Style& Strikethrough() { return SetStrikethrough(), *this; }
  Style& FontName(FontName fontName) { return SetFontName(fontName), *this; }
  Style& FontSize(double size) { return SetFontSize(size), *this; }
  Style& Color(const wxColor &color) { return SetColor(color), *this; }
  Style& Color(uint8_t r, uint8_t g, uint8_t b) { return SetColor({r, g, b}), *this; }
  Style& Color(wxSystemColour sysColour) { return SetColor(sysColour), *this; }
  Style& ChangeLightness(int alpha) { return SetColor(GetColor().ChangeLightness(alpha)), *this; }
  Style& Name(const wxString &name) { return SetName(name), *this; }

  wxFontInfo GetAsFontInfo() const;

  bool IsFontLessThan(const Style &o) const { return GetFontHash() < o.GetFontHash(); }
  bool IsFontEqualTo(const Style &) const;

  bool IsFontOk() const { return GetFont().IsOk(); }
  const wxFont& GetFont() const {
    return (m.fontHash && m.font) ? *m.font : LookupFont();
  }
  const wxFont& GetFontAt(double fontSize) const;
  void SetFromFont(const wxFont&);

  static bool IsFractionalFontSizeSupported();
  static double GetFontSize(const wxFont &);
  static void SetFontSize(wxFont &, double fontSize);

private:
  friend struct StyleFontHasher;
  friend class FontCache;
  void SetFromFontNoCache(const wxFont &);

  struct Data
  {
    wxFontFamily family = UNSET_Family;
    wxFontEncoding encoding = UNSET_Encoding;
    int weight = UNSET_Weight;
    wxFontStyle fontStyle = UNSET_FontStyle;
    class FontName fontName = UNSET_FontName;
    bool underlined : 1, setUnderlined : 1;
    bool strikethrough : 1, setStrikethrough : 1;
    //! Hash of the font family, encoding, weight, style, and name.
    mutable size_t attributeHash = 0;

    mutable double fontSize = UNSET_FontSize;
    //! Hash of the m_attributeHash and font size
    mutable size_t fontHash = 0;
    mutable const wxFont *font = nullptr;

    wxColor color = UNSET_Color;
    wxString styleName;
    Data() : underlined(false), setUnderlined(false),
        strikethrough(false), setStrikethrough(false) {}
  } m;

  TreeNode m_node{this};

  const Style *ResolveStyle(bool (Style::*isSet)() const) const;
  void PropagateAttributeChange(bool (Style::*isSet)() const) const;
  void PropagateFontSizeChange() const;

  size_t GetAttributeHash() const;
  size_t GetSizeHash() const;
  size_t GetFontHash() const;

  void LinkBaseStyle(const Style *up) { if (up) m_node.LinkUp(&up->m_node); }
  const wxFont& LookupFont() const;
};

//! Hash functor of the font size and attributes of the style
struct StyleFontHasher final
{
  size_t operator()(const Style &style) const { return style.GetFontHash(); }
};

//! Less-than-comparator of the font size and attributes of the style
struct StyleFontLess final
{
  bool operator()(const Style &l, const Style &r) const { return l.IsFontLessThan(r); }
};

//! Equals-comparator of the font size and attributes of the style
struct StyleFontEquals final
{
  bool operator()(const Style &l, const Style &r) const { return l.IsFontEqualTo(r); }
};

/*! All text styles known to wxMaxima
 *
 * \attention If this list is changed, the config dialogue
 * sometimes needs additional tweaking after that.
 */
enum TextStyle
{
  TS_DEFAULT             = 0,
  TS_VARIABLE            = 1,
  TS_NUMBER              = 2,
  TS_FUNCTION            = 3,
  TS_SPECIAL_CONSTANT    = 4,
  TS_GREEK_CONSTANT      = 5,
  TS_STRING              = 6,
  TS_INPUT               = 7,
  TS_MAIN_PROMPT         = 8,
  TS_OTHER_PROMPT        = 9,
  TS_LABEL               = 10,
  TS_USERLABEL           = 11,
  TS_HIGHLIGHT           = 12,
  TS_WARNING             = 13,
  TS_ERROR               = 14,
  TS_TEXT                = 15,
  TS_HEADING6            = 16,
  TS_HEADING5            = 17,
  TS_SUBSUBSECTION       = 18,
  TS_SUBSECTION          = 19,
  TS_SECTION             = 20,
  TS_TITLE               = 21,
  TS_TEXT_BACKGROUND     = 22,
  TS_DOCUMENT_BACKGROUND = 23,
  TS_CELL_BRACKET        = 24,
  TS_ACTIVE_CELL_BRACKET = 25,
  TS_CURSOR              = 26,
  TS_SELECTION           = 27,
  TS_EQUALSSELECTION     = 28,
  TS_OUTDATED            = 29,
  TS_CODE_VARIABLE       = 30,
  TS_CODE_FUNCTION       = 31,
  TS_CODE_COMMENT        = 32,
  TS_CODE_NUMBER         = 33,
  TS_CODE_STRING         = 34,
  TS_CODE_OPERATOR       = 35,
  TS_CODE_LISP           = 36,
  TS_CODE_ENDOFLINE      = 37,
  TS_MATH_DEFAULT        = 38,
  TS_CODE_DEFAULT        = 39,
  TS_SYMBOL_DEFAULT      = 40,
  TS_TEX_CMEX            = 41,
  TS_TEX_CMSY            = 42,
  TS_TEX_CMR             = 43,
  TS_TEX_CMMI            = 44,
  TS_TEX_CMTI            = 45,
  NUMBEROFSTYLES //!< This is not a style, but its value tells us how many styles are defined
};

#endif // TEXTSTYLE_H
