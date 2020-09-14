// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2020 Kuba Ober <kuba@mareimbrium.org>
//
//  Everyone is permitted to copy and distribute verbatim copies
//  of this licence document, but changing it is not allowed.
//
//                       WXWINDOWS LIBRARY LICENCE
//     TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
//
//  This library is free software; you can redistribute it and/or modify it
//  under the terms of the GNU Library General Public Licence as published by
//  the Free Software Foundation; either version 2 of the Licence, or (at your
//  option) any later version.
//
//  This library is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public
//  Licence for more details.
//
//  You should have received a copy of the GNU Library General Public Licence
//  along with this software, usually in a file named COPYING.LIB.  If not,
//  write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth
//  Floor, Boston, MA 02110-1301 USA.
//
//  EXCEPTION NOTICE
//
//  1. As a special exception, the copyright holders of this library give
//  permission for additional uses of the text contained in this release of the
//  library as licenced under the wxWindows Library Licence, applying either
//  version 3.1 of the Licence, or (at your option) any later version of the
//  Licence as published by the copyright holders of version 3.1 of the Licence
//  document.
//
//  2. The exception is that you may use, copy, link, modify and distribute
//  under your own terms, binary object code versions of works based on the
//  Library.
//
//  3. If you copy code from files distributed under the terms of the GNU
//  General Public Licence or the GNU Library General Public Licence into a
//  copy of this library, as this licence permits, the exception does not apply
//  to the code that you add in this way.  To avoid misleading anyone as to the
//  status of such modified files, you must delete this exception notice from
//  such code and/or adjust the licensing conditions notice accordingly.
//
//  4. If you write modifications of your own for this library, it is your
//  choice whether to permit this exception to apply to your modifications.  If
//  you do not wish that, you must delete the exception notice from such code
//  and/or adjust the licensing conditions notice accordingly.
//
//  SPDX-License-Identifier: wxWindows

#define CATCH_CONFIG_RUNNER
#include "FontAttribs.cpp"
#include <catch2/catch.hpp>

constexpr auto Size_Unit = 0.05f;
constexpr auto Size_Range = AFontSize::Maximum_Size - AFontSize::Minimum_Size;
constexpr auto Size_Unit_Margin = Size_Unit * (1 + 1e-5);

constexpr auto val1_4 = AFontSize::Minimum_Size + Size_Range * 1 / 4;
constexpr auto val2_4 = AFontSize::Minimum_Size + Size_Range * 2 / 4;
constexpr auto val3_4 = AFontSize::Minimum_Size + Size_Range * 3 / 4;
constexpr auto size1_4 = AFontSize(val1_4);
constexpr auto size2_4 = AFontSize(val2_4);
constexpr auto size3_4 = AFontSize(val3_4);

#define CHECK_NOT_NULL(fontSize) \
  do { \
    THEN("It is not null") { \
      REQUIRE(fontSize.IsValid()); \
      REQUIRE(!fontSize.IsNull()); \
    } \
  } while (0)

#define CHECK_NULL(fontSize) \
  do { \
    THEN("It is null") { \
      REQUIRE(fontSize.IsNull()); \
      REQUIRE(!fontSize.IsValid()); \
      REQUIRE(!fontSize.IsMinimal()); \
    } \
  } while (0)

SCENARIO("AFontSize is null by default") {
  GIVEN("A default-constructed AFontSize") {
    AFontSize fontSize;
    CHECK_NULL(fontSize);
    REQUIRE(0 < AFontSize::Minimum_Size);
    REQUIRE(AFontSize::Minimum_Size < AFontSize::Maximum_Size);
  }
}

#define CHECK_REPRESENTS(size, value) \
  do { \
    auto constexpr wxMargin = wxCHECK_VERSION(3,1,2) ? Size_Unit_Margin : (1 + 1e-5); \
    THEN("It Represents that size") { \
      REQUIRE(size.Get()       == Approx(value).margin(Size_Unit_Margin)); \
      REQUIRE(size.GetAsLong() == Approx(value).margin(1 + 1e-5)); \
      REQUIRE(size.GetForWX()  == Approx(value).margin(wxMargin)); \
    } \
  } while (0)

SCENARIO("AFontSize represents its value") {
  GIVEN("AFontsize constructed to some intermediate size") {
    AFontSize fontSize(val2_4);
    CHECK_REPRESENTS(fontSize, val2_4);
  }
}

#define CHECK_COMPARE(left, right, eqOp, ltOp, gtOp) \
  do { \
    REQUIRE( eqOp AFontSize::Equals()(left, right)); \
    REQUIRE( eqOp(left == right)); \
    REQUIRE(!eqOp(left != right)); \
    REQUIRE( ltOp(left <  right)); \
    REQUIRE( gtOp(left >  right)); \
  } while (0)

SCENARIO("AFontSize compares") {
  CHECK_COMPARE(size1_4, size1_4, !!,  !,  ! );
  CHECK_COMPARE(size1_4, size3_4,  !, !!,  ! );
  CHECK_COMPARE(size3_4, size1_4,  !,  !, !! );
}

SCENARIO("AFontSize is assignable and copy-constructible") {
  AFontSize sizeA;
  sizeA = size1_4;
  CHECK_COMPARE(sizeA, size1_4, !!,  !,  ! );
  CHECK_COMPARE(sizeA, size3_4,  !, !!,  ! );
  CHECK_COMPARE(size3_4, sizeA,  !,  !, !! );
  CHECK_REPRESENTS(sizeA, val1_4);

  AFontSize sizeC(size1_4);
  CHECK_COMPARE(sizeC, size1_4, !!,  !,  ! );
  CHECK_COMPARE(sizeC, size3_4,  !, !!,  ! );
  CHECK_COMPARE(size3_4, sizeC,  !,  !, !! );
  CHECK_REPRESENTS(sizeC, val1_4);
}

#define CHECK_MINMAX(fontSize, limitSize, minimCheck) \
  do { \
    THEN("It's clamped to a minimum or maximum size") { \
      REQUIRE(fontSize.Get() == Approx(AFontSize::limitSize).margin(1e-5)); \
      REQUIRE(minimCheck); \
    } \
    CHECK_NOT_NULL(fontSize); \
    AND_WHEN("It can be subsequently cleared") { \
      fontSize.Clear(); \
      CHECK_NULL(fontSize); \
    } \
  } while (0)

SCENARIO("AFontSize clamps its values")
{
  GIVEN("AFontSize constructed too small") {
    auto fontSize = AFontSize(AFontSize::Minimum_Size / 2);
    CHECK_MINMAX(fontSize, Minimum_Size, fontSize.IsMinimal());
  }

  GIVEN("A default-constructed AFontSize subsequently set too small") {
    AFontSize fontSize;
    fontSize.Set(AFontSize::Minimum_Size / 2);
    CHECK_MINMAX(fontSize, Minimum_Size, fontSize.IsMinimal());
  }

  GIVEN("AFontSize constructed too large") {
    auto fontSize = AFontSize(AFontSize::Maximum_Size * 2);
    CHECK_MINMAX(fontSize, Maximum_Size, !fontSize.IsMinimal());
  }

  GIVEN("A default-constructed AFontSize subsequently set too large") {
    AFontSize fontSize;
    fontSize.Set(AFontSize::Maximum_Size * 2);
    CHECK_MINMAX(fontSize, Maximum_Size, !fontSize.IsMinimal());
  }

  GIVEN("AFontSize constructed too small vs a minimum (from double)") {
    AFontSize fontSize{ size2_4, val1_4 };
    THEN("It is clamped to the given minimum") {
      REQUIRE(fontSize == size2_4);
    }
    CHECK_NOT_NULL(fontSize);
  }
  GIVEN("AFontSize constructed too small vs a minimum (from size)") {
    AFontSize fontSize{ size2_4, size1_4 };
    THEN("It is clamped to the given minimum") {
      REQUIRE(fontSize == size2_4);
    }
    CHECK_NOT_NULL(fontSize);
  }
  GIVEN("AFontSize constructed larger than a minimum (from double)") {
    AFontSize fontSize{ size2_4, val3_4 };
    THEN("It is clamped to the given minimum") {
      REQUIRE(fontSize.Get() == Approx(val3_4).margin(Size_Unit_Margin));
    }
    CHECK_NOT_NULL(fontSize);
  }
  GIVEN("AFontSize constructed larger than a minimum (from size)") {
    AFontSize fontSize{ size2_4, size3_4 };
    THEN("It is clamped to the given minimum") {
      REQUIRE(fontSize == size3_4);
    }
    CHECK_NOT_NULL(fontSize);
  }
}

#define CHECK_EXPR(size, op, operand, descr) \
  do { \
    WHEN(descr) { \
      REQUIRE((size op operand) == Approx(size.Get() op operand).margin(1e-5)); \
      REQUIRE((operand op size) == Approx(operand op size.Get()).margin(1e-5)); \
    } \
  } while(0)

SCENARIO("AFontSize supports double operations") {
  static constexpr auto offset = Size_Range / 3;
  CHECK_EXPR(size2_4, +, offset, "added to");
  CHECK_EXPR(size2_4, -, offset, "subtracted from");
  CHECK_EXPR(size2_4, *, 0.25, "multiplied by");
  CHECK_EXPR(size2_4, /, 3.0, "divided by");
  WHEN("subtracted-and-set") {
    auto size = size3_4;
    size -= offset;
    CHECK_REPRESENTS(size, (val3_4 - offset));
  }
}

SCENARIO("EqualToWithin works") {
  WHEN("comparing a null size to a non-null size") THEN ("they compare unequal")
  {
    REQUIRE_FALSE(EqualToWithin({}, size1_4, {}));
    REQUIRE_FALSE(EqualToWithin({}, size1_4, 0.1f));
  }
  WHEN("comparing a non-null size to a null size") THEN ("they compare unequal")
  {
    REQUIRE_FALSE(EqualToWithin(size1_4, {}, {}));
    REQUIRE_FALSE(EqualToWithin(size1_4, {}, 0.1f));
  }
  WHEN("comparing null sizes") THEN ("they compare equal")
  {
    REQUIRE(EqualToWithin({}, {}, {}));
    REQUIRE(EqualToWithin({}, {}, 0.1f));
  }
  WHEN("comparing smaller to a larger size") THEN("they compare based on the magnitude of difference")
  {
    REQUIRE(EqualToWithin(AFontSize(20.0f), AFontSize(20.1f), 0.2f));
    REQUIRE_FALSE(EqualToWithin(AFontSize(20.0f), AFontSize(20.2f), 0.2f));
  }
  WHEN("comparing larger to a smaller size") THEN("they compare based on the magnitude of difference")
  {
    REQUIRE(EqualToWithin(AFontSize(20.1f), AFontSize(20.0f), 0.2f));
    REQUIRE_FALSE(EqualToWithin(AFontSize(20.2f), AFontSize(20.0f), 0.2f));
  }
}

// If we don't provide our own main when compiling on MinGW
// we currently get an error message that WinMain@16 is missing
// (https://github.com/catchorg/Catch2/issues/1287)
int main(int argc, const char* argv[])
{
    return Catch::Session().run(argc, argv);
}
