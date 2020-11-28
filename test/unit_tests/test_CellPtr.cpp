// -*- mode: c++; c-file-style: "linux"; c-basic-offset: 2; indent-tabs-mode: nil -*-
//
//  Copyright (C) 2020      Kuba Ober <kuba@bertec.com>
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

#define CATCH_CONFIG_RUNNER
#define CELLPTR_COUNT_INSTANCES 1
#include "Cell.cpp"
#include "CellImpl.h"
#include "CellIterators.h"
#include "CellPtr.cpp"
#include "FontAttribs.cpp"
#include "StringUtils.cpp"
#include <catch2/catch.hpp>
#include <stx/optional.hpp>
#include <array>

AFontSize Style::GetFontSize() const { return {}; }
AFontName Style::Default_FontName() { return {}; }
constexpr const AFontSize Style::Default_FontSize;
AFontSize Configuration::Scale_Px(AFontSize) const { return {}; }
wxColour Configuration::GetColor(TextStyle) { return {}; }
void Configuration::NotifyOfCellRedraw(const Cell *) {}
bool Configuration::HideMarkerForThisMessage(wxString) {return false;}
void CellListBuilderBase::base_Append(std::unique_ptr<Cell> &&) {}
void CellList::DeleteList(Cell *) {}
bool Configuration::InUpdateRegion(wxRect) const { return true; }
Configuration::Configuration(wxDC *, Configuration::InitOpt) : m_dc{} {}
Configuration::~Configuration() {}

class TestCell : public Observed {};

SCENARIO("Observed lifetimes are tracked") {
  GIVEN("no Observed objects") {
    REQUIRE(Observed::GetLiveInstanceCount() == 0);
    WHEN("one Observed added") {
      TestCell obs1;
      THEN("its presence is recorded")
      REQUIRE(Observed::GetLiveInstanceCount() == 1);

      AND_WHEN("second Observed is added") {
        TestCell obs2;
        THEN("its presence is recorded")
        REQUIRE(Observed::GetLiveInstanceCount() == 2);
      }

      AND_WHEN("second Observed has been added and removed") {
        { TestCell obs2; }
        THEN("its absence is recorded")
        REQUIRE(Observed::GetLiveInstanceCount() == 1);
      }
    }

    WHEN("two Observeds have been added and removed") {
      { TestCell obs1, obs2; }
      THEN("their absence is recorded")
      REQUIRE(Observed::GetLiveInstanceCount() == 0);
    }
  }
}

SCENARIO("CellPtr lifetimes are tracked") {
  GIVEN("no CellPtr objects") {
    REQUIRE(CellPtrBase::GetLiveInstanceCount() == 0);
    WHEN("one CellPtr is added") {
      CellPtr<TestCell> ptr1;
      THEN("its presence is recorded")
      REQUIRE(CellPtrBase::GetLiveInstanceCount() == 1);

      WHEN("second CellPtr is added") {
        CellPtr<TestCell> ptr2;
        THEN("its presence is recorded")
        REQUIRE(CellPtrBase::GetLiveInstanceCount() == 2);
      }

      WHEN("second CellPtr has been added and removed") {
        { CellPtr<TestCell> ptr2; }
        THEN("its absence is recorded")
        REQUIRE(CellPtrBase::GetLiveInstanceCount() == 1);
      }
    }
    WHEN("two CellPtrs have been added and removed") {
      { CellPtr<Cell> ptr1, ptr2; }
      THEN("their absence is recorded")
      REQUIRE(CellPtrBase::GetLiveInstanceCount() == 0);
    }
  }
}

//! Tests that a commutative binary operator returns true both ways
#define REQUIRE_COMM_OP_T(left, op, right) \
  do { \
    REQUIRE(left op right); \
    REQUIRE(right op left); \
  } while(0)

//! Tests that a commutative binary operator returns false both ways
#define REQUIRE_COMM_OP_F(left, op, right) \
  do { \
    REQUIRE_FALSE(left op right); \
    REQUIRE_FALSE(right op left); \
  } while(0)

//! Tests that arguments compare equal and not unequal both ways
#define REQUIRE_COMM_OP_EQ(left, right) \
  do { \
    REQUIRE_COMM_OP_T(left, ==, right); \
    REQUIRE_COMM_OP_F(left, !=, right); \
  } while (0)

//! Tests that arguments compare unequal and not equal both ways
#define REQUIRE_COMM_OP_NEQ(left, right) \
  do { \
    REQUIRE_COMM_OP_T(left, !=, right); \
    REQUIRE_COMM_OP_F(left, ==, right); \
  } while (0)

//! Tests that an operator returns true in given operand order, but is not commutative
#define REQUIRE_NONCOMM_OP_T(left, op, right) \
  do { \
    REQUIRE(left op right); \
    REQUIRE_FALSE(right op left); \
  } while (0)

//! Tests that an operator returns false in given operand order, but is not commutative
#define REQUIRE_NONCOMM_OP_F(left, op, right) \
  do { \
    REQUIRE_FALSE(left op right); \
    REQUIRE(right op left); \
  } while (0)

SCENARIO("One CellPtr tracks Observed") {
  GIVEN("A CellPtr<Cell>") {
    CellPtr<TestCell> ptr;

    WHEN("CellPtr is default-constructed") {
      THEN("it points to no object") REQUIRE(ptr.get() == nullptr);
      AND_THEN("it is false in bool context") REQUIRE(bool(ptr) == false);
      AND_THEN("it compares equal to a nullptr") REQUIRE_COMM_OP_EQ(ptr, nullptr);
      AND_THEN("it compares equal to CellPtr<Cell>()") {
        REQUIRE_COMM_OP_EQ(ptr, CellPtr<TestCell>());
        CellPtr<TestCell> other;
        REQUIRE_COMM_OP_EQ(ptr, other);
      }
      AND_THEN("it compares equal to a null Cell pointer") {
        REQUIRE_COMM_OP_EQ(ptr, static_cast<TestCell*>(0));
      }
      AND_THEN("it compares equal to a null Observed pointer") {
        REQUIRE_COMM_OP_EQ(ptr, static_cast<Observed*>(0));
      }
    }

    WHEN("CellPtr is assigned an object") {
      stx::optional<TestCell> obs1;
      obs1.emplace();
      ptr = &*obs1;

      THEN("it is true in bool context") REQUIRE(bool(ptr) == true);
      AND_THEN("get() points to that object") REQUIRE(ptr.get() == &*obs1);
      AND_THEN("it compares equal to a Cell* to that object") {
        REQUIRE_COMM_OP_EQ(ptr, static_cast<TestCell*>(&*obs1));
      }
      AND_THEN("it compares equal to an Observed* to that object") {
        REQUIRE_COMM_OP_EQ(ptr, static_cast<Observed*>(&*obs1));
      }
      AND_THEN("it compares equal to CellPtr<Cell>(of that object)") {
        REQUIRE_COMM_OP_EQ(ptr, CellPtr<TestCell>(&*obs1));
        CellPtr<TestCell> other(&*obs1);
        REQUIRE_COMM_OP_EQ(ptr, other);
      }
      AND_THEN("it compares unequal to a nullptr") {
        REQUIRE_COMM_OP_NEQ(ptr, nullptr);
      }
      AND_THEN("it compares unequal to CellPtr<Cell>()") {
        REQUIRE_COMM_OP_NEQ(ptr, CellPtr<TestCell>());
        CellPtr<Cell> other;
        REQUIRE_COMM_OP_NEQ(ptr, other);
      }
      AND_THEN("it compares unequal to a null Cell pointer") {
        REQUIRE_COMM_OP_NEQ(ptr, static_cast<TestCell*>(0));
      }
      AND_THEN("it compares unequal to a null Observed pointer") {
        REQUIRE_COMM_OP_NEQ(ptr, static_cast<Observed*>(0));
      }

      AND_WHEN("CellPtr is assigned another object") {
        stx::optional<TestCell> obs2;
        obs2.emplace();
        ptr = &*obs2;
        THEN("it points to that object") REQUIRE(ptr.get() == &*obs2);
        AND_THEN("it is true in bool context") REQUIRE(bool(ptr) == true);

        AND_WHEN("that object dies") {
          obs2.reset();
          THEN("CellPtr points to no object") REQUIRE(ptr.get() == nullptr);
          AND_THEN("it is false in bool context") REQUIRE(bool(ptr) == false);

          AND_WHEN("Cellptr is assigned the first object") {
            ptr = &*obs1;
            THEN("it points to that object") REQUIRE(ptr.get() == &*obs1);
            AND_THEN("it is true in bool context") REQUIRE(bool(ptr) == true);

            AND_WHEN("that object dies") {
              obs1.reset();
              THEN("CellPtr points to no object") REQUIRE(ptr.get() == nullptr);
              AND_THEN("it is false in bool context") REQUIRE(bool(ptr) == false);
            }
          }
        }
      }
    }
  }
}

SCENARIO("A pair of CellPtrs track a pair of Observeds without interference") {
  GIVEN("A pair of CellPtr<Cell>s and a pair of Cells") {
    std::array<CellPtr<TestCell>, 2> ptr;
    std::array<stx::optional<TestCell>, 2> obs;

    obs[0].emplace();
    obs[1].emplace();

    WHEN("each CellPtr is assigned a different object") {
      ptr[0] = &*(obs[0]);
      ptr[1] = &*(obs[1]);
      THEN("it points to the assigned object") {
        REQUIRE(ptr[0].get() == &*obs[0]);
        REQUIRE(ptr[1].get() == &*obs[1]);
      }
      THEN("the less-than operator is noncommutative")
      { REQUIRE_NONCOMM_OP_T(ptr[0], <, ptr[1]); }

      THEN("one pointer compares less accordingly to the Observed* of the other object") {
        REQUIRE_NONCOMM_OP_T(ptr[0], <, static_cast<Observed*>(&*obs[1]));
        REQUIRE_NONCOMM_OP_F(ptr[1], <, static_cast<Observed*>(&*obs[0]));
      }

      THEN("one pointer compares less accordingly to the Cell* of the other object") {
        REQUIRE_NONCOMM_OP_T(ptr[0], <, static_cast<TestCell*>(&*obs[1]));
        REQUIRE_NONCOMM_OP_F(ptr[1], <, static_cast<TestCell*>(&*obs[0]));
      }

      WHEN("we check again") {
        THEN("the CellPtrs still point to those objects") {
          REQUIRE(ptr[0].get() == &*obs[0]);
          REQUIRE(ptr[1].get() == &*obs[1]);
        }

        WHEN("one object dies") {
          obs[0].reset();

          THEN("the CellPtr that was set to it now points to no object")
          { REQUIRE(ptr[0].get() == nullptr); }

          AND_THEN("the other CellPtr still points to its object")
          { REQUIRE(ptr[1].get() == &*obs[1]); }

          THEN("the null pointer compares less than the non-null one")
          { REQUIRE_NONCOMM_OP_T(ptr[0], <, ptr[1]); }

          THEN("the null pointer compares less than the Observed* non-null one") {
            REQUIRE_NONCOMM_OP_T(ptr[0], <, static_cast<Observed*>(ptr[1].get()));
            REQUIRE_NONCOMM_OP_F(ptr[1], <, static_cast<Observed*>(ptr[0].get()));
          }

          THEN("the null pointer compares less than the Cell* non-null one") {
            REQUIRE_NONCOMM_OP_T(ptr[0], <, static_cast<TestCell*>(ptr[1].get()));
            REQUIRE_NONCOMM_OP_F(ptr[1], <, static_cast<TestCell*>(ptr[0].get()));
          }

          WHEN("the other object dies") {
            obs[1].reset();

            THEN("the Cellptr that was set to it now points to no object")
            { REQUIRE(ptr[1].get() == nullptr); }

            AND_THEN("the other CellPtr still points to no object")
            { REQUIRE(ptr[0].get() == nullptr); }

            THEN("the pointers are never less than each other")
            { REQUIRE_COMM_OP_F(ptr[0], <, ptr[1]); }

            WHEN("the objects are recreated at the same address") {
              obs[0].emplace();
              obs[1].emplace();

              THEN("the CellPtrs still point to no object") {
                REQUIRE(ptr[0].get() == nullptr);
                REQUIRE(ptr[1].get() == nullptr);
              }
            }
          }
        }
      }
    }

    WHEN("both CellPtr are assigned to the same object") {
      ptr[0] = &*obs[0];
      ptr[1] = &*obs[0];

      THEN("they both point to this object") {
        REQUIRE(ptr[0].get() == &*obs[0]);
        REQUIRE(ptr[1].get() == &*obs[0]);
      }
      THEN("the pointers are never less than each other")
      { REQUIRE_COMM_OP_F(ptr[0], <, ptr[1]); }

      THEN("one pointer is never less when compared to to the Observed* of the other") {
        REQUIRE_COMM_OP_F(ptr[0], <, static_cast<Observed*>(ptr[1].get()));
        REQUIRE_COMM_OP_F(ptr[1], <, static_cast<Observed*>(ptr[0].get()));
      }

      THEN("one pointer is never less when compared to to the Cell* of the other") {
        REQUIRE_COMM_OP_F(ptr[0], <, static_cast<TestCell*>(ptr[1].get()));
        REQUIRE_COMM_OP_F(ptr[1], <, static_cast<TestCell*>(ptr[0].get()));
      }

      AND_WHEN("they are assigned to another object") {
        ptr[0] = &*obs[1];
        ptr[1] = &*obs[1];
        THEN("they both point to that object") {
          REQUIRE(ptr[0].get() == &*obs[1]);
          REQUIRE(ptr[1].get() == &*obs[1]);
        }
      }
    }
  }
}

SCENARIO("A CellPtr can be returned") {
  GIVEN("a function returning a null CellPtr") {
    auto const returner = []{ return CellPtr<Cell>(); };
    WHEN("the function is invoked") THEN("it doesn't crash and returns null")
      REQUIRE(!returner());
  }
  GIVEN("a function returning a non-null CellPtr") {
    static TestCell cell;
    auto const returner = []{ return CellPtr<TestCell>(&cell); };
    WHEN("the function is invoked")
      THEN("it doesn't crash and the returned pointer points to the expected cell")
        REQUIRE(returner() == &cell);
  }
}

SCENARIO("A CellPtr in a structure") {
  struct Carrier {
    CellPtr<TestCell> ptr;
  };
  GIVEN("a default-constructed instance of the structure") {
    Carrier carrier;
    THEN("it is copyable")
    {
      Carrier copy = carrier;
      REQUIRE(!copy.ptr);
    }
    THEN("it is moveable")
    {
      Carrier moved = std::move(carrier);
      REQUIRE(!moved.ptr);
    }
  }
  GIVEN("a structure instance pointing to a cell") {
    TestCell cell;
    Carrier carrier{ CellPtr<TestCell>(&cell) };
    THEN("it is copyable and the copy points into the same cell")
    {
      Carrier copy = carrier;
      REQUIRE(copy.ptr == &cell);
    }
    THEN("it is moveable and the move points into the same cell")
    {
      Carrier moved = std::move(carrier);
      REQUIRE(moved.ptr == &cell);
    }
  }
  GIVEN("a function returning a CellPtr in a structure") {
    auto const returner = []{
      Carrier carrier;
      return carrier;
    };
    WHEN("the function is invoked") THEN("it doesn't crash and returns null")
      REQUIRE(!returner().ptr);
  }
}

SCENARIO("A CellPtr drops the reference to Observed's control block as soon as it can")
{
  GIVEN("a CellPtr and a cell")
  {
    stx::optional<TestCell> cell;
    CellPtr<TestCell> ptr1;
    CellPtr<TestCell> ptr2;

    WHEN("they are default-constructed") {
      cell.emplace();
      THEN("the cell has no control block") REQUIRE(!cell->HasControlBlock());
      AND_THEN("the pointer has no control block") REQUIRE(!ptr1.HasControlBlock());
    }

    WHEN("the a single pointer points to the cell") {
      cell.emplace();
      ptr1 = &cell.value();
      THEN("no control blocks are used") {
        REQUIRE(!cell->HasControlBlock());
        REQUIRE(!ptr1.HasControlBlock());
      }
      AND_THEN("the cell and the pointer reference each other directly"){
        REQUIRE(cell->HasOneCellPtr());
        REQUIRE(ptr1.HasOneObserved());
      }
      AND_WHEN("the cell is destroyed") {
        cell.reset();
        THEN("the pointer points to no cell")
          REQUIRE(!ptr1);
        AND_THEN("the pointer does not use a control block")
          REQUIRE(!ptr1.HasControlBlock());
      }
    }

    WHEN("two pointers point to the cell") {
      cell.emplace();
      ptr1 = &cell.value();
      ptr2 = &cell.value();
      THEN("a control block is used") {
        REQUIRE(cell->HasControlBlock());
        REQUIRE(ptr1.HasControlBlock());
        REQUIRE(ptr2.HasControlBlock());
      }
      AND_WHEN("one pointer is nulled out") {
        ptr1 = nullptr;
        THEN("a control block is still used") {
          REQUIRE(cell->HasControlBlock());
          REQUIRE(ptr2.HasControlBlock());
        }
        AND_WHEN("the other pointer is nulled out") {
          ptr2 = nullptr;
          THEN("the control block is dropped") {
            REQUIRE(!cell->HasControlBlock());
          }
        }
      }
      AND_WHEN("the object is destroyed") {
        cell.reset();
        THEN("the control blocks are still used") {
          REQUIRE(ptr1.HasControlBlock());
          REQUIRE(ptr2.HasControlBlock());
        }
        AND_THEN("when one of the pointers is accessed") {
          REQUIRE(!ptr1);
          THEN("the control blocks are still used")
            REQUIRE(ptr2.HasControlBlock());
          AND_THEN("when the other pointer is accessed") {
            REQUIRE(!ptr2);
            THEN("there is no more control block")
              REQUIRE(!ptr2.HasControlBlock());
          }
        }
      }
    }
  }
}

class FullTestCell : public Cell {
public:
  FullTestCell(Configuration **config) : Cell({}, config) {}
  FullTestCell(const FullTestCell &) : Cell({}, {}) {}
  std::unique_ptr<Cell> Copy() const override;
  const CellTypeInfo &GetInfo() override;
  Cell *GetNextToDraw() const override { return {}; }
  void SetNextToDraw(Cell *) override {}
};
DEFINE_CELL(FullTestCell)

SCENARIO("An InnerCellIterator skips null cells")
{
  GIVEN("A list of two null owning cell pointers") {
    std::unique_ptr<Cell> inner[2];
    WHEN("An inner cell iterator is created on that list") {
      InnerCellIterator it(&inner[0], &inner[1]);
      InnerCellAdapter range(it);
      THEN("The iterator equals the end iterator")
        REQUIRE(it == range.end());
      AND_THEN("The range is empty")
        REQUIRE(range.begin() == range.end());
      AND_THEN("The range-for loop over the range skips the loop body")
        for(auto &cell : range) {
          wxUnusedVar(cell);
          REQUIRE(false);
        }
    }
  }
  GIVEN("A list of a non-null then null owning cell pointer") {
    Configuration configuration;
    Configuration *config = &configuration;
    std::unique_ptr<Cell> inner[2]{std::make_unique<FullTestCell>(&config), nullptr};
    WHEN("An inner cell iterator is created on that list") {
      InnerCellIterator it(&inner[0], &inner[1]);
      InnerCellAdapter range(it);
      THEN("The iterator doesn't equal the end iterator")
        REQUIRE(it != range.end());
      AND_THEN("The range is not empty")
        REQUIRE(range.begin() != range.end());
      AND_THEN("The range begins with the cell")
        REQUIRE(&*range.begin() == inner[0].get());
      AND_THEN("A range-for loop over the range iterates over the cell once")
      {
        std::vector<Cell *> trace;
        for(auto &cell : range) trace.push_back(&cell);
        REQUIRE(trace.size() == 1);
        REQUIRE(trace.front() == inner[0].get());
      }
    }
  }
  GIVEN("A list of a null then non-null owning cell pointer") {
    Configuration configuration;
    Configuration *config = &configuration;
    std::unique_ptr<Cell> inner[2]{nullptr, std::make_unique<FullTestCell>(&config)};
    WHEN("An inner cell iterator is created on that list") {
      InnerCellIterator it(&inner[0], &inner[1]);
      InnerCellAdapter range(it);
      THEN("The iterator doesn't equal the end iterator")
        REQUIRE(it != range.end());
      AND_THEN("The range is not empty")
        REQUIRE(range.begin() != range.end());
      AND_THEN("The range begins with the cell")
        REQUIRE(&*range.begin() == inner[1].get());
      AND_THEN("A range-for loop over the range iterates over the cell once")
      {
        std::vector<Cell *> trace;
        for(auto &cell : range) trace.push_back(&cell);
        REQUIRE(trace.size() == 1);
        REQUIRE(trace.front() == inner[1].get());
      }
    }
  }
}

// If we don't provide our own main when compiling on MinGW
// we currently get an error message that WinMain@16 is missing
// (https://github.com/catchorg/Catch2/issues/1287)
int main(int argc, const char* argv[])
{
    return Catch::Session().run(argc, argv);
}
