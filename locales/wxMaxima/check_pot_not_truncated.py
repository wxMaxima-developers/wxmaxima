#!/usr/bin/env python3
"""Fail if a freshly regenerated wxMaxima.pot has lost a suspicious number of
strings compared to the committed one.

This is the gate that makes an automated, unattended POT refresh safe to let
push on its own. Without it, the refresh job would faithfully commit whatever
xgettext produced -- including a POT with whole directories missing.

That is not hypothetical. This project's extraction glob was flat `src/*`
for years, so every string under `src/cells`, `src/wizards` and friends was
silently absent; about 7000 translations were eventually recovered from git
history. Note what the older per-commit "is the POT up to date" check could
*not* do about it: it compared the committed POT against a fresh regeneration,
and once a truncated POT had been committed, both sides were truncated
identically and the check passed. Comparing against what was there *before*
is what catches this, which is why this script exists and why it compares
msgids rather than lines.

Strings do legitimately disappear -- a feature gets deleted, a message gets
reworded. So this is a threshold, not a tripwire: it asks "did we lose an
implausible amount at once", and when it fires the answer is for a human to
look, not for anything to be discarded.

Usage: check_pot_not_truncated.py OLD_POT NEW_POT [--max-removed N]
                                                 [--max-removed-fraction F]

Exits 0 if the new POT is safe to commit, 1 otherwise.
"""
import argparse
import re
import sys

#! Absolute floor: fewer removals than this are never worth blocking on,
#! however small the catalogue. Deleting one dialog can easily remove a
#! handful of strings.
DEFAULT_MAX_REMOVED = 50
#! ...and relative ceiling, so a small catalogue cannot lose most of itself
#! while staying under the absolute floor. Whichever is *stricter* applies.
DEFAULT_MAX_REMOVED_FRACTION = 0.05


def msgids(path):
    """Every msgid in a .pot file, as a set.

    Deliberately a small hand-rolled parse rather than polib: this runs in
    the refresh job before anything else is installed, and it must not be
    the reason that job fails. It only needs msgids, which are easy to read
    exactly -- a msgid line plus any number of continuation strings, with
    the header's empty msgid and obsolete (#~) entries skipped.
    """
    ids = set()
    current = None
    with open(path, encoding="utf-8") as f:
        for line in f:
            line = line.rstrip("\n")
            if line.startswith("#~"):
                # An obsolete entry is already not offered to translators;
                # it neither counts as present nor as newly removed.
                if current is not None:
                    ids.add(current)
                    current = None
                continue
            if line.startswith("msgid "):
                if current is not None:
                    ids.add(current)
                current = line[len("msgid "):].strip()
            elif current is not None:
                if line.startswith('"'):
                    current += line.strip()
                else:
                    ids.add(current)
                    current = None
    if current is not None:
        ids.add(current)
    # The header entry is msgid "" -- not a translatable string.
    ids.discard('""')
    return ids


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("old_pot")
    parser.add_argument("new_pot")
    parser.add_argument("--max-removed", type=int, default=DEFAULT_MAX_REMOVED)
    parser.add_argument("--max-removed-fraction", type=float,
                        default=DEFAULT_MAX_REMOVED_FRACTION)
    args = parser.parse_args()

    old = msgids(args.old_pot)
    new = msgids(args.new_pot)

    removed = old - new
    added = new - old

    print(f"Committed POT: {len(old)} strings")
    print(f"Regenerated:   {len(new)} strings "
          f"({len(added)} added, {len(removed)} removed)")

    if not removed:
        print("No strings removed. Safe to commit.")
        return 0

    # Whichever limit is stricter for this catalogue's size.
    fraction_limit = int(len(old) * args.max_removed_fraction)
    limit = min(args.max_removed, fraction_limit) if old else args.max_removed

    for msgid in sorted(removed)[:20]:
        print(f"  removed: {msgid}")
    if len(removed) > 20:
        print(f"  ... and {len(removed) - 20} more")

    if len(removed) > limit:
        print(
            f"::error::{len(removed)} strings would disappear from "
            f"wxMaxima.pot, over the limit of {limit}. A string missing from "
            f"the POT is a string no translator is ever offered, in any "
            f"language, so this refuses to commit rather than quietly "
            f"shrinking the catalogue. If the extraction itself broke (see "
            f"POT_SOURCE_FILES in locales/wxMaxima/CMakeLists.txt -- it is an "
            f"explicit two-level glob, not a recursive one), fix that. If the "
            f"removals are genuine, regenerate and commit by hand."
        )
        return 1

    print(f"{len(removed)} strings removed, within the limit of {limit}. "
          f"Safe to commit.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
