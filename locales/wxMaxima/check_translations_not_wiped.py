#!/usr/bin/env python3
"""Fail if any .po file in the current tree has fewer translated strings than
the same file on a base ref (default: origin/main).

Guards against a failure mode that has hit this repo's translations three
times (see AGENTS.md's translations section): a bulk .po edit built from a
stale base silently blanks already-translated strings back to empty, and
looks perfectly ordinary in review -- a plain 3-way git merge does not
understand PO-file semantics, and translated-string counts cannot tell
"reverted to worse text" from "line wrapping changed." Each time it was
464 translations across 16 languages, caught only by diffing them by hand.

The external sync that produced those has since been dropped, but nothing
about the mechanism was specific to it: any bulk catalogue edit can do the
same. Hence the comparison here is keyed on msgid+msgctxt rather than on a
line diff.

Usage: check_translations_not_wiped.py [base-ref]
"""
import subprocess
import sys
from pathlib import Path

import polib


def translated_map(po):
    return {(e.msgid, e.msgctxt): e.msgstr for e in po if not e.obsolete}


def load_from_ref(ref, path):
    exists = subprocess.run(
        ["git", "cat-file", "-e", f"{ref}:{path}"], capture_output=True
    )
    if exists.returncode != 0:
        return None
    content = subprocess.run(
        ["git", "show", f"{ref}:{path}"], capture_output=True, check=True
    ).stdout
    tmp = Path("/tmp") / (Path(path).name + ".base-ref.po")
    tmp.write_bytes(content)
    return polib.pofile(str(tmp))


def main():
    base_ref = sys.argv[1] if len(sys.argv) > 1 else "origin/main"
    po_dir = Path("locales/wxMaxima")
    total_wiped = 0
    any_wiped = False

    for path in sorted(po_dir.glob("*.po")):
        rel = str(path)
        base_po = load_from_ref(base_ref, rel)
        if base_po is None:
            continue  # new file on this branch, nothing to regress against

        head_po = polib.pofile(str(path))
        base_map = translated_map(base_po)
        head_map = translated_map(head_po)

        wiped = [
            key
            for key, base_str in base_map.items()
            if base_str and not head_map.get(key)
        ]
        if wiped:
            any_wiped = True
            total_wiped += len(wiped)
            print(
                f"::error::{path}: {len(wiped)} translation(s) would be wiped "
                f"to empty (translated on {base_ref}, empty or missing here)"
            )

    if any_wiped:
        print(
            f"::error::Total: {total_wiped} translation(s) would be wiped "
            f"compared to {base_ref}. This is the stale-catalogue regression "
            f"documented in AGENTS.md's translations section -- do not merge "
            f"as-is. Rebuild the .po changes on top of {base_ref} rather than "
            f"on whatever older snapshot they were generated from."
        )
        return 1

    print(f"No translations would be wiped compared to {base_ref}.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
