#!/usr/bin/env python3
"""Fail if any .po file in the current tree has fewer translated strings than
the same file on a base ref (default: origin/main).

Guards against a failure mode that has hit this repo's translations twice
now (see AGENTS.md's translations section): a Crowdin export branch that
forked before a translation-restoring commit landed silently blanks those
strings back to empty the next time its export gets merged, since Crowdin
has no concept of "this string used to be translated, don't overwrite it
with nothing." Re-syncing the Crowdin-side branch from main before it
generates its next export is the actual fix; this script is the safety net
that catches a bad merge before it happens, not a substitute for that fix.

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
            f"compared to {base_ref}. This is the Crowdin-stale-branch "
            f"regression documented in AGENTS.md's translations section -- "
            f"do not merge as-is. Re-sync the source branch from {base_ref} "
            f"before generating its export again."
        )
        return 1

    print(f"No translations would be wiped compared to {base_ref}.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
