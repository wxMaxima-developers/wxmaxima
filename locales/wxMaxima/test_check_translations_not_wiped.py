#!/usr/bin/env python3
"""Tests for check_translations_not_wiped.py.

Each test builds a throwaway git repository holding a locales/wxMaxima/
catalogue, commits it as the base, changes the working tree and runs the
script against the base commit, exactly as CI does against origin/main.

Run with: python3 -m unittest locales/wxMaxima/test_check_translations_not_wiped.py
(needs polib, like the script itself).
"""
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

SCRIPT = Path(__file__).resolve().parent / "check_translations_not_wiped.py"

HEADER = '''msgid ""
msgstr ""
"Content-Type: text/plain; charset=UTF-8\\n"

'''


def entry(msgid, msgstr="", obsolete=False):
    prefix = "#~ " if obsolete else ""
    return f'{prefix}msgid "{msgid}"\n{prefix}msgstr "{msgstr}"\n\n'


class CheckTranslationsNotWiped(unittest.TestCase):
    def setUp(self):
        self._tmp = tempfile.TemporaryDirectory()
        self.repo = Path(self._tmp.name)
        self.po_dir = self.repo / "locales" / "wxMaxima"
        self.po_dir.mkdir(parents=True)
        self.git("init", "-q")
        self.git("config", "user.email", "test@example.com")
        self.git("config", "user.name", "test")

    def tearDown(self):
        self._tmp.cleanup()

    def git(self, *args):
        subprocess.run(["git", *args], cwd=self.repo, check=True,
                       capture_output=True)

    def write(self, name, *entries):
        (self.po_dir / name).write_text(HEADER + "".join(entries),
                                        encoding="utf-8")

    def commit_base(self):
        self.git("add", "-A")
        self.git("commit", "-q", "-m", "base")

    def run_check(self):
        return subprocess.run([sys.executable, str(SCRIPT), "HEAD"],
                              cwd=self.repo, capture_output=True, text=True)

    def test_unchanged_catalogue_passes(self):
        self.write("wxMaxima.pot", entry("Open"), entry("Save"))
        self.write("de.po", entry("Open", "Öffnen"), entry("Save", "Speichern"))
        self.commit_base()
        self.assertEqual(self.run_check().returncode, 0)

    def test_blanked_translation_of_a_current_string_fails(self):
        self.write("wxMaxima.pot", entry("Open"), entry("Save"))
        self.write("de.po", entry("Open", "Öffnen"), entry("Save", "Speichern"))
        self.commit_base()
        self.write("de.po", entry("Open", "Öffnen"), entry("Save"))
        result = self.run_check()
        self.assertEqual(result.returncode, 1)
        self.assertIn("1 translation(s) would be wiped", result.stdout)

    def test_dropped_entry_of_a_current_string_fails(self):
        self.write("wxMaxima.pot", entry("Open"), entry("Save"))
        self.write("de.po", entry("Open", "Öffnen"), entry("Save", "Speichern"))
        self.commit_base()
        self.write("de.po", entry("Open", "Öffnen"))
        self.assertEqual(self.run_check().returncode, 1)

    def test_string_removed_from_the_source_passes(self):
        # What the weekly refresh produces after a feature is deleted: the
        # msgid leaves the template and msgmerge makes the entry obsolete.
        self.write("wxMaxima.pot", entry("Open"), entry("Save"))
        self.write("de.po", entry("Open", "Öffnen"), entry("Save", "Speichern"))
        self.commit_base()
        self.write("wxMaxima.pot", entry("Open"))
        self.write("de.po", entry("Open", "Öffnen"),
                   entry("Save", "Speichern", obsolete=True))
        self.assertEqual(self.run_check().returncode, 0)

    def test_string_removed_and_entry_gone_passes(self):
        # msgmerge can also reuse a retired translation as a fuzzy
        # suggestion for a similar new msgid and drop the old entry.
        self.write("wxMaxima.pot", entry("Open"), entry("Help browser:"))
        self.write("de.po", entry("Open", "Öffnen"),
                   entry("Help browser:", "Hilfe-Browser:"))
        self.commit_base()
        self.write("wxMaxima.pot", entry("Open"), entry("Show help in:"))
        self.write("de.po", entry("Open", "Öffnen"), entry("Show help in:"))
        self.assertEqual(self.run_check().returncode, 0)

    def test_without_a_template_every_msgid_counts(self):
        self.write("de.po", entry("Open", "Öffnen"), entry("Save", "Speichern"))
        self.commit_base()
        self.write("de.po", entry("Open", "Öffnen"),
                   entry("Save", "Speichern", obsolete=True))
        self.assertEqual(self.run_check().returncode, 1)


if __name__ == "__main__":
    unittest.main()
