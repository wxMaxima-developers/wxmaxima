# Which markdownlint (mdl) rules apply to this project's Markdown docs.
#
# Referenced from .mdlrc, which is what `mdl` reads automatically; the ctest
# target check-markdown runs it over the tracked *.md files.
#
# This is a Ruby file evaluated by mdl, which is also why the rules live here
# rather than in a `rules "~MD001", ...` line in .mdlrc: only a style file can
# pass parameters to a rule (see MD029 below).

all

# Line length. Our docs are hard-wrapped by hand at no single width, and
# info/wxmaxima.md carries long links and tables.
exclude_rule 'MD013'

# Several top-level headers per file: NEWS.md is a changelog with one "# <version>"
# per release, which is exactly what this rule forbids.
exclude_rule 'MD025'

# Trailing punctuation in a header: our headers sometimes end in "?" or ":".
exclude_rule 'MD026'

# Inline HTML: the manual uses it for anchors and images.
exclude_rule 'MD033'

# Unordered list indentation. Disabled because mdl 0.17's implementation does
# not measure what it claims: given a single nested bullet it accepts an indent
# of 1, 3 or 6 spaces and rejects 2, 4 and 5, so there is no indentation a
# document could use to satisfy it. MD005 below still catches list items that
# are indented inconsistently *with each other*, which is the part that actually
# affects rendering.
exclude_rule 'MD007'

# Ordered list numbering: allow both "1. 1. 1." and "1. 2. 3.". mdl's default
# permits only the former, but a numbered procedure is easier to follow in the
# source when the numbers are real.
rule 'MD029', :style => :one_or_ordered
