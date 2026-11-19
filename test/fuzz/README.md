# Parser fuzzers

Coverage-guided [libFuzzer](https://llvm.org/docs/LibFuzzer.html) targets for
wxMaxima's parsers. The two worksheet parsers run with their real cell,
`Configuration` and (hidden) `Worksheet` dependencies, so each parsed cell is
also laid out — those fuzzers exercise the 2D geometry engine, not just the
readers. The command-line fuzzer needs nothing but wxBase.

| Target | Parses | Exercises |
|--------|--------|-----------|
| `fuzz_wxm`        | `.wxm` files via `Format::TreeFromWXM` | the old, paste-exposed input/text parser |
| `fuzz_mathparser` | the MathML-like `content.xml` of `.wxmx` via `MathParser::ParseTag` | Maxima's rich 2D output: fractions, matrices, integrals, sub/superscripts, … |
| `fuzz_cmdline`    | the command line via `wxm::cmdline::Parse`/`Interpret` (`src/CommandLine.cpp`) | wxCmdLineParser with our option table, the `wxmxdiff` alias, file names on their way to a window and to Maxima |

## The command-line fuzzer

`fuzz_cmdline` is built from three files rather than the whole application,
so `ninja -C build-fuzz fuzz_cmdline` takes seconds, and it needs no display:

```bash
ninja -C build-fuzz fuzz_cmdline
./build-fuzz/src/fuzz_cmdline -max_total_time=300 test/fuzz/corpus_cmdline
```

The first input byte chooses how the rest becomes a command line: bit 0 off
means NUL-separated arguments (what `main()` gets on Linux/macOS), on means
one string wxWidgets splits (what `WinMain()` gets); bit 1 names the program
`wxmxdiff`. Besides crashes it treats a failed wxWidgets assertion as a
finding and checks properties: `wxmxdiff` must see the same files as
`wxmaxima --diff`, no file argument may be lost, and every file name must
survive `MaximaStringLiteral()` unchanged. What it found is pinned down by
`test/unit_tests/test_CommandLine.cpp`:

- `--+`, `--=x` and other arguments that start with `--` and a character
  that can't be part of an option name make wxWidgets' own parser assert
  (it asks the empty option name for its last character; still so in
  wxWidgets' master as of 2026-11). `wxm::cmdline::Parse()` works around it.

## Building (requires Clang)

libFuzzer is a Clang feature, so configure a separate build directory with a
Clang compiler and `-DWXM_FUZZ=ON`:

```bash
cmake -S . -B build-fuzz -G Ninja \
      -DCMAKE_BUILD_TYPE=Debug -DCMAKE_CXX_COMPILER=clang++ -DWXM_FUZZ=ON
ninja -C build-fuzz fuzz_wxm fuzz_mathparser fuzz_cmdline
```

For `fuzz_wxm` and `fuzz_mathparser` the whole application (minus `main.cpp`, which holds libFuzzer's own `main()`)
is compiled with `-fsanitize=fuzzer-no-link,address`; the targets link with
`-fsanitize=fuzzer,address`.

## Running

The harness backs the parser with a real (hidden) `Worksheet`, and wxGTK routes
font/DC work through GTK, so a display is needed — run under `Xvfb`:

```bash
xvfb-run -a ./build-fuzz/src/fuzz_wxm        test/fuzz/corpus_wxm
xvfb-run -a ./build-fuzz/src/fuzz_mathparser test/fuzz/corpus_mathparser
```

Useful flags: `-max_total_time=600` (stop after N s), `-timeout=20` (per-input),
`-rss_limit_mb=4096`, `-artifact_prefix=…/` (where crashing inputs are saved).

To reproduce a saved crash:

```bash
xvfb-run -a ./build-fuzz/src/fuzz_wxm path/to/crash-<hash>
```

## Corpus

`corpus_wxm/` and `corpus_mathparser/` hold a small set of representative seeds
(`.wxm` files and extracted `content.xml`). libFuzzer grows the corpus in place
as it discovers new code paths; commit genuinely new, minimised inputs to turn
them into permanent regression seeds.

## Notes

- Each cell construction triggers layout, so throughput is modest (~15–30
  exec/s) — this is intentional (it fuzzes parse **and** layout).
- Don't run both fuzzers (or a parallel build) at once on a small machine: CPU
  contention can make a single execution exceed `-timeout` and produce a
  spurious `timeout-*` artifact for an input that is actually fast. Re-time any
  `timeout-*` artifact standalone before treating it as a finding.
- `MathParser` caps XML nesting at depth 250 (substituting a "nesting limit
  exceeded" cell), so deeply-nested inputs can't stack-overflow.
