# Parser fuzzers

Coverage-guided [libFuzzer](https://llvm.org/docs/LibFuzzer.html) targets for
wxMaxima's two parsers. They run the real parser with its real cell,
`Configuration` and (hidden) `Worksheet` dependencies, so each parsed cell is
also laid out — the fuzzers exercise the 2D geometry engine, not just the
readers.

| Target | Parses | Exercises |
|--------|--------|-----------|
| `fuzz_wxm`        | `.wxm` files via `Format::TreeFromWXM` | the old, paste-exposed input/text parser |
| `fuzz_mathparser` | the MathML-like `content.xml` of `.wxmx` via `MathParser::ParseTag` | Maxima's rich 2D output: fractions, matrices, integrals, sub/superscripts, … |

## Building (requires Clang)

libFuzzer is a Clang feature, so configure a separate build directory with a
Clang compiler and `-DWXM_FUZZ=ON`:

```bash
cmake -S . -B build-fuzz -G Ninja \
      -DCMAKE_BUILD_TYPE=Debug -DCMAKE_CXX_COMPILER=clang++ -DWXM_FUZZ=ON
ninja -C build-fuzz fuzz_wxm fuzz_mathparser
```

The whole application (minus `main.cpp`, which holds libFuzzer's own `main()`)
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
