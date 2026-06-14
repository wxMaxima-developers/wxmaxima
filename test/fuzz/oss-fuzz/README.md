# OSS-Fuzz integration files

[OSS-Fuzz](https://github.com/google/oss-fuzz) runs libFuzzer targets like ours
continuously, for free, with automatic crash reporting. These files are the
integration for it. They are kept here for version control; to actually enable
OSS-Fuzz they must be submitted as a pull request to the OSS-Fuzz repository
under `projects/wxmaxima/`:

```
projects/wxmaxima/project.yaml
projects/wxmaxima/Dockerfile
projects/wxmaxima/build.sh
```

## Status: prepared, NOT yet validated on OSS-Fuzz

This scaffolding is a correct starting point, but it has **not** been run through
OSS-Fuzz's infrastructure yet, and two things genuinely need checking first:

1. **wxWidgets 3.2 is built from source** (it isn't packaged on the base image).
   `build.sh` does this with the OSS-Fuzz flags; the build is slow but cached in
   `$WORK`.
2. **The harnesses need an X display at run time** (wxGTK routes font/DC work
   through GTK). They start their own `Xvfb` (see `../fuzz_init.h`), so **Xvfb
   must exist in the OSS-Fuzz *run* image**, not just the build image. This is the
   main unknown — most OSS-Fuzz targets are display-free. If the run image lacks
   Xvfb, the alternative is a display-free harness that stubs `Configuration`
   (the way `test/unit_tests` does) so no GTK/display is needed.

## Validate locally before submitting

Use OSS-Fuzz's helper to build and smoke-run the targets in the real containers:

```bash
git clone https://github.com/google/oss-fuzz
cd oss-fuzz
mkdir -p projects/wxmaxima
cp <wxmaxima>/test/fuzz/oss-fuzz/{project.yaml,Dockerfile,build.sh} projects/wxmaxima/
python3 infra/helper.py build_image  wxmaxima
python3 infra/helper.py build_fuzzers --sanitizer address wxmaxima
python3 infra/helper.py check_build   wxmaxima
python3 infra/helper.py run_fuzzer    wxmaxima fuzz_mathparser
```

`check_build` / `run_fuzzer` is where the display dependency will show up if the
run image has no Xvfb. Fix that (or switch to a display-free harness) before
opening the PR.

## How the CMake honours OSS-Fuzz

`-DWXM_FUZZ=ON` plus a Clang compiler builds the targets. When `$LIB_FUZZING_ENGINE`
is set (as OSS-Fuzz does), the CMake links it instead of `-fsanitize=fuzzer` and
relies on `$CXXFLAGS` for the instrumentation; otherwise it adds
`-fsanitize=fuzzer,address` itself for standalone/CI builds.
