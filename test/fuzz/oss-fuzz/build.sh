#!/bin/bash -eu
# OSS-Fuzz build script for the wxMaxima parser fuzzers.
#
# OSS-Fuzz provides $CC/$CXX, $CFLAGS/$CXXFLAGS (with the fuzzer coverage
# instrumentation + the selected sanitizer) and $LIB_FUZZING_ENGINE (the engine
# to link). Our CMake (WXM_FUZZ) honours $LIB_FUZZING_ENGINE automatically.

WX_PREFIX="$WORK/wx"

# 1. Build wxWidgets (GTK3, static) with the OSS-Fuzz flags so AddressSanitizer
#    works across the boundary. This is the slow part.
if [ ! -f "$WX_PREFIX/bin/wx-config" ]; then
  mkdir -p "$WORK/wxbuild"
  pushd "$WORK/wxbuild" >/dev/null
  "$SRC/wxWidgets/configure" \
      --prefix="$WX_PREFIX" --disable-shared --enable-unicode \
      --with-gtk=3 --without-opengl --disable-sys-libs \
      CC="$CC" CXX="$CXX" CFLAGS="$CFLAGS" CXXFLAGS="$CXXFLAGS"
  make -j"$(nproc)"
  make install
  popd >/dev/null
fi
export PATH="$WX_PREFIX/bin:$PATH"

# 2. Configure + build the fuzz targets.
cmake -S "$SRC/wxmaxima" -B "$WORK/wxmbuild" -G Ninja \
      -DCMAKE_BUILD_TYPE=Debug \
      -DCMAKE_C_COMPILER="$CC" -DCMAKE_CXX_COMPILER="$CXX" \
      -DwxWidgets_CONFIG_EXECUTABLE="$WX_PREFIX/bin/wx-config" \
      -DWXM_FUZZ=ON
ninja -C "$WORK/wxmbuild" fuzz_wxm fuzz_mathparser

# 3. Stage fuzzers + their seed corpora.
cp "$WORK/wxmbuild/src/fuzz_wxm"        "$OUT/"
cp "$WORK/wxmbuild/src/fuzz_mathparser" "$OUT/"
( cd "$SRC/wxmaxima/test/fuzz/corpus_wxm"        && zip -qr "$OUT/fuzz_wxm_seed_corpus.zip" . )
( cd "$SRC/wxmaxima/test/fuzz/corpus_mathparser" && zip -qr "$OUT/fuzz_mathparser_seed_corpus.zip" . )
