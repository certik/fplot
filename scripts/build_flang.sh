#!/usr/bin/env bash
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BUILD="$ROOT/build/flang"
mkdir -p "$BUILD" "$ROOT/tests/out"

# Use the `flang` frontend on PATH. Do not honor $FC: conda-forge's
# flang_linux-64 activation sets FC="${CHOST}-flang", which becomes
# "-flang" under pixi because CHOST is unset.
case "${FLANG:-}" in
    ""|-*) FLANG=flang ;;
esac
echo "Using compiler: $FLANG"

cd "$BUILD"
# Compile modules in dependency order
$FLANG -c "$ROOT/src/fplot_style.f90"
$FLANG -c "$ROOT/src/fplot_ticks.f90"
$FLANG -c "$ROOT/src/fplot_svg.f90"
$FLANG -c "$ROOT/src/fplot.f90"

# Test program
$FLANG -o "$BUILD/test_plots" \
    "$ROOT/tests/test_plots.f90" \
    fplot_style.o fplot_ticks.o fplot_svg.o fplot.o

# Demo
$FLANG -o "$BUILD/demo" \
    "$ROOT/examples/demo.f90" \
    fplot_style.o fplot_ticks.o fplot_svg.o fplot.o

echo "Built: $BUILD/test_plots $BUILD/demo"
