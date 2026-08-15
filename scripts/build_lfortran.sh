#!/usr/bin/env bash
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BUILD="$ROOT/build/lfortran"
mkdir -p "$BUILD" "$ROOT/tests/out"

LFORTRAN="${LFORTRAN:-lfortran}"
echo "Using compiler: $LFORTRAN"

# LFortran can compile modules and link; use -c for objects when supported.
cd "$BUILD"

# Prefer a single shared-module build
$LFORTRAN -c "$ROOT/src/fplot_colors.f90"
$LFORTRAN -c "$ROOT/src/fplot_style.f90"
$LFORTRAN -c "$ROOT/src/fplot_render.f90"
$LFORTRAN -c "$ROOT/src/fplot_scale.f90"
$LFORTRAN -c "$ROOT/src/fplot_cmap.f90"
$LFORTRAN -c "$ROOT/src/fplot_ticks.f90"
$LFORTRAN -c "$ROOT/src/fplot_contour.f90"
$LFORTRAN -c "$ROOT/src/fplot_svg.f90"
$LFORTRAN -c "$ROOT/src/fplot_backend_svg.f90"
$LFORTRAN -c "$ROOT/src/fplot.f90"

$LFORTRAN -o "$BUILD/test_plots" \
    "$ROOT/tests/test_plots.f90" \
    fplot_colors.o fplot_style.o fplot_render.o fplot_scale.o fplot_cmap.o fplot_ticks.o fplot_contour.o fplot_svg.o fplot_backend_svg.o fplot.o

$LFORTRAN -o "$BUILD/demo" \
    "$ROOT/examples/demo.f90" \
    fplot_colors.o fplot_style.o fplot_render.o fplot_scale.o fplot_cmap.o fplot_ticks.o fplot_contour.o fplot_svg.o fplot_backend_svg.o fplot.o

echo "Built: $BUILD/test_plots $BUILD/demo"
echo "Jupyter: use display_data('image/svg+xml', render_svg()) with lfortran_display"
