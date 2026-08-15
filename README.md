# fplot

[![CI](https://github.com/certik/fplot/actions/workflows/ci.yml/badge.svg)](https://github.com/certik/fplot/actions/workflows/ci.yml)

Pure Fortran plotting library that emits SVG text, with a pylab-style API.

```fortran
use fplot
call plot(x, y, "k-", label="sin")
call title("My plot")
call xlabel("x")
call ylabel("y")
call grid(.true.)
call legend()
call savefig("out.svg")   ! file backend
call show()               ! Jupyter (LFortran) or writes fplot_show.svg

! subplots
call clf()
call subplot(2, 1, 1)
call plot(x, y, "b-", label="sin")
call title("top")
call subplot(2, 1, 2)
call plot(x, y2, "r--", label="cos")
call title("bottom")
call suptitle("figure title")
```

## Features (MVP)

- `plot`, `scatter`, `bar`, `hist`, `fill_between`, `errorbar`
- `semilogx`, `semilogy`, `loglog`, `axhline`, `axvline`
- `text` and `annotate` (with a leader line to the annotated point)
- Format strings: colors `bgrcmykw` / `C0`–`C9`, linestyles `-` `--` `:` `-.`,
  markers `o x . s ^ v < > * + D`
- Optional `label=`, `lw=`, `color=`, `marker=`
- Title, axis labels, grid, `xlim` / `ylim`, `clf` / `figure`
- `legend(loc=...)`, `xticks` / `yticks` with optional labels, `minorticks_on`
- Subplots: `subplot(m, n, i)` and figure-level `suptitle`; per-axes state
  (series, labels, grid, legend, scale, limits) with matplotlib's default
  subplot spacing
- SVG defaults aligned with matplotlib (6.4×4.8 in, tab10 colors, subplot margins)

## Build

Requires [pixi](https://pixi.sh). LFortran is installed by pixi on all platforms.
Flang is installed by pixi on Linux via conda-forge `flang_linux-64` (compiler
plus `libflang-rt`); on macOS provide a system Flang on `PATH`.

```bash
pixi install

# Flang (development)
pixi run build-flang
pixi run test-flang
pixi run demo-flang

# LFortran
pixi run build-lfortran
pixi run test-lfortran

# Compare against matplotlib reference SVGs
pixi run compare
```

CI (Linux) runs `pixi run test-flang`, `pixi run test-lfortran` and
`pixi run compare`.

## Layout

```
src/           library modules
examples/      demo.f90
tests/         Fortran test plots, matplotlib refs, compare script
scripts/       build_flang.sh, build_lfortran.sh
```

## Jupyter (LFortran)

With LFortran as a Jupyter kernel, display the SVG interactively:

```fortran
use fplot
use lfortran_display
! ... plot / title / grid / legend ...
call display_data("image/svg+xml", render_svg())
```

`show()` always writes `fplot_show.svg` (works with Flang and LFortran offline).

## Comparison note

Output is **visually** similar to matplotlib, not bit-identical SVG. Matplotlib embeds DejaVu glyph outlines and rich metadata; fplot uses SVG `<text>` and simpler geometry.
