# fplot

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
```

## Features (MVP)

- `plot`, `semilogx`, `semilogy`, `loglog`
- Format strings: colors `bgrcmykw` / `C0`–`C9`, markers `ox.`, linestyles `-` `--` `:` `-.`
- Optional `label=`, `lw=`, `color=`
- Title, axis labels, grid, legend, `xlim` / `ylim`, `clf` / `figure`
- SVG defaults aligned with matplotlib (6.4×4.8 in, tab10 colors, subplot margins)

## Build

Requires **Flang** or **LFortran** on `PATH`, and [pixi](https://pixi.sh) for Python comparison tooling.

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
