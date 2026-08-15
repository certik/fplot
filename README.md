# fplot

[![CI](https://github.com/certik/fplot/actions/workflows/ci.yml/badge.svg)](https://github.com/certik/fplot/actions/workflows/ci.yml)

Pure Fortran plotting library with a pylab-style API, writing SVG, PNG and PDF.
No external graphics library: the PNG rasterizer and its font are part of fplot.

```fortran
use fplot
call plot(x, y, "k-", label="sin")
call title("My plot")
call xlabel("x")
call ylabel("y")
call grid(.true.)
call legend()
call savefig("out.svg")   ! or "out.png", "out.pdf"
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
- `scatter` also takes per-point `sizes=` and color-mapped `cvals=`
  (separate keywords because Fortran cannot overload one dummy as
  scalar-or-array the way matplotlib's `s=` and `c=` do)
- `step`, `stem`, `barh`, `pie`, `boxplot`, `violinplot`
- `semilogx`, `semilogy`, `loglog`, `axhline`, `axvline`
- `text` and `annotate` (with a leader line to the annotated point)
- `imshow` with `cmap`, `extent`, `origin`, `vmin`/`vmax` and square-pixel
  `aspect`, plus `colorbar`
- `contour` and `contourf` with automatic or explicit `levels`
- Colormaps: `viridis`, `plasma`, `inferno`, `magma`, `gray`, `coolwarm`
- Format strings: colors `bgrcmykw` / `C0`–`C9`, linestyles `-` `--` `:` `-.`,
  markers `o x . s ^ v < > * + D`
- Optional `label=`, `lw=`, `color=`, `marker=`, `alpha=`
- Title, axis labels, grid, `xlim` / `ylim`, `clf` / `figure(figsize=, dpi=)`
- `savefig(file, transparent=, facecolor=)`; the extension picks the format,
  one of `.svg`, `.png` or `.pdf`, and `dpi=` sizes the PNG raster
- `axis("on"|"off"|"equal"|"scaled"|"tight"|"auto")` and `set_aspect`
- `set_xscale` / `set_yscale` with `"linear"`, `"log"` or `"symlog"`
- `tick_params(axis=, direction=, length=, labelsize=, rotation=)` and `spines`
- `legend(loc=, fontsize=, ncol=, frameon=, title=, bbox_to_anchor=)`,
  `xticks` / `yticks` with optional labels, `minorticks_on`
- Font sizes: `fontsize=` on `title` / `xlabel` / `ylabel` / `suptitle`, and
  `set_fontsize(size=, title=, labels=, ticks=, legend=)` to set them globally
- `savefig(dpi=, bbox_inches="tight", pad_inches=)` to crop to the drawing
- Several live figures at once: `figure(num=)`, `gcf()`, `close(num=, all=)`
- `subplots(m, n, axs, sharex=, sharey=)` hands back axes handles, so
  `call axs(1,2)%plot(x, y)` works alongside the stateful `subplot` style;
  shared axes span the union of the group and drop their inner tick labels
- Subplots: `subplot(m, n, i)` and figure-level `suptitle`; per-axes state
  (series, labels, grid, legend, scale, limits) with matplotlib's default
  subplot spacing
- `subplots_adjust` and `tight_layout`
- `twinx` and `twiny` for a second y or x axis on the same plot
- Bars: `bar`/`barh` with `bottom=`/`left=` for stacks, `colors=` for a
  color per bar, `edgecolor=`/`linewidth=`, and `bar_label(fmt=, padding=)`
- 49 matplotlib colormaps, any of them reversed with a `_r` suffix, and
  `imshow(norm="log")` for a logarithmic color scale
- `errorbar` with `xerr=` and asymmetric `yerr_lo=`/`yerr_hi=` arms
- `fill_between(where=)` to shade only where a condition holds
- `hist(bins=, bin_edges=, density=, cumulative=, histtype="step"/"stepfilled")`
- `axhspan`/`axvspan` shaded bands and `hlines`/`vlines` line runs
- `style_use("ggplot")` and friends (`seaborn`, `fivethirtyeight`,
  `dark_background`, `grayscale`, `default`), or `rc(figsize=, dpi=,
  fontsize=, linewidth=, grid=, facecolor=, axes_facecolor=, grid_color=,
  text_color=, color_cycle=)` for one setting at a time
- Colors as CSS4/X11 names, `tab:` names, `#rgb`, `#rrggbb`, `#rrggbbaa`,
  single letters, `C0`-`C9`, or a greyscale fraction such as `"0.5"`
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

# Compare against matplotlib references
pixi run compare       # SVG, structurally
pixi run compare-png   # PNG, pixel by pixel
pixi run compare-pdf   # PDF, rasterized by pdftoppm
```

The SVG comparison is structural because a viewer, not fplot, decides what an
SVG looks like. PNG and PDF are compared as pixels, since there fplot decides.

CI (Linux) runs `pixi run test-flang` and `pixi run test-lfortran`, then all
three comparisons.

## Layout

```
src/           library modules
examples/      demo.f90
tests/         Fortran test plots, matplotlib refs, compare scripts
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
