#!/usr/bin/env python3
"""Generate matplotlib reference SVGs matching fplot test cases."""

from __future__ import annotations

from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import LogNorm
import matplotlib.dates as mdates
from matplotlib.patches import Rectangle, Circle, Ellipse, Polygon
import datetime
import numpy as np

ROOT = Path(__file__).resolve().parent
REF = ROOT / "refs"
OUT_NAMES = [
    "basic_line",
    "multi_style",
    "markers_only",
    "semilogx",
    "semilogy",
    "loglog",
    "subplots_2x1",
    "subplots_2x2",
    "markers_gallery",
    "scatter",
    "bar",
    "hist",
    "fill_between",
    "errorbar",
    "hv_lines",
    "text_annotate",
    "ticks_legend",
    "figsize",
    "many_series",
    "alpha",
    "facecolor",
    "imshow",
    "imshow_cbar",
    "imshow_log",
    "subplots_shared",
    "style_ggplot",
    "spans",
    "bar_stacked",
    "bar_colors",
    "hist_opts",
    "hist_bins",
    "errorbar_xy",
    "fill_where",
    "cmap_reversed",
    "cmap_lognorm",
    "categorical",
    "mathtext",
    "pcolormesh",
    "gridspec",
    "dates",
    "quiver",
    "clabel",
    "inset",
    "patches",
    "polar",
    "interp",
    "hist2d",
    "hexbin",
    "matshow",
    "eventplot",
    "broken_barh",
    "scatter_cmap",
    "contour",
    "contourf",
    "step",
    "stem",
    "barh",
    "pie",
    "boxplot",
    "violinplot",
    "symlog",
    "axis_equal",
    "tick_style",
    "tight_layout",
    "subplots_adjust",
    "twinx",
    "colors",
    "fontsize",
    "legend_opts",
    "savefig_tight",
    "figures",
]


def setup_fig():
    fig, ax = plt.subplots(figsize=(6.4, 4.8))
    return fig, ax


def save(fig, name: str, **kw) -> None:
    REF.mkdir(parents=True, exist_ok=True)
    path = REF / f"{name}.svg"
    fig.savefig(path, format="svg", **kw)
    # A PNG reference too: the raster backend has to be measured against
    # matplotlib's own pixels, not against a rasterization of its SVG, which
    # would only tell us how the SVG renderer differs.
    fig.savefig(REF / f"{name}.png", format="png", **kw)
    fig.savefig(REF / f"{name}.pdf", format="pdf", **kw)
    plt.close(fig)
    print(f"wrote {path}")


def main() -> None:
    n = 100
    m = 20
    x = np.linspace(0, 2 * np.pi, n)
    y = np.sin(x)
    y2 = np.cos(x)
    y3 = 0.5 * np.sin(2 * x)

    xl = np.logspace(-1, 1, m)
    yl = xl**2
    yl2 = 10.0 * np.exp(-xl)

    # 1 basic_line
    fig, ax = setup_fig()
    ax.plot(x, y, "k-", label="sin")
    ax.set_title("Basic line")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.grid(True)
    ax.legend()
    ax.set_xlim(0, 2 * np.pi)
    ax.set_ylim(-1.2, 1.2)
    save(fig, "basic_line")

    # 2 multi_style
    fig, ax = setup_fig()
    ax.plot(x, y, "b-o", label="sin")
    ax.plot(x, y2, "r--", label="cos")
    ax.plot(x, y3, "g.", label="half sin2")
    ax.set_title("Multiple styles")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.grid(True)
    ax.legend()
    ax.set_xlim(0, 2 * np.pi)
    ax.set_ylim(-1.5, 1.5)
    save(fig, "multi_style")

    # 3 markers_only
    fig, ax = setup_fig()
    xs, ys, ys2, ys3 = x[::5], y[::5], y2[::5], y3[::5]
    ax.plot(xs, ys, "rx", label="x marks")
    ax.plot(xs, ys2, "bo", label="circles")
    ax.plot(xs, ys3, "g.", label="points")
    ax.set_title("Markers only")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.legend()
    ax.set_xlim(0, 2 * np.pi)
    ax.set_ylim(-1.5, 1.5)
    save(fig, "markers_only")

    # 4 semilogx
    fig, ax = setup_fig()
    ax.semilogx(xl, yl, "b-", label="x^2")
    ax.set_title("semilogx")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.grid(True)
    ax.legend()
    ax.set_xlim(0.1, 10.0)
    ax.set_ylim(0.0, 120.0)
    save(fig, "semilogx")

    # 5 semilogy
    fig, ax = setup_fig()
    ax.semilogy(xl, yl2, "r-o", label="10*exp(-x)")
    ax.set_title("semilogy")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.grid(True)
    ax.legend()
    ax.set_xlim(0.1, 10.0)
    ax.set_ylim(1e-4, 20.0)
    save(fig, "semilogy")

    # 6 loglog
    fig, ax = setup_fig()
    ax.loglog(xl, yl, "k-", label="x^2")
    ax.set_title("loglog")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.grid(True)
    ax.legend()
    ax.set_xlim(0.1, 10.0)
    ax.set_ylim(0.01, 100.0)
    save(fig, "loglog")

    # 7 subplots_2x1
    fig, (ax_top, ax_bot) = plt.subplots(2, 1, figsize=(6.4, 4.8))
    ax_top.plot(x, y, "b-", label="sin")
    ax_top.set_title("top: sin")
    ax_top.set_xlabel("x")
    ax_top.set_ylabel("y")
    ax_top.grid(True)
    ax_top.legend()

    ax_bot.plot(x, y2, "r--", label="cos")
    ax_bot.set_title("bottom: cos")
    ax_bot.set_xlabel("x")
    ax_bot.set_ylabel("y")
    ax_bot.grid(True)
    ax_bot.legend()

    fig.suptitle("fplot subplots")
    save(fig, "subplots_2x1")

    # 8 subplots_2x2
    fig, axs = plt.subplots(2, 2, figsize=(6.4, 4.8))
    axs[0, 0].plot(x, y, "b-", label="sin")
    axs[0, 0].set_title("sin")
    axs[0, 0].grid(True)

    axs[0, 1].plot(x, y2, "r--", label="cos")
    axs[0, 1].set_title("cos")
    axs[0, 1].grid(True)

    axs[1, 0].plot(x, y3, "g-", label="0.5 sin(2x)")
    axs[1, 0].set_title("half sin 2x")
    axs[1, 0].grid(True)

    axs[1, 1].semilogx(xl, yl, "k-", label="x^2")
    axs[1, 1].set_title("semilogx panel")
    axs[1, 1].grid(True)

    fig.suptitle("fplot 2x2 subplots")
    save(fig, "subplots_2x2")

    # 9 markers_gallery
    codes = ["o", "x", ".", "s", "^", "v", "<", ">", "*", "+", "D"]
    xm = np.arange(1.0, 7.0)
    fig, ax = setup_fig()
    for i, code in enumerate(codes, start=1):
        ax.plot(xm, np.full_like(xm, float(i)), marker=code, linestyle="None",
                label=code)
    ax.set_title("Markers")
    ax.set_xlim(0.0, 7.0)
    ax.set_ylim(0.0, len(codes) + 1.0)
    save(fig, "markers_gallery")

    # 10 scatter
    xs = np.linspace(0.0, 10.0, 30)
    ys = np.sin(xs) + 0.1 * xs
    fig, ax = setup_fig()
    ax.scatter(xs, ys, label="points")
    ax.set_title("Scatter")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.grid(True)
    ax.legend()
    save(fig, "scatter")

    # 11 bar
    xb = np.arange(1.0, 7.0)
    hb = np.array([3.0, 5.0, 2.0, 7.0, 4.0, 6.0])
    hb2 = np.array([1.0, 2.0, 4.0, 1.0, 3.0, 2.0])
    fig, ax = setup_fig()
    ax.bar(xb, hb, label="counts")
    ax.set_title("Bar")
    ax.set_xlabel("category")
    ax.set_ylabel("value")
    ax.legend()
    save(fig, "bar")

    # 12 hist
    xh = np.array([
        0.2, 0.5, 0.7, 1.1, 1.3, 1.4, 1.8, 2.0, 2.1, 2.3,
        2.4, 2.6, 2.9, 3.0, 3.2, 3.3, 3.7, 4.0, 4.4, 4.9,
    ])
    fig, ax = setup_fig()
    ax.hist(xh, bins=8, label="samples")
    ax.set_title("Histogram")
    ax.set_xlabel("value")
    ax.set_ylabel("count")
    ax.legend()
    save(fig, "hist")

    # 13 fill_between
    fig, ax = setup_fig()
    ax.fill_between(x, y, y3, alpha=0.5, label="band")
    ax.plot(x, y, "b-", label="sin")
    ax.set_title("Fill between")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.grid(True)
    ax.legend()
    save(fig, "fill_between")

    # 14 errorbar
    xe = np.arange(1.0, 9.0)
    ye = np.sqrt(xe)
    ee = 0.15 * ye
    fig, ax = setup_fig()
    ax.errorbar(xe, ye, yerr=ee, fmt="o-", capsize=3.0, label="meas")
    ax.set_title("Errorbar")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.grid(True)
    ax.legend()
    save(fig, "errorbar")

    # 15 hv_lines
    fig, ax = setup_fig()
    ax.plot(x, y, "b-", label="sin")
    ax.axhline(0.0, color="k", linestyle="--")
    ax.axvline(np.pi, color="r", linestyle=":")
    ax.set_title("Reference lines")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.grid(True)
    ax.legend()
    save(fig, "hv_lines")

    # 16 text_annotate
    fig, ax = setup_fig()
    ax.plot(x, y, "b-", label="sin")
    ax.text(1.0, 0.8, "peak region", color="k")
    ax.annotate("minimum", xy=(4.712, -1.0), xytext=(2.2, -0.55), color="r")
    ax.set_title("Text and annotate")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.grid(True)
    save(fig, "text_annotate")

    # 17 ticks_legend
    fig, ax = setup_fig()
    ax.plot(x, y, "b-", label="sin")
    ax.plot(x, np.cos(x), "r--", label="cos")
    ax.set_xticks([0.0, 1.5708, 3.1416, 4.7124, 6.2832],
                  ["0", "pi/2", "pi", "3pi/2", "2pi"])
    ax.set_yticks([-1.0, 0.0, 1.0])
    ax.minorticks_on()
    ax.legend(loc="lower left")
    ax.set_title("Ticks and legend placement")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    save(fig, "ticks_legend")

    # 18 figsize
    fig, ax = plt.subplots(figsize=(8.0, 3.0))
    ax.plot(x, y, "b-", label="sin")
    ax.set_title("Wide figure")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.grid(True)
    ax.legend()
    save(fig, "figsize")

    # 19 many_series
    fig, ax = setup_fig()
    for i in range(1, 41):
        ax.plot(x, np.sin(x + 0.05 * i))
    ax.set_title("Forty series")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    save(fig, "many_series")

    # 20 alpha
    fig, ax = setup_fig()
    ax.plot(x, y, "b-", label="sin", alpha=0.35)
    ax.plot(x, np.cos(x), "r-o", label="cos", alpha=0.6)
    ax.scatter(x[:30], y[:30], s=80.0, c="g", label="pts", alpha=0.5)
    ax.set_title("Alpha")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.legend()
    save(fig, "alpha")

    # 21 facecolor
    fig, ax = setup_fig()
    ax.plot(x, y, "b-")
    ax.set_title("Figure facecolor")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.grid(True)
    save(fig, "facecolor", facecolor="#eeeeee")

    # 22/23 imshow
    nzr, nzc = 8, 16
    zimg = np.array([[np.sin(0.4 * (j + 1)) + np.cos(0.5 * (i + 1))
                      for j in range(nzc)] for i in range(nzr)])
    zlog = np.array([[10.0 ** (0.5 * (i + j + 2) / 4.0)
                      for j in range(nzc)] for i in range(nzr)])

    fig, ax = setup_fig()
    ax.imshow(zimg)
    ax.set_title("imshow")
    save(fig, "imshow")

    fig, ax = setup_fig()
    im = ax.imshow(zimg, cmap="plasma", extent=(0.0, 4.0, 0.0, 2.0), origin="lower")
    cb = fig.colorbar(im)
    cb.set_label("value")
    ax.set_title("imshow with colorbar")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    save(fig, "imshow_cbar")

    # 24 scatter_cmap
    ns = 30
    svals = np.array([20.0 + 8.0 * (i + 1) for i in range(ns)])
    cvals = np.array([float(i + 1) for i in range(ns)])
    fig, ax = setup_fig()
    sc = ax.scatter(x[:ns], y[:ns], s=svals, c=cvals, cmap="viridis")
    cb = fig.colorbar(sc)
    cb.set_label("c")
    ax.set_title("Scatter with c and s arrays")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    save(fig, "scatter_cmap")

    # 25/26 contour
    fig, ax = setup_fig()
    ax.contour(zimg)
    ax.set_title("contour")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    save(fig, "contour")

    fig, ax = setup_fig()
    cf = ax.contourf(zimg, cmap="coolwarm")
    fig.colorbar(cf)
    ax.set_title("contourf")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    save(fig, "contourf")

    # 27-30 step / stem / barh / pie
    fig, ax = setup_fig()
    ax.step(xb, hb, where="mid")
    ax.set_title("step")
    save(fig, "step")

    fig, ax = setup_fig()
    ax.stem(xb, hb)
    ax.set_title("stem")
    save(fig, "stem")

    fig, ax = setup_fig()
    ax.barh(xb, hb)
    ax.set_title("barh")
    save(fig, "barh")

    fig, ax = setup_fig()
    ax.pie(hb, labels=["a", "b", "c", "d", "e", "f"])
    ax.set_title("pie")
    save(fig, "pie")

    # 31-32 boxplot / violinplot
    k = np.arange(1, 41, dtype=float)
    dist1 = np.sin(k) + 0.3 * np.cos(2.7 * k)
    dist2 = 1.0 + 2.0 * np.sin(0.7 * k) ** 3

    fig, ax = setup_fig()
    ax.boxplot([dist1, dist2])
    ax.set_title("boxplot")
    save(fig, "boxplot")

    fig, ax = setup_fig()
    ax.violinplot([dist1, dist2])
    ax.set_title("violinplot")
    save(fig, "violinplot")

    # 33 symlog
    xsym = np.linspace(-100.0, 100.0, 201)
    fig, ax = setup_fig()
    ax.plot(xsym, xsym)
    ax.set_yscale("symlog")
    ax.set_title("symlog")
    save(fig, "symlog")

    # 34 axis equal
    fig, ax = setup_fig()
    ax.plot(xb, hb, marker="o")
    ax.axis("equal")
    ax.set_title("axis equal")
    save(fig, "axis_equal")

    # 35 tick styling
    fig, ax = setup_fig()
    ax.plot(xb, hb)
    ax.tick_params(direction="in", labelsize=8.0)
    ax.tick_params(axis="x", rotation=45.0)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.set_title("tick_params")
    save(fig, "tick_style")

    # 36 tight_layout
    fig, axs = plt.subplots(1, 2)
    axs[0].plot(xb, hb * 1e3)
    axs[0].set_ylabel("value")
    axs[0].set_xlabel("category")
    axs[1].plot(xb, hb)
    axs[1].set_xlabel("category")
    fig.tight_layout()
    save(fig, "tight_layout")

    # 37 subplots_adjust
    fig, axs = plt.subplots(2, 1)
    axs[0].plot(xb, hb)
    axs[1].plot(xb, hb)
    fig.subplots_adjust(left=0.2, hspace=0.5)
    save(fig, "subplots_adjust")

    # 38 twinx
    fig, ax = setup_fig()
    ax.plot(xb, hb)
    ax.set_ylabel("left")
    ax.set_xlabel("x")
    ax2 = ax.twinx()
    ax2.plot(xb, hb * 100.0, color="C1")
    ax2.set_ylabel("right")
    ax.set_title("twinx")
    save(fig, "twinx")

    # 39 color spellings
    fig, ax = setup_fig()
    xsm = [0.0, 1.0]
    for i, c in enumerate(
        ["red", "tab:orange", "steelblue", "#0f0", "0.5", "#8c564bcc"], start=1
    ):
        ax.plot(xsm, [float(i)] * 2, color=c, lw=3.0)
    ax.set_title("color names")
    save(fig, "colors")

    # 40 font sizes
    fig, ax = setup_fig()
    ax.plot(xb, hb, label="sine")
    ax.set_title("big title", fontsize=17.0)
    ax.set_xlabel("x axis", fontsize=14.0)
    ax.set_ylabel("y axis", fontsize=14.0)
    ax.tick_params(labelsize=13.0)
    ax.legend(fontsize=12.0)
    save(fig, "fontsize")

    # 41 legend options
    fig, ax = setup_fig()
    ax.plot(xb, hb, label="alpha")
    ax.plot(xb, hb * 0.5, label="beta")
    ax.plot(xb, hb * 0.25, label="gamma")
    ax.plot(xb, hb * 0.125, label="delta")
    ax.legend(loc="upper right", ncol=2, title="series", frameon=False)
    save(fig, "legend_opts")

    # 42 savefig bbox_inches="tight"
    fig, ax = setup_fig()
    ax.plot(xb, hb)
    ax.set_title("tight")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    save(fig, "savefig_tight", bbox_inches="tight", dpi=200)

    # 43 two live figures kept apart, then closed
    f1, a1 = setup_fig()
    a1.plot(xb, hb, "r-")
    a1.set_title("figure one")
    f2, a2 = setup_fig()
    a2.plot(xb, -hb, "b-")
    a2.set_title("figure two")
    plt.close(f2)
    save(f1, "figures")

    # 44 an image on a log axis
    fig, ax = setup_fig()
    ax.imshow(zimg, extent=(1.0, 1000.0, 0.0, 4.0), aspect="auto")
    ax.set_xscale("log")
    ax.set_title("imshow on a log axis")
    save(fig, "imshow_log")

    # 45 subplots with axes handles and a shared x axis
    fig, axs = plt.subplots(2, 2, sharex=True, figsize=(6.4, 4.8))
    axs[0, 0].plot(x, y, "b-", label="sin")
    axs[0, 0].set_title("one")
    axs[0, 0].legend()
    axs[0, 1].scatter(xs, ys, s=18.0, c="r")
    axs[0, 1].set_title("two")
    axs[1, 0].bar(xb, hb, color="g")
    axs[1, 0].set_xlabel("x")
    axs[1, 1].plot(x, y3, "m--")
    axs[1, 1].grid(True)
    axs[1, 1].set_xlabel("x")
    fig.suptitle("subplots with handles")
    save(fig, "subplots_shared")

    # 46 a style sheet
    with plt.style.context("ggplot"):
        fig, ax = plt.subplots(figsize=(6.4, 4.8))
        ax.plot(x, y, label="sin")
        ax.plot(x, y2, label="cos")
        ax.set_title("ggplot style")
        ax.set_xlabel("x")
        ax.set_ylabel("y")
        ax.legend()
        save(fig, "style_ggplot")

    # 47 spans and line runs
    fig, ax = setup_fig()
    ax.plot(x, y, "b-")
    ax.axhspan(-0.5, 0.5, color="orange", alpha=0.3)
    ax.axvspan(1.0, 2.0, color="green", alpha=0.2)
    ax.hlines([-1.0, 1.0], 0.0, 3.0, color="red", linestyle="--")
    ax.vlines([4.0, 5.0], -1.0, 0.0, color="purple")
    ax.set_title("spans and line runs")
    save(fig, "spans")

    # 48 stacked and labelled bars
    fig, ax = setup_fig()
    ax.bar(xb, hb, width=0.6, color="tab:blue", label="first")
    ax.bar(xb, hb2, width=0.6, bottom=hb, color="tab:orange", label="second")
    ax.bar_label(ax.containers[-1], padding=3)
    ax.legend()
    ax.set_title("stacked bars")
    save(fig, "bar_stacked")

    # 49 per-bar colors and horizontal bar labels
    fig, ax = setup_fig()
    ax.barh(xb, hb, color=["red", "green", "blue", "orange", "purple", "brown"])
    ax.bar_label(ax.containers[-1], padding=2)
    ax.set_title("bars in their own colors")
    save(fig, "bar_colors")

    # 50 histogram options
    fig, ax = setup_fig()
    ax.hist(dist1, bins=12, density=True, color="tab:blue", alpha=0.6,
            label="density")
    ax.hist(dist1, bins=12, density=True, histtype="step", color="k",
            label="step")
    ax.legend()
    ax.set_title("histogram options")
    save(fig, "hist_opts")

    # 51 uneven bins, cumulative
    fig, ax = setup_fig()
    ax.hist(dist1, bins=[-3.0, -1.0, 0.0, 1.5, 3.0], cumulative=True,
            color="tab:green")
    ax.set_title("uneven bins, cumulative")
    save(fig, "hist_bins")

    # 52 asymmetric errors in both directions
    fig, ax = setup_fig()
    ax.errorbar(xe, ye, yerr=[ee, 0.5 * ee], xerr=0.3 * ee, fmt="o",
                color="tab:red", capsize=4.0)
    ax.set_title("asymmetric errors")
    save(fig, "errorbar_xy")

    # 53 fill_between with a condition
    fig, ax = setup_fig()
    ax.plot(x, y, "k-")
    ax.fill_between(x, y, 0.0, where=(y > 0.0), color="tab:green", alpha=0.4,
                    label="positive")
    ax.fill_between(x, y, 0.0, where=(y < 0.0), color="tab:red", alpha=0.4,
                    label="negative")
    ax.legend()
    ax.set_title("fill_between where")
    save(fig, "fill_where")

    # 54 a reversed colormap
    fig, ax = setup_fig()
    im = ax.imshow(zimg, cmap="RdBu_r", aspect="auto")
    fig.colorbar(im, ax=ax)
    ax.set_title("RdBu_r")
    save(fig, "cmap_reversed")

    # 55 a logarithmic color norm
    fig, ax = setup_fig()
    im = ax.imshow(zlog, cmap="magma", norm=LogNorm(), aspect="auto")
    fig.colorbar(im, ax=ax)
    ax.set_title("log color norm")
    save(fig, "cmap_lognorm")

    # 56 categories instead of numbers
    fig, ax = setup_fig()
    ax.bar(["apple", "banana", "cherry", "date"], hb[:4], color="tab:purple")
    ax.set_ylabel("count")
    ax.set_title("categories")
    save(fig, "categorical")

    # 57 mathtext in the labels
    fig, ax = setup_fig()
    ax.plot(x, y)
    ax.set_xlabel(r"$x_{i}$ [m]")
    ax.set_ylabel(r"$E = mc^{2}$")
    ax.set_title(r"$10^{-3} < T^{2}_{n} < 10^{3}$")
    save(fig, "mathtext")

    # 58 a mesh with uneven cells
    fig, ax = setup_fig()
    xe = np.array([0.0, 0.5, 1.5, 3.0, 5.0, 8.0, 9.0, 10.0, 11.0, 12.0,
                   13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0])
    ye = np.array([0.0, 1.0, 2.0, 4.0, 6.0, 6.5, 7.0, 8.0, 10.0])
    m = ax.pcolormesh(xe, ye, zimg, cmap="viridis")
    fig.colorbar(m, ax=ax)
    ax.set_title("pcolormesh")
    save(fig, "pcolormesh")

    # 59 panels spanning several cells
    fig = plt.figure(figsize=(6.4, 4.8), dpi=100)
    a1 = plt.subplot2grid((3, 3), (0, 0), colspan=3)
    a2 = plt.subplot2grid((3, 3), (1, 0), colspan=2, rowspan=2)
    a3 = plt.subplot2grid((3, 3), (1, 2), rowspan=2)
    a1.plot(x, y)
    a1.set_title("wide")
    a2.plot(x, y2)
    a2.set_title("big")
    a3.plot(x, y)
    a3.set_title("tall")
    save(fig, "gridspec")

    # 60 a date axis
    fig, ax = setup_fig()
    t0 = mdates.date2num(datetime.datetime(2024, 1, 1))
    td = t0 + np.arange(0.0, 366.0, 1.0)
    ax.plot(td, np.sin(2.0 * np.pi * np.arange(366) / 365.0))
    ax.xaxis_date()
    ax.set_title("dates")
    save(fig, "dates")

    # 61 a vector field
    fig, ax = setup_fig()
    qy, qx = np.meshgrid(np.arange(8) * 0.5, np.arange(8) * 0.5, indexing="ij")
    ax.quiver(qx, qy, np.cos(qx), np.sin(qy))
    ax.set_title("quiver")
    save(fig, "quiver")

    # 62 labelled contour lines
    fig, ax = setup_fig()
    cs = ax.contour(zimg)
    ax.clabel(cs)
    ax.set_title("clabel")
    save(fig, "clabel")

    # 63 an inset and a secondary axis
    fig, ax = setup_fig()
    ax.plot(x, y)
    ax.set_title("inset")
    ax.secondary_xaxis("top", functions=(lambda v: 2.0 * v, lambda v: v / 2.0))
    axi = ax.inset_axes([0.6, 0.6, 0.35, 0.3])
    axi.plot(x, y2)
    save(fig, "inset")

    # 64 plain shapes
    fig, ax = setup_fig()
    ax.add_patch(Rectangle((0.1, 0.1), 0.4, 0.2, facecolor="tab:orange",
                           edgecolor="black"))
    ax.add_patch(Circle((0.7, 0.7), 0.15))
    ax.add_patch(Ellipse((0.3, 0.7), 0.4, 0.2, angle=30.0,
                         facecolor="tab:green", alpha=0.5))
    ax.add_patch(Polygon([[0.6, 0.1], [0.9, 0.1], [0.75, 0.4]],
                         edgecolor="tab:red", lw=2.0, fill=False))
    ax.set_title("patches")
    save(fig, "patches")

    # 65 a polar plot
    fig = plt.figure(figsize=(6.4, 4.8), dpi=100)
    ax = fig.add_subplot(projection="polar")
    th = np.linspace(0.0, 2.0 * np.pi, 100)
    ax.plot(th, 1.0 + np.cos(th))
    ax.set_title("polar")
    save(fig, "polar")

    # 66 a smoothed image
    fig, ax = setup_fig()
    ax.imshow(zimg, interpolation="bilinear")
    ax.set_title("interp")
    save(fig, "interp")

    # 67/68 two dimensional histograms
    seed = np.arange(1, 501, dtype=float)
    ha = np.sin(seed * 12.9898) * 43758.5453
    hb = np.sin(seed * 12.9898 + 1.0) * 43758.5453
    hxx = (ha - np.floor(ha)) + (hb - np.floor(hb)) - 1.0
    ha = np.sin(seed * 78.233) * 43758.5453
    hb = np.sin(seed * 78.233 + 1.0) * 43758.5453
    hyy = (ha - np.floor(ha)) + (hb - np.floor(hb)) - 1.0

    fig, ax = setup_fig()
    ax.hist2d(hxx, hyy, bins=[12, 12])
    ax.set_title("hist2d")
    save(fig, "hist2d")

    fig, ax = setup_fig()
    ax.hexbin(hxx, hyy, gridsize=15)
    ax.set_title("hexbin")
    save(fig, "hexbin")

    # 69 a matrix as an image
    fig, ax = setup_fig()
    zm = np.array([[float(i * j) for j in range(1, 7)] for i in range(1, 7)])
    ax.matshow(zm)
    save(fig, "matshow")

    # 70 a row of events
    fig, ax = setup_fig()
    seed = np.arange(1, 41, dtype=float) * 3.7
    ea = np.sin(seed) * 43758.5453
    eb = np.sin(seed + 1.0) * 43758.5453
    ev = 10.0 * ((ea - np.floor(ea)) + (eb - np.floor(eb)))
    ax.eventplot(ev, color="C0")
    ax.set_title("eventplot")
    save(fig, "eventplot")

    # 71 bars broken into pieces
    fig, ax = setup_fig()
    ax.broken_barh([(1.0, 2.0), (4.0, 2.0), (7.0, 3.0)], (1.0, 0.8), color="C0")
    ax.broken_barh([(2.0, 3.0), (6.0, 3.0)], (2.5, 0.8), color="C1")
    ax.set_title("broken_barh")
    save(fig, "broken_barh")

    print("All matplotlib references written.")


if __name__ == "__main__":
    main()
