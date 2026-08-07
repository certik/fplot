#!/usr/bin/env python3
"""Generate matplotlib reference SVGs matching fplot test cases."""

from __future__ import annotations

from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
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
]


def setup_fig():
    fig, ax = plt.subplots(figsize=(6.4, 4.8))
    return fig, ax


def save(fig, name: str) -> None:
    REF.mkdir(parents=True, exist_ok=True)
    path = REF / f"{name}.svg"
    fig.savefig(path, format="svg")
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

    print("All matplotlib references written.")


if __name__ == "__main__":
    main()
