#!/usr/bin/env python3
"""Compare fplot SVGs to matplotlib references (visual/structural, not bit-identical)."""

from __future__ import annotations

import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent
REF = ROOT / "refs"
OUT = ROOT / "out"

CASES = [
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
]


def parse_canvas(svg: str) -> tuple[str | None, str | None, str | None]:
    w = re.search(r'width="([^"]+)"', svg)
    h = re.search(r'height="([^"]+)"', svg)
    vb = re.search(r'viewBox="([^"]+)"', svg)
    return (
        w.group(1) if w else None,
        h.group(1) if h else None,
        vb.group(1) if vb else None,
    )


def count_tags(svg: str, tag: str) -> int:
    return len(re.findall(rf"<{tag}[\s/>]", svg))


def main() -> int:
    hard_fail = 0
    print("Comparing fplot SVGs to matplotlib references")
    print("=" * 60)

    for name in CASES:
        ref_path = REF / f"{name}.svg"
        out_path = OUT / f"{name}.svg"
        print(f"\n[{name}]")

        if not ref_path.exists():
            print(f"  MISSING ref: {ref_path}")
            hard_fail += 1
            continue
        if not out_path.exists():
            print(f"  MISSING fplot: {out_path}")
            hard_fail += 1
            continue

        ref = ref_path.read_text(encoding="utf-8", errors="replace")
        out = out_path.read_text(encoding="utf-8", errors="replace")

        rw, rh, rvb = parse_canvas(ref)
        ow, oh, ovb = parse_canvas(out)

        print(f"  matplotlib canvas: width={rw} height={rh} viewBox={rvb}")
        print(f"  fplot canvas:      width={ow} height={oh} viewBox={ovb}")

        # The matplotlib reference defines the expected canvas, so cases with a
        # non-default figsize are checked against their own reference.
        def num(s: str | None) -> float | None:
            if s is None:
                return None
            m = re.match(r"([0-9.]+)", s)
            return float(m.group(1)) if m else None

        for label, a, b in [
            ("width", num(rw), num(ow)),
            ("height", num(rh), num(oh)),
        ]:
            if a is None:
                print(f"  HARD: could not read {label} from the matplotlib reference")
                hard_fail += 1
            elif b is None or abs(a - b) > 0.05:
                print(f"  HARD: fplot {label}={b} expected ~{a}")
                hard_fail += 1
            else:
                print(f"  ok: {label} matches expected {a}")

        print(
            f"  elements mpl:  line={count_tags(ref, 'path')} "
            f"(paths) text≈{ref.count('<text')}"
        )
        print(
            f"  elements fplot: polyline={count_tags(out, 'polyline')} "
            f"circle={count_tags(out, 'circle')} line={count_tags(out, 'line')} "
            f"text={out.count('<text')}"
        )

        # Axes count: matplotlib <g id="axes_N"> vs fplot clip paths.
        n_ax_mpl = len(re.findall(r'<g id="axes_\d+"', ref))
        n_ax_fplot = len(re.findall(r'<clipPath id="axclip', out))
        if n_ax_mpl == n_ax_fplot:
            print(f"  ok: axes count matches ({n_ax_mpl})")
        else:
            print(f"  HARD: axes count mpl={n_ax_mpl} fplot={n_ax_fplot}")
            hard_fail += 1
        print(f"  sizes: mpl={len(ref)} bytes, fplot={len(out)} bytes")

        # Optional raster compare
        try:
            import io

            from PIL import Image
            import cairosvg  # type: ignore

            def raster(svg_text: str) -> Image.Image:
                png = cairosvg.svg2png(bytestring=svg_text.encode("utf-8"), dpi=72)
                return Image.open(io.BytesIO(png)).convert("RGB")

            im_r = raster(ref)
            im_o = raster(out)
            if im_r.size != im_o.size:
                im_o = im_o.resize(im_r.size)
            import numpy as np

            a = np.asarray(im_r, dtype=np.float32)
            b = np.asarray(im_o, dtype=np.float32)
            mae = float(np.mean(np.abs(a - b)))
            print(f"  raster MAE (0-255): {mae:.2f}")
        except Exception as e:
            print(f"  raster compare skipped ({type(e).__name__}: {e})")

    print("\n" + "=" * 60)
    print("Known intentional differences:")
    print("  - matplotlib embeds DejaVu glyph paths; fplot uses <text>")
    print("  - matplotlib uses path IDs/metadata; fplot uses simpler SVG")
    print("  - tick locations may differ slightly (nice-number algorithm)")
    print("  - marker geometry is approximate")

    if hard_fail:
        print(f"\nFAILED with {hard_fail} hard error(s)")
        return 1
    print("\nOK: all cases present with matching canvas size")
    return 0


if __name__ == "__main__":
    sys.exit(main())
