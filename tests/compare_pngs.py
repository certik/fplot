"""Compare fplot's PNGs to matplotlib's, pixel by pixel.

The SVG comparison next door checks structure, because two renderers can
draw the same SVG differently and a bit comparison would be meaningless. For
PNG there is no such excuse: fplot rasterizes the figure itself, so it can be
held to the actual pixels matplotlib produces.

The figures are not expected to be identical. fplot lays out independently, so
a tick can land a fraction of a point away from matplotlib's, and after pixel
snapping a fraction of a point becomes a whole pixel. Text is the other source
of difference: matplotlib hints glyphs through FreeType and fplot fills the
outlines unhinted, so stems land on slightly different pixels. What this test
is for is catching the errors that are not sub-pixel: a missing element, a
wrong colour, a shape drawn in the wrong place.

    pixi run compare-png
"""

import sys
from pathlib import Path

import numpy as np
from PIL import Image

ROOT = Path(__file__).resolve().parent
REF = ROOT / "refs"
OUT = ROOT / "out"

# Mean absolute channel difference, out of 255, averaged over the image. At
# these levels the figures are visually indistinguishable side by side; the
# number is dominated by antialiasing along shared edges.
MEAN_LIMIT = 8.0

# Cases whose canvas fplot sizes itself, where a small disagreement about the
# bounding box is expected rather than a fault.
SIZE_TOLERANCE = {"savefig_tight": 0.02}
DEFAULT_SIZE_TOLERANCE = 0.0


def compare(name: str, ref: Path, out: Path) -> tuple[float, str]:
    a = np.asarray(Image.open(ref).convert("RGB")).astype(np.int16)
    b = np.asarray(Image.open(out).convert("RGB")).astype(np.int16)

    if a.shape != b.shape:
        tol = SIZE_TOLERANCE.get(name, DEFAULT_SIZE_TOLERANCE)
        dh = abs(a.shape[0] - b.shape[0]) / a.shape[0]
        dw = abs(a.shape[1] - b.shape[1]) / a.shape[1]
        if dh > tol or dw > tol:
            return -1.0, f"size {b.shape[1]}x{b.shape[0]} vs {a.shape[1]}x{a.shape[0]}"
        # Same figure, marginally different crop: compare the shared region.
        h = min(a.shape[0], b.shape[0])
        w = min(a.shape[1], b.shape[1])
        a, b = a[:h, :w], b[:h, :w]

    d = np.abs(a - b)
    return float(d.mean()), "%.1f%% of pixels differ by >32" % (
        (d.max(axis=2) > 32).mean() * 100
    )


def main() -> int:
    if not REF.exists():
        print("no matplotlib references; run: pixi run mpl-refs")
        return 1

    results = []
    missing = []
    for out in sorted(OUT.glob("*.png")):
        ref = REF / out.name
        if not ref.exists():
            missing.append(out.name)
            continue
        results.append((out.name, *compare(out.stem, ref, out)))

    if not results:
        print("no PNGs to compare; run: pixi run test-flang")
        return 1

    print("Comparing fplot PNGs to matplotlib references\n")
    results.sort(key=lambda r: -r[1])
    failed = [r for r in results if r[1] < 0 or r[1] > MEAN_LIMIT]

    for name, mean, note in results[:10]:
        flag = "FAIL" if (mean < 0 or mean > MEAN_LIMIT) else "ok  "
        shown = "n/a" if mean < 0 else "%5.2f" % mean
        print(f"  {flag} {name:<24} mean={shown}  {note}")

    good = [r[1] for r in results if r[1] >= 0]
    print(f"\n  {len(results)} cases, mean absolute difference {np.mean(good):.2f}/255")

    if missing:
        print(f"\n  no reference for: {', '.join(missing)}")
    if failed:
        print(f"\nFAIL: {len(failed)} case(s) over the limit of {MEAN_LIMIT}")
        return 1
    print("\nOK: every case within tolerance")
    return 0


if __name__ == "__main__":
    sys.exit(main())
