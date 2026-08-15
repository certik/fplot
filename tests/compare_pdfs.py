"""Compare fplot's PDFs to matplotlib's by rasterizing both.

A PDF cannot be compared as bytes and is not worth comparing as structure:
two files can describe the same page with completely different operators. So
both sides are handed to the same rasterizer and the resulting images are
compared, which asks the only question that matters, whether the page looks
the same.

Text is the expected difference and it is a real one, not an artifact of the
measurement. Matplotlib embeds a subset of DejaVu Sans and draws text with
the same outlines it uses everywhere else. fplot's PDF backend refers to the
base-14 Helvetica instead, so the file needs no embedded font at all, but the
glyphs are a different typeface and are spaced by Helvetica's widths while
the surrounding layout was computed from DejaVu's. Everything that is not
text should agree closely, and the per-case limit is set to catch it when it
does not.

    pixi run compare-pdf
"""

import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

import numpy as np
from PIL import Image

ROOT = Path(__file__).resolve().parent
REF = ROOT / "refs"
OUT = ROOT / "out"

DPI = 100

# Mean absolute channel difference out of 255. Looser than the PNG limit
# because a whole typeface differs, not just its hinting.
MEAN_LIMIT = 12.0

SIZE_TOLERANCE = {"savefig_tight": 0.02}


def render(pdf: Path, tmp: Path) -> np.ndarray | None:
    stem = str(tmp / pdf.stem)
    r = subprocess.run(
        ["pdftoppm", "-r", str(DPI), "-png", "-singlefile", str(pdf), stem],
        capture_output=True,
    )
    png = Path(stem + ".png")
    if r.returncode != 0 or not png.exists():
        return None
    return np.asarray(Image.open(png).convert("RGB")).astype(np.int16)


def main() -> int:
    if shutil.which("pdftoppm") is None:
        print("pdftoppm not found; it comes from the poppler dependency")
        return 1
    if not REF.exists():
        print("no matplotlib references; run: pixi run mpl-refs")
        return 1

    results = []
    broken = []
    with tempfile.TemporaryDirectory() as td:
        # Reference and output share a stem, so they are rendered into
        # separate directories to keep pdftoppm from overwriting one with
        # the other.
        ours = Path(td) / "out"
        theirs = Path(td) / "ref"
        ours.mkdir()
        theirs.mkdir()
        for out in sorted(OUT.glob("*.pdf")):
            ref = REF / out.name
            if not ref.exists():
                continue
            b = render(out, ours)
            a = render(ref, theirs)
            if b is None:
                broken.append(out.name)
                continue
            if a is None:
                continue
            if a.shape != b.shape:
                tol = SIZE_TOLERANCE.get(out.stem, 0.0)
                dh = abs(a.shape[0] - b.shape[0]) / a.shape[0]
                dw = abs(a.shape[1] - b.shape[1]) / a.shape[1]
                if dh > tol or dw > tol:
                    results.append((out.name, -1.0, f"size {b.shape} vs {a.shape}"))
                    continue
                h, w = min(a.shape[0], b.shape[0]), min(a.shape[1], b.shape[1])
                a, b = a[:h, :w], b[:h, :w]
            d = np.abs(a - b)
            results.append(
                (
                    out.name,
                    float(d.mean()),
                    "%.1f%% of pixels differ by >32" % ((d.max(axis=2) > 32).mean() * 100),
                )
            )

    if broken:
        print(f"FAIL: {len(broken)} PDF(s) could not be rendered at all:")
        for n in broken:
            print(f"  {n}")
        return 1
    if not results:
        print("no PDFs to compare; run: pixi run test-flang")
        return 1

    print("Comparing fplot PDFs to matplotlib references\n")
    results.sort(key=lambda r: -r[1])
    failed = [r for r in results if r[1] < 0 or r[1] > MEAN_LIMIT]

    for name, mean, note in results[:10]:
        flag = "FAIL" if (mean < 0 or mean > MEAN_LIMIT) else "ok  "
        shown = "n/a" if mean < 0 else "%5.2f" % mean
        print(f"  {flag} {name:<24} mean={shown}  {note}")

    good = [r[1] for r in results if r[1] >= 0]
    print(f"\n  {len(results)} cases, mean absolute difference {np.mean(good):.2f}/255")

    if failed:
        print(f"\nFAIL: {len(failed)} case(s) over the limit of {MEAN_LIMIT}")
        return 1
    print("\nOK: every case within tolerance")
    return 0


if __name__ == "__main__":
    sys.exit(main())
