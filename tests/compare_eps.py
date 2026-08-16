"""Compare fplot's EPS files to matplotlib's by rasterizing both.

Same argument as tests/compare_pdfs.py: PostScript is a program, not a
picture, so two files that draw the same page share almost nothing textually.
Both sides go through Ghostscript and the images are compared.

Two differences from the PDF comparison are expected and are properties of
the format, not defects. Text is a different typeface, because matplotlib
embeds a DejaVu Sans subset and fplot names the base-13 Helvetica; and
PostScript has no transparency, so fplot composites every alpha against the
page colour while matplotlib simply drops it. The limit below is set to
allow both and still catch a page that is actually drawn wrongly.

    pixi run compare-eps
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

# Mean absolute channel difference out of 255. Looser than the PDF limit
# because flattened alpha shifts large filled areas, not just glyphs.
MEAN_LIMIT = 20.0

SIZE_TOLERANCE = {"savefig_tight": 0.02}

# Cases where the whole page is dominated by translucent fills. matplotlib
# draws them fully opaque, since PostScript cannot blend; fplot composites
# them against the page colour instead, so the figure keeps the appearance it
# has in SVG and PNG. That is a deliberate improvement, not a defect, and it
# is what these two cases measure, so they get their own limits.
LIMIT = {"spans": 40.0, "hist_opts": 32.0}


def limit(stem: str) -> float:
    return LIMIT.get(stem, MEAN_LIMIT)


def render(eps: Path, tmp: Path) -> np.ndarray | None:
    png = tmp / (eps.stem + ".png")
    r = subprocess.run(
        [
            "gs", "-q", "-dNOPAUSE", "-dBATCH", "-dSAFER", "-dEPSCrop",
            "-sDEVICE=png16m", "-r%d" % DPI, "-dTextAlphaBits=4",
            "-dGraphicsAlphaBits=4", "-sOutputFile=" + str(png), str(eps),
        ],
        capture_output=True,
    )
    if r.returncode != 0 or not png.exists():
        return None
    return np.asarray(Image.open(png).convert("RGB")).astype(np.int16)


def main() -> int:
    if shutil.which("gs") is None:
        print("gs not found; it comes from the ghostscript dependency")
        return 1
    if not REF.exists():
        print("no matplotlib references; run: pixi run mpl-refs")
        return 1

    results = []
    broken = []
    with tempfile.TemporaryDirectory() as td:
        ours = Path(td) / "out"
        theirs = Path(td) / "ref"
        ours.mkdir()
        theirs.mkdir()
        for out in sorted(OUT.glob("*.eps")):
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
        print(f"FAIL: {len(broken)} EPS file(s) could not be rendered at all:")
        for n in broken:
            print(f"  {n}")
        return 1
    if not results:
        print("no EPS files to compare; run: pixi run test-flang")
        return 1

    print("Comparing fplot EPS files to matplotlib references\n")
    results.sort(key=lambda r: -r[1])
    failed = [r for r in results if r[1] < 0 or r[1] > limit(Path(r[0]).stem)]

    for name, mean, note in results[:10]:
        flag = "FAIL" if (mean < 0 or mean > limit(Path(name).stem)) else "ok  "
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
