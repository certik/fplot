"""Compare fplot's animated GIF to matplotlib's, frame by frame.

The two files will never be byte-identical: both sides pick their own
256-colour palette, and fplot picks one palette for the whole animation
where Pillow picks one per frame. So the comparison decodes both and asks
the only question worth asking, whether frame k looks like frame k.

    pixi run compare-gif
"""

import sys
from pathlib import Path

import numpy as np
from PIL import Image, ImageSequence

ROOT = Path(__file__).resolve().parent
REF = ROOT / "refs"
OUT = ROOT / "out"

# Mean absolute channel difference out of 255, per frame. Looser than the
# PNG limit because a shared 256-colour palette has to cover every frame.
MEAN_LIMIT = 12.0


def frames(path):
    im = Image.open(path)
    return [np.asarray(f.convert("RGB")).astype(np.int16)
            for f in ImageSequence.Iterator(im)]


def main() -> int:
    failed = 0
    total = 0
    means = []
    for out in sorted(OUT.glob("*.gif")):
        ref = REF / out.name
        if not ref.exists():
            continue
        a = frames(ref)
        b = frames(out)
        total += 1
        if len(a) != len(b):
            print(f"  FAIL {out.name}: {len(b)} frames vs {len(a)}")
            failed += 1
            continue
        if a[0].shape != b[0].shape:
            print(f"  FAIL {out.name}: size {b[0].shape} vs {a[0].shape}")
            failed += 1
            continue
        worst = max(float(np.abs(x - y).mean()) for x, y in zip(a, b))
        means.append(worst)
        flag = "FAIL" if worst > MEAN_LIMIT else "ok  "
        if worst > MEAN_LIMIT:
            failed += 1
        print(f"  {flag} {out.name:<24} {len(b)} frames, worst frame "
              f"mean={worst:5.2f}")

    if total == 0:
        print("no GIFs to compare; run: pixi run test-flang")
        return 1
    print(f"\n  {total} animation(s)")
    if failed:
        print(f"\nFAIL: {failed} over the limit of {MEAN_LIMIT}")
        return 1
    print("\nOK: every frame within tolerance")
    return 0


if __name__ == "__main__":
    sys.exit(main())
