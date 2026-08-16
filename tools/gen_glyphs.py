"""Regenerate src/fplot_glyphs.f90 from DejaVu Sans.

The vector backends hand the string to the format and let the viewer find
the font. PNG has no such luxury: it has to turn text into filled outlines
itself, so the outlines have to be in the library.

DejaVu Sans is matplotlib's default face, so using it is what makes fplot's
PNG text land in the same place as matplotlib's. Only the regular weight is
emitted because that is the only face fplot ever asks for; if bold or italic
are ever exposed, add them here rather than synthesizing them.

TrueType curves are quadratic and the rendering API only speaks cubics, so
the quadratics are converted exactly on the way out. Coordinates are kept in
font units and scaled by the backend at draw time.

    pixi run python tools/gen_glyphs.py
"""

import os
from pathlib import Path

import matplotlib
from fontTools.pens.recordingPen import RecordingPen
from fontTools.ttLib import TTFont

OUT = Path(__file__).resolve().parent.parent / "src" / "fplot_glyphs.f90"
TTF = (
    Path(os.path.dirname(matplotlib.__file__))
    / "mpl-data"
    / "fonts"
    / "ttf"
    / "DejaVuSans.ttf"
)

# ASCII, and then the Latin-1 places that carry a symbol worth having:
# the degree sign a polar axes labels its angles with, and the few others
# that turn up in axis labels. The gaps between them are left empty, which
# costs four numbers each and keeps the table a plain contiguous lookup.
FIRST, LAST = 32, 255
KEEP = set(range(32, 127)) | {0xB0, 0xB1, 0xB5, 0xD7, 0xF7}

VERB_MOVE, VERB_LINE, VERB_CUBIC, VERB_CLOSE = 1, 2, 3, 4


def quad_to_cubic(p0, q, p2):
    """Exact degree elevation of a quadratic Bezier to a cubic."""
    c1 = (p0[0] + 2.0 / 3.0 * (q[0] - p0[0]), p0[1] + 2.0 / 3.0 * (q[1] - p0[1]))
    c2 = (p2[0] + 2.0 / 3.0 * (q[0] - p2[0]), p2[1] + 2.0 / 3.0 * (q[1] - p2[1]))
    return c1, c2


def outline(glyphset, name):
    """Return (verbs, points) for one glyph, cubics only."""
    pen = RecordingPen()
    glyphset[name].draw(pen)

    verbs: list[int] = []
    pts: list[tuple[float, float]] = []
    cur = (0.0, 0.0)
    start = (0.0, 0.0)

    for op, args in pen.value:
        if op == "moveTo":
            cur = start = args[0]
            verbs.append(VERB_MOVE)
            pts.append(cur)
        elif op == "lineTo":
            cur = args[0]
            verbs.append(VERB_LINE)
            pts.append(cur)
        elif op == "curveTo":
            for i in range(0, len(args), 3):
                c1, c2, p = args[i], args[i + 1], args[i + 2]
                verbs.append(VERB_CUBIC)
                pts.extend([c1, c2, p])
                cur = p
        elif op == "qCurveTo":
            # TrueType packs consecutive off-curve points with the on-curve
            # point between them implied at the midpoint. A trailing None
            # means the contour is entirely off-curve and wraps around.
            pl = list(args)
            if pl[-1] is None:
                pl = pl[:-1]
                mid = (
                    (pl[0][0] + pl[-1][0]) / 2.0,
                    (pl[0][1] + pl[-1][1]) / 2.0,
                )
                cur = start = mid
                verbs.append(VERB_MOVE)
                pts.append(cur)
                pl = pl + [mid]
            for i in range(len(pl) - 1):
                q = pl[i]
                nxt = pl[i + 1]
                end = nxt if i == len(pl) - 2 else ((q[0] + nxt[0]) / 2.0, (q[1] + nxt[1]) / 2.0)
                c1, c2 = quad_to_cubic(cur, q, end)
                verbs.append(VERB_CUBIC)
                pts.extend([c1, c2, end])
                cur = end
        elif op == "closePath":
            verbs.append(VERB_CLOSE)
            cur = start
        elif op == "endPath":
            pass
        else:
            raise SystemExit(f"unhandled pen op {op}")

    return verbs, pts


def wrap(values, per_line, indent):
    """Fortran continuation-friendly wrapping of an array constructor body."""
    out = []
    for i in range(0, len(values), per_line):
        chunk = ", ".join(values[i : i + per_line])
        cont = ", &" if i + per_line < len(values) else " &"
        out.append(f"{indent}{chunk}{cont}")
    return out


def main() -> None:
    font = TTFont(TTF)
    upem = font["head"].unitsPerEm
    hmtx = font["hmtx"]
    cmap = font.getBestCmap()
    glyphset = font.getGlyphSet()

    asc = font["hhea"].ascent
    desc = font["hhea"].descent
    # sxHeight only exists in OS/2 version 2 and later; DejaVu predates it,
    # so take it from the top of the "x" outline, which is its definition.
    xh = max(pt[1] for pt in outline(glyphset, cmap[ord("x")])[1])

    all_verbs: list[int] = []
    all_x: list[float] = []
    all_y: list[float] = []
    vbeg, vlen, pbeg, adv = [], [], [], []

    for code in range(FIRST, LAST + 1):
        if code not in KEEP:
            vbeg.append(len(all_verbs) + 1)
            vlen.append(0)
            pbeg.append(len(all_x) + 1)
            adv.append(0)
            continue
        name = cmap.get(code)
        if name is None:
            raise SystemExit(f"DejaVu Sans has no glyph for U+{code:04X}")
        verbs, pts = outline(glyphset, name)
        vbeg.append(len(all_verbs) + 1)
        vlen.append(len(verbs))
        pbeg.append(len(all_x) + 1)
        adv.append(hmtx[name][0])
        all_verbs.extend(verbs)
        all_x.extend(p[0] for p in pts)
        all_y.extend(p[1] for p in pts)

    nv, np_ = len(all_verbs), len(all_x)

    L = [
        "! fplot_glyphs — DejaVu Sans outlines, generated by tools/gen_glyphs.py.",
        "!",
        "! Do not edit. The PNG backend fills text as outlines because a raster",
        "! file cannot defer to a font the way SVG and PDF do, and these are the",
        "! outlines matplotlib itself draws with.",
        "!",
        "! Coordinates are in font units; divide by EM and multiply by the point",
        "! size. Curves are cubic, matching fplot_render's path verbs, so the",
        "! backend fills a glyph with exactly the code that fills any other path.",
        "",
        "module fplot_glyphs",
        "    use fplot_style, only: dp",
        "    implicit none",
        "    private",
        "",
        "    public :: EM, ASCENT, DESCENT, XHEIGHT, GLYPH_FIRST, GLYPH_LAST",
        "    public :: glyph_advance, glyph_verbs, glyph_points",
        "",
        f"    real(dp), parameter :: EM = {float(upem)}_dp",
        f"    real(dp), parameter :: ASCENT = {float(asc)}_dp",
        f"    real(dp), parameter :: DESCENT = {float(desc)}_dp",
        f"    real(dp), parameter :: XHEIGHT = {float(xh)}_dp",
        f"    integer, parameter :: GLYPH_FIRST = {FIRST}",
        f"    integer, parameter :: GLYPH_LAST = {LAST}",
        "",
        f"    integer, parameter :: NCH = {LAST - FIRST + 1}",
        f"    integer, parameter :: NV = {nv}",
        f"    integer, parameter :: NP = {np_}",
        "",
        "    ! Per character: where its verbs and points start, how many verbs,",
        "    ! and the advance width to the next character.",
        "    integer, parameter :: VBEG(NCH) = [ &",
    ]
    L += wrap([str(v) for v in vbeg], 12, "        ")
    L += ["        ]", "", "    integer, parameter :: VLEN(NCH) = [ &"]
    L += wrap([str(v) for v in vlen], 12, "        ")
    L += ["        ]", "", "    integer, parameter :: PBEG(NCH) = [ &"]
    L += wrap([str(v) for v in pbeg], 12, "        ")
    L += ["        ]", "", "    integer, parameter :: ADV(NCH) = [ &"]
    L += wrap([str(v) for v in adv], 12, "        ")
    L += ["        ]", "", "    integer, parameter :: VERBS(NV) = [ &"]
    L += wrap([str(v) for v in all_verbs], 25, "        ")
    L += ["        ]", "", "    real(dp), parameter :: PX(NP) = [ &"]
    L += wrap([f"{v:.1f}_dp" for v in all_x], 8, "        ")
    L += ["        ]", "", "    real(dp), parameter :: PY(NP) = [ &"]
    L += wrap([f"{v:.1f}_dp" for v in all_y], 8, "        ")
    L += [
        "        ]",
        "",
        "contains",
        "",
        "    ! Advance width in font units. Characters outside the table are",
        "    ! treated as spaces so that an unexpected byte cannot crash a plot.",
        "    function glyph_advance(c) result(a)",
        "        integer, intent(in) :: c",
        "        real(dp) :: a",
        "        if (c < GLYPH_FIRST .or. c > GLYPH_LAST) then",
        "            a = real(ADV(1), dp)",
        "        else",
        "            a = real(ADV(c - GLYPH_FIRST + 1), dp)",
        "        end if",
        "    end function glyph_advance",
        "",
        "    ! The verbs for one character, empty when there is nothing to draw.",
        "    subroutine glyph_verbs(c, v, n)",
        "        integer, intent(in) :: c",
        "        integer, allocatable, intent(out) :: v(:)",
        "        integer, intent(out) :: n",
        "        integer :: i",
        "        if (c < GLYPH_FIRST .or. c > GLYPH_LAST) then",
        "            n = 0",
        "            allocate (v(0))",
        "            return",
        "        end if",
        "        i = c - GLYPH_FIRST + 1",
        "        n = VLEN(i)",
        "        allocate (v(n))",
        "        if (n > 0) v = VERBS(VBEG(i):VBEG(i) + n - 1)",
        "    end subroutine glyph_verbs",
        "",
        "    ! The points for one character, in font units.",
        "    subroutine glyph_points(c, x, y, n)",
        "        integer, intent(in) :: c",
        "        real(dp), allocatable, intent(out) :: x(:), y(:)",
        "        integer, intent(out) :: n",
        "        integer :: i, j, nv_, k, p0",
        "        if (c < GLYPH_FIRST .or. c > GLYPH_LAST) then",
        "            n = 0",
        "            allocate (x(0), y(0))",
        "            return",
        "        end if",
        "        i = c - GLYPH_FIRST + 1",
        "        nv_ = VLEN(i)",
        "        n = 0",
        "        do j = 1, nv_",
        "            k = VERBS(VBEG(i) + j - 1)",
        "            if (k == 1 .or. k == 2) n = n + 1",
        "            if (k == 3) n = n + 3",
        "        end do",
        "        allocate (x(n), y(n))",
        "        p0 = PBEG(i)",
        "        if (n > 0) then",
        "            x = PX(p0:p0 + n - 1)",
        "            y = PY(p0:p0 + n - 1)",
        "        end if",
        "    end subroutine glyph_points",
        "",
        "end module fplot_glyphs",
        "",
    ]

    OUT.write_text("\n".join(L))
    print(f"wrote {OUT} ({nv} verbs, {np_} points)")


if __name__ == "__main__":
    main()
