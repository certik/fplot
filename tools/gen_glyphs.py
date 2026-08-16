"""Regenerate src/fplot_glyphs.f90 from DejaVu Sans.

The vector backends hand the string to the format and let the viewer find
the font. PNG has no such luxury: it has to turn text into filled outlines
itself, so the outlines have to be in the library.

DejaVu Sans is matplotlib's default face, so using it is what makes fplot's
PNG text land in the same place as matplotlib's. Regular, bold and oblique
are emitted, and they are the real faces rather than a regular one smeared
or sheared, which is what matplotlib draws too.

TrueType curves are quadratic and the rendering API only speaks cubics, so
the quadratics are converted exactly on the way out. Coordinates are kept in
font units and scaled by the backend at draw time.

    pixi run python tools/gen_glyphs.py
"""

import os
from pathlib import Path

import matplotlib
from fontTools.pens.recordingPen import DecomposingRecordingPen
from fontTools.ttLib import TTFont

OUT = Path(__file__).resolve().parent.parent / "src" / "fplot_glyphs.f90"
TTFDIR = Path(os.path.dirname(matplotlib.__file__)) / "mpl-data" / "fonts" / "ttf"

# The order here is the face numbering the library uses: regular, bold,
# oblique, bold oblique.
FACES = [
    "DejaVuSans.ttf",
    "DejaVuSans-Bold.ttf",
    "DejaVuSans-Oblique.ttf",
    "DejaVuSans-BoldOblique.ttf",
]

# The table is a few contiguous ranges of code points, each with the set
# of places inside it worth carrying. Gaps are left empty, which costs
# four numbers each and keeps the lookup a subtraction.
#
# Latin-1 for text, and the symbols that turn up in axis labels: the
# degree sign a polar axes labels its angles with, and its neighbours.
# Greek because mathtext spells "\\alpha" and there is nothing else to
# draw it with.
GREEK = (set(range(0x391, 0x3AA)) | set(range(0x3B1, 0x3CA))) - {0x3A2}
BLOCKS = [
    (32, 255, set(range(32, 127)) | {0xB0, 0xB1, 0xB5, 0xD7, 0xF7}),
    (0x391, 0x3C9, GREEK),
]

VERB_MOVE, VERB_LINE, VERB_CUBIC, VERB_CLOSE = 1, 2, 3, 4


def quad_to_cubic(p0, q, p2):
    """Exact degree elevation of a quadratic Bezier to a cubic."""
    c1 = (p0[0] + 2.0 / 3.0 * (q[0] - p0[0]), p0[1] + 2.0 / 3.0 * (q[1] - p0[1]))
    c2 = (p2[0] + 2.0 / 3.0 * (q[0] - p2[0]), p2[1] + 2.0 / 3.0 * (q[1] - p2[1]))
    return c1, c2


def outline(glyphset, name):
    """Return (verbs, points) for one glyph, cubics only.

    Greek capitals are built out of Latin ones in DejaVu, so the pen has to
    be one that resolves a component into the outline it stands for.
    """
    pen = DecomposingRecordingPen(glyphset)
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


def block_offsets():
    """Each block with the index it starts at in the per-face table."""
    out = []
    off = 0
    for first, last, _ in BLOCKS:
        out.append((first, last, off))
        off += last - first + 1
    return out


def wrap(values, per_line, indent):
    """Fortran continuation-friendly wrapping of an array constructor body."""
    out = []
    for i in range(0, len(values), per_line):
        chunk = ", ".join(values[i : i + per_line])
        cont = ", &" if i + per_line < len(values) else " &"
        out.append(f"{indent}{chunk}{cont}")
    return out


def face_tables(path):
    """Per-character tables for one face."""
    font = TTFont(path)
    hmtx = font["hmtx"]
    cmap = font.getBestCmap()
    glyphset = font.getGlyphSet()

    verbs_all: list[int] = []
    xs: list[float] = []
    ys: list[float] = []
    vbeg, vlen, pbeg, adv = [], [], [], []

    codes = [c for first, last, _ in BLOCKS for c in range(first, last + 1)]
    keep = set().union(*[k for _, _, k in BLOCKS])

    for code in codes:
        if code not in keep:
            vbeg.append(len(verbs_all) + 1)
            vlen.append(0)
            pbeg.append(len(xs) + 1)
            adv.append(0)
            continue
        name = cmap.get(code)
        if name is None:
            raise SystemExit(f"{path.name} has no glyph for U+{code:04X}")
        verbs, pts = outline(glyphset, name)
        vbeg.append(len(verbs_all) + 1)
        vlen.append(len(verbs))
        pbeg.append(len(xs) + 1)
        adv.append(hmtx[name][0])
        verbs_all.extend(verbs)
        xs.extend(p[0] for p in pts)
        ys.extend(p[1] for p in pts)

    return font, glyphset, cmap, vbeg, vlen, pbeg, adv, verbs_all, xs, ys


def main() -> None:
    all_verbs: list[int] = []
    all_x: list[float] = []
    all_y: list[float] = []
    vbeg, vlen, pbeg, adv = [], [], [], []
    upem = asc = desc = xh = None

    for face in FACES:
        f, gs, cm, vb, vl, pb, ad, vv, xx, yy = face_tables(TTFDIR / face)
        # Offsets are cumulative so that the four faces sit in one flat table
        # and a face is nothing more than a stride into it.
        vbeg += [v + len(all_verbs) for v in vb]
        pbeg += [p + len(all_x) for p in pb]
        vlen += vl
        adv += ad
        all_verbs += vv
        all_x += xx
        all_y += yy
        if upem is None:
            upem = f["head"].unitsPerEm
            asc = f["hhea"].ascent
            desc = f["hhea"].descent
            # sxHeight only exists in OS/2 version 2 and later; DejaVu
            # predates it, so take it from the top of the "x" outline,
            # which is its definition.
            xh = max(pt[1] for pt in outline(gs, cm[ord("x")])[1])

    nv, np_ = len(all_verbs), len(all_x)
    nface = len(FACES)
    nch = sum(last - first + 1 for first, last, _ in BLOCKS)

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
        "!",
        "! Four faces are stacked in one table: regular, bold, oblique and bold",
        "! oblique, selected by the face argument.",
        "",
        "module fplot_glyphs",
        "    use fplot_style, only: dp",
        "    implicit none",
        "    private",
        "",
        "    public :: EM, ASCENT, DESCENT, XHEIGHT",
        "    public :: FACE_REGULAR, FACE_BOLD, FACE_OBLIQUE, FACE_BOLD_OBLIQUE",
        "    public :: glyph_advance, glyph_verbs, glyph_points",
        "",
        f"    real(dp), parameter :: EM = {float(upem)}_dp",
        f"    real(dp), parameter :: ASCENT = {float(asc)}_dp",
        f"    real(dp), parameter :: DESCENT = {float(desc)}_dp",
        f"    real(dp), parameter :: XHEIGHT = {float(xh)}_dp",

        "",
        "    integer, parameter :: FACE_REGULAR = 1",
        "    integer, parameter :: FACE_BOLD = 2",
        "    integer, parameter :: FACE_OBLIQUE = 3",
        "    integer, parameter :: FACE_BOLD_OBLIQUE = 4",
        "",
        f"    integer, parameter :: NCH = {nch}",
        f"    integer, parameter :: NFACE = {nface}",
        f"    integer, parameter :: NV = {nv}",
        f"    integer, parameter :: NP = {np_}",
        "",
        "    ! Per character and face: where its verbs and points start, how",
        "    ! many verbs, and the advance width to the next character.",
        "    integer, parameter :: VBEG(NCH*NFACE) = [ &",
    ]
    L += wrap([str(v) for v in vbeg], 12, "        ")
    L += ["        ]", "", "    integer, parameter :: VLEN(NCH*NFACE) = [ &"]
    L += wrap([str(v) for v in vlen], 12, "        ")
    L += ["        ]", "", "    integer, parameter :: PBEG(NCH*NFACE) = [ &"]
    L += wrap([str(v) for v in pbeg], 12, "        ")
    L += ["        ]", "", "    integer, parameter :: ADV(NCH*NFACE) = [ &"]
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
        "    ! Index into the flat tables. Code points outside the table are",
        "    ! treated as spaces so that an unexpected one cannot crash a plot,",
        "    ! and an unknown face falls back on the regular one.",
        "    pure integer function slot(c, face)",
        "        integer, intent(in) :: c",
        "        integer, intent(in), optional :: face",
        "        integer :: f, i",
        "        f = FACE_REGULAR",
        "        if (present(face)) f = face",
        "        if (f < 1 .or. f > NFACE) f = FACE_REGULAR",
        "        i = 1",
    ] + [
        f"        if (c >= {first} .and. c <= {last}) i = c - {first} + {off + 1}"
        for first, last, off in block_offsets()
    ] + [
        "        slot = (f - 1)*NCH + i",
        "    end function slot",
        "",
        "    ! Advance width in font units.",
        "    function glyph_advance(c, face) result(a)",
        "        integer, intent(in) :: c",
        "        integer, intent(in), optional :: face",
        "        real(dp) :: a",
        "        a = real(ADV(slot(c, face)), dp)",
        "    end function glyph_advance",
        "",
        "    ! The verbs for one character, empty when there is nothing to draw.",
        "    subroutine glyph_verbs(c, v, n, face)",
        "        integer, intent(in) :: c",
        "        integer, allocatable, intent(out) :: v(:)",
        "        integer, intent(out) :: n",
        "        integer, intent(in), optional :: face",
        "        integer :: i",
        "        i = slot(c, face)",
        "        n = VLEN(i)",
        "        allocate (v(n))",
        "        if (n > 0) v = VERBS(VBEG(i):VBEG(i) + n - 1)",
        "    end subroutine glyph_verbs",
        "",
        "    ! The points for one character, in font units.",
        "    subroutine glyph_points(c, x, y, n, face)",
        "        integer, intent(in) :: c",
        "        real(dp), allocatable, intent(out) :: x(:), y(:)",
        "        integer, intent(out) :: n",
        "        integer, intent(in), optional :: face",
        "        integer :: i, j, nv_, k, p0",
        "        i = slot(c, face)",
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
