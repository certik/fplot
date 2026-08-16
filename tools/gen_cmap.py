"""Regenerate src/fplot_cmap.f90 from matplotlib's colormaps.

The Fortran library has no way to evaluate matplotlib's colormap definitions,
so the tables are sampled here and interpolated at run time. Run this if a
colormap is added or the anchor count changes:

    pixi run python tools/gen_cmap.py
"""

from pathlib import Path

import matplotlib

# The order fixes the integer id of every map, and the first one is the
# default. Sequential first, then diverging, then the rest.
NAMES = [
    "viridis", "plasma", "inferno", "magma", "cividis", "gray",
    "Blues", "Greens", "Oranges", "Reds", "Purples", "Greys",
    "YlGnBu", "YlOrRd", "GnBu", "BuPu", "hot", "afmhot", "copper", "bone",
    "pink", "cool", "spring", "summer", "autumn", "winter",
    "coolwarm", "bwr", "seismic", "RdBu", "RdYlBu", "RdYlGn", "Spectral",
    "PiYG", "PRGn", "BrBG", "PuOr",
    "jet", "rainbow", "turbo", "hsv", "twilight", "cubehelix",
    "terrain", "ocean", "gist_earth", "nipy_spectral", "CMRmap", "gnuplot",
]
NA = 65
OUT = Path(__file__).resolve().parent.parent / "src" / "fplot_cmap.f90"

REST = r'''
contains

    ! matplotlib's name, with a "_r" suffix meaning the map read backwards.
    ! An unknown name falls back to viridis, matplotlib's default.
    pure function cmap_from_str(name) result(id)
        character(len=*), intent(in) :: name
        integer :: id, i, n
        character(len=32) :: c
        logical :: rev

        c = adjustl(name)
        n = len_trim(c)
        rev = .false.
        if (n > 2) then
            if (c(n - 1:n) == "_r") then
                rev = .true.
                n = n - 2
            end if
        end if

        id = CMAP_VIRIDIS
        do i = 1, CMAP_COUNT
            if (len_trim(NAMES(i)) == n) then
                if (c(1:n) == NAMES(i)(1:n)) then
                    id = i - 1
                    exit
                end if
            end if
        end do
        ! One alias matplotlib keeps for compatibility.
        if (c(1:n) == "grey") id = CMAP_GRAY
        if (rev) id = id + CMAP_REVERSED
    end function cmap_from_str

    pure function cmap_name(id) result(s)
        integer, intent(in) :: id
        character(len=32) :: s
        s = NAMES(modulo(id, CMAP_REVERSED) + 1)
        if (id >= CMAP_REVERSED) s = trim(s) // "_r"
    end function cmap_name

    ! Color at position t in [0, 1], linearly interpolated between anchors.
    pure subroutine cmap_rgb(id, t, r, g, b)
        integer, intent(in) :: id
        real(dp), intent(in) :: t
        integer, intent(out) :: r, g, b
        real(dp) :: u, f
        integer :: i0, i1, m, base

        m = id
        u = t
        ! A NaN would otherwise propagate into the index arithmetic below.
        if (u /= u) u = 0.0_dp
        if (m >= CMAP_REVERSED) then
            m = m - CMAP_REVERSED
            u = 1.0_dp - u
        end if
        if (m < 0 .or. m >= CMAP_COUNT) m = CMAP_VIRIDIS
        base = 3 * CMAP_N_ANCHOR * m

        u = max(0.0_dp, min(1.0_dp, u)) * real(CMAP_N_ANCHOR - 1, dp)
        i0 = int(u)
        if (i0 > CMAP_N_ANCHOR - 2) i0 = CMAP_N_ANCHOR - 2
        i1 = i0 + 1
        f = u - real(i0, dp)

        r = lerp(DATA(base + 3*i0 + 1), DATA(base + 3*i1 + 1), f)
        g = lerp(DATA(base + 3*i0 + 2), DATA(base + 3*i1 + 2), f)
        b = lerp(DATA(base + 3*i0 + 3), DATA(base + 3*i1 + 3), f)
    end subroutine cmap_rgb

    pure function cmap_color(id, t) result(hex)
        integer, intent(in) :: id
        real(dp), intent(in) :: t
        character(len=7) :: hex
        integer :: r, g, b
        call cmap_rgb(id, t, r, g, b)
        hex = "#" // hex2(r) // hex2(g) // hex2(b)
    end function cmap_color

    pure function lerp(a, b, f) result(v)
        integer, intent(in) :: a, b
        real(dp), intent(in) :: f
        integer :: v
        v = nint(real(a, dp) + f * real(b - a, dp))
        v = max(0, min(255, v))
    end function lerp

    pure function hex2(v) result(s)
        integer, intent(in) :: v
        character(len=2) :: s
        character(len=16), parameter :: D = "0123456789abcdef"
        integer :: w
        w = max(0, min(255, v))
        s(1:1) = D(w / 16 + 1:w / 16 + 1)
        s(2:2) = D(modulo(w, 16) + 1:modulo(w, 16) + 1)
    end function hex2

end module fplot_cmap
'''


def values(name: str) -> list[int]:
    m = matplotlib.colormaps[name]
    vals: list[int] = []
    for i in range(NA):
        r, g, b, _ = m(i / (NA - 1))
        vals += [round(r * 255), round(g * 255), round(b * 255)]
    return vals


def rows(vals: list[int]) -> list[str]:
    out: list[str] = []
    line: list[str] = []
    for v in vals:
        line.append(f"{v:4d}")
        if len(line) == 12:
            out.append(", ".join(line))
            line = []
    if line:
        out.append(", ".join(line))
    return out


def main() -> None:
    n = len(NAMES)
    width = max(len(x) for x in NAMES)
    flat: list[int] = []
    for name in NAMES:
        flat += values(name)

    head = [
        "! fplot_cmap - matplotlib colormaps for the image, contour and scatter kinds.",
        "!",
        f"! Generated by tools/gen_cmap.py; do not edit by hand. Each map is {NA} evenly",
        "! spaced anchors sampled from matplotlib and linearly interpolated in between,",
        "! which reproduces the smooth originals to within a level or two out of 255.",
        "module fplot_cmap",
        "    use fplot_style, only: dp",
        "    implicit none",
        "    private",
        "",
        "    public :: CMAP_VIRIDIS, CMAP_PLASMA, CMAP_INFERNO, CMAP_MAGMA",
        "    public :: CMAP_GRAY, CMAP_COOLWARM, CMAP_REVERSED",
        "    public :: cmap_from_str, cmap_color, cmap_rgb, cmap_name",
        "",
        f"    integer, parameter, public :: CMAP_N_ANCHOR = {NA}",
        f"    integer, parameter, public :: CMAP_COUNT = {n}",
        "",
        '    ! A reversed map ("viridis_r") is the same table read backwards, so it',
        "    ! is the same id with this flag added rather than a table of its own.",
        "    integer, parameter :: CMAP_REVERSED = 1000",
        "",
    ]
    for c in ["viridis", "plasma", "inferno", "magma", "gray", "coolwarm"]:
        head.append(f"    integer, parameter :: CMAP_{c.upper()} = {NAMES.index(c)}")
    head.append("")
    head.append(f"    character(len={width}), parameter :: NAMES(CMAP_COUNT) = [ &")
    for j, name in enumerate(NAMES):
        end = "]" if j == n - 1 else ", &"
        head.append(f'        "{name:<{width}}"{end}')
    head.append("")
    head.append("    ! The anchors of every map, one map after another.")
    head.append("    integer, parameter :: DATA(3 * CMAP_N_ANCHOR * CMAP_COUNT) = [ &")
    rs = rows(flat)
    for j, r in enumerate(rs):
        end = "]" if j == len(rs) - 1 else ", &"
        head.append(f"        {r}{end}")

    OUT.write_text("\n".join(head) + REST, encoding="utf-8")
    print(f"wrote {OUT} ({n} maps)")


if __name__ == "__main__":
    main()
