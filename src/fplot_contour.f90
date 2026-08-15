! fplot_contour — the geometry behind contour and contourf.
!
! Each grid cell is split into two triangles and the work is done per triangle.
! Linear interpolation is exact on a triangle, so a level crossing is always a
! single segment and a band is always one convex polygon. The usual marching
! squares saddle ambiguity, where a cell straddling a level has two equally
! valid resolutions, simply cannot arise.
module fplot_contour
    use fplot_style, only: dp
    use fplot_ticks, only: nice_number
    implicit none
    private

    public :: tri_level, tri_band, contour_levels, MAX_POLY

    ! A triangle clipped by two half planes gains at most two vertices.
    integer, parameter :: MAX_POLY = 8

contains

    ! Round levels spanning the data, the way matplotlib picks them when the
    ! caller gives none. Unlike a tick locator this must cover the data, so the
    ! outermost levels sit just outside it rather than just inside.
    pure subroutine contour_levels(zmin, zmax, nbins, lev, nlev)
        real(dp), intent(in) :: zmin, zmax
        integer, intent(in) :: nbins
        real(dp), intent(out) :: lev(:)
        integer, intent(out) :: nlev
        real(dp) :: step, lo
        integer :: i

        step = nice_number((zmax - zmin) / real(nbins, dp), .true.)
        if (step <= 0.0_dp) step = 1.0_dp
        lo = floor(zmin / step) * step

        nlev = 0
        do i = 0, size(lev) - 1
            nlev = nlev + 1
            lev(nlev) = lo + real(i, dp) * step
            if (lev(nlev) >= zmax) exit
        end do
    end subroutine contour_levels

    ! Segment where the plane through the triangle crosses lev. Returns ns = 0
    ! when the level misses the triangle, otherwise ns = 2 endpoints.
    pure subroutine tri_level(px, py, pv, lev, sx, sy, ns)
        real(dp), intent(in) :: px(3), py(3), pv(3), lev
        real(dp), intent(out) :: sx(2), sy(2)
        integer, intent(out) :: ns
        integer :: i, j
        real(dp) :: t

        ns = 0
        do i = 1, 3
            j = merge(1, i + 1, i == 3)
            ! A vertex exactly on the level would otherwise be emitted twice,
            ! once for each incident edge, so only the ascending side counts.
            if ((pv(i) < lev .and. pv(j) >= lev) .or. &
                (pv(j) < lev .and. pv(i) >= lev)) then
                t = (lev - pv(i)) / (pv(j) - pv(i))
                if (ns < 2) then
                    ns = ns + 1
                    sx(ns) = px(i) + t * (px(j) - px(i))
                    sy(ns) = py(i) + t * (py(j) - py(i))
                end if
            end if
        end do
        if (ns /= 2) ns = 0
    end subroutine tri_level

    ! Part of the triangle where lo <= f <= hi, as a convex polygon.
    pure subroutine tri_band(px, py, pv, lo, hi, qx, qy, nq)
        real(dp), intent(in) :: px(3), py(3), pv(3), lo, hi
        real(dp), intent(out) :: qx(MAX_POLY), qy(MAX_POLY)
        integer, intent(out) :: nq
        real(dp) :: ax(MAX_POLY), ay(MAX_POLY), av(MAX_POLY)
        real(dp) :: bx(MAX_POLY), by(MAX_POLY), bv(MAX_POLY)
        integer :: na, nb

        na = 3
        ax(1:3) = px
        ay(1:3) = py
        av(1:3) = pv

        call clip_half(ax, ay, av, na, lo, .true., bx, by, bv, nb)
        call clip_half(bx, by, bv, nb, hi, .false., qx, qy, av, nq)
    end subroutine tri_band

    ! Sutherland-Hodgman against a single half plane in the value field,
    ! keeping f >= bound when keep_above is set and f <= bound otherwise.
    pure subroutine clip_half(px, py, pv, np, bound, keep_above, qx, qy, qv, nq)
        real(dp), intent(in) :: px(MAX_POLY), py(MAX_POLY), pv(MAX_POLY), bound
        integer, intent(in) :: np
        logical, intent(in) :: keep_above
        real(dp), intent(out) :: qx(MAX_POLY), qy(MAX_POLY), qv(MAX_POLY)
        integer, intent(out) :: nq
        integer :: i, j
        logical :: in_i, in_j
        real(dp) :: t

        nq = 0
        if (np < 3) return
        do i = 1, np
            j = merge(1, i + 1, i == np)
            in_i = inside(pv(i), bound, keep_above)
            in_j = inside(pv(j), bound, keep_above)

            if (in_i) call push(qx, qy, qv, nq, px(i), py(i), pv(i))

            if (in_i .neqv. in_j) then
                if (pv(j) /= pv(i)) then
                    t = (bound - pv(i)) / (pv(j) - pv(i))
                    call push(qx, qy, qv, nq, &
                              px(i) + t * (px(j) - px(i)), &
                              py(i) + t * (py(j) - py(i)), bound)
                end if
            end if
        end do
    end subroutine clip_half

    pure logical function inside(v, bound, keep_above)
        real(dp), intent(in) :: v, bound
        logical, intent(in) :: keep_above
        if (keep_above) then
            inside = v >= bound
        else
            inside = v <= bound
        end if
    end function inside

    pure subroutine push(qx, qy, qv, nq, x, y, v)
        real(dp), intent(inout) :: qx(MAX_POLY), qy(MAX_POLY), qv(MAX_POLY)
        integer, intent(inout) :: nq
        real(dp), intent(in) :: x, y, v
        if (nq >= MAX_POLY) return
        nq = nq + 1
        qx(nq) = x
        qy(nq) = y
        qv(nq) = v
    end subroutine push

end module fplot_contour
