! Text as an outline, for the backends that normally hand text to the
! format and let it find a font.
!
! Helvetica has no greek in it, and neither has WinAnsi, so a label that
! says "alpha" in mathtext has nothing to be set in. Rather than embed a
! font, the string is drawn: the same DejaVu outlines the PNG backend
! fills, in canvas coordinates, ready for draw_path. It is only reached
! for strings that need it, so ordinary labels stay selectable text.
module fplot_textpath
    use fplot_style, only: dp, utf8_next
    use fplot_glyphs, only: EM, glyph_advance, glyph_verbs, glyph_points
    implicit none
    private

    public :: text_needs_outline, text_path, text_path_width

contains

    ! True when the string has a code point no simple font encoding covers.
    pure function text_needs_outline(s) result(yes)
        character(len=*), intent(in) :: s
        logical :: yes
        integer :: i, code, nb
        yes = .false.
        i = 1
        do while (i <= len(s))
            call utf8_next(s, i, code, nb)
            if (code > 255) then
                yes = .true.
                return
            end if
            i = i + nb
        end do
    end function text_needs_outline

    ! Width of the string in points, from the same outlines.
    function text_path_width(s, fsize, face) result(w)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: fsize
        integer, intent(in) :: face
        real(dp) :: w
        integer :: i, code, nb
        w = 0.0_dp
        i = 1
        do while (i <= len(s))
            call utf8_next(s, i, code, nb)
            w = w + glyph_advance(code, face)
            i = i + nb
        end do
        w = w*fsize/EM
    end function text_path_width

    ! The outline of s, with the baseline starting at (x, y) and turned by
    ! angle degrees about that point. The path is in the verbs the
    ! rendering API speaks, so a backend fills it with the code it already
    ! has for any other path. The point list carries one point per move and
    ! line and three per cubic, as every path in the API does.
    subroutine text_path(s, x, y, fsize, face, angle, px, py, verbs, nv)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: x, y, fsize, angle
        integer, intent(in) :: face
        real(dp), allocatable, intent(out) :: px(:), py(:)
        integer, allocatable, intent(out) :: verbs(:)
        integer, intent(out) :: nv
        real(dp), allocatable :: gx(:), gy(:)
        integer, allocatable :: gv(:)
        integer :: i, k, ng, np, code, nb, np_tot, ip
        real(dp) :: pen, sc, ca, sa, rad, lx, ly

        sc = fsize/EM
        rad = angle*acos(-1.0_dp)/180.0_dp
        ca = cos(rad)
        sa = sin(rad)

        call outline_size(s, face, nv, np_tot)
        allocate (verbs(max(nv, 1)), px(max(np_tot, 1)), py(max(np_tot, 1)))
        verbs = 0
        px = 0.0_dp
        py = 0.0_dp

        pen = 0.0_dp
        nv = 0
        ip = 0
        i = 1
        do while (i <= len(s))
            call utf8_next(s, i, code, nb)
            i = i + nb
            call glyph_verbs(code, gv, ng, face)
            call glyph_points(code, gx, gy, np, face)
            do k = 1, ng
                verbs(nv + k) = gv(k)
            end do
            nv = nv + ng
            do k = 1, np
                ! Font units are y up and the canvas is y down, so the
                ! glyph is flipped as it is placed.
                lx = pen + gx(k)*sc
                ly = -gy(k)*sc
                px(ip + k) = x + lx*ca - ly*sa
                py(ip + k) = y + lx*sa + ly*ca
            end do
            ip = ip + np
            pen = pen + glyph_advance(code, face)*sc
        end do
    end subroutine text_path

    ! How many verbs and points the whole string comes to.
    subroutine outline_size(s, face, nv, np_tot)
        character(len=*), intent(in) :: s
        integer, intent(in) :: face
        integer, intent(out) :: nv, np_tot
        real(dp), allocatable :: gx(:), gy(:)
        integer, allocatable :: gv(:)
        integer :: i, code, nb, ng, np

        nv = 0
        np_tot = 0
        i = 1
        do while (i <= len(s))
            call utf8_next(s, i, code, nb)
            i = i + nb
            call glyph_verbs(code, gv, ng, face)
            call glyph_points(code, gx, gy, np, face)
            nv = nv + ng
            np_tot = np_tot + np
        end do
    end subroutine outline_size

end module fplot_textpath
