! Mathtext layout.
!
! matplotlib lets any label contain a TeX fragment between dollar signs,
! and that is by far the most used piece of it: exponents in "$10^{3}$"
! and indices in "$x_i$". The layout is done here, once, and turned into
! a list of plain text runs with an offset and a size each. Every backend
! then draws the runs it already knows how to draw, so none of them needs
! to know what mathtext is.
!
! Only the layout is TeX; the glyphs are the ordinary text font, upright
! rather than italic, because the one font that is compiled into this
! library has no italic cut and a backend that faked one would not match
! the others.
module fplot_mathtext
    use fplot_glyphs, only: EM, glyph_advance
    implicit none
    private

    integer, parameter :: dp = kind(0.0d0)

    public :: mrun_t, math_layout, math_width, math_is

    ! One run of plain text: draw s(1:n) at size, offset by (dx, dy) from
    ! the origin of the whole string. dy grows downwards, as the canvas does.
    integer, parameter, public :: MAX_RUNS = 64
    type :: mrun_t
        character(len=64) :: s = ""
        integer :: n = 0
        real(dp) :: dx = 0.0_dp
        real(dp) :: dy = 0.0_dp
        real(dp) :: size = 10.0_dp
    end type mrun_t

    ! matplotlib's mathtext constants: a script is 0.7 of its parent, a
    ! superscript sits 0.44 em above the baseline and a subscript 0.2 em
    ! below it, both measured in the parent's size.
    real(dp), parameter :: SCRIPT = 0.7_dp
    real(dp), parameter :: SUP_RISE = 0.44_dp
    real(dp), parameter :: SUB_DROP = 0.20_dp
    real(dp), parameter :: MIN_SIZE = 0.4_dp
    ! Spelled this way because a backslash inside a string literal is not
    ! portable: some compilers read it as the start of an escape.
    character, parameter :: BS = achar(92)

contains

    ! Is there anything in this string for the mathtext engine to do?
    pure function math_is(s) result(yes)
        character(len=*), intent(in) :: s
        logical :: yes
        integer :: i
        yes = .false.
        do i = 1, len(s)
            if (s(i:i) == "$") then
                yes = .true.
                return
            end if
        end do
    end function math_is

    ! Width of a string in points, mathtext or not.
    function math_width(s, size) result(w)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: size
        real(dp) :: w
        type(mrun_t) :: runs(MAX_RUNS)
        integer :: nr
        call math_layout(s, size, runs, nr, w)
    end function math_width

    ! Lay a string out into runs. Text outside the dollar signs is copied
    ! through unchanged, so a string with no mathtext in it comes back as
    ! a single run and costs nothing.
    subroutine math_layout(s, size, runs, nr, width)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: size
        type(mrun_t), intent(out) :: runs(MAX_RUNS)
        integer, intent(out) :: nr
        real(dp), intent(out) :: width
        integer :: i, j
        real(dp) :: pen, xnew

        nr = 0
        pen = 0.0_dp
        i = 1
        do while (i <= len(s))
            if (s(i:i) == "$") then
                j = i + 1
                do while (j <= len(s))
                    if (s(j:j) == "$") exit
                    j = j + 1
                end do
                if (j > i + 1) then
                    call layout_math(s(i + 1:j - 1), size, pen, 0.0_dp, &
                                     runs, nr, xnew)
                    pen = xnew
                end if
                i = j + 1
            else
                j = i
                do while (j <= len(s))
                    if (s(j:j) == "$") exit
                    j = j + 1
                end do
                call emit(s(i:j - 1), size, pen, 0.0_dp, runs, nr, xnew)
                pen = xnew
                i = j
            end if
        end do
        width = pen
    end subroutine math_layout

    ! ------------------------------------------------------------------
    ! The math mode itself
    ! ------------------------------------------------------------------

    ! Lay out one math fragment starting at pen x = x0, baseline y = y0.
    ! xend comes back as the pen position after the fragment.
    recursive subroutine layout_math(s, size, x0, y0, runs, nr, xend)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: size, x0, y0
        type(mrun_t), intent(inout) :: runs(MAX_RUNS)
        integer, intent(inout) :: nr
        real(dp), intent(out) :: xend
        real(dp) :: pen, sbase, smax, sx, ssize
        integer :: i, j

        pen = x0
        i = 1
        do while (i <= len(s))
            select case (s(i:i))
            case ("^", "_")
                ! Every script that follows the same base hangs at the
                ! same x, so a base with both takes only the wider one.
                sbase = pen
                smax = pen
                ssize = max(size*SCRIPT, MIN_SIZE*size)
                do
                    call unit_end(s, i + 1, j)
                    if (j > i) then
                        if (s(i:i) == "^") then
                            call layout_unit(s(i + 1:j), ssize, sbase, &
                                             y0 - SUP_RISE*size, runs, nr, sx)
                        else
                            call layout_unit(s(i + 1:j), ssize, sbase, &
                                             y0 + SUB_DROP*size, runs, nr, sx)
                        end if
                        smax = max(smax, sx)
                    end if
                    i = j + 1
                    if (i > len(s)) exit
                    if (s(i:i) /= "^" .and. s(i:i) /= "_") exit
                end do
                pen = smax
            case ("{")
                call group_end(s, i, j)
                call layout_math(s(i + 1:j - 1), size, pen, y0, runs, nr, sx)
                pen = sx
                i = j + 1
            case (BS)
                call command_end(s, i, j)
                call layout_command(s(i:j), size, pen, y0, runs, nr, sx)
                pen = sx
                i = j + 1
            case ("}")
                i = i + 1
            case default
                ! Run of ordinary characters, up to the next thing that
                ! is not one. Spaces are kept: TeX drops them and then
                ! puts its own back around the operators, and keeping
                ! them lands in about the same place for far less work.
                j = i
                do while (j <= len(s))
                    if (s(j:j) == "^" .or. s(j:j) == "_" .or. s(j:j) == "{" &
                        .or. s(j:j) == "}" .or. s(j:j) == BS) exit
                    j = j + 1
                end do
                call emit(s(i:j - 1), size, pen, y0, runs, nr, sx)
                pen = sx
                i = j
            end select
        end do
        xend = pen
    end subroutine layout_math

    ! One unit: a group, a command, or a single character.
    recursive subroutine layout_unit(s, size, x0, y0, runs, nr, xend)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: size, x0, y0
        type(mrun_t), intent(inout) :: runs(MAX_RUNS)
        integer, intent(inout) :: nr
        real(dp), intent(out) :: xend
        integer :: j

        if (len(s) == 0) then
            xend = x0
        else if (s(1:1) == "{") then
            call group_end(s, 1, j)
            call layout_math(s(2:j - 1), size, x0, y0, runs, nr, xend)
        else
            call layout_math(s, size, x0, y0, runs, nr, xend)
        end if
    end subroutine layout_unit

    ! The handful of commands worth having without a math font: the
    ! spaces, and the wrappers that only change the style of a group.
    recursive subroutine layout_command(s, size, x0, y0, runs, nr, xend)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: size, x0, y0
        type(mrun_t), intent(inout) :: runs(MAX_RUNS)
        integer, intent(inout) :: nr
        real(dp), intent(out) :: xend
        integer :: j

        xend = x0
        if (len(s) < 2) return
        select case (s(2:2))
        case (",")
            xend = x0 + 0.167_dp*size
            return
        case (";")
            xend = x0 + 0.278_dp*size
            return
        case (" ")
            xend = x0 + 0.333_dp*size
            return
        case ("$")
            call emit("$", size, x0, y0, runs, nr, xend)
            return
        end select

        ! \mathrm{...} and its relatives: lay the group out as it is.
        j = index(s, "{")
        if (j > 0) then
            call layout_math(s(j + 1:len(s) - 1), size, x0, y0, runs, nr, xend)
        else
            ! An unknown command degrades to its own name, which at least
            ! shows what was asked for instead of vanishing.
            call emit(s(2:), size, x0, y0, runs, nr, xend)
        end if
    end subroutine layout_command

    ! ------------------------------------------------------------------
    ! Scanning helpers
    ! ------------------------------------------------------------------

    ! End of the group that starts at s(i:i) == "{".
    pure subroutine group_end(s, i, j)
        character(len=*), intent(in) :: s
        integer, intent(in) :: i
        integer, intent(out) :: j
        integer :: depth
        depth = 0
        do j = i, len(s)
            if (s(j:j) == "{") depth = depth + 1
            if (s(j:j) == "}") then
                depth = depth - 1
                if (depth == 0) return
            end if
        end do
        j = len(s)
    end subroutine group_end

    ! End of the command that starts at s(i:i) == "\". A command is the
    ! backslash plus its letters, plus a braced argument if there is one.
    pure subroutine command_end(s, i, j)
        character(len=*), intent(in) :: s
        integer, intent(in) :: i
        integer, intent(out) :: j
        integer :: k
        if (i + 1 > len(s)) then
            j = i
            return
        end if
        if (.not. is_alpha(s(i + 1:i + 1))) then
            j = i + 1
            return
        end if
        j = i + 1
        do while (j < len(s))
            if (.not. is_alpha(s(j + 1:j + 1))) exit
            j = j + 1
        end do
        if (j < len(s)) then
            if (s(j + 1:j + 1) == "{") then
                call group_end(s, j + 1, k)
                j = k
            end if
        end if
    end subroutine command_end

    ! End of the unit that starts at index i, for a script argument.
    pure subroutine unit_end(s, i, j)
        character(len=*), intent(in) :: s
        integer, intent(in) :: i
        integer, intent(out) :: j
        if (i > len(s)) then
            j = i - 1
        else if (s(i:i) == "{") then
            call group_end(s, i, j)
        else if (s(i:i) == BS) then
            call command_end(s, i, j)
        else
            j = i
        end if
    end subroutine unit_end

    pure function is_alpha(c) result(yes)
        character, intent(in) :: c
        logical :: yes
        yes = (c >= "a" .and. c <= "z") .or. (c >= "A" .and. c <= "Z")
    end function is_alpha

    ! ------------------------------------------------------------------
    ! Runs
    ! ------------------------------------------------------------------

    subroutine emit(s, size, x0, y0, runs, nr, xend)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: size, x0, y0
        type(mrun_t), intent(inout) :: runs(MAX_RUNS)
        integer, intent(inout) :: nr
        real(dp), intent(out) :: xend
        integer :: n

        xend = x0 + run_width(s, size)
        n = min(len(s), 64)
        if (n <= 0 .or. nr >= MAX_RUNS) return
        nr = nr + 1
        runs(nr)%s = s(1:n)
        runs(nr)%n = n
        runs(nr)%dx = x0
        runs(nr)%dy = y0
        runs(nr)%size = size
    end subroutine emit

    function run_width(s, size) result(w)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: size
        real(dp) :: w
        integer :: i
        w = 0.0_dp
        do i = 1, len(s)
            w = w + glyph_advance(iachar(s(i:i)))
        end do
        w = w*size/EM
    end function run_width

end module fplot_mathtext
