! Mathtext layout.
!
! matplotlib lets any label contain a TeX fragment between dollar signs,
! and that is by far the most used piece of it: exponents in "$10^{3}$"
! and indices in "$x_i$". The layout is done here, once, and turned into
! a list of plain text runs with an offset and a size each. Every backend
! then draws the runs it already knows how to draw, so none of them needs
! to know what mathtext is.
!
! Only the layout is TeX. The glyphs are the ordinary text font, sloped
! for the letters of a math fragment and upright for everything else,
! which is TeX's own rule and matplotlib's.
module fplot_mathtext
    use fplot_glyphs, only: EM, glyph_advance
    use fplot_style, only: utf8_next, utf8_char
    implicit none
    private

    integer, parameter :: dp = kind(0.0d0)

    public :: mrun_t, math_layout, math_width, math_is

    ! One run of plain text: draw s(1:n) at size, offset by (dx, dy) from
    ! the origin of the whole string. dy grows downwards, as the canvas does.
    !
    ! A run with line = .true. is not text at all but a straight stroke of
    ! width lw from (dx, dy) to (x2, y2): the bar of a fraction and the
    ! radical of a root are drawn, not set, and there is no sensible glyph
    ! to borrow for either.
    integer, parameter, public :: MAX_RUNS = 64
    type :: mrun_t
        character(len=64) :: s = ""
        integer :: n = 0
        real(dp) :: dx = 0.0_dp
        real(dp) :: dy = 0.0_dp
        real(dp) :: size = 10.0_dp
        logical :: italic = .false.
        logical :: line = .false.
        real(dp) :: x2 = 0.0_dp
        real(dp) :: y2 = 0.0_dp
        real(dp) :: lw = 0.0_dp
    end type mrun_t

    ! matplotlib's mathtext constants: a script is 0.7 of its parent, a
    ! superscript sits 0.44 em above the baseline and a subscript 0.2 em
    ! below it, both measured in the parent's size.
    real(dp), parameter :: SCRIPT = 0.7_dp
    real(dp), parameter :: SUP_RISE = 0.44_dp
    real(dp), parameter :: SUB_DROP = 0.20_dp
    real(dp), parameter :: MIN_SIZE = 0.4_dp
    ! Fractions and roots, measured off matplotlib's own output at 20 pt
    ! and written here as fractions of the size. The bar of a fraction is
    ! RULE thick and sits AXIS above the baseline, which is TeX's axis
    ! height; the two halves are set a script smaller, PAD clear of the
    ! ends of the bar.
    real(dp), parameter :: RULE = 0.0625_dp
    real(dp), parameter :: AXIS = 0.2544_dp
    real(dp), parameter :: NUM_RISE = 0.3581_dp
    real(dp), parameter :: DEN_DROP = 0.3712_dp
    real(dp), parameter :: PAD = 0.0625_dp
    ! The root sign: how wide it is, how far it reaches above and below
    ! the baseline, and the gap it leaves around what is under it.
    real(dp), parameter :: ROOT_W = 0.62_dp
    real(dp), parameter :: ROOT_TOP = 1.08_dp
    real(dp), parameter :: ROOT_BOT = 0.22_dp
    real(dp), parameter :: ROOT_PAD = 0.125_dp
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
    recursive subroutine layout_math(s, size, x0, y0, runs, nr, xend, math)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: size, x0, y0
        type(mrun_t), intent(inout) :: runs(MAX_RUNS)
        integer, intent(inout) :: nr
        real(dp), intent(out) :: xend
        ! \mathrm and its relatives turn the sloping off for their group.
        logical, intent(in), optional :: math
        logical :: mm
        real(dp) :: pen, sbase, smax, sx, ssize
        integer :: i, j

        mm = .true.
        if (present(math)) mm = math

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
                                             y0 - SUP_RISE*size, runs, nr, sx, mm)
                        else
                            call layout_unit(s(i + 1:j), ssize, sbase, &
                                             y0 + SUB_DROP*size, runs, nr, sx, mm)
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
                call layout_math(s(i + 1:j - 1), size, pen, y0, runs, nr, sx, mm)
                pen = sx
                i = j + 1
            case (BS)
                call command_end(s, i, j)
                call layout_command(s(i:j), size, pen, y0, runs, nr, sx, mm)
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
                call emit(s(i:j - 1), size, pen, y0, runs, nr, sx, mm)
                pen = sx
                i = j
            end select
        end do
        xend = pen
    end subroutine layout_math

    ! One unit: a group, a command, or a single character.
    recursive subroutine layout_unit(s, size, x0, y0, runs, nr, xend, math)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: size, x0, y0
        type(mrun_t), intent(inout) :: runs(MAX_RUNS)
        integer, intent(inout) :: nr
        real(dp), intent(out) :: xend
        logical, intent(in), optional :: math
        integer :: j

        if (len(s) == 0) then
            xend = x0
        else if (s(1:1) == "{") then
            call group_end(s, 1, j)
            call layout_math(s(2:j - 1), size, x0, y0, runs, nr, xend, math)
        else
            call layout_math(s, size, x0, y0, runs, nr, xend, math)
        end if
    end subroutine layout_unit

    ! The handful of commands worth having without a math font: the
    ! spaces, and the wrappers that only change the style of a group.
    recursive subroutine layout_command(s, size, x0, y0, runs, nr, xend, math)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: size, x0, y0
        type(mrun_t), intent(inout) :: runs(MAX_RUNS)
        integer, intent(inout) :: nr
        real(dp), intent(out) :: xend
        logical, intent(in), optional :: math
        logical :: mm
        integer :: j, code

        mm = .true.
        if (present(math)) mm = math

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

        code = greek_code(cmd_name(s))
        if (code > 0) then
            ! Lowercase greek slopes and uppercase greek stands upright,
            ! which is how TeX sets them and what matplotlib draws.
            call emit_run(utf8_char(code), size, x0, y0, runs, nr, xend, &
                          code >= 945)
            return
        end if
        if (cmd_name(s) == "frac") then
            call layout_frac(s, size, x0, y0, runs, nr, xend, mm)
            return
        end if
        if (cmd_name(s) == "sqrt") then
            call layout_sqrt(s, size, x0, y0, runs, nr, xend, mm)
            return
        end if

        ! \mathrm{...} and its relatives: lay the group out as it is, and
        ! upright, which is the only reason \mathrm is ever written.
        j = index(s, "{")
        if (j > 0) then
            call layout_math(s(j + 1:len(s) - 1), size, x0, y0, runs, nr, xend, &
                             mm .and. s(2:j - 1) /= "mathrm")
        else
            ! An unknown command degrades to its own name, which at least
            ! shows what was asked for instead of vanishing.
            call emit(s(2:), size, x0, y0, runs, nr, xend)
        end if
    end subroutine layout_command

    ! \frac{a}{b}: the two halves a script smaller, each centred over the
    ! bar. They are laid out at the left edge and then slid across, which
    ! is the only way to centre something whose width is not known until
    ! it has been laid out.
    recursive subroutine layout_frac(s, size, x0, y0, runs, nr, xend, math)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: size, x0, y0
        type(mrun_t), intent(inout) :: runs(MAX_RUNS)
        integer, intent(inout) :: nr
        real(dp), intent(out) :: xend
        logical, intent(in) :: math
        integer :: n0, n1, d0, d1, kn, kd
        real(dp) :: ssize, xl, xn, xd, wn, wd, w, yb

        call cmd_arg(s, 1, n0, n1)
        call cmd_arg(s, 2, d0, d1)
        ssize = max(size*SCRIPT, MIN_SIZE*size)
        xl = x0 + PAD*size

        kn = nr + 1
        call layout_math(s(n0:n1), ssize, xl, y0 - NUM_RISE*size, runs, nr, xn, math)
        wn = xn - xl
        kd = nr + 1
        call layout_math(s(d0:d1), ssize, xl, y0 + DEN_DROP*size, runs, nr, xd, math)
        wd = xd - xl

        w = max(wn, wd)
        call shift_runs(runs, kn, kd - 1, 0.5_dp*(w - wn))
        call shift_runs(runs, kd, nr, 0.5_dp*(w - wd))

        yb = y0 - AXIS*size
        call emit_line(xl, yb, xl + w, yb, RULE*size, runs, nr)
        xend = xl + w + PAD*size
    end subroutine layout_frac

    ! \sqrt{a}: the sign is drawn rather than set, since the text font has
    ! no radical in it, and the bar over it is one more stroke.
    recursive subroutine layout_sqrt(s, size, x0, y0, runs, nr, xend, math)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: size, x0, y0
        type(mrun_t), intent(inout) :: runs(MAX_RUNS)
        integer, intent(inout) :: nr
        real(dp), intent(out) :: xend
        logical, intent(in) :: math
        integer :: a0, a1
        real(dp) :: top, bot, wr, th, xb, xe

        call cmd_arg(s, 1, a0, a1)
        th = RULE*size
        wr = ROOT_W*size
        top = y0 - ROOT_TOP*size
        bot = y0 + ROOT_BOT*size
        xb = x0 + wr + ROOT_PAD*size
        call layout_math(s(a0:a1), size, xb, y0, runs, nr, xe, math)

        ! The sign itself: a short rise to the foot, a long rise to the
        ! top, then the bar across whatever is under the root.
        call emit_line(x0, y0 - 0.35_dp*size, x0 + 0.24_dp*wr, bot, th, runs, nr)
        call emit_line(x0 + 0.24_dp*wr, bot, x0 + wr, top, th, runs, nr)
        call emit_line(x0 + wr, top, xe + ROOT_PAD*size, top, th, runs, nr)
        xend = xe + ROOT_PAD*size
    end subroutine layout_sqrt

    ! Slide runs k0..k1 across by dx, once their width is known.
    pure subroutine shift_runs(runs, k0, k1, dx)
        type(mrun_t), intent(inout) :: runs(MAX_RUNS)
        integer, intent(in) :: k0, k1
        real(dp), intent(in) :: dx
        integer :: k
        do k = max(k0, 1), min(k1, MAX_RUNS)
            runs(k)%dx = runs(k)%dx + dx
            runs(k)%x2 = runs(k)%x2 + dx
        end do
    end subroutine shift_runs

    subroutine emit_line(x1, y1, x2, y2, lw, runs, nr)
        real(dp), intent(in) :: x1, y1, x2, y2, lw
        type(mrun_t), intent(inout) :: runs(MAX_RUNS)
        integer, intent(inout) :: nr
        if (nr >= MAX_RUNS) return
        nr = nr + 1
        runs(nr)%n = 0
        runs(nr)%line = .true.
        runs(nr)%dx = x1
        runs(nr)%dy = y1
        runs(nr)%x2 = x2
        runs(nr)%y2 = y2
        runs(nr)%lw = lw
    end subroutine emit_line


    ! The letters of the command that s starts with, without the backslash.
    pure function cmd_name(s) result(name)
        character(len=*), intent(in) :: s
        character(len=16) :: name
        integer :: j
        name = ""
        j = 2
        do while (j <= len(s))
            if (.not. is_alpha(s(j:j))) exit
            j = j + 1
        end do
        if (j > 2 .and. j - 2 <= len(name)) name = s(2:j - 1)
    end function cmd_name

    ! The k-th braced argument of a command, empty when there is no such
    ! argument. i0:i1 is the text inside the braces.
    pure subroutine cmd_arg(s, k, i0, i1)
        character(len=*), intent(in) :: s
        integer, intent(in) :: k
        integer, intent(out) :: i0, i1
        integer :: i, j, seen
        i0 = 1
        i1 = 0
        seen = 0
        i = 2
        do while (i <= len(s))
            if (s(i:i) == "{") then
                call group_end(s, i, j)
                seen = seen + 1
                if (seen == k) then
                    i0 = i + 1
                    i1 = j - 1
                    return
                end if
                i = j + 1
            else
                i = i + 1
            end if
        end do
    end subroutine cmd_arg

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
        integer :: k, a, nargs
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
        ! One braced argument, or two for the commands that take two.
        nargs = 1
        if (s(i + 1:j) == "frac") nargs = 2
        do a = 1, nargs
            if (j >= len(s)) exit
            if (s(j + 1:j + 1) /= "{") exit
            call group_end(s, j + 1, k)
            j = k
        end do
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

    ! The greek letters, by their TeX names. Everything else is left to
    ! degrade to its own name, which at least shows what was asked for.
    pure function greek_code(name) result(code)
        character(len=*), intent(in) :: name
        integer :: code
        select case (name)
        case ("alpha"); code = 945
        case ("beta"); code = 946
        case ("gamma"); code = 947
        case ("delta"); code = 948
        case ("epsilon", "varepsilon"); code = 949
        case ("zeta"); code = 950
        case ("eta"); code = 951
        case ("theta", "vartheta"); code = 952
        case ("iota"); code = 953
        case ("kappa"); code = 954
        case ("lambda"); code = 955
        case ("mu"); code = 956
        case ("nu"); code = 957
        case ("xi"); code = 958
        case ("pi"); code = 960
        case ("rho", "varrho"); code = 961
        case ("varsigma"); code = 962
        case ("sigma"); code = 963
        case ("tau"); code = 964
        case ("upsilon"); code = 965
        case ("phi", "varphi"); code = 966
        case ("chi"); code = 967
        case ("psi"); code = 968
        case ("omega"); code = 969
        case ("Gamma"); code = 915
        case ("Delta"); code = 916
        case ("Theta"); code = 920
        case ("Lambda"); code = 923
        case ("Xi"); code = 926
        case ("Pi"); code = 928
        case ("Sigma"); code = 931
        case ("Upsilon"); code = 933
        case ("Phi"); code = 934
        case ("Psi"); code = 936
        case ("Omega"); code = 937
        ! The multiplication sign, which log tick labels are written with.
        case ("times"); code = 215
        case default; code = 0
        end select
    end function greek_code

    ! ------------------------------------------------------------------
    ! Runs
    ! ------------------------------------------------------------------

    ! In math mode the letters slope and everything else stays upright, so
    ! the text is broken at every change of class and each piece emitted on
    ! its own. Outside math mode it is one run, as it always was.
    subroutine emit(s, size, x0, y0, runs, nr, xend, math)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: size, x0, y0
        type(mrun_t), intent(inout) :: runs(MAX_RUNS)
        integer, intent(inout) :: nr
        real(dp), intent(out) :: xend
        logical, intent(in), optional :: math
        logical :: mm
        real(dp) :: pen, nxt
        integer :: i, j

        mm = .false.
        if (present(math)) mm = math
        if (.not. mm) then
            call emit_run(s, size, x0, y0, runs, nr, xend, .false.)
            return
        end if
        pen = x0
        i = 1
        do while (i <= len(s))
            j = i
            do while (j < len(s))
                if (is_alpha(s(j + 1:j + 1)) .neqv. is_alpha(s(i:i))) exit
                j = j + 1
            end do
            call emit_run(s(i:j), size, pen, y0, runs, nr, nxt, is_alpha(s(i:i)))
            pen = nxt
            i = j + 1
        end do
        xend = pen
    end subroutine emit

    subroutine emit_run(s, size, x0, y0, runs, nr, xend, italic)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: size, x0, y0
        type(mrun_t), intent(inout) :: runs(MAX_RUNS)
        integer, intent(inout) :: nr
        real(dp), intent(out) :: xend
        logical, intent(in) :: italic
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
        runs(nr)%italic = italic
        runs(nr)%line = .false.
    end subroutine emit_run

    function run_width(s, size) result(w)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: size
        real(dp) :: w
        integer :: i, code, nb
        w = 0.0_dp
        i = 1
        do while (i <= len(s))
            call utf8_next(s, i, code, nb)
            w = w + glyph_advance(code)
            i = i + nb
        end do
        w = w*size/EM
    end function run_width

end module fplot_mathtext
