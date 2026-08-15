! Style parsing and color/marker/linestyle helpers for fplot.
module fplot_style
    implicit none
    private

    integer, parameter, public :: dp = kind(0.0d0)

    integer, parameter, public :: MARKER_NONE = 0
    integer, parameter, public :: MARKER_CIRCLE = 1
    integer, parameter, public :: MARKER_X = 2
    integer, parameter, public :: MARKER_POINT = 3
    integer, parameter, public :: MARKER_SQUARE = 4
    integer, parameter, public :: MARKER_TRI_UP = 5
    integer, parameter, public :: MARKER_TRI_DOWN = 6
    integer, parameter, public :: MARKER_TRI_LEFT = 7
    integer, parameter, public :: MARKER_TRI_RIGHT = 8
    integer, parameter, public :: MARKER_STAR = 9
    integer, parameter, public :: MARKER_PLUS = 10
    integer, parameter, public :: MARKER_DIAMOND = 11

    integer, parameter, public :: LINE_NONE = 0
    integer, parameter, public :: LINE_SOLID = 1
    integer, parameter, public :: LINE_DASHED = 2
    integer, parameter, public :: LINE_DOTTED = 3
    integer, parameter, public :: LINE_DASHDOT = 4

    integer, parameter, public :: N_TAB10 = 10
    character(len=7), parameter, public :: TAB10(N_TAB10) = [ &
        "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", &
        "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf" ]

    public :: parse_fmt
    public :: color_from_char
    public :: color_from_C
    public :: marker_from_char
    public :: is_hex_color
    public :: default_linewidth
    public :: default_markersize

    real(dp), parameter :: default_linewidth = 1.5_dp
    real(dp), parameter :: default_markersize = 6.0_dp

contains

    pure function is_hex_color(s) result(ok)
        character(len=*), intent(in) :: s
        logical :: ok
        integer :: n
        n = len_trim(s)
        ok = (n == 7 .and. s(1:1) == "#")
    end function is_hex_color

    pure function color_from_char(c) result(col)
        character(len=1), intent(in) :: c
        character(len=7) :: col
        select case (c)
        case ("b"); col = "#0000ff"
        case ("g"); col = "#008000"
        case ("r"); col = "#ff0000"
        case ("c"); col = "#00bfbf"
        case ("m"); col = "#bf00bf"
        case ("y"); col = "#bfbf00"
        case ("k"); col = "#000000"
        case ("w"); col = "#ffffff"
        case default; col = ""
        end select
    end function color_from_char

    pure function color_from_C(idx) result(col)
        integer, intent(in) :: idx
        character(len=7) :: col
        integer :: i
        i = mod(idx, N_TAB10)
        if (i < 0) i = i + N_TAB10
        col = TAB10(i + 1)
    end function color_from_C

    ! Matplotlib's single-character marker codes. MARKER_NONE means "not a
    ! marker character".
    pure function marker_from_char(c) result(mk)
        character(len=1), intent(in) :: c
        integer :: mk
        select case (c)
        case ("o"); mk = MARKER_CIRCLE
        case ("x"); mk = MARKER_X
        case ("."); mk = MARKER_POINT
        case ("s"); mk = MARKER_SQUARE
        case ("^"); mk = MARKER_TRI_UP
        case ("v"); mk = MARKER_TRI_DOWN
        case ("<"); mk = MARKER_TRI_LEFT
        case (">"); mk = MARKER_TRI_RIGHT
        case ("*"); mk = MARKER_STAR
        case ("+"); mk = MARKER_PLUS
        case ("D"); mk = MARKER_DIAMOND
        case default; mk = MARKER_NONE
        end select
    end function marker_from_char

    subroutine parse_fmt(fmt, color, marker, linestyle)
        ! Parse a matplotlib-like format string (color + marker + linestyle).
        character(len=*), intent(in) :: fmt
        character(len=7), intent(out) :: color
        integer, intent(out) :: marker, linestyle
        character(len=64) :: s
        integer :: i, n, m
        character(len=1) :: c
        logical :: got_color, got_marker, got_line

        color = ""
        marker = MARKER_NONE
        linestyle = LINE_NONE
        got_color = .false.
        got_marker = .false.
        got_line = .false.

        s = adjustl(fmt)
        n = len_trim(s)
        if (n == 0) return

        i = 1
        do while (i <= n)
            c = s(i:i)

            ! C0-C9 cycle colors
            if (c == "C" .and. i < n) then
                if (s(i+1:i+1) >= "0" .and. s(i+1:i+1) <= "9") then
                    read (s(i+1:i+1), *) m
                    color = color_from_C(m)
                    got_color = .true.
                    i = i + 2
                    cycle
                end if
            end if

            ! named single-letter colors
            if (.not. got_color) then
                select case (c)
                case ("b", "g", "r", "c", "m", "y", "k", "w")
                    color = color_from_char(c)
                    got_color = .true.
                    i = i + 1
                    cycle
                end select
            end if

            ! markers
            if (.not. got_marker) then
                m = marker_from_char(c)
                if (m /= MARKER_NONE) then
                    marker = m
                    got_marker = .true.
                    i = i + 1
                    cycle
                end if
            end if

            ! linestyles: --, -., -, :
            if (.not. got_line) then
                if (c == "-" .and. i < n .and. s(i+1:i+1) == "-") then
                    linestyle = LINE_DASHED
                    got_line = .true.
                    i = i + 2
                    cycle
                else if (c == "-" .and. i < n .and. s(i+1:i+1) == ".") then
                    linestyle = LINE_DASHDOT
                    got_line = .true.
                    i = i + 2
                    cycle
                else if (c == "-") then
                    linestyle = LINE_SOLID
                    got_line = .true.
                    i = i + 1
                    cycle
                else if (c == ":") then
                    linestyle = LINE_DOTTED
                    got_line = .true.
                    i = i + 1
                    cycle
                end if
            end if

            ! unknown character — skip
            i = i + 1
        end do

        ! If only a marker was given, no line. If only color, solid line.
        ! If nothing style-like, leave as NONE so caller can apply defaults.
        if (got_marker .and. .not. got_line) then
            linestyle = LINE_NONE
        else if (got_color .and. .not. got_line .and. .not. got_marker) then
            linestyle = LINE_SOLID
        else if (.not. got_line .and. .not. got_marker) then
            linestyle = LINE_SOLID
        end if
    end subroutine parse_fmt

end module fplot_style
