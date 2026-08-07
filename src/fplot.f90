! fplot — pure Fortran pylab-style SVG plotting library.
module fplot
    use fplot_style
    use fplot_ticks
    use fplot_svg
    implicit none
    private

    public :: dp
    public :: plot, semilogx, semilogy, loglog
    public :: title, xlabel, ylabel, grid, legend
    public :: xlim, ylim, clf, savefig, show, figure
    public :: render_svg

    integer, parameter :: SCALE_LINEAR = 0
    integer, parameter :: SCALE_LOG = 1
    integer, parameter :: MAX_SERIES = 32
    integer, parameter :: MAX_POINTS = 100000

    type :: series_t
        integer :: n = 0
        real(dp), allocatable :: x(:)
        real(dp), allocatable :: y(:)
        character(len=7) :: color = "#1f77b4"
        integer :: marker = MARKER_NONE
        integer :: linestyle = LINE_SOLID
        real(dp) :: linewidth = 1.5_dp
        real(dp) :: markersize = 6.0_dp
        character(len=128) :: label = ""
    end type series_t

    ! Figure state (pylab current figure)
    real(dp), save :: fig_w_in = 6.4_dp
    real(dp), save :: fig_h_in = 4.8_dp
    real(dp), save :: sub_left = 0.125_dp
    real(dp), save :: sub_right = 0.9_dp
    real(dp), save :: sub_bottom = 0.11_dp
    real(dp), save :: sub_top = 0.88_dp
    integer, save :: n_series = 0
    type(series_t), save :: series(MAX_SERIES)
    character(len=256), save :: fig_title = ""
    character(len=256), save :: fig_xlabel = ""
    character(len=256), save :: fig_ylabel = ""
    logical, save :: grid_on = .false.
    logical, save :: legend_on = .false.
    integer, save :: xscale = SCALE_LINEAR
    integer, save :: yscale = SCALE_LINEAR
    logical, save :: xlim_set = .false.
    logical, save :: ylim_set = .false.
    real(dp), save :: xmin_user = 0.0_dp, xmax_user = 1.0_dp
    real(dp), save :: ymin_user = 0.0_dp, ymax_user = 1.0_dp
    integer, save :: color_cycle = 0
    logical, save :: fig_initialized = .false.

contains

    subroutine ensure_fig()
        if (.not. fig_initialized) call clf()
    end subroutine ensure_fig

    subroutine figure()
        call clf()
    end subroutine figure

    subroutine free_series(i)
        integer, intent(in) :: i
        if (allocated(series(i)%x)) deallocate (series(i)%x)
        if (allocated(series(i)%y)) deallocate (series(i)%y)
        series(i)%n = 0
        series(i)%label = ""
        series(i)%color = "#1f77b4"
        series(i)%marker = MARKER_NONE
        series(i)%linestyle = LINE_SOLID
        series(i)%linewidth = default_linewidth
        series(i)%markersize = default_markersize
    end subroutine free_series

    subroutine clf()
        integer :: i
        do i = 1, MAX_SERIES
            call free_series(i)
        end do
        n_series = 0
        fig_w_in = 6.4_dp
        fig_h_in = 4.8_dp
        sub_left = 0.125_dp
        sub_right = 0.9_dp
        sub_bottom = 0.11_dp
        sub_top = 0.88_dp
        fig_title = ""
        fig_xlabel = ""
        fig_ylabel = ""
        grid_on = .false.
        legend_on = .false.
        xscale = SCALE_LINEAR
        yscale = SCALE_LINEAR
        xlim_set = .false.
        ylim_set = .false.
        xmin_user = 0.0_dp
        xmax_user = 1.0_dp
        ymin_user = 0.0_dp
        ymax_user = 1.0_dp
        color_cycle = 0
        fig_initialized = .true.
    end subroutine clf

    subroutine title(s)
        character(len=*), intent(in) :: s
        call ensure_fig()
        fig_title = s
    end subroutine title

    subroutine xlabel(s)
        character(len=*), intent(in) :: s
        call ensure_fig()
        fig_xlabel = s
    end subroutine xlabel

    subroutine ylabel(s)
        character(len=*), intent(in) :: s
        call ensure_fig()
        fig_ylabel = s
    end subroutine ylabel

    subroutine grid(on)
        logical, intent(in) :: on
        call ensure_fig()
        grid_on = on
    end subroutine grid

    subroutine legend()
        call ensure_fig()
        legend_on = .true.
    end subroutine legend

    subroutine xlim(xmin, xmax)
        real(dp), intent(in) :: xmin, xmax
        call ensure_fig()
        xmin_user = xmin
        xmax_user = xmax
        xlim_set = .true.
    end subroutine xlim

    subroutine ylim(ymin, ymax)
        real(dp), intent(in) :: ymin, ymax
        call ensure_fig()
        ymin_user = ymin
        ymax_user = ymax
        ylim_set = .true.
    end subroutine ylim

    subroutine plot(x, y, fmt, label, lw, color, marker, linestyle)
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: fmt, label, color, marker, linestyle
        real(dp), intent(in), optional :: lw
        call ensure_fig()
        call add_series(x, y, fmt, label, lw, color, marker, linestyle)
    end subroutine plot

    subroutine semilogx(x, y, fmt, label, lw, color)
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: fmt, label, color
        real(dp), intent(in), optional :: lw
        call ensure_fig()
        xscale = SCALE_LOG
        call add_series(x, y, fmt, label, lw, color)
    end subroutine semilogx

    subroutine semilogy(x, y, fmt, label, lw, color)
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: fmt, label, color
        real(dp), intent(in), optional :: lw
        call ensure_fig()
        yscale = SCALE_LOG
        call add_series(x, y, fmt, label, lw, color)
    end subroutine semilogy

    subroutine loglog(x, y, fmt, label, lw, color)
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: fmt, label, color
        real(dp), intent(in), optional :: lw
        call ensure_fig()
        xscale = SCALE_LOG
        yscale = SCALE_LOG
        call add_series(x, y, fmt, label, lw, color)
    end subroutine loglog

    subroutine add_series(x, y, fmt, label, lw, color, marker, linestyle)
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: fmt, label, color, marker, linestyle
        real(dp), intent(in), optional :: lw
        integer :: n, is, m, ls
        character(len=7) :: col
        character(len=32) :: f
        logical :: have_fmt

        n = min(size(x), size(y))
        if (n <= 0) return
        if (n > MAX_POINTS) n = MAX_POINTS
        if (n_series >= MAX_SERIES) return

        n_series = n_series + 1
        is = n_series
        call free_series(is)

        allocate (series(is)%x(n), series(is)%y(n))
        series(is)%x(1:n) = x(1:n)
        series(is)%y(1:n) = y(1:n)
        series(is)%n = n
        series(is)%linewidth = default_linewidth
        series(is)%markersize = default_markersize
        series(is)%label = ""
        series(is)%marker = MARKER_NONE
        series(is)%linestyle = LINE_SOLID
        series(is)%color = ""

        have_fmt = .false.
        f = ""
        if (present(fmt)) then
            if (len_trim(fmt) > 0) then
                f = fmt
                have_fmt = .true.
            end if
        end if

        col = ""
        m = MARKER_NONE
        ls = LINE_NONE
        if (have_fmt) then
            call parse_fmt(trim(f), col, m, ls)
            series(is)%marker = m
            if (ls == LINE_NONE .and. m == MARKER_NONE) then
                series(is)%linestyle = LINE_SOLID
            else if (ls == LINE_NONE .and. m /= MARKER_NONE) then
                series(is)%linestyle = LINE_NONE
            else
                series(is)%linestyle = ls
            end if
            if (len_trim(col) > 0) series(is)%color = col
        end if

        if (present(color)) then
            if (len_trim(color) > 0) then
                if (is_hex_color(trim(color))) then
                    series(is)%color = color(1:7)
                else if (len_trim(color) == 1) then
                    series(is)%color = color_from_char(color(1:1))
                else if (len_trim(color) >= 2 .and. color(1:1) == "C") then
                    read (color(2:2), *) m
                    series(is)%color = color_from_C(m)
                end if
            end if
        end if

        if (present(marker)) then
            select case (trim(marker))
            case ("o"); series(is)%marker = MARKER_CIRCLE
            case ("x"); series(is)%marker = MARKER_X
            case ("."); series(is)%marker = MARKER_POINT
            case ("None", "none", ""); series(is)%marker = MARKER_NONE
            end select
        end if

        if (present(linestyle)) then
            select case (trim(linestyle))
            case ("-"); series(is)%linestyle = LINE_SOLID
            case ("--"); series(is)%linestyle = LINE_DASHED
            case (":"); series(is)%linestyle = LINE_DOTTED
            case ("-."); series(is)%linestyle = LINE_DASHDOT
            case ("None", "none", ""); series(is)%linestyle = LINE_NONE
            end select
        end if

        if (present(lw)) series(is)%linewidth = lw
        if (present(label)) series(is)%label = label

        if (len_trim(series(is)%color) == 0) then
            series(is)%color = color_from_C(color_cycle)
            color_cycle = color_cycle + 1
        end if

        ! marker-only format string => no line
        if (have_fmt .and. series(is)%marker /= MARKER_NONE) then
            if (index(f, "-") == 0 .and. index(f, ":") == 0) then
                series(is)%linestyle = LINE_NONE
            end if
        end if
    end subroutine add_series

    subroutine compute_limits(xmin, xmax, ymin, ymax)
        real(dp), intent(out) :: xmin, xmax, ymin, ymax
        integer :: i, j
        real(dp) :: xv, yv, dx, dy
        logical :: any

        if (xlim_set) then
            xmin = xmin_user
            xmax = xmax_user
        else
            any = .false.
            xmin = huge(1.0_dp)
            xmax = -huge(1.0_dp)
            do i = 1, n_series
                do j = 1, series(i)%n
                    xv = series(i)%x(j)
                    if (xscale == SCALE_LOG .and. xv <= 0.0_dp) cycle
                    any = .true.
                    if (xv < xmin) xmin = xv
                    if (xv > xmax) xmax = xv
                end do
            end do
            if (.not. any) then
                xmin = 0.0_dp
                xmax = 1.0_dp
            end if
        end if

        if (ylim_set) then
            ymin = ymin_user
            ymax = ymax_user
        else
            any = .false.
            ymin = huge(1.0_dp)
            ymax = -huge(1.0_dp)
            do i = 1, n_series
                do j = 1, series(i)%n
                    yv = series(i)%y(j)
                    if (yscale == SCALE_LOG .and. yv <= 0.0_dp) cycle
                    any = .true.
                    if (yv < ymin) ymin = yv
                    if (yv > ymax) ymax = yv
                end do
            end do
            if (.not. any) then
                ymin = 0.0_dp
                ymax = 1.0_dp
            end if
        end if

        if (.not. xlim_set) then
            if (xscale == SCALE_LOG) then
                if (xmin <= 0.0_dp) xmin = tiny(1.0_dp)
                if (xmax <= xmin) xmax = xmin * 10.0_dp
                dx = log10(xmax / xmin)
                if (dx <= 0.0_dp) dx = 1.0_dp
                xmin = xmin / (10.0_dp ** (0.05_dp * dx))
                xmax = xmax * (10.0_dp ** (0.05_dp * dx))
            else
                dx = xmax - xmin
                if (abs(dx) < 1.0e-30_dp) dx = 1.0_dp
                xmin = xmin - 0.05_dp * dx
                xmax = xmax + 0.05_dp * dx
            end if
        end if

        if (.not. ylim_set) then
            if (yscale == SCALE_LOG) then
                if (ymin <= 0.0_dp) ymin = tiny(1.0_dp)
                if (ymax <= ymin) ymax = ymin * 10.0_dp
                dy = log10(ymax / ymin)
                if (dy <= 0.0_dp) dy = 1.0_dp
                ymin = ymin / (10.0_dp ** (0.05_dp * dy))
                ymax = ymax * (10.0_dp ** (0.05_dp * dy))
            else
                dy = ymax - ymin
                if (abs(dy) < 1.0e-30_dp) dy = 1.0_dp
                ymin = ymin - 0.05_dp * dy
                ymax = ymax + 0.05_dp * dy
            end if
        end if
    end subroutine compute_limits

    pure function map_x(x, xmin, xmax, ax_l, ax_w, is_log) result(px)
        real(dp), intent(in) :: x, xmin, xmax, ax_l, ax_w
        logical, intent(in) :: is_log
        real(dp) :: px, t
        if (is_log) then
            if (x <= 0.0_dp .or. xmin <= 0.0_dp .or. xmax <= xmin) then
                px = ax_l
                return
            end if
            t = (log10(x) - log10(xmin)) / (log10(xmax) - log10(xmin))
        else
            if (xmax == xmin) then
                t = 0.5_dp
            else
                t = (x - xmin) / (xmax - xmin)
            end if
        end if
        px = ax_l + t * ax_w
    end function map_x

    pure function map_y(y, ymin, ymax, ax_b, ax_h, is_log) result(py)
        real(dp), intent(in) :: y, ymin, ymax, ax_b, ax_h
        logical, intent(in) :: is_log
        real(dp) :: py, t
        if (is_log) then
            if (y <= 0.0_dp .or. ymin <= 0.0_dp .or. ymax <= ymin) then
                py = ax_b
                return
            end if
            t = (log10(y) - log10(ymin)) / (log10(ymax) - log10(ymin))
        else
            if (ymax == ymin) then
                t = 0.5_dp
            else
                t = (y - ymin) / (ymax - ymin)
            end if
        end if
        py = ax_b - t * ax_h
    end function map_y

    subroutine append_num(b, x)
        type(svg_builder), intent(inout) :: b
        real(dp), intent(in) :: x
        character(len=64) :: s
        integer :: n
        call fmt_num(x, s, n)
        call builder_append(b, s(1:n))
    end subroutine append_num

    subroutine append_dash(b, ls)
        type(svg_builder), intent(inout) :: b
        integer, intent(in) :: ls
        select case (ls)
        case (LINE_DASHED)
            call builder_append(b, ' stroke-dasharray="5.55,2.4"')
        case (LINE_DOTTED)
            call builder_append(b, ' stroke-dasharray="1.5,2.475"')
        case (LINE_DASHDOT)
            call builder_append(b, ' stroke-dasharray="9.9,2.4,1.5,2.4"')
        end select
    end subroutine append_dash

    function render_svg() result(svg)
        character(len=:), allocatable :: svg
        type(svg_builder) :: b
        real(dp) :: W, H, ax_l, ax_r, ax_b, ax_t, ax_w, ax_h
        real(dp) :: xmin, xmax, ymin, ymax
        real(dp) :: xticks(MAX_TICKS), yticks(MAX_TICKS)
        integer :: nxt, nyt, i, j, n, nl
        real(dp) :: px, py, ms, r, mid
        character(len=64) :: lbl
        character(len=512) :: esc
        integer :: ln, en
        logical :: xlog, ylog
        integer :: n_leg
        real(dp) :: leg_x, leg_y, leg_w, leg_h, row_h

        call ensure_fig()
        call builder_init(b)

        W = fig_w_in * 72.0_dp
        H = fig_h_in * 72.0_dp
        ax_l = sub_left * W
        ax_r = sub_right * W
        ax_b = (1.0_dp - sub_bottom) * H
        ax_t = (1.0_dp - sub_top) * H
        ax_w = ax_r - ax_l
        ax_h = ax_b - ax_t

        call compute_limits(xmin, xmax, ymin, ymax)
        xlog = xscale == SCALE_LOG
        ylog = yscale == SCALE_LOG

        if (xlog) then
            call log_ticks(xmin, xmax, xticks, nxt)
        else
            call linear_ticks(xmin, xmax, 6, xticks, nxt)
        end if
        if (ylog) then
            call log_ticks(ymin, ymax, yticks, nyt)
        else
            call linear_ticks(ymin, ymax, 6, yticks, nyt)
        end if

        call builder_append(b, '<?xml version="1.0" encoding="utf-8" standalone="no"?>')
        call builder_append(b, new_line("a"))
        call builder_append(b, '<svg xmlns="http://www.w3.org/2000/svg" ')
        call builder_append(b, 'xmlns:xlink="http://www.w3.org/1999/xlink" width="')
        call append_num(b, W)
        call builder_append(b, 'pt" height="')
        call append_num(b, H)
        call builder_append(b, 'pt" viewBox="0 0 ')
        call append_num(b, W)
        call builder_append(b, " ")
        call append_num(b, H)
        call builder_append(b, '" version="1.1">')
        call builder_append(b, new_line("a"))

        ! background
        call builder_append(b, '<rect x="0" y="0" width="')
        call append_num(b, W)
        call builder_append(b, '" height="')
        call append_num(b, H)
        call builder_append(b, '" fill="#ffffff"/>')
        call builder_append(b, new_line("a"))

        ! clip
        call builder_append(b, '<defs><clipPath id="axclip"><rect x="')
        call append_num(b, ax_l)
        call builder_append(b, '" y="')
        call append_num(b, ax_t)
        call builder_append(b, '" width="')
        call append_num(b, ax_w)
        call builder_append(b, '" height="')
        call append_num(b, ax_h)
        call builder_append(b, '"/></clipPath></defs>')
        call builder_append(b, new_line("a"))

        ! axes face
        call builder_append(b, '<rect x="')
        call append_num(b, ax_l)
        call builder_append(b, '" y="')
        call append_num(b, ax_t)
        call builder_append(b, '" width="')
        call append_num(b, ax_w)
        call builder_append(b, '" height="')
        call append_num(b, ax_h)
        call builder_append(b, '" fill="#ffffff"/>')
        call builder_append(b, new_line("a"))

        ! grid
        if (grid_on) then
            do i = 1, nxt
                px = map_x(xticks(i), xmin, xmax, ax_l, ax_w, xlog)
                call builder_append(b, '<line x1="')
                call append_num(b, px)
                call builder_append(b, '" y1="')
                call append_num(b, ax_t)
                call builder_append(b, '" x2="')
                call append_num(b, px)
                call builder_append(b, '" y2="')
                call append_num(b, ax_b)
                call builder_append(b, '" stroke="#b0b0b0" stroke-width="0.8"/>')
                call builder_append(b, new_line("a"))
            end do
            do i = 1, nyt
                py = map_y(yticks(i), ymin, ymax, ax_b, ax_h, ylog)
                call builder_append(b, '<line x1="')
                call append_num(b, ax_l)
                call builder_append(b, '" y1="')
                call append_num(b, py)
                call builder_append(b, '" x2="')
                call append_num(b, ax_r)
                call builder_append(b, '" y2="')
                call append_num(b, py)
                call builder_append(b, '" stroke="#b0b0b0" stroke-width="0.8"/>')
                call builder_append(b, new_line("a"))
            end do
        end if

        ! data
        call builder_append(b, '<g clip-path="url(#axclip)">')
        call builder_append(b, new_line("a"))
        do i = 1, n_series
            n = series(i)%n
            if (n <= 0) cycle

            if (series(i)%linestyle /= LINE_NONE .and. n >= 2) then
                call builder_append(b, '<polyline fill="none" stroke="')
                call builder_append(b, trim(series(i)%color))
                call builder_append(b, '" stroke-width="')
                call append_num(b, series(i)%linewidth)
                call builder_append(b, '" stroke-linejoin="round" stroke-linecap="butt"')
                call append_dash(b, series(i)%linestyle)
                call builder_append(b, ' points="')
                nl = 0
                do j = 1, n
                    if (xscale == SCALE_LOG .and. series(i)%x(j) <= 0.0_dp) cycle
                    if (yscale == SCALE_LOG .and. series(i)%y(j) <= 0.0_dp) cycle
                    px = map_x(series(i)%x(j), xmin, xmax, ax_l, ax_w, xlog)
                    py = map_y(series(i)%y(j), ymin, ymax, ax_b, ax_h, ylog)
                    if (nl > 0) call builder_append(b, " ")
                    call append_num(b, px)
                    call builder_append(b, ",")
                    call append_num(b, py)
                    nl = nl + 1
                end do
                call builder_append(b, '"/>')
                call builder_append(b, new_line("a"))
            end if

            if (series(i)%marker /= MARKER_NONE) then
                ms = series(i)%markersize
                do j = 1, n
                    if (xscale == SCALE_LOG .and. series(i)%x(j) <= 0.0_dp) cycle
                    if (yscale == SCALE_LOG .and. series(i)%y(j) <= 0.0_dp) cycle
                    px = map_x(series(i)%x(j), xmin, xmax, ax_l, ax_w, xlog)
                    py = map_y(series(i)%y(j), ymin, ymax, ax_b, ax_h, ylog)
                    select case (series(i)%marker)
                    case (MARKER_CIRCLE)
                        r = 0.5_dp * ms * 0.75_dp
                        call builder_append(b, '<circle cx="')
                        call append_num(b, px)
                        call builder_append(b, '" cy="')
                        call append_num(b, py)
                        call builder_append(b, '" r="')
                        call append_num(b, r)
                        call builder_append(b, '" fill="')
                        call builder_append(b, trim(series(i)%color))
                        call builder_append(b, '" stroke="')
                        call builder_append(b, trim(series(i)%color))
                        call builder_append(b, '" stroke-width="1"/>')
                        call builder_append(b, new_line("a"))
                    case (MARKER_POINT)
                        r = 0.5_dp * ms * 0.35_dp
                        call builder_append(b, '<circle cx="')
                        call append_num(b, px)
                        call builder_append(b, '" cy="')
                        call append_num(b, py)
                        call builder_append(b, '" r="')
                        call append_num(b, r)
                        call builder_append(b, '" fill="')
                        call builder_append(b, trim(series(i)%color))
                        call builder_append(b, '"/>')
                        call builder_append(b, new_line("a"))
                    case (MARKER_X)
                        r = 0.5_dp * ms * 0.7_dp
                        call builder_append(b, '<path d="M ')
                        call append_num(b, px - r)
                        call builder_append(b, " ")
                        call append_num(b, py - r)
                        call builder_append(b, " L ")
                        call append_num(b, px + r)
                        call builder_append(b, " ")
                        call append_num(b, py + r)
                        call builder_append(b, " M ")
                        call append_num(b, px - r)
                        call builder_append(b, " ")
                        call append_num(b, py + r)
                        call builder_append(b, " L ")
                        call append_num(b, px + r)
                        call builder_append(b, " ")
                        call append_num(b, py - r)
                        call builder_append(b, '" stroke="')
                        call builder_append(b, trim(series(i)%color))
                        call builder_append(b, '" stroke-width="1.5" fill="none"/>')
                        call builder_append(b, new_line("a"))
                    end select
                end do
            end if
        end do
        call builder_append(b, "</g>")
        call builder_append(b, new_line("a"))

        ! spines
        call builder_append(b, '<rect x="')
        call append_num(b, ax_l)
        call builder_append(b, '" y="')
        call append_num(b, ax_t)
        call builder_append(b, '" width="')
        call append_num(b, ax_w)
        call builder_append(b, '" height="')
        call append_num(b, ax_h)
        call builder_append(b, '" fill="none" stroke="#000000" stroke-width="0.8"/>')
        call builder_append(b, new_line("a"))

        ! x ticks
        do i = 1, nxt
            px = map_x(xticks(i), xmin, xmax, ax_l, ax_w, xlog)
            call builder_append(b, '<line x1="')
            call append_num(b, px)
            call builder_append(b, '" y1="')
            call append_num(b, ax_b)
            call builder_append(b, '" x2="')
            call append_num(b, px)
            call builder_append(b, '" y2="')
            call append_num(b, ax_b + 3.5_dp)
            call builder_append(b, '" stroke="#000000" stroke-width="0.8"/>')
            call builder_append(b, new_line("a"))
            call format_tick_to(xticks(i), xlog, lbl, ln)
            call builder_append(b, '<text x="')
            call append_num(b, px)
            call builder_append(b, '" y="')
            call append_num(b, ax_b + 16.0_dp)
            call builder_append(b, '" text-anchor="middle" font-family="DejaVu Sans, sans-serif" ')
            call builder_append(b, 'font-size="10" fill="#000000">')
            call builder_append(b, lbl(1:ln))
            call builder_append(b, "</text>")
            call builder_append(b, new_line("a"))
        end do

        ! y ticks
        do i = 1, nyt
            py = map_y(yticks(i), ymin, ymax, ax_b, ax_h, ylog)
            call builder_append(b, '<line x1="')
            call append_num(b, ax_l)
            call builder_append(b, '" y1="')
            call append_num(b, py)
            call builder_append(b, '" x2="')
            call append_num(b, ax_l - 3.5_dp)
            call builder_append(b, '" y2="')
            call append_num(b, py)
            call builder_append(b, '" stroke="#000000" stroke-width="0.8"/>')
            call builder_append(b, new_line("a"))
            call format_tick_to(yticks(i), ylog, lbl, ln)
            call builder_append(b, '<text x="')
            call append_num(b, ax_l - 7.0_dp)
            call builder_append(b, '" y="')
            call append_num(b, py + 3.5_dp)
            call builder_append(b, '" text-anchor="end" font-family="DejaVu Sans, sans-serif" ')
            call builder_append(b, 'font-size="10" fill="#000000">')
            call builder_append(b, lbl(1:ln))
            call builder_append(b, "</text>")
            call builder_append(b, new_line("a"))
        end do

        ! xlabel
        if (len_trim(fig_xlabel) > 0) then
            call xml_escape_to(fig_xlabel, esc, en)
            call builder_append(b, '<text x="')
            call append_num(b, 0.5_dp * (ax_l + ax_r))
            call builder_append(b, '" y="')
            call append_num(b, H - 8.0_dp)
            call builder_append(b, '" text-anchor="middle" font-family="DejaVu Sans, sans-serif" ')
            call builder_append(b, 'font-size="11" fill="#000000">')
            call builder_append(b, esc(1:en))
            call builder_append(b, "</text>")
            call builder_append(b, new_line("a"))
        end if

        ! ylabel
        if (len_trim(fig_ylabel) > 0) then
            call xml_escape_to(fig_ylabel, esc, en)
            call builder_append(b, '<text x="')
            call append_num(b, 16.0_dp)
            call builder_append(b, '" y="')
            call append_num(b, 0.5_dp * (ax_t + ax_b))
            call builder_append(b, '" text-anchor="middle" font-family="DejaVu Sans, sans-serif" ')
            call builder_append(b, 'font-size="11" fill="#000000" transform="rotate(-90 ')
            call append_num(b, 16.0_dp)
            call builder_append(b, " ")
            call append_num(b, 0.5_dp * (ax_t + ax_b))
            call builder_append(b, ')">')
            call builder_append(b, esc(1:en))
            call builder_append(b, "</text>")
            call builder_append(b, new_line("a"))
        end if

        ! title
        if (len_trim(fig_title) > 0) then
            call xml_escape_to(fig_title, esc, en)
            call builder_append(b, '<text x="')
            call append_num(b, 0.5_dp * (ax_l + ax_r))
            call builder_append(b, '" y="')
            call append_num(b, ax_t - 10.0_dp)
            call builder_append(b, '" text-anchor="middle" font-family="DejaVu Sans, sans-serif" ')
            call builder_append(b, 'font-size="12" fill="#000000">')
            call builder_append(b, esc(1:en))
            call builder_append(b, "</text>")
            call builder_append(b, new_line("a"))
        end if

        ! legend
        if (legend_on) then
            n_leg = 0
            do i = 1, n_series
                if (len_trim(series(i)%label) > 0) n_leg = n_leg + 1
            end do
            if (n_leg > 0) then
                row_h = 18.0_dp
                leg_w = 100.0_dp
                leg_h = 8.0_dp + real(n_leg, dp) * row_h
                leg_x = ax_r - leg_w - 8.0_dp
                leg_y = ax_t + 8.0_dp
                call builder_append(b, '<rect x="')
                call append_num(b, leg_x)
                call builder_append(b, '" y="')
                call append_num(b, leg_y)
                call builder_append(b, '" width="')
                call append_num(b, leg_w)
                call builder_append(b, '" height="')
                call append_num(b, leg_h)
                call builder_append(b, '" fill="#ffffff" stroke="#cccccc" stroke-width="0.8" rx="2"/>')
                call builder_append(b, new_line("a"))
                n = 0
                do i = 1, n_series
                    if (len_trim(series(i)%label) == 0) cycle
                    n = n + 1
                    py = leg_y + 4.0_dp + (real(n, dp) - 0.5_dp) * row_h
                    if (series(i)%linestyle /= LINE_NONE) then
                        call builder_append(b, '<line x1="')
                        call append_num(b, leg_x + 8.0_dp)
                        call builder_append(b, '" y1="')
                        call append_num(b, py)
                        call builder_append(b, '" x2="')
                        call append_num(b, leg_x + 28.0_dp)
                        call builder_append(b, '" y2="')
                        call append_num(b, py)
                        call builder_append(b, '" stroke="')
                        call builder_append(b, trim(series(i)%color))
                        call builder_append(b, '" stroke-width="')
                        call append_num(b, series(i)%linewidth)
                        call builder_append(b, '"')
                        call append_dash(b, series(i)%linestyle)
                        call builder_append(b, "/>")
                        call builder_append(b, new_line("a"))
                    end if
                    mid = leg_x + 18.0_dp
                    if (series(i)%marker == MARKER_CIRCLE) then
                        call builder_append(b, '<circle cx="')
                        call append_num(b, mid)
                        call builder_append(b, '" cy="')
                        call append_num(b, py)
                        call builder_append(b, '" r="3" fill="')
                        call builder_append(b, trim(series(i)%color))
                        call builder_append(b, '"/>')
                        call builder_append(b, new_line("a"))
                    else if (series(i)%marker == MARKER_POINT) then
                        call builder_append(b, '<circle cx="')
                        call append_num(b, mid)
                        call builder_append(b, '" cy="')
                        call append_num(b, py)
                        call builder_append(b, '" r="1.5" fill="')
                        call builder_append(b, trim(series(i)%color))
                        call builder_append(b, '"/>')
                        call builder_append(b, new_line("a"))
                    else if (series(i)%marker == MARKER_X) then
                        r = 3.0_dp
                        call builder_append(b, '<path d="M ')
                        call append_num(b, mid - r)
                        call builder_append(b, " ")
                        call append_num(b, py - r)
                        call builder_append(b, " L ")
                        call append_num(b, mid + r)
                        call builder_append(b, " ")
                        call append_num(b, py + r)
                        call builder_append(b, " M ")
                        call append_num(b, mid - r)
                        call builder_append(b, " ")
                        call append_num(b, py + r)
                        call builder_append(b, " L ")
                        call append_num(b, mid + r)
                        call builder_append(b, " ")
                        call append_num(b, py - r)
                        call builder_append(b, '" stroke="')
                        call builder_append(b, trim(series(i)%color))
                        call builder_append(b, '" stroke-width="1.5" fill="none"/>')
                        call builder_append(b, new_line("a"))
                    end if
                    call xml_escape_to(series(i)%label, esc, en)
                    call builder_append(b, '<text x="')
                    call append_num(b, leg_x + 34.0_dp)
                    call builder_append(b, '" y="')
                    call append_num(b, py + 3.5_dp)
                    call builder_append(b, '" font-family="DejaVu Sans, sans-serif" font-size="10" fill="#000000">')
                    call builder_append(b, esc(1:en))
                    call builder_append(b, "</text>")
                    call builder_append(b, new_line("a"))
                end do
            end if
        end if

        call builder_append(b, "</svg>")
        call builder_append(b, new_line("a"))
        svg = builder_get(b)
    end function render_svg

    subroutine savefig(filename)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: svg
        integer :: u, ios, n

        svg = render_svg()
        n = len(svg)
        open (newunit=u, file=trim(filename), status="replace", action="write", &
              form="unformatted", access="stream", iostat=ios)
        if (ios /= 0) then
            ! fallback formatted
            open (newunit=u, file=trim(filename), status="replace", action="write", &
                  form="formatted", iostat=ios)
            if (ios /= 0) then
                print *, "fplot: failed to open ", trim(filename)
                return
            end if
            write (u, "(A)") svg
            close (u)
            return
        end if
        if (n > 0) write (u) svg
        close (u)
    end subroutine savefig

    subroutine show()
        ! File backend. In LFortran Jupyter notebooks, prefer:
        !   use lfortran_display
        !   call display_data("image/svg+xml", render_svg())
        call savefig("fplot_show.svg")
        print *, "fplot: wrote fplot_show.svg"
        print *, "fplot: for Jupyter use display_data('image/svg+xml', render_svg())"
    end subroutine show

end module fplot
