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
    public :: subplot, suptitle

    integer, parameter :: SCALE_LINEAR = 0
    integer, parameter :: SCALE_LOG = 1
    integer, parameter :: MAX_SERIES = 32
    integer, parameter :: MAX_POINTS = 100000
    integer, parameter :: MAX_AXES = 16

    ! Default figure margins (matplotlib rcParams), in figure fractions.
    real(dp), parameter :: MARGIN_LEFT = 0.125_dp
    real(dp), parameter :: MARGIN_RIGHT = 0.9_dp
    real(dp), parameter :: MARGIN_BOTTOM = 0.11_dp
    real(dp), parameter :: MARGIN_TOP = 0.88_dp
    ! Default subplot spacing (matplotlib rcParams), relative to axes size.
    real(dp), parameter :: WSPACE = 0.2_dp
    real(dp), parameter :: HSPACE = 0.2_dp

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

    type :: axes_t
        integer :: n_series = 0
        type(series_t) :: series(MAX_SERIES)
        character(len=256) :: title = ""
        character(len=256) :: xlabel = ""
        character(len=256) :: ylabel = ""
        logical :: grid_on = .false.
        logical :: legend_on = .false.
        integer :: xscale = SCALE_LINEAR
        integer :: yscale = SCALE_LINEAR
        logical :: xlim_set = .false.
        logical :: ylim_set = .false.
        real(dp) :: xmin_user = 0.0_dp, xmax_user = 1.0_dp
        real(dp) :: ymin_user = 0.0_dp, ymax_user = 1.0_dp
        integer :: color_cycle = 0
        ! Axes position in figure fractions (matplotlib convention).
        real(dp) :: left = MARGIN_LEFT, right = MARGIN_RIGHT
        real(dp) :: bottom = MARGIN_BOTTOM, top = MARGIN_TOP
    end type axes_t

    ! Figure state (pylab current figure)
    real(dp), save :: fig_w_in = 6.4_dp
    real(dp), save :: fig_h_in = 4.8_dp
    character(len=256), save :: fig_suptitle = ""
    type(axes_t), allocatable, save :: ax(:)
    integer, save :: n_ax = 0
    integer, save :: cur_i = 0
    integer, save :: grid_m = 0, grid_n = 0
    logical, save :: fig_initialized = .false.

contains

    subroutine ensure_fig()
        if (.not. fig_initialized) call clf()
        ! No axes yet: create a single full-figure axes (pylab default).
        if (cur_i < 1 .or. cur_i > n_ax) then
            call new_axes_grid(1, 1)
            cur_i = 1
        end if
    end subroutine ensure_fig

    subroutine figure()
        call clf()
    end subroutine figure

    subroutine clf()
        cur_i = 0
        if (allocated(ax)) deallocate (ax)
        n_ax = 0
        grid_m = 0
        grid_n = 0
        fig_w_in = 6.4_dp
        fig_h_in = 4.8_dp
        fig_suptitle = ""
        fig_initialized = .true.
    end subroutine clf

    ! Replacing the grid discards existing axes.
    subroutine new_axes_grid(m, n)
        integer, intent(in) :: m, n
        integer :: i, r, c
        real(dp) :: w, h

        cur_i = 0
        if (allocated(ax)) deallocate (ax)

        n_ax = m * n
        allocate (ax(n_ax))

        ! Total span available for the grid, in figure fractions.
        w = (MARGIN_RIGHT - MARGIN_LEFT) / &
            (real(n, dp) + WSPACE * real(n - 1, dp))
        h = (MARGIN_TOP - MARGIN_BOTTOM) / &
            (real(m, dp) + HSPACE * real(m - 1, dp))

        do i = 1, n_ax
            r = (i - 1) / n          ! row from the top
            c = mod(i - 1, n)        ! column from the left
            ax(i)%left = MARGIN_LEFT + real(c, dp) * (w + WSPACE * w)
            ax(i)%right = ax(i)%left + w
            ax(i)%bottom = MARGIN_BOTTOM + real(m - 1 - r, dp) * (h + HSPACE * h)
            ax(i)%top = ax(i)%bottom + h
        end do

        grid_m = m
        grid_n = n
    end subroutine new_axes_grid

    ! Select (or create) the i-th axes of an m x n grid.
    ! Forms: subplot(m, n, i), subplot(m, n) [i=1], subplot(231) [code].
    subroutine subplot(m, n, i)
        integer, intent(in) :: m
        integer, intent(in), optional :: n, i
        integer :: mm, nn, ii

        if (present(n)) then
            ! subplot(m, n[, i]) form
            mm = m
            nn = n
            if (present(i)) then
                ii = i
            else
                ii = 1
            end if
        else
            ! Single-integer form: subplot(1) or a 3-digit code like 231.
            mm = m
            if (mm == 1) then
                nn = 1
                ii = 1
            else if (mm >= 100 .and. mm <= 999) then
                ii = mod(mm, 10)
                nn = (mm / 10) - 10 * (mm / 100)
                mm = mm / 100
            else
                print *, "fplot: subplot single argument must be 1 or a 3-digit code (e.g. 231)"
                return
            end if
        end if

        if (mm < 1 .or. nn < 1 .or. ii < 1 .or. ii > mm * nn) then
            print *, "fplot: invalid subplot indices (m=", mm, ", n=", nn, &
                     ", i=", ii, ")"
            return
        end if
        if (mm * nn > MAX_AXES) then
            print *, "fplot: too many subplots (max ", MAX_AXES, ")"
            return
        end if

        if (.not. fig_initialized) call clf()

        if (n_ax == mm * nn .and. grid_m == mm .and. grid_n == nn) then
            cur_i = ii
        else
            call new_axes_grid(mm, nn)
            cur_i = ii
        end if
    end subroutine subplot

    subroutine suptitle(s)
        character(len=*), intent(in) :: s
        call ensure_fig()
        fig_suptitle = s
    end subroutine suptitle

    subroutine title(s)
        character(len=*), intent(in) :: s
        call ensure_fig()
        ax(cur_i)%title = s
    end subroutine title

    subroutine xlabel(s)
        character(len=*), intent(in) :: s
        call ensure_fig()
        ax(cur_i)%xlabel = s
    end subroutine xlabel

    subroutine ylabel(s)
        character(len=*), intent(in) :: s
        call ensure_fig()
        ax(cur_i)%ylabel = s
    end subroutine ylabel

    subroutine grid(on)
        logical, intent(in) :: on
        call ensure_fig()
        ax(cur_i)%grid_on = on
    end subroutine grid

    subroutine legend()
        call ensure_fig()
        ax(cur_i)%legend_on = .true.
    end subroutine legend

    subroutine xlim(xmin, xmax)
        real(dp), intent(in) :: xmin, xmax
        call ensure_fig()
        ax(cur_i)%xmin_user = xmin
        ax(cur_i)%xmax_user = xmax
        ax(cur_i)%xlim_set = .true.
    end subroutine xlim

    subroutine ylim(ymin, ymax)
        real(dp), intent(in) :: ymin, ymax
        call ensure_fig()
        ax(cur_i)%ymin_user = ymin
        ax(cur_i)%ymax_user = ymax
        ax(cur_i)%ylim_set = .true.
    end subroutine ylim

    subroutine plot(x, y, fmt, label, lw, color, marker, linestyle)
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: fmt, label, color, marker, linestyle
        real(dp), intent(in), optional :: lw
        call ensure_fig()
        call add_series(cur_i, x, y, fmt, label, lw, color, marker, linestyle)
    end subroutine plot

    subroutine semilogx(x, y, fmt, label, lw, color)
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: fmt, label, color
        real(dp), intent(in), optional :: lw
        call ensure_fig()
        ax(cur_i)%xscale = SCALE_LOG
        call add_series(cur_i, x, y, fmt, label, lw, color)
    end subroutine semilogx

    subroutine semilogy(x, y, fmt, label, lw, color)
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: fmt, label, color
        real(dp), intent(in), optional :: lw
        call ensure_fig()
        ax(cur_i)%yscale = SCALE_LOG
        call add_series(cur_i, x, y, fmt, label, lw, color)
    end subroutine semilogy

    subroutine loglog(x, y, fmt, label, lw, color)
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: fmt, label, color
        real(dp), intent(in), optional :: lw
        call ensure_fig()
        ax(cur_i)%xscale = SCALE_LOG
        ax(cur_i)%yscale = SCALE_LOG
        call add_series(cur_i, x, y, fmt, label, lw, color)
    end subroutine loglog

    subroutine add_series(ia, x, y, fmt, label, lw, color, marker, linestyle)
        integer, intent(in) :: ia
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
        if (ax(ia)%n_series >= MAX_SERIES) return

        ax(ia)%n_series = ax(ia)%n_series + 1
        is = ax(ia)%n_series

        allocate (ax(ia)%series(is)%x(n), ax(ia)%series(is)%y(n))
        ax(ia)%series(is)%x(1:n) = x(1:n)
        ax(ia)%series(is)%y(1:n) = y(1:n)
        ax(ia)%series(is)%n = n
        ax(ia)%series(is)%linewidth = default_linewidth
        ax(ia)%series(is)%markersize = default_markersize
        ax(ia)%series(is)%label = ""
        ax(ia)%series(is)%marker = MARKER_NONE
        ax(ia)%series(is)%linestyle = LINE_SOLID
        ax(ia)%series(is)%color = ""

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
            ax(ia)%series(is)%marker = m
            if (ls == LINE_NONE .and. m == MARKER_NONE) then
                ax(ia)%series(is)%linestyle = LINE_SOLID
            else if (ls == LINE_NONE .and. m /= MARKER_NONE) then
                ax(ia)%series(is)%linestyle = LINE_NONE
            else
                ax(ia)%series(is)%linestyle = ls
            end if
            if (len_trim(col) > 0) ax(ia)%series(is)%color = col
        end if

        if (present(color)) then
            if (len_trim(color) > 0) then
                if (is_hex_color(trim(color))) then
                    ax(ia)%series(is)%color = color(1:7)
                else if (len_trim(color) == 1) then
                    ax(ia)%series(is)%color = color_from_char(color(1:1))
                else if (len_trim(color) >= 2 .and. color(1:1) == "C") then
                    read (color(2:2), *) m
                    ax(ia)%series(is)%color = color_from_C(m)
                end if
            end if
        end if

        if (present(marker)) then
            select case (trim(marker))
            case ("o"); ax(ia)%series(is)%marker = MARKER_CIRCLE
            case ("x"); ax(ia)%series(is)%marker = MARKER_X
            case ("."); ax(ia)%series(is)%marker = MARKER_POINT
            case ("None", "none", ""); ax(ia)%series(is)%marker = MARKER_NONE
            end select
        end if

        if (present(linestyle)) then
            select case (trim(linestyle))
            case ("-"); ax(ia)%series(is)%linestyle = LINE_SOLID
            case ("--"); ax(ia)%series(is)%linestyle = LINE_DASHED
            case (":"); ax(ia)%series(is)%linestyle = LINE_DOTTED
            case ("-."); ax(ia)%series(is)%linestyle = LINE_DASHDOT
            case ("None", "none", ""); ax(ia)%series(is)%linestyle = LINE_NONE
            end select
        end if

        if (present(lw)) ax(ia)%series(is)%linewidth = lw
        if (present(label)) ax(ia)%series(is)%label = label

        if (len_trim(ax(ia)%series(is)%color) == 0) then
            ax(ia)%series(is)%color = color_from_C(ax(ia)%color_cycle)
            ax(ia)%color_cycle = ax(ia)%color_cycle + 1
        end if

        ! marker-only format string => no line
        if (have_fmt .and. ax(ia)%series(is)%marker /= MARKER_NONE) then
            if (index(f, "-") == 0 .and. index(f, ":") == 0) then
                ax(ia)%series(is)%linestyle = LINE_NONE
            end if
        end if
    end subroutine add_series

    subroutine compute_limits(a, xmin, xmax, ymin, ymax)
        type(axes_t), intent(in) :: a
        real(dp), intent(out) :: xmin, xmax, ymin, ymax
        integer :: i, j
        real(dp) :: xv, yv, dx, dy
        logical :: any

        if (a%xlim_set) then
            xmin = a%xmin_user
            xmax = a%xmax_user
        else
            any = .false.
            xmin = huge(1.0_dp)
            xmax = -huge(1.0_dp)
            do i = 1, a%n_series
                do j = 1, a%series(i)%n
                    xv = a%series(i)%x(j)
                    if (a%xscale == SCALE_LOG .and. xv <= 0.0_dp) cycle
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

        if (a%ylim_set) then
            ymin = a%ymin_user
            ymax = a%ymax_user
        else
            any = .false.
            ymin = huge(1.0_dp)
            ymax = -huge(1.0_dp)
            do i = 1, a%n_series
                do j = 1, a%series(i)%n
                    yv = a%series(i)%y(j)
                    if (a%yscale == SCALE_LOG .and. yv <= 0.0_dp) cycle
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

        if (.not. a%xlim_set) then
            if (a%xscale == SCALE_LOG) then
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

        if (.not. a%ylim_set) then
            if (a%yscale == SCALE_LOG) then
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

    pure function int_to_str(i) result(s)
        integer, intent(in) :: i
        character(len=:), allocatable :: s
        character(len=8) :: tmp
        write (tmp, "(I0)") i
        s = trim(tmp)
    end function int_to_str

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

    subroutine render_axes(b, a, idx, W, H)
        type(svg_builder), intent(inout) :: b
        type(axes_t), intent(in) :: a
        integer, intent(in) :: idx
        real(dp), intent(in) :: W, H
        real(dp) :: axl, axr, axb, axt, axw, axh
        real(dp) :: xmin, xmax, ymin, ymax
        real(dp) :: xticks(MAX_TICKS), yticks(MAX_TICKS)
        integer :: nxt, nyt, i, j, n, nl
        real(dp) :: px, py, ms, r, mid
        character(len=64) :: lbl
        character(len=512) :: esc
        integer :: ln, en
        logical :: xlog, ylog
        integer :: n_leg, k
        real(dp) :: leg_x, leg_y, leg_w, leg_h, row_h

        axl = a%left * W
        axr = a%right * W
        axb = (1.0_dp - a%bottom) * H
        axt = (1.0_dp - a%top) * H
        axw = axr - axl
        axh = axb - axt

        call compute_limits(a, xmin, xmax, ymin, ymax)
        xlog = a%xscale == SCALE_LOG
        ylog = a%yscale == SCALE_LOG

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

        ! axes face
        call builder_append(b, '<rect x="')
        call append_num(b, axl)
        call builder_append(b, '" y="')
        call append_num(b, axt)
        call builder_append(b, '" width="')
        call append_num(b, axw)
        call builder_append(b, '" height="')
        call append_num(b, axh)
        call builder_append(b, '" fill="#ffffff"/>')
        call builder_append(b, new_line("a"))

        ! grid
        if (a%grid_on) then
            do i = 1, nxt
                px = map_x(xticks(i), xmin, xmax, axl, axw, xlog)
                call builder_append(b, '<line x1="')
                call append_num(b, px)
                call builder_append(b, '" y1="')
                call append_num(b, axt)
                call builder_append(b, '" x2="')
                call append_num(b, px)
                call builder_append(b, '" y2="')
                call append_num(b, axb)
                call builder_append(b, '" stroke="#b0b0b0" stroke-width="0.8"/>')
                call builder_append(b, new_line("a"))
            end do
            do i = 1, nyt
                py = map_y(yticks(i), ymin, ymax, axb, axh, ylog)
                call builder_append(b, '<line x1="')
                call append_num(b, axl)
                call builder_append(b, '" y1="')
                call append_num(b, py)
                call builder_append(b, '" x2="')
                call append_num(b, axr)
                call builder_append(b, '" y2="')
                call append_num(b, py)
                call builder_append(b, '" stroke="#b0b0b0" stroke-width="0.8"/>')
                call builder_append(b, new_line("a"))
            end do
        end if

        ! data
        call builder_append(b, '<g clip-path="url(#axclip')
        call builder_append(b, int_to_str(idx))
        call builder_append(b, ')">')
        call builder_append(b, new_line("a"))
        do i = 1, a%n_series
            n = a%series(i)%n
            if (n <= 0) cycle

            if (a%series(i)%linestyle /= LINE_NONE .and. n >= 2) then
                call builder_append(b, '<polyline fill="none" stroke="')
                call builder_append(b, trim(a%series(i)%color))
                call builder_append(b, '" stroke-width="')
                call append_num(b, a%series(i)%linewidth)
                call builder_append(b, '" stroke-linejoin="round" stroke-linecap="butt"')
                call append_dash(b, a%series(i)%linestyle)
                call builder_append(b, ' points="')
                nl = 0
                do j = 1, n
                    if (a%xscale == SCALE_LOG .and. a%series(i)%x(j) <= 0.0_dp) cycle
                    if (a%yscale == SCALE_LOG .and. a%series(i)%y(j) <= 0.0_dp) cycle
                    px = map_x(a%series(i)%x(j), xmin, xmax, axl, axw, xlog)
                    py = map_y(a%series(i)%y(j), ymin, ymax, axb, axh, ylog)
                    if (nl > 0) call builder_append(b, " ")
                    call append_num(b, px)
                    call builder_append(b, ",")
                    call append_num(b, py)
                    nl = nl + 1
                end do
                call builder_append(b, '"/>')
                call builder_append(b, new_line("a"))
            end if

            if (a%series(i)%marker /= MARKER_NONE) then
                ms = a%series(i)%markersize
                do j = 1, n
                    if (a%xscale == SCALE_LOG .and. a%series(i)%x(j) <= 0.0_dp) cycle
                    if (a%yscale == SCALE_LOG .and. a%series(i)%y(j) <= 0.0_dp) cycle
                    px = map_x(a%series(i)%x(j), xmin, xmax, axl, axw, xlog)
                    py = map_y(a%series(i)%y(j), ymin, ymax, axb, axh, ylog)
                    select case (a%series(i)%marker)
                    case (MARKER_CIRCLE)
                        r = 0.5_dp * ms * 0.75_dp
                        call builder_append(b, '<circle cx="')
                        call append_num(b, px)
                        call builder_append(b, '" cy="')
                        call append_num(b, py)
                        call builder_append(b, '" r="')
                        call append_num(b, r)
                        call builder_append(b, '" fill="')
                        call builder_append(b, trim(a%series(i)%color))
                        call builder_append(b, '" stroke="')
                        call builder_append(b, trim(a%series(i)%color))
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
                        call builder_append(b, trim(a%series(i)%color))
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
                        call builder_append(b, trim(a%series(i)%color))
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
        call append_num(b, axl)
        call builder_append(b, '" y="')
        call append_num(b, axt)
        call builder_append(b, '" width="')
        call append_num(b, axw)
        call builder_append(b, '" height="')
        call append_num(b, axh)
        call builder_append(b, '" fill="none" stroke="#000000" stroke-width="0.8"/>')
        call builder_append(b, new_line("a"))

        ! x ticks
        do i = 1, nxt
            px = map_x(xticks(i), xmin, xmax, axl, axw, xlog)
            call builder_append(b, '<line x1="')
            call append_num(b, px)
            call builder_append(b, '" y1="')
            call append_num(b, axb)
            call builder_append(b, '" x2="')
            call append_num(b, px)
            call builder_append(b, '" y2="')
            call append_num(b, axb + 3.5_dp)
            call builder_append(b, '" stroke="#000000" stroke-width="0.8"/>')
            call builder_append(b, new_line("a"))
            call format_tick_to(xticks(i), xlog, lbl, ln)
            call builder_append(b, '<text x="')
            call append_num(b, px)
            call builder_append(b, '" y="')
            call append_num(b, axb + 16.0_dp)
            call builder_append(b, '" text-anchor="middle" font-family="DejaVu Sans, sans-serif" ')
            call builder_append(b, 'font-size="10" fill="#000000">')
            call builder_append(b, lbl(1:ln))
            call builder_append(b, "</text>")
            call builder_append(b, new_line("a"))
        end do

        ! y ticks
        do i = 1, nyt
            py = map_y(yticks(i), ymin, ymax, axb, axh, ylog)
            call builder_append(b, '<line x1="')
            call append_num(b, axl)
            call builder_append(b, '" y1="')
            call append_num(b, py)
            call builder_append(b, '" x2="')
            call append_num(b, axl - 3.5_dp)
            call builder_append(b, '" y2="')
            call append_num(b, py)
            call builder_append(b, '" stroke="#000000" stroke-width="0.8"/>')
            call builder_append(b, new_line("a"))
            call format_tick_to(yticks(i), ylog, lbl, ln)
            call builder_append(b, '<text x="')
            call append_num(b, axl - 7.0_dp)
            call builder_append(b, '" y="')
            call append_num(b, py + 3.5_dp)
            call builder_append(b, '" text-anchor="end" font-family="DejaVu Sans, sans-serif" ')
            call builder_append(b, 'font-size="10" fill="#000000">')
            call builder_append(b, lbl(1:ln))
            call builder_append(b, "</text>")
            call builder_append(b, new_line("a"))
        end do

        if (len_trim(a%xlabel) > 0) then
            call xml_escape_to(a%xlabel, esc, en)
            call builder_append(b, '<text x="')
            call append_num(b, 0.5_dp * (axl + axr))
            call builder_append(b, '" y="')
            call append_num(b, axb + 28.6_dp)
            call builder_append(b, '" text-anchor="middle" font-family="DejaVu Sans, sans-serif" ')
            call builder_append(b, 'font-size="11" fill="#000000">')
            call builder_append(b, esc(1:en))
            call builder_append(b, "</text>")
            call builder_append(b, new_line("a"))
        end if

        if (len_trim(a%ylabel) > 0) then
            call xml_escape_to(a%ylabel, esc, en)
            call builder_append(b, '<text x="')
            call append_num(b, axl - 34.0_dp)
            call builder_append(b, '" y="')
            call append_num(b, 0.5_dp * (axt + axb))
            call builder_append(b, '" text-anchor="middle" font-family="DejaVu Sans, sans-serif" ')
            call builder_append(b, 'font-size="11" fill="#000000" transform="rotate(-90 ')
            call append_num(b, axl - 34.0_dp)
            call builder_append(b, " ")
            call append_num(b, 0.5_dp * (axt + axb))
            call builder_append(b, ')">')
            call builder_append(b, esc(1:en))
            call builder_append(b, "</text>")
            call builder_append(b, new_line("a"))
        end if

        ! title
        if (len_trim(a%title) > 0) then
            call xml_escape_to(a%title, esc, en)
            call builder_append(b, '<text x="')
            call append_num(b, 0.5_dp * (axl + axr))
            call builder_append(b, '" y="')
            call append_num(b, axt - 6.0_dp)
            call builder_append(b, '" text-anchor="middle" font-family="DejaVu Sans, sans-serif" ')
            call builder_append(b, 'font-size="12" fill="#000000">')
            call builder_append(b, esc(1:en))
            call builder_append(b, "</text>")
            call builder_append(b, new_line("a"))
        end if

        ! legend
        if (a%legend_on) then
            n_leg = 0
            do i = 1, a%n_series
                if (len_trim(a%series(i)%label) > 0) n_leg = n_leg + 1
            end do
            if (n_leg > 0) then
                row_h = 18.0_dp
                leg_w = 100.0_dp
                leg_h = 8.0_dp + real(n_leg, dp) * row_h
                leg_x = axr - leg_w - 8.0_dp
                leg_y = axt + 8.0_dp
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
                k = 0
                do i = 1, a%n_series
                    if (len_trim(a%series(i)%label) == 0) cycle
                    k = k + 1
                    py = leg_y + 4.0_dp + (real(k, dp) - 0.5_dp) * row_h
                    if (a%series(i)%linestyle /= LINE_NONE) then
                        call builder_append(b, '<line x1="')
                        call append_num(b, leg_x + 8.0_dp)
                        call builder_append(b, '" y1="')
                        call append_num(b, py)
                        call builder_append(b, '" x2="')
                        call append_num(b, leg_x + 28.0_dp)
                        call builder_append(b, '" y2="')
                        call append_num(b, py)
                        call builder_append(b, '" stroke="')
                        call builder_append(b, trim(a%series(i)%color))
                        call builder_append(b, '" stroke-width="')
                        call append_num(b, a%series(i)%linewidth)
                        call builder_append(b, '"')
                        call append_dash(b, a%series(i)%linestyle)
                        call builder_append(b, "/>")
                        call builder_append(b, new_line("a"))
                    end if
                    mid = leg_x + 18.0_dp
                    if (a%series(i)%marker == MARKER_CIRCLE) then
                        call builder_append(b, '<circle cx="')
                        call append_num(b, mid)
                        call builder_append(b, '" cy="')
                        call append_num(b, py)
                        call builder_append(b, '" r="3" fill="')
                        call builder_append(b, trim(a%series(i)%color))
                        call builder_append(b, '"/>')
                        call builder_append(b, new_line("a"))
                    else if (a%series(i)%marker == MARKER_POINT) then
                        call builder_append(b, '<circle cx="')
                        call append_num(b, mid)
                        call builder_append(b, '" cy="')
                        call append_num(b, py)
                        call builder_append(b, '" r="1.5" fill="')
                        call builder_append(b, trim(a%series(i)%color))
                        call builder_append(b, '"/>')
                        call builder_append(b, new_line("a"))
                    else if (a%series(i)%marker == MARKER_X) then
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
                        call builder_append(b, trim(a%series(i)%color))
                        call builder_append(b, '" stroke-width="1.5" fill="none"/>')
                        call builder_append(b, new_line("a"))
                    end if
                    call xml_escape_to(a%series(i)%label, esc, en)
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
    end subroutine render_axes

    function render_svg() result(svg)
        character(len=:), allocatable :: svg
        type(svg_builder) :: b
        real(dp) :: W, H
        character(len=512) :: esc
        integer :: i, en

        call ensure_fig()
        call builder_init(b)

        W = fig_w_in * 72.0_dp
        H = fig_h_in * 72.0_dp

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

        ! clip paths (one per axes)
        call builder_append(b, "<defs>")
        do i = 1, n_ax
            call builder_append(b, '<clipPath id="axclip')
            call builder_append(b, int_to_str(i))
            call builder_append(b, '"><rect x="')
            call append_num(b, ax(i)%left * W)
            call builder_append(b, '" y="')
            call append_num(b, (1.0_dp - ax(i)%top) * H)
            call builder_append(b, '" width="')
            call append_num(b, (ax(i)%right - ax(i)%left) * W)
            call builder_append(b, '" height="')
            call append_num(b, (ax(i)%top - ax(i)%bottom) * H)
            call builder_append(b, '"/></clipPath>')
        end do
        call builder_append(b, "</defs>")
        call builder_append(b, new_line("a"))

        ! axes (subplots)
        do i = 1, n_ax
            call render_axes(b, ax(i), i, W, H)
        end do

        ! suptitle (figure-level, above all axes)
        if (len_trim(fig_suptitle) > 0) then
            call xml_escape_to(fig_suptitle, esc, en)
            call builder_append(b, '<text x="')
            call append_num(b, 0.5_dp * W)
            call builder_append(b, '" y="')
            call append_num(b, (1.0_dp - 0.98_dp) * H + 4.2_dp)
            call builder_append(b, '" text-anchor="middle" font-family="DejaVu Sans, sans-serif" ')
            call builder_append(b, 'font-size="12" fill="#000000">')
            call builder_append(b, esc(1:en))
            call builder_append(b, "</text>")
            call builder_append(b, new_line("a"))
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
