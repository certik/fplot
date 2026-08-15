! fplot — pure Fortran pylab-style SVG plotting library.
module fplot
    use fplot_style
    use fplot_ticks
    use fplot_svg
    implicit none
    private

    public :: dp
    public :: plot, scatter, semilogx, semilogy, loglog
    public :: bar, hist, fill_between, errorbar, axhline, axvline
    public :: title, xlabel, ylabel, grid, legend
    public :: xlim, ylim, clf, savefig, show, figure
    public :: render_svg
    public :: subplot, suptitle

    integer, parameter :: SCALE_LINEAR = 0
    integer, parameter :: SCALE_LOG = 1
    integer, parameter :: MAX_SERIES = 32
    integer, parameter :: MAX_POINTS = 100000

    ! Default figure margins (matplotlib rcParams), in figure fractions.
    real(dp), parameter :: MARGIN_LEFT = 0.125_dp
    real(dp), parameter :: MARGIN_RIGHT = 0.9_dp
    real(dp), parameter :: MARGIN_BOTTOM = 0.11_dp
    real(dp), parameter :: MARGIN_TOP = 0.88_dp
    ! Default subplot spacing (matplotlib rcParams), relative to axes size.
    real(dp), parameter :: WSPACE = 0.2_dp
    real(dp), parameter :: HSPACE = 0.2_dp
    ! Suptitle baseline (matplotlib figure.suptitle default y), in figure
    ! fractions measured from the bottom.
    real(dp), parameter :: SUPTITLE_Y = 0.98_dp
    ! Mean advance width of DejaVu Sans at the 10 pt legend font size, used
    ! to size the legend box to its labels.
    real(dp), parameter :: LEGEND_CHAR_W = 5.6_dp
    real(dp), parameter :: PI = 3.141592653589793_dp

    ! What a series draws. LINE covers plot/scatter/semilog*; the rest are
    ! the shape-based plot types.
    integer, parameter :: SERIES_LINE = 0
    integer, parameter :: SERIES_BAR = 1
    integer, parameter :: SERIES_FILL = 2
    integer, parameter :: SERIES_ERRORBAR = 3
    integer, parameter :: SERIES_HLINE = 4
    integer, parameter :: SERIES_VLINE = 5

    type :: series_t
        integer :: kind = SERIES_LINE
        integer :: n = 0
        real(dp), allocatable :: x(:)
        real(dp), allocatable :: y(:)
        ! FILL: lower edge. ERRORBAR: symmetric y error.
        real(dp), allocatable :: y2(:)
        character(len=7) :: color = "#1f77b4"
        integer :: marker = MARKER_NONE
        integer :: linestyle = LINE_SOLID
        real(dp) :: linewidth = 1.5_dp
        real(dp) :: markersize = 6.0_dp
        real(dp) :: width = 0.8_dp
        real(dp) :: alpha = 1.0_dp
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
        real(dp) :: w, h, dx, dy

        cur_i = 0
        if (allocated(ax)) deallocate (ax)

        n_ax = m * n
        allocate (ax(n_ax))

        ! Cell size and cell pitch (cell plus gap), in figure fractions.
        w = (MARGIN_RIGHT - MARGIN_LEFT) / &
            (real(n, dp) + WSPACE * real(n - 1, dp))
        h = (MARGIN_TOP - MARGIN_BOTTOM) / &
            (real(m, dp) + HSPACE * real(m - 1, dp))
        dx = w * (1.0_dp + WSPACE)
        dy = h * (1.0_dp + HSPACE)

        do i = 1, n_ax
            r = (i - 1) / n          ! row from the top
            c = mod(i - 1, n)        ! column from the left
            ax(i)%left = MARGIN_LEFT + real(c, dp) * dx
            ax(i)%right = ax(i)%left + w
            ax(i)%bottom = MARGIN_BOTTOM + real(m - 1 - r, dp) * dy
            ax(i)%top = ax(i)%bottom + h
        end do

        grid_m = m
        grid_n = n
    end subroutine new_axes_grid

    ! Select the i-th axes (row-major) of an m x n grid, creating the grid
    ! if it differs from the current one.
    subroutine subplot(m, n, i)
        integer, intent(in) :: m, n, i

        if (m < 1 .or. n < 1 .or. i < 1 .or. i > m * n) then
            print *, "fplot: invalid subplot indices: m=", m, " n=", n, " i=", i
            error stop
        end if

        if (.not. fig_initialized) call clf()
        if (grid_m /= m .or. grid_n /= n) call new_axes_grid(m, n)
        cur_i = i
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

    ! Marker-only plot. s is the marker area in points^2 (matplotlib's
    ! convention), so the marker size is its square root.
    subroutine scatter(x, y, s, c, marker, label)
        real(dp), intent(in) :: x(:), y(:)
        real(dp), intent(in), optional :: s
        character(len=*), intent(in), optional :: c, marker, label
        integer :: is

        call ensure_fig()
        if (present(marker)) then
            call add_series(cur_i, x, y, label=label, color=c, marker=marker, &
                            linestyle="None")
        else
            call add_series(cur_i, x, y, label=label, color=c, marker="o", &
                            linestyle="None")
        end if
        is = ax(cur_i)%n_series
        if (is >= 1) then
            if (present(s)) ax(cur_i)%series(is)%markersize = sqrt(max(s, 0.0_dp))
        end if
    end subroutine scatter

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

    ! Start a new series of the given kind and return its index, applying the
    ! shared bookkeeping (point count, colour cycling, label).
    function new_shape_series(kd, x, y, color, label, alpha) result(is)
        integer, intent(in) :: kd
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: color, label
        real(dp), intent(in), optional :: alpha
        integer :: is, n

        is = 0
        n = min(size(x), size(y))
        if (n <= 0) return
        if (n > MAX_POINTS) n = MAX_POINTS
        if (ax(cur_i)%n_series >= MAX_SERIES) return

        ax(cur_i)%n_series = ax(cur_i)%n_series + 1
        is = ax(cur_i)%n_series
        allocate (ax(cur_i)%series(is)%x(n), ax(cur_i)%series(is)%y(n))
        ax(cur_i)%series(is)%x(1:n) = x(1:n)
        ax(cur_i)%series(is)%y(1:n) = y(1:n)
        ax(cur_i)%series(is)%n = n
        ax(cur_i)%series(is)%kind = kd
        ax(cur_i)%series(is)%marker = MARKER_NONE
        ax(cur_i)%series(is)%linestyle = LINE_SOLID
        ax(cur_i)%series(is)%linewidth = default_linewidth
        ax(cur_i)%series(is)%markersize = default_markersize
        ax(cur_i)%series(is)%label = ""
        if (present(label)) ax(cur_i)%series(is)%label = label
        if (present(alpha)) ax(cur_i)%series(is)%alpha = alpha

        ax(cur_i)%series(is)%color = resolve_color(color)
        if (len_trim(ax(cur_i)%series(is)%color) == 0) then
            ax(cur_i)%series(is)%color = color_from_C(ax(cur_i)%color_cycle)
            ax(cur_i)%color_cycle = ax(cur_i)%color_cycle + 1
        end if
    end function new_shape_series

    ! Accept "#rrggbb", a single-letter name, or a "C<n>" cycle index.
    function resolve_color(color) result(col)
        character(len=*), intent(in), optional :: color
        character(len=7) :: col
        integer :: m

        col = ""
        if (.not. present(color)) return
        if (len_trim(color) == 0) return
        if (is_hex_color(trim(color))) then
            col = color(1:7)
        else if (len_trim(color) == 1) then
            col = color_from_char(color(1:1))
        else if (len_trim(color) >= 2 .and. color(1:1) == "C") then
            read (color(2:2), *) m
            col = color_from_C(m)
        end if
    end function resolve_color

    ! Vertical bars of the given heights, centred on x and drawn from y = 0.
    subroutine bar(x, height, width, color, label, alpha)
        real(dp), intent(in) :: x(:), height(:)
        real(dp), intent(in), optional :: width, alpha
        character(len=*), intent(in), optional :: color, label
        integer :: is

        call ensure_fig()
        is = new_shape_series(SERIES_BAR, x, height, color, label, alpha)
        if (is < 1) return
        if (present(width)) ax(cur_i)%series(is)%width = width
    end subroutine bar

    ! Histogram of x using `bins` equal-width bins over the data range.
    subroutine hist(x, bins, color, label, alpha)
        real(dp), intent(in) :: x(:)
        integer, intent(in), optional :: bins
        character(len=*), intent(in), optional :: color, label
        real(dp), intent(in), optional :: alpha
        integer :: nb, i, k, n, is
        real(dp) :: lo, hi, w
        real(dp), allocatable :: centers(:), counts(:)

        n = size(x)
        if (n <= 0) return
        nb = 10
        if (present(bins)) nb = bins
        if (nb < 1) return

        lo = minval(x)
        hi = maxval(x)
        if (hi <= lo) then
            lo = lo - 0.5_dp
            hi = hi + 0.5_dp
        end if
        w = (hi - lo) / real(nb, dp)

        allocate (centers(nb), counts(nb))
        counts = 0.0_dp
        do i = 1, nb
            centers(i) = lo + (real(i, dp) - 0.5_dp) * w
        end do
        do i = 1, n
            k = int((x(i) - lo) / w) + 1
            if (k < 1) k = 1
            if (k > nb) k = nb          ! the top edge belongs to the last bin
            counts(k) = counts(k) + 1.0_dp
        end do

        call ensure_fig()
        is = new_shape_series(SERIES_BAR, centers, counts, color, label, alpha)
        if (is >= 1) ax(cur_i)%series(is)%width = w
    end subroutine hist

    ! Shade between y1 and y2 (default 0).
    subroutine fill_between(x, y1, y2, color, label, alpha)
        real(dp), intent(in) :: x(:), y1(:)
        real(dp), intent(in), optional :: y2(:)
        character(len=*), intent(in), optional :: color, label
        real(dp), intent(in), optional :: alpha
        integer :: is, n

        call ensure_fig()
        is = new_shape_series(SERIES_FILL, x, y1, color, label, alpha)
        if (is < 1) return
        n = ax(cur_i)%series(is)%n
        allocate (ax(cur_i)%series(is)%y2(n))
        if (present(y2)) then
            ax(cur_i)%series(is)%y2(1:n) = y2(1:n)
        else
            ax(cur_i)%series(is)%y2(1:n) = 0.0_dp
        end if
    end subroutine fill_between

    ! Line plot with symmetric vertical error bars.
    subroutine errorbar(x, y, yerr, fmt, color, label, capsize, marker)
        real(dp), intent(in) :: x(:), y(:), yerr(:)
        character(len=*), intent(in), optional :: fmt, color, label, marker
        real(dp), intent(in), optional :: capsize
        integer :: is, n, mk, ls
        character(len=7) :: col

        call ensure_fig()
        is = new_shape_series(SERIES_ERRORBAR, x, y, color, label)
        if (is < 1) return
        n = ax(cur_i)%series(is)%n
        allocate (ax(cur_i)%series(is)%y2(n))
        ax(cur_i)%series(is)%y2(1:n) = abs(yerr(1:n))

        if (present(fmt)) then
            if (len_trim(fmt) > 0) then
                call parse_fmt(trim(fmt), col, mk, ls)
                ax(cur_i)%series(is)%marker = mk
                ax(cur_i)%series(is)%linestyle = ls
                if (len_trim(col) > 0) ax(cur_i)%series(is)%color = col
            end if
        end if
        if (present(marker)) ax(cur_i)%series(is)%marker = marker_from_char(marker(1:1))
        ! width doubles as the cap half-width, in points.
        ax(cur_i)%series(is)%width = 3.0_dp
        if (present(capsize)) ax(cur_i)%series(is)%width = capsize
    end subroutine errorbar

    ! Reference line spanning the full axes.
    subroutine axhline(y, color, linestyle, lw, label)
        real(dp), intent(in) :: y
        character(len=*), intent(in), optional :: color, linestyle, label
        real(dp), intent(in), optional :: lw
        call add_ref_line(SERIES_HLINE, y, color, linestyle, lw, label)
    end subroutine axhline

    subroutine axvline(x, color, linestyle, lw, label)
        real(dp), intent(in) :: x
        character(len=*), intent(in), optional :: color, linestyle, label
        real(dp), intent(in), optional :: lw
        call add_ref_line(SERIES_VLINE, x, color, linestyle, lw, label)
    end subroutine axvline

    subroutine add_ref_line(kd, v, color, linestyle, lw, label)
        integer, intent(in) :: kd
        real(dp), intent(in) :: v
        character(len=*), intent(in), optional :: color, linestyle, label
        real(dp), intent(in), optional :: lw
        integer :: is

        call ensure_fig()
        is = new_shape_series(kd, [v], [v], color, label)
        if (is < 1) return
        ! Reference lines default to black, not to the colour cycle.
        if (.not. present(color)) then
            ax(cur_i)%series(is)%color = "#000000"
            ax(cur_i)%color_cycle = ax(cur_i)%color_cycle - 1
        end if
        if (present(linestyle)) ax(cur_i)%series(is)%linestyle = linestyle_from_str(linestyle)
        if (present(lw)) ax(cur_i)%series(is)%linewidth = lw
    end subroutine add_ref_line

    pure function linestyle_from_str(s) result(ls)
        character(len=*), intent(in) :: s
        integer :: ls
        select case (trim(s))
        case ("-"); ls = LINE_SOLID
        case ("--"); ls = LINE_DASHED
        case (":"); ls = LINE_DOTTED
        case ("-."); ls = LINE_DASHDOT
        case ("None", "none", ""); ls = LINE_NONE
        case default; ls = LINE_SOLID
        end select
    end function linestyle_from_str

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

        col = resolve_color(color)
        if (len_trim(col) > 0) ax(ia)%series(is)%color = col

        if (present(marker)) then
            select case (trim(marker))
            case ("None", "none", ""); ax(ia)%series(is)%marker = MARKER_NONE
            case default
                ax(ia)%series(is)%marker = marker_from_char(marker(1:1))
            end select
        end if

        if (present(linestyle)) &
            ax(ia)%series(is)%linestyle = linestyle_from_str(linestyle)

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
        real(dp) :: xv, yv, ylo, yhi, dx, dy, hw
        logical :: anyx, anyy, sticky_lo, sticky_hi

        anyx = .false.
        anyy = .false.
        sticky_lo = .false.
        sticky_hi = .false.
        xmin = huge(1.0_dp)
        xmax = -huge(1.0_dp)
        ymin = huge(1.0_dp)
        ymax = -huge(1.0_dp)

        do i = 1, a%n_series
            ! Bars occupy a span in x; a hline/vline constrains one axis only.
            hw = 0.0_dp
            if (a%series(i)%kind == SERIES_BAR) hw = 0.5_dp * a%series(i)%width

            do j = 1, a%series(i)%n
                if (a%series(i)%kind /= SERIES_HLINE) then
                    xv = a%series(i)%x(j)
                    if (.not. (a%xscale == SCALE_LOG .and. xv - hw <= 0.0_dp)) then
                        anyx = .true.
                        xmin = min(xmin, xv - hw)
                        xmax = max(xmax, xv + hw)
                    end if
                end if

                if (a%series(i)%kind /= SERIES_VLINE) then
                    yv = a%series(i)%y(j)
                    ylo = yv
                    yhi = yv
                    select case (a%series(i)%kind)
                    case (SERIES_BAR)
                        ! Bars are drawn from the y=0 baseline.
                        ylo = min(0.0_dp, yv)
                        yhi = max(0.0_dp, yv)
                    case (SERIES_FILL)
                        ylo = min(yv, a%series(i)%y2(j))
                        yhi = max(yv, a%series(i)%y2(j))
                    case (SERIES_ERRORBAR)
                        ylo = yv - a%series(i)%y2(j)
                        yhi = yv + a%series(i)%y2(j)
                    end select
                    if (.not. (a%yscale == SCALE_LOG .and. yhi <= 0.0_dp)) then
                        anyy = .true.
                        ymin = min(ymin, ylo)
                        ymax = max(ymax, yhi)
                    end if
                end if
            end do

            ! Matplotlib treats the bar baseline as a sticky edge: no margin
            ! is added on the side the bars grow from.
            if (a%series(i)%kind == SERIES_BAR .and. a%series(i)%n > 0) then
                if (ymin >= 0.0_dp) sticky_lo = .true.
                if (ymax <= 0.0_dp) sticky_hi = .true.
            end if
        end do

        if (a%xlim_set) then
            xmin = a%xmin_user
            xmax = a%xmax_user
        else
            if (.not. anyx) then
                xmin = 0.0_dp
                xmax = 1.0_dp
            end if
            call expand_limits(xmin, xmax, a%xscale == SCALE_LOG, .false., .false.)
        end if

        if (a%ylim_set) then
            ymin = a%ymin_user
            ymax = a%ymax_user
        else
            if (.not. anyy) then
                ymin = 0.0_dp
                ymax = 1.0_dp
            end if
            call expand_limits(ymin, ymax, a%yscale == SCALE_LOG, sticky_lo, sticky_hi)
        end if
    end subroutine compute_limits

    ! Pad a data range by matplotlib's 5% margin. A sticky edge (the bar
    ! baseline) is left exactly where it is.
    subroutine expand_limits(lo, hi, is_log, sticky_lo, sticky_hi)
        real(dp), intent(inout) :: lo, hi
        logical, intent(in) :: is_log, sticky_lo, sticky_hi
        real(dp) :: d, f

        if (is_log) then
            if (lo <= 0.0_dp) lo = tiny(1.0_dp)
            if (hi <= lo) hi = lo * 10.0_dp
            d = log10(hi / lo)
            if (d <= 0.0_dp) d = 1.0_dp
            f = 10.0_dp ** (0.05_dp * d)
            if (.not. sticky_lo) lo = lo / f
            if (.not. sticky_hi) hi = hi * f
        else
            d = hi - lo
            if (abs(d) < 1.0e-30_dp) d = 1.0_dp
            if (.not. sticky_lo) lo = lo - 0.05_dp * d
            if (.not. sticky_hi) hi = hi + 0.05_dp * d
        end if
    end subroutine expand_limits

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
        character(len=12) :: tmp
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

    ! Emit a stroked open path through the given points, e.g. an "x" or "+".
    subroutine append_stroke_path(b, px, py, np, color, lw)
        type(svg_builder), intent(inout) :: b
        real(dp), intent(in) :: px(:), py(:)
        integer, intent(in) :: np
        character(len=*), intent(in) :: color
        real(dp), intent(in) :: lw
        integer :: i
        call builder_append(b, '<path d="')
        do i = 1, np
            if (i == 1) then
                call builder_append(b, "M ")
            else
                call builder_append(b, " L ")
            end if
            call append_num(b, px(i))
            call builder_append(b, " ")
            call append_num(b, py(i))
        end do
        call builder_append(b, '" stroke="')
        call builder_append(b, color)
        call builder_append(b, '" stroke-width="')
        call append_num(b, lw)
        call builder_append(b, '" fill="none"/>')
        call builder_append(b, new_line("a"))
    end subroutine append_stroke_path

    ! Emit a filled closed polygon, used by the shaped markers and by
    ! fill_between.
    subroutine append_polygon(b, px, py, np, color, alpha)
        type(svg_builder), intent(inout) :: b
        real(dp), intent(in) :: px(:), py(:)
        integer, intent(in) :: np
        character(len=*), intent(in) :: color
        real(dp), intent(in) :: alpha
        integer :: i
        call builder_append(b, '<polygon points="')
        do i = 1, np
            if (i > 1) call builder_append(b, " ")
            call append_num(b, px(i))
            call builder_append(b, ",")
            call append_num(b, py(i))
        end do
        call builder_append(b, '" fill="')
        call builder_append(b, color)
        if (alpha < 1.0_dp) then
            call builder_append(b, '" fill-opacity="')
            call append_num(b, alpha)
        end if
        call builder_append(b, '"/>')
        call builder_append(b, new_line("a"))
    end subroutine append_polygon

    ! Draw one marker of kind mk centred at (cx, cy), sized like a matplotlib
    ! marker of markersize ms.
    subroutine append_marker(b, mk, cx, cy, ms, color)
        type(svg_builder), intent(inout) :: b
        integer, intent(in) :: mk
        real(dp), intent(in) :: cx, cy, ms
        character(len=*), intent(in) :: color
        real(dp) :: r, xs(11), ys(11)
        integer :: i
        real(dp) :: ang

        select case (mk)
        case (MARKER_CIRCLE, MARKER_POINT)
            if (mk == MARKER_POINT) then
                r = 0.5_dp * ms * 0.35_dp
            else
                r = 0.5_dp * ms * 0.75_dp
            end if
            call builder_append(b, '<circle cx="')
            call append_num(b, cx)
            call builder_append(b, '" cy="')
            call append_num(b, cy)
            call builder_append(b, '" r="')
            call append_num(b, r)
            call builder_append(b, '" fill="')
            call builder_append(b, color)
            if (mk == MARKER_CIRCLE) then
                call builder_append(b, '" stroke="')
                call builder_append(b, color)
                call builder_append(b, '" stroke-width="1"/>')
            else
                call builder_append(b, '"/>')
            end if
            call builder_append(b, new_line("a"))
        case (MARKER_X)
            r = 0.5_dp * ms * 0.7_dp
            call append_stroke_path(b, [cx - r, cx + r], [cy - r, cy + r], 2, color, 1.5_dp)
            call append_stroke_path(b, [cx - r, cx + r], [cy + r, cy - r], 2, color, 1.5_dp)
        case (MARKER_PLUS)
            r = 0.5_dp * ms * 0.75_dp
            call append_stroke_path(b, [cx - r, cx + r], [cy, cy], 2, color, 1.5_dp)
            call append_stroke_path(b, [cx, cx], [cy - r, cy + r], 2, color, 1.5_dp)
        case (MARKER_SQUARE)
            r = 0.5_dp * ms * 0.75_dp
            call append_polygon(b, [cx - r, cx + r, cx + r, cx - r], &
                                [cy - r, cy - r, cy + r, cy + r], 4, color, 1.0_dp)
        case (MARKER_DIAMOND)
            r = 0.5_dp * ms * 0.75_dp
            call append_polygon(b, [cx, cx + r, cx, cx - r], &
                                [cy - r, cy, cy + r, cy], 4, color, 1.0_dp)
        case (MARKER_TRI_UP)
            r = 0.5_dp * ms * 0.85_dp
            call append_polygon(b, [cx, cx + r, cx - r], &
                                [cy - r, cy + r, cy + r], 3, color, 1.0_dp)
        case (MARKER_TRI_DOWN)
            r = 0.5_dp * ms * 0.85_dp
            call append_polygon(b, [cx, cx + r, cx - r], &
                                [cy + r, cy - r, cy - r], 3, color, 1.0_dp)
        case (MARKER_TRI_LEFT)
            r = 0.5_dp * ms * 0.85_dp
            call append_polygon(b, [cx - r, cx + r, cx + r], &
                                [cy, cy - r, cy + r], 3, color, 1.0_dp)
        case (MARKER_TRI_RIGHT)
            r = 0.5_dp * ms * 0.85_dp
            call append_polygon(b, [cx + r, cx - r, cx - r], &
                                [cy, cy - r, cy + r], 3, color, 1.0_dp)
        case (MARKER_STAR)
            ! Five-pointed star: alternate outer and inner vertices.
            r = 0.5_dp * ms * 0.95_dp
            do i = 1, 10
                ang = -0.5_dp * PI + real(i - 1, dp) * PI / 5.0_dp
                if (mod(i, 2) == 1) then
                    xs(i) = cx + r * cos(ang)
                    ys(i) = cy + r * sin(ang)
                else
                    xs(i) = cx + 0.4_dp * r * cos(ang)
                    ys(i) = cy + 0.4_dp * r * sin(ang)
                end if
            end do
            call append_polygon(b, xs, ys, 10, color, 1.0_dp)
        end select
    end subroutine append_marker

    ! Straight line segment in pixel coordinates.
    subroutine append_line(b, x1, y1, x2, y2, color, lw, ls)
        type(svg_builder), intent(inout) :: b
        real(dp), intent(in) :: x1, y1, x2, y2, lw
        character(len=*), intent(in) :: color
        integer, intent(in) :: ls
        if (ls == LINE_NONE) return
        call builder_append(b, '<line x1="')
        call append_num(b, x1)
        call builder_append(b, '" y1="')
        call append_num(b, y1)
        call builder_append(b, '" x2="')
        call append_num(b, x2)
        call builder_append(b, '" y2="')
        call append_num(b, y2)
        call builder_append(b, '" stroke="')
        call builder_append(b, color)
        call builder_append(b, '" stroke-width="')
        call append_num(b, lw)
        call builder_append(b, '"')
        call append_dash(b, ls)
        call builder_append(b, "/>")
        call builder_append(b, new_line("a"))
    end subroutine append_line

    ! One bar of a bar/hist series, drawn from the y = 0 baseline.
    subroutine append_bar(b, s, j, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xlog, ylog)
        type(svg_builder), intent(inout) :: b
        type(series_t), intent(in) :: s
        integer, intent(in) :: j
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        logical, intent(in) :: xlog, ylog
        real(dp) :: xa, xb, ya, yb, hw

        hw = 0.5_dp * s%width
        xa = map_x(s%x(j) - hw, xmin, xmax, ax_l, ax_w, xlog)
        xb = map_x(s%x(j) + hw, xmin, xmax, ax_l, ax_w, xlog)
        ya = map_y(0.0_dp, ymin, ymax, ax_b, ax_h, ylog)
        yb = map_y(s%y(j), ymin, ymax, ax_b, ax_h, ylog)

        call builder_append(b, '<rect x="')
        call append_num(b, min(xa, xb))
        call builder_append(b, '" y="')
        call append_num(b, min(ya, yb))
        call builder_append(b, '" width="')
        call append_num(b, abs(xb - xa))
        call builder_append(b, '" height="')
        call append_num(b, abs(yb - ya))
        call builder_append(b, '" fill="')
        call builder_append(b, trim(s%color))
        if (s%alpha < 1.0_dp) then
            call builder_append(b, '" fill-opacity="')
            call append_num(b, s%alpha)
        end if
        call builder_append(b, '" stroke="#ffffff" stroke-width="0.5"/>')
        call builder_append(b, new_line("a"))
    end subroutine append_bar

    ! Shaded region between y and y2, as a single closed polygon.
    subroutine append_fill(b, s, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xlog, ylog)
        type(svg_builder), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        logical, intent(in) :: xlog, ylog
        integer :: j, np
        real(dp), allocatable :: px(:), py(:)

        np = 2 * s%n
        allocate (px(np), py(np))
        do j = 1, s%n
            px(j) = map_x(s%x(j), xmin, xmax, ax_l, ax_w, xlog)
            py(j) = map_y(s%y(j), ymin, ymax, ax_b, ax_h, ylog)
        end do
        ! Return along the lower edge to close the band.
        do j = 1, s%n
            px(s%n + j) = map_x(s%x(s%n - j + 1), xmin, xmax, ax_l, ax_w, xlog)
            py(s%n + j) = map_y(s%y2(s%n - j + 1), ymin, ymax, ax_b, ax_h, ylog)
        end do
        call append_polygon(b, px, py, np, trim(s%color), s%alpha)
    end subroutine append_fill

    ! Vertical error bar with caps for point j.
    subroutine append_errorbar(b, s, j, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xlog, ylog)
        type(svg_builder), intent(inout) :: b
        type(series_t), intent(in) :: s
        integer, intent(in) :: j
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        logical, intent(in) :: xlog, ylog
        real(dp) :: px, plo, phi, cap

        px = map_x(s%x(j), xmin, xmax, ax_l, ax_w, xlog)
        plo = map_y(s%y(j) - s%y2(j), ymin, ymax, ax_b, ax_h, ylog)
        phi = map_y(s%y(j) + s%y2(j), ymin, ymax, ax_b, ax_h, ylog)
        call append_line(b, px, plo, px, phi, trim(s%color), 1.0_dp, LINE_SOLID)

        cap = s%width
        if (cap > 0.0_dp) then
            call append_line(b, px - cap, plo, px + cap, plo, trim(s%color), 1.0_dp, LINE_SOLID)
            call append_line(b, px - cap, phi, px + cap, phi, trim(s%color), 1.0_dp, LINE_SOLID)
        end if
    end subroutine append_errorbar

    subroutine render_axes(b, a, idx, W, H)
        type(svg_builder), intent(inout) :: b
        type(axes_t), intent(in) :: a
        integer, intent(in) :: idx
        real(dp), intent(in) :: W, H
        real(dp) :: ax_l, ax_r, ax_b, ax_t, ax_w, ax_h
        real(dp) :: xmin, xmax, ymin, ymax
        real(dp) :: xticks(MAX_TICKS), yticks(MAX_TICKS)
        integer :: nxt, nyt, i, j, n, nl
        real(dp) :: px, py, ms, r, mid
        character(len=64) :: lbl
        character(len=512) :: esc
        integer :: ln, en
        logical :: xlog, ylog
        integer :: n_leg, k, max_lbl
        real(dp) :: leg_x, leg_y, leg_w, leg_h, row_h

        ax_l = a%left * W
        ax_r = a%right * W
        ax_b = (1.0_dp - a%bottom) * H
        ax_t = (1.0_dp - a%top) * H
        ax_w = ax_r - ax_l
        ax_h = ax_b - ax_t

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

        ! clip path for this axes' data
        call builder_append(b, '<defs><clipPath id="axclip')
        call builder_append(b, int_to_str(idx))
        call builder_append(b, '"><rect x="')
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
        if (a%grid_on) then
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
        call builder_append(b, '<g clip-path="url(#axclip')
        call builder_append(b, int_to_str(idx))
        call builder_append(b, ')">')
        call builder_append(b, new_line("a"))
        do i = 1, a%n_series
            n = a%series(i)%n
            if (n <= 0) cycle

            select case (a%series(i)%kind)
            case (SERIES_BAR)
                do j = 1, n
                    call append_bar(b, a%series(i), j, xmin, xmax, ymin, ymax, &
                                    ax_l, ax_w, ax_b, ax_h, xlog, ylog)
                end do
                cycle
            case (SERIES_FILL)
                call append_fill(b, a%series(i), xmin, xmax, ymin, ymax, &
                                 ax_l, ax_w, ax_b, ax_h, xlog, ylog)
                cycle
            case (SERIES_HLINE)
                py = map_y(a%series(i)%y(1), ymin, ymax, ax_b, ax_h, ylog)
                call append_line(b, ax_l, py, ax_l + ax_w, py, &
                                 trim(a%series(i)%color), a%series(i)%linewidth, &
                                 a%series(i)%linestyle)
                cycle
            case (SERIES_VLINE)
                px = map_x(a%series(i)%x(1), xmin, xmax, ax_l, ax_w, xlog)
                call append_line(b, px, ax_b, px, ax_b - ax_h, &
                                 trim(a%series(i)%color), a%series(i)%linewidth, &
                                 a%series(i)%linestyle)
                cycle
            case (SERIES_ERRORBAR)
                do j = 1, n
                    call append_errorbar(b, a%series(i), j, xmin, xmax, ymin, ymax, &
                                         ax_l, ax_w, ax_b, ax_h, xlog, ylog)
                end do
            end select

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
                    px = map_x(a%series(i)%x(j), xmin, xmax, ax_l, ax_w, xlog)
                    py = map_y(a%series(i)%y(j), ymin, ymax, ax_b, ax_h, ylog)
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
                    px = map_x(a%series(i)%x(j), xmin, xmax, ax_l, ax_w, xlog)
                    py = map_y(a%series(i)%y(j), ymin, ymax, ax_b, ax_h, ylog)
                    call append_marker(b, a%series(i)%marker, px, py, ms, &
                                       trim(a%series(i)%color))
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

        if (len_trim(a%xlabel) > 0) then
            call xml_escape_to(a%xlabel, esc, en)
            call builder_append(b, '<text x="')
            call append_num(b, 0.5_dp * (ax_l + ax_r))
            call builder_append(b, '" y="')
            call append_num(b, ax_b + 28.6_dp)
            call builder_append(b, '" text-anchor="middle" font-family="DejaVu Sans, sans-serif" ')
            call builder_append(b, 'font-size="11" fill="#000000">')
            call builder_append(b, esc(1:en))
            call builder_append(b, "</text>")
            call builder_append(b, new_line("a"))
        end if

        if (len_trim(a%ylabel) > 0) then
            call xml_escape_to(a%ylabel, esc, en)
            call builder_append(b, '<text x="')
            call append_num(b, ax_l - 34.0_dp)
            call builder_append(b, '" y="')
            call append_num(b, 0.5_dp * (ax_t + ax_b))
            call builder_append(b, '" text-anchor="middle" font-family="DejaVu Sans, sans-serif" ')
            call builder_append(b, 'font-size="11" fill="#000000" transform="rotate(-90 ')
            call append_num(b, ax_l - 34.0_dp)
            call builder_append(b, " ")
            call append_num(b, 0.5_dp * (ax_t + ax_b))
            call builder_append(b, ')">')
            call builder_append(b, esc(1:en))
            call builder_append(b, "</text>")
            call builder_append(b, new_line("a"))
        end if

        ! title
        if (len_trim(a%title) > 0) then
            call xml_escape_to(a%title, esc, en)
            call builder_append(b, '<text x="')
            call append_num(b, 0.5_dp * (ax_l + ax_r))
            call builder_append(b, '" y="')
            call append_num(b, ax_t - 6.0_dp)
            call builder_append(b, '" text-anchor="middle" font-family="DejaVu Sans, sans-serif" ')
            call builder_append(b, 'font-size="12" fill="#000000">')
            call builder_append(b, esc(1:en))
            call builder_append(b, "</text>")
            call builder_append(b, new_line("a"))
        end if

        ! legend
        if (a%legend_on) then
            n_leg = 0
            max_lbl = 0
            do i = 1, a%n_series
                if (len_trim(a%series(i)%label) > 0) then
                    n_leg = n_leg + 1
                    max_lbl = max(max_lbl, len_trim(a%series(i)%label))
                end if
            end do
            if (n_leg > 0) then
                row_h = 18.0_dp
                ! Sample line (leg_x+8 .. leg_x+28), gap to the text at
                ! leg_x+34, the label itself, and a trailing pad.
                leg_w = 34.0_dp + real(max_lbl, dp) * LEGEND_CHAR_W + 8.0_dp
                leg_w = min(leg_w, ax_w - 16.0_dp)
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
                    if (a%series(i)%marker /= MARKER_NONE) then
                        call append_marker(b, a%series(i)%marker, mid, py, &
                                           a%series(i)%markersize, &
                                           trim(a%series(i)%color))
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
            call append_num(b, (1.0_dp - SUPTITLE_Y) * H + 4.2_dp)
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
