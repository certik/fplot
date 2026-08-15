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
    public :: text, annotate
    public :: xticks, yticks, minorticks_on
    public :: title, xlabel, ylabel, grid, legend
    public :: xlim, ylim, clf, savefig, show, figure
    public :: render_svg
    public :: subplot, suptitle

    integer, parameter :: SCALE_LINEAR = 0
    integer, parameter :: SCALE_LOG = 1
    ! Initial slot count for the per-axes series and text arrays; both grow
    ! on demand, so this is only the allocation granularity.
    integer, parameter :: INIT_SLOTS = 8
    integer, parameter :: MAX_MINOR = 256

    real(dp), parameter :: FIG_W_DEFAULT = 6.4_dp
    real(dp), parameter :: FIG_H_DEFAULT = 4.8_dp
    real(dp), parameter :: DPI_DEFAULT = 100.0_dp

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

    type :: text_t
        real(dp) :: x = 0.0_dp, y = 0.0_dp
        ! Arrow tail; only used when has_arrow is set.
        real(dp) :: xtail = 0.0_dp, ytail = 0.0_dp
        logical :: has_arrow = .false.
        real(dp) :: fontsize = 10.0_dp
        character(len=7) :: color = "#000000"
        character(len=8) :: ha = "left"
        character(len=128) :: s = ""
    end type text_t

    type :: axes_t
        integer :: n_series = 0
        type(series_t), allocatable :: series(:)
        integer :: n_texts = 0
        type(text_t), allocatable :: texts(:)
        character(len=256) :: title = ""
        character(len=256) :: xlabel = ""
        character(len=256) :: ylabel = ""
        logical :: grid_on = .false.
        logical :: legend_on = .false.
        character(len=16) :: legend_loc = "upper right"
        logical :: minor_ticks = .false.
        ! User-specified tick positions and optional labels.
        integer :: n_xticks = 0, n_yticks = 0
        real(dp) :: xtick_pos(MAX_TICKS), ytick_pos(MAX_TICKS)
        logical :: xtick_labeled = .false., ytick_labeled = .false.
        character(len=24) :: xtick_lab(MAX_TICKS), ytick_lab(MAX_TICKS)
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
    real(dp), save :: fig_w_in = FIG_W_DEFAULT
    real(dp), save :: fig_h_in = FIG_H_DEFAULT
    ! Kept for savefig backends that rasterize; SVG is resolution independent,
    ! so matplotlib emits the same inches*72 pt canvas at any dpi.
    real(dp), save :: fig_dpi = DPI_DEFAULT
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

    subroutine figure(figsize, dpi)
        real(dp), intent(in), optional :: figsize(2), dpi
        call clf()
        fig_w_in = FIG_W_DEFAULT
        fig_h_in = FIG_H_DEFAULT
        fig_dpi = DPI_DEFAULT
        if (present(figsize)) then
            if (figsize(1) <= 0.0_dp .or. figsize(2) <= 0.0_dp) &
                error stop "fplot: figure figsize must be positive"
            fig_w_in = figsize(1)
            fig_h_in = figsize(2)
        end if
        if (present(dpi)) then
            if (dpi <= 0.0_dp) error stop "fplot: figure dpi must be positive"
            fig_dpi = dpi
        end if
    end subroutine figure

    subroutine clf()
        cur_i = 0
        if (allocated(ax)) deallocate (ax)
        n_ax = 0
        grid_m = 0
        grid_n = 0
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

    subroutine legend(loc)
        character(len=*), intent(in), optional :: loc
        call ensure_fig()
        ax(cur_i)%legend_on = .true.
        if (present(loc)) ax(cur_i)%legend_loc = loc
    end subroutine legend

    subroutine xticks(vals, labels)
        real(dp), intent(in) :: vals(:)
        character(len=*), intent(in), optional :: labels(:)
        call ensure_fig()
        call set_ticks(vals, labels, ax(cur_i)%n_xticks, ax(cur_i)%xtick_pos, &
                       ax(cur_i)%xtick_labeled, ax(cur_i)%xtick_lab)
    end subroutine xticks

    subroutine yticks(vals, labels)
        real(dp), intent(in) :: vals(:)
        character(len=*), intent(in), optional :: labels(:)
        call ensure_fig()
        call set_ticks(vals, labels, ax(cur_i)%n_yticks, ax(cur_i)%ytick_pos, &
                       ax(cur_i)%ytick_labeled, ax(cur_i)%ytick_lab)
    end subroutine yticks

    subroutine set_ticks(vals, labels, n, pos, labeled, lab)
        real(dp), intent(in) :: vals(:)
        character(len=*), intent(in), optional :: labels(:)
        integer, intent(out) :: n
        real(dp), intent(out) :: pos(MAX_TICKS)
        logical, intent(out) :: labeled
        character(len=24), intent(out) :: lab(MAX_TICKS)
        integer :: i
        n = min(size(vals), MAX_TICKS)
        labeled = present(labels)
        do i = 1, n
            pos(i) = vals(i)
            if (labeled) lab(i) = labels(i)
        end do
    end subroutine set_ticks

    subroutine minorticks_on()
        call ensure_fig()
        ax(cur_i)%minor_ticks = .true.
    end subroutine minorticks_on

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

    subroutine plot(x, y, fmt, label, lw, color, marker, linestyle, alpha)
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: fmt, label, color, marker, linestyle
        real(dp), intent(in), optional :: lw, alpha
        call ensure_fig()
        call add_series(cur_i, x, y, fmt, label, lw, color, marker, linestyle, alpha)
    end subroutine plot

    ! Marker-only plot. s is the marker area in points^2 (matplotlib's
    ! convention), so the marker size is its square root.
    subroutine scatter(x, y, s, c, marker, label, alpha)
        real(dp), intent(in) :: x(:), y(:)
        real(dp), intent(in), optional :: s, alpha
        character(len=*), intent(in), optional :: c, marker, label
        integer :: is

        call ensure_fig()
        if (present(marker)) then
            call add_series(cur_i, x, y, label=label, color=c, marker=marker, &
                            linestyle="None", alpha=alpha)
        else
            call add_series(cur_i, x, y, label=label, color=c, marker="o", &
                            linestyle="None", alpha=alpha)
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

    ! Claim the next series slot, doubling the array when it is full.
    subroutine push_series(a, is)
        type(axes_t), intent(inout) :: a
        integer, intent(out) :: is
        type(series_t), allocatable :: tmp(:)
        integer :: cap, i

        if (.not. allocated(a%series)) allocate (a%series(INIT_SLOTS))
        cap = size(a%series)
        if (a%n_series >= cap) then
            allocate (tmp(2 * cap))
            do i = 1, cap
                tmp(i) = a%series(i)
            end do
            call move_alloc(tmp, a%series)
        end if
        a%n_series = a%n_series + 1
        is = a%n_series
    end subroutine push_series

    subroutine push_text(a, it)
        type(axes_t), intent(inout) :: a
        integer, intent(out) :: it
        type(text_t), allocatable :: tmp(:)
        integer :: cap, i

        if (.not. allocated(a%texts)) allocate (a%texts(INIT_SLOTS))
        cap = size(a%texts)
        if (a%n_texts >= cap) then
            allocate (tmp(2 * cap))
            do i = 1, cap
                tmp(i) = a%texts(i)
            end do
            call move_alloc(tmp, a%texts)
        end if
        a%n_texts = a%n_texts + 1
        it = a%n_texts
    end subroutine push_text

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
        call push_series(ax(cur_i), is)
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

    ! Text at a point in data coordinates.
    subroutine text(x, y, s, color, fontsize, ha)
        real(dp), intent(in) :: x, y
        character(len=*), intent(in) :: s
        character(len=*), intent(in), optional :: color, ha
        real(dp), intent(in), optional :: fontsize
        call add_text(x, y, s, color, fontsize, ha, .false., 0.0_dp, 0.0_dp)
    end subroutine text

    ! Text at (xtext, ytext) with an arrow pointing at (x, y).
    subroutine annotate(s, x, y, xtext, ytext, color, fontsize, ha)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: x, y
        real(dp), intent(in), optional :: xtext, ytext, fontsize
        character(len=*), intent(in), optional :: color, ha
        real(dp) :: xt, yt
        logical :: arrow

        xt = x
        yt = y
        arrow = present(xtext) .or. present(ytext)
        if (present(xtext)) xt = xtext
        if (present(ytext)) yt = ytext
        ! The label sits at the text position; the arrow runs back to (x, y).
        call add_text(xt, yt, s, color, fontsize, ha, arrow, x, y)
    end subroutine annotate

    subroutine add_text(x, y, s, color, fontsize, ha, arrow, xarr, yarr)
        real(dp), intent(in) :: x, y, xarr, yarr
        character(len=*), intent(in) :: s
        character(len=*), intent(in), optional :: color, ha
        real(dp), intent(in), optional :: fontsize
        logical, intent(in) :: arrow
        integer :: it
        character(len=7) :: col

        call ensure_fig()
        call push_text(ax(cur_i), it)
        ax(cur_i)%texts(it)%x = x
        ax(cur_i)%texts(it)%y = y
        ax(cur_i)%texts(it)%s = s
        ax(cur_i)%texts(it)%has_arrow = arrow
        ax(cur_i)%texts(it)%xtail = xarr
        ax(cur_i)%texts(it)%ytail = yarr
        ax(cur_i)%texts(it)%fontsize = 10.0_dp
        ax(cur_i)%texts(it)%ha = "left"
        ax(cur_i)%texts(it)%color = "#000000"
        if (present(fontsize)) ax(cur_i)%texts(it)%fontsize = fontsize
        if (present(ha)) ax(cur_i)%texts(it)%ha = ha
        col = resolve_color(color)
        if (len_trim(col) > 0) ax(cur_i)%texts(it)%color = col
    end subroutine add_text

    subroutine add_series(ia, x, y, fmt, label, lw, color, marker, linestyle, alpha)
        integer, intent(in) :: ia
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: fmt, label, color, marker, linestyle
        real(dp), intent(in), optional :: lw, alpha
        integer :: n, is, m, ls
        character(len=7) :: col
        character(len=32) :: f
        logical :: have_fmt

        n = min(size(x), size(y))
        if (n <= 0) return
        call push_series(ax(ia), is)

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
        if (present(alpha)) ax(ia)%series(is)%alpha = alpha
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
    ! matplotlib writes stroke-opacity/fill-opacity per element rather than a
    ! group opacity; emitting nothing at alpha == 1 keeps the common case terse.
    subroutine append_opacity(b, attr, alpha)
        type(svg_builder), intent(inout) :: b
        character(len=*), intent(in) :: attr
        real(dp), intent(in) :: alpha
        if (alpha >= 1.0_dp) return
        call builder_append(b, '" ')
        call builder_append(b, attr)
        call builder_append(b, '="')
        call append_num(b, alpha)
    end subroutine append_opacity

    subroutine append_stroke_path(b, px, py, np, color, lw, alpha)
        type(svg_builder), intent(inout) :: b
        real(dp), intent(in) :: px(:), py(:)
        integer, intent(in) :: np
        character(len=*), intent(in) :: color
        real(dp), intent(in) :: lw, alpha
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
        call append_opacity(b, "stroke-opacity", alpha)
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
        call append_opacity(b, "fill-opacity", alpha)
        call builder_append(b, '"/>')
        call builder_append(b, new_line("a"))
    end subroutine append_polygon

    ! Draw one marker of kind mk centred at (cx, cy), sized like a matplotlib
    ! marker of markersize ms.
    subroutine append_marker(b, mk, cx, cy, ms, color, alpha)
        type(svg_builder), intent(inout) :: b
        integer, intent(in) :: mk
        real(dp), intent(in) :: cx, cy, ms
        character(len=*), intent(in) :: color
        real(dp), intent(in) :: alpha
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
            call append_opacity(b, "fill-opacity", alpha)
            if (mk == MARKER_CIRCLE) then
                call builder_append(b, '" stroke="')
                call builder_append(b, color)
                call builder_append(b, '" stroke-width="1')
                call append_opacity(b, "stroke-opacity", alpha)
                call builder_append(b, '"/>')
            else
                call builder_append(b, '"/>')
            end if
            call builder_append(b, new_line("a"))
        case (MARKER_X)
            r = 0.5_dp * ms * 0.7_dp
            call append_stroke_path(b, [cx - r, cx + r], [cy - r, cy + r], 2, color, 1.5_dp, alpha)
            call append_stroke_path(b, [cx - r, cx + r], [cy + r, cy - r], 2, color, 1.5_dp, alpha)
        case (MARKER_PLUS)
            r = 0.5_dp * ms * 0.75_dp
            call append_stroke_path(b, [cx - r, cx + r], [cy, cy], 2, color, 1.5_dp, alpha)
            call append_stroke_path(b, [cx, cx], [cy - r, cy + r], 2, color, 1.5_dp, alpha)
        case (MARKER_SQUARE)
            r = 0.5_dp * ms * 0.75_dp
            call append_polygon(b, [cx - r, cx + r, cx + r, cx - r], &
                                [cy - r, cy - r, cy + r, cy + r], 4, color, alpha)
        case (MARKER_DIAMOND)
            r = 0.5_dp * ms * 0.75_dp
            call append_polygon(b, [cx, cx + r, cx, cx - r], &
                                [cy - r, cy, cy + r, cy], 4, color, alpha)
        case (MARKER_TRI_UP)
            r = 0.5_dp * ms * 0.85_dp
            call append_polygon(b, [cx, cx + r, cx - r], &
                                [cy - r, cy + r, cy + r], 3, color, alpha)
        case (MARKER_TRI_DOWN)
            r = 0.5_dp * ms * 0.85_dp
            call append_polygon(b, [cx, cx + r, cx - r], &
                                [cy + r, cy - r, cy - r], 3, color, alpha)
        case (MARKER_TRI_LEFT)
            r = 0.5_dp * ms * 0.85_dp
            call append_polygon(b, [cx - r, cx + r, cx + r], &
                                [cy, cy - r, cy + r], 3, color, alpha)
        case (MARKER_TRI_RIGHT)
            r = 0.5_dp * ms * 0.85_dp
            call append_polygon(b, [cx + r, cx - r, cx - r], &
                                [cy, cy - r, cy + r], 3, color, alpha)
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
            call append_polygon(b, xs, ys, 10, color, alpha)
        end select
    end subroutine append_marker

    ! Text element. anchor is a matplotlib horizontal alignment
    ! (left/center/right) or an SVG anchor (start/middle/end).
    subroutine append_text(b, x, y, s, anchor, fontsize, color, transform)
        type(svg_builder), intent(inout) :: b
        real(dp), intent(in) :: x, y, fontsize
        character(len=*), intent(in) :: s, anchor, color
        character(len=*), intent(in), optional :: transform

        call builder_append(b, '<text x="')
        call append_num(b, x)
        call builder_append(b, '" y="')
        call append_num(b, y)
        call builder_append(b, '" text-anchor="')
        select case (anchor)
        case ("left", "start"); call builder_append(b, "start")
        case ("right", "end"); call builder_append(b, "end")
        case default; call builder_append(b, "middle")
        end select
        call builder_append(b, '" font-family="DejaVu Sans, sans-serif" font-size="')
        call append_num(b, fontsize)
        call builder_append(b, '" fill="')
        call builder_append(b, color)
        if (present(transform)) then
            call builder_append(b, '" transform="')
            call builder_append(b, transform)
        end if
        call builder_append(b, '">')
        call builder_append(b, s)
        call builder_append(b, "</text>")
        call builder_append(b, new_line("a"))
    end subroutine append_text

    ! Straight line segment in pixel coordinates.
    subroutine append_line(b, x1, y1, x2, y2, color, lw, ls, alpha)
        type(svg_builder), intent(inout) :: b
        real(dp), intent(in) :: x1, y1, x2, y2, lw
        character(len=*), intent(in) :: color
        integer, intent(in) :: ls
        real(dp), intent(in) :: alpha
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
        call append_opacity(b, "stroke-opacity", alpha)
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
        call append_opacity(b, "fill-opacity", s%alpha)
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
        call append_line(b, px, plo, px, phi, trim(s%color), 1.0_dp, LINE_SOLID, s%alpha)

        cap = s%width
        if (cap > 0.0_dp) then
            call append_line(b, px - cap, plo, px + cap, plo, trim(s%color), 1.0_dp, LINE_SOLID, s%alpha)
            call append_line(b, px - cap, phi, px + cap, phi, trim(s%color), 1.0_dp, LINE_SOLID, s%alpha)
        end if
    end subroutine append_errorbar

    ! User-set tick positions win over the automatic locator.
    subroutine axis_ticks(n_user, user_pos, vmin, vmax, is_log, t, nt)
        integer, intent(in) :: n_user
        real(dp), intent(in) :: user_pos(MAX_TICKS), vmin, vmax
        logical, intent(in) :: is_log
        real(dp), intent(out) :: t(MAX_TICKS)
        integer, intent(out) :: nt
        if (n_user > 0) then
            nt = n_user
            t(1:nt) = user_pos(1:nt)
        else if (is_log) then
            call log_ticks(vmin, vmax, t, nt)
        else
            call linear_ticks(vmin, vmax, 6, t, nt)
        end if
    end subroutine axis_ticks

    ! Minor ticks subdivide each major interval; a log axis already places its
    ! majors one decade apart, so the 2..9 multiples are what belong between.
    subroutine minor_positions(t, nt, vmin, vmax, is_log, m, nm)
        real(dp), intent(in) :: t(MAX_TICKS), vmin, vmax
        integer, intent(in) :: nt
        logical, intent(in) :: is_log
        real(dp), intent(out) :: m(MAX_MINOR)
        integer, intent(out) :: nm
        integer :: i, k
        real(dp) :: step, v

        nm = 0
        if (nt < 2) return
        do i = 1, nt - 1
            if (is_log) then
                do k = 2, 9
                    v = t(i) * real(k, dp)
                    if (v > t(i + 1)) exit
                    call push_minor(m, nm, v, vmin, vmax)
                end do
            else
                step = (t(i + 1) - t(i)) / 5.0_dp
                do k = 1, 4
                    call push_minor(m, nm, t(i) + real(k, dp) * step, vmin, vmax)
                end do
            end if
        end do
    end subroutine minor_positions

    subroutine push_minor(m, nm, v, vmin, vmax)
        real(dp), intent(inout) :: m(MAX_MINOR)
        integer, intent(inout) :: nm
        real(dp), intent(in) :: v, vmin, vmax
        if (v < vmin .or. v > vmax) return
        if (nm >= MAX_MINOR) return
        nm = nm + 1
        m(nm) = v
    end subroutine push_minor

    ! matplotlib's "best" needs a data-overlap search; upper right is the
    ! placement it picks for the common case, so we use it as the fallback.
    subroutine legend_origin(loc, ax_l, ax_r, ax_t, ax_b, leg_w, leg_h, leg_x, leg_y)
        character(len=*), intent(in) :: loc
        real(dp), intent(in) :: ax_l, ax_r, ax_t, ax_b, leg_w, leg_h
        real(dp), intent(out) :: leg_x, leg_y
        real(dp), parameter :: pad = 8.0_dp

        select case (trim(loc))
        case ("upper left", "left")
            leg_x = ax_l + pad
            leg_y = ax_t + pad
        case ("lower left")
            leg_x = ax_l + pad
            leg_y = ax_b - leg_h - pad
        case ("lower right")
            leg_x = ax_r - leg_w - pad
            leg_y = ax_b - leg_h - pad
        case ("lower center", "lower")
            leg_x = 0.5_dp * (ax_l + ax_r - leg_w)
            leg_y = ax_b - leg_h - pad
        case ("upper center", "upper")
            leg_x = 0.5_dp * (ax_l + ax_r - leg_w)
            leg_y = ax_t + pad
        case ("center")
            leg_x = 0.5_dp * (ax_l + ax_r - leg_w)
            leg_y = 0.5_dp * (ax_t + ax_b - leg_h)
        case ("center left")
            leg_x = ax_l + pad
            leg_y = 0.5_dp * (ax_t + ax_b - leg_h)
        case ("center right", "right")
            leg_x = ax_r - leg_w - pad
            leg_y = 0.5_dp * (ax_t + ax_b - leg_h)
        case default
            leg_x = ax_r - leg_w - pad
            leg_y = ax_t + pad
        end select
    end subroutine legend_origin

    subroutine append_tick(b, x1, y1, x2, y2)
        type(svg_builder), intent(inout) :: b
        real(dp), intent(in) :: x1, y1, x2, y2
        call builder_append(b, '<line x1="')
        call append_num(b, x1)
        call builder_append(b, '" y1="')
        call append_num(b, y1)
        call builder_append(b, '" x2="')
        call append_num(b, x2)
        call builder_append(b, '" y2="')
        call append_num(b, y2)
        call builder_append(b, '" stroke="#000000" stroke-width="0.8"/>')
        call builder_append(b, new_line("a"))
    end subroutine append_tick

    subroutine tick_label(labeled, lab, i, v, is_log, out, n)
        logical, intent(in) :: labeled
        character(len=24), intent(in) :: lab(MAX_TICKS)
        integer, intent(in) :: i
        real(dp), intent(in) :: v
        logical, intent(in) :: is_log
        character(len=*), intent(out) :: out
        integer, intent(out) :: n
        if (labeled) then
            n = len_trim(lab(i))
            out(1:n) = trim(lab(i))
        else
            call format_tick_to(v, is_log, out, n)
        end if
    end subroutine tick_label

    subroutine render_axes(b, a, idx, W, H, clear)
        type(svg_builder), intent(inout) :: b
        type(axes_t), intent(in) :: a
        integer, intent(in) :: idx
        real(dp), intent(in) :: W, H
        ! matplotlib's transparent=True clears the axes patch as well.
        logical, intent(in) :: clear
        real(dp) :: ax_l, ax_r, ax_b, ax_t, ax_w, ax_h
        real(dp) :: xmin, xmax, ymin, ymax
        real(dp) :: xticks(MAX_TICKS), yticks(MAX_TICKS)
        real(dp) :: xminor(MAX_MINOR), yminor(MAX_MINOR)
        integer :: nxt, nyt, nxm, nym, i, j, n, nl
        real(dp) :: px, py, ms, r, mid
        character(len=64) :: lbl
        character(len=64) :: tx, ty
        integer :: tn, tyn
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

        call axis_ticks(a%n_xticks, a%xtick_pos, xmin, xmax, xlog, xticks, nxt)
        call axis_ticks(a%n_yticks, a%ytick_pos, ymin, ymax, ylog, yticks, nyt)
        if (a%minor_ticks) then
            call minor_positions(xticks, nxt, xmin, xmax, xlog, xminor, nxm)
            call minor_positions(yticks, nyt, ymin, ymax, ylog, yminor, nym)
        else
            nxm = 0
            nym = 0
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
        if (.not. clear) then
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
        end if

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
                                 a%series(i)%linestyle, a%series(i)%alpha)
                cycle
            case (SERIES_VLINE)
                px = map_x(a%series(i)%x(1), xmin, xmax, ax_l, ax_w, xlog)
                call append_line(b, px, ax_b, px, ax_b - ax_h, &
                                 trim(a%series(i)%color), a%series(i)%linewidth, &
                                 a%series(i)%linestyle, a%series(i)%alpha)
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
                call append_opacity(b, "stroke-opacity", a%series(i)%alpha)
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
                                       trim(a%series(i)%color), a%series(i)%alpha)
                end do
            end if
        end do
        ! annotations, in data coordinates
        do i = 1, a%n_texts
            px = map_x(a%texts(i)%x, xmin, xmax, ax_l, ax_w, xlog)
            py = map_y(a%texts(i)%y, ymin, ymax, ax_b, ax_h, ylog)
            if (a%texts(i)%has_arrow) then
                call append_line(b, px, py, &
                                 map_x(a%texts(i)%xtail, xmin, xmax, ax_l, ax_w, xlog), &
                                 map_y(a%texts(i)%ytail, ymin, ymax, ax_b, ax_h, ylog), &
                                 trim(a%texts(i)%color), 1.0_dp, LINE_SOLID, 1.0_dp)
            end if
            call xml_escape_to(a%texts(i)%s, esc, en)
            call append_text(b, px, py + 3.5_dp, esc(1:en), &
                             trim(a%texts(i)%ha), a%texts(i)%fontsize, &
                             trim(a%texts(i)%color))
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
            call append_tick(b, px, ax_b, px, ax_b + 3.5_dp)
            call tick_label(a%xtick_labeled, a%xtick_lab, i, xticks(i), xlog, lbl, ln)
            call append_text(b, px, ax_b + 16.0_dp, lbl(1:ln), "center", 10.0_dp, "#000000")
        end do
        do i = 1, nxm
            px = map_x(xminor(i), xmin, xmax, ax_l, ax_w, xlog)
            call append_tick(b, px, ax_b, px, ax_b + 2.0_dp)
        end do

        ! y ticks
        do i = 1, nyt
            py = map_y(yticks(i), ymin, ymax, ax_b, ax_h, ylog)
            call append_tick(b, ax_l, py, ax_l - 3.5_dp, py)
            call tick_label(a%ytick_labeled, a%ytick_lab, i, yticks(i), ylog, lbl, ln)
            call append_text(b, ax_l - 7.0_dp, py + 3.5_dp, lbl(1:ln), "right", 10.0_dp, "#000000")
        end do
        do i = 1, nym
            py = map_y(yminor(i), ymin, ymax, ax_b, ax_h, ylog)
            call append_tick(b, ax_l, py, ax_l - 2.0_dp, py)
        end do

        if (len_trim(a%xlabel) > 0) then
            call xml_escape_to(a%xlabel, esc, en)
            call append_text(b, 0.5_dp * (ax_l + ax_r), ax_b + 28.6_dp, esc(1:en), &
                             "center", 11.0_dp, "#000000")
        end if

        if (len_trim(a%ylabel) > 0) then
            call xml_escape_to(a%ylabel, esc, en)
            call fmt_num(ax_l - 34.0_dp, tx, tn)
            call fmt_num(0.5_dp * (ax_t + ax_b), ty, tyn)
            call append_text(b, ax_l - 34.0_dp, 0.5_dp * (ax_t + ax_b), esc(1:en), &
                             "center", 11.0_dp, "#000000", &
                             "rotate(-90 " // tx(1:tn) // " " // ty(1:tyn) // ")")
        end if

        ! title
        if (len_trim(a%title) > 0) then
            call xml_escape_to(a%title, esc, en)
            call append_text(b, 0.5_dp * (ax_l + ax_r), ax_t - 6.0_dp, esc(1:en), &
                             "center", 12.0_dp, "#000000")
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
                call legend_origin(a%legend_loc, ax_l, ax_r, ax_t, ax_b, &
                                   leg_w, leg_h, leg_x, leg_y)
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
                                           trim(a%series(i)%color), a%series(i)%alpha)
                    end if
                    call xml_escape_to(a%series(i)%label, esc, en)
                    call append_text(b, leg_x + 34.0_dp, py + 3.5_dp, esc(1:en), &
                                     "left", 10.0_dp, "#000000")
                end do
            end if
        end if
    end subroutine render_axes

    function render_svg(facecolor, transparent) result(svg)
        character(len=*), intent(in), optional :: facecolor
        logical, intent(in), optional :: transparent
        character(len=:), allocatable :: svg
        type(svg_builder) :: b
        real(dp) :: W, H
        character(len=512) :: esc
        character(len=7) :: face
        logical :: clear
        integer :: i, en

        call ensure_fig()
        clear = .false.
        if (present(transparent)) clear = transparent
        face = resolve_color(facecolor)
        if (len_trim(face) == 0) face = "#ffffff"
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

        ! background; transparent drops the figure patch entirely
        if (.not. clear) then
            call builder_append(b, '<rect x="0" y="0" width="')
            call append_num(b, W)
            call builder_append(b, '" height="')
            call append_num(b, H)
            call builder_append(b, '" fill="')
            call builder_append(b, face)
            call builder_append(b, '"/>')
            call builder_append(b, new_line("a"))
        end if

        ! axes (subplots)
        do i = 1, n_ax
            call render_axes(b, ax(i), i, W, H, clear)
        end do

        ! suptitle (figure-level, above all axes)
        if (len_trim(fig_suptitle) > 0) then
            call xml_escape_to(fig_suptitle, esc, en)
            call append_text(b, 0.5_dp * W, (1.0_dp - SUPTITLE_Y) * H + 4.2_dp, &
                             esc(1:en), "center", 12.0_dp, "#000000")
        end if

        call builder_append(b, "</svg>")
        call builder_append(b, new_line("a"))
        svg = builder_get(b)
    end function render_svg

    ! Writing SVG bytes into a .png would produce a file no viewer can open,
    ! so an unsupported extension is a hard error rather than a silent default.
    subroutine check_svg_ext(filename)
        character(len=*), intent(in) :: filename
        integer :: d, sl
        character(len=:), allocatable :: ext

        d = index(filename, ".", back=.true.)
        sl = max(index(filename, "/", back=.true.), index(filename, achar(92), back=.true.))
        if (d <= sl + 1) return
        ext = lower(filename(d + 1:len_trim(filename)))
        if (ext == "svg") return
        print *, "fplot: cannot write ", trim(filename)
        print *, "fplot: only .svg output is supported, not ." // ext
        error stop "fplot: unsupported savefig format"
    end subroutine check_svg_ext

    pure function lower(s) result(t)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: t
        integer :: i, c
        do i = 1, len(s)
            c = iachar(s(i:i))
            if (c >= iachar("A") .and. c <= iachar("Z")) then
                t(i:i) = achar(c + 32)
            else
                t(i:i) = s(i:i)
            end if
        end do
    end function lower

    subroutine savefig(filename, transparent, facecolor)
        character(len=*), intent(in) :: filename
        logical, intent(in), optional :: transparent
        character(len=*), intent(in), optional :: facecolor
        character(len=:), allocatable :: svg
        integer :: u, ios, n

        call check_svg_ext(filename)
        svg = render_svg(facecolor, transparent)
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
