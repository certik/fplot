! fplot — pure Fortran pylab-style SVG plotting library.
module fplot
    use fplot_colors
    use fplot_style
    use fplot_scale
    use fplot_cmap
    use fplot_contour
    use fplot_ticks
    use fplot_svg
    implicit none
    private

    public :: dp
    public :: plot, scatter, semilogx, semilogy, loglog
    public :: set_xscale, set_yscale
    public :: bar, barh, hist, fill_between, errorbar, axhline, axvline
    public :: step, stem, pie, boxplot, violinplot
    public :: axis, set_aspect, tick_params, spines
    public :: text, annotate
    public :: xticks, yticks, minorticks_on
    public :: imshow, colorbar, contour, contourf
    public :: title, xlabel, ylabel, grid, legend
    public :: xlim, ylim, clf, savefig, show, figure
    public :: render_svg
    public :: subplot, suptitle, subplots_adjust, tight_layout
    public :: twinx, twiny
    public :: set_fontsize
    public :: close, gcf

    ! Initial slot count for the per-axes series and text arrays; both grow
    ! on demand, so this is only the allocation granularity.
    integer, parameter :: INIT_SLOTS = 8
    integer, parameter :: MAX_MINOR = 256

    ! Colorbar layout, as fractions of the axes box before it was shrunk.
    real(dp), parameter :: CBAR_SHRINK = 0.8_dp
    real(dp), parameter :: CBAR_X = 0.85_dp
    real(dp), parameter :: CBAR_W = 0.03725_dp
    integer, parameter :: CBAR_SLICES = 64

    real(dp), parameter :: FIG_W_DEFAULT = 6.4_dp
    real(dp), parameter :: FIG_H_DEFAULT = 4.8_dp
    real(dp), parameter :: DPI_DEFAULT = 100.0_dp

    ! Default figure margins (matplotlib rcParams), in figure fractions.
    ! Tick geometry and label size, matching matplotlib's rcParams.
    real(dp), parameter :: TICK_LEN = 3.5_dp
    real(dp), parameter :: TICK_FONT = 10.0_dp
    ! Minor ticks are shorter than majors by this factor.
    real(dp), parameter :: MINOR_FRAC = 2.0_dp / 3.5_dp
    integer, parameter :: SPINE_LEFT = 1, SPINE_RIGHT = 2
    integer, parameter :: SPINE_BOTTOM = 3, SPINE_TOP = 4

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
    ! Points per inch, matplotlib's font sizes, and the width of one digit
    ! in DejaVu Sans as a fraction of the font size.
    real(dp), parameter :: PT_PER_IN = 72.0_dp
    real(dp), parameter :: TITLE_FONT = 12.0_dp
    real(dp), parameter :: SUPTITLE_FONT = 12.0_dp
    real(dp), parameter :: LABEL_FONT = 11.0_dp
    real(dp), parameter :: LEGEND_FONT = 10.0_dp
    real(dp), parameter :: DIGIT_W = 0.636_dp
    ! Height of a one-line label including its leading, in font sizes.
    real(dp), parameter :: LABEL_BOX = 1.45_dp
    real(dp), parameter :: PI = 3.141592653589793_dp

    ! What a series draws. LINE covers plot/scatter/semilog*; the rest are
    ! the shape-based plot types.
    integer, parameter :: SERIES_LINE = 0
    integer, parameter :: SERIES_BAR = 1
    integer, parameter :: SERIES_FILL = 2
    integer, parameter :: SERIES_ERRORBAR = 3
    integer, parameter :: SERIES_HLINE = 4
    integer, parameter :: SERIES_VLINE = 5
    integer, parameter :: SERIES_BARH = 6
    integer, parameter :: SERIES_STEM = 7
    integer, parameter :: SERIES_PIE = 8
    integer, parameter :: SERIES_BOX = 9
    integer, parameter :: SERIES_VIOLIN = 10

    type :: series_t
        integer :: kind = SERIES_LINE
        integer :: n = 0
        real(dp), allocatable :: x(:)
        real(dp), allocatable :: y(:)
        ! FILL: lower edge. ERRORBAR: symmetric y error.
        real(dp), allocatable :: y2(:)
        ! BOX/VIOLIN: the position on the category axis.
        real(dp) :: pos = 1.0_dp
        character(len=7) :: color = "#1f77b4"
        integer :: marker = MARKER_NONE
        integer :: linestyle = LINE_SOLID
        real(dp) :: linewidth = 1.5_dp
        real(dp) :: markersize = 6.0_dp
        ! Per-point overrides used by scatter; unallocated means uniform.
        real(dp), allocatable :: psize(:)
        character(len=7), allocatable :: pcolor(:)
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
        ! Image (imshow). One image per axes, as in normal matplotlib use.
        ! Contour set (contour / contourf).
        logical :: frame_off = .false.
        logical :: has_cont = .false.
        logical :: cont_filled = .false.
        real(dp), allocatable :: cz(:, :)
        real(dp), allocatable :: clev(:)
        integer :: cont_cmap = CMAP_VIRIDIS
        real(dp) :: cont_ext(4) = [0.0_dp, 1.0_dp, 0.0_dp, 1.0_dp]
        logical :: has_img = .false.
        ! Set by imshow, and by a scatter that maps c values, so that
        ! colorbar() has a range and colormap to draw.
        logical :: has_cmap_src = .false.
        real(dp), allocatable :: img(:, :)
        integer :: img_cmap = CMAP_VIRIDIS
        real(dp) :: img_vmin = 0.0_dp, img_vmax = 1.0_dp
        real(dp) :: img_ext(4) = [0.0_dp, 1.0_dp, 0.0_dp, 1.0_dp]
        logical :: img_origin_upper = .true.
        ! Data units per point in y over the same in x. Zero means auto.
        ! adjustable="box" shrinks the axes to suit; "datalim" widens the
        ! limits instead, which is what matplotlib's axis("equal") does.
        ! Tick styling. dir is +1 outward, -1 inward, 0 for both.
        real(dp) :: xtick_len = TICK_LEN, ytick_len = TICK_LEN
        real(dp) :: xtick_dir = 1.0_dp, ytick_dir = 1.0_dp
        real(dp) :: xtick_rot = 0.0_dp, ytick_rot = 0.0_dp
        real(dp) :: xtick_size = TICK_FONT, ytick_size = TICK_FONT
        real(dp) :: title_size = TITLE_FONT
        real(dp) :: xlabel_size = LABEL_FONT, ylabel_size = LABEL_FONT
        real(dp) :: legend_size = LEGEND_FONT
        integer :: legend_ncol = 1
        logical :: legend_frame = .true.
        character(len=64) :: legend_title = ""
        real(dp) :: legend_bbox(2) = 0.0_dp
        logical :: legend_has_bbox = .false.
        logical :: spine(4) = .true.
        ! Twin axes: which axes to borrow limits from, and which side this
        ! one's own ticks and label go on. A twin also lets the axes beneath
        ! it show through, so it draws no background.
        integer :: share_x = 0, share_y = 0
        logical :: y_right = .false., x_top = .false.
        logical :: xaxis_off = .false., yaxis_off = .false.
        logical :: patch_off = .false.
        real(dp) :: aspect = 0.0_dp
        logical :: aspect_datalim = .false.
        ! axis("tight"): fit the data exactly, with no 5% margin.
        logical :: tight = .false.
        logical :: cbar_on = .false.
        character(len=32) :: cbar_label = ""
        type(scale_t) :: xsc
        type(scale_t) :: ysc
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
    ! Figure margins and inter-subplot spacing, as matplotlib's
    ! subplots_adjust names them.
    real(dp), save :: fig_left = MARGIN_LEFT, fig_right = MARGIN_RIGHT
    real(dp), save :: fig_bottom = MARGIN_BOTTOM, fig_top = MARGIN_TOP
    real(dp), save :: fig_wspace = WSPACE, fig_hspace = HSPACE
    character(len=256), save :: fig_suptitle = ""
    ! Font sizes for anything not set on an individual axes. New axes take
    ! their sizes from here, so set_fontsize before or after plotting behaves
    ! the same way.
    real(dp), save :: def_title = TITLE_FONT, def_label = LABEL_FONT
    real(dp), save :: def_tick = TICK_FONT, def_legend = LEGEND_FONT
    real(dp), save :: fig_suptitle_size = SUPTITLE_FONT
    type(axes_t), allocatable, save :: ax(:)
    integer, save :: n_ax = 0
    integer, save :: cur_i = 0
    integer, save :: grid_m = 0, grid_n = 0
    logical, save :: fig_initialized = .false.

    ! A parked figure. Holds exactly the module state above, so switching
    ! figures is a copy in and a copy out rather than threading a figure
    ! object through every renderer routine. Any new figure-level variable
    ! must be added here and to stash_fig/unstash_fig.
    type :: figure_t
        logical :: live = .false.
        real(dp) :: w_in, h_in, dpi
        real(dp) :: left, right, bottom, top, wspace, hspace
        character(len=256) :: suptitle
        real(dp) :: d_title, d_label, d_tick, d_legend, suptitle_size
        type(axes_t), allocatable :: ax(:)
        integer :: n_ax, cur_i, grid_m, grid_n
    end type figure_t
    type(figure_t), allocatable, save :: figs(:)
    integer, save :: cur_fig = 0

    ! Pie wedges and colorbar cells are laid out in figure geometry, not on a
    ! user scale, so they map through a plain linear one.
    type(scale_t), parameter :: linear_scale = scale_t(SCALE_LINEAR, 2.0_dp, 1.0_dp)

contains

    subroutine ensure_fig()
        if (.not. fig_initialized) call clf()
        if (cur_fig < 1) then
            cur_fig = next_free_fig()
            call grow_figs(cur_fig)
            figs(cur_fig)%live = .true.
        end if
        ! No axes yet: create a single full-figure axes (pylab default).
        if (cur_i < 1 .or. cur_i > n_ax) then
            call new_axes_grid(1, 1)
            cur_i = 1
        end if
    end subroutine ensure_fig

    ! Copy the live figure state into slot k of the store, and back out again.
    ! These two are the only places that know the full field list.
    subroutine stash_fig(k)
        integer, intent(in) :: k
        figs(k)%live = .true.
        figs(k)%w_in = fig_w_in
        figs(k)%h_in = fig_h_in
        figs(k)%dpi = fig_dpi
        figs(k)%left = fig_left
        figs(k)%right = fig_right
        figs(k)%bottom = fig_bottom
        figs(k)%top = fig_top
        figs(k)%wspace = fig_wspace
        figs(k)%hspace = fig_hspace
        figs(k)%suptitle = fig_suptitle
        figs(k)%d_title = def_title
        figs(k)%d_label = def_label
        figs(k)%d_tick = def_tick
        figs(k)%d_legend = def_legend
        figs(k)%suptitle_size = fig_suptitle_size
        figs(k)%n_ax = n_ax
        figs(k)%cur_i = cur_i
        figs(k)%grid_m = grid_m
        figs(k)%grid_n = grid_n
        if (allocated(figs(k)%ax)) deallocate (figs(k)%ax)
        ! Allocated explicitly rather than relying on reallocation on
        ! assignment, which is not on by default in every compiler.
        if (allocated(ax)) then
            allocate (figs(k)%ax(size(ax)))
            figs(k)%ax = ax
        end if
    end subroutine stash_fig

    subroutine unstash_fig(k)
        integer, intent(in) :: k
        fig_w_in = figs(k)%w_in
        fig_h_in = figs(k)%h_in
        fig_dpi = figs(k)%dpi
        fig_left = figs(k)%left
        fig_right = figs(k)%right
        fig_bottom = figs(k)%bottom
        fig_top = figs(k)%top
        fig_wspace = figs(k)%wspace
        fig_hspace = figs(k)%hspace
        fig_suptitle = figs(k)%suptitle
        def_title = figs(k)%d_title
        def_label = figs(k)%d_label
        def_tick = figs(k)%d_tick
        def_legend = figs(k)%d_legend
        fig_suptitle_size = figs(k)%suptitle_size
        n_ax = figs(k)%n_ax
        cur_i = figs(k)%cur_i
        grid_m = figs(k)%grid_m
        grid_n = figs(k)%grid_n
        if (allocated(ax)) deallocate (ax)
        if (allocated(figs(k)%ax)) then
            allocate (ax(size(figs(k)%ax)))
            ax = figs(k)%ax
        end if
        fig_initialized = .true.
    end subroutine unstash_fig

    subroutine grow_figs(k)
        integer, intent(in) :: k
        type(figure_t), allocatable :: tmp(:)
        integer :: i
        if (.not. allocated(figs)) allocate (figs(0))
        if (k <= size(figs)) return
        allocate (tmp(k))
        do i = 1, size(figs)
            tmp(i) = figs(i)
        end do
        call move_alloc(tmp, figs)
    end subroutine grow_figs

    ! The number of the active figure, matplotlib's gcf().number.
    function gcf() result(num)
        integer :: num
        call ensure_fig()
        num = cur_fig
    end function gcf

    ! close() drops the active figure, close(num) a specific one and
    ! close(all=.true.) every one, freeing the axes and their series data.
    subroutine close(num, all)
        integer, intent(in), optional :: num
        logical, intent(in), optional :: all
        integer :: k, i
        if (present(all)) then
            if (all) then
                if (allocated(figs)) deallocate (figs)
                cur_fig = 0
                call clf()
                fig_initialized = .false.
                return
            end if
        end if
        k = cur_fig
        if (present(num)) k = num
        if (k < 1) return
        if (allocated(figs)) then
            if (k <= size(figs)) then
                figs(k)%live = .false.
                if (allocated(figs(k)%ax)) deallocate (figs(k)%ax)
            end if
        end if
        if (k == cur_fig) then
            call clf()
            fig_initialized = .false.
            cur_fig = 0
            ! Fall back to whichever figure is still open, as pyplot does.
            if (allocated(figs)) then
                do i = size(figs), 1, -1
                    if (figs(i)%live) then
                        cur_fig = i
                        call unstash_fig(i)
                        exit
                    end if
                end do
            end if
        end if
    end subroutine close

    subroutine figure(figsize, dpi, num)
        real(dp), intent(in), optional :: figsize(2), dpi
        integer, intent(in), optional :: num
        integer :: k
        ! Park the figure we are leaving so it can be returned to by number.
        if (cur_fig > 0 .and. fig_initialized) then
            call grow_figs(cur_fig)
            call stash_fig(cur_fig)
        end if
        if (present(num)) then
            if (num < 1) error stop "fplot: figure num must be positive"
            k = num
        else
            k = next_free_fig()
        end if
        call grow_figs(k)
        if (figs(k)%live .and. .not. present(figsize) .and. .not. present(dpi)) then
            ! Reselecting an existing figure resumes it untouched.
            cur_fig = k
            call unstash_fig(k)
            return
        end if
        cur_fig = k
        figs(k)%live = .true.
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

    ! Lowest number not currently in use, matching pyplot's figure numbering.
    function next_free_fig() result(k)
        integer :: k
        if (.not. allocated(figs)) then
            k = 1
            return
        end if
        do k = 1, size(figs)
            if (.not. figs(k)%live) return
        end do
        k = size(figs) + 1
    end function next_free_fig

    subroutine clf()
        cur_i = 0
        if (allocated(ax)) deallocate (ax)
        n_ax = 0
        grid_m = 0
        grid_n = 0
        fig_suptitle = ""
        fig_left = MARGIN_LEFT
        fig_right = MARGIN_RIGHT
        fig_bottom = MARGIN_BOTTOM
        fig_top = MARGIN_TOP
        fig_wspace = WSPACE
        fig_hspace = HSPACE
        def_title = TITLE_FONT
        def_label = LABEL_FONT
        def_tick = TICK_FONT
        def_legend = LEGEND_FONT
        fig_suptitle_size = SUPTITLE_FONT
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
        do i = 1, n_ax
            call apply_font_defaults(ax(i))
        end do
        grid_m = m
        grid_n = n
        call layout_grid()
    end subroutine new_axes_grid

    ! Baseline of the x tick labels below the axes. Reduces to matplotlib's
    ! 16.0 at the default tick size and grows with the ascent of larger text.
    pure function xtick_gap(a) result(v)
        type(axes_t), intent(in) :: a
        real(dp) :: v
        v = 8.4_dp + 0.76_dp * a%xtick_size
    end function xtick_gap

    subroutine apply_font_defaults(a)
        type(axes_t), intent(inout) :: a
        a%title_size = def_title
        a%xlabel_size = def_label
        a%ylabel_size = def_label
        a%xtick_size = def_tick
        a%ytick_size = def_tick
        a%legend_size = def_legend
    end subroutine apply_font_defaults

    ! Place the existing axes in the current margins. Called again whenever
    ! those margins move, so the axes objects themselves survive.
    subroutine layout_grid()
        integer :: i, r, c
        real(dp) :: w, h, dx, dy

        if (grid_m < 1 .or. grid_n < 1) return

        ! Cell size and cell pitch (cell plus gap), in figure fractions.
        w = (fig_right - fig_left) / &
            (real(grid_n, dp) + fig_wspace * real(grid_n - 1, dp))
        h = (fig_top - fig_bottom) / &
            (real(grid_m, dp) + fig_hspace * real(grid_m - 1, dp))
        dx = w * (1.0_dp + fig_wspace)
        dy = h * (1.0_dp + fig_hspace)

        do i = 1, min(n_ax, grid_m * grid_n)
            r = (i - 1) / grid_n     ! row from the top
            c = mod(i - 1, grid_n)   ! column from the left
            ax(i)%left = fig_left + real(c, dp) * dx
            ax(i)%right = ax(i)%left + w
            ax(i)%bottom = fig_bottom + real(grid_m - 1 - r, dp) * dy
            ax(i)%top = ax(i)%bottom + h
        end do

        ! Twins sit exactly on top of the axes they were made from.
        do i = grid_m * grid_n + 1, n_ax
            r = max(ax(i)%share_x, ax(i)%share_y)
            if (r < 1) cycle
            ax(i)%left = ax(r)%left
            ax(i)%right = ax(r)%right
            ax(i)%bottom = ax(r)%bottom
            ax(i)%top = ax(r)%top
        end do
    end subroutine layout_grid

    ! A second axes over the current one, sharing its x axis and putting its
    ! own y axis on the right. It becomes the current axes.
    subroutine twinx()
        call add_twin(.true.)
    end subroutine twinx

    subroutine twiny()
        call add_twin(.false.)
    end subroutine twiny

    subroutine add_twin(share_x_axis)
        logical, intent(in) :: share_x_axis
        type(axes_t), allocatable :: tmp(:)
        integer :: parent

        call ensure_fig()
        parent = cur_i

        call move_alloc(ax, tmp)
        allocate (ax(n_ax + 1))
        ax(1:n_ax) = tmp
        n_ax = n_ax + 1
        cur_i = n_ax

        ax(cur_i)%left = ax(parent)%left
        ax(cur_i)%right = ax(parent)%right
        ax(cur_i)%bottom = ax(parent)%bottom
        ax(cur_i)%top = ax(parent)%top
        ax(cur_i)%patch_off = .true.
        if (share_x_axis) then
            ax(cur_i)%share_x = parent
            ax(cur_i)%xsc = ax(parent)%xsc
            ax(cur_i)%xaxis_off = .true.
            ax(cur_i)%y_right = .true.
        else
            ax(cur_i)%share_y = parent
            ax(cur_i)%ysc = ax(parent)%ysc
            ax(cur_i)%yaxis_off = .true.
            ax(cur_i)%x_top = .true.
        end if
        ! matplotlib keeps counting through one cycle across twinned axes,
        ! so the second curve does not come out the same color as the first.
        ax(cur_i)%color_cycle = ax(parent)%color_cycle
    end subroutine add_twin

    subroutine subplots_adjust(left, right, bottom, top, wspace, hspace)
        real(dp), intent(in), optional :: left, right, bottom, top, wspace, hspace
        call ensure_fig()
        if (present(left)) fig_left = left
        if (present(right)) fig_right = right
        if (present(bottom)) fig_bottom = bottom
        if (present(top)) fig_top = top
        if (present(wspace)) fig_wspace = wspace
        if (present(hspace)) fig_hspace = hspace
        if (fig_right <= fig_left .or. fig_top <= fig_bottom) &
            error stop "fplot: subplots_adjust left<right and bottom<top required"
        call layout_grid()
    end subroutine subplots_adjust

    ! Shrink the margins to what the decorations actually need, so that long
    ! tick labels stop running into the neighbouring subplot or off the page.
    subroutine tight_layout(pad)
        real(dp), intent(in), optional :: pad
        real(dp) :: p, W, H, need_l, need_b, need_t, need_r, inner_l, inner_b
        integer :: i

        call ensure_fig()
        p = 1.08_dp * TICK_FONT
        if (present(pad)) p = pad * TICK_FONT
        W = fig_w_in * PT_PER_IN
        H = fig_h_in * PT_PER_IN

        need_l = 0.0_dp
        need_b = 0.0_dp
        need_t = 0.0_dp
        need_r = 0.0_dp
        inner_l = 0.0_dp
        inner_b = 0.0_dp
        do i = 1, n_ax
            need_l = max(need_l, decor_left(ax(i)))
            need_b = max(need_b, decor_bottom(ax(i)))
            need_t = max(need_t, decor_top(ax(i)))
            ! Only axes away from the left column and the bottom row put
            ! decorations into the gaps between subplots.
            if (mod(i - 1, grid_n) > 0) inner_l = max(inner_l, decor_left(ax(i)))
            if ((i - 1) / grid_n < grid_m - 1) inner_b = max(inner_b, decor_bottom(ax(i)))
        end do
        if (len_trim(fig_suptitle) > 0) need_t = need_t + LABEL_BOX * fig_suptitle_size

        fig_left = (p + need_l) / W
        fig_right = 1.0_dp - (p + need_r) / W
        fig_bottom = (p + need_b) / H
        fig_top = 1.0_dp - (p + need_t) / H

        ! Inner subplots carry the same decorations, so the gaps between them
        ! have to hold those decorations and the same pad again.
        fig_wspace = spacing_for((p + inner_l) / W, fig_right - fig_left, grid_n)
        fig_hspace = spacing_for((p + inner_b) / H, fig_top - fig_bottom, grid_m)
        call layout_grid()
    end subroutine tight_layout

    ! Fraction of a cell that leaves a gap of g between n cells spanning
    ! span, all in figure fractions.
    pure function spacing_for(g, span, n) result(f)
        real(dp), intent(in) :: g, span
        integer, intent(in) :: n
        real(dp) :: f, denom
        f = 0.0_dp
        if (n < 2) return
        denom = span - g * real(n - 1, dp)
        if (denom <= 0.0_dp) return
        f = g * real(n, dp) / denom
    end function spacing_for

    ! Point widths and heights of the decorations outside each axes edge.
    function decor_left(a) result(v)
        type(axes_t), intent(in) :: a
        real(dp) :: v
        v = TICK_LEN + 2.0_dp + tick_label_width(a)
        if (len_trim(a%ylabel) > 0) v = v + LABEL_BOX * a%ylabel_size
    end function decor_left

    function decor_bottom(a) result(v)
        type(axes_t), intent(in) :: a
        real(dp) :: v
        v = TICK_LEN + 1.0_dp + 1.15_dp * a%xtick_size
        if (a%xtick_rot /= 0.0_dp) v = v + tick_label_width(a) * &
                                        abs(sin(a%xtick_rot * PI / 180.0_dp))
        if (len_trim(a%xlabel) > 0) v = v + LABEL_BOX * a%xlabel_size
    end function decor_bottom

    ! How far the decorations reach to the right of the axes box. Only a
    ! colorbar and its labels live out there.
    function decor_right(a, W) result(v)
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: W
        real(dp) :: v, l0, w0
        v = 0.0_dp
        if (.not. a%cbar_on) return
        l0 = a%left * W
        w0 = (a%right * W - l0) / CBAR_SHRINK
        v = l0 + (CBAR_X + CBAR_W) * w0 - a%right * W + 7.0_dp
        if (len_trim(a%cbar_label) > 0) then
            v = v + 34.0_dp
        else
            v = v + 4.0_dp * a%ytick_size
        end if
    end function decor_right

    ! Bounding box of everything actually drawn, in canvas points. This is
    ! what savefig(bbox_inches="tight") crops to, and it is built from the
    ! same decoration estimates that tight_layout uses.
    subroutine drawn_bbox(W, H, x0, y0, x1, y1)
        real(dp), intent(in) :: W, H
        real(dp), intent(out) :: x0, y0, x1, y1
        integer :: i
        real(dp) :: l, r, t, bt
        x0 = W
        y0 = H
        x1 = 0.0_dp
        y1 = 0.0_dp
        do i = 1, n_ax
            l = ax(i)%left * W - decor_left(ax(i))
            r = ax(i)%right * W + decor_right(ax(i), W)
            t = (1.0_dp - ax(i)%top) * H - decor_top(ax(i))
            bt = (1.0_dp - ax(i)%bottom) * H + decor_bottom(ax(i))
            x0 = min(x0, l)
            x1 = max(x1, r)
            y0 = min(y0, t)
            y1 = max(y1, bt)
        end do
        if (len_trim(fig_suptitle) > 0) &
            y0 = min(y0, (1.0_dp - SUPTITLE_Y) * H + 4.2_dp - fig_suptitle_size)
        if (x1 <= x0 .or. y1 <= y0) then
            x0 = 0.0_dp
            y0 = 0.0_dp
            x1 = W
            y1 = H
        end if
    end subroutine drawn_bbox

    function decor_top(a) result(v)
        type(axes_t), intent(in) :: a
        real(dp) :: v
        v = 0.0_dp
        if (len_trim(a%title) > 0) v = LABEL_BOX * a%title_size
    end function decor_top

    ! Widest tick label an axes will draw, in points. Tick text is digits,
    ! a sign and a point, all of which are the same width in DejaVu Sans.
    function tick_label_width(a) result(v)
        type(axes_t), intent(in) :: a
        real(dp) :: v, xmin, xmax, ymin, ymax
        real(dp) :: t(MAX_TICKS)
        character(len=64) :: lbl
        integer :: nt, i, ln
        v = 0.0_dp
        call compute_limits(a, xmin, xmax, ymin, ymax)
        call axis_ticks(a%n_yticks, a%ytick_pos, min(ymin, ymax), max(ymin, ymax), &
                        a%ysc, t, nt)
        do i = 1, nt
            call tick_label(a%ytick_labeled, a%ytick_lab, i, t(i), a%ysc, lbl, ln)
            v = max(v, real(ln, dp) * DIGIT_W * a%ytick_size)
        end do
    end function tick_label_width


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

    subroutine suptitle(s, fontsize)
        character(len=*), intent(in) :: s
        real(dp), intent(in), optional :: fontsize
        call ensure_fig()
        fig_suptitle = s
        if (present(fontsize)) fig_suptitle_size = fontsize
    end subroutine suptitle

    subroutine title(s, fontsize)
        character(len=*), intent(in) :: s
        real(dp), intent(in), optional :: fontsize
        call ensure_fig()
        ax(cur_i)%title = s
        if (present(fontsize)) ax(cur_i)%title_size = fontsize
    end subroutine title

    subroutine xlabel(s, fontsize)
        character(len=*), intent(in) :: s
        real(dp), intent(in), optional :: fontsize
        call ensure_fig()
        ax(cur_i)%xlabel = s
        if (present(fontsize)) ax(cur_i)%xlabel_size = fontsize
    end subroutine xlabel

    subroutine ylabel(s, fontsize)
        character(len=*), intent(in) :: s
        real(dp), intent(in), optional :: fontsize
        call ensure_fig()
        ax(cur_i)%ylabel = s
        if (present(fontsize)) ax(cur_i)%ylabel_size = fontsize
    end subroutine ylabel

    ! One place to set text sizes. size= sets everything at once, which is the
    ! common request; the individual arguments override it. Applies to the
    ! axes that already exist as well as any made later, so it works whether
    ! it is called before or after plotting.
    subroutine set_fontsize(size, title, labels, ticks, legend)
        real(dp), intent(in), optional :: size, title, labels, ticks, legend
        integer :: i

        call ensure_fig()
        if (present(size)) then
            ! Keep matplotlib's proportions: the title is a little larger than
            ! the axis labels, which are larger than the tick labels.
            def_title = size * TITLE_FONT / LABEL_FONT
            def_label = size
            def_tick = size * TICK_FONT / LABEL_FONT
            def_legend = size * LEGEND_FONT / LABEL_FONT
            fig_suptitle_size = size * SUPTITLE_FONT / LABEL_FONT
        end if
        if (present(title)) then
            def_title = title
            fig_suptitle_size = title
        end if
        if (present(labels)) def_label = labels
        if (present(ticks)) def_tick = ticks
        if (present(legend)) def_legend = legend

        do i = 1, n_ax
            ax(i)%title_size = def_title
            ax(i)%xlabel_size = def_label
            ax(i)%ylabel_size = def_label
            ax(i)%xtick_size = def_tick
            ax(i)%ytick_size = def_tick
            ax(i)%legend_size = def_legend
        end do
    end subroutine set_fontsize

    subroutine grid(on)
        logical, intent(in) :: on
        call ensure_fig()
        ax(cur_i)%grid_on = on
    end subroutine grid

    ! bbox_to_anchor is in axes coordinates, so (1.02, 1.0) with the default
    ! loc="upper right" is matplotlib's usual recipe for parking the legend
    ! just outside the right-hand edge. When it is given, loc names which
    ! corner of the legend sits on the anchor rather than a position in the
    ! axes, exactly as matplotlib treats it.
    subroutine legend(loc, fontsize, ncol, frameon, title, bbox_to_anchor)
        character(len=*), intent(in), optional :: loc, title
        real(dp), intent(in), optional :: fontsize, bbox_to_anchor(2)
        integer, intent(in), optional :: ncol
        logical, intent(in), optional :: frameon
        call ensure_fig()
        ax(cur_i)%legend_on = .true.
        if (present(loc)) ax(cur_i)%legend_loc = loc
        if (present(fontsize)) ax(cur_i)%legend_size = fontsize
        if (present(ncol)) ax(cur_i)%legend_ncol = max(1, ncol)
        if (present(frameon)) ax(cur_i)%legend_frame = frameon
        if (present(title)) ax(cur_i)%legend_title = title
        if (present(bbox_to_anchor)) then
            ax(cur_i)%legend_bbox = bbox_to_anchor
            ax(cur_i)%legend_has_bbox = .true.
        end if
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
    ! s and c are the scalar forms; sizes and cvals are their per-point
    ! equivalents. Fortran cannot overload one dummy as scalar-or-array, so
    ! they are separate keywords rather than matplotlib's single s= and c=.
    subroutine scatter(x, y, s, c, marker, label, alpha, sizes, cvals, cmap, vmin, vmax)
        real(dp), intent(in) :: x(:), y(:)
        real(dp), intent(in), optional :: s, alpha, vmin, vmax
        real(dp), intent(in), optional :: sizes(:), cvals(:)
        character(len=*), intent(in), optional :: c, marker, label, cmap
        integer :: is, n, k, id
        real(dp) :: lo, hi

        call ensure_fig()
        if (present(marker)) then
            call add_series(cur_i, x, y, label=label, color=c, marker=marker, &
                            linestyle="None", alpha=alpha)
        else
            call add_series(cur_i, x, y, label=label, color=c, marker="o", &
                            linestyle="None", alpha=alpha)
        end if
        is = ax(cur_i)%n_series
        if (is < 1) return
        n = ax(cur_i)%series(is)%n

        ! matplotlib's s is an area in points squared.
        if (present(s)) ax(cur_i)%series(is)%markersize = sqrt(max(s, 0.0_dp))
        if (present(sizes)) then
            allocate (ax(cur_i)%series(is)%psize(n))
            do k = 1, n
                ax(cur_i)%series(is)%psize(k) = &
                    sqrt(max(sizes(min(k, size(sizes))), 0.0_dp))
            end do
        end if

        if (present(cvals)) then
            id = CMAP_VIRIDIS
            if (present(cmap)) id = cmap_from_str(cmap)
            lo = minval(cvals)
            hi = maxval(cvals)
            if (present(vmin)) lo = vmin
            if (present(vmax)) hi = vmax
            if (hi <= lo) hi = lo + 1.0_dp
            allocate (ax(cur_i)%series(is)%pcolor(n))
            do k = 1, n
                ax(cur_i)%series(is)%pcolor(k) = &
                    cmap_color(id, (cvals(min(k, size(cvals))) - lo) / (hi - lo))
            end do
            ax(cur_i)%has_cmap_src = .true.
            ax(cur_i)%img_cmap = id
            ax(cur_i)%img_vmin = lo
            ax(cur_i)%img_vmax = hi
        end if
    end subroutine scatter

    ! Choose the axis transform explicitly: "linear", "log" or "symlog".
    ! Until now the scale was only ever implied by which plotting call was
    ! used, which leaves no way to put a log axis under a bar chart.
    subroutine set_xscale(name, linthresh, linscale)
        character(len=*), intent(in) :: name
        real(dp), intent(in), optional :: linthresh, linscale
        call ensure_fig()
        ax(cur_i)%xsc = make_scale(name, linthresh, linscale)
    end subroutine set_xscale

    subroutine set_yscale(name, linthresh, linscale)
        character(len=*), intent(in) :: name
        real(dp), intent(in), optional :: linthresh, linscale
        call ensure_fig()
        ax(cur_i)%ysc = make_scale(name, linthresh, linscale)
    end subroutine set_yscale

    function make_scale(name, linthresh, linscale) result(s)
        character(len=*), intent(in) :: name
        real(dp), intent(in), optional :: linthresh, linscale
        type(scale_t) :: s

        select case (lower(name))
        case ("linear")
            s%kind = SCALE_LINEAR
        case ("log")
            s%kind = SCALE_LOG
        case ("symlog")
            s%kind = SCALE_SYMLOG
        case default
            error stop "fplot: unknown scale, expected linear, log or symlog"
        end select
        if (present(linthresh)) then
            if (linthresh <= 0.0_dp) error stop "fplot: linthresh must be positive"
            s%linthresh = linthresh
        end if
        if (present(linscale)) then
            if (linscale <= 0.0_dp) error stop "fplot: linscale must be positive"
            s%linscale = linscale
        end if
    end function make_scale

    subroutine semilogx(x, y, fmt, label, lw, color)
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: fmt, label, color
        real(dp), intent(in), optional :: lw
        call ensure_fig()
        ax(cur_i)%xsc%kind = SCALE_LOG
        call add_series(cur_i, x, y, fmt, label, lw, color)
    end subroutine semilogx

    subroutine semilogy(x, y, fmt, label, lw, color)
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: fmt, label, color
        real(dp), intent(in), optional :: lw
        call ensure_fig()
        ax(cur_i)%ysc%kind = SCALE_LOG
        call add_series(cur_i, x, y, fmt, label, lw, color)
    end subroutine semilogy

    subroutine loglog(x, y, fmt, label, lw, color)
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: fmt, label, color
        real(dp), intent(in), optional :: lw
        call ensure_fig()
        ax(cur_i)%xsc%kind = SCALE_LOG
        ax(cur_i)%ysc%kind = SCALE_LOG
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
    ! shared bookkeeping (point count, color cycling, label).
    function new_shape_series(kd, x, y, color, label, alpha) result(is)
        integer, intent(in) :: kd
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: color, label
        real(dp), intent(in), optional :: alpha
        integer :: is, n
        real(dp) :: ca

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

        ax(cur_i)%series(is)%color = resolve_color(color, ca)
        if (ca >= 0.0_dp .and. .not. present(alpha)) ax(cur_i)%series(is)%alpha = ca
        if (len_trim(ax(cur_i)%series(is)%color) == 0) then
            ax(cur_i)%series(is)%color = color_from_C(ax(cur_i)%color_cycle)
            ax(cur_i)%color_cycle = ax(cur_i)%color_cycle + 1
        end if
    end function new_shape_series

    ! Every spelling of a color matplotlib accepts: "#rgb", "#rrggbb",
    ! "#rrggbbaa", a CSS4/X11 or "tab:" name, a single-letter code, a "C<n>"
    ! cycle index, or a greyscale fraction such as "0.5". Returns an empty
    ! string if the color is not recognised, which lets callers fall back to
    ! the cycle. An "#rrggbbaa" alpha comes back through alpha_out.
    function resolve_color(color, alpha_out) result(col)
        character(len=*), intent(in), optional :: color
        real(dp), intent(out), optional :: alpha_out
        character(len=7) :: col
        character(len=:), allocatable :: c
        integer :: m, n, ios
        real(dp) :: g

        col = ""
        if (present(alpha_out)) alpha_out = -1.0_dp
        if (.not. present(color)) return
        if (len_trim(color) == 0) return
        c = trim(adjustl(color))
        n = len(c)

        if (c(1:1) == "#") then
            select case (n)
            case (4)
                ! "#rgb" is shorthand for "#rrggbb" with each digit doubled.
                col = "#" // c(2:2) // c(2:2) // c(3:3) // c(3:3) // c(4:4) // c(4:4)
            case (7)
                col = c
            case (9)
                col = c(1:7)
                if (present(alpha_out)) alpha_out = real(hex_byte(c(8:9)), dp) / 255.0_dp
            end select
            return
        end if

        if (n >= 2 .and. c(1:1) == "C") then
            m = -1
            read (c(2:n), *, iostat=ios) m
            if (ios == 0 .and. m >= 0) then
                col = color_from_C(m)
                return
            end if
        end if

        if (n == 1) then
            col = color_from_char(c(1:1))
            if (len_trim(col) > 0) return
        end if

        col = color_from_name(c)
        if (len_trim(col) > 0) return

        ! A bare number is a shade of grey, "0" black through "1" white.
        read (c, *, iostat=ios) g
        if (ios == 0 .and. g >= 0.0_dp .and. g <= 1.0_dp) then
            m = nint(g * 255.0_dp)
            col = "#" // hex_pair(m) // hex_pair(m) // hex_pair(m)
        end if
    end function resolve_color

    pure function hex_byte(s) result(v)
        character(len=2), intent(in) :: s
        integer :: v
        v = 16 * hex_digit(s(1:1)) + hex_digit(s(2:2))
    end function hex_byte

    pure function hex_digit(c) result(v)
        character(len=1), intent(in) :: c
        integer :: v, k
        k = iachar(c)
        if (k >= 48 .and. k <= 57) then
            v = k - 48
        else if (k >= 97 .and. k <= 102) then
            v = k - 87
        else if (k >= 65 .and. k <= 70) then
            v = k - 55
        else
            v = 0
        end if
    end function hex_digit

    pure function hex_pair(v) result(s)
        integer, intent(in) :: v
        character(len=2) :: s
        character(len=16), parameter :: D = "0123456789abcdef"
        integer :: w
        w = max(0, min(255, v))
        s = D(w / 16 + 1:w / 16 + 1) // D(mod(w, 16) + 1:mod(w, 16) + 1)
    end function hex_pair

    ! Draw z as an image. z is indexed (row, column) and, with the default
    ! origin="upper", row 1 is drawn at the top, which is why that case gives
    ! a descending y axis exactly as matplotlib does.
    subroutine imshow(z, cmap, vmin, vmax, extent, origin, aspect)
        real(dp), intent(in) :: z(:, :)
        character(len=*), intent(in), optional :: cmap, origin, aspect
        real(dp), intent(in), optional :: vmin, vmax, extent(4)
        integer :: nr, nc
        real(dp) :: lo, hi

        call ensure_fig()
        nr = size(z, 1)
        nc = size(z, 2)
        if (nr < 1 .or. nc < 1) return

        if (allocated(ax(cur_i)%img)) deallocate (ax(cur_i)%img)
        allocate (ax(cur_i)%img(nr, nc))
        ax(cur_i)%img = z
        ax(cur_i)%has_img = .true.
        ax(cur_i)%has_cmap_src = .true.

        ax(cur_i)%img_cmap = CMAP_VIRIDIS
        if (present(cmap)) ax(cur_i)%img_cmap = cmap_from_str(cmap)

        lo = minval(z)
        hi = maxval(z)
        if (present(vmin)) lo = vmin
        if (present(vmax)) hi = vmax
        if (hi <= lo) hi = lo + 1.0_dp
        ax(cur_i)%img_vmin = lo
        ax(cur_i)%img_vmax = hi

        ax(cur_i)%img_origin_upper = .true.
        if (present(origin)) ax(cur_i)%img_origin_upper = trim(origin) /= "lower"

        ! Pixel centres sit on integers, so the edges fall on the half values.
        if (present(extent)) then
            ax(cur_i)%img_ext = extent
        else
            ax(cur_i)%img_ext = [-0.5_dp, real(nc, dp) - 0.5_dp, &
                                 -0.5_dp, real(nr, dp) - 0.5_dp]
        end if

        ax(cur_i)%aspect = 1.0_dp
        if (present(aspect)) then
            if (trim(aspect) == "auto") ax(cur_i)%aspect = 0.0_dp
        end if
    end subroutine imshow

    subroutine contour(z, levels, cmap, extent)
        real(dp), intent(in) :: z(:, :)
        real(dp), intent(in), optional :: levels(:), extent(4)
        character(len=*), intent(in), optional :: cmap
        call add_contour(z, levels, cmap, extent, .false.)
    end subroutine contour

    subroutine contourf(z, levels, cmap, extent)
        real(dp), intent(in) :: z(:, :)
        real(dp), intent(in), optional :: levels(:), extent(4)
        character(len=*), intent(in), optional :: cmap
        call add_contour(z, levels, cmap, extent, .true.)
    end subroutine contourf

    ! z is indexed (row, column) with row 1 at the bottom, which is how
    ! matplotlib orients a contour set: unlike imshow, the y axis ascends.
    subroutine add_contour(z, levels, cmap, extent, filled)
        real(dp), intent(in) :: z(:, :)
        real(dp), intent(in), optional :: levels(:), extent(4)
        character(len=*), intent(in), optional :: cmap
        logical, intent(in) :: filled
        integer :: nr, nc, nt
        real(dp) :: t(MAX_TICKS)

        call ensure_fig()
        nr = size(z, 1)
        nc = size(z, 2)
        if (nr < 2 .or. nc < 2) return

        if (allocated(ax(cur_i)%cz)) deallocate (ax(cur_i)%cz)
        allocate (ax(cur_i)%cz(nr, nc))
        ax(cur_i)%cz = z
        ax(cur_i)%has_cont = .true.
        ax(cur_i)%cont_filled = filled

        ax(cur_i)%cont_cmap = CMAP_VIRIDIS
        if (present(cmap)) ax(cur_i)%cont_cmap = cmap_from_str(cmap)

        if (allocated(ax(cur_i)%clev)) deallocate (ax(cur_i)%clev)
        if (present(levels)) then
            allocate (ax(cur_i)%clev(size(levels)))
            ax(cur_i)%clev = levels
        else
            ! matplotlib picks round levels spanning the data.
            call contour_levels(minval(z), maxval(z), 8, t, nt)
            allocate (ax(cur_i)%clev(nt))
            ax(cur_i)%clev = t(1:nt)
        end if

        if (present(extent)) then
            ax(cur_i)%cont_ext = extent
        else
            ax(cur_i)%cont_ext = [0.0_dp, real(nc - 1, dp), 0.0_dp, real(nr - 1, dp)]
        end if

        ax(cur_i)%has_cmap_src = .true.
        ax(cur_i)%img_cmap = ax(cur_i)%cont_cmap
        ax(cur_i)%img_vmin = ax(cur_i)%clev(1)
        ax(cur_i)%img_vmax = ax(cur_i)%clev(size(ax(cur_i)%clev))
    end subroutine add_contour

    ! Staircase line. matplotlib draws this as an ordinary line through a
    ! doubled-up sequence of points, so building that sequence here is enough
    ! and the renderer needs to know nothing about steps.
    subroutine step(x, y, where, label, color, lw, linestyle, alpha)
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: where, label, color, linestyle
        real(dp), intent(in), optional :: lw, alpha
        integer :: n, i, k
        character(len=8) :: w
        real(dp), allocatable :: sx(:), sy(:)

        n = min(size(x), size(y))
        if (n <= 0) return
        w = "pre"
        if (present(where)) w = where

        ! Each sample contributes two points: the tread of its step and the
        ! riser to the next one. Where the riser sits is what `where` selects.
        allocate (sx(2 * n), sy(2 * n))
        k = 0
        do i = 1, n
            if (trim(w) == "mid") then
                if (i == 1) then
                    sx(k + 1) = x(1)
                else
                    sx(k + 1) = 0.5_dp * (x(i - 1) + x(i))
                end if
                if (i == n) then
                    sx(k + 2) = x(n)
                else
                    sx(k + 2) = 0.5_dp * (x(i) + x(i + 1))
                end if
                sy(k + 1) = y(i)
                sy(k + 2) = y(i)
            else if (trim(w) == "post") then
                sx(k + 1) = x(i)
                sy(k + 1) = y(i)
                if (i == n) then
                    sx(k + 2) = x(n)
                else
                    sx(k + 2) = x(i + 1)
                end if
                sy(k + 2) = y(i)
            else
                sx(k + 1) = x(i)
                if (i == 1) then
                    sy(k + 1) = y(1)
                else
                    sy(k + 1) = y(i - 1)
                end if
                sx(k + 2) = x(i)
                sy(k + 2) = y(i)
            end if
            k = k + 2
        end do

        call plot(sx(1:k), sy(1:k), label=label, color=color, lw=lw, &
                  linestyle=linestyle, alpha=alpha)
    end subroutine step

    ! Horizontal bars: y locates each bar and width is its length.
    subroutine barh(y, width, height, color, label, alpha)
        real(dp), intent(in) :: y(:), width(:)
        real(dp), intent(in), optional :: height, alpha
        character(len=*), intent(in), optional :: color, label
        integer :: is

        call ensure_fig()
        is = new_shape_series(SERIES_BARH, y, width, color, label, alpha)
        if (is < 1) return
        if (present(height)) ax(cur_i)%series(is)%width = height
    end subroutine barh

    ! Markers on stalks rising from y = 0, with a baseline along the bottom.
    subroutine stem(x, y, color, label, alpha)
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: color, label
        real(dp), intent(in), optional :: alpha
        integer :: is

        call ensure_fig()
        is = new_shape_series(SERIES_STEM, x, y, color, label, alpha)
        if (is < 1) return
        ax(cur_i)%series(is)%marker = MARKER_CIRCLE
    end subroutine stem

    ! Pie chart. matplotlib turns the axes into a unit square centred on the
    ! origin and hides the frame, so the wedges are plain data-space geometry.
    subroutine pie(values, labels, cmap)
        real(dp), intent(in) :: values(:)
        character(len=*), intent(in), optional :: labels(:), cmap
        integer :: is, i, n

        call ensure_fig()
        n = size(values)
        if (n <= 0) return
        if (any(values < 0.0_dp) .or. sum(values) <= 0.0_dp) return

        is = new_shape_series(SERIES_PIE, values, values)
        if (is < 1) return
        allocate (ax(cur_i)%series(is)%pcolor(n))
        do i = 1, n
            if (present(cmap)) then
                ax(cur_i)%series(is)%pcolor(i) = &
                    cmap_color(cmap_from_str(cmap), real(i - 1, dp) / real(max(n - 1, 1), dp))
            else
                ax(cur_i)%series(is)%pcolor(i) = color_from_C(i - 1)
            end if
        end do
        if (present(labels)) then
            do i = 1, min(n, size(labels))
                call add_pie_label(values, i, labels(i))
            end do
        end if

        call xlim(-1.25_dp, 1.25_dp)
        call ylim(-1.25_dp, 1.25_dp)
        ax(cur_i)%aspect = 1.0_dp
        ax(cur_i)%frame_off = .true.
    end subroutine pie

    ! Place one wedge label just outside the arc, at the wedge mid angle.
    subroutine add_pie_label(values, i, lab)
        real(dp), intent(in) :: values(:)
        integer, intent(in) :: i
        character(len=*), intent(in) :: lab
        real(dp) :: a0, a1, mid, tot
        integer :: it

        tot = sum(values)
        a0 = 2.0_dp * PI * sum(values(1:i - 1)) / tot
        a1 = 2.0_dp * PI * sum(values(1:i)) / tot
        mid = 0.5_dp * (a0 + a1)
        call push_text(ax(cur_i), it)
        ax(cur_i)%texts(it)%x = 1.1_dp * cos(mid)
        ax(cur_i)%texts(it)%y = 1.1_dp * sin(mid)
        ax(cur_i)%texts(it)%s = lab
        ax(cur_i)%texts(it)%ha = "center"
    end subroutine add_pie_label

    ! matplotlib's tick_params, for the settings that change what is drawn:
    ! which axis, the tick direction, its length, and the size and rotation
    ! of the tick labels.
    subroutine tick_params(axis, direction, length, labelsize, rotation)
        character(len=*), intent(in), optional :: axis, direction
        real(dp), intent(in), optional :: length, labelsize, rotation
        logical :: dox, doy
        real(dp) :: d

        call ensure_fig()
        dox = .true.
        doy = .true.
        if (present(axis)) then
            select case (lower(axis))
            case ("x")
                doy = .false.
            case ("y")
                dox = .false.
            case ("both")
            case default
                error stop "fplot: tick_params axis must be x, y or both"
            end select
        end if

        if (present(direction)) then
            select case (lower(direction))
            case ("out")
                d = 1.0_dp
            case ("in")
                d = -1.0_dp
            case ("inout")
                d = 0.0_dp
            case default
                error stop "fplot: tick direction must be in, out or inout"
            end select
            if (dox) ax(cur_i)%xtick_dir = d
            if (doy) ax(cur_i)%ytick_dir = d
        end if
        if (present(length)) then
            if (dox) ax(cur_i)%xtick_len = length
            if (doy) ax(cur_i)%ytick_len = length
        end if
        if (present(labelsize)) then
            if (dox) ax(cur_i)%xtick_size = labelsize
            if (doy) ax(cur_i)%ytick_size = labelsize
        end if
        if (present(rotation)) then
            if (dox) ax(cur_i)%xtick_rot = rotation
            if (doy) ax(cur_i)%ytick_rot = rotation
        end if
    end subroutine tick_params

    ! Show or hide individual spines. Absent arguments are left alone.
    subroutine spines(left, right, bottom, top)
        logical, intent(in), optional :: left, right, bottom, top
        call ensure_fig()
        if (present(left)) ax(cur_i)%spine(SPINE_LEFT) = left
        if (present(right)) ax(cur_i)%spine(SPINE_RIGHT) = right
        if (present(bottom)) ax(cur_i)%spine(SPINE_BOTTOM) = bottom
        if (present(top)) ax(cur_i)%spine(SPINE_TOP) = top
    end subroutine spines

    ! matplotlib's axis(): "on"/"off" for the frame, "equal"/"scaled" for
    ! square units, "tight" to drop the data margin, "auto" to undo them.
    subroutine axis(mode)
        character(len=*), intent(in) :: mode
        call ensure_fig()
        select case (lower(mode))
        case ("off")
            ax(cur_i)%frame_off = .true.
        case ("on")
            ax(cur_i)%frame_off = .false.
        case ("equal")
            call set_aspect(1.0_dp, "datalim")
        case ("scaled")
            call set_aspect(1.0_dp, "box")
        case ("tight")
            ax(cur_i)%tight = .true.
        case ("auto")
            ax(cur_i)%aspect = 0.0_dp
            ax(cur_i)%tight = .false.
        case default
            error stop "fplot: unknown axis mode"
        end select
    end subroutine axis

    ! ratio is the length of one y unit over the length of one x unit.
    subroutine set_aspect(ratio, adjustable)
        real(dp), intent(in) :: ratio
        character(len=*), intent(in), optional :: adjustable
        call ensure_fig()
        if (ratio <= 0.0_dp) error stop "fplot: aspect ratio must be positive"
        ax(cur_i)%aspect = ratio
        ax(cur_i)%aspect_datalim = .false.
        if (present(adjustable)) then
            select case (lower(adjustable))
            case ("box")
                ax(cur_i)%aspect_datalim = .false.
            case ("datalim")
                ax(cur_i)%aspect_datalim = .true.
            case default
                error stop "fplot: adjustable must be box or datalim"
            end select
        end if
    end subroutine set_aspect

    ! One box per call. Fortran has no ragged arrays, so a group of datasets
    ! of differing sizes is built by calling this once per dataset rather than
    ! by passing matplotlib's list.
    subroutine boxplot(y, position, width, color, label)
        real(dp), intent(in) :: y(:)
        real(dp), intent(in), optional :: position, width
        character(len=*), intent(in), optional :: color, label
        call add_dist_series(SERIES_BOX, y, position, width, color, label, 0.15_dp)
    end subroutine boxplot

    subroutine violinplot(y, position, width, color, label)
        real(dp), intent(in) :: y(:)
        real(dp), intent(in), optional :: position, width
        character(len=*), intent(in), optional :: color, label
        call add_dist_series(SERIES_VIOLIN, y, position, width, color, label, 0.5_dp)
    end subroutine violinplot

    subroutine add_dist_series(kd, y, position, width, color, label, wdefault)
        integer, intent(in) :: kd
        real(dp), intent(in) :: y(:), wdefault
        real(dp), intent(in), optional :: position, width
        character(len=*), intent(in), optional :: color, label
        integer :: is, i, nd

        call ensure_fig()
        if (size(y) < 1) return

        ! Unpositioned distributions line up at 1, 2, 3, ... in the order added.
        nd = 0
        do i = 1, ax(cur_i)%n_series
            if (ax(cur_i)%series(i)%kind == SERIES_BOX .or. &
                ax(cur_i)%series(i)%kind == SERIES_VIOLIN) nd = nd + 1
        end do

        is = new_shape_series(kd, y, y, color, label)
        if (is < 1) return
        call sort_in_place(ax(cur_i)%series(is)%y)
        ax(cur_i)%series(is)%pos = real(nd + 1, dp)
        if (present(position)) ax(cur_i)%series(is)%pos = position
        ax(cur_i)%series(is)%width = wdefault
        if (present(width)) ax(cur_i)%series(is)%width = width
        ! Neither kind takes a turn in the color cycle: matplotlib draws box
        ! furniture in black and every violin in the first cycle color.
        if (.not. present(color)) then
            if (kd == SERIES_BOX) then
                ax(cur_i)%series(is)%color = "#000000"
            else
                ax(cur_i)%series(is)%color = color_from_C(0)
            end if
            ax(cur_i)%color_cycle = ax(cur_i)%color_cycle - 1
        end if
    end subroutine add_dist_series

    ! Insertion sort. The samples behind one box are few and already close to
    ! sorted often enough that anything cleverer would not pay for itself.
    pure subroutine sort_in_place(v)
        real(dp), intent(inout) :: v(:)
        integer :: i, j
        real(dp) :: t
        do i = 2, size(v)
            t = v(i)
            j = i - 1
            do while (j >= 1)
                if (v(j) <= t) exit
                v(j + 1) = v(j)
                j = j - 1
            end do
            v(j + 1) = t
        end do
    end subroutine sort_in_place

    ! Linear-interpolated quantile of an already sorted sample, matching the
    ! default of numpy.percentile.
    pure function quantile(v, q) result(r)
        real(dp), intent(in) :: v(:), q
        real(dp) :: r, h
        integer :: lo
        h = q * real(size(v) - 1, dp)
        lo = min(max(int(floor(h)), 0), size(v) - 1)
        if (lo + 1 >= size(v)) then
            r = v(size(v))
        else
            r = v(lo + 1) + (h - real(lo, dp)) * (v(lo + 2) - v(lo + 1))
        end if
    end function quantile

    subroutine colorbar(label)
        character(len=*), intent(in), optional :: label
        call ensure_fig()
        if (.not. ax(cur_i)%has_cmap_src) return
        if (ax(cur_i)%cbar_on) return
        ax(cur_i)%cbar_on = .true.
        if (present(label)) ax(cur_i)%cbar_label = label
        ! Same split matplotlib uses: the axes keeps 80% of its width and the
        ! bar sits in the gap that frees up.
        ax(cur_i)%right = ax(cur_i)%left + CBAR_SHRINK * (ax(cur_i)%right - ax(cur_i)%left)
    end subroutine colorbar

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
        ! Reference lines default to black, not to the color cycle.
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
        real(dp) :: ca
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

        col = resolve_color(color, ca)
        if (len_trim(col) > 0) ax(ia)%series(is)%color = col
        ! An "#rrggbbaa" spelling carries its own alpha; an explicit alpha=
        ! argument still wins, matching matplotlib.
        if (ca >= 0.0_dp) ax(ia)%series(is)%alpha = ca

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
        real(dp) :: xv, yv, ylo, yhi, xlo, xhi, dx, dy, hw
        logical :: anyx, anyy, sticky_lo, sticky_hi, sx_lo, sx_hi

        anyx = .false.
        anyy = .false.
        sticky_lo = .false.
        sticky_hi = .false.
        sx_lo = .false.
        sx_hi = .false.
        xmin = huge(1.0_dp)
        xmax = -huge(1.0_dp)
        ymin = huge(1.0_dp)
        ymax = -huge(1.0_dp)

        do i = 1, a%n_series
            ! Bars occupy a span in x; a hline/vline constrains one axis only.
            hw = 0.0_dp
            if (a%series(i)%kind == SERIES_BAR) hw = 0.5_dp * a%series(i)%width

            ! A pie sets its own limits, and horizontal bars use the two axes
            ! the other way round, so neither fits the loop below.
            if (a%series(i)%kind == SERIES_PIE) cycle
            if (a%series(i)%kind == SERIES_BOX .or. &
                a%series(i)%kind == SERIES_VIOLIN) then
                anyx = .true.
                if (a%series(i)%kind == SERIES_BOX) then
                    ! matplotlib pins the category axis half a slot beyond the
                    ! outermost box, with no extra margin. A violin instead
                    ! autoscales to its own body like any other artist.
                    sx_lo = .true.
                    sx_hi = .true.
                    xmin = min(xmin, a%series(i)%pos - 0.5_dp)
                    xmax = max(xmax, a%series(i)%pos + 0.5_dp)
                else
                    xmin = min(xmin, a%series(i)%pos - 0.5_dp * a%series(i)%width)
                    xmax = max(xmax, a%series(i)%pos + 0.5_dp * a%series(i)%width)
                end if
                do j = 1, a%series(i)%n
                    anyy = .true.
                    ymin = min(ymin, a%series(i)%y(j))
                    ymax = max(ymax, a%series(i)%y(j))
                end do
                cycle
            end if
            if (a%series(i)%kind == SERIES_BARH) then
                hw = 0.5_dp * a%series(i)%width
                do j = 1, a%series(i)%n
                    anyx = .true.
                    anyy = .true.
                    xmin = min(xmin, 0.0_dp, a%series(i)%y(j))
                    xmax = max(xmax, 0.0_dp, a%series(i)%y(j))
                    ymin = min(ymin, a%series(i)%x(j) - hw)
                    ymax = max(ymax, a%series(i)%x(j) + hw)
                end do
                if (xmin >= 0.0_dp) sx_lo = .true.
                if (xmax <= 0.0_dp) sx_hi = .true.
                cycle
            end if

            do j = 1, a%series(i)%n
                if (a%series(i)%kind /= SERIES_HLINE) then
                    xv = a%series(i)%x(j)
                    if (.not. (a%xsc%kind == SCALE_LOG .and. xv - hw <= 0.0_dp)) then
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
                    if (.not. (a%ysc%kind == SCALE_LOG .and. yhi <= 0.0_dp)) then
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

        if (a%has_cont) then
            anyx = .true.
            anyy = .true.
            xmin = min(xmin, a%cont_ext(1))
            xmax = max(xmax, a%cont_ext(2))
            ymin = min(ymin, a%cont_ext(3))
            ymax = max(ymax, a%cont_ext(4))
        end if

        if (a%has_img) then
            anyx = .true.
            anyy = .true.
            xmin = min(xmin, a%img_ext(1))
            xmax = max(xmax, a%img_ext(2))
            ymin = min(ymin, a%img_ext(3))
            ymax = max(ymax, a%img_ext(4))
        end if

        if (a%xlim_set) then
            xmin = a%xmin_user
            xmax = a%xmax_user
        else
            if (.not. anyx) then
                xmin = 0.0_dp
                xmax = 1.0_dp
            end if
            if (.not. a%tight) call expand_limits(xmin, xmax, a%xsc, sx_lo, sx_hi)
        end if

        if (a%ylim_set) then
            ymin = a%ymin_user
            ymax = a%ymax_user
        else
            if (.not. anyy) then
                ymin = 0.0_dp
                ymax = 1.0_dp
            end if
            if (.not. a%tight) call expand_limits(ymin, ymax, a%ysc, sticky_lo, sticky_hi)
        end if

        if (a%has_cont) then
            if (.not. a%xlim_set) then
                xmin = a%cont_ext(1)
                xmax = a%cont_ext(2)
            end if
            if (.not. a%ylim_set) then
                ymin = a%cont_ext(3)
                ymax = a%cont_ext(4)
            end if
        end if

        ! An image fits its extent exactly, and origin="upper" puts the first
        ! row at the top, which matplotlib expresses as a descending y axis.
        if (a%has_img) then
            if (.not. a%xlim_set) then
                xmin = a%img_ext(1)
                xmax = a%img_ext(2)
            end if
            if (.not. a%ylim_set) then
                ymin = a%img_ext(3)
                ymax = a%img_ext(4)
                if (a%img_origin_upper) call swap(ymin, ymax)
            end if
        end if
        ! A twin borrows the shared axis wholesale, so the two sets of data
        ! stay registered against each other.
        if (a%share_x > 0) call compute_limits(ax(a%share_x), xmin, xmax, ylo, yhi)
        if (a%share_y > 0) call compute_limits(ax(a%share_y), xlo, xhi, ymin, ymax)
    end subroutine compute_limits

    pure subroutine grow_about_centre(lo, hi, f)
        real(dp), intent(inout) :: lo, hi
        real(dp), intent(in) :: f
        real(dp) :: c, h
        c = 0.5_dp * (lo + hi)
        h = 0.5_dp * (hi - lo) * f
        lo = c - h
        hi = c + h
    end subroutine grow_about_centre

    pure subroutine swap(u, v)
        real(dp), intent(inout) :: u, v
        real(dp) :: t
        t = u
        u = v
        v = t
    end subroutine swap

    ! Pad a data range by matplotlib's 5% margin. A sticky edge (the bar
    ! baseline) is left exactly where it is.
    subroutine expand_limits(lo, hi, sc, sticky_lo, sticky_hi)
        real(dp), intent(inout) :: lo, hi
        type(scale_t), intent(in) :: sc
        logical, intent(in) :: sticky_lo, sticky_hi
        real(dp) :: u0, u1, d

        if (sc%kind == SCALE_LOG) then
            if (lo <= 0.0_dp) lo = tiny(1.0_dp)
            if (hi <= lo) hi = lo * 10.0_dp
        end if

        ! The margin is 5% of the drawn length, so it has to be measured in
        ! transformed space; on a linear axis that is the same thing.
        u0 = scale_fwd(sc, lo)
        u1 = scale_fwd(sc, hi)
        d = u1 - u0
        if (abs(d) < 1.0e-30_dp) d = 1.0_dp
        if (.not. sticky_lo) lo = scale_inv(sc, u0 - 0.05_dp * d)
        if (.not. sticky_hi) hi = scale_inv(sc, u1 + 0.05_dp * d)
    end subroutine expand_limits

    pure function map_x(x, xmin, xmax, ax_l, ax_w, sc) result(px)
        real(dp), intent(in) :: x, xmin, xmax, ax_l, ax_w
        type(scale_t), intent(in) :: sc
        real(dp) :: px
        px = ax_l + axis_frac(x, xmin, xmax, sc) * ax_w
    end function map_x

    pure function map_y(y, ymin, ymax, ax_b, ax_h, sc) result(py)
        real(dp), intent(in) :: y, ymin, ymax, ax_b, ax_h
        type(scale_t), intent(in) :: sc
        real(dp) :: py
        py = ax_b - axis_frac(y, ymin, ymax, sc) * ax_h
    end function map_y

    ! Where v sits along the axis, as a fraction from the low end. Every
    ! scale is linear once the values have gone through its transform.
    pure function axis_frac(v, vmin, vmax, sc) result(t)
        real(dp), intent(in) :: v, vmin, vmax
        type(scale_t), intent(in) :: sc
        real(dp) :: t, u0, u1
        u0 = scale_fwd(sc, vmin)
        u1 = scale_fwd(sc, vmax)
        if (u1 == u0) then
            t = 0.5_dp
        else
            t = (scale_fwd(sc, v) - u0) / (u1 - u0)
        end if
    end function axis_frac

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
    subroutine append_polygon(b, px, py, np, color, alpha, seal)
        type(svg_builder), intent(inout) :: b
        real(dp), intent(in) :: px(:), py(:)
        integer, intent(in) :: np
        character(len=*), intent(in) :: color
        real(dp), intent(in) :: alpha
        logical, intent(in), optional :: seal
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
        ! Abutting polygons leave a hairline of background showing through
        ! where the renderer antialiases both edges, so seal the seam by
        ! stroking the outline in the fill color.
        if (present(seal)) then
            if (seal) then
                call builder_append(b, '" stroke="')
                call builder_append(b, color)
                call builder_append(b, '" stroke-width="0.5')
            end if
        end if
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
    subroutine append_bar(b, s, j, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        type(svg_builder), intent(inout) :: b
        type(series_t), intent(in) :: s
        integer, intent(in) :: j
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        real(dp) :: xa, xb, ya, yb, hw

        hw = 0.5_dp * s%width
        if (s%kind == SERIES_BARH) then
            ! x holds the bar position and y its length, so the roles of the
            ! two axes are simply swapped.
            ya = map_y(s%x(j) - hw, ymin, ymax, ax_b, ax_h, ysc)
            yb = map_y(s%x(j) + hw, ymin, ymax, ax_b, ax_h, ysc)
            xa = map_x(0.0_dp, xmin, xmax, ax_l, ax_w, xsc)
            xb = map_x(s%y(j), xmin, xmax, ax_l, ax_w, xsc)
        else
            xa = map_x(s%x(j) - hw, xmin, xmax, ax_l, ax_w, xsc)
            xb = map_x(s%x(j) + hw, xmin, xmax, ax_l, ax_w, xsc)
            ya = map_y(0.0_dp, ymin, ymax, ax_b, ax_h, ysc)
            yb = map_y(s%y(j), ymin, ymax, ax_b, ax_h, ysc)
        end if

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
    subroutine append_fill(b, s, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        type(svg_builder), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        integer :: j, np
        real(dp), allocatable :: px(:), py(:)

        np = 2 * s%n
        allocate (px(np), py(np))
        do j = 1, s%n
            px(j) = map_x(s%x(j), xmin, xmax, ax_l, ax_w, xsc)
            py(j) = map_y(s%y(j), ymin, ymax, ax_b, ax_h, ysc)
        end do
        ! Return along the lower edge to close the band.
        do j = 1, s%n
            px(s%n + j) = map_x(s%x(s%n - j + 1), xmin, xmax, ax_l, ax_w, xsc)
            py(s%n + j) = map_y(s%y2(s%n - j + 1), ymin, ymax, ax_b, ax_h, ysc)
        end do
        call append_polygon(b, px, py, np, trim(s%color), s%alpha)
    end subroutine append_fill

    ! Vertical error bar with caps for point j.
    subroutine append_errorbar(b, s, j, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        type(svg_builder), intent(inout) :: b
        type(series_t), intent(in) :: s
        integer, intent(in) :: j
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        real(dp) :: px, plo, phi, cap

        px = map_x(s%x(j), xmin, xmax, ax_l, ax_w, xsc)
        plo = map_y(s%y(j) - s%y2(j), ymin, ymax, ax_b, ax_h, ysc)
        phi = map_y(s%y(j) + s%y2(j), ymin, ymax, ax_b, ax_h, ysc)
        call append_line(b, px, plo, px, phi, trim(s%color), 1.0_dp, LINE_SOLID, s%alpha)

        cap = s%width
        if (cap > 0.0_dp) then
            call append_line(b, px - cap, plo, px + cap, plo, trim(s%color), 1.0_dp, LINE_SOLID, s%alpha)
            call append_line(b, px - cap, phi, px + cap, phi, trim(s%color), 1.0_dp, LINE_SOLID, s%alpha)
        end if
    end subroutine append_errorbar

    ! User-set tick positions win over the automatic locator.
    subroutine axis_ticks(n_user, user_pos, vmin, vmax, sc, t, nt)
        integer, intent(in) :: n_user
        real(dp), intent(in) :: user_pos(MAX_TICKS), vmin, vmax
        type(scale_t), intent(in) :: sc
        real(dp), intent(out) :: t(MAX_TICKS)
        integer, intent(out) :: nt
        if (n_user > 0) then
            nt = n_user
            t(1:nt) = user_pos(1:nt)
        else
            select case (sc%kind)
            case (SCALE_LOG)
                call log_ticks(vmin, vmax, t, nt)
            case (SCALE_SYMLOG)
                call symlog_ticks(vmin, vmax, t, nt)
            case default
                call linear_ticks(vmin, vmax, 6, t, nt)
            end select
        end if
    end subroutine axis_ticks

    ! Minor ticks subdivide each major interval; a log axis already places its
    ! majors one decade apart, so the 2..9 multiples are what belong between.
    subroutine minor_positions(t, nt, vmin, vmax, sc, m, nm)
        real(dp), intent(in) :: t(MAX_TICKS), vmin, vmax
        integer, intent(in) :: nt
        type(scale_t), intent(in) :: sc
        real(dp), intent(out) :: m(MAX_MINOR)
        integer, intent(out) :: nm
        integer :: i, k
        real(dp) :: step, v

        nm = 0
        if (nt < 2) return
        do i = 1, nt - 1
            if (sc%kind == SCALE_LOG) then
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

    ! Box, median, whiskers at 1.5 IQR clipped to the data, and the outliers
    ! beyond them as open circles.
    subroutine append_box(b, s, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        type(svg_builder), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        real(dp) :: q1, q2, q3, iqr, wlo, whi, hw, cw
        real(dp) :: xl, xr, cl, cr, xc, y1, y3, ym, yl, yh
        integer :: j

        q1 = quantile(s%y(1:s%n), 0.25_dp)
        q2 = quantile(s%y(1:s%n), 0.5_dp)
        q3 = quantile(s%y(1:s%n), 0.75_dp)
        iqr = q3 - q1
        wlo = q1
        whi = q3
        do j = 1, s%n
            if (s%y(j) >= q1 - 1.5_dp * iqr) then
                wlo = s%y(j)
                exit
            end if
        end do
        do j = s%n, 1, -1
            if (s%y(j) <= q3 + 1.5_dp * iqr) then
                whi = s%y(j)
                exit
            end if
        end do

        hw = 0.5_dp * s%width
        cw = 0.25_dp * s%width
        xl = map_x(s%pos - hw, xmin, xmax, ax_l, ax_w, xsc)
        xr = map_x(s%pos + hw, xmin, xmax, ax_l, ax_w, xsc)
        cl = map_x(s%pos - cw, xmin, xmax, ax_l, ax_w, xsc)
        cr = map_x(s%pos + cw, xmin, xmax, ax_l, ax_w, xsc)
        xc = map_x(s%pos, xmin, xmax, ax_l, ax_w, xsc)
        y1 = map_y(q1, ymin, ymax, ax_b, ax_h, ysc)
        y3 = map_y(q3, ymin, ymax, ax_b, ax_h, ysc)
        ym = map_y(q2, ymin, ymax, ax_b, ax_h, ysc)
        yl = map_y(wlo, ymin, ymax, ax_b, ax_h, ysc)
        yh = map_y(whi, ymin, ymax, ax_b, ax_h, ysc)

        call append_stroke_path(b, [xl, xr, xr, xl, xl], [y1, y1, y3, y3, y1], 5, &
                                trim(s%color), 1.0_dp, s%alpha)
        call append_line(b, xc, y1, xc, yl, trim(s%color), 1.0_dp, LINE_SOLID, s%alpha)
        call append_line(b, xc, y3, xc, yh, trim(s%color), 1.0_dp, LINE_SOLID, s%alpha)
        call append_line(b, cl, yl, cr, yl, trim(s%color), 1.0_dp, LINE_SOLID, s%alpha)
        call append_line(b, cl, yh, cr, yh, trim(s%color), 1.0_dp, LINE_SOLID, s%alpha)
        call append_line(b, xl, ym, xr, ym, "#ff7f0e", 1.0_dp, LINE_SOLID, s%alpha)

        do j = 1, s%n
            if (s%y(j) >= wlo .and. s%y(j) <= whi) cycle
            call append_open_circle(b, xc, map_y(s%y(j), ymin, ymax, ax_b, ax_h, ysc), &
                                    3.0_dp, trim(s%color))
        end do
    end subroutine append_box

    subroutine append_open_circle(b, cx, cy, r, color)
        type(svg_builder), intent(inout) :: b
        real(dp), intent(in) :: cx, cy, r
        character(len=*), intent(in) :: color
        call builder_append(b, '<circle cx="')
        call append_num(b, cx)
        call builder_append(b, '" cy="')
        call append_num(b, cy)
        call builder_append(b, '" r="')
        call append_num(b, r)
        call builder_append(b, '" fill="none" stroke="')
        call builder_append(b, color)
        call builder_append(b, '" stroke-width="1"/>')
        call builder_append(b, new_line("a"))
    end subroutine append_open_circle

    ! Mirrored Gaussian kernel density estimate, plus the min/max/range bars
    ! matplotlib draws over it.
    subroutine append_violin(b, s, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        type(svg_builder), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        integer, parameter :: NK = 100
        real(dp) :: g(NK), d(NK)
        real(dp) :: px(2 * NK), py(2 * NK)
        real(dp) :: lo, hi, mu, var, h, dmax, hw, cw, u
        integer :: i, j

        lo = s%y(1)
        hi = s%y(s%n)
        if (hi <= lo) return

        mu = sum(s%y(1:s%n)) / real(s%n, dp)
        var = sum((s%y(1:s%n) - mu)**2) / real(s%n - 1, dp)
        ! Scott's rule, as used by scipy's gaussian_kde and so by matplotlib.
        h = sqrt(var) * real(s%n, dp)**(-0.2_dp)
        if (h <= 0.0_dp) return

        do i = 1, NK
            g(i) = lo + (hi - lo) * real(i - 1, dp) / real(NK - 1, dp)
            d(i) = 0.0_dp
            do j = 1, s%n
                u = (g(i) - s%y(j)) / h
                d(i) = d(i) + exp(-0.5_dp * u * u)
            end do
        end do
        dmax = maxval(d)
        if (dmax <= 0.0_dp) return

        hw = 0.5_dp * s%width
        do i = 1, NK
            px(i) = map_x(s%pos - hw * d(i) / dmax, xmin, xmax, ax_l, ax_w, xsc)
            py(i) = map_y(g(i), ymin, ymax, ax_b, ax_h, ysc)
            px(2 * NK + 1 - i) = map_x(s%pos + hw * d(i) / dmax, xmin, xmax, ax_l, ax_w, xsc)
            py(2 * NK + 1 - i) = py(i)
        end do
        call append_polygon(b, px, py, 2 * NK, trim(s%color), 0.3_dp)

        cw = 0.5_dp * hw
        call append_line(b, map_x(s%pos, xmin, xmax, ax_l, ax_w, xsc), &
                         map_y(lo, ymin, ymax, ax_b, ax_h, ysc), &
                         map_x(s%pos, xmin, xmax, ax_l, ax_w, xsc), &
                         map_y(hi, ymin, ymax, ax_b, ax_h, ysc), &
                         trim(s%color), 1.5_dp, LINE_SOLID, 1.0_dp)
        do i = 1, 2
            u = merge(lo, hi, i == 1)
            call append_line(b, map_x(s%pos - cw, xmin, xmax, ax_l, ax_w, xsc), &
                             map_y(u, ymin, ymax, ax_b, ax_h, ysc), &
                             map_x(s%pos + cw, xmin, xmax, ax_l, ax_w, xsc), &
                             map_y(u, ymin, ymax, ax_b, ax_h, ysc), &
                             trim(s%color), 1.5_dp, LINE_SOLID, 1.0_dp)
        end do
    end subroutine append_violin

    ! One <path> per wedge: a radius out, the arc, and back to the centre.
    subroutine append_pie(b, s, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h)
        type(svg_builder), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        integer :: i
        real(dp) :: tot, a0, a1, cx, cy

        tot = sum(s%y(1:s%n))
        if (tot <= 0.0_dp) return
        cx = map_x(0.0_dp, xmin, xmax, ax_l, ax_w, linear_scale)
        cy = map_y(0.0_dp, ymin, ymax, ax_b, ax_h, linear_scale)

        a1 = 0.0_dp
        do i = 1, s%n
            a0 = a1
            a1 = a0 + 2.0_dp * PI * s%y(i) / tot
            call builder_append(b, '<path d="M ')
            call append_num(b, cx)
            call builder_append(b, " ")
            call append_num(b, cy)
            call builder_append(b, " L ")
            call append_wedge_point(b, a0, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h)
            call builder_append(b, " A ")
            call append_num(b, 0.5_dp * ax_w / (xmax - xmin) * 2.0_dp)
            call builder_append(b, " ")
            call append_num(b, 0.5_dp * ax_h / (ymax - ymin) * 2.0_dp)
            call builder_append(b, " 0 ")
            ! The large-arc flag turns on past half a turn; the sweep flag is
            ! 0 because SVG y grows downwards, inverting the sense of rotation.
            call builder_append(b, merge("1", "0", a1 - a0 > PI))
            call builder_append(b, " 0 ")
            call append_wedge_point(b, a1, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h)
            call builder_append(b, ' Z" fill="')
            call builder_append(b, trim(s%pcolor(i)))
            call append_opacity(b, "fill-opacity", s%alpha)
            call builder_append(b, '" stroke="#ffffff" stroke-width="1"/>')
            call builder_append(b, new_line("a"))
        end do
    end subroutine append_pie

    subroutine append_wedge_point(b, ang, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h)
        type(svg_builder), intent(inout) :: b
        real(dp), intent(in) :: ang, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        call append_num(b, map_x(cos(ang), xmin, xmax, ax_l, ax_w, linear_scale))
        call builder_append(b, " ")
        call append_num(b, map_y(sin(ang), ymin, ymax, ax_b, ax_h, linear_scale))
    end subroutine append_wedge_point

    ! Walk every cell as two triangles, emitting filled bands or level lines.
    subroutine append_contour(b, a, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        type(svg_builder), intent(inout) :: b
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        integer :: nr, nc, i, j, k, tri, nq, ns, nlev, v
        real(dp) :: dx, dy, gx(2), gy(2)
        real(dp) :: tx(3), ty(3), tv(3)
        real(dp) :: qx(MAX_POLY), qy(MAX_POLY)
        real(dp) :: sx(2), sy(2)
        real(dp) :: px(MAX_POLY), py(MAX_POLY)
        real(dp) :: lo, hi, t

        nr = size(a%cz, 1)
        nc = size(a%cz, 2)
        nlev = size(a%clev)
        dx = (a%cont_ext(2) - a%cont_ext(1)) / real(nc - 1, dp)
        dy = (a%cont_ext(4) - a%cont_ext(3)) / real(nr - 1, dp)

        do i = 1, nr - 1
            gy(1) = a%cont_ext(3) + real(i - 1, dp) * dy
            gy(2) = gy(1) + dy
            do j = 1, nc - 1
                gx(1) = a%cont_ext(1) + real(j - 1, dp) * dx
                gx(2) = gx(1) + dx

                do tri = 1, 2
                    call cell_triangle(a%cz, i, j, gx, gy, tri, tx, ty, tv)

                    if (a%cont_filled) then
                        do k = 1, nlev - 1
                            lo = a%clev(k)
                            hi = a%clev(k + 1)
                            call tri_band(tx, ty, tv, lo, hi, qx, qy, nq)
                            if (nq < 3) cycle
                            do v = 1, nq
                                px(v) = map_x(qx(v), xmin, xmax, ax_l, ax_w, xsc)
                                py(v) = map_y(qy(v), ymin, ymax, ax_b, ax_h, ysc)
                            end do
                            t = (real(k, dp) - 0.5_dp) / real(nlev - 1, dp)
                            call append_polygon(b, px, py, nq, &
                                                cmap_color(a%cont_cmap, t), 1.0_dp, seal=.true.)
                        end do
                    else
                        do k = 1, nlev
                            call tri_level(tx, ty, tv, a%clev(k), sx, sy, ns)
                            if (ns /= 2) cycle
                            t = real(k - 1, dp) / real(max(nlev - 1, 1), dp)
                            call append_stroke_path(b, &
                                [map_x(sx(1), xmin, xmax, ax_l, ax_w, xsc), &
                                 map_x(sx(2), xmin, xmax, ax_l, ax_w, xsc)], &
                                [map_y(sy(1), ymin, ymax, ax_b, ax_h, ysc), &
                                 map_y(sy(2), ymin, ymax, ax_b, ax_h, ysc)], &
                                2, cmap_color(a%cont_cmap, t), 1.5_dp, 1.0_dp)
                        end do
                    end if
                end do
            end do
        end do
    end subroutine append_contour

    ! The two triangles of cell (i, j), sharing the diagonal.
    pure subroutine cell_triangle(z, i, j, gx, gy, tri, tx, ty, tv)
        real(dp), intent(in) :: z(:, :), gx(2), gy(2)
        integer, intent(in) :: i, j, tri
        real(dp), intent(out) :: tx(3), ty(3), tv(3)
        if (tri == 1) then
            tx = [gx(1), gx(2), gx(2)]
            ty = [gy(1), gy(1), gy(2)]
            tv = [z(i, j), z(i, j + 1), z(i + 1, j + 1)]
        else
            tx = [gx(1), gx(2), gx(1)]
            ty = [gy(1), gy(2), gy(2)]
            tv = [z(i, j), z(i + 1, j + 1), z(i + 1, j)]
        end if
    end subroutine cell_triangle

    ! One <rect> per sample. SVG has no raster primitive we can reach without
    ! embedding an encoded image, and nearest-neighbour cells are what
    ! matplotlib's default interpolation looks like at these sizes anyway.
    subroutine append_image(b, a, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        type(svg_builder), intent(inout) :: b
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        integer :: nr, nc, i, j
        real(dp) :: xe0, xe1, ye0, ye1, px0, px1, py0, py1, t, dxc, dyc

        nr = size(a%img, 1)
        nc = size(a%img, 2)
        dxc = (a%img_ext(2) - a%img_ext(1)) / real(nc, dp)
        dyc = (a%img_ext(4) - a%img_ext(3)) / real(nr, dp)

        do i = 1, nr
            ye0 = a%img_ext(3) + real(i - 1, dp) * dyc
            ye1 = ye0 + dyc
            py0 = map_y(ye0, ymin, ymax, ax_b, ax_h, ysc)
            py1 = map_y(ye1, ymin, ymax, ax_b, ax_h, ysc)
            do j = 1, nc
                xe0 = a%img_ext(1) + real(j - 1, dp) * dxc
                xe1 = xe0 + dxc
                px0 = map_x(xe0, xmin, xmax, ax_l, ax_w, xsc)
                px1 = map_x(xe1, xmin, xmax, ax_l, ax_w, xsc)
                t = (a%img(i, j) - a%img_vmin) / (a%img_vmax - a%img_vmin)
                call append_cell(b, min(px0, px1), min(py0, py1), &
                                 abs(px1 - px0), abs(py1 - py0), &
                                 cmap_color(a%img_cmap, t))
            end do
        end do
    end subroutine append_image

    ! Cells are grown by a hairline so that neighbours overlap; without it the
    ! renderer leaves visible seams between abutting rectangles.
    subroutine append_cell(b, x, y, w, h, color)
        type(svg_builder), intent(inout) :: b
        real(dp), intent(in) :: x, y, w, h
        character(len=*), intent(in) :: color
        real(dp), parameter :: BLEED = 0.05_dp
        call builder_append(b, '<rect x="')
        call append_num(b, x - BLEED)
        call builder_append(b, '" y="')
        call append_num(b, y - BLEED)
        call builder_append(b, '" width="')
        call append_num(b, w + 2.0_dp * BLEED)
        call builder_append(b, '" height="')
        call append_num(b, h + 2.0_dp * BLEED)
        call builder_append(b, '" fill="')
        call builder_append(b, color)
        call builder_append(b, '"/>')
        call builder_append(b, new_line("a"))
    end subroutine append_cell

    ! Vertical gradient strip plus its own frame, ticks and labels.
    subroutine append_colorbar(b, a, idx, W, H)
        type(svg_builder), intent(inout) :: b
        type(axes_t), intent(in) :: a
        integer, intent(in) :: idx
        real(dp), intent(in) :: W, H
        real(dp) :: bx, bw, bt, bb, bh, y0, y1, t, v, py
        real(dp) :: cb_ticks(MAX_TICKS), lo, hi
        integer :: i, nt, ln
        character(len=64) :: lbl
        character(len=512) :: esc
        real(dp) :: w0, l0

        ! The axes was already shrunk by colorbar(), so recover the original
        ! box that the bar fractions are defined against.
        l0 = a%left * W
        w0 = (a%right * W - l0) / CBAR_SHRINK
        bx = l0 + CBAR_X * w0
        bw = CBAR_W * w0
        bt = (1.0_dp - a%top) * H
        bb = (1.0_dp - a%bottom) * H
        bh = bb - bt

        call builder_append(b, '<defs><clipPath id="axclip')
        call builder_append(b, int_to_str(idx))
        call builder_append(b, 'cb"><rect x="')
        call append_num(b, bx)
        call builder_append(b, '" y="')
        call append_num(b, bt)
        call builder_append(b, '" width="')
        call append_num(b, bw)
        call builder_append(b, '" height="')
        call append_num(b, bh)
        call builder_append(b, '"/></clipPath></defs>')
        call builder_append(b, new_line("a"))

        do i = 1, CBAR_SLICES
            y1 = bb - real(i - 1, dp) * bh / real(CBAR_SLICES, dp)
            y0 = bb - real(i, dp) * bh / real(CBAR_SLICES, dp)
            t = (real(i, dp) - 0.5_dp) / real(CBAR_SLICES, dp)
            call append_cell(b, bx, y0, bw, y1 - y0, cmap_color(a%img_cmap, t))
        end do

        call builder_append(b, '<rect x="')
        call append_num(b, bx)
        call builder_append(b, '" y="')
        call append_num(b, bt)
        call builder_append(b, '" width="')
        call append_num(b, bw)
        call builder_append(b, '" height="')
        call append_num(b, bh)
        call builder_append(b, '" fill="none" stroke="#000000" stroke-width="0.8"/>')
        call builder_append(b, new_line("a"))

        lo = a%img_vmin
        hi = a%img_vmax
        call linear_ticks(lo, hi, 6, cb_ticks, nt)
        do i = 1, nt
            v = cb_ticks(i)
            if (v < lo .or. v > hi) cycle
            py = bb - (v - lo) / (hi - lo) * bh
            call append_tick(b, bx + bw, py, bx + bw + 3.5_dp, py)
            call format_tick_to(v, .false., lbl, ln)
            call append_text(b, bx + bw + 7.0_dp, py + 3.5_dp, lbl(1:ln), &
                             "left", a%ytick_size, "#000000")
        end do

        if (len_trim(a%cbar_label) > 0) then
            call xml_escape_to(a%cbar_label, esc, ln)
            call append_text(b, bx + bw + 34.0_dp, 0.5_dp * (bt + bb), esc(1:ln), &
                             "center", a%ylabel_size, "#000000", &
                             "rotate(-90 " // trim(fmt_pt(bx + bw + 34.0_dp)) // " " // &
                             trim(fmt_pt(0.5_dp * (bt + bb))) // ")")
        end if
    end subroutine append_colorbar

    function fmt_pt(v) result(t)
        real(dp), intent(in) :: v
        character(len=64) :: t
        integer :: n
        call fmt_num(v, t, n)
        t = t(1:n)
    end function fmt_pt

    ! Per-point color when scatter mapped c values, otherwise the series color.
    function point_color(s, j) result(col)
        type(series_t), intent(in) :: s
        integer, intent(in) :: j
        character(len=7) :: col
        if (allocated(s%pcolor)) then
            col = s%pcolor(j)
        else
            col = s%color
        end if
    end function point_color

    ! matplotlib's "best" needs a data-overlap search; upper right is the
    ! placement it picks for the common case, so we use it as the fallback.
    ! Place the legend box against a point given in axes coordinates. loc then
    ! names the corner of the box that touches that point, which is what lets
    ! loc="upper left" with bbox_to_anchor=[1.02, 1] sit outside the axes.
    subroutine legend_anchor(loc, ax_l, ax_t, ax_b, ax_w, bbox, &
                             leg_w, leg_h, leg_x, leg_y)
        character(len=*), intent(in) :: loc
        real(dp), intent(in) :: ax_l, ax_t, ax_b, ax_w, bbox(2), leg_w, leg_h
        real(dp), intent(out) :: leg_x, leg_y
        real(dp) :: px, py
        px = ax_l + bbox(1) * ax_w
        py = ax_b - bbox(2) * (ax_b - ax_t)
        if (index(loc, "right") > 0) then
            leg_x = px - leg_w
        else if (index(loc, "center") > 0 .and. index(loc, "left") == 0) then
            leg_x = px - 0.5_dp * leg_w
        else
            leg_x = px
        end if
        if (index(loc, "lower") > 0) then
            leg_y = py - leg_h
        else if (index(loc, "upper") > 0 .or. loc == "best") then
            leg_y = py
        else
            leg_y = py - 0.5_dp * leg_h
        end if
    end subroutine legend_anchor

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

    subroutine append_spine(b, x1, y1, x2, y2)
        type(svg_builder), intent(inout) :: b
        real(dp), intent(in) :: x1, y1, x2, y2
        call append_line(b, x1, y1, x2, y2, "#000000", 0.8_dp, LINE_SOLID, 1.0_dp)
    end subroutine append_spine

    ! A tick at (x, y) on a spine whose outward normal is (ox, oy). dir 1
    ! puts it outside the axes, -1 inside, 0 straddling the spine.
    subroutine append_tick_at(b, x, y, ox, oy, dir, length)
        type(svg_builder), intent(inout) :: b
        real(dp), intent(in) :: x, y, ox, oy, dir, length
        real(dp) :: a, c
        if (dir > 0.0_dp) then
            a = 0.0_dp
            c = length
        else if (dir < 0.0_dp) then
            a = 0.0_dp
            c = -length
        else
            a = -length
            c = length
        end if
        call append_tick(b, x + ox * a, y + oy * a, x + ox * c, y + oy * c)
    end subroutine append_tick_at

    ! A tick label, rotated about its anchor when asked. SVG rotates
    ! clockwise and matplotlib counter-clockwise, hence the sign.
    subroutine append_tick_text(b, x, y, s, anchor, fontsize, rot)
        type(svg_builder), intent(inout) :: b
        real(dp), intent(in) :: x, y, fontsize, rot
        character(len=*), intent(in) :: s, anchor
        character(len=96) :: tr
        character(len=32) :: n1, n2, n3
        integer :: k1, k2, k3

        if (rot == 0.0_dp) then
            call append_text(b, x, y, s, anchor, fontsize, "#000000")
            return
        end if
        call fmt_num(-rot, n1, k1)
        call fmt_num(x, n2, k2)
        call fmt_num(y, n3, k3)
        tr = "rotate(" // n1(1:k1) // " " // n2(1:k2) // " " // n3(1:k3) // ")"
        call append_text(b, x, y, s, anchor, fontsize, "#000000", trim(tr))
    end subroutine append_tick_text

    subroutine tick_label(labeled, lab, i, v, sc, out, n)
        logical, intent(in) :: labeled
        character(len=24), intent(in) :: lab(MAX_TICKS)
        integer, intent(in) :: i
        real(dp), intent(in) :: v
        type(scale_t), intent(in) :: sc
        character(len=*), intent(out) :: out
        integer, intent(out) :: n
        if (labeled) then
            n = len_trim(lab(i))
            out(1:n) = trim(lab(i))
        else
            call format_tick_to(v, sc%kind == SCALE_LOG, out, n)
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
        real(dp) :: span_x, span_y, sc, new_w, new_h
        integer :: nxt, nyt, nxm, nym, i, j, n, nl
        real(dp) :: px, py, ms, r, mid
        real(dp) :: x_edge, x_out, y_edge, y_out
        character(len=64) :: lbl
        character(len=64) :: tx, ty
        integer :: tn, tyn
        character(len=512) :: esc
        integer :: ln, en
        type(scale_t) :: xsc, ysc
        integer :: n_leg, k, max_lbl, n_col, n_row, lc, lr
        real(dp) :: leg_x, leg_y, leg_w, leg_h, row_h, col_w, ttl_h, leg_x0

        ax_l = a%left * W
        ax_r = a%right * W
        ax_b = (1.0_dp - a%bottom) * H
        ax_t = (1.0_dp - a%top) * H
        ax_w = ax_r - ax_l
        ax_h = ax_b - ax_t

        call compute_limits(a, xmin, xmax, ymin, ymax)

        if (a%aspect > 0.0_dp) then
            span_x = abs(xmax - xmin)
            span_y = abs(ymax - ymin)
            if (span_x > 0.0_dp .and. span_y > 0.0_dp) then
                if (a%aspect_datalim) then
                    ! Stretch whichever range is drawn too short, about its
                    ! own centre, so the box keeps the place it was given.
                    sc = (ax_h / span_y) / (ax_w / span_x) / a%aspect
                    if (sc > 1.0_dp) then
                        call grow_about_centre(ymin, ymax, sc)
                    else
                        call grow_about_centre(xmin, xmax, 1.0_dp / sc)
                    end if
                else
                    sc = min(ax_w / span_x, ax_h / (a%aspect * span_y))
                    new_w = sc * span_x
                    new_h = sc * a%aspect * span_y
                    ax_l = ax_l + 0.5_dp * (ax_w - new_w)
                    ax_t = ax_t + 0.5_dp * (ax_h - new_h)
                    ax_w = new_w
                    ax_h = new_h
                    ax_r = ax_l + ax_w
                    ax_b = ax_t + ax_h
                end if
            end if
        end if
        xsc = a%xsc
        ysc = a%ysc

        call axis_ticks(a%n_xticks, a%xtick_pos, xmin, xmax, xsc, xticks, nxt)
        call axis_ticks(a%n_yticks, a%ytick_pos, min(ymin, ymax), max(ymin, ymax), &
                        ysc, yticks, nyt)
        if (a%minor_ticks) then
            call minor_positions(xticks, nxt, xmin, xmax, xsc, xminor, nxm)
            call minor_positions(yticks, nyt, ymin, ymax, ysc, yminor, nym)
        else
            nxm = 0
            nym = 0
        end if

        ! axis("off") leaves only the artists: no frame, no ticks, no labels.
        if (a%frame_off) then
            nxt = 0
            nyt = 0
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
        if (.not. clear .and. .not. a%patch_off) then
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
                px = map_x(xticks(i), xmin, xmax, ax_l, ax_w, xsc)
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
                py = map_y(yticks(i), ymin, ymax, ax_b, ax_h, ysc)
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

        if (a%has_img .or. a%has_cont) then
            call builder_append(b, '<g clip-path="url(#axclip')
            call builder_append(b, int_to_str(idx))
            call builder_append(b, ')">')
            call builder_append(b, new_line("a"))
            if (a%has_img) &
                call append_image(b, a, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            if (a%has_cont) &
                call append_contour(b, a, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call builder_append(b, "</g>")
            call builder_append(b, new_line("a"))
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
            case (SERIES_BOX)
                call append_box(b, a%series(i), xmin, xmax, ymin, ymax, &
                                ax_l, ax_w, ax_b, ax_h, xsc, ysc)
                cycle
            case (SERIES_VIOLIN)
                call append_violin(b, a%series(i), xmin, xmax, ymin, ymax, &
                                   ax_l, ax_w, ax_b, ax_h, xsc, ysc)
                cycle
            case (SERIES_PIE)
                call append_pie(b, a%series(i), xmin, xmax, ymin, ymax, &
                                ax_l, ax_w, ax_b, ax_h)
                cycle
            case (SERIES_STEM)
                ! Stalk from the baseline to each sample, then the baseline
                ! itself, which matplotlib always draws in red.
                py = map_y(0.0_dp, ymin, ymax, ax_b, ax_h, ysc)
                do j = 1, n
                    px = map_x(a%series(i)%x(j), xmin, xmax, ax_l, ax_w, xsc)
                    call append_line(b, px, py, px, &
                                     map_y(a%series(i)%y(j), ymin, ymax, ax_b, ax_h, ysc), &
                                     trim(a%series(i)%color), a%series(i)%linewidth, &
                                     LINE_SOLID, a%series(i)%alpha)
                end do
                call append_line(b, &
                                 map_x(a%series(i)%x(1), xmin, xmax, ax_l, ax_w, xsc), py, &
                                 map_x(a%series(i)%x(n), xmin, xmax, ax_l, ax_w, xsc), py, &
                                 "#d62728", a%series(i)%linewidth, LINE_SOLID, &
                                 a%series(i)%alpha)
                do j = 1, n
                    call append_marker(b, MARKER_CIRCLE, &
                                       map_x(a%series(i)%x(j), xmin, xmax, ax_l, ax_w, xsc), &
                                       map_y(a%series(i)%y(j), ymin, ymax, ax_b, ax_h, ysc), &
                                       a%series(i)%markersize, trim(a%series(i)%color), &
                                       a%series(i)%alpha)
                end do
                cycle
            case (SERIES_BAR, SERIES_BARH)
                do j = 1, n
                    call append_bar(b, a%series(i), j, xmin, xmax, ymin, ymax, &
                                    ax_l, ax_w, ax_b, ax_h, xsc, ysc)
                end do
                cycle
            case (SERIES_FILL)
                call append_fill(b, a%series(i), xmin, xmax, ymin, ymax, &
                                 ax_l, ax_w, ax_b, ax_h, xsc, ysc)
                cycle
            case (SERIES_HLINE)
                py = map_y(a%series(i)%y(1), ymin, ymax, ax_b, ax_h, ysc)
                call append_line(b, ax_l, py, ax_l + ax_w, py, &
                                 trim(a%series(i)%color), a%series(i)%linewidth, &
                                 a%series(i)%linestyle, a%series(i)%alpha)
                cycle
            case (SERIES_VLINE)
                px = map_x(a%series(i)%x(1), xmin, xmax, ax_l, ax_w, xsc)
                call append_line(b, px, ax_b, px, ax_b - ax_h, &
                                 trim(a%series(i)%color), a%series(i)%linewidth, &
                                 a%series(i)%linestyle, a%series(i)%alpha)
                cycle
            case (SERIES_ERRORBAR)
                do j = 1, n
                    call append_errorbar(b, a%series(i), j, xmin, xmax, ymin, ymax, &
                                         ax_l, ax_w, ax_b, ax_h, xsc, ysc)
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
                    if (a%xsc%kind == SCALE_LOG .and. a%series(i)%x(j) <= 0.0_dp) cycle
                    if (a%ysc%kind == SCALE_LOG .and. a%series(i)%y(j) <= 0.0_dp) cycle
                    px = map_x(a%series(i)%x(j), xmin, xmax, ax_l, ax_w, xsc)
                    py = map_y(a%series(i)%y(j), ymin, ymax, ax_b, ax_h, ysc)
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
                    if (a%xsc%kind == SCALE_LOG .and. a%series(i)%x(j) <= 0.0_dp) cycle
                    if (a%ysc%kind == SCALE_LOG .and. a%series(i)%y(j) <= 0.0_dp) cycle
                    px = map_x(a%series(i)%x(j), xmin, xmax, ax_l, ax_w, xsc)
                    py = map_y(a%series(i)%y(j), ymin, ymax, ax_b, ax_h, ysc)
                    if (allocated(a%series(i)%psize)) ms = a%series(i)%psize(j)
                    call append_marker(b, a%series(i)%marker, px, py, ms, &
                                       point_color(a%series(i), j), a%series(i)%alpha)
                end do
            end if
        end do
        ! annotations, in data coordinates
        do i = 1, a%n_texts
            px = map_x(a%texts(i)%x, xmin, xmax, ax_l, ax_w, xsc)
            py = map_y(a%texts(i)%y, ymin, ymax, ax_b, ax_h, ysc)
            if (a%texts(i)%has_arrow) then
                call append_line(b, px, py, &
                                 map_x(a%texts(i)%xtail, xmin, xmax, ax_l, ax_w, xsc), &
                                 map_y(a%texts(i)%ytail, ymin, ymax, ax_b, ax_h, ysc), &
                                 trim(a%texts(i)%color), 1.0_dp, LINE_SOLID, 1.0_dp)
            end if
            call xml_escape_to(a%texts(i)%s, esc, en)
            call append_text(b, px, py + 3.5_dp, esc(1:en), &
                             trim(a%texts(i)%ha), a%texts(i)%fontsize, &
                             trim(a%texts(i)%color))
        end do

        call builder_append(b, "</g>")
        call builder_append(b, new_line("a"))

        ! spines. All four still go out as one <rect>, so a plot that leaves
        ! them alone renders exactly as it did before they could be hidden.
        if (.not. a%frame_off) then
            if (all(a%spine)) then
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
            else
                if (a%spine(SPINE_LEFT)) call append_spine(b, ax_l, ax_t, ax_l, ax_b)
                if (a%spine(SPINE_RIGHT)) call append_spine(b, ax_r, ax_t, ax_r, ax_b)
                if (a%spine(SPINE_BOTTOM)) call append_spine(b, ax_l, ax_b, ax_r, ax_b)
                if (a%spine(SPINE_TOP)) call append_spine(b, ax_l, ax_t, ax_r, ax_t)
            end if
        end if

        ! x ticks, on the bottom unless this is a twiny
        if (a%x_top) then
            x_edge = ax_t
            x_out = -1.0_dp
        else
            x_edge = ax_b
            x_out = 1.0_dp
        end if
        if (a%yaxis_off) nyt = 0
        if (a%xaxis_off) nxt = 0
        if (a%xaxis_off) nxm = 0
        if (a%yaxis_off) nym = 0

        do i = 1, nxt
            px = map_x(xticks(i), xmin, xmax, ax_l, ax_w, xsc)
            call append_tick_at(b, px, x_edge, 0.0_dp, x_out, a%xtick_dir, a%xtick_len)
            call tick_label(a%xtick_labeled, a%xtick_lab, i, xticks(i), xsc, lbl, ln)
            call append_tick_text(b, px, x_edge + x_out * xtick_gap(a) - &
                                  merge(6.0_dp, 0.0_dp, a%x_top), lbl(1:ln), "center", &
                                  a%xtick_size, a%xtick_rot)
        end do
        do i = 1, nxm
            px = map_x(xminor(i), xmin, xmax, ax_l, ax_w, xsc)
            call append_tick_at(b, px, x_edge, 0.0_dp, x_out, a%xtick_dir, &
                                MINOR_FRAC * a%xtick_len)
        end do

        ! y ticks, on the left unless this is a twinx
        if (a%y_right) then
            y_edge = ax_r
            y_out = 1.0_dp
        else
            y_edge = ax_l
            y_out = -1.0_dp
        end if

        do i = 1, nyt
            py = map_y(yticks(i), ymin, ymax, ax_b, ax_h, ysc)
            call append_tick_at(b, y_edge, py, y_out, 0.0_dp, a%ytick_dir, a%ytick_len)
            call tick_label(a%ytick_labeled, a%ytick_lab, i, yticks(i), ysc, lbl, ln)
            call append_tick_text(b, y_edge + y_out * 7.0_dp, py + 3.5_dp, lbl(1:ln), &
                                  merge("left ", "right", a%y_right), &
                                  a%ytick_size, a%ytick_rot)
        end do
        do i = 1, nym
            py = map_y(yminor(i), ymin, ymax, ax_b, ax_h, ysc)
            call append_tick_at(b, y_edge, py, y_out, 0.0_dp, a%ytick_dir, &
                                MINOR_FRAC * a%ytick_len)
        end do

        if (len_trim(a%xlabel) > 0) then
            call xml_escape_to(a%xlabel, esc, en)
            call append_text(b, 0.5_dp * (ax_l + ax_r), &
                             ax_b + xtick_gap(a) + 0.24_dp * a%xtick_size + 1.84_dp &
                             + 0.76_dp * a%xlabel_size, esc(1:en), &
                             "center", a%xlabel_size, "#000000")
        end if

        if (len_trim(a%ylabel) > 0) then
            call xml_escape_to(a%ylabel, esc, en)
            ! The right-hand label of a twinx faces the other way, so that it
            ! reads from outside the axes just as the left-hand one does.
            mid = y_edge + y_out * (34.0_dp + 0.76_dp * (a%ylabel_size - LABEL_FONT) &
                                    + 1.15_dp * (a%ytick_size - TICK_FONT))
            call fmt_num(mid, tx, tn)
            call fmt_num(0.5_dp * (ax_t + ax_b), ty, tyn)
            call append_text(b, mid, 0.5_dp * (ax_t + ax_b), esc(1:en), &
                             "center", a%ylabel_size, "#000000", &
                             "rotate(" // merge("90 ", "-90", a%y_right) // " " // &
                             tx(1:tn) // " " // ty(1:tyn) // ")")
        end if

        ! title
        if (len_trim(a%title) > 0) then
            call xml_escape_to(a%title, esc, en)
            call append_text(b, 0.5_dp * (ax_l + ax_r), &
                             ax_t - 0.5_dp * a%title_size, esc(1:en), &
                             "center", a%title_size, "#000000")
        end if

        if (a%cbar_on) call append_colorbar(b, a, idx, W, H)

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
                row_h = 18.0_dp * a%legend_size / LEGEND_FONT
                n_col = min(max(1, a%legend_ncol), n_leg)
                n_row = (n_leg + n_col - 1) / n_col
                ! Sample line (leg_x+8 .. leg_x+28), gap to the text at
                ! leg_x+34, the label itself, and a trailing pad.
                col_w = 34.0_dp + real(max_lbl, dp) * LEGEND_CHAR_W &
                        * a%legend_size / LEGEND_FONT + 8.0_dp
                leg_w = real(n_col, dp) * col_w
                ttl_h = 0.0_dp
                if (len_trim(a%legend_title) > 0) ttl_h = row_h
                leg_h = 8.0_dp + ttl_h + real(n_row, dp) * row_h
                if (a%legend_has_bbox) then
                    call legend_anchor(a%legend_loc, ax_l, ax_t, ax_b, &
                                       ax_w, a%legend_bbox, leg_w, leg_h, &
                                       leg_x, leg_y)
                else
                    leg_w = min(leg_w, ax_w - 16.0_dp)
                    call legend_origin(a%legend_loc, ax_l, ax_r, ax_t, ax_b, &
                                       leg_w, leg_h, leg_x, leg_y)
                end if
                if (len_trim(a%legend_title) > 0) then
                    call xml_escape_to(a%legend_title, esc, en)
                    call append_text(b, leg_x + 0.5_dp * leg_w, &
                                     leg_y + 4.0_dp + 0.5_dp * row_h + 3.5_dp, &
                                     esc(1:en), "center", a%legend_size, "#000000")
                end if
                if (a%legend_frame) then
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
                end if
                leg_x0 = leg_x
                k = 0
                do i = 1, a%n_series
                    if (len_trim(a%series(i)%label) == 0) cycle
                    k = k + 1
                    lc = (k - 1) / n_row
                    lr = k - 1 - lc * n_row
                    leg_x = leg_x0 + real(lc, dp) * col_w
                    py = leg_y + 4.0_dp + ttl_h + (real(lr, dp) + 0.5_dp) * row_h
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
                                     "left", a%legend_size, "#000000")
                end do
            end if
        end if
    end subroutine render_axes

    function render_svg(facecolor, transparent, bbox_inches, pad_inches) result(svg)
        character(len=*), intent(in), optional :: facecolor, bbox_inches
        logical, intent(in), optional :: transparent
        real(dp), intent(in), optional :: pad_inches
        character(len=:), allocatable :: svg
        type(svg_builder) :: b
        real(dp) :: W, H, vx, vy, vw, vh, bpad
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

        W = fig_w_in * PT_PER_IN
        H = fig_h_in * PT_PER_IN
        vx = 0.0_dp
        vy = 0.0_dp
        vw = W
        vh = H
        if (present(bbox_inches)) then
            if (lower(trim(bbox_inches)) == "tight") then
                bpad = 0.1_dp
                if (present(pad_inches)) bpad = pad_inches
                bpad = bpad * PT_PER_IN
                ! Cropping is expressed as a shifted viewBox, so the drawing
                ! itself needs no translation.
                call drawn_bbox(W, H, vx, vy, vw, vh)
                vx = vx - bpad
                vy = vy - bpad
                vw = vw - vx + bpad
                vh = vh - vy + bpad
            else if (len_trim(bbox_inches) > 0) then
                error stop "fplot: bbox_inches must be 'tight'"
            end if
        end if

        call builder_append(b, '<?xml version="1.0" encoding="utf-8" standalone="no"?>')
        call builder_append(b, new_line("a"))
        call builder_append(b, '<svg xmlns="http://www.w3.org/2000/svg" ')
        call builder_append(b, 'xmlns:xlink="http://www.w3.org/1999/xlink" width="')
        call append_num(b, vw)
        call builder_append(b, 'pt" height="')
        call append_num(b, vh)
        call builder_append(b, 'pt" viewBox="')
        call append_num(b, vx)
        call builder_append(b, " ")
        call append_num(b, vy)
        call builder_append(b, " ")
        call append_num(b, vw)
        call builder_append(b, " ")
        call append_num(b, vh)
        call builder_append(b, '" version="1.1">')
        call builder_append(b, new_line("a"))

        ! background; transparent drops the figure patch entirely
        if (.not. clear) then
            call builder_append(b, '<rect x="')
            call append_num(b, vx)
            call builder_append(b, '" y="')
            call append_num(b, vy)
            call builder_append(b, '" width="')
            call append_num(b, vw)
            call builder_append(b, '" height="')
            call append_num(b, vh)
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
                             esc(1:en), "center", fig_suptitle_size, "#000000")
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

    ! dpi is accepted and remembered, but SVG is resolution independent and
    ! matplotlib emits the same inches*72 canvas at any dpi, so it does not
    ! change the output here either.
    subroutine savefig(filename, transparent, facecolor, dpi, bbox_inches, pad_inches)
        character(len=*), intent(in) :: filename
        logical, intent(in), optional :: transparent
        character(len=*), intent(in), optional :: facecolor, bbox_inches
        real(dp), intent(in), optional :: dpi, pad_inches
        character(len=:), allocatable :: svg
        integer :: u, ios, n

        call check_svg_ext(filename)
        if (present(dpi)) then
            if (dpi <= 0.0_dp) error stop "fplot: savefig dpi must be positive"
            fig_dpi = dpi
        end if
        svg = render_svg(facecolor, transparent, bbox_inches, pad_inches)
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
