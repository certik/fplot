! fplot — pure Fortran pylab-style SVG plotting library.
module fplot
    use fplot_colors
    use fplot_style
    use fplot_scale
    use fplot_cmap
    use fplot_contour
    use fplot_tri, only: delaunay
    use fplot_ticks
    use fplot_svg
    use fplot_render
    use fplot_backend_svg
    use fplot_backend_pdf
    use fplot_backend_eps
    use fplot_gif, only: gif_encode
    use fplot_proj3d
    use fplot_backend_png
    use fplot_mathtext
    use fplot_dates
    implicit none
    private

    public :: dp
    public :: plot, scatter, semilogx, semilogy, loglog
    public :: set_xscale, set_yscale
    public :: bar, barh, hist, fill_between, fill_betweenx, stackplot
    public :: errorbar, axhline, axvline, axline
    public :: pcolormesh, pcolor, hist2d, hexbin
    public :: matshow, eventplot, broken_barh, streamplot, table
    public :: add_axes, secondary_xaxis, secondary_yaxis
    public :: add_rectangle, add_circle, add_ellipse, add_polygon
    public :: add_arrow, add_path
    public :: polar, set_polar
    public :: quiver
    public :: axhspan, axvspan, hlines, vlines, bar_label
    public :: step, stem, pie, boxplot, violinplot
    public :: axis, set_aspect, tick_params, spines
    public :: margins, autoscale
    public :: text, annotate, figtext, set_facecolor
    public :: xticks, yticks, minorticks_on, locator_params
    public :: ticklabel_format, tick_format, tick_locator
    public :: imshow, colorbar, contour, contourf, clabel
    public :: triplot, tripcolor
    public :: title, xlabel, ylabel, grid, legend
    public :: xlim, ylim, clf, savefig, show, figure
    public :: get_xlim, get_ylim, invert_xaxis, invert_yaxis
    public :: set_bad, set_under, set_over, set_cmap_colors
    public :: figlegend
    public :: render_svg, render_pdf, render_png, render_eps
    public :: add_frame, save_animation
    public :: axes3d, plot3d, scatter3d, plot_surface, plot_wireframe
    public :: view_init, zlabel, zlim
    public :: subplot, subplot2grid, subplot_mosaic, gridspec, suptitle
    public :: subplots_adjust, tight_layout, constrained_layout
    public :: xaxis_date, yaxis_date, date_num
    public :: twinx, twiny
    public :: set_fontsize, set_zorder
    public :: close, gcf
    public :: axes, subplots, sca
    public :: style_use, rc

    ! How the ticks of an axis are written. FMT_AUTO is matplotlib's
    ! ScalarFormatter, which is what an axis does unless it is told
    ! otherwise; the rest are its named formatters.
    ! Most rows or columns a figure's grid can be given a ratio for.
    integer, parameter :: MAX_RATIO = 32

    integer, parameter :: FMT_AUTO = 0
    integer, parameter :: FMT_PERCENT = 1
    integer, parameter :: FMT_COMMA = 2
    integer, parameter :: FMT_FIXED = 3

    ! What one axis agreed to write on its ticks: how many decimals, and
    ! the offset and power of ten factored out of every label and written
    ! once at the end of the axis instead.
    type :: tickfmt_t
        integer :: dec = 0
        real(dp) :: off = 0.0_dp
        integer :: oom = 0
        integer :: style = FMT_AUTO
        real(dp) :: whole = 100.0_dp
    end type tickfmt_t

    ! Initial slot count for the per-axes series and text arrays; both grow
    ! on demand, so this is only the allocation granularity.
    integer, parameter :: INIT_SLOTS = 8
    integer, parameter :: MAX_MINOR = 256

    ! Colorbar layout, as fractions of the axes box before it was shrunk.
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
    ! matplotlib's axes.labelpad, the gap between the tick labels and the
    ! axis label.
    real(dp), parameter :: LABEL_PAD = 4.0_dp
    real(dp), parameter :: PI = 3.141592653589793_dp

    ! What a series draws. LINE covers plot/scatter/semilog*; the rest are
    ! the shape-based plot types.
    ! How a value is turned into a place on the colormap.
    integer, parameter :: NORM_LINEAR = 0
    integer, parameter :: NORM_LOG = 1
    integer, parameter :: NORM_CENTER = 2
    integer, parameter :: NORM_POWER = 3
    integer, parameter :: NORM_SYMLOG = 4

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
    ! HLINES/VLINES: y (x) holds the position of each line and x, y2 its two
    ! ends. HSPAN/VSPAN: two points, one pair in data coordinates and the
    ! other in axes fractions.
    integer, parameter :: SERIES_HLINES = 11
    integer, parameter :: SERIES_VLINES = 12
    integer, parameter :: SERIES_HSPAN = 13
    integer, parameter :: SERIES_VSPAN = 14
    ! PATCH: a closed ring of vertices, filled and outlined.
    integer, parameter :: SERIES_PATCH = 16

    ! matplotlib's default zorders for the artists fplot draws.
    real(dp), parameter :: Z_PATCH = 1.0_dp, Z_GRID = 1.5_dp, Z_LINE = 2.0_dp
    ! QUIVER: x, y hold the tails and qu, qv the vectors.
    integer, parameter :: SERIES_QUIVER = 15
    ! ARROWHEAD: x, y hold the tips and qu, qv the direction. Drawn at a
    ! fixed size in points, which is what streamplot wants.
    integer, parameter :: SERIES_ARROWHEAD = 17
    ! 3D: LINE3D and SCATTER3D carry a z alongside x and y; SURFACE carries
    ! a grid, drawn as one quadrilateral per cell.
    integer, parameter :: SERIES_LINE3D = 18
    integer, parameter :: SERIES_SCATTER3D = 19
    integer, parameter :: SERIES_SURFACE = 20
    ! AXLINE: an endless line through the two points in x(1:2), y(1:2). It
    ! is drawn to the edges of the axes and takes no part in the limits.
    integer, parameter :: SERIES_AXLINE = 21
    ! The most points one streamline may have.
    integer, parameter :: MAX_STREAM_PTS = 20000

    ! Working state for streamplot: the field in grid coordinates, and the
    ! coarse mask that keeps the streamlines apart by refusing a second one
    ! through any cell.
    type :: stream_t
        integer :: nx = 0, ny = 0
        real(dp), allocatable :: u(:, :), v(:, :), sp(:, :)
        integer :: mnx = 30, mny = 30
        integer, allocatable :: mask(:, :)
        real(dp) :: g2mx = 1.0_dp, g2my = 1.0_dp
        real(dp) :: m2gx = 1.0_dp, m2gy = 1.0_dp
        integer :: cx = -1, cy = -1
        integer :: nclaim = 0
        integer, allocatable :: claim(:, :)
    end type stream_t

    type :: series_t
        integer :: kind = SERIES_LINE
        integer :: n = 0
        real(dp), allocatable :: x(:)
        real(dp), allocatable :: y(:)
        ! FILL: lower edge. BAR: the baseline it is stacked on.
        real(dp), allocatable :: y2(:)
        ! ERRORBAR: the four arms, each already a distance from the point.
        real(dp), allocatable :: eylo(:), eyhi(:), exlo(:), exhi(:)
        ! PATCH: whether the ring is filled at all. Its outline is drawn
        ! only when edgecolor names one, as in matplotlib, where a patch is
        ! filled and edgeless unless asked otherwise.
        logical :: patch_fill = .true.
        ! Where the artist sits in the stack. Negative means "whatever this
        ! kind of artist gets by default", which is what series_z works out.
        real(dp) :: zorder = -1.0_dp
        ! A patch drawn from an arbitrary path carries its own verbs; a
        ! plain polygon leaves this alone and every point is a line to.
        integer, allocatable :: pverb(:)
        ! Most patches never ask for room of their own; the ones a plotting
        ! call makes for itself, such as broken_barh, do.
        logical :: patch_scales = .false.
        ! A cell of a mesh, whose seam with its neighbour must not show.
        logical :: patch_seal = .false.
        ! FILL: set by fill_betweenx, where x holds the independent
        ! coordinate and y, y2 the two edges, all with the axes swapped.
        logical :: horiz = .false.
        ! Surfaces: a colormap over z rather than one flat color, and the
        ! wireframe form, which rules the grid instead of filling it.
        integer :: scmap = -1
        logical :: wire = .false.
        ! QUIVER: the vector at each point, and how it is drawn. A negative
        ! scale or width means matplotlib's autoscale.
        real(dp), allocatable :: qu(:), qv(:)
        real(dp) :: qscale = -1.0_dp
        real(dp) :: qwidth = -1.0_dp
        ! BOX/VIOLIN: the position on the category axis, and how the box
        ! is drawn: across instead of up, waisted at the median, with the
        ! mean marked, and filled rather than left as an outline.
        real(dp) :: pos = 1.0_dp
        logical :: box_vert = .true.
        logical :: box_notch = .false.
        logical :: box_mean = .false.
        logical :: box_fill = .false.
        real(dp) :: whis = 1.5_dp
        ! PIE: how far each wedge is pushed out along its own mid angle,
        ! where the first wedge starts, and which way the wedges run.
        real(dp), allocatable :: pexp(:)
        real(dp) :: pie_start = 0.0_dp
        real(dp) :: pie_radius = 1.0_dp
        logical :: pie_ccw = .true.
        character(len=7) :: color = "#1f77b4"
        integer :: marker = MARKER_NONE
        integer :: linestyle = LINE_SOLID
        real(dp) :: linewidth = 1.5_dp
        real(dp) :: markersize = 6.0_dp
        ! Marker colours, empty meaning "the colour of the line", and a
        ! stride so that only every n-th point carries a marker.
        character(len=7) :: mfc = "", mec = ""
        real(dp) :: mew = -1.0_dp
        integer :: markevery = 1
        ! A dash pattern of the caller's own, in points, which overrides
        ! the one the line style would give.
        integer :: n_dash = 0
        real(dp) :: dashes(4) = 0.0_dp
        ! Per-point overrides used by scatter; unallocated means uniform.
        real(dp), allocatable :: psize(:)
        character(len=7), allocatable :: pcolor(:)
        real(dp) :: width = 0.8_dp
        ! Per-bar width, for a histogram with uneven bins.
        real(dp), allocatable :: bwidth(:)
        real(dp) :: alpha = 1.0_dp
        ! bar_label: written at draw time, when the bar top is known in
        ! points and the padding can be honoured exactly.
        logical :: bar_labels = .false.
        real(dp) :: bar_pad = 3.0_dp
        real(dp) :: bar_label_size = 10.0_dp
        character(len=16) :: bar_fmt = ""
        ! matplotlib's hatch: the lines ruled over the fill, and the colour
        ! they are ruled in, black unless an edge colour was named.
        character(len=8) :: hatch = ""
        character(len=7) :: hcolor = ""
        character(len=7) :: edgecolor = "#ffffff"
        real(dp) :: edgewidth = 0.5_dp
        ! Error bars carry their own colour and weight, apart from whatever
        ! the line or the bars are drawn in.
        character(len=7) :: ecolor = ""
        real(dp) :: elw = 1.5_dp, ecap = 0.0_dp
        ! 3D: the third coordinate of a line or scatter, and the grid of a
        ! surface, in the row = y, column = x order the 2D grids use.
        real(dp), allocatable :: z(:)
        real(dp), allocatable :: zg(:, :)
        character(len=128) :: label = ""
    end type series_t

    type :: text_t
        real(dp) :: x = 0.0_dp, y = 0.0_dp
        ! Arrow tail; only used when has_arrow is set.
        real(dp) :: xtail = 0.0_dp, ytail = 0.0_dp
        logical :: has_arrow = .false.
        ! "->" adds a head at the annotated point; the shaft is pulled back
        ! from both ends by shrink points so it never touches either.
        logical :: arrow_head = .false.
        character(len=7) :: arrow_color = "#000000"
        real(dp) :: arrow_lw = 1.0_dp, arrow_shrink = 2.0_dp
        ! matplotlib's "arc3": the shaft bows out of the straight line by
        ! this fraction of its length. Zero leaves it straight.
        real(dp) :: arc_rad = 0.0_dp
        real(dp) :: fontsize = 10.0_dp
        character(len=7) :: color = "#000000"
        character(len=8) :: ha = "left"
        ! Empty means the older placement, which is neither matplotlib's
        ! "baseline" nor any of the others but is what every plot drawn so
        ! far expects.
        character(len=8) :: va = ""
        real(dp) :: rot = 0.0_dp
        integer :: weight = WEIGHT_NORMAL, slant = SLANT_ROMAN
        ! An optional box behind the text.
        logical :: has_box = .false.
        character(len=7) :: box_fc = "#ffffff", box_ec = ""
        real(dp) :: box_alpha = 1.0_dp, box_pad = 0.3_dp
        ! "square" or "round"; round corners have matplotlib's radius of
        ! one pad.
        character(len=8) :: box_style = "square"
        ! figtext places in figure coordinates rather than data ones;
        ! transform="axes" places in fractions of the axes box, so that a
        ! note stays put when the data range changes.
        logical :: in_fig = .false.
        logical :: in_axes = .false.
        character(len=256) :: s = ""
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
        ! Empty means the style sheet's axes colour.
        character(len=7) :: facecolor = ""
        real(dp) :: face_alpha = 1.0_dp
        ! Empty or negative means "whatever the style sheet says".
        character(len=8) :: grid_axis = "both"
        character(len=8) :: grid_which = "major"
        character(len=16) :: grid_color = ""
        integer :: grid_ls = -1
        real(dp) :: grid_lw = -1.0_dp
        real(dp) :: grid_alpha = -1.0_dp
        logical :: legend_on = .false.
        character(len=16) :: legend_loc = "best"
        logical :: minor_ticks = .false.
        ! User-specified tick positions and optional labels.
        integer :: n_xticks = 0, n_yticks = 0
        real(dp) :: xtick_pos(MAX_TICKS), ytick_pos(MAX_TICKS)
        ! Minor ticks placed by hand, and the ends the locator is told to
        ! leave alone ("lower", "upper" or "both").
        integer :: n_xminor = 0, n_yminor = 0
        real(dp) :: xminor_pos(MAX_TICKS), yminor_pos(MAX_TICKS)
        character(len=6) :: xtick_prune = "", ytick_prune = ""
        logical :: xtick_labeled = .false., ytick_labeled = .false.
        character(len=24) :: xtick_lab(MAX_TICKS), ytick_lab(MAX_TICKS)
        ! Image (imshow). One image per axes, as in normal matplotlib use.
        ! Contour set (contour / contourf).
        logical :: frame_off = .false.
        logical :: has_cont = .false.
        logical :: cont_filled = .false.
        ! clabel: a level written into the line itself, the line broken to
        ! make room for it.
        logical :: cont_labels = .false.
        real(dp) :: clab_size = 10.0_dp
        real(dp), allocatable :: cz(:, :)
        real(dp), allocatable :: clev(:)
        integer :: cont_cmap = CMAP_VIRIDIS
        real(dp) :: cont_ext(4) = [0.0_dp, 1.0_dp, 0.0_dp, 1.0_dp]
        logical :: has_img = .false.
        ! imshow(interpolation=): "nearest", the default, hands the samples
        ! over as they are; "bilinear" resamples them at the size the image
        ! is drawn.
        logical :: img_bilinear = .false.
        ! Set by imshow, and by a scatter that maps c values, so that
        ! colorbar() has a range and colormap to draw.
        logical :: has_cmap_src = .false.
        real(dp), allocatable :: img(:, :)
        integer :: img_cmap = CMAP_VIRIDIS
        real(dp) :: img_vmin = 0.0_dp, img_vmax = 1.0_dp
        ! How a value is placed on the colormap: linearly, or by its
        ! logarithm, matplotlib's LogNorm.
        ! How values are mapped onto the colormap.
        integer :: img_norm = NORM_LINEAR
        real(dp) :: img_vcenter = 0.0_dp, img_gamma = 1.0_dp
        real(dp) :: img_linthresh = 1.0_dp
        ! BoundaryNorm: the edges of the bands the data is sorted into. The
        ! image then takes one flat color per band rather than a ramp.
        real(dp), allocatable :: img_bounds(:)
        real(dp) :: img_ext(4) = [0.0_dp, 1.0_dp, 0.0_dp, 1.0_dp]
        logical :: img_origin_upper = .true.
        ! Colours for the values a colormap has nothing to say about, and
        ! a colormap of the caller's own making.
        character(len=7) :: cmap_bad = "", cmap_under = "", cmap_over = ""
        character(len=7), allocatable :: cmap_list(:)
        ! An image whose colours are given outright rather than through a
        ! colormap: (row, column, channel) with three or four channels.
        real(dp), allocatable :: img_rgb(:, :, :)
        logical :: has_rgb = .false.
        ! pcolormesh keeps the same samples in img, but with its own cell
        ! edges instead of an evenly divided extent.
        ! Where the axes sits in the figure's grid, zero based, and how
        ! many cells it spans. A plain subplot spans one of each.
        integer :: g_row = 0, g_col = 0
        integer :: g_rowspan = 1, g_colspan = 1
        ! An axis whose numbers are days since 1970-01-01, and so takes
        ! its ticks and labels from the calendar.
        logical :: x_date = .false., y_date = .false.
        ! Formatter and locator chosen by the user. A style of FMT_AUTO, a
        ! decimal count of -1, a base of zero and a bin count of zero all
        ! mean "whatever matplotlib would have done".
        integer :: xfmt_style = FMT_AUTO, yfmt_style = FMT_AUTO
        integer :: xfmt_dec = -1, yfmt_dec = -1
        real(dp) :: xfmt_whole = 100.0_dp, yfmt_whole = 100.0_dp
        logical :: x_use_offset = .true., y_use_offset = .true.
        integer :: x_scilo = -5, x_scihi = 6
        integer :: y_scilo = -5, y_scihi = 6
        real(dp) :: xtick_base = 0.0_dp, ytick_base = 0.0_dp
        integer :: xtick_nbins = 0, ytick_nbins = 0
        logical :: has_mesh = .false.
        real(dp), allocatable :: mesh_x(:), mesh_y(:)
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
        ! loc is "center", "left" or "right"; the pads are matplotlib's
        ! labelpad in points, measured from its default of LABEL_PAD.
        character(len=6) :: title_loc = "center"
        real(dp) :: xlabel_pad = LABEL_PAD, ylabel_pad = LABEL_PAD
        ! Face of the title and the two axis labels.
        integer :: title_w = WEIGHT_NORMAL, title_sl = SLANT_ROMAN
        integer :: xlabel_w = WEIGHT_NORMAL, xlabel_sl = SLANT_ROMAN
        integer :: ylabel_w = WEIGHT_NORMAL, ylabel_sl = SLANT_ROMAN
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
        ! An axes placed by hand rather than in the grid. An inset keeps
        ! its rectangle in the fractions of the axes it sits in, so it
        ! follows that axes when the layout moves.
        ! A polar axes: x is the angle in radians and y the radius, and the
        ! box holds a circle rather than a rectangle.
        logical :: polar = .false.
        logical :: fixed_pos = .false.
        integer :: inset_of = 0
        real(dp) :: inset_rect(4) = [0.0_dp, 0.0_dp, 1.0_dp, 1.0_dp]
        ! A secondary axis carries the limits of the axes it belongs to,
        ! through v -> sec_scale*v + sec_offset.
        integer :: sec_of = 0
        logical :: sec_is_x = .true.
        real(dp) :: sec_scale = 1.0_dp, sec_offset = 0.0_dp
        ! Shared axes (subplots(sharex=)). Unlike a twin, which borrows the
        ! other axes' limits wholesale, a share group takes the union of what
        ! all its members hold, so every panel shows the same span. link_x is
        ! the index of the axes the group is named after.
        integer :: link_x = 0, link_y = 0
        logical :: xticklabels_off = .false., yticklabels_off = .false.
        logical :: y_right = .false., x_top = .false.
        ! Room a plotting call asks for beyond what it draws. eventplot
        ! keeps a whole line length clear above and below its strokes.
        logical :: yroom_set = .false.
        real(dp) :: yroom(2) = 0.0_dp
        ! A table of text below (or above) the axes.
        logical :: has_table = .false.
        character(len=32), allocatable :: tbl_cells(:, :), tbl_col(:), tbl_row(:)
        real(dp), allocatable :: tbl_w(:)
        real(dp) :: tbl_size = 10.0_dp
        character(len=16) :: tbl_loc = "bottom"
        ! The running top of a stack of histograms, for hist(stacked=.true.).
        real(dp), allocatable :: hstack(:)
        logical :: xroom_set = .false.
        real(dp) :: xroom(2) = 0.0_dp
        ! Whether that room is a sticky edge, taking no margin beyond it.
        logical :: room_sticks = .false.
        ! A 3D axes. The camera angles are matplotlib's defaults and the z
        ! limits behave like the other two, except that z takes no margin.
        logical :: is3d = .false.
        real(dp) :: elev = 30.0_dp, azim = -60.0_dp
        logical :: zlim_set = .false.
        real(dp) :: zmin_user = 0.0_dp, zmax_user = 1.0_dp
        character(len=256) :: zlabel = ""
        logical :: xaxis_off = .false., yaxis_off = .false.
        logical :: patch_off = .false.
        real(dp) :: aspect = 0.0_dp
        logical :: aspect_datalim = .false.
        ! How much room past the data each axis takes, as a fraction of the
        ! drawn length. Matplotlib's five percent unless margins() says
        ! otherwise; zero is axis("tight"), which fits the data exactly.
        real(dp) :: xmargin = 0.05_dp, ymargin = 0.05_dp
        logical :: cbar_on = .false.
        ! Which way the bar runs, and matplotlib's four numbers for where it
        ! sits: how much of the box it takes, how much space is left between
        ! it and the axes, how much of the length it uses, and how many
        ! times longer it is than it is thick.
        logical :: cbar_horiz = .false.
        real(dp) :: cbar_frac = 0.15_dp
        real(dp) :: cbar_pad = 0.05_dp
        real(dp) :: cbar_shrink = 1.0_dp
        real(dp) :: cbar_aspect = 20.0_dp
        character(len=32) :: cbar_label = ""
        type(scale_t) :: xsc
        type(scale_t) :: ysc
        logical :: x_inv = .false., y_inv = .false.
        logical :: xlim_set = .false.
        logical :: ylim_set = .false.
        real(dp) :: xmin_user = 0.0_dp, xmax_user = 1.0_dp
        real(dp) :: ymin_user = 0.0_dp, ymax_user = 1.0_dp
        integer :: color_cycle = 0
        ! Axes position in figure fractions (matplotlib convention).
        real(dp) :: left = MARGIN_LEFT, right = MARGIN_RIGHT
        real(dp) :: bottom = MARGIN_BOTTOM, top = MARGIN_TOP
    end type axes_t

    ! A handle to one axes of the current figure, so that a script can say
    ! ax%plot(...) instead of selecting an axes and then drawing into it.
    !
    ! It holds an index, not a copy: fplot is stateful, and every binding is
    ! "make this the current axes, then call the module procedure of the same
    ! name". That keeps one implementation of each plot type rather than two,
    ! and means the two styles can be mixed freely. Anything without a
    ! binding is still reachable by making the axes current with sca.
    type :: axes
        integer :: idx = 0
    contains
        procedure :: sca => ax_sca
        procedure :: inset_axes => ax_inset_axes
        procedure :: secondary_xaxis => ax_secondary_xaxis
        procedure :: secondary_yaxis => ax_secondary_yaxis
        procedure, private :: ax_plot, ax_plot_cat, ax_plot_y
        generic :: plot => ax_plot, ax_plot_cat, ax_plot_y
        procedure :: scatter => ax_scatter
        procedure, private :: ax_bar, ax_bar_cat, ax_barh, ax_barh_cat
        generic :: bar => ax_bar, ax_bar_cat
        generic :: barh => ax_barh, ax_barh_cat
        procedure :: bar_label => ax_bar_label
        procedure :: hist => ax_hist
        procedure :: fill_between => ax_fill_between
        procedure :: fill_betweenx => ax_fill_betweenx
        procedure :: stackplot => ax_stackplot
        procedure :: errorbar => ax_errorbar
        procedure :: step => ax_step
        procedure :: stem => ax_stem
        procedure :: quiver => ax_quiver
        procedure :: set_polar => ax_set_polar
        procedure :: add_rectangle => ax_add_rectangle
        procedure :: add_circle => ax_add_circle
        procedure :: add_ellipse => ax_add_ellipse
        procedure :: add_polygon => ax_add_polygon
        procedure, private :: ax_imshow, ax_imshow_rgb
        generic :: imshow => ax_imshow, ax_imshow_rgb
        procedure :: xaxis_date => ax_xaxis_date
        procedure :: yaxis_date => ax_yaxis_date
        procedure :: pcolormesh => ax_pcolormesh
        procedure :: pcolor => ax_pcolormesh
        procedure :: clabel => ax_clabel
        procedure :: contour => ax_contour
        procedure :: contourf => ax_contourf
        procedure :: colorbar => ax_colorbar
        procedure :: margins => ax_margins
        procedure :: autoscale => ax_autoscale
        procedure :: axhline => ax_axhline
        procedure :: axvline => ax_axvline
        procedure :: axline => ax_axline
        procedure :: axhspan => ax_axhspan
        procedure :: axvspan => ax_axvspan
        procedure :: hlines => ax_hlines
        procedure :: vlines => ax_vlines
        procedure :: text => ax_text
        procedure :: annotate => ax_annotate
        procedure :: set_title => ax_set_title
        procedure :: set_xlabel => ax_set_xlabel
        procedure :: set_ylabel => ax_set_ylabel
        procedure :: set_xlim => ax_set_xlim
        procedure :: set_ylim => ax_set_ylim
        procedure :: get_xlim => ax_get_xlim
        procedure :: get_ylim => ax_get_ylim
        procedure :: invert_xaxis => ax_invert_xaxis
        procedure :: invert_yaxis => ax_invert_yaxis
        procedure :: set_xscale => ax_set_xscale
        procedure :: set_yscale => ax_set_yscale
        procedure :: set_xticks => ax_set_xticks
        procedure :: set_yticks => ax_set_yticks
        procedure :: set_aspect => ax_set_aspect
        procedure :: grid => ax_grid
        procedure :: set_facecolor => ax_set_facecolor
        procedure :: legend => ax_legend
        procedure :: tick_params => ax_tick_params
        procedure :: spines => ax_spines
        procedure :: axis => ax_axis
        procedure :: set_zorder => ax_set_zorder
        procedure :: axes3d => ax_axes3d
        procedure :: boxplot => ax_boxplot
        procedure :: violinplot => ax_violinplot
        procedure :: broken_barh => ax_broken_barh
        procedure :: eventplot => ax_eventplot
        procedure :: hexbin => ax_hexbin
        procedure :: hist2d => ax_hist2d
        procedure :: loglog => ax_loglog
        procedure :: semilogx => ax_semilogx
        procedure :: semilogy => ax_semilogy
        procedure :: matshow => ax_matshow
        procedure :: minorticks_on => ax_minorticks_on
        procedure :: pie => ax_pie
        procedure :: polar => ax_polar
        procedure :: streamplot => ax_streamplot
        procedure :: table => ax_table
        procedure :: tick_format => ax_tick_format
        procedure :: tick_locator => ax_tick_locator
        procedure :: ticklabel_format => ax_ticklabel_format
        procedure :: add_arrow => ax_add_arrow
        procedure :: add_path => ax_add_path
        procedure :: plot3d => ax_plot3d
        procedure :: scatter3d => ax_scatter3d
        procedure :: plot_surface => ax_plot_surface
        procedure :: plot_wireframe => ax_plot_wireframe
        procedure :: view_init => ax_view_init
        procedure :: set_zlabel => ax_set_zlabel
        procedure :: set_zlim => ax_set_zlim
        procedure :: twinx => ax_twinx
        procedure :: twiny => ax_twiny
    end type axes

    ! subplots(m, n, axs) fills a grid; the rank of axs decides whether the
    ! panels come back shaped or in a row, and a lone axes needs no counts.
    ! Categories instead of numbers on an axis: the same call with a list
    ! of names, which are placed at 0, 1, 2, ... and become the tick labels.
    interface bar
        module procedure bar_num, bar_cat
    end interface bar

    interface barh
        module procedure barh_num, barh_cat
    end interface barh

    interface plot
        module procedure plot_num, plot_cat, plot_y
    end interface plot

    interface imshow
        module procedure imshow_z, imshow_rgb
    end interface imshow

    ! One dataset or a whole block of them, a row to each.
    interface boxplot
        module procedure boxplot_one, boxplot_many
    end interface boxplot

    interface violinplot
        module procedure violinplot_one, violinplot_many
    end interface violinplot

    interface subplots
        module procedure subplots_grid, subplots_row, subplots_one
    end interface subplots

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
    ! Animation frames, packed RGB, all frames end to end. Kept as bytes
    ! rather than as figures because a figure cannot be replayed: the caller
    ! redraws and calls add_frame, exactly as a matplotlib update function
    ! redraws and returns.
    character(len=:), allocatable, save :: anim_pix
    integer, save :: anim_n = 0, anim_w = 0, anim_h = 0
    ! Filled length of anim_pix. The reel is grown by doubling and written
    ! into in place: appending with // would build a temporary as large as
    ! the whole animation on every frame.
    integer, save :: anim_len = 0
    ! Font sizes for anything not set on an individual axes. New axes take
    ! their sizes from here, so set_fontsize before or after plotting behaves
    ! the same way.
    real(dp), save :: def_title = TITLE_FONT, def_label = LABEL_FONT
    real(dp), save :: def_tick = TICK_FONT, def_legend = LEGEND_FONT
    real(dp), save :: fig_suptitle_size = SUPTITLE_FONT
    integer, save :: fig_suptitle_w = WEIGHT_NORMAL
    integer, save :: fig_suptitle_sl = SLANT_ROMAN

    ! ------------------------------------------------------------------
    ! Global defaults, matplotlib's rcParams. A style is nothing but a set
    ! of these: everything below is consulted while drawing or copied into
    ! an axes as it is made, so style_use before the first plot call is what
    ! a user expects it to be.
    ! ------------------------------------------------------------------
    integer, parameter :: MAX_CYCLE = 12
    integer, save :: rc_n_cycle = 0      ! zero means the built-in tab10
    character(len=7), save :: rc_cycle(MAX_CYCLE) = "#000000"
    character(len=7), save :: rc_fig_face = "#ffffff"
    character(len=7), save :: rc_axes_face = "#ffffff"
    character(len=7), save :: rc_grid_color = "#b0b0b0"
    character(len=7), save :: rc_text_color = "#000000"
    character(len=7), save :: rc_spine_color = "#000000"
    real(dp), save :: rc_grid_lw = 0.8_dp
    real(dp), save :: rc_spine_lw = 0.8_dp
    character(len=7), save :: rc_legend_face = "#ffffff"
    character(len=7), save :: rc_legend_edge = "#cccccc"
    real(dp), save :: rc_lw = default_linewidth
    logical, save :: rc_grid = .false.

    ! Clipping region currently in force. See set_clip.
    type(clip_t), save :: g_clip
    type(axes_t), allocatable, save :: ax(:)
    integer, save :: n_ax = 0
    integer, save :: cur_i = 0
    integer, save :: grid_m = 0, grid_n = 0
    ! Relative column widths and row heights, GridSpec's width_ratios and
    ! height_ratios. All ones, the default, is an even grid.
    real(dp), save :: fig_wratio(MAX_RATIO) = 1.0_dp
    real(dp), save :: fig_hratio(MAX_RATIO) = 1.0_dp
    ! A grid whose cells are filled in one at a time by subplot2grid,
    ! rather than all at once by subplot.
    logical, save :: grid_sparse = .false.
    logical, save :: fig_initialized = .false.
    ! Refit the margins to the decorations just before every draw.
    logical, save :: fig_constrained = .false.

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
        logical :: sparse, constrained
        real(dp) :: wratio(MAX_RATIO), hratio(MAX_RATIO)
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
        figs(k)%sparse = grid_sparse
        figs(k)%constrained = fig_constrained
        figs(k)%wratio = fig_wratio
        figs(k)%hratio = fig_hratio
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
        fig_wratio = figs(k)%wratio
        fig_hratio = figs(k)%hratio
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
        grid_sparse = figs(k)%sparse
        fig_constrained = figs(k)%constrained
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
        grid_sparse = .false.
        fig_suptitle = ""
        fig_left = MARGIN_LEFT
        fig_right = MARGIN_RIGHT
        fig_bottom = MARGIN_BOTTOM
        fig_top = MARGIN_TOP
        fig_wspace = WSPACE
        fig_hspace = HSPACE
        fig_constrained = .false.
        fig_wratio = 1.0_dp
        fig_hratio = 1.0_dp
        def_title = TITLE_FONT
        def_label = LABEL_FONT
        def_tick = TICK_FONT
        def_legend = LEGEND_FONT
        fig_suptitle_size = SUPTITLE_FONT
        fig_suptitle_w = WEIGHT_NORMAL
        fig_suptitle_sl = SLANT_ROMAN
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
            ax(i)%g_row = (i - 1) / n
            ax(i)%g_col = mod(i - 1, n)
        end do
        grid_m = m
        grid_n = n
        grid_sparse = .false.
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
        a%grid_on = rc_grid
        a%title_size = def_title
        a%xlabel_size = def_label
        a%ylabel_size = def_label
        a%xtick_size = def_tick
        a%ytick_size = def_tick
        a%legend_size = def_legend
    end subroutine apply_font_defaults

    ! The i-th color of the active cycle, wrapping. This is what "C0".."C9"
    ! resolve to as well, so a style changes both at once.
    pure function cycle_color(idx) result(col)
        integer, intent(in) :: idx
        character(len=7) :: col
        integer :: i
        if (rc_n_cycle <= 0) then
            col = color_from_C(idx)
        else
            i = mod(idx, rc_n_cycle)
            if (i < 0) i = i + rc_n_cycle
            col = rc_cycle(i + 1)
        end if
    end function cycle_color

    subroutine set_cycle(c)
        character(len=7), intent(in) :: c(:)
        rc_n_cycle = min(size(c), MAX_CYCLE)
        rc_cycle(1:rc_n_cycle) = c(1:rc_n_cycle)
    end subroutine set_cycle

    ! matplotlib's style.use. Each style is just a block of rcParams, so
    ! this sets the same globals a user could set one at a time with rc().
    subroutine style_use(name)
        character(len=*), intent(in) :: name

        ! Start from the built-in defaults so styles do not accumulate.
        rc_n_cycle = 0
        rc_fig_face = "#ffffff"
        rc_axes_face = "#ffffff"
        rc_grid_color = "#b0b0b0"
        rc_text_color = "#000000"
        rc_spine_color = "#000000"
        rc_grid_lw = 0.8_dp
        rc_spine_lw = 0.8_dp
        rc_legend_face = "#ffffff"
        rc_legend_edge = "#cccccc"
        rc_lw = default_linewidth
        rc_grid = .false.
        def_title = 12.0_dp
        def_label = 10.0_dp
        def_tick = 10.0_dp
        def_legend = 10.0_dp

        select case (lower(trim(name)))
        case ("default", "classic")
        case ("ggplot")
            rc_axes_face = "#e5e5e5"
            rc_legend_face = "#e5e5e5"
            rc_grid_color = "#ffffff"
            rc_grid_lw = 1.0_dp
            rc_spine_color = "#ffffff"
            rc_spine_lw = 1.0_dp
            rc_text_color = "#555555"
            rc_grid = .true.
            def_title = 14.4_dp
            def_label = 12.0_dp
            call set_cycle(["#e24a33", "#348abd", "#988ed5", "#777777", &
                            "#fbc15e", "#8eba42", "#ffb5b8"])
        case ("seaborn", "seaborn-darkgrid")
            rc_axes_face = "#eaeaf2"
            rc_legend_face = "#eaeaf2"
            rc_grid_color = "#ffffff"
            rc_grid_lw = 1.0_dp
            rc_spine_color = "#eaeaf2"
            rc_text_color = "#262626"
            rc_grid = .true.
            call set_cycle(["#4c72b0", "#dd8452", "#55a868", "#c44e52", &
                            "#8172b3", "#937860", "#da8bc3", "#8c8c8c", &
                            "#ccb974", "#64b5cd"])
        case ("fivethirtyeight")
            rc_fig_face = "#f0f0f0"
            rc_axes_face = "#f0f0f0"
            rc_legend_face = "#f0f0f0"
            rc_grid_color = "#cbcbcb"
            rc_grid_lw = 1.0_dp
            rc_spine_color = "#f0f0f0"
            rc_text_color = "#555555"
            rc_lw = 4.0_dp
            rc_grid = .true.
            call set_cycle(["#008fd5", "#fc4f30", "#e5ae38", "#6d904f", &
                            "#8b8b8b", "#810f7c"])
        case ("dark_background")
            rc_fig_face = "#000000"
            rc_axes_face = "#000000"
            rc_legend_face = "#000000"
            rc_legend_edge = "#ffffff"
            rc_grid_color = "#555555"
            rc_spine_color = "#ffffff"
            rc_text_color = "#ffffff"
            call set_cycle(["#8dd3c7", "#feffb3", "#bfbbd9", "#fa8174", &
                            "#81b1d2", "#fdb462", "#b3de69", "#bc82bd", &
                            "#ccebc4", "#ffed6f"])
        case ("grayscale")
            call set_cycle(["#000000", "#545454", "#7f7f7f", "#b0b0b0", &
                            "#d4d4d4"])
        case default
            error stop "fplot: unknown style"
        end select
    end subroutine style_use

    ! Individual rcParams, for the settings a user is most likely to want
    ! without adopting a whole style.
    subroutine rc(figsize, dpi, fontsize, linewidth, grid, facecolor, &
                  axes_facecolor, grid_color, text_color, color_cycle)
        real(dp), intent(in), optional :: figsize(2), dpi, fontsize, linewidth
        logical, intent(in), optional :: grid
        character(len=*), intent(in), optional :: facecolor, axes_facecolor
        character(len=*), intent(in), optional :: grid_color, text_color
        character(len=7), intent(in), optional :: color_cycle(:)

        if (present(figsize)) then
            fig_w_in = figsize(1)
            fig_h_in = figsize(2)
        end if
        if (present(dpi)) fig_dpi = dpi
        if (present(fontsize)) then
            def_title = fontsize*1.2_dp
            def_label = fontsize
            def_tick = fontsize
            def_legend = fontsize
        end if
        if (present(linewidth)) rc_lw = linewidth
        if (present(grid)) rc_grid = grid
        if (present(facecolor)) rc_fig_face = resolve_color(facecolor)
        if (present(axes_facecolor)) rc_axes_face = resolve_color(axes_facecolor)
        if (present(grid_color)) rc_grid_color = resolve_color(grid_color)
        if (present(text_color)) then
            rc_text_color = resolve_color(text_color)
            rc_spine_color = rc_text_color
        end if
        if (present(color_cycle)) call set_cycle(color_cycle)
    end subroutine rc

    ! Place the existing axes in the current margins. Called again whenever
    ! those margins move, so the axes objects themselves survive.
    ! Where each column starts and how wide it is, in figure fractions.
    ! The gap between cells is the same everywhere, as it is in matplotlib:
    ! wspace and hspace are fractions of the average cell, not of each one,
    ! so uneven ratios change the cells and leave the gaps alone.
    subroutine grid_edges(nc, lo, hi, space, ratio, pos, len)
        integer, intent(in) :: nc
        real(dp), intent(in) :: lo, hi, space, ratio(MAX_RATIO)
        real(dp), intent(out) :: pos(MAX_RATIO), len(MAX_RATIO)
        real(dp) :: cell, sep, total, sumr, p
        integer :: k

        cell = (hi - lo)/(real(nc, dp) + space*real(nc - 1, dp))
        sep = space*cell
        total = cell*real(nc, dp)
        sumr = 0.0_dp
        do k = 1, nc
            sumr = sumr + max(ratio(k), 0.0_dp)
        end do
        if (sumr <= 0.0_dp) sumr = real(nc, dp)
        p = lo
        do k = 1, nc
            len(k) = total*max(ratio(k), 0.0_dp)/sumr
            pos(k) = p
            p = p + len(k) + sep
        end do
    end subroutine grid_edges

    subroutine layout_grid()
        integer :: i, r, c
        real(dp) :: xpos(MAX_RATIO), xlen(MAX_RATIO)
        real(dp) :: ypos(MAX_RATIO), ylen(MAX_RATIO)

        if (grid_m < 1 .or. grid_n < 1) return
        if (grid_m > MAX_RATIO .or. grid_n > MAX_RATIO) return

        call grid_edges(grid_n, fig_left, fig_right, fig_wspace, fig_wratio, xpos, xlen)
        ! Rows are laid out from the top, since that is how they are counted.
        call grid_edges(grid_m, 1.0_dp - fig_top, 1.0_dp - fig_bottom, fig_hspace, &
                        fig_hratio, ypos, ylen)

        do i = 1, n_ax
            if (ax(i)%fixed_pos .or. ax(i)%inset_of > 0) cycle
            ! Twins sit exactly on top of the axes they were made from.
            r = max(ax(i)%share_x, ax(i)%share_y)
            if (r >= 1) cycle
            r = ax(i)%g_row          ! row from the top
            c = ax(i)%g_col          ! column from the left
            ax(i)%left = xpos(c + 1)
            ax(i)%right = xpos(c + ax(i)%g_colspan) + xlen(c + ax(i)%g_colspan)
            ax(i)%top = 1.0_dp - ypos(r + 1)
            ax(i)%bottom = 1.0_dp - (ypos(r + ax(i)%g_rowspan) + ylen(r + ax(i)%g_rowspan))
        end do

        do i = 1, n_ax
            r = ax(i)%inset_of
            if (r < 1) cycle
            ax(i)%left = ax(r)%left + ax(i)%inset_rect(1)*(ax(r)%right - ax(r)%left)
            ax(i)%bottom = ax(r)%bottom + ax(i)%inset_rect(2)*(ax(r)%top - ax(r)%bottom)
            ax(i)%right = ax(i)%left + ax(i)%inset_rect(3)*(ax(r)%right - ax(r)%left)
            ax(i)%top = ax(i)%bottom + ax(i)%inset_rect(4)*(ax(r)%top - ax(r)%bottom)
        end do

        do i = 1, n_ax
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
    ! matplotlib's constrained_layout. Ours is the tight_layout fit, run at
    ! draw time rather than as a solve, which comes to the same thing for
    ! the plain grids a figure usually has.
    subroutine constrained_layout(on)
        logical, intent(in), optional :: on
        call ensure_fig()
        fig_constrained = .true.
        if (present(on)) fig_constrained = on
    end subroutine constrained_layout

    subroutine tight_layout(pad)
        real(dp), intent(in), optional :: pad
        real(dp) :: p, W, H, need_l, need_b, need_t, need_r, inner_l, inner_b
        real(dp) :: inner_t, gp
        integer :: i

        call ensure_fig()
        p = 1.08_dp * TICK_FONT
        ! constrained_layout pads by a flat 1/24 inch instead.
        if (fig_constrained) p = PT_PER_IN / 24.0_dp
        if (present(pad)) p = pad * TICK_FONT
        W = fig_w_in * PT_PER_IN
        H = fig_h_in * PT_PER_IN
        need_l = 0.0_dp
        need_b = 0.0_dp
        need_t = 0.0_dp
        need_r = 0.0_dp
        inner_l = 0.0_dp
        inner_b = 0.0_dp
        inner_t = 0.0_dp
        do i = 1, n_ax
            ! An axes the caller placed itself is not the grid's business.
            if (ax(i)%fixed_pos .or. ax(i)%inset_of > 0) cycle
            need_l = max(need_l, decor_left(ax(i)))
            need_b = max(need_b, decor_bottom(ax(i), W, H))
            need_t = max(need_t, decor_top(ax(i)))
            ! Only axes away from the left column and the bottom row put
            ! decorations into the gaps between subplots.
            if (ax(i)%g_col > 0) inner_l = max(inner_l, decor_left(ax(i)))
            if (ax(i)%g_row + ax(i)%g_rowspan < grid_m) &
                inner_b = max(inner_b, decor_bottom(ax(i), W, H))
            ! and the row below puts its title into the same gap.
            if (ax(i)%g_row > 0) inner_t = max(inner_t, decor_top(ax(i)))
        end do
        if (len_trim(fig_suptitle) > 0) need_t = need_t + LABEL_BOX * fig_suptitle_size

        fig_left = (p + need_l) / W
        fig_right = 1.0_dp - (p + need_r) / W
        fig_bottom = (p + need_b) / H
        fig_top = 1.0_dp - (p + need_t) / H

        ! Inner subplots carry the same decorations, so the gaps between them
        ! have to hold those decorations and the same pad again.
        ! constrained_layout pads both sides of a gap; tight_layout the one.
        gp = p
        if (fig_constrained) gp = 2.0_dp * p
        fig_wspace = spacing_for((gp + inner_l) / W, fig_right - fig_left, grid_n)
        fig_hspace = spacing_for((gp + inner_b + inner_t) / H, fig_top - fig_bottom, grid_m)
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
        ! The label is drawn a fixed distance out, so with narrow tick
        ! labels it reaches further than they do and it is what decides
        ! how much room the axes needs on its left.
        if (len_trim(a%ylabel) > 0) &
            v = ylabel_out(a) + 0.24_dp * a%ylabel_size
    end function decor_left

    ! Baseline of the y label, in points outside the axes. matplotlib
    ! hangs it off the tick labels, so a plot whose ticks read 1 to 7
    ! carries its label much closer in than one reading -1.00 to 1.00.
    function ylabel_out(a) result(v)
        type(axes_t), intent(in) :: a
        real(dp) :: v
        v = TICK_LEN + 2.0_dp + tick_label_width(a) + a%ylabel_pad &
            + 0.76_dp * a%ylabel_size
    end function ylabel_out

    function decor_bottom(a, W, H) result(v)
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: W, H
        real(dp) :: v, bx, by, bw, bh
        v = TICK_LEN + 1.0_dp + 1.15_dp * a%xtick_size
        ! A horizontal colorbar hangs below the axes, with tick labels of
        ! its own under that.
        if (a%cbar_on .and. a%cbar_horiz) then
            call cbar_box(a, W, H, bx, by, bw, bh)
            v = v + by + bh - (1.0_dp - a%bottom) * H &
                + xtick_gap(a) + 0.4_dp * a%xtick_size
            if (len_trim(a%cbar_label) > 0) v = v + LABEL_BOX * a%xlabel_size
        end if
        if (a%xtick_rot /= 0.0_dp) v = v + tick_label_width(a) * &
                                        abs(sin(a%xtick_rot * PI / 180.0_dp))
        if (len_trim(a%xlabel) > 0) &
            v = v + LABEL_BOX * a%xlabel_size + a%xlabel_pad - LABEL_PAD
    end function decor_bottom

    ! How far the decorations reach to the right of the axes box. Only a
    ! colorbar and its labels live out there.
    function decor_right(a, W, H) result(v)
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: W, H
        real(dp) :: v, bx, by, bw, bh
        v = 0.0_dp
        if (.not. a%cbar_on) return
        if (a%cbar_horiz) return
        call cbar_box(a, W, H, bx, by, bw, bh)
        v = bx + bw - a%right * W + 7.0_dp
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
            r = ax(i)%right * W + decor_right(ax(i), W, H)
            t = (1.0_dp - ax(i)%top) * H - decor_top(ax(i))
            bt = (1.0_dp - ax(i)%bottom) * H + decor_bottom(ax(i), W, H)
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
        integer :: nt, i, ln, du
        v = 0.0_dp
        call compute_limits(a, xmin, xmax, ymin, ymax)
        call axis_ticks(a%n_yticks, a%ytick_pos, min(ymin, ymax), max(ymin, ymax), &
                        a%ysc, nbins_for(a%ytick_nbins, 0.0_dp, a%ytick_size, .false.), &
                        a%y_date, t, nt, du, a%ytick_base)
        do i = 1, nt
            call tick_label(a%ytick_labeled, a%ytick_lab, i, t(i), a%ysc, &
                            axis_fmt(t, nt, a, .false., min(ymin, ymax), &
                                     max(ymin, ymax)), du, lbl, ln)
            if (math_is(lbl(1:ln))) then
                v = max(v, math_width(lbl(1:ln), a%ytick_size))
            else
                v = max(v, real(ln, dp) * DIGIT_W * a%ytick_size)
            end if
        end do
    end function tick_label_width


    ! Select the i-th axes (row-major) of an m x n grid, creating the grid
    ! if it differs from the current one.
    ! Treat an axis as dates: its numbers are days since 1970-01-01, the
    ! ticks land on round dates and the labels are written as dates.
    subroutine xaxis_date()
        call ensure_fig()
        ax(cur_i)%x_date = .true.
    end subroutine xaxis_date

    subroutine yaxis_date()
        call ensure_fig()
        ax(cur_i)%y_date = .true.
    end subroutine yaxis_date

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

    ! One axes spanning several cells of a shape(1) x shape(2) grid, with
    ! its top left corner at loc, counted from zero as matplotlib counts
    ! it. Cells nobody asks for stay empty, which is how a wide panel over
    ! two narrow ones is built.
    ! Grow the axes array by one and make the new axes current.
    subroutine push_axes()
        type(axes_t), allocatable :: tmp(:)

        call ensure_fig()
        if (n_ax > 0) then
            call move_alloc(ax, tmp)
            allocate (ax(n_ax + 1))
            ax(1:n_ax) = tmp
        else
            if (allocated(ax)) deallocate (ax)
            allocate (ax(1))
        end if
        n_ax = n_ax + 1
        cur_i = n_ax
        call apply_font_defaults(ax(cur_i))
    end subroutine push_axes

    ! An axes at a rectangle of the figure the caller chose, [left, bottom,
    ! width, height] in figure fractions. The grid never moves it.
    function add_axes(rect) result(h)
        real(dp), intent(in) :: rect(4)
        type(axes) :: h

        call push_axes()
        ax(cur_i)%fixed_pos = .true.
        ax(cur_i)%left = rect(1)
        ax(cur_i)%bottom = rect(2)
        ax(cur_i)%right = rect(1) + rect(3)
        ax(cur_i)%top = rect(2) + rect(4)
        h%idx = cur_i
    end function add_axes

    ! A small axes inside another one, its rectangle given in the fractions
    ! of that axes, so it goes on fitting when the layout moves.
    function ax_inset_axes(self, bounds) result(h)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: bounds(4)
        type(axes) :: h
        integer :: parent

        parent = self%idx
        call push_axes()
        ax(cur_i)%inset_of = parent
        ax(cur_i)%inset_rect = bounds
        call layout_grid()
        h%idx = cur_i
    end function ax_inset_axes

    function ax_secondary_xaxis(self, location, scale, offset) result(h)
        class(axes), intent(in) :: self
        character(len=*), intent(in), optional :: location
        real(dp), intent(in), optional :: scale, offset
        type(axes) :: h
        h = add_secondary(self%idx, .true., location, scale, offset)
    end function ax_secondary_xaxis

    function ax_secondary_yaxis(self, location, scale, offset) result(h)
        class(axes), intent(in) :: self
        character(len=*), intent(in), optional :: location
        real(dp), intent(in), optional :: scale, offset
        type(axes) :: h
        h = add_secondary(self%idx, .false., location, scale, offset)
    end function ax_secondary_yaxis

    function secondary_xaxis(location, scale, offset) result(h)
        character(len=*), intent(in), optional :: location
        real(dp), intent(in), optional :: scale, offset
        type(axes) :: h
        call ensure_fig()
        h = add_secondary(cur_i, .true., location, scale, offset)
    end function secondary_xaxis

    function secondary_yaxis(location, scale, offset) result(h)
        character(len=*), intent(in), optional :: location
        real(dp), intent(in), optional :: scale, offset
        type(axes) :: h
        call ensure_fig()
        h = add_secondary(cur_i, .false., location, scale, offset)
    end function secondary_yaxis

    ! A second axis along one edge, reading the same data in other units.
    ! matplotlib takes a pair of functions; here the two units are related
    ! by scale and offset, which is what a change of units amounts to.
    function add_secondary(parent, is_x, location, scale, offset) result(h)
        integer, intent(in) :: parent
        logical, intent(in) :: is_x
        character(len=*), intent(in), optional :: location
        real(dp), intent(in), optional :: scale, offset
        type(axes) :: h
        character(len=8) :: loc

        loc = "top"
        if (.not. is_x) loc = "right"
        if (present(location)) loc = location

        call push_axes()
        ! Sitting on the whole of its parent keeps the two axes registered
        ! against each other however the layout moves afterwards.
        ax(cur_i)%inset_of = parent
        ax(cur_i)%inset_rect = [0.0_dp, 0.0_dp, 1.0_dp, 1.0_dp]
        ax(cur_i)%patch_off = .true.
        ax(cur_i)%sec_of = parent
        ax(cur_i)%sec_is_x = is_x
        if (present(scale)) ax(cur_i)%sec_scale = scale
        if (present(offset)) ax(cur_i)%sec_offset = offset
        if (is_x) then
            ax(cur_i)%yaxis_off = .true.
            ax(cur_i)%x_top = loc /= "bottom"
            ax(cur_i)%xsc = ax(parent)%xsc
        else
            ax(cur_i)%xaxis_off = .true.
            ax(cur_i)%y_right = loc /= "left"
            ax(cur_i)%ysc = ax(parent)%ysc
        end if
        call layout_grid()
        h%idx = cur_i
    end function add_secondary

    ! GridSpec's width_ratios and height_ratios: the columns and rows of the
    ! grid need not be equal. Call it before the subplots are made; the
    ! ratios stay with the figure and the panels are laid out to them.
    subroutine gridspec(width_ratios, height_ratios)
        real(dp), intent(in), optional :: width_ratios(:), height_ratios(:)
        integer :: k

        call ensure_fig()
        if (present(width_ratios)) then
            fig_wratio = 1.0_dp
            do k = 1, min(size(width_ratios), MAX_RATIO)
                fig_wratio(k) = width_ratios(k)
            end do
        end if
        if (present(height_ratios)) then
            fig_hratio = 1.0_dp
            do k = 1, min(size(height_ratios), MAX_RATIO)
                fig_hratio(k) = height_ratios(k)
            end do
        end if
    end subroutine gridspec

    function subplot2grid(shape, loc, rowspan, colspan) result(h)
        integer, intent(in) :: shape(2), loc(2)
        integer, intent(in), optional :: rowspan, colspan
        type(axes) :: h
        type(axes_t), allocatable :: tmp(:)
        integer :: rs, cs

        rs = 1
        cs = 1
        if (present(rowspan)) rs = max(1, rowspan)
        if (present(colspan)) cs = max(1, colspan)
        if (shape(1) < 1 .or. shape(2) < 1) then
            print *, "fplot: invalid subplot2grid shape:", shape
            error stop
        end if

        call ensure_fig()
        ! A grid of a different shape, or one that subplot filled in for
        ! us, is not ours to add to.
        if (.not. grid_sparse .or. grid_m /= shape(1) .or. grid_n /= shape(2)) then
            if (allocated(ax)) deallocate (ax)
            n_ax = 0
            grid_m = shape(1)
            grid_n = shape(2)
            grid_sparse = .true.
        end if

        if (n_ax > 0) then
            call move_alloc(ax, tmp)
            allocate (ax(n_ax + 1))
            ax(1:n_ax) = tmp
        else
            allocate (ax(1))
        end if
        n_ax = n_ax + 1
        cur_i = n_ax
        call apply_font_defaults(ax(cur_i))
        ax(cur_i)%g_row = max(0, min(loc(1), grid_m - 1))
        ax(cur_i)%g_col = max(0, min(loc(2), grid_n - 1))
        ax(cur_i)%g_rowspan = min(rs, grid_m - ax(cur_i)%g_row)
        ax(cur_i)%g_colspan = min(cs, grid_n - ax(cur_i)%g_col)
        call layout_grid()
        h%idx = cur_i
    end function subplot2grid

    ! matplotlib's subplot_mosaic, drawn as a picture of the figure: one
    ! string per row of the grid, one character per cell, and a panel for
    ! every distinct character, spanning the cells that carry it. A space
    ! or a full stop leaves the cell empty.
    !
    ! keys comes back holding the characters in the order they were first
    ! met, so axs(k) is the panel named keys(k:k).
    subroutine subplot_mosaic(rows, keys, axs)
        character(len=*), intent(in) :: rows(:)
        character(len=*), intent(out) :: keys
        type(axes), intent(out) :: axs(:)
        integer :: nr, nc, i, j, k, m, nk, r0, r1, c0, c1
        character(len=1) :: c

        call ensure_fig()
        call clf()
        nr = size(rows)
        nc = 0
        do i = 1, nr
            nc = max(nc, len_trim(rows(i)))
        end do
        keys = ""
        nk = 0
        if (nr < 1 .or. nc < 1) return

        do i = 1, nr
            do j = 1, min(nc, len(rows(i)))
                c = rows(i)(j:j)
                if (c == " " .or. c == ".") cycle
                if (index(keys, c) > 0) cycle
                nk = nk + 1
                if (nk > len(keys) .or. nk > size(axs)) return
                keys(nk:nk) = c

                ! The panel covers the block of cells carrying this
                ! character, which matplotlib requires to be a rectangle.
                r0 = nr
                r1 = 1
                c0 = nc
                c1 = 1
                do k = 1, nr
                    do m = 1, min(nc, len(rows(k)))
                        if (rows(k)(m:m) /= c) cycle
                        r0 = min(r0, k)
                        r1 = max(r1, k)
                        c0 = min(c0, m)
                        c1 = max(c1, m)
                    end do
                end do
                axs(nk) = subplot2grid([nr, nc], [r0 - 1, c0 - 1], &
                                       rowspan=r1 - r0 + 1, colspan=c1 - c0 + 1)
            end do
        end do
    end subroutine subplot_mosaic

    ! ----------------------------------------------------------------------
    ! Axes handles. subplots makes a new figure, as matplotlib's does, and
    ! hands back one handle per panel.
    ! ----------------------------------------------------------------------

    subroutine subplots_grid(nrows, ncols, axs, sharex, sharey)
        integer, intent(in) :: nrows, ncols
        type(axes), allocatable, intent(out) :: axs(:, :)
        logical, intent(in), optional :: sharex, sharey
        integer :: r, c

        call make_grid(nrows, ncols, sharex, sharey)
        allocate (axs(nrows, ncols))
        do r = 1, nrows
            do c = 1, ncols
                axs(r, c)%idx = (r - 1)*ncols + c
            end do
        end do
    end subroutine subplots_grid

    subroutine subplots_row(nrows, ncols, axs, sharex, sharey)
        integer, intent(in) :: nrows, ncols
        type(axes), allocatable, intent(out) :: axs(:)
        logical, intent(in), optional :: sharex, sharey
        integer :: i

        call make_grid(nrows, ncols, sharex, sharey)
        allocate (axs(nrows*ncols))
        do i = 1, nrows*ncols
            axs(i)%idx = i
        end do
    end subroutine subplots_row

    subroutine subplots_one(ax_out)
        type(axes), intent(out) :: ax_out

        call make_grid(1, 1)
        ax_out%idx = 1
    end subroutine subplots_one

    ! Sharing is expressed on the axes themselves: a group is named after its
    ! first member, and only the outer panels keep their tick labels, which
    ! is most of why anyone asks for it.
    subroutine make_grid(m, n, sharex, sharey)
        integer, intent(in) :: m, n
        logical, intent(in), optional :: sharex, sharey
        integer :: i, r, c
        logical :: sx, sy

        sx = .false.
        sy = .false.
        if (present(sharex)) sx = sharex
        if (present(sharey)) sy = sharey

        call figure()
        if (m /= 1 .or. n /= 1) call new_axes_grid(m, n)
        cur_i = 1

        do i = 1, n_ax
            r = (i - 1)/n + 1
            c = i - (r - 1)*n
            if (sx) then
                ax(i)%link_x = 1
                ax(i)%xticklabels_off = r < m
            end if
            if (sy) then
                ax(i)%link_y = 1
                ax(i)%yticklabels_off = c > 1
            end if
        end do
    end subroutine make_grid

    ! Make an axes current: the module-level spelling of matplotlib's sca,
    ! and the way to reach anything that has no binding on the handle.
    subroutine sca(a)
        type(axes), intent(in) :: a

        call ensure_fig()
        if (a%idx >= 1 .and. a%idx <= n_ax) cur_i = a%idx
    end subroutine sca

    subroutine ax_sca(self)
        class(axes), intent(in) :: self

        call ensure_fig()
        if (self%idx >= 1 .and. self%idx <= n_ax) cur_i = self%idx
    end subroutine ax_sca

    subroutine ax_plot(self, x, y, fmt, label, lw, color, marker, linestyle, &
                       alpha, markersize, markerfacecolor, markeredgecolor, &
                       markeredgewidth, markevery, drawstyle, dashes)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: fmt, label, color, marker, linestyle
        character(len=*), intent(in), optional :: markerfacecolor, markeredgecolor
        character(len=*), intent(in), optional :: drawstyle
        real(dp), intent(in), optional :: lw, alpha, markersize, markeredgewidth
        real(dp), intent(in), optional :: dashes(:)
        integer, intent(in), optional :: markevery
        call ax_sca(self)
        call plot(x, y, fmt, label, lw, color, marker, linestyle, alpha, &
                  markersize, markerfacecolor, markeredgecolor, &
                  markeredgewidth, markevery, drawstyle, dashes)
    end subroutine ax_plot

    subroutine ax_plot_y(self, y, fmt, label, lw, color, marker, linestyle, &
                         alpha, markersize, markerfacecolor, markeredgecolor, &
                         markeredgewidth, markevery, drawstyle, dashes)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: y(:)
        character(len=*), intent(in), optional :: fmt, label, color, marker, linestyle
        character(len=*), intent(in), optional :: markerfacecolor, markeredgecolor
        character(len=*), intent(in), optional :: drawstyle
        real(dp), intent(in), optional :: lw, alpha, markersize, markeredgewidth
        real(dp), intent(in), optional :: dashes(:)
        integer, intent(in), optional :: markevery
        call ax_sca(self)
        call plot(y, fmt, label, lw, color, marker, linestyle, alpha, &
                  markersize, markerfacecolor, markeredgecolor, &
                  markeredgewidth, markevery, drawstyle, dashes)
    end subroutine ax_plot_y

    subroutine ax_plot_cat(self, cats, y, fmt, label, lw, color, marker, &
                           linestyle, alpha)
        class(axes), intent(in) :: self
        character(len=*), intent(in) :: cats(:)
        real(dp), intent(in) :: y(:)
        character(len=*), intent(in), optional :: fmt, label, color, marker, linestyle
        real(dp), intent(in), optional :: lw, alpha
        call ax_sca(self)
        call plot(cats, y, fmt, label, lw, color, marker, linestyle, alpha)
    end subroutine ax_plot_cat

    subroutine ax_bar_cat(self, cats, height, width, color, label, alpha, &
                          bottom, colors, edgecolor, linewidth)
        class(axes), intent(in) :: self
        character(len=*), intent(in) :: cats(:)
        real(dp), intent(in) :: height(:)
        real(dp), intent(in), optional :: width, alpha, bottom(:), linewidth
        character(len=*), intent(in), optional :: color, label, edgecolor
        character(len=*), intent(in), optional :: colors(:)
        call ax_sca(self)
        call bar(cats, height, width, color, label, alpha, bottom, colors, &
                 edgecolor, linewidth)
    end subroutine ax_bar_cat

    subroutine ax_barh_cat(self, cats, width, height, color, label, alpha, &
                           left, colors, edgecolor, linewidth)
        class(axes), intent(in) :: self
        character(len=*), intent(in) :: cats(:)
        real(dp), intent(in) :: width(:)
        real(dp), intent(in), optional :: height, alpha, left(:), linewidth
        character(len=*), intent(in), optional :: color, label, edgecolor
        character(len=*), intent(in), optional :: colors(:)
        call ax_sca(self)
        call barh(cats, width, height, color, label, alpha, left, colors, &
                  edgecolor, linewidth)
    end subroutine ax_barh_cat

    subroutine ax_scatter(self, x, y, s, c, marker, label, alpha, sizes, cvals, &
                          cmap, vmin, vmax)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:)
        real(dp), intent(in), optional :: s, alpha, vmin, vmax
        real(dp), intent(in), optional :: sizes(:), cvals(:)
        character(len=*), intent(in), optional :: c, marker, label, cmap
        call ax_sca(self)
        call scatter(x, y, s, c, marker, label, alpha, sizes, cvals, cmap, vmin, vmax)
    end subroutine ax_scatter

    subroutine ax_bar(self, x, height, width, color, label, alpha, bottom, &
                      colors, edgecolor, linewidth, hatch, yerr, align, &
                      tick_label, ecolor, capsize)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), height(:)
        real(dp), intent(in), optional :: width, alpha, bottom(:), linewidth
        real(dp), intent(in), optional :: yerr(:), capsize
        character(len=*), intent(in), optional :: color, label, edgecolor, hatch
        character(len=*), intent(in), optional :: colors(:), align, ecolor
        character(len=*), intent(in), optional :: tick_label(:)
        call ax_sca(self)
        call bar(x, height, width, color, label, alpha, bottom, colors, &
                 edgecolor, linewidth, hatch, yerr, align, tick_label, &
                 ecolor, capsize)
    end subroutine ax_bar

    subroutine ax_barh(self, y, width, height, color, label, alpha, left, &
                       colors, edgecolor, linewidth)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: y(:), width(:)
        real(dp), intent(in), optional :: height, alpha, left(:), linewidth
        character(len=*), intent(in), optional :: color, label, edgecolor
        character(len=*), intent(in), optional :: colors(:)
        call ax_sca(self)
        call barh(y, width, height, color, label, alpha, left, colors, &
                  edgecolor, linewidth)
    end subroutine ax_barh

    subroutine ax_bar_label(self, fmt, padding, fontsize)
        class(axes), intent(in) :: self
        character(len=*), intent(in), optional :: fmt
        real(dp), intent(in), optional :: padding, fontsize
        call ax_sca(self)
        call bar_label(fmt, padding, fontsize)
    end subroutine ax_bar_label

    subroutine ax_hist(self, x, bins, color, label, alpha, bin_edges, &
                       density, cumulative, histtype, weights, stacked, &
                       orientation, log, rwidth)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:)
        integer, intent(in), optional :: bins
        character(len=*), intent(in), optional :: color, label, histtype
        character(len=*), intent(in), optional :: orientation
        real(dp), intent(in), optional :: alpha, bin_edges(:), weights(:), rwidth
        logical, intent(in), optional :: density, cumulative, stacked, log
        call ax_sca(self)
        call hist(x, bins, color, label, alpha, bin_edges, density, &
                  cumulative, histtype, weights, stacked, orientation, &
                  log, rwidth)
    end subroutine ax_hist

    subroutine ax_fill_between(self, x, y1, y2, color, label, alpha, where, &
                               hatch, edgecolor)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y1(:)
        real(dp), intent(in), optional :: y2(:)
        character(len=*), intent(in), optional :: color, label, hatch, edgecolor
        real(dp), intent(in), optional :: alpha
        logical, intent(in), optional :: where(:)
        call ax_sca(self)
        call fill_between(x, y1, y2, color, label, alpha, where, hatch, edgecolor)
    end subroutine ax_fill_between

    subroutine ax_fill_betweenx(self, y, x1, x2, color, label, alpha, where, &
                                hatch, edgecolor)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: y(:), x1(:)
        real(dp), intent(in), optional :: x2(:)
        character(len=*), intent(in), optional :: color, label, hatch, edgecolor
        real(dp), intent(in), optional :: alpha
        logical, intent(in), optional :: where(:)
        call ax_sca(self)
        call fill_betweenx(y, x1, x2, color, label, alpha, where, hatch, edgecolor)
    end subroutine ax_fill_betweenx

    subroutine ax_stackplot(self, x, y, labels, colors, alpha)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:, :)
        character(len=*), intent(in), optional :: labels(:), colors(:)
        real(dp), intent(in), optional :: alpha
        call ax_sca(self)
        call stackplot(x, y, labels, colors, alpha)
    end subroutine ax_stackplot

    subroutine ax_errorbar(self, x, y, yerr, fmt, color, label, capsize, &
                           marker, xerr, yerr_lo, yerr_hi, xerr_lo, xerr_hi)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:)
        real(dp), intent(in), optional :: yerr(:), xerr(:)
        real(dp), intent(in), optional :: yerr_lo(:), yerr_hi(:)
        real(dp), intent(in), optional :: xerr_lo(:), xerr_hi(:)
        character(len=*), intent(in), optional :: fmt, color, label, marker
        real(dp), intent(in), optional :: capsize
        call ax_sca(self)
        call errorbar(x, y, yerr, fmt, color, label, capsize, marker, xerr, &
                      yerr_lo, yerr_hi, xerr_lo, xerr_hi)
    end subroutine ax_errorbar

    subroutine ax_step(self, x, y, where, label, color, lw, linestyle, alpha)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: where, label, color, linestyle
        real(dp), intent(in), optional :: lw, alpha
        call ax_sca(self)
        call step(x, y, where, label, color, lw, linestyle, alpha)
    end subroutine ax_step

    subroutine ax_set_polar(self)
        class(axes), intent(in) :: self
        call ax_sca(self)
        call set_polar()
    end subroutine ax_set_polar

    subroutine ax_add_rectangle(self, xy, width, height, angle, facecolor, &
                                edgecolor, lw, alpha, fill, hatch)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: xy(2), width, height
        real(dp), intent(in), optional :: angle, lw, alpha
        character(len=*), intent(in), optional :: facecolor, edgecolor, hatch
        logical, intent(in), optional :: fill
        call ax_sca(self)
        call add_rectangle(xy, width, height, angle, facecolor, edgecolor, lw, &
                           alpha, fill, hatch)
    end subroutine ax_add_rectangle

    subroutine ax_add_circle(self, xy, radius, facecolor, edgecolor, lw, alpha, fill)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: xy(2), radius
        real(dp), intent(in), optional :: lw, alpha
        character(len=*), intent(in), optional :: facecolor, edgecolor
        logical, intent(in), optional :: fill
        call ax_sca(self)
        call add_circle(xy, radius, facecolor, edgecolor, lw, alpha, fill)
    end subroutine ax_add_circle

    subroutine ax_add_ellipse(self, xy, width, height, angle, facecolor, &
                              edgecolor, lw, alpha, fill)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: xy(2), width, height
        real(dp), intent(in), optional :: angle, lw, alpha
        character(len=*), intent(in), optional :: facecolor, edgecolor
        logical, intent(in), optional :: fill
        call ax_sca(self)
        call add_ellipse(xy, width, height, angle, facecolor, edgecolor, lw, alpha, fill)
    end subroutine ax_add_ellipse

    subroutine ax_add_polygon(self, x, y, facecolor, edgecolor, lw, alpha, fill, hatch)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:)
        real(dp), intent(in), optional :: lw, alpha
        character(len=*), intent(in), optional :: facecolor, edgecolor, hatch
        logical, intent(in), optional :: fill
        call ax_sca(self)
        call add_polygon(x, y, facecolor, edgecolor, lw, alpha, fill, hatch)
    end subroutine ax_add_polygon

    subroutine ax_set_zorder(self, z)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: z
        call ax_sca(self)
        call set_zorder(z)
    end subroutine ax_set_zorder

    subroutine ax_axes3d(self, elev, azim)
        class(axes), intent(in) :: self
        real(dp), intent(in), optional :: elev, azim
        call ax_sca(self)
        call axes3d(elev, azim)
    end subroutine ax_axes3d

    subroutine ax_boxplot(self, y, position, width, color, label)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: y(:)
        real(dp), intent(in), optional :: position, width
        character(len=*), intent(in), optional :: color, label
        call ax_sca(self)
        call boxplot(y, position, width, color, label)
    end subroutine ax_boxplot

    subroutine ax_violinplot(self, y, position, width, color, label)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: y(:)
        real(dp), intent(in), optional :: position, width
        character(len=*), intent(in), optional :: color, label
        call ax_sca(self)
        call violinplot(y, position, width, color, label)
    end subroutine ax_violinplot

    subroutine ax_broken_barh(self, xranges, yrange, color, alpha, edgecolor, lw)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: xranges(:, :), yrange(2)
        character(len=*), intent(in), optional :: color, edgecolor
        real(dp), intent(in), optional :: alpha, lw
        call ax_sca(self)
        call broken_barh(xranges, yrange, color, alpha, edgecolor, lw)
    end subroutine ax_broken_barh

    subroutine ax_eventplot(self, positions, lineoffset, linelength, color, lw)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: positions(:)
        real(dp), intent(in), optional :: lineoffset, linelength, lw
        character(len=*), intent(in), optional :: color
        call ax_sca(self)
        call eventplot(positions, lineoffset, linelength, color, lw)
    end subroutine ax_eventplot

    subroutine ax_hexbin(self, x, y, gridsize, cmap, mincnt)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:)
        integer, intent(in), optional :: gridsize, mincnt
        character(len=*), intent(in), optional :: cmap
        call ax_sca(self)
        call hexbin(x, y, gridsize, cmap, mincnt)
    end subroutine ax_hexbin

    subroutine ax_hist2d(self, x, y, bins, cmap, vmin, vmax)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:)
        integer, intent(in), optional :: bins(2)
        character(len=*), intent(in), optional :: cmap
        real(dp), intent(in), optional :: vmin, vmax
        call ax_sca(self)
        call hist2d(x, y, bins, cmap, vmin, vmax)
    end subroutine ax_hist2d

    subroutine ax_loglog(self, x, y, fmt, label, lw, color)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: fmt, label, color
        real(dp), intent(in), optional :: lw
        call ax_sca(self)
        call loglog(x, y, fmt, label, lw, color)
    end subroutine ax_loglog

    subroutine ax_semilogx(self, x, y, fmt, label, lw, color)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: fmt, label, color
        real(dp), intent(in), optional :: lw
        call ax_sca(self)
        call semilogx(x, y, fmt, label, lw, color)
    end subroutine ax_semilogx

    subroutine ax_semilogy(self, x, y, fmt, label, lw, color)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: fmt, label, color
        real(dp), intent(in), optional :: lw
        call ax_sca(self)
        call semilogy(x, y, fmt, label, lw, color)
    end subroutine ax_semilogy

    subroutine ax_matshow(self, z, cmap, vmin, vmax)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: z(:, :)
        character(len=*), intent(in), optional :: cmap
        real(dp), intent(in), optional :: vmin, vmax
        call ax_sca(self)
        call matshow(z, cmap, vmin, vmax)
    end subroutine ax_matshow

    subroutine ax_minorticks_on(self)
        class(axes), intent(in) :: self
        call ax_sca(self)
        call minorticks_on()
    end subroutine ax_minorticks_on

    subroutine ax_pie(self, values, labels, cmap)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: values(:)
        character(len=*), intent(in), optional :: labels(:), cmap
        call ax_sca(self)
        call pie(values, labels, cmap)
    end subroutine ax_pie

    subroutine ax_polar(self, theta, r, color, label, lw, linestyle, marker, alpha)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: theta(:), r(:)
        character(len=*), intent(in), optional :: color, label, linestyle, marker
        real(dp), intent(in), optional :: lw, alpha
        call ax_sca(self)
        call polar(theta, r, color, label, lw, linestyle, marker, alpha)
    end subroutine ax_polar

    subroutine ax_streamplot(self, x, y, u, v, density, color, lw, arrowsize)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:), u(:, :), v(:, :)
        real(dp), intent(in), optional :: density, lw, arrowsize
        character(len=*), intent(in), optional :: color
        call ax_sca(self)
        call streamplot(x, y, u, v, density, color, lw, arrowsize)
    end subroutine ax_streamplot

    subroutine ax_table(self, cell_text, col_labels, row_labels, col_widths, loc, fontsize)
        class(axes), intent(in) :: self
        character(len=*), intent(in) :: cell_text(:, :)
        character(len=*), intent(in), optional :: col_labels(:), row_labels(:), loc
        real(dp), intent(in), optional :: col_widths(:), fontsize
        call ax_sca(self)
        call table(cell_text, col_labels, row_labels, col_widths, loc, fontsize)
    end subroutine ax_table

    subroutine ax_tick_format(self, axis, style, decimals, whole)
        class(axes), intent(in) :: self
        character(len=*), intent(in) :: style
        character(len=*), intent(in), optional :: axis
        integer, intent(in), optional :: decimals
        real(dp), intent(in), optional :: whole
        call ax_sca(self)
        call tick_format(axis, style, decimals, whole)
    end subroutine ax_tick_format

    subroutine ax_tick_locator(self, axis, base, nbins)
        class(axes), intent(in) :: self
        character(len=*), intent(in), optional :: axis
        real(dp), intent(in), optional :: base
        integer, intent(in), optional :: nbins
        call ax_sca(self)
        call tick_locator(axis, base, nbins)
    end subroutine ax_tick_locator

    subroutine ax_ticklabel_format(self, axis, style, useoffset, scilimits)
        class(axes), intent(in) :: self
        character(len=*), intent(in), optional :: axis, style
        logical, intent(in), optional :: useoffset
        integer, intent(in), optional :: scilimits(2)
        call ax_sca(self)
        call ticklabel_format(axis, style, useoffset, scilimits)
    end subroutine ax_ticklabel_format

    subroutine ax_add_arrow(self, x, y, dx, dy, width, facecolor, edgecolor, lw, alpha, fill)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x, y, dx, dy
        real(dp), intent(in), optional :: width, lw, alpha
        character(len=*), intent(in), optional :: facecolor, edgecolor
        logical, intent(in), optional :: fill
        call ax_sca(self)
        call add_arrow(x, y, dx, dy, width, facecolor, edgecolor, lw, alpha, fill)
    end subroutine ax_add_arrow

    subroutine ax_add_path(self, x, y, codes, facecolor, edgecolor, lw, alpha, fill)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in) :: codes
        real(dp), intent(in), optional :: lw, alpha
        character(len=*), intent(in), optional :: facecolor, edgecolor
        logical, intent(in), optional :: fill
        call ax_sca(self)
        call add_path(x, y, codes, facecolor, edgecolor, lw, alpha, fill)
    end subroutine ax_add_path

    subroutine ax_plot3d(self, x, y, z, fmt, label, lw, color, marker, linestyle, alpha)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:), z(:)
        character(len=*), intent(in), optional :: fmt, label, color, marker, linestyle
        real(dp), intent(in), optional :: lw, alpha
        call ax_sca(self)
        call plot3d(x, y, z, fmt, label, lw, color, marker, linestyle, alpha)
    end subroutine ax_plot3d

    subroutine ax_scatter3d(self, x, y, z, s, c, marker, label, alpha)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:), z(:)
        real(dp), intent(in), optional :: s, alpha
        character(len=*), intent(in), optional :: c, marker, label
        call ax_sca(self)
        call scatter3d(x, y, z, s, c, marker, label, alpha)
    end subroutine ax_scatter3d

    subroutine ax_plot_wireframe(self, x, y, z, color, alpha, lw)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:), z(:, :)
        character(len=*), intent(in), optional :: color
        real(dp), intent(in), optional :: alpha, lw
        call ax_sca(self)
        call plot_wireframe(x, y, z, color, alpha, lw)
    end subroutine ax_plot_wireframe

    subroutine ax_plot_surface(self, x, y, z, color, alpha, cmap)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:), z(:, :)
        character(len=*), intent(in), optional :: color, cmap
        real(dp), intent(in), optional :: alpha
        call ax_sca(self)
        call plot_surface(x, y, z, color, alpha, cmap)
    end subroutine ax_plot_surface

    subroutine ax_view_init(self, elev, azim)
        class(axes), intent(in) :: self
        real(dp), intent(in), optional :: elev, azim
        call ax_sca(self)
        call view_init(elev, azim)
    end subroutine ax_view_init

    subroutine ax_set_zlabel(self, s)
        class(axes), intent(in) :: self
        character(len=*), intent(in) :: s
        call ax_sca(self)
        call zlabel(s)
    end subroutine ax_set_zlabel

    subroutine ax_set_zlim(self, lo, hi)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: lo, hi
        call ax_sca(self)
        call zlim(lo, hi)
    end subroutine ax_set_zlim
    subroutine ax_quiver(self, x, y, u, v, color, scale, width, label)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:), u(:), v(:)
        character(len=*), intent(in), optional :: color, label
        real(dp), intent(in), optional :: scale, width

        call ax_sca(self)
        call quiver(x, y, u, v, color, scale, width, label)
    end subroutine ax_quiver

    subroutine ax_stem(self, x, y, color, label, alpha)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: color, label
        real(dp), intent(in), optional :: alpha
        call ax_sca(self)
        call stem(x, y, color, label, alpha)
    end subroutine ax_stem

    subroutine ax_imshow(self, z, cmap, vmin, vmax, extent, origin, aspect, &
                         norm, interpolation, boundaries, vcenter, gamma, &
                         linthresh)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: z(:, :)
        character(len=*), intent(in), optional :: cmap, origin, aspect, norm
        character(len=*), intent(in), optional :: interpolation
        real(dp), intent(in), optional :: vmin, vmax, extent(4), boundaries(:)
        real(dp), intent(in), optional :: vcenter, gamma, linthresh
        call ax_sca(self)
        call imshow(z, cmap, vmin, vmax, extent, origin, aspect, norm, &
                    interpolation, boundaries, vcenter, gamma, linthresh)
    end subroutine ax_imshow

    subroutine ax_imshow_rgb(self, z, extent, origin, aspect)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: z(:, :, :)
        character(len=*), intent(in), optional :: origin, aspect
        real(dp), intent(in), optional :: extent(4)
        call ax_sca(self)
        call imshow(z, extent, origin, aspect)
    end subroutine ax_imshow_rgb

    subroutine ax_xaxis_date(self)
        class(axes), intent(in) :: self
        call ax_sca(self)
        call xaxis_date()
    end subroutine ax_xaxis_date

    subroutine ax_yaxis_date(self)
        class(axes), intent(in) :: self
        call ax_sca(self)
        call yaxis_date()
    end subroutine ax_yaxis_date

    subroutine ax_pcolormesh(self, x, y, c, cmap, vmin, vmax, norm, vcenter, &
                             gamma, linthresh)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:), c(:, :)
        character(len=*), intent(in), optional :: cmap, norm
        real(dp), intent(in), optional :: vmin, vmax, vcenter, gamma, linthresh
        call ax_sca(self)
        call pcolormesh(x, y, c, cmap, vmin, vmax, norm, vcenter, gamma, linthresh)
    end subroutine ax_pcolormesh

    subroutine ax_clabel(self, fontsize)
        class(axes), intent(in) :: self
        real(dp), intent(in), optional :: fontsize
        call ax_sca(self)
        call clabel(fontsize)
    end subroutine ax_clabel

    subroutine ax_contour(self, z, levels, cmap, extent)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: z(:, :)
        real(dp), intent(in), optional :: levels(:), extent(4)
        character(len=*), intent(in), optional :: cmap
        call ax_sca(self)
        call contour(z, levels, cmap, extent)
    end subroutine ax_contour

    subroutine ax_contourf(self, z, levels, cmap, extent)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: z(:, :)
        real(dp), intent(in), optional :: levels(:), extent(4)
        character(len=*), intent(in), optional :: cmap
        call ax_sca(self)
        call contourf(z, levels, cmap, extent)
    end subroutine ax_contourf

    subroutine ax_colorbar(self, label, orientation, fraction, pad, shrink, aspect)
        class(axes), intent(in) :: self
        character(len=*), intent(in), optional :: label, orientation
        real(dp), intent(in), optional :: fraction, pad, shrink, aspect
        call ax_sca(self)
        call colorbar(label, orientation, fraction, pad, shrink, aspect)
    end subroutine ax_colorbar

    subroutine ax_margins(self, m, x, y)
        class(axes), intent(in) :: self
        real(dp), intent(in), optional :: m, x, y
        call ax_sca(self)
        call margins(m, x, y)
    end subroutine ax_margins

    subroutine ax_autoscale(self, enable, axis, tight)
        class(axes), intent(in) :: self
        logical, intent(in), optional :: enable, tight
        character(len=*), intent(in), optional :: axis
        call ax_sca(self)
        call autoscale(enable, axis, tight)
    end subroutine ax_autoscale

    subroutine ax_axhline(self, y, color, linestyle, lw, label)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: y
        character(len=*), intent(in), optional :: color, linestyle, label
        real(dp), intent(in), optional :: lw
        call ax_sca(self)
        call axhline(y, color, linestyle, lw, label)
    end subroutine ax_axhline

    subroutine ax_axhspan(self, ymin, ymax, xmin, xmax, color, alpha, label)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: ymin, ymax
        real(dp), intent(in), optional :: xmin, xmax, alpha
        character(len=*), intent(in), optional :: color, label
        call ax_sca(self)
        call axhspan(ymin, ymax, xmin, xmax, color, alpha, label)
    end subroutine ax_axhspan

    subroutine ax_axvspan(self, xmin, xmax, ymin, ymax, color, alpha, label)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: xmin, xmax
        real(dp), intent(in), optional :: ymin, ymax, alpha
        character(len=*), intent(in), optional :: color, label
        call ax_sca(self)
        call axvspan(xmin, xmax, ymin, ymax, color, alpha, label)
    end subroutine ax_axvspan

    subroutine ax_hlines(self, y, xmin, xmax, color, linestyle, lw, label)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: y(:), xmin, xmax
        character(len=*), intent(in), optional :: color, linestyle, label
        real(dp), intent(in), optional :: lw
        call ax_sca(self)
        call hlines(y, xmin, xmax, color, linestyle, lw, label)
    end subroutine ax_hlines

    subroutine ax_vlines(self, x, ymin, ymax, color, linestyle, lw, label)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), ymin, ymax
        character(len=*), intent(in), optional :: color, linestyle, label
        real(dp), intent(in), optional :: lw
        call ax_sca(self)
        call vlines(x, ymin, ymax, color, linestyle, lw, label)
    end subroutine ax_vlines

    subroutine ax_axvline(self, x, color, linestyle, lw, label)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x
        character(len=*), intent(in), optional :: color, linestyle, label
        real(dp), intent(in), optional :: lw
        call ax_sca(self)
        call axvline(x, color, linestyle, lw, label)
    end subroutine ax_axvline

    subroutine ax_axline(self, xy1, xy2, slope, color, linestyle, lw, label)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: xy1(2)
        real(dp), intent(in), optional :: xy2(2), slope
        character(len=*), intent(in), optional :: color, linestyle, label
        real(dp), intent(in), optional :: lw
        call ax_sca(self)
        call axline(xy1, xy2, slope, color, linestyle, lw, label)
    end subroutine ax_axline

    subroutine ax_text(self, x, y, s, color, fontsize, ha, fontweight, &
                       fontstyle, transform)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x, y
        character(len=*), intent(in) :: s
        character(len=*), intent(in), optional :: color, ha, fontweight, fontstyle
        character(len=*), intent(in), optional :: transform
        real(dp), intent(in), optional :: fontsize
        call ax_sca(self)
        call text(x, y, s, color, fontsize, ha, fontweight, fontstyle, &
                  transform=transform)
    end subroutine ax_text

    subroutine ax_annotate(self, s, x, y, xtext, ytext, color, fontsize, ha, &
                           fontweight, fontstyle, transform)
        class(axes), intent(in) :: self
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: x, y
        real(dp), intent(in), optional :: xtext, ytext, fontsize
        character(len=*), intent(in), optional :: color, ha, fontweight, fontstyle
        character(len=*), intent(in), optional :: transform
        call ax_sca(self)
        call annotate(s, x, y, xtext, ytext, color, fontsize, ha, &
                      fontweight, fontstyle, transform=transform)
    end subroutine ax_annotate

    subroutine ax_set_title(self, s, fontsize, fontweight, fontstyle, loc)
        class(axes), intent(in) :: self
        character(len=*), intent(in) :: s
        real(dp), intent(in), optional :: fontsize
        character(len=*), intent(in), optional :: fontweight, fontstyle, loc
        call ax_sca(self)
        call title(s, fontsize, fontweight, fontstyle, loc)
    end subroutine ax_set_title

    subroutine ax_set_xlabel(self, s, fontsize, fontweight, fontstyle, labelpad)
        class(axes), intent(in) :: self
        character(len=*), intent(in) :: s
        real(dp), intent(in), optional :: fontsize, labelpad
        character(len=*), intent(in), optional :: fontweight, fontstyle
        call ax_sca(self)
        call xlabel(s, fontsize, fontweight, fontstyle, labelpad)
    end subroutine ax_set_xlabel

    subroutine ax_set_ylabel(self, s, fontsize, fontweight, fontstyle, labelpad)
        class(axes), intent(in) :: self
        character(len=*), intent(in) :: s
        real(dp), intent(in), optional :: fontsize, labelpad
        character(len=*), intent(in), optional :: fontweight, fontstyle
        call ax_sca(self)
        call ylabel(s, fontsize, fontweight, fontstyle, labelpad)
    end subroutine ax_set_ylabel

    subroutine ax_set_xlim(self, xmin, xmax)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: xmin, xmax
        call ax_sca(self)
        call xlim(xmin, xmax)
    end subroutine ax_set_xlim

    subroutine ax_set_ylim(self, ymin, ymax)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: ymin, ymax
        call ax_sca(self)
        call ylim(ymin, ymax)
    end subroutine ax_set_ylim

    subroutine ax_get_xlim(self, xmin, xmax)
        class(axes), intent(in) :: self
        real(dp), intent(out) :: xmin, xmax
        call ax_sca(self)
        call get_xlim(xmin, xmax)
    end subroutine ax_get_xlim

    subroutine ax_get_ylim(self, ymin, ymax)
        class(axes), intent(in) :: self
        real(dp), intent(out) :: ymin, ymax
        call ax_sca(self)
        call get_ylim(ymin, ymax)
    end subroutine ax_get_ylim

    subroutine ax_invert_xaxis(self)
        class(axes), intent(in) :: self
        call ax_sca(self)
        call invert_xaxis()
    end subroutine ax_invert_xaxis

    subroutine ax_invert_yaxis(self)
        class(axes), intent(in) :: self
        call ax_sca(self)
        call invert_yaxis()
    end subroutine ax_invert_yaxis

    subroutine ax_set_xscale(self, name, linthresh, linscale)
        class(axes), intent(in) :: self
        character(len=*), intent(in) :: name
        real(dp), intent(in), optional :: linthresh, linscale
        call ax_sca(self)
        call set_xscale(name, linthresh, linscale)
    end subroutine ax_set_xscale

    subroutine ax_set_yscale(self, name, linthresh, linscale)
        class(axes), intent(in) :: self
        character(len=*), intent(in) :: name
        real(dp), intent(in), optional :: linthresh, linscale
        call ax_sca(self)
        call set_yscale(name, linthresh, linscale)
    end subroutine ax_set_yscale

    subroutine ax_set_xticks(self, vals, labels, minor)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: vals(:)
        character(len=*), intent(in), optional :: labels(:)
        logical, intent(in), optional :: minor
        call ax_sca(self)
        call xticks(vals, labels, minor)
    end subroutine ax_set_xticks

    subroutine ax_set_yticks(self, vals, labels, minor)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: vals(:)
        character(len=*), intent(in), optional :: labels(:)
        logical, intent(in), optional :: minor
        call ax_sca(self)
        call yticks(vals, labels, minor)
    end subroutine ax_set_yticks

    subroutine ax_set_aspect(self, ratio, adjustable)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: ratio
        character(len=*), intent(in), optional :: adjustable
        call ax_sca(self)
        call set_aspect(ratio, adjustable)
    end subroutine ax_set_aspect

    subroutine ax_set_facecolor(self, color, alpha)
        class(axes), intent(in) :: self
        character(len=*), intent(in) :: color
        real(dp), intent(in), optional :: alpha
        call ax_sca(self)
        call set_facecolor(color, alpha)
    end subroutine ax_set_facecolor

    subroutine ax_grid(self, on, axis, which, color, linestyle, lw, alpha)
        class(axes), intent(in) :: self
        logical, intent(in) :: on
        character(len=*), intent(in), optional :: axis, which, color, linestyle
        real(dp), intent(in), optional :: lw, alpha
        call ax_sca(self)
        call grid(on, axis, which, color, linestyle, lw, alpha)
    end subroutine ax_grid

    subroutine ax_legend(self, loc, fontsize, ncol, frameon, title, bbox_to_anchor)
        class(axes), intent(in) :: self
        character(len=*), intent(in), optional :: loc, title
        real(dp), intent(in), optional :: fontsize, bbox_to_anchor(2)
        integer, intent(in), optional :: ncol
        logical, intent(in), optional :: frameon
        call ax_sca(self)
        call legend(loc, fontsize, ncol, frameon, title, bbox_to_anchor)
    end subroutine ax_legend

    subroutine ax_tick_params(self, axis, direction, length, labelsize, rotation)
        class(axes), intent(in) :: self
        character(len=*), intent(in), optional :: axis, direction
        real(dp), intent(in), optional :: length, labelsize, rotation
        call ax_sca(self)
        call tick_params(axis, direction, length, labelsize, rotation)
    end subroutine ax_tick_params

    subroutine ax_spines(self, left, right, bottom, top)
        class(axes), intent(in) :: self
        logical, intent(in), optional :: left, right, bottom, top
        call ax_sca(self)
        call spines(left, right, bottom, top)
    end subroutine ax_spines

    subroutine ax_axis(self, mode)
        class(axes), intent(in) :: self
        character(len=*), intent(in) :: mode
        call ax_sca(self)
        call axis(mode)
    end subroutine ax_axis

    ! A twin is a new axes, so it comes back as its own handle.
    function ax_twinx(self) result(t)
        class(axes), intent(in) :: self
        type(axes) :: t
        call ax_sca(self)
        call twinx()
        t%idx = cur_i
    end function ax_twinx

    function ax_twiny(self) result(t)
        class(axes), intent(in) :: self
        type(axes) :: t
        call ax_sca(self)
        call twiny()
        t%idx = cur_i
    end function ax_twiny

    subroutine suptitle(s, fontsize, fontweight, fontstyle)
        character(len=*), intent(in) :: s
        real(dp), intent(in), optional :: fontsize
        character(len=*), intent(in), optional :: fontweight, fontstyle
        call ensure_fig()
        fig_suptitle = s
        if (present(fontsize)) fig_suptitle_size = fontsize
        if (present(fontweight)) fig_suptitle_w = weight_from_str(fontweight)
        if (present(fontstyle)) fig_suptitle_sl = slant_from_str(fontstyle)
    end subroutine suptitle

    subroutine title(s, fontsize, fontweight, fontstyle, loc)
        character(len=*), intent(in) :: s
        real(dp), intent(in), optional :: fontsize
        character(len=*), intent(in), optional :: fontweight, fontstyle, loc
        call ensure_fig()
        ax(cur_i)%title = s
        if (present(loc)) ax(cur_i)%title_loc = loc
        if (present(fontsize)) ax(cur_i)%title_size = fontsize
        if (present(fontweight)) ax(cur_i)%title_w = weight_from_str(fontweight)
        if (present(fontstyle)) ax(cur_i)%title_sl = slant_from_str(fontstyle)
    end subroutine title

    subroutine xlabel(s, fontsize, fontweight, fontstyle, labelpad)
        character(len=*), intent(in) :: s
        real(dp), intent(in), optional :: fontsize, labelpad
        character(len=*), intent(in), optional :: fontweight, fontstyle
        call ensure_fig()
        ax(cur_i)%xlabel = s
        if (present(labelpad)) ax(cur_i)%xlabel_pad = labelpad
        if (present(fontsize)) ax(cur_i)%xlabel_size = fontsize
        if (present(fontweight)) ax(cur_i)%xlabel_w = weight_from_str(fontweight)
        if (present(fontstyle)) ax(cur_i)%xlabel_sl = slant_from_str(fontstyle)
    end subroutine xlabel

    subroutine ylabel(s, fontsize, fontweight, fontstyle, labelpad)
        character(len=*), intent(in) :: s
        real(dp), intent(in), optional :: fontsize, labelpad
        character(len=*), intent(in), optional :: fontweight, fontstyle
        call ensure_fig()
        ax(cur_i)%ylabel = s
        if (present(labelpad)) ax(cur_i)%ylabel_pad = labelpad
        if (present(fontsize)) ax(cur_i)%ylabel_size = fontsize
        if (present(fontweight)) ax(cur_i)%ylabel_w = weight_from_str(fontweight)
        if (present(fontstyle)) ax(cur_i)%ylabel_sl = slant_from_str(fontstyle)
    end subroutine ylabel

    ! matplotlib's spellings for the two faces it can pick without a font
    ! file of its own.
    pure integer function weight_from_str(s)
        character(len=*), intent(in) :: s
        weight_from_str = WEIGHT_NORMAL
        if (s == "bold" .or. s == "heavy" .or. s == "black") &
            weight_from_str = WEIGHT_BOLD
    end function weight_from_str

    pure integer function slant_from_str(s)
        character(len=*), intent(in) :: s
        slant_from_str = SLANT_ROMAN
        if (s == "italic" .or. s == "oblique") slant_from_str = SLANT_ITALIC
    end function slant_from_str

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

    ! The background of one axes, where rc(axes_facecolor=) sets them all.
    subroutine set_facecolor(color, alpha)
        character(len=*), intent(in) :: color
        real(dp), intent(in), optional :: alpha
        call ensure_fig()
        ax(cur_i)%facecolor = resolve_color(color)
        if (present(alpha)) ax(cur_i)%face_alpha = alpha
    end subroutine set_facecolor

    subroutine grid(on, axis, which, color, linestyle, lw, alpha)
        logical, intent(in) :: on
        character(len=*), intent(in), optional :: axis, which, color, linestyle
        real(dp), intent(in), optional :: lw, alpha
        call ensure_fig()
        ax(cur_i)%grid_on = on
        if (present(axis)) ax(cur_i)%grid_axis = axis
        if (present(which)) ax(cur_i)%grid_which = which
        if (present(color)) ax(cur_i)%grid_color = color
        if (present(linestyle)) ax(cur_i)%grid_ls = linestyle_from_str(linestyle)
        if (present(lw)) ax(cur_i)%grid_lw = lw
        if (present(alpha)) ax(cur_i)%grid_alpha = alpha
    end subroutine grid

    ! bbox_to_anchor is in axes coordinates, so (1.02, 1.0) with the default
    ! loc="upper right" is matplotlib's usual recipe for parking the legend
    ! just outside the right-hand edge. When it is given, loc names which
    ! corner of the legend sits on the anchor rather than a position in the
    ! axes, exactly as matplotlib treats it.
    ! One legend for the whole figure: an invisible axes covering the
    ! canvas, carrying a copy of every labelled series in the figure. The
    ! ordinary legend machinery then places it against the figure rather
    ! than against any one panel, which is exactly what figlegend means.
    subroutine figlegend(loc, fontsize, ncol, frameon, title)
        character(len=*), intent(in), optional :: loc, title
        real(dp), intent(in), optional :: fontsize
        integer, intent(in), optional :: ncol
        logical, intent(in), optional :: frameon
        integer :: i, j, is, host, n_before
        type(axes) :: h

        call ensure_fig()
        n_before = n_ax
        h = add_axes([0.0_dp, 0.0_dp, 1.0_dp, 1.0_dp])
        host = h%idx
        ax(host)%patch_off = .true.
        ax(host)%frame_off = .true.
        do i = 1, n_before
            do j = 1, ax(i)%n_series
                if (.not. in_legend(ax(i)%series(j))) cycle
                call push_series(ax(host), is)
                ax(host)%series(is) = ax(i)%series(j)
                ! The copy is there for its label alone; nothing of it is
                ! drawn, and an empty axes autoscales to the unit square.
                if (allocated(ax(host)%series(is)%x)) &
                    deallocate (ax(host)%series(is)%x)
                if (allocated(ax(host)%series(is)%y)) &
                    deallocate (ax(host)%series(is)%y)
                ax(host)%series(is)%n = 0
            end do
        end do
        call legend(loc, fontsize, ncol, frameon, title)
        cur_i = 1
    end subroutine figlegend

    ! Whether an artist shows up in the legend at all. matplotlib keeps
    ! out anything whose label starts with an underscore, which is how an
    ! artist that must carry a name for other reasons stays out of it.
    pure function in_legend(s) result(yes)
        type(series_t), intent(in) :: s
        logical :: yes
        yes = len_trim(s%label) > 0
        if (yes) yes = s%label(1:1) /= "_"
    end function in_legend

    subroutine legend(loc, fontsize, ncol, frameon, title, bbox_to_anchor, labels)
        character(len=*), intent(in), optional :: loc, title
        character(len=*), intent(in), optional :: labels(:)
        real(dp), intent(in), optional :: fontsize, bbox_to_anchor(2)
        integer, intent(in), optional :: ncol
        logical, intent(in), optional :: frameon
        integer :: i
        call ensure_fig()
        ! matplotlib's legend(labels): the names are handed to the artists
        ! in the order they were added, whatever they were called before.
        if (present(labels)) then
            do i = 1, min(ax(cur_i)%n_series, size(labels))
                ax(cur_i)%series(i)%label = labels(i)
            end do
        end if
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


    ! ------------------------------------------------------------------
    ! Formatters and locators. matplotlib reaches these through the axis
    ! object (set_major_formatter, set_major_locator) and a pyplot
    ! shortcut; there is one shortcut per job here instead, because a
    ! formatter object of our own would be a class with one method and
    ! nothing to say.
    ! ------------------------------------------------------------------

    ! matplotlib's ticklabel_format: whether an axis may factor an offset
    ! or a power of ten out of its labels, and from where.
    subroutine ticklabel_format(axis, style, useoffset, scilimits)
        character(len=*), intent(in), optional :: axis, style
        logical, intent(in), optional :: useoffset
        integer, intent(in), optional :: scilimits(2)
        logical :: dox, doy
        integer :: lo, hi

        call ensure_fig()
        call which_axis(axis, dox, doy)
        if (present(useoffset)) then
            if (dox) ax(cur_i)%x_use_offset = useoffset
            if (doy) ax(cur_i)%y_use_offset = useoffset
        end if
        lo = -5
        hi = 6
        if (present(style)) then
            select case (lower(style))
            case ("plain")
                ! Never factor a power of ten out: no limit is ever met.
                lo = -huge(1)/2
                hi = huge(1)/2
            case ("sci", "scientific")
                ! Always factor one out, which is what (0, 0) means.
                lo = 0
                hi = 0
            end select
        end if
        if (present(scilimits)) then
            lo = scilimits(1)
            hi = scilimits(2)
        end if
        if (present(style) .or. present(scilimits)) then
            if (dox) then
                ax(cur_i)%x_scilo = lo
                ax(cur_i)%x_scihi = hi
            end if
            if (doy) then
                ax(cur_i)%y_scilo = lo
                ax(cur_i)%y_scihi = hi
            end if
        end if
    end subroutine ticklabel_format

    ! One of matplotlib's named formatters: "percent" is PercentFormatter,
    ! "comma" is a StrMethodFormatter with a thousands separator, "fixed"
    ! is a FormatStrFormatter with a set number of decimals, and "auto" is
    ! the ScalarFormatter every axis starts with.
    subroutine tick_format(axis, style, decimals, whole)
        character(len=*), intent(in) :: style
        character(len=*), intent(in), optional :: axis
        integer, intent(in), optional :: decimals
        real(dp), intent(in), optional :: whole
        logical :: dox, doy
        integer :: st, dec

        call ensure_fig()
        call which_axis(axis, dox, doy)
        select case (lower(style))
        case ("percent"); st = FMT_PERCENT
        case ("comma", "thousands"); st = FMT_COMMA
        case ("fixed"); st = FMT_FIXED
        case default; st = FMT_AUTO
        end select
        dec = -1
        if (present(decimals)) dec = decimals
        ! A percentage with no decimals asked for is written whole, which
        ! is what PercentFormatter defaults to.
        if (st == FMT_PERCENT .and. dec < 0) dec = 0
        if (dox) then
            ax(cur_i)%xfmt_style = st
            ax(cur_i)%xfmt_dec = dec
            if (present(whole)) ax(cur_i)%xfmt_whole = whole
        end if
        if (doy) then
            ax(cur_i)%yfmt_style = st
            ax(cur_i)%yfmt_dec = dec
            if (present(whole)) ax(cur_i)%yfmt_whole = whole
        end if
    end subroutine tick_format

    ! base is MultipleLocator, nbins is MaxNLocator. Neither moves the
    ! limits; they only decide where the ticks land inside them.
    subroutine tick_locator(axis, base, nbins)
        character(len=*), intent(in), optional :: axis
        real(dp), intent(in), optional :: base
        integer, intent(in), optional :: nbins
        logical :: dox, doy

        call ensure_fig()
        call which_axis(axis, dox, doy)
        if (present(base)) then
            if (dox) ax(cur_i)%xtick_base = base
            if (doy) ax(cur_i)%ytick_base = base
        end if
        if (present(nbins)) then
            if (dox) ax(cur_i)%xtick_nbins = nbins
            if (doy) ax(cur_i)%ytick_nbins = nbins
        end if
    end subroutine tick_locator

    ! "x", "y" or "both", the way every matplotlib call that takes an axis
    ! name spells it. Absent means both.
    subroutine which_axis(axis, dox, doy)
        character(len=*), intent(in), optional :: axis
        logical, intent(out) :: dox, doy
        dox = .true.
        doy = .true.
        if (.not. present(axis)) return
        select case (lower(axis))
        case ("x"); doy = .false.
        case ("y"); dox = .false.
        end select
    end subroutine which_axis

    subroutine xticks(vals, labels, minor)
        real(dp), intent(in) :: vals(:)
        character(len=*), intent(in), optional :: labels(:)
        logical, intent(in), optional :: minor
        call ensure_fig()
        if (is_minor(minor)) then
            call set_minor_ticks(vals, ax(cur_i)%n_xminor, ax(cur_i)%xminor_pos)
            return
        end if
        call set_ticks(vals, labels, ax(cur_i)%n_xticks, ax(cur_i)%xtick_pos, &
                       ax(cur_i)%xtick_labeled, ax(cur_i)%xtick_lab)
    end subroutine xticks

    subroutine yticks(vals, labels, minor)
        real(dp), intent(in) :: vals(:)
        character(len=*), intent(in), optional :: labels(:)
        logical, intent(in), optional :: minor
        call ensure_fig()
        if (is_minor(minor)) then
            call set_minor_ticks(vals, ax(cur_i)%n_yminor, ax(cur_i)%yminor_pos)
            return
        end if
        call set_ticks(vals, labels, ax(cur_i)%n_yticks, ax(cur_i)%ytick_pos, &
                       ax(cur_i)%ytick_labeled, ax(cur_i)%ytick_lab)
    end subroutine yticks

    pure function is_minor(minor) result(v)
        logical, intent(in), optional :: minor
        logical :: v
        v = .false.
        if (present(minor)) v = minor
    end function is_minor

    subroutine set_minor_ticks(vals, n, pos)
        real(dp), intent(in) :: vals(:)
        integer, intent(out) :: n
        real(dp), intent(out) :: pos(MAX_TICKS)
        integer :: i
        n = min(size(vals), MAX_TICKS)
        do i = 1, n
            pos(i) = vals(i)
        end do
    end subroutine set_minor_ticks

    ! matplotlib's locator_params: how many intervals the locator may use,
    ! and whether to drop the tick at one end so that it does not collide
    ! with a neighbouring subplot.
    subroutine locator_params(axis, nbins, prune)
        character(len=*), intent(in), optional :: axis, prune
        integer, intent(in), optional :: nbins
        logical :: dox, doy
        call ensure_fig()
        call which_axis(axis, dox, doy)
        if (present(nbins)) then
            if (dox) ax(cur_i)%xtick_nbins = nbins
            if (doy) ax(cur_i)%ytick_nbins = nbins
        end if
        if (present(prune)) then
            if (dox) ax(cur_i)%xtick_prune = prune
            if (doy) ax(cur_i)%ytick_prune = prune
        end if
    end subroutine locator_params

    ! Drop the tick at one or both ends of a located set.
    pure subroutine prune_ticks(prune, t, n)
        character(len=*), intent(in) :: prune
        real(dp), intent(inout) :: t(:)
        integer, intent(inout) :: n
        if (n <= 0) return
        select case (trim(prune))
        case ("lower", "both")
            t(1:n - 1) = t(2:n)
            n = n - 1
        end select
        if (n <= 0) return
        select case (trim(prune))
        case ("upper", "both")
            n = n - 1
        end select
    end subroutine prune_ticks

    subroutine set_ticks(vals, labels, n, pos, labeled, lab)
        real(dp), intent(in) :: vals(:)
        character(len=*), intent(in), optional :: labels(:)
        integer, intent(out) :: n
        real(dp), intent(out) :: pos(MAX_TICKS)
        logical, intent(out) :: labeled
        character(len=24), intent(out) :: lab(MAX_TICKS)
        integer :: i
        ! An empty list means no ticks at all, which is not the same as
        ! never having asked for any.
        if (size(vals) == 0) then
            n = -1
            labeled = .false.
            return
        end if
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

    ! The limits actually in force, autoscaling included, so that a caller
    ! can place something relative to what the axes ended up showing.
    subroutine get_xlim(xmin, xmax)
        real(dp), intent(out) :: xmin, xmax
        real(dp) :: ylo, yhi
        call ensure_fig()
        call compute_limits(ax(cur_i), xmin, xmax, ylo, yhi)
    end subroutine get_xlim

    subroutine get_ylim(ymin, ymax)
        real(dp), intent(out) :: ymin, ymax
        real(dp) :: xlo, xhi
        call ensure_fig()
        call compute_limits(ax(cur_i), xlo, xhi, ymin, ymax)
    end subroutine get_ylim

    ! Turn an axis round, so that it counts down instead of up.
    subroutine invert_xaxis()
        call ensure_fig()
        ax(cur_i)%x_inv = .not. ax(cur_i)%x_inv
    end subroutine invert_xaxis

    subroutine invert_yaxis()
        call ensure_fig()
        ax(cur_i)%y_inv = .not. ax(cur_i)%y_inv
    end subroutine invert_yaxis

    ! matplotlib's artist.set_zorder, applied to the artist just drawn.
    ! Fortran has no artist objects to hang a keyword off, so rather than
    ! add a zorder= to every plotting call this names the one that would
    ! have taken it: the series most recently added to these axes.
    subroutine set_zorder(z)
        real(dp), intent(in) :: z

        call ensure_fig()
        if (ax(cur_i)%n_series < 1) &
            error stop "fplot: set_zorder needs something drawn first"
        if (z < 0.0_dp) error stop "fplot: zorder must not be negative"
        ax(cur_i)%series(ax(cur_i)%n_series)%zorder = z
    end subroutine set_zorder

    subroutine plot_num(x, y, fmt, label, lw, color, marker, linestyle, alpha, &
                        markersize, markerfacecolor, markeredgecolor, &
                        markeredgewidth, markevery, drawstyle, dashes)
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: fmt, label, color, marker, linestyle
        character(len=*), intent(in), optional :: markerfacecolor, markeredgecolor
        character(len=*), intent(in), optional :: drawstyle
        real(dp), intent(in), optional :: lw, alpha, markersize, markeredgewidth
        real(dp), intent(in), optional :: dashes(:)
        integer, intent(in), optional :: markevery
        integer :: is
        real(dp), allocatable :: sx(:), sy(:)

        call ensure_fig()
        if (present(drawstyle)) then
            ! A drawstyle is a step under another name, so it is drawn by
            ! the same code rather than a second copy of it.
            call stair_points(x, y, step_where(drawstyle), sx, sy)
            call add_series(cur_i, sx, sy, fmt, label, lw, color, marker, &
                            linestyle, alpha)
        else
            call add_series(cur_i, x, y, fmt, label, lw, color, marker, &
                            linestyle, alpha)
        end if
        is = ax(cur_i)%n_series
        if (is < 1) return
        if (present(markersize)) ax(cur_i)%series(is)%markersize = markersize
        if (present(markerfacecolor)) &
            ax(cur_i)%series(is)%mfc = resolve_color(markerfacecolor)
        if (present(markeredgecolor)) &
            ax(cur_i)%series(is)%mec = resolve_color(markeredgecolor)
        if (present(markeredgewidth)) ax(cur_i)%series(is)%mew = markeredgewidth
        if (present(markevery)) ax(cur_i)%series(is)%markevery = max(1, markevery)
        if (present(dashes)) then
            ax(cur_i)%series(is)%n_dash = min(size(dashes), 4)
            ax(cur_i)%series(is)%dashes(1:ax(cur_i)%series(is)%n_dash) = &
                dashes(1:ax(cur_i)%series(is)%n_dash)
        end if
    end subroutine plot_num

    ! plot(y): matplotlib numbers the points 0, 1, 2 ... when it is given
    ! only one array, and so does this.
    subroutine plot_y(y, fmt, label, lw, color, marker, linestyle, alpha, &
                      markersize, markerfacecolor, markeredgecolor, &
                      markeredgewidth, markevery, drawstyle, dashes)
        real(dp), intent(in) :: y(:)
        character(len=*), intent(in), optional :: fmt, label, color, marker, linestyle
        character(len=*), intent(in), optional :: markerfacecolor, markeredgecolor
        character(len=*), intent(in), optional :: drawstyle
        real(dp), intent(in), optional :: lw, alpha, markersize, markeredgewidth
        real(dp), intent(in), optional :: dashes(:)
        integer, intent(in), optional :: markevery
        integer :: i
        real(dp) :: idx(size(y))

        do i = 1, size(y)
            idx(i) = real(i - 1, dp)
        end do
        call plot_num(idx, y, fmt, label, lw, color, marker, linestyle, alpha, &
                      markersize, markerfacecolor, markeredgecolor, &
                      markeredgewidth, markevery, drawstyle, dashes)
    end subroutine plot_y

    subroutine plot_cat(cats, y, fmt, label, lw, color, marker, linestyle, alpha)
        character(len=*), intent(in) :: cats(:)
        real(dp), intent(in) :: y(:)
        character(len=*), intent(in), optional :: fmt, label, color, marker, linestyle
        real(dp), intent(in), optional :: lw, alpha
        call plot_num(category_positions(size(cats)), y, fmt, label, lw, color, &
                      marker, linestyle, alpha)
        call xticks(category_positions(size(cats)), cats)
    end subroutine plot_cat

    ! Marker-only plot. s is the marker area in points^2 (matplotlib's
    ! convention), so the marker size is its square root.
    ! s and c are the scalar forms; sizes and cvals are their per-point
    ! equivalents. Fortran cannot overload one dummy as scalar-or-array, so
    ! they are separate keywords rather than matplotlib's single s= and c=.
    subroutine scatter(x, y, s, c, marker, label, alpha, sizes, cvals, cmap, vmin, vmax, &
                       edgecolors, linewidths)
        real(dp), intent(in) :: x(:), y(:)
        real(dp), intent(in), optional :: s, alpha, vmin, vmax, linewidths
        real(dp), intent(in), optional :: sizes(:), cvals(:)
        character(len=*), intent(in), optional :: c, marker, label, cmap, edgecolors
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

        ! matplotlib draws scatter markers with no edge at all by default,
        ! so an edge only appears once one is asked for.
        if (present(edgecolors)) &
            ax(cur_i)%series(is)%mec = resolve_color(edgecolors)
        if (present(linewidths)) ax(cur_i)%series(is)%mew = max(linewidths, 0.0_dp)

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
        ax(cur_i)%series(is)%linewidth = rc_lw
        ax(cur_i)%series(is)%markersize = default_markersize
        ax(cur_i)%series(is)%label = ""
        if (present(label)) ax(cur_i)%series(is)%label = label
        if (present(alpha)) ax(cur_i)%series(is)%alpha = alpha

        ax(cur_i)%series(is)%color = resolve_color(color, ca)
        if (ca >= 0.0_dp .and. .not. present(alpha)) ax(cur_i)%series(is)%alpha = ca
        if (len_trim(ax(cur_i)%series(is)%color) == 0) then
            ax(cur_i)%series(is)%color = cycle_color(ax(cur_i)%color_cycle)
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
                col = cycle_color(m)
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
    subroutine imshow_z(z, cmap, vmin, vmax, extent, origin, aspect, norm, &
                        interpolation, boundaries, vcenter, gamma, linthresh)
        real(dp), intent(in) :: z(:, :)
        character(len=*), intent(in), optional :: cmap, origin, aspect, norm, interpolation
        real(dp), intent(in), optional :: vmin, vmax, extent(4), boundaries(:)
        real(dp), intent(in), optional :: vcenter, gamma, linthresh
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

        ax(cur_i)%img_bilinear = .false.
        if (present(interpolation)) &
            ax(cur_i)%img_bilinear = trim(interpolation) == "bilinear"

        ax(cur_i)%img_norm = NORM_LINEAR
        if (present(norm)) ax(cur_i)%img_norm = norm_from_str(norm)
        call norm_params(vcenter, gamma, linthresh)

        if (allocated(ax(cur_i)%img_bounds)) deallocate (ax(cur_i)%img_bounds)
        if (present(boundaries)) then
            if (size(boundaries) >= 2) then
                allocate (ax(cur_i)%img_bounds(size(boundaries)))
                ax(cur_i)%img_bounds = boundaries
            end if
        end if

        if (ax(cur_i)%img_norm == NORM_LOG) then
            ! A log scale cannot start at zero, so the smallest positive
            ! sample sets the bottom of the range.
            lo = huge(1.0_dp)
            if (any(z > 0.0_dp)) lo = minval(z, mask=(z > 0.0_dp))
            hi = maxval(z)
        else if (any(z == z)) then
            ! Values that are not there at all say nothing about the range.
            lo = minval(z, mask=(z == z))
            hi = maxval(z, mask=(z == z))
        else
            lo = 0.0_dp
            hi = 1.0_dp
        end if
        if (present(vmin)) lo = vmin
        if (present(vmax)) hi = vmax
        ! The bands say what the range is; anything outside them is clamped.
        if (allocated(ax(cur_i)%img_bounds)) then
            lo = ax(cur_i)%img_bounds(1)
            hi = ax(cur_i)%img_bounds(size(ax(cur_i)%img_bounds))
        end if
        if (hi <= lo) hi = lo + 1.0_dp
        ax(cur_i)%img_vmin = lo
        ax(cur_i)%img_vmax = hi

        ax(cur_i)%has_mesh = .false.
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
    end subroutine imshow_z

    ! An image whose colours are given directly: z is (row, column, channel)
    ! with three channels for RGB or four for RGBA, each in 0..1 as
    ! matplotlib reads a float array. There is nothing here for a colorbar
    ! to describe, so none is offered.
    subroutine imshow_rgb(z, extent, origin, aspect)
        real(dp), intent(in) :: z(:, :, :)
        character(len=*), intent(in), optional :: origin, aspect
        real(dp), intent(in), optional :: extent(4)
        integer :: nr, nc, nch

        call ensure_fig()
        nr = size(z, 1)
        nc = size(z, 2)
        nch = size(z, 3)
        if (nr < 1 .or. nc < 1 .or. nch < 3) return

        if (allocated(ax(cur_i)%img)) deallocate (ax(cur_i)%img)
        allocate (ax(cur_i)%img(nr, nc))
        ax(cur_i)%img = 0.0_dp
        if (allocated(ax(cur_i)%img_rgb)) deallocate (ax(cur_i)%img_rgb)
        allocate (ax(cur_i)%img_rgb(nr, nc, nch))
        ax(cur_i)%img_rgb = min(1.0_dp, max(0.0_dp, z))
        ax(cur_i)%has_img = .true.
        ax(cur_i)%has_rgb = .true.
        ax(cur_i)%has_cmap_src = .false.
        ax(cur_i)%has_mesh = .false.
        ax(cur_i)%img_bilinear = .false.
        ax(cur_i)%img_norm = NORM_LINEAR

        ax(cur_i)%img_origin_upper = .true.
        if (present(origin)) ax(cur_i)%img_origin_upper = trim(origin) /= "lower"
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
    end subroutine imshow_rgb

    ! The colour of one image sample as a hex string, for the paths that
    ! draw the image as rectangles rather than as a raster.
    pure function img_hex(a, i, j) result(hex)
        type(axes_t), intent(in) :: a
        integer, intent(in) :: i, j
        character(len=7) :: hex
        character(len=16), parameter :: D = "0123456789abcdef"
        integer :: c(4), k
        if (.not. a%has_rgb) then
            hex = img_color(a, a%img(i, j))
            return
        end if
        call img_rgba(a, i, j, c)
        hex = "#000000"
        do k = 1, 3
            hex(2*k:2*k) = D(c(k)/16 + 1:c(k)/16 + 1)
            hex(2*k + 1:2*k + 1) = D(mod(c(k), 16) + 1:mod(c(k), 16) + 1)
        end do
    end function img_hex

    ! The colour of one image sample, ready for the raster.
    pure subroutine img_rgba(a, i, j, c)
        type(axes_t), intent(in) :: a
        integer, intent(in) :: i, j
        integer, intent(out) :: c(4)
        c(4) = 255
        if (.not. a%has_rgb) then
            if (img_bad_hidden(a, a%img(i, j))) then
                ! matplotlib leaves missing samples fully transparent
                ! unless a colour was set aside for them.
                c = [255, 255, 255, 0]
                return
            end if
        end if
        if (a%has_rgb) then
            c(1:3) = nint(255.0_dp*a%img_rgb(i, j, 1:3))
            if (size(a%img_rgb, 3) >= 4) c(4) = nint(255.0_dp*a%img_rgb(i, j, 4))
        else
            c(1:3) = hex_rgb(img_color(a, a%img(i, j)))
        end if
    end subroutine img_rgba

    ! matshow: an image of a matrix. The first row is at the top, the
    ! cells are square and the column numbers run along the top, which is
    ! how a matrix is written down.
    subroutine matshow(z, cmap, vmin, vmax)
        real(dp), intent(in) :: z(:, :)
        character(len=*), intent(in), optional :: cmap
        real(dp), intent(in), optional :: vmin, vmax

        call imshow(z, cmap, vmin, vmax, aspect="equal")
        ax(cur_i)%x_top = .true.
    end subroutine matshow

    ! A row of events, each drawn as a stroke across the line it sits on.
    subroutine eventplot(positions, lineoffset, linelength, color, lw)
        real(dp), intent(in) :: positions(:)
        real(dp), intent(in), optional :: lineoffset, linelength, lw
        character(len=*), intent(in), optional :: color
        real(dp) :: off, len_

        off = 1.0_dp
        len_ = 1.0_dp
        if (present(lineoffset)) off = lineoffset
        if (present(linelength)) len_ = linelength
        call vlines(positions, off - 0.5_dp*len_, off + 0.5_dp*len_, &
                    color=color, lw=lw)
        ! matplotlib keeps a whole line length of room either side of the
        ! row, not the half it actually draws.
        if (ax(cur_i)%yroom_set) then
            ax(cur_i)%yroom = [min(ax(cur_i)%yroom(1), off - len_), &
                               max(ax(cur_i)%yroom(2), off + len_)]
        else
            ax(cur_i)%yroom = [off - len_, off + len_]
            ax(cur_i)%yroom_set = .true.
        end if
    end subroutine eventplot

    ! Bars along one row: each range is a start and a width, and the row
    ! is a bottom and a height.
    subroutine broken_barh(xranges, yrange, color, alpha, edgecolor, lw)
        real(dp), intent(in) :: xranges(:, :), yrange(2)
        character(len=*), intent(in), optional :: color, edgecolor
        real(dp), intent(in), optional :: alpha, lw
        integer :: i, is

        call ensure_fig()
        do i = 1, size(xranges, 1)
            call add_rectangle([xranges(i, 1), yrange(1)], xranges(i, 2), yrange(2), &
                               facecolor=color, edgecolor=edgecolor, lw=lw, alpha=alpha)
            is = ax(cur_i)%n_series
            if (is > 0) ax(cur_i)%series(is)%patch_scales = .true.
        end do
    end subroutine broken_barh

    ! A table of text, laid out the way matplotlib lays one out: every cell
    ! the same height, a fixed fraction of the axes wide unless told
    ! otherwise, and the whole block placed against an edge of the axes.
    subroutine table(cell_text, col_labels, row_labels, col_widths, loc, fontsize)
        character(len=*), intent(in) :: cell_text(:, :)
        character(len=*), intent(in), optional :: col_labels(:), row_labels(:), loc
        real(dp), intent(in), optional :: col_widths(:), fontsize
        integer :: nr, nc, i, j

        call ensure_fig()
        nr = size(cell_text, 1)
        nc = size(cell_text, 2)
        if (nr < 1 .or. nc < 1) return

        if (allocated(ax(cur_i)%tbl_cells)) deallocate (ax(cur_i)%tbl_cells)
        allocate (ax(cur_i)%tbl_cells(nr, nc))
        do i = 1, nr
            do j = 1, nc
                ax(cur_i)%tbl_cells(i, j) = cell_text(i, j)
            end do
        end do

        if (allocated(ax(cur_i)%tbl_col)) deallocate (ax(cur_i)%tbl_col)
        if (present(col_labels)) then
            allocate (ax(cur_i)%tbl_col(nc))
            do j = 1, min(nc, size(col_labels))
                ax(cur_i)%tbl_col(j) = col_labels(j)
            end do
        end if

        if (allocated(ax(cur_i)%tbl_row)) deallocate (ax(cur_i)%tbl_row)
        if (present(row_labels)) then
            allocate (ax(cur_i)%tbl_row(nr))
            do i = 1, min(nr, size(row_labels))
                ax(cur_i)%tbl_row(i) = row_labels(i)
            end do
        end if

        if (allocated(ax(cur_i)%tbl_w)) deallocate (ax(cur_i)%tbl_w)
        allocate (ax(cur_i)%tbl_w(nc))
        ax(cur_i)%tbl_w = 1.0_dp/real(nc, dp)
        if (present(col_widths)) then
            do j = 1, min(nc, size(col_widths))
                ax(cur_i)%tbl_w(j) = col_widths(j)
            end do
        end if

        ax(cur_i)%tbl_size = 10.0_dp
        if (present(fontsize)) ax(cur_i)%tbl_size = fontsize
        ax(cur_i)%tbl_loc = "bottom"
        if (present(loc)) ax(cur_i)%tbl_loc = loc
        ax(cur_i)%has_table = .true.
    end subroutine table

    ! Streamlines of a vector field. This follows matplotlib closely: the
    ! field is integrated with an adaptive Heun step in grid coordinates,
    ! seeds spiral inwards from the corner of a 30x30 mask, and a line stops
    ! as soon as it enters a cell another line already went through.
    !
    ! u and v are indexed (row, column), that is (y, x), as everywhere else.
    subroutine streamplot(x, y, u, v, density, color, lw, arrowsize)
        real(dp), intent(in) :: x(:), y(:), u(:, :), v(:, :)
        real(dp), intent(in), optional :: density, lw, arrowsize
        character(len=*), intent(in), optional :: color
        type(stream_t) :: st
        real(dp), parameter :: MINLENGTH = 0.1_dp
        real(dp), allocatable :: xs(:), ys(:), tx(:), ty(:), ahx(:), ahy(:), ahu(:), ahv(:)
        real(dp) :: dx, dy, x0, y0, dens, tot, sl, half, ca
        integer :: nx, ny, i, j, np, is, na, k, nseed, sx, sy, sd(5)
        character(len=32) :: col

        call ensure_fig()
        nx = size(x)
        ny = size(y)
        if (nx < 2 .or. ny < 2) return
        if (size(u, 1) /= ny .or. size(u, 2) /= nx) return
        if (size(v, 1) /= ny .or. size(v, 2) /= nx) return

        dx = x(2) - x(1)
        dy = y(2) - y(1)
        x0 = x(1)
        y0 = y(1)
        dens = 1.0_dp
        if (present(density)) dens = density

        ! Velocities in grid coordinates, and the speed in axes coordinates,
        ! which is what the arc length is measured in.
        st%nx = nx
        st%ny = ny
        allocate (st%u(ny, nx), st%v(ny, nx), st%sp(ny, nx))
        st%u = u/dx
        st%v = v/dy
        st%sp = sqrt((st%u/real(nx - 1, dp))**2 + (st%v/real(ny - 1, dp))**2)

        st%mnx = int(30.0_dp*dens)
        st%mny = int(30.0_dp*dens)
        if (st%mnx < 1 .or. st%mny < 1) return
        allocate (st%mask(st%mny, st%mnx), st%claim(2, st%mnx*st%mny))
        st%mask = 0
        st%g2mx = real(st%mnx - 1, dp)/real(nx - 1, dp)
        st%g2my = real(st%mny - 1, dp)/real(ny - 1, dp)
        ! Going back the other way through the reciprocal, not by dividing:
        ! a seed on the far edge must land a hair outside the grid, exactly
        ! as it does in matplotlib, or it starts a line that should not be.
        st%m2gx = 1.0_dp/st%g2mx
        st%m2gy = 1.0_dp/st%g2my

        col = resolve_color(color, ca)
        if (len_trim(col) == 0) then
            col = cycle_color(ax(cur_i)%color_cycle)
            ax(cur_i)%color_cycle = ax(cur_i)%color_cycle + 1
        end if

        allocate (xs(MAX_STREAM_PTS), ys(MAX_STREAM_PTS))
        allocate (tx(MAX_STREAM_PTS), ty(MAX_STREAM_PTS))
        nseed = st%mnx*st%mny
        allocate (ahx(nseed), ahy(nseed), ahu(nseed), ahv(nseed))
        na = 0

        sx = 0
        sy = 0
        do i = 1, nseed
            call stream_seed(st%mnx, st%mny, i, sx, sy, sd)
            if (st%mask(sy + 1, sx + 1) /= 0) cycle
            call stream_line(st, real(sx, dp)*st%m2gx, real(sy, dp)*st%m2gy, &
                             xs, ys, np, tot)
            if (np < 2 .or. tot <= MINLENGTH) cycle

            do j = 1, np
                tx(j) = x0 + xs(j)*dx
                ty(j) = y0 + ys(j)*dy
            end do
            is = new_shape_series(SERIES_LINE, tx(1:np), ty(1:np), trim(col))
            if (is < 1) cycle
            if (present(lw)) ax(cur_i)%series(is)%linewidth = lw

            ! One arrowhead per line, at the halfway point along it.
            sl = 0.0_dp
            do j = 1, np - 1
                sl = sl + hypot(tx(j + 1) - tx(j), ty(j + 1) - ty(j))
            end do
            half = 0.5_dp*sl
            sl = 0.0_dp
            k = np - 1
            do j = 1, np - 1
                sl = sl + hypot(tx(j + 1) - tx(j), ty(j + 1) - ty(j))
                if (sl >= half) then
                    k = j
                    exit
                end if
            end do
            na = na + 1
            ! The tip sits where matplotlib puts the head of its arrow patch,
            ! at the middle of the segment the halfway point falls in.
            ahx(na) = 0.5_dp*(tx(k) + tx(k + 1))
            ahy(na) = 0.5_dp*(ty(k) + ty(k + 1))
            ahu(na) = tx(k + 1) - tx(k)
            ahv(na) = ty(k + 1) - ty(k)
        end do

        if (na > 0) then
            is = new_shape_series(SERIES_ARROWHEAD, ahx(1:na), ahy(1:na), trim(col))
            if (is > 0) then
                allocate (ax(cur_i)%series(is)%qu(na), ax(cur_i)%series(is)%qv(na))
                ax(cur_i)%series(is)%qu = ahu(1:na)
                ax(cur_i)%series(is)%qv = ahv(1:na)
            end if
        end if

        ! matplotlib makes the edges of the field sticky, so the axes end
        ! exactly on the grid with no margin.
        ax(cur_i)%xroom = [x(1), x(nx)]
        ax(cur_i)%xroom_set = .true.
        if (ax(cur_i)%yroom_set) then
            ax(cur_i)%yroom = [min(ax(cur_i)%yroom(1), y(1)), max(ax(cur_i)%yroom(2), y(ny))]
        else
            ax(cur_i)%yroom = [y(1), y(ny)]
            ax(cur_i)%yroom_set = .true.
        end if
        ax(cur_i)%room_sticks = .true.
    end subroutine streamplot

    ! Seed points spiral inwards from the corner of the mask, which is what
    ! gives the boundary streamlines first claim on the cells.
    ! st holds the four closing edges and the direction of travel, so that
    ! the walk keeps no state of its own.
    subroutine stream_seed(nx, ny, i, x, y, sd)
        integer, intent(in) :: nx, ny, i
        integer, intent(inout) :: x, y, sd(5)
        integer :: xfirst, yfirst, xlast, ylast, dirn

        if (i == 1) then
            sd = [0, 1, nx - 1, ny - 1, 0]
            x = 0
            y = 0
            return
        end if
        xfirst = sd(1)
        yfirst = sd(2)
        xlast = sd(3)
        ylast = sd(4)
        dirn = sd(5)
        select case (dirn)
        case (0)
            x = x + 1
            if (x >= xlast) then
                xlast = xlast - 1
                dirn = 1
            end if
        case (1)
            y = y + 1
            if (y >= ylast) then
                ylast = ylast - 1
                dirn = 2
            end if
        case (2)
            x = x - 1
            if (x <= xfirst) then
                xfirst = xfirst + 1
                dirn = 3
            end if
        case default
            y = y - 1
            if (y <= yfirst) then
                yfirst = yfirst + 1
                dirn = 0
            end if
        end select
        sd = [xfirst, yfirst, xlast, ylast, dirn]
    end subroutine stream_seed

    ! Bilinear lookup on the integer grid, with the position given in grid
    ! coordinates running 0..n-1.
    function stream_interp(a, xi, yi) result(r)
        real(dp), intent(in) :: a(:, :), xi, yi
        real(dp) :: r, xt, yt, a0, a1
        integer :: ix, iy, ixn, iyn

        ix = int(xi)
        iy = int(yi)
        ixn = min(ix + 1, size(a, 2) - 1)
        iyn = min(iy + 1, size(a, 1) - 1)
        xt = xi - real(ix, dp)
        yt = yi - real(iy, dp)
        a0 = a(iy + 1, ix + 1)*(1.0_dp - xt) + a(iy + 1, ixn + 1)*xt
        a1 = a(iyn + 1, ix + 1)*(1.0_dp - xt) + a(iyn + 1, ixn + 1)*xt
        r = a0*(1.0_dp - yt) + a1*yt
    end function stream_interp

    function stream_in_grid(st, xi, yi) result(r)
        type(stream_t), intent(in) :: st
        real(dp), intent(in) :: xi, yi
        logical :: r
        r = xi >= 0.0_dp .and. xi <= real(st%nx - 1, dp) .and. &
            yi >= 0.0_dp .and. yi <= real(st%ny - 1, dp)
    end function stream_in_grid

    ! The direction of travel at a point, normalised so that a step in the
    ! parameter is a step in arc length. code is 1 off the grid and 2 where
    ! the field stalls.
    subroutine stream_dir(st, xi, yi, dirn, dxi, dyi, code)
        type(stream_t), intent(in) :: st
        real(dp), intent(in) :: xi, yi
        integer, intent(in) :: dirn
        real(dp), intent(out) :: dxi, dyi
        integer, intent(out) :: code
        real(dp) :: ds_dt

        dxi = 0.0_dp
        dyi = 0.0_dp
        code = 0
        if (.not. stream_in_grid(st, xi, yi)) then
            code = 1
            return
        end if
        ds_dt = stream_interp(st%sp, xi, yi)
        if (ds_dt == 0.0_dp) then
            code = 2
            return
        end if
        dxi = real(dirn, dp)*stream_interp(st%u, xi, yi)/ds_dt
        dyi = real(dirn, dp)*stream_interp(st%v, xi, yi)/ds_dt
    end subroutine stream_dir

    ! Claim the mask cell a point falls in. ok comes back false when another
    ! streamline already went through it.
    subroutine stream_claim(st, xg, yg, ok)
        type(stream_t), intent(inout) :: st
        real(dp), intent(in) :: xg, yg
        logical, intent(out) :: ok
        integer :: mx, my

        ok = .true.
        mx = nint(xg*st%g2mx)
        my = nint(yg*st%g2my)
        if (mx == st%cx .and. my == st%cy) return
        if (st%mask(my + 1, mx + 1) /= 0) then
            ok = .false.
            return
        end if
        st%nclaim = st%nclaim + 1
        st%claim(1, st%nclaim) = mx
        st%claim(2, st%nclaim) = my
        st%mask(my + 1, mx + 1) = 1
        st%cx = mx
        st%cy = my
    end subroutine stream_claim

    ! A whole streamline through the seed: backwards from it, then forwards,
    ! joined up. The cells claimed along the way are released again if the
    ! line turns out to be too short to keep.
    subroutine stream_line(st, x0, y0, xs, ys, np, tot)
        type(stream_t), intent(inout) :: st
        real(dp), intent(in) :: x0, y0
        real(dp), intent(inout) :: xs(:), ys(:)
        integer, intent(out) :: np
        real(dp), intent(out) :: tot
        real(dp), allocatable :: bx(:), by(:), fx(:), fy(:)
        real(dp) :: sb, sf
        integer :: nb, nf, j
        logical :: ok

        np = 0
        tot = 0.0_dp
        st%nclaim = 0
        st%cx = -1
        st%cy = -1
        call stream_claim(st, x0, y0, ok)
        if (.not. ok) return

        allocate (bx(MAX_STREAM_PTS), by(MAX_STREAM_PTS))
        allocate (fx(MAX_STREAM_PTS), fy(MAX_STREAM_PTS))
        call stream_rk12(st, x0, y0, -1, bx, by, nb, sb)
        st%cx = nint(x0*st%g2mx)
        st%cy = nint(y0*st%g2my)
        call stream_rk12(st, x0, y0, 1, fx, fy, nf, sf)
        tot = sb + sf

        do j = nb, 1, -1
            np = np + 1
            xs(np) = bx(j)
            ys(np) = by(j)
        end do
        do j = 2, nf
            np = np + 1
            xs(np) = fx(j)
            ys(np) = fy(j)
        end do
        if (tot <= 0.1_dp) then
            do j = 1, st%nclaim
                st%mask(st%claim(2, j) + 1, st%claim(1, j) + 1) = 0
            end do
        end if
    end subroutine stream_line

    ! Heun's method with an adaptive step: cheap, and small enough a step to
    ! visit every mask cell on the way, which is what the mask needs.
    subroutine stream_rk12(st, x0, y0, dirn, xs, ys, np, tot)
        type(stream_t), intent(inout) :: st
        real(dp), intent(in) :: x0, y0
        integer, intent(in) :: dirn
        real(dp), intent(inout) :: xs(:), ys(:)
        integer, intent(out) :: np
        real(dp), intent(out) :: tot
        real(dp), parameter :: MAXERROR = 0.003_dp, MAXLENGTH = 4.0_dp
        real(dp) :: maxds, ds, xi, yi, k1x, k1y, k2x, k2y, dx1, dy1, dx2, dy2, err
        integer :: code
        logical :: ok

        maxds = min(1.0_dp/real(st%mnx, dp), 1.0_dp/real(st%mny, dp), 0.1_dp)
        ds = maxds
        tot = 0.0_dp
        xi = x0
        yi = y0
        np = 0

        do
            if (.not. stream_in_grid(st, xi, yi)) then
                if (np > 0) call stream_euler(st, xs, ys, np, dirn, tot)
                exit
            end if
            if (np >= size(xs)) exit
            np = np + 1
            xs(np) = xi
            ys(np) = yi

            call stream_dir(st, xi, yi, dirn, k1x, k1y, code)
            if (code == 1) then
                call stream_euler(st, xs, ys, np, dirn, tot)
                exit
            else if (code == 2) then
                exit
            end if
            call stream_dir(st, xi + ds*k1x, yi + ds*k1y, dirn, k2x, k2y, code)
            if (code == 1) then
                call stream_euler(st, xs, ys, np, dirn, tot)
                exit
            else if (code == 2) then
                exit
            end if

            dx1 = ds*k1x
            dy1 = ds*k1y
            dx2 = ds*0.5_dp*(k1x + k2x)
            dy2 = ds*0.5_dp*(k1y + k2y)
            ! The error is measured in axes coordinates, so that it means the
            ! same thing whatever the grid.
            err = hypot((dx2 - dx1)/real(st%nx - 1, dp), (dy2 - dy1)/real(st%ny - 1, dp))

            if (err < MAXERROR) then
                xi = xi + dx2
                yi = yi + dy2
                ! Leaving the grid ends the line here: matplotlib only takes
                ! the Euler step to the boundary when the trial point of the
                ! step, not the step itself, falls outside.
                if (.not. stream_in_grid(st, xi, yi)) exit
                call stream_claim(st, xi, yi, ok)
                if (.not. ok) exit
                if (tot + ds > MAXLENGTH) exit
                tot = tot + ds
            end if

            if (err == 0.0_dp) then
                ds = maxds
            else
                ds = min(maxds, 0.85_dp*ds*sqrt(MAXERROR/err))
            end if
        end do
    end subroutine stream_rk12

    ! One plain Euler step out to the edge of the grid, so a line that leaves
    ! the field ends on the boundary rather than short of it.
    subroutine stream_euler(st, xs, ys, np, dirn, tot)
        type(stream_t), intent(in) :: st
        real(dp), intent(inout) :: xs(:), ys(:), tot
        integer, intent(inout) :: np
        integer, intent(in) :: dirn
        real(dp) :: xi, yi, cx, cy, dsx, dsy, ds
        integer :: code

        if (np < 1 .or. np >= size(xs)) return
        xi = xs(np)
        yi = ys(np)
        call stream_dir(st, xi, yi, dirn, cx, cy, code)
        if (code /= 0) return
        dsx = huge(1.0_dp)
        dsy = huge(1.0_dp)
        if (cx < 0.0_dp) then
            dsx = xi/(-cx)
        else if (cx > 0.0_dp) then
            dsx = (real(st%nx - 1, dp) - xi)/cx
        end if
        if (cy < 0.0_dp) then
            dsy = yi/(-cy)
        else if (cy > 0.0_dp) then
            dsy = (real(st%ny - 1, dp) - yi)/cy
        end if
        ds = min(dsx, dsy)
        np = np + 1
        xs(np) = xi + cx*ds
        ys(np) = yi + cy*ds
        tot = tot + ds
    end subroutine stream_euler

    ! A two dimensional histogram: count the points into a grid of cells
    ! and hand the counts to pcolormesh, which is how matplotlib draws one.
    subroutine hist2d(x, y, bins, cmap, vmin, vmax)
        real(dp), intent(in) :: x(:), y(:)
        integer, intent(in), optional :: bins(2)
        character(len=*), intent(in), optional :: cmap
        real(dp), intent(in), optional :: vmin, vmax
        integer :: nb(2), n, i, jx, jy
        real(dp) :: x0, x1, y0, y1
        real(dp), allocatable :: xe(:), ye(:), h(:, :)

        call ensure_fig()
        n = min(size(x), size(y))
        if (n < 1) return
        nb = [10, 10]
        if (present(bins)) nb = max(1, bins)

        x0 = minval(x(1:n)); x1 = maxval(x(1:n))
        y0 = minval(y(1:n)); y1 = maxval(y(1:n))
        if (x1 <= x0) x1 = x0 + 1.0_dp
        if (y1 <= y0) y1 = y0 + 1.0_dp

        allocate (xe(nb(1) + 1), ye(nb(2) + 1), h(nb(2), nb(1)))
        do i = 1, nb(1) + 1
            xe(i) = x0 + (x1 - x0)*real(i - 1, dp)/real(nb(1), dp)
        end do
        do i = 1, nb(2) + 1
            ye(i) = y0 + (y1 - y0)*real(i - 1, dp)/real(nb(2), dp)
        end do

        h = 0.0_dp
        do i = 1, n
            ! The last cell owns its right edge, as numpy's histogram does.
            jx = min(nb(1), 1 + int((x(i) - x0)/(x1 - x0)*real(nb(1), dp)))
            jy = min(nb(2), 1 + int((y(i) - y0)/(y1 - y0)*real(nb(2), dp)))
            h(jy, jx) = h(jy, jx) + 1.0_dp
        end do

        call pcolormesh(xe, ye, h, cmap, vmin, vmax)
    end subroutine hist2d

    ! Hexagonal binning. matplotlib lays two rectangular grids over the
    ! data, one offset half a cell from the other, and gives each point to
    ! whichever centre is nearer: those centres are the hexagon centres,
    ! and this is the same arithmetic.
    subroutine hexbin(x, y, gridsize, cmap, mincnt)
        real(dp), intent(in) :: x(:), y(:)
        integer, intent(in), optional :: gridsize, mincnt
        character(len=*), intent(in), optional :: cmap
        integer :: nx, ny, nx1, ny1, nx2, ny2, n, i, j, k, ix1, iy1, ix2, iy2, mc
        real(dp) :: x0, x1, y0, y1, sx, sy, ix, iy, d1, d2, pad, cmax, t, cxp, cyp
        real(dp) :: hx(6), hy(6)
        real(dp), allocatable :: c1(:, :), c2(:, :)

        call ensure_fig()
        n = min(size(x), size(y))
        if (n < 1) return
        nx = 100
        if (present(gridsize)) nx = max(1, gridsize)
        ny = int(real(nx, dp)/sqrt(3.0_dp))
        ny = max(1, ny)
        ! matplotlib draws every cell of the grid, empty ones included, at
        ! the bottom of the colormap. mincnt raises that floor.
        mc = 0
        if (present(mincnt)) mc = mincnt

        x0 = minval(x(1:n)); x1 = maxval(x(1:n))
        y0 = minval(y(1:n)); y1 = maxval(y(1:n))
        pad = 1.0e-9_dp*(x1 - x0)
        x0 = x0 - pad; x1 = x1 + pad
        if (x1 <= x0) x1 = x0 + 1.0_dp
        if (y1 <= y0) y1 = y0 + 1.0_dp
        sx = (x1 - x0)/real(nx, dp)
        sy = (y1 - y0)/real(ny, dp)

        nx1 = nx + 1; ny1 = ny + 1
        nx2 = nx; ny2 = ny
        allocate (c1(nx1, ny1), c2(nx2, ny2))
        c1 = 0.0_dp
        c2 = 0.0_dp

        do i = 1, n
            ix = (x(i) - x0)/sx
            iy = (y(i) - y0)/sy
            ix1 = nint(ix); iy1 = nint(iy)
            ix2 = floor(ix); iy2 = floor(iy)
            d1 = (ix - real(ix1, dp))**2 + 3.0_dp*(iy - real(iy1, dp))**2
            d2 = (ix - real(ix2, dp) - 0.5_dp)**2 + 3.0_dp*(iy - real(iy2, dp) - 0.5_dp)**2
            if (d1 < d2) then
                if (ix1 >= 0 .and. ix1 < nx1 .and. iy1 >= 0 .and. iy1 < ny1) &
                    c1(ix1 + 1, iy1 + 1) = c1(ix1 + 1, iy1 + 1) + 1.0_dp
            else
                if (ix2 >= 0 .and. ix2 < nx2 .and. iy2 >= 0 .and. iy2 < ny2) &
                    c2(ix2 + 1, iy2 + 1) = c2(ix2 + 1, iy2 + 1) + 1.0_dp
            end if
        end do

        cmax = max(maxval(c1), maxval(c2))
        if (cmax <= 0.0_dp) return

        ! The hexagon is a cell wide and two thirds of a cell tall at the
        ! points, which is what makes the two grids interlock.
        hx = 0.5_dp*sx*[1.0_dp, 1.0_dp, 0.0_dp, -1.0_dp, -1.0_dp, 0.0_dp]
        hy = (sy/3.0_dp)*[-0.5_dp, 0.5_dp, 1.0_dp, 0.5_dp, -0.5_dp, -1.0_dp]

        ax(cur_i)%img_cmap = CMAP_VIRIDIS
        if (present(cmap)) ax(cur_i)%img_cmap = cmap_from_str(cmap)
        ax(cur_i)%img_vmin = real(mc, dp)
        ax(cur_i)%img_vmax = cmax
        ax(cur_i)%has_cmap_src = .true.

        do k = 1, 2
            do i = 1, merge(nx1, nx2, k == 1)
                do j = 1, merge(ny1, ny2, k == 1)
                    if (k == 1) then
                        if (c1(i, j) < real(mc, dp)) cycle
                        t = (c1(i, j) - real(mc, dp))/max(cmax - real(mc, dp), 1.0_dp)
                        cxp = x0 + real(i - 1, dp)*sx
                        cyp = y0 + real(j - 1, dp)*sy
                    else
                        if (c2(i, j) < real(mc, dp)) cycle
                        t = (c2(i, j) - real(mc, dp))/max(cmax - real(mc, dp), 1.0_dp)
                        cxp = x0 + (real(i - 1, dp) + 0.5_dp)*sx
                        cyp = y0 + (real(j - 1, dp) + 0.5_dp)*sy
                    end if
                    call add_polygon(cxp + hx, cyp + hy, &
                                     facecolor=cmap_color(ax(cur_i)%img_cmap, t))
                end do
            end do
        end do

        ! Patches never ask for room of their own, so the axes is told what
        ! the binning covered, margins and all.
        call xlim(x0 - 0.05_dp*(x1 - x0), x1 + 0.05_dp*(x1 - x0))
        call ylim(y0 - 0.05_dp*(y1 - y0), y1 + 0.05_dp*(y1 - y0))
    end subroutine hexbin

    ! A grid of coloured cells with edges of the caller's choosing. x and
    ! y are the edges, one more than the samples along that direction; if
    ! they are the same length as the samples they are taken as centres,
    ! which is matplotlib's shading="nearest".
    subroutine pcolormesh(x, y, c, cmap, vmin, vmax, norm, vcenter, gamma, &
                          linthresh)
        real(dp), intent(in) :: x(:), y(:), c(:, :)
        character(len=*), intent(in), optional :: cmap, norm
        real(dp), intent(in), optional :: vmin, vmax, vcenter, gamma, linthresh
        integer :: nr, nc
        real(dp) :: lo, hi

        call ensure_fig()
        nr = size(c, 1)
        nc = size(c, 2)
        if (nr < 1 .or. nc < 1) return
        if (size(x) < nc .or. size(y) < nr) return

        if (allocated(ax(cur_i)%img)) deallocate (ax(cur_i)%img)
        allocate (ax(cur_i)%img(nr, nc))
        ax(cur_i)%img = c
        ax(cur_i)%has_img = .true.
        ax(cur_i)%has_mesh = .true.
        ax(cur_i)%has_cmap_src = .true.
        ax(cur_i)%img_origin_upper = .false.

        if (allocated(ax(cur_i)%mesh_x)) deallocate (ax(cur_i)%mesh_x)
        if (allocated(ax(cur_i)%mesh_y)) deallocate (ax(cur_i)%mesh_y)
        call cell_edges(x, nc, ax(cur_i)%mesh_x)
        call cell_edges(y, nr, ax(cur_i)%mesh_y)

        ax(cur_i)%img_cmap = CMAP_VIRIDIS
        if (present(cmap)) ax(cur_i)%img_cmap = cmap_from_str(cmap)
        ax(cur_i)%img_norm = NORM_LINEAR
        if (present(norm)) ax(cur_i)%img_norm = norm_from_str(norm)
        call norm_params(vcenter, gamma, linthresh)

        if (ax(cur_i)%img_norm == NORM_LOG) then
            lo = huge(1.0_dp)
            if (any(c > 0.0_dp)) lo = minval(c, mask=(c > 0.0_dp))
            hi = maxval(c)
        else
            lo = minval(c)
            hi = maxval(c)
        end if
        if (present(vmin)) lo = vmin
        if (present(vmax)) hi = vmax
        if (hi <= lo) hi = lo + 1.0_dp
        ax(cur_i)%img_vmin = lo
        ax(cur_i)%img_vmax = hi

        ax(cur_i)%img_ext = [minval(ax(cur_i)%mesh_x), maxval(ax(cur_i)%mesh_x), &
                             minval(ax(cur_i)%mesh_y), maxval(ax(cur_i)%mesh_y)]
        ! A mesh does not force a square aspect, unlike an image.
        ax(cur_i)%aspect = 0.0_dp
    end subroutine pcolormesh

    ! matplotlib's pcolor differs from pcolormesh only in how it is drawn,
    ! and both come out of one cell loop here.
    subroutine pcolor(x, y, c, cmap, vmin, vmax, norm)
        real(dp), intent(in) :: x(:), y(:), c(:, :)
        character(len=*), intent(in), optional :: cmap, norm
        real(dp), intent(in), optional :: vmin, vmax
        call pcolormesh(x, y, c, cmap, vmin, vmax, norm)
    end subroutine pcolor

    ! n+1 edges from either the edges themselves or the n cell centres.
    subroutine cell_edges(v, n, e)
        real(dp), intent(in) :: v(:)
        integer, intent(in) :: n
        real(dp), allocatable, intent(out) :: e(:)
        integer :: i

        allocate (e(n + 1))
        if (size(v) >= n + 1) then
            e = v(1:n + 1)
            return
        end if
        do i = 2, n
            e(i) = 0.5_dp*(v(i - 1) + v(i))
        end do
        if (n == 1) then
            e(1) = v(1) - 0.5_dp
            e(2) = v(1) + 0.5_dp
        else
            e(1) = v(1) - 0.5_dp*(v(2) - v(1))
            e(n + 1) = v(n) + 0.5_dp*(v(n) - v(n - 1))
        end if
    end subroutine cell_edges

    ! ------------------------------------------------------------------
    ! 3D axes. matplotlib picks the projection when the axes is created;
    ! with one current axes the same thing is said by turning that axes
    ! into a 3D one, and any of the 3D plotting calls does it implicitly.
    ! ------------------------------------------------------------------

    subroutine axes3d(elev, azim)
        real(dp), intent(in), optional :: elev, azim

        call ensure_fig()
        ax(cur_i)%is3d = .true.
        ! A 3D axes is gridded by default (rcParams axes3d.grid), unlike a
        ! 2D one.
        ax(cur_i)%grid_on = .true.
        if (present(elev)) ax(cur_i)%elev = elev
        if (present(azim)) ax(cur_i)%azim = azim
    end subroutine axes3d

    subroutine view_init(elev, azim)
        real(dp), intent(in), optional :: elev, azim

        call axes3d(elev, azim)
    end subroutine view_init

    subroutine zlabel(s)
        character(len=*), intent(in) :: s

        call ensure_fig()
        ax(cur_i)%zlabel = s
    end subroutine zlabel

    subroutine zlim(lo, hi)
        real(dp), intent(in) :: lo, hi

        call ensure_fig()
        ax(cur_i)%zlim_set = .true.
        ax(cur_i)%zmin_user = lo
        ax(cur_i)%zmax_user = hi
    end subroutine zlim

    subroutine plot3d(x, y, z, fmt, label, lw, color, marker, linestyle, alpha)
        real(dp), intent(in) :: x(:), y(:), z(:)
        character(len=*), intent(in), optional :: fmt, label, color, marker, linestyle
        real(dp), intent(in), optional :: lw, alpha
        integer :: is, n

        call axes3d()
        call add_series(cur_i, x, y, fmt, label, lw, color, marker, linestyle, alpha)
        is = ax(cur_i)%n_series
        if (is < 1) return
        n = min(ax(cur_i)%series(is)%n, size(z))
        ax(cur_i)%series(is)%n = n
        ax(cur_i)%series(is)%kind = SERIES_LINE3D
        allocate (ax(cur_i)%series(is)%z(n))
        ax(cur_i)%series(is)%z(1:n) = z(1:n)
    end subroutine plot3d

    subroutine scatter3d(x, y, z, s, c, marker, label, alpha)
        real(dp), intent(in) :: x(:), y(:), z(:)
        real(dp), intent(in), optional :: s, alpha
        character(len=*), intent(in), optional :: c, marker, label
        integer :: is, n

        call axes3d()
        call scatter(x, y, s, c, marker, label, alpha)
        is = ax(cur_i)%n_series
        if (is < 1) return
        n = min(ax(cur_i)%series(is)%n, size(z))
        ax(cur_i)%series(is)%n = n
        ax(cur_i)%series(is)%kind = SERIES_SCATTER3D
        allocate (ax(cur_i)%series(is)%z(n))
        ax(cur_i)%series(is)%z(1:n) = z(1:n)
    end subroutine scatter3d

    ! z is indexed (row, column) = (y, x), as the 2D grids are.
    subroutine plot_surface(x, y, z, color, alpha, cmap)
        real(dp), intent(in) :: x(:), y(:), z(:, :)
        character(len=*), intent(in), optional :: color, cmap
        real(dp), intent(in), optional :: alpha
        integer :: is, nx, ny

        call axes3d()
        ny = size(z, 1)
        nx = size(z, 2)
        if (nx < 2 .or. ny < 2) return
        if (size(x) < nx .or. size(y) < ny) return
        call push_series(ax(cur_i), is)
        ax(cur_i)%series(is)%kind = SERIES_SURFACE
        ax(cur_i)%series(is)%n = 0
        allocate (ax(cur_i)%series(is)%x(nx), ax(cur_i)%series(is)%y(ny))
        allocate (ax(cur_i)%series(is)%zg(ny, nx))
        ax(cur_i)%series(is)%x(1:nx) = x(1:nx)
        ax(cur_i)%series(is)%y(1:ny) = y(1:ny)
        ax(cur_i)%series(is)%zg = z(1:ny, 1:nx)
        ax(cur_i)%series(is)%color = resolve_color(color)
        if (len_trim(ax(cur_i)%series(is)%color) == 0) then
            ax(cur_i)%series(is)%color = cycle_color(ax(cur_i)%color_cycle)
            ax(cur_i)%color_cycle = ax(cur_i)%color_cycle + 1
        end if
        if (present(cmap)) ax(cur_i)%series(is)%scmap = cmap_from_str(cmap)
        if (present(alpha)) ax(cur_i)%series(is)%alpha = alpha
    end subroutine plot_surface

    ! The same grid, ruled rather than filled. matplotlib draws every line
    ! of the mesh whether it is in front or behind, and so does this.
    subroutine plot_wireframe(x, y, z, color, alpha, lw)
        real(dp), intent(in) :: x(:), y(:), z(:, :)
        character(len=*), intent(in), optional :: color
        real(dp), intent(in), optional :: alpha, lw
        integer :: is

        call plot_surface(x, y, z, color, alpha)
        is = ax(cur_i)%n_series
        if (is < 1) return
        ax(cur_i)%series(is)%wire = .true.
        if (present(lw)) ax(cur_i)%series(is)%linewidth = lw
    end subroutine plot_wireframe

    ! ----------------------------------------------------------------------
    ! Scattered points, triangulated. matplotlib triangulates with Qhull
    ! and draws the result with the ordinary artists; so does this, which
    ! is why these are so short.
    ! ----------------------------------------------------------------------

    ! The edges of the triangulation, as one line broken at every jump,
    ! which is how matplotlib draws it too.
    subroutine triplot(x, y, color, lw, linestyle, marker, label)
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: color, linestyle, marker, label
        real(dp), intent(in), optional :: lw
        integer, allocatable :: tri(:, :)
        real(dp), allocatable :: ex(:), ey(:)
        integer :: nt, i, k
        real(dp) :: nan, zero

        call ensure_fig()
        call delaunay(x, y, tri, nt)
        if (nt < 1) return
        ! The break between one triangle and the next, which plot draws as
        ! a gap just as matplotlib does.
        zero = 0.0_dp
        nan = zero/zero
        ! Four points and a break for each triangle: round it and back to
        ! the start.
        allocate (ex(5*nt), ey(5*nt))
        k = 0
        do i = 1, nt
            ex(k + 1:k + 3) = x(tri(:, i))
            ey(k + 1:k + 3) = y(tri(:, i))
            ex(k + 4) = x(tri(1, i))
            ey(k + 4) = y(tri(1, i))
            ex(k + 5) = nan
            ey(k + 5) = nan
            k = k + 5
        end do
        call plot(ex, ey, color=color, lw=lw, linestyle=linestyle, label=label)
        ! matplotlib draws the points as a second artist, which is why
        ! they come out in the next colour of the cycle.
        if (present(marker)) &
            call plot(x, y, color=color, marker=marker, linestyle="none")
    end subroutine triplot

    ! A flat-shaded triangle per triangle, coloured by the mean of its
    ! three values, which is matplotlib's default shading.
    subroutine tripcolor(x, y, z, cmap, vmin, vmax, edgecolor, alpha)
        real(dp), intent(in) :: x(:), y(:), z(:)
        character(len=*), intent(in), optional :: cmap, edgecolor
        real(dp), intent(in), optional :: vmin, vmax, alpha
        integer, allocatable :: tri(:, :)
        integer :: nt, i, cm
        real(dp) :: lo, hi, v, tx(3), ty(3)
        real(dp), allocatable :: fv(:)

        call ensure_fig()
        call delaunay(x, y, tri, nt)
        if (nt < 1) return
        cm = CMAP_VIRIDIS
        if (present(cmap)) cm = cmap_from_str(cmap)
        ! Flat shading colours a triangle by the mean of its corners, and
        ! it is those means, not the values themselves, that the colours
        ! and the colorbar are scaled to.
        allocate (fv(nt))
        do i = 1, nt
            fv(i) = sum(z(tri(:, i)))/3.0_dp
        end do
        lo = minval(fv)
        hi = maxval(fv)
        if (present(vmin)) lo = vmin
        if (present(vmax)) hi = vmax
        if (hi <= lo) hi = lo + 1.0_dp
        do i = 1, nt
            tx = x(tri(:, i))
            ty = y(tri(:, i))
            v = fv(i)
            call add_polygon(tx, ty, &
                             facecolor=cmap_color(cm, (v - lo)/(hi - lo)), &
                             edgecolor=edgecolor, alpha=alpha)
            ! Unlike a lone patch, a mesh cell is data and sets the limits,
            ! and its seam with the next cell must not show.
            ax(cur_i)%series(ax(cur_i)%n_series)%patch_scales = .true.
            ax(cur_i)%series(ax(cur_i)%n_series)%patch_seal = .true.
        end do
        ! What the colorbar reads.
        ax(cur_i)%has_cmap_src = .true.
        ax(cur_i)%img_cmap = cm
        ax(cur_i)%img_vmin = lo
        ax(cur_i)%img_vmax = hi
    end subroutine tripcolor

    subroutine contour(z, levels, cmap, extent)
        real(dp), intent(in) :: z(:, :)
        real(dp), intent(in), optional :: levels(:), extent(4)
        character(len=*), intent(in), optional :: cmap
        call add_contour(z, levels, cmap, extent, .false.)
    end subroutine contour

    ! Write each level into its own contour line. There is nothing to
    ! label until the lines are laid out in points, so this only records
    ! the wish and the drawing code does the work.
    subroutine clabel(fontsize)
        real(dp), intent(in), optional :: fontsize

        call ensure_fig()
        ax(cur_i)%cont_labels = .true.
        if (present(fontsize)) ax(cur_i)%clab_size = fontsize
    end subroutine clabel

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
        character(len=8) :: w
        real(dp), allocatable :: sx(:), sy(:)

        w = "pre"
        if (present(where)) w = where
        call stair_points(x, y, w, sx, sy)
        if (size(sx) == 0) return
        call plot(sx, sy, label=label, color=color, lw=lw, &
                  linestyle=linestyle, alpha=alpha)
    end subroutine step

    ! matplotlib spells the same three shapes two ways: where= for step and
    ! drawstyle= for plot.
    pure function step_where(drawstyle) result(w)
        character(len=*), intent(in) :: drawstyle
        character(len=8) :: w
        select case (drawstyle)
        case ("steps-post", "steps"); w = "post"
        case ("steps-mid"); w = "mid"
        case default; w = "pre"
        end select
    end function step_where

    ! Each sample contributes two points: the tread of its step and the
    ! riser to the next one. Where the riser sits is what `where` selects.
    pure subroutine stair_points(x, y, where, sx, sy)
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in) :: where
        real(dp), allocatable, intent(out) :: sx(:), sy(:)
        integer :: n, i, k

        n = min(size(x), size(y))
        if (n <= 0) then
            allocate (sx(0), sy(0))
            return
        end if
        allocate (sx(2*n), sy(2*n))
        k = 0
        do i = 1, n
            if (trim(where) == "mid") then
                if (i == 1) then
                    sx(k + 1) = x(1)
                else
                    sx(k + 1) = 0.5_dp*(x(i - 1) + x(i))
                end if
                if (i == n) then
                    sx(k + 2) = x(n)
                else
                    sx(k + 2) = 0.5_dp*(x(i) + x(i + 1))
                end if
                sy(k + 1) = y(i)
                sy(k + 2) = y(i)
            else if (trim(where) == "post") then
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
    end subroutine stair_points

    ! Horizontal bars: y locates each bar and width is its length.
    subroutine barh_num(y, width, height, color, label, alpha, left, colors, &
                        edgecolor, linewidth, hatch)
        real(dp), intent(in) :: y(:), width(:)
        real(dp), intent(in), optional :: height, alpha, left(:), linewidth
        character(len=*), intent(in), optional :: color, label, edgecolor, hatch
        character(len=*), intent(in), optional :: colors(:)
        integer :: is

        call ensure_fig()
        is = new_shape_series(SERIES_BARH, y, width, color, label, alpha)
        if (is < 1) return
        if (present(height)) ax(cur_i)%series(is)%width = height
        call bar_options(is, left, colors, edgecolor, linewidth, hatch)
    end subroutine barh_num

    subroutine barh_cat(cats, width, height, color, label, alpha, left, &
                        colors, edgecolor, linewidth, hatch)
        character(len=*), intent(in) :: cats(:)
        real(dp), intent(in) :: width(:)
        real(dp), intent(in), optional :: height, alpha, left(:), linewidth
        character(len=*), intent(in), optional :: color, label, edgecolor, hatch
        character(len=*), intent(in), optional :: colors(:)
        call barh_num(category_positions(size(cats)), width, height, color, &
                      label, alpha, left, colors, edgecolor, linewidth, hatch)
        call yticks(category_positions(size(cats)), cats)
    end subroutine barh_cat

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

    ! Turn the current axes into a polar one: angle along x, radius along y.
    subroutine set_polar()
        call ensure_fig()
        ax(cur_i)%polar = .true.
        ! A polar axes carries its grid unless the caller says otherwise,
        ! which is how matplotlib draws one.
        ax(cur_i)%grid_on = .true.
    end subroutine set_polar

    ! pylab's polar(): make the axes polar and plot on it in one call.
    subroutine polar(theta, r, color, label, lw, linestyle, marker, alpha)
        real(dp), intent(in) :: theta(:), r(:)
        character(len=*), intent(in), optional :: color, label, linestyle, marker
        real(dp), intent(in), optional :: lw, alpha

        call ensure_fig()
        call set_polar()
        call plot(theta, r, color=color, label=label, lw=lw, &
                  linestyle=linestyle, marker=marker, alpha=alpha)
    end subroutine polar

    ! ----------------------------------------------------------------------
    ! Patches: plain shapes in data coordinates. Each one is kept as the
    ! ring of points it comes down to, so a circle drawn on axes of
    ! different scales leans exactly as matplotlib's does.
    ! ----------------------------------------------------------------------

    subroutine add_rectangle(xy, width, height, angle, facecolor, edgecolor, &
                             lw, alpha, fill, hatch)
        real(dp), intent(in) :: xy(2), width, height
        real(dp), intent(in), optional :: angle, lw, alpha
        character(len=*), intent(in), optional :: facecolor, edgecolor, hatch
        logical, intent(in), optional :: fill
        real(dp) :: px(4), py(4), rx(4), ry(4), a, ca, sa
        integer :: i

        px = [0.0_dp, width, width, 0.0_dp]
        py = [0.0_dp, 0.0_dp, height, height]
        a = 0.0_dp
        if (present(angle)) a = angle
        ! A rectangle turns about the corner it is given, as matplotlib's does.
        ca = cos(a*PI/180.0_dp)
        sa = sin(a*PI/180.0_dp)
        do i = 1, 4
            rx(i) = xy(1) + px(i)*ca - py(i)*sa
            ry(i) = xy(2) + px(i)*sa + py(i)*ca
        end do
        call add_polygon(rx, ry, facecolor, edgecolor, lw, alpha, fill, hatch)
    end subroutine add_rectangle

    subroutine add_circle(xy, radius, facecolor, edgecolor, lw, alpha, fill)
        real(dp), intent(in) :: xy(2), radius
        real(dp), intent(in), optional :: lw, alpha
        character(len=*), intent(in), optional :: facecolor, edgecolor
        logical, intent(in), optional :: fill
        call add_ellipse(xy, 2.0_dp*radius, 2.0_dp*radius, 0.0_dp, &
                         facecolor, edgecolor, lw, alpha, fill)
    end subroutine add_circle

    subroutine add_ellipse(xy, width, height, angle, facecolor, edgecolor, lw, alpha, fill)
        real(dp), intent(in) :: xy(2), width, height
        real(dp), intent(in), optional :: angle, lw, alpha
        character(len=*), intent(in), optional :: facecolor, edgecolor
        logical, intent(in), optional :: fill
        integer, parameter :: N = 72
        real(dp) :: px(N), py(N), t, cx, cy, a, ca, sa
        integer :: i

        a = 0.0_dp
        if (present(angle)) a = angle
        ca = cos(a*PI/180.0_dp)
        sa = sin(a*PI/180.0_dp)
        do i = 1, N
            t = 2.0_dp*PI*real(i - 1, dp)/real(N, dp)
            cx = 0.5_dp*width*cos(t)
            cy = 0.5_dp*height*sin(t)
            px(i) = xy(1) + cx*ca - cy*sa
            py(i) = xy(2) + cx*sa + cy*ca
        end do
        call add_polygon(px, py, facecolor, edgecolor, lw, alpha, fill)
    end subroutine add_ellipse

    subroutine add_polygon(x, y, facecolor, edgecolor, lw, alpha, fill, hatch)
        real(dp), intent(in) :: x(:), y(:)
        real(dp), intent(in), optional :: lw, alpha
        character(len=*), intent(in), optional :: facecolor, edgecolor, hatch
        logical, intent(in), optional :: fill
        integer :: is

        call ensure_fig()
        is = new_shape_series(SERIES_PATCH, x, y, facecolor, alpha=alpha)
        if (is == 0) return
        ! A patch does not take a turn of the color cycle: matplotlib gives
        ! every one of them the same first color unless told otherwise.
        if (.not. present(facecolor)) then
            ax(cur_i)%series(is)%color = "#1f77b4"
            ax(cur_i)%color_cycle = ax(cur_i)%color_cycle - 1
        end if
        ax(cur_i)%series(is)%edgecolor = ""
        if (present(edgecolor)) ax(cur_i)%series(is)%edgecolor = resolve_color(edgecolor)
        ax(cur_i)%series(is)%edgewidth = 1.0_dp
        if (present(lw)) ax(cur_i)%series(is)%edgewidth = lw
        if (present(fill)) ax(cur_i)%series(is)%patch_fill = fill
        if (present(hatch)) ax(cur_i)%series(is)%hatch = hatch
        if (present(edgecolor)) ax(cur_i)%series(is)%hcolor = resolve_color(edgecolor)
    end subroutine add_polygon

    ! matplotlib's Arrow patch: the same unit outline it uses, scaled to the
    ! length of (dx, dy) and to `width` across, then turned to point along
    ! the vector. It is built in data coordinates, as matplotlib builds it,
    ! so an axes that is not square shears the arrow in the same way.
    subroutine add_arrow(x, y, dx, dy, width, facecolor, edgecolor, lw, alpha, fill)
        real(dp), intent(in) :: x, y, dx, dy
        real(dp), intent(in), optional :: width, lw, alpha
        character(len=*), intent(in), optional :: facecolor, edgecolor
        logical, intent(in), optional :: fill
        real(dp), parameter :: UX(7) = [0.0_dp, 0.0_dp, 0.8_dp, 0.8_dp, &
                                        1.0_dp, 0.8_dp, 0.8_dp]
        real(dp), parameter :: UY(7) = [0.1_dp, -0.1_dp, -0.1_dp, -0.3_dp, &
                                        0.0_dp, 0.3_dp, 0.1_dp]
        real(dp) :: w, ln, ct, st, ax_(7), ay(7), u, v
        integer :: j

        w = 1.0_dp
        if (present(width)) w = width
        ln = hypot(dx, dy)
        if (ln <= 0.0_dp) return
        ct = dx/ln
        st = dy/ln
        do j = 1, 7
            u = UX(j)*ln
            v = UY(j)*w
            ax_(j) = x + ct*u - st*v
            ay(j) = y + st*u + ct*v
        end do
        call add_polygon(ax_, ay, facecolor, edgecolor, lw, alpha, fill)
    end subroutine add_arrow

    ! An arbitrary path, as matplotlib's PathPatch draws one. `codes` has a
    ! letter per verb: M moves, L draws a line, C draws a cubic from the
    ! next three points and Z closes back to the last move.
    subroutine add_path(x, y, codes, facecolor, edgecolor, lw, alpha, fill)
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in) :: codes
        real(dp), intent(in), optional :: lw, alpha
        character(len=*), intent(in), optional :: facecolor, edgecolor
        logical, intent(in), optional :: fill
        integer :: is, j, np, nv
        integer :: v(len_trim(codes))

        nv = len_trim(codes)
        np = 0
        do j = 1, nv
            select case (codes(j:j))
            case ("M", "m")
                v(j) = VERB_MOVE
                np = np + 1
            case ("L", "l")
                v(j) = VERB_LINE
                np = np + 1
            case ("C", "c")
                v(j) = VERB_CUBIC
                np = np + 3
            case ("Z", "z")
                v(j) = VERB_CLOSE
            case default
                print *, "fplot: add_path: unknown code ", codes(j:j)
                error stop
            end select
        end do
        if (np < 2 .or. np > min(size(x), size(y))) then
            print *, "fplot: add_path: codes need", np, "points, given", &
                min(size(x), size(y))
            error stop
        end if

        call add_polygon(x(1:np), y(1:np), facecolor, edgecolor, lw, alpha, fill)
        is = ax(cur_i)%n_series
        if (is < 1) return
        allocate (ax(cur_i)%series(is)%pverb(nv))
        ax(cur_i)%series(is)%pverb = v
    end subroutine add_path

    ! A field of arrows, one per point, pointing along (u, v). Left to
    ! itself matplotlib sizes the arrows from the field: the shaft is a
    ! fixed fraction of the axes width and the scale is set so a vector of
    ! average length draws an arrow of a comfortable size.
    subroutine quiver(x, y, u, v, color, scale, width, label)
        real(dp), intent(in) :: x(:), y(:), u(:), v(:)
        character(len=*), intent(in), optional :: color, label
        real(dp), intent(in), optional :: scale, width
        integer :: is, n

        call ensure_fig()
        n = min(size(x), size(y), size(u), size(v))
        if (n <= 0) return
        is = new_shape_series(SERIES_QUIVER, x(1:n), y(1:n), color, label)
        if (is == 0) return
        allocate (ax(cur_i)%series(is)%qu(n), ax(cur_i)%series(is)%qv(n))
        ax(cur_i)%series(is)%qu(1:n) = u(1:n)
        ax(cur_i)%series(is)%qv(1:n) = v(1:n)
        ! Arrows are black unless asked otherwise; they do not take a turn
        ! of the color cycle in matplotlib either.
        if (.not. present(color)) then
            ax(cur_i)%series(is)%color = "#000000"
            ax(cur_i)%color_cycle = ax(cur_i)%color_cycle - 1
        end if
        if (present(scale)) ax(cur_i)%series(is)%qscale = scale
        if (present(width)) ax(cur_i)%series(is)%qwidth = width
    end subroutine quiver

    ! Pie chart. matplotlib turns the axes into a unit square centred on the
    ! origin and hides the frame, so the wedges are plain data-space geometry.
    subroutine pie(values, labels, cmap, explode, startangle, counterclock, &
                   autopct, pctdistance, labeldistance, radius, colors, &
                   edgecolor, linewidth)
        real(dp), intent(in) :: values(:)
        character(len=*), intent(in), optional :: labels(:), cmap, autopct
        character(len=*), intent(in), optional :: colors(:), edgecolor
        real(dp), intent(in), optional :: explode(:), startangle, linewidth
        real(dp), intent(in), optional :: pctdistance, labeldistance, radius
        logical, intent(in), optional :: counterclock
        integer :: is, i, n
        real(dp) :: rad, pd, ld, off

        call ensure_fig()
        n = size(values)
        if (n <= 0) return
        if (any(values < 0.0_dp) .or. sum(values) <= 0.0_dp) return

        is = new_shape_series(SERIES_PIE, values, values)
        if (is < 1) return
        allocate (ax(cur_i)%series(is)%pcolor(n))
        do i = 1, n
            if (present(colors)) then
                ax(cur_i)%series(is)%pcolor(i) = &
                    resolve_color(colors(mod(i - 1, size(colors)) + 1))
            else if (present(cmap)) then
                ax(cur_i)%series(is)%pcolor(i) = &
                    cmap_color(cmap_from_str(cmap), real(i - 1, dp) / real(max(n - 1, 1), dp))
            else
                ax(cur_i)%series(is)%pcolor(i) = cycle_color(i - 1)
            end if
        end do

        if (present(edgecolor)) &
            ax(cur_i)%series(is)%hcolor = resolve_color(edgecolor)
        if (present(linewidth)) ax(cur_i)%series(is)%linewidth = linewidth
        rad = 1.0_dp
        if (present(radius)) rad = radius
        ax(cur_i)%series(is)%pie_radius = rad
        if (present(startangle)) &
            ax(cur_i)%series(is)%pie_start = startangle*PI/180.0_dp
        if (present(counterclock)) ax(cur_i)%series(is)%pie_ccw = counterclock
        allocate (ax(cur_i)%series(is)%pexp(n))
        ax(cur_i)%series(is)%pexp = 0.0_dp
        if (present(explode)) then
            do i = 1, min(n, size(explode))
                ax(cur_i)%series(is)%pexp(i) = explode(i)
            end do
        end if

        ! matplotlib measures both distances in radii from the centre of
        ! the wedge, which is itself pushed out by explode.
        ld = 1.1_dp
        pd = 0.6_dp
        if (present(labeldistance)) ld = labeldistance
        if (present(pctdistance)) pd = pctdistance
        do i = 1, n
            off = ax(cur_i)%series(is)%pexp(i)
            if (present(labels)) then
                if (i <= size(labels)) &
                    call add_pie_text(ax(cur_i)%series(is), values, i, labels(i), &
                                      ld*rad, off*rad)
            end if
            if (present(autopct)) &
                call add_pie_text(ax(cur_i)%series(is), values, i, &
                                  pct_text(autopct, 100.0_dp*values(i)/sum(values)), &
                                  pd*rad, off*rad)
        end do

        call xlim(-1.25_dp, 1.25_dp)
        call ylim(-1.25_dp, 1.25_dp)
        ax(cur_i)%aspect = 1.0_dp
        ax(cur_i)%frame_off = .true.
    end subroutine pie

    ! The mid angle of wedge i, honouring where the pie starts and which
    ! way round it runs. Both the wedges and their labels are placed from
    ! here, so the two cannot drift apart.
    pure function pie_mid(s, values, i) result(mid)
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: values(:)
        integer, intent(in) :: i
        real(dp) :: mid, tot, dir

        tot = sum(values)
        dir = 1.0_dp
        if (.not. s%pie_ccw) dir = -1.0_dp
        mid = s%pie_start + dir*PI*(sum(values(1:i - 1)) + sum(values(1:i)))/tot
    end function pie_mid

    ! Place one wedge's text at the given distance out along its mid angle,
    ! measured from the centre the wedge itself sits on.
    subroutine add_pie_text(s, values, i, lab, dist, off)
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: values(:), dist, off
        integer, intent(in) :: i
        character(len=*), intent(in) :: lab
        real(dp) :: mid
        integer :: it

        mid = pie_mid(s, values, i)
        call push_text(ax(cur_i), it)
        ax(cur_i)%texts(it)%x = (dist + off)*cos(mid)
        ax(cur_i)%texts(it)%y = (dist + off)*sin(mid)
        ax(cur_i)%texts(it)%s = lab
        ax(cur_i)%texts(it)%ha = "center"
        ax(cur_i)%texts(it)%va = "center"
    end subroutine add_pie_text

    ! matplotlib's autopct format string, for the printf forms a pie chart
    ! actually uses: %f with an optional width and precision, and %% for a
    ! literal per cent sign. Anything else is copied out as it stands.
    function pct_text(fmt, v) result(out)
        character(len=*), intent(in) :: fmt
        real(dp), intent(in) :: v
        character(len=64) :: out
        character(len=32) :: num, spec
        integer :: i, j, dec, ios

        out = ""
        j = 0
        i = 1
        do while (i <= len_trim(fmt))
            if (fmt(i:i) /= "%") then
                j = j + 1
                out(j:j) = fmt(i:i)
                i = i + 1
                cycle
            end if
            if (i < len_trim(fmt)) then
                if (fmt(i + 1:i + 1) == "%") then
                    j = j + 1
                    out(j:j) = "%"
                    i = i + 2
                    cycle
                end if
            end if
            ! A conversion: read up to the terminating letter, then take
            ! the precision out of it.
            spec = ""
            i = i + 1
            do while (i <= len_trim(fmt))
                spec = trim(spec)//fmt(i:i)
                if (index("fFeEgG", fmt(i:i)) > 0) exit
                i = i + 1
            end do
            i = i + 1
            dec = 6
            if (index(spec, ".") > 0) then
                read (spec(index(spec, ".") + 1:len_trim(spec) - 1), *, iostat=ios) dec
                if (ios /= 0) dec = 1
            else
                dec = 0
            end if
            write (spec, "(I0)") max(0, dec)
            write (num, "(f0."//trim(spec)//")") v
            ! A value below one writes as ".5" without the leading zero.
            if (num(1:1) == ".") num = "0"//trim(num)
            out(j + 1:) = trim(num)
            j = j + len_trim(num)
        end do
    end function pct_text

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
            ax(cur_i)%xmargin = 0.0_dp
            ax(cur_i)%ymargin = 0.0_dp
        case ("auto")
            ax(cur_i)%aspect = 0.0_dp
            ax(cur_i)%xmargin = 0.05_dp
            ax(cur_i)%ymargin = 0.05_dp
        case default
            error stop "fplot: unknown axis mode"
        end select
    end subroutine axis

    ! matplotlib's margins(): the room left past the data, as a fraction of
    ! the drawn length. One value sets both axes, x= and y= one each.
    subroutine margins(m, x, y)
        real(dp), intent(in), optional :: m, x, y
        call ensure_fig()
        if (present(m)) then
            ax(cur_i)%xmargin = m
            ax(cur_i)%ymargin = m
        end if
        if (present(x)) ax(cur_i)%xmargin = x
        if (present(y)) ax(cur_i)%ymargin = y
    end subroutine margins

    ! matplotlib's autoscale(). enable=.false. pins the limits where the
    ! data has put them so far, which is the only way to stop an axis from
    ! growing with what is drawn next; enable=.true. hands it back to the
    ! data. tight= drops the margin, or puts the usual five percent back.
    subroutine autoscale(enable, axis, tight)
        logical, intent(in), optional :: enable, tight
        character(len=*), intent(in), optional :: axis
        logical :: dox, doy, on
        real(dp) :: xmn, xmx, ymn, ymx

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
                error stop "fplot: autoscale axis must be x, y or both"
            end select
        end if

        if (present(tight)) then
            if (tight) then
                if (dox) ax(cur_i)%xmargin = 0.0_dp
                if (doy) ax(cur_i)%ymargin = 0.0_dp
            else
                if (dox) ax(cur_i)%xmargin = 0.05_dp
                if (doy) ax(cur_i)%ymargin = 0.05_dp
            end if
        end if

        on = .true.
        if (present(enable)) on = enable
        if (on) then
            if (dox) ax(cur_i)%xlim_set = .false.
            if (doy) ax(cur_i)%ylim_set = .false.
            return
        end if
        call compute_limits(ax(cur_i), xmn, xmx, ymn, ymx)
        if (dox) then
            ax(cur_i)%xmin_user = xmn
            ax(cur_i)%xmax_user = xmx
            ax(cur_i)%xlim_set = .true.
        end if
        if (doy) then
            ax(cur_i)%ymin_user = ymn
            ax(cur_i)%ymax_user = ymx
            ax(cur_i)%ylim_set = .true.
        end if
    end subroutine autoscale

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
    ! by passing matplotlib's list; datasets of one size can go in together
    ! as a matrix, a row to each.
    subroutine boxplot_one(y, position, width, color, label, vert, notch, &
                           showmeans, patch_artist, whis)
        real(dp), intent(in) :: y(:)
        real(dp), intent(in), optional :: position, width, whis
        character(len=*), intent(in), optional :: color, label
        logical, intent(in), optional :: vert, notch, showmeans, patch_artist
        integer :: is

        call add_dist_series(SERIES_BOX, y, position, width, color, label, 0.15_dp)
        is = ax(cur_i)%n_series
        if (is < 1) return
        if (present(vert)) ax(cur_i)%series(is)%box_vert = vert
        if (present(notch)) ax(cur_i)%series(is)%box_notch = notch
        if (present(showmeans)) ax(cur_i)%series(is)%box_mean = showmeans
        if (present(whis)) ax(cur_i)%series(is)%whis = whis
        if (present(patch_artist)) then
            ax(cur_i)%series(is)%box_fill = patch_artist
            ! A filled box takes the first cycle colour, as matplotlib's
            ! patch_artist boxes do, and keeps its black furniture.
            if (patch_artist .and. .not. present(color)) &
                ax(cur_i)%series(is)%hcolor = cycle_color(0)
        end if
    end subroutine boxplot_one

    ! A row per dataset. labels name the categories along the axis the
    ! boxes are ranged on.
    subroutine boxplot_many(y, labels, positions, width, color, vert, notch, &
                            showmeans, patch_artist, whis)
        real(dp), intent(in) :: y(:, :)
        character(len=*), intent(in), optional :: labels(:), color
        real(dp), intent(in), optional :: positions(:), width, whis
        logical, intent(in), optional :: vert, notch, showmeans, patch_artist
        integer :: k, nk
        real(dp), allocatable :: pos(:)
        logical :: up

        call ensure_fig()
        nk = size(y, 1)
        if (nk < 1) return
        allocate (pos(nk))
        do k = 1, nk
            pos(k) = real(k, dp)
            if (present(positions)) then
                if (k <= size(positions)) pos(k) = positions(k)
            end if
            call boxplot_one(y(k, :), pos(k), width, color, vert=vert, &
                             notch=notch, showmeans=showmeans, &
                             patch_artist=patch_artist, whis=whis)
        end do
        if (present(labels)) then
            up = .true.
            if (present(vert)) up = vert
            if (up) then
                call xticks(pos, labels)
            else
                call yticks(pos, labels)
            end if
        end if
    end subroutine boxplot_many

    subroutine violinplot_one(y, position, width, color, label, vert, showmeans, &
                              showmedians, showextrema)
        real(dp), intent(in) :: y(:)
        real(dp), intent(in), optional :: position, width
        character(len=*), intent(in), optional :: color, label
        logical, intent(in), optional :: vert, showmeans, showmedians, showextrema
        integer :: is

        call add_dist_series(SERIES_VIOLIN, y, position, width, color, label, 0.5_dp)
        is = ax(cur_i)%n_series
        if (is < 1) return
        if (present(vert)) ax(cur_i)%series(is)%box_vert = vert
        if (present(showmeans)) ax(cur_i)%series(is)%box_mean = showmeans
        ! matplotlib draws the extrema bars unless told not to, and the
        ! median only when asked.
        if (present(showmedians)) ax(cur_i)%series(is)%box_notch = showmedians
        if (present(showextrema)) ax(cur_i)%series(is)%box_fill = .not. showextrema
    end subroutine violinplot_one

    subroutine violinplot_many(y, labels, positions, width, color, vert, &
                               showmeans, showmedians, showextrema)
        real(dp), intent(in) :: y(:, :)
        character(len=*), intent(in), optional :: labels(:), color
        real(dp), intent(in), optional :: positions(:), width
        logical, intent(in), optional :: vert, showmeans, showmedians, showextrema
        integer :: k, nk
        real(dp), allocatable :: pos(:)
        logical :: up

        call ensure_fig()
        nk = size(y, 1)
        if (nk < 1) return
        allocate (pos(nk))
        do k = 1, nk
            pos(k) = real(k, dp)
            if (present(positions)) then
                if (k <= size(positions)) pos(k) = positions(k)
            end if
            call violinplot_one(y(k, :), pos(k), width, color, vert=vert, &
                                showmeans=showmeans, showmedians=showmedians, &
                                showextrema=showextrema)
        end do
        if (present(labels)) then
            up = .true.
            if (present(vert)) up = vert
            if (up) then
                call xticks(pos, labels)
            else
                call yticks(pos, labels)
            end if
        end if
    end subroutine violinplot_many

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
                ax(cur_i)%series(is)%color = cycle_color(0)
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

    subroutine colorbar(label, orientation, fraction, pad, shrink, aspect)
        character(len=*), intent(in), optional :: label, orientation
        real(dp), intent(in), optional :: fraction, pad, shrink, aspect
        real(dp) :: keep
        call ensure_fig()
        if (.not. ax(cur_i)%has_cmap_src) return
        if (ax(cur_i)%cbar_on) return
        ax(cur_i)%cbar_on = .true.
        if (present(label)) ax(cur_i)%cbar_label = label
        if (present(orientation)) then
            select case (lower(orientation))
            case ("vertical")
            case ("horizontal")
                ax(cur_i)%cbar_horiz = .true.
                ! matplotlib leaves more room under a horizontal bar,
                ! because its tick labels sit under it too.
                ax(cur_i)%cbar_pad = 0.15_dp
            case default
                error stop "fplot: colorbar orientation must be vertical or horizontal"
            end select
        end if
        if (present(fraction)) ax(cur_i)%cbar_frac = fraction
        if (present(pad)) ax(cur_i)%cbar_pad = pad
        if (present(shrink)) ax(cur_i)%cbar_shrink = shrink
        if (present(aspect)) ax(cur_i)%cbar_aspect = aspect

        ! The bar is cut out of the axes box, as in matplotlib: the axes
        ! keeps what is left of the width, or of the height.
        keep = 1.0_dp - ax(cur_i)%cbar_frac - ax(cur_i)%cbar_pad
        if (ax(cur_i)%cbar_horiz) then
            ax(cur_i)%bottom = ax(cur_i)%top - keep * (ax(cur_i)%top - ax(cur_i)%bottom)
        else
            ax(cur_i)%right = ax(cur_i)%left + keep * (ax(cur_i)%right - ax(cur_i)%left)
        end if
    end subroutine colorbar

    ! Where the bar is drawn, in canvas points. The axes was already shrunk
    ! to make room for it, and the fractions are measured against the box it
    ! was cut from, so that box has to be recovered first.
    subroutine cbar_box(a, W, H, bx, by, bw, bh)
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: W, H
        real(dp), intent(out) :: bx, by, bw, bh
        real(dp) :: keep, l0, w0, b0, h0, full

        keep = 1.0_dp - a%cbar_frac - a%cbar_pad
        if (keep < 0.05_dp) keep = 0.05_dp
        if (a%cbar_horiz) then
            h0 = (a%top - a%bottom) / keep
            b0 = a%top - h0
            full = (a%right - a%left) * W
            bw = a%cbar_shrink * full
            bx = a%left * W + 0.5_dp * (full - bw)
            bh = bw / a%cbar_aspect
            ! The bar hangs from the top of the strip it was given.
            by = H - (b0 + a%cbar_frac * h0) * H
        else
            l0 = a%left * W
            w0 = (a%right * W - l0) / keep
            full = (a%top - a%bottom) * H
            bh = a%cbar_shrink * full
            bx = l0 + (1.0_dp - a%cbar_frac) * w0
            by = (1.0_dp - a%top) * H + 0.5_dp * (full - bh)
            bw = bh / a%cbar_aspect
        end if
    end subroutine cbar_box

    ! Vertical bars of the given heights, centred on x and drawn from y = 0.
    ! bottom stacks this series on top of another; colors gives every bar
    ! its own color, as matplotlib's list-valued color does.
    subroutine bar_num(x, height, width, color, label, alpha, bottom, colors, &
                       edgecolor, linewidth, hatch, yerr, align, tick_label, &
                       ecolor, capsize)
        real(dp), intent(in) :: x(:), height(:)
        real(dp), intent(in), optional :: width, alpha, bottom(:), linewidth
        real(dp), intent(in), optional :: yerr(:), capsize
        character(len=*), intent(in), optional :: color, label, edgecolor, hatch
        character(len=*), intent(in), optional :: colors(:), align, ecolor
        character(len=*), intent(in), optional :: tick_label(:)
        real(dp), allocatable :: pos(:)
        integer :: is, n

        call ensure_fig()
        n = min(size(x), size(height))
        allocate (pos(n))
        pos = x(1:n)
        ! align="edge" measures from the left edge of the bar rather than
        ! from its middle, so the positions move by half a width.
        if (present(align)) then
            if (align == "edge") then
                if (present(width)) then
                    pos = pos + 0.5_dp*width
                else
                    pos = pos + 0.4_dp
                end if
            end if
        end if
        is = new_shape_series(SERIES_BAR, pos, height, color, label, alpha)
        if (is < 1) return
        if (present(width)) ax(cur_i)%series(is)%width = width
        call bar_options(is, bottom, colors, edgecolor, linewidth, hatch)
        if (present(yerr)) then
            call set_arm(ax(cur_i)%series(is)%eylo, n, yerr)
            call set_arm(ax(cur_i)%series(is)%eyhi, n, yerr)
            ! matplotlib draws bar error bars in black, with no caps unless
            ! a capsize is asked for.
            ax(cur_i)%series(is)%ecolor = "#000000"
            ax(cur_i)%series(is)%ecap = 0.0_dp
            if (present(ecolor)) ax(cur_i)%series(is)%ecolor = resolve_color(ecolor)
            if (present(capsize)) ax(cur_i)%series(is)%ecap = capsize
        end if
        if (present(tick_label)) call xticks(pos, tick_label)
    end subroutine bar_num

    subroutine bar_cat(cats, height, width, color, label, alpha, bottom, &
                       colors, edgecolor, linewidth, hatch)
        character(len=*), intent(in) :: cats(:)
        real(dp), intent(in) :: height(:)
        real(dp), intent(in), optional :: width, alpha, bottom(:), linewidth
        character(len=*), intent(in), optional :: color, label, edgecolor, hatch
        character(len=*), intent(in), optional :: colors(:)
        call bar_num(category_positions(size(cats)), height, width, color, &
                     label, alpha, bottom, colors, edgecolor, linewidth, hatch)
        call xticks(category_positions(size(cats)), cats)
    end subroutine bar_cat

    ! Categories sit at 0, 1, 2, ..., as matplotlib places them.
    pure function category_positions(n) result(v)
        integer, intent(in) :: n
        real(dp) :: v(n)
        integer :: i
        do i = 1, n
            v(i) = real(i - 1, dp)
        end do
    end function category_positions

    subroutine bar_options(is, base, colors, edgecolor, linewidth, hatch)
        integer, intent(in) :: is
        real(dp), intent(in), optional :: base(:), linewidth
        character(len=*), intent(in), optional :: colors(:), edgecolor, hatch
        integer :: n, j

        n = ax(cur_i)%series(is)%n
        if (present(base)) then
            allocate (ax(cur_i)%series(is)%y2(n))
            do j = 1, n
                ax(cur_i)%series(is)%y2(j) = base(min(j, size(base)))
            end do
        end if
        if (present(colors)) then
            allocate (ax(cur_i)%series(is)%pcolor(n))
            do j = 1, n
                ax(cur_i)%series(is)%pcolor(j) = &
                    resolve_color(colors(min(j, size(colors))))
            end do
        end if
        if (present(edgecolor)) then
            ax(cur_i)%series(is)%edgecolor = resolve_color(edgecolor)
            ax(cur_i)%series(is)%hcolor = resolve_color(edgecolor)
        end if
        if (present(linewidth)) ax(cur_i)%series(is)%edgewidth = linewidth
        if (present(hatch)) ax(cur_i)%series(is)%hatch = hatch
    end subroutine bar_options

    ! Label every bar of the most recent bar series with its value. fmt is a
    ! Fortran edit descriptor such as "(f4.1)"; the default prints the value
    ! as compactly as it can.
    subroutine bar_label(fmt, padding, fontsize)
        character(len=*), intent(in), optional :: fmt
        real(dp), intent(in), optional :: padding, fontsize
        integer :: i

        call ensure_fig()
        do i = ax(cur_i)%n_series, 1, -1
            if (ax(cur_i)%series(i)%kind == SERIES_BAR .or. &
                ax(cur_i)%series(i)%kind == SERIES_BARH) then
                ax(cur_i)%series(i)%bar_labels = .true.
                ax(cur_i)%series(i)%bar_label_size = def_tick
                if (present(fmt)) ax(cur_i)%series(i)%bar_fmt = fmt
                if (present(padding)) ax(cur_i)%series(i)%bar_pad = padding
                if (present(fontsize)) ax(cur_i)%series(i)%bar_label_size = fontsize
                return
            end if
        end do
    end subroutine bar_label

    ! Half the width of bar j, which uneven histogram bins make per-bar.
    pure function bar_hw(s, j) result(v)
        type(series_t), intent(in) :: s
        integer, intent(in) :: j
        real(dp) :: v
        v = 0.5_dp * s%width
        if (allocated(s%bwidth)) v = 0.5_dp * s%bwidth(j)
    end function bar_hw

    ! Histogram of x using `bins` equal-width bins over the data range.
    subroutine hist(x, bins, color, label, alpha, bin_edges, density, &
                    cumulative, histtype, weights, stacked, orientation, &
                    log, rwidth)
        real(dp), intent(in) :: x(:)
        integer, intent(in), optional :: bins
        character(len=*), intent(in), optional :: color, label, histtype
        character(len=*), intent(in), optional :: orientation
        real(dp), intent(in), optional :: alpha, bin_edges(:), weights(:), rwidth
        logical, intent(in), optional :: density, cumulative, stacked, log
        integer :: nb, i, k, n, is
        real(dp) :: lo, hi, w, tot
        real(dp), allocatable :: edges(:), centers(:), counts(:), widths(:)
        real(dp), allocatable :: sx(:), sy(:), base(:), sb(:)
        logical :: norm, cum, stk, horiz
        character(len=16) :: ht

        n = size(x)
        if (n <= 0) return
        norm = .false.
        cum = .false.
        if (present(density)) norm = density
        if (present(cumulative)) cum = cumulative
        stk = .false.
        if (present(stacked)) stk = stacked
        ht = "bar"
        if (present(histtype)) ht = histtype
        horiz = .false.
        if (present(orientation)) horiz = orientation == "horizontal"

        if (present(bin_edges)) then
            nb = size(bin_edges) - 1
            if (nb < 1) return
            allocate (edges(nb + 1))
            edges = bin_edges
        else
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
            allocate (edges(nb + 1))
            do i = 1, nb + 1
                edges(i) = lo + real(i - 1, dp) * w
            end do
        end if

        allocate (centers(nb), counts(nb), widths(nb))
        counts = 0.0_dp
        do i = 1, nb
            centers(i) = 0.5_dp * (edges(i) + edges(i + 1))
            widths(i) = edges(i + 1) - edges(i)
        end do
        do i = 1, n
            if (x(i) < edges(1) .or. x(i) > edges(nb + 1)) cycle
            k = bin_of(x(i), edges, nb)
            if (present(weights)) then
                counts(k) = counts(k) + weights(min(i, size(weights)))
            else
                counts(k) = counts(k) + 1.0_dp
            end if
        end do

        ! density normalises by the total area, so the bars integrate to one
        ! even when the bins are of different widths.
        if (norm) then
            tot = sum(counts)
            if (tot > 0.0_dp) counts = counts / (tot * widths)
        end if
        if (cum) then
            do i = 2, nb
                counts(i) = counts(i) + counts(i - 1)
            end do
        end if

        ! rwidth leaves a gap between the bars by shrinking each about its
        ! own centre; the bins themselves are untouched.
        if (present(rwidth)) widths = widths*rwidth

        call ensure_fig()
        ! log puts the count axis on a log scale, which is the y axis for an
        ! upright histogram and the x axis for one lying on its side.
        if (present(log)) then
            if (log) then
                if (horiz) then
                    call set_xscale("log")
                else
                    call set_yscale("log")
                end if
            end if
        end if
        ! A stacked histogram starts where the last one in these axes ended.
        allocate (base(nb))
        base = 0.0_dp
        if (stk) then
            if (allocated(ax(cur_i)%hstack)) then
                if (size(ax(cur_i)%hstack) == nb) base = ax(cur_i)%hstack
                deallocate (ax(cur_i)%hstack)
            end if
            allocate (ax(cur_i)%hstack(nb))
            ax(cur_i)%hstack = base + counts
        else if (allocated(ax(cur_i)%hstack)) then
            deallocate (ax(cur_i)%hstack)
        end if

        select case (trim(ht))
        case ("step", "stepfilled")
            ! The outline of the same bars: up at every left edge, across
            ! the top, and back down to the baseline at both ends.
            allocate (sx(2*nb + 2), sy(2*nb + 2), sb(2*nb + 2))
            sx(1) = edges(1)
            sy(1) = base(1)
            sb(1) = base(1)
            do i = 1, nb
                sx(2*i) = edges(i)
                sy(2*i) = base(i) + counts(i)
                sb(2*i) = base(i)
                sx(2*i + 1) = edges(i + 1)
                sy(2*i + 1) = base(i) + counts(i)
                sb(2*i + 1) = base(i)
            end do
            sx(2*nb + 2) = edges(nb + 1)
            sy(2*nb + 2) = base(nb)
            sb(2*nb + 2) = base(nb)
            if (trim(ht) == "stepfilled") then
                if (horiz) then
                    call fill_betweenx(sx, sy, sb, color, label, alpha)
                else
                    call fill_between(sx, sy, sb, color, label, alpha)
                end if
            else if (horiz) then
                is = new_shape_series(SERIES_LINE, sy, sx, color, label, alpha)
            else
                is = new_shape_series(SERIES_LINE, sx, sy, color, label, alpha)
            end if
        case default
            ! Horizontal bars hold the bin position in x and the count in y
            ! exactly as barh does, so nothing below has to know the
            ! difference.
            is = new_shape_series(merge(SERIES_BARH, SERIES_BAR, horiz), &
                                  centers, counts, color, label, alpha)
            if (is >= 1) then
                ! Histogram bars touch, so a contrasting edge would show up
                ! as a seam between them. Bars narrowed by rwidth stand
                ! apart, and then there is no seam to hide.
                ax(cur_i)%series(is)%edgecolor = ax(cur_i)%series(is)%color
                if (present(rwidth)) then
                    if (rwidth < 1.0_dp) ax(cur_i)%series(is)%edgewidth = 0.0_dp
                end if
                ax(cur_i)%series(is)%width = widths(1)
                allocate (ax(cur_i)%series(is)%bwidth(nb))
                ax(cur_i)%series(is)%bwidth = widths
                if (stk) then
                    allocate (ax(cur_i)%series(is)%y2(nb))
                    ax(cur_i)%series(is)%y2 = base
                end if
            end if
        end select
    end subroutine hist

    ! The bin of v, with the top edge belonging to the last bin.
    pure function bin_of(v, edges, nb) result(k)
        real(dp), intent(in) :: v, edges(:)
        integer, intent(in) :: nb
        integer :: k
        do k = 1, nb - 1
            if (v < edges(k + 1)) return
        end do
        k = nb
    end function bin_of

    ! Shade between y1 and y2 (default 0).
    ! where selects the x range to shade. matplotlib fills each run of true
    ! values as its own polygon, and so does this.
    subroutine fill_between(x, y1, y2, color, label, alpha, where, hatch, edgecolor, &
                            interpolate)
        real(dp), intent(in) :: x(:), y1(:)
        real(dp), intent(in), optional :: y2(:)
        character(len=*), intent(in), optional :: color, label, hatch, edgecolor
        real(dp), intent(in), optional :: alpha
        logical, intent(in), optional :: where(:), interpolate
        call fill_core(.false., x, y1, y2, color, label, alpha, where, hatch, edgecolor, &
                       interpolate)
    end subroutine fill_between

    ! The same band, but between two curves in x, run along y. The stored
    ! series carries the independent coordinate in x and the two edges in
    ! y and y2 exactly as fill_between does; only `horiz` says which way
    ! round the limits and the polygon are read.
    subroutine fill_betweenx(y, x1, x2, color, label, alpha, where, hatch, edgecolor, &
                             interpolate)
        real(dp), intent(in) :: y(:), x1(:)
        real(dp), intent(in), optional :: x2(:)
        character(len=*), intent(in), optional :: color, label, hatch, edgecolor
        real(dp), intent(in), optional :: alpha
        logical, intent(in), optional :: where(:), interpolate
        call fill_core(.true., y, x1, x2, color, label, alpha, where, hatch, edgecolor, &
                       interpolate)
    end subroutine fill_betweenx

    subroutine fill_core(horiz, x, y1, y2, color, label, alpha, where, hatch, edgecolor, &
                         interpolate)
        logical, intent(in) :: horiz
        real(dp), intent(in) :: x(:), y1(:)
        real(dp), intent(in), optional :: y2(:)
        character(len=*), intent(in), optional :: color, label, hatch, edgecolor
        real(dp), intent(in), optional :: alpha
        logical, intent(in), optional :: where(:), interpolate
        integer :: n, i, j, k, m
        character(len=7) :: col
        logical :: first, interp
        real(dp), allocatable :: xr(:), ar(:), br(:)

        call ensure_fig()
        n = min(size(x), size(y1))
        if (n < 1) return
        if (.not. present(where)) then
            call add_fill(horiz, x(1:n), y1(1:n), y2, color, label, alpha, hatch, edgecolor)
            return
        end if

        interp = .false.
        if (present(interpolate)) interp = interpolate .and. present(y2)

        ! Every run after the first reuses the color of the first, and only
        ! the first carries the label, so the group is one legend entry.
        first = .true.
        col = ""
        i = 1
        do while (i <= n)
            if (.not. where(i)) then
                i = i + 1
                cycle
            end if
            j = i
            do while (j < n)
                if (.not. where(j + 1)) exit
                j = j + 1
            end do
            if (allocated(xr)) deallocate (xr)
            if (allocated(ar)) deallocate (ar)
            if (allocated(br)) deallocate (br)
            if (interp) then
                call interp_run(x, y1, y2, i, j, n, xr, ar, br, m)
            else
                m = j - i + 1
                allocate (xr(m), ar(m), br(m))
                xr = x(i:j)
                ar = y1(i:j)
                br = 0.0_dp
                if (present(y2)) then
                    if (size(y2) >= j) br = y2(i:j)
                end if
            end if
            if (first) then
                call add_fill(horiz, xr(1:m), ar(1:m), br(1:m), color, label, alpha, hatch, edgecolor)
            else
                call add_fill(horiz, xr(1:m), ar(1:m), br(1:m), col, alpha=alpha, &
                              hatch=hatch, edgecolor=edgecolor)
            end if
            k = ax(cur_i)%n_series
            if (first .and. k >= 1) then
                ! The runs are one artist, so they take one cycle step and
                ! one legend entry between them.
                col = ax(cur_i)%series(k)%color
                first = .false.
            end if
            i = j + 1
        end do
    end subroutine fill_core

    ! matplotlib's interpolate=: the shaded run is carried out to the point
    ! where the two curves actually cross, instead of stopping at the last
    ! sample on the near side of the crossing. The crossing is found by
    ! linear interpolation of y1 - y2 across the straddling interval, which
    ! is what matplotlib does too.
    subroutine interp_run(x, y1, y2, i, j, n, xr, ar, br, m)
        real(dp), intent(in) :: x(:), y1(:), y2(:)
        integer, intent(in) :: i, j, n
        real(dp), allocatable, intent(out) :: xr(:), ar(:), br(:)
        integer, intent(out) :: m
        integer :: k
        real(dp) :: t, xc, yc
        logical :: pre, post

        pre = i > 1
        post = j < n
        m = (j - i + 1)
        allocate (xr(m + 2), ar(m + 2), br(m + 2))
        m = 0
        if (pre) then
            call crossing(x, y1, y2, i - 1, i, t, xc, yc)
            if (t >= 0.0_dp) then
                m = 1
                xr(1) = xc
                ar(1) = yc
                br(1) = yc
            end if
        end if
        do k = i, j
            m = m + 1
            xr(m) = x(k)
            ar(m) = y1(k)
            br(m) = y2(k)
        end do
        if (post) then
            call crossing(x, y1, y2, j, j + 1, t, xc, yc)
            if (t >= 0.0_dp) then
                m = m + 1
                xr(m) = xc
                ar(m) = yc
                br(m) = yc
            end if
        end if
    end subroutine interp_run

    ! Where y1 - y2 changes sign between samples p and q. t < 0 says the
    ! two never meet there and the caller should leave the run as it is.
    subroutine crossing(x, y1, y2, p, q, t, xc, yc)
        real(dp), intent(in) :: x(:), y1(:), y2(:)
        integer, intent(in) :: p, q
        real(dp), intent(out) :: t, xc, yc
        real(dp) :: d0, d1

        d0 = y1(p) - y2(p)
        d1 = y1(q) - y2(q)
        t = -1.0_dp
        xc = 0.0_dp
        yc = 0.0_dp
        if (abs(d0 - d1) <= 0.0_dp) return
        t = d0 / (d0 - d1)
        if (t < 0.0_dp .or. t > 1.0_dp) then
            t = -1.0_dp
            return
        end if
        xc = x(p) + t * (x(q) - x(p))
        yc = y1(p) + t * (y1(q) - y1(p))
    end subroutine crossing

    ! matplotlib's stackplot: every row of y is one layer, drawn as a band
    ! from the sum of the layers below it to that sum plus its own values.
    subroutine stackplot(x, y, labels, colors, alpha)
        real(dp), intent(in) :: x(:), y(:, :)
        character(len=*), intent(in), optional :: labels(:), colors(:)
        real(dp), intent(in), optional :: alpha
        real(dp), allocatable :: lo(:), hi(:)
        integer :: n, nl, k
        logical :: has_c, has_l

        call ensure_fig()
        n = min(size(x), size(y, 2))
        nl = size(y, 1)
        if (n < 2 .or. nl < 1) return
        allocate (lo(n), hi(n))
        lo = 0.0_dp
        do k = 1, nl
            hi = lo + y(k, 1:n)
            has_c = .false.
            has_l = .false.
            if (present(colors)) has_c = k <= size(colors)
            if (present(labels)) has_l = k <= size(labels)
            if (has_c .and. has_l) then
                call add_fill(.false., x(1:n), hi, lo, colors(k), labels(k), alpha)
            else if (has_c) then
                call add_fill(.false., x(1:n), hi, lo, colors(k), alpha=alpha)
            else if (has_l) then
                call add_fill(.false., x(1:n), hi, lo, label=labels(k), alpha=alpha)
            else
                call add_fill(.false., x(1:n), hi, lo, alpha=alpha)
            end if
            lo = hi
        end do
    end subroutine stackplot

    ! y2(i:j) if it is there at all, which keeps the optional optional.
    function slice(v, i, j) result(w)
        real(dp), intent(in), optional :: v(:)
        integer, intent(in) :: i, j
        real(dp), allocatable :: w(:)
        if (present(v)) then
            allocate (w(max(0, j - i + 1)))
            w = v(i:j)
        else
            allocate (w(0))
        end if
    end function slice

    subroutine add_fill(horiz, x, y1, y2, color, label, alpha, hatch, edgecolor)
        logical, intent(in) :: horiz
        real(dp), intent(in) :: x(:), y1(:)
        real(dp), intent(in), optional :: y2(:)
        character(len=*), intent(in), optional :: color, label, hatch, edgecolor
        real(dp), intent(in), optional :: alpha
        integer :: is, n

        is = new_shape_series(SERIES_FILL, x, y1, color, label, alpha)
        if (is < 1) return
        ax(cur_i)%series(is)%horiz = horiz
        if (present(hatch)) ax(cur_i)%series(is)%hatch = hatch
        if (present(edgecolor)) ax(cur_i)%series(is)%hcolor = resolve_color(edgecolor)
        n = ax(cur_i)%series(is)%n
        allocate (ax(cur_i)%series(is)%y2(n))
        ax(cur_i)%series(is)%y2(1:n) = 0.0_dp
        if (present(y2)) then
            if (size(y2) >= n) ax(cur_i)%series(is)%y2(1:n) = y2(1:n)
        end if
    end subroutine add_fill

    ! Line plot with symmetric vertical error bars.
    ! yerr/xerr are symmetric; the _lo/_hi pairs give an asymmetric error,
    ! matplotlib's 2xN array written as two arrays.
    subroutine errorbar(x, y, yerr, fmt, color, label, capsize, marker, &
                        xerr, yerr_lo, yerr_hi, xerr_lo, xerr_hi, &
                        ecolor, elinewidth, lw, alpha)
        real(dp), intent(in) :: x(:), y(:)
        real(dp), intent(in), optional :: yerr(:), xerr(:)
        real(dp), intent(in), optional :: yerr_lo(:), yerr_hi(:)
        real(dp), intent(in), optional :: xerr_lo(:), xerr_hi(:)
        character(len=*), intent(in), optional :: fmt, color, label, marker
        character(len=*), intent(in), optional :: ecolor
        real(dp), intent(in), optional :: capsize, elinewidth, lw, alpha
        integer :: is, n, mk, ls
        character(len=7) :: col

        call ensure_fig()
        is = new_shape_series(SERIES_ERRORBAR, x, y, color, label, alpha)
        if (is < 1) return
        n = ax(cur_i)%series(is)%n
        call set_arm(ax(cur_i)%series(is)%eylo, n, yerr, yerr_lo)
        call set_arm(ax(cur_i)%series(is)%eyhi, n, yerr, yerr_hi)
        call set_arm(ax(cur_i)%series(is)%exlo, n, xerr, xerr_lo)
        call set_arm(ax(cur_i)%series(is)%exhi, n, xerr, xerr_hi)

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
        if (present(ecolor)) ax(cur_i)%series(is)%ecolor = resolve_color(ecolor)
        if (present(elinewidth)) ax(cur_i)%series(is)%elw = elinewidth
        if (present(lw)) ax(cur_i)%series(is)%linewidth = lw
    end subroutine errorbar

    ! One arm of the error bars: the asymmetric value if it was given, else
    ! the symmetric one, else nothing at all.
    subroutine set_arm(arm, n, sym, side)
        real(dp), allocatable, intent(out) :: arm(:)
        integer, intent(in) :: n
        real(dp), intent(in), optional :: sym(:), side(:)
        if (present(side)) then
            allocate (arm(n))
            arm = abs(side(1:n))
        else if (present(sym)) then
            allocate (arm(n))
            arm = abs(sym(1:n))
        end if
    end subroutine set_arm

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

    ! An endless line through xy1, either through xy2 or with the given
    ! slope. Like matplotlib's axline it is drawn to the edges of the axes
    ! and does not stretch them. slope is meaningless on a log axis, and
    ! matplotlib refuses it there; here it is simply taken literally.
    subroutine axline(xy1, xy2, slope, color, linestyle, lw, label)
        real(dp), intent(in) :: xy1(2)
        real(dp), intent(in), optional :: xy2(2), slope
        character(len=*), intent(in), optional :: color, linestyle, label
        real(dp), intent(in), optional :: lw
        real(dp) :: p2(2)
        integer :: is

        call ensure_fig()
        if (present(xy2)) then
            p2 = xy2
        else if (present(slope)) then
            p2 = [xy1(1) + 1.0_dp, xy1(2) + slope]
        else
            return
        end if
        is = new_shape_series(SERIES_AXLINE, [xy1(1), p2(1)], [xy1(2), p2(2)], &
                              color, label)
        if (is < 1) return
        if (.not. present(color)) then
            ax(cur_i)%series(is)%color = "#000000"
            ax(cur_i)%color_cycle = ax(cur_i)%color_cycle - 1
        end if
        if (present(linestyle)) ax(cur_i)%series(is)%linestyle = linestyle_from_str(linestyle)
        ax(cur_i)%series(is)%linewidth = 1.5_dp
        if (present(lw)) ax(cur_i)%series(is)%linewidth = lw
    end subroutine axline

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

    ! A run of horizontal lines, each from xmin to xmax in data coordinates.
    subroutine hlines(y, xmin, xmax, color, linestyle, lw, label)
        real(dp), intent(in) :: y(:), xmin, xmax
        character(len=*), intent(in), optional :: color, linestyle, label
        real(dp), intent(in), optional :: lw
        call add_lines(SERIES_HLINES, y, xmin, xmax, color, linestyle, lw, label)
    end subroutine hlines

    subroutine vlines(x, ymin, ymax, color, linestyle, lw, label)
        real(dp), intent(in) :: x(:), ymin, ymax
        character(len=*), intent(in), optional :: color, linestyle, label
        real(dp), intent(in), optional :: lw
        call add_lines(SERIES_VLINES, x, ymin, ymax, color, linestyle, lw, label)
    end subroutine vlines

    subroutine add_lines(kd, v, lo, hi, color, linestyle, lw, label)
        integer, intent(in) :: kd
        real(dp), intent(in) :: v(:), lo, hi
        character(len=*), intent(in), optional :: color, linestyle, label
        real(dp), intent(in), optional :: lw
        integer :: is, n

        call ensure_fig()
        n = size(v)
        if (n < 1) return
        is = new_shape_series(kd, spread(lo, 1, n), v, color, label)
        if (is < 1) return
        allocate (ax(cur_i)%series(is)%y2(n))
        ax(cur_i)%series(is)%y2 = hi
        if (kd == SERIES_VLINES) then
            ! x carries the positions and y the two ends for a vertical run.
            ax(cur_i)%series(is)%x = v
            ax(cur_i)%series(is)%y = lo
        end if
        if (.not. present(color)) then
            ax(cur_i)%series(is)%color = "#000000"
            ax(cur_i)%color_cycle = ax(cur_i)%color_cycle - 1
        end if
        if (present(linestyle)) ax(cur_i)%series(is)%linestyle = linestyle_from_str(linestyle)
        if (present(lw)) ax(cur_i)%series(is)%linewidth = lw
    end subroutine add_lines

    ! A shaded band. The span runs the full width (height) of the axes unless
    ! the cross-axis limits are given, and those are axes fractions, not data,
    ! exactly as in matplotlib.
    subroutine axhspan(ymin, ymax, xmin, xmax, color, alpha, label)
        real(dp), intent(in) :: ymin, ymax
        real(dp), intent(in), optional :: xmin, xmax, alpha
        character(len=*), intent(in), optional :: color, label
        call add_span(SERIES_HSPAN, ymin, ymax, xmin, xmax, color, alpha, label)
    end subroutine axhspan

    subroutine axvspan(xmin, xmax, ymin, ymax, color, alpha, label)
        real(dp), intent(in) :: xmin, xmax
        real(dp), intent(in), optional :: ymin, ymax, alpha
        character(len=*), intent(in), optional :: color, label
        call add_span(SERIES_VSPAN, xmin, xmax, ymin, ymax, color, alpha, label)
    end subroutine axvspan

    subroutine add_span(kd, lo, hi, flo, fhi, color, alpha, label)
        integer, intent(in) :: kd
        real(dp), intent(in) :: lo, hi
        real(dp), intent(in), optional :: flo, fhi, alpha
        character(len=*), intent(in), optional :: color, label
        real(dp) :: f0, f1
        integer :: is

        call ensure_fig()
        f0 = 0.0_dp
        f1 = 1.0_dp
        if (present(flo)) f0 = flo
        if (present(fhi)) f1 = fhi
        if (kd == SERIES_HSPAN) then
            is = new_shape_series(kd, [f0, f1], [lo, hi], color, label, alpha)
        else
            is = new_shape_series(kd, [lo, hi], [f0, f1], color, label, alpha)
        end if
        if (is < 1) return
        ! A patch takes matplotlib's default patch color rather than the next
        ! color of the line cycle.
        if (.not. present(color)) then
            ax(cur_i)%series(is)%color = cycle_color(0)
            ax(cur_i)%color_cycle = ax(cur_i)%color_cycle - 1
        end if
    end subroutine add_span

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

    ! One number out of a matplotlib style string, as in "arc3,rad=0.3"
    ! or "round,pad=0.5". The default comes back when the key is absent.
    function style_number(spec, key, dflt) result(v)
        character(len=*), intent(in) :: spec, key
        real(dp), intent(in) :: dflt
        real(dp) :: v
        integer :: i, j, ios

        v = dflt
        i = index(spec, trim(key)//"=")
        if (i == 0) return
        i = i + len_trim(key) + 1
        j = index(spec(i:), ",")
        if (j == 0) then
            j = len_trim(spec)
        else
            j = i + j - 2
        end if
        if (j < i) return
        read (spec(i:j), *, iostat=ios) v
        if (ios /= 0) v = dflt
    end function style_number

    ! Text at a point in data coordinates.
    subroutine text(x, y, s, color, fontsize, ha, fontweight, fontstyle, &
                    va, rotation, bbox_facecolor, bbox_edgecolor, bbox_alpha, &
                    bbox_pad, transform, boxstyle)
        real(dp), intent(in) :: x, y
        character(len=*), intent(in) :: s
        character(len=*), intent(in), optional :: color, ha, fontweight, fontstyle
        character(len=*), intent(in), optional :: va, bbox_facecolor, bbox_edgecolor
        character(len=*), intent(in), optional :: transform, boxstyle
        real(dp), intent(in), optional :: fontsize, rotation, bbox_alpha, bbox_pad
        call add_text(x, y, s, color, fontsize, ha, .false., 0.0_dp, 0.0_dp, &
                      fontweight, fontstyle, va, rotation, bbox_facecolor, &
                      bbox_edgecolor, bbox_alpha, bbox_pad, transform, &
                      boxstyle=boxstyle)
    end subroutine text

    ! The same, but placed in figure coordinates, so that a note can sit
    ! anywhere on the canvas rather than inside one axes.
    subroutine figtext(x, y, s, color, fontsize, ha, fontweight, fontstyle, &
                       va, rotation)
        real(dp), intent(in) :: x, y
        character(len=*), intent(in) :: s
        character(len=*), intent(in), optional :: color, ha, fontweight, fontstyle
        character(len=*), intent(in), optional :: va
        real(dp), intent(in), optional :: fontsize, rotation
        integer :: it
        call add_text(x, y, s, color, fontsize, ha, .false., 0.0_dp, 0.0_dp, &
                      fontweight, fontstyle, va, rotation)
        it = ax(cur_i)%n_texts
        ax(cur_i)%texts(it)%in_fig = .true.
    end subroutine figtext

    ! Text at (xtext, ytext) with an arrow pointing at (x, y).
    subroutine annotate(s, x, y, xtext, ytext, color, fontsize, ha, &
                        fontweight, fontstyle, va, rotation, bbox_facecolor, &
                        bbox_edgecolor, bbox_alpha, bbox_pad, transform, &
                        arrowstyle, arrowcolor, arrowlw, shrink, &
                        connectionstyle, boxstyle)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: x, y
        real(dp), intent(in), optional :: xtext, ytext, fontsize
        character(len=*), intent(in), optional :: color, ha, fontweight, fontstyle
        character(len=*), intent(in), optional :: va, bbox_facecolor, bbox_edgecolor
        character(len=*), intent(in), optional :: transform, arrowstyle, arrowcolor
        character(len=*), intent(in), optional :: connectionstyle, boxstyle
        real(dp), intent(in), optional :: rotation, bbox_alpha, bbox_pad
        real(dp), intent(in), optional :: arrowlw, shrink
        real(dp) :: xt, yt
        logical :: arrow

        xt = x
        yt = y
        ! Like matplotlib, the connector appears only when it is asked for.
        arrow = present(arrowstyle)
        if (present(xtext)) xt = xtext
        if (present(ytext)) yt = ytext
        ! The label sits at the text position; the arrow runs back to (x, y).
        call add_text(xt, yt, s, color, fontsize, ha, arrow, x, y, &
                      fontweight, fontstyle, va, rotation, bbox_facecolor, &
                      bbox_edgecolor, bbox_alpha, bbox_pad, transform, &
                      arrowstyle, arrowcolor, arrowlw, shrink, &
                      connectionstyle, boxstyle)
    end subroutine annotate

    subroutine add_text(x, y, s, color, fontsize, ha, arrow, xarr, yarr, &
                        fontweight, fontstyle, va, rotation, bbox_facecolor, &
                        bbox_edgecolor, bbox_alpha, bbox_pad, transform, &
                        arrowstyle, arrowcolor, arrowlw, shrink, &
                        connectionstyle, boxstyle)
        real(dp), intent(in) :: x, y, xarr, yarr
        character(len=*), intent(in) :: s
        character(len=*), intent(in), optional :: color, ha, fontweight, fontstyle
        character(len=*), intent(in), optional :: va, bbox_facecolor, bbox_edgecolor
        character(len=*), intent(in), optional :: transform, arrowstyle, arrowcolor
        character(len=*), intent(in), optional :: connectionstyle, boxstyle
        real(dp), intent(in), optional :: fontsize, rotation, bbox_alpha, bbox_pad
        real(dp), intent(in), optional :: arrowlw, shrink
        logical, intent(in) :: arrow
        integer :: it
        character(len=7) :: col

        call ensure_fig()
        call push_text(ax(cur_i), it)
        ax(cur_i)%texts(it)%x = x
        ax(cur_i)%texts(it)%y = y
        ax(cur_i)%texts(it)%s = s
        ax(cur_i)%texts(it)%has_arrow = arrow
        if (present(fontweight)) &
            ax(cur_i)%texts(it)%weight = weight_from_str(fontweight)
        if (present(fontstyle)) &
            ax(cur_i)%texts(it)%slant = slant_from_str(fontstyle)
        ax(cur_i)%texts(it)%xtail = xarr
        ax(cur_i)%texts(it)%ytail = yarr
        ax(cur_i)%texts(it)%fontsize = 10.0_dp
        ax(cur_i)%texts(it)%ha = "left"
        ax(cur_i)%texts(it)%color = rc_text_color
        if (present(fontsize)) ax(cur_i)%texts(it)%fontsize = fontsize
        if (present(ha)) ax(cur_i)%texts(it)%ha = ha
        if (present(va)) ax(cur_i)%texts(it)%va = va
        if (present(rotation)) ax(cur_i)%texts(it)%rot = rotation
        if (present(bbox_facecolor)) then
            ax(cur_i)%texts(it)%has_box = .true.
            ax(cur_i)%texts(it)%box_fc = resolve_color(bbox_facecolor)
        end if
        if (present(bbox_edgecolor)) then
            ax(cur_i)%texts(it)%has_box = .true.
            ax(cur_i)%texts(it)%box_ec = resolve_color(bbox_edgecolor)
        end if
        if (present(bbox_alpha)) ax(cur_i)%texts(it)%box_alpha = bbox_alpha
        if (present(bbox_pad)) ax(cur_i)%texts(it)%box_pad = bbox_pad
        if (present(boxstyle)) then
            ax(cur_i)%texts(it)%has_box = .true.
            ax(cur_i)%texts(it)%box_style = boxstyle
            ax(cur_i)%texts(it)%box_pad = style_number(boxstyle, "pad", &
                                                       ax(cur_i)%texts(it)%box_pad)
        end if
        if (present(connectionstyle)) &
            ax(cur_i)%texts(it)%arc_rad = style_number(connectionstyle, "rad", 0.0_dp)
        col = resolve_color(color)
        if (len_trim(col) > 0) ax(cur_i)%texts(it)%color = col
        if (present(arrowstyle)) &
            ax(cur_i)%texts(it)%arrow_head = index(arrowstyle, ">") > 0
        if (present(arrowcolor)) &
            ax(cur_i)%texts(it)%arrow_color = resolve_color(arrowcolor)
        if (present(arrowlw)) ax(cur_i)%texts(it)%arrow_lw = arrowlw
        if (present(shrink)) ax(cur_i)%texts(it)%arrow_shrink = shrink
        if (present(transform)) then
            select case (transform)
            case ("axes"); ax(cur_i)%texts(it)%in_axes = .true.
            case ("figure"); ax(cur_i)%texts(it)%in_fig = .true.
            end select
        end if
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
        ax(ia)%series(is)%linewidth = rc_lw
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
            ax(ia)%series(is)%color = cycle_color(ax(ia)%color_cycle)
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
        real(dp) :: pxlo, pxhi, pylo, pyhi
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
        pxlo = huge(1.0_dp)
        pxhi = -huge(1.0_dp)
        pylo = huge(1.0_dp)
        pyhi = -huge(1.0_dp)

        do i = 1, a%n_series
            ! Bars occupy a span in x; a hline/vline constrains one axis only.
            hw = 0.0_dp
            if (a%series(i)%kind == SERIES_BAR) hw = 0.5_dp * a%series(i)%width

            ! A pie sets its own limits, and horizontal bars use the two axes
            ! the other way round, so neither fits the loop below.
            if (a%series(i)%kind == SERIES_PIE) cycle
            ! An endless line has no extent of its own: it is drawn to
            ! whatever the rest of the plot decided the limits are.
            if (a%series(i)%kind == SERIES_AXLINE) cycle
            ! A band drawn along y reads the same three arrays with the
            ! axes the other way round.
            if (a%series(i)%kind == SERIES_FILL .and. a%series(i)%horiz) then
                do j = 1, a%series(i)%n
                    if (.not. (finite(a%series(i)%x(j)) .and. &
                               finite(a%series(i)%y(j)) .and. &
                               finite(a%series(i)%y2(j)))) cycle
                    anyx = .true.
                    anyy = .true.
                    xmin = min(xmin, a%series(i)%y(j), a%series(i)%y2(j))
                    xmax = max(xmax, a%series(i)%y(j), a%series(i)%y2(j))
                    ymin = min(ymin, a%series(i)%x(j))
                    ymax = max(ymax, a%series(i)%x(j))
                end do
                cycle
            end if
            ! A patch widens the limits but never asks for any of its own:
            ! matplotlib leaves an axes holding nothing but patches at its
            ! default square, and only stretches to them once something
            ! else has autoscaled it.
            if (a%series(i)%kind == SERIES_PATCH .and. .not. a%series(i)%patch_scales) then
                do j = 1, a%series(i)%n
                    pxlo = min(pxlo, a%series(i)%x(j))
                    pxhi = max(pxhi, a%series(i)%x(j))
                    pylo = min(pylo, a%series(i)%y(j))
                    pyhi = max(pyhi, a%series(i)%y(j))
                end do
                cycle
            end if
            if ((a%series(i)%kind == SERIES_BOX .or. &
                 a%series(i)%kind == SERIES_VIOLIN) .and. &
                .not. a%series(i)%box_vert) then
                ! Laid across, the two axes trade jobs.
                anyy = .true.
                sticky_lo = .true.
                sticky_hi = .true.
                ymin = min(ymin, a%series(i)%pos - 0.5_dp)
                ymax = max(ymax, a%series(i)%pos + 0.5_dp)
                do j = 1, a%series(i)%n
                    anyx = .true.
                    xmin = min(xmin, a%series(i)%y(j))
                    xmax = max(xmax, a%series(i)%y(j))
                end do
                cycle
            end if
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
            ! A span constrains only its own axis; the other pair is a
            ! fraction of the axes, which cannot ask for any data room.
            if (a%series(i)%kind == SERIES_HSPAN) then
                anyy = .true.
                ymin = min(ymin, a%series(i)%y(1), a%series(i)%y(2))
                ymax = max(ymax, a%series(i)%y(1), a%series(i)%y(2))
                cycle
            end if
            if (a%series(i)%kind == SERIES_VSPAN) then
                anyx = .true.
                xmin = min(xmin, a%series(i)%x(1), a%series(i)%x(2))
                xmax = max(xmax, a%series(i)%x(1), a%series(i)%x(2))
                cycle
            end if
            if (a%series(i)%kind == SERIES_HLINES .or. &
                a%series(i)%kind == SERIES_VLINES) then
                do j = 1, a%series(i)%n
                    anyx = .true.
                    anyy = .true.
                    xmin = min(xmin, a%series(i)%x(j))
                    xmax = max(xmax, a%series(i)%x(j))
                    ymin = min(ymin, a%series(i)%y(j))
                    ymax = max(ymax, a%series(i)%y(j))
                    if (a%series(i)%kind == SERIES_HLINES) then
                        xmin = min(xmin, a%series(i)%y2(j))
                        xmax = max(xmax, a%series(i)%y2(j))
                    else
                        ymin = min(ymin, a%series(i)%y2(j))
                        ymax = max(ymax, a%series(i)%y2(j))
                    end if
                end do
                cycle
            end if
            if (a%series(i)%kind == SERIES_BARH) then
                do j = 1, a%series(i)%n
                    hw = bar_hw(a%series(i), j)
                    anyx = .true.
                    anyy = .true.
                    xmin = min(xmin, bar_base(a%series(i), j), &
                               bar_base(a%series(i), j) + a%series(i)%y(j))
                    xmax = max(xmax, bar_base(a%series(i), j), &
                               bar_base(a%series(i), j) + a%series(i)%y(j))
                    ymin = min(ymin, a%series(i)%x(j) - hw)
                    ymax = max(ymax, a%series(i)%x(j) + hw)
                end do
                if (xmin >= 0.0_dp) sx_lo = .true.
                if (xmax <= 0.0_dp) sx_hi = .true.
                cycle
            end if

            do j = 1, a%series(i)%n
                ! Missing data does not stretch the axes to infinity.
                if (a%series(i)%kind /= SERIES_HLINE) then
                    if (.not. finite(a%series(i)%x(j))) cycle
                end if
                if (a%series(i)%kind /= SERIES_VLINE) then
                    if (.not. finite(a%series(i)%y(j))) cycle
                end if
                ! A band whose other edge is missing is missing here too,
                ! and a NaN left in the comparisons below would poison the
                ! limits for every other point as well.
                if (a%series(i)%kind == SERIES_FILL) then
                    if (.not. finite(a%series(i)%y2(j))) cycle
                end if
                if (a%series(i)%kind /= SERIES_HLINE) then
                    if (a%series(i)%kind == SERIES_BAR) hw = bar_hw(a%series(i), j)
                    xv = a%series(i)%x(j)
                    xlo = xv - hw
                    xhi = xv + hw
                    if (a%series(i)%kind == SERIES_ERRORBAR) then
                        xlo = xv - arm(a%series(i)%exlo, j)
                        xhi = xv + arm(a%series(i)%exhi, j)
                    end if
                    if (a%xsc%kind == SCALE_LOG .and. xlo <= 0.0_dp) xlo = xhi
                    if (.not. (a%xsc%kind == SCALE_LOG .and. xlo <= 0.0_dp)) then
                        anyx = .true.
                        xmin = min(xmin, xlo)
                        xmax = max(xmax, xhi)
                    end if
                end if

                if (a%series(i)%kind /= SERIES_VLINE) then
                    yv = a%series(i)%y(j)
                    ylo = yv
                    yhi = yv
                    select case (a%series(i)%kind)
                    case (SERIES_BAR)
                        ! Bars are drawn from the y=0 baseline, or from the
                        ! series they were stacked on.
                        ylo = min(bar_base(a%series(i), j), bar_base(a%series(i), j) + yv)
                        yhi = max(bar_base(a%series(i), j), bar_base(a%series(i), j) + yv)
                        ylo = min(ylo, bar_base(a%series(i), j) + yv &
                                  - arm(a%series(i)%eylo, j))
                        yhi = max(yhi, bar_base(a%series(i), j) + yv &
                                  + arm(a%series(i)%eyhi, j))
                    case (SERIES_FILL)
                        ylo = min(yv, a%series(i)%y2(j))
                        yhi = max(yv, a%series(i)%y2(j))
                    case (SERIES_ERRORBAR)
                        ylo = yv - arm(a%series(i)%eylo, j)
                        yhi = yv + arm(a%series(i)%eyhi, j)
                    end select
                    ! On a log axis a bar reaching down to zero has no
                    ! bottom to speak of, so only its top counts towards the
                    ! limits.
                    if (a%ysc%kind == SCALE_LOG .and. ylo <= 0.0_dp) ylo = yhi
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

        if (anyx .and. pxhi >= pxlo) then
            xmin = min(xmin, pxlo)
            xmax = max(xmax, pxhi)
        end if
        if (anyy .and. pyhi >= pylo) then
            ymin = min(ymin, pylo)
            ymax = max(ymax, pyhi)
        end if
        if (a%yroom_set) then
            anyy = .true.
            ymin = min(ymin, a%yroom(1))
            ymax = max(ymax, a%yroom(2))
            if (a%room_sticks) then
                sticky_lo = .true.
                sticky_hi = .true.
            end if
        end if
        if (a%xroom_set) then
            anyx = .true.
            xmin = min(xmin, a%xroom(1))
            xmax = max(xmax, a%xroom(2))
            if (a%room_sticks) then
                sx_lo = .true.
                sx_hi = .true.
            end if
        end if

        if (a%xlim_set) then
            xmin = a%xmin_user
            xmax = a%xmax_user
        else
            ! An axes with nothing to autoscale to keeps the plain unit
            ! square matplotlib gives it, margins and all.
            if (anyx) then
                call expand_limits(xmin, xmax, a%xsc, a%xmargin, sx_lo, sx_hi)
            else
                xmin = 0.0_dp
                xmax = 1.0_dp
            end if
        end if

        if (a%ylim_set) then
            ymin = a%ymin_user
            ymax = a%ymax_user
        else
            if (anyy) then
                call expand_limits(ymin, ymax, a%ysc, a%ymargin, sticky_lo, sticky_hi)
            else
                ymin = 0.0_dp
                ymax = 1.0_dp
            end if
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
                if (a%img_origin_upper .and. .not. a%has_mesh) call swap(ymin, ymax)
            end if
        end if
        ! A twin borrows the shared axis wholesale, so the two sets of data
        ! stay registered against each other.
        if (a%sec_of > 0) then
            call compute_limits(ax(a%sec_of), xlo, xhi, ylo, yhi)
            if (a%sec_is_x) then
                xmin = a%sec_scale*xlo + a%sec_offset
                xmax = a%sec_scale*xhi + a%sec_offset
            else
                ymin = a%sec_scale*ylo + a%sec_offset
                ymax = a%sec_scale*yhi + a%sec_offset
            end if
        end if
        if (a%share_x > 0) call compute_limits(ax(a%share_x), xmin, xmax, ylo, yhi)
        if (a%share_y > 0) call compute_limits(ax(a%share_y), xlo, xhi, ymin, ymax)
        ! A share group spans everything its members hold, so the panels line
        ! up and a feature at one x is at the same place in all of them.
        if (a%link_x > 0) call union_limits(a%link_x, .true., xmin, xmax)
        if (a%link_y > 0) call union_limits(a%link_y, .false., ymin, ymax)
        ! An inverted axis is simply one whose limits run the other way; the
        ! data-to-device mapping and the tick locators already cope, since
        ! origin="upper" images have always produced a descending y.
        if (a%x_inv) call swap(xmin, xmax)
        if (a%y_inv) call swap(ymin, ymax)
    end subroutine compute_limits

    ! The limits of one axis across a share group. Each member is measured
    ! with its own link cleared, which is what stops this coming back here.
    recursive subroutine union_limits(root, is_x, lo, hi)
        integer, intent(in) :: root
        logical, intent(in) :: is_x
        real(dp), intent(inout) :: lo, hi
        type(axes_t) :: m
        real(dp) :: x0, x1, y0, y1
        integer :: i

        do i = 1, n_ax
            if (is_x) then
                if (ax(i)%link_x /= root) cycle
            else
                if (ax(i)%link_y /= root) cycle
            end if
            m = ax(i)
            m%link_x = 0
            m%link_y = 0
            call compute_limits(m, x0, x1, y0, y1)
            if (is_x) then
                lo = min(lo, x0)
                hi = max(hi, x1)
            else
                lo = min(lo, y0)
                hi = max(hi, y1)
            end if
        end do
    end subroutine union_limits

    ! Whether a value can be drawn at all. matplotlib treats NaN and
    ! infinity as missing data: the point is skipped, the line is broken
    ! there, and neither takes part in setting the limits.
    pure function finite(v) result(ok)
        real(dp), intent(in) :: v
        logical :: ok
        ok = (v == v) .and. abs(v) <= huge(1.0_dp)
    end function finite

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

    ! Pad a data range by the axis margin. A sticky edge (the bar
    ! baseline) is left exactly where it is.
    subroutine expand_limits(lo, hi, sc, margin, sticky_lo, sticky_hi)
        real(dp), intent(inout) :: lo, hi
        type(scale_t), intent(in) :: sc
        real(dp), intent(in) :: margin
        logical, intent(in) :: sticky_lo, sticky_hi
        real(dp) :: u0, u1, d

        if (sc%kind == SCALE_LOG) then
            if (lo <= 0.0_dp) lo = tiny(1.0_dp)
            if (hi <= lo) hi = lo * 10.0_dp
        end if

        if (margin <= 0.0_dp) return
        ! The margin is a fraction of the drawn length, so it has to be
        ! measured in transformed space; on a linear axis that is the same
        ! thing.
        u0 = scale_fwd(sc, lo)
        u1 = scale_fwd(sc, hi)
        d = u1 - u0
        if (abs(d) < 1.0e-30_dp) d = 1.0_dp
        if (.not. sticky_lo) lo = scale_inv(sc, u0 - margin * d)
        if (.not. sticky_hi) hi = scale_inv(sc, u1 + margin * d)
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

    pure function int_to_str(i) result(s)
        integer, intent(in) :: i
        character(len=:), allocatable :: s
        character(len=12) :: tmp
        write (tmp, "(I0)") i
        s = trim(tmp)
    end function int_to_str

    ! fplot carries colors as "#rrggbb" because that is what a format string
    ! and a colormap table both produce; the renderer wants components.
    pure function hex_rgb(s) result(c)
        character(len=*), intent(in) :: s
        integer :: c(3), i
        c = 0
        if (len(s) < 7) return
        do i = 1, 3
            c(i) = hex_byte(s(2*i:2*i + 1))
        end do
    end function hex_rgb

    ! The clip in force for the primitives being emitted now. The rendering
    ! API is stateless by design, but fplot draws in regions ("everything
    ! inside the axes box"), so the front end tracks the current region here
    ! and stamps it into each paint rather than passing it through fifteen
    ! layers of call.
    subroutine set_clip(x, y, w, h)
        real(dp), intent(in) :: x, y, w, h
        g_clip%on = .true.
        g_clip%x = x
        g_clip%y = y
        g_clip%w = w
        g_clip%h = h
    end subroutine set_clip

    subroutine clear_clip()
        g_clip%on = .false.
    end subroutine clear_clip

    ! Stroke-only paint.
    function pen(color, lw, alpha, ls) result(p)
        character(len=*), intent(in) :: color
        real(dp), intent(in) :: lw
        real(dp), intent(in), optional :: alpha
        integer, intent(in), optional :: ls
        type(paint_t) :: p
        p%filled = .false.
        p%stroked = .true.
        p%stroke_rgb = hex_rgb(color)
        p%line_width = lw
        if (present(alpha)) p%stroke_alpha = alpha
        if (present(ls)) call set_dash(p, ls)
        p%clip = g_clip
    end function pen

    ! Fill-only paint.
    function brush(color, alpha) result(p)
        character(len=*), intent(in) :: color
        real(dp), intent(in), optional :: alpha
        type(paint_t) :: p
        p%filled = .true.
        p%stroked = .false.
        p%fill_rgb = hex_rgb(color)
        if (present(alpha)) p%fill_alpha = alpha
        p%clip = g_clip
    end function brush

    ! matplotlib's dash patterns, in points, for its four line styles.
    subroutine set_dash(p, ls)
        type(paint_t), intent(inout) :: p
        integer, intent(in) :: ls
        select case (ls)
        case (LINE_DASHED)
            p%n_dash = 2
            p%dash(1:2) = [5.55_dp, 2.4_dp]
        case (LINE_DOTTED)
            p%n_dash = 2
            p%dash(1:2) = [1.5_dp, 2.475_dp]
        case (LINE_DASHDOT)
            p%n_dash = 4
            p%dash(1:4) = [9.9_dp, 2.4_dp, 1.5_dp, 2.4_dp]
        end select
    end subroutine set_dash

    ! Verbs for an open polyline through np points.
    pure function line_verbs(np) result(v)
        integer, intent(in) :: np
        integer :: v(np), i
        v(1) = VERB_MOVE
        do i = 2, np
            v(i) = VERB_LINE
        end do
    end function line_verbs

    subroutine append_stroke_path(b, px, py, np, color, lw, alpha)
        class(renderer_t), intent(inout) :: b
        real(dp), intent(in) :: px(:), py(:)
        integer, intent(in) :: np
        character(len=*), intent(in) :: color
        real(dp), intent(in) :: lw, alpha
        if (np < 2) return
        call b%draw_path(px(1:np), py(1:np), line_verbs(np), np, &
                         pen(color, lw, alpha))
    end subroutine append_stroke_path

    ! A filled closed polygon, used by the shaped markers and by fill_between.
    subroutine append_polygon(b, px, py, np, color, alpha, seal)
        class(renderer_t), intent(inout) :: b
        real(dp), intent(in) :: px(:), py(:)
        integer, intent(in) :: np
        character(len=*), intent(in) :: color
        real(dp), intent(in) :: alpha
        logical, intent(in), optional :: seal
        type(paint_t) :: p
        integer :: v(np + 1)

        if (np < 2) return
        v(1:np) = line_verbs(np)
        v(np + 1) = VERB_CLOSE
        p = brush(color, alpha)
        ! Abutting polygons leave a hairline of background showing through
        ! where the renderer antialiases both edges, so seal the seam by
        ! stroking the outline in the fill color.
        if (present(seal)) then
            if (seal) then
                p%stroked = .true.
                p%stroke_rgb = p%fill_rgb
                p%stroke_alpha = alpha
                p%line_width = 0.5_dp
            end if
        end if
        call b%draw_path(px(1:np), py(1:np), v, np + 1, p)
    end subroutine append_polygon

    ! The outline of one marker, described about the origin, together with
    ! how it should be painted. Handing the renderer a shape plus a list of
    ! positions lets every backend name the outline once and then reference
    ! it: an SVG <use>, a PDF form XObject, a cached sprite in PNG.
    subroutine marker_shape(mk, ms, mx, my, mv, nv, np, do_fill, do_stroke, lw)
        integer, intent(in) :: mk
        real(dp), intent(in) :: ms
        real(dp), intent(out) :: mx(:), my(:)
        integer, intent(out) :: mv(:), nv, np
        logical, intent(out) :: do_fill, do_stroke
        real(dp), intent(out) :: lw
        real(dp) :: r, k, ang
        integer :: i

        do_fill = .true.
        do_stroke = .false.
        lw = 1.0_dp
        nv = 0
        np = 0

        select case (mk)
        case (MARKER_CIRCLE, MARKER_POINT)
            if (mk == MARKER_POINT) then
                r = 0.5_dp*ms*0.5_dp
            else
                r = 0.5_dp*ms
                do_stroke = .true.
            end if
            ! Four cubics with the standard offset match a circle to about a
            ! part in ten thousand, which is far below a pixel here.
            k = r*0.5522847498307933_dp
            mx(1) = r;  my(1) = 0.0_dp
            mx(2) = r;  my(2) = k
            mx(3) = k;  my(3) = r
            mx(4) = 0.0_dp; my(4) = r
            mx(5) = -k; my(5) = r
            mx(6) = -r; my(6) = k
            mx(7) = -r; my(7) = 0.0_dp
            mx(8) = -r; my(8) = -k
            mx(9) = -k; my(9) = -r
            mx(10) = 0.0_dp; my(10) = -r
            mx(11) = k; my(11) = -r
            mx(12) = r; my(12) = -k
            mx(13) = r; my(13) = 0.0_dp
            np = 13
            mv(1:6) = [VERB_MOVE, VERB_CUBIC, VERB_CUBIC, VERB_CUBIC, &
                       VERB_CUBIC, VERB_CLOSE]
            nv = 6
        case (MARKER_X)
            r = 0.5_dp*ms
            mx(1:4) = [-r, r, -r, r]
            my(1:4) = [-r, r, r, -r]
            mv(1:4) = [VERB_MOVE, VERB_LINE, VERB_MOVE, VERB_LINE]
            np = 4
            nv = 4
            do_fill = .false.
            do_stroke = .true.
            lw = 1.5_dp
        case (MARKER_PLUS)
            r = 0.5_dp*ms
            mx(1:4) = [-r, r, 0.0_dp, 0.0_dp]
            my(1:4) = [0.0_dp, 0.0_dp, -r, r]
            mv(1:4) = [VERB_MOVE, VERB_LINE, VERB_MOVE, VERB_LINE]
            np = 4
            nv = 4
            do_fill = .false.
            do_stroke = .true.
            lw = 1.5_dp
        case (MARKER_SQUARE)
            r = 0.5_dp*ms
            mx(1:4) = [-r, r, r, -r]
            my(1:4) = [-r, -r, r, r]
            np = 4
        case (MARKER_DIAMOND)
            ! The diamond is the unit square turned on its corner, so it
            ! reaches a half diagonal rather than a half side.
            r = 0.5_dp*ms*sqrt(2.0_dp)
            mx(1:4) = [0.0_dp, r, 0.0_dp, -r]
            my(1:4) = [-r, 0.0_dp, r, 0.0_dp]
            np = 4
        case (MARKER_TRI_UP)
            r = 0.5_dp*ms
            mx(1:3) = [0.0_dp, r, -r]
            my(1:3) = [-r, r, r]
            np = 3
        case (MARKER_TRI_DOWN)
            r = 0.5_dp*ms
            mx(1:3) = [0.0_dp, r, -r]
            my(1:3) = [r, -r, -r]
            np = 3
        case (MARKER_TRI_LEFT)
            r = 0.5_dp*ms
            mx(1:3) = [-r, r, r]
            my(1:3) = [0.0_dp, -r, r]
            np = 3
        case (MARKER_TRI_RIGHT)
            r = 0.5_dp*ms
            mx(1:3) = [r, -r, -r]
            my(1:3) = [0.0_dp, -r, r]
            np = 3
        case (MARKER_STAR)
            ! Five-pointed star: alternate outer and inner vertices.
            r = 0.5_dp*ms
            do i = 1, 10
                ang = -0.5_dp*PI + real(i - 1, dp)*PI/5.0_dp
                if (mod(i, 2) == 1) then
                    mx(i) = r*cos(ang)
                    my(i) = r*sin(ang)
                else
                    mx(i) = 0.5_dp*r*cos(ang)
                    my(i) = 0.5_dp*r*sin(ang)
                end if
            end do
            np = 10
        end select

        ! The polygonal markers all share one closed outline.
        if (nv == 0 .and. np > 0) then
            mv(1:np) = line_verbs(np)
            mv(np + 1) = VERB_CLOSE
            nv = np + 1
        end if
    end subroutine marker_shape

    ! Draw the same marker at every point of x/y.
    subroutine append_markers(b, mk, x, y, n, ms, color, alpha, face, edge, ewidth)
        class(renderer_t), intent(inout) :: b
        integer, intent(in) :: mk, n
        real(dp), intent(in) :: x(:), y(:), ms, alpha
        character(len=*), intent(in) :: color
        ! Empty face or edge means the colour of the line, which is what a
        ! marker takes when nothing else is said.
        character(len=*), intent(in), optional :: face, edge
        real(dp), intent(in), optional :: ewidth
        real(dp) :: mx(16), my(16), lw
        integer :: mv(16), nv, np
        logical :: do_fill, do_stroke
        type(paint_t) :: p

        if (mk == MARKER_NONE .or. n <= 0) return
        call marker_shape(mk, ms, mx, my, mv, nv, np, do_fill, do_stroke, lw)
        if (nv <= 0) return

        p%filled = do_fill
        p%stroked = do_stroke
        p%fill_rgb = hex_rgb(color)
        p%stroke_rgb = p%fill_rgb
        if (present(face)) then
            if (len_trim(face) > 0) then
                p%filled = .true.
                p%fill_rgb = hex_rgb(face)
            end if
        end if
        if (present(edge)) then
            if (len_trim(edge) > 0) then
                p%stroked = .true.
                p%stroke_rgb = hex_rgb(edge)
                if (lw <= 0.0_dp) lw = 1.0_dp
            end if
        end if
        if (present(ewidth)) then
            if (ewidth >= 0.0_dp) then
                p%stroked = p%stroked .or. ewidth > 0.0_dp
                lw = ewidth
            end if
        end if
        p%fill_alpha = alpha
        p%stroke_alpha = alpha
        p%line_width = lw
        p%clip = g_clip
        call b%draw_markers(x(1:n), y(1:n), mx(1:np), my(1:np), mv(1:nv), nv, p)
    end subroutine append_markers

    ! One marker, for the legend key and anywhere else a single point is
    ! drawn on its own.
    subroutine append_marker(b, mk, cx, cy, ms, color, alpha)
        class(renderer_t), intent(inout) :: b
        integer, intent(in) :: mk
        real(dp), intent(in) :: cx, cy, ms
        character(len=*), intent(in) :: color
        real(dp), intent(in) :: alpha
        call append_markers(b, mk, [cx], [cy], 1, ms, color, alpha)
    end subroutine append_marker

    ! Text element. anchor is a matplotlib horizontal alignment
    ! (left/center/right) or an SVG anchor (start/middle/end). rot is a
    ! matplotlib rotation, counterclockwise in degrees.
    subroutine append_text(b, x, y, s, anchor, fontsize, color, rot, weight, &
                           slant, baseline)
        class(renderer_t), intent(inout) :: b
        real(dp), intent(in) :: x, y, fontsize
        character(len=*), intent(in) :: s, anchor, color
        real(dp), intent(in), optional :: rot
        integer, intent(in), optional :: weight, slant, baseline
        type(font_t) :: f
        integer :: an, bl
        real(dp) :: ang

        select case (anchor)
        case ("left", "start"); an = ANCHOR_START
        case ("right", "end"); an = ANCHOR_END
        case default; an = ANCHOR_MIDDLE
        end select
        f%size = fontsize
        if (present(weight)) f%weight = weight
        if (present(slant)) f%slant = slant
        bl = BASE_ALPHABETIC
        if (present(baseline)) bl = baseline
        ang = 0.0_dp
        ! The API turns angles clockwise, matplotlib counterclockwise.
        if (present(rot)) ang = -rot
        if (math_is(s)) then
            call append_math(b, x, y, s, f, brush(color), an, ang)
        else
            call b%draw_text(x, y, s, f, brush(color), an, bl, ang)
        end if
    end subroutine append_text

    ! Mathtext is laid out into plain runs here, so that the backends only
    ! ever see text they already know how to draw. The runs are placed
    ! along the text direction, which is why the offsets are rotated by
    ! the same angle as the string itself.
    subroutine append_math(b, x, y, s, f, p, anchor, ang)
        class(renderer_t), intent(inout) :: b
        real(dp), intent(in) :: x, y, ang
        character(len=*), intent(in) :: s
        type(font_t), intent(in) :: f
        type(paint_t), intent(in) :: p
        integer, intent(in) :: anchor
        type(mrun_t) :: runs(MAX_RUNS)
        type(font_t) :: rf
        type(paint_t) :: lp
        integer :: nr, i
        real(dp) :: w, x0, ca, sa, rad

        call math_layout(s, f%size, runs, nr, w)
        select case (anchor)
        case (ANCHOR_MIDDLE); x0 = -0.5_dp*w
        case (ANCHOR_END); x0 = -w
        case default; x0 = 0.0_dp
        end select
        rad = ang*PI/180.0_dp
        ca = cos(rad)
        sa = sin(rad)
        rf = f
        do i = 1, nr
            if (runs(i)%line) then
                lp = p
                lp%filled = .false.
                lp%stroked = .true.
                lp%stroke_rgb = p%fill_rgb
                lp%stroke_alpha = p%fill_alpha
                lp%line_width = runs(i)%lw
                lp%cap = CAP_BUTT
                call b%draw_path([x + (x0 + runs(i)%dx)*ca - runs(i)%dy*sa, &
                                  x + (x0 + runs(i)%x2)*ca - runs(i)%y2*sa], &
                                 [y + (x0 + runs(i)%dx)*sa + runs(i)%dy*ca, &
                                  y + (x0 + runs(i)%x2)*sa + runs(i)%y2*ca], &
                                 [VERB_MOVE, VERB_LINE], 2, lp)
                cycle
            end if
            rf%size = runs(i)%size
            rf%slant = SLANT_ROMAN
            if (runs(i)%italic) rf%slant = SLANT_ITALIC
            call b%draw_text(x + (x0 + runs(i)%dx)*ca - runs(i)%dy*sa, &
                             y + (x0 + runs(i)%dx)*sa + runs(i)%dy*ca, &
                             runs(i)%s(1:runs(i)%n), rf, p, ANCHOR_START, &
                             BASE_ALPHABETIC, ang)
        end do
    end subroutine append_math

    ! How many lines the string has, and where the k-th one lies in it.
    pure integer function line_count(s)
        character(len=*), intent(in) :: s
        integer :: i
        line_count = 1
        do i = 1, len_trim(s)
            if (s(i:i) == achar(10)) line_count = line_count + 1
        end do
    end function line_count

    pure subroutine line_bounds(s, k, i0, i1)
        character(len=*), intent(in) :: s
        integer, intent(in) :: k
        integer, intent(out) :: i0, i1
        integer :: i, n, seen

        n = len_trim(s)
        seen = 1
        i0 = 1
        i1 = n
        do i = 1, n
            if (s(i:i) /= achar(10)) cycle
            if (seen == k) then
                i1 = i - 1
                return
            end if
            seen = seen + 1
            i0 = i + 1
        end do
    end subroutine line_bounds

    ! The connector of an annotation: a shaft from the text to the point it
    ! talks about, pulled back at both ends, and optionally a two-stroke head
    ! at the point. Head geometry follows matplotlib's "->" style: it reaches
    ! 0.4 em back along the shaft and 0.2 em out to each side.
    subroutine append_arrow(b, t, px, py, tx, ty)
        class(renderer_t), intent(inout) :: b
        type(text_t), intent(in) :: t
        real(dp), intent(in) :: px, py, tx, ty
        real(dp) :: dx, dy, d, ux, uy, x0, y0, x1, y1, hl, hw
        real(dp) :: bx(3), by(3), cx, cy, qx(4), qy(4)

        dx = tx - px
        dy = ty - py
        d = sqrt(dx*dx + dy*dy)
        if (d <= 0.0_dp) return
        ! The control point of matplotlib's arc3: the midpoint pushed out
        ! sideways by rad times the length of the chord. The sideways
        ! direction is matplotlib's, which is the other way round here
        ! because pixels count downwards.
        cx = 0.5_dp*(px + tx) - t%arc_rad*dy
        cy = 0.5_dp*(py + ty) + t%arc_rad*dx
        ! Both ends are pulled back along the direction the shaft leaves in,
        ! which for a bowed shaft is the direction of the control point.
        call unit_towards(px, py, cx, cy, tx, ty, ux, uy)
        if (d <= 2.0_dp*t%arrow_shrink) return
        x0 = px + ux*t%arrow_shrink
        y0 = py + uy*t%arrow_shrink
        call unit_towards(tx, ty, cx, cy, px, py, ux, uy)
        x1 = tx + ux*t%arrow_shrink
        y1 = ty + uy*t%arrow_shrink
        ! The head points the way the shaft arrives, so keep that direction.
        ux = -ux
        uy = -uy
        if (t%arc_rad == 0.0_dp) then
            call append_line(b, x0, y0, x1, y1, trim(t%arrow_color), t%arrow_lw, &
                             LINE_SOLID, 1.0_dp)
        else
            ! The quadratic through the control point, written as the cubic
            ! the renderer draws.
            qx = [x0, x0 + 2.0_dp/3.0_dp*(cx - x0), x1 + 2.0_dp/3.0_dp*(cx - x1), x1]
            qy = [y0, y0 + 2.0_dp/3.0_dp*(cy - y0), y1 + 2.0_dp/3.0_dp*(cy - y1), y1]
            call b%draw_path(qx, qy, [VERB_MOVE, VERB_CUBIC], 2, &
                             pen(trim(t%arrow_color), t%arrow_lw, 1.0_dp))
        end if
        if (.not. t%arrow_head) return
        hl = 0.4_dp*t%fontsize
        hw = 0.2_dp*t%fontsize
        bx(1) = x1 - hl*ux + hw*uy
        by(1) = y1 - hl*uy - hw*ux
        bx(2) = x1
        by(2) = y1
        bx(3) = x1 - hl*ux - hw*uy
        by(3) = y1 - hl*uy + hw*ux
        call append_stroke_path(b, bx, by, 3, trim(t%arrow_color), t%arrow_lw, &
                                1.0_dp)
    end subroutine append_arrow

    ! Unit vector from (x, y) towards (cx, cy), or towards the far end
    ! when the two coincide.
    subroutine unit_towards(x, y, cx, cy, fx, fy, ux, uy)
        real(dp), intent(in) :: x, y, cx, cy, fx, fy
        real(dp), intent(out) :: ux, uy
        real(dp) :: dx, dy, d
        dx = cx - x
        dy = cy - y
        d = sqrt(dx*dx + dy*dy)
        if (d <= 0.0_dp) then
            dx = fx - x
            dy = fy - y
            d = sqrt(dx*dx + dy*dy)
        end if
        if (d <= 0.0_dp) then
            ux = 0.0_dp
            uy = 0.0_dp
            return
        end if
        ux = dx/d
        uy = dy/d
    end subroutine unit_towards

    ! One annotation: its box, then its lines. A string is broken at every
    ! newline and the lines are stacked at matplotlib's spacing of 1.2 times
    ! the font size.
    subroutine append_annotation(b, t, px, py)
        class(renderer_t), intent(inout) :: b
        type(text_t), intent(in) :: t
        real(dp), intent(in) :: px, py
        integer :: nl, k, i0, i1, base
        real(dp) :: lh, dy, wmax, x0, y0, pad, hgt

        lh = 1.2_dp*t%fontsize
        nl = line_count(t%s)

        ! The vertical anchor is the backend's, except that the older
        ! placement has no name in matplotlib and so has none here either.
        select case (trim(t%va))
        case ("center"); base = BASE_MIDDLE
        case ("top"); base = BASE_TOP
        case ("bottom"); base = BASE_BOTTOM
        case default; base = BASE_ALPHABETIC
        end select
        dy = 0.0_dp
        if (len_trim(t%va) == 0) dy = 3.5_dp
        ! Extra lines hang below the first, so a block that is centred or
        ! bottom-aligned has to be lifted by the rest of its height.
        select case (trim(t%va))
        case ("center"); dy = dy - 0.5_dp*(nl - 1)*lh
        case ("bottom", "baseline"); dy = dy - (nl - 1)*lh
        end select

        if (t%has_box) then
            wmax = 0.0_dp
            do k = 1, nl
                call line_bounds(t%s, k, i0, i1)
                if (i1 >= i0) wmax = max(wmax, math_width(t%s(i0:i1), t%fontsize))
            end do
            pad = t%box_pad*t%fontsize
            hgt = (nl - 1)*lh + t%fontsize
            select case (trim(t%ha))
            case ("center"); x0 = px - 0.5_dp*wmax
            case ("right", "end"); x0 = px - wmax
            case default; x0 = px
            end select
            y0 = py + dy - 0.76_dp*t%fontsize
            if (base == BASE_MIDDLE) y0 = py + dy - 0.5_dp*t%fontsize
            if (base == BASE_TOP) y0 = py + dy
            if (base == BASE_BOTTOM) y0 = py + dy - hgt
            if (trim(t%box_style) == "round") then
                call append_round_rect(b, x0 - pad, y0 - pad, wmax + 2.0_dp*pad, &
                                       hgt + 2.0_dp*pad, pad, trim(t%box_fc), &
                                       t%box_alpha, trim(t%box_ec))
            else if (len_trim(t%box_ec) > 0) then
                call append_rect(b, x0 - pad, y0 - pad, wmax + 2.0_dp*pad, &
                                 hgt + 2.0_dp*pad, trim(t%box_fc), t%box_alpha, &
                                 trim(t%box_ec), 1.0_dp)
            else
                call append_rect(b, x0 - pad, y0 - pad, wmax + 2.0_dp*pad, &
                                 hgt + 2.0_dp*pad, trim(t%box_fc), t%box_alpha)
            end if
        end if

        do k = 1, nl
            call line_bounds(t%s, k, i0, i1)
            if (i1 < i0) cycle
            call append_text(b, px, py + dy + (k - 1)*lh, t%s(i0:i1), &
                             trim(t%ha), t%fontsize, trim(t%color), rot=t%rot, &
                             weight=t%weight, slant=t%slant, baseline=base)
        end do
    end subroutine append_annotation

    ! Straight line segment in pixel coordinates.
    subroutine append_line(b, x1, y1, x2, y2, color, lw, ls, alpha)
        class(renderer_t), intent(inout) :: b
        real(dp), intent(in) :: x1, y1, x2, y2, lw
        character(len=*), intent(in) :: color
        integer, intent(in) :: ls
        real(dp), intent(in) :: alpha
        if (ls == LINE_NONE) return
        call b%draw_path([x1, x2], [y1, y2], [VERB_MOVE, VERB_LINE], 2, &
                         pen(color, lw, alpha, ls))
    end subroutine append_line

    ! matplotlib's hatching: lines ruled across the shape and clipped to
    ! it. The pattern is anchored to the canvas rather than to the shape,
    ! so that neighbouring shapes line up, and the spacings are the ones
    ! matplotlib puts in its 72 point tile. "/", "\", "|", "-", "+" and "x"
    ! say which way the lines run; repeating a character packs them closer,
    ! which is what matplotlib's density does.
    subroutine append_hatch(b, px, py, np, pattern, color, alpha)
        class(renderer_t), intent(inout) :: b
        real(dp), intent(in) :: px(:), py(:)
        integer, intent(in) :: np
        character(len=*), intent(in) :: pattern, color
        real(dp), intent(in) :: alpha
        character(len=7) :: col

        col = "#000000"
        if (len_trim(color) > 0) col = color
        call hatch_family(b, px, py, np, 1, char_count(pattern, "|") &
                          + char_count(pattern, "+"), col, alpha)
        call hatch_family(b, px, py, np, 2, char_count(pattern, "-") &
                          + char_count(pattern, "+"), col, alpha)
        call hatch_family(b, px, py, np, 3, char_count(pattern, "/") &
                          + char_count(pattern, "x"), col, alpha)
        call hatch_family(b, px, py, np, 4, char_count(pattern, achar(92)) &
                          + char_count(pattern, "x"), col, alpha)
    end subroutine append_hatch

    pure function char_count(s, c) result(n)
        character(len=*), intent(in) :: s, c
        integer :: n, i
        n = 0
        do i = 1, len_trim(s)
            if (s(i:i) == c) n = n + 1
        end do
    end function char_count

    ! One family of parallel lines. Each line is written as a point and a
    ! direction, and the family as the offsets c of its members: x for the
    ! uprights, y for the flats, x+y and x-y for the two diagonals.
    subroutine hatch_family(b, px, py, np, fam, nrep, color, alpha)
        class(renderer_t), intent(inout) :: b
        real(dp), intent(in) :: px(:), py(:)
        integer, intent(in) :: np, fam, nrep
        character(len=*), intent(in) :: color
        real(dp), intent(in) :: alpha
        real(dp) :: x0, x1, y0, y1, step, c0, cmin, cmax, c, dx, dy, ox, oy
        integer :: k, k0, k1

        if (nrep < 1 .or. np < 3) return
        x0 = minval(px(1:np))
        x1 = maxval(px(1:np))
        y0 = minval(py(1:np))
        y1 = maxval(py(1:np))

        select case (fam)
        case (1)
            step = 12.0_dp/real(nrep, dp)
            c0 = 0.5_dp*step
            cmin = x0
            cmax = x1
            dx = 0.0_dp
            dy = 1.0_dp
        case (2)
            step = 12.0_dp/real(nrep, dp)
            c0 = 0.5_dp*step
            cmin = y0
            cmax = y1
            dx = 1.0_dp
            dy = 0.0_dp
        case (3)
            ! The diagonals are twice as far apart in c as the uprights,
            ! which is what leaves them the same distance apart on paper.
            step = 24.0_dp/real(nrep, dp)
            c0 = 0.0_dp
            cmin = x0 + y0
            cmax = x1 + y1
            dx = 1.0_dp
            dy = -1.0_dp
        case default
            step = 24.0_dp/real(nrep, dp)
            c0 = 0.0_dp
            cmin = x0 - y1
            cmax = x1 - y0
            dx = 1.0_dp
            dy = 1.0_dp
        end select

        k0 = floor((cmin - c0)/step)
        k1 = ceiling((cmax - c0)/step)
        do k = k0, k1
            c = c0 + real(k, dp)*step
            select case (fam)
            case (2)
                ox = 0.0_dp
                oy = c
            case default
                ox = c
                oy = 0.0_dp
            end select
            call hatch_line(b, px, py, np, ox, oy, dx, dy, color, alpha)
        end do
    end subroutine hatch_family

    ! One ruled line, cut to the parts of it that lie inside the shape.
    ! Where the line crosses the outline an odd number of times it is in,
    ! so the crossings sort into pairs and every pair is one dash.
    subroutine hatch_line(b, px, py, np, ox, oy, dx, dy, color, alpha)
        class(renderer_t), intent(inout) :: b
        real(dp), intent(in) :: px(:), py(:)
        integer, intent(in) :: np
        real(dp), intent(in) :: ox, oy, dx, dy
        character(len=*), intent(in) :: color
        real(dp), intent(in) :: alpha
        real(dp) :: ts(np + 1), ex, ey, det, t, s, ax_, ay
        integer :: i, j, nt

        nt = 0
        do i = 1, np
            j = i + 1
            if (j > np) j = 1
            ax_ = px(i)
            ay = py(i)
            ex = px(j) - ax_
            ey = py(j) - ay
            det = -dx*ey + ex*dy
            if (abs(det) < 1.0e-12_dp) cycle
            s = (dx*(ay - oy) - dy*(ax_ - ox))/det
            ! Half open, so a crossing exactly at a vertex counts once.
            if (s < 0.0_dp .or. s >= 1.0_dp) cycle
            t = (-(ax_ - ox)*ey + ex*(ay - oy))/det
            nt = nt + 1
            ts(nt) = t
        end do
        if (nt < 2) return
        call sort_in_place(ts(1:nt))
        do i = 1, nt - 1, 2
            call append_line(b, ox + ts(i)*dx, oy + ts(i)*dy, &
                             ox + ts(i + 1)*dx, oy + ts(i + 1)*dy, &
                             color, 1.0_dp, LINE_SOLID, alpha)
        end do
    end subroutine hatch_line

    ! An axis aligned filled rectangle, optionally outlined.
    ! A rectangle with quarter-circle corners of radius r, which is
    ! matplotlib's "round" box style.
    subroutine append_round_rect(b, x, y, w, h, r, color, alpha, edge)
        class(renderer_t), intent(inout) :: b
        real(dp), intent(in) :: x, y, w, h, r, alpha
        character(len=*), intent(in) :: color, edge
        real(dp) :: px(17), py(17), k, rr
        integer :: v(10)
        type(paint_t) :: p

        rr = min(r, 0.5_dp*w, 0.5_dp*h)
        k = rr*(1.0_dp - 0.5522847498307933_dp)
        px = [x + rr, x + w - rr, x + w - k, x + w, x + w, &
              x + w, x + w, x + w - k, x + w - rr, x + rr, &
              x + k, x, x, x, x, x + k, x + rr]
        py = [y, y, y, y + k, y + rr, &
              y + h - rr, y + h - k, y + h, y + h, y + h, &
              y + h, y + h - k, y + h - rr, y + rr, y + k, y, y]
        v = [VERB_MOVE, VERB_LINE, VERB_CUBIC, VERB_LINE, VERB_CUBIC, &
             VERB_LINE, VERB_CUBIC, VERB_LINE, VERB_CUBIC, VERB_CLOSE]
        p%clip = g_clip
        if (len_trim(color) > 0) then
            p%filled = .true.
            p%fill_rgb = hex_rgb(color)
            p%fill_alpha = alpha
        end if
        if (len_trim(edge) > 0) then
            p%stroked = .true.
            p%stroke_rgb = hex_rgb(edge)
            p%line_width = 1.0_dp
        end if
        call b%draw_path(px, py, v, 10, p)
    end subroutine append_round_rect

    subroutine append_rect(b, x, y, w, h, color, alpha, edge, elw)
        class(renderer_t), intent(inout) :: b
        real(dp), intent(in) :: x, y, w, h
        character(len=*), intent(in) :: color
        real(dp), intent(in), optional :: alpha
        character(len=*), intent(in), optional :: edge
        real(dp), intent(in), optional :: elw
        type(paint_t) :: p

        p%clip = g_clip
        if (len_trim(color) > 0) then
            p%filled = .true.
            p%fill_rgb = hex_rgb(color)
            if (present(alpha)) p%fill_alpha = alpha
        end if
        if (present(edge)) then
            p%stroked = .true.
            p%stroke_rgb = hex_rgb(edge)
            p%line_width = 1.0_dp
            if (present(elw)) p%line_width = elw
        end if
        call b%draw_rect(x, y, w, h, p)
    end subroutine append_rect

    ! A shaded band: one pair of coordinates is data, the other a fraction
    ! of the axes box.
    subroutine append_span(b, s, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        real(dp) :: xa, xb, ya, yb

        if (s%kind == SERIES_HSPAN) then
            xa = ax_l + s%x(1)*ax_w
            xb = ax_l + s%x(2)*ax_w
            ya = map_y(s%y(1), ymin, ymax, ax_b, ax_h, ysc)
            yb = map_y(s%y(2), ymin, ymax, ax_b, ax_h, ysc)
        else
            xa = map_x(s%x(1), xmin, xmax, ax_l, ax_w, xsc)
            xb = map_x(s%x(2), xmin, xmax, ax_l, ax_w, xsc)
            ya = ax_b - s%y(1)*ax_h
            yb = ax_b - s%y(2)*ax_h
        end if
        call append_rect(b, min(xa, xb), min(ya, yb), abs(xb - xa), &
                         abs(yb - ya), trim(s%color), s%alpha)
    end subroutine append_span

    ! One bar of a bar/hist series, drawn from the y = 0 baseline.
    subroutine append_bar(b, s, j, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        integer, intent(in) :: j
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        real(dp) :: xa, xb, ya, yb, hw

        hw = 0.5_dp * s%width
        if (allocated(s%bwidth)) hw = 0.5_dp * s%bwidth(j)
        if (s%kind == SERIES_BARH) then
            ! x holds the bar position and y its length, so the roles of the
            ! two axes are simply swapped.
            ya = map_y(s%x(j) - hw, ymin, ymax, ax_b, ax_h, ysc)
            yb = map_y(s%x(j) + hw, ymin, ymax, ax_b, ax_h, ysc)
            xa = map_x(bar_base(s, j), xmin, xmax, ax_l, ax_w, xsc)
            xb = map_x(bar_base(s, j) + s%y(j), xmin, xmax, ax_l, ax_w, xsc)
        else
            xa = map_x(s%x(j) - hw, xmin, xmax, ax_l, ax_w, xsc)
            xb = map_x(s%x(j) + hw, xmin, xmax, ax_l, ax_w, xsc)
            ya = map_y(bar_base(s, j), ymin, ymax, ax_b, ax_h, ysc)
            yb = map_y(bar_base(s, j) + s%y(j), ymin, ymax, ax_b, ax_h, ysc)
        end if

        if (s%edgewidth > 0.0_dp) then
            call append_rect(b, min(xa, xb), min(ya, yb), abs(xb - xa), &
                             abs(yb - ya), point_color(s, j), s%alpha, &
                             trim(s%edgecolor), s%edgewidth)
        else
            call append_rect(b, min(xa, xb), min(ya, yb), abs(xb - xa), &
                             abs(yb - ya), point_color(s, j), s%alpha)
        end if
        if (len_trim(s%hatch) > 0) &
            call append_hatch(b, [xa, xb, xb, xa], [ya, ya, yb, yb], 4, &
                              trim(s%hatch), trim(s%hcolor), s%alpha)
        if (s%bar_labels) call append_bar_label(b, s, j, xa, xb, ya, yb)
        if (allocated(s%eyhi)) call append_bar_err(b, s, j, ymin, ymax, ax_b, &
                                                   ax_h, ysc, 0.5_dp*(xa + xb))
    end subroutine append_bar

    ! The error bar on top of a bar. It is centred on the end of the bar,
    ! which is where the value is, not on the value axis origin.
    subroutine append_bar_err(b, s, j, ymin, ymax, ax_b, ax_h, ysc, px)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        integer, intent(in) :: j
        real(dp), intent(in) :: ymin, ymax, ax_b, ax_h, px
        type(scale_t), intent(in) :: ysc
        real(dp) :: top, plo, phi, cap

        top = bar_base(s, j) + s%y(j)
        plo = map_y(top - arm(s%eylo, j), ymin, ymax, ax_b, ax_h, ysc)
        phi = map_y(top + arm(s%eyhi, j), ymin, ymax, ax_b, ax_h, ysc)
        call append_line(b, px, plo, px, phi, trim(s%ecolor), s%elw, LINE_SOLID, 1.0_dp)
        cap = s%ecap
        if (cap <= 0.0_dp) return
        call append_line(b, px - cap, plo, px + cap, plo, trim(s%ecolor), &
                         s%elw, LINE_SOLID, 1.0_dp)
        call append_line(b, px - cap, phi, px + cap, phi, trim(s%ecolor), &
                         s%elw, LINE_SOLID, 1.0_dp)
    end subroutine append_bar_err

    ! Where a bar starts: y = 0 unless the series was stacked on another.
    pure function bar_base(s, j) result(v)
        type(series_t), intent(in) :: s
        integer, intent(in) :: j
        real(dp) :: v
        v = 0.0_dp
        if (allocated(s%y2)) v = s%y2(j)
    end function bar_base

    ! The value written at the end of a bar. The padding is in points, which
    ! is why this waits until the bar has been placed on the canvas.
    subroutine append_bar_label(b, s, j, xa, xb, ya, yb)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        integer, intent(in) :: j
        real(dp), intent(in) :: xa, xb, ya, yb
        character(len=32) :: txt
        real(dp) :: tx, ty, fs
        integer :: tn

        txt = ""
        fs = s%bar_label_size
        if (len_trim(s%bar_fmt) > 0) then
            write (txt, s%bar_fmt) s%y(j)
        else
            call format_tick_to(s%y(j), .false., txt, tn)
        end if
        if (s%kind == SERIES_BARH) then
            ! Just past the end of the bar, vertically centred on it.
            tx = xb + s%bar_pad + 1.0_dp
            if (xb < xa) tx = xb - s%bar_pad - 1.0_dp
            ty = 0.5_dp*(ya + yb) + 0.36_dp*fs
            if (xb < xa) then
                call append_text(b, tx, ty, trim(adjustl(txt)), "right", fs, rc_text_color)
            else
                call append_text(b, tx, ty, trim(adjustl(txt)), "left", fs, rc_text_color)
            end if
        else
            ! Above the top of the bar, or below it for a bar that hangs
            ! down. 0.21 em is the descent of the font, which is what puts
            ! the bottom of the text, not its baseline, at the padding.
            ty = min(ya, yb) - s%bar_pad - 0.21_dp*fs
            if (yb > ya) ty = max(ya, yb) + s%bar_pad + 0.73_dp*fs
            tx = 0.5_dp*(xa + xb)
            call append_text(b, tx, ty, trim(adjustl(txt)), "center", fs, rc_text_color)
        end if
    end subroutine append_bar_label

    ! Shaded region between y and y2, as a single closed polygon.
    ! A patch: filled first, then outlined, so the outline sits on top of
    ! its own fill exactly as it does in matplotlib.
    subroutine append_patch(b, s, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        real(dp) :: px(s%n + 1), py(s%n + 1)
        type(paint_t) :: p
        integer :: j

        if (allocated(s%pverb)) then
            do j = 1, s%n
                px(j) = map_x(s%x(j), xmin, xmax, ax_l, ax_w, xsc)
                py(j) = map_y(s%y(j), ymin, ymax, ax_b, ax_h, ysc)
            end do
            if (s%patch_fill) then
                p = brush(trim(s%color), s%alpha)
                call b%draw_path(px(1:s%n), py(1:s%n), s%pverb, size(s%pverb), p)
            end if
            if (len_trim(s%edgecolor) > 0) then
                p = pen(trim(s%edgecolor), s%edgewidth, s%alpha)
                call b%draw_path(px(1:s%n), py(1:s%n), s%pverb, size(s%pverb), p)
            end if
            return
        end if

        if (s%n < 3) return
        do j = 1, s%n
            px(j) = map_x(s%x(j), xmin, xmax, ax_l, ax_w, xsc)
            py(j) = map_y(s%y(j), ymin, ymax, ax_b, ax_h, ysc)
        end do
        px(s%n + 1) = px(1)
        py(s%n + 1) = py(1)
        if (s%patch_fill) call append_polygon(b, px, py, s%n, trim(s%color), &
                                              s%alpha, seal=s%patch_seal)
        if (len_trim(s%hatch) > 0) &
            call append_hatch(b, px, py, s%n, trim(s%hatch), trim(s%hcolor), s%alpha)
        if (len_trim(s%edgecolor) > 0) &
            call append_stroke_path(b, px, py, s%n + 1, trim(s%edgecolor), &
                                    s%edgewidth, s%alpha)
    end subroutine append_patch

    ! One filled polygon per arrow, shaped exactly as matplotlib shapes it:
    ! a shaft of width w, a head three w wide and five w long whose barbs
    ! reach back four and a half w. Everything is measured in shaft widths
    ! and then scaled, which is why the arrows keep their proportions
    ! however long they are.
    subroutine append_quiver(b, s, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        real(dp), parameter :: HEAD_W = 3.0_dp, HEAD_L = 5.0_dp, HEAD_AX = 4.5_dp
        real(dp) :: w, sc, sn, amean, mag, ln, ct, st, px(8), py(8), qx(8), qy(8)
        real(dp) :: x0, y0
        integer :: j, k

        if (s%n <= 0) return
        amean = 0.0_dp
        do j = 1, s%n
            amean = amean + hypot(s%qu(j), s%qv(j))
        end do
        amean = amean/real(s%n, dp)

        w = s%qwidth
        if (w < 0.0_dp) w = 0.06_dp/min(25.0_dp, max(8.0_dp, sqrt(real(s%n, dp))))
        w = w*ax_w
        sc = s%qscale
        if (sc < 0.0_dp) then
            sn = max(10.0_dp, sqrt(real(s%n, dp)))
            sc = 1.8_dp*amean*sn
            if (sc <= 0.0_dp) sc = 1.0_dp
        end if

        do j = 1, s%n
            mag = hypot(s%qu(j), s%qv(j))
            if (mag <= 0.0_dp) cycle
            ! Arrow length in shaft widths, so the outline below is pure
            ! geometry and needs no further conditioning.
            ln = mag*ax_w/(sc*w)
            ct = s%qu(j)/mag
            st = s%qv(j)/mag
            qx = [0.0_dp, ln - HEAD_AX, ln - HEAD_L, ln, ln - HEAD_L, ln - HEAD_AX, 0.0_dp, 0.0_dp]
            qy = 0.5_dp*[1.0_dp, 1.0_dp, HEAD_W, 0.0_dp, -HEAD_W, -1.0_dp, -1.0_dp, 1.0_dp]
            ! A vector too short for a shaft is drawn as head alone,
            ! shrunk to the length it has.
            if (ln < HEAD_L) then
                qx = (ln/HEAD_L)*[0.0_dp, HEAD_L - HEAD_AX, HEAD_L - HEAD_L, HEAD_L, &
                                  HEAD_L - HEAD_L, HEAD_L - HEAD_AX, 0.0_dp, 0.0_dp]
                qy = (ln/HEAD_L)*qy
            end if
            x0 = map_x(s%x(j), xmin, xmax, ax_l, ax_w, xsc)
            y0 = map_y(s%y(j), ymin, ymax, ax_b, ax_h, ysc)
            do k = 1, 8
                px(k) = x0 + w*(qx(k)*ct - qy(k)*st)
                py(k) = y0 - w*(qx(k)*st + qy(k)*ct)
            end do
            call append_polygon(b, px, py, 7, trim(s%color), s%alpha)
        end do
    end subroutine append_quiver

    ! matplotlib draws these as a FancyArrowPatch with the "-|>" style at a
    ! mutation scale of ten: a filled triangle four points long and four
    ! points across, with its tip on the curve.
    ! Draw the table. Cells are a fixed 1.2 line heights tall, the text is
    ! right aligned in the body, left in the row labels and centered in the
    ! column headings, each a tenth of a cell in from its edge.
    subroutine render_table(b, a, ax_l, ax_w, ax_b, ax_h)
        class(renderer_t), intent(inout) :: b
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: ax_l, ax_w, ax_b, ax_h
        real(dp), parameter :: PAD = 0.1_dp
        real(dp) :: fs, ch, rlw, tw, x0, y0, cx, cy
        real(dp), allocatable :: cw(:)
        integer :: nr, nc, nrows, i, j, r
        character(len=32) :: txt

        if (.not. a%has_table) return
        nr = size(a%tbl_cells, 1)
        nc = size(a%tbl_cells, 2)
        nrows = nr
        if (allocated(a%tbl_col)) nrows = nrows + 1

        fs = a%tbl_size
        ch = 1.2_dp*fs
        allocate (cw(nc))
        cw = a%tbl_w*ax_w
        tw = sum(cw)

        ! The row labels get whatever width their text needs, as matplotlib
        ! sizes that column automatically.
        rlw = 0.0_dp
        if (allocated(a%tbl_row)) then
            do i = 1, nr
                rlw = max(rlw, math_width(trim(a%tbl_row(i)), fs)*(1.0_dp + 2.0_dp*PAD))
            end do
        end if

        x0 = ax_l + 0.5_dp*ax_w - 0.5_dp*tw
        select case (trim(a%tbl_loc))
        case ("top")
            y0 = ax_b - ax_h - real(nrows, dp)*ch
        case ("center")
            y0 = ax_b - 0.5_dp*ax_h - 0.5_dp*real(nrows, dp)*ch
        case default
            y0 = ax_b
        end select

        do r = 1, nrows
            cy = y0 + real(r - 1, dp)*ch
            cx = x0
            if (allocated(a%tbl_row) .and. r > nrows - nr) then
                call table_cell(b, cx - rlw, cy, rlw, ch, &
                                trim(a%tbl_row(r - (nrows - nr))), "left", fs, PAD)
            end if
            do j = 1, nc
                if (r == 1 .and. allocated(a%tbl_col)) then
                    txt = a%tbl_col(j)
                    call table_cell(b, cx, cy, cw(j), ch, trim(txt), "center", fs, PAD)
                else
                    i = r - (nrows - nr)
                    txt = a%tbl_cells(i, j)
                    call table_cell(b, cx, cy, cw(j), ch, trim(txt), "right", fs, PAD)
                end if
                cx = cx + cw(j)
            end do
        end do
    end subroutine render_table

    ! One cell: a white box with a black edge, and its text placed by loc.
    subroutine table_cell(b, x, y, w, h, s, loc, fs, pad)
        class(renderer_t), intent(inout) :: b
        real(dp), intent(in) :: x, y, w, h, fs, pad
        character(len=*), intent(in) :: s, loc
        real(dp) :: xs(5), ys(5), tx

        if (w <= 0.0_dp) return
        xs = [x, x + w, x + w, x, x]
        ys = [y, y, y + h, y + h, y]
        call append_polygon(b, xs, ys, 4, "#ffffff", 1.0_dp)
        call append_stroke_path(b, xs, ys, 5, "#000000", 1.0_dp, 1.0_dp)
        if (len_trim(s) == 0) return
        select case (loc)
        case ("left")
            tx = x + pad*w
        case ("center")
            tx = x + 0.5_dp*w
        case default
            tx = x + (1.0_dp - pad)*w
        end select
        call append_text(b, tx, y + 0.5_dp*h + 0.36_dp*fs, s, loc, fs, "#000000")
    end subroutine table_cell

    subroutine append_arrowheads(b, s, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        ! "-|>" at a mutation scale of ten: four points long and two either
        ! side. The barbs are then pushed back far enough that the stroke
        ! around the head does not overshoot the line it sits on.
        real(dp), parameter :: HEAD_L = 4.0_dp, HEAD_W = 2.0_dp
        real(dp) :: x0, y0, dx, dy, mag, ct, st, px(4), py(4)
        real(dp) :: dist, cs, sn, d, lw
        integer :: j

        dist = hypot(HEAD_L, HEAD_W)
        cs = HEAD_L/dist
        sn = HEAD_W/dist
        lw = s%linewidth
        d = dist + 0.5_dp*lw/sn

        do j = 1, s%n
            x0 = map_x(s%x(j), xmin, xmax, ax_l, ax_w, xsc)
            y0 = map_y(s%y(j), ymin, ymax, ax_b, ax_h, ysc)
            ! The direction is given in data units, so it has to go through
            ! the same mapping before it means anything on the page.
            dx = map_x(s%x(j) + s%qu(j), xmin, xmax, ax_l, ax_w, xsc) - x0
            dy = map_y(s%y(j) + s%qv(j), ymin, ymax, ax_b, ax_h, ysc) - y0
            mag = hypot(dx, dy)
            if (mag <= 0.0_dp) cycle
            ct = dx/mag
            st = dy/mag
            ! matplotlib shrinks both ends of the arrow by two points before
            ! putting the head on, so the tip lands short of the point given.
            x0 = x0 - 2.0_dp*ct
            y0 = y0 - 2.0_dp*st
            px(1) = x0
            py(1) = y0
            px(2) = x0 - d*(cs*ct - sn*st)
            py(2) = y0 - d*(cs*st + sn*ct)
            px(3) = x0 - d*(cs*ct + sn*st)
            py(3) = y0 - d*(cs*st - sn*ct)
            px(4) = px(1)
            py(4) = py(1)
            call append_polygon(b, px, py, 3, trim(s%color), s%alpha)
            call append_stroke_path(b, px, py, 4, trim(s%color), lw, s%alpha)
        end do
    end subroutine append_arrowheads

    subroutine append_fill(b, s, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        integer :: j, np, j0, j1
        real(dp), allocatable :: px(:), py(:)
        logical, allocatable :: ok(:)

        ! Missing data cuts the band in two, as it does in matplotlib: each
        ! unbroken run of points becomes a polygon of its own.
        allocate (ok(s%n), px(2 * s%n), py(2 * s%n))
        do j = 1, s%n
            ok(j) = finite(s%x(j)) .and. finite(s%y(j)) .and. finite(s%y2(j))
        end do

        j0 = 1
        do while (j0 <= s%n)
            if (.not. ok(j0)) then
                j0 = j0 + 1
                cycle
            end if
            j1 = j0
            do while (j1 < s%n)
                if (.not. ok(j1 + 1)) exit
                j1 = j1 + 1
            end do
            np = j1 - j0 + 1
            if (np >= 2) then
                do j = 1, np
                    if (s%horiz) then
                        px(j) = map_x(s%y(j0 + j - 1), xmin, xmax, ax_l, ax_w, xsc)
                        py(j) = map_y(s%x(j0 + j - 1), ymin, ymax, ax_b, ax_h, ysc)
                        px(2*np - j + 1) = map_x(s%y2(j0 + j - 1), xmin, xmax, ax_l, ax_w, xsc)
                        py(2*np - j + 1) = py(j)
                    else
                        px(j) = map_x(s%x(j0 + j - 1), xmin, xmax, ax_l, ax_w, xsc)
                        py(j) = map_y(s%y(j0 + j - 1), ymin, ymax, ax_b, ax_h, ysc)
                        ! Return along the lower edge to close the band.
                        px(2*np - j + 1) = px(j)
                        py(2*np - j + 1) = map_y(s%y2(j0 + j - 1), ymin, ymax, ax_b, ax_h, ysc)
                    end if
                end do
                call append_polygon(b, px, py, 2 * np, trim(s%color), s%alpha)
                if (len_trim(s%hatch) > 0) &
                    call append_hatch(b, px, py, 2 * np, trim(s%hatch), &
                                      trim(s%hcolor), s%alpha)
            end if
            j0 = j1 + 1
        end do
    end subroutine append_fill

    ! An endless line, clipped to the axes rectangle in pixels by Liang and
    ! Barsky's parameter test, which needs no special case for a line that
    ! happens to be vertical or horizontal.
    subroutine append_axline(b, s, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        real(dp) :: x1, y1, x2, y2, dx, dy, t0, t1

        x1 = map_x(s%x(1), xmin, xmax, ax_l, ax_w, xsc)
        y1 = map_y(s%y(1), ymin, ymax, ax_b, ax_h, ysc)
        x2 = map_x(s%x(2), xmin, xmax, ax_l, ax_w, xsc)
        y2 = map_y(s%y(2), ymin, ymax, ax_b, ax_h, ysc)
        dx = x2 - x1
        dy = y2 - y1
        if (abs(dx) < 1.0e-9_dp .and. abs(dy) < 1.0e-9_dp) return

        t0 = -1.0e9_dp
        t1 = 1.0e9_dp
        call slab(-dx, x1 - ax_l, t0, t1)
        call slab(dx, ax_l + ax_w - x1, t0, t1)
        call slab(-dy, y1 - (ax_b - ax_h), t0, t1)
        call slab(dy, ax_b - y1, t0, t1)
        if (t0 > t1) return
        call append_line(b, x1 + t0*dx, y1 + t0*dy, x1 + t1*dx, y1 + t1*dy, &
                         trim(s%color), s%linewidth, s%linestyle, s%alpha)
    end subroutine append_axline

    ! One edge of the clipping rectangle: p is the outward component of the
    ! direction, q the distance to the edge. p < 0 means the line enters
    ! through this edge, p > 0 that it leaves through it.
    subroutine slab(p, q, t0, t1)
        real(dp), intent(in) :: p, q
        real(dp), intent(inout) :: t0, t1
        real(dp) :: r
        if (abs(p) < 1.0e-12_dp) then
            ! Parallel to the edge: either wholly inside it, or nothing.
            if (q < 0.0_dp) then
                t0 = 1.0_dp
                t1 = 0.0_dp
            end if
            return
        end if
        r = q/p
        if (p < 0.0_dp) then
            t0 = max(t0, r)
        else
            t1 = min(t1, r)
        end if
    end subroutine slab

    ! Vertical error bar with caps for point j.
    subroutine append_errorbar(b, s, j, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        integer, intent(in) :: j
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        real(dp) :: px, py, plo, phi, cap, elw
        character(len=7) :: ec

        px = map_x(s%x(j), xmin, xmax, ax_l, ax_w, xsc)
        py = map_y(s%y(j), ymin, ymax, ax_b, ax_h, ysc)
        cap = s%width
        elw = s%elw
        ec = s%ecolor
        if (len_trim(ec) == 0) ec = s%color

        if (allocated(s%eylo) .or. allocated(s%eyhi)) then
            plo = map_y(s%y(j) - arm(s%eylo, j), ymin, ymax, ax_b, ax_h, ysc)
            phi = map_y(s%y(j) + arm(s%eyhi, j), ymin, ymax, ax_b, ax_h, ysc)
            call append_line(b, px, plo, px, phi, trim(ec), elw, LINE_SOLID, s%alpha)
            if (cap > 0.0_dp) then
                call append_line(b, px - cap, plo, px + cap, plo, trim(ec), &
                                 elw, LINE_SOLID, s%alpha)
                call append_line(b, px - cap, phi, px + cap, phi, trim(ec), &
                                 elw, LINE_SOLID, s%alpha)
            end if
        end if

        if (allocated(s%exlo) .or. allocated(s%exhi)) then
            plo = map_x(s%x(j) - arm(s%exlo, j), xmin, xmax, ax_l, ax_w, xsc)
            phi = map_x(s%x(j) + arm(s%exhi, j), xmin, xmax, ax_l, ax_w, xsc)
            call append_line(b, plo, py, phi, py, trim(ec), elw, LINE_SOLID, s%alpha)
            if (cap > 0.0_dp) then
                call append_line(b, plo, py - cap, plo, py + cap, trim(ec), &
                                 elw, LINE_SOLID, s%alpha)
                call append_line(b, phi, py - cap, phi, py + cap, trim(ec), &
                                 elw, LINE_SOLID, s%alpha)
            end if
        end if
    end subroutine append_errorbar

    ! One error arm, zero where the series has none.
    pure function arm(e, j) result(v)
        real(dp), allocatable, intent(in) :: e(:)
        integer, intent(in) :: j
        real(dp) :: v
        v = 0.0_dp
        if (allocated(e)) v = e(j)
    end function arm

    ! User-set tick positions win over the automatic locator.
    ! How many intervals matplotlib would ask its locator for: as many as
    ! the axis is long enough to label, assuming tick text about three
    ! times as wide as it is tall, and never more than nine.
    pure function tick_space(length, size, horizontal) result(n)
        real(dp), intent(in) :: length, size
        logical, intent(in) :: horizontal
        integer :: n
        real(dp) :: w
        if (horizontal) then
            w = 3.0_dp*size
        else
            w = 2.0_dp*size
        end if
        if (w <= 0.0_dp) then
            n = 9
        else
            n = max(1, min(9, int(length/w)))
        end if
    end function tick_space

    ! How many intervals to ask the locator for: what the user asked for
    ! with MaxNLocator, else as many as the axis is long enough to label.
    ! A length of zero is the layout pass, which has no geometry yet and
    ! uses matplotlib's own default of nine.
    pure function nbins_for(nbins, length, size, horizontal) result(n)
        integer, intent(in) :: nbins
        real(dp), intent(in) :: length, size
        logical, intent(in) :: horizontal
        integer :: n
        if (nbins > 0) then
            n = nbins
        else if (length <= 0.0_dp) then
            n = 9
        else
            n = tick_space(length, size, horizontal)
        end if
    end function nbins_for

    ! base, when it is positive, is MultipleLocator: ticks every base
    ! units rather than wherever the automatic locator would put them.
    subroutine axis_ticks(n_user, user_pos, vmin, vmax, sc, nbins, is_date, &
                          t, nt, date_unit, base)
        integer, intent(in) :: n_user
        real(dp), intent(in) :: user_pos(MAX_TICKS), vmin, vmax
        type(scale_t), intent(in) :: sc
        integer, intent(in) :: nbins
        logical, intent(in) :: is_date
        real(dp), intent(out) :: t(MAX_TICKS)
        integer, intent(out) :: nt
        integer, intent(out) :: date_unit
        real(dp), intent(in), optional :: base

        date_unit = 0
        if (present(base)) then
            if (base > 0.0_dp .and. n_user == 0) then
                call multiple_ticks(vmin, vmax, base, t, nt)
                return
            end if
        end if
        if (n_user < 0) then
            nt = 0
        else if (n_user > 0) then
            nt = n_user
            t(1:nt) = user_pos(1:nt)
        else if (is_date) then
            call date_ticks(vmin, vmax, nbins, t, nt, date_unit)
        else
            select case (sc%kind)
            case (SCALE_LOG)
                call log_ticks(vmin, vmax, t, nt)
            case (SCALE_SYMLOG)
                call symlog_ticks(vmin, vmax, t, nt)
            case default
                call linear_ticks(vmin, vmax, nbins, t, nt)
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
        ! A log axis has its minors between the decades whether or not any
        ! decade falls inside the view, which a short axis need not.
        if (sc%kind == SCALE_LOG) then
            if (vmin <= 0.0_dp .or. vmax <= vmin) return
            do i = floor(log10(vmin)), floor(log10(vmax))
                do k = 2, 9
                    call push_minor(m, nm, real(k, dp)*10.0_dp**i, vmin, vmax)
                end do
            end do
            return
        end if
        if (nt < 2) return
        do i = 1, nt - 1
            step = (t(i + 1) - t(i)) / 5.0_dp
            do k = 1, 4
                call push_minor(m, nm, t(i) + real(k, dp) * step, vmin, vmax)
            end do
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
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        real(dp) :: q1, q2, q3, iqr, wlo, whi, hw, cw, ci, mean
        real(dp) :: bx(12), by(12), px, py
        real(dp) :: lo_c, hi_c, ctr, nlo_c, nhi_c, mlo, mhi
        integer :: j, np

        q1 = quantile(s%y(1:s%n), 0.25_dp)
        q2 = quantile(s%y(1:s%n), 0.5_dp)
        q3 = quantile(s%y(1:s%n), 0.75_dp)
        iqr = q3 - q1
        wlo = q1
        whi = q3
        do j = 1, s%n
            if (s%y(j) >= q1 - s%whis*iqr) then
                wlo = s%y(j)
                exit
            end if
        end do
        do j = s%n, 1, -1
            if (s%y(j) <= q3 + s%whis*iqr) then
                whi = s%y(j)
                exit
            end if
        end do

        hw = 0.5_dp*s%width
        cw = 0.25_dp*s%width
        ! The three offsets across the category axis: the box edges, the
        ! waist of a notched box, and the centre line the whiskers run on.
        lo_c = pos_coord(s, s%pos - hw, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        hi_c = pos_coord(s, s%pos + hw, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        nlo_c = pos_coord(s, s%pos - cw, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        nhi_c = pos_coord(s, s%pos + cw, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        ctr = pos_coord(s, s%pos, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)

        ! matplotlib's notch reaches 1.57 IQR / sqrt(N) either side of the
        ! median, which is the 95% interval for it.
        ci = 1.57_dp*iqr/sqrt(real(s%n, dp))
        mlo = q2 - ci
        mhi = q2 + ci
        if (.not. s%box_notch) then
            mlo = q2
            mhi = q2
        end if

        ! The box outline, waisted at the median when notched.
        if (s%box_notch) then
            call box_pt(s, lo_c, q1, bx, by, 1, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call box_pt(s, hi_c, q1, bx, by, 2, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call box_pt(s, hi_c, mlo, bx, by, 3, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call box_pt(s, nhi_c, q2, bx, by, 4, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call box_pt(s, hi_c, mhi, bx, by, 5, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call box_pt(s, hi_c, q3, bx, by, 6, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call box_pt(s, lo_c, q3, bx, by, 7, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call box_pt(s, lo_c, mhi, bx, by, 8, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call box_pt(s, nlo_c, q2, bx, by, 9, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call box_pt(s, lo_c, mlo, bx, by, 10, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call box_pt(s, lo_c, q1, bx, by, 11, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            np = 11
        else
            call box_pt(s, lo_c, q1, bx, by, 1, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call box_pt(s, hi_c, q1, bx, by, 2, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call box_pt(s, hi_c, q3, bx, by, 3, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call box_pt(s, lo_c, q3, bx, by, 4, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call box_pt(s, lo_c, q1, bx, by, 5, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            np = 5
        end if

        if (s%box_fill) call append_polygon(b, bx(1:np - 1), by(1:np - 1), np - 1, &
                                            trim(s%hcolor), s%alpha)
        call append_stroke_path(b, bx(1:np), by(1:np), np, trim(s%color), 1.0_dp, s%alpha)

        ! Whiskers along the centre line, with a cap on each.
        call box_seg(b, s, ctr, q1, ctr, wlo, trim(s%color), xmin, xmax, ymin, ymax, &
                     ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        call box_seg(b, s, ctr, q3, ctr, whi, trim(s%color), xmin, xmax, ymin, ymax, &
                     ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        call box_seg(b, s, nlo_c, wlo, nhi_c, wlo, trim(s%color), xmin, xmax, ymin, ymax, &
                     ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        call box_seg(b, s, nlo_c, whi, nhi_c, whi, trim(s%color), xmin, xmax, ymin, ymax, &
                     ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        ! The median, drawn between the waist of a notched box.
        if (s%box_notch) then
            call box_seg(b, s, nlo_c, q2, nhi_c, q2, "#ff7f0e", xmin, xmax, ymin, ymax, &
                         ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        else
            call box_seg(b, s, lo_c, q2, hi_c, q2, "#ff7f0e", xmin, xmax, ymin, ymax, &
                         ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        end if

        do j = 1, s%n
            if (s%y(j) >= wlo .and. s%y(j) <= whi) cycle
            call box_pt(s, ctr, s%y(j), bx, by, 1, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call append_open_circle(b, bx(1), by(1), 3.0_dp, trim(s%color))
        end do

        ! matplotlib marks the mean with a green triangle.
        if (s%box_mean) then
            mean = sum(s%y(1:s%n))/real(s%n, dp)
            call box_pt(s, ctr, mean, bx, by, 1, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            px = bx(1)
            py = by(1)
            call append_markers(b, MARKER_TRI_UP, [px], [py], 1, 6.0_dp, "#2ca02c", s%alpha)
        end if
    end subroutine append_box

    ! The canvas coordinate of a position on the category axis, whichever
    ! axis that is for this box.
    function pos_coord(s, v, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc) result(c)
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: v, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        real(dp) :: c
        if (s%box_vert) then
            c = map_x(v, xmin, xmax, ax_l, ax_w, xsc)
        else
            c = map_y(v, ymin, ymax, ax_b, ax_h, ysc)
        end if
    end function pos_coord

    ! Store one point of the box, given an already mapped category
    ! coordinate and a value still in data units.
    subroutine box_pt(s, c, v, bx, by, k, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: c, v, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        real(dp), intent(inout) :: bx(:), by(:)
        integer, intent(in) :: k
        type(scale_t), intent(in) :: xsc, ysc
        if (s%box_vert) then
            bx(k) = c
            by(k) = map_y(v, ymin, ymax, ax_b, ax_h, ysc)
        else
            bx(k) = map_x(v, xmin, xmax, ax_l, ax_w, xsc)
            by(k) = c
        end if
    end subroutine box_pt

    ! One straight piece of box furniture, from a category/value pair to
    ! another.
    subroutine box_seg(b, s, c0, v0, c1, v1, color, xmin, xmax, ymin, ymax, &
                       ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: c0, v0, c1, v1
        character(len=*), intent(in) :: color
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        real(dp) :: bx(2), by(2)

        call box_pt(s, c0, v0, bx, by, 1, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        call box_pt(s, c1, v1, bx, by, 2, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        call append_line(b, bx(1), by(1), bx(2), by(2), color, 1.0_dp, LINE_SOLID, s%alpha)
    end subroutine box_seg

    subroutine append_open_circle(b, cx, cy, r, color)
        class(renderer_t), intent(inout) :: b
        real(dp), intent(in) :: cx, cy, r
        character(len=*), intent(in) :: color
        call b%draw_circle(cx, cy, r, pen(color, 1.0_dp))
    end subroutine append_open_circle

    ! Mirrored Gaussian kernel density estimate, plus the min/max/range bars
    ! matplotlib draws over it.
    subroutine append_violin(b, s, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        integer, parameter :: NK = 100
        real(dp) :: g(NK), d(NK)
        real(dp) :: px(2*NK), py(2*NK)
        real(dp) :: lo, hi, mu, var, h, dmax, hw, cw, u, med
        real(dp) :: c_lo, c_hi, ctr
        integer :: i, j

        lo = s%y(1)
        hi = s%y(s%n)
        if (hi <= lo) return

        mu = sum(s%y(1:s%n))/real(s%n, dp)
        var = sum((s%y(1:s%n) - mu)**2)/real(s%n - 1, dp)
        ! Scott's rule, as used by scipy's gaussian_kde and so by matplotlib.
        h = sqrt(var)*real(s%n, dp)**(-0.2_dp)
        if (h <= 0.0_dp) return

        do i = 1, NK
            g(i) = lo + (hi - lo)*real(i - 1, dp)/real(NK - 1, dp)
            d(i) = 0.0_dp
            do j = 1, s%n
                u = (g(i) - s%y(j))/h
                d(i) = d(i) + exp(-0.5_dp*u*u)
            end do
        end do
        dmax = maxval(d)
        if (dmax <= 0.0_dp) return

        hw = 0.5_dp*s%width
        cw = 0.5_dp*hw
        do i = 1, NK
            call box_pt(s, pos_coord(s, s%pos - hw*d(i)/dmax, xmin, xmax, ymin, ymax, &
                                     ax_l, ax_w, ax_b, ax_h, xsc, ysc), &
                        g(i), px, py, i, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call box_pt(s, pos_coord(s, s%pos + hw*d(i)/dmax, xmin, xmax, ymin, ymax, &
                                     ax_l, ax_w, ax_b, ax_h, xsc, ysc), &
                        g(i), px, py, 2*NK + 1 - i, xmin, xmax, ymin, ymax, &
                        ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        end do
        call append_polygon(b, px, py, 2*NK, trim(s%color), 0.3_dp)

        c_lo = pos_coord(s, s%pos - cw, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        c_hi = pos_coord(s, s%pos + cw, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        ctr = pos_coord(s, s%pos, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)

        ! box_fill stands for "the extrema were turned off" on a violin.
        if (.not. s%box_fill) then
            call violin_bar(b, s, ctr, lo, ctr, hi, xmin, xmax, ymin, ymax, &
                            ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call violin_bar(b, s, c_lo, lo, c_hi, lo, xmin, xmax, ymin, ymax, &
                            ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call violin_bar(b, s, c_lo, hi, c_hi, hi, xmin, xmax, ymin, ymax, &
                            ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        end if
        if (s%box_mean) &
            call violin_bar(b, s, c_lo, mu, c_hi, mu, xmin, xmax, ymin, ymax, &
                            ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        ! box_notch stands for "show the median" on a violin.
        if (s%box_notch) then
            med = quantile(s%y(1:s%n), 0.5_dp)
            call violin_bar(b, s, c_lo, med, c_hi, med, xmin, xmax, ymin, ymax, &
                            ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        end if
    end subroutine append_violin

    ! One bar of violin furniture, in the violin's own colour and weight.
    subroutine violin_bar(b, s, c0, v0, c1, v1, xmin, xmax, ymin, ymax, &
                          ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: c0, v0, c1, v1
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        real(dp) :: bx(2), by(2)

        call box_pt(s, c0, v0, bx, by, 1, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        call box_pt(s, c1, v1, bx, by, 2, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        call append_line(b, bx(1), by(1), bx(2), by(2), trim(s%color), 1.5_dp, LINE_SOLID, 1.0_dp)
    end subroutine violin_bar

    ! One <path> per wedge: a radius out, the arc, and back to the centre.
    subroutine append_pie(b, s, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        integer :: i
        real(dp) :: tot, a0, a1, cx, cy, rx, ry, dir, off, mid

        tot = sum(s%y(1:s%n))
        if (tot <= 0.0_dp) return
        cx = map_x(0.0_dp, xmin, xmax, ax_l, ax_w, linear_scale)
        cy = map_y(0.0_dp, ymin, ymax, ax_b, ax_h, linear_scale)

        rx = s%pie_radius*ax_w/(xmax - xmin)
        ry = s%pie_radius*ax_h/(ymax - ymin)
        dir = 1.0_dp
        if (.not. s%pie_ccw) dir = -1.0_dp

        a1 = s%pie_start
        do i = 1, s%n
            a0 = a1
            a1 = a0 + dir*2.0_dp*PI*s%y(i)/tot
            ! An exploded wedge is the same wedge about a centre pushed
            ! out along its own mid angle.
            off = 0.0_dp
            if (allocated(s%pexp)) off = s%pexp(i)*s%pie_radius
            mid = 0.5_dp*(a0 + a1)
            call append_wedge(b, cx + off*ax_w/(xmax - xmin)*cos(mid), &
                              cy - off*ax_h/(ymax - ymin)*sin(mid), rx, ry, &
                              a0, a1, trim(s%pcolor(i)), s%alpha, &
                              edge=trim(s%hcolor), ewidth=s%linewidth)
        end do
    end subroutine append_pie

    ! One wedge: out along a radius, round the rim, back to the centre.
    !
    ! The rim is emitted as cubics rather than as an SVG arc. Only SVG has an
    ! arc primitive, so flattening here is what lets the same wedge reach a
    ! PDF or a rasterizer unchanged. Splitting at 90 degrees keeps the error
    ! of the standard cubic approximation far below a pixel.
    subroutine append_wedge(b, cx, cy, rx, ry, a0, a1, color, alpha, edge, ewidth)
        class(renderer_t), intent(inout) :: b
        real(dp), intent(in) :: cx, cy, rx, ry, a0, a1, alpha
        character(len=*), intent(in) :: color
        ! matplotlib leaves a wedge edgeless unless wedgeprops asks for one.
        character(len=*), intent(in), optional :: edge
        real(dp), intent(in), optional :: ewidth
        integer, parameter :: MAXSEG = 8
        real(dp) :: px(2 + 3*MAXSEG), py(2 + 3*MAXSEG)
        integer :: verbs(3 + MAXSEG), np, nv, nseg, i
        real(dp) :: t0, t1, dt, al
        type(paint_t) :: p

        nseg = max(1, min(MAXSEG, int(abs(a1 - a0)/(0.5_dp*PI)) + 1))
        dt = (a1 - a0)/real(nseg, dp)
        ! y grows downwards on the canvas, so the sine term is subtracted.
        px(1) = cx
        py(1) = cy
        px(2) = cx + rx*cos(a0)
        py(2) = cy - ry*sin(a0)
        np = 2
        verbs(1) = VERB_MOVE
        verbs(2) = VERB_LINE
        nv = 2

        al = 4.0_dp/3.0_dp*tan(0.25_dp*dt)
        do i = 1, nseg
            t0 = a0 + real(i - 1, dp)*dt
            t1 = t0 + dt
            ! Control points ride the tangents at each end of the segment.
            px(np + 1) = cx + rx*cos(t0) + al*(-rx*sin(t0))
            py(np + 1) = cy - ry*sin(t0) + al*(-ry*cos(t0))
            px(np + 2) = cx + rx*cos(t1) - al*(-rx*sin(t1))
            py(np + 2) = cy - ry*sin(t1) - al*(-ry*cos(t1))
            px(np + 3) = cx + rx*cos(t1)
            py(np + 3) = cy - ry*sin(t1)
            np = np + 3
            nv = nv + 1
            verbs(nv) = VERB_CUBIC
        end do
        nv = nv + 1
        verbs(nv) = VERB_CLOSE

        p = brush(color, alpha)
        if (present(edge)) then
            if (len_trim(edge) > 0) then
                p%stroked = .true.
                p%stroke_rgb = hex_rgb(edge)
                p%stroke_alpha = alpha
                p%line_width = 1.0_dp
                if (present(ewidth)) p%line_width = ewidth
            end if
        end if
        call b%draw_path(px(1:np), py(1:np), verbs(1:nv), nv, p)
    end subroutine append_wedge

    ! Level lines, one level at a time. The triangles give unordered
    ! segments, so they are first chained end to end into whole contours:
    ! a contour has to be a single line before a label can be dropped into
    ! the middle of it and the line broken to make room.
    subroutine append_contour_lines(b, a, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        real(dp), allocatable :: ex(:, :), ey(:, :), vx(:), vy(:), lx(:), ly(:)
        logical, allocatable :: used(:)
        integer :: nr, nc, nlev, nseg, i, j, k, tri, ns, np, nv, i0, i1
        real(dp) :: dx, dy, gx(2), gy(2), tx(3), ty(3), tv(3), sx(2), sy(2)
        real(dp) :: t, tol, cx, cy, ang, halfw
        character(len=32) :: lbl
        integer :: nl, dec

        nr = size(a%cz, 1)
        nc = size(a%cz, 2)
        nlev = size(a%clev)
        dx = (a%cont_ext(2) - a%cont_ext(1))/real(nc - 1, dp)
        dy = (a%cont_ext(4) - a%cont_ext(3))/real(nr - 1, dp)
        tol = 1.0e-9_dp*(abs(dx) + abs(dy))
        dec = tick_decimals(a%clev, nlev)

        nseg = 2*(nr - 1)*(nc - 1)
        allocate (ex(2, nseg), ey(2, nseg), used(nseg))
        allocate (vx(nseg + 1), vy(nseg + 1), lx(nseg + 1), ly(nseg + 1))

        do k = 1, nlev
            ns = 0
            do i = 1, nr - 1
                gy(1) = a%cont_ext(3) + real(i - 1, dp)*dy
                gy(2) = gy(1) + dy
                do j = 1, nc - 1
                    gx(1) = a%cont_ext(1) + real(j - 1, dp)*dx
                    gx(2) = gx(1) + dx
                    do tri = 1, 2
                        call cell_triangle(a%cz, i, j, gx, gy, tri, tx, ty, tv)
                        call tri_level(tx, ty, tv, a%clev(k), sx, sy, np)
                        if (np /= 2) cycle
                        ns = ns + 1
                        ex(:, ns) = sx
                        ey(:, ns) = sy
                    end do
                end do
            end do
            if (ns == 0) cycle

            t = real(k - 1, dp)/real(max(nlev - 1, 1), dp)
            call format_tick_fixed(a%clev(k), dec, lbl, nl)
            halfw = 0.5_dp*real(nl, dp)*DIGIT_W*a%clab_size + 3.0_dp

            used(1:ns) = .false.
            do
                call chain_polyline(ex, ey, used, ns, tol, i, vx, vy, nv)
                if (nv == 0) exit
                do j = 1, nv
                    lx(j) = map_x(vx(j), xmin, xmax, ax_l, ax_w, xsc)
                    ly(j) = map_y(vy(j), ymin, ymax, ax_b, ax_h, ysc)
                end do
                i0 = 0
                ! Every separate run of a level gets its own label, as
                ! matplotlib labels each of them.
                if (a%cont_labels) call label_window(lx, ly, nv, halfw, i0, i1, cx, cy, ang)
                if (i0 > 0) then
                    call append_stroke_path(b, lx(1:i0), ly(1:i0), i0, &
                                            cmap_color(a%cont_cmap, t), 1.5_dp, 1.0_dp)
                    call append_stroke_path(b, lx(i1:nv), ly(i1:nv), nv - i1 + 1, &
                                            cmap_color(a%cont_cmap, t), 1.5_dp, 1.0_dp)
                    call append_text(b, cx, cy + 0.36_dp*a%clab_size, lbl(1:nl), &
                                     "middle", a%clab_size, &
                                     cmap_color(a%cont_cmap, t), ang)
                else
                    call append_stroke_path(b, lx(1:nv), ly(1:nv), nv, &
                                            cmap_color(a%cont_cmap, t), 1.5_dp, 1.0_dp)
                end if
            end do
        end do
    end subroutine append_contour_lines

    ! Take the first segment nobody has used yet and grow it in both
    ! directions for as long as another segment starts where this one ends.
    ! Returns nv = 0 once every segment belongs to a line.
    subroutine chain_polyline(ex, ey, used, ns, tol, id, vx, vy, nv)
        real(dp), intent(in) :: ex(:, :), ey(:, :), tol
        logical, intent(inout) :: used(:)
        integer, intent(in) :: ns
        integer, intent(out) :: id, nv
        real(dp), intent(out) :: vx(:), vy(:)
        integer :: s, i, head, tail, e
        real(dp) :: hx, hy, tx, ty

        nv = 0
        id = 0
        s = 0
        do i = 1, ns
            if (.not. used(i)) then
                s = i
                exit
            end if
        end do
        if (s == 0) return
        id = s
        used(s) = .true.
        ! The line is built from the middle outwards, so it is laid down
        ! back to front and then walked forwards.
        head = size(vx)/2
        tail = head + 1
        vx(head) = ex(1, s); vy(head) = ey(1, s)
        vx(tail) = ex(2, s); vy(tail) = ey(2, s)

        do
            tx = vx(tail); ty = vy(tail)
            e = 0
            do i = 1, ns
                if (used(i)) cycle
                if (near(ex(1, i), ey(1, i), tx, ty, tol)) then
                    e = 2
                else if (near(ex(2, i), ey(2, i), tx, ty, tol)) then
                    e = 1
                else
                    cycle
                end if
                used(i) = .true.
                tail = tail + 1
                vx(tail) = ex(e, i); vy(tail) = ey(e, i)
                exit
            end do
            if (e == 0 .or. tail >= size(vx)) exit
        end do

        do
            hx = vx(head); hy = vy(head)
            e = 0
            do i = 1, ns
                if (used(i)) cycle
                if (near(ex(1, i), ey(1, i), hx, hy, tol)) then
                    e = 2
                else if (near(ex(2, i), ey(2, i), hx, hy, tol)) then
                    e = 1
                else
                    cycle
                end if
                used(i) = .true.
                head = head - 1
                vx(head) = ex(e, i); vy(head) = ey(e, i)
                exit
            end do
            if (e == 0 .or. head <= 1) exit
        end do

        nv = tail - head + 1
        vx(1:nv) = vx(head:tail)
        vy(1:nv) = vy(head:tail)
    end subroutine chain_polyline

    pure function near(ax1, ay1, bx, by, tol) result(q)
        real(dp), intent(in) :: ax1, ay1, bx, by, tol
        logical :: q
        q = abs(ax1 - bx) <= tol .and. abs(ay1 - by) <= tol
    end function near

    ! Where to break the line for its label: the straightest stretch long
    ! enough to hold it, which is what matplotlib looks for too. i0 comes
    ! back as 0 when the line is too short to be worth breaking.
    subroutine label_window(px, py, n, halfw, i0, i1, cx, cy, ang)
        real(dp), intent(in) :: px(:), py(:), halfw
        integer, intent(in) :: n
        integer, intent(out) :: i0, i1
        real(dp), intent(out) :: cx, cy, ang
        real(dp) :: cum(n), dev, bdev, dxs, dys, len2, d
        integer :: m, j, ja, jb

        i0 = 0
        i1 = 0
        cx = 0.0_dp; cy = 0.0_dp; ang = 0.0_dp
        cum(1) = 0.0_dp
        do m = 2, n
            cum(m) = cum(m - 1) + hypot(px(m) - px(m - 1), py(m) - py(m - 1))
        end do
        if (cum(n) < 3.0_dp*halfw) return

        bdev = huge(1.0_dp)
        do m = 2, n - 1
            if (cum(m) < halfw .or. cum(n) - cum(m) < halfw) cycle
            ja = m
            do while (ja > 1 .and. cum(m) - cum(ja) < halfw)
                ja = ja - 1
            end do
            jb = m
            do while (jb < n .and. cum(jb) - cum(m) < halfw)
                jb = jb + 1
            end do
            dxs = px(jb) - px(ja)
            dys = py(jb) - py(ja)
            len2 = dxs*dxs + dys*dys
            if (len2 <= 0.0_dp) cycle
            dev = 0.0_dp
            do j = ja, jb
                d = abs(dxs*(py(j) - py(ja)) - dys*(px(j) - px(ja)))/sqrt(len2)
                dev = max(dev, d)
            end do
            if (dev < bdev) then
                bdev = dev
                i0 = ja
                i1 = jb
            end if
        end do
        if (i0 == 0) return

        cx = 0.5_dp*(px(i0) + px(i1))
        cy = 0.5_dp*(py(i0) + py(i1))
        ! Device y grows downwards, and a label is never written upside
        ! down: past the vertical it reads the other way round.
        ang = atan2(-(py(i1) - py(i0)), px(i1) - px(i0))*180.0_dp/PI
        if (ang > 90.0_dp) ang = ang - 180.0_dp
        if (ang < -90.0_dp) ang = ang + 180.0_dp
    end subroutine label_window

    ! Walk every cell as two triangles, emitting filled bands or level lines.
    subroutine append_contour(b, a, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
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

        if (.not. a%cont_filled) then
            call append_contour_lines(b, a, xmin, xmax, ymin, ymax, &
                                      ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            return
        end if

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

    ! An image is one draw_image call. Every format has a way to place a
    ! raster, and using it costs one object in the file where a rectangle per
    ! sample costs nr*nc: a 200x200 image is 40000 elements the other way.
    !
    ! A blit assumes the samples are evenly spaced on the canvas, which is
    ! true only while both axes are linear, so a log axis keeps the per-cell
    ! path below.
    subroutine append_image(b, a, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        integer :: nr, nc, i, j, si, sj, fx, fy, c(4)
        integer, allocatable :: rgba(:, :, :)
        real(dp) :: px0, px1, py0, py1
        type(paint_t) :: p
        logical :: flip_x, flip_y

        if (a%has_mesh .or. xsc%kind /= SCALE_LINEAR .or. ysc%kind /= SCALE_LINEAR) then
            call append_image_cells(b, a, xmin, xmax, ymin, ymax, &
                                    ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            return
        end if

        nr = size(a%img, 1)
        nc = size(a%img, 2)
        px0 = map_x(a%img_ext(1), xmin, xmax, ax_l, ax_w, xsc)
        px1 = map_x(a%img_ext(2), xmin, xmax, ax_l, ax_w, xsc)
        py0 = map_y(a%img_ext(3), ymin, ymax, ax_b, ax_h, ysc)
        py1 = map_y(a%img_ext(4), ymin, ymax, ax_b, ax_h, ysc)

        ! draw_image wants its rows from the top of the canvas down, and the
        ! image's own rows run up the extent, so whichever end of the extent
        ! lands at the top decides where row 1 goes. This is how origin=
        ! "upper" reaches the file: it descends the y axis, which is what
        ! puts the first row at the top.
        flip_x = px1 < px0
        flip_y = py0 > py1

        ! The raster is sent at roughly the size it will be drawn, which is
        ! what matplotlib does and for the same reason: a backend or a viewer
        ! asked to enlarge a 16x12 image will often interpolate it, and these
        ! images are small enough that interpolating turns the data into a
        ! blur. Repeating each sample a whole number of times is exactly
        ! nearest neighbour, so no value is invented by doing it.
        if (a%img_bilinear) then
            call append_image_smooth(b, a, px0, px1, py0, py1, flip_x, flip_y)
            return
        end if

        fx = fill_factor(abs(px1 - px0), nc)
        fy = fill_factor(abs(py1 - py0), nr)

        allocate (rgba(4, nc*fx, nr*fy))
        do i = 1, nr*fy
            si = (i - 1)/fy + 1
            if (flip_y) si = nr - si + 1
            do j = 1, nc*fx
                sj = (j - 1)/fx + 1
                if (flip_x) sj = nc - sj + 1
                call img_rgba(a, si, sj, c)
                rgba(1:4, j, i) = c
            end do
        end do

        p%clip = g_clip
        call b%draw_image(min(px0, px1), min(py0, py1), abs(px1 - px0), &
                          abs(py1 - py0), rgba, nc*fx, nr*fy, p)
    end subroutine append_image

    ! interpolation="bilinear": the raster is built at the size the image
    ! is drawn and every pixel is read from the four samples around it, so
    ! the picture comes out smooth rather than blocked. The samples sit at
    ! the middles of their cells, which is what puts the outermost half
    ! cell at a flat color rather than running off the data.
    subroutine append_image_smooth(b, a, px0, px1, py0, py1, flip_x, flip_y)
        class(renderer_t), intent(inout) :: b
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: px0, px1, py0, py1
        logical, intent(in) :: flip_x, flip_y
        integer, parameter :: MAX_SIDE = 2048
        integer, allocatable :: rgba(:, :, :)
        integer :: nr, nc, ow, oh, i, j, i0, i1, j0, j1
        real(dp) :: u, v, fu, fv, val, t
        type(paint_t) :: p

        nr = size(a%img, 1)
        nc = size(a%img, 2)
        ow = max(1, min(MAX_SIDE, nint(abs(px1 - px0))))
        oh = max(1, min(MAX_SIDE, nint(abs(py1 - py0))))
        allocate (rgba(4, ow, oh))

        do i = 1, oh
            v = (real(i, dp) - 0.5_dp)/real(oh, dp)*real(nr, dp) - 0.5_dp
            if (flip_y) v = real(nr - 1, dp) - v
            i0 = floor(v)
            fv = v - real(i0, dp)
            i1 = min(nr - 1, max(0, i0 + 1))
            i0 = min(nr - 1, max(0, i0))
            do j = 1, ow
                u = (real(j, dp) - 0.5_dp)/real(ow, dp)*real(nc, dp) - 0.5_dp
                if (flip_x) u = real(nc - 1, dp) - u
                j0 = floor(u)
                fu = u - real(j0, dp)
                j1 = min(nc - 1, max(0, j0 + 1))
                j0 = min(nc - 1, max(0, j0))
                val = (1.0_dp - fv)*((1.0_dp - fu)*a%img(i0 + 1, j0 + 1) &
                                     + fu*a%img(i0 + 1, j1 + 1)) &
                      + fv*((1.0_dp - fu)*a%img(i1 + 1, j0 + 1) &
                            + fu*a%img(i1 + 1, j1 + 1))
                rgba(1:3, j, i) = hex_rgb(img_color(a, val))
                rgba(4, j, i) = 255
            end do
        end do

        p%clip = g_clip
        call b%draw_image(min(px0, px1), min(py0, py1), abs(px1 - px0), &
                          abs(py1 - py0), rgba, ow, oh, p)
    end subroutine append_image_smooth

    ! How many times to repeat each sample so the raster covers the space it
    ! is drawn in, bounded so that a large image is left as it is rather than
    ! grown into something the file has to carry.
    pure function fill_factor(extent, n) result(f)
        real(dp), intent(in) :: extent
        integer, intent(in) :: n
        integer :: f
        integer, parameter :: MAX_SAMPLES = 2048

        f = max(1, ceiling(extent/real(max(n, 1), dp)))
        f = min(f, max(1, MAX_SAMPLES/max(n, 1)))
    end function fill_factor

    ! One rectangle per sample. Correct whatever the axes do to the spacing,
    ! and so the answer when a scale is not linear and a blit would put the
    ! samples in the wrong places.
    subroutine append_image_cells(b, a, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
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
            if (a%has_mesh) then
                ye0 = a%mesh_y(i)
                ye1 = a%mesh_y(i + 1)
            else
                ye0 = a%img_ext(3) + real(i - 1, dp) * dyc
                ye1 = ye0 + dyc
            end if
            py0 = map_y(ye0, ymin, ymax, ax_b, ax_h, ysc)
            py1 = map_y(ye1, ymin, ymax, ax_b, ax_h, ysc)
            do j = 1, nc
                if (a%has_mesh) then
                    xe0 = a%mesh_x(j)
                    xe1 = a%mesh_x(j + 1)
                else
                    xe0 = a%img_ext(1) + real(j - 1, dp) * dxc
                    xe1 = xe0 + dxc
                end if
                px0 = map_x(xe0, xmin, xmax, ax_l, ax_w, xsc)
                px1 = map_x(xe1, xmin, xmax, ax_l, ax_w, xsc)
                if (img_bad_hidden(a, a%img(i, j))) cycle
                call append_cell(b, min(px0, px1), min(py0, py1), &
                                 abs(px1 - px0), abs(py1 - py0), &
                                 img_hex(a, i, j))
            end do
        end do
    end subroutine append_image_cells

    ! Cells are grown by a hairline so that neighbours overlap; without it the
    ! renderer leaves visible seams between abutting rectangles.
    subroutine append_cell(b, x, y, w, h, color)
        class(renderer_t), intent(inout) :: b
        real(dp), intent(in) :: x, y, w, h
        character(len=*), intent(in) :: color
        real(dp), parameter :: BLEED = 0.05_dp
        call append_rect(b, x - BLEED, y - BLEED, w + 2.0_dp*BLEED, &
                         h + 2.0_dp*BLEED, color)
    end subroutine append_cell

    ! The grid lines of an axes, drawn between the patches and the lines
    ! because that is where matplotlib's axes.axisbelow="line" puts them.
    subroutine append_grid(b, a, xt, nxt, yt, nyt, xm, nxm, ym, nym, &
                           xmin, xmax, ymin, ymax, &
                           ax_l, ax_r, ax_t, ax_b, ax_w, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: xt(:), yt(:), xm(:), ym(:)
        integer, intent(in) :: nxt, nyt, nxm, nym
        real(dp), intent(in) :: xmin, xmax, ymin, ymax
        real(dp), intent(in) :: ax_l, ax_r, ax_t, ax_b, ax_w, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        character(len=7) :: col
        real(dp) :: p, lw, alpha, a2
        integer :: i, ls

        if (.not. a%grid_on) return
        col = rc_grid_color
        alpha = 1.0_dp
        if (len_trim(a%grid_color) > 0) col = resolve_color(trim(a%grid_color), a2)
        lw = rc_grid_lw
        if (a%grid_lw >= 0.0_dp) lw = a%grid_lw
        ls = LINE_SOLID
        if (a%grid_ls >= 0) ls = a%grid_ls
        if (a%grid_alpha >= 0.0_dp) alpha = a%grid_alpha
        if (grid_does(a, "major")) then
            call grid_lines(b, xt, nxt, yt, nyt)
        end if
        if (grid_does(a, "minor")) then
            call grid_lines(b, xm, nxm, ym, nym)
        end if
    contains
        subroutine grid_lines(bb, gx, ngx, gy, ngy)
            class(renderer_t), intent(inout) :: bb
            real(dp), intent(in) :: gx(:), gy(:)
            integer, intent(in) :: ngx, ngy
            if (a%grid_axis /= "y") then
                do i = 1, ngx
                    p = map_x(gx(i), xmin, xmax, ax_l, ax_w, xsc)
                    call append_line(bb, p, ax_t, p, ax_b, col, lw, ls, alpha)
                end do
            end if
            if (a%grid_axis /= "x") then
                do i = 1, ngy
                    p = map_y(gy(i), ymin, ymax, ax_b, ax_h, ysc)
                    call append_line(bb, ax_l, p, ax_r, p, col, lw, ls, alpha)
                end do
            end if
        end subroutine grid_lines
    end subroutine append_grid

    ! Does the grid cover this tier of ticks?
    pure logical function grid_does(a, which)
        type(axes_t), intent(in) :: a
        character(len=*), intent(in) :: which
        grid_does = a%grid_which == which .or. a%grid_which == "both"
    end function grid_does

    ! Minor ticks are wanted whenever they are asked for outright or the
    ! grid is drawn through them.
    pure logical function wants_minor(a)
        type(axes_t), intent(in) :: a
        wants_minor = a%minor_ticks .or. (a%grid_on .and. grid_does(a, "minor"))
    end function wants_minor

    ! Vertical gradient strip plus its own frame, ticks and labels.
    subroutine append_colorbar(b, a, idx, W, H)
        class(renderer_t), intent(inout) :: b
        type(axes_t), intent(in) :: a
        integer, intent(in) :: idx
        real(dp), intent(in) :: W, H
        real(dp) :: bx, bw, bt, bb, bh, y0, y1, t, v, py, px
        real(dp) :: cb_ticks(MAX_TICKS), lo, hi
        integer :: i, nt, ln, dec
        character(len=64) :: lbl
        character(len=512) :: esc

        call cbar_box(a, W, H, bx, bt, bw, bh)
        bb = bt + bh

        ! Slices are grown slightly so they abut without seams, so keep them
        ! inside the bar.
        call set_clip(bx, bt, bw, bh)
        if (allocated(a%img_bounds)) then
            ! One block per band, as long as the band is wide in the data.
            lo = a%img_bounds(1)
            hi = a%img_bounds(size(a%img_bounds))
            do i = 1, size(a%img_bounds) - 1
                y0 = (a%img_bounds(i) - lo) / (hi - lo)
                y1 = (a%img_bounds(i + 1) - lo) / (hi - lo)
                v = 0.5_dp * (a%img_bounds(i) + a%img_bounds(i + 1))
                call cbar_band(b, a%cbar_horiz, bx, bt, bw, bh, y0, y1, &
                               img_color(a, v))
            end do
        else
            do i = 1, CBAR_SLICES
                y0 = real(i - 1, dp) / real(CBAR_SLICES, dp)
                y1 = real(i, dp) / real(CBAR_SLICES, dp)
                t = (real(i, dp) - 0.5_dp) / real(CBAR_SLICES, dp)
                call cbar_band(b, a%cbar_horiz, bx, bt, bw, bh, y0, y1, &
                               map_color(a, t))
            end do
        end if
        call clear_clip()

        call b%draw_rect(bx, bt, bw, bh, pen(rc_spine_color, rc_spine_lw))

        lo = a%img_vmin
        hi = a%img_vmax
        if (allocated(a%img_bounds)) then
            ! A tick at every band edge, which is what the bands mean.
            nt = min(MAX_TICKS, size(a%img_bounds))
            cb_ticks(1:nt) = a%img_bounds(1:nt)
        else if (a%img_norm == NORM_LOG) then
            call log_ticks(lo, hi, cb_ticks, nt)
        else if (a%cbar_horiz) then
            call linear_ticks(lo, hi, tick_space(bw, a%xtick_size, .true.), &
                              cb_ticks, nt)
        else
            call linear_ticks(lo, hi, tick_space(bh, a%ytick_size, .false.), &
                              cb_ticks, nt)
        end if
        dec = -1
        if (.not. a%img_norm == NORM_LOG) dec = tick_decimals(cb_ticks, nt)

        do i = 1, nt
            v = cb_ticks(i)
            if (v < lo .or. v > hi) cycle
            t = (v - lo)/(hi - lo)
            ! Bands place their ticks at the edges they stand for; every
            ! other norm puts a tick where the norm puts the value.
            if (.not. allocated(a%img_bounds)) t = cmap_t(a, v)
            if (a%img_norm == NORM_LOG) then
                call format_tick_to(v, .true., lbl, ln)
            else
                call format_tick_fixed(v, dec, lbl, ln)
            end if
            if (a%cbar_horiz) then
                px = bx + t * bw
                call append_tick(b, px, bb, px, bb + 3.5_dp)
                call append_text(b, px, bb + xtick_gap(a), lbl(1:ln), &
                                 "middle", a%xtick_size, rc_text_color)
            else
                py = bb - t * bh
                call append_tick(b, bx + bw, py, bx + bw + 3.5_dp, py)
                call append_text(b, bx + bw + 7.0_dp, py + 3.5_dp, lbl(1:ln), &
                                 "left", a%ytick_size, rc_text_color)
            end if
        end do

        if (len_trim(a%cbar_label) > 0) then
            if (a%cbar_horiz) then
                call append_text(b, bx + 0.5_dp * bw, bb + xtick_gap(a) &
                                 + LABEL_BOX * a%xlabel_size, trim(a%cbar_label), &
                                 "middle", a%xlabel_size, rc_text_color)
            else
                call append_text(b, bx + bw + 34.0_dp, 0.5_dp * (bt + bb), &
                                 trim(a%cbar_label), "center", a%ylabel_size, &
                                 rc_text_color, 90.0_dp)
            end if
        end if
    end subroutine append_colorbar

    ! One slice of the bar, covering the fractions u0 to u1 of its length.
    ! A vertical bar runs from the bottom up, a horizontal one left to
    ! right, which is all that separates the two.
    subroutine cbar_band(b, horiz, bx, by, bw, bh, u0, u1, col)
        class(renderer_t), intent(inout) :: b
        logical, intent(in) :: horiz
        real(dp), intent(in) :: bx, by, bw, bh, u0, u1
        character(len=*), intent(in) :: col
        if (horiz) then
            call append_cell(b, bx + u0 * bw, by, (u1 - u0) * bw, bh, col)
        else
            call append_cell(b, bx, by + (1.0_dp - u1) * bh, bw, (u1 - u0) * bh, col)
        end if
    end subroutine cbar_band

    function fmt_pt(v) result(t)
        real(dp), intent(in) :: v
        character(len=64) :: t
        integer :: n
        call fmt_num(v, t, n)
        t = t(1:n)
    end function fmt_pt

    ! Per-point color when scatter mapped c values, otherwise the series color.
    ! The knobs the nonlinear norms turn. A centred norm needs a centre;
    ! without one it would be the plain linear norm anyway.
    subroutine norm_params(vcenter, gamma, linthresh)
        real(dp), intent(in), optional :: vcenter, gamma, linthresh
        if (present(vcenter)) ax(cur_i)%img_vcenter = vcenter
        if (present(gamma)) ax(cur_i)%img_gamma = gamma
        if (present(linthresh)) ax(cur_i)%img_linthresh = linthresh
    end subroutine norm_params

    ! A place on the colormap as a colour, honouring a colormap the caller
    ! built themselves.
    pure function map_color(a, t) result(hex)
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: t
        character(len=7) :: hex
        real(dp) :: u, f
        integer :: n, i0
        if (.not. allocated(a%cmap_list)) then
            hex = cmap_color(a%img_cmap, t)
            return
        end if
        n = size(a%cmap_list)
        if (n == 1) then
            hex = a%cmap_list(1)
            return
        end if
        u = max(0.0_dp, min(1.0_dp, t))*real(n - 1, dp)
        i0 = min(n - 2, int(u))
        f = u - real(i0, dp)
        hex = blend_hex(a%cmap_list(i0 + 1), a%cmap_list(i0 + 2), f)
    end function map_color

    ! Two colours mixed, as a colormap built from a list of stops mixes them.
    pure function blend_hex(c0, c1, f) result(hex)
        character(len=*), intent(in) :: c0, c1
        real(dp), intent(in) :: f
        character(len=7) :: hex
        character(len=16), parameter :: D = "0123456789abcdef"
        integer :: a3(3), b3(3), v, k
        a3 = hex_rgb(c0)
        b3 = hex_rgb(c1)
        hex = "#000000"
        do k = 1, 3
            v = nint(real(a3(k), dp) + f*real(b3(k) - a3(k), dp))
            v = max(0, min(255, v))
            hex(2*k:2*k) = D(v/16 + 1:v/16 + 1)
            hex(2*k + 1:2*k + 1) = D(mod(v, 16) + 1:mod(v, 16) + 1)
        end do
    end function blend_hex

    ! Whether a sample is simply not drawn: it is missing and nothing was
    ! said about what to put in its place.
    pure function img_bad_hidden(a, v) result(hidden)
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: v
        logical :: hidden
        hidden = (.not. finite(v)) .and. len_trim(a%cmap_bad) == 0
    end function img_bad_hidden

    ! The colour of one value: the colours set aside for the values that
    ! fall outside the range, or off the scale altogether, take precedence
    ! over the colormap itself.
    pure function img_color(a, v) result(hex)
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: v
        character(len=7) :: hex
        if (.not. finite(v)) then
            hex = a%cmap_bad
            if (len_trim(hex) == 0) hex = map_color(a, 0.0_dp)
            return
        end if
        if (v < a%img_vmin .and. len_trim(a%cmap_under) > 0) then
            hex = a%cmap_under
            return
        end if
        if (v > a%img_vmax .and. len_trim(a%cmap_over) > 0) then
            hex = a%cmap_over
            return
        end if
        hex = map_color(a, cmap_t(a, v))
    end function img_color

    ! matplotlib's Colormap.set_bad / set_under / set_over, and
    ! LinearSegmentedColormap.from_list, applied to the current axes.
    subroutine set_bad(color)
        character(len=*), intent(in) :: color
        call ensure_fig()
        ax(cur_i)%cmap_bad = resolve_color(color)
    end subroutine set_bad

    subroutine set_under(color)
        character(len=*), intent(in) :: color
        call ensure_fig()
        ax(cur_i)%cmap_under = resolve_color(color)
    end subroutine set_under

    subroutine set_over(color)
        character(len=*), intent(in) :: color
        call ensure_fig()
        ax(cur_i)%cmap_over = resolve_color(color)
    end subroutine set_over

    subroutine set_cmap_colors(colors)
        character(len=*), intent(in) :: colors(:)
        integer :: i
        call ensure_fig()
        if (allocated(ax(cur_i)%cmap_list)) deallocate (ax(cur_i)%cmap_list)
        if (size(colors) < 1) return
        allocate (ax(cur_i)%cmap_list(size(colors)))
        do i = 1, size(colors)
            ax(cur_i)%cmap_list(i) = resolve_color(colors(i))
        end do
    end subroutine set_cmap_colors

    pure function norm_from_str(s) result(k)
        character(len=*), intent(in) :: s
        integer :: k
        select case (trim(s))
        case ("log"); k = NORM_LOG
        case ("centered", "twoslope"); k = NORM_CENTER
        case ("power"); k = NORM_POWER
        case ("symlog"); k = NORM_SYMLOG
        case default; k = NORM_LINEAR
        end select
    end function norm_from_str

    ! matplotlib's SymLogNorm with base 10 and linscale 1: linear within
    ! linthresh of zero, logarithmic outside it, and continuous at the join.
    pure function symlog_fwd(v, linthresh) result(u)
        real(dp), intent(in) :: v, linthresh
        real(dp) :: u, adj, lt
        lt = max(linthresh, tiny(1.0_dp))
        adj = 1.0_dp/(1.0_dp - exp(-1.0_dp))
        if (abs(v) <= lt) then
            u = v*adj
        else
            u = sign(lt*(adj + log10(abs(v)/lt)), v)
        end if
    end function symlog_fwd

    ! Where a value sits on the colormap, in [0, 1].
    pure function cmap_t(a, v) result(t)
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: v
        real(dp) :: t, lo, hi
        integer :: i, nb
        lo = a%img_vmin
        hi = a%img_vmax
        if (allocated(a%img_bounds)) then
            ! BoundaryNorm: find the band, then take the color matplotlib
            ! gives that band, which is band*(N-1)/(nband-1) of the map.
            nb = size(a%img_bounds) - 1
            i = 0
            do while (i < nb)
                if (v < a%img_bounds(i + 2)) exit
                i = i + 1
            end do
            if (v < a%img_bounds(1)) i = 0
            i = max(0, min(nb - 1, i))
            if (nb <= 1) then
                t = 0.0_dp
            else
                t = real(int(real(i, dp) * 255.0_dp / real(nb - 1, dp)), dp) / 255.0_dp
            end if
            return
        end if
        select case (a%img_norm)
        case (NORM_LOG)
            ! Anything at or below zero has no logarithm, so it takes the
            ! bottom of the map, as matplotlib's LogNorm does.
            if (v <= 0.0_dp .or. lo <= 0.0_dp .or. hi <= lo) then
                t = 0.0_dp
            else
                t = (log10(v) - log10(lo))/(log10(hi) - log10(lo))
            end if
        case (NORM_CENTER)
            ! TwoSlopeNorm: the centre lands in the middle of the map, and
            ! each side is stretched to fill its half.
            if (v < a%img_vcenter) then
                t = 0.0_dp
                if (a%img_vcenter > lo) &
                    t = 0.5_dp*(v - lo)/(a%img_vcenter - lo)
            else
                t = 1.0_dp
                if (hi > a%img_vcenter) &
                    t = 0.5_dp + 0.5_dp*(v - a%img_vcenter)/(hi - a%img_vcenter)
            end if
        case (NORM_POWER)
            t = (v - lo)/(hi - lo)
            t = max(0.0_dp, min(1.0_dp, t))**a%img_gamma
        case (NORM_SYMLOG)
            t = symlog_fwd(hi, a%img_linthresh) - symlog_fwd(lo, a%img_linthresh)
            if (t <= 0.0_dp) then
                t = 0.0_dp
            else
                t = (symlog_fwd(v, a%img_linthresh) &
                     - symlog_fwd(lo, a%img_linthresh))/t
            end if
        case default
            t = (v - lo)/(hi - lo)
        end select
    end function cmap_t

    ! matplotlib layers artists rather than drawing them in call order: an
    ! image is at 0, a patch at 1, the grid at 1.5, a line at 2 and text at
    ! 3. So a bar drawn after a line still sits under it, and the grid rules
    ! across the bars but not across the lines.
    pure function series_z(s) result(z)
        type(series_t), intent(in) :: s
        real(dp) :: z
        if (s%zorder >= 0.0_dp) then
            z = s%zorder
        else if (is_patch_series(s%kind) .or. s%kind == SERIES_PATCH .or. &
                 s%kind == SERIES_PIE .or. s%kind == SERIES_VIOLIN) then
            z = Z_PATCH
        else
            z = Z_LINE
        end if
    end function series_z

    ! Series drawn as filled shapes, which take a swatch in the legend.
    pure function is_patch_series(kd) result(v)
        integer, intent(in) :: kd
        logical :: v
        v = kd == SERIES_BAR .or. kd == SERIES_BARH .or. kd == SERIES_FILL .or. &
            kd == SERIES_HSPAN .or. kd == SERIES_VSPAN
    end function is_patch_series

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

    ! matplotlib's loc="best": try each of the ten placements in its own
    ! order and take the one the data runs into least, ties going to the
    ! earlier candidate. matplotlib counts artist vertices inside the box;
    ! so does this.
    function legend_best(a, xmin, xmax, ymin, ymax, xsc, ysc, &
                         ax_l, ax_r, ax_t, ax_b, leg_w, leg_h) result(loc)
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: xmin, xmax, ymin, ymax
        type(scale_t), intent(in) :: xsc, ysc
        real(dp), intent(in) :: ax_l, ax_r, ax_t, ax_b, leg_w, leg_h
        character(len=16) :: loc
        character(len=16), parameter :: cand(10) = &
            [character(len=16) :: "upper right", "upper left", "lower left", &
             "lower right", "center right", "center left", "center right", &
             "lower center", "upper center", "center"]
        integer :: k, i, j, bad, best
        real(dp) :: lx, ly, px, py, ax_w, ax_h

        ax_w = ax_r - ax_l
        ax_h = ax_b - ax_t
        loc = cand(1)
        best = huge(1)
        do k = 1, 10
            call legend_origin(cand(k), ax_l, ax_r, ax_t, ax_b, leg_w, leg_h, lx, ly)
            bad = 0
            do i = 1, a%n_series
                if (.not. allocated(a%series(i)%x)) cycle
                if (.not. allocated(a%series(i)%y)) cycle
                do j = 1, a%series(i)%n
                    px = map_x(a%series(i)%x(j), xmin, xmax, ax_l, ax_w, xsc)
                    py = map_y(a%series(i)%y(j), ymin, ymax, ax_b, ax_h, ysc)
                    if (px >= lx .and. px <= lx + leg_w .and. &
                        py >= ly .and. py <= ly + leg_h) bad = bad + 1
                end do
            end do
            if (bad < best) then
                best = bad
                loc = cand(k)
            end if
            if (best == 0) exit
        end do
    end function legend_best

    subroutine legend_origin(loc, ax_l, ax_r, ax_t, ax_b, leg_w, leg_h, leg_x, leg_y)
        character(len=*), intent(in) :: loc
        real(dp), intent(in) :: ax_l, ax_r, ax_t, ax_b, leg_w, leg_h
        real(dp), intent(out) :: leg_x, leg_y
        ! matplotlib's borderaxespad: half the legend font size.
        real(dp), parameter :: pad = 5.0_dp

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
        class(renderer_t), intent(inout) :: b
        real(dp), intent(in) :: x1, y1, x2, y2
        call append_line(b, x1, y1, x2, y2, rc_text_color, 0.8_dp, LINE_SOLID, 1.0_dp)
    end subroutine append_tick

    subroutine append_spine(b, x1, y1, x2, y2)
        class(renderer_t), intent(inout) :: b
        real(dp), intent(in) :: x1, y1, x2, y2
        call append_line(b, x1, y1, x2, y2, rc_spine_color, rc_spine_lw, LINE_SOLID, 1.0_dp)
    end subroutine append_spine

    ! A tick at (x, y) on a spine whose outward normal is (ox, oy). dir 1
    ! puts it outside the axes, -1 inside, 0 straddling the spine.
    subroutine append_tick_at(b, x, y, ox, oy, dir, length)
        class(renderer_t), intent(inout) :: b
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

    ! A tick label, rotated about its anchor when asked.
    subroutine append_tick_text(b, x, y, s, anchor, fontsize, rot)
        class(renderer_t), intent(inout) :: b
        real(dp), intent(in) :: x, y, fontsize, rot
        character(len=*), intent(in) :: s, anchor
        call append_text(b, x, y, s, anchor, fontsize, rc_text_color, rot)
    end subroutine append_tick_text

    ! f carries what the whole axis agreed on: the decimal count, or -1
    ! when the scale writes its own labels, and anything factored out.
    subroutine tick_label(labeled, lab, i, v, sc, f, date_unit, out, n)
        logical, intent(in) :: labeled
        character(len=24), intent(in) :: lab(MAX_TICKS)
        integer, intent(in) :: i
        real(dp), intent(in) :: v
        type(scale_t), intent(in) :: sc
        type(tickfmt_t), intent(in) :: f
        integer, intent(in) :: date_unit
        character(len=*), intent(out) :: out
        integer, intent(out) :: n
        if (labeled) then
            n = len_trim(lab(i))
            out(1:n) = trim(lab(i))
        else if (date_unit > 0) then
            call format_date(v, date_unit, out, n)
        else if (sc%kind == SCALE_LOG) then
            call format_tick_to(v, .true., out, n)
        else
            select case (f%style)
            case (FMT_PERCENT)
                call format_percent(v, f%whole, f%dec, out, n)
            case (FMT_COMMA)
                call format_grouped(v, f%dec, out, n)
            case default
                call format_tick_fixed((v - f%off)/10.0_dp**f%oom, f%dec, out, n)
            end select
        end if
    end subroutine tick_label

    ! The label for one minor tick, empty unless the axis is logarithmic
    ! and short enough that matplotlib would label its multiples.
    subroutine log_minor_label(v, sc, vmin, vmax, out, n)
        real(dp), intent(in) :: v, vmin, vmax
        type(scale_t), intent(in) :: sc
        character(len=*), intent(out) :: out
        integer, intent(out) :: n
        integer :: k, expn

        n = 0
        if (sc%kind /= SCALE_LOG) return
        if (vmin <= 0.0_dp .or. vmax <= vmin .or. v <= 0.0_dp) return
        expn = floor(log10(v) + 1.0e-9_dp)
        k = nint(v/10.0_dp**expn)
        if (.not. log_minor_labelled(k, log10(vmax/vmin))) return
        call format_log_minor_to(v, out, n)
    end subroutine log_minor_label

    ! The decimal count for one axis: none when the scale is not linear,
    ! since those label themselves.
    function axis_decimals(t, nt, sc) result(d)
        real(dp), intent(in) :: t(MAX_TICKS)
        integer, intent(in) :: nt
        type(scale_t), intent(in) :: sc
        integer :: d
        if (sc%kind /= SCALE_LINEAR) then
            d = -1
        else
            d = tick_decimals(t, nt)
        end if
    end function axis_decimals

    ! The same for an axis that may factor an offset or a power of ten out
    ! of its labels. Only a linear axis does; a log or date axis writes its
    ! own labels and has nothing to factor.
    function axis_fmt(t, nt, a, is_x, vmin, vmax) result(f)
        real(dp), intent(in) :: t(MAX_TICKS), vmin, vmax
        integer, intent(in) :: nt
        type(axes_t), intent(in) :: a
        logical, intent(in) :: is_x
        type(tickfmt_t) :: f
        type(scale_t) :: sc

        sc = a%ysc
        if (is_x) sc = a%xsc
        if (sc%kind /= SCALE_LINEAR) then
            f%dec = -1
            return
        end if
        if (is_x) then
            f%style = a%xfmt_style
            f%whole = a%xfmt_whole
            call tick_offset(t, nt, vmin, vmax, f%off, f%oom, &
                             a%x_use_offset, a%x_scilo, a%x_scihi)
        else
            f%style = a%yfmt_style
            f%whole = a%yfmt_whole
            call tick_offset(t, nt, vmin, vmax, f%off, f%oom, &
                             a%y_use_offset, a%y_scilo, a%y_scihi)
        end if
        ! A named formatter writes the value itself, so nothing may be
        ! taken out of it first.
        if (f%style /= FMT_AUTO) then
            f%off = 0.0_dp
            f%oom = 0
        end if
        f%dec = tick_decimals_at(t, nt, f%off, f%oom)
        if (is_x .and. a%xfmt_dec >= 0) f%dec = a%xfmt_dec
        if (.not. is_x .and. a%yfmt_dec >= 0) f%dec = a%yfmt_dec
    end function axis_fmt

    ! A polar axes. The box holds the largest circle that fits: the angle
    ! runs anticlockwise from the right and the radius from the middle out,
    ! so the whole of the drawing is one change of coordinates away from
    ! everything else here.
    subroutine render_polar(b, a, ax_l, ax_r, ax_b, ax_t)
        class(renderer_t), intent(inout) :: b
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: ax_l, ax_r, ax_b, ax_t
        integer, parameter :: NC = 180
        real(dp) :: cx, cy, R, rmax, t(MAX_TICKS), cxs(NC + 1), cys(NC + 1)
        real(dp) :: ang, rr, lx, ly
        real(dp), allocatable :: px(:), py(:)
        character(len=64) :: lbl
        integer :: nt, i, j, k, n, unit, dec

        cx = 0.5_dp*(ax_l + ax_r)
        cy = 0.5_dp*(ax_t + ax_b)
        R = 0.5_dp*min(ax_r - ax_l, ax_b - ax_t)

        ! The radius starts at the middle and reaches 5% past the data, as
        ! matplotlib scales it.
        rmax = 0.0_dp
        do i = 1, a%n_series
            do j = 1, a%series(i)%n
                rmax = max(rmax, a%series(i)%y(j))
            end do
        end do
        if (a%ylim_set) rmax = a%ymax_user
        if (rmax <= 0.0_dp) rmax = 1.0_dp
        if (.not. a%ylim_set) rmax = 1.05_dp*rmax

        call linear_ticks(0.0_dp, rmax, tick_space(ax_b - ax_t, a%ytick_size, .false.), t, nt)
        dec = tick_decimals(t, nt)

        ! Grid: a circle at every radial tick and a spoke every 45 degrees.
        if (a%grid_on) then
            do k = 1, nt
                if (t(k) <= 0.0_dp .or. t(k) > rmax) cycle
                call polar_circle(cx, cy, R*t(k)/rmax, cxs, cys)
                call append_stroke_path(b, cxs, cys, NC + 1, rc_grid_color, 0.8_dp, 1.0_dp)
            end do
            do k = 0, 7
                ang = real(k, dp)*PI/4.0_dp
                call append_stroke_path(b, [cx, cx + R*cos(ang)], &
                                        [cy, cy - R*sin(ang)], 2, &
                                        rc_grid_color, 0.8_dp, 1.0_dp)
            end do
        end if

        ! The data, drawn straight through the change of coordinates.
        do i = 1, a%n_series
            n = a%series(i)%n
            if (n < 1) cycle
            allocate (px(n), py(n))
            do j = 1, n
                rr = R*a%series(i)%y(j)/rmax
                px(j) = cx + rr*cos(a%series(i)%x(j))
                py(j) = cy - rr*sin(a%series(i)%x(j))
            end do
            if (a%series(i)%linestyle /= LINE_NONE .and. n >= 2) &
                call append_stroke_path(b, px, py, n, trim(a%series(i)%color), &
                                        a%series(i)%linewidth, a%series(i)%alpha)
            if (a%series(i)%marker /= MARKER_NONE) &
                call append_markers(b, a%series(i)%marker, px, py, n, &
                                    a%series(i)%markersize, trim(a%series(i)%color), &
                                    a%series(i)%alpha)
            deallocate (px, py)
        end do

        ! The outer circle is the whole of the frame a polar axes has.
        call polar_circle(cx, cy, R, cxs, cys)
        call append_stroke_path(b, cxs, cys, NC + 1, rc_spine_color, 0.8_dp, 1.0_dp)

        ! Angles are labelled outside the circle, radii along the 22.5
        ! degree line, which is where matplotlib puts them.
        do k = 0, 7
            ang = real(k, dp)*PI/4.0_dp
            ! The degree sign is byte 176, which the backends know how to
            ! write: a character reference in SVG, WinAnsi in PDF, and an
            ! outline of its own in PNG.
            write (lbl, "(I0,A)") k*45, achar(176)
            lx = cx + (R + 4.0_dp + 0.5_dp*a%xtick_size)*cos(ang)
            ly = cy - (R + 4.0_dp + 0.5_dp*a%xtick_size)*sin(ang) + 0.36_dp*a%xtick_size
            call append_text(b, lx, ly, trim(lbl), "middle", a%xtick_size, rc_text_color)
        end do
        do k = 1, nt
            if (t(k) <= 0.0_dp .or. t(k) > rmax) cycle
            call format_tick_fixed(t(k), dec, lbl, n)
            rr = R*t(k)/rmax
            lx = cx + rr*cos(22.5_dp*PI/180.0_dp)
            ly = cy - rr*sin(22.5_dp*PI/180.0_dp) + 0.36_dp*a%ytick_size
            call append_text(b, lx, ly, lbl(1:n), "middle", a%ytick_size, rc_text_color)
        end do

        ! The title clears the angle label above the circle rather than
        ! sitting on the box, which would run straight through it.
        if (len_trim(a%title) > 0) &
            call append_text(b, cx, cy - R - 4.0_dp - 1.6_dp*a%xtick_size &
                             - 0.5_dp*a%title_size, trim(a%title), &
                             "center", a%title_size, rc_text_color)
    end subroutine render_polar

    ! ------------------------------------------------------------------
    ! A 3D axes. The camera lives in fplot_proj3d; what is here is what
    ! the camera is pointed at: the limits, the three back panes and their
    ! grids, the three axis lines with ticks and labels, and the data.
    !
    ! There is no depth buffer, only the painter's algorithm: faces are
    ! sorted back to front and drawn in that order. mplot3d does the same
    ! and has the same limitation, that surfaces which intersect one
    ! another come out wrong.
    ! ------------------------------------------------------------------

    ! Data bounds, then matplotlib's 3D margins: five percent in x and y,
    ! none in z, and then the whole box widened by 25/24 about its centre,
    ! which is the compensation mplot3d applies to every 3D axes.
    subroutine limits3d(a, lims)
        type(axes_t), intent(in) :: a
        real(dp), intent(out) :: lims(6)
        real(dp), parameter :: MARG(3) = [0.05_dp, 0.05_dp, 0.0_dp]
        real(dp) :: lo(3), hi(3), c, d
        logical :: have
        integer :: i, k

        lo = huge(1.0_dp)
        hi = -huge(1.0_dp)
        have = .false.
        do i = 1, a%n_series
            select case (a%series(i)%kind)
            case (SERIES_LINE3D, SERIES_SCATTER3D)
                do k = 1, a%series(i)%n
                    lo(1) = min(lo(1), a%series(i)%x(k))
                    hi(1) = max(hi(1), a%series(i)%x(k))
                    lo(2) = min(lo(2), a%series(i)%y(k))
                    hi(2) = max(hi(2), a%series(i)%y(k))
                    lo(3) = min(lo(3), a%series(i)%z(k))
                    hi(3) = max(hi(3), a%series(i)%z(k))
                end do
                have = .true.
            case (SERIES_SURFACE)
                lo(1) = min(lo(1), minval(a%series(i)%x))
                hi(1) = max(hi(1), maxval(a%series(i)%x))
                lo(2) = min(lo(2), minval(a%series(i)%y))
                hi(2) = max(hi(2), maxval(a%series(i)%y))
                lo(3) = min(lo(3), minval(a%series(i)%zg))
                hi(3) = max(hi(3), maxval(a%series(i)%zg))
                have = .true.
            end select
        end do
        if (.not. have) then
            lo = 0.0_dp
            hi = 1.0_dp
        end if

        do i = 1, 3
            if (hi(i) <= lo(i)) then
                c = 0.5_dp*(lo(i) + hi(i))
                lo(i) = c - 0.5_dp
                hi(i) = c + 0.5_dp
            end if
            d = hi(i) - lo(i)
            lo(i) = lo(i) - MARG(i)*d
            hi(i) = hi(i) + MARG(i)*d
            c = 0.5_dp*(lo(i) + hi(i))
            lo(i) = c + (lo(i) - c)*25.0_dp/24.0_dp
            hi(i) = c + (hi(i) - c)*25.0_dp/24.0_dp
            lims(2*i - 1) = lo(i)
            lims(2*i) = hi(i)
        end do

        ! Limits set by hand get none of that: they are taken as given.
        if (a%xlim_set) lims(1:2) = [a%xmin_user, a%xmax_user]
        if (a%ylim_set) lims(3:4) = [a%ymin_user, a%ymax_user]
        if (a%zlim_set) lims(5:6) = [a%zmin_user, a%zmax_user]
    end subroutine limits3d

    ! A data point to device points, with the projected depth alongside.
    subroutine dev3(M, bl, bt, side, x, y, z, ux, uy, uz)
        real(dp), intent(in) :: M(4, 4), bl, bt, side, x, y, z
        real(dp), intent(out) :: ux, uy, uz
        real(dp) :: px, py, span

        call proj3d_point(M, x, y, z, px, py, uz)
        span = PROJ3D_VIEW_MAX - PROJ3D_VIEW_MIN
        ux = bl + (px - PROJ3D_VIEW_MIN)/span*side
        uy = bt + (PROJ3D_VIEW_MAX - py)/span*side
    end subroutine dev3

    ! Push a point away from the middle of the box along every axis but
    ! one, which is how mplot3d keeps tick and axis labels clear of the box.
    pure function move_out(p, centers, d, keep) result(q)
        real(dp), intent(in) :: p(3), centers(3), d(3)
        integer, intent(in) :: keep
        real(dp) :: q(3)
        integer :: j

        q = p
        do j = 1, 3
            if (j == keep) cycle
            if (p(j) < centers(j)) then
                q(j) = p(j) - d(j)
            else
                q(j) = p(j) + d(j)
            end if
        end do
    end function move_out

    ! Order 1..n so that key decreases: the painter's algorithm needs the
    ! far things first. Insertion sort, because n is a few thousand faces
    ! at worst and the sort is not what costs.
    pure subroutine order_far_first(key, n, idx)
        real(dp), intent(in) :: key(:)
        integer, intent(in) :: n
        integer, intent(out) :: idx(:)
        integer :: i, j, t

        do i = 1, n
            idx(i) = i
        end do
        do i = 2, n
            t = idx(i)
            j = i - 1
            do while (j >= 1)
                if (key(idx(j)) >= key(t)) exit
                idx(j + 1) = idx(j)
                j = j - 1
            end do
            idx(j + 1) = t
        end do
    end subroutine order_far_first

    subroutine render_axes3d(b, a, ax_l, ax_r, ax_b, ax_t)
        class(renderer_t), intent(inout) :: b
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: ax_l, ax_r, ax_b, ax_t
        ! The six faces of the box as corner numbers: the two x faces, then
        ! the two y faces, then the two z faces.
        integer, parameter :: PLANES(4, 6) = reshape([ &
                                             1, 4, 8, 5, 2, 3, 7, 6, &
                                             1, 2, 6, 5, 4, 3, 7, 8, &
                                             1, 2, 3, 4, 5, 6, 7, 8], [4, 6])
        ! The rcParams axes3d.*axis.panecolor greys.
        character(len=7), parameter :: PANE(3) = ["#f2f2f2", "#e6e6e6", "#ececec"]
        ! Which coordinate a tick mark runs along, per axis.
        integer, parameter :: TICKDIR(3) = [2, 1, 1]
        ! The two coordinates that pin each axis line to an edge of the box.
        integer, parameter :: JUG(2, 3) = reshape([2, 1, 1, 2, 1, 3], [2, 3])
        real(dp) :: lims(6), M(4, 4), side, bl, bt
        real(dp) :: ccx(8), ccy(8), ccz(8), dx(8), dy(8), dz(8)
        real(dp) :: mins(3), maxs(3), centers(3), deltas(3), minmax(3), maxmin(3)
        real(dp) :: e1(3), e2(3), p(3), q(3), lab_d(3), tick_d(3)
        real(dp) :: gx(3), gy(3), gd(3), ex(2), ey(2), tx(2), ty(2), td2
        real(dp) :: tk(MAX_TICKS, 3), none(MAX_TICKS)
        integer :: ntk(3), dec(3)
        real(dp) :: dpp, tdel, t_out, t_in, ang, fs
        logical :: highs(3)
        integer :: i, k, j1, j2, pl, td, ln, du
        character(len=64) :: lbl
        character(len=256) :: axl
        type(scale_t) :: lin

        ! mplot3d squares the axes box before drawing, so that the picture
        ! does not stretch when the figure does.
        side = min(ax_r - ax_l, ax_b - ax_t)
        bl = 0.5_dp*(ax_l + ax_r) - 0.5_dp*side
        bt = 0.5_dp*(ax_t + ax_b) - 0.5_dp*side

        call limits3d(a, lims)
        call proj3d_matrix(lims, a%elev, a%azim, M)
        do i = 1, 3
            mins(i) = lims(2*i - 1)
            maxs(i) = lims(2*i)
        end do
        centers = 0.5_dp*(mins + maxs)
        deltas = 0.08_dp*(maxs - mins)

        ccx = [lims(1), lims(2), lims(2), lims(1), lims(1), lims(2), lims(2), lims(1)]
        ccy = [lims(3), lims(3), lims(4), lims(4), lims(3), lims(3), lims(4), lims(4)]
        ccz = [lims(5), lims(5), lims(5), lims(5), lims(6), lims(6), lims(6), lims(6)]
        do k = 1, 8
            call dev3(M, bl, bt, side, ccx(k), ccy(k), ccz(k), dx(k), dy(k), dz(k))
        end do

        ! Of each pair of parallel faces, the one further from the camera is
        ! the one that gets a pane and a grid.
        do i = 1, 3
            highs(i) = sum(dz(PLANES(:, 2*i - 1)))/4.0_dp < sum(dz(PLANES(:, 2*i)))/4.0_dp
            if (highs(i)) then
                minmax(i) = maxs(i)
                maxmin(i) = mins(i)
            else
                minmax(i) = mins(i)
                maxmin(i) = maxs(i)
            end if
        end do

        do i = 1, 3
            pl = 2*i - 1
            if (highs(i)) pl = 2*i
            call append_polygon(b, dx(PLANES(:, pl)), dy(PLANES(:, pl)), 4, &
                                PANE(i), 0.5_dp, .true.)
        end do

        ! x and y run across the picture and z up it, so x and y are spaced
        ! as a horizontal axis is and z as a vertical one.
        none = 0.0_dp
        call axis_ticks(a%n_xticks, a%xtick_pos, lims(1), lims(2), lin, &
                        tick_space(side, a%xtick_size, .true.), .false., tk(:, 1), ntk(1), du)
        call axis_ticks(a%n_yticks, a%ytick_pos, lims(3), lims(4), lin, &
                        tick_space(side, a%ytick_size, .true.), .false., tk(:, 2), ntk(2), du)
        call axis_ticks(0, none, lims(5), lims(6), lin, &
                        tick_space(side, a%xtick_size, .false.), .false., tk(:, 3), ntk(3), du)
        do i = 1, 3
            dec(i) = axis_decimals(tk(:, i), ntk(i), lin)
        end do

        if (a%grid_on) then
            do i = 1, 3
                ! A grid line runs from the far edge of one pane, through
                ! the corner where the two panes meet, to the far edge of
                ! the other.
                j1 = mod(i, 3) + 1
                j2 = mod(i + 1, 3) + 1
                do k = 1, ntk(i)
                    if (tk(k, i) < mins(i) .or. tk(k, i) > maxs(i)) cycle
                    p = minmax
                    p(i) = tk(k, i)
                    q = p
                    q(j1) = maxmin(j1)
                    call dev3(M, bl, bt, side, q(1), q(2), q(3), gx(1), gy(1), gd(1))
                    call dev3(M, bl, bt, side, p(1), p(2), p(3), gx(2), gy(2), gd(2))
                    q = p
                    q(j2) = maxmin(j2)
                    call dev3(M, bl, bt, side, q(1), q(2), q(3), gx(3), gy(3), gd(3))
                    call append_stroke_path(b, gx, gy, 3, rc_grid_color, rc_grid_lw, 1.0_dp)
                end do
            end do
        end if

        ! What one point is worth in data units. A 3D axes has no one
        ! direction to measure a point along, so mplot3d averages the two
        ! sides of the box, and so do we.
        dpp = 48.0_dp/(2.0_dp*side)
        tick_d = (3.5_dp + 8.0_dp)*dpp*deltas
        lab_d = (LABEL_PAD + 21.0_dp)*dpp*deltas

        do i = 1, 3
            fs = a%xtick_size
            e1 = minmax
            e1(JUG(1, i)) = maxmin(JUG(1, i))
            e2 = e1
            e2(JUG(2, i)) = maxmin(JUG(2, i))
            call dev3(M, bl, bt, side, e1(1), e1(2), e1(3), ex(1), ey(1), td2)
            call dev3(M, bl, bt, side, e2(1), e2(2), e2(3), ex(2), ey(2), td2)
            call append_stroke_path(b, ex, ey, 2, rc_spine_color, rc_spine_lw, 1.0_dp)

            td = TICKDIR(i)
            tdel = deltas(td)
            if (.not. highs(td)) tdel = -tdel
            t_out = e1(td) + 0.1_dp*tdel
            t_in = e1(td) - 0.2_dp*tdel

            do k = 1, ntk(i)
                if (tk(k, i) < mins(i) .or. tk(k, i) > maxs(i)) cycle
                q = e1
                q(i) = tk(k, i)
                q(td) = t_out
                call dev3(M, bl, bt, side, q(1), q(2), q(3), tx(1), ty(1), td2)
                q(td) = t_in
                call dev3(M, bl, bt, side, q(1), q(2), q(3), tx(2), ty(2), td2)
                call append_stroke_path(b, tx, ty, 2, rc_spine_color, 0.8_dp, 1.0_dp)

                q(td) = e1(td)
                q = move_out(q, centers, tick_d, i)
                call dev3(M, bl, bt, side, q(1), q(2), q(3), tx(1), ty(1), td2)
                ! All three axes label their ticks the way an x axis does:
                ! centred, and hanging from the top of the text.
                call format_tick_fixed(tk(k, i), dec(i), lbl, ln)
                call append_text(b, tx(1), ty(1) + 0.76_dp*fs, lbl(1:ln), &
                                 "center", fs, rc_text_color)
            end do

            axl = ""
            if (i == 1) axl = a%xlabel
            if (i == 2) axl = a%ylabel
            if (i == 3) axl = a%zlabel
            if (len_trim(axl) == 0) cycle
            q = move_out(0.5_dp*(e1 + e2), centers, lab_d, i)
            call dev3(M, bl, bt, side, q(1), q(2), q(3), tx(1), ty(1), td2)
            ! A short label stays upright; a long one is turned to lie along
            ! its own axis, which is where matplotlib draws the line too.
            ang = 0.0_dp
            if (len_trim(axl) > 4) ang = atan2(ey(1) - ey(2), ex(2) - ex(1))*180.0_dp/PI
            call append_text(b, tx(1), ty(1) + 0.36_dp*a%xlabel_size, trim(axl), &
                             "center", a%xlabel_size, rc_text_color, ang)
        end do

        call render_series3d(b, a, M, bl, bt, side)

        if (len_trim(a%title) > 0) &
            call append_text(b, bl + 0.5_dp*side, bt - 0.5_dp*a%title_size, &
                             trim(a%title), "center", a%title_size, rc_text_color)
    end subroutine render_axes3d

    subroutine render_series3d(b, a, M, bl, bt, side)
        class(renderer_t), intent(inout) :: b
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: M(4, 4), bl, bt, side
        real(dp), allocatable :: px(:), py(:), pd(:)
        integer, allocatable :: idx(:)
        real(dp) :: sat, lo, span, one(1), oney(1)
        integer :: i, k, n
        type(paint_t) :: p

        do i = 1, a%n_series
            n = a%series(i)%n
            select case (a%series(i)%kind)
            case (SERIES_LINE3D)
                allocate (px(n), py(n), pd(n))
                do k = 1, n
                    call dev3(M, bl, bt, side, a%series(i)%x(k), a%series(i)%y(k), &
                              a%series(i)%z(k), px(k), py(k), pd(k))
                end do
                if (a%series(i)%linestyle /= LINE_NONE) then
                    p = pen(a%series(i)%color, a%series(i)%linewidth, &
                            a%series(i)%alpha, a%series(i)%linestyle)
                    call b%draw_path(px, py, line_verbs(n), n, p)
                end if
                call append_markers(b, a%series(i)%marker, px, py, n, &
                                    a%series(i)%markersize, a%series(i)%color, &
                                    a%series(i)%alpha)
                deallocate (px, py, pd)
            case (SERIES_SCATTER3D)
                allocate (px(n), py(n), pd(n), idx(n))
                do k = 1, n
                    call dev3(M, bl, bt, side, a%series(i)%x(k), a%series(i)%y(k), &
                              a%series(i)%z(k), px(k), py(k), pd(k))
                end do
                call order_far_first(pd, n, idx)
                ! Depth shading: the further a point is, the more it fades
                ! into the background, down to three tenths opacity.
                lo = minval(pd)
                span = sqrt((maxval(px) - minval(px))**2 + (maxval(py) - minval(py))**2 &
                            + (maxval(pd) - lo)**2)
                do k = 1, n
                    sat = 1.0_dp
                    if (span > 0.0_dp) sat = 1.0_dp - (pd(idx(k)) - lo)/span
                    sat = max(0.3_dp, min(1.0_dp, sat))
                    one(1) = px(idx(k))
                    oney(1) = py(idx(k))
                    call append_markers(b, a%series(i)%marker, one, oney, 1, &
                                        a%series(i)%markersize, a%series(i)%color, &
                                        a%series(i)%alpha*sat)
                end do
                deallocate (px, py, pd, idx)
            case (SERIES_SURFACE)
                call render_surface(b, a%series(i), M, bl, bt, side)
            end select
        end do
    end subroutine render_series3d

    ! One quadrilateral per cell of the grid, lit by matplotlib's default
    ! light source and painted back to front.
    subroutine render_surface(b, s, M, bl, bt, side)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: M(4, 4), bl, bt, side
        ! LightSource(azdeg=225, altdeg=19.4712), as a direction.
        real(dp), parameter :: AZ = (90.0_dp - 225.0_dp)*PI/180.0_dp
        real(dp), parameter :: ALT = 19.4712_dp*PI/180.0_dp
        real(dp) :: dir(3)
        real(dp), allocatable :: fx(:, :), fy(:, :), depth(:)
        integer, allocatable :: idx(:)
        real(dp) :: cx(4), cy(4), cz(4), ux, uy, uz, v1(3), v2(3), nrm(3), nl, shade
        real(dp) :: zlo, zhi, t
        integer :: nx, ny, nf, f, i, j, c, rgb(3)
        character(len=7) :: col

        dir = [cos(AZ)*cos(ALT), sin(AZ)*cos(ALT), sin(ALT)]
        nx = size(s%x)
        ny = size(s%y)
        nf = (nx - 1)*(ny - 1)
        if (nf <= 0) return
        if (s%wire) then
            call render_wireframe(b, s, M, bl, bt, side)
            return
        end if
        allocate (fx(4, nf), fy(4, nf), depth(nf), idx(nf))
        rgb = hex_rgb(s%color)
        zlo = minval(s%zg)
        zhi = maxval(s%zg)

        f = 0
        do j = 1, ny - 1
            do i = 1, nx - 1
                f = f + 1
                ! The perimeter of the cell, anticlockwise in index space.
                cx = [s%x(i), s%x(i + 1), s%x(i + 1), s%x(i)]
                cy = [s%y(j), s%y(j), s%y(j + 1), s%y(j + 1)]
                cz = [s%zg(j, i), s%zg(j, i + 1), s%zg(j + 1, i + 1), s%zg(j + 1, i)]
                depth(f) = 0.0_dp
                do c = 1, 4
                    call dev3(M, bl, bt, side, cx(c), cy(c), cz(c), ux, uy, uz)
                    fx(c, f) = ux
                    fy(c, f) = uy
                    depth(f) = depth(f) + 0.25_dp*uz
                end do
            end do
        end do

        call order_far_first(depth, nf, idx)

        f = 0
        do j = 1, ny - 1
            do i = 1, nx - 1
                f = f + 1
                cx = [s%x(i), s%x(i + 1), s%x(i + 1), s%x(i)]
                cy = [s%y(j), s%y(j), s%y(j + 1), s%y(j + 1)]
                cz = [s%zg(j, i), s%zg(j, i + 1), s%zg(j + 1, i + 1), s%zg(j + 1, i)]
                v1 = [cx(1) - cx(2), cy(1) - cy(2), cz(1) - cz(2)]
                v2 = [cx(2) - cx(3), cy(2) - cy(3), cz(2) - cz(3)]
                nrm = [v1(2)*v2(3) - v1(3)*v2(2), v1(3)*v2(1) - v1(1)*v2(3), &
                       v1(1)*v2(2) - v1(2)*v2(1)]
                nl = sqrt(sum(nrm**2))
                shade = 0.0_dp
                if (nl > 0.0_dp) shade = dot_product(nrm/nl, dir)
                depth(f) = 0.3_dp + 0.7_dp*(shade + 1.0_dp)/2.0_dp
            end do
        end do

        do f = 1, nf
            ! depth now carries the lighting factor for each face, and idx
            ! the order to paint them in.
            if (s%scmap >= 0) then
                ! A colormapped surface takes its color from the height of
                ! the cell, and is then lit exactly as a flat one is.
                j = (idx(f) - 1)/(nx - 1) + 1
                i = idx(f) - (j - 1)*(nx - 1)
                t = 0.0_dp
                if (zhi > zlo) t = (0.25_dp*(s%zg(j, i) + s%zg(j, i + 1) &
                                             + s%zg(j + 1, i) + s%zg(j + 1, i + 1)) &
                                    - zlo)/(zhi - zlo)
                rgb = hex_rgb(cmap_color(s%scmap, t))
            end if
            col = "#"//hex_pair(nint(rgb(1)*depth(idx(f)))) &
                  //hex_pair(nint(rgb(2)*depth(idx(f)))) &
                  //hex_pair(nint(rgb(3)*depth(idx(f))))
            call append_polygon(b, fx(:, idx(f)), fy(:, idx(f)), 4, col, s%alpha, .true.)
        end do
        deallocate (fx, fy, depth, idx)
    end subroutine render_surface

    ! Every line of the mesh, in front and behind alike: matplotlib hides
    ! nothing in a wireframe either.
    subroutine render_wireframe(b, s, M, bl, bt, side)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: M(4, 4), bl, bt, side
        real(dp), allocatable :: gx(:, :), gy(:, :)
        real(dp) :: uz
        integer :: nx, ny, i, j

        nx = size(s%x)
        ny = size(s%y)
        allocate (gx(ny, nx), gy(ny, nx))
        do j = 1, ny
            do i = 1, nx
                call dev3(M, bl, bt, side, s%x(i), s%y(j), s%zg(j, i), &
                          gx(j, i), gy(j, i), uz)
            end do
        end do
        do j = 1, ny
            call append_stroke_path(b, gx(j, :), gy(j, :), nx, s%color, s%linewidth, &
                                    s%alpha)
        end do
        do i = 1, nx
            call append_stroke_path(b, gx(:, i), gy(:, i), ny, s%color, s%linewidth, &
                                    s%alpha)
        end do
        deallocate (gx, gy)
    end subroutine render_wireframe

    pure subroutine polar_circle(cx, cy, r, px, py)
        real(dp), intent(in) :: cx, cy, r
        real(dp), intent(out) :: px(:), py(:)
        integer :: i, n
        real(dp) :: t

        n = size(px) - 1
        do i = 1, n + 1
            t = 2.0_dp*PI*real(i - 1, dp)/real(n, dp)
            px(i) = cx + r*cos(t)
            py(i) = cy - r*sin(t)
        end do
    end subroutine polar_circle

    subroutine render_axes(b, a, idx, W, H, clear)
        class(renderer_t), intent(inout) :: b
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
        integer :: x_unit, y_unit
        real(dp) :: px, py, ms, r, mid
        real(dp) :: x_edge, x_out, y_edge, y_out
        character(len=64) :: lbl
        character(len=64) :: tx, ty
        type(tickfmt_t) :: xfmt, yfmt
        integer :: tn, tyn
        character(len=512) :: esc
        integer :: ln, en
        type(scale_t) :: xsc, ysc
        integer :: n_leg, k, max_lbl, n_col, n_row, lc, lr
        real(dp) :: leg_x, leg_y, leg_w, leg_h, row_h, col_w, ttl_h, leg_x0
        character(len=16) :: leg_loc
        real(dp), allocatable :: lx(:), ly(:), mkx(:), mky(:)
        logical, allocatable :: lstart(:)
        integer, allocatable :: ord(:)
        logical :: brk, grid_done
        integer :: nm
        integer :: k0, k1, ii
        type(paint_t) :: pnt

        ax_l = a%left * W
        ax_r = a%right * W
        ax_b = (1.0_dp - a%bottom) * H
        ax_t = (1.0_dp - a%top) * H
        ax_w = ax_r - ax_l
        ax_h = ax_b - ax_t

        if (a%polar) then
            call render_polar(b, a, ax_l, ax_r, ax_b, ax_t)
            return
        end if

        if (a%is3d) then
            call render_axes3d(b, a, ax_l, ax_r, ax_b, ax_t)
            return
        end if

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

        call axis_ticks(a%n_xticks, a%xtick_pos, xmin, xmax, xsc, &
                        nbins_for(a%xtick_nbins, ax_w, a%xtick_size, .true.), a%x_date, &
                        xticks, nxt, x_unit, a%xtick_base)
        call axis_ticks(a%n_yticks, a%ytick_pos, min(ymin, ymax), max(ymin, ymax), &
                        ysc, nbins_for(a%ytick_nbins, ax_h, a%ytick_size, .false.), &
                        a%y_date, yticks, nyt, y_unit, a%ytick_base)
        if (a%n_xticks == 0) call prune_ticks(a%xtick_prune, xticks, nxt)
        if (a%n_yticks == 0) call prune_ticks(a%ytick_prune, yticks, nyt)
        xfmt = axis_fmt(xticks, nxt, a, .true., xmin, xmax)
        yfmt = axis_fmt(yticks, nyt, a, .false., min(ymin, ymax), max(ymin, ymax))
        if (wants_minor(a) .or. a%xsc%kind == SCALE_LOG .or. &
            a%ysc%kind == SCALE_LOG) then
            call minor_positions(xticks, nxt, xmin, xmax, xsc, xminor, nxm)
            call minor_positions(yticks, nyt, ymin, ymax, ysc, yminor, nym)
        else
            nxm = 0
            nym = 0
        end if
        ! Minor ticks placed by hand stand in for the automatic ones, and
        ! asking for them is asking to see them.
        if (a%n_xminor > 0) then
            nxm = a%n_xminor
            xminor(1:nxm) = a%xminor_pos(1:nxm)
        end if
        if (a%n_yminor > 0) then
            nym = a%n_yminor
            yminor(1:nym) = a%yminor_pos(1:nym)
        end if

        ! axis("off") leaves only the artists: no frame, no ticks, no labels.
        if (a%frame_off) then
            nxt = 0
            nyt = 0
            nxm = 0
            nym = 0
        end if

        ! axes face
        if (.not. clear .and. .not. a%patch_off) then
            if (len_trim(a%facecolor) > 0) then
                call append_rect(b, ax_l, ax_t, ax_w, ax_h, trim(a%facecolor), &
                                 a%face_alpha)
            else
                call append_rect(b, ax_l, ax_t, ax_w, ax_h, rc_axes_face)
            end if
        end if

        if (a%has_img .or. a%has_cont) then
            call set_clip(ax_l, ax_t, ax_w, ax_h)
            if (a%has_img) &
                call append_image(b, a, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            if (a%has_cont) &
                call append_contour(b, a, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call clear_clip()
        end if

        ! data, layered rather than in call order. Series of equal zorder
        ! keep the order they were added in, which is what matplotlib's
        ! stable sort does too.
        allocate (ord(max(1, a%n_series)))
        do i = 1, a%n_series
            ord(i) = i
        end do
        do i = 2, a%n_series
            k0 = ord(i)
            k1 = i - 1
            do while (k1 >= 1)
                if (series_z(a%series(ord(k1))) <= series_z(a%series(k0))) exit
                ord(k1 + 1) = ord(k1)
                k1 = k1 - 1
            end do
            ord(k1 + 1) = k0
        end do

        call set_clip(ax_l, ax_t, ax_w, ax_h)
        grid_done = .false.
        do ii = 1, a%n_series
            i = ord(ii)
            if (.not. grid_done .and. series_z(a%series(i)) >= Z_GRID) then
                call append_grid(b, a, xticks, nxt, yticks, nyt, xminor, nxm, &
                                 yminor, nym, xmin, xmax, &
                                 ymin, ymax, ax_l, ax_r, ax_t, ax_b, ax_w, ax_h, &
                                 xsc, ysc)
                grid_done = .true.
            end if
            n = a%series(i)%n
            if (n <= 0) cycle
            if (allocated(lstart)) deallocate (lstart)
            if (allocated(lx)) deallocate (lx)
            if (allocated(ly)) deallocate (ly)
            if (allocated(mkx)) deallocate (mkx)
            if (allocated(mky)) deallocate (mky)
            allocate (lx(n), ly(n), lstart(n), mkx(n), mky(n))

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
            case (SERIES_AXLINE)
                call append_axline(b, a%series(i), xmin, xmax, ymin, ymax, &
                                   ax_l, ax_w, ax_b, ax_h, xsc, ysc)
                cycle
            case (SERIES_HLINES)
                do j = 1, n
                    py = map_y(a%series(i)%y(j), ymin, ymax, ax_b, ax_h, ysc)
                    call append_line(b, &
                        map_x(a%series(i)%x(j), xmin, xmax, ax_l, ax_w, xsc), py, &
                        map_x(a%series(i)%y2(j), xmin, xmax, ax_l, ax_w, xsc), py, &
                        trim(a%series(i)%color), a%series(i)%linewidth, &
                        a%series(i)%linestyle, a%series(i)%alpha)
                end do
                cycle
            case (SERIES_VLINES)
                do j = 1, n
                    px = map_x(a%series(i)%x(j), xmin, xmax, ax_l, ax_w, xsc)
                    call append_line(b, px, &
                        map_y(a%series(i)%y(j), ymin, ymax, ax_b, ax_h, ysc), px, &
                        map_y(a%series(i)%y2(j), ymin, ymax, ax_b, ax_h, ysc), &
                        trim(a%series(i)%color), a%series(i)%linewidth, &
                        a%series(i)%linestyle, a%series(i)%alpha)
                end do
                cycle
            case (SERIES_PATCH)
                call append_patch(b, a%series(i), xmin, xmax, ymin, ymax, &
                                  ax_l, ax_w, ax_b, ax_h, xsc, ysc)
                cycle
            case (SERIES_QUIVER)
                call append_quiver(b, a%series(i), xmin, xmax, ymin, ymax, &
                                   ax_l, ax_w, ax_b, ax_h, xsc, ysc)
                cycle
            case (SERIES_ARROWHEAD)
                call append_arrowheads(b, a%series(i), xmin, xmax, ymin, ymax, &
                                       ax_l, ax_w, ax_b, ax_h, xsc, ysc)
                cycle
            case (SERIES_HSPAN, SERIES_VSPAN)
                call append_span(b, a%series(i), xmin, xmax, ymin, ymax, &
                                 ax_l, ax_w, ax_b, ax_h, xsc, ysc)
                cycle
            case (SERIES_ERRORBAR)
                do j = 1, n
                    call append_errorbar(b, a%series(i), j, xmin, xmax, ymin, ymax, &
                                         ax_l, ax_w, ax_b, ax_h, xsc, ysc)
                end do
            end select

            ! Points that cannot be drawn are dropped once, and both the line
            ! and its markers then work from the same list. A point is
            ! missing if it is NaN or infinite, or if a log axis cannot
            ! place it; lstart then remembers where the line has to restart,
            ! because matplotlib leaves a gap rather than drawing across it.
            nl = 0
            brk = .true.
            do j = 1, n
                if (.not. finite(a%series(i)%x(j)) .or. &
                    .not. finite(a%series(i)%y(j)) .or. &
                    (a%xsc%kind == SCALE_LOG .and. a%series(i)%x(j) <= 0.0_dp) .or. &
                    (a%ysc%kind == SCALE_LOG .and. a%series(i)%y(j) <= 0.0_dp)) then
                    brk = .true.
                    cycle
                end if
                nl = nl + 1
                lx(nl) = map_x(a%series(i)%x(j), xmin, xmax, ax_l, ax_w, xsc)
                ly(nl) = map_y(a%series(i)%y(j), ymin, ymax, ax_b, ax_h, ysc)
                lstart(nl) = brk
                brk = .false.
            end do

            if (a%series(i)%linestyle /= LINE_NONE .and. nl >= 2) then
                pnt = pen(trim(a%series(i)%color), a%series(i)%linewidth, &
                          a%series(i)%alpha, a%series(i)%linestyle)
                if (a%series(i)%n_dash > 0) then
                    pnt%n_dash = a%series(i)%n_dash
                    pnt%dash(1:pnt%n_dash) = &
                        a%series(i)%dashes(1:a%series(i)%n_dash)
                end if
                pnt%join = JOIN_ROUND
                pnt%cap = CAP_BUTT
                k0 = 1
                do while (k0 <= nl)
                    k1 = k0 + 1
                    do while (k1 <= nl)
                        if (lstart(k1)) exit
                        k1 = k1 + 1
                    end do
                    if (k1 - k0 >= 2) &
                        call b%draw_path(lx(k0:k1 - 1), ly(k0:k1 - 1), &
                                         line_verbs(k1 - k0), k1 - k0, pnt)
                    k0 = k1
                end do
            end if

            if (a%series(i)%marker /= MARKER_NONE .and. nl > 0) then
                if (allocated(a%series(i)%psize) .or. allocated(a%series(i)%pcolor)) then
                    ! scatter gave every point its own size or color, so the
                    ! shape cannot be shared between them.
                    ms = a%series(i)%markersize
                    do j = 1, nl
                        if (allocated(a%series(i)%psize)) ms = a%series(i)%psize(j)
                        call append_marker(b, a%series(i)%marker, lx(j), ly(j), ms, &
                                           point_color(a%series(i), j), &
                                           a%series(i)%alpha)
                    end do
                else
                    ! markevery thins the points; the line keeps all of them.
                    nm = 0
                    do j = 1, nl, a%series(i)%markevery
                        nm = nm + 1
                        mkx(nm) = lx(j)
                        mky(nm) = ly(j)
                    end do
                    call append_markers(b, a%series(i)%marker, mkx, mky, nm, &
                                        a%series(i)%markersize, &
                                        trim(a%series(i)%color), a%series(i)%alpha, &
                                        face=trim(a%series(i)%mfc), &
                                        edge=trim(a%series(i)%mec), &
                                        ewidth=a%series(i)%mew)
                end if
            end if
        end do
        if (.not. grid_done) &
            call append_grid(b, a, xticks, nxt, yticks, nyt, xminor, nxm, &
                                 yminor, nym, xmin, xmax, &
                             ymin, ymax, ax_l, ax_r, ax_t, ax_b, ax_w, ax_h, xsc, ysc)
        ! annotations, in data coordinates
        do i = 1, a%n_texts
            ! figtext is placed on the canvas, so it waits until the clip
            ! is dropped below.
            if (a%texts(i)%in_fig) cycle
            if (a%texts(i)%in_axes) then
                px = ax_l + a%texts(i)%x*ax_w
                py = ax_b - a%texts(i)%y*ax_h
            else
                px = map_x(a%texts(i)%x, xmin, xmax, ax_l, ax_w, xsc)
                py = map_y(a%texts(i)%y, ymin, ymax, ax_b, ax_h, ysc)
            end if
            if (a%texts(i)%has_arrow) &
                call append_arrow(b, a%texts(i), px, py, &
                                  map_x(a%texts(i)%xtail, xmin, xmax, ax_l, ax_w, xsc), &
                                  map_y(a%texts(i)%ytail, ymin, ymax, ax_b, ax_h, ysc))
            call append_annotation(b, a%texts(i), px, py)
        end do

        call clear_clip()

        do i = 1, a%n_texts
            if (.not. a%texts(i)%in_fig) cycle
            call append_annotation(b, a%texts(i), a%texts(i)%x*W, &
                                   (1.0_dp - a%texts(i)%y)*H)
        end do

        ! spines. All four still go out as one <rect>, so a plot that leaves
        ! them alone renders exactly as it did before they could be hidden.
        if (.not. a%frame_off) then
            if (all(a%spine)) then
                call b%draw_rect(ax_l, ax_t, ax_w, ax_h, pen(rc_spine_color, rc_spine_lw))
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
        if (a%xaxis_off .or. .not. (a%minor_ticks .or. a%n_xminor > 0 .or. &
                                    xsc%kind == SCALE_LOG)) nxm = 0
        if (a%yaxis_off .or. .not. (a%minor_ticks .or. a%n_yminor > 0 .or. &
                                    ysc%kind == SCALE_LOG)) nym = 0

        do i = 1, nxt
            px = map_x(xticks(i), xmin, xmax, ax_l, ax_w, xsc)
            call append_tick_at(b, px, x_edge, 0.0_dp, x_out, a%xtick_dir, a%xtick_len)
            if (.not. a%xticklabels_off) then
                call tick_label(a%xtick_labeled, a%xtick_lab, i, xticks(i), xsc, &
                                xfmt, x_unit, lbl, ln)
                call append_tick_text(b, px, x_edge + x_out * xtick_gap(a) - &
                                      merge(6.0_dp, 0.0_dp, a%x_top), lbl(1:ln), "center", &
                                      a%xtick_size, a%xtick_rot)
            end if
        end do
        do i = 1, nxm
            px = map_x(xminor(i), xmin, xmax, ax_l, ax_w, xsc)
            call append_tick_at(b, px, x_edge, 0.0_dp, x_out, a%xtick_dir, &
                                MINOR_FRAC * a%xtick_len)
            ! A log axis spanning a decade or less has too few decades to
            ! label, so matplotlib labels the multiples between them.
            if (a%xticklabels_off) cycle
            call log_minor_label(xminor(i), xsc, xmin, xmax, lbl, ln)
            if (ln > 0) &
                call append_tick_text(b, px, x_edge + x_out * xtick_gap(a) - &
                                      merge(6.0_dp, 0.0_dp, a%x_top), lbl(1:ln), &
                                      "center", a%xtick_size, a%xtick_rot)
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
            if (.not. a%yticklabels_off) then
                call tick_label(a%ytick_labeled, a%ytick_lab, i, yticks(i), ysc, &
                                yfmt, y_unit, lbl, ln)
                call append_tick_text(b, y_edge + y_out * 7.0_dp, py + 3.5_dp, lbl(1:ln), &
                                      merge("left ", "right", a%y_right), &
                                      a%ytick_size, a%ytick_rot)
            end if
        end do
        do i = 1, nym
            py = map_y(yminor(i), ymin, ymax, ax_b, ax_h, ysc)
            call append_tick_at(b, y_edge, py, y_out, 0.0_dp, a%ytick_dir, &
                                MINOR_FRAC * a%ytick_len)
            if (a%yticklabels_off) cycle
            call log_minor_label(yminor(i), ysc, ymin, ymax, lbl, ln)
            if (ln > 0) &
                call append_tick_text(b, y_edge + y_out * 7.0_dp, py + 3.5_dp, &
                                      lbl(1:ln), merge("left ", "right", a%y_right), &
                                      a%ytick_size, a%ytick_rot)
        end do

        ! What the labels left out, written once at the end of the axis:
        ! matplotlib puts it past the far end of the x axis and above the
        ! top of the y axis, three points clear of the tick labels.
        if (nxt > 0 .and. .not. a%xticklabels_off) then
            call format_offset_text(xfmt%off, xfmt%oom, lbl, ln)
            if (ln > 0) &
                call append_text(b, ax_r, ax_b + xtick_gap(a) + 0.24_dp*a%xtick_size &
                                 + 3.0_dp + 0.76_dp*a%xtick_size, lbl(1:ln), "right", &
                                 a%xtick_size, rc_text_color)
        end if
        if (nyt > 0 .and. .not. a%yticklabels_off) then
            call format_offset_text(yfmt%off, yfmt%oom, lbl, ln)
            if (ln > 0) &
                call append_text(b, y_edge, ax_t - 3.0_dp, lbl(1:ln), &
                                 merge("right", "left ", a%y_right), &
                                 a%ytick_size, rc_text_color)
        end if

        if (len_trim(a%xlabel) > 0) then
            call append_text(b, 0.5_dp * (ax_l + ax_r), &
                             ax_b + xtick_gap(a) + 0.24_dp * a%xtick_size + 1.84_dp &
                             + a%xlabel_pad - LABEL_PAD &
                             + 0.76_dp * a%xlabel_size, trim(a%xlabel), &
                             "center", a%xlabel_size, rc_text_color, &
                             weight=a%xlabel_w, slant=a%xlabel_sl)
        end if

        if (len_trim(a%ylabel) > 0) then
            ! The right-hand label of a twinx faces the other way, so that it
            ! reads from outside the axes just as the left-hand one does.
            mid = y_edge + y_out * ylabel_out(a)
            call append_text(b, mid, 0.5_dp*(ax_t + ax_b), trim(a%ylabel), &
                             "center", a%ylabel_size, rc_text_color, &
                             merge(-90.0_dp, 90.0_dp, a%y_right), &
                             weight=a%ylabel_w, slant=a%ylabel_sl)
        end if

        ! title, centred over the axes unless it was asked to sit against
        ! one end of it
        if (len_trim(a%title) > 0) then
            select case (trim(a%title_loc))
            case ("left")
                call append_text(b, ax_l, ax_t - 0.5_dp*a%title_size, &
                                 trim(a%title), "left", a%title_size, &
                                 rc_text_color, weight=a%title_w, slant=a%title_sl)
            case ("right")
                call append_text(b, ax_r, ax_t - 0.5_dp*a%title_size, &
                                 trim(a%title), "right", a%title_size, &
                                 rc_text_color, weight=a%title_w, slant=a%title_sl)
            case default
                call append_text(b, 0.5_dp*(ax_l + ax_r), &
                                 ax_t - 0.5_dp*a%title_size, trim(a%title), &
                                 "center", a%title_size, rc_text_color, &
                                 weight=a%title_w, slant=a%title_sl)
            end select
        end if


        ! legend
        if (a%legend_on) then
            n_leg = 0
            max_lbl = 0
            do i = 1, a%n_series
                if (in_legend(a%series(i))) then
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
                    leg_loc = a%legend_loc
                    if (trim(leg_loc) == "best") &
                        leg_loc = legend_best(a, xmin, xmax, ymin, ymax, xsc, ysc, &
                                              ax_l, ax_r, ax_t, ax_b, leg_w, leg_h)
                    call legend_origin(leg_loc, ax_l, ax_r, ax_t, ax_b, &
                                       leg_w, leg_h, leg_x, leg_y)
                end if
                if (len_trim(a%legend_title) > 0) then
                    call append_text(b, leg_x + 0.5_dp * leg_w, &
                                     leg_y + 4.0_dp + 0.5_dp * row_h + 3.5_dp, &
                                     trim(a%legend_title), "center", &
                                     a%legend_size, rc_text_color)
                end if
                if (a%legend_frame) then
                    pnt = brush(rc_legend_face)
                    pnt%stroked = .true.
                    pnt%stroke_rgb = hex_rgb(rc_legend_edge)
                    pnt%line_width = 0.8_dp
                    call b%draw_rect(leg_x, leg_y, leg_w, leg_h, pnt, 2.0_dp)
                end if
                leg_x0 = leg_x
                k = 0
                do i = 1, a%n_series
                    if (.not. in_legend(a%series(i))) cycle
                    k = k + 1
                    lc = (k - 1) / n_row
                    lr = k - 1 - lc * n_row
                    leg_x = leg_x0 + real(lc, dp) * col_w
                    py = leg_y + 4.0_dp + ttl_h + (real(lr, dp) + 0.5_dp) * row_h
                    if (is_patch_series(a%series(i)%kind)) then
                        ! Anything filled shows a swatch, not a line: a bar
                        ! or a band has no line to show.
                        call append_rect(b, leg_x + 8.0_dp, &
                                         py - 0.35_dp*a%legend_size, 20.0_dp, &
                                         0.7_dp*a%legend_size, &
                                         point_color(a%series(i), 1), &
                                         a%series(i)%alpha)
                    else if (a%series(i)%linestyle /= LINE_NONE) then
                        call append_line(b, leg_x + 8.0_dp, py, leg_x + 28.0_dp, py, &
                                         trim(a%series(i)%color), &
                                         a%series(i)%linewidth, &
                                         a%series(i)%linestyle, 1.0_dp)
                    end if
                    mid = leg_x + 18.0_dp
                    if (a%series(i)%marker /= MARKER_NONE) then
                        call append_marker(b, a%series(i)%marker, mid, py, &
                                           a%series(i)%markersize, &
                                           trim(a%series(i)%color), a%series(i)%alpha)
                    end if
                    call append_text(b, leg_x + 34.0_dp, py + 3.5_dp, &
                                     trim(a%series(i)%label), &
                                     "left", a%legend_size, rc_text_color)
                end do
            end if
        end if

        call render_table(b, a, ax_l, ax_w, ax_b, ax_h)
    end subroutine render_axes

    ! Draw the whole figure into any backend. Everything above this point is
    ! format independent; the only thing render_svg and its PDF and PNG
    ! counterparts add is the choice of renderer.
    subroutine render_figure(b, facecolor, transparent, bbox_inches, pad_inches)
        class(renderer_t), intent(inout) :: b
        character(len=*), intent(in), optional :: facecolor, bbox_inches
        logical, intent(in), optional :: transparent
        real(dp), intent(in), optional :: pad_inches
        real(dp) :: W, H, vx, vy, vw, vh, bpad
        character(len=512) :: esc
        character(len=7) :: face
        logical :: clear
        integer :: i, en, n_grp

        call ensure_fig()
        ! constrained_layout fits the decorations just before the drawing
        ! goes out, when every label the figure will carry is known.
        if (fig_constrained) call tight_layout()
        clear = .false.
        if (present(transparent)) clear = transparent
        face = resolve_color(facecolor)
        if (len_trim(face) == 0) face = rc_fig_face
        call clear_clip()

        W = fig_w_in*PT_PER_IN
        H = fig_h_in*PT_PER_IN
        vx = 0.0_dp
        vy = 0.0_dp
        vw = W
        vh = H
        if (present(bbox_inches)) then
            if (lower(trim(bbox_inches)) == "tight") then
                bpad = 0.1_dp
                if (present(pad_inches)) bpad = pad_inches
                bpad = bpad*PT_PER_IN
                ! Cropping moves the window onto the drawing, so the drawing
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

        ! transparent drops the figure patch entirely
        if (clear) then
            call b%open_canvas(vw, vh, x0=vx, y0=vy)
        else
            call b%open_canvas(vw, vh, bg_rgb=hex_rgb(face), x0=vx, y0=vy)
        end if

        ! matplotlib counts a colorbar as an axes in its own right, and it is
        ! drawn after the axes it belongs to, so the numbering interleaves.
        n_grp = 0
        do i = 1, n_ax
            n_grp = n_grp + 1
            call b%begin_group("axes_"//int_to_str(n_grp))
            call render_axes(b, ax(i), i, W, H, clear)
            call b%end_group()
            if (ax(i)%cbar_on) then
                n_grp = n_grp + 1
                call b%begin_group("axes_"//int_to_str(n_grp))
                call append_colorbar(b, ax(i), i, W, H)
                call b%end_group()
            end if
        end do

        ! suptitle (figure-level, above all axes)
        if (len_trim(fig_suptitle) > 0) then
            call append_text(b, 0.5_dp*W, (1.0_dp - SUPTITLE_Y)*H + 4.2_dp, &
                             trim(fig_suptitle), "center", fig_suptitle_size, &
                             rc_text_color, weight=fig_suptitle_w, &
                             slant=fig_suptitle_sl)
        end if

        call b%close_canvas()
    end subroutine render_figure

    function render_svg(facecolor, transparent, bbox_inches, pad_inches) result(svg)
        character(len=*), intent(in), optional :: facecolor, bbox_inches
        logical, intent(in), optional :: transparent
        real(dp), intent(in), optional :: pad_inches
        character(len=:), allocatable :: svg
        type(svg_renderer_t) :: r
        call render_figure(r, facecolor, transparent, bbox_inches, pad_inches)
        svg = r%bytes()
    end function render_svg

    function render_pdf(facecolor, transparent, bbox_inches, pad_inches) result(pdf)
        character(len=*), intent(in), optional :: facecolor, bbox_inches
        logical, intent(in), optional :: transparent
        real(dp), intent(in), optional :: pad_inches
        character(len=:), allocatable :: pdf
        type(pdf_renderer_t) :: r
        call render_figure(r, facecolor, transparent, bbox_inches, pad_inches)
        pdf = r%bytes()
    end function render_pdf

    ! Unlike the vector formats, dpi is not decorative here: it is what
    ! decides how many pixels the figure is rasterized into.
    function render_eps(facecolor, transparent, bbox_inches, pad_inches) result(eps)
        character(len=*), intent(in), optional :: facecolor, bbox_inches
        logical, intent(in), optional :: transparent
        real(dp), intent(in), optional :: pad_inches
        character(len=:), allocatable :: eps
        type(eps_renderer_t) :: r
        call render_figure(r, facecolor, transparent, bbox_inches, pad_inches)
        eps = r%bytes()
    end function render_eps

    function render_png(facecolor, transparent, bbox_inches, pad_inches) result(png)
        character(len=*), intent(in), optional :: facecolor, bbox_inches
        logical, intent(in), optional :: transparent
        real(dp), intent(in), optional :: pad_inches
        character(len=:), allocatable :: png
        type(png_renderer_t) :: r
        call r%set_dpi(fig_dpi)
        call render_figure(r, facecolor, transparent, bbox_inches, pad_inches)
        png = r%bytes()
    end function render_png

    ! The extension picks the backend, as it does in matplotlib. A name with
    ! no extension at all is taken as SVG.
    function file_ext(filename) result(ext)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: ext
        integer :: d, sl

        d = index(filename, ".", back=.true.)
        sl = max(index(filename, "/", back=.true.), &
                 index(filename, achar(92), back=.true.))
        if (d <= sl + 1) then
            ext = "svg"
        else
            ext = lower(filename(d + 1:len_trim(filename)))
        end if
    end function file_ext

    ! Writing SVG bytes into a .png would produce a file no viewer can open,
    ! so an unsupported extension is a hard error rather than a silent default.
    subroutine reject_ext(filename)
        character(len=*), intent(in) :: filename
        print *, "fplot: cannot write ", trim(filename)
        print *, "fplot: supported formats are .svg, .pdf, .eps and .png, not ." &
            //file_ext(filename)
        error stop "fplot: unsupported savefig format"
    end subroutine reject_ext

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

    ! dpi sizes the raster in the PNG backend. The vector formats ignore it,
    ! as matplotlib does: they emit the same inches*72 canvas at any dpi.
    subroutine savefig(filename, transparent, facecolor, dpi, bbox_inches, pad_inches)
        character(len=*), intent(in) :: filename
        logical, intent(in), optional :: transparent
        character(len=*), intent(in), optional :: facecolor, bbox_inches
        real(dp), intent(in), optional :: dpi, pad_inches
        character(len=:), allocatable :: svg
        real(dp) :: saved_dpi

        ! dpi given here applies to this file only, as it does in matplotlib;
        ! the figure's own dpi is what figure(dpi=) set and outlives the call.
        saved_dpi = fig_dpi
        if (present(dpi)) then
            if (dpi <= 0.0_dp) error stop "fplot: savefig dpi must be positive"
            fig_dpi = dpi
        end if
        select case (file_ext(filename))
        case ("svg")
            svg = render_svg(facecolor, transparent, bbox_inches, pad_inches)
        case ("pdf")
            svg = render_pdf(facecolor, transparent, bbox_inches, pad_inches)
        case ("png")
            svg = render_png(facecolor, transparent, bbox_inches, pad_inches)
        case ("eps")
            svg = render_eps(facecolor, transparent, bbox_inches, pad_inches)
        case default
            call reject_ext(filename)
            return
        end select
        fig_dpi = saved_dpi
        call write_bytes(filename, svg)
    end subroutine savefig

    subroutine write_bytes(filename, data)
        character(len=*), intent(in) :: filename, data
        integer :: u, ios

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
            write (u, "(A)") data
            close (u)
            return
        end if
        if (len(data) > 0) write (u) data
        close (u)
    end subroutine write_bytes

    ! ------------------------------------------------------------------
    ! Animation. matplotlib builds an Animation object around a callback;
    ! Fortran has loops, so the loop stays in the caller and each pass adds
    ! the figure as it stands to the reel.
    ! ------------------------------------------------------------------

    subroutine add_frame(facecolor, dpi)
        character(len=*), intent(in), optional :: facecolor
        real(dp), intent(in), optional :: dpi
        type(png_renderer_t) :: r
        real(dp) :: saved_dpi

        saved_dpi = fig_dpi
        if (present(dpi)) fig_dpi = dpi
        r%keep_pixels = .true.
        call r%set_dpi(fig_dpi)
        call render_figure(r, facecolor)
        fig_dpi = saved_dpi

        if (anim_n > 0) then
            if (r%pw /= anim_w .or. r%ph /= anim_h) then
                print *, "fplot: add_frame ignored, frame size changed"
                return
            end if
        end if
        anim_w = r%pw
        anim_h = r%ph
        call anim_append(r%rgb)
        anim_n = anim_n + 1
    end subroutine add_frame

    subroutine anim_append(frame)
        character(len=*), intent(in) :: frame
        character(len=:), allocatable :: bigger
        integer :: cap, need

        need = anim_len + len(frame)
        cap = 0
        if (allocated(anim_pix)) cap = len(anim_pix)
        if (cap < need) then
            cap = max(2*cap, need)
            allocate (character(len=cap) :: bigger)
            if (anim_len > 0) bigger(1:anim_len) = anim_pix(1:anim_len)
            call move_alloc(bigger, anim_pix)
        end if
        anim_pix(anim_len + 1:need) = frame
        anim_len = need
    end subroutine anim_append

    ! Writes the frames collected so far and starts a new reel, so that a
    ! program can make several animations without a reset call.
    subroutine save_animation(filename, fps, loop)
        character(len=*), intent(in) :: filename
        real(dp), intent(in), optional :: fps
        logical, intent(in), optional :: loop
        real(dp) :: rate
        logical :: rep
        integer :: delay

        if (anim_n == 0) then
            print *, "fplot: no frames; call add_frame before save_animation"
            return
        end if
        if (file_ext(filename) /= "gif") then
            print *, "fplot: save_animation writes .gif, not .", file_ext(filename)
            return
        end if
        rate = 5.0_dp
        if (present(fps)) then
            if (fps <= 0.0_dp) error stop "fplot: save_animation fps must be positive"
            rate = fps
        end if
        rep = .true.
        if (present(loop)) rep = loop

        ! GIF counts delays in hundredths of a second and viewers treat 0 as
        ! "as fast as you like", so a frame never asks for less than one.
        delay = max(1, nint(100.0_dp/rate))
        call write_bytes(filename, gif_encode(anim_w, anim_h, anim_n, &
                                              anim_pix(1:anim_len), delay, rep))
        anim_n = 0
        anim_w = 0
        anim_h = 0
        anim_len = 0
        deallocate (anim_pix)
    end subroutine save_animation

    subroutine show()
        ! File backend. In LFortran Jupyter notebooks, prefer:
        !   use lfortran_display
        !   call display_data("image/svg+xml", render_svg())
        call savefig("fplot_show.svg")
        print *, "fplot: wrote fplot_show.svg"
        print *, "fplot: for Jupyter use display_data('image/svg+xml', render_svg())"
    end subroutine show

end module fplot
