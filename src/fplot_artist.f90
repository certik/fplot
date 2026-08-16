! fplot_artist - the drawing primitives every plot type is built from.
!
! Nothing here knows what is being drawn: these take a renderer and points
! on the canvas and put strokes, fills, markers and text on it. The plot
! types turn data into calls to these, which is the division matplotlib
! draws between its artists and its backends.
module fplot_artist
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
    use fplot_state
    use fplot_state
    implicit none
    public

contains

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

    subroutine append_open_circle(b, cx, cy, r, color)
        class(renderer_t), intent(inout) :: b
        real(dp), intent(in) :: cx, cy, r
        character(len=*), intent(in) :: color
        call b%draw_circle(cx, cy, r, pen(color, 1.0_dp))
    end subroutine append_open_circle

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

end module fplot_artist
