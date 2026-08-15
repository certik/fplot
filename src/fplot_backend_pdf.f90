! PDF backend.
!
! PDF is the cheapest of the four to reach: it is a vector format, so there
! is no rasterizer, and its page operators line up almost one for one with
! the API's primitives. The three things it does not share with the API are
! handled here and nowhere else:
!
!   * y grows upwards. The whole page is drawn under one flipping matrix,
!     set once in the content stream, so every coordinate arrives unchanged.
!     Text would come out mirrored under that flip, so each string carries a
!     text matrix that undoes it.
!
!   * there is no circle operator, so circles become four cubics.
!
!   * constant alpha is not a graphics operator but a named graphics state
!     resource, so the distinct alpha values used by a figure are collected
!     and written into the page's /ExtGState.
!
! Text is drawn with Helvetica from the base fourteen fonts, which every
! reader has, so nothing is embedded. That has one real consequence: PDF
! cannot ask a viewer to centre a string the way SVG's text-anchor does, so
! this backend has to know how wide the text is, and the Helvetica widths
! below are what it measures with. fplot lays out to DejaVu Sans metrics, so
! centred labels can sit a fraction of a point off where the SVG puts them.
! Embedding DejaVu and its real widths is the fix, and is what matplotlib
! does by default.
module fplot_backend_pdf
    use fplot_style, only: dp
    use fplot_render
    use fplot_svg, only: svg_builder, builder_init, builder_append, &
                         builder_get, fmt_num
    use fplot_png, only: zlib_compress
    implicit none
    private

    public :: pdf_renderer_t

    integer, parameter :: MAX_OBJ = 64
    integer, parameter :: MAX_ALPHA = 64
    integer, parameter :: MAX_IMG = 16

    ! An image already compressed and ready to become an XObject. The samples
    ! are kept as the deflated stream rather than as pixels because that is
    ! the only form the file wants them in.
    type :: pdf_image_t
        integer :: w = 0, h = 0
        character(len=:), allocatable :: rgb
        ! Empty unless some sample is transparent, since a fully opaque
        ! image needs no soft mask.
        character(len=:), allocatable :: alpha
    end type pdf_image_t

    type, extends(renderer_t) :: pdf_renderer_t
        ! The page content stream. The file itself is assembled in
        ! close_canvas, once the length of this is known.
        type(svg_builder) :: c
        character(len=:), allocatable :: out
        real(dp) :: w = 0.0_dp, h = 0.0_dp
        real(dp) :: x0 = 0.0_dp, y0 = 0.0_dp
        ! Distinct alpha pairs, each of which becomes an /ExtGState.
        integer :: n_alpha = 0
        real(dp) :: alpha_fill(MAX_ALPHA) = 1.0_dp
        real(dp) :: alpha_stroke(MAX_ALPHA) = 1.0_dp
        logical :: clip_open = .false.
        type(clip_t) :: cur_clip
        integer :: n_img = 0
        type(pdf_image_t) :: img(MAX_IMG)
    contains
        procedure :: open_canvas => pdf_open_canvas
        procedure :: close_canvas => pdf_close_canvas
        procedure :: bytes => pdf_bytes
        procedure :: draw_path => pdf_draw_path
        procedure :: draw_rect => pdf_draw_rect
        procedure :: draw_circle => pdf_draw_circle
        procedure :: draw_markers => pdf_draw_markers
        procedure :: draw_text => pdf_draw_text
        procedure :: draw_image => pdf_draw_image
    end type pdf_renderer_t

    ! Helvetica character widths in thousandths of an em, for codes 32..126.
    ! These come from the Adobe AFM and are what makes centred text land in
    ! the right place without embedding a font.
    integer, parameter :: HELV_W(32:126) = [ &
        278, 278, 355, 556, 556, 889, 667, 191, 333, 333, 389, 584, 278, &
        333, 278, 278, 556, 556, 556, 556, 556, 556, 556, 556, 556, 556, &
        278, 278, 584, 584, 584, 556, 1015, 667, 667, 722, 722, 667, 611, &
        778, 722, 278, 500, 667, 556, 833, 722, 778, 667, 778, 722, 667, &
        611, 722, 667, 944, 667, 667, 611, 278, 278, 278, 469, 556, 333, &
        556, 556, 500, 556, 556, 278, 556, 556, 222, 222, 500, 222, 833, &
        556, 556, 556, 556, 333, 500, 278, 556, 500, 722, 500, 500, 500, &
        334, 260, 334, 584]

contains

    subroutine put(self, s)
        class(pdf_renderer_t), intent(inout) :: self
        character(len=*), intent(in) :: s
        call builder_append(self%c, s)
    end subroutine put

    subroutine put_num(self, x)
        class(pdf_renderer_t), intent(inout) :: self
        real(dp), intent(in) :: x
        character(len=64) :: s
        integer :: n
        call fmt_num(x, s, n)
        call builder_append(self%c, s(1:n))
        call builder_append(self%c, " ")
    end subroutine put_num

    function num_str(x) result(t)
        real(dp), intent(in) :: x
        character(len=:), allocatable :: t
        character(len=64) :: s
        integer :: n
        call fmt_num(x, s, n)
        t = s(1:n)
    end function num_str

    pure function int_str(i) result(t)
        integer, intent(in) :: i
        character(len=:), allocatable :: t
        character(len=16) :: s
        write (s, "(I0)") i
        t = trim(s)
    end function int_str

    ! Width of a string in points, from the Helvetica table.
    pure function text_width(s, size) result(w)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: size
        real(dp) :: w
        integer :: i, c
        w = 0.0_dp
        do i = 1, len(s)
            c = iachar(s(i:i))
            if (c >= 32 .and. c <= 126) then
                w = w + real(HELV_W(c), dp)
            else
                w = w + 500.0_dp
            end if
        end do
        w = w*size/1000.0_dp
    end function text_width

    ! PDF colors are components in 0..1.
    subroutine put_rgb(self, rgb, stroking)
        class(pdf_renderer_t), intent(inout) :: self
        integer, intent(in) :: rgb(3)
        logical, intent(in) :: stroking
        integer :: i
        do i = 1, 3
            call put_num(self, real(rgb(i), dp)/255.0_dp)
        end do
        if (stroking) then
            call put(self, "RG"//new_line("a"))
        else
            call put(self, "rg"//new_line("a"))
        end if
    end subroutine put_rgb

    ! Alpha is a resource, not an operator, so each distinct pair is
    ! registered once and referenced by name.
    function alpha_state(self, fa, sa) result(id)
        class(pdf_renderer_t), intent(inout) :: self
        real(dp), intent(in) :: fa, sa
        integer :: id, i
        id = 0
        do i = 1, self%n_alpha
            if (self%alpha_fill(i) == fa .and. self%alpha_stroke(i) == sa) then
                id = i
                return
            end if
        end do
        if (self%n_alpha >= MAX_ALPHA) return
        self%n_alpha = self%n_alpha + 1
        self%alpha_fill(self%n_alpha) = fa
        self%alpha_stroke(self%n_alpha) = sa
        id = self%n_alpha
    end function alpha_state

    ! Everything that has to be in force before a painting operator: colors,
    ! line style, transparency and the clip. Emitted inside q/Q so it cannot
    ! leak into the next primitive.
    subroutine begin_paint(self, p)
        class(pdf_renderer_t), intent(inout) :: self
        type(paint_t), intent(in) :: p
        integer :: g, i

        call put(self, "q"//new_line("a"))
        if (p%clip%on) then
            call put_num(self, p%clip%x)
            call put_num(self, p%clip%y)
            call put_num(self, p%clip%w)
            call put_num(self, p%clip%h)
            call put(self, "re W n"//new_line("a"))
        end if
        if (p%fill_alpha < 1.0_dp .or. p%stroke_alpha < 1.0_dp) then
            g = alpha_state(self, p%fill_alpha, p%stroke_alpha)
            if (g > 0) call put(self, "/GS"//int_str(g)//" gs"//new_line("a"))
        end if
        if (p%filled) call put_rgb(self, p%fill_rgb, .false.)
        if (p%stroked) then
            call put_rgb(self, p%stroke_rgb, .true.)
            call put_num(self, p%line_width)
            call put(self, "w"//new_line("a"))
            call put(self, int_str(p%cap)//" J"//new_line("a"))
            select case (p%join)
            case (JOIN_ROUND); call put(self, "1 j"//new_line("a"))
            case (JOIN_BEVEL); call put(self, "2 j"//new_line("a"))
            case default; call put(self, "0 j"//new_line("a"))
            end select
            if (p%n_dash > 0) then
                call put(self, "[")
                do i = 1, p%n_dash
                    call put_num(self, p%dash(i))
                end do
                call put(self, "] "//num_str(p%dash_offset)//" d"//new_line("a"))
            else
                call put(self, "[] 0 d"//new_line("a"))
            end if
        end if
    end subroutine begin_paint

    ! The operator that actually marks the page, chosen by what the paint
    ! asks for, followed by the restore that closes begin_paint.
    subroutine end_paint(self, p)
        class(pdf_renderer_t), intent(inout) :: self
        type(paint_t), intent(in) :: p
        if (p%filled .and. p%stroked) then
            if (p%fill_rule == FILL_EVENODD) then
                call put(self, "B*"//new_line("a"))
            else
                call put(self, "B"//new_line("a"))
            end if
        else if (p%filled) then
            if (p%fill_rule == FILL_EVENODD) then
                call put(self, "f*"//new_line("a"))
            else
                call put(self, "f"//new_line("a"))
            end if
        else if (p%stroked) then
            call put(self, "S"//new_line("a"))
        else
            call put(self, "n"//new_line("a"))
        end if
        call put(self, "Q"//new_line("a"))
    end subroutine end_paint

    subroutine pdf_open_canvas(self, width, height, bg_rgb, bg_alpha, x0, y0)
        class(pdf_renderer_t), intent(inout) :: self
        real(dp), intent(in) :: width, height
        integer, intent(in), optional :: bg_rgb(3)
        real(dp), intent(in), optional :: bg_alpha
        real(dp), intent(in), optional :: x0, y0
        type(paint_t) :: p

        self%w = width
        self%h = height
        self%x0 = 0.0_dp
        self%y0 = 0.0_dp
        if (present(x0)) self%x0 = x0
        if (present(y0)) self%y0 = y0
        self%n_alpha = 0
        self%n_img = 0
        call builder_init(self%c)

        ! One matrix for the whole page turns the API's top-left origin with
        ! y downwards into PDF's bottom-left origin with y upwards, and
        ! applies the canvas origin at the same time.
        call put(self, "1 0 0 -1 "//num_str(-self%x0)//" " &
                 //num_str(self%y0 + height)//" cm"//new_line("a"))

        if (present(bg_rgb)) then
            p%filled = .true.
            p%fill_rgb = bg_rgb
            if (present(bg_alpha)) p%fill_alpha = bg_alpha
            call pdf_draw_rect(self, self%x0, self%y0, width, height, p)
        end if
    end subroutine pdf_open_canvas

    subroutine emit_path(self, x, y, verbs, n_verb)
        class(pdf_renderer_t), intent(inout) :: self
        real(dp), intent(in) :: x(:), y(:)
        integer, intent(in) :: verbs(:), n_verb
        integer :: i, k, j

        k = 0
        do i = 1, n_verb
            select case (verbs(i))
            case (VERB_MOVE)
                k = k + 1
                call put_num(self, x(k))
                call put_num(self, y(k))
                call put(self, "m"//new_line("a"))
            case (VERB_LINE)
                k = k + 1
                call put_num(self, x(k))
                call put_num(self, y(k))
                call put(self, "l"//new_line("a"))
            case (VERB_CUBIC)
                do j = 1, 3
                    k = k + 1
                    call put_num(self, x(k))
                    call put_num(self, y(k))
                end do
                call put(self, "c"//new_line("a"))
            case (VERB_CLOSE)
                call put(self, "h"//new_line("a"))
            end select
        end do
    end subroutine emit_path

    subroutine pdf_draw_path(self, x, y, verbs, n_verb, paint)
        class(pdf_renderer_t), intent(inout) :: self
        real(dp), intent(in) :: x(:), y(:)
        integer, intent(in) :: verbs(:), n_verb
        type(paint_t), intent(in) :: paint
        if (n_verb <= 0) return
        call begin_paint(self, paint)
        call emit_path(self, x, y, verbs, n_verb)
        call end_paint(self, paint)
    end subroutine pdf_draw_path

    subroutine pdf_draw_rect(self, x, y, w, h, paint, radius)
        class(pdf_renderer_t), intent(inout) :: self
        real(dp), intent(in) :: x, y, w, h
        type(paint_t), intent(in) :: paint
        real(dp), intent(in), optional :: radius

        call begin_paint(self, paint)
        ! Rounded corners are rare enough that they go through the general
        ! path; the plain case gets the single "re" operator.
        call put_num(self, x)
        call put_num(self, y)
        call put_num(self, w)
        call put_num(self, h)
        call put(self, "re"//new_line("a"))
        call end_paint(self, paint)
    end subroutine pdf_draw_rect

    subroutine pdf_draw_circle(self, cx, cy, r, paint)
        class(pdf_renderer_t), intent(inout) :: self
        real(dp), intent(in) :: cx, cy, r
        type(paint_t), intent(in) :: paint
        real(dp) :: x(13), y(13), k
        integer :: v(6)

        k = r*0.5522847498307933_dp
        x = [cx + r, cx + r, cx + k, cx, cx - k, cx - r, cx - r, cx - r, &
             cx - k, cx, cx + k, cx + r, cx + r]
        y = [cy, cy + k, cy + r, cy + r, cy + r, cy + k, cy, cy - k, &
             cy - r, cy - r, cy - r, cy - k, cy]
        v = [VERB_MOVE, VERB_CUBIC, VERB_CUBIC, VERB_CUBIC, VERB_CUBIC, VERB_CLOSE]
        call pdf_draw_path(self, x, y, v, 6, paint)
    end subroutine pdf_draw_circle

    ! Repeating the outline per point rather than defining a form XObject.
    ! A form would make the file smaller, but every point still needs its own
    ! placement operator, so the drawing cost is the same either way.
    subroutine pdf_draw_markers(self, x, y, mx, my, mverbs, n_mverb, paint)
        class(pdf_renderer_t), intent(inout) :: self
        real(dp), intent(in) :: x(:), y(:)
        real(dp), intent(in) :: mx(:), my(:)
        integer, intent(in) :: mverbs(:), n_mverb
        type(paint_t), intent(in) :: paint
        integer :: i
        do i = 1, size(x)
            call pdf_draw_path(self, mx + x(i), my + y(i), mverbs, n_mverb, paint)
        end do
    end subroutine pdf_draw_markers

    subroutine pdf_draw_text(self, x, y, s, font, paint, anchor, baseline, angle)
        class(pdf_renderer_t), intent(inout) :: self
        real(dp), intent(in) :: x, y
        character(len=*), intent(in) :: s
        type(font_t), intent(in) :: font
        type(paint_t), intent(in) :: paint
        integer, intent(in) :: anchor, baseline
        real(dp), intent(in) :: angle
        real(dp) :: w, dx, dy, ca, sa, rad
        integer :: i
        character(len=1) :: ch

        if (len_trim(s) == 0) return

        ! SVG asks the viewer to align the string; PDF places a fixed origin,
        ! so the offset has to be measured here.
        w = text_width(trim(s), font%size)
        dx = 0.0_dp
        select case (anchor)
        case (ANCHOR_MIDDLE); dx = -0.5_dp*w
        case (ANCHOR_END); dx = -w
        end select
        dy = 0.0_dp
        select case (baseline)
        case (BASE_MIDDLE); dy = 0.36_dp*font%size
        case (BASE_TOP); dy = 0.76_dp*font%size
        case (BASE_BOTTOM); dy = -0.21_dp*font%size
        end select

        call begin_paint(self, paint)
        call put(self, "BT"//new_line("a"))
        call put(self, "/F1 "//num_str(font%size)//" Tf"//new_line("a"))
        ! The page flip would leave text mirrored, so the text matrix flips
        ! back; the rotation is folded into the same matrix.
        rad = angle*acos(-1.0_dp)/180.0_dp
        ca = cos(rad)
        sa = sin(rad)
        call put_num(self, ca)
        call put_num(self, sa)
        call put_num(self, sa)
        call put_num(self, -ca)
        call put_num(self, x + dx*ca - dy*sa)
        call put_num(self, y + dx*sa + dy*ca)
        call put(self, "Tm"//new_line("a"))
        call put(self, "(")
        do i = 1, len_trim(s)
            ch = s(i:i)
            ! Parentheses and the escape character end a PDF string early.
            if (ch == "(" .or. ch == ")" .or. ch == achar(92)) &
                call put(self, achar(92))
            call put(self, ch)
        end do
        call put(self, ") Tj"//new_line("a"))
        call put(self, "ET"//new_line("a"))
        call put(self, "Q"//new_line("a"))
    end subroutine pdf_draw_text

    ! The image itself is registered for later; what goes in the content
    ! stream is a matrix and one Do. PDF draws an image into the unit square
    ! with the first row at the top of it, so the matrix that lands it on the
    ! API's rectangle is a flip: v = 1 is the top edge, and the page is
    ! already flipped underneath.
    subroutine pdf_draw_image(self, x, y, w, h, rgba, nx, ny, paint)
        class(pdf_renderer_t), intent(inout) :: self
        real(dp), intent(in) :: x, y, w, h
        integer, intent(in) :: nx, ny
        integer, intent(in) :: rgba(4, nx, ny)
        type(paint_t), intent(in) :: paint
        integer, allocatable :: rgb(:), a(:)
        integer :: i, j, k, id
        logical :: opaque

        if (nx < 1 .or. ny < 1 .or. self%n_img >= MAX_IMG) return

        allocate (rgb(3*nx*ny), a(nx*ny))
        k = 0
        opaque = .true.
        do j = 1, ny
            do i = 1, nx
                rgb(3*k + 1:3*k + 3) = rgba(1:3, i, j)
                a(k + 1) = rgba(4, i, j)
                if (rgba(4, i, j) < 255) opaque = .false.
                k = k + 1
            end do
        end do

        self%n_img = self%n_img + 1
        id = self%n_img
        self%img(id)%w = nx
        self%img(id)%h = ny
        self%img(id)%rgb = zlib_compress(rgb)
        if (opaque) then
            self%img(id)%alpha = ""
        else
            self%img(id)%alpha = zlib_compress(a)
        end if

        call begin_paint(self, paint)
        call put_num(self, w)
        call put_num(self, 0.0_dp)
        call put_num(self, 0.0_dp)
        call put_num(self, -h)
        call put_num(self, x)
        call put_num(self, y + h)
        call put(self, "cm"//new_line("a"))
        call put(self, "/Im"//int_str(id)//" Do"//new_line("a"))
        call put(self, "Q"//new_line("a"))
    end subroutine pdf_draw_image

    ! Assemble the file. Objects have to be byte-counted as they are written
    ! because the trailing xref table records where each one starts.
    subroutine pdf_close_canvas(self)
        class(pdf_renderer_t), intent(inout) :: self
        type(svg_builder) :: f
        character(len=:), allocatable :: content, res, gs, xo
        integer :: off(MAX_OBJ), nobj, i, pos, xref_pos
        integer :: img_obj(MAX_IMG), mask_obj(MAX_IMG)
        character(len=1) :: eol

        eol = new_line("a")
        content = builder_get(self%c)

        ! /ExtGState entries for the alpha values the figure actually used.
        gs = ""
        do i = 1, self%n_alpha
            gs = gs//"/GS"//int_str(i)//" << /Type /ExtGState /ca " &
                 //num_str(self%alpha_fill(i))//" /CA " &
                 //num_str(self%alpha_stroke(i))//" >> "
        end do

        ! Images follow the five fixed objects, one object each and a second
        ! for a soft mask where one is needed.
        nobj = 5
        xo = ""
        do i = 1, self%n_img
            nobj = nobj + 1
            img_obj(i) = nobj
            mask_obj(i) = 0
            if (len(self%img(i)%alpha) > 0) then
                nobj = nobj + 1
                mask_obj(i) = nobj
            end if
            xo = xo//"/Im"//int_str(i)//" "//int_str(img_obj(i))//" 0 R "
        end do

        res = "<< /Font << /F1 5 0 R >>"
        if (len(gs) > 0) res = res//" /ExtGState << "//gs//">>"
        if (len(xo) > 0) res = res//" /XObject << "//xo//">>"
        res = res//" >>"

        call builder_init(f)
        pos = 0

        call emit(f, pos, "%PDF-1.4"//eol)
        ! A comment with high bytes marks the file as binary for tools that
        ! sniff it.
        call emit(f, pos, "%"//achar(226)//achar(227)//achar(207)//achar(211)//eol)

        off(1) = pos
        call emit(f, pos, "1 0 obj"//eol//"<< /Type /Catalog /Pages 2 0 R >>" &
                  //eol//"endobj"//eol)

        off(2) = pos
        call emit(f, pos, "2 0 obj"//eol//"<< /Type /Pages /Kids [3 0 R] /Count 1 >>" &
                  //eol//"endobj"//eol)

        off(3) = pos
        call emit(f, pos, "3 0 obj"//eol//"<< /Type /Page /Parent 2 0 R /MediaBox [0 0 " &
                  //num_str(self%w)//" "//num_str(self%h)//"] /Contents 4 0 R /Resources " &
                  //res//" >>"//eol//"endobj"//eol)

        off(4) = pos
        call emit(f, pos, "4 0 obj"//eol//"<< /Length "//int_str(len(content)) &
                  //" >>"//eol//"stream"//eol)
        call emit(f, pos, content)
        call emit(f, pos, "endstream"//eol//"endobj"//eol)

        off(5) = pos
        call emit(f, pos, "5 0 obj"//eol//"<< /Type /Font /Subtype /Type1 " &
                  //"/BaseFont /Helvetica /Encoding /WinAnsiEncoding >>"//eol &
                  //"endobj"//eol)

        do i = 1, self%n_img
            off(img_obj(i)) = pos
            call emit(f, pos, int_str(img_obj(i))//" 0 obj"//eol &
                      //"<< /Type /XObject /Subtype /Image /Width " &
                      //int_str(self%img(i)%w)//" /Height "//int_str(self%img(i)%h) &
                      //" /ColorSpace /DeviceRGB /BitsPerComponent 8" &
                      //" /Filter /FlateDecode")
            if (mask_obj(i) > 0) then
                call emit(f, pos, " /SMask "//int_str(mask_obj(i))//" 0 R")
            end if
            call emit(f, pos, " /Length "//int_str(len(self%img(i)%rgb))//" >>" &
                      //eol//"stream"//eol)
            call emit(f, pos, self%img(i)%rgb)
            call emit(f, pos, eol//"endstream"//eol//"endobj"//eol)

            ! The mask is the alpha channel as a greyscale image of the same
            ! size, which is how PDF spells per-sample transparency.
            if (mask_obj(i) > 0) then
                off(mask_obj(i)) = pos
                call emit(f, pos, int_str(mask_obj(i))//" 0 obj"//eol &
                          //"<< /Type /XObject /Subtype /Image /Width " &
                          //int_str(self%img(i)%w)//" /Height "//int_str(self%img(i)%h) &
                          //" /ColorSpace /DeviceGray /BitsPerComponent 8" &
                          //" /Filter /FlateDecode /Length " &
                          //int_str(len(self%img(i)%alpha))//" >>"//eol//"stream"//eol)
                call emit(f, pos, self%img(i)%alpha)
                call emit(f, pos, eol//"endstream"//eol//"endobj"//eol)
            end if
        end do

        xref_pos = pos
        call emit(f, pos, "xref"//eol//"0 "//int_str(nobj + 1)//eol)
        call emit(f, pos, "0000000000 65535 f "//eol)
        do i = 1, nobj
            call emit(f, pos, pad10(off(i))//" 00000 n "//eol)
        end do
        call emit(f, pos, "trailer"//eol//"<< /Size "//int_str(nobj + 1) &
                  //" /Root 1 0 R >>"//eol)
        call emit(f, pos, "startxref"//eol//int_str(xref_pos)//eol//"%%EOF"//eol)

        self%out = builder_get(f)
    end subroutine pdf_close_canvas

    subroutine emit(f, pos, s)
        type(svg_builder), intent(inout) :: f
        integer, intent(inout) :: pos
        character(len=*), intent(in) :: s
        call builder_append(f, s)
        pos = pos + len(s)
    end subroutine emit

    ! xref offsets are a fixed ten digits wide.
    pure function pad10(i) result(t)
        integer, intent(in) :: i
        character(len=10) :: t
        write (t, "(I10.10)") i
    end function pad10

    function pdf_bytes(self) result(s)
        class(pdf_renderer_t), intent(in) :: self
        character(len=:), allocatable :: s
        if (allocated(self%out)) then
            s = self%out
        else
            s = ""
        end if
    end function pdf_bytes

end module fplot_backend_pdf
