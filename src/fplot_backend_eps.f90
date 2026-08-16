! EPS backend.
!
! PostScript is the oldest of the four formats and the one with the fewest
! features, so this backend is the PDF one with three things taken away and
! one added:
!
!   * there is no transparency. PostScript has no alpha at all, so a
!     translucent color is composited against the page here instead, which
!     is the only way to get the intended appearance into the file. That is
!     exact where the thing underneath is the white page and approximate
!     where it is not, and matplotlib's own PS output does not even try.
!
!   * y grows upwards, as in PDF, and is handled the same way: one flipping
!     matrix for the whole page, undone around each string so text is not
!     drawn mirrored.
!
!   * there is no arc operator that would help, so circles become four
!     cubics, exactly as in PDF.
!
! What it adds is the DSC header, %%BoundingBox above all, which is what
! makes the file embeddable in a document.
!
! Text is Helvetica from the base thirty-five fonts. As in PDF, that means
! this backend has to measure strings itself to place centred text, and the
! widths it measures with are Helvetica's rather than DejaVu's.
module fplot_backend_eps
    use fplot_style, only: dp
    use fplot_render
    use fplot_svg, only: svg_builder, builder_init, builder_append, &
                         builder_get, fmt_num
    implicit none
    private

    public :: eps_renderer_t

    type, extends(renderer_t) :: eps_renderer_t
        type(svg_builder) :: c
        character(len=:), allocatable :: out
        real(dp) :: w = 0.0_dp, h = 0.0_dp
        real(dp) :: x0 = 0.0_dp, y0 = 0.0_dp
        ! What a translucent color is composited against.
        integer :: page_rgb(3) = [255, 255, 255]
    contains
        procedure :: open_canvas => eps_open_canvas
        procedure :: close_canvas => eps_close_canvas
        procedure :: bytes => eps_bytes
        procedure :: draw_path => eps_draw_path
        procedure :: draw_rect => eps_draw_rect
        procedure :: draw_circle => eps_draw_circle
        procedure :: draw_markers => eps_draw_markers
        procedure :: draw_text => eps_draw_text
        procedure :: draw_image => eps_draw_image
    end type eps_renderer_t

    ! Helvetica character widths in thousandths of an em, for codes 32..126,
    ! from the Adobe AFM.
    integer, parameter :: HELV_W(32:126) = [ &
        278, 278, 355, 556, 556, 889, 667, 191, 333, 333, 389, 584, 278, &
        333, 278, 278, 556, 556, 556, 556, 556, 556, 556, 556, 556, 556, &
        278, 278, 584, 584, 584, 556, 1015, 667, 667, 722, 722, 667, 611, &
        778, 722, 278, 500, 667, 556, 833, 722, 778, 667, 778, 722, 667, &
        611, 722, 667, 944, 667, 667, 611, 278, 278, 278, 469, 556, 333, &
        556, 556, 500, 556, 556, 278, 556, 556, 222, 222, 500, 222, 833, &
        556, 556, 556, 556, 333, 500, 278, 556, 500, 722, 500, 500, 500, &
        334, 260, 334, 584]

    ! The same for Helvetica-Bold; oblique shares the regular widths.
    integer, parameter :: HELVB_W(32:126) = [ &
        278, 333, 474, 556, 556, 889, 722, 278, 333, 333, 389, 584, 278, &
        333, 278, 278, 556, 556, 556, 556, 556, 556, 556, 556, 556, 556, &
        333, 333, 584, 584, 584, 611, 975, 722, 722, 722, 722, 667, 611, &
        778, 722, 278, 556, 722, 611, 833, 722, 778, 667, 778, 722, 667, &
        611, 722, 667, 944, 667, 667, 611, 333, 278, 333, 584, 556, 278, &
        556, 611, 556, 611, 556, 333, 611, 611, 278, 278, 556, 278, 889, &
        611, 611, 611, 611, 389, 556, 333, 611, 556, 778, 556, 556, 500, &
        389, 280, 389, 584]

contains

    subroutine put(self, s)
        class(eps_renderer_t), intent(inout) :: self
        character(len=*), intent(in) :: s
        call builder_append(self%c, s)
    end subroutine put

    subroutine put_num(self, x)
        class(eps_renderer_t), intent(inout) :: self
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

    ! The base-thirty-five name of the face the font asks for.
    pure function face_name(font) result(t)
        type(font_t), intent(in) :: font
        character(len=:), allocatable :: t
        logical :: b, o
        b = font%weight == WEIGHT_BOLD
        o = font%slant == SLANT_ITALIC
        if (b .and. o) then
            t = "Helvetica-BoldOblique"
        else if (b) then
            t = "Helvetica-Bold"
        else if (o) then
            t = "Helvetica-Oblique"
        else
            t = "Helvetica"
        end if
    end function face_name

    pure function text_width(s, size, bold) result(w)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: size
        logical, intent(in) :: bold
        real(dp) :: w
        integer :: i, c
        w = 0.0_dp
        do i = 1, len(s)
            c = iachar(s(i:i))
            if (c >= 32 .and. c <= 126) then
                if (bold) then
                    w = w + real(HELVB_W(c), dp)
                else
                    w = w + real(HELV_W(c), dp)
                end if
            else
                w = w + 500.0_dp
            end if
        end do
        w = w*size/1000.0_dp
    end function text_width

    ! A translucent color has to become an opaque one, since the format
    ! cannot express the first.
    pure function blend(rgb, a, page) result(o)
        integer, intent(in) :: rgb(3), page(3)
        real(dp), intent(in) :: a
        integer :: o(3), i
        do i = 1, 3
            o(i) = nint(a*real(rgb(i), dp) + (1.0_dp - a)*real(page(i), dp))
        end do
    end function blend

    subroutine put_rgb(self, rgb, alpha)
        class(eps_renderer_t), intent(inout) :: self
        integer, intent(in) :: rgb(3)
        real(dp), intent(in) :: alpha
        integer :: c(3), i
        c = blend(rgb, alpha, self%page_rgb)
        do i = 1, 3
            call put_num(self, real(c(i), dp)/255.0_dp)
        end do
        call put(self, "setrgbcolor"//new_line("a"))
    end subroutine put_rgb

    ! Everything that has to be in force before a painting operator, inside
    ! gsave/grestore so it cannot leak into the next primitive.
    subroutine begin_paint(self, p)
        class(eps_renderer_t), intent(inout) :: self
        type(paint_t), intent(in) :: p
        integer :: i

        call put(self, "gsave"//new_line("a"))
        if (p%clip%on) then
            call put_num(self, p%clip%x)
            call put_num(self, p%clip%y)
            call put_num(self, p%clip%w)
            call put_num(self, p%clip%h)
            call put(self, "rectclip"//new_line("a"))
        end if
        if (p%stroked) then
            call put_num(self, p%line_width)
            call put(self, "setlinewidth"//new_line("a"))
            call put(self, int_str(p%cap)//" setlinecap"//new_line("a"))
            select case (p%join)
            case (JOIN_ROUND); call put(self, "1 setlinejoin"//new_line("a"))
            case (JOIN_BEVEL); call put(self, "2 setlinejoin"//new_line("a"))
            case default; call put(self, "0 setlinejoin"//new_line("a"))
            end select
            call put(self, "[")
            do i = 1, p%n_dash
                call put_num(self, p%dash(i))
            end do
            call put(self, "] "//num_str(p%dash_offset)//" setdash"//new_line("a"))
        end if
    end subroutine begin_paint

    ! Fill and stroke are separate operators in PostScript and each consumes
    ! the path, so a primitive that wants both keeps a copy of it.
    subroutine end_paint(self, p)
        class(eps_renderer_t), intent(inout) :: self
        type(paint_t), intent(in) :: p

        if (p%filled .and. p%stroked) call put(self, "gsave"//new_line("a"))
        if (p%filled) then
            call put_rgb(self, p%fill_rgb, p%fill_alpha)
            if (p%fill_rule == FILL_EVENODD) then
                call put(self, "eofill"//new_line("a"))
            else
                call put(self, "fill"//new_line("a"))
            end if
        end if
        if (p%filled .and. p%stroked) call put(self, "grestore"//new_line("a"))
        if (p%stroked) then
            call put_rgb(self, p%stroke_rgb, p%stroke_alpha)
            call put(self, "stroke"//new_line("a"))
        end if
        if (.not. (p%filled .or. p%stroked)) call put(self, "newpath"//new_line("a"))
        call put(self, "grestore"//new_line("a"))
    end subroutine end_paint

    subroutine eps_open_canvas(self, width, height, bg_rgb, bg_alpha, x0, y0)
        class(eps_renderer_t), intent(inout) :: self
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
        call builder_init(self%c)

        ! One matrix for the whole page turns the API's top-left origin with
        ! y downwards into PostScript's bottom-left origin with y upwards,
        ! and applies the canvas origin at the same time.
        call put(self, "[1 0 0 -1 "//num_str(-self%x0)//" " &
                 //num_str(self%y0 + height)//"] concat"//new_line("a"))

        if (present(bg_rgb)) then
            self%page_rgb = bg_rgb
            p%filled = .true.
            p%fill_rgb = bg_rgb
            if (present(bg_alpha)) p%fill_alpha = bg_alpha
            call eps_draw_rect(self, self%x0, self%y0, width, height, p)
        end if
    end subroutine eps_open_canvas

    subroutine emit_path(self, x, y, verbs, n_verb)
        class(eps_renderer_t), intent(inout) :: self
        real(dp), intent(in) :: x(:), y(:)
        integer, intent(in) :: verbs(:), n_verb
        integer :: i, k, j

        call put(self, "newpath"//new_line("a"))
        k = 0
        do i = 1, n_verb
            select case (verbs(i))
            case (VERB_MOVE)
                k = k + 1
                call put_num(self, x(k))
                call put_num(self, y(k))
                call put(self, "moveto"//new_line("a"))
            case (VERB_LINE)
                k = k + 1
                call put_num(self, x(k))
                call put_num(self, y(k))
                call put(self, "lineto"//new_line("a"))
            case (VERB_CUBIC)
                do j = 1, 3
                    k = k + 1
                    call put_num(self, x(k))
                    call put_num(self, y(k))
                end do
                call put(self, "curveto"//new_line("a"))
            case (VERB_CLOSE)
                call put(self, "closepath"//new_line("a"))
            end select
        end do
    end subroutine emit_path

    subroutine eps_draw_path(self, x, y, verbs, n_verb, paint)
        class(eps_renderer_t), intent(inout) :: self
        real(dp), intent(in) :: x(:), y(:)
        integer, intent(in) :: verbs(:), n_verb
        type(paint_t), intent(in) :: paint
        if (n_verb <= 0) return
        call begin_paint(self, paint)
        call emit_path(self, x, y, verbs, n_verb)
        call end_paint(self, paint)
    end subroutine eps_draw_path

    subroutine eps_draw_rect(self, x, y, w, h, paint, radius)
        class(eps_renderer_t), intent(inout) :: self
        real(dp), intent(in) :: x, y, w, h
        type(paint_t), intent(in) :: paint
        real(dp), intent(in), optional :: radius

        call begin_paint(self, paint)
        call put(self, "newpath"//new_line("a"))
        call put_num(self, x)
        call put_num(self, y)
        call put(self, "moveto"//new_line("a"))
        call put_num(self, w)
        call put(self, "0 rlineto"//new_line("a"))
        call put_num(self, 0.0_dp)
        call put_num(self, h)
        call put(self, "rlineto"//new_line("a"))
        call put_num(self, -w)
        call put(self, "0 rlineto"//new_line("a"))
        call put(self, "closepath"//new_line("a"))
        call end_paint(self, paint)
    end subroutine eps_draw_rect

    subroutine eps_draw_circle(self, cx, cy, r, paint)
        class(eps_renderer_t), intent(inout) :: self
        real(dp), intent(in) :: cx, cy, r
        type(paint_t), intent(in) :: paint

        call begin_paint(self, paint)
        call put(self, "newpath"//new_line("a"))
        call put_num(self, cx)
        call put_num(self, cy)
        call put_num(self, r)
        call put(self, "0 360 arc closepath"//new_line("a"))
        call end_paint(self, paint)
    end subroutine eps_draw_circle

    subroutine eps_draw_markers(self, x, y, mx, my, mverbs, n_mverb, paint)
        class(eps_renderer_t), intent(inout) :: self
        real(dp), intent(in) :: x(:), y(:)
        real(dp), intent(in) :: mx(:), my(:)
        integer, intent(in) :: mverbs(:), n_mverb
        type(paint_t), intent(in) :: paint
        integer :: i
        do i = 1, size(x)
            call eps_draw_path(self, mx + x(i), my + y(i), mverbs, n_mverb, paint)
        end do
    end subroutine eps_draw_markers

    subroutine eps_draw_text(self, x, y, s, font, paint, anchor, baseline, angle)
        class(eps_renderer_t), intent(inout) :: self
        real(dp), intent(in) :: x, y
        character(len=*), intent(in) :: s
        type(font_t), intent(in) :: font
        type(paint_t), intent(in) :: paint
        integer, intent(in) :: anchor, baseline
        real(dp), intent(in) :: angle
        real(dp) :: w, dx, dy
        integer :: i
        character(len=1) :: ch

        if (len_trim(s) == 0) return

        ! SVG asks the viewer to align the string; PostScript places a fixed
        ! origin, so the offset has to be measured here.
        w = text_width(trim(s), font%size, font%weight == WEIGHT_BOLD)
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

        call put(self, "gsave"//new_line("a"))
        call put_rgb(self, paint%fill_rgb, paint%fill_alpha)
        call put_num(self, x)
        call put_num(self, y)
        call put(self, "translate"//new_line("a"))
        ! Undo the page flip, or every glyph would come out mirrored, and
        ! turn the string by its own angle in the same upright frame.
        call put(self, "1 -1 scale"//new_line("a"))
        call put_num(self, angle)
        call put(self, "rotate"//new_line("a"))
        call put(self, "/"//face_name(font)//" findfont "//num_str(font%size) &
                 //" scalefont setfont"//new_line("a"))
        call put_num(self, dx)
        call put_num(self, -dy)
        call put(self, "moveto"//new_line("a"))
        call put(self, "(")
        do i = 1, len_trim(s)
            ch = s(i:i)
            ! Parentheses and the escape character end a string early.
            if (ch == "(" .or. ch == ")" .or. ch == achar(92)) &
                call put(self, achar(92))
            call put(self, ch)
        end do
        call put(self, ") show"//new_line("a"))
        call put(self, "grestore"//new_line("a"))
    end subroutine eps_draw_text

    ! The samples go into the file as hexadecimal, which every PostScript
    ! interpreter reads and which keeps the file printable. Transparency is
    ! composited against the page here, as everywhere else in this backend.
    subroutine eps_draw_image(self, x, y, w, h, rgba, nx, ny, paint)
        class(eps_renderer_t), intent(inout) :: self
        real(dp), intent(in) :: x, y, w, h
        integer, intent(in) :: nx, ny
        integer, intent(in) :: rgba(4, nx, ny)
        type(paint_t), intent(in) :: paint
        character(len=*), parameter :: HEX = "0123456789abcdef"
        integer :: i, j, k, c(3)
        character(len=1) :: buf(6)

        if (nx < 1 .or. ny < 1) return

        call put(self, "gsave"//new_line("a"))
        call put_num(self, x)
        call put_num(self, y + h)
        call put(self, "translate"//new_line("a"))
        call put_num(self, w)
        call put_num(self, -h)
        call put(self, "scale"//new_line("a"))
        call put(self, int_str(nx)//" "//int_str(ny)//" 8 [" &
                 //int_str(nx)//" 0 0 "//int_str(-ny)//" 0 "//int_str(ny)//"]" &
                 //new_line("a"))
        call put(self, "{currentfile "//int_str(3*nx) &
                 //" string readhexstring pop} bind" &
                 //new_line("a"))
        call put(self, "false 3 colorimage"//new_line("a"))

        k = 0
        do j = 1, ny
            do i = 1, nx
                c = blend(rgba(1:3, i, j), real(rgba(4, i, j), dp)/255.0_dp, &
                          self%page_rgb)
                buf(1) = HEX(c(1)/16 + 1:c(1)/16 + 1)
                buf(2) = HEX(mod(c(1), 16) + 1:mod(c(1), 16) + 1)
                buf(3) = HEX(c(2)/16 + 1:c(2)/16 + 1)
                buf(4) = HEX(mod(c(2), 16) + 1:mod(c(2), 16) + 1)
                buf(5) = HEX(c(3)/16 + 1:c(3)/16 + 1)
                buf(6) = HEX(mod(c(3), 16) + 1:mod(c(3), 16) + 1)
                call put(self, buf(1)//buf(2)//buf(3)//buf(4)//buf(5)//buf(6))
                k = k + 1
                if (mod(k, 12) == 0) call put(self, new_line("a"))
            end do
        end do
        call put(self, new_line("a")//"grestore"//new_line("a"))
    end subroutine eps_draw_image

    ! Wrap the page in the DSC comments that make it an EPS rather than a
    ! bare PostScript program: the bounding box above all, since that is what
    ! a document that embeds the figure reads.
    subroutine eps_close_canvas(self)
        class(eps_renderer_t), intent(inout) :: self
        type(svg_builder) :: f
        character(len=1) :: eol

        eol = new_line("a")
        call builder_init(f)
        call builder_append(f, "%!PS-Adobe-3.0 EPSF-3.0"//eol)
        call builder_append(f, "%%Creator: fplot"//eol)
        call builder_append(f, "%%BoundingBox: 0 0 "//int_str(ceiling(self%w)) &
                            //" "//int_str(ceiling(self%h))//eol)
        call builder_append(f, "%%HiResBoundingBox: 0 0 "//num_str(self%w) &
                            //" "//num_str(self%h)//eol)
        call builder_append(f, "%%EndComments"//eol)
        call builder_append(f, "%%BeginProlog"//eol//"%%EndProlog"//eol)
        call builder_append(f, "%%Page: 1 1"//eol)
        call builder_append(f, builder_get(self%c))
        call builder_append(f, "showpage"//eol//"%%EOF"//eol)
        self%out = builder_get(f)
    end subroutine eps_close_canvas

    function eps_bytes(self) result(s)
        class(eps_renderer_t), intent(in) :: self
        character(len=:), allocatable :: s
        if (allocated(self%out)) then
            s = self%out
        else
            s = ""
        end if
    end function eps_bytes

end module fplot_backend_eps
