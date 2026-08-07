! SVG string builder for fplot.
module fplot_svg
    use fplot_style, only: dp
    implicit none
    private

    public :: svg_builder
    public :: builder_init
    public :: builder_append
    public :: builder_get
    public :: builder_clear
    public :: fmt_num
    public :: xml_escape_to

    integer, parameter :: BUILDER_INIT_CAP = 65536

    type :: svg_builder
        character(len=:), allocatable :: buf
        integer :: n = 0
        integer :: cap = 0
    end type svg_builder

contains

    subroutine builder_init(b, initial_cap)
        type(svg_builder), intent(out) :: b
        integer, intent(in), optional :: initial_cap
        integer :: c
        c = BUILDER_INIT_CAP
        if (present(initial_cap)) c = max(1024, initial_cap)
        if (allocated(b%buf)) deallocate (b%buf)
        allocate (character(len=c) :: b%buf)
        b%n = 0
        b%cap = c
    end subroutine builder_init

    subroutine builder_clear(b)
        type(svg_builder), intent(inout) :: b
        b%n = 0
    end subroutine builder_clear

    subroutine builder_grow(b, need)
        type(svg_builder), intent(inout) :: b
        integer, intent(in) :: need
        character(len=:), allocatable :: tmp
        integer :: new_cap, old_n
        if (need <= b%cap) return
        new_cap = max(b%cap * 2, need + 4096)
        old_n = b%n
        allocate (character(len=new_cap) :: tmp)
        if (old_n > 0) tmp(1:old_n) = b%buf(1:old_n)
        call move_alloc(tmp, b%buf)
        b%cap = new_cap
        b%n = old_n
    end subroutine builder_grow

    subroutine builder_append(b, s)
        type(svg_builder), intent(inout) :: b
        character(len=*), intent(in) :: s
        integer :: m
        m = len(s)
        if (m <= 0) return
        if (b%n + m > b%cap) call builder_grow(b, b%n + m)
        b%buf(b%n + 1:b%n + m) = s(1:m)
        b%n = b%n + m
    end subroutine builder_append

    function builder_get(b) result(s)
        type(svg_builder), intent(in) :: b
        character(len=:), allocatable :: s
        if (b%n <= 0) then
            allocate (character(len=0) :: s)
        else
            allocate (character(len=b%n) :: s)
            s = b%buf(1:b%n)
        end if
    end function builder_get

    subroutine fmt_num(x, s, n)
        ! Format a real into s(1:n) without using trim/adjustl allocators.
        real(dp), intent(in) :: x
        character(len=*), intent(out) :: s
        integer, intent(out) :: n
        character(len=64) :: tmp
        integer :: i, j, k, dot

        if (x /= x) then
            s = "nan"
            n = 3
            return
        end if
        if (abs(x) < 1.0e-12_dp) then
            s = "0"
            n = 1
            return
        end if

        write (tmp, "(F24.6)") x
        ! left-trim spaces
        j = 1
        do while (j < 24 .and. tmp(j:j) == " ")
            j = j + 1
        end do
        ! copy and strip trailing zeros
        k = 0
        do i = j, 24
            k = k + 1
            s(k:k) = tmp(i:i)
        end do
        ! remove trailing spaces
        do while (k > 0 .and. s(k:k) == " ")
            k = k - 1
        end do
        dot = 0
        do i = 1, k
            if (s(i:i) == ".") then
                dot = i
                exit
            end if
        end do
        if (dot > 0) then
            do while (k > dot .and. s(k:k) == "0")
                k = k - 1
            end do
            if (k == dot) k = k - 1
        end if
        if (k <= 0) then
            s = "0"
            n = 1
        else
            n = k
        end if
    end subroutine fmt_num

    subroutine xml_escape_to(s, out, n)
        character(len=*), intent(in) :: s
        character(len=*), intent(out) :: out
        integer, intent(out) :: n
        integer :: i, m
        character(len=1) :: c

        n = 0
        m = len_trim(s)
        do i = 1, m
            c = s(i:i)
            select case (c)
            case ("&")
                if (n + 5 > len(out)) exit
                out(n+1:n+5) = "&amp;"
                n = n + 5
            case ("<")
                if (n + 4 > len(out)) exit
                out(n+1:n+4) = "&lt;"
                n = n + 4
            case (">")
                if (n + 4 > len(out)) exit
                out(n+1:n+4) = "&gt;"
                n = n + 4
            case ('"')
                if (n + 6 > len(out)) exit
                out(n+1:n+6) = "&quot;"
                n = n + 6
            case default
                if (n + 1 > len(out)) exit
                n = n + 1
                out(n:n) = c
            end select
        end do
    end subroutine xml_escape_to

end module fplot_svg
