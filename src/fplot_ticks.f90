! Nice tick generation for linear and log axes.
module fplot_ticks
    use fplot_style, only: dp
    implicit none
    private

    integer, parameter, public :: MAX_TICKS = 32

    public :: linear_ticks
    public :: nice_number
    public :: log_ticks
    public :: format_tick_to

contains

    pure function nice_number(x, round_up) result(nice)
        real(dp), intent(in) :: x
        logical, intent(in) :: round_up
        real(dp) :: nice
        real(dp) :: expn, f, nf

        if (x <= 0.0_dp) then
            nice = 1.0_dp
            return
        end if

        expn = floor(log10(x))
        f = x / (10.0_dp ** expn)

        if (round_up) then
            if (f < 1.5_dp) then
                nf = 1.0_dp
            else if (f < 3.0_dp) then
                nf = 2.0_dp
            else if (f < 7.0_dp) then
                nf = 5.0_dp
            else
                nf = 10.0_dp
            end if
        else
            if (f <= 1.0_dp) then
                nf = 1.0_dp
            else if (f <= 2.0_dp) then
                nf = 2.0_dp
            else if (f <= 5.0_dp) then
                nf = 5.0_dp
            else
                nf = 10.0_dp
            end if
        end if

        nice = nf * (10.0_dp ** expn)
    end function nice_number

    subroutine linear_ticks(vmin, vmax, max_ticks, ticks, n_ticks)
        real(dp), intent(in) :: vmin, vmax
        integer, intent(in) :: max_ticks
        real(dp), intent(out) :: ticks(MAX_TICKS)
        integer, intent(out) :: n_ticks
        real(dp) :: lo, hi, range, step, start, t
        integer :: i, mt, n

        lo = min(vmin, vmax)
        hi = max(vmin, vmax)
        if (abs(hi - lo) < 1.0e-30_dp * max(1.0_dp, abs(lo), abs(hi))) then
            lo = lo - 1.0_dp
            hi = hi + 1.0_dp
        end if

        mt = max(2, min(max_ticks, MAX_TICKS))
        range = nice_number(hi - lo, .false.)
        step = nice_number(range / real(mt - 1, dp), .true.)
        if (step <= 0.0_dp) step = hi - lo
        start = ceiling(lo / step - 1.0e-12_dp) * step

        n = 0
        do i = 0, MAX_TICKS - 1
            t = start + real(i, dp) * step
            if (t > hi + abs(step) * 1.0e-9_dp) exit
            if (t >= lo - abs(step) * 1.0e-9_dp) then
                n = n + 1
                ticks(n) = t
            end if
        end do

        if (n == 0) then
            n = 2
            ticks(1) = lo
            ticks(2) = hi
        end if
        n_ticks = n
    end subroutine linear_ticks

    subroutine log_ticks(vmin, vmax, ticks, n_ticks)
        real(dp), intent(in) :: vmin, vmax
        real(dp), intent(out) :: ticks(MAX_TICKS)
        integer, intent(out) :: n_ticks
        real(dp) :: lo, hi, t
        integer :: i0, i1, i, n

        lo = min(vmin, vmax)
        hi = max(vmin, vmax)
        if (lo <= 0.0_dp) lo = tiny(1.0_dp)
        if (hi <= 0.0_dp) hi = 1.0_dp

        i0 = int(floor(log10(lo)))
        i1 = int(ceiling(log10(hi)))

        n = 0
        do i = i0, i1
            t = 10.0_dp ** real(i, dp)
            if (t >= lo * (1.0_dp - 1.0e-12_dp) .and. t <= hi * (1.0_dp + 1.0e-12_dp)) then
                if (n < MAX_TICKS) then
                    n = n + 1
                    ticks(n) = t
                end if
            end if
        end do

        if (n == 0) then
            n = 2
            ticks(1) = lo
            ticks(2) = hi
        end if
        n_ticks = n
    end subroutine log_ticks

    subroutine format_tick_to(v, is_log, s, n)
        real(dp), intent(in) :: v
        logical, intent(in) :: is_log
        character(len=*), intent(out) :: s
        integer, intent(out) :: n
        real(dp) :: av
        integer :: expn
        character(len=32) :: tmp
        integer :: i, j, k, dot

        av = abs(v)
        if (is_log .or. (av > 0.0_dp .and. (av >= 1.0e4_dp .or. av < 1.0e-3_dp))) then
            if (av < 1.0e-30_dp) then
                s = "0"
                n = 1
                return
            end if
            expn = nint(log10(av))
            if (abs(av / (10.0_dp ** expn) - 1.0_dp) < 1.0e-8_dp) then
                if (expn == 0) then
                    s = "1"
                    n = 1
                else if (expn == 1) then
                    s = "10"
                    n = 2
                else
                    if (v < 0.0_dp) then
                        write (tmp, '("-1e",I0)') expn
                    else
                        write (tmp, '("1e",I0)') expn
                    end if
                    n = len_trim(tmp)
                    s(1:n) = tmp(1:n)
                end if
                return
            end if
        end if

        if (abs(v - real(nint(v), dp)) < 1.0e-8_dp * max(1.0_dp, av)) then
            write (tmp, "(I0)") nint(v)
            n = len_trim(tmp)
            s(1:n) = tmp(1:n)
            return
        end if

        write (tmp, "(F20.4)") v
        j = 1
        do while (j < 20 .and. tmp(j:j) == " ")
            j = j + 1
        end do
        k = 0
        do i = j, 20
            if (tmp(i:i) == " ") exit
            k = k + 1
            s(k:k) = tmp(i:i)
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
    end subroutine format_tick_to

end module fplot_ticks
