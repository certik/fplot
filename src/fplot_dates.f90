! Dates on an axis.
!
! matplotlib plots dates as plain numbers — days since 1970-01-01 — and
! then hands the axis a locator and a formatter that know what those
! numbers mean. This module is that arithmetic: the conversion both ways,
! a locator that steps in years, months, days, hours, minutes or seconds,
! and the formats matplotlib's AutoDateFormatter uses for each of them.
module fplot_dates
    use fplot_style, only: dp
    implicit none
    private

    public :: date_num, num_date, date_ticks, format_date
    public :: UNIT_YEAR, UNIT_MONTH, UNIT_DAY, UNIT_HOUR, UNIT_MINUTE, UNIT_SECOND

    integer, parameter :: UNIT_YEAR = 1
    integer, parameter :: UNIT_MONTH = 2
    integer, parameter :: UNIT_DAY = 3
    integer, parameter :: UNIT_HOUR = 4
    integer, parameter :: UNIT_MINUTE = 5
    integer, parameter :: UNIT_SECOND = 6

    real(dp), parameter :: SEC = 1.0_dp/86400.0_dp

contains

    ! Days since 1970-01-01, which is what matplotlib's date2num returns.
    pure function date_num(y, mo, d, h, mi, s) result(v)
        integer, intent(in) :: y, mo, d
        integer, intent(in), optional :: h, mi, s
        real(dp) :: v
        integer :: hh, mm, ss

        hh = 0
        mm = 0
        ss = 0
        if (present(h)) hh = h
        if (present(mi)) mm = mi
        if (present(s)) ss = s
        v = real(days_from_civil(y, mo, d), dp) &
            + (real(hh, dp)*3600.0_dp + real(mm, dp)*60.0_dp + real(ss, dp))*SEC
    end function date_num

    pure subroutine num_date(v, y, mo, d, h, mi, s)
        real(dp), intent(in) :: v
        integer, intent(out) :: y, mo, d, h, mi, s
        integer :: z, rem

        z = floor(v)
        ! Rounded to the second, so that a value that is a hair under
        ! midnight does not come back as 23:59:59 of the day before.
        rem = nint((v - real(z, dp))*86400.0_dp)
        if (rem >= 86400) then
            rem = rem - 86400
            z = z + 1
        end if
        call civil_from_days(z, y, mo, d)
        h = rem/3600
        mi = mod(rem/60, 60)
        s = mod(rem, 60)
    end subroutine num_date

    ! Howard Hinnant's civil calendar algorithms, which are exact for any
    ! year a plot is likely to carry and need no tables.
    pure function days_from_civil(y, m, d) result(z)
        integer, intent(in) :: y, m, d
        integer :: z, yy, era, yoe, doy, doe

        yy = y
        if (m <= 2) yy = yy - 1
        if (yy >= 0) then
            era = yy/400
        else
            era = (yy - 399)/400
        end if
        yoe = yy - era*400
        if (m > 2) then
            doy = (153*(m - 3) + 2)/5 + d - 1
        else
            doy = (153*(m + 9) + 2)/5 + d - 1
        end if
        doe = yoe*365 + yoe/4 - yoe/100 + doy
        z = era*146097 + doe - 719468
    end function days_from_civil

    pure subroutine civil_from_days(z0, y, m, d)
        integer, intent(in) :: z0
        integer, intent(out) :: y, m, d
        integer :: z, era, doe, yoe, doy, mp, yy

        z = z0 + 719468
        if (z >= 0) then
            era = z/146097
        else
            era = (z - 146096)/146097
        end if
        doe = z - era*146097
        yoe = (doe - doe/1460 + doe/36524 - doe/146096)/365
        yy = yoe + era*400
        doy = doe - (365*yoe + yoe/4 - yoe/100)
        mp = (5*doy + 2)/153
        d = doy - (153*mp + 2)/5 + 1
        if (mp < 10) then
            m = mp + 3
        else
            m = mp - 9
        end if
        y = yy
        if (m <= 2) y = y + 1
    end subroutine civil_from_days

    ! Ticks on round dates: the finest unit and step that still fits in
    ! the number of ticks the axis has room for. unit comes back so that
    ! the labels can be written the way matplotlib writes them.
    subroutine date_ticks(vmin, vmax, nbins, t, nt, unit)
        real(dp), intent(in) :: vmin, vmax
        integer, intent(in) :: nbins
        real(dp), intent(out) :: t(:)
        integer, intent(out) :: nt
        integer, intent(out) :: unit
        ! The steps matplotlib's AutoDateLocator is willing to take.
        integer, parameter :: NU = 6
        integer, parameter :: NS = 7
        integer, parameter :: STEPS(NS, NU) = reshape([ &
            1, 2, 5, 10, 20, 50, 100, &      ! years
            1, 2, 3, 4, 6, 0, 0, &           ! months
            1, 2, 3, 7, 14, 0, 0, &          ! days
            1, 2, 3, 4, 6, 12, 0, &          ! hours
            1, 5, 10, 15, 30, 0, 0, &        ! minutes
            1, 5, 10, 15, 30, 0, 0], &       ! seconds
            [NS, NU])
        ! matplotlib will not use a unit that gives it fewer than three
        ! ticks, and once it has one it takes the smallest step that
        ! keeps the count under the unit's maximum.
        integer, parameter :: MINTICKS = 3
        integer, parameter :: MAXTICKS(NU) = [11, 12, 11, 12, 11, 11]
        real(dp) :: span, num
        integer :: u, k, step, cap

        span = abs(vmax - vmin)
        unit = UNIT_SECOND
        nt = 0
        do u = UNIT_YEAR, UNIT_SECOND
            num = span/unit_days(u)
            if (num < real(MINTICKS, dp)) cycle
            cap = min(MAXTICKS(u), max(2, nbins + 2))
            step = 0
            do k = 1, NS
                if (STEPS(k, u) == 0) cycle
                step = STEPS(k, u)
                if (num <= real(step*(cap - 1), dp)) exit
            end do
            call ticks_for(vmin, vmax, u, step, t, nt)
            unit = u
            if (nt >= 2) return
        end do
        ! Nothing has three ticks in it, so the span is very short.
        call ticks_for(vmin, vmax, UNIT_SECOND, 1, t, nt)
        unit = UNIT_SECOND
    end subroutine date_ticks

    pure function unit_days(u) result(v)
        integer, intent(in) :: u
        real(dp) :: v
        select case (u)
        case (UNIT_YEAR); v = 365.25_dp
        case (UNIT_MONTH); v = 30.4375_dp
        case (UNIT_DAY); v = 1.0_dp
        case (UNIT_HOUR); v = 1.0_dp/24.0_dp
        case (UNIT_MINUTE); v = 1.0_dp/1440.0_dp
        case default; v = SEC
        end select
    end function unit_days

    ! Every round date of the given unit and step inside the view. Years
    ! and months are stepped on the calendar rather than by adding a mean
    ! length, so the ticks land on the first of the month.
    subroutine ticks_for(vmin, vmax, unit, step, t, nt)
        real(dp), intent(in) :: vmin, vmax
        integer, intent(in) :: unit, step
        real(dp), intent(out) :: t(:)
        integer, intent(out) :: nt
        integer :: y, mo, d, h, mi, s, k, mtot
        real(dp) :: lo, hi, v, dt

        lo = min(vmin, vmax)
        hi = max(vmin, vmax)
        nt = 0
        call num_date(lo, y, mo, d, h, mi, s)

        select case (unit)
        case (UNIT_YEAR)
            y = (y/step)*step
            do k = 0, 1000
                v = date_num(y + k*step, 1, 1)
                if (v > hi) exit
                if (v >= lo) call push(t, nt, v)
            end do
        case (UNIT_MONTH)
            mtot = y*12 + (mo - 1)
            mtot = (mtot/step)*step
            do k = 0, 1000
                v = date_num((mtot + k*step)/12, mod(mtot + k*step, 12) + 1, 1)
                if (v > hi) exit
                if (v >= lo) call push(t, nt, v)
            end do
        case default
            dt = unit_days(unit)*real(step, dp)
            ! Start from the round value at or before the low end.
            v = floor(lo/dt + 1.0e-9_dp)*dt
            do k = 0, 100000
                if (v > hi + 1.0e-9_dp) exit
                if (v >= lo - 1.0e-9_dp) call push(t, nt, v)
                v = v + dt
                if (nt >= size(t)) exit
            end do
        end select
    end subroutine ticks_for

    pure subroutine push(t, nt, v)
        real(dp), intent(inout) :: t(:)
        integer, intent(inout) :: nt
        real(dp), intent(in) :: v
        if (nt >= size(t)) return
        nt = nt + 1
        t(nt) = v
    end subroutine push

    ! The formats matplotlib's AutoDateFormatter uses, one per unit.
    subroutine format_date(v, unit, s, n)
        real(dp), intent(in) :: v
        integer, intent(in) :: unit
        character(len=*), intent(out) :: s
        integer, intent(out) :: n
        character(len=32) :: tmp
        integer :: y, mo, d, h, mi, se

        call num_date(v, y, mo, d, h, mi, se)
        select case (unit)
        case (UNIT_YEAR)
            write (tmp, '(I0)') y
        case (UNIT_MONTH)
            write (tmp, '(I0,"-",I2.2)') y, mo
        case (UNIT_DAY)
            write (tmp, '(I0,"-",I2.2,"-",I2.2)') y, mo, d
        case (UNIT_SECOND)
            write (tmp, '(I2.2,":",I2.2,":",I2.2)') h, mi, se
        case default
            write (tmp, '(I2.2,":",I2.2)') h, mi
        end select
        n = len_trim(tmp)
        s(1:n) = tmp(1:n)
    end subroutine format_date

end module fplot_dates
