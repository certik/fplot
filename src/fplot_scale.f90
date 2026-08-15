! fplot_scale — how a data value becomes a position along an axis.
!
! Every axis is a monotone map from data space to a linear "screen" space.
! Collecting that map behind one type means the renderer, the limit padding
! and the tick placement all ask the same question instead of each carrying
! its own `is this a log axis` flag.
module fplot_scale
    use fplot_style, only: dp
    implicit none
    private

    public :: scale_t, scale_fwd, scale_inv
    public :: SCALE_LINEAR, SCALE_LOG, SCALE_SYMLOG

    integer, parameter :: SCALE_LINEAR = 0
    integer, parameter :: SCALE_LOG = 1
    integer, parameter :: SCALE_SYMLOG = 2

    type :: scale_t
        integer :: kind = SCALE_LINEAR
        ! symlog only: the half-width of the linear region around zero, and
        ! how many decades wide that region is drawn.
        real(dp) :: linthresh = 2.0_dp
        real(dp) :: linscale = 1.0_dp
    end type scale_t

contains

    pure function scale_fwd(s, v) result(u)
        type(scale_t), intent(in) :: s
        real(dp), intent(in) :: v
        real(dp) :: u, a

        select case (s%kind)
        case (SCALE_LOG)
            ! Non-positive values have no place on a log axis; clamping them
            ! to the smallest representable one keeps the map total.
            u = log10(max(v, tiny(1.0_dp)))
        case (SCALE_SYMLOG)
            a = abs(v)
            if (a <= s%linthresh) then
                u = v * linscale_adj(s)
            else
                u = sign(s%linthresh * (linscale_adj(s) + log10(a / s%linthresh)), v)
            end if
        case default
            u = v
        end select
    end function scale_fwd

    pure function scale_inv(s, u) result(v)
        type(scale_t), intent(in) :: s
        real(dp), intent(in) :: u
        real(dp) :: v, edge

        select case (s%kind)
        case (SCALE_LOG)
            v = 10.0_dp ** u
        case (SCALE_SYMLOG)
            edge = s%linthresh * linscale_adj(s)
            if (abs(u) <= edge) then
                v = u / linscale_adj(s)
            else
                v = sign(s%linthresh * 10.0_dp ** (abs(u) / s%linthresh - linscale_adj(s)), u)
            end if
        case default
            v = u
        end select
    end function scale_inv

    ! The linear region is stretched so that `linscale` decades of the log
    ! region and the linear region are drawn the same length.
    pure function linscale_adj(s) result(f)
        type(scale_t), intent(in) :: s
        real(dp) :: f
        f = s%linscale / (1.0_dp - 0.1_dp)
    end function linscale_adj

end module fplot_scale
