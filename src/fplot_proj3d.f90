! fplot_proj3d — the 3D camera: the matrix mplot3d builds and applies.
!
! Everything here is matplotlib's mpl_toolkits/mplot3d/proj3d.py, in the same
! order and with the same constants, because the point of a 3D plot in this
! library is to look like the 3D plot matplotlib would have drawn. The four
! steps are: scale the data box to the unit cube (stretched by the plot-box
! aspect), look at its centre from a point on a sphere around it, rotate the
! world so the camera is at the origin, and divide by depth.
!
! The module is pure arithmetic, with no idea what an axes or a series is,
! which is why it sits outside fplot.f90 rather than inside it.

module fplot_proj3d
    implicit none
    private

    integer, parameter :: dp = kind(1.0d0)

    public :: proj3d_matrix, proj3d_point, proj3d_box_aspect
    public :: PROJ3D_DIST, PROJ3D_FOCAL, PROJ3D_VIEW_MIN, PROJ3D_VIEW_MAX

    ! The camera distance and focal length mplot3d fixes; it exposes them but
    ! nothing in ordinary use changes them.
    real(dp), parameter :: PROJ3D_DIST = 10.0_dp
    real(dp), parameter :: PROJ3D_FOCAL = 1.0_dp

    ! The 2D window the projection lands in: mplot3d's set_top_view, which is
    ! (-0.95/dist, 0.9/dist), shifted up and left to leave room for labels.
    real(dp), parameter :: PROJ3D_VIEW_MIN = -0.95_dp/PROJ3D_DIST
    real(dp), parameter :: PROJ3D_VIEW_MAX = 0.9_dp/PROJ3D_DIST

contains

    ! The default plot box, 4:4:3, normalized the way set_box_aspect does it.
    ! The two odd factors are matplotlib's own: 1.8294... was tuned to keep
    ! the 3.2 appearance and 25/24 compensates for the 3.9 margin change.
    function proj3d_box_aspect() result(pb)
        real(dp) :: pb(3)
        real(dp) :: a(3), s

        a = [4.0_dp, 4.0_dp, 3.0_dp]
        s = 1.8294640721620434_dp*25.0_dp/24.0_dp/sqrt(sum(a*a))
        pb = a*s
    end function proj3d_box_aspect

    ! lims is (xmin, xmax, ymin, ymax, zmin, zmax); elev and azim are in
    ! degrees, as in view_init.
    subroutine proj3d_matrix(lims, elev, azim, M)
        real(dp), intent(in) :: lims(6), elev, azim
        real(dp), intent(out) :: M(4, 4)
        real(dp) :: pb(3), world(4, 4), view(4, 4), persp(4, 4), rot(4, 4)
        real(dp) :: trans(4, 4), R(3), ps(3), eye(3), u(3), v(3), w(3)
        real(dp) :: d(3), rad, er, ar
        integer :: i

        pb = proj3d_box_aspect()

        d(1) = (lims(2) - lims(1))/pb(1)
        d(2) = (lims(4) - lims(3))/pb(2)
        d(3) = (lims(6) - lims(5))/pb(3)
        world = 0.0_dp
        do i = 1, 3
            world(i, i) = 1.0_dp/d(i)
            world(i, 4) = -lims(2*i - 1)/d(i)
        end do
        world(4, 4) = 1.0_dp

        ! The eye sits on a sphere around the middle of the box, at the
        ! elevation and azimuth asked for, and the focal length scales the
        ! distance so that changing it zooms rather than dollies.
        R = 0.5_dp*pb
        rad = acos(-1.0_dp)/180.0_dp
        er = elev*rad
        ar = azim*rad
        ps = [cos(er)*cos(ar), cos(er)*sin(ar), sin(er)]
        eye = R + PROJ3D_DIST*ps*PROJ3D_FOCAL

        ! Viewing axes: w out of the screen, u to the right, v up. The
        ! vertical is z, so V = (0, 0, 1) and u = V x w.
        w = R + PROJ3D_DIST*ps - R
        w = w/sqrt(sum(w*w))
        u = [-w(2), w(1), 0.0_dp]
        u = u/sqrt(sum(u*u))
        v = [w(2)*u(3) - w(3)*u(2), w(3)*u(1) - w(1)*u(3), &
             w(1)*u(2) - w(2)*u(1)]

        rot = 0.0_dp
        rot(4, 4) = 1.0_dp
        rot(1, 1:3) = u
        rot(2, 1:3) = v
        rot(3, 1:3) = w
        trans = 0.0_dp
        do i = 1, 4
            trans(i, i) = 1.0_dp
        end do
        trans(1:3, 4) = -eye
        view = matmul(rot, trans)

        ! Perspective, with zfront = -dist and zback = dist: the b term of
        ! _persp_transformation vanishes and c reduces to -dist.
        persp = 0.0_dp
        persp(1, 1) = PROJ3D_FOCAL
        persp(2, 2) = PROJ3D_FOCAL
        persp(3, 4) = -PROJ3D_DIST
        persp(4, 3) = -1.0_dp

        M = matmul(persp, matmul(view, world))
    end subroutine proj3d_matrix

    ! A point in data coordinates to the projected (x, y, depth). Depth is
    ! kept because that is what the painter's algorithm sorts on.
    pure subroutine proj3d_point(M, x, y, z, px, py, pz)
        real(dp), intent(in) :: M(4, 4), x, y, z
        real(dp), intent(out) :: px, py, pz
        real(dp) :: q(4)

        q = matmul(M, [x, y, z, 1.0_dp])
        px = q(1)/q(4)
        py = q(2)/q(4)
        pz = q(3)/q(4)
    end subroutine proj3d_point

end module fplot_proj3d
