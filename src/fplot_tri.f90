! fplot_tri — a triangulation of scattered points.
!
! matplotlib's tri module hands its points to Qhull and gets a Delaunay
! triangulation back. This is the same triangulation, built here by the
! Bowyer-Watson insertion: start from one huge triangle covering
! everything, add the points one at a time, and each time throw away the
! triangles whose circumcircle the new point falls inside and stitch the
! hole up to it.
!
! The work is quadratic in the number of points, which is what a plot of
! a few thousand of them can well afford.

module fplot_tri
    use fplot_style, only: dp
    implicit none
    private

    public :: delaunay

contains

    ! The triangles of the Delaunay triangulation of (x, y), as triples of
    ! indices into the points. tri must be allocatable; it comes back with
    ! three rows and one column per triangle.
    subroutine delaunay(x, y, tri, ntri)
        real(dp), intent(in) :: x(:), y(:)
        integer, allocatable, intent(out) :: tri(:, :)
        integer, intent(out) :: ntri
        real(dp), allocatable :: px(:), py(:)
        integer, allocatable :: t(:, :), bad(:), edge(:, :)
        integer :: n, i, j, k, nt, nbad, ne, maxt
        real(dp) :: xmin, xmax, ymin, ymax, cx, cy, d
        logical :: shared

        n = size(x)
        ntri = 0
        if (n < 3) then
            allocate (tri(3, 0))
            return
        end if

        ! The three extra points of the enclosing triangle sit at the end,
        ! so that a triangle still touching them is easy to spot.
        allocate (px(n + 3), py(n + 3))
        px(1:n) = x(1:n)
        py(1:n) = y(1:n)
        xmin = minval(x)
        xmax = maxval(x)
        ymin = minval(y)
        ymax = maxval(y)
        cx = 0.5_dp*(xmin + xmax)
        cy = 0.5_dp*(ymin + ymax)
        d = max(xmax - xmin, ymax - ymin)
        if (d <= 0.0_dp) d = 1.0_dp
        d = 20.0_dp*d
        px(n + 1) = cx - d
        py(n + 1) = cy - d
        px(n + 2) = cx + d
        py(n + 2) = cy - d
        px(n + 3) = cx
        py(n + 3) = cy + d

        maxt = 4*n + 16
        allocate (t(3, maxt), bad(maxt), edge(2, 3*maxt))
        nt = 1
        t(:, 1) = [n + 1, n + 2, n + 3]

        do i = 1, n
            nbad = 0
            do j = 1, nt
                if (in_circle(px, py, t(:, j), px(i), py(i))) then
                    nbad = nbad + 1
                    bad(nbad) = j
                end if
            end do
            if (nbad == 0) cycle

            ! The hole left behind is bounded by the edges belonging to
            ! exactly one of the discarded triangles.
            ne = 0
            do j = 1, nbad
                call add_edge(edge, ne, t(1, bad(j)), t(2, bad(j)))
                call add_edge(edge, ne, t(2, bad(j)), t(3, bad(j)))
                call add_edge(edge, ne, t(3, bad(j)), t(1, bad(j)))
            end do

            ! Drop the discarded triangles, keeping the rest packed.
            k = 0
            do j = 1, nt
                if (any(bad(1:nbad) == j)) cycle
                k = k + 1
                t(:, k) = t(:, j)
            end do
            nt = k

            do j = 1, ne
                if (edge(1, j) == 0) cycle
                if (nt >= maxt) exit
                nt = nt + 1
                t(:, nt) = [edge(1, j), edge(2, j), i]
            end do
        end do

        ! Anything still holding on to the enclosing triangle is outside
        ! the points and goes.
        allocate (tri(3, nt))
        do j = 1, nt
            if (any(t(:, j) > n)) cycle
            ntri = ntri + 1
            tri(:, ntri) = t(:, j)
        end do
    end subroutine delaunay

    ! Remember an edge, or cancel it if it is already there: an edge two
    ! discarded triangles share is inside the hole, not on its boundary.
    subroutine add_edge(edge, ne, a, b)
        integer, intent(inout) :: edge(:, :)
        integer, intent(inout) :: ne
        integer, intent(in) :: a, b
        integer :: j

        do j = 1, ne
            if ((edge(1, j) == a .and. edge(2, j) == b) .or. &
                (edge(1, j) == b .and. edge(2, j) == a)) then
                edge(1, j) = 0
                edge(2, j) = 0
                return
            end if
        end do
        ne = ne + 1
        edge(1, ne) = a
        edge(2, ne) = b
    end subroutine add_edge

    ! Is (qx, qy) inside the circumcircle of the triangle? The usual
    ! determinant, with the sign of the triangle's area taken out so that
    ! the winding does not matter.
    pure logical function in_circle(px, py, t, qx, qy) result(yes)
        real(dp), intent(in) :: px(:), py(:), qx, qy
        integer, intent(in) :: t(3)
        real(dp) :: ax, ay, bx, by, cx, cy, det, area

        ax = px(t(1)) - qx
        ay = py(t(1)) - qy
        bx = px(t(2)) - qx
        by = py(t(2)) - qy
        cx = px(t(3)) - qx
        cy = py(t(3)) - qy
        det = (ax*ax + ay*ay)*(bx*cy - by*cx) &
              - (bx*bx + by*by)*(ax*cy - ay*cx) &
              + (cx*cx + cy*cy)*(ax*by - ay*bx)
        area = (px(t(2)) - px(t(1)))*(py(t(3)) - py(t(1))) &
               - (py(t(2)) - py(t(1)))*(px(t(3)) - px(t(1)))
        if (area < 0.0_dp) det = -det
        yes = det > 0.0_dp
    end function in_circle

end module fplot_tri
