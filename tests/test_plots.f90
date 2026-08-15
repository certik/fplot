program test_plots
    use fplot
    implicit none

    integer, parameter :: n = 100
    integer, parameter :: m = 20
    integer, parameter :: mk = 6
    integer, parameter :: n_marks = 11
    real(dp) :: x(n), y(n), y2(n), y3(n)
    real(dp) :: xl(m), yl(m), yl2(m)
    real(dp) :: xm(mk), ym(mk)
    character(len=1), parameter :: mark_codes(n_marks) = &
        ["o", "x", ".", "s", "^", "v", "<", ">", "*", "+", "D"]
    integer :: i
    real(dp), parameter :: pi = 3.14159265358979323846_dp

    ! Shared data
    do i = 1, n
        x(i) = 2.0_dp * pi * real(i - 1, dp) / real(n - 1, dp)
        y(i) = sin(x(i))
        y2(i) = cos(x(i))
        y3(i) = 0.5_dp * sin(2.0_dp * x(i))
    end do

    do i = 1, m
        xl(i) = 10.0_dp ** (real(i - 1, dp) / real(m - 1, dp) * 2.0_dp - 1.0_dp)  ! 0.1 .. 10
        yl(i) = xl(i) ** 2
        yl2(i) = 10.0_dp * exp(-xl(i))
    end do

    ! 1) basic_line
    call clf()
    call plot(x, y, "k-", label="sin")
    call title("Basic line")
    call xlabel("x")
    call ylabel("y")
    call grid(.true.)
    call legend()
    call xlim(0.0_dp, 2.0_dp * pi)
    call ylim(-1.2_dp, 1.2_dp)
    call savefig("tests/out/basic_line.svg")
    print *, "wrote tests/out/basic_line.svg"

    ! 2) multi_style
    call clf()
    call plot(x, y, "b-o", label="sin")
    call plot(x, y2, "r--", label="cos")
    call plot(x, y3, "g.", label="half sin2")
    call title("Multiple styles")
    call xlabel("x")
    call ylabel("y")
    call grid(.true.)
    call legend()
    call xlim(0.0_dp, 2.0_dp * pi)
    call ylim(-1.5_dp, 1.5_dp)
    call savefig("tests/out/multi_style.svg")
    print *, "wrote tests/out/multi_style.svg"

    ! 3) markers_only
    call clf()
    call plot(x(1:n:5), y(1:n:5), "rx", label="x marks")
    call plot(x(1:n:5), y2(1:n:5), "bo", label="circles")
    call plot(x(1:n:5), y3(1:n:5), "g.", label="points")
    call title("Markers only")
    call xlabel("x")
    call ylabel("y")
    call legend()
    call xlim(0.0_dp, 2.0_dp * pi)
    call ylim(-1.5_dp, 1.5_dp)
    call savefig("tests/out/markers_only.svg")
    print *, "wrote tests/out/markers_only.svg"

    ! 4) semilogx
    call clf()
    call semilogx(xl, yl, "b-", label="x^2")
    call title("semilogx")
    call xlabel("x")
    call ylabel("y")
    call grid(.true.)
    call legend()
    call xlim(0.1_dp, 10.0_dp)
    call ylim(0.0_dp, 120.0_dp)
    call savefig("tests/out/semilogx.svg")
    print *, "wrote tests/out/semilogx.svg"

    ! 5) semilogy
    call clf()
    call semilogy(xl, yl2, "r-o", label="10*exp(-x)")
    call title("semilogy")
    call xlabel("x")
    call ylabel("y")
    call grid(.true.)
    call legend()
    call xlim(0.1_dp, 10.0_dp)
    call ylim(1.0e-4_dp, 20.0_dp)
    call savefig("tests/out/semilogy.svg")
    print *, "wrote tests/out/semilogy.svg"

    ! 6) loglog
    call clf()
    call loglog(xl, yl, "k-", label="x^2")
    call title("loglog")
    call xlabel("x")
    call ylabel("y")
    call grid(.true.)
    call legend()
    call xlim(0.1_dp, 10.0_dp)
    call ylim(0.01_dp, 100.0_dp)
    call savefig("tests/out/loglog.svg")
    print *, "wrote tests/out/loglog.svg"

    ! 7) subplots_2x1 (two rows, one column; exercises subplot(m,n,i),
    !    per-axes state, and suptitle)
    call clf()
    call subplot(2, 1, 1)
    call plot(x, y, "b-", label="sin")
    call title("top: sin")
    call xlabel("x")
    call ylabel("y")
    call grid(.true.)
    call legend()

    call subplot(2, 1, 2)
    call plot(x, y2, "r--", label="cos")
    call title("bottom: cos")
    call xlabel("x")
    call ylabel("y")
    call grid(.true.)
    call legend()

    call suptitle("fplot subplots")
    call savefig("tests/out/subplots_2x1.svg")
    print *, "wrote tests/out/subplots_2x1.svg"

    ! 8) subplots_2x2 (four panels; also checks per-axes log scale)
    call clf()
    call subplot(2, 2, 1)
    call plot(x, y, "b-", label="sin")
    call title("sin")
    call grid(.true.)

    call subplot(2, 2, 2)
    call plot(x, y2, "r--", label="cos")
    call title("cos")
    call grid(.true.)

    call subplot(2, 2, 3)
    call plot(x, y3, "g-", label="0.5 sin(2x)")
    call title("half sin 2x")
    call grid(.true.)

    call subplot(2, 2, 4)
    call semilogx(xl, yl, "k-", label="x^2")
    call title("semilogx panel")
    call grid(.true.)

    call suptitle("fplot 2x2 subplots")
    call savefig("tests/out/subplots_2x2.svg")
    print *, "wrote tests/out/subplots_2x2.svg"

    ! 9) markers gallery (one row per marker code)
    call clf()
    do i = 1, mk
        xm(i) = real(i, dp)
        ym(i) = 1.0_dp
    end do
    do i = 1, n_marks
        call plot(xm, ym * real(i, dp), marker=mark_codes(i), linestyle="None", &
                  label=mark_codes(i))
    end do
    call title("Markers")
    call xlim(0.0_dp, real(mk + 1, dp))
    call ylim(0.0_dp, real(n_marks + 1, dp))
    call savefig("tests/out/markers_gallery.svg")
    print *, "wrote tests/out/markers_gallery.svg"

    print *, "All test plots written."
end program test_plots
