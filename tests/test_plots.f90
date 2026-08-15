program test_plots
    use fplot
    implicit none

    integer, parameter :: n = 100
    integer, parameter :: m = 20
    integer, parameter :: mk = 6
    integer, parameter :: n_marks = 11
    integer, parameter :: ns = 30
    integer, parameter :: nb = 6
    integer, parameter :: nh = 20
    integer, parameter :: ne = 8
    real(dp) :: x(n), y(n), y2(n), y3(n)
    real(dp) :: xl(m), yl(m), yl2(m)
    real(dp) :: xm(mk), ym(mk)
    real(dp) :: xs(ns), ys(ns)
    integer, parameter :: nd = 40
    real(dp) :: dist1(nd), dist2(nd)
    real(dp) :: xe(ne), ye(ne), ee(ne)
    real(dp), parameter :: xb(nb) = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, 6.0_dp]
    real(dp), parameter :: hb(nb) = [3.0_dp, 5.0_dp, 2.0_dp, 7.0_dp, 4.0_dp, 6.0_dp]
    real(dp), parameter :: xh(nh) = [ &
        0.2_dp, 0.5_dp, 0.7_dp, 1.1_dp, 1.3_dp, 1.4_dp, 1.8_dp, 2.0_dp, 2.1_dp, 2.3_dp, &
        2.4_dp, 2.6_dp, 2.9_dp, 3.0_dp, 3.2_dp, 3.3_dp, 3.7_dp, 4.0_dp, 4.4_dp, 4.9_dp]
    character(len=1), parameter :: mark_codes(n_marks) = &
        ["o", "x", ".", "s", "^", "v", "<", ">", "*", "+", "D"]
    integer :: i, j
    integer, parameter :: nzr = 8, nzc = 16
    real(dp) :: zimg(nzr, nzc)
    real(dp) :: svals(ns), cvals(ns)
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

    ! 10) scatter
    call clf()
    do i = 1, ns
        xs(i) = 10.0_dp * real(i - 1, dp) / real(ns - 1, dp)
        ys(i) = sin(xs(i)) + 0.1_dp * xs(i)
    end do
    call scatter(xs, ys, label="points")
    call title("Scatter")
    call xlabel("x")
    call ylabel("y")
    call grid(.true.)
    call legend()
    call savefig("tests/out/scatter.svg")
    print *, "wrote tests/out/scatter.svg"

    ! 11) bar
    call clf()
    call bar(xb, hb, label="counts")
    call title("Bar")
    call xlabel("category")
    call ylabel("value")
    call legend()
    call savefig("tests/out/bar.svg")
    print *, "wrote tests/out/bar.svg"

    ! 12) hist
    call clf()
    call hist(xh, bins=8, label="samples")
    call title("Histogram")
    call xlabel("value")
    call ylabel("count")
    call legend()
    call savefig("tests/out/hist.svg")
    print *, "wrote tests/out/hist.svg"

    ! 13) fill_between
    call clf()
    call fill_between(x, y, y3, alpha=0.5_dp, label="band")
    call plot(x, y, "b-", label="sin")
    call title("Fill between")
    call xlabel("x")
    call ylabel("y")
    call grid(.true.)
    call legend()
    call savefig("tests/out/fill_between.svg")
    print *, "wrote tests/out/fill_between.svg"

    ! 14) errorbar
    call clf()
    do i = 1, ne
        xe(i) = real(i, dp)
        ye(i) = sqrt(xe(i))
        ee(i) = 0.15_dp * ye(i)
    end do
    call errorbar(xe, ye, ee, fmt="o-", capsize=3.0_dp, label="meas")
    call title("Errorbar")
    call xlabel("x")
    call ylabel("y")
    call grid(.true.)
    call legend()
    call savefig("tests/out/errorbar.svg")
    print *, "wrote tests/out/errorbar.svg"

    ! 15) hv_lines
    call clf()
    call plot(x, y, "b-", label="sin")
    call axhline(0.0_dp, color="k", linestyle="--")
    call axvline(pi, color="r", linestyle=":")
    call title("Reference lines")
    call xlabel("x")
    call ylabel("y")
    call grid(.true.)
    call legend()
    call savefig("tests/out/hv_lines.svg")
    print *, "wrote tests/out/hv_lines.svg"

    ! 16) text and annotate
    call clf()
    call plot(x, y, "b-", label="sin")
    call text(1.0_dp, 0.8_dp, "peak region", color="k")
    call annotate("minimum", 4.712_dp, -1.0_dp, xtext=2.2_dp, ytext=-0.55_dp, color="r")
    call title("Text and annotate")
    call xlabel("x")
    call ylabel("y")
    call grid(.true.)
    call savefig("tests/out/text_annotate.svg")
    print *, "wrote tests/out/text_annotate.svg"

    ! 17) custom ticks, minor ticks and legend placement
    call clf()
    call plot(x, y, "b-", label="sin")
    call plot(x, cos(x), "r--", label="cos")
    call xticks([0.0_dp, 1.5708_dp, 3.1416_dp, 4.7124_dp, 6.2832_dp], &
                ["0    ", "pi/2 ", "pi   ", "3pi/2", "2pi  "])
    call yticks([-1.0_dp, 0.0_dp, 1.0_dp])
    call minorticks_on()
    call legend(loc="lower left")
    call title("Ticks and legend placement")
    call xlabel("x")
    call ylabel("y")
    call savefig("tests/out/ticks_legend.svg")
    print *, "wrote tests/out/ticks_legend.svg"

    ! 18) a non-default figure size
    call figure(figsize=[8.0_dp, 3.0_dp])
    call plot(x, y, "b-", label="sin")
    call title("Wide figure")
    call xlabel("x")
    call ylabel("y")
    call grid(.true.)
    call legend()
    call savefig("tests/out/figsize.svg")
    print *, "wrote tests/out/figsize.svg"

    ! 19) more series than the old fixed 32-slot cap.
    ! figure(), not clf(), because clf() keeps the previous canvas size.
    call figure()
    do i = 1, 40
        call plot(x, sin(x + 0.05_dp * real(i, dp)))
    end do
    call title("Forty series")
    call xlabel("x")
    call ylabel("y")
    call savefig("tests/out/many_series.svg")
    print *, "wrote tests/out/many_series.svg"

    ! 20) alpha on lines and markers
    call figure()
    call plot(x, y, "b-", label="sin", alpha=0.35_dp)
    call plot(x, cos(x), "r-o", label="cos", alpha=0.6_dp)
    call scatter(x(1:ns), y(1:ns), s=80.0_dp, c="g", label="pts", alpha=0.5_dp)
    call title("Alpha")
    call xlabel("x")
    call ylabel("y")
    call legend()
    call savefig("tests/out/alpha.svg")
    print *, "wrote tests/out/alpha.svg"

    ! 21) a non-default figure facecolor
    call figure()
    call plot(x, y, "b-")
    call title("Figure facecolor")
    call xlabel("x")
    call ylabel("y")
    call grid(.true.)
    call savefig("tests/out/facecolor.svg", facecolor="#eeeeee")
    print *, "wrote tests/out/facecolor.svg"

    do i = 1, nzr
        do j = 1, nzc
            zimg(i, j) = sin(0.4_dp * real(j, dp)) + cos(0.5_dp * real(i, dp))
        end do
    end do

    do i = 1, ns
        svals(i) = 20.0_dp + 8.0_dp * real(i, dp)
        cvals(i) = real(i, dp)
    end do

    ! 22) imshow with the default upper origin
    call figure()
    call imshow(zimg)
    call title("imshow")
    call savefig("tests/out/imshow.svg")
    print *, "wrote tests/out/imshow.svg"

    ! 23) imshow with an extent, lower origin and a colorbar
    call figure()
    call imshow(zimg, cmap="plasma", extent=[0.0_dp, 4.0_dp, 0.0_dp, 2.0_dp], &
                origin="lower")
    call colorbar(label="value")
    call title("imshow with colorbar")
    call xlabel("x")
    call ylabel("y")
    call savefig("tests/out/imshow_cbar.svg")
    print *, "wrote tests/out/imshow_cbar.svg"

    ! 24) scatter with per-point sizes and colour-mapped values
    call figure()
    call scatter(x(1:ns), y(1:ns), sizes=svals, cvals=cvals, cmap="viridis")
    call colorbar(label="c")
    call title("Scatter with c and s arrays")
    call xlabel("x")
    call ylabel("y")
    call savefig("tests/out/scatter_cmap.svg")
    print *, "wrote tests/out/scatter_cmap.svg"

    ! 25) contour lines
    call figure()
    call contour(zimg)
    call title("contour")
    call xlabel("x")
    call ylabel("y")
    call savefig("tests/out/contour.svg")
    print *, "wrote tests/out/contour.svg"

    ! 26) filled contours with a colorbar
    call figure()
    call contourf(zimg, cmap="coolwarm")
    call colorbar()
    call title("contourf")
    call xlabel("x")
    call ylabel("y")
    call savefig("tests/out/contourf.svg")
    print *, "wrote tests/out/contourf.svg"

    ! 27) step
    call figure()
    call step(xb, hb, where="mid")
    call title("step")
    call savefig("tests/out/step.svg")
    print *, "wrote tests/out/step.svg"

    ! 28) stem
    call figure()
    call stem(xb, hb)
    call title("stem")
    call savefig("tests/out/stem.svg")
    print *, "wrote tests/out/stem.svg"

    ! 29) barh
    call figure()
    call barh(xb, hb)
    call title("barh")
    call savefig("tests/out/barh.svg")
    print *, "wrote tests/out/barh.svg"

    ! 30) pie
    call figure()
    call pie(hb, labels=["a", "b", "c", "d", "e", "f"])
    call title("pie")
    call savefig("tests/out/pie.svg")
    print *, "wrote tests/out/pie.svg"

    do i = 1, nd
        dist1(i) = sin(real(i, dp)) + 0.3_dp * cos(2.7_dp * real(i, dp))
        dist2(i) = 1.0_dp + 2.0_dp * sin(0.7_dp * real(i, dp))**3
    end do

    ! 31) boxplot
    call figure()
    call boxplot(dist1)
    call boxplot(dist2)
    call title("boxplot")
    call savefig("tests/out/boxplot.svg")
    print *, "wrote tests/out/boxplot.svg"

    ! 32) violinplot
    call figure()
    call violinplot(dist1)
    call violinplot(dist2)
    call title("violinplot")
    call savefig("tests/out/violinplot.svg")
    print *, "wrote tests/out/violinplot.svg"

    print *, "All test plots written."
end program test_plots
