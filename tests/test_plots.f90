program test_plots
    use fplot
    implicit none
    integer :: fig1
    type(axes), allocatable :: axs(:, :)

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
    integer, parameter :: nsym = 201
    real(dp) :: xsym(nsym)
    integer, parameter :: nsm = 2
    real(dp) :: xsm(nsm) = [0.0_dp, 1.0_dp], ysm(nsm)
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
    call save_all("basic_line")

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
    call save_all("multi_style")

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
    call save_all("markers_only")

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
    call save_all("semilogx")

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
    call save_all("semilogy")

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
    call save_all("loglog")

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
    call save_all("subplots_2x1")

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
    call save_all("subplots_2x2")

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
    call save_all("markers_gallery")

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
    call save_all("scatter")

    ! 11) bar
    call clf()
    call bar(xb, hb, label="counts")
    call title("Bar")
    call xlabel("category")
    call ylabel("value")
    call legend()
    call save_all("bar")

    ! 12) hist
    call clf()
    call hist(xh, bins=8, label="samples")
    call title("Histogram")
    call xlabel("value")
    call ylabel("count")
    call legend()
    call save_all("hist")

    ! 13) fill_between
    call clf()
    call fill_between(x, y, y3, alpha=0.5_dp, label="band")
    call plot(x, y, "b-", label="sin")
    call title("Fill between")
    call xlabel("x")
    call ylabel("y")
    call grid(.true.)
    call legend()
    call save_all("fill_between")

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
    call save_all("errorbar")

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
    call save_all("hv_lines")

    ! 16) text and annotate
    call clf()
    call plot(x, y, "b-", label="sin")
    call text(1.0_dp, 0.8_dp, "peak region", color="k")
    call annotate("minimum", 4.712_dp, -1.0_dp, xtext=2.2_dp, ytext=-0.55_dp, color="r")
    call title("Text and annotate")
    call xlabel("x")
    call ylabel("y")
    call grid(.true.)
    call save_all("text_annotate")

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
    call save_all("ticks_legend")

    ! 18) a non-default figure size
    call figure(figsize=[8.0_dp, 3.0_dp])
    call plot(x, y, "b-", label="sin")
    call title("Wide figure")
    call xlabel("x")
    call ylabel("y")
    call grid(.true.)
    call legend()
    call save_all("figsize")

    ! 19) more series than the old fixed 32-slot cap.
    ! figure(), not clf(), because clf() keeps the previous canvas size.
    call figure()
    do i = 1, 40
        call plot(x, sin(x + 0.05_dp * real(i, dp)))
    end do
    call title("Forty series")
    call xlabel("x")
    call ylabel("y")
    call save_all("many_series")

    ! 20) alpha on lines and markers
    call figure()
    call plot(x, y, "b-", label="sin", alpha=0.35_dp)
    call plot(x, cos(x), "r-o", label="cos", alpha=0.6_dp)
    call scatter(x(1:ns), y(1:ns), s=80.0_dp, c="g", label="pts", alpha=0.5_dp)
    call title("Alpha")
    call xlabel("x")
    call ylabel("y")
    call legend()
    call save_all("alpha")

    ! 21) a non-default figure facecolor
    call figure()
    call plot(x, y, "b-")
    call title("Figure facecolor")
    call xlabel("x")
    call ylabel("y")
    call grid(.true.)
    call save_all("facecolor", facecolor="#eeeeee")

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
    call save_all("imshow")

    ! 23) imshow with an extent, lower origin and a colorbar
    call figure()
    call imshow(zimg, cmap="plasma", extent=[0.0_dp, 4.0_dp, 0.0_dp, 2.0_dp], &
                origin="lower")
    call colorbar(label="value")
    call title("imshow with colorbar")
    call xlabel("x")
    call ylabel("y")
    call save_all("imshow_cbar")

    ! 24) scatter with per-point sizes and color-mapped values
    call figure()
    call scatter(x(1:ns), y(1:ns), sizes=svals, cvals=cvals, cmap="viridis")
    call colorbar(label="c")
    call title("Scatter with c and s arrays")
    call xlabel("x")
    call ylabel("y")
    call save_all("scatter_cmap")

    ! 25) contour lines
    call figure()
    call contour(zimg)
    call title("contour")
    call xlabel("x")
    call ylabel("y")
    call save_all("contour")

    ! 26) filled contours with a colorbar
    call figure()
    call contourf(zimg, cmap="coolwarm")
    call colorbar()
    call title("contourf")
    call xlabel("x")
    call ylabel("y")
    call save_all("contourf")

    ! 27) step
    call figure()
    call step(xb, hb, where="mid")
    call title("step")
    call save_all("step")

    ! 28) stem
    call figure()
    call stem(xb, hb)
    call title("stem")
    call save_all("stem")

    ! 29) barh
    call figure()
    call barh(xb, hb)
    call title("barh")
    call save_all("barh")

    ! 30) pie
    call figure()
    call pie(hb, labels=["a", "b", "c", "d", "e", "f"])
    call title("pie")
    call save_all("pie")

    do i = 1, nd
        dist1(i) = sin(real(i, dp)) + 0.3_dp * cos(2.7_dp * real(i, dp))
        dist2(i) = 1.0_dp + 2.0_dp * sin(0.7_dp * real(i, dp))**3
    end do

    ! 31) boxplot
    call figure()
    call boxplot(dist1)
    call boxplot(dist2)
    call title("boxplot")
    call save_all("boxplot")

    ! 32) violinplot
    call figure()
    call violinplot(dist1)
    call violinplot(dist2)
    call title("violinplot")
    call save_all("violinplot")

    do i = 1, nsym
        xsym(i) = -100.0_dp + real(i - 1, dp)
    end do

    ! 33) symlog y scale
    call figure()
    call plot(xsym, xsym)
    call set_yscale("symlog")
    call title("symlog")
    call save_all("symlog")

    ! 34) axis("equal")
    call figure()
    call plot(xb, hb, marker="o")
    call axis("equal")
    call title("axis equal")
    call save_all("axis_equal")

    ! 35) tick styling and hidden spines
    call figure()
    call plot(xb, hb)
    call tick_params(direction="in", labelsize=8.0_dp)
    call tick_params(axis="x", rotation=45.0_dp)
    call spines(top=.false., right=.false.)
    call title("tick_params")
    call save_all("tick_style")

    ! 36) tight_layout with long tick labels
    call figure()
    call subplot(1, 2, 1)
    call plot(xb, hb * 1000.0_dp)
    call ylabel("value")
    call xlabel("category")
    call subplot(1, 2, 2)
    call plot(xb, hb)
    call xlabel("category")
    call tight_layout()
    call save_all("tight_layout")

    ! 37) subplots_adjust
    call figure()
    call subplot(2, 1, 1)
    call plot(xb, hb)
    call subplot(2, 1, 2)
    call plot(xb, hb)
    call subplots_adjust(left=0.2_dp, hspace=0.5_dp)
    call save_all("subplots_adjust")

    ! 38) twinx
    call figure()
    call plot(xb, hb)
    call ylabel("left")
    call xlabel("x")
    call twinx()
    call plot(xb, hb * 100.0_dp)
    call ylabel("right")
    call title("twinx")
    call save_all("twinx")

    ! 39) color spellings
    call figure()
    do i = 1, 6
        ysm = real(i, dp) + 0.0_dp * xsm
        select case (i)
        case (1); call plot(xsm, ysm, color="red", lw=3.0_dp)
        case (2); call plot(xsm, ysm, color="tab:orange", lw=3.0_dp)
        case (3); call plot(xsm, ysm, color="steelblue", lw=3.0_dp)
        case (4); call plot(xsm, ysm, color="#0f0", lw=3.0_dp)
        case (5); call plot(xsm, ysm, color="0.5", lw=3.0_dp)
        case (6); call plot(xsm, ysm, color="#8c564bcc", lw=3.0_dp)
        end select
    end do
    call title("color names")
    call save_all("colors")

    ! 40) font sizes
    call figure()
    call plot(xb, hb, label="sine")
    call title("big title", fontsize=17.0_dp)
    call xlabel("x axis", fontsize=14.0_dp)
    call ylabel("y axis", fontsize=14.0_dp)
    call tick_params(labelsize=13.0_dp)
    call legend(fontsize=12.0_dp)
    call save_all("fontsize")

    ! 41) legend options
    call figure()
    call plot(xb, hb, label="alpha")
    call plot(xb, hb * 0.5_dp, label="beta")
    call plot(xb, hb * 0.25_dp, label="gamma")
    call plot(xb, hb * 0.125_dp, label="delta")
    call legend(loc="upper right", ncol=2, title="series", frameon=.false.)
    call save_all("legend_opts")

    ! 42) savefig(bbox_inches="tight")
    call figure()
    call plot(xb, hb)
    call title("tight")
    call xlabel("x")
    call ylabel("y")
    call save_all("savefig_tight", bbox_inches="tight", dpi=200.0_dp)

    ! 43) two live figures kept apart, then closed
    call figure()
    fig1 = gcf()
    call plot(xb, hb, "r-")
    call title("figure one")
    call figure()
    call plot(xb, -hb, "b-")
    call title("figure two")
    call close()
    call figure(num=fig1)
    call save_all("figures")
    call close(all=.true.)

    ! 44) an image on a log axis, where the samples are no longer evenly
    ! spaced on the canvas and so cannot be sent as one raster
    call figure()
    call imshow(zimg, extent=[1.0_dp, 1000.0_dp, 0.0_dp, 4.0_dp], aspect="auto")
    call set_xscale("log")
    call title("imshow on a log axis")
    call save_all("imshow_log")

    ! 45) subplots with axes handles and a shared x axis
    call subplots(2, 2, axs, sharex=.true.)
    call axs(1, 1)%plot(x, y, "b-", label="sin")
    call axs(1, 1)%set_title("one")
    call axs(1, 1)%legend()
    call axs(1, 2)%scatter(xs, ys, s=18.0_dp, c="r")
    call axs(1, 2)%set_title("two")
    call axs(2, 1)%bar(xb, hb, color="g")
    call axs(2, 1)%set_xlabel("x")
    call axs(2, 2)%plot(x, y3, "m--")
    call axs(2, 2)%grid(.true.)
    call axs(2, 2)%set_xlabel("x")
    call suptitle("subplots with handles")
    call save_all("subplots_shared")

    ! 46) a style sheet
    call style_use("ggplot")
    call clf()
    call plot(x, y, label="sin")
    call plot(x, y2, label="cos")
    call title("ggplot style")
    call xlabel("x")
    call ylabel("y")
    call legend()
    call save_all("style_ggplot")
    call style_use("default")

    print *, "All test plots written."

contains

    ! Every case is written in all three formats, so the three savefig calls
    ! and the line that reports them are stated once here rather than once
    ! per case. The options are passed straight through: an absent optional
    ! stays absent, so a plain case says nothing about facecolor or dpi.
    subroutine save_all(stem, facecolor, bbox_inches, dpi)
        character(len=*), intent(in) :: stem
        character(len=*), intent(in), optional :: facecolor, bbox_inches
        real(dp), intent(in), optional :: dpi

        call savefig("tests/out/"//stem//".svg", facecolor=facecolor, &
                     bbox_inches=bbox_inches, dpi=dpi)
        call savefig("tests/out/"//stem//".pdf", facecolor=facecolor, &
                     bbox_inches=bbox_inches, dpi=dpi)
        call savefig("tests/out/"//stem//".png", facecolor=facecolor, &
                     bbox_inches=bbox_inches, dpi=dpi)
        print *, "wrote tests/out/"//stem//".svg, .pdf and .png"
    end subroutine save_all

end program test_plots
