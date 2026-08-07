program demo
    use fplot
    implicit none
    integer, parameter :: n = 200
    real(dp) :: x(n), y1(n), y2(n)
    integer :: i
    real(dp), parameter :: pi = 3.14159265358979323846_dp

    do i = 1, n
        x(i) = 4.0_dp * pi * real(i - 1, dp) / real(n - 1, dp)
        y1(i) = sin(x(i)) * exp(-0.1_dp * x(i))
        y2(i) = cos(x(i)) * exp(-0.1_dp * x(i))
    end do

    call plot(x, y1, "b-", label="damped sin", lw=2.0_dp)
    call plot(x, y2, "r--", label="damped cos")
    call title("fplot demo")
    call xlabel("time")
    call ylabel("amplitude")
    call grid(.true.)
    call legend()
    call savefig("demo.svg")
    print *, "Wrote demo.svg"
    call show()
end program demo
