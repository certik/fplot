! fplot_gif — the GIF container: colour quantization and LZW.
!
! An animation has to be one file, and the one format every browser, viewer
! and chat window plays without a codec is still GIF. The cost is 256 colours
! per file, so most of this module is about choosing those 256 well; the
! encoding itself is a page of LZW.
!
! Frames arrive as packed RGB bytes, all frames in one string, because that
! is what a caller accumulating frames already has and it avoids an array of
! deferred-length strings.
!
! No giflib, nothing to link.

module fplot_gif
    implicit none
    private

    public :: gif_encode

    ! Colours are bucketed to five bits per channel before anything else.
    ! 32768 buckets is small enough to histogram outright and fine enough
    ! that the error it introduces is below what the 256-colour palette
    ! introduces anyway.
    integer, parameter :: BITS = 5
    integer, parameter :: LEVELS = 2**BITS
    integer, parameter :: NBUCKET = LEVELS**3
    integer, parameter :: NPAL = 256

    ! LZW for GIF: 8-bit symbols, so clear is 256 and end-of-information 257.
    integer, parameter :: CLEAR = 256, EOI = 257, MAXCODE = 4095

    type :: buf_t
        integer, allocatable :: b(:)
        integer :: n = 0
        integer :: acc = 0
        integer :: nbits = 0
    end type buf_t

contains

    ! ------------------------------------------------------------------
    ! Byte buffer
    ! ------------------------------------------------------------------

    subroutine push(buf, v)
        type(buf_t), intent(inout) :: buf
        integer, intent(in) :: v
        integer, allocatable :: t(:)

        if (.not. allocated(buf%b)) allocate (buf%b(65536))
        if (buf%n >= size(buf%b)) then
            allocate (t(2*size(buf%b)))
            t(1:buf%n) = buf%b(1:buf%n)
            call move_alloc(t, buf%b)
        end if
        buf%n = buf%n + 1
        buf%b(buf%n) = iand(v, 255)
    end subroutine push

    subroutine push_str(buf, s)
        type(buf_t), intent(inout) :: buf
        character(len=*), intent(in) :: s
        integer :: i

        do i = 1, len(s)
            call push(buf, iachar(s(i:i)))
        end do
    end subroutine push_str

    ! Every multi-byte field in a GIF is little endian.
    subroutine push_u16(buf, v)
        type(buf_t), intent(inout) :: buf
        integer, intent(in) :: v

        call push(buf, iand(v, 255))
        call push(buf, iand(ishft(v, -8), 255))
    end subroutine push_u16

    ! LZW codes are packed least significant bit first, like deflate.
    subroutine put_bits(buf, code, nb)
        type(buf_t), intent(inout) :: buf
        integer, intent(in) :: code, nb
        integer :: i

        do i = 0, nb - 1
            if (btest(code, i)) buf%acc = ibset(buf%acc, buf%nbits)
            buf%nbits = buf%nbits + 1
            if (buf%nbits == 8) then
                call push(buf, buf%acc)
                buf%acc = 0
                buf%nbits = 0
            end if
        end do
    end subroutine put_bits

    subroutine flush_bits(buf)
        type(buf_t), intent(inout) :: buf

        if (buf%nbits > 0) then
            call push(buf, buf%acc)
            buf%acc = 0
            buf%nbits = 0
        end if
    end subroutine flush_bits

    function to_str(buf) result(s)
        type(buf_t), intent(in) :: buf
        character(len=:), allocatable :: s
        integer :: i

        allocate (character(len=buf%n) :: s)
        do i = 1, buf%n
            s(i:i) = achar(buf%b(i))
        end do
    end function to_str

    ! ------------------------------------------------------------------
    ! Median cut
    !
    ! One palette for the whole animation rather than one per frame: a local
    ! table per frame would track each frame better, but the small differences
    ! between the tables show up as the background shimmering from frame to
    ! frame, which is far more visible than the extra error.
    ! ------------------------------------------------------------------

    subroutine build_palette(pix, npix, pal, map)
        character(len=*), intent(in) :: pix
        integer, intent(in) :: npix
        integer, intent(out) :: pal(3, 0:NPAL - 1)
        integer, intent(out) :: map(0:NBUCKET - 1)
        integer, allocatable :: cnt(:), idx(:)
        real, allocatable :: sr(:), sg(:), sb(:)
        integer, allocatable :: lo(:), hi(:), pop(:)
        integer :: i, r, g, b, k, nb, nbox, big, ibig, ch, m, split, half, run
        integer :: rmin, rmax, gmin, gmax, bmin, bmax, v
        real :: tr, tg, tb, tn

        allocate (cnt(0:NBUCKET - 1), sr(0:NBUCKET - 1), sg(0:NBUCKET - 1), &
                  sb(0:NBUCKET - 1))
        cnt = 0
        sr = 0.0
        sg = 0.0
        sb = 0.0

        do i = 0, npix - 1
            r = iachar(pix(3*i + 1:3*i + 1))
            g = iachar(pix(3*i + 2:3*i + 2))
            b = iachar(pix(3*i + 3:3*i + 3))
            k = bucket(r, g, b)
            cnt(k) = cnt(k) + 1
            sr(k) = sr(k) + real(r)
            sg(k) = sg(k) + real(g)
            sb(k) = sb(k) + real(b)
        end do

        nb = count(cnt > 0)
        allocate (idx(max(nb, 1)))
        nb = 0
        do k = 0, NBUCKET - 1
            if (cnt(k) > 0) then
                nb = nb + 1
                idx(nb) = k
            end if
        end do

        allocate (lo(NPAL), hi(NPAL), pop(NPAL))
        nbox = 1
        lo(1) = 1
        hi(1) = nb
        pop(1) = npix

        ! Split the box holding the most pixels, along its longest axis, at
        ! the point that halves its pixel count. Splitting by pixel count
        ! rather than by bucket count is what keeps the palette on the
        ! colours the figure actually uses a lot of.
        do while (nbox < NPAL)
            big = 0
            ibig = 0
            do i = 1, nbox
                if (hi(i) > lo(i) .and. pop(i) > big) then
                    big = pop(i)
                    ibig = i
                end if
            end do
            if (ibig == 0) exit

            rmin = LEVELS; rmax = -1; gmin = LEVELS; gmax = -1
            bmin = LEVELS; bmax = -1
            do i = lo(ibig), hi(ibig)
                call unbucket(idx(i), r, g, b)
                rmin = min(rmin, r); rmax = max(rmax, r)
                gmin = min(gmin, g); gmax = max(gmax, g)
                bmin = min(bmin, b); bmax = max(bmax, b)
            end do
            ch = 1
            m = rmax - rmin
            if (gmax - gmin > m) then
                ch = 2
                m = gmax - gmin
            end if
            if (bmax - bmin > m) ch = 3

            call sort_range(idx, lo(ibig), hi(ibig), ch)

            half = pop(ibig)/2
            run = 0
            split = lo(ibig)
            do i = lo(ibig), hi(ibig) - 1
                run = run + cnt(idx(i))
                split = i
                if (run >= half) exit
            end do

            nbox = nbox + 1
            lo(nbox) = split + 1
            hi(nbox) = hi(ibig)
            hi(ibig) = split
            pop(ibig) = 0
            pop(nbox) = 0
            do i = lo(ibig), hi(ibig)
                pop(ibig) = pop(ibig) + cnt(idx(i))
            end do
            do i = lo(nbox), hi(nbox)
                pop(nbox) = pop(nbox) + cnt(idx(i))
            end do
        end do

        pal = 0
        map = 0
        do i = 1, nbox
            tr = 0.0; tg = 0.0; tb = 0.0; tn = 0.0
            do k = lo(i), hi(i)
                v = idx(k)
                tr = tr + sr(v)
                tg = tg + sg(v)
                tb = tb + sb(v)
                tn = tn + real(cnt(v))
                map(v) = i - 1
            end do
            if (tn > 0.0) then
                pal(1, i - 1) = min(255, max(0, nint(tr/tn)))
                pal(2, i - 1) = min(255, max(0, nint(tg/tn)))
                pal(3, i - 1) = min(255, max(0, nint(tb/tn)))
            end if
        end do
    end subroutine build_palette

    function bucket(r, g, b) result(k)
        integer, intent(in) :: r, g, b
        integer :: k

        k = ishft(ishft(r, BITS - 8), 2*BITS) &
            + ishft(ishft(g, BITS - 8), BITS) + ishft(b, BITS - 8)
    end function bucket

    subroutine unbucket(k, r, g, b)
        integer, intent(in) :: k
        integer, intent(out) :: r, g, b

        r = ishft(k, -2*BITS)
        g = iand(ishft(k, -BITS), LEVELS - 1)
        b = iand(k, LEVELS - 1)
    end subroutine unbucket

    ! Insertion sort would be quadratic on the 32768-bucket first box, so
    ! this is a plain quicksort with the middle element as the pivot.
    recursive subroutine sort_range(idx, lo, hi, ch)
        integer, intent(inout) :: idx(:)
        integer, intent(in) :: lo, hi, ch
        integer :: i, j, p, t

        if (hi <= lo) return
        p = key(idx((lo + hi)/2), ch)
        i = lo
        j = hi
        do while (i <= j)
            do while (key(idx(i), ch) < p)
                i = i + 1
            end do
            do while (key(idx(j), ch) > p)
                j = j - 1
            end do
            if (i <= j) then
                t = idx(i); idx(i) = idx(j); idx(j) = t
                i = i + 1
                j = j - 1
            end if
        end do
        if (lo < j) call sort_range(idx, lo, j, ch)
        if (i < hi) call sort_range(idx, i, hi, ch)
    end subroutine sort_range

    function key(k, ch) result(v)
        integer, intent(in) :: k, ch
        integer :: v, r, g, b

        call unbucket(k, r, g, b)
        if (ch == 1) then
            v = r
        else if (ch == 2) then
            v = g
        else
            v = b
        end if
    end function key

    ! ------------------------------------------------------------------
    ! LZW
    ! ------------------------------------------------------------------

    subroutine lzw(out, ind, n)
        type(buf_t), intent(inout) :: out
        integer, intent(in) :: ind(:)
        integer, intent(in) :: n
        type(buf_t) :: raw
        integer, allocatable :: dict(:)
        integer :: i, k, prefix, next, csize, hkey, sub, j

        ! The dictionary is a flat (prefix, suffix) table rather than a hash:
        ! 4096*256 entries is four megabytes, which buys an exact lookup with
        ! no collision handling at all.
        allocate (dict(0:4096*256 - 1))
        dict = 0

        call put_bits(raw, CLEAR, 9)
        next = EOI + 1
        csize = 9
        prefix = -1

        do i = 1, n
            k = ind(i)
            if (prefix < 0) then
                prefix = k
                cycle
            end if
            hkey = prefix*256 + k
            if (dict(hkey) > 0) then
                prefix = dict(hkey)
                cycle
            end if
            call put_bits(raw, prefix, csize)
            if (next > MAXCODE) then
                call put_bits(raw, CLEAR, csize)
                dict = 0
                next = EOI + 1
                csize = 9
            else
                dict(hkey) = next
                next = next + 1
                if (next > 2**csize .and. csize < 12) csize = csize + 1
            end if
            prefix = k
        end do

        if (prefix >= 0) call put_bits(raw, prefix, csize)
        call put_bits(raw, EOI, csize)
        call flush_bits(raw)

        ! The code stream is carried in sub-blocks of at most 255 bytes, each
        ! preceded by its length, and terminated by a zero-length block.
        call push(out, 8)
        i = 1
        do while (i <= raw%n)
            sub = min(255, raw%n - i + 1)
            call push(out, sub)
            do j = 0, sub - 1
                call push(out, raw%b(i + j))
            end do
            i = i + sub
        end do
        call push(out, 0)
    end subroutine lzw

    ! ------------------------------------------------------------------
    ! The file
    ! ------------------------------------------------------------------

    ! pix holds nframes frames of w*h packed RGB bytes, one after another.
    ! delay_cs is the frame delay in hundredths of a second, which is the
    ! only unit GIF has; loop true repeats forever.
    function gif_encode(w, h, nframes, pix, delay_cs, loop) result(bytes)
        integer, intent(in) :: w, h, nframes, delay_cs
        character(len=*), intent(in) :: pix
        logical, intent(in) :: loop
        character(len=:), allocatable :: bytes
        type(buf_t) :: out
        integer :: pal(3, 0:NPAL - 1)
        integer, allocatable :: map(:), ind(:)
        integer :: f, i, base, r, g, b, npix

        npix = w*h
        allocate (map(0:NBUCKET - 1), ind(npix))
        call build_palette(pix, npix*nframes, pal, map)

        call push_str(out, "GIF89a")
        call push_u16(out, w)
        call push_u16(out, h)
        ! Global colour table, 8 bits of colour resolution, 256 entries.
        call push(out, 247)
        call push(out, 0)
        call push(out, 0)
        do i = 0, NPAL - 1
            call push(out, pal(1, i))
            call push(out, pal(2, i))
            call push(out, pal(3, i))
        end do

        ! Looping is not in the GIF specification at all; it is the Netscape
        ! application extension that every viewer implements instead.
        if (loop) then
            call push(out, 33)
            call push(out, 255)
            call push(out, 11)
            call push_str(out, "NETSCAPE2.0")
            call push(out, 3)
            call push(out, 1)
            call push_u16(out, 0)
            call push(out, 0)
        end if

        do f = 1, nframes
            base = (f - 1)*npix*3
            do i = 1, npix
                r = iachar(pix(base + 3*i - 2:base + 3*i - 2))
                g = iachar(pix(base + 3*i - 1:base + 3*i - 1))
                b = iachar(pix(base + 3*i:base + 3*i))
                ind(i) = map(bucket(r, g, b))
            end do

            ! Graphic control extension: disposal 1, leave the frame in place,
            ! which is right because every frame here is a full repaint.
            call push(out, 33)
            call push(out, 249)
            call push(out, 4)
            call push(out, 4)
            call push_u16(out, max(delay_cs, 1))
            call push(out, 0)
            call push(out, 0)

            call push(out, 44)
            call push_u16(out, 0)
            call push_u16(out, 0)
            call push_u16(out, w)
            call push_u16(out, h)
            call push(out, 0)

            call lzw(out, ind, npix)
        end do

        call push(out, 59)
        bytes = to_str(out)
    end function gif_encode

end module fplot_gif
