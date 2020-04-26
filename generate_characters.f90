module rand_tools

implicit none

contains

subroutine rand_int_array(i,n,m)
    integer, intent(inout) :: i(:)
    integer, intent(in) :: n, m
    real, allocatable :: u(:)
    allocate(u(size(i)))
    call random_number(u)
    i = n + FLOOR((m+1-n)*u)
end subroutine

integer function rand_int(n,m)
    integer, intent(in) :: n, m

    real :: u
    call random_number(u)
    rand_int = n + FLOOR((m+1-n)*u)

end function

subroutine init_random_seed()
integer :: i, n, clock
integer, dimension(:), allocatable :: seed

call random_seed(size = n)
allocate(seed(n))

call system_clock(count=clock)

seed = clock + 37 * (/ (i - 1, i = 1, n) /)
call random_seed(put = seed)

deallocate(seed)
end subroutine

subroutine generate_random_characters(filename,nchar)
    character(len=*), intent(in) :: filename
    integer, intent(in) :: nchar
    integer :: i, unit, r

    !   33-47   !"#$%&'()*+,-./
    !   48-57   0123456789
    !   58-64   :;<=>?@
    !   65-70   ABCDEF
    !   71-90   GHIJKLMNOPQRSTUVWXYZ
    !   91-96   [\]^_`
    !  97-102   abcdef
    ! 103-122   ghijklmnopqrstuvwxyz
    ! 123-126   {|}~

    open(newunit=unit,file=filename)

    do i = 1, nchar
        r = rand_int(33,126)
        write(unit,'(a1)') achar(r)
    end do

    close(unit)
end subroutine


subroutine generate_random_characters_stream(filename,nchar)
    character(len=*), intent(in) :: filename
    integer, intent(in) :: nchar
    character(len=:), allocatable :: stream
    integer :: i, unit
    integer, allocatable :: r(:)

    !   33-47   !"#$%&'()*+,-./
    !   48-57   0123456789
    !   58-64   :;<=>?@
    !   65-70   ABCDEF
    !   71-90   GHIJKLMNOPQRSTUVWXYZ
    !   91-96   [\]^_`
    !  97-102   abcdef
    ! 103-122   ghijklmnopqrstuvwxyz
    ! 123-126   {|}~

    open(newunit=unit,file=filename)

    allocate(r(nchar))
    call rand_int_array(r,33,126)

    allocate(character(nchar) :: stream)
    do i = 1, nchar
        stream(i:i) = achar(r(i))
    end do

    write(unit,'(A)') stream
    close(unit)
end subroutine

end module

program generate_chars

    use rand_tools

    integer :: exp, sz
    character(len=30) :: filename

    do exp = 3, 8
        sz = 10**exp
        write(filename,'(A,I0,A)') 'chars-',sz,'.txt'
        print *, filename
        ! call generate_random_characters(filename,sz)
        call generate_random_characters_stream(filename,sz)
    end do

end program