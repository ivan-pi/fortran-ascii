module ascii_simd

implicit none
public

contains

subroutine tolower(str)
    implicit none
    character(len=*), intent(inout) :: str

    integer(1), parameter :: A = iachar('A'), Z = iachar('Z')
    integer(1), parameter :: diff_case = iachar('A', 1) - iachar('a', 1)

    integer(1) :: r
    logical(1) :: m

    integer :: i, l

    l = len_trim(str)
#if defined(USE_VLEN)
    !$omp simd simdlen(USE_VLEN) private(r,m)
#else
    !$omp simd private(r,m)
#endif
    do i = 1, l
        r = iachar(str(i:i),1)
        m = (r >= A) .and. (r <= Z)
        if (m) r = r - diff_case
        str(i:i) = achar(r)
    end do

end subroutine

subroutine toupper(str)
    implicit none
    character(len=*), intent(inout) :: str

    integer(1), parameter :: a = iachar('a'), z = iachar('z')
    integer(1), parameter :: diff_case = iachar('A', 1) - iachar('a', 1)

    integer(1) :: r
    logical(1) :: m

    integer :: i, l

    l = len_trim(str)
    !$omp simd private(r,m)
    do i = 1, l
        r = iachar(str(i:i),1)
        m = (r >= a) .and. (r <= z)
        if (m) r = r + diff_case
        str(i:i) = achar(r)
    end do

end subroutine

subroutine lower_sc(str)
    character(len=*), intent(inout) :: str
    integer                      :: i
    integer                      :: iend

    iend = len_trim(str)
    do i = 1, iend          ! step thru each letter in the string in specified range
        select case (str(i:i))
        case ('A':'Z')
            str(i:i) = achar(iachar(str(i:i)) + 32)     ! change letter to miniscule
        case default
        end select
    end do

end subroutine

    ! Find semicolon location
    integer function findsc(str)
        character(len=*), intent(in) :: str  

        integer(1), parameter :: sc = iachar(';',1)
        integer(1) :: r(16)
        !logical(1) :: t(16)
        logical :: z
        integer :: i, k 

        if (mod(len(str),16) /= 0) then
            error stop "Length should be multiple of 16"
        end if

        do i = 1, len(str), 16
            ! read chars into integer register
            r = transfer(str(i:i+16),r)
        
            ! search for semicolon
            z = .false.
            !$omp simd simdlen(16) reduction(.or.: z)
            do k = 1, 16
            !    t(k) = r(k) == sc
                z = z .or. (r(k) == sc)
            end do

            if (z) then
               ! semicolon was found
                findsc = i + findloc(r,sc,dim=1) - 1
                return
            end if
        end do

        ! semicolon not found
        findsc = 0
    end function

    integer function find_scan(str) result(pos)
        character(len=*), intent(in) :: str
        pos = scan(str,';')
    end function

    integer function find_findloc(str) result(pos)
        character(len=*), intent(in) :: str
        pos = z(str,len(str))
    contains
        integer function z(s,n)
            character(len=1) :: s(n)
            integer, intent(in) :: n 
            z = findloc(s,';',dim=1)
        end function
    end function

end module