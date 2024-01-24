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

    integer function find_scan(str) result(pos)
        character(len=*), intent(in) :: str
        pos = scan(str,';')
    end function

    integer function find_index(str) result(pos)
        character(len=*), intent(in) :: str
        pos = index(str,';')
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

    ! Find semicolon location
    integer function find_v1(str) result(pos)
        character(len=*), intent(in) :: str  

        integer, parameter :: slen = 32
        integer(1), parameter :: sc = iachar(';',1)
        integer(1) :: r(slen)
        !logical(1) :: t(16)
        logical :: z
        integer :: i, k 

        if (mod(len(str),slen) /= 0) then
            write(*,'(A,I0)') "ERROR: String length should be multiple of ",slen
            error stop
        end if

        do i = 1, len(str), slen

            ! search for semicolon
            z = .false.
            !$omp simd reduction(.or.: z)
            do k = i, i+slen-1
                z = z .or. (iachar(str(k:k)) == sc)
            end do

            if (z) then
                ! semicolon was found
                pos = i + scan(str(i:i+slen-1),';') - 1
                return
            end if
        end do

        ! semicolon not found
        pos = 0
    end function

    ! Find semicolon location
    integer function find_v2(str) result(pos)
        character(len=*), intent(in) :: str  

        integer, parameter :: slen = 32
        integer(1), parameter :: sc = iachar(';',1)
        integer(1) :: r(slen)
        logical(1) :: t(slen)
        logical :: z
        integer :: i, k 

        if (mod(len(str),slen) /= 0) then
            write(*,'(A,I0)') "ERROR: String length should be multiple of ",slen
            error stop
        end if

        do i = 1, len(str), slen
            
            ! read chars into integer register (is this portable???)
            r = transfer(str(i:i+slen-1),r,slen)

            !$omp simd
            do k = 1, slen
                t(k) = r(k) == sc
            end do

            z = .false.
            !$omp simd reduction(.or.: z)
            do k = 1, slen
                z = z .or. t(k)
            end do

            if (z) then
                ! semicolon was found
                do k = 1, slen
                    if (r(k) == sc) then
                        pos = i + k - 1
                        return
                    end if
                end do
            end if
        end do

        ! semicolon not found
        pos = 0
    end function

    ! Find semicolon location
    integer function find_v3(str) result(pos)
        character(len=*), intent(in) :: str  

        integer, parameter :: slen = 32
        logical :: z
        integer :: i

        if (mod(len(str),slen) /= 0) then
            write(*,'(A,I0)') "ERROR: String length should be multiple of ",slen
            error stop
        end if

        do i = 1, len(str), slen
            
            z = has_semicolon(str(i:i+slen-1))
            if (z) then
                ! semicolon found
                pos = i + search(str(i:i+slen-1)) - 1
                return
            end if

        end do

        ! semicolon not found
        pos = 0

    contains

        logical function has_semicolon(str)
            character(len=1), intent(in) :: str(slen)
            has_semicolon = any(str == ';')
        end function

        integer function search(str) result(pos)
            character(len=1), intent(in) :: str(slen)
            integer :: k 
            pos = findloc(str,';',dim=1)
        end function

    end function

end module