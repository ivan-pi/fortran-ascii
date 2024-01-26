module time_ascii
    use iso_fortran_env, only: i64 => int64

    implicit none

    integer, parameter :: dp = kind(1.0d0)

    abstract interface
        subroutine find(str, pos) 
          character(len=*), intent(in) :: str
          integer, intent(out) :: pos
        end subroutine
    end interface

contains

    subroutine measure(f,filename,sz)
        procedure(find) :: f
        character(len=*), intent(in) :: filename
        integer, intent(in) :: sz
        
        real(dp) :: time

        character(len=:), allocatable :: chars
        integer :: reps, unit, nreps
        integer(i64) :: c1, c2, cr
        integer :: ires, nsz

        call system_clock(count_rate=cr)

        nsz = sz - mod(sz,32)
        allocate(character(len=nsz) :: chars)

        open(newunit=unit,file=filename)
        read(unit,'(a)') chars
        close(unit)

        chars(nsz:nsz) = ';'

      nreps = 1
      do
        call system_clock(count=c1)
        do reps = 1, nreps
            call f(chars,ires)
            if (ires /= nsz) error stop "SEARCH FAILED!"
        end do
        call system_clock(count=c2)
        time = (real(c2 - c1,dp)/real(cr,dp))
        if (time > 0.2_dp) exit
        nreps = nreps * 2
      end do

      time = time / real(nreps,dp)

      write(*,'(I16,2(2X,ES14.3),I16)') nsz, nsz / time, mb_per_s(nsz,time), nreps

    end subroutine

    real(dp) function mb_per_s(sz,time)
      integer, intent(in) :: sz
      real(dp), intent(in) :: time
      mb_per_s = ( real(sz,dp) / real(1024,dp)**2 ) / time
    end function

end module

program benchmark_scan
  use ascii_simd, only: find_v1, find_v2, find_v3, &
    find_scan, find_findloc, find_index
  
  use time_ascii, only: measure
  implicit none

  integer :: exp, sz
  character(len=30) :: filename


  write(*,'(A1,4A16)')"#", "size", "chars/s", "MB/s", "#reps"

  write(*,'(A)') "# --- scan ---"
  do exp = 3, 8
      sz = 10**exp
      write(filename,'(A,I0,A)') 'chars-',sz,'.txt'
      ! print *, filename
      call measure(find1,trim(filename), sz)
  end do

  ! write(*,'(//,A)') "# --- index ---"
  ! do exp = 3, 8
  !     sz = 10**exp
  !     write(filename,'(A,I0,A)') 'chars-',sz,'.txt'
  !     ! print *, filename
  !     call measure(find2,trim(filename), sz)
  ! end do


  ! write(*,'(//,A)') "# --- findloc ---"
  ! do exp = 3, 8
  !     sz = 10**exp
  !     write(filename,'(A,I0,A)') 'chars-',sz,'.txt'
  !     ! print *, filename
  !     call measure(find3,trim(filename), sz)
  ! end do

  ! write(*,'(//,A)') "# --- custom 1 ---"
  ! do exp = 3, 8
  !     sz = 10**exp
  !     write(filename,'(A,I0,A)') 'chars-',sz,'.txt'
  !     ! print *, filename
  !     call measure(find4,trim(filename), sz)
  ! end do


  write(*,'(//,A)') "# --- custom 2 ---"
  do exp = 3, 8
      sz = 10**exp
      write(filename,'(A,I0,A)') 'chars-',sz,'.txt'
      ! print *, filename
      call measure(find5,trim(filename), sz)
  end do

  ! write(*,'(//,A)') "# --- custom 3 ---"
  ! do exp = 3, 8
  !     sz = 10**exp
  !     write(filename,'(A,I0,A)') 'chars-',sz,'.txt'
  !     ! print *, filename
  !     call measure(find6,trim(filename), sz)
  ! end do

  write(*,'(//,A)') "# --- custom 4 ---"
  do exp = 3, 8
      sz = 10**exp
      write(filename,'(A,I0,A)') 'chars-',sz,'.txt'
      ! print *, filename
      call measure(find7,trim(filename), sz)
  end do

contains
  
  subroutine find1(str,i)
    character(len=*), intent(in) :: str
    integer, intent(out) :: i
    i = find_scan(str)
  end subroutine

  subroutine find2(str,i)
    character(len=*), intent(in) :: str
    integer, intent(out) :: i
    i = find_index(str)
  end subroutine
  
  subroutine find3(str,i)
    character(len=*), intent(in) :: str
    integer, intent(out) :: i
    i = find_findloc(str)
  end subroutine
  
  subroutine find4(str,i)
    character(len=*), intent(in) :: str
    integer, intent(out) :: i
    i = find_v1(str)
  end subroutine

  subroutine find5(str,i)
    character(len=*), intent(in) :: str
    integer, intent(out) :: i
    i = find_v2(str)
  end subroutine

  subroutine find6(str,i)
    character(len=*), intent(in) :: str
    integer, intent(out) :: i
    i = find_v3(str)
  end subroutine

  subroutine find7(str,i)
    character(len=*), intent(in) :: str
    integer, intent(out) :: i

    interface
      integer function scan(str)
        character(len=*), intent(in) :: str
      end function
    end interface
    i = scan(str)
  end subroutine

end program