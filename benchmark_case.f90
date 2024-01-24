module time_ascii
    use iso_fortran_env, only: i64 => int64

    implicit none

    integer, parameter :: dp = kind(1.0d0)

    abstract interface
        subroutine inplace_transform(str)
          character(len=*), intent(inout) :: str
        end subroutine
    end interface

contains

    subroutine measure(transform,filename,sz)
        procedure(inplace_transform) :: transform
        character(len=*), intent(in) :: filename
        integer, intent(in) :: sz
        
        real(dp) :: time

        character(len=:), allocatable :: chars
        integer :: reps, unit, nreps
        integer(i64) :: c1, c2, cr

        call system_clock(count_rate=cr)
        allocate(character(len=sz) :: chars)

        open(newunit=unit,file=filename)
        read(unit,'(a)') chars
        chars(sz:sz) = ';'
        close(unit)

      nreps = 1
      do
        call system_clock(count=c1)
        do reps = 1, nreps
            call transform(chars)
        end do
        call system_clock(count=c2)
        time = (real(c2 - c1,dp)/real(cr,dp))
        if (time > 0.2_dp) exit
        nreps = nreps * 2
      end do

      time = time / real(nreps,dp)

      
      write(*,'(I16,2(2X,ES14.3),I16)') sz, sz / time, mb_per_s(sz,time), nreps

    end subroutine

    real(dp) function mb_per_s(sz,time)
      integer, intent(in) :: sz
      real(dp), intent(in) :: time
      mb_per_s = ( real(sz,dp) / real(1024,dp)**2 ) / time
    end function

end module

program benchmark_case
  use ascii_simd, only: tolower, lower_sc
  use time_ascii
  implicit none

  integer :: exp, sz
  character(len=30) :: filename


  write(*,'(4A16)') "size", "chars/s", "MB/s", "#reps"

  print *, "--- tolower simd ---"
  do exp = 3, 9
      sz = 10**exp
      write(filename,'(A,I0,A)') 'chars-',sz,'.txt'
      ! print *, filename
      call measure(tolower,trim(filename), sz)
  end do

  ! print *, "--- tolower select case ---"
  ! do exp = 3, 9
  !     sz = 10**exp
  !     write(filename,'(A,I0,A)') 'chars-',sz,'.txt'
  !     ! print *, filename
  !     call measure(lower_sc,trim(filename), sz)
  ! end do

end program