module time_ascii

    use fortran_ascii
    implicit none

    integer, parameter :: dp = kind(1.0d0)

    abstract interface
        pure logical function validation_func_interface(c)
            character(len=1), intent(in) :: c
        end function
    end interface

    type :: ppa
        procedure(validation_func_interface), pointer, nopass :: pcf
    end type ppa

    type(ppa) :: pcfs(12) 

contains

    subroutine init_procedures()
        pcfs(1)%pcf => is_control
        pcfs(2)%pcf => is_printable
        pcfs(3)%pcf => is_white
        pcfs(4)%pcf => is_blank
        pcfs(5)%pcf => is_graphical
        pcfs(6)%pcf => is_punctuation
        pcfs(7)%pcf => is_alphanum
        pcfs(8)%pcf => is_alpha
        pcfs(9)%pcf => is_upper
        pcfs(10)%pcf => is_lower
        pcfs(11)%pcf => is_digit
        pcfs(12)%pcf => is_hex_digit
    end subroutine


    subroutine time_character_file(filename,sz,times)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: sz
        real(dp), intent(out) :: times(12)

        character(len=:), allocatable :: chars
        integer :: i, j, reps, unit
        real(dp) :: t1, t2, total
        logical :: res
        integer(8) :: c1, c2, cr
        call system_clock(count_rate=cr)
        allocate(character(len=sz) :: chars)

        open(newunit=unit,file=filename)
        read(unit,'(a)') chars


        do j = 1, 12
            total = 0.0_dp
            do reps = 1, 10
                ! call cpu_time(t1)
                call system_clock(count=c1)
                do i = 1, sz
                    res = pcfs(j)%pcf(chars(i:i))
                end do
                ! call cpu_time(t2)
                call system_clock(count=c2)
                ! total = total + (t2 - t1)
                total = total + real(c2 - c1,dp)
            end do
            ! times(j) = total/10.0_dp
            times(j) = total/(10.0_dp*cr)
        end do

        close(unit)
    end subroutine

end module

program benchmark

  use time_ascii
  implicit none

  integer :: exp, sz
  character(len=30) :: filename
  real(dp) :: times(12)

  character(14) :: funcs(12) = [ character(14) :: 'is_control','is_printable','is_white',&
  'is_blank','is_graphical','is_punctuation','is_alphanum','is_alpha','is_upper',&
  'is_lower','is_digit','is_hex_digit']   


  call init_procedures

  write(*,'(A9,*(X,A14))') "#        ", adjustr(funcs)
  do exp = 3, 8
      sz = 10**exp
      write(filename,'(A,I0,A)') 'chars-',sz,'.txt'
      ! print *, filename
      call time_character_file(trim(filename),sz,times)
      write(*,'(I9,*(X,E14.6))') sz, times
  end do

end program