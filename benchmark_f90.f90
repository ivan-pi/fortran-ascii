module time_ascii

  use fortran_ascii
  use iso_fortran_env, only: i64 => int64
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

    procedure(validation_func_interface), pointer :: vf
    character(len=:), allocatable :: chars
    integer :: i, j, reps, unit
    logical :: res
    integer(i64) :: c1, c2, cr

    call system_clock(count_rate=cr)
    allocate(character(len=sz) :: chars)

    open(newunit=unit,file=filename)
    read(unit,'(a)') chars

    do j = 1, 12
      vf => pcfs(j)%pcf
      call system_clock(count=c1)
      do reps = 1, 10
        do i = 1, sz
          res = vf(chars(i:i))
        end do
      end do
      call system_clock(count=c2)
      times(j) = (10*sz)/(real(c2-c1,dp)/real(cr,dp))
    end do

    close(unit)
  end subroutine


  subroutine time_is_alpha(filename,sz,times)

    use is_alpha_mod

    character(len=*), intent(in) :: filename
    integer, intent(in) :: sz
    real(dp), intent(out) :: times(8)

    logical :: res(8)
    character(len=:), allocatable :: chars
    integer :: unit
    integer(i64) :: c1, c2, cr

    call system_clock(count_rate=cr)
    allocate(character(len=sz) :: chars)

    open(newunit=unit,file=filename)
    read(unit,'(a)') chars

    call measure(times(1),res(1),is_alpha_int)
    call measure(times(2),res(2),is_alpha_lexical)
    call measure(times(3),res(3),is_alpha_compare)
    call measure(times(4),res(4),is_alpha_index)
    call measure(times(5),res(5),is_alpha_select)
    call measure(times(6),res(6),is_alpha_ctype)
    call measure(times(7),res(7),is_alpha_dynamic_table)
    call measure(times(8),res(8),is_alpha_static_table)

    print *, res

    close(unit)

  contains

    subroutine measure(time,res,proc)
      real(dp), intent(out) :: time
      logical, intent(out) :: res
      interface
        function proc(c) result(res)
          character(len=1), intent(in) :: c
          logical :: res
        end function
      end interface

      integer :: i, reps

      call system_clock(count=c1)
      do reps = 1, 10
        do i = 1, sz
          res = proc(chars(i:i))
        end do
      end do
      call system_clock(count=c2)
      time = (10*sz)/(real(c2-c1,dp)/real(cr,dp))
    end subroutine

  end subroutine

end module

program benchmark

  use time_ascii
  implicit none

  integer :: exp, sz
  character(len=30) :: filename
  real(dp) :: times(12), alpha_times(8)

  character(14) :: funcs(12)

  funcs = [character(14) :: 'is_control','is_printable','is_white',&
  'is_blank','is_graphical','is_punctuation','is_alphanum','is_alpha','is_upper',&
  'is_lower','is_digit','is_hex_digit']

  ! call init_procedures()

  ! write(*,'(A1,8X,*(X,A14))') "#", adjustr(funcs)
  ! do exp = 3, 8
  !   sz = 10**exp
  !   write(filename,'(A,I0,A)') 'chars-',sz,'.txt'
  !   ! print *, filename
  !   call time_character_file(trim(filename),sz,times)
  !   write(*,'(I9,*(X,E14.6))') sz, times
  ! end do

  write(*,*)

  do exp = 3, 8
    sz = 10**exp
    write(filename,'(A,I0,A)') 'chars-',sz,'.txt'
    ! print *, filename
    call time_is_alpha(trim(filename),sz,alpha_times)
    write(*,'(I9,*(X,E14.6))') sz, alpha_times
  end do

end program