program kinds

  use iso_fortran_env, only: integer_kinds
  use iso_fortran_env, only: real_kinds
  use iso_fortran_env, only: logical_kinds
  use iso_fortran_env, only: character_kinds

  implicit none

  write(*,*) "{"
  write(*,*) '"integer" : ', array2char(integer_kinds), ','
  write(*,*) '"real" : ', array2char(real_kinds), ','
  write(*,*) '"logical" : ', array2char(logical_kinds), ','
  write(*,*) '"character" : ', array2char(character_kinds)
  write(*,*) "}"

contains

  function i2char(i) result (c)
    integer, intent(in) :: i
    character(:), allocatable :: c
    integer, parameter :: DI = 20
    character(*), parameter :: FI = '(I20)'
    character(DI) :: s
    write(s,FI) i
    c = trim(adjustl(s))
  end function

  function array2char(a) result (c)
    integer, intent(in) :: a(:)
    character(:), allocatable :: c
    integer :: i
    c = '['
    do i = 1, size(a)
        c = c // ' ' // i2char(a(i))
      if (i /= size(a) ) then
        c = c // ','
      end if
    end do
    c = c // ']'
  end function

end program
