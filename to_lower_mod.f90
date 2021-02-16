module to_lower_mod

  implicit none

  public :: to_lower_index

contains

  pure function to_lower_index(c) result(res)
    character(len=1), intent(in) :: c
    character(len=1) :: res

    integer :: i
    character(len=26), parameter :: &
      upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ", &
      lower = "abcdefghijklmnopqrstuvwxyz"

    res = c
    i = index(upper, c)
    if (i > 0) res = lower(i:i)
  end function

end module