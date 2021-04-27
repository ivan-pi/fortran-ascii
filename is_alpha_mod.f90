! Based upon the thread:
!   https://comp.lang.fortran.narkive.com/efGDzquz/important-is-alpha
!
module is_alpha_mod

  use, intrinsic :: iso_fortran_env, only: int8
  implicit none

  public :: is_alpha_int
  public :: is_alpha_raw
  public :: is_alpha_lexical
  public :: is_alpha_compare
  public :: is_alpha_index
  public :: is_alpha_select
  public :: is_alpha_ctype
  public :: is_alpha_dynamic_table
  public :: is_alpha_static_table

contains

  pure function is_alpha_int(c) result(res)
    character(len=1), intent(in) :: c
    logical :: res
    integer :: ic
    ic = iachar(c)
    res = (ic >= 65 .and. ic <= 90) .or. &
          (ic >= 97 .and. ic <= 122)
  end function

  pure function is_alpha_raw(c) result(res)
    character(len=1), intent(in) :: c
    logical :: res
    res = ("A" <= c .and. c <= "Z") .or. ("a" <= c .and. c <= "z")
  end function

  pure function is_alpha_lexical(c) result(res)
    character(len=1), intent(in) :: c
    logical :: res
    res = (lle('A',c) .and. lle(c,'Z')) .or. (lle('a',c) .and. lle(c,'z'))
  end function

  pure function is_alpha_compare(c) result(res)
    character(len=1), intent(in) :: c
    logical :: res
    res = abs(ishft(ibclr(iachar(c),5), 1) -  155) <= 25
  end function

  pure function is_alpha_index(c) result(res)
    character(len=1), intent(in) :: c
    logical :: res
    res = index( &
        string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz', &
        substring = c) > 0
  end function

  pure function is_alpha_select(c) result(res)
    character(len=1), intent(in) :: c
    logical :: res
    select case(iachar(c))
      case (65:90, 97:122)
        res = .true.
      case default
        res = .false.
    end select
  end function

  pure function is_alpha_ctype(c) result(res)
    use, intrinsic :: iso_c_binding, only: c_int
    character(len=1), intent(in) :: c
    logical :: res
    interface
      pure integer(c_int) function isalpha(c) bind(c,name='isalpha')
        import c_int
        integer(c_int), intent(in), value :: c
      end function
    end interface
    res = isalpha(iachar(c,c_int)) > 0
  end function

  function is_alpha_dynamic_table(c) result(res)
    character(len=1), intent(in) :: c
    logical :: res

    logical, save :: in_alphabet(-128:127), not_setup
    data in_alphabet /256*.false./, not_setup /.true./

    if (not_setup) then
      block
        integer :: i
        associate(alphabet => &
          'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz')
          do i = 1, len(alphabet)
            in_alphabet(iachar(alphabet(i:i))) = .true.
          end do
        end associate
      end block
      not_setup = .false.
    end if

    res = in_alphabet(iachar(c,int8))
  end function

  pure function is_alpha_static_table(c) result(res)
    ! implicit integer(i)
    character(len=1), intent(in) :: c
    logical :: res

    logical, parameter :: in_alphabet(-128:127) = [ &
      .false., .false., .false., .false., .false., .false., .false., .false., &
      .false., .false., .false., .false., .false., .false., .false., .false., &
      .false., .false., .false., .false., .false., .false., .false., .false., &
      .false., .false., .false., .false., .false., .false., .false., .false., &
      .false., .false., .false., .false., .false., .false., .false., .false., &
      .false., .false., .false., .false., .false., .false., .false., .false., &
      .false., .false., .false., .false., .false., .false., .false., .false., &
      .false., .false., .false., .false., .false., .false., .false., .false., &
      .false., .false., .false., .false., .false., .false., .false., .false., &
      .false., .false., .false., .false., .false., .false., .false., .false., &
      .false., .false., .false., .false., .false., .false., .false., .false., &
      .false., .false., .false., .false., .false., .false., .false., .false., &
      .false., .false., .false., .false., .false., .false., .false., .false., &
      .false., .false., .false., .false., .false., .false., .false., .false., &
      .false., .false., .false., .false., .false., .false., .false., .false., &
      .false., .false., .false., .false., .false., .false., .false., .false., &
      .false., .false., .false., .false., .false., .false., .false., .false., &
      .false., .false., .false., .false., .false., .false., .false., .false., &
      .false., .false., .false., .false., .false., .false., .false., .false., &
      .false., .false., .false., .false., .false., .false., .false., .false., &
      .false., .false., .false., .false., .false., .false., .false., .false., &
      .false., .false., .false., .false., .false., .false., .false., .false., &
      .false., .false., .false., .false., .false., .false., .false., .false., &
      .false., .false., .false., .false., .false., .false., .false., .false., &
      .false.,  .true.,  .true.,  .true.,  .true.,  .true.,  .true.,  .true., &
       .true.,  .true.,  .true.,  .true.,  .true.,  .true.,  .true.,  .true., &
       .true.,  .true.,  .true.,  .true.,  .true.,  .true.,  .true.,  .true., &
       .true.,  .true.,  .true., .false., .false., .false., .false., .false., &
      .false.,  .true.,  .true.,  .true.,  .true.,  .true.,  .true.,  .true., &
       .true.,  .true.,  .true.,  .true.,  .true.,  .true.,  .true.,  .true., &
       .true.,  .true.,  .true.,  .true.,  .true.,  .true.,  .true.,  .true., &
       .true.,  .true.,  .true., .false., .false., .false., .false., .false.  &
    ]

    res = in_alphabet(iachar(c,int8))
  end function

end module