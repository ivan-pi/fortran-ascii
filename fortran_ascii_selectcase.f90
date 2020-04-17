submodule (fortran_ascii) fortran_ascii_selectcase

integer, parameter :: diff_case = iachar('A')-iachar('a')

contains

  !> Checks whether `c` is an ASCII letter (A .. Z, a .. z).
  pure module logical function is_alpha(c)
    character(len=1), intent(in) :: c !! The character to test.
    select case(iachar(c))
      case (65:90,97:122)
        is_alpha = .true.
      case default
        is_alpha = .false.
    end select
  end function

  !> Checks whether `c` is a letter or a number (0 .. 9, a .. z, A .. Z).
  pure module logical function is_alphanum(c)
    character(len=1), intent(in) :: c !! The character to test.
    select case(iachar(c))
      case (48:57,65:90,97:122)
        is_alphanum = .true.
      case default
        is_alphanum = .false.
    end select
  end function

  !> Checks whether or not `c` is in the ASCII character set -
  !  i.e. in the range 0 .. 0x7F.
  pure module logical function is_ascii(c)
    character(len=1), intent(in) :: c !! The character to test.
    select case(iachar(c))
      case (0:127)
        is_ascii = .true.
      case default
        is_ascii = .false.
    end select
  end function

  !>  Checks whether `c` is a control character.
  pure module logical function is_control(c)
    character(len=1), intent(in) :: c !! The character to test.
    select case(iachar(c))
      case (0:31,127)
        is_control = .true.
      case default
        is_control = .false.
    end select
  end function

  !>  Checks whether `c` is a digit (0 .. 9).
  pure module logical function is_digit(c)
    character(len=1), intent(in) :: c !! The character to test.
    select case(iachar(c))
      case (48:57)
        is_digit = .true.
      case default
        is_digit = .false.
    end select
  end function

  !>  Checks whether `c` is a digit in base 8 (0 .. 7).
  pure module logical function is_octal_digit(c)
    character(len=1), intent(in) :: c !! The character to test.
    select case(iachar(c))
      case (48:55)
        is_octal_digit = .true.
      case default
        is_octal_digit = .false.
    end select
  end function

  !>  Checks whether `c` is a digit in base 16 (0 .. 9, A .. F, a .. f).
  pure module logical function is_hex_digit(c)
    character(len=1), intent(in) :: c !! The character to test.
    select case(iachar(c))
      case (48:57,65:70,97:102)
        is_hex_digit = .true.
      case default
        is_hex_digit = .false.
    end select
  end function

  !>  Checks whether or not `c` is a punctuation character. That includes
  !   all ASCII characters which are not control characters, letters,
  !   digits, or whitespace.
  pure module logical function is_punctuation(c)
    character(len=1), intent(in) :: c !! The character to test.
    select case(iachar(c))
      case (33:47,58:64,91:96,123:126)
        is_punctuation = .true.
      case default
        is_punctuation = .false.
    end select
  end function

  !>  Checks whether or not `c` is a printable character other than the
  !   space character.
  pure module logical function is_graphical(c)
    character(len=1), intent(in) :: c !! The character to test.
    select case(iachar(c))
      case (33:126)
        is_graphical = .true.
      case default
        is_graphical = .false.
    end select
  end function

  !>  Checks whether or not `c` is a printable character - including the
  !   space character.
  pure module logical function is_printable(c)
    character(len=1), intent(in) :: c !! The character to test.
    select case(iachar(c))
      case (32:126)
        is_printable = .true.
      case default
        is_printable = .false.
    end select
  end function

  !>  Checks whether `c` is a lowercase ASCII letter (a .. z).
  pure module logical function is_lower(c)
    character(len=1), intent(in) :: c !! The character to test.
    select case(iachar(c))
      case (97:122)
        is_lower = .true.
      case default
        is_lower = .false.
    end select
  end function

  !>  Checks whether `c` is an uppercase ASCII letter (A .. Z).
  pure module logical function is_upper(c)
    character(len=1), intent(in) :: c !! The character to test.
    select case(iachar(c))
      case (65:90)
        is_upper = .true.
      case default
        is_upper = .false.
    end select
  end function

  !>  Checks whether or not `c` is a whitespace character. That includes the
  !   space, tab, vertical tab, form feed, carriage return, and linefeed
  !   characters.
  pure module logical function is_white(c)
    character(len=1), intent(in) :: c !! The character to test.
    select case(iachar(c))
      case (9:13,32)
        is_white = .true.
      case default
        is_white = .false.
    end select
  end function

  !>  Checks whether or not `c` is a blank character. That includes 
  !   the space and tab characters
  pure module logical function is_blank(c)
    character(len=1), intent(in) :: c !! The character to test.        
    select case(iachar(c))
      case (9,32)
        is_blank = .true.
      case default
        is_blank = .false.
    end select
  end function

  !>  Returns the corresponding lowercase letter, if `c` is an uppercase
  !   ASCII character, otherwise `c` itself.
  pure module function to_lower(c) result(t)
    character(len=1), intent(in) :: c !! A character.
    character(len=1) :: t
    integer :: ic
    ic = iachar(c)
    select case(ic)
      case (65:90)
        t = achar(ic - diff_case)
      case default
        t = c
    end select  
  end function

  !>  Returns the corresponding uppercase letter, if `c` is a lowercase
  !   ASCII character, otherwise `c` itself.
  pure module function to_upper(c) result(t)
    character(len=1), intent(in) :: c !! A character.
    character(len=1) :: t
    integer :: ic
    ic = iachar(c)
    select case(ic)
      case (97:122)
        t = achar(ic + diff_case)
      case default
        t = c
    end select  
  end function

end submodule