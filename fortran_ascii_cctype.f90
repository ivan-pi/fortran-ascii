submodule (fortran_ascii) fortran_ascii_cctype

use iso_c_binding, only: c_bool, c_int, c_char
use cctype

contains

  !> Checks whether `c` is an ASCII letter (A .. Z, a .. z).
  pure module logical function is_alpha(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_alpha = isalpha(iachar(c,c_int)) > 0
  end function

  !> Checks whether `c` is a letter or a number (0 .. 9, a .. z, A .. Z).
  pure module logical function is_alphanum(c)
      character(len=1), intent(in) :: c !! The character to test.
      is_alphanum = isalnum(iachar(c,c_int)) > 0
  end function

  !> Checks whether or not `c` is in the ASCII character set -
  !  i.e. in the range 0 .. 0x7F.
  pure module logical function is_ascii(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_ascii = iachar(c) < 128
  end function

  !>  Checks whether `c` is a control character.
  pure module logical function is_control(c)
    character(len=1), intent(in) :: c !! The character to test.
    integer :: ic
    ic = iachar(c)
    is_control = iscntrl(iachar(c,c_int)) > 0
  end function

  !>  Checks whether `c` is a digit (0 .. 9).
  pure module logical function is_digit(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_digit = isdigit(iachar(c,c_int)) > 0
  end function

  !>  Checks whether `c` is a digit in base 8 (0 .. 7).
  pure module logical function is_octal_digit(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_octal_digit = (c >= '0') .and. (c <= '7');
  end function

  !>  Checks whether `c` is a digit in base 16 (0 .. 9, A .. F, a .. f).
  pure module logical function is_hex_digit(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_hex_digit = isxdigit(iachar(c,c_int)) > 0
  end function

  !>  Checks whether or not `c` is a punctuation character. That includes
  !   all ASCII characters which are not control characters, letters,
  !   digits, or whitespace.
  pure module logical function is_punctuation(c)
    character(len=1), intent(in) :: c !! The character to test.
    integer :: ic
    ic = iachar(c) !       '~'                 '!'
    is_punctuation = ispunct(iachar(c,c_int)) > 0
  end function

  !>  Checks whether or not `c` is a printable character other than the
  !   space character.
  pure module logical function is_graphical(c)
    character(len=1), intent(in) :: c !! The character to test.
    integer :: ic
    ic = iachar(c) !  '!'                     '~'
    is_graphical = isgraph(iachar(c,c_int)) > 0
  end function

  !>  Checks whether or not `c` is a printable character - including the
  !   space character.
  pure module logical function is_printable(c)
    character(len=1), intent(in) :: c !! The character to test.
    integer :: ic
    ic = iachar(c)                    ! '~'
    is_printable = isprint(iachar(c,c_int)) > 0
  end function

  !>  Checks whether `c` is a lowercase ASCII letter (a .. z).
  pure module logical function is_lower(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_lower = islower(iachar(c,c_int)) > 0
  end function

  !>  Checks whether `c` is an uppercase ASCII letter (A .. Z).
  pure module logical function is_upper(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_upper = isupper(iachar(c,c_int)) > 0
  end function

  !>  Checks whether or not `c` is a whitespace character. That includes the
  !   space, tab, vertical tab, form feed, carriage return, and linefeed
  !   characters.
  pure module logical function is_white(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_white = isspace(iachar(c,c_int)) > 0
  end function

  !>  Checks whether or not `c` is a blank character. That includes 
  !   the space and tab characters
  pure module logical function is_blank(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_blank = (c == ' ') .or. (iachar(c) == 9);
  end function

  !>  Returns the corresponding lowercase letter, if `c` is an uppercase
  !   ASCII character, otherwise `c` itself.
  pure module function to_lower(c) result(t)
    character(len=1), intent(in) :: c !! A character.
    character(len=1) :: t
    t = achar(tolower(iachar(c,c_int)))
  end function

  !>  Returns the corresponding uppercase letter, if `c` is a lowercase
  !   ASCII character, otherwise `c` itself.
  pure module function to_upper(c) result(t)
    character(len=1), intent(in) :: c !! A character.
    character(len=1) :: t
    t = achar(toupper(iachar(c,c_int)))
  end function

end submodule