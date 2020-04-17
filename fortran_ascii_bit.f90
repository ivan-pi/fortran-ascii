submodule (fortran_ascii) fortran_ascii_bit

! pos  0 - alphabetical
! pos  1 - digit
! pos  2 - alphabetical or digit
! pos  3 - punctuation
! pos  4 - control
! pos  5 - graphical
! pos  6 - printable
! pos  7 - whitespace
! pos  8 - blank (space or tab)
! pos  9 - lower
! pos 10 - upper
! pos 11 - octal digit
! pos 12 - hex_digit

integer, parameter :: ascii_table(0:127) = [16,16,16,16,16,16,16,16,16,400,144,&
144,144,144,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,448,104,104,&
104,104,104,104,104,104,104,104,104,104,104,104,104,6246,6246,6246,6246,6246,&
6246,6246,6246,4198,4198,104,104,104,104,104,104,104,5221,5221,5221,5221,5221,&
5221,1125,1125,1125,1125,1125,1125,1125,1125,1125,1125,1125,1125,1125,1125,1125,&
1125,1125,1125,1125,1125,104,104,104,104,104,104,4709,4709,4709,4709,4709,4709,&
613,613,613,613,613,613,613,613,613,613,613,613,613,613,613,613,613,613,&
613,613,104,104,104,104,16]

integer, parameter :: diff_case = iachar('A')-iachar('a')

contains

  !>  Checks whether `c` is an ASCII letter (A .. Z, a .. z).
  pure module logical function is_alpha(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_alpha = btest(ascii_table(iachar(c)),0)
  end function

  !>  Checks whether `c` is a letter or a number (0 .. 9, a .. z, A .. Z).
  pure module logical function is_alphanum(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_alphanum = btest(ascii_table(iachar(c)),2)
  end function

  !>  Checks whether or not `c` is in the ASCII character set -
  !   i.e. in the range 0 .. 0x7F.
  pure module logical function is_ascii(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_ascii = iachar(c) < 128
  end function

  !>  Checks whether `c` is a control character.
  pure module logical function is_control(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_control = btest(ascii_table(iachar(c)),4)
  end function

  !>  Checks whether `c` is a digit (0 .. 9).
  pure module logical function is_digit(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_digit = btest(ascii_table(iachar(c)),1)
  end function

  !>  Checks whether `c` is a digit in base 8 (0 .. 7).
  pure module logical function is_octal_digit(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_octal_digit = btest(ascii_table(iachar(c)),11)
  end function

  !>  Checks whether `c` is a digit in base 16 (0 .. 9, A .. F, a .. f).
  pure module logical function is_hex_digit(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_hex_digit = btest(ascii_table(iachar(c)),12)
  end function

  !>  Checks whether or not `c` is a punctuation character. That includes
  !   all ASCII characters which are not control characters, letters,
  !   digits, or whitespace.
  pure module logical function is_punctuation(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_punctuation = btest(ascii_table(iachar(c)),3)
  end function

  !>  Checks whether or not `c` is a printable character other than the
  !   space character.
  pure module logical function is_graphical(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_graphical = btest(ascii_table(iachar(c)),5)
  end function

  !>  Checks whether or not `c` is a printable character - including the
  !   space character.
  pure module logical function is_printable(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_printable = btest(ascii_table(iachar(c)),6)
  end function

  !>  Checks whether `c` is a lowercase ASCII letter (a .. z).
  pure module logical function is_lower(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_lower = btest(ascii_table(iachar(c)),9)
  end function

  !>  Checks whether `c` is an uppercase ASCII letter (A .. Z).
  pure module logical function is_upper(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_upper = btest(ascii_table(iachar(c)),10)
  end function

  !>  Checks whether or not `c` is a whitespace character. That includes the
  !   space, tab, vertical tab, form feed, carriage return, and linefeed
  !   characters.
  pure module logical function is_white(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_white = btest(ascii_table(iachar(c)),7)
  end function

  !>  Checks whether or not `c` is a blank character. That includes 
  !   the space and tab characters
  pure module logical function is_blank(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_blank = btest(ascii_table(iachar(c)),8)
  end function

  !>  Returns the corresponding lowercase letter, if `c` is an uppercase
  !   ASCII character, otherwise `c` itself.
  pure module function to_lower(c) result(t)
    character(len=1), intent(in) :: c !! A character.
    character(len=1) :: t
    t = c
    if (is_upper(t)) t = achar(iachar(t) - diff_case)
  end function

  !>  Returns the corresponding uppercase letter, if `c` is a lowercase
  !   ASCII character, otherwise `c` itself.
  pure module function to_upper(c) result(t)
    character(len=1), intent(in) :: c !! A character.
    character(len=1) :: t
    t = c
    if (is_lower(t)) t = achar(iachar(t) + diff_case)
  end function

end submodule