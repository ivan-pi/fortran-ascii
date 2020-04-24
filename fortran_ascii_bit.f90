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

integer, parameter :: lookup_table(11) = [16,400,144,448,104,6246,4198,5221,1125,4709,613]

contains

  pure logical function test_char(c,p)
    use iso_fortran_env, only: i16 => int16
    character(len=1), intent(in) :: c
    integer, intent(in) :: p
    integer :: ic
    ic = iachar(c)
    select case(ic)
    case(0:8,14:31,127) ! nul, other control codes, del
      ! test_char = btest(lookup_table(1),p)
      test_char = btest(16_i16,p)
    case(9) ! TAB
      ! test_char = btest(lookup_table(2),p)
      test_char = btest(400_i16,p)
    case(10:13) ! LF, VT, FF, CR (whitespace control codes)
      ! test_char = btest(lookup_table(3),p)
      test_char = btest(144_i16,p)
    case(32) ! " "
      ! test_char = btest(lookup_table(4),p)
      test_char = btest(448_i16,p)
    case(33:47,58:64,91:96,123:126) ! !"#$%&'()*+,-./  :;<=>?@ [\]^_` {|}~
      ! test_char = btest(lookup_table(5),p)
      test_char = btest(104_i16,p)
    case(48:55) ! 0 .. 7
      ! test_char = btest(lookup_table(6),p)
      test_char = btest(6246_i16,p)
    case(56:57) ! 8, 9
      ! test_char = btest(lookup_table(7),p)
      test_char = btest(4198_i16,p)
    case(65:70) ! A .. F
      ! test_char = btest(lookup_table(8),p)
      test_char = btest(5221_i16,p)
    case(71:90) ! G .. Z
      ! test_char = btest(lookup_table(9),p)
      test_char = btest(1125_i16,p)
    case(97:102) ! a .. f
      ! test_char = btest(lookup_table(10),p)
      test_char = btest(4709_i16,p)
    case(103:122) ! g .. z
      ! test_char = btest(lookup_table(11),p)
      test_char = btest(613_i16,p)
    case default
      test_char = .false. ! not ascii
    end select
  end function


  !>  Checks whether `c` is an ASCII letter (A .. Z, a .. z).
  pure module logical function is_alpha(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_alpha = btest(ascii_table(iachar(c)),0)
    ! is_alpha = test_char(c,0)
  end function

  !>  Checks whether `c` is a letter or a number (0 .. 9, a .. z, A .. Z).
  pure module logical function is_alphanum(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_alphanum = btest(ascii_table(iachar(c)),2)
    ! is_alphanum = test_char(c,2)
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
    ! is_control = test_char(c,4)
  end function

  !>  Checks whether `c` is a digit (0 .. 9).
  pure module logical function is_digit(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_digit = btest(ascii_table(iachar(c)),1)
    ! is_digit = test_char(c,1)
  end function

  !>  Checks whether `c` is a digit in base 8 (0 .. 7).
  pure module logical function is_octal_digit(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_octal_digit = btest(ascii_table(iachar(c)),11)
    ! is_octal_digit = test_char(c,11)
  end function

  !>  Checks whether `c` is a digit in base 16 (0 .. 9, A .. F, a .. f).
  pure module logical function is_hex_digit(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_hex_digit = btest(ascii_table(iachar(c)),12)
    ! is_hex_digit = test_char(c,12)
  end function

  !>  Checks whether or not `c` is a punctuation character. That includes
  !   all ASCII characters which are not control characters, letters,
  !   digits, or whitespace.
  pure module logical function is_punctuation(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_punctuation = btest(ascii_table(iachar(c)),3)
    ! is_punctuation = test_char(c,3)
  end function

  !>  Checks whether or not `c` is a printable character other than the
  !   space character.
  pure module logical function is_graphical(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_graphical = btest(ascii_table(iachar(c)),5)
    ! is_graphical = test_char(c,5)
  end function

  !>  Checks whether or not `c` is a printable character - including the
  !   space character.
  pure module logical function is_printable(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_printable = btest(ascii_table(iachar(c)),6)
    ! is_printable = test_char(c,6)
  end function

  !>  Checks whether `c` is a lowercase ASCII letter (a .. z).
  pure module logical function is_lower(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_lower = btest(ascii_table(iachar(c)),9)
    ! is_lower = test_char(c,9)
  end function

  !>  Checks whether `c` is an uppercase ASCII letter (A .. Z).
  pure module logical function is_upper(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_upper = btest(ascii_table(iachar(c)),10)
    ! is_upper = test_char(c,10)
  end function

  !>  Checks whether or not `c` is a whitespace character. That includes the
  !   space, tab, vertical tab, form feed, carriage return, and linefeed
  !   characters.
  pure module logical function is_white(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_white = btest(ascii_table(iachar(c)),7)
    ! is_white = test_char(c,7)
  end function

  !>  Checks whether or not `c` is a blank character. That includes 
  !   the space and tab characters
  pure module logical function is_blank(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_blank = btest(ascii_table(iachar(c)),8)
    ! is_blank = test_char(c,8)
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