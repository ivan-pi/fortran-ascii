submodule (fortran_ascii) fortran_ascii_bit

use iso_fortran_env, only: i16 => int16, i8 => int8

! pos  0 - 
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

type :: cbase
  integer(i16) :: upper       = shiftl(1_i16,0)
  integer(i16) :: lower       = shiftl(1_i16,1)
  integer(i16) :: alpha       = shiftl(1_i16,2)
  integer(i16) :: digit       = shiftl(1_i16,3)
  integer(i16) :: hex_digit   = shiftl(1_i16,4)
  integer(i16) :: octal_digit = shiftl(1_i16,5)
  integer(i16) :: space       = shiftl(1_i16,6)
  integer(i16) :: printable   = shiftl(1_i16,7)
  integer(i16) :: graphical   = ior(ior(shiftl(1_i16,2),shiftl(1_i16,3)),shiftl(1_i16,10)) ! alnum|punct
  integer(i16) :: control     = shiftl(1_i16,9)
  integer(i16) :: punctuation = shiftl(1_i16,10)
  integer(i16) :: alphanum    = ior(shiftl(1_i16,2),shiftl(1_i16,3)) ! alpha|digit
  integer(i16) :: blank       = shiftl(1_i16,12)
end type

type(cbase), parameter :: cb = cbase()

! char(kind=1,len=1) is 1 byte (with gfortran)
integer(i16), parameter :: table(0:255) = [ integer(i16) ::&
      cb%control,& ! null 
      cb%control,& ! ^A 
      cb%control,& ! ^B 
      cb%control,& ! ^C 
      cb%control,& ! ^D 
      cb%control,& ! ^E 
      cb%control,& ! ^F 
      cb%control,& ! ^G 
      cb%control,& ! ^H 
      ior(ior(cb%space,cb%control),cb%blank),& ! tab 
      ior(cb%space,cb%control),& ! LF 
      ior(cb%space,cb%control),& ! ^K 
      ior(cb%space,cb%control),& ! FF 
      ior(cb%space,cb%control),& ! ^M 
      cb%control,& ! ^N 
      cb%control,& ! ^O 
      cb%control,& ! ^P 
      cb%control,& ! ^Q 
      cb%control,& ! ^R 
      cb%control,& ! ^S 
      cb%control,& ! ^T 
      cb%control,& ! ^U 
      cb%control,& ! ^V 
      cb%control,& ! ^W 
      cb%control,& ! ^X 
      cb%control,& ! ^Y 
      cb%control,& ! ^Z 
      cb%control,& ! esc 
      cb%control,& ! ^\ 
      cb%control,& ! ^] 
      cb%control,& ! ^^ 
      cb%control,& ! ^_ 
      ior(ior(cb%space,cb%printable),cb%blank),& !   
      ior(cb%punctuation,cb%printable),& ! ! 
      ior(cb%punctuation,cb%printable),& ! " 
      ior(cb%punctuation,cb%printable),& ! # 
      ior(cb%punctuation,cb%printable),& ! $ 
      ior(cb%punctuation,cb%printable),& ! % 
      ior(cb%punctuation,cb%printable),& ! & 
      ior(cb%punctuation,cb%printable),& ! ' 
      ior(cb%punctuation,cb%printable),& ! ( 
      ior(cb%punctuation,cb%printable),& ! ) 
      ior(cb%punctuation,cb%printable),& ! * 
      ior(cb%punctuation,cb%printable),& ! + 
      ior(cb%punctuation,cb%printable),& ! , 
      ior(cb%punctuation,cb%printable),& ! - 
      ior(cb%punctuation,cb%printable),& ! . 
      ior(cb%punctuation,cb%printable),& ! / 
      ior(ior(cb%digit,cb%hex_digit),cb%printable),& ! 0 
      ior(ior(cb%digit,cb%hex_digit),cb%printable),& ! 1 
      ior(ior(cb%digit,cb%hex_digit),cb%printable),& ! 2 
      ior(ior(cb%digit,cb%hex_digit),cb%printable),& ! 3 
      ior(ior(cb%digit,cb%hex_digit),cb%printable),& ! 4 
      ior(ior(cb%digit,cb%hex_digit),cb%printable),& ! 5 
      ior(ior(cb%digit,cb%hex_digit),cb%printable),& ! 6 
      ior(ior(cb%digit,cb%hex_digit),cb%printable),& ! 7 
      ior(ior(cb%digit,cb%hex_digit),cb%printable),& ! 8 
      ior(ior(cb%digit,cb%hex_digit),cb%printable),& ! 9 
      ior(cb%punctuation,cb%printable),& ! : 
      ior(cb%punctuation,cb%printable),& ! ; 
      ior(cb%punctuation,cb%printable),& ! < 
      ior(cb%punctuation,cb%printable),& ! = 
      ior(cb%punctuation,cb%printable),& ! > 
      ior(cb%punctuation,cb%printable),& ! ? 
      ior(cb%punctuation,cb%printable),& ! ! 
      ior(ior(ior(cb%alpha,cb%upper),cb%hex_digit),cb%printable),& ! A 
      ior(ior(ior(cb%alpha,cb%upper),cb%hex_digit),cb%printable),& ! B 
      ior(ior(ior(cb%alpha,cb%upper),cb%hex_digit),cb%printable),& ! C 
      ior(ior(ior(cb%alpha,cb%upper),cb%hex_digit),cb%printable),& ! D 
      ior(ior(ior(cb%alpha,cb%upper),cb%hex_digit),cb%printable),& ! E 
      ior(ior(ior(cb%alpha,cb%upper),cb%hex_digit),cb%printable),& ! F 
      ior(ior(cb%alpha,cb%upper),cb%printable),& ! G 
      ior(ior(cb%alpha,cb%upper),cb%printable),& ! H 
      ior(ior(cb%alpha,cb%upper),cb%printable),& ! I 
      ior(ior(cb%alpha,cb%upper),cb%printable),& ! J 
      ior(ior(cb%alpha,cb%upper),cb%printable),& ! K 
      ior(ior(cb%alpha,cb%upper),cb%printable),& ! L 
      ior(ior(cb%alpha,cb%upper),cb%printable),& ! M 
      ior(ior(cb%alpha,cb%upper),cb%printable),& ! N 
      ior(ior(cb%alpha,cb%upper),cb%printable),& ! O 
      ior(ior(cb%alpha,cb%upper),cb%printable),& ! P 
      ior(ior(cb%alpha,cb%upper),cb%printable),& ! Q 
      ior(ior(cb%alpha,cb%upper),cb%printable),& ! R 
      ior(ior(cb%alpha,cb%upper),cb%printable),& ! S 
      ior(ior(cb%alpha,cb%upper),cb%printable),& ! T 
      ior(ior(cb%alpha,cb%upper),cb%printable),& ! U 
      ior(ior(cb%alpha,cb%upper),cb%printable),& ! V 
      ior(ior(cb%alpha,cb%upper),cb%printable),& ! W 
      ior(ior(cb%alpha,cb%upper),cb%printable),& ! X 
      ior(ior(cb%alpha,cb%upper),cb%printable),& ! Y 
      ior(ior(cb%alpha,cb%upper),cb%printable),& ! Z 
      ior(cb%punctuation,cb%printable),& ! [ 
      ior(cb%punctuation,cb%printable),& ! \ 
      ior(cb%punctuation,cb%printable),& ! ] 
      ior(cb%punctuation,cb%printable),& ! ^ 
      ior(cb%punctuation,cb%printable),& ! _ 
      ior(cb%punctuation,cb%printable),& ! ` 
      ior(ior(ior(cb%alpha,cb%lower),cb%hex_digit),cb%printable),& ! a 
      ior(ior(ior(cb%alpha,cb%lower),cb%hex_digit),cb%printable),& ! b 
      ior(ior(ior(cb%alpha,cb%lower),cb%hex_digit),cb%printable),& ! c 
      ior(ior(ior(cb%alpha,cb%lower),cb%hex_digit),cb%printable),& ! d 
      ior(ior(ior(cb%alpha,cb%lower),cb%hex_digit),cb%printable),& ! e 
      ior(ior(ior(cb%alpha,cb%lower),cb%hex_digit),cb%printable),& ! f 
      ior(ior(cb%alpha,cb%lower),cb%printable),& ! g 
      ior(ior(cb%alpha,cb%lower),cb%printable),& ! h 
      ior(ior(cb%alpha,cb%lower),cb%printable),& ! i 
      ior(ior(cb%alpha,cb%lower),cb%printable),& ! j 
      ior(ior(cb%alpha,cb%lower),cb%printable),& ! k 
      ior(ior(cb%alpha,cb%lower),cb%printable),& ! l 
      ior(ior(cb%alpha,cb%lower),cb%printable),& ! m 
      ior(ior(cb%alpha,cb%lower),cb%printable),& ! n 
      ior(ior(cb%alpha,cb%lower),cb%printable),& ! o 
      ior(ior(cb%alpha,cb%lower),cb%printable),& ! p 
      ior(ior(cb%alpha,cb%lower),cb%printable),& ! q 
      ior(ior(cb%alpha,cb%lower),cb%printable),& ! r 
      ior(ior(cb%alpha,cb%lower),cb%printable),& ! s 
      ior(ior(cb%alpha,cb%lower),cb%printable),& ! t 
      ior(ior(cb%alpha,cb%lower),cb%printable),& ! u 
      ior(ior(cb%alpha,cb%lower),cb%printable),& ! v 
      ior(ior(cb%alpha,cb%lower),cb%printable),& ! w 
      ior(ior(cb%alpha,cb%lower),cb%printable),& ! x 
      ior(ior(cb%alpha,cb%lower),cb%printable),& ! y 
      ior(ior(cb%alpha,cb%lower),cb%printable),& ! x 
      ior(cb%punctuation,cb%printable),& ! { 
      ior(cb%punctuation,cb%printable),& ! | 
      ior(cb%punctuation,cb%printable),& ! } 
      ior(cb%punctuation,cb%printable),& ! ~ 
      cb%control,& ! del (0x7f)
      ! The next 128 entries are all 0.
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,&
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,&
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,&
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,&
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,&
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,&
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,&
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]


integer, parameter :: ascii_table(0:127) = [8,8,8,8,8,8,8,8,8,200,72,72,72,72,8,8,8,8,8,8,8,8,8,8,8,&
8,8,8,8,8,8,8,224,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,1586,1586,1586,1586,1586,1586,1586,&
1586,1074,1074,52,52,52,52,52,52,52,1329,1329,1329,1329,1329,1329,305,305,305,305,305,305,305,305,&
305,305,305,305,305,305,305,305,305,305,305,305,52,52,52,52,52,52,1073,1073,1073,1073,1073,1073,49,&
49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,52,52,52,52,8]

integer, parameter :: diff_case = iachar('A')-iachar('a')

integer, parameter :: lookup_table(11) = [8,200,72,224,52,1586,1074,1329,305,1073,49]

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
      test_char = btest(8_i16,p)
    case(9) ! TAB
      ! test_char = btest(lookup_table(2),p)
      test_char = btest(200_i16,p)
    case(10:13) ! LF, VT, FF, CR (whitespace control codes)
      ! test_char = btest(lookup_table(3),p)
      test_char = btest(72_i16,p)
    case(32) ! " "
      ! test_char = btest(lookup_table(4),p)
      test_char = btest(224_i16,p)
    case(33:47,58:64,91:96,123:126) ! !"#$%&'()*+,-./  :;<=>?@ [\]^_` {|}~
      ! test_char = btest(lookup_table(5),p)
      test_char = btest(52_i16,p)
    case(48:55) ! 0 .. 7
      ! test_char = btest(lookup_table(6),p)
      test_char = btest(1586_i16,p)
    case(56:57) ! 8, 9
      ! test_char = btest(lookup_table(7),p)
      test_char = btest(1074_i16,p)
    case(65:70) ! A .. F
      ! test_char = btest(lookup_table(8),p)
      test_char = btest(1329_i16,p)
    case(71:90) ! G .. Z
      ! test_char = btest(lookup_table(9),p)
      test_char = btest(305_i16,p)
    case(97:102) ! a .. f
      ! test_char = btest(lookup_table(10),p)
      test_char = btest(1073_i16,p)
    case(103:122) ! g .. z
      ! test_char = btest(lookup_table(11),p)
      test_char = btest(49_i16,p)
    case default
      test_char = .false. ! not ascii
    end select
  end function


  !>  Checks whether `c` is an ASCII letter (A .. Z, a .. z).
  pure module logical function is_alpha(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_alpha = btest(table(iachar(c,i8)),2)
    ! is_alpha = btest(ascii_table(iachar(c)),0)
    ! is_alpha = test_char(c,0)
  end function

  !>  Checks whether `c` is a letter or a number (0 .. 9, a .. z, A .. Z).
  pure module logical function is_alphanum(c)
    character(len=1), intent(in) :: c !! The character to test.
    integer(i16) :: m
    ! is_alphanum = btest(table(iachar(c)),2) .or. btest(table(iachar(c)),3)
    ! is_alphanum = (iand(table(iachar(c)),cb%alphanum)) == cb%alphanum
    m = iand(table(iachar(c,i8)),cb%alphanum)
    ! is_alphanum = m == cb%alpha .or. m == cb%digit
    is_alphanum = m > 0
    ! is_alphanum = btest(ascii_table(iachar(c)),0) .and. btest(ascii_table(iachar(c)),1)
    ! is_alphanum = test_char(c,0) .and. test_char(c,1)
  end function

  !>  Checks whether or not `c` is in the ASCII character set -
  !   i.e. in the range 0 .. 0x7F.
  pure module logical function is_ascii(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_ascii = iachar(c,i8) < 128
  end function

  !>  Checks whether `c` is a control character.
  pure module logical function is_control(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_control = btest(table(iachar(c,i8)),9)
    ! is_control = btest(ascii_table(iachar(c)),4)
    ! is_control = test_char(c,3)
  end function

  !>  Checks whether `c` is a digit (0 .. 9).
  pure module logical function is_digit(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_digit = btest(table(iachar(c,i8)),3)
    ! is_digit = btest(ascii_table(iachar(c)),1)
    ! is_digit = test_char(c,1)
  end function

  !>  Checks whether `c` is a digit in base 8 (0 .. 7).
  pure module logical function is_octal_digit(c)
    character(len=1), intent(in) :: c !! The character to test.
    ! is_octal_digit = btest(ascii_table(iachar(c)),9)
    is_octal_digit = test_char(c,9)
  end function

  !>  Checks whether `c` is a digit in base 16 (0 .. 9, A .. F, a .. f).
  pure module logical function is_hex_digit(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_hex_digit = btest(table(iachar(c,i8)),4)
    ! is_hex_digit = btest(ascii_table(iachar(c)),10)
    ! is_hex_digit = test_char(c,10)
  end function

  !>  Checks whether or not `c` is a punctuation character. That includes
  !   all ASCII characters which are not control characters, letters,
  !   digits, or whitespace.
  pure module logical function is_punctuation(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_punctuation = btest(table(iachar(c,i8)),10)
    ! is_punctuation = btest(ascii_table(iachar(c)),2)
    ! is_punctuation = test_char(c,2)
  end function

  !>  Checks whether or not `c` is a printable character other than the
  !   space character.
  pure module logical function is_graphical(c)
    character(len=1), intent(in) :: c !! The character to test.
    integer(i16) :: m
    integer :: ic
    ! ic = iachar(c) !  '!'                     '~'
    ! is_graphical = (z'21' <= ic) .and. (ic <= z'7E')
    m = iand(table(iachar(c,i8)),cb%graphical)
    is_graphical = m /= 0!== cb%alpha .or. m == cb%digit .or. m == cb%punctuation
    ! is_graphical = btest(table(iachar(c)),8)
    ! is_graphical = btest(ascii_table(iachar(c)),4)
    ! is_graphical = test_char(c,4)
  end function

  !>  Checks whether or not `c` is a printable character - including the
  !   space character.
  pure module logical function is_printable(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_printable = btest(table(iachar(c,i8)),7)
    ! is_printable = btest(ascii_table(iachar(c)),5)
    ! is_printable = test_char(c,5)
  end function

  !>  Checks whether `c` is a lowercase ASCII letter (a .. z).
  pure module logical function is_lower(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_lower = btest(table(iachar(c,i8)),1)
    ! is_lower = .not. btest(ascii_table(iachar(c)),8)
    ! is_lower = .not. test_char(c,8)
  end function

  !>  Checks whether `c` is an uppercase ASCII letter (A .. Z).
  pure module logical function is_upper(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_upper = btest(table(iachar(c,i8)),0)
    ! is_upper = btest(ascii_table(iachar(c)),8)
    ! is_upper = test_char(c,8)
  end function

  !>  Checks whether or not `c` is a whitespace character. That includes the
  !   space, tab, vertical tab, form feed, carriage return, and linefeed
  !   characters.
  pure module logical function is_white(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_white = btest(table(iachar(c,i8)),6)
    ! is_white = btest(ascii_table(iachar(c)),6)
    ! is_white = test_char(c,6)
  end function

  !>  Checks whether or not `c` is a blank character. That includes 
  !   the space and tab characters
  pure module logical function is_blank(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_blank = btest(table(iachar(c,i8)),12)
    ! is_blank = btest(ascii_table(iachar(c)),7)
    ! is_blank = test_char(c,7)
  end function

  !>  Returns the corresponding lowercase letter, if `c` is an uppercase
  !   ASCII character, otherwise `c` itself.
  pure module function to_lower(c) result(t)
    character(len=1), intent(in) :: c !! A character.
    character(len=1) :: t
    t = c
    if (is_upper(t)) t = achar(iachar(t,i8) - diff_case)
  end function

  !>  Returns the corresponding uppercase letter, if `c` is a lowercase
  !   ASCII character, otherwise `c` itself.
  pure module function to_upper(c) result(t)
    character(len=1), intent(in) :: c !! A character.
    character(len=1) :: t
    t = c
    if (is_lower(t)) t = achar(iachar(t,i8) + diff_case)
  end function

end submodule