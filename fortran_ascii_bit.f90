submodule (fortran_ascii) fortran_ascii_bit
use iso_fortran_env, only: i16 => int16, i8 => int8

!
! Integer masks
!
integer(i16), parameter :: m_upper       = shiftl(1_i16,0)
integer(i16), parameter :: m_lower       = shiftl(1_i16,1)
integer(i16), parameter :: m_alpha       = shiftl(1_i16,2)
integer(i16), parameter :: m_digit       = shiftl(1_i16,3)
integer(i16), parameter :: m_hex_digit   = shiftl(1_i16,4)
integer(i16), parameter :: m_octal_digit = shiftl(1_i16,5)
integer(i16), parameter :: m_space       = shiftl(1_i16,6)
integer(i16), parameter :: m_printable   = shiftl(1_i16,7)
integer(i16), parameter :: m_graphical   = ior(ior(m_alpha,m_digit),shiftl(1_i16,10)) ! alnum|punct
integer(i16), parameter :: m_control     = shiftl(1_i16,9)
integer(i16), parameter :: m_punctuation = shiftl(1_i16,10)
integer(i16), parameter :: m_alphanum    = ior(m_alpha,m_digit)
integer(i16), parameter :: m_blank       = shiftl(1_i16,12)

! char(kind=1,len=1) is 1 byte (with gfortran)
integer(i16), parameter :: table(-128:127) = [ integer(i16) ::&
      ! The first 128 entries are all 0.
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,&
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,&
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,&
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,&
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,&
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,&
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,&
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,&
      m_control,& ! null 
      m_control,& ! ^A 
      m_control,& ! ^B 
      m_control,& ! ^C 
      m_control,& ! ^D 
      m_control,& ! ^E 
      m_control,& ! ^F 
      m_control,& ! ^G 
      m_control,& ! ^H 
      ior(ior(m_space,m_control),m_blank),& ! tab 
      ior(m_space,m_control),& ! LF 
      ior(m_space,m_control),& ! ^K 
      ior(m_space,m_control),& ! FF 
      ior(m_space,m_control),& ! ^M 
      m_control,& ! ^N 
      m_control,& ! ^O 
      m_control,& ! ^P 
      m_control,& ! ^Q 
      m_control,& ! ^R 
      m_control,& ! ^S 
      m_control,& ! ^T 
      m_control,& ! ^U 
      m_control,& ! ^V 
      m_control,& ! ^W 
      m_control,& ! ^X 
      m_control,& ! ^Y 
      m_control,& ! ^Z 
      m_control,& ! esc 
      m_control,& ! ^\ 
      m_control,& ! ^] 
      m_control,& ! ^^ 
      m_control,& ! ^_ 
      ior(ior(m_space,m_printable),m_blank),& ! space   
      ior(m_punctuation,m_printable),& ! ! 
      ior(m_punctuation,m_printable),& ! " 
      ior(m_punctuation,m_printable),& ! # 
      ior(m_punctuation,m_printable),& ! $ 
      ior(m_punctuation,m_printable),& ! % 
      ior(m_punctuation,m_printable),& ! & 
      ior(m_punctuation,m_printable),& ! ' 
      ior(m_punctuation,m_printable),& ! ( 
      ior(m_punctuation,m_printable),& ! ) 
      ior(m_punctuation,m_printable),& ! * 
      ior(m_punctuation,m_printable),& ! + 
      ior(m_punctuation,m_printable),& ! , 
      ior(m_punctuation,m_printable),& ! - 
      ior(m_punctuation,m_printable),& ! . 
      ior(m_punctuation,m_printable),& ! / 
      ior(ior(ior(m_digit,m_hex_digit),m_printable),m_octal_digit),& ! 0 
      ior(ior(ior(m_digit,m_hex_digit),m_printable),m_octal_digit),& ! 1 
      ior(ior(ior(m_digit,m_hex_digit),m_printable),m_octal_digit),& ! 2 
      ior(ior(ior(m_digit,m_hex_digit),m_printable),m_octal_digit),& ! 3 
      ior(ior(ior(m_digit,m_hex_digit),m_printable),m_octal_digit),& ! 4 
      ior(ior(ior(m_digit,m_hex_digit),m_printable),m_octal_digit),& ! 5 
      ior(ior(ior(m_digit,m_hex_digit),m_printable),m_octal_digit),& ! 6 
      ior(ior(ior(m_digit,m_hex_digit),m_printable),m_octal_digit),& ! 7 
      ior(ior(m_digit,m_hex_digit),m_printable),& ! 8 
      ior(ior(m_digit,m_hex_digit),m_printable),& ! 9 
      ior(m_punctuation,m_printable),& ! : 
      ior(m_punctuation,m_printable),& ! ; 
      ior(m_punctuation,m_printable),& ! < 
      ior(m_punctuation,m_printable),& ! = 
      ior(m_punctuation,m_printable),& ! > 
      ior(m_punctuation,m_printable),& ! ? 
      ior(m_punctuation,m_printable),& ! ! 
      ior(ior(ior(m_alpha,m_upper),m_hex_digit),m_printable),& ! A 
      ior(ior(ior(m_alpha,m_upper),m_hex_digit),m_printable),& ! B 
      ior(ior(ior(m_alpha,m_upper),m_hex_digit),m_printable),& ! C 
      ior(ior(ior(m_alpha,m_upper),m_hex_digit),m_printable),& ! D 
      ior(ior(ior(m_alpha,m_upper),m_hex_digit),m_printable),& ! E 
      ior(ior(ior(m_alpha,m_upper),m_hex_digit),m_printable),& ! F 
      ior(ior(m_alpha,m_upper),m_printable),& ! G 
      ior(ior(m_alpha,m_upper),m_printable),& ! H 
      ior(ior(m_alpha,m_upper),m_printable),& ! I 
      ior(ior(m_alpha,m_upper),m_printable),& ! J 
      ior(ior(m_alpha,m_upper),m_printable),& ! K 
      ior(ior(m_alpha,m_upper),m_printable),& ! L 
      ior(ior(m_alpha,m_upper),m_printable),& ! M 
      ior(ior(m_alpha,m_upper),m_printable),& ! N 
      ior(ior(m_alpha,m_upper),m_printable),& ! O 
      ior(ior(m_alpha,m_upper),m_printable),& ! P 
      ior(ior(m_alpha,m_upper),m_printable),& ! Q 
      ior(ior(m_alpha,m_upper),m_printable),& ! R 
      ior(ior(m_alpha,m_upper),m_printable),& ! S 
      ior(ior(m_alpha,m_upper),m_printable),& ! T 
      ior(ior(m_alpha,m_upper),m_printable),& ! U 
      ior(ior(m_alpha,m_upper),m_printable),& ! V 
      ior(ior(m_alpha,m_upper),m_printable),& ! W 
      ior(ior(m_alpha,m_upper),m_printable),& ! X 
      ior(ior(m_alpha,m_upper),m_printable),& ! Y 
      ior(ior(m_alpha,m_upper),m_printable),& ! Z 
      ior(m_punctuation,m_printable),& ! [ 
      ior(m_punctuation,m_printable),& ! \ 
      ior(m_punctuation,m_printable),& ! ] 
      ior(m_punctuation,m_printable),& ! ^ 
      ior(m_punctuation,m_printable),& ! _ 
      ior(m_punctuation,m_printable),& ! ` 
      ior(ior(ior(m_alpha,m_lower),m_hex_digit),m_printable),& ! a 
      ior(ior(ior(m_alpha,m_lower),m_hex_digit),m_printable),& ! b 
      ior(ior(ior(m_alpha,m_lower),m_hex_digit),m_printable),& ! c 
      ior(ior(ior(m_alpha,m_lower),m_hex_digit),m_printable),& ! d 
      ior(ior(ior(m_alpha,m_lower),m_hex_digit),m_printable),& ! e 
      ior(ior(ior(m_alpha,m_lower),m_hex_digit),m_printable),& ! f 
      ior(ior(m_alpha,m_lower),m_printable),& ! g 
      ior(ior(m_alpha,m_lower),m_printable),& ! h 
      ior(ior(m_alpha,m_lower),m_printable),& ! i 
      ior(ior(m_alpha,m_lower),m_printable),& ! j 
      ior(ior(m_alpha,m_lower),m_printable),& ! k 
      ior(ior(m_alpha,m_lower),m_printable),& ! l 
      ior(ior(m_alpha,m_lower),m_printable),& ! m 
      ior(ior(m_alpha,m_lower),m_printable),& ! n 
      ior(ior(m_alpha,m_lower),m_printable),& ! o 
      ior(ior(m_alpha,m_lower),m_printable),& ! p 
      ior(ior(m_alpha,m_lower),m_printable),& ! q 
      ior(ior(m_alpha,m_lower),m_printable),& ! r 
      ior(ior(m_alpha,m_lower),m_printable),& ! s 
      ior(ior(m_alpha,m_lower),m_printable),& ! t 
      ior(ior(m_alpha,m_lower),m_printable),& ! u 
      ior(ior(m_alpha,m_lower),m_printable),& ! v 
      ior(ior(m_alpha,m_lower),m_printable),& ! w 
      ior(ior(m_alpha,m_lower),m_printable),& ! x 
      ior(ior(m_alpha,m_lower),m_printable),& ! y 
      ior(ior(m_alpha,m_lower),m_printable),& ! x 
      ior(m_punctuation,m_printable),& ! { 
      ior(m_punctuation,m_printable),& ! | 
      ior(m_punctuation,m_printable),& ! } 
      ior(m_punctuation,m_printable),& ! ~ 
      m_control] ! del (0x7f)

integer, parameter :: diff_case = iachar('A')-iachar('a')

contains

  module subroutine print_lookup_table()
    integer :: i
    character(len=1) :: c
    do i = 0, 127
        c = achar(i)
        if (is_printable(c)) then
          write(*,'(I3,3X,A,3X,I4,3X,B0.16)') i, c, table(i), table(i)
        end if
    end do
  end subroutine

  !>  Checks whether `c` is an ASCII letter (A .. Z, a .. z).
  pure module logical function is_alpha(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_alpha = btest(table(iachar(c,i8)),2)
  end function

  !>  Checks whether `c` is a letter or a number (0 .. 9, a .. z, A .. Z).
  pure module logical function is_alphanum(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_alphanum = iand(table(iachar(c,i8)),m_alphanum) /= 0
  end function

  !>  Checks whether or not `c` is in the ASCII character set -
  !   i.e. in the range 0 .. 0x7F.
  pure module logical function is_ascii(c)
    character(len=1), intent(in) :: c !! The character to test.
    ! is_ascii = iachar(c) < 128
    is_ascii = iachar(c,i8) >= 0
  end function

  !>  Checks whether `c` is a control character.
  pure module logical function is_control(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_control = iand(table(iachar(c,i8)),m_control) /= 0
  end function

  !>  Checks whether `c` is a digit (0 .. 9).
  pure module logical function is_digit(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_digit = btest(table(iachar(c,i8)),3)
  end function

  !>  Checks whether `c` is a digit in base 8 (0 .. 7).
  pure module logical function is_octal_digit(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_octal_digit = btest(table(iachar(c,i8)),5)
  end function

  !>  Checks whether `c` is a digit in base 16 (0 .. 9, A .. F, a .. f).
  pure module logical function is_hex_digit(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_hex_digit = btest(table(iachar(c,i8)),4)
  end function

  !>  Checks whether or not `c` is a punctuation character. That includes
  !   all ASCII characters which are not control characters, letters,
  !   digits, or whitespace.
  pure module logical function is_punctuation(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_punctuation = btest(table(iachar(c,i8)),10)
  end function

  !>  Checks whether or not `c` is a printable character other than the
  !   space character.
  pure module logical function is_graphical(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_graphical = iand(table(iachar(c,i8)),m_graphical) /= 0
  end function

  !>  Checks whether or not `c` is a printable character - including the
  !   space character.
  pure module logical function is_printable(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_printable = btest(table(iachar(c,i8)),7)
  end function

  !>  Checks whether `c` is a lowercase ASCII letter (a .. z).
  pure module logical function is_lower(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_lower = btest(table(iachar(c,i8)),1)
  end function

  !>  Checks whether `c` is an uppercase ASCII letter (A .. Z).
  pure module logical function is_upper(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_upper = btest(table(iachar(c,i8)),0)
  end function

  !>  Checks whether or not `c` is a whitespace character. That includes the
  !   space, tab, vertical tab, form feed, carriage return, and linefeed
  !   characters.
  pure module logical function is_white(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_white = btest(table(iachar(c,i8)),6)
  end function

  !>  Checks whether or not `c` is a blank character. That includes 
  !   the space and tab characters
  pure module logical function is_blank(c)
    character(len=1), intent(in) :: c !! The character to test.
    is_blank = btest(table(iachar(c,i8)),12)
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