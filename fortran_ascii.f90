module fortran_ascii

  implicit none
  private

  !-------------
  ! Public API
  !-------------

  !public :: print_lookup_table

  !
  ! Character validation functions
  !
  public :: is_alpha, is_alphanum
  public :: is_digit, is_hex_digit, is_octal_digit
  public :: is_control, is_white, is_blank
  public :: is_ascii, is_punctuation
  public :: is_graphical, is_printable
  public :: is_lower, is_upper

  !
  ! Character conversion functions
  !
  public :: to_lower, to_upper

  !--------------------------
  ! Ascii control characters
  !--------------------------

  !
  ! Control characters in the ASCII table (see www.asciitable.com).
  !
  character(len=1), public, parameter :: ascii_nul = achar(int(z'00')) !! Null
  character(len=1), public, parameter :: ascii_soh = achar(int(z'01')) !! Start of heading
  character(len=1), public, parameter :: ascii_stx = achar(int(z'02')) !! Start of text
  character(len=1), public, parameter :: ascii_etx = achar(int(z'03')) !! End of text
  character(len=1), public, parameter :: ascii_eot = achar(int(z'04')) !! End of transmission
  character(len=1), public, parameter :: ascii_enq = achar(int(z'05')) !! Enquiry
  character(len=1), public, parameter :: ascii_ack = achar(int(z'06')) !! Acknowledge
  character(len=1), public, parameter :: ascii_bel = achar(int(z'07')) !! Bell
  character(len=1), public, parameter :: ascii_bs  = achar(int(z'08')) !! Backspace
  character(len=1), public, parameter :: ascii_tab = achar(int(z'09')) !! Horizontal tab
  character(len=1), public, parameter :: ascii_lf  = achar(int(z'0A')) !! NL line feed, new line
  character(len=1), public, parameter :: ascii_vt  = achar(int(z'0B')) !! Vertical tab
  character(len=1), public, parameter :: ascii_ff  = achar(int(z'0C')) !! NP form feed, new page
  character(len=1), public, parameter :: ascii_cr  = achar(int(z'0D')) !! Carriage return
  character(len=1), public, parameter :: ascii_so  = achar(int(z'0E')) !! Shift out
  character(len=1), public, parameter :: ascii_si  = achar(int(z'0F')) !! Shift in
  character(len=1), public, parameter :: ascii_dle = achar(int(z'10')) !! Data link escape
  character(len=1), public, parameter :: ascii_dc1 = achar(int(z'11')) !! Device control 1
  character(len=1), public, parameter :: ascii_dc2 = achar(int(z'12')) !! Device control 2
  character(len=1), public, parameter :: ascii_dc3 = achar(int(z'13')) !! Device control 3
  character(len=1), public, parameter :: ascii_dc4 = achar(int(z'14')) !! Device control 4
  character(len=1), public, parameter :: ascii_nak = achar(int(z'15')) !! Negative acknowledge
  character(len=1), public, parameter :: ascii_syn = achar(int(z'16')) !! Synchronous idle
  character(len=1), public, parameter :: ascii_etb = achar(int(z'17')) !! End of transmission block
  character(len=1), public, parameter :: ascii_can = achar(int(z'18')) !! Cancel
  character(len=1), public, parameter :: ascii_em  = achar(int(z'19')) !! End of medium
  character(len=1), public, parameter :: ascii_sub = achar(int(z'1A')) !! Substitute
  character(len=1), public, parameter :: ascii_esc = achar(int(z'1B')) !! Escape
  character(len=1), public, parameter :: ascii_fs  = achar(int(z'1C')) !! File separator
  character(len=1), public, parameter :: ascii_gs  = achar(int(z'1D')) !! Group separator
  character(len=1), public, parameter :: ascii_rs  = achar(int(z'1E')) !! Record separator
  character(len=1), public, parameter :: ascii_us  = achar(int(z'1F')) !! Unit separator
  character(len=1), public, parameter :: ascii_del = achar(int(z'7F')) !! Delete

  !
  ! Same, but as a tiny namespace
  !
  type :: ascii_control_char_t
    character(len=1) :: nul = achar(int(z'00')) !! Null
    character(len=1) :: soh = achar(int(z'01')) !! Start of heading
    character(len=1) :: stx = achar(int(z'02')) !! Start of text
    character(len=1) :: etx = achar(int(z'03')) !! End of text
    character(len=1) :: eot = achar(int(z'04')) !! End of transmission
    character(len=1) :: enq = achar(int(z'05')) !! Enquiry
    character(len=1) :: ack = achar(int(z'06')) !! Acknowledge
    character(len=1) :: bel = achar(int(z'07')) !! Bell
    character(len=1) :: bs  = achar(int(z'08')) !! Backspace
    character(len=1) :: tab = achar(int(z'09')) !! Horizontal tab
    character(len=1) :: lf  = achar(int(z'0A')) !! NL line feed, new line
    character(len=1) :: vt  = achar(int(z'0B')) !! Vertical tab
    character(len=1) :: ff  = achar(int(z'0C')) !! NP form feed, new page
    character(len=1) :: cr  = achar(int(z'0D')) !! Carriage return
    character(len=1) :: so  = achar(int(z'0E')) !! Shift out
    character(len=1) :: si  = achar(int(z'0F')) !! Shift in
    character(len=1) :: dle = achar(int(z'10')) !! Data link escape
    character(len=1) :: dc1 = achar(int(z'11')) !! Device control 1
    character(len=1) :: dc2 = achar(int(z'12')) !! Device control 2
    character(len=1) :: dc3 = achar(int(z'13')) !! Device control 3
    character(len=1) :: dc4 = achar(int(z'14')) !! Device control 4
    character(len=1) :: nak = achar(int(z'15')) !! Negative acknowledge
    character(len=1) :: syn = achar(int(z'16')) !! Synchronous idle
    character(len=1) :: etb = achar(int(z'17')) !! End of transmission block
    character(len=1) :: can = achar(int(z'18')) !! Cancel
    character(len=1) :: em  = achar(int(z'19')) !! End of medium
    character(len=1) :: sub = achar(int(z'1A')) !! Substitute
    character(len=1) :: esc = achar(int(z'1B')) !! Escape
    character(len=1) :: fs  = achar(int(z'1C')) !! File separator
    character(len=1) :: gs  = achar(int(z'1D')) !! Group separator
    character(len=1) :: rs  = achar(int(z'1E')) !! Record separator
    character(len=1) :: us  = achar(int(z'1F')) !! Unit separator
    character(len=1) :: del = achar(int(z'7F')) !! Delete
  end type

  ! A single instance of the ascii control characters (initialized to default values)
  type(ascii_control_char_t), public, parameter :: ascii_control_char = ascii_control_char_t()

  !
  ! Constant character sequences
  !
  public :: fullhex_digits, hex_digits, lowerhex_digits, digits, octal_digits
  public :: letters, uppercase, lowercase, whitespace

  character(len=*), parameter :: fullhex_digits = "0123456789ABCDEFabcdef" !! 0 .. 9A .. Fa .. f
  character(len=*), parameter :: hex_digits = fullhex_digits(1:16) !! 0 .. 9A .. F
  character(len=*), parameter :: lowerhex_digits = "0123456789abcdef" !! 0 .. 9a .. f
  character(len=*), parameter :: digits = hex_digits(1:10) !! 0 .. 9
  character(len=*), parameter :: octal_digits = digits(1:8) !! 0 .. 7
  character(len=*), parameter :: letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" !! A .. Za .. z
  character(len=*), parameter :: uppercase = letters(1:26) !! A .. Z
  character(len=*), parameter :: lowercase = letters(27:) !! a .. z
  character(len=*), parameter :: whitespace = " "//ascii_tab//ascii_vt//ascii_cr//ascii_lf//ascii_ff !! ASCII _whitespace

  !
  ! Character validation routines
  !
  interface

    !> Whether `c` is an ASCII letter (A .. Z, a .. z).
    pure module logical function is_alpha(c)
      character(len=1), intent(in) :: c !! The character to test.
    end function is_alpha

    !> Whether `c` is a letter or a number (0 .. 9, a .. z, A .. Z).
    pure module logical function is_alphanum(c)
      character(len=1), intent(in) :: c !! The character to test.
    end function is_alphanum

    !> Whether or not `c` is in the ASCII character set - i.e. in the
    !  range 0 .. 0x7F.
    pure module logical function is_ascii(c)
      character(len=1), intent(in) :: c !! The character to test.
    end function is_ascii

    !> Whether `c` is a control character.
    pure module logical function is_control(c)
      character(len=1), intent(in) :: c !! The character to test.
    end function is_control

    !> Whether `c` is a digit (0 .. 9).
    pure module logical function is_digit(c)
      character(len=1), intent(in) :: c !! The character to test.
    end function is_digit

    !> Whether `c` is a digit in base 8 (0 .. 7).
    pure module logical function is_octal_digit(c)
      character(len=1), intent(in) :: c !! The character to test.
    end function is_octal_digit

    !> Whether `c` is a digit in base 16 (0 .. 9, A .. F, a .. f).
    pure module logical function is_hex_digit(c)
      character(len=1), intent(in) :: c !! The character to test.
    end function is_hex_digit

    !> Whether or not `c` is a punctuation character. That includes
    !  all ASCII characters which are not control characters, letters, digits, or
    !  whitespace.
    pure module logical function is_punctuation(c)
      character(len=1), intent(in) :: c !! The character to test.
    end function is_punctuation

    !> Whether or not `c` is a printable character other than the
    !  space character.
    pure module logical function is_graphical(c)
      character(len=1), intent(in) :: c !! The character to test.
    end function is_graphical

    !> Whether or not `c` is a printable character - including the
    !  space character.
    pure module logical function is_printable(c)
      character(len=1), intent(in) :: c !! The character to test.
    end function is_printable

    !> Whether `c` is a lowercase ASCII letter (a .. z).
    pure module logical function is_lower(c)
      character(len=1), intent(in) :: c !! The character to test.
    end function is_lower

    !> Whether `c` is an uppercase ASCII letter (A .. Z).
    pure module logical function is_upper(c)
      character(len=1), intent(in) :: c !! The character to test.
    end function is_upper

    !> Whether or not `c` is a whitespace character. That includes the
    !  space, tab, vertical tab, form feed, carriage return, and linefeed
    !  characters.
    pure module logical function is_white(c)
      character(len=1), intent(in) :: c !! The character to test.
    end function is_white

    !> Whether or not `c` is a blank character. That includes the
    !  only the space and tab characters
    pure module logical function is_blank(c)
      character(len=1), intent(in) :: c !! The character to test.
    end function is_blank

    !> Returns the corresponding lowercase letter, if `c` is an uppercase
    !  ASCII character, otherwise `c` itself.
    pure module function to_lower(c) result(t)
      character(len=1), intent(in) :: c !! A character.
      character(len=1) :: t
    end function to_lower

    !> Returns the corresponding uppercase letter, if `c` is a lowercase
    !  ASCII character, otherwise `c` itself.
    pure module function to_upper(c) result(t)
      character(len=1), intent(in) :: c !! A character.
      character(len=1) :: t
    end function to_upper

  end interface

  ! interface
  !   module subroutine print_lookup_table()
  !   end subroutine
  ! end interface

end module