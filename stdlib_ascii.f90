module stdlib_ascii

    implicit none
    private

    public :: is_alpha
    public :: is_alphanum
    public :: is_lower
    public :: is_upper
    public :: is_digit
    public :: is_octal_digit
    public :: is_hex_digit
    public :: is_control
    public :: is_white
    public :: is_blank
    public :: is_ascii
    public :: is_punctuation
    public :: is_graphical
    public :: is_printable
    public :: to_lower
    public :: to_upper

    integer, parameter :: ascii = selected_char_kind('ASCII')
    integer, parameter :: cd = selected_char_kind('DEFAULT')

    character(len=*), public, parameter :: fullhex_digits = "0123456789ABCDEFabcdef" !! 0 .. 9A .. Fa .. f
    character(len=*), public, parameter :: hex_digits = fullhex_digits(1:16) !! 0 .. 9A .. F
    character(len=*), public, parameter :: lowerhex_digits = "0123456789abcdef" !! 0 .. 9a .. f
    character(len=*), public, parameter :: digits = hex_digits(1:10) !! 0 .. 9
    character(len=*), public, parameter :: octal_digits = digits(1:8) !! 0 .. 7
    character(len=*), public, parameter :: letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" !! A .. Za .. z
    character(len=*), public, parameter :: uppercase = letters(1:26) !! A .. Z
    character(len=*), public, parameter :: lowercase = letters(27:) !! a .. z

    ! All control characters in the ASCII table (see www.asciitable.com).
    character(len=1), public, parameter :: NUL = achar(z'00') !! Null
    character(len=1), public, parameter :: SOH = achar(z'01') !! Start of heading
    character(len=1), public, parameter :: STX = achar(z'02') !! Start of text
    character(len=1), public, parameter :: ETX = achar(z'03') !! End of text
    character(len=1), public, parameter :: EOT = achar(z'04') !! End of transmission
    character(len=1), public, parameter :: ENQ = achar(z'05') !! Enquiry
    character(len=1), public, parameter :: ACK = achar(z'06') !! Acknowledge
    character(len=1), public, parameter :: BEL = achar(z'07') !! Bell
    character(len=1), public, parameter :: BS  = achar(z'08') !! Backspace
    character(len=1), public, parameter :: TAB = achar(z'09') !! Horizontal tab
    character(len=1), public, parameter :: LF  = achar(z'0A') !! NL line feed, new line
    character(len=1), public, parameter :: VT  = achar(z'0B') !! Vertical tab
    character(len=1), public, parameter :: FF  = achar(z'0C') !! NP form feed, new page
    character(len=1), public, parameter :: CR  = achar(z'0D') !! Carriage return
    character(len=1), public, parameter :: SO  = achar(z'0E') !! Shift out
    character(len=1), public, parameter :: SI  = achar(z'0F') !! Shift in
    character(len=1), public, parameter :: DLE = achar(z'10') !! Data link escape
    character(len=1), public, parameter :: DC1 = achar(z'11') !! Device control 1
    character(len=1), public, parameter :: DC2 = achar(z'12') !! Device control 2
    character(len=1), public, parameter :: DC3 = achar(z'13') !! Device control 3
    character(len=1), public, parameter :: DC4 = achar(z'14') !! Device control 4
    character(len=1), public, parameter :: NAK = achar(z'15') !! Negative acknowledge
    character(len=1), public, parameter :: SYN = achar(z'16') !! Synchronous idle
    character(len=1), public, parameter :: ETB = achar(z'17') !! End of transmission block
    character(len=1), public, parameter :: CAN = achar(z'18') !! Cancel
    character(len=1), public, parameter :: EM  = achar(z'19') !! End of medium
    character(len=1), public, parameter :: SUB = achar(z'1A') !! Substitute
    character(len=1), public, parameter :: ESC = achar(z'1B') !! Escape
    character(len=1), public, parameter :: FS  = achar(z'1C') !! File separator
    character(len=1), public, parameter :: GS  = achar(z'1D') !! Group separator
    character(len=1), public, parameter :: RS  = achar(z'1E') !! Record separator
    character(len=1), public, parameter :: US  = achar(z'1F') !! Unit separator
    character(len=1), public, parameter :: DEL = achar(z'7F') !! Delete

    character(len=*), public, parameter :: whitespace = " "//TAB//VT//CR//LF//FF !! ASCII _whitespace

contains

    !> Whether `c` is an ASCII letter (A .. Z, a .. z).
    pure logical function is_alpha(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_alpha = (c >= 'A' .and. c <= 'Z') .or. (c >= 'a' .and. c <= 'z')
    end function

    !> Whether `c` is a letter or a number (0 .. 9, a .. z, A .. Z).
    pure logical function is_alphanum(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_alphanum = (c >= '0' .and. c <= '9') .or. (c >= 'a' .and. c <= 'z') .or. (c >= 'A' .and. c <= 'Z')
    end function

    !> Whether or not `c` is in the ASCII character set - i.e. in the
    !  range 0 .. 0x7F.
    pure logical function is_ascii(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_ascii = iachar(c) <= z'7F'
    end function

    !> Whether `c` is a control character.
    pure logical function is_control(c)
        character(len=1), intent(in) :: c !! The character to test.
        integer :: ic
        ic = iachar(c)
        is_control = ic < z'20' .or. ic == z'7F'
    end function

    !> Whether `c` is a digit (0 .. 9).
    pure logical function is_digit(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_digit = ('0' <= c) .and. (c <= '9')
    end function

    !> Whether `c` is a digit in base 8 (0 .. 7).
    pure logical function is_octal_digit(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_octal_digit = (c >= '0') .and. (c <= '7');
    end function

    !> Whether `c` is a digit in base 16 (0 .. 9, A .. F, a .. f).
    pure logical function is_hex_digit(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_hex_digit = (c >= '0' .and. c <= '9') .or. (c >= 'a' .and. c <= 'f') .or. (c >= 'A' .and. c <= 'F')
    end function

    !> Whether or not `c` is a punctuation character. That includes
    !  all ASCII characters which are not control characters, letters, digits, or
    !  whitespace.
    pure logical function is_punctuation(c)
        character(len=1), intent(in) :: c !! The character to test.
        integer :: ic
        ic = iachar(c) !       '~'                 '!'
        is_punctuation = (ic <= z'7E') .and. (ic >= z'21') .and. (.not. is_alphanum(c))
    end function

    !> Whether or not `c` is a printable character other than the
    !  space character.
    pure logical function is_graphical(c)
        character(len=1), intent(in) :: c !! The character to test.
        integer :: ic
        ic = iachar(c) !  '!'                     '~'
        is_graphical = (z'21' <= ic) .and. (ic <= z'7E')
    end function

    !> Whether or not `c` is a printable character - including the
    !  space character.
    pure logical function is_printable(c)
        character(len=1), intent(in) :: c !! The character to test.
        integer :: ic
        ic = iachar(c)                    ! '~'
        is_printable = c >= ' ' .and. ic <= z'7E'
    end function

    !> Whether `c` is a lowercase ASCII letter (a .. z).
    pure logical function is_lower(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_lower = (c >= 'a') .and. (c <= 'z')
    end function

    !> Whether `c` is an uppercase ASCII letter (A .. Z).
    pure logical function is_upper(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_upper = (c <= 'Z') .and. ('A' <= c)
    end function

    !> Whether or not `c` is a whitespace character. That includes the
    !  space, tab, vertical tab, form feed, carriage return, and linefeed
    !  characters.
    pure logical function is_white(c)
        character(len=1), intent(in) :: c !! The character to test.
        integer :: ic
        ic = iachar(c)             ! TAB, LF, VT, FF, CR
        is_white = (c == ' ') .or. (ic >= z'09' .and. ic <= z'0D');
    end function

    !> Whether or not `c` is a blank character. That includes the
    !  only the space and tab characters
    pure logical function is_blank(c)
        character(len=1), intent(in) :: c !! The character to test.
        integer :: ic
        ic = iachar(c)             ! TAB
        is_blank = (c == ' ') .or. (ic == z'09');
    end function

    !> Returns the corresponding lowercase letter, if `c` is an uppercase
    !  ASCII character, otherwise `c` itself.
    pure function to_lower(c) result(t)
        character(len=1), intent(in) :: c !! A character.
        character(len=1) :: t
        integer :: diff
        diff = iachar('A')-iachar('a')
        t = c
        ! if uppercase, make lowercase
        if (is_upper(t)) t = achar(iachar(t) - diff)
    end function

    !> Returns the corresponding uppercase letter, if `c` is a lowercase
    !  ASCII character, otherwise `c` itself.
    pure function to_upper(c) result(t)
        character(len=1), intent(in) :: c !! A character.
        character(len=1) :: t
        integer :: diff
        diff = iachar('A')-iachar('a')
        t = c
        ! if lowercase, make uppercase
        if (is_lower(t)) t = achar(iachar(t) + diff)
    end function

end module