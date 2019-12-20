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
    elemental logical function is_alpha(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_alpha = (c >= 'A' .and. c <= 'Z') .or. (c >= 'a' .and. c <= 'z')
    end function

    !> Whether `c` is a letter or a number (0 .. 9, a .. z, A .. Z).
    elemental logical function is_alphanum(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_alphanum = (c >= '0' .and. c <= '9') .or. (c >= 'a' .and. c <= 'z') .or. (c >= 'A' .and. c <= 'Z')
    end function

    !> Whether or not `c` is in the ASCII character set - i.e. in the
    !  range 0 .. 0x7F.
    elemental logical function is_ascii(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_ascii = iachar(c) <= z'7F'
    end function

    !> Whether `c` is a control character.
    elemental logical function is_control(c)
        character(len=1), intent(in) :: c !! The character to test.
        integer :: ic
        ic = iachar(c)
        is_control = ic < z'20' .or. ic == z'7F'
    end function

    !> Whether `c` is a digit (0 .. 9).
    elemental logical function is_digit(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_digit = ('0' <= c) .and. (c <= '9')
    end function

    !> Whether `c` is a digit in base 8 (0 .. 7).
    elemental logical function is_octal_digit(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_octal_digit = (c >= '0') .and. (c <= '7');
    end function

    !> Whether `c` is a digit in base 16 (0 .. 9, A .. F, a .. f).
    elemental logical function is_hex_digit(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_hex_digit = (c >= '0' .and. c <= '9') .or. (c >= 'a' .and. c <= 'f') .or. (c >= 'A' .and. c <= 'F')
    end function

    !> Whether or not `c` is a punctuation character. That includes
    !  all ASCII characters which are not control characters, letters, digits, or
    !  whitespace.
    elemental logical function is_punctuation(c)
        character(len=1), intent(in) :: c !! The character to test.
        integer :: ic
        ic = iachar(c) !       '~'                 '!'
        is_punctuation = (ic <= z'7E') .and. (ic >= z'21') .and. (.not. is_alphanum(c))
    end function

    !> Whether or not `c` is a printable character other than the
    !  space character.
    elemental logical function is_graphical(c)
        character(len=1), intent(in) :: c !! The character to test.
        integer :: ic
        ic = iachar(c)
        is_graphical = (z'21' <= ic) .and. (ic <= z'7E')
    end function

    !> Whether or not `c` is a printable character - including the
    !  space character.
    elemental logical function is_printable(c)
        character(len=1), intent(in) :: c !! The character to test.
        integer :: ic
        ic = iachar(c)
        is_printable = c >= ' ' .and. ic <= iachar('~')
    end function

    !> Whether `c` is a lowercase ASCII letter (a .. z).
    elemental logical function is_lower(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_lower = (c >= 'a') .and. (c <= 'z')
    end function

    !> Whether `c` is an uppercase ASCII letter (A .. Z).
    elemental logical function is_upper(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_upper = (c <= 'Z') .and. ('A' <= c)
    end function

    !> Whether or not `c` is a whitespace character. That includes the
    !  space, tab, vertical tab, form feed, carriage return, and linefeed
    !  characters.
    elemental logical function is_white(c)
        character(len=1), intent(in) :: c !! The character to test.
        integer :: ic
        ic = iachar(c)             ! TAB, LF, VT, FF, CR
        is_white = (c == ' ') .or. (ic >= z'09' .and. ic <= z'0D');
    end function

    !> Returns the corresponding lowercase letter, if `c` is an uppercase
    !  ASCII character, otherwise `c` itself.
    elemental function to_lower(c) result(t)
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
    elemental function to_upper(c) result(t)
        character(len=1), intent(in) :: c !! A character.
        character(len=1) :: t
        integer :: diff
        diff = iachar('A')-iachar('a')
        t = c
        ! if lowercase, make uppercase
        if (is_lower(t)) t = achar(iachar(t) + diff)
    end function

end module

module test_ascii

    use stdlib_ascii
    implicit none

    integer, parameter :: u = selected_char_kind('ISO_10646')

contains

    subroutine test_is_alphanum_1
        print *, "test_is_alphanum_1"
        print *, is_alphanum('A')
        print *, is_alphanum('1')
        print *, .not. is_alphanum('#')

        ! N.B.: does not return true for non-ASCII Unicode alphanumerics
        print *, .not. is_alphanum('á')
    end subroutine

    subroutine test_is_alphanum_2
        integer :: i
        character(len=:), allocatable :: clist
        print *, "test_is_alphanum_2"
        clist = digits//octal_digits//fullhex_digits//letters//lowercase//uppercase
        do i = 1, len(clist)
            print *, is_alphanum(clist(i:i))
        end do

        clist = whitespace
        do i = 1, len(clist)
            print *, .not. is_alphanum(clist(i:i))
        end do
    end subroutine

    subroutine test_is_alpha_1
        print *, "test_is_alpha_1"
        print *, is_alpha('A')
        print *, .not. is_alpha('1')
        print *, .not. is_alpha('#')

        ! N.B.: does not return true for non-ASCII Unicode alphabetic characters
        print *, .not. is_alpha('á')
    end subroutine

    subroutine test_is_alpha_2
        integer :: i
        character(len=:), allocatable :: clist
        print *, "test_is_alpha_2"
        clist = letters//lowercase//uppercase
        do i = 1, len(clist)
            print *, is_alpha(clist(i:i))
        end do

        clist = digits//octal_digits//whitespace
        do i = 1, len(clist)
            print *, .not. is_alpha(clist(i:i))
        end do
    end subroutine

    subroutine test_is_lower_1
        print *, "test_is_lower_1"
        print *, is_lower('a')
        print *, .not. is_lower('A')
        print *, .not. is_lower('#')

        ! N.B.: does not return true for non-ASCII Unicode lowercase letters
        print *, .not. is_lower('á')
        print *, .not. is_lower('Á')
    end subroutine

    subroutine test_is_lower_2
        integer :: i
        character(len=:), allocatable :: clist
        print *, "test_is_lower_2"
        do i = 1, len(lowercase)
            print *, is_lower(lowercase(i:i))
        end do

        clist = digits//uppercase//whitespace
        do i = 1, len(clist)
            print *, .not. is_lower(clist(i:i))
        end do
    end subroutine

    subroutine test_is_upper_1
        print *, "test_is_upper_1"
        print *, is_upper('A')
        print *, .not. is_upper('a')
        print *, .not. is_upper('#')

        ! N.B.: does not return true for non-ASCII Unicode uppercase letters
        print *, .not. is_upper('á')
        print *, .not. is_upper('Á')
    end subroutine

    subroutine test_is_upper_2
        integer :: i
        character(len=:), allocatable :: clist
        print *, "test_is_upper_2"
        do i = 1, len(uppercase)
            print *, is_upper(uppercase(i:i))
        end do

        clist = digits//lowercase//whitespace
        do i = 1, len(clist)
            print *, .not. is_upper(clist(i:i))
        end do
    end subroutine


    subroutine test_is_digit_1
        print *, "test_is_digit_1"
        print *, is_digit('3')
        print *, is_digit('8')
        print *, .not. is_digit('B')
        print *, .not. is_digit('#')

        ! N.B.: does not return true for non-ASCII Unicode numbers
        print *, .not. is_digit('０') ! full-width digit zero (U+FF10)
        print *, .not. is_digit('４') ! full-width digit four (U+FF14)
    end subroutine

    subroutine test_is_digit_2
        integer :: i
        character(len=:), allocatable :: clist
        print *, "test_is_digit_2"
        do i = 1, len(digits)
            print *, is_digit(digits(i:i))
        end do

        clist = letters//whitespace
        do i = 1, len(clist)
            print *, .not. is_digit(clist(i:i))
        end do
    end subroutine

    subroutine test_is_octal_digit_1
        print *, "test_is_octal_digit_1"
        print *,       is_octal_digit('0')
        print *,       is_octal_digit('7')
        print *, .not. is_octal_digit('8')
        print *, .not. is_octal_digit('A')
        print *, .not. is_octal_digit('#')
    end subroutine

    subroutine test_is_octal_digit_2
        integer :: i
        character(len=:), allocatable :: clist
        print *, "test_is_octal_digit_2"
        do i = 1, len(octal_digits)
            print *, is_octal_digit(octal_digits(i:i))
        end do
        clist = letters//'89'//whitespace
        do i = 1, len(clist)
            print *, .not. is_octal_digit(clist(i:i))
        end do
    end subroutine

    subroutine test_is_hex_digit_1
        print *, "test_is_hex_digit_1"
        print *,       is_hex_digit('0')
        print *,       is_hex_digit('A')
        print *,       is_hex_digit('f') !! lowercase hex digits are accepted
        print *, .not. is_hex_digit('g')
        print *, .not. is_hex_digit('G')
        print *, .not. is_hex_digit('#')
    end subroutine

    subroutine test_is_hex_digit_2
        integer :: i
        character(len=:), allocatable :: clist
        print *, "test_is_hex_digit_2"
        do i = 1, len(fullhex_digits)
            print *, is_hex_digit(fullhex_digits(i:i))
        end do
        clist = lowercase(7:)//uppercase(7:)//whitespace
        do i = 1, len(clist)
            print *, .not. is_hex_digit(clist(i:i))
        end do
    end subroutine

    subroutine test_is_white_1
        print *, "test_is_white_1"
        print *, is_white(' ')
        print *, is_white(TAB)
        print *, is_white(LF)
        print *, .not. is_white('1')
        print *, .not. is_white('a')
        print *, .not. is_white('#')
    end subroutine

    subroutine test_is_white_2
        integer :: i
        character(len=:), allocatable :: clist
        print *, "test_is_white_2"
        do i = 1, len(whitespace)
            print *, is_white(whitespace(i:i))
        end do
        clist = digits//letters
        do i = 1, len(clist)
            print *, .not. is_white(clist(i:i))
        end do
    end subroutine

    subroutine test_is_control_1
        print *, "test_is_control_1"
        ! print *, is_control('\0')
        ! print *, is_control('\022')
        print *, is_control(new_line('a')) ! newline is both whitespace and control
        print *, .not. is_control(' ')
        print *, .not. is_control('1')
        print *, .not. is_control('a')
        print *, .not. is_control('#')

        ! N.B.: non-ASCII Unicode control characters are not recognized:
        ! print *, .not. is_control('\u0080')
        ! print *, .not. is_control('\u2028')
        ! print *, .not. is_control('\u2029')
    end subroutine

    subroutine test_is_control_2
        integer :: i
        character(len=:), allocatable :: clist
        print *, "test_is_control_2"
        do i = 0, 31
            print *, is_control(achar(i))
        end do
        print *, is_control(del)

        clist = digits//letters//' '
        do i = 1, len(clist)
            print *, .not. is_control(clist(i:i)), clist(i:i)
        end do
    end subroutine

    subroutine test_is_punctuation_1
        print *, "test_is_punctuation_1"
        print *, is_punctuation('.')
        print *, is_punctuation(',')
        print *, is_punctuation(':')
        print *, is_punctuation('!')
        print *, is_punctuation('#')
        print *, is_punctuation('~')
        print *, is_punctuation('+')
        print *, is_punctuation('_')

        print *, .not. is_punctuation('1')
        print *, .not. is_punctuation('a')
        print *, .not. is_punctuation(' ')
        print *, .not. is_punctuation(LF) ! new line character
        print *, .not. is_punctuation(NUL)

        ! N.B.: Non-ASCII Unicode punctuation characters are not recognized.
        ! print *, is_punctuation('\u2012') ! (U+2012 = en-dash)
    end subroutine

    subroutine test_is_punctuation_2
        integer :: i
        character(len=1) :: c
        print *, "test_is_punctuation_2"
        do i = 0, 127
            c = achar(i)
            if (is_control(c) .or. is_alphanum(c) .or. c == ' ') then
                print *, .not. is_punctuation(c)
            else
                print *, is_punctuation(c)
            end if
        end do
    end subroutine

    subroutine test_is_graphical_1
        print *, "test_is_graphical"
        print *, is_graphical('1')
        print *, is_graphical('a')
        print *, is_graphical('#')
        print *, .not. is_graphical(' ') ! whitespace is not graphical
        print *, .not. is_graphical(LF)
        print *, .not. is_graphical(NUL)

        ! N.B.: Unicode graphical characters are not regarded as such.
        print *, .not. is_graphical('ä')
    end subroutine

    subroutine test_is_graphical_2
        integer :: i
        character(len=1) :: c
        print *, "test_is_graphical_2"
        do i = 0, 127
            c = achar(i)
            if (is_control(c) .or. c == ' ') then
                print *, .not. is_graphical(c)
            else
                print *, is_graphical(c)
            end if
        end do
    end subroutine

    subroutine test_is_printable_1
        print *, "test_is_printable_1"
        print *, is_printable(' ') ! whitespace is printable
        print *, is_printable('1')
        print *, is_printable('a')
        print *, is_printable('#')
        print *, .not. is_printable(NUL) ! control characters are not printable

        ! N.B.: Printable non-ASCII Unicode characters are not recognized.
        print *, .not. is_printable('ä')
    end subroutine

    subroutine test_is_printable_2
        integer :: i
        character(len=1) :: c
        print *, "test_is_printable_2"
        do i = 0, 127
            c = achar(i)
            if (is_control(c)) then
                print *, .not. is_printable(c)
            else
                print *, is_printable(c)
            end if
        end do
    end subroutine

    subroutine test_is_ascii_1()
        print *, "test_is_ascii_1"
        print *, is_ascii('a')
        print *, .not. is_ascii('ä')
    end subroutine

    subroutine test_is_ascii_2()
        integer :: i
        print *, "test_is_ascii_2"
        do i = 0, 127
            print *, i, is_ascii(achar(i))
        end do
        print *, 128, .not. is_ascii(achar(128)) ! raises compiler warning

    end subroutine

    subroutine test_to_lower()
        print *, to_lower('a') == 'a'
        print *, to_lower('A') == 'a'
        print *, to_lower('#') == '#'
    end subroutine

    subroutine test_to_upper()
        print *, to_upper('a') == 'A'
        print *, to_upper('A') == 'A'
        print *, to_upper('#') == '#'
    end subroutine
end module

program ascii

    use stdlib_ascii
    use test_ascii
    character(len=1) :: c

    print *, "Lowercase ", lowercase
    print *, "Uppercase ", uppercase
    print *, "Digits ", digits
    print *, "Octal digits ", octal_digits
    print *, "Full hex digits ", fullhex_digits
    print *, "Hex digits ", hex_digits
    print *, "Lower hex digits ", lowerhex_digits

    call test_is_alphanum_1
    call test_is_alphanum_2
    call test_is_alpha_1
    call test_is_alpha_2
    call test_is_lower_1
    call test_is_lower_2
    call test_is_upper_1
    call test_is_upper_2
    call test_is_digit_1
    call test_is_digit_2
    call test_is_octal_digit_1
    call test_is_octal_digit_2
    call test_is_hex_digit_1
    call test_is_hex_digit_2
    call test_is_white_1
    call test_is_white_2
    call test_is_control_1
    call test_is_control_2
    call test_is_punctuation_1
    call test_is_punctuation_2
    call test_is_graphical_1
    call test_is_graphical_2
    call test_is_printable_1
    call test_is_printable_2
    call test_is_ascii_1
    call test_is_ascii_2
    call test_to_lower
    call test_to_upper
end program