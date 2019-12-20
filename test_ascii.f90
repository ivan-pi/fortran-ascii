program test_ascii

    ! use stdlib_error, only: assert
    use stdlib_ascii

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
    call test_to_lower_1
    call test_to_lower_2
    call test_to_upper_1
    call test_to_upper_2

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

    subroutine test_to_lower_1()
        print *, to_lower('a') == 'a'
        print *, to_lower('A') == 'a'
        print *, to_lower('#') == '#'
    end subroutine

    subroutine test_to_lower_2()
        integer :: i
        character(len=1) :: c
        do i = 1, len(uppercase)
            print *, to_lower(uppercase(i:i)) == lowercase(i:i)
        end do
        do i = 0, 127
            c = achar(i)
            if (c < 'A' .or. c > 'Z') then
                print *, to_lower(c) == c
            else
                print *, to_lower(c) /= c
            end if
        end do
    end subroutine

    subroutine test_to_upper_1()
        print *, to_upper('a') == 'A'
        print *, to_upper('A') == 'A'
        print *, to_upper('#') == '#'
    end subroutine

    subroutine test_to_upper_2()
        integer :: i
        character(len=1) :: c
        do i = 1, len(lowercase)
            print *, to_upper(lowercase(i:i)) == uppercase(i:i)
        end do
        do i = 0, 127
            c = achar(i)
            if (c < 'a' .or. c > 'z') then
                print *, to_upper(c) == c
            else
                print *, to_upper(c) /= c
            end if
        end do
    end subroutine
end program