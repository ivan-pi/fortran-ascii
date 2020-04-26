program generate_ascii_table

    use iso_fortran_env, only: int16
    use fortran_ascii
    implicit none

    integer(int16) :: ascii_table(0:127), i
    character(len=1) :: c

    ! Initialize all bits to zero
    ascii_table = 0

    do i = 0, 127

        c = achar(i)

        if (is_upper(c))       ascii_table(i) = ibset(ascii_table(i),0)
        if (is_lower(c))       ascii_table(i) = ibset(ascii_table(i),1)
        if (is_alpha(c))       ascii_table(i) = ibset(ascii_table(i),2)
        if (is_digit(c))       ascii_table(i) = ibset(ascii_table(i),3)
        if (is_hex_digit(c))   ascii_table(i) = ibset(ascii_table(i),4)
        if (is_octal_digit(c)) ascii_table(i) = ibset(ascii_table(i),5)
        if (is_white(c))       ascii_table(i) = ibset(ascii_table(i),6)
        if (is_printable(c))   ascii_table(i) = ibset(ascii_table(i),7)
        if (is_graphical(c))   ascii_table(i) = ibset(ascii_table(i),8)
        if (is_control(c))     ascii_table(i) = ibset(ascii_table(i),9)
        if (is_punctuation(c)) ascii_table(i) = ibset(ascii_table(i),10)
        if (is_alphanum(c))    ascii_table(i) = ibset(ascii_table(i),11)
        if (is_blank(c))       ascii_table(i) = ibset(ascii_table(i),12)

    end do

    do i = 0, 127
        c = achar(i)
        if (is_graphical(c)) then
            write(*,'(I3,3X,A,3X,I4,3X,B0.13)') i, c, ascii_table(i), ascii_table(i)
        end if
    end do

end program