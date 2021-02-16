program generate_ascii_table

    use iso_fortran_env, only: i8 => int8
    use fortran_ascii
    implicit none

    integer(i8) :: tables(0:12,-128:127)
    integer :: i
    character(len=1) :: c
    character(len=40) :: fmt

    ! Initialize all bits to zero
    tables = 0

    do i = 0, 127

        c = achar(i)

        if (is_upper(c))       tables(0,i) = 1
        if (is_lower(c))       tables(1,i) = 1
        if (is_alpha(c))       tables(2,i) = 1
        if (is_digit(c))       tables(3,i) = 1
        if (is_hex_digit(c))   tables(4,i) = 1
        if (is_octal_digit(c)) tables(5,i) = 1
        if (is_white(c))       tables(6,i) = 1
        if (is_printable(c))   tables(7,i) = 1
        if (is_graphical(c))   tables(8,i) = 1
        if (is_control(c))     tables(9,i) = 1
        if (is_punctuation(c)) tables(10,i) = 1
        if (is_alphanum(c))    tables(11,i) = 1
        if (is_blank(c))       tables(12,i) = 1

    end do

    fmt = "(*(16(I1,:,',',X),/))"

    write(*,*) "is_upper"
    write(*,fmt) tables(0,:)
    write(*,*) "is_lower"
    write(*,fmt) tables(1,:)
    write(*,*) "is_alpha"
    write(*,fmt) tables(2,:)
    write(*,*) "is_digit"
    write(*,fmt) tables(3,:)
    write(*,*) "is_hex_digit"
    write(*,fmt) tables(4,:)
    write(*,*) "is_octal_digit"
    write(*,fmt) tables(5,:)
    write(*,*) "is_white"
    write(*,fmt) tables(6,:)
    write(*,*) "is_printable"
    write(*,fmt) tables(7,:)
    write(*,*) "is_graphical"
    write(*,fmt) tables(8,:)
    write(*,*) "is_control"
    write(*,fmt) tables(9,:)
    write(*,*) "is_punctuation"
    write(*,fmt) tables(10,:)
    write(*,*) "is_alphanum"
    write(*,fmt) tables(11,:)
    write(*,*) "is_blank"
    write(*,fmt) tables(12,:)
end program