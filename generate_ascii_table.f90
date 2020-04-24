program generate_ascii_table

    use iso_fortran_env, only: int16
    use fortran_ascii
    use mod_functional, only: set
    implicit none

    integer(int16) :: ascii_table(0:127), i, j
    integer(int16), allocatable :: ascii_table_set(:)
    character(len=1) :: c
    logical :: res

    ! Initialize all bits to zero
    ascii_table = 0

    do i = 0, 127

        c = achar(i)

        if (is_alpha(c))       ascii_table(i) = ibset(ascii_table(i),0)
        if (is_digit(c))       ascii_table(i) = ibset(ascii_table(i),1)
        if (is_alphanum(c))    ascii_table(i) = ibset(ascii_table(i),2)
        if (is_punctuation(c)) ascii_table(i) = ibset(ascii_table(i),3)
        if (is_control(c))     ascii_table(i) = ibset(ascii_table(i),4)
        if (is_graphical(c))   ascii_table(i) = ibset(ascii_table(i),5)
        if (is_printable(c))   ascii_table(i) = ibset(ascii_table(i),6)
        if (is_white(c))       ascii_table(i) = ibset(ascii_table(i),7)
        if (is_blank(c))       ascii_table(i) = ibset(ascii_table(i),8)
        if (is_lower(c))       ascii_table(i) = ibset(ascii_table(i),9)
        if (is_upper(c))       ascii_table(i) = ibset(ascii_table(i),10)
        if (is_octal_digit(c)) ascii_table(i) = ibset(ascii_table(i),11)
        if (is_hex_digit(c))   ascii_table(i) = ibset(ascii_table(i),12)

    end do

    do i = 0, 127
        c = achar(i)
        if (is_graphical(c)) then
            write(*,'(I3,3X,A,3X,I4,3X,B0.13)') i, c, ascii_table(i), ascii_table(i)
        end if
    end do
    
    write(*,'(/,A)') "Full table:"
    write(*,'(A1,128(I0,:,","))',advance='no') "[",(ascii_table(i),i=0,127)
    write(*,'(A1)') "]"

    ascii_table_set = set(ascii_table)

    write(*,'(/,A)') "Reduced table:"
    write(*,'(A1,*(I0,:,","))',advance='no') "[",ascii_table_set
    write(*,'(A1)') "]"

    write(*,'(/,A)') "Indexes:"
    do i=1,size(ascii_table_set)
        write(*,'("[",*(I0,:,","))',advance='no') pack([(j,j=0,127)],ascii_table==ascii_table_set(i))
        write(*,'(A1)') "]"
    end do

end program