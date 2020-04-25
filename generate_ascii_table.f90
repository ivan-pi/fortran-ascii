program generate_ascii_table

    use iso_fortran_env, only: int16, i16 => int16
    use fortran_ascii
    use mod_functional, only: set
    implicit none

type :: cbase
  integer(int16) :: upper       = shiftl(1,0)
  integer(int16) :: lower       = shiftl(1,1)
  integer(int16) :: alpha       = shiftl(1,2)
  integer(int16) :: digit       = shiftl(1,3)
  integer(int16) :: hex_digit   = shiftl(1,4)
  integer(int16) :: octal_digit = shiftl(1,5)
  integer(int16) :: space       = shiftl(1,6)
  integer(int16) :: printable   = shiftl(1,7)
  integer(int16) :: graphical   = ior(ior(shiftl(1,2),shiftl(1,3)),shiftl(1,10)) ! alnum|punct
  integer(int16) :: control     = shiftl(1,9)
  integer(int16) :: punctuation = shiftl(1,10)
  integer(int16) :: alphanum    = ior(shiftl(1,2),shiftl(1,3)) ! alpha|digit
  integer(int16) :: blank       = shiftl(1,12)
end type

type(cbase), parameter :: cb = cbase()

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
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,&
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,&
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,&
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,&
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,&
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,&
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,&
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

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
        if (is_punctuation(c)) ascii_table(i) = ibset(ascii_table(i),2)
        if (is_control(c))     ascii_table(i) = ibset(ascii_table(i),3)
        if (is_graphical(c))   ascii_table(i) = ibset(ascii_table(i),4)
        if (is_printable(c))   ascii_table(i) = ibset(ascii_table(i),5)
        if (is_white(c))       ascii_table(i) = ibset(ascii_table(i),6)
        if (is_blank(c))       ascii_table(i) = ibset(ascii_table(i),7)
        if (is_upper(c))       ascii_table(i) = ibset(ascii_table(i),8)
        if (is_octal_digit(c)) ascii_table(i) = ibset(ascii_table(i),9)
        if (is_hex_digit(c))   ascii_table(i) = ibset(ascii_table(i),10)

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

    print'(I5,X,B16)', cb%upper       ,cb%upper       
    print'(I5,X,B16)', cb%lower       ,cb%lower       
    print'(I5,X,B16)', cb%alpha       ,cb%alpha       
    print'(I5,X,B16)', cb%digit       ,cb%digit       
    print'(I5,X,B16)', cb%hex_digit   ,cb%hex_digit   
    print'(I5,X,B16)', cb%octal_digit ,cb%octal_digit 
    print'(I5,X,B16)', cb%space       ,cb%space       
    print'(I5,X,B16)', cb%printable   ,cb%printable   
    print'(I5,X,B16)', cb%graphical   ,cb%graphical   
    print'(I5,X,B16)', cb%control     ,cb%control     
    print'(I5,X,B16)', cb%punctuation ,cb%punctuation 
    print'(I5,X,B16)', cb%alphanum    ,cb%alphanum    
    print'(I5,X,B16)', cb%blank       ,cb%blank    

    print *, table

    print '(/,I4,X,B0.16)', table(iachar('A',i16)), table(iachar('A',i16))
     print '(I4,X,B0.16)', cb%alphanum, cb%alphanum
    print'(B0.16)',iand(ior(table(iachar('A',i16)),cb%alpha),ior(table(iachar('A',i16)),cb%digit))
    print'(B0.16)',ior(ior(table(iachar('A',i16)),cb%alpha),cb%digit)
    print '(B0.16,X,B0.16)', iand(ior(ior(table(iachar('A',i16)),cb%alpha),cb%digit),cb%alphanum), cb%alphanum
  
    print '(/,I4,X,B0.16)', table(iachar('#',i16)), table(iachar('#',i16))
     print '(I4,X,B0.16)', cb%alphanum, cb%alphanum
    print'(B0.16)',iand(ior(table(iachar('#',i16)),cb%alpha),ior(table(iachar('#',i16)),cb%digit))
    print'(B0.16)',ior(ior(table(iachar('#',i16)),cb%alpha),cb%digit)
    print '(B0.16,X,B0.16)', iand(ior(ior(table(iachar('#',i16)),cb%alpha),cb%digit),cb%alphanum), cb%alphanum

    print '(B0.16,X,B0.16)',iand(table(iachar('A',i16)),cb%alphanum),ieor(table(iachar('A',i16)),cb%alphanum)
    print '(B0.16,X,B0.16)',iand(table(iachar('#',i16)),cb%alphanum),ieor(table(iachar('#',i16)),cb%alphanum)
end program