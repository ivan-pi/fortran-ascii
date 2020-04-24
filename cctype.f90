module cctype

  use iso_c_binding, only: c_int
  implicit none
  public

  !
  ! Character classification functions
  !
  interface

    pure integer(c_int) function isalnum(c) bind(c,name='isalnum')
      import c_int
      integer(c_int), intent(in), value :: c
    end function isalnum

    pure integer(c_int) function isalpha(c) bind(c,name='isalpha')
      import c_int
      integer(c_int), intent(in), value :: c
    end function isalpha

    pure integer(c_int) function isblank(c) bind(c,name='isblank')
      import c_int
      integer(c_int), intent(in), value :: c
    end function isblank

    pure integer(c_int) function iscntrl(c) bind(c,name='iscntrl')
      import c_int
      integer(c_int), intent(in), value :: c
    end function iscntrl

    pure integer(c_int) function isdigit(c) bind(c,name='isdigit')
      import c_int
      integer(c_int), intent(in), value :: c
    end function isdigit

    pure integer(c_int) function isgraph(c) bind(c,name='isgraph')
      import c_int
      integer(c_int), intent(in), value :: c
    end function isgraph

    pure integer(c_int) function islower(c) bind(c,name='islower')
      import c_int
      integer(c_int), intent(in), value :: c
    end function islower

    pure integer(c_int) function isprint(c) bind(c,name='isprint')
      import c_int
      integer(c_int), intent(in), value :: c
    end function isprint

    pure integer(c_int) function ispunct(c) bind(c,name='ispunct')
      import c_int
      integer(c_int), intent(in), value :: c
    end function ispunct

    pure integer(c_int) function isspace(c) bind(c,name='isspace')
      import c_int
      integer(c_int), intent(in), value :: c
    end function isspace

    pure integer(c_int) function isupper(c) bind(c,name='isupper')
      import c_int
      integer(c_int), intent(in), value :: c
    end function isupper

    pure integer(c_int) function isxdigit(c) bind(c,name='isxdigit')
      import c_int
      integer(c_int), intent(in), value :: c
    end function isxdigit

  end interface


  !
  ! Character conversion functions
  !
  interface

    pure integer(c_int) function tolower(c) bind(c,name='tolower')
      import c_int
      integer(c_int), intent(in), value :: c
    end function

    pure integer(c_int) function toupper(c) bind(c,name='toupper')
      import c_int
      integer(c_int), intent(in), value :: c
    end function

  end interface

end module