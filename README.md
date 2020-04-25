# Fortran functions which operate on ASCII characters

This repository contains Fortran routines aiming to reproduce the functionality found in [`<ctype.h>`](https://en.wikipedia.org/wiki/C_character_classification) header of the C standard library or [std.ascii](https://dlang.org/phobos/std_ascii.html) namespace from the D runtime library.

Currently, this is just my personal testing ground that should later become a pull request to the recent Fortran [stdlib](https://github.com/fortran-lang/stdlib) project (see also issue [#11](https://github.com/fortran-lang/stdlib/issues/11) and my previous pull requests [#32](https://github.com/fortran-lang/stdlib/pull/32), and [#49](https://github.com/fortran-lang/stdlib/pull/49)).

TODO: 
 * Unicode support (or wide characters in general)

## Routines

### Character classification

All Fortran routines accept a default `character(len=1)` dummy variable and return a logical value. This differs from the C routines which accept and return an integer value.

|                          Purpose                          |     Fortran      |   C/C++  |       D       |   Python*   |
|:---------------------------------------------------------|:----------------:|:--------:|:-------------:|:-----------:|
|            Checks if a character is alphabetic            |    `is_alpha`    |  `isalpha` |    `isAlpha`    |   `isalpha`   |
|           Checks if a character is alphanumeric           |   `is_alphanum`  |  `isalnum` |   `isAlphaNum`  |   `isalnum`   |
|    Checks if a character is in the ASCII character set    |    `is_ascii`    |      /    |    `isASCII`    |   `isascii`   |
|        Checks if a character is a control character       |   `is_control`   |  `iscntrl` |   `isControl`   |             |
|              Checks if a character is a digit             |    `is_digit`    |  `isdigit` |    `isDigit`    |   `isdigit`   |
|         Checks if a character is a octal character        | `is_octal_digit` |     /    |  `isOctalDigit` |             |
| Checks if a character is a hexadecimal character          | `is_hex_digit`   | `isxdigit` | `isHexDigit`    |             |
| Checks if a character is a punctuation character          | `is_punctuation` | `ispunct`  | `isPunctuation` |             |
| Checks if a character is a graphical character            | `is_graphical`   | `isgraph`  | `isGraphical`  |             |
| Checks if a character is a printing character             | `is_printable`   | `isprint`  | `isPrintable`   | `isprintable` |
| Checks if a character is an uppercase character           | `is_upper`       | `isupper`  | `isUpper`       | `isupper`     |
| Checks if a character is lowercase                        | `is_lower`       | `islower`  | `isLower`       | `islower`     |
| Checks if a character is a whitespace character           | `is_white`       | `isspace`  | `isWhite`       | `isspace`     |
| Checks if a character is a blank character (space or tab) | `is_blank`       | `isblank`  | /             |             |


### Case conversion

These routines accept a `character(len=1)` argument and return the same character with swapped case. 

|                          Purpose                          |     Fortran      |   C/C++  |       D       |   Python*   |
|:---------------------------------------------------------|:----------------:|:--------:|:-------------:|:-----------:|
| Converts a character to lowercase                         | `to_lower`       | `tolower`  | `toLower`       |             |
| Converts a character to uppercase                         | `to_upper`       | `toupper`  | `toUpper`       |             |

## Implementation approaches

### Direct approach

In this approach we use comparison operators and the default character collating sequence (if applicable) to validate the characters. 

Example for `is_alphanum`:

```fortran
pure logical function is_alphanum(c)
  character(len=1), intent(in) :: c !! The character to test.
  is_alphanum = (c >= '0' .and. c <= '9') .or. (c >= 'a' .and. c <= 'z') &
      .or. (c >= 'A' .and. c <= 'Z')
end function
```

For characters with no guarantee on position in the collating sequence we use the `iachar` intrinsic to check the position in the ASCII character set.

Example for `is_control`:

```fortran
pure logical function is_control(c)
  character(len=1), intent(in) :: c !! The character to test.
  integer :: ic
  ic = iachar(c)
  is_control = ic < z'20' .or. ic == z'7F'
end function
```

### Select case 

Instead of comparison operators, we can also use the select case construct and `iachar` intrinsic to verify the character belongs to a specific set.

Example for `is_alphanum`:
```fortran
pure logical function is_alphanum(c)
  character(len=1), intent(in) :: c !! The character to test.
  select case(iachar(c))
    case (48:57,65:90,97:122) ! A .. Z, 0 .. 9, a .. z
      is_alphanum = .true.
    case default
      is_alphanum = .false.
  end select
end function
```

Example for `is_control`:
```fortran
pure logical function is_control(c)
  character(len=1), intent(in) :: c !! The character to test.
  select case(iachar(c))
    case (0:31,127)
      is_control = .true.
    case default
      is_control = .false.
  end select
end function
```
### Lookup table

As noted on the [C character classification](https://en.wikipedia.org/wiki/C_character_classification) Wikipedia page:

> ...the character classification routines are not written as comparison tests. In most C libraries, they are written as static table lookups instead of macros or functions. 

This approach can be mimicked also in Fortran. Since there are 13 functions (excluding `is_ascii`) we require an integer with a storage size of at least 13 bits to encode the various properties. Here we simply use the `int16` from the `iso_fortran_env` module. On architectures with word sizes smaller than 8 bits, it might be benefical to use the result of `selected_int_kind(4)`; this covers integers in the range from -10^4 to 10^4 (exclusive), thereby including the largest necessary mask 2^12 = 4096. First we create the bitmasks for the different character properties by left-shifting the value one:
```fortran
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
```
The mask for alphanumerical characters is built using the bitwise logical **or** of the digit and alphabetical masks. Similarly, the mask for graphical characters is built from the masks for alphabetical characters, digits, and punctuation.

Next, we populate a look-up table, assigning the ASCII characters their respective masks: 

<details>
  <summary>Click here to see the complete lookup table</summary>

```fortran
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
```
</details>

The reason why the first 128 characters are filled with zeros will be explained later. To make sure things worked correctly, we can write a short program to display the characters and their binary property masks:
```fortran
  do i = 0, 127
      c = achar(i)
      if (is_printable(c)) then
        write(*,'(I3,3X,A,3X,I4,3X,B0.16)') i, c, table(i), table(i)
      end if
  end do
```
<details>
  <summary>Click here to see the result.</summary>
  
```
 32       4288   0001000011000000
 33   !   1152   0000010010000000
 34   "   1152   0000010010000000
 35   #   1152   0000010010000000
 36   $   1152   0000010010000000
 37   %   1152   0000010010000000
 38   &   1152   0000010010000000
 39   '   1152   0000010010000000
 40   (   1152   0000010010000000
 41   )   1152   0000010010000000
 42   *   1152   0000010010000000
 43   +   1152   0000010010000000
 44   ,   1152   0000010010000000
 45   -   1152   0000010010000000
 46   .   1152   0000010010000000
 47   /   1152   0000010010000000
 48   0    184   0000000010111000
 49   1    184   0000000010111000
 50   2    184   0000000010111000
 51   3    184   0000000010111000
 52   4    184   0000000010111000
 53   5    184   0000000010111000
 54   6    184   0000000010111000
 55   7    184   0000000010111000
 56   8    152   0000000010011000
 57   9    152   0000000010011000
 58   :   1152   0000010010000000
 59   ;   1152   0000010010000000
 60   <   1152   0000010010000000
 61   =   1152   0000010010000000
 62   >   1152   0000010010000000
 63   ?   1152   0000010010000000
 64   @   1152   0000010010000000
 65   A    149   0000000010010101
 66   B    149   0000000010010101
 67   C    149   0000000010010101
 68   D    149   0000000010010101
 69   E    149   0000000010010101
 70   F    149   0000000010010101
 71   G    133   0000000010000101
 72   H    133   0000000010000101
 73   I    133   0000000010000101
 74   J    133   0000000010000101
 75   K    133   0000000010000101
 76   L    133   0000000010000101
 77   M    133   0000000010000101
 78   N    133   0000000010000101
 79   O    133   0000000010000101
 80   P    133   0000000010000101
 81   Q    133   0000000010000101
 82   R    133   0000000010000101
 83   S    133   0000000010000101
 84   T    133   0000000010000101
 85   U    133   0000000010000101
 86   V    133   0000000010000101
 87   W    133   0000000010000101
 88   X    133   0000000010000101
 89   Y    133   0000000010000101
 90   Z    133   0000000010000101
 91   [   1152   0000010010000000
 92   \   1152   0000010010000000
 93   ]   1152   0000010010000000
 94   ^   1152   0000010010000000
 95   _   1152   0000010010000000
 96   `   1152   0000010010000000
 97   a    150   0000000010010110
 98   b    150   0000000010010110
 99   c    150   0000000010010110
100   d    150   0000000010010110
101   e    150   0000000010010110
102   f    150   0000000010010110
103   g    134   0000000010000110
104   h    134   0000000010000110
105   i    134   0000000010000110
106   j    134   0000000010000110
107   k    134   0000000010000110
108   l    134   0000000010000110
109   m    134   0000000010000110
110   n    134   0000000010000110
111   o    134   0000000010000110
112   p    134   0000000010000110
113   q    134   0000000010000110
114   r    134   0000000010000110
115   s    134   0000000010000110
116   t    134   0000000010000110
117   u    134   0000000010000110
118   v    134   0000000010000110
119   w    134   0000000010000110
120   x    134   0000000010000110
121   y    134   0000000010000110
122   z    134   0000000010000110
123   {   1152   0000010010000000
124   |   1152   0000010010000000
125   }   1152   0000010010000000
126   ~   1152   0000010010000000
```
</details>

We can easily see how characters with the same set of properties (alpha,digit,upper,lower,...) share the same bitmask.


For the actual validation functions we use the `iachar` intrinsic to find the correct value in the lookup table and call the `btest` intrinsic to verify if the bit encoding the target property is set.

Example for `is_alpha`:

```fortran
pure logical function is_alpha(c)
  character(len=1), intent(in) :: c !! The character to test.
  is_alpha = btest(table(iachar(c,i8)),2)
end function
```

A second approach is to apply a bitwise logical **and** and check the result is not zero. (See [this](http://pdebuyl.be/blog/2018/fortran-bitmasks.html) blog post by Pierre de Buyl to brush up on Fortran bitmasks.)

Example for `is_control`:
```fortran
pure logical function is_control(c)
  character(len=1), intent(in) :: c !! The character to test.
  is_control = iand(table(iachar(c,i8)),m_control) /= 0
end function
```

For the functions `is_graphical` and `is_alphanum` (these properties are encoded via multiple bits) we can either follow the second approach or test multiple bits (e.g. for `is_alphanum` we need to test the bits at positions 2 and 3 with a logical `.or.` inbetween).

At the time of writing, I am still not sure how to deal correctly with any non-ASCII characters, which might still fit into the size of the `character(len=1)` type (typically, this is one byte). The way this seems to be dealt with in the C standard library is by casting the character as an `unsigned short` integer taking on 8-bits (values from 0 to 255) and using a lookup table with 256 values. Since Fortran does not provide unsigned integers, the approach I've adopted is to use custom array bounds from -128 to 127, which fit the range of values for the `integer(int8)` type. Hence, the zeros in the first 128 table elements. To make this work as desired, we also need to provide `int8` (from the `iso_fortran_env` module) as the kind parameter in calls to the intrinsic `iachar(c[,kind])`.

### C binding

For compilers such as gfortran and ifort, the final executable is typically linked to the C standard library (see [this]() discussion). This means we can easily borrow the functionality from C (apart from the function for octal digits, which is not part of the C standard library). 

First, we create a module that defines the interfaces of the functions in `<ctype.h>`:

```fortran
module cctype

  use iso_c_binding, only: c_int
  implicit none
  public

  interface
    pure integer(c_int) function isalnum(c) bind(c,name='isalnum')
      import c_int
      integer(c_int), intent(in), value :: c
    end function isalnum
    pure integer(c_int) function iscntrl(c) bind(c,name='iscntrl')
      import c_int
      integer(c_int), intent(in), value :: c
    end function iscntrl

    ! ... remaining functions ...

  end interface
end module
```

Then we use the `iachar` intrinsic to convert Fortran characters to C integers and pass them to C routines. The return value of the C routines is an integer different from zero (i.e., `true`) for characters which have the tested property, and zero (i.e. `false`) otherwise.

Example for `is_alphanum`:
```fortran
pure logical function is_alphanum(c)
  character(len=1), intent(in) :: c !! The character to test.
  is_alphanum = isalnum(iachar(c,c_int)) /= 0
end function
```

Example for `is_control`:
```fortran
pure logical function is_control(c)
  character(len=1), intent(in) :: c !! The character to test.
  is_control = iscntrl(iachar(c,c_int)) /= 0
end function
```

