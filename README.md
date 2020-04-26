# Fortran functions which operate on ASCII characters

This repository contains Fortran routines aiming to reproduce the functionality found in [`<ctype.h>`](https://en.wikipedia.org/wiki/C_character_classification) header of the C standard library or [std.ascii](https://dlang.org/phobos/std_ascii.html) namespace from the D runtime library.

Currently, this is just my personal testing ground that should later become a pull request to the recent Fortran [stdlib](https://github.com/fortran-lang/stdlib) project (see also issue [#11](https://github.com/fortran-lang/stdlib/issues/11) and my previous pull requests [#32](https://github.com/fortran-lang/stdlib/pull/32), and [#49](https://github.com/fortran-lang/stdlib/pull/49)).

TODO: 
 * Unicode support (or wide characters in general)

## Building

```
git clone https://github.com/ivan-pi/fortran-ascii
cd fortran-ascii
```

A Makefile is provided to build the following executables via `make <target>`, where target is one of:
* `test_ascii` (default target): test routines for the ASCII functions
* `benchmark_f90`: benchmarking the Fortran routines
* `benchmark_cpp`: benchmark the C++ routines
* `generate_characters`: generates character strings used by the benchmarks
* `generate_ascii_table`: generates the ASCII lookup table (not relevant anymore)
* `print_table`: print the lookup table defined in `fortran_ascii_bits.f90`

To set the compiler and flags use:
```
make FC=gfortran-9 FFLAGS="-O3 -march=native" benchmark_f90
```

One of the main goals of this repository is to test different implementations of the character routines (see sections below). This can be done very smoothly using submodules, which separate the function interfaces from the actual implementations.

The Makefile uses the `SM` variable to link with a specific submodule:
```
make SM=bit benchmark_f90
```
Try switching between `pure`, `bit`, `selectcase`, and `cctype`, and comparing the performance. 

A shell script is available to run benchmarks for all four implementations and a reference in C++:
```
./run_benchmarks.sh
```
The results are given as *processed characters per second* and are exported as txt files. If you switch the compiler run `make clean` between trials.

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

Instead of comparison operators, we can also use the select case construct and `iachar` intrinsic to verify the character belongs to a specific set. This approach can be found in the Dan Nagle's [`character_functions`](http://www.daniellnagle.com/charfunc.html) module.

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
    case (0:31,127) ! contral chars, delete
      is_control = .true.
    case default
      is_control = .false.
  end select
end function
```
### Lookup table

As noted on the [C character classification](https://en.wikipedia.org/wiki/C_character_classification) Wikipedia page:

> ...the character classification routines are not written as comparison tests. In most C libraries, they are written as static table lookups instead of macros or functions. 

This approach can be mimicked also in Fortran. Since there are 13 functions (excluding `is_ascii`) we require an integer with a storage size of at least 13 bits to encode the various properties. ~(Perhaps even 11-bits suffice.)~ Here we simply use the `int16` from the `iso_fortran_env` module. On architectures with word sizes smaller than 8 bits, it might be benefical to use the result of `selected_int_kind(4)`; this covers integers in the range from -10^4 to 10^4 (exclusive), thereby including the largest necessary mask 2^12 = 4096. First we create the bitmasks for the different character properties by left-shifting the value one:
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

## Results

The results shown below are from a ThinkPad T530 with an Intel(R) Core(TM) i5-3320M 2.6 GHz CPU and 16GB of RAM (DDR3 SODIMM). The operating system is Ubuntu 16.04 64-bit.

### gfortran

The benchmark drivers (for both gfortran and g++) were compiled with `-O3 -mtune=native`. The first column is the number of characters tested. The remaining columns give the number of processed characters per second. Each row is the average of 10 loop iterations.

#### Direct approach (`pure`)

```
#             is_control   is_printable       is_white       is_blank   is_graphical is_punctuation    is_alphanum       is_alpha       is_upper       is_lower       is_digit   is_hex_digit
     1000   0.436376E+09   0.510178E+09   0.205423E+09   0.205537E+09   0.512348E+09   0.240229E+09   0.222782E+09   0.439715E+09   0.512636E+09   0.512689E+09   0.439522E+09   0.306251E+09
    10000   0.441628E+09   0.515177E+09   0.206147E+09   0.190267E+09   0.447571E+09   0.205753E+09   0.138547E+09   0.441565E+09   0.514324E+09   0.512805E+09   0.380120E+09   0.243273E+09
   100000   0.438197E+09   0.514540E+09   0.203249E+09   0.196803E+09   0.520780E+09   0.199336E+09   0.168836E+09   0.450742E+09   0.531140E+09   0.538579E+09   0.451680E+09   0.264668E+09
  1000000   0.453349E+09   0.533992E+09   0.207663E+09   0.212638E+09   0.532194E+09   0.203849E+09   0.171358E+09   0.450056E+09   0.533272E+09   0.534162E+09   0.442647E+09   0.250858E+09
 10000000   0.453059E+09   0.534110E+09   0.200340E+09   0.207512E+09   0.530243E+09   0.205621E+09   0.170661E+09   0.453215E+09   0.529308E+09   0.530995E+09   0.456523E+09   0.261193E+09
100000000   0.449823E+09   0.519476E+09   0.200751E+09   0.207241E+09   0.525285E+09   0.204245E+09   0.170677E+09   0.454241E+09   0.526595E+09   0.523955E+09   0.447773E+09   0.257317E+09
```

#### Select case (`selectcase`)

```
#             is_control   is_printable       is_white       is_blank   is_graphical is_punctuation    is_alphanum       is_alpha       is_upper       is_lower       is_digit   is_hex_digit
     1000   0.407116E+09   0.466179E+09   0.364339E+09   0.466005E+09   0.468384E+09   0.192645E+09   0.251889E+09   0.270307E+09   0.468099E+09   0.468362E+09   0.468165E+09   0.253370E+09
    10000   0.411387E+09   0.470095E+09   0.337599E+09   0.470124E+09   0.420412E+09   0.163874E+09   0.162568E+09   0.219899E+09   0.470186E+09   0.470186E+09   0.412773E+09   0.196008E+09
   100000   0.397653E+09   0.448962E+09   0.348757E+09   0.440026E+09   0.428319E+09   0.159251E+09   0.144233E+09   0.209260E+09   0.437327E+09   0.453841E+09   0.463464E+09   0.192677E+09
  1000000   0.399835E+09   0.452608E+09   0.356962E+09   0.454932E+09   0.455237E+09   0.164419E+09   0.147127E+09   0.209768E+09   0.447012E+09   0.441371E+09   0.449242E+09   0.189593E+09
 10000000   0.391792E+09   0.451275E+09   0.346732E+09   0.447205E+09   0.446551E+09   0.163093E+09   0.145857E+09   0.209207E+09   0.452912E+09   0.456771E+09   0.453956E+09   0.184135E+09
100000000   0.383293E+09   0.451252E+09   0.351914E+09   0.453965E+09   0.454288E+09   0.163675E+09   0.144253E+09   0.208260E+09   0.450433E+09   0.447128E+09   0.452328E+09   0.188713E+09
```

#### Lookup table (`bit`)

```
#             is_control   is_printable       is_white       is_blank   is_graphical is_punctuation    is_alphanum       is_alpha       is_upper       is_lower       is_digit   is_hex_digit
     1000   0.435768E+09   0.438251E+09   0.383789E+09   0.416112E+09   0.401139E+09   0.439754E+09   0.369399E+09   0.350435E+09   0.400721E+09   0.439967E+09   0.439580E+09   0.440025E+09
    10000   0.441606E+09   0.441653E+09   0.441657E+09   0.441692E+09   0.441655E+09   0.441681E+09   0.380208E+09   0.386466E+09   0.434295E+09   0.447411E+09   0.441328E+09   0.423212E+09
   100000   0.423571E+09   0.454646E+09   0.437196E+09   0.412275E+09   0.470332E+09   0.443671E+09   0.377215E+09   0.385465E+09   0.439103E+09   0.441739E+09   0.438257E+09   0.415640E+09
  1000000   0.450630E+09   0.455206E+09   0.455765E+09   0.454546E+09   0.453904E+09   0.449706E+09   0.400598E+09   0.403505E+09   0.458734E+09   0.457389E+09   0.456458E+09   0.456329E+09
 10000000   0.440918E+09   0.455957E+09   0.459487E+09   0.438602E+09   0.436751E+09   0.444259E+09   0.397790E+09   0.399249E+09   0.453598E+09   0.454421E+09   0.454519E+09   0.457407E+09
100000000   0.453538E+09   0.455238E+09   0.455722E+09   0.455825E+09   0.447765E+09   0.451045E+09   0.396887E+09   0.396298E+09   0.450666E+09   0.453295E+09   0.454355E+09   0.454523E+09
```

#### C binding (`cctype`)

```
#             is_control   is_printable       is_white       is_blank   is_graphical is_punctuation    is_alphanum       is_alpha       is_upper       is_lower       is_digit   is_hex_digit
     1000   0.251092E+09   0.272035E+09   0.273329E+09   0.218780E+09   0.273306E+09   0.273351E+09   0.272755E+09   0.273299E+09   0.273314E+09   0.273306E+09   0.252385E+09   0.273321E+09
    10000   0.230432E+09   0.256219E+09   0.249654E+09   0.197767E+09   0.274221E+09   0.274275E+09   0.274265E+09   0.274283E+09   0.274283E+09   0.274272E+09   0.253188E+09   0.242044E+09
   100000   0.242549E+09   0.259654E+09   0.247580E+09   0.215712E+09   0.262890E+09   0.270165E+09   0.252719E+09   0.226700E+09   0.202675E+09   0.253355E+09   0.237485E+09   0.259957E+09
  1000000   0.244287E+09   0.260393E+09   0.257390E+09   0.204849E+09   0.259779E+09   0.259276E+09   0.260195E+09   0.254179E+09   0.261309E+09   0.263270E+09   0.242690E+09   0.257315E+09
 10000000   0.236185E+09   0.219963E+09   0.252501E+09   0.212385E+09   0.264635E+09   0.262989E+09   0.262331E+09   0.260359E+09   0.257619E+09   0.244248E+09   0.239207E+09   0.260912E+09
100000000   0.236846E+09   0.257114E+09   0.252510E+09   0.206398E+09   0.256275E+09   0.257716E+09   0.258873E+09   0.256519E+09   0.256447E+09   0.251607E+09   0.237740E+09   0.260600E+09
```

#### Reference in C++ (`cpp`)

Order of the functions is the same as in the Fortran benchmark.

```
     1000   2.564103e+08   2.500000e+08   2.222222e+08   2.941176e+08   2.564103e+08   2.564103e+08   2.564103e+08   2.564103e+08   2.857143e+08   2.500000e+08   2.564103e+08   2.564103e+08
    10000   2.392344e+08   2.364066e+08   2.702703e+08   3.300330e+08   3.472222e+08   3.424658e+08   2.865330e+08   4.629630e+08   4.975124e+08   5.291005e+08   5.319149e+08   5.319149e+08
   100000   5.157298e+08   5.246590e+08   5.162623e+08   5.089059e+08   5.138746e+08   5.396654e+08   5.149331e+08   5.485464e+08   5.485464e+08   5.232862e+08   5.373455e+08   5.175983e+08
  1000000   5.330206e+08   3.956948e+08   5.169027e+08   5.184302e+08   5.344450e+08   5.110645e+08   5.307856e+08   5.293246e+08   5.293806e+08   5.241640e+08   5.184571e+08   5.305321e+08
 10000000   5.227829e+08   4.664919e+08   5.221305e+08   5.279190e+08   4.673771e+08   5.135131e+08   5.192863e+08   5.231494e+08   4.846229e+08   5.259366e+08   5.285971e+08   5.272565e+08
100000000   4.973872e+08   4.798384e+08   4.087577e+08   4.466940e+08   4.857283e+08   5.001245e+08   5.078617e+08   3.863635e+08   4.847989e+08   5.007273e+08   5.022019e+08   4.965584e+08
```