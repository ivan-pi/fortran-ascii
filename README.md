# Fortran functions which operate on ASCII characters

This repository contains Fortran routines aiming to reproduce the functionality found in [`<ctype.h>`](https://en.wikipedia.org/wiki/C_character_classification) header of the C standard library or [std.ascii](https://dlang.org/phobos/std_ascii.html) namespace from the D runtime library.

Currently, this is just my personal testing ground that should later become a pull request to the Fortran [stdlib] project (see also issue https://github.com/fortran-lang/stdlib/issues/11 and my previous pull requests https://github.com/fortran-lang/stdlib/pull/32, https://github.com/fortran-lang/stdlib/pull/49).

TODO: 
 * Unicode support
 * Test wrapping the C++ library routines

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
pure module logical function is_alphanum(c)
  character(len=1), intent(in) :: c !! The character to test.
  is_alphanum = (c >= '0' .and. c <= '9') .or. (c >= 'a' .and. c <= 'z') &
      .or. (c >= 'A' .and. c <= 'Z')
end function
```

For characters with no guarantee on position in the collating sequence we use the `iachar` intrinsic to check the position in the ASCII character set.

Example for `is_control`:

```fortran
pure module logical function is_control(c)
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
pure module logical function is_alphanum(c)
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
pure module logical function is_control(c)
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

This approach can be mimicked also in Fortran. Since there are 13 functions (excluding `is_ascii`) we require an integer with a storage size of at least 13 bits. We can generate the lookup table using either the "direct" or "select-case" approach (having verified its correctness first) to set the individual bit values in the lookup table.

The following program generates the table and outputs the result as a formatted array:
```fortran
program generate_ascii_table

    use iso_fortran_env, only: int16
    use fortran_ascii
    implicit none

    integer(int64) :: ascii_table(0:127), i
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
    
    write(*,'(A1,128(I0,:,","))',advance='no') "[",(ascii_table(i),i=0,127)
    write(*,'(A1)') "]"

end program
``` 

The output of this program is:
```
[16,16,16,16,16,16,16,16,16,400,144,144,144,144,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,448,104,104,104,104,104,104,104,104,104,104,104,104,104,104,104,6246,6246,6246,6246,6246,6246,6246,6246,4198,4198,104,104,104,104,104,104,104,5221,5221,5221,5221,5221,5221,1125,1125,1125,1125,1125,1125,1125,1125,1125,1125,1125,1125,1125,1125,1125,1125,1125,1125,1125,1125,104,104,104,104,104,104,4709,4709,4709,4709,4709,4709,613,613,613,613,613,613,613,613,613,613,613,613,613,613,613,613,613,613,613,613,104,104,104,104,16]
```

We can also diplay the bit patterns and characters (only the printable ones excluding the space character) by adding the following code to the program above:
```fortran
    do i = 0, 127
        c = achar(i)
        if (is_graphical(c)) then
            write(*,'(I3,3X,A,3X,I4,3X,B0.13)') i, c, ascii_table(i), ascii_table(i)
        end if
    end do
```
<details>
  <summary>Click here to see the result.</summary>
  
```
 33   !    104   0000001101000
 34   "    104   0000001101000
 35   #    104   0000001101000
 36   $    104   0000001101000
 37   %    104   0000001101000
 38   &    104   0000001101000
 39   '    104   0000001101000
 40   (    104   0000001101000
 41   )    104   0000001101000
 42   *    104   0000001101000
 43   +    104   0000001101000
 44   ,    104   0000001101000
 45   -    104   0000001101000
 46   .    104   0000001101000
 47   /    104   0000001101000
 48   0   6246   1100001100110
 49   1   6246   1100001100110
 50   2   6246   1100001100110
 51   3   6246   1100001100110
 52   4   6246   1100001100110
 53   5   6246   1100001100110
 54   6   6246   1100001100110
 55   7   6246   1100001100110
 56   8   4198   1000001100110
 57   9   4198   1000001100110
 58   :    104   0000001101000
 59   ;    104   0000001101000
 60   <    104   0000001101000
 61   =    104   0000001101000
 62   >    104   0000001101000
 63   ?    104   0000001101000
 64   @    104   0000001101000
 65   A   5221   1010001100101
 66   B   5221   1010001100101
 67   C   5221   1010001100101
 68   D   5221   1010001100101
 69   E   5221   1010001100101
 70   F   5221   1010001100101
 71   G   1125   0010001100101
 72   H   1125   0010001100101
 73   I   1125   0010001100101
 74   J   1125   0010001100101
 75   K   1125   0010001100101
 76   L   1125   0010001100101
 77   M   1125   0010001100101
 78   N   1125   0010001100101
 79   O   1125   0010001100101
 80   P   1125   0010001100101
 81   Q   1125   0010001100101
 82   R   1125   0010001100101
 83   S   1125   0010001100101
 84   T   1125   0010001100101
 85   U   1125   0010001100101
 86   V   1125   0010001100101
 87   W   1125   0010001100101
 88   X   1125   0010001100101
 89   Y   1125   0010001100101
 90   Z   1125   0010001100101
 91   [    104   0000001101000
 92   \    104   0000001101000
 93   ]    104   0000001101000
 94   ^    104   0000001101000
 95   _    104   0000001101000
 96   `    104   0000001101000
 97   a   4709   1001001100101
 98   b   4709   1001001100101
 99   c   4709   1001001100101
100   d   4709   1001001100101
101   e   4709   1001001100101
102   f   4709   1001001100101
103   g    613   0001001100101
104   h    613   0001001100101
105   i    613   0001001100101
106   j    613   0001001100101
107   k    613   0001001100101
108   l    613   0001001100101
109   m    613   0001001100101
110   n    613   0001001100101
111   o    613   0001001100101
112   p    613   0001001100101
113   q    613   0001001100101
114   r    613   0001001100101
115   s    613   0001001100101
116   t    613   0001001100101
117   u    613   0001001100101
118   v    613   0001001100101
119   w    613   0001001100101
120   x    613   0001001100101
121   y    613   0001001100101
122   z    613   0001001100101
123   {    104   0000001101000
124   |    104   0000001101000
125   }    104   0000001101000
126   ~    104   0000001101000
```
</details>

We can immediately see that many character share the same bit pattern (i.e., same properties). To reduce storage requirements we can try and shrink the lookup table into a set of unique values using the [functional-fortran](https://wavebitscientific.github.io/functional-fortran/) module. The `pack` intrinsic then helps us find the ASCII character indexes which correspond to a certain bitmask:

```fortran
program reduce_ascii_table
use iso_fortran_env, only: int16
use mod_functional, only: set
implicit none

integer(int16), parameter :: full_table(0:127) = [16,16,16,16,16,16,16,16,16,400,144,&144,144,144,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,448,104,104,&104,104,104,104,104,104,104,104,104,104,104,104,104,6246,6246,6246,6246,6246,&
6246,6246,6246,4198,4198,104,104,104,104,104,104,104,5221,5221,5221,5221,5221,&
5221,1125,1125,1125,1125,1125,1125,1125,1125,1125,1125,1125,1125,1125,1125,&
1125,1125,1125,1125,1125,1125,104,104,104,104,104,104,4709,4709,4709,4709,&
4709,4709,613,613,613,613,613,613,613,613,613,613,613,613,613,613,613,613,613,&
613,613,613,104,104,104,104,16]

integer(int16), allocatable :: reduced_table(:)
integer(int16) :: i, j

reduced_table = set(full_table)

write(*,'(/,A)') "Reduced table:"
write(*,'(A1,*(I0,:,","))',advance='no') "[",reduced_table
write(*,'(A1)') "]"

write(*,'(/,A)') "Indexes:"
do i=1,size(reduced_table)
    write(*,'(*(I0,:,","))') pack([(j,j=0,127)],full_table==reduced_table(i))
end do

end program
```
The output is:
```
Reduced table:
16,400,144,448,104,6246,4198,5221,1125,4709,613

Indexes:
0,1,2,3,4,5,6,7,8,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,127
9
10,11,12,13
32
33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,58,59,60,61,62,63,64,91,92,93,94,95,96,123,124,125,126
48,49,50,51,52,53,54,55
56,57
65,66,67,68,69,70
71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90
97,98,99,100,101,102
103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122
```

We can incorporate these indexes into a select case construct.


### C binding

For compilers such as gfortran and ifort, the final executable is typically linked to the C standard library. This means we can easily borrow the functionality from C. First we create a module that defines the interfaces of the functions in `<ctype.h>`:

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

    ...

  end interface
end module
```

Then we use the `iachar` intrinsic to convert the Fortran character to a C integer and call the C routines. The return value of the C routines is different from zero (i.e., `true`) if the character belongs to a certain subset, and zero (i.e. `false`) otherwise.

Example for `is_alphanum`:
```fortran
pure module logical function is_alphanum(c)
  character(len=1), intent(in) :: c !! The character to test.
  is_alphanum = isalnum(iachar(c,c_int)) /= 0
end function
```

Example for `is_control`:
```fortran
pure module logical function is_control(c)
  character(len=1), intent(in) :: c !! The character to test.
  is_control = iscntrl(iachar(c,c_int)) /= 0
end function
```

