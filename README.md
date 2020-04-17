# fortran-ascii

Fortran functions for classifying and converting ASCII characters

## 

##

```
    print *, btest(ascii_table(iachar("#")),2), btest(ascii_table(iachar("A")),2)

    res = is_alpha(ascii_'\u00E4')
    print *, res, is_alpha('\à'), '\u00E4', ascii_'\u00E4', iachar(ascii_'\u00E4')

    print *, transfer(ascii_'\u00E4',i)
```