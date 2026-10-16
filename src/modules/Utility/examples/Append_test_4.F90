```fortran
PROGRAM main
    USE easifemBase
    IMPLICIT NONE
    INTEGER(I4B), ALLOCATABLE :: intvec1(:)
```

Now lets us append a single ENTRY to a vector.

```fortran
CALL Append(intvec1, 1, mask=.true.)
CALL Append(intvec1, 1, mask=.false.)
CALL OK( ALL(intvec1 .EQ. [1]), "Append single ENTRY"  )
```

Appending a vector to a vector.

```fortran
CALL Append(intvec1, [2,3], mask=[.true., .true.])
CALL Append(intvec1, [4,5], mask=[.true., .false.])
CALL OK( ALL(intvec1 .EQ. [1,2,3,4]), "Append a vector"  )
```

```fortran
END PROGRAM main
```
