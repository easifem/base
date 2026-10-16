```fortran
PROGRAM main
    USE easifemBase
    IMPLICIT NONE
    INTEGER(I4B), ALLOCATABLE :: C(:), B(:), A(:)
```

```fortran title="A and B are vectors"
A = [1]
B = [2]
CALL Append(C, A, B)
CALL OK( ALL(C .EQ. [1, 2]), "tests(1):"  )
```

```fortran title="A is vector B is scalar"
A = [1]
CALL Append(C, A, 2)
CALL OK( ALL(C .EQ. [1, 2]), "tests(2):"  )
```

```fortran
END PROGRAM main
```
