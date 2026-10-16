```fortran
PROGRAM main
    USE easifemBase
    IMPLICIT NONE
    INTEGER(I4B), ALLOCATABLE :: C(:), B(:), A(:)
```

```fortran title="A and B are vectors"
  A = [1, 2]
  B = [3, 4]
  !!
  !! Append B(1) and B(2) to A and assign to C.
  !!
  CALL Append(C, A, B, mask=[.true., .true.]) 
  CALL OK( ALL(C .EQ. [1, 2, 3, 4]), "Append:"  )

  !!
  !! Append B(1) to A and assign to C.
  !!
  CALL Append(C, A, B, mask=[.true., .false.]) 
  CALL OK( ALL(C .EQ. [1, 2, 3]), "Append:"  )


  !!
  !! Do not append B to A, just assign A to C.
  !!
  CALL Append(C, A, B, mask=[.false., .false.]) 
  CALL OK( ALL(C .EQ. [1, 2]), "Append:" )
```

```fortran title="A is vector B is scalar"
  !!
  !! Append 3 to A and then assign result to C
  !!
  CALL Append(C, A, 3, mask=.true.)
  CALL OK( ALL(C .EQ. [1, 2, 3]), "Append:"  )

  !!
  !! Do not append 3 to A, assign result to C, that is C =A
  !!
  CALL Append(C, A, 3, mask=.false.)
  CALL OK( ALL(C .EQ. [1, 2]), "Append:"  )
```

```fortran
END PROGRAM main
```
