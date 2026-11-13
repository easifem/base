PROGRAM main
USE GlobalData, ONLY: I4B, LGT, DFP
USE AppendUtility, ONLY: Append
USE Test_Method, ONLY: OK
IMPLICIT NONE
INTEGER(I4B), ALLOCATABLE :: avec(:)
! Now lets us append a single ENTRY to a vector.
CALL Append(avec, 1, mask=.TRUE.)
CALL Append(avec, 1, mask=.FALSE.)
CALL OK(ALL(avec .EQ. [1]), "Append single ENTRY")
! Appending a vector to a vector.
CALL Append(avec, [2, 3], mask=[.TRUE., .TRUE.])
CALL Append(avec, [4, 5], mask=[.TRUE., .FALSE.])
CALL OK(ALL(avec .EQ. [1, 2, 3, 4]), "Append a vector")
END PROGRAM main
