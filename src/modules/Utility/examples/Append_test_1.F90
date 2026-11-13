PROGRAM main
USE GlobalData, ONLY: I4B, LGT, DFP
USE AppendUtility, ONLY: Append
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), ALLOCATABLE :: avec(:)

! Append scalar to a vector
CALL Append(avec, 1)
CALL OK(ALL(avec .EQ. [1]), "Append single ENTRY")

! Append a vector to a vector
CALL Append(avec, [2, 3])
CALL OK(ALL(avec .EQ. [1, 2, 3]), "Append a vector")

DEALLOCATE (avec)
END PROGRAM main
