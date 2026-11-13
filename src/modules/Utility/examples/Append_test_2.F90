PROGRAM main
USE GlobalData, ONLY: I4B, LGT, DFP
USE AppendUtility, ONLY: Append
USE AppendUtility, ONLY: OPERATOR(.APPEND.)
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), ALLOCATABLE :: avec(:)

! lets us append a single ENTRY to a vector of integers.
avec = [1] .APPEND.2
CALL OK(ALL(avec .EQ. [1, 2]), "Append single ENTRY")

! Appending a vector to a vector.
avec = avec.APPEND. [3]
CALL OK(ALL(avec .EQ. [1, 2, 3]), "Append a vector")

! Cleanup
DEALLOCATE (avec)
END PROGRAM main
