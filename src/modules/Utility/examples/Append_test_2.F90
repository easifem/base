PROGRAM main
USE GlobalData, ONLY: I4B, LGT, DFP
USE AppendUtility, ONLY: Append
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), ALLOCATABLE :: intvec1(:)
REAL(DFP), ALLOCATABLE :: realvec1(:)

! Now lets us append a single ENTRY to a vector of integers.

intvec1 = [1] .append.2
CALL OK(ALL(intvec1 .EQ. [1, 2]), "Append single ENTRY")

! Appending a vector to a vector.

intvec1 = intvec1.append. [3]
CALL OK(ALL(intvec1 .EQ. [1, 2, 3]), "Append a vector")

! Now lets us append a single real value to realvec.

realvec1 = [1.0_DFP] .append.2.0_DFP
CALL OK(ALL(realvec1 .EQ. [1.0_DFP, 2.0_DFP]), "Append single ENTRY")

! Appending vector to a vector.

realvec1 = realvec1.append. [3.0_DFP]
CALL OK(ALL(realvec1 .EQ. [1.0_DFP, 2.0_DFP, 3.0_DFP]), "Append a vector")

! Cleanup

DEALLOCATE (intvec1, realvec1)
END PROGRAM main
