PROGRAM main
USE easifemBase
IMPLICIT NONE
INTEGER(I4B), ALLOCATABLE :: intvec1(:)
REAL(DFP), ALLOCATABLE :: realvec1(:)

CALL Append(intvec1, 1)
CALL OK(ALL(intvec1 .EQ. [1]), "Append single ENTRY")

CALL Append(intvec1, [2, 3])
CALL OK(ALL(intvec1 .EQ. [1, 2, 3]), "Append a vector")

CALL Append(realvec1, 1.0_DFP)
CALL OK(ALL(realvec1 .EQ. [1.0_DFP]), "Append single ENTRY")

CALL Append(realvec1, [2.0_DFP, 3.0_DFP])
CALL OK(ALL(realvec1 .EQ. [1.0_DFP, 2.0_DFP, 3.0_DFP]), "Append a vector")

DEALLOCATE (intvec1, realvec1)
END PROGRAM main
