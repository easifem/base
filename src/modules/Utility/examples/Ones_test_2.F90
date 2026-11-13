!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Ones for a real vector

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE OnesUtility, ONLY: Ones
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), ALLOCATABLE :: a(:)
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

a = Ones(4_I4B, 1.0_DFP)
CALL OK(SIZE(a) .EQ. 4_I4B, "size of the result")
CALL OK(ALL(ABS(a - 1.0_DFP) .LT. tol), "all entries are one")

! only the kind of datatype matters, not its value
a = Ones(3_I4B, 0.0_DFP)
CALL OK(ALL(ABS(a - 1.0_DFP) .LT. tol), "value of datatype is ignored")
CALL OK(KIND(Ones(3_I4B, 0.0_DFP)) .EQ. DFP, "kind of the result")

! zero size
a = Ones(0_I4B, 1.0_DFP)
CALL OK(SIZE(a) .EQ. 0_I4B, "zero size")

END PROGRAM main
