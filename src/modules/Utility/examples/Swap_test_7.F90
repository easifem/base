!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Swap the indices of a rank-2 array

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE SwapUtility, ONLY: Swap
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: b(3, 4)
REAL(DFP), ALLOCATABLE :: a(:, :)
INTEGER(I4B) :: i, j
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! b(i, j) = 10*i + j
DO j = 1, 4
  DO i = 1, 3
    b(i, j) = REAL(10 * i + j, DFP)
  END DO
END DO

! i1 = 1, i2 = 2: a is a copy of b
CALL Swap(a=a, b=b, i1=1, i2=2)
CALL OK(ALL(SHAPE(a) .EQ. [3, 4]), "identity: shape")
CALL OK(ALL(ABS(a - b) .LT. tol), "identity: a = b")

! i1 = 2, i2 = 1: a is the transpose of b
CALL Swap(a=a, b=b, i1=2, i2=1)
CALL OK(ALL(SHAPE(a) .EQ. [4, 3]), "transpose: shape")
CALL OK(ALL(ABS(a - TRANSPOSE(b)) .LT. tol), "transpose: a = b^T")
CALL OK(ABS(a(4, 3) - 34.0_DFP) .LT. tol, "transpose: a(4,3)")

! i1 = i2 is not a permutation: a is a copy of b
CALL Swap(a=a, b=b, i1=1, i2=1)
CALL OK(ALL(SHAPE(a) .EQ. [3, 4]), "i1 = i2: shape")
CALL OK(ALL(ABS(a - b) .LT. tol), "i1 = i2: a = b")

! an index out of range: a is a copy of b
CALL Swap(a=a, b=b, i1=3, i2=1)
CALL OK(ALL(ABS(a - b) .LT. tol), "out of range: a = b")

END PROGRAM main
