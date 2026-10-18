!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Swap_ the indices of a rank-2 array without allocation

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE SwapUtility, ONLY: Swap_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: b(3, 4), a(5, 5)
INTEGER(I4B) :: i, j
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! b(i, j) = 10*i + j
DO j = 1, 4
  DO i = 1, 3
    b(i, j) = REAL(10 * i + j, DFP)
  END DO
END DO

! i1 = 2, i2 = 1: a(1:4, 1:3) is the transpose of b
a = -1.0_DFP
CALL Swap_(a=a, b=b, i1=2, i2=1)
CALL OK(ALL(ABS(a(1:4, 1:3) - TRANSPOSE(b)) .LT. tol), &
        "transpose: leading block")

! entries outside the leading block are not changed
CALL OK(ALL(ABS(a(5, :) + 1.0_DFP) .LT. tol), "row 5 untouched")
CALL OK(ALL(ABS(a(:, 4:5) + 1.0_DFP) .LT. tol), "columns 4:5 untouched")

! i1 = 1, i2 = 2: a(1:3, 1:4) is a copy of b
a = -1.0_DFP
CALL Swap_(a=a, b=b, i1=1, i2=2)
CALL OK(ALL(ABS(a(1:3, 1:4) - b) .LT. tol), "identity: leading block")
CALL OK(ALL(ABS(a(4:5, :) + 1.0_DFP) .LT. tol), "rows 4:5 untouched")

END PROGRAM main
