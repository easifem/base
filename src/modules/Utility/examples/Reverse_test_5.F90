!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Reverse a real rank-3 array along a dimension

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ReverseUtility, ONLY: Reverse
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: a(3, 3, 3), a0(3, 3, 3), b(3, 3, 3)
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
INTEGER(I4B) :: ii

a0 = RESHAPE([(REAL(ii, DFP), ii=1, 27)], [3, 3, 3])

! dim=1 on the whole array
a = a0
CALL Reverse(a, 1_I4B, 3_I4B, 1_I4B, 3_I4B, 1_I4B, 3_I4B, 1_I4B)
b = a0(3:1:-1, :, :)
CALL OK(ALL(ABS(a - b) .LT. tol), "dim=1")

! dim=2 on the whole array
a = a0
CALL Reverse(a, 1_I4B, 3_I4B, 1_I4B, 3_I4B, 1_I4B, 3_I4B, 2_I4B)
b = a0(:, 3:1:-1, :)
CALL OK(ALL(ABS(a - b) .LT. tol), "dim=2")

! dim=3 on the whole array
a = a0
CALL Reverse(a, 1_I4B, 3_I4B, 1_I4B, 3_I4B, 1_I4B, 3_I4B, 3_I4B)
b = a0(:, :, 3:1:-1)
CALL OK(ALL(ABS(a - b) .LT. tol), "dim=3")

! dim=3 on a block, other entries are untouched
a = a0
CALL Reverse(a, 1_I4B, 2_I4B, 2_I4B, 3_I4B, 1_I4B, 3_I4B, 3_I4B)
b = a0
b(1:2, 2:3, :) = a0(1:2, 2:3, 3:1:-1)
CALL OK(ALL(ABS(a - b) .LT. tol), "dim=3 on a(1:2, 2:3, 1:3)")

END PROGRAM main
