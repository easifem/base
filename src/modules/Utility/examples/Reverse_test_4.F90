!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Reverse a real matrix along a dimension

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ReverseUtility, ONLY: Reverse
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: a(3, 3), a0(3, 3), b(3, 3)
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
INTEGER(I4B) :: ii

a0 = RESHAPE([(REAL(ii, DFP), ii=1, 9)], [3, 3])

! dim=1: flip the matrix upside down
a = a0
CALL Reverse(a, 1_I4B, 3_I4B, 1_I4B, 3_I4B, 1_I4B)
b = a0(3:1:-1, :)
CALL OK(ALL(ABS(a - b) .LT. tol), "dim=1 on the whole matrix")

! dim=2: flip the matrix left to right
a = a0
CALL Reverse(a, 1_I4B, 3_I4B, 1_I4B, 3_I4B, 2_I4B)
b = a0(:, 3:1:-1)
CALL OK(ALL(ABS(a - b) .LT. tol), "dim=2 on the whole matrix")

! dim=2 on a block, other entries are untouched
a = a0
CALL Reverse(a, 2_I4B, 3_I4B, 2_I4B, 3_I4B, 2_I4B)
b = a0
b(2:3, 2:3) = a0(2:3, 3:2:-1)
CALL OK(ALL(ABS(a - b) .LT. tol), "dim=2 on a(2:3, 2:3)")

END PROGRAM main
