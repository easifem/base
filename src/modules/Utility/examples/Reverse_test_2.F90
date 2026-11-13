!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Reverse a part of a real vector in place

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ReverseUtility, ONLY: Reverse
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: a(5), b(5)
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! reverse the whole vector (odd length)
a = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP]
CALL Reverse(a, 1_I4B, 5_I4B)
b = [5.0_DFP, 4.0_DFP, 3.0_DFP, 2.0_DFP, 1.0_DFP]
CALL OK(ALL(ABS(a - b) .LT. tol), "reverse a(1:5)")

! reverse the last four entries (even length)
a = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP]
CALL Reverse(a, 2_I4B, 5_I4B)
b = [1.0_DFP, 5.0_DFP, 4.0_DFP, 3.0_DFP, 2.0_DFP]
CALL OK(ALL(ABS(a - b) .LT. tol), "reverse a(2:5)")
CALL OK(ABS(a(1) - 1.0_DFP) .LT. tol, "a(1) is untouched")

! reverse two entries
a = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP]
CALL Reverse(a, 1_I4B, 2_I4B)
b = [2.0_DFP, 1.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP]
CALL OK(ALL(ABS(a - b) .LT. tol), "reverse a(1:2)")

END PROGRAM main
