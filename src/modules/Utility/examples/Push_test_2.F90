!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Push for real vectors

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE PushPopUtility, ONLY: Push
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: vec(3)
REAL(DFP), ALLOCATABLE :: ans(:)
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

vec = [1.5_DFP, 2.5_DFP, 3.5_DFP]

! push at the front
ans = Push(vec, 1_I4B, -1.0_DFP)
CALL OK(ALL(ABS(ans - [-1.0_DFP, 1.5_DFP, 2.5_DFP, 3.5_DFP]) .LT. tol), &
        "push at pos=1")

! push in the middle
ans = Push(vec, 2_I4B, -1.0_DFP)
CALL OK(ALL(ABS(ans - [1.5_DFP, -1.0_DFP, 2.5_DFP, 3.5_DFP]) .LT. tol), &
        "push at pos=2")

! pos = SIZE(vec) + 1 appends
ans = Push(vec, 4_I4B, -1.0_DFP)
CALL OK(ALL(ABS(ans - [1.5_DFP, 2.5_DFP, 3.5_DFP, -1.0_DFP]) .LT. tol), &
        "push at pos=4")

! the result has one more entry
CALL OK(SIZE(ans) .EQ. 4_I4B, "size of the result")

END PROGRAM main
