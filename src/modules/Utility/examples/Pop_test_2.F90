!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Pop for real vectors

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE PushPopUtility, ONLY: Pop
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: vec(4), one(1)
REAL(DFP), ALLOCATABLE :: ans(:)
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

vec = [1.5_DFP, 2.5_DFP, 3.5_DFP, 4.5_DFP]

! pop the first entry
ans = Pop(vec, 1_I4B)
CALL OK(ALL(ABS(ans - [2.5_DFP, 3.5_DFP, 4.5_DFP]) .LT. tol), &
        "pop at pos=1")

! pop a middle entry
ans = Pop(vec, 3_I4B)
CALL OK(ALL(ABS(ans - [1.5_DFP, 2.5_DFP, 4.5_DFP]) .LT. tol), &
        "pop at pos=3")

! pop the last entry
ans = Pop(vec, 4_I4B)
CALL OK(ALL(ABS(ans - [1.5_DFP, 2.5_DFP, 3.5_DFP]) .LT. tol), &
        "pop at pos=4")

! a vector of size 1 gives an empty vector
one = [7.0_DFP]
ans = Pop(one, 1_I4B)
CALL OK(SIZE(ans) .EQ. 0_I4B, "pop from a vector of size 1")

END PROGRAM main
