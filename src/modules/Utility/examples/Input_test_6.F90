!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Input for real matrices (rank-2 arrays)

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE InputUtility, ONLY: Input
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP) :: a(2, 3), b(2, 3), c(2, 3)
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

a = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP], &
            [2_I4B, 3_I4B])
b = 0.5_DFP * a

! option is present
c = Input(a, b)
CALL OK(ALL(ABS(c - b) .LT. tol), "option is present")

! option is absent
c = Input(a)
CALL OK(ALL(ABS(c - a) .LT. tol), "option is absent")

! the shape of the result is the shape of default
CALL OK(ALL(SHAPE(Input(a)) .EQ. [2_I4B, 3_I4B]), "shape of the result")
CALL OK(ALL(SHAPE(Input(a, b)) .EQ. [2_I4B, 3_I4B]), &
        "shape of the result with option")

! array sections
CALL OK(ALL(ABS(Input(a(:, 1:2), b(:, 2:3)) - b(:, 2:3)) .LT. tol), &
        "option is an array section")
CALL OK(ALL(ABS(Input(a(:, 2:3)) - a(:, 2:3)) .LT. tol), &
        "default is an array section")

END PROGRAM main
