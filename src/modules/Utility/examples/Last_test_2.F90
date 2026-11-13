!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Last element of a real vector

PROGRAM main
USE GlobalData, ONLY: DFP
USE TailUtility, ONLY: LAST
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: x(3), y(1)

x = [1.0_DFP, 2.0_DFP, 3.0_DFP]
CALL OK(ABS(LAST(x) - 3.0_DFP) .LE. tol, "last of a vector")
CALL OK(LAST(x) .EQ. x(SIZE(x)), "last is x(SIZE(x))")

! array section
CALL OK(ABS(LAST(x(1:2)) - 2.0_DFP) .LE. tol, "last of x(1:2)")

! vector with a single element
y = [7.25_DFP]
CALL OK(ABS(LAST(y) - 7.25_DFP) .LE. tol, "last of a vector of size 1")

END PROGRAM main
