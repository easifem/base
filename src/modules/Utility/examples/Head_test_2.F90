!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: HEAD of a real vector

PROGRAM main
USE GlobalData, ONLY: DFP
USE HeadUtility, ONLY: HEAD
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: x(4), y(1), z(3)

x = [1.5_DFP, 2.5_DFP, 3.5_DFP, 4.5_DFP]
CALL OK(ABS(HEAD(x) - 1.5_DFP) .LE. tol, "head of a vector")
CALL OK(HEAD(x) .EQ. x(1), "head is x(1)")
CALL OK(HEAD(x) .NE. x(SIZE(x)), "head is not the last element")

! array section
CALL OK(ABS(HEAD(x(2:3)) - 2.5_DFP) .LE. tol, "head of x(2:3)")

! vector with a single element
y = [7.25_DFP]
CALL OK(ABS(HEAD(y) - 7.25_DFP) .LE. tol, "head of a vector of size 1")

! negative values
z = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
CALL OK(ABS(HEAD(z) + 0.5_DFP) .LE. tol, "head with negative entry")

END PROGRAM main
