!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Tail of a real vector

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE TailUtility, ONLY: TAIL
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: x(5), y(1), t(4)

x = [1.5_DFP, 2.5_DFP, 3.5_DFP, 4.5_DFP, 5.5_DFP]
CALL OK(ALL(ABS(TAIL(x) - x(2:)) .LE. tol), "tail is x(2:)")
CALL OK(SIZE(TAIL(x)) .EQ. 4_I4B, "size of the tail")
t = TAIL(x)
CALL OK(ABS(t(1) - 2.5_DFP) .LE. tol, "first entry of tail")

! tail of the tail
CALL OK(ALL(ABS(TAIL(TAIL(x)) - x(3:)) .LE. tol), "tail of tail")

! vector with a single element gives an empty vector
y = [7.25_DFP]
CALL OK(SIZE(TAIL(y)) .EQ. 0_I4B, "tail of a vector of size 1")

END PROGRAM main
