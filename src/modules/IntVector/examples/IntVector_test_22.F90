!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: Testing Copy_(x, y)

PROGRAM main
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: Copy_
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: x(4)

x = 0
CALL Copy_(x=x, y=[7_I4B, 8_I4B])
CALL OK(ALL(x .EQ. [7, 8, 0, 0]), "Copy_: x(1:2) = y")

END PROGRAM main
