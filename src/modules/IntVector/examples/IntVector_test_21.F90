!> author: Vikas Sharma, Ph. D.
! date: 2026-11-03
! summary: Testing Copy_(x, x_start, y, y_start, y_end)

PROGRAM main
USE GlobalData, ONLY: I4B
USE IntVector_Method, ONLY: Copy_
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: x(5)

x = 0
CALL Copy_(x=x, x_start=2_I4B, y=[10_I4B, 20_I4B, 30_I4B, 40_I4B], &
           y_start=2_I4B, y_end=3_I4B)
CALL OK(ALL(x .EQ. [0, 20, 30, 0, 0]), "Copy_: x(2:3) = y(2:3)")

END PROGRAM main
