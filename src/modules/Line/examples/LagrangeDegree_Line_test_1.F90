!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Monomial exponents of the Lagrange basis on a line.

PROGRAM main
USE GlobalData, ONLY: I4B
USE LineInterpolationUtility, ONLY: LagrangeDegree_Line
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: cubic basis has exponents 0, 1, 2, 3
BLOCK
  INTEGER(I4B), ALLOCATABLE :: degree(:, :)
  degree = LagrangeDegree_Line(3_I4B)
  CALL OK(ALL(SHAPE(degree) == [4, 1]), "Shape is 4 by 1")
  CALL OK(ALL(degree(:, 1) == [0, 1, 2, 3]), "Exponents 0 to 3")
END BLOCK

! Test 2: order 0 has only the constant
BLOCK
  INTEGER(I4B), ALLOCATABLE :: degree(:, :)
  degree = LagrangeDegree_Line(0_I4B)
  CALL OK(ALL(SHAPE(degree) == [1, 1]) .AND. degree(1, 1) == 0, &
          "Order 0: one exponent, 0")
END BLOCK

END PROGRAM main
