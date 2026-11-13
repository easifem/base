!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Return the monomial exponent table for an element.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line
USE LagrangePolynomialUtility, ONLY: LagrangeDegree
USE Test_Method, ONLY: OK
IMPLICIT NONE
INTEGER(I4B), ALLOCATABLE :: degree(:, :)

degree = LagrangeDegree(order=2_I4B, elemType=Line)
CALL OK(ALL(SHAPE(degree) == [3, 1]), "One exponent per monomial")
CALL OK(ALL(degree(:, 1) == [0_I4B, 1_I4B, 2_I4B]), &
  "Increasing line monomial powers")
END PROGRAM main
