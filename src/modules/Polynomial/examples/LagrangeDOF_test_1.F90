!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Count total Lagrange interpolation DOFs.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line
USE LagrangePolynomialUtility, ONLY: LagrangeDOF
USE Test_Method, ONLY: OK
IMPLICIT NONE
INTEGER(I4B) :: ndof

ndof = LagrangeDOF(order=2_I4B, elemType=Line)
CALL OK(ndof == 3, "Quadratic line degree count")
END PROGRAM main
