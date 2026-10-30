!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Count interior Lagrange interpolation DOFs.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Line
USE LagrangePolynomialUtility, ONLY: LagrangeInDOF
USE Test_Method, ONLY: OK
IMPLICIT NONE
INTEGER(I4B) :: ndof

ndof = LagrangeInDOF(order=2_I4B, elemType=Line)
CALL OK(ndof == 1, "Quadratic line degree count")
END PROGRAM main
