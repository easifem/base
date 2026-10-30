!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Build a Jacobi matrix from monic recurrence coefficients.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE OrthogonalPolynomialUtility, ONLY: JacobiMatrix
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: a(0:1), b(0:1), d(2), e(1)

a = 0.0_DFP
b = [2.0_DFP, 1.0_DFP/3.0_DFP]
CALL JacobiMatrix(alphaCoeff=a, betaCoeff=b, D=d, E=e)
CALL OK(MAXVAL(ABS(d)) < 1.0E-11_DFP, "Diagonal coefficients")
CALL OK(ABS(e(1)-SQRT(1.0_DFP/3.0_DFP)) < 1.0E-11_DFP, &
  "Off diagonal coefficient")
END PROGRAM main
