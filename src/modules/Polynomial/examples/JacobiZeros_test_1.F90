!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Return the n roots of a degree n polynomial.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE JacobiPolynomialUtility, ONLY: JacobiZeros
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: z(2)

z = JacobiZeros(alpha=0.0_DFP, beta=0.0_DFP, n=2_I4B)
CALL OK(MAXVAL(ABS(z*z-1.0_DFP/3.0_DFP)) &
  < 1.0E-11_DFP, "Roots satisfy the quadratic")
END PROGRAM main
