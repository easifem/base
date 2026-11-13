!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Return the ratio of consecutive leading coefficients.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalLeadingCoeffRatio
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: y

y = UltrasphericalLeadingCoeffRatio(lambda=0.5_DFP, n=2_I4B)
CALL OK(ABS(y-(5.0_DFP/3.0_DFP)) < 1.0E-11_DFP, &
  "Degree two coefficient or norm")
END PROGRAM main
