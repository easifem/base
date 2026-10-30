!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Return the ratio of consecutive leading coefficients.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1LeadingCoeffRatio
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: y

y = Chebyshev1LeadingCoeffRatio(n=2_I4B)
CALL OK(ABS(y-(2.0_DFP)) < 1.0E-11_DFP, &
  "Degree two coefficient or norm")
END PROGRAM main
