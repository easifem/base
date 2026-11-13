!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Expand one polynomial in monomials.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1MonomialExpansion
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: c(3)

c = Chebyshev1MonomialExpansion(n=2_I4B)
CALL OK(MAXVAL(ABS(c- &
  ([-1.0_DFP, 0.0_DFP, 2.0_DFP]))) < 1.0E-11_DFP, &
  "Coefficients are ordered by increasing power")
END PROGRAM main
