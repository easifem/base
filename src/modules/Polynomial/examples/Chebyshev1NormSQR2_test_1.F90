!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Return weighted squared norms from degree zero through n.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1NormSQR2
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: y(0:2)

y = Chebyshev1NormSQR2(n=2_I4B)
CALL OK(ABS(y(2)-(ACOS(-1.0_DFP)/2.0_DFP)) < 1.0E-11_DFP, &
  "Norm array includes degree two")
END PROGRAM main
