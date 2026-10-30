!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Build the symmetric tridiagonal Lobatto Jacobi matrix.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1JacobiLobattoMatrix
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: d(4), e(3)

CALL Chebyshev1JacobiLobattoMatrix(n=2_I4B, D=d, E=e)
CALL OK(ALL(e > 0.0_DFP), "Positive off diagonal")
CALL OK(MAXVAL(ABS(d)) < 1.0E-11_DFP, &
  "Symmetric weight has a zero diagonal")
END PROGRAM main
