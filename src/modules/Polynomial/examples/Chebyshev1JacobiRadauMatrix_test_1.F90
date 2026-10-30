!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Build the symmetric tridiagonal Radau Jacobi matrix.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Chebyshev1PolynomialUtility, ONLY: Chebyshev1JacobiRadauMatrix
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: d(3), e(2)

CALL Chebyshev1JacobiRadauMatrix(a=-1.0_DFP, n=2_I4B, D=d, E=e)
CALL OK(ALL(e > 0.0_DFP), "Positive off diagonal")
END PROGRAM main
