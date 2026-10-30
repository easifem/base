!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Dispatch basis Laplacians by polynomial family.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: poly
USE OrthogonalPolynomialUtility, ONLY: LaplacianEvalAllOrthopol_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(3)
REAL(DFP) :: y(3, 3)
INTEGER(I4B) :: nr, nc

x = [-0.5_DFP, 0.0_DFP, 0.5_DFP]
CALL LaplacianEvalAllOrthopol_(n=2_I4B, x=x, orthopol=poly%Monomial, &
  ans=y, nrow=nr, ncol=nc)
CALL OK(MAXVAL(ABS(y(:, 3)- &
  (2.0_DFP))) < 1.0E-11_DFP, "Monomial quadratic")
END PROGRAM main
