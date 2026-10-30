!> author: OpenAI Codex
! date: 2026-10-30
! summary: Project sampled values onto the polynomial basis.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Gauss
USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalTransform_
USE UltrasphericalPolynomialUtility, ONLY: UltrasphericalGaussQuadrature
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(3), w(3), f(3), c(0:2)
INTEGER(I4B) :: ns

CALL UltrasphericalGaussQuadrature(lambda=0.5_DFP, n=3_I4B, pt=x, wt=w)
f = 0.5_DFP*(3.0_DFP*x*x-1.0_DFP)
CALL UltrasphericalTransform_( &
  lambda=0.5_DFP, n=2_I4B, &
  coeff=f, &
  x=x, w=w, &
  quadType=Gauss, ans=c, tsize=ns)
CALL OK(MAXVAL(ABS(c- &
  [0.0_DFP, 0.0_DFP, 1.0_DFP])) < 1.0E-10_DFP, &
  "Recover the single degree two modal coefficient")
END PROGRAM main
