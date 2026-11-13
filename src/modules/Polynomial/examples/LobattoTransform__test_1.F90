!> author: Vikas Sharma, Ph.D.
! date: 2026-10-30
! summary: Project sampled values onto the polynomial basis.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Gauss
USE LobattoPolynomialUtility, ONLY: LobattoTransform_
USE LegendrePolynomialUtility, ONLY: LegendreGaussQuadrature
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP) :: x(4), w(4), f(4), c(0:2)
INTEGER(I4B) :: ns

CALL LegendreGaussQuadrature(n=4_I4B, pt=x, wt=w)
f = 1.5_DFP*(x*x-1.0_DFP)/SQRT(6.0_DFP)
CALL LobattoTransform_( &
  n=2_I4B, coeff=f, x=x, w=w, quadType=Gauss, ans=c, tsize=ns)
CALL OK(MAXVAL(ABS(c- &
  [0.0_DFP, 0.0_DFP, 1.0_DFP])) < 1.0E-10_DFP, &
  "Recover the single degree two modal coefficient")
END PROGRAM main
