!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Testing DubinerGradient_Quadrangle against central finite
! differences of Dubiner_Quadrangle

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE QuadrangleInterpolationUtility, ONLY: Dubiner_Quadrangle
USE QuadrangleInterpolationUtility, ONLY: DubinerGradient_Quadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: h = 1.0E-6_DFP, tol = 1.0E-7_DFP
INTEGER(I4B), PARAMETER :: order = 2, nb = 6
REAL(DFP) :: xij(2, 2), g(2, nb, 2), fp(2, nb), fm(2, nb), xh(2, 2)

xij(:, 1) = [0.2_DFP, -0.4_DFP]
xij(:, 2) = [-0.3_DFP, 0.5_DFP]

g = DubinerGradient_Quadrangle(order=order, xij=xij)

! Test 1: x derivative of all modes
BLOCK
  xh = xij
  xh(1, :) = xij(1, :) + h
  fp = Dubiner_Quadrangle(order=order, xij=xh)
  xh(1, :) = xij(1, :) - h
  fm = Dubiner_Quadrangle(order=order, xij=xh)
  CALL OK(MAXVAL(ABS(g(:, :, 1) - (fp - fm) / (2.0_DFP * h))) .LT. tol, &
          "order=2: d/dx agrees with finite differences")
END BLOCK

! Test 2: y derivative of all modes
BLOCK
  xh = xij
  xh(2, :) = xij(2, :) + h
  fp = Dubiner_Quadrangle(order=order, xij=xh)
  xh(2, :) = xij(2, :) - h
  fm = Dubiner_Quadrangle(order=order, xij=xh)
  CALL OK(MAXVAL(ABS(g(:, :, 2) - (fp - fm) / (2.0_DFP * h))) .LT. tol, &
          "order=2: d/dy agrees with finite differences")
END BLOCK

END PROGRAM main
