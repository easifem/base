!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Testing LagrangeGradientEvalAll_Quadrangle for order 2 against
! finite differences and for different auxiliary bases

PROGRAM main
USE BaseType, ONLY: poly => TypePolynomialOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE QuadrangleInterpolationUtility, ONLY: EquidistancePoint_Quadrangle
USE QuadrangleInterpolationUtility, ONLY: LagrangeEvalAll_Quadrangle
USE QuadrangleInterpolationUtility, &
  ONLY: LagrangeGradientEvalAll_Quadrangle
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP, h = 1.0E-6_DFP, &
                        tolfd = 1.0E-7_DFP
REAL(DFP), ALLOCATABLE :: nodes(:, :)
REAL(DFP) :: x(2, 2), gm(2, 9, 2)

! Quadratic equidistant nodes on the biunit square, VEFC layout
nodes = EquidistancePoint_Quadrangle(order=2_I4B)

x(:, 1) = [0.3_DFP, -0.2_DFP]
x(:, 2) = [-0.7_DFP, 0.45_DFP]

gm = LagrangeGradientEvalAll_Quadrangle(order=2_I4B, x=x, xij=nodes, &
                                        basisType=poly%monomial)

! Test 1: the gradients of all polynomials add up to zero
BLOCK
  CALL OK(MAXVAL(ABS(SUM(gm, DIM=2))) .LT. tol, &
          "sum of gradients is zero")
END BLOCK

! Test 2: gradients agree with central finite differences
BLOCK
  REAL(DFP) :: xh(2, 2), fp(2, 9), fm(2, 9)

  xh = x
  xh(1, :) = x(1, :) + h
  fp = LagrangeEvalAll_Quadrangle(order=2_I4B, x=xh, xij=nodes)
  xh(1, :) = x(1, :) - h
  fm = LagrangeEvalAll_Quadrangle(order=2_I4B, x=xh, xij=nodes)
  CALL OK(MAXVAL(ABS(gm(:, :, 1) - (fp - fm) / (2.0_DFP * h))) .LT. tolfd, &
          "d/dx agrees with finite differences")

  xh = x
  xh(2, :) = x(2, :) + h
  fp = LagrangeEvalAll_Quadrangle(order=2_I4B, x=xh, xij=nodes)
  xh(2, :) = x(2, :) - h
  fm = LagrangeEvalAll_Quadrangle(order=2_I4B, x=xh, xij=nodes)
  CALL OK(MAXVAL(ABS(gm(:, :, 2) - (fp - fm) / (2.0_DFP * h))) .LT. tolfd, &
          "d/dy agrees with finite differences")
END BLOCK

! Test 3: the gradients do not depend on the auxiliary basis
BLOCK
  REAL(DFP) :: gl(2, 9, 2), gh(2, 9, 2)

  gl = LagrangeGradientEvalAll_Quadrangle(order=2_I4B, x=x, xij=nodes, &
                                          basisType=poly%legendre)
  gh = LagrangeGradientEvalAll_Quadrangle(order=2_I4B, x=x, xij=nodes, &
                                          basisType=poly%hierarchical)
  CALL OK(MAXVAL(ABS(gm - gl)) .LT. tol, &
          "monomial and Legendre give the same gradients")
  CALL OK(MAXVAL(ABS(gm - gh)) .LT. tol, &
          "monomial and hierarchical give the same gradients")
END BLOCK

END PROGRAM main
