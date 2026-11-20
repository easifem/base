!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing LagrangeEvalAll_Triangle at one point and for several
! basis types

PROGRAM main
USE BaseType, ONLY: poly => TypePolynomialOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TriangleInterpolationUtility, ONLY: EquidistancePoint_Triangle
USE TriangleInterpolationUtility, ONLY: LagrangeEvalAll_Triangle
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: order = 2, n = 6
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), ALLOCATABLE :: xij(:, :)

xij = EquidistancePoint_Triangle(order=order)

! Test 1: one point, delta property at a node
BLOCK
  REAL(DFP) :: ans(n), delta(n)

  ans = LagrangeEvalAll_Triangle(order=order, x=xij(:, 5), xij=xij, &
                                 refTriangle="UNIT")
  delta = 0.0_DFP
  delta(5) = 1.0_DFP
  CALL OK(MAXVAL(ABS(ans - delta)) .LT. tol, "one point: delta property")
END BLOCK

! Test 2: one point, partition of unity and exact quadratic
! u(x, y) = x*x + 2*y is reproduced exactly
BLOCK
  REAL(DFP) :: ans(n), x(2), u(n)

  x = [0.2_DFP, 0.3_DFP]
  ans = LagrangeEvalAll_Triangle(order=order, x=x, xij=xij, &
                                 refTriangle="UNIT")
  CALL OK(ABS(SUM(ans) - 1.0_DFP) .LT. tol, "one point: sum is one")

  u = xij(1, :)**2 + 2.0_DFP * xij(2, :)
  CALL OK(ABS(DOT_PRODUCT(ans, u) - (x(1)**2 + 2.0_DFP * x(2))) .LT. tol, &
          "one point: quadratic is interpolated exactly")
END BLOCK

! Test 3: one point, reuse of coeff with firstCall
BLOCK
  REAL(DFP) :: a(n), b(n), coeff(n, n), x(2)

  x = [0.1_DFP, 0.6_DFP]
  a = LagrangeEvalAll_Triangle(order=order, x=x, xij=xij, &
                               refTriangle="UNIT", coeff=coeff, &
                               firstCall=.TRUE.)
  b = LagrangeEvalAll_Triangle(order=order, x=x, xij=xij, &
                               refTriangle="UNIT", coeff=coeff, &
                               firstCall=.FALSE.)
  CALL OK(MAXVAL(ABS(a - b)) .LT. tol, "one point: stored coeff is reused")
END BLOCK

! Test 4: many points, the Lagrange basis does not depend on basisType
BLOCK
  REAL(DFP) :: x(2, 3), a(3, n), b(3, n)

  x(:, 1) = [0.2_DFP, 0.3_DFP]
  x(:, 2) = [0.6_DFP, 0.1_DFP]
  x(:, 3) = [0.1_DFP, 0.1_DFP]

  a = LagrangeEvalAll_Triangle(order=order, x=x, xij=xij, &
                               refTriangle="UNIT", basisType=poly%monomial)
  b = LagrangeEvalAll_Triangle(order=order, x=x, xij=xij, &
                               refTriangle="UNIT", basisType=poly%orthogonal)
  CALL OK(MAXVAL(ABS(a - b)) .LT. tol, "monomial and orthogonal agree")

  b = LagrangeEvalAll_Triangle(order=order, x=x, xij=xij, &
                               refTriangle="UNIT", &
                               basisType=poly%hierarchical)
  CALL OK(MAXVAL(ABS(a - b)) .LT. tol, "monomial and hierarchical agree")
END BLOCK

END PROGRAM main
