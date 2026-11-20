!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing LagrangeGradientEvalAll_Tetrahedron for order 2

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TetrahedronInterpolationUtility, ONLY: EquidistancePoint_Tetrahedron
USE TetrahedronInterpolationUtility, ONLY: &
  LagrangeGradientEvalAll_Tetrahedron
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: order = 2, n = 10
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), ALLOCATABLE :: nodes(:, :)
REAL(DFP) :: p(3, 1), dN(1, n, 3)

nodes = EquidistancePoint_Tetrahedron(order=order)
p(:, 1) = [0.1_DFP, 0.2_DFP, 0.3_DFP]
dN = LagrangeGradientEvalAll_Tetrahedron(order=order, x=p, xij=nodes)

! Test 1: gradients of the basis functions sum to zero
BLOCK
  CALL OK(MAXVAL(ABS(SUM(dN(1, :, :), dim=1))) .LT. tol, &
          "sum of gradients is zero")
END BLOCK

! Test 2: gradient of f = x*y is (y, x, 0)
BLOCK
  REAL(DFP) :: f(n), g(3)
  INTEGER(I4B) :: ii
  f = nodes(1, :) * nodes(2, :)
  DO ii = 1, 3
    g(ii) = DOT_PRODUCT(f, dN(1, :, ii))
  END DO
  CALL OK(MAXVAL(ABS(g - [p(2, 1), p(1, 1), 0.0_DFP])) .LT. tol, &
          "gradient of x*y")
END BLOCK

! Test 3: gradient of f = z**2 is (0, 0, 2z)
BLOCK
  REAL(DFP) :: f(n), g(3)
  INTEGER(I4B) :: ii
  f = nodes(3, :)**2
  DO ii = 1, 3
    g(ii) = DOT_PRODUCT(f, dN(1, :, ii))
  END DO
  CALL OK(MAXVAL(ABS(g - [0.0_DFP, 0.0_DFP, 2.0_DFP * p(3, 1)])) .LT. tol, &
          "gradient of z**2")
END BLOCK

END PROGRAM main
