!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing LagrangeEvalAll_Tetrahedron at one point, order 2

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE Test_Method, ONLY: OK
USE TetrahedronInterpolationUtility, ONLY: EquidistancePoint_Tetrahedron
USE TetrahedronInterpolationUtility, ONLY: LagrangeEvalAll_Tetrahedron
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: order = 2, n = 10
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), ALLOCATABLE :: nodes(:, :)
REAL(DFP) :: p(3)

nodes = EquidistancePoint_Tetrahedron(order=order)
p = [0.1_DFP, 0.2_DFP, 0.3_DFP]

! Test 1: at node 7 only basis function 7 is 1
BLOCK
  REAL(DFP) :: y(n), e7(n)
  e7 = 0.0_DFP
  e7(7) = 1.0_DFP
  y = LagrangeEvalAll_Tetrahedron(order=order, x=nodes(:, 7), xij=nodes)
  CALL OK(MAXVAL(ABS(y - e7)) .LT. tol, "cardinal property at node 7")
END BLOCK

! Test 2: partition of unity at an interior point
BLOCK
  REAL(DFP) :: y(n)
  y = LagrangeEvalAll_Tetrahedron(order=order, x=p, xij=nodes)
  CALL OK(ABS(SUM(y) - 1.0_DFP) .LT. tol, "basis functions sum to 1")
END BLOCK

! Test 3: interpolation reproduces f = x*y + z**2 exactly
BLOCK
  REAL(DFP) :: y(n), f(n), fp
  y = LagrangeEvalAll_Tetrahedron(order=order, x=p, xij=nodes)
  f = nodes(1, :) * nodes(2, :) + nodes(3, :)**2
  fp = p(1) * p(2) + p(3)**2
  CALL OK(ABS(DOT_PRODUCT(f, y) - fp) .LT. tol, &
          "quadratic function is reproduced")
END BLOCK

! Test 4: coefficients computed once and reused with firstCall false
BLOCK
  REAL(DFP) :: y1(n), y2(n), coeff(n, n)
  y1 = LagrangeEvalAll_Tetrahedron(order=order, x=p, xij=nodes, &
                                   coeff=coeff, firstCall=.TRUE._LGT)
  y2 = LagrangeEvalAll_Tetrahedron(order=order, x=nodes(:, 2), xij=nodes, &
                                   coeff=coeff, firstCall=.FALSE._LGT)
  CALL OK(ABS(SUM(y1) - 1.0_DFP) .LT. tol, "first call computes coeff")
  CALL OK(ABS(y2(2) - 1.0_DFP) .LT. tol .AND. &
          ABS(SUM(y2) - 1.0_DFP) .LT. tol, "second call reuses coeff")
END BLOCK

END PROGRAM main
