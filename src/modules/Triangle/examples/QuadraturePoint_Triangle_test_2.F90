!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing QuadraturePoint_Triangle on a triangle in 3D space

PROGRAM main
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TriangleInterpolationUtility, ONLY: QuadraturePoint_Triangle
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: xij(3, 3)

! triangle (0,0,0), (1,0,0), (0,1,1); its area is SQRT(2)/2
xij(:, 1) = [0.0_DFP, 0.0_DFP, 0.0_DFP]
xij(:, 2) = [1.0_DFP, 0.0_DFP, 0.0_DFP]
xij(:, 3) = [0.0_DFP, 1.0_DFP, 1.0_DFP]

! Test 1: economical rule, rows are x, y, z and weight
BLOCK
  REAL(DFP), ALLOCATABLE :: q(:, :)

  q = QuadraturePoint_Triangle(order=2_I4B, quadType=qpopt%GaussLegendre, &
                               refTriangle="UNIT", xij=xij)
  CALL OK(SIZE(q, 1) .EQ. 4, "order 2: x, y, z, weight rows")
  CALL OK(ABS(SUM(q(4, :)) - SQRT(2.0_DFP) / 2.0_DFP) .LT. tol, &
          "order 2: weights sum to the area")
  CALL OK(MAXVAL(ABS(q(3, :) - q(2, :))) .LT. tol, &
          "order 2: points lie in the plane z = y")
END BLOCK

! Test 2: tensor product rule (order > 20), rows are x, y, z and weight
BLOCK
  REAL(DFP), ALLOCATABLE :: q(:, :)

  q = QuadraturePoint_Triangle(order=22_I4B, &
                               quadType=qpopt%GaussLegendre, &
                               refTriangle="UNIT", xij=xij)
  CALL OK(SIZE(q, 1) .EQ. 4, "order 22: x, y, z, weight rows")
  CALL OK(ABS(SUM(q(4, :)) - SQRT(2.0_DFP) / 2.0_DFP) .LT. tol, &
          "order 22: weights sum to the area")
END BLOCK

END PROGRAM main
