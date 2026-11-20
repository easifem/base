!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Quadrature points on a segment, chosen by order or by number of
! points.
!
! The last row of the result holds the weights. When the segment is given,
! the points are mapped to it and the weights are scaled by its half
! length.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: GaussLegendreQP
USE GlobalData, ONLY: GaussLegendreLobattoQP
USE LineInterpolationUtility, ONLY: QuadraturePoint_Line
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP

! Test 1: by order, 1D segment [0, 2]; exact for x**3
BLOCK
  REAL(DFP), ALLOCATABLE :: q(:, :)
  q = QuadraturePoint_Line(order=3_I4B, quadType=GaussLegendreQP, &
                           xij=[0.0_DFP, 2.0_DFP], layout="INCREASING")
  CALL OK(ALL(SHAPE(q) == [2, 2]), "Order 3 Gauss-Legendre: two points")
  CALL OK(ABS(SUM(q(2, :)) - 2.0_DFP) < tol, "Weights sum to length")
  CALL OK(ABS(SUM(q(2, :) * q(1, :)**3) - 4.0_DFP) < tol, &
          "Integral of x**3 on [0, 2] is 4")
END BLOCK

! Test 2: by number of points, 1D segment [0, 2]; exact for x**5
BLOCK
  REAL(DFP), ALLOCATABLE :: q(:, :)
  q = QuadraturePoint_Line(nips=[3_I4B], quadType=GaussLegendreQP, &
                           xij=[0.0_DFP, 2.0_DFP], layout="INCREASING")
  CALL OK(ALL(SHAPE(q) == [2, 3]), "Three points requested")
  CALL OK(ABS(SUM(q(2, :) * q(1, :)**5) - 64.0_DFP / 6.0_DFP) < tol, &
          "Integral of x**5 on [0, 2] is 32/3")
END BLOCK

! Test 3: by number of points, 2D segment from (0,0) to (3,4)
BLOCK
  REAL(DFP), ALLOCATABLE :: q(:, :)
  REAL(DFP) :: xij(2, 2)
  xij(:, 1) = [0.0_DFP, 0.0_DFP]
  xij(:, 2) = [3.0_DFP, 4.0_DFP]
  q = QuadraturePoint_Line(nips=[2_I4B], quadType=GaussLegendreQP, &
                           layout="INCREASING", xij=xij)
  CALL OK(ALL(SHAPE(q) == [3, 2]), "2D: two coordinate rows and weights")
  CALL OK(ABS(SUM(q(3, :)) - 5.0_DFP) < tol, "2D: weights sum to length")
  CALL OK(MAXVAL(ABS(3.0_DFP * q(2, :) - 4.0_DFP * q(1, :))) < tol, &
          "2D: points lie on the segment")
END BLOCK

! Test 4: by order, reference line, Lobatto rule in VEFC layout
BLOCK
  REAL(DFP), ALLOCATABLE :: q(:, :)
  q = QuadraturePoint_Line(order=3_I4B, quadType=GaussLegendreLobattoQP, &
                           layout="VEFC")
  CALL OK(ALL(SHAPE(q) == [2, 3]), "Lobatto order 3: three points")
  CALL OK(MAXVAL(ABS(q(1, :) - [-1.0_DFP, 1.0_DFP, 0.0_DFP])) < tol, &
          "Lobatto points: ends first")
  CALL OK(MAXVAL(ABS(q(2, :) - [1.0_DFP, 1.0_DFP, 4.0_DFP] / 3.0_DFP)) &
          < tol, "Lobatto weights follow the points")
END BLOCK

END PROGRAM main
