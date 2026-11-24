!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing CellBasis_Triangle at interior points

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TriangleInterpolationUtility, ONLY: CellBasis_Triangle
USE TriangleInterpolationUtility, ONLY: HeirarchicalBasis_Triangle
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! Test 1: order 3 has one bubble, 6 * lambda1 * lambda2 * lambda3
! (the zeroth Lobatto kernel is -SQRT(6))
BLOCK
  REAL(DFP) :: x(2, 2), ans(2, 1), lam(3, 2)
  INTEGER(I4B) :: ii

  x(:, 1) = [1.0_DFP, 1.0_DFP] / 3.0_DFP
  x(:, 2) = [0.2_DFP, 0.5_DFP]
  DO ii = 1, 2
    lam(:, ii) = [1.0_DFP - x(1, ii) - x(2, ii), x(1, ii), x(2, ii)]
  END DO

  ans = CellBasis_Triangle(order=3_I4B, xij=x, refTriangle="UNIT")
  CALL OK(ABS(ans(1, 1) - 2.0_DFP / 9.0_DFP) .LT. tol, &
          "order 3: bubble at the centroid is 2/9")
  CALL OK(ABS(ans(2, 1) - 6.0_DFP * PRODUCT(lam(:, 2))) .LT. tol, &
          "order 3: bubble at an interior point")
END BLOCK

! Test 2: order 5 cell modes are the last columns of the full basis
BLOCK
  INTEGER(I4B), PARAMETER :: order = 5, nc = 6, ntot = 3 * order + nc
  REAL(DFP) :: x(2, 3), cell(3, nc), full(3, ntot)

  x(:, 1) = [0.2_DFP, 0.3_DFP]
  x(:, 2) = [0.5_DFP, 0.2_DFP]
  x(:, 3) = [0.1_DFP, 0.6_DFP]

  cell = CellBasis_Triangle(order=order, xij=x, refTriangle="UNIT")
  full = HeirarchicalBasis_Triangle(order=order, xij=x, refTriangle="UNIT")
  CALL OK(MAXVAL(ABS(cell - full(:, ntot - nc + 1:ntot))) .LT. tol, &
          "order 5: same as cell part of the hierarchical basis")
END BLOCK

END PROGRAM main
