!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing EquidistanceInPoint_Triangle with given vertices

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TriangleInterpolationUtility, ONLY: EquidistanceInPoint_Triangle
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: xij(2, 3)

! triangle with vertices (0,0), (3,0), (0,3)
xij(:, 1) = [0.0_DFP, 0.0_DFP]
xij(:, 2) = [3.0_DFP, 0.0_DFP]
xij(:, 3) = [0.0_DFP, 3.0_DFP]

! Test 1: order < 3 has no interior points
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:, :)

  ans = EquidistanceInPoint_Triangle(order=2_I4B, xij=xij)
  CALL OK(SIZE(ans, 2) .EQ. 0, "order 2: no interior points")
END BLOCK

! Test 2: order 3 has one interior point, the centroid of xij
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:, :)

  ans = EquidistanceInPoint_Triangle(order=3_I4B, xij=xij)
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 1]), "order 3: one interior point")
  CALL OK(MAXVAL(ABS(ans(:, 1) - [1.0_DFP, 1.0_DFP])) .LT. tol, &
          "order 3: interior point is the centroid of xij")
END BLOCK

! Test 3: order 4 has three interior points (i/4, j/4) * 3 with
! i, j >= 1 and i + j <= 3
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:, :)
  REAL(DFP) :: expected(2, 3)

  expected(:, 1) = [0.75_DFP, 0.75_DFP]
  expected(:, 2) = [1.50_DFP, 0.75_DFP]
  expected(:, 3) = [0.75_DFP, 1.50_DFP]

  ans = EquidistanceInPoint_Triangle(order=4_I4B, xij=xij)
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 3]), "order 4: three interior points")
  CALL OK(MAXVAL(ABS(ans - expected)) .LT. tol, &
          "order 4: interior points of the scaled triangle")
END BLOCK

! Test 4: without xij the unit triangle is used
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:, :)

  ans = EquidistanceInPoint_Triangle(order=4_I4B)
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 3]), "order 4, unit: three points")
  CALL OK(MAXVAL(ABS(ans(:, 1) - [0.25_DFP, 0.25_DFP])) .LT. tol, &
          "order 4, unit: first interior point")
END BLOCK

END PROGRAM main
