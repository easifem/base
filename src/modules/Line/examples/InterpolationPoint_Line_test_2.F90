!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Interpolation points on a segment: point types and layouts.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: Equidistance
USE GlobalData, ONLY: GaussLegendre
USE GlobalData, ONLY: GaussLegendreLobatto
USE LineInterpolationUtility, ONLY: InterpolationPoint_Line
USE LineInterpolationUtility, ONLY: InterpolationPoint_Line_
USE Test_Method, ONLY: OK
IMPLICIT NONE
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP

! Test 1: 1D ends, equidistance, VEFC layout
BLOCK
  REAL(DFP), ALLOCATABLE :: y(:)
  y = InterpolationPoint_Line(order=2_I4B, ipType=Equidistance, &
                              xij=[0.0_DFP, 2.0_DFP], layout="VEFC")
  CALL OK(MAXVAL(ABS(y - [0.0_DFP, 2.0_DFP, 1.0_DFP])) < tol, &
          "Equidistance VEFC: ends first")
END BLOCK

! Test 2: 1D ends, equidistance, INCREASING layout
BLOCK
  REAL(DFP), ALLOCATABLE :: y(:)
  y = InterpolationPoint_Line(order=2_I4B, ipType=Equidistance, &
                              xij=[0.0_DFP, 2.0_DFP], layout="INCREASING")
  CALL OK(MAXVAL(ABS(y - [0.0_DFP, 1.0_DFP, 2.0_DFP])) < tol, &
          "Equidistance INCREASING: sorted")
END BLOCK

! Test 3: 1D ends, Gauss-Legendre-Lobatto, both layouts
BLOCK
  REAL(DFP), ALLOCATABLE :: y(:)
  y = InterpolationPoint_Line(order=2_I4B, ipType=GaussLegendreLobatto, &
                              xij=[0.0_DFP, 2.0_DFP], layout="VEFC")
  CALL OK(MAXVAL(ABS(y - [0.0_DFP, 2.0_DFP, 1.0_DFP])) < tol, &
          "GLL VEFC mapped to [0, 2]")
  y = InterpolationPoint_Line(order=2_I4B, ipType=GaussLegendreLobatto, &
                              xij=[0.0_DFP, 2.0_DFP], layout="INCREASING")
  CALL OK(MAXVAL(ABS(y - [0.0_DFP, 1.0_DFP, 2.0_DFP])) < tol, &
          "GLL INCREASING mapped to [0, 2]")
END BLOCK

! Test 4: 1D ends, Gauss-Legendre has no end point
BLOCK
  REAL(DFP), ALLOCATABLE :: y(:)
  REAL(DFP) :: a
  a = 1.0_DFP / SQRT(3.0_DFP)
  y = InterpolationPoint_Line(order=1_I4B, ipType=GaussLegendre, &
                              xij=[-1.0_DFP, 1.0_DFP], layout="INCREASING")
  CALL OK(MAXVAL(ABS(y - [-a, a])) < tol, "Two Gauss-Legendre points")
END BLOCK

! Test 5: 1D ends, order 0 gives the midpoint
BLOCK
  REAL(DFP), ALLOCATABLE :: y(:)
  y = InterpolationPoint_Line(order=0_I4B, ipType=Equidistance, &
                              xij=[0.0_DFP, 2.0_DFP], layout="VEFC")
  CALL OK(SIZE(y) == 1 .AND. ABS(y(1) - 1.0_DFP) < tol, &
          "Order 0 point is the midpoint")
END BLOCK

! Test 6: subroutine with 1D ends
BLOCK
  REAL(DFP) :: buf(4)
  INTEGER(I4B) :: tsize
  CALL InterpolationPoint_Line_(order=2_I4B, ipType=GaussLegendreLobatto, &
                                ans=buf, tsize=tsize, &
                                xij=[-1.0_DFP, 1.0_DFP], layout="VEFC")
  CALL OK(tsize == 3, "Subroutine writes three points")
  CALL OK(MAXVAL(ABS(buf(1:3) - [-1.0_DFP, 1.0_DFP, 0.0_DFP])) < tol, &
          "Subroutine GLL points in VEFC layout")
END BLOCK

! Test 7: 2D segment from (0,0) to (2,2), GLL points
BLOCK
  REAL(DFP), ALLOCATABLE :: y(:, :)
  REAL(DFP) :: xij(2, 2)
  xij(:, 1) = [0.0_DFP, 0.0_DFP]
  xij(:, 2) = [2.0_DFP, 2.0_DFP]
  y = InterpolationPoint_Line(order=2_I4B, ipType=GaussLegendreLobatto, &
                              layout="VEFC", xij=xij)
  CALL OK(ALL(SHAPE(y) == [2, 3]), "2D segment: 2 rows, 3 points")
  CALL OK(MAXVAL(ABS(y(:, 3) - [1.0_DFP, 1.0_DFP])) < tol, &
          "2D segment: interior point at the midpoint")
  CALL OK(MAXVAL(ABS(y(:, 2) - [2.0_DFP, 2.0_DFP])) < tol, &
          "2D segment: second end point")
END BLOCK

END PROGRAM main
