!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing InterpolationPoint_Triangle for several ipType values

PROGRAM main
USE BaseType, ONLY: ipopt => TypeInterpolationOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TriangleInterpolationUtility, ONLY: InterpolationPoint_Triangle
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP) :: vert(2, 3)

vert(:, 1) = [0.0_DFP, 0.0_DFP]
vert(:, 2) = [1.0_DFP, 0.0_DFP]
vert(:, 3) = [0.0_DFP, 1.0_DFP]

! Test 1: ipType=Center gives the centroid
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:, :)
  REAL(DFP) :: xij(2, 3)

  ans = InterpolationPoint_Triangle(order=1_I4B, ipType=ipopt%Center, &
                                    layout="VEFC")
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 1]), "Center: one point")
  CALL OK(MAXVAL(ABS(ans(:, 1) - 1.0_DFP / 3.0_DFP)) .LT. tol, &
          "Center: centroid of unit triangle")

  xij = 2.0_DFP * vert
  ans = InterpolationPoint_Triangle(order=1_I4B, ipType=ipopt%Center, &
                                    layout="VEFC", xij=xij)
  CALL OK(MAXVAL(ABS(ans(:, 1) - 2.0_DFP / 3.0_DFP)) .LT. tol, &
          "Center: centroid of given triangle")
END BLOCK

! Test 2: Lobatto-type nodes keep the vertices first (VEFC layout)
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:, :)

  ans = InterpolationPoint_Triangle(order=3_I4B, &
                                    ipType=ipopt%IsaacLegendre, &
                                    layout="VEFC")
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 10]), "IsaacLegendre: ten nodes")
  CALL OK(MAXVAL(ABS(ans(:, 1:3) - vert)) .LT. tol, &
          "IsaacLegendre: vertices first")

  ans = InterpolationPoint_Triangle(order=3_I4B, &
                                    ipType=ipopt%BlythPozLegendre, &
                                    layout="VEFC")
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 10]), "BlythPozLegendre: ten nodes")
  CALL OK(MAXVAL(ABS(ans(:, 1:3) - vert)) .LT. tol, &
          "BlythPozLegendre: vertices first")
END BLOCK

! Test 3: for order 2, the Lobatto nodes are the equidistant nodes
BLOCK
  REAL(DFP), ALLOCATABLE :: a(:, :), b(:, :)

  a = InterpolationPoint_Triangle(order=2_I4B, &
                                  ipType=ipopt%Equidistance, layout="VEFC")
  b = InterpolationPoint_Triangle(order=2_I4B, &
                                  ipType=ipopt%GaussLegendreLobatto, &
                                  layout="VEFC")
  CALL OK(ALL(SHAPE(a) .EQ. SHAPE(b)), "order 2: same number of nodes")
  CALL OK(MAXVAL(ABS(a - b)) .LT. tol, &
          "order 2: Lobatto nodes are equidistant")
END BLOCK

! Test 4: ipType handled by the default branch (GaussLegendre)
BLOCK
  REAL(DFP), ALLOCATABLE :: ans(:, :)

  ans = InterpolationPoint_Triangle(order=2_I4B, &
                                    ipType=ipopt%GaussLegendre, &
                                    layout="VEFC")
  CALL OK(ALL(SHAPE(ans) .EQ. [2, 6]), "GaussLegendre: six nodes")
END BLOCK

END PROGRAM main
