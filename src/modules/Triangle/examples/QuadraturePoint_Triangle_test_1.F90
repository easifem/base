!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing QuadraturePoint_Triangle with a given number of points

PROGRAM main
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TriangleInterpolationUtility, ONLY: QuadratureNumber_Triangle
USE TriangleInterpolationUtility, ONLY: QuadraturePoint_Triangle
USE TriangleInterpolationUtility, ONLY: QuadraturePoint_Triangle_
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
INTEGER(I4B) :: nips(1)

! six point rule, exact for polynomials of degree 4
nips(1) = QuadratureNumber_Triangle(order=4_I4B, &
                                    quadType=qpopt%GaussLegendre)

! Test 1: UNIT triangle
BLOCK
  REAL(DFP), ALLOCATABLE :: q(:, :)
  REAL(DFP) :: s

  q = QuadraturePoint_Triangle(nips=nips, quadType=qpopt%GaussLegendre, &
                               refTriangle="UNIT")
  CALL OK(ALL(SHAPE(q) .EQ. [3, 6]), "UNIT: x, y, weight rows")
  CALL OK(ABS(SUM(q(3, :)) - 0.5_DFP) .LT. tol, "UNIT: area is 1/2")

  ! integral of x^2 y^2 over the unit triangle is 2! 2! / 6! = 1/180
  s = SUM(q(3, :) * q(1, :)**2 * q(2, :)**2)
  CALL OK(ABS(s - 1.0_DFP / 180.0_DFP) .LT. tol, &
          "UNIT: degree 4 monomial is exact")
END BLOCK

! Test 2: BIUNIT triangle
BLOCK
  REAL(DFP), ALLOCATABLE :: q(:, :)

  q = QuadraturePoint_Triangle(nips=nips, quadType=qpopt%GaussLegendre, &
                               refTriangle="BIUNIT")
  CALL OK(ABS(SUM(q(3, :)) - 2.0_DFP) .LT. tol, "BIUNIT: area is 2")
  CALL OK(ALL(q(1, :) + q(2, :) .LE. 0.0_DFP) .AND. &
          ALL(q(1:2, :) .GE. -1.0_DFP), "BIUNIT: points are inside")
END BLOCK

! Test 3: general planar triangle given by xij
BLOCK
  REAL(DFP), ALLOCATABLE :: q(:, :)
  REAL(DFP) :: xij(2, 3)

  xij(:, 1) = [1.0_DFP, 1.0_DFP]
  xij(:, 2) = [4.0_DFP, 1.0_DFP]
  xij(:, 3) = [1.0_DFP, 3.0_DFP]

  q = QuadraturePoint_Triangle(nips=nips, quadType=qpopt%GaussLegendre, &
                               refTriangle="UNIT", xij=xij)
  CALL OK(ABS(SUM(q(3, :)) - 3.0_DFP) .LT. tol, "xij: area is 3")

  ! centroid of the triangle is (2, 5/3)
  CALL OK(ABS(SUM(q(3, :) * q(1, :)) / 3.0_DFP - 2.0_DFP) .LT. tol .AND. &
          ABS(SUM(q(3, :) * q(2, :)) / 3.0_DFP - 5.0_DFP / 3.0_DFP) &
          .LT. tol, "xij: centroid")
END BLOCK

! Test 4: subroutine version with a given number of points
BLOCK
  REAL(DFP), ALLOCATABLE :: q(:, :)
  REAL(DFP) :: buffer(3, 6)
  INTEGER(I4B) :: nrow, ncol

  CALL QuadraturePoint_Triangle_(nips=nips, quadType=qpopt%GaussLegendre, &
                                 refTriangle="UNIT", ans=buffer, &
                                 nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 3 .AND. ncol .EQ. 6, "subroutine: size")

  q = QuadraturePoint_Triangle(nips=nips, quadType=qpopt%GaussLegendre, &
                               refTriangle="UNIT")
  CALL OK(MAXVAL(ABS(q - buffer)) .LT. tol, &
          "subroutine: same as function")
END BLOCK

END PROGRAM main
