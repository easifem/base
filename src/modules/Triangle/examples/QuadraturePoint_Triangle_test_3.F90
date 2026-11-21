!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing QuadraturePoint_Triangle for orders without an
! economical rule (order 0 and order > 20)

PROGRAM main
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TriangleInterpolationUtility, ONLY: QuadratureNumber_Triangle
USE TriangleInterpolationUtility, ONLY: QuadraturePoint_Triangle
USE TriangleInterpolationUtility, ONLY: QuadraturePoint_Triangle_
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-11_DFP

! Test 1: order 25 uses the tensor product rule
BLOCK
  REAL(DFP), ALLOCATABLE :: q(:, :)
  REAL(DFP) :: s, exact
  INTEGER(I4B) :: n

  n = QuadratureNumber_Triangle(order=25_I4B, quadType=qpopt%GaussLegendre)
  q = QuadraturePoint_Triangle(order=25_I4B, &
                               quadType=qpopt%GaussLegendre, &
                               refTriangle="UNIT")
  CALL OK(ALL(SHAPE(q) .EQ. [3, n]), "order 25: shape")
  CALL OK(ABS(SUM(q(3, :)) - 0.5_DFP) .LT. tol, "order 25: area is 1/2")

  ! integral of x^12 y^12 over the unit triangle is 12! 12! / 26!
  exact = GAMMA(13.0_DFP)**2 / GAMMA(27.0_DFP)
  s = SUM(q(3, :) * q(1, :)**12 * q(2, :)**12)
  CALL OK(ABS(s - exact) / exact .LT. 1.0E-8_DFP, &
          "order 25: degree 24 monomial is exact")
END BLOCK

! Test 2: subroutine version for order 25
BLOCK
  REAL(DFP), ALLOCATABLE :: q(:, :), buffer(:, :)
  INTEGER(I4B) :: n, nrow, ncol

  n = QuadratureNumber_Triangle(order=25_I4B, quadType=qpopt%GaussLegendre)
  ALLOCATE (buffer(3, n))
  CALL QuadraturePoint_Triangle_(order=25_I4B, &
                                 quadType=qpopt%GaussLegendre, &
                                 refTriangle="UNIT", ans=buffer, &
                                 nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 3 .AND. ncol .EQ. n, "order 25 subroutine: size")

  q = QuadraturePoint_Triangle(order=25_I4B, &
                               quadType=qpopt%GaussLegendre, &
                               refTriangle="UNIT")
  CALL OK(MAXVAL(ABS(q - buffer)) .LT. tol, &
          "order 25: function and subroutine agree")
END BLOCK

! Test 3: order 0 integrates constants
BLOCK
  REAL(DFP), ALLOCATABLE :: q(:, :)

  q = QuadraturePoint_Triangle(order=0_I4B, quadType=qpopt%GaussLegendre, &
                               refTriangle="UNIT")
  CALL OK(SIZE(q, 2) .GE. 1, "order 0: at least one point")
  CALL OK(ABS(SUM(q(3, :)) - 0.5_DFP) .LT. tol, "order 0: area is 1/2")
END BLOCK

END PROGRAM main
