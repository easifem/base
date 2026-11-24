!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing QuadratureNumber_Triangle for economical and tensor
! product rules

PROGRAM main
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TriangleInterpolationUtility, ONLY: QuadratureNumber_Triangle
IMPLICIT NONE

! Test 1: economical (Solin) rules for order <= 20
BLOCK
  CALL OK(QuadratureNumber_Triangle(1_I4B, qpopt%GaussLegendre) .EQ. 1, &
          "order 1: one point")
  CALL OK(QuadratureNumber_Triangle(2_I4B, qpopt%GaussLegendre) .EQ. 3, &
          "order 2: three points")
  CALL OK(QuadratureNumber_Triangle(4_I4B, qpopt%GaussLegendre) .EQ. 6, &
          "order 4: six points")
  CALL OK(QuadratureNumber_Triangle(20_I4B, qpopt%GaussLegendre) .EQ. 79, &
          "order 20: 79 points")
END BLOCK

! Test 2: tensor product (collapsed square) rule for order > 20
! n = 1 + order / 2 points in one direction, n + 1 in the other
BLOCK
  CALL OK(QuadratureNumber_Triangle(25_I4B, qpopt%GaussLegendre) .EQ. &
          13 * 14, "order 25: 13 x 14 points")
  CALL OK(QuadratureNumber_Triangle(0_I4B, qpopt%GaussLegendre) .EQ. 2, &
          "order 0: 1 x 2 points")
END BLOCK

END PROGRAM main
