!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Number of quadrature points needed to integrate a polynomial
! of a given degree on a line.
!
! An n-point Gauss rule is exact up to degree 2n-1, a Radau rule up to
! 2n-2 and a Lobatto rule up to 2n-3.

PROGRAM main
USE BaseType, ONLY: qpopt => TypeQuadratureOpt
USE GlobalData, ONLY: I4B
USE LineInterpolationUtility, ONLY: QuadratureNumber_Line
USE Test_Method, ONLY: OK
IMPLICIT NONE

! Test 1: Gauss rules need 1 + order/2 points
BLOCK
  CALL OK(QuadratureNumber_Line(1_I4B, qpopt%GaussLegendre) == 1, &
          "Gauss-Legendre, order 1: 1 point")
  CALL OK(QuadratureNumber_Line(3_I4B, qpopt%GaussLegendre) == 2, &
          "Gauss-Legendre, order 3: 2 points")
  CALL OK(QuadratureNumber_Line(4_I4B, qpopt%GaussChebyshev) == 3, &
          "Gauss-Chebyshev, order 4: 3 points")
END BLOCK

! Test 2: Radau rules need 2 + (order-1)/2 points
BLOCK
  CALL OK(QuadratureNumber_Line(2_I4B, qpopt%GaussLegendreRadauLeft) == 2, &
          "Radau left, order 2: 2 points")
  CALL OK(QuadratureNumber_Line(4_I4B, qpopt%GaussLegendreRadauRight) == 3, &
          "Radau right, order 4: 3 points")
END BLOCK

! Test 3: Lobatto rules need 2 + order/2 points
BLOCK
  CALL OK(QuadratureNumber_Line(3_I4B, qpopt%GaussLegendreLobatto) == 3, &
          "Lobatto, order 3: 3 points")
  CALL OK(QuadratureNumber_Line(5_I4B, qpopt%GaussLegendreLobatto) == 4, &
          "Lobatto, order 5: 4 points")
END BLOCK

END PROGRAM main
