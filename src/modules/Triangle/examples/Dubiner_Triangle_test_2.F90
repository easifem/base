!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing Dubiner_Triangle on a grid of collapsed coordinates

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TriangleInterpolationUtility, ONLY: Dubiner_Triangle
USE TriangleInterpolationUtility, ONLY: Dubiner_Triangle_
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: order = 3, n = 10
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP

! The grid overload takes the collapsed (square) coordinates xi, eta in
! [-1, 1] for BIUNIT. The triangle point of (xi, eta) is
! x = (1 + xi) (1 - eta) / 2 - 1, y = eta.

! Test 1: function, one grid point equals the point overload
BLOCK
  REAL(DFP) :: xi(1), eta(1), xt(2, 1), a(1, n), b(1, n)

  xi = 0.2_DFP
  eta = -0.4_DFP
  xt(1, 1) = (1.0_DFP + xi(1)) * (1.0_DFP - eta(1)) / 2.0_DFP - 1.0_DFP
  xt(2, 1) = eta(1)

  a = Dubiner_Triangle(order=order, x=xi, y=eta, refTriangle="BIUNIT")
  b = Dubiner_Triangle(order=order, xij=xt, refTriangle="BIUNIT")
  CALL OK(MAXVAL(ABS(a - b)) .LT. tol, "grid point equals triangle point")
END BLOCK

! Test 2: subroutine, two values of xi and one value of eta
BLOCK
  REAL(DFP) :: xi(2), eta(1), ans(2, n), b(2, n), xt(2, 2)
  INTEGER(I4B) :: nrow, ncol, ii

  xi = [-0.5_DFP, 0.7_DFP]
  eta = 0.1_DFP
  DO ii = 1, 2
    xt(1, ii) = (1.0_DFP + xi(ii)) * (1.0_DFP - eta(1)) / 2.0_DFP - 1.0_DFP
    xt(2, ii) = eta(1)
  END DO

  CALL Dubiner_Triangle_(order=order, x=xi, y=eta, refTriangle="BIUNIT", &
                         ans=ans, nrow=nrow, ncol=ncol)
  CALL OK(nrow .EQ. 2 .AND. ncol .EQ. n, "subroutine: size")

  b = Dubiner_Triangle(order=order, xij=xt, refTriangle="BIUNIT")
  CALL OK(MAXVAL(ABS(ans - b)) .LT. tol, &
          "subroutine: grid points equal triangle points")
END BLOCK

! Test 3: the first mode is constant
BLOCK
  REAL(DFP) :: xi(3), eta(2), a(6, n)

  xi = [-0.9_DFP, 0.0_DFP, 0.8_DFP]
  eta = [-0.3_DFP, 0.5_DFP]
  a = Dubiner_Triangle(order=order, x=xi, y=eta, refTriangle="BIUNIT")
  CALL OK(MAXVAL(ABS(a(:, 1) - a(1, 1))) .LT. tol, "first mode is constant")
END BLOCK

END PROGRAM main
