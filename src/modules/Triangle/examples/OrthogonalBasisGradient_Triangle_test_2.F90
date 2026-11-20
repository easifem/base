!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing OrthogonalBasisGradient_Triangle against central
! differences of Dubiner_Triangle on UNIT and BIUNIT triangles

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TriangleInterpolationUtility, ONLY: Dubiner_Triangle
USE TriangleInterpolationUtility, ONLY: OrthogonalBasisGradient_Triangle
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: order = 3, n = 10, np = 3
REAL(DFP), PARAMETER :: h = 1.0E-6_DFP, tol = 1.0E-6_DFP

! Test 1: UNIT triangle
BLOCK
  REAL(DFP) :: x(2, np)

  x(:, 1) = [0.2_DFP, 0.3_DFP]
  x(:, 2) = [0.5_DFP, 0.2_DFP]
  x(:, 3) = [0.1_DFP, 0.6_DFP]
  CALL check(x, "UNIT")
END BLOCK

! Test 2: BIUNIT triangle
BLOCK
  REAL(DFP) :: x(2, np)

  x(:, 1) = [-0.6_DFP, -0.4_DFP]
  x(:, 2) = [0.0_DFP, -0.6_DFP]
  x(:, 3) = [-0.8_DFP, 0.2_DFP]
  CALL check(x, "BIUNIT")
END BLOCK

CONTAINS

SUBROUTINE check(x, refTriangle)
  REAL(DFP), INTENT(IN) :: x(:, :)
  CHARACTER(*), INTENT(IN) :: refTriangle

  REAL(DFP) :: g(np, n, 2), fp(np, n), fm(np, n), xp(2, np), xm(2, np)
  INTEGER(I4B) :: dim

  g = OrthogonalBasisGradient_Triangle(order=order, xij=x, &
                                       refTriangle=refTriangle)
  DO dim = 1, 2
    xp = x
    xm = x
    xp(dim, :) = xp(dim, :) + h
    xm(dim, :) = xm(dim, :) - h
    fp = Dubiner_Triangle(order=order, xij=xp, refTriangle=refTriangle)
    fm = Dubiner_Triangle(order=order, xij=xm, refTriangle=refTriangle)
    CALL OK(MAXVAL(ABS(g(:, :, dim) - (fp - fm) / (2.0_DFP * h))) &
            .LT. tol, refTriangle//": gradient matches differences")
  END DO
END SUBROUTINE check

END PROGRAM main
