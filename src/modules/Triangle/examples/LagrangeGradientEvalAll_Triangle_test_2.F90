!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing LagrangeGradientEvalAll_Triangle against central
! differences of LagrangeEvalAll_Triangle

PROGRAM main
USE BaseType, ONLY: poly => TypePolynomialOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TriangleInterpolationUtility, ONLY: EquidistancePoint_Triangle
USE TriangleInterpolationUtility, ONLY: LagrangeEvalAll_Triangle
USE TriangleInterpolationUtility, ONLY: LagrangeGradientEvalAll_Triangle
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: order = 3, n = 10, np = 3
REAL(DFP), PARAMETER :: h = 1.0E-6_DFP, tol = 1.0E-6_DFP
REAL(DFP), ALLOCATABLE :: xij(:, :)
REAL(DFP) :: x(2, np)

xij = EquidistancePoint_Triangle(order=order)

x(:, 1) = [0.2_DFP, 0.3_DFP]
x(:, 2) = [0.5_DFP, 0.2_DFP]
x(:, 3) = [0.1_DFP, 0.7_DFP]

! Test 1: monomial basis
BLOCK
  CALL check(poly%monomial, "monomial")
END BLOCK

! Test 2: orthogonal basis
BLOCK
  CALL check(poly%orthogonal, "orthogonal")
END BLOCK

! Test 3: gradient of the partition of unity is zero
BLOCK
  REAL(DFP) :: g(np, n, 2)

  g = LagrangeGradientEvalAll_Triangle(order=order, x=x, xij=xij, &
                                       refTriangle="UNIT")
  CALL OK(MAXVAL(ABS(SUM(g, DIM=2))) .LT. 1.0E-9_DFP, &
          "sum of gradients is zero")
END BLOCK

CONTAINS

SUBROUTINE check(basisType, label)
  INTEGER(I4B), INTENT(IN) :: basisType
  CHARACTER(*), INTENT(IN) :: label

  REAL(DFP) :: g(np, n, 2), fp(np, n), fm(np, n), xp(2, np), xm(2, np)
  INTEGER(I4B) :: dim

  g = LagrangeGradientEvalAll_Triangle(order=order, x=x, xij=xij, &
                                       refTriangle="UNIT", &
                                       basisType=basisType)
  DO dim = 1, 2
    xp = x
    xm = x
    xp(dim, :) = xp(dim, :) + h
    xm(dim, :) = xm(dim, :) - h
    fp = LagrangeEvalAll_Triangle(order=order, x=xp, xij=xij, &
                                  refTriangle="UNIT", basisType=basisType)
    fm = LagrangeEvalAll_Triangle(order=order, x=xm, xij=xij, &
                                  refTriangle="UNIT", basisType=basisType)
    CALL OK(MAXVAL(ABS(g(:, :, dim) - (fp - fm) / (2.0_DFP * h))) &
            .LT. tol, label//": gradient matches differences")
  END DO
END SUBROUTINE check

END PROGRAM main
