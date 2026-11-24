!> author: Vikas Sharma, Ph. D.
! date: 2026-11-20
! summary: Testing LagrangeEvalAll_Triangle_ at one point and with
! user workspace

PROGRAM main
USE BaseType, ONLY: poly => TypePolynomialOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TriangleInterpolationUtility, ONLY: EquidistancePoint_Triangle
USE TriangleInterpolationUtility, ONLY: LagrangeEvalAll_Triangle
USE TriangleInterpolationUtility, ONLY: LagrangeEvalAll_Triangle_
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: order = 2, n = 6, np = 3
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), ALLOCATABLE :: xij(:, :)
REAL(DFP) :: x(2, np), expected(np, n)

xij = EquidistancePoint_Triangle(order=order)

x(:, 1) = [0.2_DFP, 0.3_DFP]
x(:, 2) = [0.6_DFP, 0.1_DFP]
x(:, 3) = [0.1_DFP, 0.1_DFP]

expected = LagrangeEvalAll_Triangle(order=order, x=x, xij=xij, &
                                    refTriangle="UNIT")

! Test 1: one point
BLOCK
  REAL(DFP) :: ans(n)
  INTEGER(I4B) :: tsize

  CALL LagrangeEvalAll_Triangle_(order=order, x=x(:, 2), xij=xij, &
                                 ans=ans, tsize=tsize, refTriangle="UNIT")
  CALL OK(tsize .EQ. n, "one point: tsize")
  CALL OK(MAXVAL(ABS(ans - expected(2, :))) .LT. tol, &
          "one point: same as many points")
END BLOCK

! Test 2: user workspace, coeff is made on the first call
BLOCK
  REAL(DFP) :: ans(np, n), coeff(n, n), xx(np, n)
  INTEGER(I4B) :: degree(n, 2), nrow, ncol

  CALL LagrangeEvalAll_Triangle_(order=order, x=x, xij=xij, ans=ans, &
                                 nrow=nrow, ncol=ncol, refTriangle="UNIT", &
                                 coeff=coeff, firstCall=.TRUE., &
                                 basisType=poly%monomial, xx=xx, &
                                 degree=degree)
  CALL OK(nrow .EQ. np .AND. ncol .EQ. n, "workspace: size")
  CALL OK(MAXVAL(ABS(ans - expected)) .LT. tol, "workspace: values")

  ! second call reuses coeff
  ans = 0.0_DFP
  CALL LagrangeEvalAll_Triangle_(order=order, x=x, xij=xij, ans=ans, &
                                 nrow=nrow, ncol=ncol, refTriangle="UNIT", &
                                 coeff=coeff, firstCall=.FALSE., &
                                 basisType=poly%monomial, xx=xx, &
                                 degree=degree)
  CALL OK(MAXVAL(ABS(ans - expected)) .LT. tol, &
          "workspace: stored coeff is reused")
END BLOCK

END PROGRAM main
