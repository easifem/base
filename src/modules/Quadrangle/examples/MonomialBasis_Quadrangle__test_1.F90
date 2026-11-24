!> author: Vikas Sharma, Ph.D.
! date: 2026-11-20
! summary: Testing MonomialBasis_Quadrangle_ for the size and the column
! order of the monomials

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE QuadrangleInterpolationUtility, ONLY: MonomialBasis_Quadrangle_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
INTEGER(I4B), PARAMETER :: p = 2, q = 1
REAL(DFP) :: xij(2, 3), ans(3, 6), x(3), y(3)
INTEGER(I4B) :: nrow, ncol

xij(:, 1) = [0.2_DFP, -0.4_DFP]
xij(:, 2) = [-0.3_DFP, 0.5_DFP]
xij(:, 3) = [0.7_DFP, 0.9_DFP]
x = xij(1, :)
y = xij(2, :)

CALL MonomialBasis_Quadrangle_(p=p, q=q, xij=xij, ans=ans, nrow=nrow, &
                               ncol=ncol)

! Test 1: one row per point and (p + 1) * (q + 1) columns
BLOCK
  CALL OK(nrow .EQ. 3 .AND. ncol .EQ. 6, "p=2, q=1: nrow=3, ncol=6")
END BLOCK

! Test 2: the x exponent changes fastest
! Columns: 1, x, x^2, y, x y, x^2 y
BLOCK
  CALL OK(MAXVAL(ABS(ans(:, 1) - 1.0_DFP)) .LT. tol, "column 1 is 1")
  CALL OK(MAXVAL(ABS(ans(:, 2) - x)) .LT. tol, "column 2 is x")
  CALL OK(MAXVAL(ABS(ans(:, 3) - x**2)) .LT. tol, "column 3 is x^2")
  CALL OK(MAXVAL(ABS(ans(:, 4) - y)) .LT. tol, "column 4 is y")
  CALL OK(MAXVAL(ABS(ans(:, 5) - x * y)) .LT. tol, "column 5 is x y")
  CALL OK(MAXVAL(ABS(ans(:, 6) - x**2 * y)) .LT. tol, &
          "column 6 is x^2 y")
END BLOCK

END PROGRAM main
