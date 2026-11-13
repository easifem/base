!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Sym for a real matrix

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE SymUtility, ONLY: Sym
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: n = 4
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: mat(n, n), ans(n, n), refU(n, n), refL(n, n)
INTEGER(I4B) :: ii, jj

DO jj = 1, n
  DO ii = 1, n
    mat(ii, jj) = 0.5_DFP * REAL(10 * ii + jj, DFP)
  END DO
END DO

! reference: copy one triangle into the other
DO jj = 1, n
  DO ii = 1, n
    refU(ii, jj) = mat(MIN(ii, jj), MAX(ii, jj))
    refL(ii, jj) = mat(MAX(ii, jj), MIN(ii, jj))
  END DO
END DO

ans = Sym(mat, "U")
CALL OK(ALL(ABS(ans - refU) .LT. tol), "from upper triangle")
CALL OK(ALL(ABS(ans - TRANSPOSE(ans)) .LT. tol), "symmetric (U)")
CALL OK(ABS(ans(4, 1) - 7.0_DFP) .LT. tol, "ans(4,1) = mat(1,4)")

ans = Sym(mat, "L")
CALL OK(ALL(ABS(ans - refL) .LT. tol), "from lower triangle")
CALL OK(ALL(ABS(ans - TRANSPOSE(ans)) .LT. tol), "symmetric (L)")
CALL OK(ABS(ans(1, 4) - 20.5_DFP) .LT. tol, "ans(1,4) = mat(4,1)")

! the diagonal is kept
DO ii = 1, n
  CALL OK(ABS(ans(ii, ii) - mat(ii, ii)) .LT. tol, "diagonal is kept")
END DO

END PROGRAM main
