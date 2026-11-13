!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: GetSym for a real matrix

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE SymUtility, ONLY: GetSym
USE SymUtility, ONLY: Sym
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: n = 4
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
REAL(DFP) :: mat(n, n), mat0(n, n), refU(n, n), refL(n, n)
INTEGER(I4B) :: ii, jj

DO jj = 1, n
  DO ii = 1, n
    mat0(ii, jj) = 0.5_DFP * REAL(10 * ii + jj, DFP)
    refU(ii, jj) = 0.5_DFP * REAL(10 * MIN(ii, jj) + MAX(ii, jj), DFP)
    refL(ii, jj) = 0.5_DFP * REAL(10 * MAX(ii, jj) + MIN(ii, jj), DFP)
  END DO
END DO

mat = mat0
CALL GetSym(mat, "U")
CALL OK(ALL(ABS(mat - refU) .LT. tol), "from upper triangle")
CALL OK(ALL(ABS(mat - TRANSPOSE(mat)) .LT. tol), "symmetric (U)")

mat = mat0
CALL GetSym(mat, "L")
CALL OK(ALL(ABS(mat - refL) .LT. tol), "from lower triangle")
CALL OK(ALL(ABS(mat - TRANSPOSE(mat)) .LT. tol), "symmetric (L)")

! GetSym gives the same result as Sym
mat = mat0
CALL GetSym(mat, "U")
CALL OK(ALL(ABS(mat - Sym(mat0, "U")) .LT. tol), "same as Sym")

END PROGRAM main
