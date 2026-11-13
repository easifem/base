!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Tril with flate returns the lower triangular part as a vector

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE TriagUtility, ONLY: Tril
USE TriagUtility, ONLY: TrilIndx
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: a(3, 3), i, j
INTEGER(I4B), ALLOCATABLE :: v(:), indx(:, :)
REAL(DFP) :: b(3, 5)
REAL(DFP), ALLOCATABLE :: w(:)
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
LOGICAL(LGT) :: isok

DO j = 1, 3
  DO i = 1, 3
    a(i, j) = 10 * i + j
  END DO
END DO

! diagonal by diagonal: main diagonal first, then the sub-diagonals
v = Tril(a, flate=.TRUE.)
CALL OK(ALL(v .EQ. [11, 22, 33, 21, 32, 31]), "integer, diagNo=0")

v = Tril(a, flate=.TRUE., diagNo=-1_I4B)
CALL OK(ALL(v .EQ. [21, 32, 31]), "integer, diagNo=-1")

v = Tril(a, flate=.TRUE., diagNo=1_I4B)
CALL OK(ALL(v .EQ. [12, 23, 11, 22, 33, 21, 32, 31]), &
        "integer, diagNo=1")

! the value of flate is not used
v = Tril(a, flate=.FALSE.)
CALL OK(ALL(v .EQ. [11, 22, 33, 21, 32, 31]), "flate=.FALSE. is the same")

! a diagonal outside the matrix gives an empty vector
v = Tril(a, flate=.TRUE., diagNo=-3_I4B)
CALL OK(SIZE(v) .EQ. 0_I4B, "integer, diagNo=-3: empty")

DO j = 1, 5
  DO i = 1, 3
    b(i, j) = REAL(10 * i + j, DFP)
  END DO
END DO

! real (3,5): the order is the order of TrilIndx
w = Tril(b, flate=.TRUE., diagNo=1_I4B)
indx = TrilIndx(b, diagNo=1_I4B)
CALL OK(SIZE(w) .EQ. 9_I4B, "real (3,5), diagNo=1: size")
isok = .TRUE.
DO i = 1, SIZE(w)
  isok = isok .AND. ABS(w(i) - b(indx(i, 1), indx(i, 2))) .LT. tol
END DO
CALL OK(isok, "real (3,5): same order as TrilIndx")

END PROGRAM main
