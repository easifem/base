!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: TrilIndx for a matrix of shape (m, n)

PROGRAM main
USE GlobalData, ONLY: I4B
USE TriagUtility, ONLY: TrilIndx
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), ALLOCATABLE :: indx(:, :)

! square (3,3), main diagonal and below, diagonal by diagonal
indx = TrilIndx(m=3_I4B, n=3_I4B, diagNo=0_I4B)
CALL OK(SIZE(indx, 1) .EQ. 6_I4B, "(3,3) diagNo=0: size")
CALL OK(ALL(indx(:, 1) .EQ. [1, 2, 3, 2, 3, 3]), "(3,3) diagNo=0: rows")
CALL OK(ALL(indx(:, 2) .EQ. [1, 2, 3, 1, 2, 1]), "(3,3) diagNo=0: cols")

! default n = m and diagNo = 0
CALL OK(ALL(TrilIndx(m=3_I4B) .EQ. indx), "default n and diagNo")

! first super-diagonal and below
indx = TrilIndx(m=3_I4B, n=3_I4B, diagNo=1_I4B)
CALL OK(ALL(indx(:, 1) .EQ. [1, 2, 1, 2, 3, 2, 3, 3]), &
        "(3,3) diagNo=1: rows")
CALL OK(ALL(indx(:, 2) .EQ. [2, 3, 1, 2, 3, 1, 2, 1]), &
        "(3,3) diagNo=1: cols")

! first sub-diagonal and below
indx = TrilIndx(m=3_I4B, n=3_I4B, diagNo=-1_I4B)
CALL OK(ALL(indx(:, 1) .EQ. [2, 3, 3]), "(3,3) diagNo=-1: rows")
CALL OK(ALL(indx(:, 2) .EQ. [1, 2, 1]), "(3,3) diagNo=-1: cols")

! diagNo outside the matrix
CALL OK(SIZE(TrilIndx(3_I4B, 3_I4B, -3_I4B), 1) .EQ. 0_I4B, &
        "(3,3) diagNo=-3: empty")
CALL OK(SIZE(TrilIndx(3_I4B, 3_I4B, 5_I4B), 1) .EQ. 9_I4B, &
        "(3,3) diagNo=5: all entries")

! rectangular matrices
indx = TrilIndx(m=3_I4B, n=5_I4B)
CALL OK(SIZE(indx, 1) .EQ. 6_I4B, "(3,5) diagNo=0: size")
CALL OK(ALL(indx(:, 2) - indx(:, 1) .LE. 0), "(3,5) j - i <= 0")

indx = TrilIndx(m=5_I4B, n=3_I4B)
CALL OK(SIZE(indx, 1) .EQ. 12_I4B, "(5,3) diagNo=0: size")
CALL OK(ALL(indx(12, :) .EQ. [5, 1]), "(5,3) last entry is (5,1)")

indx = TrilIndx(m=3_I4B, n=5_I4B, diagNo=1_I4B)
CALL OK(SIZE(indx, 1) .EQ. 9_I4B, "(3,5) diagNo=1: size")

END PROGRAM main
