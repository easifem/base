!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FortranIndex for a 2D array

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE IndexUtility, ONLY: FortranIndex
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: nrow = 3_I4B, ncol = 4_I4B
INTEGER(I4B) :: a(nrow, ncol), b(nrow * ncol)
INTEGER(I4B) :: ii, jj, n, indx
LOGICAL(LGT) :: isok

! fill a with 1, 2, ..., nrow*ncol in column-major order
n = 0_I4B
DO jj = 1, ncol
  DO ii = 1, nrow
    n = n + 1_I4B
    a(ii, jj) = n
  END DO
END DO

! flat storage of a
b = RESHAPE(a, [nrow * ncol])

! the index is the position of a(ii, jj) in the flat storage
isok = .TRUE.
DO jj = 1, ncol
  DO ii = 1, nrow
    indx = FortranIndex(ii, jj, nrow, ncol)
    isok = isok .AND. (b(indx) .EQ. a(ii, jj))
    isok = isok .AND. (indx .EQ. a(ii, jj))
  END DO
END DO
CALL OK(isok, "index matches the flat storage of a")

! first and last element
indx = FortranIndex(1_I4B, 1_I4B, nrow, ncol)
CALL OK(indx .EQ. 1_I4B, "index of the first element")
indx = FortranIndex(nrow, ncol, nrow, ncol)
CALL OK(indx .EQ. nrow * ncol, "index of the last element")

! a single entry: 2 + (3 - 1) * 3 = 8
indx = FortranIndex(2_I4B, 3_I4B, nrow, ncol)
CALL OK(indx .EQ. 8_I4B, "index of a(2, 3)")

! dimension of size 1
indx = FortranIndex(1_I4B, 4_I4B, 1_I4B, 5_I4B)
CALL OK(indx .EQ. 4_I4B, "nrow = 1")
indx = FortranIndex(4_I4B, 1_I4B, 5_I4B, 1_I4B)
CALL OK(indx .EQ. 4_I4B, "ncol = 1")

END PROGRAM main
