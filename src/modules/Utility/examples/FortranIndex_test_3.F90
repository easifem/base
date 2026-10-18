!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FortranIndex for a 4D array

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE IndexUtility, ONLY: FortranIndex
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: dim1 = 2_I4B, dim2 = 3_I4B
INTEGER(I4B), PARAMETER :: dim3 = 4_I4B, dim4 = 2_I4B
INTEGER(I4B), PARAMETER :: ntotal = dim1 * dim2 * dim3 * dim4
INTEGER(I4B) :: a(dim1, dim2, dim3, dim4), b(ntotal)
INTEGER(I4B) :: ii, jj, kk, ll, n, indx
LOGICAL(LGT) :: isok

! fill a with 1, 2, ..., ntotal in column-major order
n = 0_I4B
DO ll = 1, dim4
  DO kk = 1, dim3
    DO jj = 1, dim2
      DO ii = 1, dim1
        n = n + 1_I4B
        a(ii, jj, kk, ll) = n
      END DO
    END DO
  END DO
END DO

! flat storage of a
b = RESHAPE(a, [ntotal])

! the index is the position of a(ii, jj, kk, ll) in the flat storage
isok = .TRUE.
DO ll = 1, dim4
  DO kk = 1, dim3
    DO jj = 1, dim2
      DO ii = 1, dim1
        indx = FortranIndex(ii, jj, kk, ll, dim1, dim2, dim3, dim4)
        isok = isok .AND. (b(indx) .EQ. a(ii, jj, kk, ll))
        isok = isok .AND. (indx .EQ. a(ii, jj, kk, ll))
      END DO
    END DO
  END DO
END DO
CALL OK(isok, "index matches the flat storage of a")

! first and last element
indx = FortranIndex(1_I4B, 1_I4B, 1_I4B, 1_I4B, dim1, dim2, dim3, dim4)
CALL OK(indx .EQ. 1_I4B, "index of the first element")
indx = FortranIndex(dim1, dim2, dim3, dim4, dim1, dim2, dim3, dim4)
CALL OK(indx .EQ. ntotal, "index of the last element")

! a single entry: 1 + (2-1)*2 + (3-1)*2*3 + (2-1)*2*3*4 = 39
indx = FortranIndex(1_I4B, 2_I4B, 3_I4B, 2_I4B, dim1, dim2, dim3, dim4)
CALL OK(indx .EQ. 39_I4B, "index of a(1, 2, 3, 2)")

! dimensions of size 1
indx = FortranIndex(1_I4B, 1_I4B, 1_I4B, 4_I4B, &
                    1_I4B, 1_I4B, 1_I4B, 4_I4B)
CALL OK(indx .EQ. 4_I4B, "dim1 = dim2 = dim3 = 1")

END PROGRAM main
