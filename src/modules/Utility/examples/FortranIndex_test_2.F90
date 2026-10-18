!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: FortranIndex for a 3D array

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE IndexUtility, ONLY: FortranIndex
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: dim1 = 2_I4B, dim2 = 3_I4B, dim3 = 4_I4B
INTEGER(I4B) :: a(dim1, dim2, dim3), b(dim1 * dim2 * dim3)
INTEGER(I4B) :: ii, jj, kk, n, indx
LOGICAL(LGT) :: isok

! fill a with 1, 2, ..., dim1*dim2*dim3 in column-major order
n = 0_I4B
DO kk = 1, dim3
  DO jj = 1, dim2
    DO ii = 1, dim1
      n = n + 1_I4B
      a(ii, jj, kk) = n
    END DO
  END DO
END DO

! flat storage of a
b = RESHAPE(a, [dim1 * dim2 * dim3])

! the index is the position of a(ii, jj, kk) in the flat storage
isok = .TRUE.
DO kk = 1, dim3
  DO jj = 1, dim2
    DO ii = 1, dim1
      indx = FortranIndex(ii, jj, kk, dim1, dim2, dim3)
      isok = isok .AND. (b(indx) .EQ. a(ii, jj, kk))
      isok = isok .AND. (indx .EQ. a(ii, jj, kk))
    END DO
  END DO
END DO
CALL OK(isok, "index matches the flat storage of a")

! first and last element
indx = FortranIndex(1_I4B, 1_I4B, 1_I4B, dim1, dim2, dim3)
CALL OK(indx .EQ. 1_I4B, "index of the first element")
indx = FortranIndex(dim1, dim2, dim3, dim1, dim2, dim3)
CALL OK(indx .EQ. dim1 * dim2 * dim3, "index of the last element")

! a single entry: 2 + (2 - 1) * 2 + (3 - 1) * 2 * 3 = 16
indx = FortranIndex(2_I4B, 2_I4B, 3_I4B, dim1, dim2, dim3)
CALL OK(indx .EQ. 16_I4B, "index of a(2, 2, 3)")

! dimensions of size 1
indx = FortranIndex(1_I4B, 1_I4B, 3_I4B, 1_I4B, 1_I4B, 5_I4B)
CALL OK(indx .EQ. 3_I4B, "dim1 = 1 and dim2 = 1")

END PROGRAM main
