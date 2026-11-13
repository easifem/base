!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: OuterProd of rank-2 and rank-4 arrays

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE ProductUtility, ONLY: OuterProd
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
INTEGER(I4B), PARAMETER :: expected(6) = [2, 3, 2, 3, 2, 3]
REAL(DFP) :: a(2, 3), b(2, 3, 2, 3)
REAL(DFP), ALLOCATABLE :: ans(:, :, :, :, :, :)
REAL(DFP) :: want
INTEGER(I4B) :: ii, i1, i2, i3, i4, i5, i6
LOGICAL(LGT) :: isok

a = RESHAPE([(REAL(ii, DFP), ii=1, 6)], [2, 3])
b = RESHAPE([(REAL(ii, DFP), ii=1, 36)], [2, 3, 2, 3])

ans = OuterProd(a, b)

! the shape of ans is the shapes of the operands, one after another
CALL OK(ALL(SHAPE(ans) .EQ. expected), "shape of ans")

! every entry is the product of the entries of the operands
isok = .TRUE.
DO i6 = 1, 3
  DO i5 = 1, 2
    DO i4 = 1, 3
      DO i3 = 1, 2
        DO i2 = 1, 3
          DO i1 = 1, 2
            want = a(i1, i2) * b(i3, i4, i5, i6)
            isok = isok .AND. &
                   (ABS(ans(i1, i2, i3, i4, i5, i6) - want) .LT. tol)
          END DO
        END DO
      END DO
    END DO
  END DO
END DO
CALL OK(isok, "all entries of ans")

! first and last entries computed by hand
CALL OK(ABS(ans(1, 1, 1, 1, 1, 1) - 1.0_DFP) .LT. tol, "first entry")
CALL OK(ABS(ans(2, 3, 2, 3, 2, 3) - 216.0_DFP) .LT. tol, "last entry")

END PROGRAM main
