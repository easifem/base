!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Get1DIndexFortran for a 2D array

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE IntegerUtility, ONLY: Get1DIndexFortran
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: a(2, 3), i, j
LOGICAL(LGT) :: isok

! a(i, j) stores its own 1D (column major) index
a = RESHAPE([1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B, 6_I4B], [2_I4B, 3_I4B])

CALL OK(Get1DIndexFortran(1_I4B, 1_I4B, 2_I4B, 3_I4B) .EQ. 1_I4B, &
        "(1, 1)")
CALL OK(Get1DIndexFortran(1_I4B, 2_I4B, 2_I4B, 3_I4B) .EQ. 3_I4B, &
        "(1, 2)")
CALL OK(Get1DIndexFortran(i=2_I4B, j=3_I4B, dim1=2_I4B, &
                          dim2=3_I4B) .EQ. 6_I4B, "(2, 3)")

isok = .TRUE.
DO j = 1, 3
  DO i = 1, 2
    isok = isok .AND. (Get1DIndexFortran(i, j, 2_I4B, 3_I4B) .EQ. a(i, j))
  END DO
END DO
CALL OK(isok, "same as column major storage")

END PROGRAM main
