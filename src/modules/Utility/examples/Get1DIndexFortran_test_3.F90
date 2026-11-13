!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: Get1DIndexFortran for a 4D array

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE IntegerUtility, ONLY: Get1DIndexFortran
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B) :: i, j, k, l, want
LOGICAL(LGT) :: isok

CALL OK(Get1DIndexFortran(1_I4B, 1_I4B, 1_I4B, 2_I4B, &
                          2_I4B, 3_I4B, 4_I4B, 2_I4B) .EQ. 25_I4B, &
        "(1, 1, 1, 2)")
CALL OK(Get1DIndexFortran(2_I4B, 3_I4B, 4_I4B, 2_I4B, &
                          2_I4B, 3_I4B, 4_I4B, 2_I4B) .EQ. 48_I4B, &
        "(2, 3, 4, 2)")

! the index increases by one when i runs fastest
isok = .TRUE.
want = 0_I4B
DO l = 1, 2
  DO k = 1, 4
    DO j = 1, 3
      DO i = 1, 2
        want = want + 1_I4B
        isok = isok .AND. (Get1DIndexFortran(i, j, k, l, 2_I4B, 3_I4B, &
                                             4_I4B, 2_I4B) .EQ. want)
      END DO
    END DO
  END DO
END DO
CALL OK(isok, "same as column major storage")

END PROGRAM main
