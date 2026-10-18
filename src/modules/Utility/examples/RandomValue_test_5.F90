!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: RandomValue selects an entry of an integer matrix

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE RandomUtility, ONLY: RandomValue
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: n = 1000
INTEGER(I4B) :: i, k(n), val(2, 3), one(1, 1)
LOGICAL(LGT) :: isok

val = RESHAPE([1_I4B, 2_I4B, 3_I4B, 4_I4B, 5_I4B, 6_I4B], &
              [2_I4B, 3_I4B])

! the selected value is an entry of val
DO i = 1, n
  k(i) = RandomValue(val)
END DO
isok = .TRUE.
DO i = 1, n
  isok = isok .AND. ANY(val .EQ. k(i))
END DO
CALL OK(isok, "selected value is an entry of val")

! the corner entries can be selected
isok = ANY(k .EQ. val(1, 1)) .AND. ANY(k .EQ. val(2, 3))
CALL OK(isok, "corner entries are selected")

! a matrix with one entry
one = 9_I4B
CALL OK(RandomValue(one) .EQ. 9_I4B, "matrix of size 1x1")

END PROGRAM main
