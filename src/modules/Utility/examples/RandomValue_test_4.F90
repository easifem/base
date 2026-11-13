!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: RandomValue selects an entry of an integer vector

PROGRAM main
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE RandomUtility, ONLY: RandomValue
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: n = 1000
INTEGER(I4B) :: i, k(n), val(4)
LOGICAL(LGT) :: isok

val = [10_I4B, 20_I4B, 30_I4B, 40_I4B]

! the selected value is an entry of val
DO i = 1, n
  k(i) = RandomValue(val)
END DO
isok = .TRUE.
DO i = 1, n
  isok = isok .AND. ANY(val .EQ. k(i))
END DO
CALL OK(isok, "selected value is an entry of val")

! the first and the last entries can be selected
isok = ANY(k .EQ. val(1)) .AND. ANY(k .EQ. val(4))
CALL OK(isok, "first and last entries are selected")

! a vector with one entry
CALL OK(RandomValue([7_I4B]) .EQ. 7_I4B, "vector of size 1")

END PROGRAM main
