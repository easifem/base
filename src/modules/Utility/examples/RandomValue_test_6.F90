!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: RandomValue selects an entry of a real vector

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE RandomUtility, ONLY: RandomValue
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: n = 1000
INTEGER(I4B) :: i
REAL(DFP) :: x(n), val(3)
LOGICAL(LGT) :: isok

val = [0.5_DFP, -1.5_DFP, 2.25_DFP]

! the selected value is an entry of val
DO i = 1, n
  x(i) = RandomValue(val)
END DO
isok = .TRUE.
DO i = 1, n
  isok = isok .AND. ANY(val .EQ. x(i))
END DO
CALL OK(isok, "selected value is an entry of val")

! the first and the last entries can be selected
isok = ANY(x .EQ. val(1)) .AND. ANY(x .EQ. val(3))
CALL OK(isok, "first and last entries are selected")

! a vector with one entry
CALL OK(RandomValue([3.5_DFP]) .EQ. 3.5_DFP, "vector of size 1")

END PROGRAM main
