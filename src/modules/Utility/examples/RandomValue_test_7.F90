!> author: Vikas Sharma, Ph. D.
! date: 2026-10-18
! summary: RandomValue selects an entry of a real matrix

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE RandomUtility, ONLY: RandomValue
USE Test_Method, ONLY: OK
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: n = 1000
INTEGER(I4B) :: i
REAL(DFP) :: x(n), val(3, 2), one(1, 1)
LOGICAL(LGT) :: isok

val = RESHAPE([0.1_DFP, 0.2_DFP, 0.3_DFP, 0.4_DFP, 0.5_DFP, 0.6_DFP], &
              [3_I4B, 2_I4B])

! the selected value is an entry of val
DO i = 1, n
  x(i) = RandomValue(val)
END DO
isok = .TRUE.
DO i = 1, n
  isok = isok .AND. ANY(val .EQ. x(i))
END DO
CALL OK(isok, "selected value is an entry of val")

! the corner entries can be selected
isok = ANY(x .EQ. val(1, 1)) .AND. ANY(x .EQ. val(3, 2))
CALL OK(isok, "corner entries are selected")

! a matrix with one entry
one = -2.0_DFP
CALL OK(RandomValue(one) .EQ. -2.0_DFP, "matrix of size 1x1")

END PROGRAM main
